-module(client_srv).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("packets.hrl").
-include("cipher_data.hrl").
-include("player.hrl").

-export([writer/2, decrypter/2]).
-export([start_link/4]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-type opts() :: [].
-export_type([opts/0]).

-record(state, {
    socket :: inet:socket(),
    transport :: module(),
    username :: undefined | binary(),
    token :: binary(),
    handshaken :: boolean(),
    decryption_cipher :: binary(),
    writer :: pid(),
    decrypter :: pid()
}).

writer(Socket, Encryption0) ->
    receive
        {Type, Args} ->
            {ok, Encoded} = packets:encode(Type, Args),
            Encryption1 = write(Socket, Encryption0, Encoded),
            writer(Socket, Encryption1)
    end.

decrypter(Socket, Encryption0) ->
    receive
        {tcp, Socket, Data} ->
            ranch_tcp:setopts(Socket, [{active, once}]),
            {Encryption1, Decrypted} = packets:apply_cipher(Encryption0, Data),
            lager:debug("Encryption0: ~10000p~nData: ~1000p~n", [lager:pr(Encryption0, ?MODULE), Data]),
            hd(get('$ancestors')) ! {decrypted, Decrypted},
            decrypter(Socket, Encryption1);
        {tcp_closed, Socket} ->
            exit({shutdown, client_disconnected});
        Err = {tcp_error, _Reason, Socket} ->
            exit(Err)
    end.

start_link(Ref, Socket, Transport, Opts) ->
    Args = [Ref, Socket, Transport, Opts],
    proc_lib:start_link(?MODULE, init, [Args]).

init([Ref, Socket, Transport, _Opts]) ->
    proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    {ok, {IP, Port}} = ranch_tcp:peername(Socket),
    lager:debug("Incoming client on ~s:~b~n", [ip_to_string(IP), Port]),
    ranch_tcp:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{
        socket = Socket, 
        transport = Transport,
        username = undefined,
        token = crypto:strong_rand_bytes(4),
        handshaken = false
    }).

handle_call(Request, _From, State) ->
    lager:debug("Request: ~p~n", [Request]),
    {reply, ignored, State}.

handle_cast(Msg, State) ->
    lager:debug("Msg: ~p~n", [Msg]),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State = #state{socket = Socket}) ->
    ranch_tcp:setopts(Socket, [{active, once}]),
    case packets:decode(Data) of
        {ok, Packet} ->
            handle_packet(Packet, State);
        Err = {error, _} ->
            lager:error("Failed to parse packet ~p with error ~p~n", [Data, Err]),
            {noreply, State}
    end;
handle_info({tcp_closed, Socket}, State = #state{socket = Socket}) ->
    {stop, {shutdown, client_disconnected}, State};
handle_info(Err = {tcp_error, _Reason, Socket}, State = #state{socket = Socket}) ->
    {stop, Err, State};

handle_info({decrypted, Decrypted}, State) ->
    case packets:decode(Decrypted) of
        {ok, Decoded} ->
            lager:debug("Decoded: ~p~n", [Decoded]);
        Err = {error, _} ->
            lager:warning("Failed to decode packet with error ~p~n", [Err]),
            {stop, {shutdown, bad_packet}, State}
    end;
handle_info(Info, State) ->
    lager:debug("Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

ip_to_string({A, B, C, D}) ->
    io_lib:format("~b.~b.~b.~b", [A,B,C,D]).

handle_packet(#packet{type = server_list_ping, fields = #{unknown_1 := 1}}, State = #state{handshaken = false, socket = Socket}) ->
    ranch_tcp:send(Socket, format_disconnect([
        "§1", 0,
        "51", 0,
        "1.4.6", 0,
        server_properties:get_motd(), 0,
        integer_to_list(stat_srv:get_player_count()), 0,
        "101"
    ])),
    {stop, {shutdown, server_list_ping}, State};
handle_packet(#packet{type = handshake, fields = #{version := 51, username := Username}}, State = #state{handshaken = false}) ->
    {ok, EncodedPacket} = packets:encode(encryption_key_request, #{
        unknown_1 => "-", 
        public_key => config:get_crypto_public_rsa(), 
        token => State#state.token
    }),
    ranch_tcp:send(State#state.socket, EncodedPacket),
    {noreply, State#state{username = Username}};
handle_packet(#packet{type = handshake}, State = #state{handshaken = false, socket = Socket}) ->
    ranch_tcp:send(Socket, format_disconnect("You're too oldschool! (Wrong client version.)")),
    gen_tcp:close(Socket),
    {stop, {shutdown, bad_handshake}, State};
handle_packet(#packet{type=encryption_key_response, fields = Fields}, State = #state{handshaken = false, socket = Socket, token = Token}) ->
    #{encrypted_sym_key := EncryptedSymKey, encrypted_token := EncryptedToken} = Fields,
    DecrToken = public_key:decrypt_private(EncryptedToken, config:get_crypto_private_rsa()),
    case DecrToken =:= Token of
        true ->
            DecrSymKey = public_key:decrypt_private(EncryptedSymKey, config:get_crypto_private_rsa()),
            {ok, EncodedPacket} = packets:encode(encryption_key_response, #{
                encrypted_sym_key => <<>>, 
                encrypted_token => <<>>
            }),
            ranch_tcp:send(Socket, EncodedPacket),
            case chat:is_valid_nickname(State#state.username) of
                true ->
                    CipherData = #cipher_data{key = DecrSymKey, ivec = DecrSymKey},
                    WriterPid = proc_lib:spawn_link(?MODULE, writer, [Socket, CipherData]),
                    register(binary_to_atom(<<(State#state.username)/binary, "_writer">>), WriterPid),
                    DecrypterPid = proc_lib:spawn_link(?MODULE, decrypter, [Socket, CipherData]),
                    register(binary_to_atom(<<(State#state.username)/binary, "_decrypter">>), DecrypterPid),
                    ok = ranch_tcp:controlling_process(Socket, DecrypterPid),
                    register(binary_to_atom(<<(State#state.username)/binary, "_processor">>), self()),
                    do_login(State#state{handshaken = true, decryption_cipher = CipherData, writer = WriterPid, decrypter = DecrypterPid});
                false ->
                    lager:warning("Attempt to login with bad username '~s'", [State#state.username]),
                    ranch_tcp:send(Socket, format_disconnect("Bad username!")),
                    {stop, {shutdown, disconnect}, State}
            end;
        false ->
            ranch_tcp:send(Socket, format_disconnect("Not ready to take clients yet!")),
            {stop, {shutdown, disconnect}, State}
    end;
handle_packet(Packet, State) ->
    lager:warning("Unhandled packet ~p~n", [lager:pr(Packet, ?MODULE)]),
    {noreply, State}.

do_login(State) ->
    %% TODO: Needs to be synchro
    player_tab:insert(#player{
        pid = self(),
        name = State#state.username
    }),
    State#state.writer ! {login_request, #{
        unknown_1 => 1, 
        unknown_2 => "DEFAULT", 
        unknown_3 => 1, 
        unknown_4 => 0, 
        unknown_5 => 0, 
        unknown_6 => 0, 
        unknown_7 => 100
    }},
    <<Flags:8>> = <<0:4, 0:1, 1:1, 0:1, 1:1>>,
    State#state.writer ! {player_abilities, #{
        unknown_1 => Flags, 
        unknown_2 => 12, 
        unknown_3 => 25
    }},
    State#state.writer ! {window_items, #{
        unknown_1 => 0, 
        unknown_2 => <<>>
    }},
    State#state.writer ! {spawn_position, #{
        unknown_1 => 0, 
        unknown_2 => 0, 
        unknown_3 => 0
    }},
    Chunk = chunk_manager_srv:get_chunk(chunk_manager_srv:tabname("world", overworld), {0, 0}),
    State#state.writer ! {map_chunk, #{
        unknown_1 => 0, 
        unknown_2 => 0, 
        unknown_3 => Chunk
    }},

    State#state.writer ! {player_list_item, #{
        unknown_1 => State#state.username, 
        unknown_2 => true, 
        unknown_3 => 1
    }},
    State#state.writer ! {player_position_look, #{
        unknown_1 => 1,
        unknown_2 => 1.62,
        unknown_3 => 1,
        unknown_4 => 1,
        unknown_5 => 1,
        unknown_6 => 1,
        unknown_7 => true
    }},
    {noreply, State}.

format_disconnect(Data) ->
    {ok, EncodedPacket} = packets:encode(disconnect, #{reason => Data}),
    EncodedPacket.

write(Socket, Encryption0, Msg) ->
    {Encryption1, EncryptedMsg} = packets:apply_cipher(Encryption0, Msg),
    ranch_tcp:send(Socket, EncryptedMsg),
    Encryption1.