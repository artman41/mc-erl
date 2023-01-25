-module(client_srv).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("packets.hrl").
-include("encryption.hrl").

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
    token :: binary()
}).

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
        token = crypto:strong_rand_bytes(4)
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

handle_packet(#packet{type = server_list_ping, fields = #{unknown_1 := 1}}, State = #state{socket = Socket}) ->
    ranch_tcp:send(Socket, format_disconnect([
        "§1", 0,
        "51", 0,
        "1.4.6", 0,
        server_properties:get_motd(), 0,
        integer_to_list(stat_srv:get_player_count()), 0,
        "101"
    ])),
    {stop, {shutdown, server_list_ping}, State};
handle_packet(#packet{type = handshake, fields = #{version := 51, username := Username}}, State) ->
    {ok, EncodedPacket} = packets:encode(encryption_key_request, #{
        unknown_1 => "-", 
        public_key => config:get_crypto_public_rsa(), 
        token => State#state.token
    }),
    ranch_tcp:send(State#state.socket, EncodedPacket),
    {noreply, State#state{username = Username}};
handle_packet(#packet{type = handshake}, State = #state{socket = Socket}) ->
    ranch_tcp:send(Socket, format_disconnect("You're too oldschool! (Wrong client version.)")),
    gen_tcp:close(Socket),
    {stop, {shutdown, bad_handshake}, State};
handle_packet(#packet{type=encryption_key_response, fields = Fields}, State = #state{socket = Socket, token = Token}) ->
    #{encrypted_sym_key := EncryptedSymKey, encrypted_token := EncryptedToken} = Fields,
    DecrToken = public_key:decrypt_private(EncryptedToken, config:get_crypto_private_rsa()),
    ranch_tcp:send(Socket, format_disconnect("Not ready to take clients yet!")),
    {stop, {shutdown, disconnect}, State};
    % case DecrToken =:= Token of
    %     true ->
    %         DecrSymKey = public_key:decrypt_private(EncryptedSymKey, config:get_crypto_private_rsa()),
    %         {ok, EncodedPacket} = packets:encode(encryption_key_response, [<<>>,<<>>]),
    %         Encryption = #encryption{key = DecrSymKey, ivec = DecrSymKey},
    %         ok;
    %     false ->
    %         ranch_tcp:send(Socket, format_disconnect("Not ready to take clients yet!")),
    %         {stop, {shutdown, disconnect}, State}
    % end;
handle_packet(Packet, State) ->
    lager:warning("Unhandled packet ~p~n", [lager:pr(Packet, ?MODULE)]),
    {noreply, State}.

format_disconnect(Data) ->
    {ok, EncodedPacket} = packets:encode(disconnect, #{reason => Data}),
    EncodedPacket.

write(Socket, Encrpytion0, Msg) ->
    {EncryptedMsg, Encryption1} = packets:encrypt(Encrpytion0, Msg),
    ranch_tcp:send(Socket, EncryptedMsg),
    Encryption1.