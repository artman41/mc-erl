-module(packets).

-include("packets.hrl").
-include("cipher_data.hrl").

-export([decode/1, encode/2]).
-export([apply_cipher/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(EUNIT_LOG(F, A), ?debugFmt("("++atom_to_list(?FUNCTION_NAME)++") " ++ F, A)).
-define(EUNIT_LOG(Msg), ?debugMsg("("++atom_to_list(?FUNCTION_NAME)++") " ++ Msg)).
-else.
-define(EUNIT_LOG(F, A), ok).
-define(EUNIT_LOG(Msg), ok).
-endif.

decode(<<Id:8, Tail/binary>>) ->
    case lists:keyfind(Id, 1, packets()) of
        {Id, PacketType, PacketFieldSpec} ->
            decode_(Tail, PacketFieldSpec, #packet{type = PacketType, fields = #{}});
        false ->
            {error, {unknown_packet_id, Id}}
    end.

-spec encode(Type :: atom(), Args :: map()) -> {ok, binary()} | {error, any()}.
encode(Type, Args) ->
    case lists:keyfind(Type, 2, packets()) of
        {PacketId, Type, PacketFieldSpec} ->
            case encode_(PacketFieldSpec, Args, []) of
                {ok, Encoded} ->
                    {ok, list_to_binary([<<PacketId:8>>, Encoded])};
                Err = {error, _} ->
                    Err
            end;
        false ->
            {error, {unknown_packet_type, Type}}
    end.

    
-spec apply_cipher(Encryption0, Msg) -> {Encryption1, EncryptedData} when
    Encryption0 :: #cipher_data{}, 
    Encryption1 :: Encryption0,
    Msg :: iolist(),
    EncryptedData :: iolist().
apply_cipher(Encryption, Msg) when is_record(Encryption, cipher_data) ->
    apply_cipher_(Encryption, Msg, []).

apply_cipher_(Encryption0 = #cipher_data{ivec = IVec0}, Msg, Acc) -> 
    ?EUNIT_LOG("Encryption0: ~p~nMsg: ~p~nAcc: ~p~n", [Encryption0, Msg, Acc]),
    case utils:get_next_char(Msg) of
        false ->
            {Encryption0, lists:reverse(Acc)};
        {Char, RestMsg} ->
            ?EUNIT_LOG("Char: ~p~nRestMsg: ~p~n", [Char, RestMsg]),
            Cipher = crypto:crypto_one_time(aes_cfb128, Encryption0#cipher_data.key, IVec0, [Char, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], []),
            <<Byte:8, _/binary>> = Cipher,
            ?EUNIT_LOG("Cipher: ~p~nByte: ~p~n", [Cipher, Byte]),
            Encryption1 = Encryption0#cipher_data{ivec = gen_ivec(IVec0, Byte)},
            ?EUNIT_LOG("Encryption1: ~p~n", [Encryption1]),
            apply_cipher_(Encryption1, RestMsg, [Byte | Acc])
    end.

%% Internal

-spec packets() -> list(PacketSpec) when 
    PacketSpec :: {PacketId, PacketType, PacketFieldSpec},
    PacketId :: non_neg_integer(),
    PacketType :: atom(),
    PacketFieldSpec :: list({FieldName :: atom(), FieldType :: atom()}).
packets() ->
    [
        {0, keep_alive, [{unknown_1, int32}]},
        {1, login_request, [{unknown_1, int32}, {unknown_2, string}, {unknown_3, int8}, {unknown_4, int8}, {unknown_5, int8}, {unknown_6, uint8}, {unknown_7, uint8}]},
        {2, handshake, [{version, int8}, {username, string}, {server_host, string}, {server_port, int32}]},
        {3, chat_message, [{unknown_1, string}]},
        {4, time_update, [{unknown_1, int64}, {unknown_2, int64}]},
        {5, entity_equipment, [{unknown_1, int32}, {unknown_2, int16}, {unknown_3, slot}]},
        {6, spawn_position, [{unknown_1, int32}, {unknown_2, int32}, {unknown_3, int32}]},
        {7, use_entity, [{unknown_1, int32}, {unknown_2, int32}, {unknown_3, bool}]},
        {8, update_health, [{unknown_1, int16}, {unknown_2, int16}, {unknown_3, float32}]},
        {9, respawn, [{unknown_1, int32}, {unknown_2, int8}, {unknown_3, int8}, {unknown_4, int16}, {unknown_5, string}]},
        {10, player, [{unknown_1, bool}]},
        {11, player_position, [{unknown_1, float64}, {unknown_2, float64}, {unknown_3, float64}, {unknown_4, float64}, {unknown_5, bool}]},
        {12, player_look, [{unknown_1, float32}, {unknown_2, float32}, {unknown_3, bool}]},
        {13, player_position_look, [{unknown_1, float64}, {unknown_2, float64}, {unknown_3, float64}, {unknown_4, float64}, {unknown_5, float32}, {unknown_6, float32}, {unknown_7, bool}]},
        {14, player_digging, [{unknown_1, int8}, {unknown_2, int32}, {unknown_3, int8}, {unknown_4, int32}, {unknown_5, int8}]},
        {15, player_block_placement, [{unknown_1, int32}, {unknown_2, uint8}, {unknown_3, int32}, {unknown_4, int8}, {unknown_5, slot}, {unknown_6, int8}, {unknown_7, int8}, {unknown_8, int8}]},
        {16, holding_change, [{unknown_1, int16}]},
        {17, use_bed, [{unknown_1, int32}, {unknown_2, int8}, {unknown_3, int32}, {unknown_4, int8}, {unknown_5, int32}]},
        {18, animation, [{unknown_1, int32}, {unknown_2, int8}]},
        {19, entity_action, [{unknown_1, int32}, {unknown_2, int8}]},
        {20, named_entity_spawn, [{unknown_1, int32}, {unknown_2, string}, {unknown_3, abs_int}, {unknown_4, abs_int}, {unknown_5, abs_int}, {unknown_6, int8}, {unknown_7, int8}, {unknown_8, int16}, {unknown_9, metadata}]},
        {22, collect_item, [{unknown_1, int32}, {unknown_2, int32}]},
        {23, add_object, [{unknown_1, int32}, {unknown_2, int8}, {unknown_3, abs_int}, {unknown_4, abs_int}, {unknown_5, abs_int}, {unknown_6, int8}, {unknown_7, int8}, {unknown_8, projectile_data}]},
        {24, mob_spawn, [{unknown_1, int32}, {unknown_2, int8}, {unknown_3, abs_int}, {unknown_4, abs_int}, {unknown_5, abs_int}, {unknown_6, int8}, {unknown_7, int8}, {unknown_8, int8}, {unknown_9, int16}, {unknown_10, int16}, {unknown_11, int16}, {unknown_12, metadata}]},
        {26, experience_orb, [{unknown_1, int32}, {unknown_2, int32}, {unknown_3, int32}, {unknown_4, int32}, {unknown_5, int16}]},
        {28, entity_velocity, [{unknown_1, int32}, {unknown_2, int16}, {unknown_3, int16}, {unknown_4, int16}]},
        {29, destroy_entity, [{unknown_1, array}, {unknown_2, int8}, {unknown_3, int32}]},
        {31, entity_move, [{unknown_1, int32}, {unknown_2, abs_byte}, {unknown_3, abs_byte}, {unknown_4, abs_byte}]},
        {32, entity_look, [{unknown_1, int32}, {unknown_2, int8}, {unknown_3, int8}]},
        {33, entity_look_move, [{unknown_1, int32}, {unknown_2, abs_byte}, {unknown_3, abs_byte}, {unknown_4, abs_byte}, {unknown_5, int8}, {unknown_6, int8}]},
        {34, entity_teleport, [{unknown_1, int32}, {unknown_2, abs_int}, {unknown_3, abs_int}, {unknown_4, abs_int}, {unknown_5, int8}, {unknown_6, int8}]},
        {35, entity_head_look, [{unknown_1, int32}, {unknown_2, int8}]},
        {38, entity_status, [{unknown_1, int32}, {unknown_2, int8}]},
        {39, attach_entity, [{unknown_1, int32}, {unknown_2, int32}]},
        {40, entity_metadata, [{unknown_1, int32}, {unknown_2, metadata}]},
        {41, entity_effect, [{unknown_1, int32}, {unknown_2, int8}, {unknown_3, int8}, {unknown_4, int16}]},
        {42, remove_entity_effect, [{unknown_1, int32}, {unknown_2, int8}]},
        {43, experience, [{unknown_1, float32}, {unknown_2, int16}, {unknown_3, int16}]},
        {51, map_chunk, [{unknown_1, int32}, {unknown_2, int32}, {unknown_3, '#chunk'}]},
        {52, multi_block_change, [{unknown_1, int32}, {unknown_2, int32}, {unknown_3, multi_block_change_data}]},
        {53, block_change, [{unknown_1, int32}, {unknown_2, int8}, {unknown_3, int32}, {unknown_4, int16}, {unknown_5, int8}]},
        {54, block_action, [{unknown_1, int32}, {unknown_2, int16}, {unknown_3, int32}, {unknown_4, int8}, {unknown_5, int8}, {unknown_6, int16}]},
        {55, block_break_animation, [{unknown_1, int32}, {unknown_2, int32}, {unknown_3, int32}, {unknown_4, int32}, {unknown_5, int8}]},
        {56, map_chunk_bulk, [{unknown_1, chunk_bulk}]},
        {60, explosion, [{unknown_1, float64}, {unknown_2, float64}, {unknown_3, float64}, {unknown_4, float32}, {unknown_5, coordinate_offsets}, {unknown_6, float32}, {unknown_7, float32}, {unknown_8, float32}]},
        {61, sound_particle_effect, [{unknown_1, int32}, {unknown_2, int32}, {unknown_3, int8}, {unknown_4, int32}, {unknown_5, int32}, {unknown_6, bool}]},
        {62, named_sound_effect, [{unknown_1, string}, {unknown_2, int32}, {unknown_3, int32}, {unknown_4, int32}, {unknown_5, float32}, {unknown_6, int8}]},
        {70, new_invalid_state, [{unknown_1, int8}, {unknown_2, int8}]},
        {71, thunderbolt, [{unknown_1, int32}, {unknown_2, bool}, {unknown_3, abs_int}, {unknown_4, abs_int}, {unknown_5, abs_int}]},
        {100, open_window, [{unknown_1, int8}, {unknown_2, int8}, {unknown_3, string}, {unknown_4, int8}]},
        {101, close_window, [{unknown_1, int8}]},
        {102, window_click, [{unknown_1, int8}, {unknown_2, int16}, {unknown_3, int8}, {unknown_4, int16}, {unknown_5, bool}, {unknown_6, slot}]},
        {103, set_slot, [{unknown_1, int8}, {unknown_2, int16}, {unknown_3, slot}]},
        {104, window_items, [{unknown_1, int8}, {unknown_2, slots}]},
        {105, update_window_property, [{unknown_1, int8}, {unknown_2, int16}, {unknown_3, int16}]},
        {106, transaction, [{unknown_1, int8}, {unknown_2, int16}, {unknown_3, bool}]},
        {107, creative_inventory_action, [{unknown_1, int16}, {unknown_2, slot}]},
        {108, enchant_item, [{unknown_1, int8}, {unknown_2, int8}]},
        {130, update_sign, [{unknown_1, int32}, {unknown_2, int16}, {unknown_3, int32}, {unknown_4, string}, {unknown_5, string}, {unknown_6, string}, {unknown_7, string}]},
        {131, item_data, [{unknown_1, int16}, {unknown_2, int16}, {unknown_3, {array, int16, binary}}]},
        {132, update_tile_entity, [{unknown_1, int32}, {unknown_2, int16}, {unknown_3, int32}, {unknown_4, int8}, {unknown_5, int16}]},
        {200, increment_statistic, [{unknown_1, int32}, {unknown_2, int8}]},
        {201, player_list_item, [{unknown_1, string}, {unknown_2, bool}, {unknown_3, int16}]},
        {202, player_abilities, [{unknown_1, int8}, {unknown_2, int8}, {unknown_3, int8}]},
        {203, tab_complete, [{unknown_1, string}]},
        {204, client_settings, [{unknown_1, string}, {unknown_2, int8}, {unknown_3, int8}, {unknown_4, int8}, {unknown_5, bool}]},
        {205, client_statuses, [{unknown_1, int8}]},
        {250, plugin_message, [{unknown_1, string}, {unknown_2, {array, int16, int8}}]},
        {252, encryption_key_response, [{encrypted_sym_key, {array, int16, binary}}, {encrypted_token, {array, int16, binary}}]},
        {253, encryption_key_request, [{unknown_1, string}, {public_key, {array, int16, binary}}, {token, {array, int16, binary}}]},
        {254, server_list_ping, [{unknown_1, int8}]},
        {255, disconnect, [{reason, string}]}
    ].

decode_(_Packet, [], Rec) ->
    {ok, Rec};
  decode_(Packet, [{Name,Type}|Tail], Rec = #packet{fields = Fields}) ->
      {Fun, Args} =
          case Type of
              {array, LenType, DataType} -> {array, [Packet, LenType, DataType]};
              _ -> {Type, [Packet]}
          end,
      try apply(packet_decoder, Fun, Args) of
          {ok, {Value, Rest}} ->
              decode_(Rest, Tail, Rec#packet{fields = Fields#{Name => Value}});
          Err = {error, _} ->
              Err
      catch error:undef ->
          {error, {unknown_type, Type}}
      end.

encode_([], _Args, Acc) ->
    {ok, lists:reverse(Acc)};
encode_([{Name, {array, LenType, DataType}}|Tail], Args, Acc) ->
    Value = maps:get(Name, Args),
    Length =
        try iolist_size(Value) of
            IOSize -> IOSize
        catch error:badarg ->
            length(Value)
        end,
    Ret = 
        try
            {ok, [
                packet_encoder:LenType(Length),
                case DataType of
                    binary ->
                        Value;
                    _ ->
                        packet_encoder:DataType(Value)
                end
            ]}
        catch error:undef:S ->
            {_, BadType, _, _} = hd(S),
            {error, {unknown_type, BadType}}
        end,
    case Ret of
        {ok, Data} ->
            encode_(Tail, Args, [Data | Acc]);
        {error, _} ->
            Ret
    end;
encode_([{Name,Type}|Tail], Args, Acc) ->
    Value = maps:get(Name, Args),
    lager:debug("Name: ~p, Type: ~p, Value: ~p~n", [Name, Type, Value]),
    try packet_encoder:Type(Value) of
        EncodedValue ->
            encode_(Tail, Args, [EncodedValue|Acc])
    catch error:undef ->
        {error, {unknown_type, Type}}
    end.

gen_ivec(<<_:1/binary, IVPart/binary>>, Byte) when 16#00 =< Byte andalso Byte =< 16#FF  ->
    <<IVPart/binary, Byte>>.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(ENCRYPTION, #cipher_data{
    key = <<41,220,25,139,193,47,112,40,188,137,14,233,76,30,189,166>>,
    ivec = <<41,220,25,139,193,47,112,40,188,137,14,233,76,30,189,166>>
}).

-define(IO_BYTES(Bytes), iolist_to_binary([if C < 32 -> [$[, integer_to_list(C), $]]; C > 139 -> [$[,integer_to_list(C), $]]; true -> C end || C <- Bytes])).

encryption_test() ->
    Msg = "This is a test message",
    {E_Encryption1, EncryptedMsg} = apply_cipher(?ENCRYPTION, Msg),
    ?debugFmt("Msg: ~p~nEncryptedMsg: ~p~n", [Msg, EncryptedMsg]),
    {D_Encryption1, DecryptedMsg} = apply_cipher(?ENCRYPTION, EncryptedMsg),
    ?debugFmt("EncryptedMsg: ~p~nDecryptedMsg: ~p~n", [EncryptedMsg, DecryptedMsg]),
    ?debugFmt("EncryptedMsg: ~s~nDecryptedMsg: ~s~n", [?IO_BYTES(EncryptedMsg), ?IO_BYTES(DecryptedMsg)]),
    ?assertEqual(Msg, DecryptedMsg).

-endif.