-module(packet_encoder).
-export([
    uint8/1,
    int8/1,
    int16/1,
    int32/1,
    int64/1,
    float32/1,
    float64/1,
    string/1,
    bool/1,
    chunk_bulk/1,
    slots/1,
    abs_byte/1,
    abs_int/1,
    metadata/1,
    multi_block_change_data/1
]).

uint8(N) when is_integer(N) andalso N >= 16#00 andalso N =< 16#ff ->
    <<N:8/unsigned>>.

int8(N) when is_integer(N) andalso N >= -16#80 andalso N =< 16#7f ->
    <<N:8/signed>>.

int16(N) when is_integer(N) andalso N >= -16#8000 andalso N =< 16#7fff ->
    <<N:16/signed>>.

int32(N) when is_integer(N) andalso N >= -16#80000000 andalso N =< 16#7fffffff ->
    <<N:32/signed>>.

int64(N) when is_integer(N) andalso N >= -16#8000000000000000 andalso N =< 16#7fffffffffffffff ->
    <<N:64/signed>>.

float32(F) when is_float(F) andalso -16#80000000 andalso F =< 16#7fffffff ->
    <<F:32/float>>.

float64(F) when is_float(F) andalso -16#8000000000000000 andalso F =< 16#7fffffffffffffff ->
    <<F:64/float>>.

string(Data) ->
    list_to_binary([
        int16(iolist_size(Data)),
        unicode:characters_to_binary(Data, unicode, utf16)
    ]).

bool(true) ->
    <<1:8>>;
bool(false) ->
    <<0:8>>.

chunk_bulk(<<Packet/binary>>) ->
    erlang:error(not_implemented).

slots(<<Packet/binary>>) ->
    erlang:error(not_implemented).

abs_byte(<<Packet/binary>>) ->
    erlang:error(not_implemented).

abs_int(<<Packet/binary>>) ->
    erlang:error(not_implemented).

metadata(<<Packet/binary>>) ->
    erlang:error(not_implemented).

multi_block_change_data(<<Packet/binary>>) ->
    erlang:error(not_implemented).

%% Internal

array_(Packet, 0, _Type, Arr) ->
    {ok, {Arr, Packet}};
array_(<<Packet0/binary>>, ElemsLeft, Type, Arr) ->
    {ok, {V, Packet1}} = Type(Packet0),
    array_(Packet1, ElemsLeft-1, Type, [V | Arr]).