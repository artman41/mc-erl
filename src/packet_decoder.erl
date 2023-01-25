-module(packet_decoder).
-export([array/3]).
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

array(<<Packet0/binary>>, LenType, binary) ->
    {ok, {Length, Packet1}} = ?MODULE:LenType(Packet0),
    <<Bin:Length/binary, Tail/binary>> = Packet1,
    {ok, {Bin, Tail}};
array(<<Packet0/binary>>, LenType, Type) ->
    {ok, {Length, Packet1}} = ?MODULE:LenType(Packet0),
    array_(Packet1, Length, Type, []).

uint8(<<I:8/unsigned, Tail/binary>>) ->
    {ok, {I, Tail}}.

int8(<<I:8/signed, Tail/binary>>) ->
    {ok, {I, Tail}}.

int16(<<I:16/signed, Tail/binary>>) ->
    {ok, {I, Tail}}.

int32(<<I:32/signed, Tail/binary>>) ->
    {ok, {I, Tail}}.

int64(<<I:64/signed, Tail/binary>>) ->
    {ok, {I, Tail}}.

float32(<<F:32/float, Tail/binary>>) ->
    {ok, {F, Tail}}.

float64(<<F:64/float, Tail/binary>>) ->
    {ok, {F, Tail}}.

string(<<Packet/binary>>) ->
    {ok, {StringLen, Bin}} = int16(Packet),
    <<String:(StringLen*2)/binary, Tail/binary>> = Bin,
    {ok, {unicode:characters_to_binary(String, utf16, unicode), Tail}}.

bool(<<I:8, Tail/binary>>) ->
    {ok, {I =:= 1, Tail}}.

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