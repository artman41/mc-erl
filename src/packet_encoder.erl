-module(packet_encoder).

-include("chunk.hrl").

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
    '#chunk'/1,
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

float32(I) when is_integer(I) ->
    float32(I*1.0);
float32(F) when is_float(F) andalso F >= -16#80000000 andalso F =< 16#7fffffff ->
    <<F:32/float>>.

float64(I) when is_integer(I) ->
    float64(I*1.0);
float64(F) when is_float(F) andalso F >= -16#8000000000000000 andalso F =< 16#7fffffffffffffff ->
    <<F:64/float>>.

string(Data) ->
    list_to_binary([
        int16(iolist_size(Data)),
        unicode:characters_to_binary(Data, unicode, utf16)
    ]).

'#chunk'(unload) ->
    [bool(true), int16(0), int16(0), int32(0)];
'#chunk'({raw, Bin}) ->
    [int32(byte_size(Bin)), int32(0), Bin];
'#chunk'({uncompressed, Uncompressed}) ->
    Bin = zlib:compress(Uncompressed),
    [int32(byte_size(Bin)), int32(0), Bin];
'#chunk'(#chunk{data = Data}) ->
    Dict = 
        lists:foldr(fun(#chunk_layer{types = Types, metadata = Metadata, block_light = BlockLight, sky_light = SkyLight}, Acc0) ->
            Acc1 = dict:append(types, Types, Acc0),
            Acc2 = dict:append(metadata, Metadata, Acc1),
            Acc3 = dict:append(block_light, BlockLight, Acc2),
            Acc4 = dict:append(sky_light, SkyLight, Acc3)
        end, dict:new(), Data#chunk_data.layers),

    List_Types = dict:fetch(types, Dict),
    List_Metadata = dict:fetch(metadata, Dict),
    List_BlockLight = dict:fetch(block_light, Dict),
    List_SkyLight = dict:fetch(sky_light, Dict),
    
    BinData = list_to_binary([List_Types, List_Metadata, List_BlockLight, List_SkyLight, Data#chunk_data.biome]),
    CompressedData = zlib:compress(BinData),

    [bool(_FullColumn = false), <<16#FF,16#FF>>, <<16#FF,16#FF>>, int32(byte_size(CompressedData)), CompressedData].

bool(true) ->
    <<1:8>>;
bool(false) ->
    <<0:8>>.

chunk_bulk(<<Packet/binary>>) ->
    erlang:error(not_implemented).

slots(_Packet) ->
    %%TODO: Properly implement
    int16(0).

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