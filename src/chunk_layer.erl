-module(chunk_layer).

-include("block.hrl").
-include("chunk.hrl").

-export([set_blocks/4]).
-export([layer_air/1, layer/2]).


set_blocks(ChunkLayer, {Fx, Fy, Fz}, {Tx, Ty, Tz}, BlockId) ->
    X=1,Z=16,Y=256,
    set_blocks_(ChunkLayer, X*Fx + Y*Fy + +Z*Fz, X*Tx + Y*Ty + Z*Tz, BlockId).

layer_air(YPos) when is_integer(YPos) ->
    #chunk_layer{
        y_position = YPos,
        types=binary:copy(<<((blocks:air())#block.id)>>,16*256),
        metadata=binary:copy(<<0>>,16*16*8),
        block_light=binary:copy(<<255>>,16*16*8),
        sky_light=binary:copy(<<255>>,16*16*8)
    }.

layer(YPos, #block{id = Id}) ->
    chunk_manager_srv:set_blocks(layer_air(YPos), {0,0,0}, {16,1,16}, Id).

%% Internal

set_blocks_(ChunkLayer0, FromOffset, ToOffset, BlockId) ->
    Diff = ToOffset - FromOffset,
    <<Left:FromOffset/binary, _:Diff/binary, Right/binary>> = ChunkLayer0#chunk_layer.types,
    ChunkLayer0#chunk_layer{
        types = <<Left/binary, (binary:copy(<<BlockId>>, Diff))/binary, Right/binary>>
    }.