-module(chunk_manager_srv).
-behaviour(gen_server).

-include("chunk.hrl").

%% API.
-export([tabname/2, get_chunk/2]).
-export([start_link/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    table :: atom()
}).

%% API.

tabname(WorldName, Dimension) when is_list(WorldName) andalso is_atom(Dimension) ->
    list_to_atom(WorldName ++ "#" ++ atom_to_list(Dimension)).

get_chunk(TabName, ChunkCoord = {_X, _Z}) ->
    Q = fun() ->
                case mnesia:read(TabName, ChunkCoord) of
                    [] ->
                        Chunk = generate_chunk(ChunkCoord),
                        mnesia:write(TabName, Chunk, write),
                        Chunk;
                    [Chunk] -> Chunk
                end
        end,
    {atomic, R} = mnesia:transaction(Q),
    R.

start_link(WorldName, Dimension) ->
    logger:info("WorldName: ~p, Dimension: ~p~n", [WorldName, Dimension]),
    gen_server:start_link({via, registry_srv, {?MODULE, WorldName, Dimension}}, ?MODULE, [WorldName, Dimension], []).

%% gen_server.

init([WorldName, Dimension]) ->
    erlang:process_flag(trap_exit, true),
    TabName = tabname(WorldName, Dimension),
    maybe_init_table(TabName),
    {ok, #state{table = TabName}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    {noreply, State};
handle_info(Info, State) ->
    lager:debug("Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal

maybe_init_table(TabName) ->
    case lists:member(TabName, mnesia:system_info(tables)) of
        true -> 
            ok;
        false ->
            {atomic, ok} = mnesia:create_table(TabName, [
                {attributes, record_info(fields, chunk)}, 
                {type, set}, 
                {disc_copies, [node()]},
                {record_name, chunk}
            ]),
            ok
    end.

generate_chunk(Pos = {X, Z}) ->
    lager:notice("generated chunk ~p~n", [Pos]),
    ChunkData = 
        #chunk_data{
            % full_column=true,
            layers=[
                chunk_layer:layer(0, blocks:bedrock()),
                chunk_layer:layer(1, blocks:stone()),
                chunk_layer:layer(2, blocks:dirt()),
                chunk_layer:layer(3, blocks:grass()),
                chunk_layer:layer_air(4)
            ],
            biome=binary:copy(<<0>>,256)
        },
    #chunk{
        position = Pos,
        data = ChunkData
    }.