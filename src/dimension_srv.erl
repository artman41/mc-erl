-module(dimension_srv).
-behaviour(gen_server).

%% API.
-export([start_link/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    chunk_manager_srv :: pid(),
    players :: list(iolist())
}).

%% API.

start_link(WorldName, Dimension) ->
    logger:info("WorldName: ~p, Dimension: ~p~n", [WorldName, Dimension]),
    gen_server:start_link({via, registry_srv, {?MODULE, WorldName, Dimension}}, ?MODULE, [WorldName, Dimension], []).

%% gen_server.

init([WorldName, Dimension]) ->
    erlang:process_flag(trap_exit, true),
    {ok, ChunkManagerSrv} = chunk_manager_sup:start_child(WorldName, Dimension),
    link(ChunkManagerSrv),
    {ok, #state{
        chunk_manager_srv = ChunkManagerSrv,
        players = []
    }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    State#state.chunk_manager_srv ! tick,
    {noreply, State};
handle_info(Info, State) ->
    lager:debug("Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
