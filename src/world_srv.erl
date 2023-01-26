-module(world_srv).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    dimension_sup :: pid(),
    tick_ref :: reference()
}).

%% API.

start_link(WorldName) ->
    logger:info("WorldName: ~p~n", [WorldName]),
    gen_server:start_link({via, registry_srv, {?MODULE, WorldName}}, ?MODULE, [WorldName], []).

%% gen_server.

init([WorldName]) ->
    erlang:process_flag(trap_exit, true),
    {ok, DimensionSup} = dimension_sup_sup:start_child(WorldName),
    link(DimensionSup),
    {ok, #state{
        dimension_sup = DimensionSup,
        tick_ref = tick()
    }}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    [Pid ! tick || {_Id, Pid, _Type, _Modules} <- supervisor:which_children(State#state.dimension_sup)],
    {noreply, State#state{tick_ref = tick()}};
handle_info(Info, State) ->
    lager:debug("Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
tick() ->
    erlang:send_after(config:get_tick_rate(), self(), tick).