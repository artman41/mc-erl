-module(stat_srv).
-behaviour(gen_server).

%% API.
-export([get_player_count/0]).
-export([increment/2, decrement/2]).
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, ?MODULE).
-define(TAB, ?MODULE).

-record(state, {
}).

-spec get_player_count() -> integer().
get_player_count() ->
    get_value(player_count, -1).

increment(Stat, Value) ->
    gen_server:call(?SERVER, {increment, Stat, Value}).

decrement(Stat, Value) ->
    gen_server:call(?SERVER, {decrement, Stat, Value}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server.

init([]) ->
    ets:new(?TAB, [protected, named_table]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
get_value(Key) ->
    case ets:lookup(?TAB, Key) of
        [] -> 
            undefined;
        [{Key, Value}] ->
            {ok, Value}
    end.

get_value(Key, Default) ->
    case get_value(Key) of
        undefined -> 
            Default;
        {ok, Value} -> 
            Value
    end.