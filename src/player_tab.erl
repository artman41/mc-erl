-module(player_tab).

-include("player.hrl").

-export([start_link/0]).
-export([init/0]).
-export([insert/1, lookup/1, lookup_by_element/2]).

-define(TAB, ?MODULE).

insert(Player) when is_record(Player, player) ->
    ets:insert(?TAB, Player).

lookup(Key) ->
    case ets:lookup(?TAB, Key) of
        [] -> 
            {error, notfound};
        [Elem] ->
            {ok, Elem}
    end.

    lookup_by_element(Index, Key) ->
        case ets:select(?TAB, [{'$1',[{'=:=',{element, Index, '$1'},Key}],['$1']}]) of
            [] -> 
                {error, notfound};
            [Elem] ->
                {ok, Elem}
        end.

start_link() ->
    proc_lib:start_link(?MODULE, init, []).

init() ->
    ets:new(?TAB, [public, named_table, {keypos, #player.pid}]),
    proc_lib:init_ack({ok, self()}),
    loop().

%% internal

loop() ->
    receive
    after infinity ->
        ok
    end.
