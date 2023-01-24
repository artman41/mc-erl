-module(dimension_sup).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(WorldName, Dimensions) ->
    supervisor:start_link(?MODULE, [WorldName, Dimensions]).

init([WorldName, Dimensions]) ->
    SupFlags = #{
        strategy => one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        #{
            id => {dimension_srv, Dimension},
            start => {dimension_srv, start_link, [WorldName, Dimension]},
            restart => permanent,
            shutdown => 2000,
            type => worker
        }
    || Dimension <- Dimensions],
    {ok, {SupFlags, Children}}.
