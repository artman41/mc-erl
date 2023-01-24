-module(world_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

start_child(WorldName) ->
    supervisor:start_child(?MODULE, [WorldName]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        #{
            id => world_srv,
            start => {world_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker
        }
    ],
    {ok, {SupFlags, Children}}.
