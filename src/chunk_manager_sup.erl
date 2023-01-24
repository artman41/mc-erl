-module(chunk_manager_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

start_child(WorldName, Dimension) ->
    supervisor:start_child(?MODULE, [WorldName, Dimension]).

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
            id => chunk_manager_srv,
            start => {chunk_manager_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker
        }
    ],
    {ok, {SupFlags, Children}}.
