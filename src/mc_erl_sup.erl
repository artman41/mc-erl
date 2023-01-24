-module(mc_erl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% World -> Dimension Sup -> [Dimension] -> Chunk Manager

init([]) ->
    SupFlags = #{
        strategy => one_for_one, 
        intensity => 1, 
        period => 5
    },
    Children = [
        #{
            id => stat_srv,
            start => {stat_srv, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker
        },
        #{
            id => chunk_manager_sup,
            start => {chunk_manager_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor
        },
        #{
            id => dimension_sup_sup,
            start => {dimension_sup_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor
        },
        #{
            id => world_sup,
            start => {world_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor
        },
        ranch:child_spec(client_acceptor, 100, ranch_tcp, [{port, server_properties:get_server_port()}], client_srv, [])
    ],
    {ok, {SupFlags, Children}}.
