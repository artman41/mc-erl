-module(server_properties).

-on_load(on_load/0).

-export([
    get_allow_flight/0,
    get_allow_nether/0,
    get_broadcast_console_to_ops/0,
    get_broadcast_rcon_to_ops/0,
    get_difficulty/0,
    get_enable_command_block/0,
    get_enable_jmx_monitoring/0,
    get_enable_query/0,
    get_enable_rcon/0,
    get_enable_status/0,
    get_enforce_secure_profile/0,
    get_enforce_whitelist/0,
    get_entity_broadcast_range_percentage/0,
    get_force_gamemode/0,
    get_function_permission_level/0,
    get_gamemode/0,
    get_generate_structures/0,
    get_generator_settings/0,
    get_hardcore/0,
    get_hide_online_players/0,
    get_initial_disabled_packs/0,
    get_initial_enabled_packs/0,
    get_level_name/0,
    get_level_seed/0,
    get_level_type/0,
    get_max_chained_neighbor_updates/0,
    get_max_players/0,
    get_max_tick_time/0,
    get_max_world_size/0,
    get_motd/0,
    get_network_compression_threshold/0,
    get_online_mode/0,
    get_op_permission_level/0,
    get_player_idle_timeout/0,
    get_prevent_proxy_connections/0,
    get_pvp/0,
    get_query_port/0,
    get_rate_limit/0,
    get_rcon_password/0,
    get_rcon_port/0,
    get_require_resource_pack/0,
    get_resource_pack/0,
    get_resource_pack_prompt/0,
    get_resource_pack_sha1/0,
    get_server_ip/0,
    get_server_port/0,
    get_simulation_distance/0,
    get_spawn_animals/0,
    get_spawn_monsters/0,
    get_spawn_npcs/0,
    get_spawn_protection/0,
    get_sync_chunk_writes/0,
    get_text_filtering_config/0,
    get_use_native_transport/0,
    get_view_distance/0,
    has_whitelist/0
]).

-define(TAB, ?MODULE).
-define(PROPERTIES_FILE, "server.properties").

-spec get_allow_flight() -> boolean().
get_allow_flight() ->
    {ok, Value} = get_value(<<"allow-flight">>),
    Value =:= <<"true">>.

-spec get_allow_nether() -> boolean().
get_allow_nether() ->
    {ok, Value} = get_value(<<"allow-nether">>),
    Value =:= <<"true">>.

-spec get_broadcast_console_to_ops() -> boolean().
get_broadcast_console_to_ops() ->
    {ok, Value} = get_value(<<"broadcast-console-to-ops">>),
    Value =:= <<"true">>.

-spec get_broadcast_rcon_to_ops() -> boolean().
get_broadcast_rcon_to_ops() ->
    {ok, Value} = get_value(<<"broadcast-rcon-to-ops">>),
    Value =:= <<"true">>.

-spec get_difficulty() -> binary().
get_difficulty() ->
    {ok, Value} = get_value(<<"difficulty">>),
    Value.

-spec get_enable_command_block() -> boolean().
get_enable_command_block() ->
    {ok, Value} = get_value(<<"enable-command-block">>),
    Value =:= <<"true">>.

-spec get_enable_jmx_monitoring() -> boolean().
get_enable_jmx_monitoring() ->
    {ok, Value} = get_value(<<"enable-jmx-monitoring">>),
    Value =:= <<"true">>.

-spec get_enable_query() -> boolean().
get_enable_query() ->
    {ok, Value} = get_value(<<"enable-query">>),
    Value =:= <<"true">>.

-spec get_enable_rcon() -> boolean().
get_enable_rcon() ->
    {ok, Value} = get_value(<<"enable-rcon">>),
    Value =:= <<"true">>.

-spec get_enable_status() -> boolean().
get_enable_status() ->
    {ok, Value} = get_value(<<"enable-status">>),
    Value =:= <<"true">>.

-spec get_enforce_secure_profile() -> boolean().
get_enforce_secure_profile() ->
    {ok, Value} = get_value(<<"enforce-secure-profile">>),
    Value =:= <<"true">>.

-spec get_enforce_whitelist() -> boolean().
get_enforce_whitelist() ->
    {ok, Value} = get_value(<<"enforce-whitelist">>),
    Value =:= <<"true">>.

-spec get_entity_broadcast_range_percentage() -> integer().
get_entity_broadcast_range_percentage() ->
    {ok, Value} = get_value(<<"entity-broadcast-range-percentage">>),
    erlang:binary_to_integer(Value).

-spec get_force_gamemode() -> boolean().
get_force_gamemode() ->
    {ok, Value} = get_value(<<"force-gamemode">>),
    Value =:= <<"true">>.

-spec get_function_permission_level() -> integer().
get_function_permission_level() ->
    {ok, Value} = get_value(<<"function-permission-level">>),
    erlang:binary_to_integer(Value).

-spec get_gamemode() -> binary().
get_gamemode() ->
    {ok, Value} = get_value(<<"gamemode">>),
    Value.

-spec get_generate_structures() -> boolean().
get_generate_structures() ->
    {ok, Value} = get_value(<<"generate-structures">>),
    Value =:= <<"true">>.

-spec get_generator_settings() -> binary().
get_generator_settings() ->
    {ok, Value} = get_value(<<"generator-settings">>),
    Value.

-spec get_hardcore() -> boolean().
get_hardcore() ->
    {ok, Value} = get_value(<<"hardcore">>),
    Value =:= <<"true">>.

-spec get_hide_online_players() -> boolean().
get_hide_online_players() ->
    {ok, Value} = get_value(<<"hide-online-players">>),
    Value =:= <<"true">>.

-spec get_initial_disabled_packs() -> binary().
get_initial_disabled_packs() ->
    {ok, Value} = get_value(<<"initial-disabled-packs">>),
    Value.

-spec get_initial_enabled_packs() -> binary().
get_initial_enabled_packs() ->
    {ok, Value} = get_value(<<"initial-enabled-packs">>),
    Value.

-spec get_level_name() -> binary().
get_level_name() ->
    {ok, Value} = get_value(<<"level-name">>),
    Value.

-spec get_level_seed() -> binary().
get_level_seed() ->
    {ok, Value} = get_value(<<"level-seed">>),
    Value.

-spec get_level_type() -> binary().
get_level_type() ->
    {ok, Value} = get_value(<<"level-type">>),
    Value.

-spec get_max_chained_neighbor_updates() -> integer().
get_max_chained_neighbor_updates() ->
    {ok, Value} = get_value(<<"max-chained-neighbor-updates">>),
    erlang:binary_to_integer(Value).

-spec get_max_players() -> integer().
get_max_players() ->
    {ok, Value} = get_value(<<"max-players">>),
    erlang:binary_to_integer(Value).

-spec get_max_tick_time() -> integer().
get_max_tick_time() ->
    {ok, Value} = get_value(<<"max-tick-time">>),
    erlang:binary_to_integer(Value).

-spec get_max_world_size() -> integer().
get_max_world_size() ->
    {ok, Value} = get_value(<<"max-world-size">>),
    erlang:binary_to_integer(Value).

-spec get_motd() -> binary().
get_motd() ->
    {ok, Value} = get_value(<<"motd">>),
    Value.

-spec get_network_compression_threshold() -> integer().
get_network_compression_threshold() ->
    {ok, Value} = get_value(<<"network-compression-threshold">>),
    erlang:binary_to_integer(Value).

-spec get_online_mode() -> boolean().
get_online_mode() ->
    {ok, Value} = get_value(<<"online-mode">>),
    Value =:= <<"true">>.

-spec get_op_permission_level() -> integer().
get_op_permission_level() ->
    {ok, Value} = get_value(<<"op-permission-level">>),
    erlang:binary_to_integer(Value).

-spec get_player_idle_timeout() -> integer().
get_player_idle_timeout() ->
    {ok, Value} = get_value(<<"player-idle-timeout">>),
    erlang:binary_to_integer(Value).

-spec get_prevent_proxy_connections() -> boolean().
get_prevent_proxy_connections() ->
    {ok, Value} = get_value(<<"prevent-proxy-connections">>),
    Value =:= <<"true">>.

-spec get_pvp() -> boolean().
get_pvp() ->
    {ok, Value} = get_value(<<"pvp">>),
    Value =:= <<"true">>.

-spec get_query_port() -> integer().
get_query_port() ->
    {ok, Value} = get_value(<<"query.port">>),
    erlang:binary_to_integer(Value).

-spec get_rate_limit() -> integer().
get_rate_limit() ->
    {ok, Value} = get_value(<<"rate-limit">>),
    erlang:binary_to_integer(Value).

-spec get_rcon_password() -> binary().
get_rcon_password() ->
    {ok, Value} = get_value(<<"rcon.password">>),
    Value.

-spec get_rcon_port() -> integer().
get_rcon_port() ->
    {ok, Value} = get_value(<<"rcon.port">>),
    erlang:binary_to_integer(Value).

-spec get_require_resource_pack() -> boolean().
get_require_resource_pack() ->
    {ok, Value} = get_value(<<"require-resource-pack">>),
    Value =:= <<"true">>.

-spec get_resource_pack() -> binary().
get_resource_pack() ->
    {ok, Value} = get_value(<<"resource-pack">>),
    Value.

-spec get_resource_pack_prompt() -> binary().
get_resource_pack_prompt() ->
    {ok, Value} = get_value(<<"resource-pack-prompt">>),
    Value.

-spec get_resource_pack_sha1() -> binary().
get_resource_pack_sha1() ->
    {ok, Value} = get_value(<<"resource-pack-sha1">>),
    Value.

-spec get_server_ip() -> binary().
get_server_ip() ->
    {ok, Value} = get_value(<<"server-ip">>),
    Value.

-spec get_server_port() -> integer().
get_server_port() ->
    {ok, Value} = get_value(<<"server-port">>),
    erlang:binary_to_integer(Value).

-spec get_simulation_distance() -> integer().
get_simulation_distance() ->
    {ok, Value} = get_value(<<"simulation-distance">>),
    erlang:binary_to_integer(Value).

-spec get_spawn_animals() -> boolean().
get_spawn_animals() ->
    {ok, Value} = get_value(<<"spawn-animals">>),
    Value =:= <<"true">>.

-spec get_spawn_monsters() -> boolean().
get_spawn_monsters() ->
    {ok, Value} = get_value(<<"spawn-monsters">>),
    Value =:= <<"true">>.

-spec get_spawn_npcs() -> boolean().
get_spawn_npcs() ->
    {ok, Value} = get_value(<<"spawn-npcs">>),
    Value =:= <<"true">>.

-spec get_spawn_protection() -> integer().
get_spawn_protection() ->
    {ok, Value} = get_value(<<"spawn-protection">>),
    erlang:binary_to_integer(Value).

-spec get_sync_chunk_writes() -> boolean().
get_sync_chunk_writes() ->
    {ok, Value} = get_value(<<"sync-chunk-writes">>),
    Value =:= <<"true">>.

-spec get_text_filtering_config() -> binary().
get_text_filtering_config() ->
    {ok, Value} = get_value(<<"text-filtering-config">>),
    Value.

-spec get_use_native_transport() -> boolean().
get_use_native_transport() ->
    {ok, Value} = get_value(<<"use-native-transport">>),
    Value =:= <<"true">>.

-spec get_view_distance() -> integer().
get_view_distance() ->
    {ok, Value} = get_value(<<"view-distance">>),
    erlang:binary_to_integer(Value).

-spec has_whitelist() -> boolean().
has_whitelist() ->
    {ok, Value} = get_value(<<"white-list">>),
    Value =:= <<"true">>.

%% INTERNAL

get_value(Key) ->
    case ets:lookup(?TAB, Key) of
        [{Key, Value}] ->
            {ok, Value};
        [] ->
            undefined
    end.

get_value(Key, Default) ->
    case get_value(Key) of
        {ok, V} -> V;
        undefined -> Default
    end.

on_load() ->
    ets:new(?TAB, [public, named_table, {heir, whereis(application_controller), undefined}]),
    case read(?PROPERTIES_FILE) of
        {ok, KeyValues} ->
            ets:insert(?TAB, KeyValues);
        {error, nofile} ->
            case write(?PROPERTIES_FILE, defaults()) of
                ok -> ok;
                Err = {error, _} ->
                    lager:error("Failed to write default server properties with error ~p~n", [Err]),
                    Err
            end,
            ets:insert(?TAB, defaults());
        {error, Reason} ->
            {current_stacktrace, ST} = process_info(self(), current_stacktrace),
            erlang:raise(error, Reason, ST)
    end,
    ok.

defaults() ->
    [
        {<<"enable-jmx-monitoring">>, <<"false">>},
        {<<"rcon.port">>, <<"25575">>},
        {<<"level-seed">>, <<"">>},
        {<<"gamemode">>, <<"survival">>},
        {<<"enable-command-block">>, <<"false">>},
        {<<"enable-query">>, <<"false">>},
        {<<"generator-settings">>, <<"{}">>},
        {<<"enforce-secure-profile">>, <<"true">>},
        {<<"level-name">>, <<"world">>},
        {<<"motd">>, <<"A Minecraft Server">>},
        {<<"query.port">>, <<"25565">>},
        {<<"pvp">>, <<"true">>},
        {<<"generate-structures">>, <<"true">>},
        {<<"max-chained-neighbor-updates">>, <<"1000000">>},
        {<<"difficulty">>, <<"easy">>},
        {<<"network-compression-threshold">>, <<"256">>},
        {<<"max-tick-time">>, <<"60000">>},
        {<<"require-resource-pack">>, <<"false">>},
        {<<"use-native-transport">>, <<"true">>},
        {<<"max-players">>, <<"20">>},
        {<<"online-mode">>, <<"true">>},
        {<<"enable-status">>, <<"true">>},
        {<<"allow-flight">>, <<"false">>},
        {<<"initial-disabled-packs">>, <<"">>},
        {<<"broadcast-rcon-to-ops">>, <<"true">>},
        {<<"view-distance">>, <<"10">>},
        {<<"server-ip">>, <<"">>},
        {<<"resource-pack-prompt">>, <<"">>},
        {<<"allow-nether">>, <<"true">>},
        {<<"server-port">>, <<"25565">>},
        {<<"enable-rcon">>, <<"false">>},
        {<<"sync-chunk-writes">>, <<"true">>},
        {<<"op-permission-level">>, <<"4">>},
        {<<"prevent-proxy-connections">>, <<"false">>},
        {<<"hide-online-players">>, <<"false">>},
        {<<"resource-pack">>, <<"">>},
        {<<"entity-broadcast-range-percentage">>, <<"100">>},
        {<<"simulation-distance">>, <<"10">>},
        {<<"rcon.password">>, <<"">>},
        {<<"player-idle-timeout">>, <<"0">>},
        {<<"force-gamemode">>, <<"false">>},
        {<<"rate-limit">>, <<"0">>},
        {<<"hardcore">>, <<"false">>},
        {<<"white-list">>, <<"false">>},
        {<<"broadcast-console-to-ops">>, <<"true">>},
        {<<"spawn-npcs">>, <<"true">>},
        {<<"spawn-animals">>, <<"true">>},
        {<<"function-permission-level">>, <<"2">>},
        {<<"initial-enabled-packs">>, <<"vanilla">>},
        {<<"level-type">>, <<"minecraft:normal">>},
        {<<"text-filtering-config">>, <<"">>},
        {<<"spawn-monsters">>, <<"true">>},
        {<<"enforce-whitelist">>, <<"false">>},
        {<<"spawn-protection">>, <<"16">>},
        {<<"resource-pack-sha1">>, <<"">>},
        {<<"max-world-size">>, <<"29999984">>}
    ].

read(File) ->
    case file:open(File, [read, binary]) of
        {ok, Dev} ->
            case read_(Dev, []) of
                {ok, KeyValues} ->
                    {ok, lists:keymerge(1, KeyValues, defaults())};
                Err = {error, _} ->
                    Err
            end;
        {error, enoent} ->
            {error, nofile};
        Err = {error, _} ->
            Err
    end.

write(File, KeyValues) ->
    {ok, Dev} = file:open(File, [write]),
        {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(os:timestamp()),
        file:write(Dev, [
            "#Minecraft server properties", $\n,
            io_lib:format("#~b:~b:~b ~b/~b/~b~n", [Hour, Minute, Second, Day, Month, Year])
        ]),
    [file:write(Dev, io_lib:format("~s=~s~n", [K, V])) || {K, V} <- lists:keymerge(1, KeyValues, defaults())],
    file:close(Dev).

%% Internal

read_(Dev, Acc0) ->
    case file:read_line(Dev) of
        {ok, <<$#, _/binary>>} ->
            read_(Dev, Acc0);
        {ok, Line} ->
            Acc1 = 
                case parse_line(Line, {key, <<>>, <<>>}) of
                    {ok, KV} ->
                        [KV|Acc0];
                    Err = {error, _} ->
                        lager:warning("Skipping line ~p with error ~p~n", [Line, Err]),
                        Acc0
                end,
            read_(Dev, Acc1);
        eof ->
            {ok, Acc0};
        Err = {error, _} ->
            Err 
    end.

parse_line(<<>>, {Side, KeyAcc, ValueAcc}) ->
    case Side of
        key ->
            {error, badformat};
        value ->
            {ok, {KeyAcc, ValueAcc}}
    end;
parse_line(<<$\n>>, {Side, KeyAcc, ValueAcc}) ->
    case Side of
        key ->
            {error, badformat};
        value ->
            {ok, {KeyAcc, ValueAcc}}
    end;
parse_line(<<$#, _/binary>>, {Side, KeyAcc, ValueAcc}) ->
    case Side of
        key ->
            {error, badformat};
        value ->
            {ok, {KeyAcc, ValueAcc}}
    end;
parse_line(<<$=, Tail/binary>>, {key, KeyAcc, ValueAcc}) ->
    parse_line(Tail, {value, KeyAcc, ValueAcc});
parse_line(<<C, Tail/binary>>, {Side, KeyAcc, ValueAcc}) ->
    case Side of
        key ->
            parse_line(Tail, {key, <<KeyAcc/binary, C>>, ValueAcc});
        value ->
            parse_line(Tail, {value, KeyAcc, <<ValueAcc/binary, C>>})
    end.