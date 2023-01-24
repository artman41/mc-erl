-module(config).

-export([
    get_dimensions/0,
    get_tick_rate/0,
    get_crypto_private_rsa/0,
    get_crypto_public_rsa/0
]).

get_dimensions() ->
    get_value(dimensions, [overworld, nether, the_end]).

get_tick_rate() ->
    get_value(tick_rate, 20).

get_crypto_private_rsa() ->
    case get_value(crypto_private_rsa) of
        {ok, V} -> V;
        undefined -> erlang:error(notfound)
    end.

get_crypto_public_rsa() ->
    case get_value(crypto_public_rsa) of
        {ok, V} -> V;
        undefined -> erlang:error(notfound)
    end.

%% internal

get_value(Key) ->
    application:get_env(mc_erl, Key).

get_value(Key, Default) ->
    case get_value(Key) of
        undefined -> Default;
        {ok, V} -> V
    end.
