-module(mc_erl_app).
-behaviour(application).

-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	server_properties:module_info(),
	ensure_mnesia_schema(),
	{PrivateKey, PublicKey} = generate_crypto(),
	application:set_env(application:get_application(), crypto_private_rsa, PrivateKey),
	application:set_env(application:get_application(), crypto_public_rsa, PublicKey),
    case mc_erl_sup:start_link() of
		Ok = {ok, _} ->
			world_sup:start_child("world"),
			Ok;
		Err ->
			Err
	end.

stop(_State) ->
    ok.

generate_crypto() ->
    {ok, PrivateKey} = cutkey:rsa(1024, 65537, [{return, key}]),
    #'RSAPrivateKey'{modulus=Modulus, publicExponent=PublicExponent} = PrivateKey,
    {'SubjectPublicKeyInfo', PublicKey, not_encrypted} = public_key:pem_entry_encode('SubjectPublicKeyInfo', #'RSAPublicKey'{modulus=Modulus, publicExponent=PublicExponent}),
	{PrivateKey, PublicKey}.

ensure_mnesia_schema() ->
	case lists:keyfind(storage_type, 1, mnesia_schema:get_table_properties(schema)) of
		{storage_type, disc_copies} ->
			ok;
		_ ->
			application:stop(mnesia),
			mnesia:create_schema([node()]),
			application:start(mnesia),
			ok
	end.