PROJECT = mc_erl

DEPS += lager
DEPS += cutkey
DEPS += cheatcraft

LOCAL_DEPS += mnesia

dep_lager = git https://github.com/erlang-lager/lager.git master
dep_cutkey = git git://github.com/brverbur/cutkey.git 599eb463cd5f6f024e182d4bca11f4189d0cf6c9
dep_cheatcraft = git git://github.com/lordnull/CheatCraft.git 4d6dcf881218abce0c69cf07b184127c6a1dfd7e

ERLC_OPTS += +debug_info +'{parse_transform, lager_transform}'

include erlang.mk

SHELL_OPTS += -config apps.config
SHELL_OPTS += -eval 'application:ensure_all_started(mc_erl)'

define copy_function.erl
	{ok, Bin} = file:read_file("$(2)"),
	[_, Match, _] = re:split(Bin, "($(1)(?:.*)\s*->.*\n(?:.*,\n)*.*\.\n)"),
	{ok, Tokens, _} = erl_scan:string(binary_to_list(Match)),
	{ok, FunAST} = erl_parse:parse(Tokens),
	FunArity = length(element(3, _Clause = hd(element(5, FunAST)))),
	{ok, Replace} = file:read_file("$(3)"),
	{Start, Length} = binary:match(Replace, <<"-export">>),
	<<Left:Start/binary, Right/binary>> = Replace,
	StitchedData0 = [Left, "\n-export([$(1)", "/", integer_to_list(FunArity), "]).\n", Right],
	StitchedData1 = [StitchedData0, "\n", Match],
	file:write_file("$(3)", StitchedData1),
	halt().
endef

autopatch-cutkey::
	sed '22s|erl_interface\.h|ei.h|g' -i $(DEPS_DIR)/cutkey/c_src/cutkey.c
	sed 's|rsa->\([a-z0-9]*\),|RSA_get0_\1(rsa),|g' -i $(DEPS_DIR)/cutkey/c_src/cutkey.c
	sed 's|\([a-z0-9A-Z_\.]*\)\s*==\s*ERL_TUPLE|(\1 == ERL_SMALL_TUPLE_EXT \|\| \1 == ERL_LARGE_TUPLE_EXT)|g' -i $(DEPS_DIR)/cutkey/c_src/cutkey.c
	$(MAKE) -f $(CURDIR)/erlang.mk -C $(DEPS_DIR)/cutkey new t=module n=crypto_helper
	$(call erlang,$(call copy_function.erl,erlint,/usr/local/erl_rel/25.2/lib/crypto-5.1.2/src/crypto.erl,$(DEPS_DIR)/cutkey/src/crypto_helper.erl))
	sed 's|crypto:erlint|crypto_helper:erlint|g' -i $(DEPS_DIR)/cutkey/src/cutkey.erl
		

autopatch-cheatcraft::
	sed 's|{mod,{cheatcraft,\[\]}},||g' -i $(DEPS_DIR)/cheatcraft/src/cheatcraft.app.src