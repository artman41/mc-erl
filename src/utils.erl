-module(utils).
-export([get_next_char/1]).

-spec get_next_char(iolist()) -> {non_neg_integer(), iolist()} | false.
get_next_char([]) ->
    false;
get_next_char(<<>>) ->
    false;
get_next_char([C | Tail]) when is_integer(C) ->
    {C, Tail};
get_next_char([B | Tail]) when is_binary(B) ->
    case get_next_char(B) of
        false ->
            get_next_char(Tail);
        {C, BinTail} ->
            {C, [BinTail | Tail]}
    end;
get_next_char(<<C:8, Tail/binary>>) ->
    {C, Tail}.