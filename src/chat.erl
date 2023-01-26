-module(chat).
-export([
    allowed_chars/0,
    is_valid_nickname/1
]).

-spec allowed_chars() -> list(non_neg_integer()).
allowed_chars() ->
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "[\\]^_abcdefghijklmnopqrstuvwxyz{|}~⌂ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ"
    "ø£Ø×ƒáíóúñÑªº¿®¬½¼¡«»".

is_valid_nickname(Name) ->
    case utils:get_next_char(Name) of
        false ->
            true;
        {C, Tail} ->
            lists:member(C, allowed_chars()) andalso is_valid_nickname(Tail)
    end.

% broadcast(World, Dimension, Msg) ->
