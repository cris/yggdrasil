-module(yggdrasil_utils).
-author('cris.kiev@gmail.com').

-export([
        get/2,
        binary_to_atom/1
    ]).

get(Key, PropList) when is_atom(Key) -> 
    BKey = atom_to_binary(Key),
    proplists:get_value(BKey, PropList);

get(Keys, PropList) when is_list(Keys) ->
    RList = [proplists:get_value(atom_to_binary(K), PropList) || K <- Keys],
    list_to_tuple(RList).


atom_to_binary(Atom) ->
    <<_:4/binary, Bin/binary>> = term_to_binary(Atom),
    Bin.

binary_to_atom(Bin) when is_binary(Bin) ->
    Size = byte_size(Bin),
    binary_to_term(<<131, 100, 0, Size, Bin/binary>>).
