-module(yggdrasil_utils).
-author('cris.kiev@gmail.com').

-export([
        get/2,
        reply_ok/0,
        reply_ok/1,
        reply_error/0,
        reply_error/1,
        binary_to_atom/1,
        bin_to_atom/1,
        bin_to_integer/1,
        binary_to_integer/1,
        atom_to_bin/1,
        atom_to_binary/1,
        test_bin2int/0,
        test_bin2int/1,
        test_bin2atom/0,
        test_bin2atom/1,
        test_atom2bin/0,
        test_atom2bin/1,
        test_binary_to_atom/1,
        test_bin_to_atom/1,
        test_binary_to_integer/1,
        test_bin_to_integer/1,
        test_list/2
    ]).

get(Key, PropList) when is_atom(Key) -> 
    BKey = atom_to_binary(Key),
    proplists:get_value(BKey, PropList);

get(Keys, PropList) when is_list(Keys) ->
    RList = [proplists:get_value(atom_to_binary(K), PropList) || K <- Keys],
    list_to_tuple(RList).

reply_ok() ->
    reply_ok([]).

reply_ok(Params) when is_list(Params) ->
    reply(200, Params).

reply_error() ->
    reply(400, []).

reply_error(Params) when is_list(Params) ->
    reply(400, Params).

reply(Code, Params) when is_integer(Code), is_list(Params) ->
    mochijson2:encode({struct, [
                {type, reply},
                {code, Code},
                {params, {struct, Params}}
            ]}).

atom_to_binary(Atom) when is_atom(Atom) ->
    <<_:4/binary, Bin/binary>> = term_to_binary(Atom),
    Bin.

atom_to_bin(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).

binary_to_atom(Bin) when is_binary(Bin) ->
    Size = byte_size(Bin),
    binary_to_term(<<131, 100, 0, Size, Bin/binary>>).

bin_to_atom(Bin) ->
    list_to_atom(binary_to_list(Bin)).

bin_to_integer(Bin) when is_binary(Bin) -> 
    bin_to_integer(Bin, 0).

bin_to_integer(<<N, Rest/binary>>, Acc) ->
    bin_to_integer(Rest, Acc * 10 + (N - $0));

bin_to_integer(<<>>, Number) ->
    Number.

binary_to_integer(Bin) when is_binary(Bin) -> 
    list_to_integer(binary_to_list(Bin)).

test_bin2atom() -> 
    test_bin2int(100000).
test_bin2atom(N) -> 
    %% my realisation
    BAtomList2 = generate_binary_list($b, 12345678901234, N),
    {TNative, _R} = timer:tc(?MODULE, test_bin_to_atom, [BAtomList2]),
    BAtomList1 = generate_binary_list($a, 12345678901234, N),
    {TMy, _R}     = timer:tc(?MODULE, test_binary_to_atom, [BAtomList1]),
    io:format("My: binary_to_atom: ~8wµs ~n", [TMy]),
    io:format("Na: bin_to_atom:    ~8wµs ~n", [TNative]).

test_bin2int() -> 
    test_bin2int(100000).
test_bin2int(N) -> 
    %% my realisation
    BinList = generate_binary_list(123456, N),
    {TMy, _R}     = timer:tc(?MODULE, test_bin_to_integer, [BinList]),
    {TNative, _R} = timer:tc(?MODULE, test_binary_to_integer, [BinList]),
    io:format("Na: binary_to_integer: ~8wµs ~n", [TMy]),
    io:format("My: bin_to_integer:    ~8wµs ~n", [TNative]).

test_binary_to_integer(BinList) when is_list(BinList) ->
    lists:foreach(
        fun binary_to_integer/1,
        BinList).

test_bin_to_integer(BinList) when is_list(BinList) ->
    lists:foreach(
        fun bin_to_integer/1,
        BinList).

test_bin_to_atom(BinList) when is_list(BinList) ->
    lists:foreach(
        fun bin_to_atom/1,
        BinList).

test_binary_to_atom(BinList) when is_list(BinList) ->
    lists:foreach(
        fun binary_to_atom/1,
        BinList).

generate_binary_list(Prefix, Start, Num) ->
    [<<Prefix, (list_to_binary(integer_to_list(N)))/binary>> || N <- lists:seq(Start, Start + Num-1)].

generate_binary_list(Start, Num) ->
    [list_to_binary(integer_to_list(N)) || N <- lists:seq(Start, Start + Num-1)].

test_atom2bin() -> 
    test_atom2bin(10000).
test_atom2bin(N) -> 
    %% my realisation
    BinList = generate_atom_list(N),
    {TMy, _R}     = timer:tc(?MODULE, test_list, [atom_to_binary, BinList]),
    {TNative, _R} = timer:tc(?MODULE, test_list, [atom_to_bin, BinList]),
    io:format("My: atom_to_binary:  ~8wµs ~n", [TMy]),
    io:format("Native: atom_to_bin: ~8wµs ~n", [TNative]).


test_list(AtomFun, List) when is_list(List) andalso is_atom(AtomFun) ->
    lists:foreach(
        fun(X) -> ?MODULE:AtomFun(X) end,
        List).

generate_atom_list(Num) ->
    [binary_to_atom(Bin) || Bin <- generate_binary_list($a, 12345678, Num)].

