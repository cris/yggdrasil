-module(yggdrasil_json_protocol).
-author('cris.kiev@gmail.com').

%% The base idea is to use JSON for mirroring HTTP-like REST
%% interface. 
%% {"verb": "GET", "resource": "/server/users",
%%  "headers": {"id": 3}, "params": {"page": 3}}
%%
%% In "headers" you can put any header, like encoding or format.

-include("yggdrasil.hrl").
-include("routes.hrl").
-include("parser.hrl").

-export([
        encode/1,
        decode/1,
        decode_resource/1
    ]).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
encode(_T) -> ok.

decode(Data) when is_binary(Data) -> 
    Json = decode_json(Data), 
    Verb     = extract_verb(Json),
    Resource = extract_resource(Json),
    Headers  = extract_headers(Json),
    Params   = extract_params(Json),
    {ok, #request{verb=Verb, resource=Resource, headers=Headers, params=Params}}.

decode_resource(Bin) when is_binary(Bin) ->
    BinList = split_resource(Bin),
    _AtomList = prepare_resource(BinList).

%%%------------------------------------------------------------------------
%%% private 
%%%------------------------------------------------------------------------

%% split binary by string
split_resource(Bin) ->
    lists:reverse(split_resource(Bin, <<>>, [])).

split_resource(<<$/, Rest/binary>>, <<>>, []) ->
    split_resource(Rest, <<>>, []);

split_resource(<<$/, N, Rest/binary>>, Element, Parts) when ?IS_NUMBER_CHAR(N) ->
    {Number, NewRest} = split_resource_number(Rest, <<N>>),
    split_resource(NewRest, <<>>,
        [{Element, yggdrasil_utils:binary_to_integer(Number)} | Parts]);

split_resource(<<C, Rest/binary>>, Acc, Parts) when ?IS_LWORD_CHAR(C) ->
    split_resource(Rest, <<Acc/binary, C>>, Parts);

split_resource(<<$/, Rest/binary>>, Element, Parts) ->
    split_resource(Rest, <<>>, [Element | Parts]);

split_resource(<<$/>>, <<>>, Parts) -> 
    Parts;
split_resource(<<$/>>, Element, Parts) ->
    [Element | Parts];

split_resource(<<>>, <<>>, Parts) -> 
    Parts;
split_resource(<<>>, Element, Parts) -> 
    [Element | Parts].

% parse number part
split_resource_number(<<N, Rest/binary>>, Acc) when ?IS_NUMBER_CHAR(N) ->
    split_resource_number(Rest, <<Acc/binary, N>>);

split_resource_number(<<$/, Rest/binary>>, Acc) ->
    {Acc, Rest};
    
split_resource_number(<<>>, Acc) ->
    {Acc, <<>>};

split_resource_number(_Bin, _Acc) ->
    throw(incorrect_resource).

prepare_resource(BinList) when is_list(BinList) ->
    lists:map(
        fun({X, Number}) -> 
                {yggdrasil_utils:binary_to_atom(X), Number};
        (X) ->
                yggdrasil_utils:binary_to_atom(X)
        end,
        BinList
    ).

%% get verb
extract_verb(Json) when is_list(Json) ->
    case proplists:get_value(<<"verb">>, Json) of
        <<"GET">>    -> 'GET';
        <<"POST">>   -> 'POST';
        <<"PUT">>    -> 'PUT';
        <<"DELETE">> -> 'DELETE';
        <<"AUTH">>   -> 'AUTH';
        _            -> throw(incorrect_verb)
    end.

extract_resource(Json) when is_list(Json) ->
    case proplists:get_value(<<"resource">>, Json) of
        undefined -> throw(incorrect_resource);
        Value     -> decode_resource(Value)
    end.

extract_headers(Json) when is_list(Json) ->
    case proplists:get_value(<<"headers">>, Json) of
        [{_K,_V} | _T] = KVList -> KVList;
        undefined               -> [{}];
        _                       -> throw(incorrect_headers)
    end.

extract_params(Json) when is_list(Json) -> 
    case proplists:get_value(<<"params">>, Json) of
        [{_K,_V} | _T] = KVList -> KVList;
        undefined               -> [{}];
        _                       -> throw(incorrect_params)
    end.


decode_json(Data) -> 
    normalize_mochiweb_json(mochijson2:decode(Data)).

normalize_mochiweb_json(V) when is_binary(V); is_number(V); is_atom(V) -> V;

normalize_mochiweb_json({struct, []}) -> [{}];
normalize_mochiweb_json({struct, KVList}) ->
    normalize_mochiweb_json(KVList);

normalize_mochiweb_json([{K,V}|TList]) ->
    [{K,normalize_mochiweb_json(V)} | normalize_mochiweb_json(TList)];

normalize_mochiweb_json([]) -> [];
normalize_mochiweb_json([H|TList]) -> 
    [normalize_mochiweb_json(H) | normalize_mochiweb_json(TList)].



