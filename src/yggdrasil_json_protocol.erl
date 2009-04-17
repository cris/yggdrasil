-module(yggdrasil_json_protocol).
-author('cris.kiev@gmail.com').

%% The base idea is to use JSON for mirroring HTTP-like REST
%% interface. 
%% {"verb": "GET", "resource": "/server/users",
%%  "headers": {"id": 3}, "params": {"page": 3}}
%%
%% In "headers" you can put any header, like encoding or format.

-include("yggdrasil.hrl").

-export([
        encode/1,
        decode/1
    ]).


decode(Data) when is_binary(Data) -> 
    Json = decode_json(Data), 
    Verb     = extract_verb(Json),
    Resource = extract_resource(Json),
    Headers  = extract_headers(Json),
    Params   = extract_params(Json),
    {ok, #request{verb=Verb, resource=Resource, headers=Headers, params=Params}}.

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
        Value     -> Value
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

encode(_T) -> ok.

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



