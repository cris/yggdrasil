-module(yggdrasil_rest_protocol).
%%====================================================================
%% REST protocol description
%%====================================================================
%% VERB RESOURCE HEADERS PARAMS\r\n
%%
%% VERB: \w[\w\d_]*, AUTH | PUT | DELETE | POST | GET | CAN_B33
%% RESOURCE: URI, /user/1/friends/3
%% HEADERS: id:NUMBER | headers:RESTHash
%% PARAMS: params:RESTHash
%% RESTHash: {NAME=STRING|DIGIT|true|false|null}
%% STRING: "true" | "false" | "null" | word | word\ with\ space | "WORDS"
%% WORDS: word+, \n | \t | \r | \\ | \" | \char -> char

-export([
        parse_request/1
    ]).

-record(state, {}).
-record(request, {
        verb,
        resource,
        headers,
        params
    }).

%-define(IS_WORD_CHAR(C), $A).

-define(IS_WORD_CHAR(C), ((C >= $A andalso C =< $Z) orelse (C >= $a andalso C =< $z) orelse (C =:= $_) orelse (C >= $0 andalso $ =< $9))).

-define(IS_NUMBER_CHAR(C), (C >= $0 andalso C =< $9)).

-define(IS_SPACE(C), (C =:= $\ )).

parse_request(Data) -> 
    {ok, Verb, Rest} = verb(Data),
    io:format("Verb is ~p", [Verb]),
    io:format("Rest is ~p", [Rest]).
    %#request{verb=Verb}.

verb(Data) ->
    verb(Data, <<>>).

verb(<<H, Rest/binary>>, Verb) -> 
    if
        ?IS_WORD_CHAR(H) ->
            verb(Rest, <<Verb/binary, H>>);
        ?IS_SPACE(H) andalso byte_size(Verb) =/= 0 ->
            {ok, Verb, Rest};
        true -> 
            {error, {"VERB: Unexpected symbol", H}}
    end.
%headers(Data) -> 
    %.
%headers(Data) -> 
    %.
%crnl(Data) ->
    %.

