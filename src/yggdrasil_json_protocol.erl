-module(yggdrasil_json_protocol).
-author('cris.kiev@gmail.com').

%% The base idea is to use JSON for mirroring HTTP-like REST
%% interface. 
%% {"verb": "GET", "resource": "/server/users",
%%  "headers": {"id": 3}, "params": {"page": 3}}
%%
%% In "headers" you can put any header, like encoding or format.

-export([
		encode/1,
		decode/1
	]).

decode(Data) when is_binary(Data) -> 
	{struct, List} = mochijson2:decode(Data),
	Verb = extract_verb(List),
	Resource = extract_resource(List),
	{ok, Headers} = extract_headers(List),
	{ok, Params} = extract_params(List),
	{ok, #request{verb=Verb, resource=Resource, headers=Headers, params=Params}}.

extract_verb(List) when is_list(List) ->
	Value = proplists:get_value(<<"verb">>, List),
	case Value of
		<<"GET">> -> 'GET';
		<<"POST">> -> 'POST';
		<<"PUT">> -> 'PUT';
		<<"DELETE">> -> 'DELETE';
		<<"AUTH">> -> 'AUTH'
			_ -> throw(incorrect_verb)
	end.




