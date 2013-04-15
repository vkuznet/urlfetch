-module(urlfetch_http).
-export([encode_headers/1, decode_headers/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

encode_headers(Headers) ->
    L = lists:flatmap(
        fun({H, V}) -> 
            [H ++ ": " ++ V]
        end,
        Headers),
    string:join(L, "\n").


decode_headers(Bin) when is_binary(Bin) ->
  decode_headers(binary_to_list(Bin));
decode_headers(String) ->
  decode_headers(String, [], []).


decode_headers([], [], Result) ->
  lists:reverse(Result);
decode_headers([], Buffer, [{Key}|Result]) ->
  decode_headers([], [], [{Key, lists:reverse(Buffer)}|Result]);
decode_headers(": " ++ String, Buffer, Result) ->
  decode_headers(String, [], [{lists:reverse(Buffer)}|Result]);
decode_headers("\n" ++ String, Buffer, [{Key}|Result]) ->
  decode_headers(String, [], [{Key, lists:reverse(Buffer)}|Result]);
decode_headers([C|String], Buffer, Result) ->
  decode_headers(String, [C|Buffer], Result).


-ifdef(TEST).
%% Unit tests.
encode_headers_test() ->
    ?assert(encode_headers([{"Content-Type","text/plain"}]) =:= "Content-Type: text/plain"),
    ?assert(encode_headers([{"Content-Type","text/plain"},{"Content-Length","42"}]) =:= "Content-Type: text/plain\nContent-Length: 42").

decode_headers_test() ->
    ?assert(decode_headers(<<"Content-Type: text/plain">>) =:= [{"Content-Type","text/plain"}]),
    ?assert(decode_headers("Content-Type: text/plain") =:= [{"Content-Type","text/plain"}]),
    ?assert(decode_headers("Content-Type: text/plain\nX-Custom-Header: foobar (1)") =:= [{"Content-Type","text/plain"},{"X-Custom-Header","foobar (1)"}]).
-endif.
