%% -*- mode: erlang -*-
-module(urlfetch_async).
-export([fetch/1, fetch/7, process_record/1, get_result/1, purge/1]).

-include("urlfetch.hrl").

%% ===================================================================
%% Public APIs
%% ===================================================================

%% @spec fetch({Id, Method, Url, Payload, Headers}) -> ok | error
%% @doc  Fetches data from an HTTP server.
fetch({Id, Method, Url, Payload, Headers})
    when  (Method =:= get)
    orelse(Method =:= post)
    orelse(Method =:= head)
    orelse(Method =:= put)
    orelse(Method =:= delete) ->
        % store new Id into cache
        Record = #cache{id=Id, timestamp=urlfetch_utils:timestamp()},
        process_record(Record),
        % process fetch request
        timer:sleep(?THROTTLE),
        spawn(urlfetch_async, fetch,
              [Id, Url, Method, Payload, Headers, ?RETRY_COUNT, ?RETRY_TIMEOUT]),
        ok;
fetch(_) ->
    error.

process_record(Record) ->
    urlfetch_cache:store(Record).
 

get_result(Id) ->
    urlfetch_cache:fetch(Id).

purge(Id) ->
    urlfetch_cache:delete(Id).

%% ===================================================================
%% Local helpers
%% ===================================================================

http_options() ->
    Ckey   = os:getenv("X509_USER_KEY"),
    Cert   = os:getenv("X509_USER_CERT"),
    CAcert = os:getenv("X509_USER_CACERT"),
    L = [{keyfile, Ckey}, {certfile, Cert}, {cacertfile, CAcert}],
    [{ssl, lists:filter(fun({_,V}) -> V =/= false end, L)}].

fetch(Id, Url, Method, Payload, Headers, Retry, Sleep) when Retry > 0 ->
    case Method of
        Method when(Method =:= put) orelse(Method =:= post) ->
            Request = {Url, Headers, "", Payload};
        _ ->
            Request = {Url, Headers}
    end,
% I move this part of code into public fetch method, such that
% we created record in cache before spawning the fetch process
%    Record = #cache{id=Id, timestamp=urlfetch_utils:timestamp()},
%    process_record(Record),
    HTTPOptions = http_options(),
    Options = [{sync, false}, {stream, self}],
    error_logger:info_msg("https:request(~p, ~p, ~p, ~p)~n",
            [Method, Request, HTTPOptions, Options]),
    case httpc:request(Method, Request, HTTPOptions, Options) of
        {ok, ReqId} ->
            case receive_chunk(Id, ReqId) of
                {ok, _} ->
                    ok;
                {error, unauthorized, {_Status, _Headers}} ->
                    ok;
                {error, timeout} ->
                    error_logger:info_msg("~p Timeout.~n", [self()]),
                    timer:sleep(Sleep),
                    fetch(Id, Url, Method, Payload, Headers, Retry-1, Sleep);
                {_, Reason} ->
                    error_logger:error_msg("~p~n", [Reason]),
                    timer:sleep(Sleep),
                    fetch(Id, Url, Method, Payload, Headers, Retry-1, Sleep)
            end;
        _ ->
            timer:sleep(Sleep),
            fetch(Id, Url, Method, Payload, Headers, Retry-1, Sleep)
    end;
fetch(Id, _, _, _, _, Retry, _) when Retry =< 0 ->
    error_logger:info_msg("~p Giving up on ~p.~n", [self(), Id]),
    purge(Id),
    {error, no_more_retry}.

receive_chunk(Id, ReqId) ->
    receive
        {http, {ReqId, {error, Reason}}} when(Reason =:= etimedout) orelse(Reason =:= timeout) -> 
            {error, timeout};
        {http, {ReqId, {{_, 401, _} = Status, Headers, _}}} -> 
            Record = #cache{
                id=Id, status_code=401, complete=true,
                timestamp=urlfetch_utils:timestamp()},
            process_record(Record),
            {error, unauthorized, {Status, Headers}};
        {http, {ReqId, Result}} -> 
            {error, Result};
        {http, {ReqId, stream_start, _Headers}} ->
%        {http, {ReqId, stream_start, Headers}} ->
%            EncodedHeaders = list_to_binary(
%                urlfetch_http:encode_headers(Headers) ++ "\n\n"),
%            Record = #cache{
%                id=Id, data=EncodedHeaders,
%                timestamp=urlfetch_utils:timestamp()},
%            process_record(Record),
            receive_chunk(Id, ReqId);
        {http, {ReqId, stream, Data}} ->
            Record = #cache{
                id=Id, data=Data, timestamp=urlfetch_utils:timestamp()},
            process_record(Record),
            receive_chunk(Id, ReqId);
        {http, {ReqId, stream_end, _Headers}} ->
            Record = #cache{
                id=Id, complete=true, timestamp=urlfetch_utils:timestamp()},
            process_record(Record),
            {ok, ReqId}
 
        after 10 * 1000 ->
            {error, timeout}
    end.

