%% @author Valenti Kuznetsov <vkuznet@gmail.com>
%% @copyright 2013 Valentin Kuznetsov.
%% @doc urlfetch_resource.

%% Comments:
%% original code for json resource has been taken from
%% https://bitbucket.org/bryan/wmexamples/src/tip/src/formjson_resource.erl
%% code for urlfetch part comes from
%% https://code.google.com/p/urlfetch/
%% The rest is given resource for webmachine

-module(urlfetch_resource).
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    urlfetch_uuid:start_link(),
    urlfetch_cache:start_link(),
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'POST'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

%% hit this with
%%   curl "http://localhost:8000/formjson?one=two&me=pope"
to_json(RD, Ctx) ->
    {json_body(wrq:req_qs(RD)), RD, Ctx}.

%% hit this with
%%   curl -X POST http://localhost:8000/formjson \
%%        -d "one=two&me=pope"
process_post(RD, Ctx) ->
    Request = mochiweb_util:parse_qs(wrq:req_body(RD)),
%    io:format("Request ~p~n", [Request]),
    {ok, Data} = process_request(Request),
    Body = mochijson:encode({struct, [{data,Data}]}),
    {true, wrq:append_to_response_body(Body, RD), Ctx}.

json_body(QS) -> mochijson:encode({struct, QS}).

%%
%% Internal APIs
%%

process_request(L) ->
    process_request(L, []).
process_request([H|T], Results) ->
%    io:format("process ~p~n", [H]),
    case H of 
        {"urls", Data} ->
            R = process_urls(string:tokens(http_uri:decode(Data), "\n"));
        _ ->
%            io:format("No urls~n")
            R = []
    end,
    process_request(T, Results++R);
process_request([], Results) ->
    {ok, parse_results(Results)}.

parse_data(L) ->
    NL = [binary_to_list(X) || X <- lists:flatten(L), is_binary(X), size(X) > 0],
    string:join(NL, "").

parse_results(L) ->
    parse_results(L, []).
parse_results([H|T], R) ->
    case H of
        {result, {Status, Data}} ->
            case Status of
                200 ->
                    parse_results(T, R++parse_data(Data)++"\n")
            end;
        {error, not_found} ->
            io:format("ERROR: No data~n")
    end;
parse_results([], R) -> R.

process_urls(L) ->
    process_urls(L, []).
process_urls([Url|T], Results) ->
    Id = urlfetch_uuid:new(),
    io:format("process URL: ~p, uid: ~p~n", [Url, Id]),
    Payload = "",
    Headers = "",
    Data = urlfetch_async:fetch({Id, get, Url, Payload, Headers}),
    case Data of
        ok ->
            Output = get_result(Id);
        error ->
            Output = {error, not_found}
    end,
%    io:format("OUTPUT: ~p~n", [Output]),
    process_urls(T, Results++[Output]);
process_urls([], Results) -> Results.

get_result(Id) ->
    case urlfetch_async:get_result(Id) of
        {result, Result} ->
            {result, Result};
        {error, retry} ->
            timer:sleep(50),
            get_result(Id);
        {error, not_found} ->
            {error, not_found}
    end.
