-module(urlfetch_worker).
-export([process_request/1]).

%% @spec process_request(List) -> {ok, string}
%% @doc  Process given list of urls
process_request(L) ->
%    error_logger:info_msg("~p process: ~p~n", [self(), L]),
    process_request(L, []).
process_request([H|T], Results) ->
    case H of 
        {"urls", Data} ->
            R = process_urls(string:tokens(http_uri:decode(Data), "\n"));
        _ ->
            R = []
    end,
    process_request(T, Results++R);
process_request([], Results) ->
    {ok, parse_results(Results)}.

%% ----------------------------------------------------------------------------
%% Internal worker APIs
%% ----------------------------------------------------------------------------

%% parse data record and create output string
parse_data(L) ->
    NL = [binary_to_list(X) || X <- L, is_binary(X), size(X) > 0],
    string:join(NL, "").

%% parse result list, extract data records with 200 status
%% and parse their content
parse_results(L) ->
    parse_results(L, []).
parse_results([H|T], R) ->
    case H of
        {result, {Status, Data}} ->
            case Status of
                200 ->
                    if  length(R) > 0 ->
                            L = R ++ "\n" ++ parse_data(Data);
                        true ->
                            L = R ++ parse_data(Data)
                    end,
                    parse_results(T, L);
                _ ->
                    error_logger:info_msg("~p Code: ~p, Data: ~p~n", [self(), Status, Data])
            end;
        {error, not_found} ->
            error_logger:info_msg("~p not found~n", [self()])
    end;
parse_results([], R) -> R.

%% process url list, for each given URL asynchronously fetch retrieval
%% function and get back the results from it
process_urls(L) ->
    process_urls(L, []).
process_urls([Url|T], Results) ->
    % create new UUID for request
    Id = urlfetch_uuid:new(),
%    io:format("process URL: ~p, uid: ~p~n", [Url, Id]),
    Payload = "",
    Headers = "",
    % send the request
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

%% get result for given UUID from the cache
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
