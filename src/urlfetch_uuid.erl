-module(urlfetch_uuid).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([new/0, random/0, utc_random/0]).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:cast(?MODULE, stop).


new() ->
    gen_server:call(?MODULE, create).


random() ->
    list_to_binary(to_hex(crypto:rand_bytes(16))).


utc_random() ->
    Now = {_, _, Micro} = now(),
    Nowish = calendar:now_to_universal_time(Now),
    Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
    Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
    list_to_binary(Prefix ++ to_hex(crypto:rand_bytes(9))).


init([]) ->
    {ok, state()}.


terminate(_Reason, _State) ->
    ok.


handle_call(create, _From, random) ->
    {reply, random(), random};
handle_call(create, _From, utc_random) ->
    {reply, utc_random(), utc_random}.


handle_cast(change, _State) ->
    {noreply, state()};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].


to_digit(N) when N < 10 ->
    $0 + N;
to_digit(N) ->
    $a + N-10.


state() ->
    utc_random.
