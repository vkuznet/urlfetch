%% -*- mode: erlang -*-
-module(urlfetch_utils).
-compile(export_all).

%% ------------------------------------------------------------------
%% Print given message to stdout
%% ------------------------------------------------------------------
print(Msg) ->
    io:format("~p~n", [Msg]).

%% @spec timestamp() -> integer
%% @doc  Generates a time stamp.
timestamp() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:now(),
    MegaSecs * 1000000 + Secs.
