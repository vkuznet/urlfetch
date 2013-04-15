%% -*- mode: erlang -*-
-module(urlfetch_cache).
-behaviour(gen_server).

%% Public APIs
-export([start_link/0,store/1,exist/1,delete/1,fetch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("urlfetch.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% Public APIs implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
store(Record) -> gen_server:call(?MODULE, {store, Record}).
exist(Id) -> gen_server:call(?MODULE, {exists, Id}).
delete(Id) -> gen_server:call(?MODULE, {delete, Id}).
fetch(Id) -> gen_server:call(?MODULE, {fetch, Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Create tables
    Table = ets:new(cache_table, [public, named_table, ordered_set]),
    {ok, Table}.

%% handle_call is used for synchronous communication between the client and
%% the server. That is, it is used when the server expects a response. 
handle_call({store, Record}, _From, Table) ->
    purge_expired(Table),
    {reply, store(Table, Record), Table};
handle_call({exists, Id}, _From, Table) ->
    purge_expired(Table),
    {reply, ets:member(Table, Id), Table};
handle_call({delete, Id}, _From, Table) ->
    purge_expired(Table),
    {reply, delete(Table, Id), Table};
handle_call({fetch, Id}, _From, Table) ->
    purge_expired(Table),
    {reply, fetch(Table, Id), Table}.

%% handle_cast is used for asynchronous communication between the client and
%% the server.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info is called by a gen_server when a timeout occurs or
%% when it receives any other message than a synchronous or asynchronous request
%% (or a system message).
handle_info(_Info, Table) ->
    purge_expired(Table),
    {noreply, Table}.

%% terminate function is called by a gen_server when it is about to terminate.
%% It should be the opposite of Module:init/1 and do any necessary cleaning up.
%% When it returns, the gen_server terminates with Reason. The return value is
%% ignored.
terminate(_Reason, _State) ->
    ok.

%% This function is called by a gen_server when it should update its internal
%% state during a release upgrade/downgrade, i.e. when the instruction
%% {update,Module,Change,...} where Change={advanced,Extra} is given in the appup
%% file.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Internal API

%% @spec store(Table, Record) -> null
%% @doc  Stores a cache record.
store(Table, Record) ->
    Id        = Record#cache.id,
    Status    = Record#cache.status_code,
    Data      = Record#cache.data,
    Complete  = Record#cache.complete,
    Timestamp = Record#cache.timestamp,

    error_logger:info_msg("~p Inserting data for ~p.~n", [self(), Id]),

    case ets:match(Table, {Id, '$1', '$2', false, '$3'}) of
        [] ->
            ets:insert(Table, {Id, Status, Data, false, Timestamp});
        [[_, OldData, _]] ->
            ets:insert(Table,
                {Id, Status, [OldData, Data], Complete, Timestamp})
    end.


%% @spec delete(Table, Id) -> null
%% @doc  Deletes a cache record.
delete(Table, Id) ->
    error_logger:info_msg("~p Deleting data for ~p.~n", [self(), Id]),
    case ets:lookup(Table, Id) of
        [] ->
            false;
        [{_, Status, Data, Complete, Timestamp}] ->
            ets:delete_object(Table,
                              {Id, Status, Data, Complete, Timestamp})
    end.


%% @spec fetch(Table, Id) -> any()
%% @doc  Fetches a cache record by Id.
fetch(Table, Id) ->
    case ets:lookup(Table, Id) of
        [] ->
            {error, not_found};
        [{_, Status, Data, Complete, _}] ->
            case Complete of
                true ->
                    {result, {Status, Data}};
                false ->
                    {error, retry}
            end
    end.


%% @spec purge_expired() -> null
%% @doc  Purges expired cache records.
purge_expired(Table) ->
    T = urlfetch_utils:timestamp() - ?EXPIRATION_INTERVAL,
    S = [{{'$1', '$2', '$3', '$4', '$5'}, [{'<', '$5', {const, T}}], [true]}],
    N = ets:select_delete(Table, S),
    if N > 0 ->
        error_logger:info_msg(
            "~p ~p expired record(s) purged.~n", [self(), N]);
        true ->
            false
    end.
