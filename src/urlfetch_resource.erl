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
         to_text/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> 
    urlfetch_uuid:start_link(),
    urlfetch_cache:start_link(),
    {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'POST'], RD, Ctx}.

%content_types_provided(RD, Ctx) ->
%    {[{"application/json", to_json}], RD, Ctx}.

content_types_provided(ReqData, Context) ->
    {[{"application/json",to_json},{"text/plain",to_text}], ReqData, Context}.

%% Handle GET request
to_text(ReqData, Context) ->
    Path = wrq:disp_path(ReqData),
    Body = io_lib:format("Hello ~s from webmachine.~n", [Path]),
    {Body, ReqData, Context}.

%% Handle GET request using application/json header, e.g.
%% curl "http://localhost:8000/formjson?one=two&me=pope"
to_json(RD, Ctx) ->
    {json_body(wrq:req_qs(RD)), RD, Ctx}.

%% Handle POST request, e.g.
%% curl -X POST http://localhost:8000/formjson -d "one=two&me=pope"
process_post(RD, Ctx) ->
    Request = mochiweb_util:parse_qs(wrq:req_body(RD)),
    {ok, Data} = urlfetch_worker:process_request(Request),
    {true, wrq:append_to_response_body(Data, RD), Ctx}.

%% Handle POST request and return data in JSON format, {"data":Data}
%% curl -X POST http://localhost:8000/formjson -d "one=two&me=pope"
%process_post(RD, Ctx) ->
%    Request = mochiweb_util:parse_qs(wrq:req_body(RD)),
%    {ok, Data} = urlfetch_worker:process_request(Request),
%    Body = mochijson:encode({struct, [{data,Data}]}),
%    {true, wrq:append_to_response_body(Body, RD), Ctx}.

json_body(QS) -> mochijson:encode({struct, QS}).

