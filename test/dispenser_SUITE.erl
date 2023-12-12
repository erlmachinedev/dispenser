-module(dispenser_SUITE).

-export([suite/0, groups/0]).

-export([all/0]).

-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).

-export([test/1]).

-include_lib("common_test/include/ct.hrl").

-export([]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [ {timetrap, _Time = {seconds, 10} } ].

groups() ->
    [ {submit, [test]},
      {stream, [test]}
    ].

all() ->
    [ {group, submit, []},
      {group, stream, []}
    ].


init_per_suite(Config) ->
    Res = Config,
    Res.

init_per_group(GroupName = submit, Config) ->
    bootstrap(GroupName),

    Res = Config,
    Res;

init_per_group(GroupName = stream, Config) ->
    bootstrap(GroupName),

    Res = Config,
    Res.

end_per_group(GroupName, _Config) ->
    ct:print("~p", [GroupName]),
  
    shutdown().

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test(Config) ->
    Pid = connect(_URI = dispenser:uri()),

    ct:print("Pid: ~p", [Pid]),
    
    %Ref = gun:post(Pid, "/2015-03-31/functions/function/invocations", [], <<"{}">>),
    %Res = gun:await_body(Pid, Ref),
    
    %ct:print("Res: ~p", [Res]),
    
    %% TODO Implement gun mock as a Fun
    
    %% TODO Consider to implement Gun emulator (recorded scenario inside process)
    
    %% TODO Lambda will not start a client until the task is ready (404 code)
    
    T = "~p",
    
    Ref0 = gun:get(Pid, "/2018-06-01/runtime/invocation/next"),
    
    {response, _, 200, Headers} = gun:await(Pid, Ref0),

    AwsRequestId = proplists:get_value(<<"lambda-runtime-aws-request-id">>, Headers),

    ct:print(T, [AwsRequestId]),

    Res0 = gun:await(Pid, Ref0),

    ct:print(T, [Res0]),

    Ref1 = gun:post(Pid, unicode:characters_to_list(["/2018-06-01/runtime/invocation/", AwsRequestId, "/response"]), [], <<"OK">>),
    Res1 = gun:await(Pid, Ref1),
    
    ct:print(T, [Res1]),
    
    ct:print(T, [gun:await(Pid, Ref1)]),

    Ref = gun:get(Pid, "/2018-06-01/runtime/invocation/next"),
    Res = gun:await(Pid, Ref),

    ct:print(T, [Res]),

    ct:log("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------

bootstrap(_Mode) ->
    Res = application:ensure_all_started(dispenser),
    Res.

shutdown() ->
    Res = application:stop(dispenser),
    Res.

connect(URI) ->
    Host = maps:get(host, URI),
    Port = maps:get(port, URI),
    
    Opts =  #{ retry => 0,
               connect_timeout => 2000,

               transport => tcp
             },

    {ok, Pid} = gun:open(Host, Port, Opts),
    {ok, _} = gun:await_up(Pid),

    Res = Pid,
    Res.