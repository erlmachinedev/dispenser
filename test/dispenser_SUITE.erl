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
      {stream, [test]},
      {report, [test]}
    ].

all() ->
    [ {group, submit, []}
      %{group, stream, []}
    ].

init_per_suite(Config) ->
    Var = "AWS_LAMBDA_RUNTIME_API",
    
    ct:print("ENV ~p", [os:getenv("AWS_LAMBDA_RUNTIME_API")]),
    
    %os:putenv(Var, "127.0.0.1:8080"),

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
    Mod = test,
    
    meck:new(Mod, [passthrough, non_strict, no_link]),
    
    %% NOTE Inspect the runtime error
    meck:expect(Mod, setup, fun () -> ok end),
    
    %% NOTE Inspect the runtime error
    meck:expect(Mod, exec, fun (_Event, _Context) -> #{test => ok} end),

    %meck:expect(Mod, exec, fun (_Event, _Context) -> error(test) end),

    meck:new(gun, [passthrough, no_link]),
    
    meck:expect(gun, open, fun open/3),
    meck:expect(gun, await_up, fun await_up/1),
    
    meck:expect(gun, get, fun get/2),
    meck:expect(gun, post, fun post/4),

    dispenser:boot(Mod),

    Loop = [{response, nofin, 202, []}, {data, fin, <<"{\"status\":\"OK\"}\n">>}],

    meck:expect(gun, await, ['_', '_'], meck:loop(Loop)),

    meck:expect(gun, await, fun (_Pid, _Ref) -> {response, nofin, 202, []} end),

    Body = <<"{}">>,
 
    meck:expect(gun, await_body, fun (_Pid, _Ref) -> {ok, Body} end),
 
    Pid = self(),
    Ref = erlang:make_ref(),

    Headers = [{<<"lambda-runtime-aws-request-id">>, <<"e6183403-a036-4179-8267-adfc503af4c2">>}],

    erlang:send(dispenser, {gun_response, Pid, Ref, nofin, 200, Headers}),
    
    %% TODO Terminate the process and start the next testcase
    
    erlang:send(dispenser, {gun_response, Pid, Ref, nofin, 500, Headers}),

    meck:expect(Mod, exec, fun (_Event, _Context) -> error(test) end),

    shutdown(),

    ct:print("bootstrap ~tp", [bootstrap(_Mode = submit)]),

    dispenser:boot(Mod),
    
    erlang:send(dispenser, {gun_response, Pid, Ref, nofin, 200, Headers}),
    %sys:get_status(dispenser),

    %% TODO Terminare the state machine (Code 500) 

    %% TODO Check that process is running
    %% NOTE {status, Pid, _Mod, [_PDict, running, _, _Dbg, Info]} = sys:get_status(Name),

    %meck:unload(Mod),    
    
    ct:print("~n~p: ~p~n", [?FUNCTION_NAME, Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------

bootstrap(_Mode) ->
    Res = application:ensure_all_started(dispenser),
    Res.

shutdown() ->
    Res = application:stop(dispenser),
    Res.


%%--------------------------------------------------------------------
%% GUN
%%--------------------------------------------------------------------

open(_Host, Port, Opts) when is_integer(Port),
                             is_map(Opts) ->
    erlbox:success(_Pid = self()).
    
await_up(Pid) when is_pid(Pid) ->
    erlbox:success(_Ret = http).
    
get(Pid, _Path) when is_pid(Pid) ->
    erlang:make_ref().

post(Pid, _Path, _Headers, Json) when is_pid(Pid),
                                      is_binary(Json) ->
    erlang:make_ref().