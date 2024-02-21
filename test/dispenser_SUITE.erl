-module(dispenser_SUITE).

-export([suite/0]).

-export([all/0]).

-export([init_per_suite/1, end_per_suite/1]).

-export([test/1]).

-include_lib("common_test/include/ct.hrl").

-export([]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [ {timetrap, _Time = {seconds, 10} } ].


all() -> [ test ].

init_per_suite(Config) ->
    ct:print("ENV ~tp", [os:getenv(_Var = "AWS_LAMBDA_RUNTIME_API")]),
    
    %os:putenv(Var, "127.0.0.1:8080"),

    Res = Config,
    Res.

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test(Config) ->
    Mod = test,

    meck:new(Mod, [passthrough, non_strict, no_link]),
    meck:new(gun, [passthrough, no_link]),
    
    meck:expect(gun, open, fun open/3),
    meck:expect(gun, await_up, fun await_up/1),
    
    meck:expect(gun, post, fun post/4),

    bootstrap(),

    meck:expect(Mod, setup, fun () -> error(test) end),
    
    Loop = [{response, nofin, 202, []}, {data, fin, <<"{\"status\":\"OK\"}\n">>}],

    meck:expect(gun, await, ['_', '_'], meck:loop(Loop)),
    
    dispenser:boot(Mod),

    %meck:expect(gun, get, fun get/2),

    %% NOTE Inspect the runtime error
    %meck:expect(Mod, exec, fun (_Event, _Context) -> #{test => ok} end),

    %meck:expect(Mod, exec, fun (_Event, _Context) -> error(test) end),

    %meck:expect(gun, await, fun (_Pid, _Ref) -> {response, nofin, 202, []} end),

    %Body = <<"{}">>,
 
    %meck:expect(gun, await_body, fun (_Pid, _Ref) -> {ok, Body} end),
 
    %Pid = self(),
    %Ref = erlang:make_ref(),

    %Headers = [{<<"lambda-runtime-aws-request-id">>, <<"e6183403-a036-4179-8267-adfc503af4c2">>}],

    %erlang:send(dispenser, {gun_response, Pid, Ref, nofin, 200, Headers}),
    
    %% TODO Terminate the process and start the next testcase
    
    %erlang:send(dispenser, {gun_response, Pid, Ref, nofin, 500, Headers}),

    %meck:expect(Mod, exec, fun (_Event, _Context) -> error(test) end),

    %meck:expect(_Mod = test, iterator, fun (Json) -> error(not_implemeted) end),
    %meck:expect(_Mod = test, next, fun (I) -> none end),
    
    %erlang:send(dispenser, {gun_response, Pid, Ref, nofin, 200, Headers}),
    %sys:get_status(dispenser),

    %% TODO Terminare the state machine (Code 500) 

    %% TODO Check that process is running
    %% NOTE {status, Pid, _Mod, [_PDict, running, _, _Dbg, Info]} = sys:get_status(Name),

    meck:unload(Mod),
    meck:unload(gun),   
    
    ct:print("Config ~tp", [Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------

bootstrap() ->
    Res = application:ensure_all_started(dispenser),
    Res.

shutdown() ->
    application:stop(dispenser).


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