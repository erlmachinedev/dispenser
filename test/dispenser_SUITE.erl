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
    meck:new(test, [passthrough, non_strict, no_link]),
    meck:new(gun, [passthrough, no_link]),
    
    meck:expect(gun, open, fun open/3),
    meck:expect(gun, await_up, fun await_up/1),
    
    meck:expect(gun, post, fun post/4),

    %% Initialization error

    bootstrap(),

    meck:expect(test, setup, fun () -> error(test) end),

    meck:expect(gun, await, ['_', '_'], _Response = await()),

    dispenser:boot(test),

    %% Next invocation

    bootstrap(),

    meck:expect(test, setup, fun () -> ok end),
    meck:expect(test, exec, fun (_Event, _Context) -> maps:new() end),

    meck:expect(gun, get, fun get/2),
    meck:expect(gun, await_body, fun await_body/2),

    dispenser:boot(test, [], fun (_) -> application:stop(_App = dispenser) end),

    Ref = erlang:make_ref(),
    Pid = self(),
    
    Key = <<"lambda-runtime-aws-request-id">>,

    Headers = [{Key, _Val = <<"e6183403-a036-4179-8267-adfc503af4c2">>}],
    
    erlang:send(dispenser, message(Pid, Ref, 200, Headers)),
    erlang:send(dispenser, message(Pid, Ref, 200, Headers)),

    %meck:expect(Mod, exec, fun (_Event, _Context) -> error(test) end),

    %meck:expect(_Mod = test, iterator, fun (Json) -> error(not_implemeted) end),
    %meck:expect(_Mod = test, next, fun (I) -> none end),
    
    %sys:get_status(dispenser),

    %% TODO Terminare the state machine (Code 500) 

    %% TODO Check that process is running
    %% NOTE {status, Pid, _Mod, [_PDict, running, _, _Dbg, Info]} = sys:get_status(Name),

    ct:print("Status ~tp", [sys:get_status(dispenser)]),

    erlang:send(dispenser, message(Pid, Ref, 500, [])),

    ct:print("Status ~tp", [catch(sys:get_status(dispenser))]),

    meck:unload(test),
    meck:unload(gun),   
    
    ct:print("Config ~tp", [Config]).

%%--------------------------------------------------------------------
%% FUNCTIONS
%%--------------------------------------------------------------------

bootstrap() ->
    application:ensure_all_started(dispenser).

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
    
await() ->
    Body = <<"{\"status\":\"OK\"}\n">>,
    Code = 202,
    
    meck:loop([{response, nofin, Code, []}, {data, fin, Body}]).
    
await_body(_Pid, _Ref) ->
    _Body = {ok, <<"{}">>}.

message(Pid, Ref, Status, Headers) ->
    _Message = {gun_response, Pid, Ref, nofin, Status, Headers}.