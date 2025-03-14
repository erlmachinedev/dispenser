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
    [{timetrap, _Time = {seconds, 10}}].


all() -> [test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test(_Config) ->  
    meck:new(test, [non_strict, no_link]), 
    meck:new(gun, [passthrough, no_link]),

    meck:new(init, [passthrough, unstick, no_link]),

    meck:expect(gun, open, fun open/3),
    meck:expect(gun, await_up, fun await_up/1),

    meck:expect(gun, await, 2, _Response = await()),
    
    meck:expect(gun, post, 3, post()),
    meck:expect(gun, post, 4, post()),

    bootstrap(),
    
    %% Initialization error

    meck:expect(test, setup, fun () -> error(test) end),

    dispenser:boot(test),
    
    {'EXIT', _} = catch(sys:get_state(dispenser)),

    %% Next Invocation (invocation response)

    meck:expect(init, stop, fun () -> ok end),

    meck:expect(test, setup, fun () -> ok end),
    meck:expect(test, exec, fun (_Event, _Context) -> maps:new() end),

    meck:expect(gun, get, fun get/2),
    meck:expect(gun, await_body, fun await_body/2),

    dispenser:boot(test),

    Ref = erlang:make_ref(),
    Pid = self(),
    
    Key = <<"lambda-runtime-aws-request-id">>,

    Headers = [{Key, _Val = <<"e6183403-a036-4179-8267-adfc503af4c2">>}],
    
    erlang:send(dispenser, message(Pid, Ref, 200, Headers)),

    {process, _} = sys:get_state(dispenser),

    %% Response streaming
    
    Next = fun ([]) -> none; 
               ([Term|I]) -> {Term, I} 
           end,
    
    meck:expect(test, iterator, _I = fun (_) -> [<<"{">>, <<"}">>] end),
    meck:expect(test, next, Next),

    meck:expect(gun, data, fun data/4),

    erlang:send(dispenser, message(Pid, Ref, 200, Headers)),
    
    {process, _} = sys:get_state(dispenser),
        
    %% Invocation error
    
    meck:expect(test, exec, fun (_Event, _Context) -> error(test) end),

    erlang:send(dispenser, message(Pid, Ref, 200, Headers)),

    {process, _} = sys:get_state(dispenser),

    %% Runtime shutdown

    erlang:send(dispenser, message(Pid, Ref, 500, [])),
    
    {'EXIT', _} = catch(sys:get_state(dispenser)),
    
    shutdown(),

    meck:unload(test),
    meck:unload(gun).

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

data(Pid, Ref, _IsFin, Data) when is_pid(Pid),
                                  is_reference(Ref),
                                  
                                  is_binary(Data) ->
    ok.

await() ->
    Body = <<"{\"status\":\"OK\"}\n">>,
    Code = 202,
    
    meck:loop([{response, nofin, Code, []}, {data, fin, Body}]).

post() -> 
    erlang:make_ref().

await_body(Pid, Ref) when is_pid(Pid),
                          is_reference(Ref) ->
    _Body = {ok, <<"{}">>}.

message(Pid, Ref, Status, Headers) ->
    _Message = {gun_response, Pid, Ref, nofin, Status, Headers}.
