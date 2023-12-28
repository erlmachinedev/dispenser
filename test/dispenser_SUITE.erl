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
    [ {group, submit, []}
      %{group, stream, []}
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
    Mod = test,
    
    meck:new(Mod, [passthrough, non_strict, no_link]),
    
    %% NOTE Inspect the runtime error
    meck:expect(Mod, setup, fun () -> ok end),
    
    %% NOTE Inspect the runtime error
    meck:expect(Mod, exec, fun (_Event, _Context) -> #{test => ok} end),

    %% TODO Implement gun mock as a Fun
    
    meck:new(gun, [passthrough, no_link]),
    

    meck:expect(gun, open, fun open/3),
    meck:expect(gun, await_up, fun await_up/1),
    
    meck:expect(gun, get, fun get/2),
    
    dispenser:boot(Mod),

    %% TODO Send the message (Code 200) from Gun

    %% TODO Repeat the test (Code 500) 


    %% send(dispenser, _Message = {gun_response, Pid, Ref, _, _Status = 200, Headers})

    %% TODO RIE interaction to pass sync test
    %% TODO Get the reponse from a runtime
    
    %% TODO Check that process is running
    %% NOTE {status, Pid, _Mod, [_PDict, running, _, _Dbg, Info]} = sys:get_status(Name),

    meck:unload(Mod),    

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

inspect(Fun) ->
    receive Fun -> Fun() 
    
    end.
    
%%--------------------------------------------------------------------
%% GUN
%%--------------------------------------------------------------------

open(_Host = "127.0.0.1", Port, Opts) when is_integer(Port),
                                           is_map(Opts) ->

    erlbox:success(_Pid = self()).
    
await_up(Pid) when is_pid(Pid) ->
    erlbox:success(_Ret = http).
    
get(Pid, _Path = "/2018-06-01/runtime/invocation/next") when is_pid(Pid) ->
    _Ref = erlang:make_ref().