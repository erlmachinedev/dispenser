-module(dispenser).

-define(AWS_LAMBDA_RUNTIME_API, "AWS_LAMBDA_RUNTIME_API").

-define(LAMBDA_TASK_ROOT, "LAMBDA_TASK_ROOT").
-define(_HANDLER, "_HANDLER").

-import(erlbox, [success/3, failure/2]).

-export([boot/1, boot/2, boot/3]).

-export([name/0]).
-export([start_link/0]).

-export([encode/1, encode/2, decode/1, decode/2]).

-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

-export([process/3]).

-export([file/1, method/0]).

-behaviour(gen_statem).

%% TODO Add the ENV list check during the start

%% TODO The usage of _HANDLER, LAMBDA_TASK_ROOT and other ENVs

%% TODO setup(path), Fun = shutdown(), is_restored(), exec

%%% API

boot(Mod) ->
    boot(Mod, _Shutdown = fun init:stop/0).

boot(Mod, Shutdown) ->
    boot(Mod, Shutdown, _Depth = -1).

boot(Mod, Shutdown, Depth) when is_function(Shutdown),
                                is_integer(Depth),
                                
                                is_atom(Mod) ->
                                
    Res = dispenser_sup:start_child(Mod, Shutdown, Depth),
    Res.

encode(Term) ->
    encode(Term, []).

encode(Term, Opts) ->
    jsx:encode(Term, Opts).
    
decode(Json) ->
    decode(Json, []).

decode(Json, Opts) ->
    jsx:decode(Json, Opts).

name() ->
    ?MODULE.

-spec start_link(module(), function(), -1 | integer >0) -> success(pid()).
start_link(Mod, Shutdown, Depth) ->
    Name = name(),
    
    Data = data(Mod, Shutdown, fun (X) -> io_lib:write(X, Depth) end),
    
    Time = 10000,
    
    gen_statem:start_link({local, Name}, ?MODULE, Data, [{timeout, Time}]).

%% gen_statem

-record(data, { module::module(), shutdown::function(), format::function() }).

-type data() :: #data{}.

init(Data) ->
    process_flag(sensitive, true),
    
    T = "~p",
    
    URI = uri(),

    %% TODO Perform a connection here

    ct:print(T, [URI]),

    try setup(Data)
    
        success(process, Data, []).
    
    catch E:R:S -> ok 
    %% TODO Runtime report (the same way as exec report)

        failure(R)
    end;

terminate(_Reason, _State, _Data) ->
    ok.

callback_mode() -> [state_functions, state_enter].

%%  State machine

process(_Type, _Msg, Data) ->
    %% TODO "If" condition to decide termination

    {repeat_state, NewData, Actions}.

%%% Data

-spec data(module(), function(), function()) -> data().
data(Mod, Shutdown, Format) ->
    #data{ module = Mod, shutdown = Shutdown, format = Format }.

%% ENV

-spec uri() -> string().
uri() -> 
    Env = os:getenv(?AWS_LAMBDA_RUNTIME_API),
    
    Res = uri_string:parse(_Uri = lists:append("http://", Env)),
    Res.
    
-spec root() -> file:filename().
root() ->
    Env = os:getenv(?LAMBDA_TASK_ROOT),
    
    Res = Env,
    Res.

-spec file(string()) -> file:filename().
file(Ext) ->
    Env = os:getenv(?_HANDLER),
    
    [File, _Method] = string:lexemes(Env, "."),
    
    Res = filename:join(Env, _Name = lists:append(File, Ext)),
    Res.

-spec method() -> string().
method() ->
    Env = os:getenv(?_HANDLER),
    
    [_File, Method] = string:lexemes(Env, "."),

    Res = Method,
    Res.