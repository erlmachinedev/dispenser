-module(dispenser).

-define(AWS_LAMBDA_RUNTIME_API, "AWS_LAMBDA_RUNTIME_API").

-define(LAMBDA_TASK_ROOT, "LAMBDA_TASK_ROOT").
-define(_HANDLER, "_HANDLER").

-import(erlbox, [success/3, failure/2]).

-export([boot/1, boot/2, boot/3]).

-export([start_link/0]).

-export([encode/1, encode/2, decode/1, decode/2]).

-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

-export([process/3]).

-export([file/1, method/0]).

-behaviour(gen_statem).

%% NOTE Client can generate more readable exceptions via erlang:error/3 

%%% API

boot(Mod) ->
    boot(Mod, _Shutdown = fun init:stop/0).

boot(Mod, Shutdown) ->
    boot(Mod, Shutdown, _Depth = -1).

boot(Mod, Shutdown, Opts) when is_function(Shutdown),
                               is_integer(Depth),
                                
                               is_map(Opts) ->
                                
    dispenser_sup:start_child(Mod, Shutdown, Opts).

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

-spec start_link(module(), function(), map()) -> success(pid()).
start_link(Mod, Shutdown, Opts) ->
    Name = name(),
    
    Format = fun (E, R, S) -> erl_error:format_exception(E, R, S, Opts) end,

    Data = data(Mod, Shutdown, Format),
    
    Time = 10000,
    
    gen_statem:start_link({local, Name}, ?MODULE, Data, [{timeout, Time}]).

%% gen_statem

-record(data, { module::module(), 
                format::function(), shutdown::function(), 

                connection::pid() 
              }).

-type data() :: #data{}.

init(Data) ->
    process_flag(sensitive, true),
    
    URI = uri(),

    Host = maps:get(host, URI),
    Port = maps:get(port, URI),
    
    {ok, Pid} = gun:open(Host, Port, _Opts = #{ transport => tcp }),
    {ok, _} = gun:await_up(Pid),
    
    monitor(process, Pid),

    try setup(Data)
    
        success(process, connection(Data, Pid), [])
    
    catch E:R:S -> 
        Body = #{ stackTrace => stacktrace(S), 
                  
                  errorType => E, 
                  errorMessage => R
                },
        
        Json = jsx:encode(Body),
        
        Ref = gun:get(Pid, "/runtime/init/error", _Headers = [], Json),
        
        {response, nofin, 200, _Headers} = gun:await(Pid, Ref1),

        ct:print(Res),

        failure(R)
    end;

terminate(_Reason, _State, _Data) ->
    ok.

callback_mode() -> [state_functions, state_enter].

%%  State machine

process(enter, _State, Data) ->
    Pid = connection(Data),
    
    Ref = gun:get(Pid, "/2018-06-01/runtime/invocation/next"),
    
    {keep_state, NewData};

process(_Type, _Msg, Data) ->
    %% TODO "If" condition to decide termination

    try exec(Event, _Context = context(Headers))
    
    %% Respond to the Lambda
    
    catch E:R:S ->
    
    %% Report invocation report
    
    end,

    {repeat_state, NewData, Actions}.

process(info, {'DOWN', _MRef, process, _Pid, Reason}, Data) ->
    %% TODO Switch to the cancellation state 

    {next_state, state, Reason, Data};

%%% Data

-spec data(module(), function(), function()) -> data().
data(Mod, Shutdown, Format) ->
    #data{ module = Mod, shutdown = Shutdown, format = Format }.

-spec connection(data(), pid()) -> data().
connection(Data, Pid) ->
    Data#data{ connection = Pid }.

-spec connection(data()) -> pid().
connection(Data) ->
    Res = Data#data.connection,
    Res.

-spec module(data()) -> module().
module(Data) ->
    Res = Data#data.module,
    Res.

-spec format(data()) -> function().
format(Data) ->
    Res = Data#data.format,
    Res.

-spec setup(data()) -> success().
setup(Data) ->
    Mod = module(Data),
    
    Mod:setup().

-spec exec(data(), event(), context()) -> binary().
exec(Data, Event, Context) ->
    Mod = module(Data),
    
    Mod:exec(Event, Context).
    
-spec stacktrace(data(), [term()]) -> [string()].
stacktrace(Data, Term) ->
    Fun = format(Data), true = is_function(Fun),
    
    Res = Fun(Term),
    Res.

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