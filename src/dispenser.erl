-module(dispenser).

-define(AWS_LAMBDA_RUNTIME_API, "AWS_LAMBDA_RUNTIME_API").

-define(LAMBDA_TASK_ROOT, "LAMBDA_TASK_ROOT").
-define(_HANDLER, "_HANDLER").

-define(_X_AMZN_TRACE_ID, "_X_AMZN_TRACE_ID").

-import(erlbox, [success/3, failure/1]).

-export([boot/1, boot/2, boot/3]).

-export([start_link/3]).

-export([encode/1, encode/2, decode/1, decode/2]).

-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

-export([process/3]).

-export([file/1, method/0]).

-behaviour(gen_statem).

-include_lib("erlbox/include/erlbox.hrl").

-type context() :: map().

%% NOTE Client can generate more readable runtime exceptions via erlang:error/3 
%% TODO Setup the trace id

%%% API

boot(Mod) ->
    boot(Mod, _Shutdown = fun init:stop/0).

boot(Mod, Shutdown) ->
    boot(Mod, Shutdown, _Opts = #{}).

boot(Mod, Shutdown, Opts) when is_atom(Mod),
                               is_function(Shutdown),
                                
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

-record(data, { connection::pid(), 
                shutdown::function(), format::function(),

                module::module()
              }).

-type data() :: #data{}.

init(Data) ->
    process_flag(sensitive, true),
    
    Pid = connect(_URI = uri()),
    
    monitor(process, Pid),

    try setup(Data),
    
        success(process, connection(Data, Pid), [])
    
    catch E:R:S -> 
        report(Pid, _Path = "/runtime/init/error", E, R, S),
        
        failure(R)
    end.

terminate(Reason, _State, Data) ->
    Fun = shutdown(Data, Reason),

    Fun().

callback_mode() -> [state_functions, state_enter].

%%  State machine

process(enter, _State, Data) ->
    Pid = connection(Data),
    
    Ref = gun:get(Pid, "/2018-06-01/runtime/invocation/next"),
    
    {keep_state, NewData};

process(info, {gun_response, _Pid, Ref, _, _Status = 200, Headers}, Data) ->

    try exec(Data, Event, _Context = context(Headers)) of

        Res -> 
            I = iterator(Data, Res),
            
            if I -> 
                stream(Data, _I = iterator(Data, Res));
            true ->
                submit(Data, Res) 
            end

    catch E:R:S -> 
        report(E, R, S)
        
    after
        %% Perform ENV update os:putenv(?_X_AMZN_TRACE_ID, )
        os:putenv(VarName, Value)
    end,

    %% TODO Perform a garbage collection 

    {repeat_state, Data, []};
    
process(info, {gun_response, _Pid, _Ref, _, _Status = 500, _Headers}, Data) ->
   %% TODO Terminate the runtime

    stop;

process(info, {'DOWN', _MRef, process, _Pid, Reason}, Data) ->
    %% TODO Terminate the runtime

    {stop, Reason, Data}.

%%% Data

-spec data(module(), function(), function()) -> data().
data(Mod, Shutdown, Format) ->
    #data{ module = Mod,
           format = Format, shutdown = Shutdown
         }.

-spec connection(data(), pid()) -> data().
connection(Data, Pid) ->
    Data#data{ connection = Pid }.

-spec connection(data()) -> pid().
connection(Data) ->
    Res = Data#data.connection,
    Res.

-spec streaming(data()) -> boolean().
streaming(Data) ->
    Res = Data#data.streaming,
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
    %% TODO Perfrom decode via Module
    Mod = module(Data),
    
    Res = Mod:exec(Event, Context),
    
    %% TODO Perfrom encode via module
    Res.
    
-spec stacktrace(data(), [term()]) -> [string()].
stacktrace(Data, Term) ->
    Fun = format(Data), true = is_function(Fun),
    
    Res = Fun(Term),
    Res.

%% HTTP

connect(URI) ->
    Host = maps:get(host, URI),
    Port = maps:get(port, URI),
    
    {ok, Pid} = gun:open(Host, Port, _Opts = #{ transport => tcp }),
    {ok, Ret} = gun:await_up(Pid),

    Ret = http,

    Res = Pid,
    Res.

stream(Data, Ret) ->
    %% TODO Generate a runtime error (status, payload)
    ok.

submit(Data, Ret) ->
    %% TODO Generate a runtime error (status, payload)
    %% TODO Respond to the Lambda in a sync mode (report if status 413)
    ok.

report(Pid, Path, E, R, S) ->
    Headers = [{<<"content-type">>, <<"application/json">>}],
    
    Body = #{ stackTrace => stacktrace(S), 
                  
              errorType => E, 
              errorMessage => R
            },
        
    Json = jsx:encode(Body),
        
    Ref = gun:post(Pid, Path, Headers, Json),
        
    {response, nofin, Code, _Headers} = gun:await(Pid, Ref1),
    
    %% TODO Print response body to console
    %% TODO Inspect the status code
    
    %% TODO Generate a runtime error (status, payload)
    
    Res = Code,
    Res.
    
%% Context

-spec context([term()]) -> context().
context(Headers) -> 
    Res = #{},
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