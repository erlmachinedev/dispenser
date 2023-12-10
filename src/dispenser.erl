-module(dispenser).

-define(AWS_LAMBDA_RUNTIME_API, "AWS_LAMBDA_RUNTIME_API").

-define(LAMBDA_TASK_ROOT, "LAMBDA_TASK_ROOT").
-define(_HANDLER, "_HANDLER").

-import(erlbox, [success/3, failure/1, callback/4]).
-import(erlang, [error/2, garbage_collect/0]).

-export([boot/1, boot/2, boot/3]).

-export([start_link/3]).

-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

-export([process/3]).

-export([file/1, method/0]).

-behaviour(gen_statem).

-include_lib("erlbox/include/erlbox.hrl").

-type connection() :: pid().
-type command() :: function(() -> ok).

-type iterator() :: term().

%% NOTE Client can generate more readable runtime exceptions via erlang:error/3 

%%% API

boot(Mod) ->
    Command = fun () -> garbage_collect(), ok end,

    boot(Mod, [Command]).

boot(Mod, Commands) ->
    boot(Mod, Commands, _Shutdown = fun init:stop/0).

-spec boot(module(), [command()], function()) -> success(pid()).
boot(Mod, Commands, Shutdown) when is_atom(Mod),
                                   is_list(Commands),
                                         
                                   is_function(Shutdown) ->
                                
    dispenser_sup:start_child(Mod, Commands, Shutdown).

-spec start_link(module(), [command()], function()) -> success(pid()).
start_link(Mod, Commands, Shutdown) ->
    Name = ?MODULE,
    
    Args = [Mod, Commands, Shutdown],
    
    gen_statem:start_link({local, Name}, ?MODULE, Args, []).

%% gen_statem

-record(data, { connection::connection(), commands::[command()],

                shutdown::function(),

                iterator::function(),
                next::function(), 
                
                setup::function(),
                exec::function(),
                
                exception::function()
              }).

-type data() :: #data{}.

init([Mod, Commands, Shutdown]) ->
    process_flag(sensitive, true),
    
    Pid = connect(_URI = uri()),
    
    monitor(process, Pid),

    Data = data(Mod, Pid, Commands, Shutdown),

    try setup(Data),
    
        success(process, Data, [])
    
    catch E:R:S -> 
        report(Pid, _Path = "/init/error", E, R, S),
        
        failure(R)
    end.

terminate(Reason, _State, Data) ->
    Fun = shutdown(Data),
    
    Fun(Reason).

callback_mode() -> [state_functions, state_enter].

%%  State machine

process(enter, _State, Data) ->
    Pid = connection(Data),
    
    invocation(Pid, _Path = "/invocation/next"),
    
    {keep_state, Data};

process(info, {gun_response, Pid, Ref, _, _Status = 200, Headers}, Data) ->
    try exec(Data, _Event = event(Pid, Ref), context(Headers)) of

        Json ->
            Path = path(Headers, "/response"),
            
            I = iterator(Data, Json),

            if I -> 
                stream(Data, _I = next(Data, I), Path);
            true ->
                submit(Data, Json, Path) 
            end

    catch E:R:S ->
        report(Pid, _Path = path(Headers, "/error"), E, R, S)
        
    after
        [ begin ok = Command(),
                ok
                
          end || Command <- commands(Data)
        ]
        
    end,

    {repeat_state, Data, []};
    
process(info, {gun_response, _Pid, _Ref, _, 500, _Headers}, _Data) ->
    stop;

process(info, {'DOWN', _MRef, process, _Pid, Reason}, Data) ->
    {stop, Reason, Data}.

%%% Data

-spec callback(callback(), module()) -> function().
callback(setup, Mod) ->
    Def = fun () -> error(not_implemented) end,
    
    fun () -> callback(Mod, setup, [], Def) end;
    
callback(decode, Mod)) ->
    Def = fun (Json) -> jsx:decode(Json) end,
    
    fun (Json) -> callback(Mod, decode, [Json], Def) end;

callback(encode, Mod) ->
    Def = fun (Term) -> jsx:encode(Term) end,
    
    fun (Term) -> callback(Mod, encode, [Term], Def) end;

callback(exec, Mod) ->
    Def = fun (Json, Context) -> error(not_implemented, [Json, Context]) end,
    
    Enc = encode(Mod),
    Dec = decode(Mod),
    
    fun (Json, Context) -> Event = Dec(Json),
                           Res = callback(Mod, exec, [Event, Context], Def),
                           
                           Enc(Res)
    end;

callback(iterator, Mod) ->
    Def = fun (_) -> false end,
    
    Res = fun (Json) -> callback(Mod, iterator, [Json], Def) end,
    Res;

callback(next, Mod) ->
    Def = fun (I) -> error(not_implemented, [I]) end,
    
    Res = fun (I) -> callback(Mod, next, [I]) end,
    Res;

callback(exception, Mod) ->
    Def = fun (E, R, S) -> erl_error:format_exception(E, R, S) end,
    
    Res = fun (E, R, S) -> callback(Mod, exception, [E, R, S]) end,
    Res.

-spec data(module(), pid(), [command()], function()) -> data().
data(Mod, Pid, Commands, Shutdown) ->
    I = callback(iterator, Mod), Next = callback(next, Mod),
    
    Setup = callback(setup, Mod),
    Exec = callback(exec, Mod),
    
    Exception = callback(exception, Mod),

    #data{ commands = Commands, shutdown = Shutdown, connection = Pid,
           
           iterator = I,
           next = Next,
           
           setup = Setup,
           exec = Exec,
           
           exception = Exception
         }.

-spec connection(data()) -> pid().
connection(Data) ->
    Data#data.connection.

-spec setup(data()) -> success().
setup(Data) ->
    Fun = Data#data.setup,
    
    Fun().

-spec exec(data(), iodata(), context()) -> iodata().
exec(Data, Json, Context) ->
    Fun = Data#data.exec,
    
    Fun(Json, Context).

-spec iterator(data(), iodata()) -> iterator().
iterator(Data, Json) ->
    Fun = Data#data.iterator,
    
    Fun(Json).

-spec next(data(), iterator()) -> {iodata(), iterator()} | none.
next(Data, I) ->
    Fun = Data#data.next,
    
    Fun(I).

-spec shutdown(data()) -> function().
shutdown(Data) ->
    Data#data.shutdown.
    
-spec exception(data(), error | exit | throw, term(), [term()]) -> [string()].
exception(Data, E, R, Stacktrace) ->
    Fun = exception(Data),
    
    Res = Fun(E, R, Stacktrace),
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

path(Headers) ->
    ReqId = proplists:get_value(<<"lambda-runtime-aws-request-id">>, Headers),
    
    Path = ["/2018-06-01/runtime/invocation/", ReqId, "/response"],

    unicode:characters_to_list(Path).

stream(Data, I, Headers) ->
    Path = path(Headers),
    
    %% TODO Generate a runtime error (status, payload)
    
    error(not_implemented, [I, Headers]).

submit(Data, Json, Headers) ->
    Pid = connection(Data),
    
    Ref = gun:post(Pid, _Path = path(Headers), [], Json),
    Res = gun:await(Pid, Ref),
 
    {response, _IsFin, Code, _} = Res,
    
    Code == 202 orelse error(Code),
    
    %% TODO Check the pressence of a message
    
    gun:flush(Ref).

report(Pid, Path, E, R, S) ->
    Headers = [{<<"content-type">>, <<"application/json">>}],
    
    Body = #{ stackTrace => exception(Data, E, R, S), 
                  
              errorType => E, 
              errorMessage => R
            },
        
    Json = jsx:encode(Body),
        
    Ref = gun:post(Pid, Path, Headers, Json),
        
    {response, nofin, Code, Headers} = gun:await(Pid, Ref),
    
    Res = gun:await(Pid, Ref),
    
    T = "~p",
    
    ct:print(T, [Code, Headers, Res]).
    %% TODO Print response body to console
    %% TODO Inspect the status code
    
    %% TODO Generate a runtime error (status, payload)

%% Event
    
event(Pid, Ref) ->
    {ok, Body} = gun:await_body(Pid, Ref),
    
    Res = Body,
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
