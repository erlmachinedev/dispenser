-module(dispenser).

-define(AWS_LAMBDA_RUNTIME_API, "AWS_LAMBDA_RUNTIME_API").

-define(LAMBDA_TASK_ROOT, "LAMBDA_TASK_ROOT").
-define(_HANDLER, "_HANDLER").

-import(erlbox, [success/3, failure/1, callback/3, callback/4]).

-export([boot/1, boot/2, boot/3]).

-export([start_link/3]).

-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

-export([process/3]).

-export([file/0, method/0]).

-behaviour(gen_statem).

-include_lib("erlbox/include/erlbox.hrl").

-type connection() :: pid().
-type command() :: function().

-type json() :: binary().

-type event() :: term().
-type context() :: map().

-type iterator() :: term().

-callback setup() -> ok.

-callback decode(json()) -> term().
-callback encode(term()) -> json().

-callback exec(event(), context()) -> term().

-callback iterator(json()) -> iterator().
-callback next(iterator()) -> {term(), iterator()} | none.

-callback exception(error | exit | throw, term(), [term()]) -> [string()].

-optional_callbacks([decode/1, encode/1, iterator/1, next/1, exception/3]).

%%% API

boot(Mod) ->
    Command = fun () -> erlang:garbage_collect(), ok end,

    boot(Mod, [Command]).

boot(Mod, Commands) ->
    Shutdown = fun (_) -> init:stop() end,
    
    boot(Mod, Commands, Shutdown).

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
        report(Data, _Path = path("/init/error"), E, R, S),
        
        failure(R)
    end.

terminate(Reason, _State, Data) ->
    Fun = shutdown(Data),
    
    ct:print("Terminated ~tp", [Data]),
    Fun(Reason).

callback_mode() -> [state_functions, state_enter].

%%  State machine

process(enter, _State, Data) ->
    _Ref = invoke(Data, _Path = path("/invocation/next")),
    
    ct:print("Ref ~p", [_Ref]),
    
    {keep_state, Data};

process(info, {gun_response, _Pid, Ref, _, _Status = 200, Headers}, Data) ->

    ct:print("Headers ~p", [Headers]),
    
    try exec(Data, _Body = body(Data, Ref), context(Headers)) of

        Json ->
        
            ct:print("Json ~p", [Json]),
            
            Path = path(Headers, "/response"),
            
            I = iterator(Data, Json),

             ct:print("I ~tp", [I]),

            if I -> 
                ct:print("I ~tp", [I]),
            
                stream(Data, _I = next(Data, I), Path);
            true ->
                submit(Data, Json, Path)
            end

    catch E:R:S ->
    
        ct:print("Catch ~p", [[E, R, S]]),
        
        report(Data, _Path = path(Headers, "/error"), E, R, S)
        
    after
        [ begin ok = Command(),
                ok
                
          end || Command <- commands(Data)
        ]
        
    end,

    {repeat_state, Data, []};

process(info, {gun_response, _Pid, _Ref, _, 500, _Headers}, _Data) ->
    ct:print("Stop message ~tp", [test]),

    stop;

process(info, {'DOWN', _MRef, process, _Pid, Reason}, Data) ->
    {stop, Reason, Data}.

%%% Data

-spec callback(atom(), module()) -> function().
callback(setup, Mod) ->
    fun () -> callback(Mod, setup, []) end;
    
callback(decode, Mod) ->
    Def = fun (Json) -> jsx:decode(Json) end,
    
    fun (Json) -> callback(Mod, decode, [Json], Def) end;

callback(encode, Mod) ->
    Def = fun (Term) -> jsx:encode(Term) end,
    
    fun (Term) -> callback(Mod, encode, [Term], Def) end;

callback(exec, Mod) ->
    Enc = callback(encode, Mod),
    Dec = callback(decode, Mod),
    
    fun (Json, Context) -> Event = Dec(Json),
                           Res = callback(Mod, exec, [Event, Context]),
                           
                           Enc(Res)
    end;

callback(iterator, Mod) ->
    Def = fun (_) -> false end,
    
    fun (Json) -> callback(Mod, iterator, [Json], Def) end;

callback(next, Mod) ->
    fun (I) -> callback(Mod, next, [I]) end;

callback(exception, Mod) ->
    Def = fun (E, R, S) -> erl_error:format_exception(E, R, S) end,
    
    fun (E, R, S) -> callback(Mod, exception, [E, R, S], Def) end.

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

-spec commands(data()) -> [command()].
commands(Data) ->
    Data#data.commands.

-spec setup(data()) -> success().
setup(Data) ->
    Fun = Data#data.setup,
    
    Fun().

-spec exec(data(), json(), context()) -> json().
exec(Data, Body, Context) ->
    Fun = Data#data.exec,
    
    Res = Fun(Body, Context), ct:print("Exec ~p", [Res]),
    Res.

-spec iterator(data(), json()) -> iterator().
iterator(Data, Json) ->
    Fun = Data#data.iterator,
    
    Fun(Json).

-spec next(data(), iterator()) -> {term(), iterator()} | none.
next(Data, I) ->
    Fun = Data#data.next,
    
    Fun(I).

-spec shutdown(data()) -> function().
shutdown(Data) ->
    Data#data.shutdown.
    
-spec exception(data(), error | exit | throw, term(), [term()]) -> [string()].
exception(Data, E, R, Stacktrace) ->
    Fun = Data#data.exception,
    
    Fun(E, R, Stacktrace).

%% HTTP

connect(URI) ->
    Host = maps:get(host, URI),
    Port = maps:get(port, URI),
    
    {ok, Pid} = gun:open(Host, Port, _Opts = #{ transport => tcp }),
    {ok, Ret} = gun:await_up(Pid),

    Ret = http,

    Res = Pid,
    Res.

path(Info) ->
    Path = ["/2018-06-01/runtime", Info],
    
    unicode:characters_to_list(Path).
    
path(Headers, Info) ->
    Key = <<"lambda-runtime-aws-request-id">>,
    
    path(["/invocation/", _Val = proplists:get_value(Key, Headers), Info]).

invoke(Data, Path) ->
    gun:get(_Pid = connection(Data), Path).

stream(Data, I, Path) ->
    Pid = connection(Data),
    
    Name0 = <<"Lambda-Runtime-Function-Response-Mode">>,
    Name1 = <<"Transfer-Encoding">>,
    
    Headers = [{Name0, <<"streaming">>}, {Name1, <<"chunked">>}],
    
    Ref = gun:post(Pid, Path, Headers),
    
    ct:print("Stream ~tp", [gun:data(Pid, Ref, nofin, <<"Bonjour !\n">>)]),
    ct:print("Stream ~tp", [gun:data(Pid, Ref, fin, <<"Bonsoir !\n">>)]),
    
    ct:print("submit ~tp", [gun:await(Pid, Ref)]).

submit(Data, Json, Path) ->
    Pid = connection(Data),
    
    Ref = gun:post(Pid, Path, [], Json),
    Res = gun:await(Pid, Ref),
 
    {response, _IsFin, Code, _} = Res,
    
    ct:print("Res ~p", [Res]),
    
    Code == 202 orelse error(Code),
    
    ct:print("submit ~tp", [gun:await(Pid, Ref)]).

report(Data, Path, E, R, S) ->
    Pid = connection(Data),
    
    Headers = [{_Name = <<"content-type">>, <<"application/json">>}],
    
    Body = #{ stackTrace => exception(Data, E, R, S), 
                  
              errorType => E, 
              errorMessage => R
            },
        
    Json = jsx:encode(Body),
    
    ct:print("Json ~tp", [Json]),
    
    Ref = gun:post(Pid, Path, Headers, Json),
    Res = gun:await(Pid, Ref),
        
    ct:print("Report ~tp", [Res]),
    
    {response, _IsFin, Code, _} = Res,
    
    Code == 202 orelse error(Code),

    ct:print("report ~tp", [gun:await(Pid, Ref)]).

body(Data, Ref) ->
    Pid = connection(Data),
    
    T = gun:await_body(Pid, Ref), ct:print("Body ~tp", [T]),
    
    {ok, Body} = T,
    
    Res = Body,
    Res.
    
%% Context

-spec context([term()]) -> map().
context(Headers) -> 
    maps:from_list(Headers).

%% ENV

-spec uri() -> string().
uri() -> 
    Env = os:getenv(?AWS_LAMBDA_RUNTIME_API),
    
    URI = lists:append("http://", Env),
    
    uri_string:parse(URI).
    
-spec root() -> file:filename().
root() ->
    os:getenv(?LAMBDA_TASK_ROOT).

-spec file() -> file:filename().
file() ->
    [File, _Method] = string:lexemes(_Env = os:getenv(?_HANDLER), "."),

    filename:join(_Root = root(), File).

-spec method() -> string().
method() ->
    Env = os:getenv(?_HANDLER),
    
    [_File, Method] = string:lexemes(Env, "."),

    Res = Method,
    Res.
