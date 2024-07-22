-module(dispenser).

-import(erlbox, [success/3, failure/1, callback/3, callback/4]).

-export([boot/1, boot/2, boot/3]).

-export([start_link/3]).

-export([init/1]).
-export([terminate/3]).
-export([callback_mode/0]).

-export([process/3]).

-behaviour(gen_statem).

-include_lib("erlbox/include/erlbox.hrl").

-type connection() :: pid().
-type command() :: function().

-type json() :: binary().

-type event() :: term().
-type context() :: map().

-type iterator() :: term().

-export_type([event/0, context/0, iterator/0]).

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
    
    Init = fun () -> data(Mod, Commands, Shutdown) end,
    
    gen_statem:start_link({local, Name}, ?MODULE, Init, []).

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

init(Init) when is_function(Init) ->
    process_flag(sensitive, true),
    
    Data = connect(Init(), _URI = uri()),
    
    monitor(process, _Pid = connection(Data)),

    try setup(Data),
    
        success(process, Data, [])
    
    catch E:R:S -> 
        report(Data, _Path = path("/init/error"), E, R, S),
        
        failure(R)
    end.

terminate(Reason, _State, Data) ->
    shutdown(Data, Reason).

callback_mode() -> [state_functions, state_enter].

%%  State machine

process(enter, _State, Data) ->
    _Ref = invoke(Data, _Path = path("/invocation/next")),
    
    {keep_state, Data};

process(info, {gun_response, Pid, Ref, _, _Status = 200, Headers}, Data) ->
    try exec(Data, _Event = event(Pid, Ref), context(Headers)) of

        Json ->
            Path = path(Headers, "/response"),
            
            I = iterator(Data, Json),

            if I == [] -> 
                submit(Data, Json, Path);
            true ->
                stream(Data, I, Path)
            end

    catch E:R:S ->
        report(Data, _Path = path(Headers, "/error"), E, R, S)
        
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
    Def = fun (_) -> [] end,
    
    fun (Json) -> callback(Mod, iterator, [Json], Def) end;

callback(next, Mod) ->
    fun (I) -> callback(Mod, next, [I]) end;

callback(exception, Mod) ->
    Def = fun (E, R, S) -> erl_error:format_exception(E, R, S) end,
    
    fun (E, R, S) -> callback(Mod, exception, [E, R, S], Def) end.

-spec data(module(), [command()], function()) -> data().
data(Mod, Commands, Shutdown) ->
    I = callback(iterator, Mod), Next = callback(next, Mod),
    
    Setup = callback(setup, Mod),
    Exec = callback(exec, Mod),
    
    Exception = callback(exception, Mod),

    #data{ commands = Commands, shutdown = Shutdown,
           
           iterator = I,
           next = Next,
           
           setup = Setup,
           exec = Exec,
           
           exception = Exception
         }.

-spec connection(data(), connection()) -> data().
connection(Data, Pid) ->
    Data#data{ connection = Pid }.

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
exec(Data, Event, Context) ->
    Fun = Data#data.exec,
    
    Res = Fun(Event, Context), 
    Res.

-spec iterator(data(), json()) -> iterator().
iterator(Data, Json) ->
    Fun = Data#data.iterator,
    
    Fun(Json).

-spec next(data(), iterator()) -> {term(), iterator()} | none.
next(Data, I) ->
    Fun = Data#data.next,
    
    Fun(I).

-spec shutdown(data(), term()) -> function().
shutdown(Data, Reason) ->
    Fun = Data#data.shutdown,
    
    Fun(Reason).
    
-spec exception(data(), error | exit | throw, term(), [term()]) -> [string()].
exception(Data, E, R, StackTrace) ->
    Fun = Data#data.exception,
    
    Fun(E, R, StackTrace).

%% HTTP

connect(Data, URI) ->
    Host = maps:get(host, URI),
    Port = maps:get(port, URI),
    
    {ok, Pid} = gun:open(Host, Port, _Opts = #{ transport => tcp }),
    {ok, Ret} = gun:await_up(Pid),

    Ret = http,

    connection(Data, Pid).

path(Info) ->
    Path = ["/2018-06-01/runtime", Info],
    
    unicode:characters_to_list(Path).
    
path(Headers, Info) ->
    Key = <<"lambda-runtime-aws-request-id">>,
    
    path(["/invocation/", _Val = proplists:get_value(Key, Headers), Info]).

invoke(Data, Path) ->
    Pid = connection(Data),

    gun:get(Pid, Path).

stream(Data, I, Path) ->
    Pid = connection(Data),
    
    Headers = [ {<<"Lambda-Runtime-Function-Response-Mode">>, <<"streaming">>}, 
                {<<"Transfer-Encoding">>, <<"chunked">>}
              ],
    
    Ref = gun:post(Pid, Path, Headers),
    
    Fun = fun (IsFin, Term) -> gun:data(Pid, Ref, IsFin, Term) end,
    
    iterate(Data, Fun, next(Data, I)),
    
    Res = gun:await(Pid, Ref),
    
    {response, _IsFin, Code, _} = Res,
    
    Code == 202 orelse error(Code),
    
    gun:await(Pid, Ref).

iterate(Data, Fun, Acc0) ->
    Term = element(1, Acc0),
    
    case next(Data, _I = element(2, Acc0)) of 
    
        none -> 
            Fun(fin, Term);
        Acc1 -> 
            Fun(nofin, Term), iterate(Data, Fun, Acc1) 
    
    end.

submit(Data, Json, Path) ->
    Pid = connection(Data),
    
    Ref = gun:post(Pid, Path, [], Json),
    Res = gun:await(Pid, Ref),
 
    {response, _IsFin, Code, _} = Res,
    
    Code == 202 orelse error(Code),
    
    gun:await(Pid, Ref).

report(Data, Path, E, R, S) ->
    Pid = connection(Data),
    
    Headers = [{<<"content-type">>, <<"application/json">>}],
    
    Body = #{ stackTrace => exception(Data, E, R, S), 
                  
              errorType => E, 
              errorMessage => R
            },
        
    Json = jsx:encode(Body),
    
    Ref = gun:post(Pid, Path, Headers, Json),
    Res = gun:await(Pid, Ref),
    
    {response, _IsFin, Code, _} = Res,
    
    Code == 202 orelse error(Code),

    gun:await(Pid, Ref).

event(Pid, Ref) ->
    T = gun:await_body(Pid, Ref),
    
    {ok, Event} = T,
    
    Res = Event,
    Res.
    
%% Context

-spec context([term()]) -> map().
context(Headers) -> 
    maps:from_list(Headers).

%% ENV

-spec uri() -> string().
uri() -> 
    Env = os:getenv("AWS_LAMBDA_RUNTIME_API"),
    
    URI = lists:append("http://", Env),
    
    uri_string:parse(URI).
    
-spec root() -> file:filename().
root() ->
    os:getenv("LAMBDA_TASK_ROOT").
