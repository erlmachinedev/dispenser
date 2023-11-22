-module(dispenser).

-define(AWS_LAMBDA_RUNTIME_API, "AWS_LAMBDA_RUNTIME_API").

-define(LAMBDA_TASK_ROOT, "LAMBDA_TASK_ROOT").
-define(_HANDLER, "_HANDLER").

-import(erlbox, [success/2, failure/2]).

-export([name/0]).
-export([start_link/0]).

%-export([init/1]).
%-export([terminate/3]).
%-export([callback_mode/0]).

%-export([initiate/3]).
%-export([transmit/3]).

-export([file/1, method/0]).

-behaviour(gen_statem).

%% TODO Add the ENV list check during the start
%% TODO Compile option (layer support)
%% TODO The usage of _HANDLER, LAMBDA_TASK_ROOT and other ENVs

%% TODO Configuration encode/decode (via callback)

%% TODO setup(path), Fun = shutdown(), is_restored(), exec

%%% API

name() ->
    ?MODULE.

-spec start_link(module(), function()) -> success(pid()).
start_link(Mod, Shutdown) ->
    Name = name(),
    
    Data = data(Mod, Shutdown),
    
    Time = 10000,
    
    gen_statem:start_link({local, Name}, ?MODULE, Data, [{timeout, Time}]).

%% gen_statem

%% gen_statem

-record(data, { switch::function(), socket::function() }).

-type data() :: #data{}.

init([]) ->
    process_flag(sensitive, true),
    
    T = "~p",
    
    URI = uri(),

    ct:print(T, [URI]),

    try setup(Data)

    success(initiate, websocket(_Data = data(F0))).
    
    catch E:R:S ->


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