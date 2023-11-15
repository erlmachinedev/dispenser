-module(dispenser).

-define(AWS_LAMBDA_RUNTIME_API, "AWS_LAMBDA_RUNTIME_API").

%-export([name/0]).
%-export([start_link/0]).

%-export([init/1]).
%-export([terminate/3]).
%-export([callback_mode/0]).

%-export([initiate/3]).
%-export([transmit/3]).

-export([uri/0]).

%-behaviour(gen_statem).

%% TODO Add the ENV list check during the start
%% TODO Elaborate compile option (layer support)
%% TODO Elaborate the usage of _HANDLER, LAMBDA_TASK_ROOT and other ENVs

-spec uri() -> string().
uri() -> 
    Env = os:getenv(?AWS_LAMBDA_RUNTIME_API),
    
    Res = uri_string:parse(_Uri = lists:append("http://", Env)),
    Res.
    