-module(dispenser_sup).

-export([start_link/0, start_child/1, start_child/2]).

-export([init/1]).

-behaviour(supervisor).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Mod) ->
	start_child(Mod, _Fun = fun io:write/1).

start_child(Mod, Fun) ->
	M = dispenser,
	F = start_link,
	
	A = [Mod, Fun],

	Spec = #{ id => M, start => {M, F, A}, shutdown => 2000 },

    supervisor:start_child(?MODULE,  Spec).

init([]) ->
	Flags = #{ strategy => one_for_one, intensity => 1, period => 5 },
	
	%% TODO 2000 ms timeout to complete the termination
	%% TODO Timeout is infinity by default
	
	erlbox:success({Flags, _Procs = []}).
