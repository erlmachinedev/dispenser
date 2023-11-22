-module(dispenser_sup).

-export([start_link/0, start_child/3]).

-export([init/1]).

-behaviour(supervisor).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Mod, Shutdown, Depth) ->
	M = dispenser,
	F = start_link,
	
	A = [Mod, Shutdown, Depth],

	Spec = #{ id => M, start => {M, F, A}, shutdown => 2000 },

    supervisor:start_child(?MODULE,  Spec).

init([]) ->
	Flags = #{ strategy => one_for_one, intensity => 1, period => 5 },
	
	erlbox:success({Flags, _Procs = []}).
