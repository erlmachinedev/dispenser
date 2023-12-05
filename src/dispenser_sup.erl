-module(dispenser_sup).

-export([start_link/0, start_child/3]).

-export([init/1]).

-behaviour(supervisor).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Mod, Shutdown, Opts) ->
	M = dispenser,
	F = start_link,
	
	A = [Mod, Shutdown, Opts],

	Spec = #{ id => M, significant => true, shutdown => 2000, 
			  
			  restart => transient,
			  start => {M, F, A}
			},

    supervisor:start_child(?MODULE,  Spec).

init([]) ->
	Flags = #{ strategy => one_for_one, auto_shutdown => any_significant,
			   
			   intensity => 1,
			   period => 5
	         },
	
	erlbox:success({Flags, _Procs = []}).
