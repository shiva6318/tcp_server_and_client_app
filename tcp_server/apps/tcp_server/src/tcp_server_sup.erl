%%%-------------------------------------------------------------------
%% @doc tcp_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define( TICK_TIME, 5 ). 			%% Ticktime in Second
-define( TICK_TIME_TRANSITION_PERIOD, 2 ).	%% Transition Period


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

init([]) ->
    
    net_kernel:set_net_ticktime( ?TICK_TIME, ?TICK_TIME_TRANSITION_PERIOD ),
    
    Main_server = { main_server_process, 
    		    {main_server_process, start_link, []},
    		    permanent,
    		    2000,
    		    worker,
    		    [main_server_process] 
    		  },
   
   Node_monitor = { node_monitoring_process,
   		    {node_monitoring_process, start_link, []},
   		    permanent,
   		    2000,
   		    worker,
   		    [node_monitoring_process]
   		  },
   		 
    ChildSpecs = [ Main_server, Node_monitor],
    
    {ok,{{one_for_one, 5, 10}, ChildSpecs}}.


