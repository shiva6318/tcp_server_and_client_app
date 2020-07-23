%%% ==================================================================
%%% File: "node_monitoring_process.erl"
%%%
%%% @author Gattu shivakrishna
%%%
%%% @version 1.0.0
%%%
%%% Created : Wednesday, July 22 2020
%%%
%%% @doc
%%% 	A Process, Handles Erlang kernel node down and node up message 
%%%     and send request to main_server to handle client connection 
%%%	and disconnects
%%%
%%% @end
%%%
%%% @copyright 2020, GSK
%%% ==================================================================

-module(node_monitoring_process).

%% ==================================================================
%% EXPORT SECTION
%% ==================================================================
-export([ start_link/0, stop/0, init/0 ]).

-export([ 
		node_monitor_loop/0
		]).


%%-------------------------------------------------------------------
%% 
%% @doc  starts node Monitoring process
%%			
%% @spec start_link() -> any() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( start_link() -> any() ).

start_link()	->
	case whereis( ?MODULE ) of
		undefined ->
			Pid = spawn_link(?MODULE, init, []),
			{ok, Pid};
		Pid	->
			{ok, Pid}
	end.

%%-------------------------------------------------------------------
%% 
%% @doc  stops node  Monitoring process
%%			
%% @spec stop() -> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( stop() -> any() ).

stop()	->
		?MODULE ! stop.

%%-------------------------------------------------------------------
%% @hidden
%% @doc  initialize node Monitoring process
%%			
%% @spec init() -> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( init() -> any() ).

init()	->
	register(?MODULE, self()),
	
	
	io:format("~n~n node monitoring is started and running ~n~n"),
	
	net_kernel:monitor_nodes( true, [ {node_type, all}, nodedown_reason ] ),
	
	node_monitor_loop().

%%-------------------------------------------------------------------
%% @hidden
%% @doc  Looping function of process, receives Erlang kernel Node 
%%	 Down and node up messages and call main_server_process 
%%	functions to update the state for client node information
%%			
%%	Function doesn't have any arguments.
%%
%% @spec node_monitor_loop() -> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( node_monitor_loop() -> any() ).

node_monitor_loop()	->

	receive
	
		{nodeup,  Node_name, _Reason } ->
		
			io:format("~n[ ~w ] : Node ( ~p ) is connected  ~n~n", [ ?MODULE, Node_name ]),
			
			gen_server:call(main_server_process, {node_up,Node_name}),
			
			node_monitor_loop();
		
		{nodedown, Node_name, _Reason} -> 
			
			io:format("~n[ ~w ] : Node  ( ~p ) is disconnected from server and Reason is ~p~n~n", [ ?MODULE, Node_name, _Reason ]),
			
			gen_server:call(main_server_process, { node_down, Node_name }),
			
			
			node_monitor_loop();
			
		
		stop ->
			{ ok, stopped };
		
		Unknown ->
		
			io:format("~n[ ~w ] : some unknown message : ~p  ~n~n", [ ?MODULE, Unknown ]),
			
			
			node_monitor_loop()
	end.


