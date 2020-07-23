%% File: "tcp_client_process.erl"
%%%
%%% @author Gattu shivakrishna < shiva.krishna17892@gmail.com >
%%%
%%% @version 1.0.0
%%%
%%% Created :Wednesday, July 22 2020
%%%
%%% @doc
%%% 	client process sends request to the server, to get data from server.
%%%
%%% @end
%%%
%%% @copyright 2020, GSK
%%% ==================================================================

-module(tcp_client_process).

%% ==================================================================
%% EXPORT SECTION
%% ==================================================================
-export([	start_link/1,
		get_server_started_time/0,
		get_connected_clients_with_server/0,
		get_disconnected_clients_with_server/0,
		send_message_to_client/1
		]).


-export([	init/1,
		stop/0
		]).




%% ==================================================================
%% MACRO DEFINITION SECTION
%% ==================================================================

-define( TICK_TIME, 5 ). 			%% Ticktime in Second
-define( TICK_TIME_TRANSITION_PERIOD, 2 ).	%% Transition Period
-define( SERVER_NODE_NAME, 'tcp_server@192.168.21.32'). %% Server node name


%% ==================================================================
%% APIs
%% ==================================================================

%%-------------------------------------------------------------------
%% 
%% @doc  starts client process
%%			
%% @spec start_link(atom()) -> tuple() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( start_link(atom()) -> tuple() ).

start_link(Node_name) when erlang:is_atom(Node_name)->

	case whereis( ?MODULE ) of
		undefined ->
			Pid = spawn(?MODULE, init, [Node_name]),
			{ok, Pid};
		Pid	->
			{ok, Pid}
	end;
	
start_link(_anything) ->

	io:format("Enter valid Node_name ('client@10.254.254.254') in atom format~n").


%%-------------------------------------------------------------------
%% 
%% @doc  This function used to send request to serverto get server 
%% 	 start time.
%%
%%	Function has no Argument
%%
%% @spec get_server_started_time() -> atom() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( get_server_started_time( ) -> atom() ).


get_server_started_time() ->

	case rpc:call(?SERVER_NODE_NAME,main_server_process,send_request_for_data,[get_server_start_time]) of
		{success, Time} ->
			[{{Year,Month,Date},{Hr,Min,Second}}] = Time,
			io:format("Server start date is ~p-~p-~p and Time is ~p:~p:~p ~n",[Date,Month,Year,Hr,Min,Second]);
		Reason ->
			io:format("error occurred while requesting server start time, Reason ~p~n",[Reason])
	end.

%%-------------------------------------------------------------------
%% 
%% @doc  This function used to send request to server to get  
%% 	 connected client with server.
%%
%%	Function has no Argument
%%
%% @spec get_connected_clients_with_server() -> atom() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( get_connected_clients_with_server( ) -> atom() ).

get_connected_clients_with_server() ->

	case rpc:call(?SERVER_NODE_NAME,main_server_process,send_request_for_data,[get_connected_clients]) of
		{success, Connected_node_list} ->
			
			io:format("Connected nodes with server are ~p ~n",[Connected_node_list]);
		Reason ->
			io:format("error occurred while requesting connected nodes, Reason ~p~n",[Reason])
	end.
	

%%-------------------------------------------------------------------
%% 
%% @doc  This function used to send request to server to get  
%% 	 disconnected client with server.
%%
%%	Function has no Argument
%%
%% @spec get_disconnected_clients_with_server() -> atom() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( get_disconnected_clients_with_server( ) -> atom() ).
	
get_disconnected_clients_with_server() ->

	case rpc:call(?SERVER_NODE_NAME,main_server_process,send_request_for_data,[get_disconnected_clients]) of
		{success, Disconnected_node_list} ->
			
			io:format("DisConnected nodes with server are ~p ~n",[Disconnected_node_list]);
		Reason ->
			io:format("error occurred while requesting connected nodes, Reason ~p~n",[Reason])
	end.

%%-------------------------------------------------------------------
%% 
%% @doc  This function used to send message to server.
%%
%%	Function has no Argument
%%
%%	Arg 1: Message indicate information to server
%%	Example: <<"two user are sending request to server">>
%%
%%
%% @spec send_message_to_client(binary()) -> atom() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( send_message_to_client( binary()) -> atom() ).

send_message_to_client(Message_for_server) when erlang:is_binary(Message_for_server)->
		
	case rpc:call(?SERVER_NODE_NAME,main_server_process,send_message_from_client,[Message_for_server]) of
		{success, message_received} ->
			io:format("message is received by server~n");
		_anything ->
			io:format("error occured while sending message Reason:~p~n",[_anything])
	end;
	
send_message_to_client(_Message_for_server) ->
	
	Binary_message = <<"Send message in this format">>,
	io:format("Enter valid message in binary format :~p~n",[Binary_message]).
	
%%-------------------------------------------------------------------
%% @hidden
%% @doc  Start_link spawn init process with IP address, Init will
%% 	 create nodename with IPaddress and start client node with
%%	setting cookie for and ticktime. It connects with server with
%%	ping function.
%%
%%	Arg1 node_name 
%%	Example: 'client@10.254.254.254'
%%			
%% @spec init(atom()) -> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( init(atom()) -> any() ).

init( Node_name ) ->
%%	Node_name = string:join(["client",erlang:atom_to_list(Ipaddress)],"@"),
	
	io:format("nodename~p~n",[Node_name]),
	
	net_kernel:start([ Node_name, longnames ]),
	
	erlang:set_cookie(node(), lynkit),
					
	net_kernel:set_net_ticktime( ?TICK_TIME, ?TICK_TIME_TRANSITION_PERIOD ),
	
	erlang:register(?MODULE, self()),
	
	net_adm:ping(?SERVER_NODE_NAME),
	
	io:format(" user can use * 'tcp_client_process' * atom to send message to client~n"),
	
	
	receive 
		stop ->
			net_kernel:stop(), %% Turns a distributed node into a non-distributed node.
			ok
	end.


%%-------------------------------------------------------------------
%% 
%% @doc  stops client process
%%			
%% @spec stop() -> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( stop() -> any() ).

stop()	->
		?MODULE ! stop.

