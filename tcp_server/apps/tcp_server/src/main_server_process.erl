%% File: "main_server_process.erl"
%%%
%%% @author Gattu shivakrishna < shiva.krishna17892@gmail.com >
%%%
%%% @version 1.0.0
%%%
%%% Created : wednesday, July 22 2020.
%%%
%%% @doc
%%% 	A gen server, Haldles Clients connection request and 
%%%     send the requested data to client
%%%
%%% @end
%%%
%%% @copyright 2020, GSK
%%% ==================================================================

-module(main_server_process).
-behaviour(gen_server).

%% ==================================================================
%% EXPORT SECTION
%% ==================================================================
-export([	start_link/0,
		send_request_for_data/1,
		send_message_from_client/1,
		stop/0
		]).


-export([	init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		code_change/3,
		terminate/2
		]).




%% ==================================================================
%% RECORD DEFINITION SECTION
%% ==================================================================


-record(gen_server_state, { server_start_time :: list(),
			    connected_clients :: list(),
			    disconnected_clients :: list() 
			  }
	).


%% ==================================================================
%% APIs
%% ==================================================================

%%-------------------------------------------------------------------
%% 
%% @doc  starts gen server
%%			
%% @spec start_link() -> any() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( start_link() -> any() ).

start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [],[]).
	

%%-------------------------------------------------------------------
%% 
%% @doc  This function used by client to get the requested data from 
%%  	 server. Requested data is to know how many nodes connected to 
%%	 server, disconnected from server and server start time.
%%
%%	Function has one Argument
%%
%%	Arg 1: Request indicates type of data from server
%%	Example : get_connected_nodes, get_disconnected_nodes
%%			
%% @spec send_request_for_data(atom()) -> tuple() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( send_request_for_data( atom()) -> tuple() ).

send_request_for_data(Request) ->

	gen_server:call(?MODULE, Request).


%%-------------------------------------------------------------------
%% 
%% @doc  This function used by client to send message to server.
%%
%%	Function has one Argument
%%
%%	Arg 1: Message indicates information to server
%%	Example : "two users are using the server"
%%			
%% @spec send_message_from_client(list()) -> tuple() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( send_message_from_client( list()) -> tuple() ).
	
send_message_from_client(Message) ->

	?MODULE ! {message, Message},
	
	{success, message_received}.

%%-------------------------------------------------------------------
%% 
%% @doc  initialize gen server
%%			
%% @spec init([]) -> { atom(), list()}
%%
%% @end
%%--------------------------------------------------------------------
%% ====================================================================

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

init([])->
	
	io:format("~n~nserver is started and running~n~n"),
	
	New_state = #gen_server_state {  server_start_time = [erlang:localtime()],
			    		connected_clients =  [],
			    		disconnected_clients = [] },
	{ok, New_state}.

%%-------------------------------------------------------------------
%% 
%% @doc  stops gen server
%%			
%% @spec stop() -> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( stop() -> any() ).

stop()->
	gen_server:cast(?MODULE, stop).


%% ==================================================================
%% Gen Server Function
%% ==================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
%%
%% Handling node up request
%%
%% @end
%% ====================================================================

-spec (handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term()).
%% ====================================================================

handle_call( { node_up, Node_name }, _From, State) ->

     New_state = 
	case lists:keyfind(Node_name, 1, State#gen_server_state.disconnected_clients) of
		false ->
			Updated_connected_clients_list = State#gen_server_state.connected_clients ++ 
							  [{Node_name, [erlang:localtime()] }],
			State#gen_server_state{connected_clients = Updated_connected_clients_list };
		_any_data_found ->
			Updated_disconnected_clients_list = lists:keydelete(Node_name, 1, State#gen_server_state.disconnected_clients),
			Updated_connected_clients_list = State#gen_server_state.connected_clients ++ 
							  [{Node_name, [erlang:localtime()] }],
			State#gen_server_state{connected_clients = Updated_connected_clients_list,
					       disconnected_clients = Updated_disconnected_clients_list }
			
	end,
	
	{ reply, { success, state_updated }, New_state };

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling node down request
%%
%% @spec  handle_call(tuple(), pid(), term()) -> tuple() 
%% @end
%%--------------------------------------------------------------------

handle_call( { node_down, Node_name }, _From, State) ->

	New_state = 
	case lists:keyfind(Node_name, 1, State#gen_server_state.connected_clients) of
		false ->
			Updated_disconnected_clients_list = State#gen_server_state.disconnected_clients ++ 
							  [{Node_name, [erlang:localtime()] }],
							  
			State#gen_server_state{disconnected_clients = Updated_disconnected_clients_list };
			
		_any_data_found ->
			Updated_connected_clients_list = lists:keydelete(Node_name, 1, State#gen_server_state.connected_clients),
			Updated_disconnected_clients_list = State#gen_server_state.disconnected_clients ++ 
							  [{Node_name, [erlang:localtime()] }],
			State#gen_server_state{connected_clients = Updated_connected_clients_list,
					       disconnected_clients = Updated_disconnected_clients_list }
			
	end,
				
	{ reply, { success, state_updated } , New_state };
	
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling get_connected_clients request
%%
%% @spec  handle_call(atom(), pid(), term()) -> tuple() 
%% @end
%%--------------------------------------------------------------------

handle_call( get_connected_clients, _From, State) ->

	Connected_node_list = State#gen_server_state.connected_clients,
	
	{ reply, {success, Connected_node_list}, State };
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling get_disconnected_clients request
%%
%% @spec  handle_call(atom(), pid(), term()) -> tuple() 
%% @end
%%--------------------------------------------------------------------
handle_call( get_disconnected_clients, _From, State) ->

	Disconnected_node_list = State#gen_server_state.disconnected_clients,
	
	{ reply, {success, Disconnected_node_list}, State };
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling get_server_start_time request
%%
%% @spec  handle_call(atom(), pid(), term()) -> tuple() 
%% @end
%%--------------------------------------------------------------------
handle_call( get_server_start_time, _From, State) ->

	Server_time  = State#gen_server_state.server_start_time,
	
	{ reply, {success, Server_time}, State };
	
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
    
handle_call(_any,_from,State) ->

	io:format("default case ~p~n",[_any]),
	
	{reply, ok, State}.

%% handle_cast/2
%% ===================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_cast(_Msg, State) -> 
	{noreply, State}.

%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_info({message, Message_from_client}, State) ->

	io:format("message from client ( ~p ) is received successfully~n",[erlang:binary_to_list(Message_from_client)]),
	
	{noreply, State};
	
handle_info(_Info, State) -> 
	{noreply, State}.

%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================

terminate(_Reason, _State) ->
	 ok.

%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.

