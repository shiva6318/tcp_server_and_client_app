%%% ==================================================================
%%% File: "gen_tcp_server_genserver.erl"
%%%
%%% @author Gattu shivakrishna
%%%
%%% @version 1.0.0
%%%
%%% Created : Saturday, AUGUST 01 2020
%%%
%%% @doc
%%% 	Server starts and listens for client request. If any
%%%     client connect to server. server will create a socket 
%%%     for that client and communication to that client and server
%%%	will happen with that socket. Server can send specific 
%%%     command to all sockets.
%%%
%%% @end
%%%
%%% @copyright 2020, GSK
%%% ==================================================================

-module(gen_tcp_server_genserver).
-behaviour(gen_server).

%% ==================================================================
%% EXPORT SECTION
%% ==================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([start/0,
	send_and_receive_msg_from_client/1,
	stop/0,
	send_command_to_clients/1,
	wait_for_new_clients/1]).

%% ====================================================================
%% RECORD SECTION
%% ====================================================================
-record(state, { listen_socket :: term(), 
		 connected_client_socket_list :: list(),
		 disconnected_client_socket_list :: list()
	       }).


%% ====================================================================
%% API functions
%% ====================================================================

%% init/1
%%
%% Function will listen for client connection.
%%
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    
    {ok, LSock} = gen_tcp:listen(5678, [binary, {active, false},{packet, 0}]),
    
    gen_server:cast(self(), accept),
    
    {ok, #state{listen_socket = LSock,connected_client_socket_list =[], disconnected_client_socket_list = []}}.


%% handle_call/3
%%
%% Function will take socket request and add client socket id in
%% state variable.
%%
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
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
	Reason :: term().
%% ====================================================================
handle_call( {add_client_socket, Socket}, _From, State) ->
    
    Updated_connected_client_list = State#state.connected_client_socket_list ++ [erlang:port_to_list(Socket),","] ,
    Updated_disconnected_client_list = State#state.disconnected_client_socket_list -- [erlang:port_to_list(Socket),","],
    
    io:format(" connected Clients ~p; Disconnected clients ~p~n~n",[Updated_connected_client_list,Updated_disconnected_client_list]),
    
    New_state= State#state{connected_client_socket_list = Updated_connected_client_list,
    	                   disconnected_client_socket_list = Updated_disconnected_client_list},  
    
    {reply, ok, New_state}; 
    

handle_call( {remove_client_socket, Socket}, _From, State) ->
    
    Updated_connected_client_list = State#state.connected_client_socket_list -- [erlang:port_to_list(Socket),","],
    Updated_disconnected_client_list = State#state.disconnected_client_socket_list ++ [erlang:port_to_list(Socket),","],
    
    io:format(" connected Clients ~p; Disconnected clients ~p~n~n",[Updated_connected_client_list,Updated_disconnected_client_list]),
        
    New_state= State#state{connected_client_socket_list = Updated_connected_client_list,
    			   disconnected_client_socket_list = Updated_disconnected_client_list},  
    
    {reply, ok, New_state}; 
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling get_connected_clients request
%%
%% @spec  handle_call(atom(), pid(), term()) -> tuple() 
%% @end
%%--------------------------------------------------------------------

handle_call( <<"get_connected_clients">>, _From, State) ->
		
	Connected_client_list = State#state.connected_client_socket_list,
	
	{ reply, Connected_client_list, State };
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling get_disconnected_clients request
%%
%% @spec  handle_call(atom(), pid(), term()) -> tuple() 
%% @end
%%--------------------------------------------------------------------
handle_call( <<"get_disconnected_clients">>, _From, State) ->

	Disconnected_client_list = State#state.disconnected_client_socket_list,
	
	Reply =
	case Disconnected_client_list of
	   [] -> "no disconnected clients";
	   _any ->
	   	Disconnected_client_list
	   end,
	
	{ reply, Reply, State };
	
handle_call(Message, _From, State) ->

    io:format("Message received from Client is (~p)~n~n",[Message]),
    
    {reply, "message received", State}.


%% handle_cast/2
%%
%% Function will receive accpet message and spawn a seperate process
%% for accept the client connection and create a socket for clients
%%
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_cast( accept, State ) ->
    
    erlang:spawn(?MODULE,wait_for_new_clients,[State#state.listen_socket]),
     
    {noreply, State};
    
handle_cast( Msg, State) ->
    
    io:format("Unexpected Message ~p~n",[Msg]),
    
    {noreply, State}.




%% handle_info/2
%%
%% Function will receive the command which server will sends to each
%% client connected to server.
%%
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

handle_info( {command, Command}, State) ->

	io:format("in info ~p~n",[State#state.connected_client_socket_list]),
	
	Fun =fun(Client_socket) ->
		case Client_socket of
			"," ->do_nothing;
			_port ->
				gen_tcp:send(erlang:list_to_port(Client_socket), Command)
		end
	     end,
	lists:foreach(Fun,State#state.connected_client_socket_list),
	
	{noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

handle_info({tcp_error, _Socket, _}, State) ->
	{stop, normal, State};

handle_info(Info, State) ->
    io:format("unexpected: ~p~n", [Info]),
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
terminate(Reason, State) ->
	io:format("Listen socket is closed Reason ~p~n",[Reason]),
	gen_tcp:close(State#state.listen_socket),
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

%% ====================================================================
%% Internal functions
%% ====================================================================

%%-------------------------------------------------------------------
%% 
%% @doc  starts the sever.
%%
%%	Function have no argument 
%%			
%% @spec start() -> any() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( start() -> any() ).

start() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).
	

%%-------------------------------------------------------------------
%% 
%% @doc Function will take argument as socket and wait for message 
%%      from client. once it receive the message it prints the message
%%      and wait for other client message.
%%
%%	Function  have one argument.
%%	Arg1 : socket.
%%	Ex: <<0.203.0>>
%%
%% @spec send_and_receive_msg_from_client( term() )-> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( send_and_receive_msg_from_client( term() ) -> any() ).
	
send_and_receive_msg_from_client( Socket ) ->

   case gen_tcp:recv(Socket, 0) of
        {ok, Message} ->
        	
        	Reply = gen_server:call( ?MODULE, Message ),
        	
        	gen_tcp:send(Socket, Reply),
       		send_and_receive_msg_from_client( Socket );
       		
        {error, _closed} ->
        	gen_server:call(?MODULE,  {remove_client_socket, Socket}),
        	io:format("client(~p) socket is closed ~p~n",[Socket,_closed]),
        	ok = gen_tcp:close(Socket),
            	{ok, closed}
    end.

%%-------------------------------------------------------------------
%% 
%% @doc  stops server process and socket will be closed
%%			
%% @spec stop() -> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( stop() -> any() ).
   
stop() ->

	gen_server:stop(?MODULE, normal, 1000).


%%-------------------------------------------------------------------
%% 
%% @doc Function will take argument as list and Send that command to
%%      handle_info. It will take all clients socket details and sends
%%      command to each socket.
%%
%%	Function  have one argument.
%%	Arg1 : command in string format.
%%	Ex:  "ls"
%%
%% @spec send_command_to_clients( list() )-> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( send_command_to_clients( list() ) -> any() ).
	
send_command_to_clients(Command) ->

	?MODULE ! {command, Command},
	
	io:format("command sent to sent to all clients ~n~n").


%%-------------------------------------------------------------------
%% 
%% @doc Function will take argument as socket and wait for client
%%      connection. if any client is connected it wil create a socket
%%      for that client and send that socket details to handle_call
%%      to store in state varible. waits for other client connection
%%
%%	Function  have one argument.
%%	Arg1 : Socket.
%%	Ex:   <<0.502.0>>
%%
%% @spec wait_for_new_clients( term() )-> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( wait_for_new_clients( term() ) -> any() ).
	
wait_for_new_clients(Listen_socket) ->

    io:format("in accept loop~n~n"),
    
    case gen_tcp:accept(Listen_socket) of
    
       {ok, Accept_socket} ->
    
	    gen_tcp:send(Accept_socket, "connected successfully"),
	    
	    gen_server:call(?MODULE, {add_client_socket, Accept_socket}),
	    
	    erlang:spawn(?MODULE, send_and_receive_msg_from_client, [Accept_socket]),
	    
	    wait_for_new_clients(Listen_socket);
      _error ->
      	   io:format("socket is closed, Reason~p~n~n",[_error])
   end.
      
      
      
