%%% ==================================================================
%%% File: "gen_tcp_client_genserver.erl"
%%%
%%% @author Gattu shivakrishna
%%%
%%% @version 1.0.0
%%%
%%% Created : Saturday, AUGUST 01 2020
%%%
%%% @doc
%%% 	Client genserver will connect to the server with Ip address
%%%	given by user. A socket will be created and communication will
%%%	happen. User can send message to server by user defined function.
%%%	We can create multiple client process by starting genserver with
%%%	different IP address.
%%%
%%% @end
%%%
%%% @copyright 2020, GSK
%%% ==================================================================


-module(gen_tcp_client_genserver).
-behaviour(gen_server).


%% ==================================================================
%% EXPORT SECTION
%% ==================================================================
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([start/1,send_message/1,get_connected_clients_with_server/0,get_disconnected_clients_with_server/0,stop/0]).


%% ====================================================================
%% RECORD SECTION
%% ====================================================================
-record(state, { client_socket }).

%% ====================================================================
%% API functions
%% ====================================================================

%% init/1
%%
%% Function will cconnect to server and sends the message to server.
%% Stores the socket id in state variable. 
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

init([SomeHostInNet]) ->
    
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 5678, 
                                 [binary, {active,true},{packet, 0}]),
    
    Replay = gen_tcp:send( Sock, " I am client "),
    
    io:format("First message from client :~p~n",[Replay]),
    
    {ok, #state{client_socket = Sock}}.


%% handle_call/3
%%
%% Function will take the message from user and sends to the server.
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

handle_call( {message, Message},_From, State) ->
    
    Reply  = gen_tcp:send( State#state.client_socket, Message ),
     
     {reply, Reply, State};
    
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
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
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%%
%% Function will receive the data from server and displays to user.
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

handle_info({tcp, Socket, Data}, State) ->
    
    io:format("data from server(~p) and data is (~p)~n",[ Socket, erlang:binary_to_list(Data)]),
    
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

	io:format("socket is closed, Reason ~p~n",[Reason]),
	gen_tcp:close(State#state.client_socket),
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
%% @doc  starts the client genserver.
%%
%%	Function take one argument in list 
%%	Example : Ipaddress , "192.168.21.32"
%%			
%% @spec start(list()) -> atom() 
%%
%% @end
%%--------------------------------------------------------------------
-spec( start(list()) -> atom() ).

start(IP_address) when erlang:is_list(IP_address)->
	gen_server:start_link({local,?MODULE}, ?MODULE, [IP_address], []);
	
start( _Ip_address) ->

	io:format("please send valid ip address in string format~n~n").

%%-------------------------------------------------------------------
%% 
%% @doc Function will take argument as message and send that message
%%      to handle call of genserver then it will send message to
%%	server 		
%%
%%	Function  have one argument.
%%	Arg1 : Message in string format.
%%	Ex: "iam client"
%%
%% @spec send_message( list() )-> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( send_message( list() ) -> any() ).

send_message( Message ) when erlang:is_list(Message)->
    
	Reply = gen_server:call(?MODULE, {message, Message}),
	
	io:format("Reply from Server ~p~n",[Reply]);

send_message( _Message ) ->

	io:format("please send message in string format~n~n").

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
	
	gen_server:call(?MODULE, {message, "get_connected_clients"}),
	
	io:format("Connected clients in server are below ~n").


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

	gen_server:call(?MODULE, {message, "get_disconnected_clients"}),
	
	io:format("DisConnected clients in server are below~n").

%%-------------------------------------------------------------------
%% 
%% @doc  stops client genserver process and socket will be closed
%%			
%% @spec stop() -> any()
%%
%% @end
%%--------------------------------------------------------------------
-spec( stop() -> any() ).
	
stop() ->
	gen_server:stop(?MODULE).

