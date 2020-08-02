gen_tcp_server
==============

They are two process, one server and other client. server will listen for client connection and
send/receive will be happen. any number of clients can connect to server. client can get client
conected and disconnected from server, it can also send message to server. server also can send 
commands to clients.



Build and Run server code
-------------------------

*Step 1* : Compile the server file
1> c(gen_tcp_server_genserver).
{ok,gen_tcp_server_genserver}

*Step 2* : start the server by below command.
2>gen_tcp_server_genserver:start().
in accept loop

{ok,<0.70.0>}

*Step 3* :  **Server is started and running !!**

*Step 4* : If any client is connected to server, client info is display

connected Clients ["#Port<0.2079>",","]; Disconnected clients []

in accept loop

Message received from Client is (<<" I am client ">>)

*Step 5* : To send any command to cliets shown below

3> gen_tcp_server_genserver:send_command_to_clients("ls").
command sent to sent to all clients 

in info ["#Port<0.2079>",",","#Port<0.2080>",","]
ok

*Step 6* : If server receives any message from client.

Message received from Client is (<<" I am client ">>)

*Step 7* :To stop the server process, use below command.

4> gen_tcp_client_genserver:stop().
Listen socket is closed Reason normal
socket is closed, Reason{error,closed}


 
Build and Run client code
-------------------------

*Step 1* : Open other terminal and compile the client file.
1> c(gen_tcp_client_genserver).
{ok,gen_tcp_client_genserver}

*Step 2* : start client process as shown below

2> gen_tcp_client_genserver:start("192.168.21.32").
First message from client :ok
{ok,<0.66.0>}
data from server(#Port<0.434>) and data is ("connected successfully")

*Step 3* : Client is connected, get number of clients connected to server as shown below.

3> gen_tcp_client_genserver:get_connected_clients_with_server().
Connected clients in server are below 
data from server(#Port<0.434>) and data is ("#Port<0.2079>,#Port<0.2080>,")
ok

*Step 4* : Get number of clients disconnected from server as shown below

4> gen_tcp_client_genserver:get_disconnected_clients_with_server(). %% when no clients are disconnected
DisConnected clients in server are below
data from server(#Port<0.434>) and data is ("no disconnected clients") 
ok
5> gen_tcp_client_genserver:get_disconnected_clients_with_server(). %% when one client is disconnected
DisConnected clients in server are below
data from server(#Port<0.434>) and data is ("#Port<0.2079>,")


*Step 5* : To send some message to server use below command.
6> gen_tcp_client_genserver:send_message("user shiva is using this process").
Reply from Server ok
ok
data from server(#Port<0.2074>) and data is ("message received")

*Step 6* :To stop the client process, use below command.

7> gen_tcp_client_genserver:stop().
socket is closed, Reason normal


