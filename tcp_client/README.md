tcp_client
==========

Server have information like connected clinent nodes and disconnected client nodes
when clients connects with sever, client get the connected client,disconnected clients,server start time
from server and client can send message to server.



 
Build and Run client code
-------------------------

*Step 1* : Open a terminal in tcp_client folder,

*Step 2* : Run erl in terminal to start erlang shell.

shiva@shiva:-/tcp_client$ erl

*Step 3* : To Compile the client  code use below command.

 c(tcp_client_process).
{ok,tcp_client_process}

*Step 4* : To start client node, use below command and give a valid node name.

2> tcp_client_process:start_link('client1@10.254.254.254').
nodename'client1@10.254.254.254'
{ok,<0.67.0>}
 user can use * 'tcp_client_process' * atom to send message to client

*Step 5* : To get connected client node information use below command.

(client1@10.254.254.254)3> tcp_client_process:get_connected_clients_with_server().

Connected nodes with server are [{'client1@10.254.254.254',
                                     [{{2020,7,23},{7,55,16}}]},
                                 {'client2@10.254.254.253',
                                     [{{2020,7,23},{7,55,34}}]}]
*Step 6* :To get disconnected client node information use below command.

(client1@10.254.254.254)4> tcp_client_process:get_disconnected_clients_with_server().
DisConnected nodes with server are [{'client@10.254.254.254',
                                     [{{2020,7,23},{7,54,6}}]}]

*Step 7* : To get server start time use below command

(client1@10.254.254.254)5> tcp_client_process:get_server_started_time().
Server start date is 23-7-2020 and Time is 7:38:13

*Step 8* : To send message to server use below command.

(client@10.254.254.254)6> tcp_client_process:send_message_to_client(<<"two users are using server">>).
message is received by server
ok



