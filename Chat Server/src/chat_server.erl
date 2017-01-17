%%%-------------------------------------------------------------------
%%% @author mohit
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Concurrent Application which takes events as Input from client and notifies clients when deadline approaches.
%%% @end
%%% Created : 17. Jan 2017 5:51 PM
%%%-------------------------------------------------------------------
-module(chat_server).
-author("mohit").
-import(server_handler, []).
%% API
-export([]).

% Basic modules : 1) start server 2) loop 3) pre loop/pre-loading data 4) private message 5) group message 6) quit.

start(Port) ->
  io:format("Hi").

pre_loop(Socket)  ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data}  ->
      io:format("Data Received : ~p~n", [binary_to_list(Data)]),
      Message = binary_to_list(Data),
      {Command, [_|Nick]} = lists:splitwith(fun(T)  -> [T] =/= ":" end, Message),
      io:format("Nick Names : ~p~n", [Nick]),
      case Command of
        "CONNECT" ->
          try_connection(Nick, Socket);
        _ ->
          gen_tcp:send(Socket, "Unknown Command! Try Again!"),
          ok

      end;
    {error, closed} ->
      ok
  end.

try_connection(Nick, Socket)  ->
  Response = gen_server:call(server_handler, {connect, Nick, Socket}),
  case Response of
    {ok, List}  ->
      % send the confirmation.
      gen_tcp:send(Socket, "Connect:Success:" ++ List ++"\n"),
      % cast to all clients saying a new client joined the chat.
      gen_server:cast(server_handler, {join, Nick}),
      % loop for the new Nickname and socket.
      loop(Nick, Socket);
    nickname_already_used ->
      gen_tcp:send(Socket, "Connect:Error: Nick Name already in use."),
      ok
  end.

loop(Nick, Socket)  ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data}  ->
      % print the data received
      io:format("Data Recieved : ~p~n", [binary_to_list(Data)]),
      Message = binary_to_list(Data),
      {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message),
      case Command of
        "CAST"  ->
          group_message(Nick, Socket, Content);
        "PRIVATE" ->
          % get the receiver here in this case.
          {Receiver, [_|Message]} = lists:splitwith(fun(T)  -> [T] =/= ":" end, Content),
          private_message(Nick, Socket, Receiver, Message);
        "LEAVE"  ->
          leave(Nick, Socket)

      end;
    {error, closed} ->
      ok
  end.

% group message code. cast the content recieved.
group_message(Nick, Socket, Content)  ->
  gen_server:cast(server_handler, {group_message, Nick, Content}),
  % again keep listening if new message arrives.
  loop(Nick, Socket).

% private message code. case the content. Add new param receiver.
private_message(Nick, Socket, Receiver, Message)  ->
  gen_server:cast(server_handler, {private_message, Nick, Receiver, Message}),
  loop(Nick, Socket).

% leave the chat server.
leave(Nick, Socket) ->
  Response = gen_server:call(server_handler, {leave, Nick}),
  case Response of
    ok  ->
      gen_tcp:send(Socket, "Bye :) \n"),
      gen_server:cast(server_handler, {leave, Nick}),
      ok;
    user_not_found  ->
      gen_tcp:send(Socket, "User not Found !"),
      ok
  end.

