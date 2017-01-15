%%%-------------------------------------------------------------------
%%% @author mohit
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Replication of echo call in unix. Send back whatever data is sent to
%%% it over a TCP Connection.
%%% @end
%%% Created : 15. Jan 2017 7:18 PM
%%%-------------------------------------------------------------------
-module(echo).
-author("mohit").

%% API
-export([listen/1]).

% define a macro.
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

% listen(port) to start the service
listen(Port) ->
  {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  accept(LSocket).

% accept the incoming socket
accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  % spawn creates a new process for a functional object. Function can be used a fun object.
  spawn(fun() -> loop(Socket) end),
  accept(LSocket).

% loop makes sure of sending back the same packet/data received.
% replicates the functionality of echo.
loop(Socket)  ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data}  ->
      gen_tcp:send(Socket, Data),
      loop(Socket);
    {error, closed} ->
      ok
  end.
