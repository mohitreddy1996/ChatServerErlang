%%%-------------------------------------------------------------------
%%% @author mohit
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Replication of Chargen Protocol. Referred Code.
%%% @end
%%% Created : 15. Jan 2017 7:19 PM
%%%-------------------------------------------------------------------
-module(chargen).
-author("mohit").

%% API
-export([listen/1]).

-define(START_CHAR_CONST, 33).
-define(END_CHAR_CONST, 127).
-define(LINE_LENGTH, 72).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

% call listen(Port) to start the service.
listen(Port)  ->
  {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  accept(ListenSocket).

% wait for incoming connections and create a new process
accept(ListenSocket)  ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> loop(Socket) end),
  accept(ListenSocket).

loop(Socket)  ->
  loop(Socket, ?START_CHAR_CONST).

loop(Socket, ?END_CHAR_CONST) ->
  loop(Socket, ?START_CHAR_CONST).

loop(Socket, StartChar) ->
  Line = make_line(StartChar),
  case gen_tcp:send(Socket, Line) of
    {error, _Reason}  ->
      exit(normal);
    ok  ->
      loop(Socket, StartChar + 1)
  end.

make_line(StartChar)  ->
  make_line(StartChar, 0).

make_line(_, ?LINE_LENGTH)  ->
  [13, 10];

make_line(?END_CHAR_CONST, Pos) ->
  make_line(?START_CHAR_CONST, Pos);

make_line(StartChar, Pos) ->
  [StartChar | make_line(StartChar + 1, Pos + 1)].


