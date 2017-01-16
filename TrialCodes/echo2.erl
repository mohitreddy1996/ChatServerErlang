-module(echo2).
-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
  Pid = spawn_link(fun() ->
    {ok, Listen} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn(fun() -> accept(Listen) end),
    timer:sleep(infinity)
                   end),
  {ok, Pid}.

accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  spawn(fun() -> accept(LSocket) end),
  loop(Socket).

loop(Socket) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, <<"hello", _/binary>>} ->
      gen_tcp:send(Socket,"Hi! What can i do for you?\n"),
      loop(Socket);
    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      loop(Socket)
  end.

