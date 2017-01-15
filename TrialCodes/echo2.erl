-module(echo2).
-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> loop(Socket) end),
    accept(LSocket).

%%get_msg(['h','e','l','l','o']) ->  "Hi";

get_msg(_) ->  "Message Received".

loop(Socket) ->
      case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
          Msg = get_msg(erlang:binary_to_list(Data)),
          case gen_tcp:send(Socket, Msg) of
              {error, _} -> exit(normal);
               ok -> loop(Socket)
          end;
        {error, _} -> ok
			end.
	    	

