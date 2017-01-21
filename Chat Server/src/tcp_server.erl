%%%-------------------------------------------------------------------
%%% @author mohit
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2017 4:00 PM
%%%-------------------------------------------------------------------
-module(tcp_server).
-author("mohit").

-behavior(gen_server).
%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/3, accept_loop/1]).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-record(server_state, {port, loop, ip = any, lsocket = null}).

init(State = #server_state{port = Port}) ->
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, ListenSocket}  ->
      NewState = State#server_state{lsocket = ListenSocket},
      {ok, accept(NewState)};
    {error, Reason} ->
      {stop, Reason}
  end.

start(Name, Port, Loop) ->
  State = #server_state{port = Port, loop = Loop},
  gen_server:start_link({local, Name}, ?MODULE, State, []).

accept_loop({Server, LSocket, {Module, LoopFunction}})  ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  gen_server:cast(Server, {accepted, self()}),
  Module:LoopFunction(Socket).

accept(State = #server_state{lsocket = LSocket, loop = Loop}) ->
  proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
  State.

handle_call(Request, From, State) ->
  {noreply, State}.

handle_cast({accepted, _Pid}, State = #server_state) ->
  {noreply, accept(State)}.

handle_info(Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  ok.

code_change(OldVsn, State, Extra) ->
  {ok, State}.