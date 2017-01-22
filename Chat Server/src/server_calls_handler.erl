%%%-------------------------------------------------------------------
%%% @author mohit
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Handles all the possible 
%%% @end
%%% Created : 17. Jan 2017 6:56 PM
%%%-------------------------------------------------------------------
-module(server_calls_handler).
-author("mohit").
-behavior(gen_server).
%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start/0]).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  Users = dict:new(),
  {ok, Users}.

% handle call is called when gen_server:call is called. Handle synchronous calls.
% handle calls for each of the cases. Connect and leave to be implemented.
% generic handle call to control error.
handle_call({connect, Nick, Socket}, _From, Users) ->
  % check if Nick is there in the Users State. If yes say nick already in use. Else send 'ok' response.
  Response = case dict:is_key(Nick, Users) of
               true ->
                 NewUsers = Users,
                 nickname_already_used;
               false  ->
                 NewUsers = dict:append(Nick, Socket, Users),
                 {ok, get_user_list(NewUsers)}
             end,
  {reply, Response, NewUsers};

handle_call({leave, Nick}, _From, Users) ->
  Response = case dict:is_key(Nick, Users) of
               true ->
                 NewUsers = dict:erase(Nick, Users),
                 ok;
               false  ->
                 NewUsers = Users,
                 user_not_found
             end,
  {reply, Response, NewUsers};

handle_call(Request, From, State) ->
  {reply, error, State}.

% Handle Casts for private message, Group Message, join, leave. Handle asynchronous calls.
% Handle Cast for other messages as errors.
handle_cast({private_message, Nick, Receiver, Message}, Users) ->
  Temp = case dict:find(Receiver, Users) of
           {ok, [Socket|_]} ->
             gen_tcp:send(Socket, "PRIVATE:" ++ Nick ++ ":" ++ Message);
           _  ->
             ok
         end,
  {noreply, Users};

handle_cast({group_message, Nick, Message}, Users) ->
  broadcast(Nick, "SAID:" ++ Nick ++ ":" ++ Message ++"\n", Users),
  {noreply, Users};

handle_cast({join, Nick}, Users) ->
  broadcast(Nick, "JOIN:" ++ Nick ++ "\n", Users),
  {noreply, Users};

handle_cast({leave, Nick}, Users) ->
  broadcast(Nick, "LEAVE:" ++ Nick ++ "\n", Users),
  {noreply, Users};

handle_cast(Request, Users) ->
  {noreply, Users}.

handle_info(Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  ok.

code_change(OldVsn, State, Extra) ->
  {ok, State}.

% get users as a string using ':' separated
get_user_list(NewUser)  ->
  Users = dict:fetch_keys(NewUser),
  string:concat(Users, ":").

% broadcast code.
broadcast(Nick, Message, Users) ->
  Sockets = lists:map(fun({_,[Value|_]}) -> Value end, dict:to_list(dict:erase(Nick, Users))),
  lists:foreach(fun(Sock) -> gen_tcp:send(Sock, Message) end, Sockets).