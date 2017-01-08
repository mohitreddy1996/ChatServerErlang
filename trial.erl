%%%-------------------------------------------------------------------
%%% @author mohit
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jan 2017 7:29 PM
%%%-------------------------------------------------------------------
-module(trial).
-author("mohit").

%% API
-export([add/2, print_hello_world/0, print_hello_and_add_2/1]).

%% function to add 2 numbers
add(A, B) ->
  A+B.

%% function print hello world.
print_hello_world() ->
  io:format("Hello World :)~n").

%% print and add 2
print_hello_and_add_2(A)  ->
  print_hello_world(),
  add(A, 2).

