%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. mar 2025 17:11
%%%-------------------------------------------------------------------
-module(qsort).
-author("Mateusz").

%% API
-export([qs/1,compare_speed_test/0]).

less_than([],_) ->
  [];
less_than(List,Arg) ->
  [Val || Val <- List, Val < Arg].

grt_eq_than([], _) ->
  [];
grt_eq_than(List,Arg) ->
  [Val || Val <- List, Val >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).

random_elems(N,Min,Max) ->
  rand:seed(exsplus),
  [rand:uniform(Max-Min+1)+Min-1 || _ <- lists:seq(1,N)].

compare_speeds([],_,_) ->
  0;

compare_speeds(List,Fun1,Fun2) ->
  {Time1,Res1} = timer:tc(Fun1,[List]),
  {Time2,Res2} = timer:tc(Fun2,[List]),

  io:format("Fun1: ~p ms, Result: ~p~n",[Time1/1000,Res1]),
  io:format("Fun2: ~p ms, Result: ~p~n",[Time2/1000,Res2]).

compare_speed_test() ->
  F1 = fun qs/1,
  F2 = fun lists:sort/1,
  List = random_elems(10,5,20),

  compare_speeds(List,F1,F2).