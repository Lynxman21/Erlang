%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
-module(myLists).
-author("Mateusz").

%% API
-export([contains/2,duplicateElements/1,sumFloats/1]).

contains([],_) ->
  false;
contains([Head|_],Val) when Head =:=  Val->
  true;
contains([_|Res],Val) ->
  contains(Res,Val).

duplicateElements([]) ->
  [];
duplicateElements([Head|Tail]) ->
  [Head,Head|duplicateElements(Tail)].

sumFloats([]) ->
  0.0;
sumFloats([Head|Tail]) when is_float(Head) ->
  Head + sumFloats(Tail);
sumFloats([Head|Tail]) when not is_float(Head) ->
sumFloats(Tail).

%W przypadku funkcji power zrobiłem rekurencję ogonową, więc tutaj już nie robiłem