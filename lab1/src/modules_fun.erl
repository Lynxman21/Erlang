-module(modules_fun).
-author("Mateusz").

%%Dodać osobną zmienną debilu
%% API
-export([power/2]).

power(Base,Exponent) ->
  power(Base,Exponent,1).

power(_,0,Acc) ->
  Acc;
power(Base,Exponent,Acc) when Exponent>0 ->
  power(Base,Exponent-1,Acc*Base);
power(Base,Exponent,Acc) when Exponent<0 ->
  power(Base,Exponent+1,Acc*Base).

