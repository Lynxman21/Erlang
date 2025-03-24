%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2025 18:23
%%%-------------------------------------------------------------------
-module(pollutionCalc).
-author("Mateusz").

%% API
-export([exampleData/0,number_of_readings/2,calculate_max/2,calculate_mean/2]).

exampleData() ->
  [
    {"London station",{{2025,3,10},{15,40,30}},[{"temp",8},{"pressure",1013},{"humidity",60},{"pol",25}]},
    {"Luxembourg station",{{2025,3,10},{10,5,59}},[{"temp",15},{"pressure",1015},{"pol",30}]},
    {"Athens station",{{2025,4,1},{11,40,5}},[{"temp",6},{"pressure",1010},{"humidity",59},{"pol",35}]}
  ].

number_of_readings([], _) ->
  0;
number_of_readings([{_,{Date,_},_}|Tail],Date) ->
  1+number_of_readings(Tail,Date);
number_of_readings([_|Tail],Date) ->
  number_of_readings(Tail,Date).

calculate_max([],_) ->
  undefined;
calculate_max(Readings,Type) ->
  Values = [Value || {_,_,Measurements} <- Readings, {MeasurementType, Value} <- Measurements, MeasurementType=:=Type],
  case Values of
    [] -> undefined;
    _ -> float(lists:max(Values))
  end.

calculate_mean([],_) ->
  undefined;
calculate_mean(Readings,Type) ->
  Values = [Value || {_,_,Measurements} <- Readings, {MeasurementsType,Value} <- Measurements, MeasurementsType=:=Type],
  case Values of
    [] -> undefined;
    _ -> lists:sum(Values)/length(Values)
  end.