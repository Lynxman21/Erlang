%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. maj 2025 15:22
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("Mateusz").

%% API
-export([generate_data/0,find_for_person/2,find_closest/2,find_for_person/3,find_closest_parallel/2]).

%% Timer test
%% {P,Sens} = sensor_dist:generate_data().
%% {TimeMicroseconds, Result} = timer:tc(sensor_dist, find_closest, [P,Sens]).

get_rand_locations(Number) when is_integer(Number),Number>0 ->
  Res = [{rand:uniform(10001)-1,rand:uniform(10001)-1} || _ <- lists:seq(1,Number)],
  {ok,Res};
get_rand_locations(Number) when not is_integer(Number) ->
  {error,"Not a integer"};
get_rand_locations(Number) when is_integer(Number), Number =< 0 ->
  {error,"Bad integer"}.

dist({X1,Y1},{X2,Y2}) ->
  X = math:pow(X1-X2,2),
  Y = math:pow(Y1-Y2,2),
  math:sqrt(X+Y);
dist(_,_) ->
  {error,"One of them is no tuple"}.

min_by_first([H|T]) ->
  Comp = fun(A = {X,_},B = {Y,_}) ->
    if
      X < Y -> A;
      true -> B
    end
         end,
  lists:foldl(Comp,H,T).

%% -------------------------------------------Public-------------------------------------------

generate_data() ->
  {ok,Loc} = get_rand_locations(1000),
  {ok,Pos} = get_rand_locations(20000),
  {Loc,Pos}.

%% -------------------------------------------Sequence-------------------------------------------

find_for_person(PersonLocation,SensorsLocations) when is_tuple(PersonLocation),is_list(SensorsLocations) ->
  Distances = [{dist(PersonLocation,SensorLocation),{PersonLocation,SensorLocation}} || SensorLocation <- SensorsLocations],
  min_by_first(Distances);
find_for_person(_,_) ->
  {error,"Invalid data"}.

find_closest(PeopleLocations, SensorsLocations) when is_list(PeopleLocations),is_list(SensorsLocations) ->
  Distances = [find_for_person(PersonLocation,SensorsLocations) || PersonLocation <- PeopleLocations],
  min_by_first(Distances);
find_closest(_, _) ->
  {error,"Invalid data"}.

%% -------------------------------------------Parallel-------------------------------------------

find_for_person(PersonLocation,SensorsLocations,ParentPID) when is_tuple(PersonLocation),is_list(SensorsLocations) ->
  Distances = [{dist(PersonLocation,SensorLocation),{PersonLocation,SensorLocation}} || SensorLocation <- SensorsLocations],
  ParentPID ! {val,min_by_first(Distances)};
find_for_person(_,_,_) ->
  {error,"Invalid data"}.

find_closest_parallel(PeopleLocations, SensorsLocations) when is_list(PeopleLocations),is_list(SensorsLocations) ->
  PID = self(),

  [spawn(fun() -> find_for_person(Person,SensorsLocations,PID) end) || Person <- PeopleLocations],

  Results = [receive
               {_,Result} -> Result
             end || _ <- PeopleLocations],
  min_by_first(Results);
find_closest_parallel(_, _) ->
  {error,"Invalid data"}.