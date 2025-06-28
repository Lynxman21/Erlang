%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. maj 2025 14:06
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Mateusz").

%% API
-export([start/0, stop/0,add_station/2, add_value/4,remove_value/3, get_one_value/3,get_station_min/2,get_daily_mean/2,get_minimum_pollution_station/1]).

%%--------------------------Server_fun--------------------------

start() ->
  register(server,spawn(fun() -> server_loop(none) end)),
  server ! init.

stop() ->
  server ! kill.

add_station(Name,Cords) ->
  server ! {add_station, {Name,Cords},self()},
  receive
    ok -> ok;
    error -> error
  end.

add_value({Lat,Lon},Date,Type,Val) ->
  server ! {add_value, {{Lat,Lon},Date,Type,Val},self()},
  receive
    ok -> ok;
    error -> error
  end;
add_value(Name,Date,Type,Val) ->
  server ! {add_value, {Name,Date,Type,Val},self()},
  receive
    ok -> ok;
    error -> error
  end.

remove_value({Lat,Lon},Date,Type) ->
  server ! {remove_value, {{Lat,Lon},Date,Type},self()},
  receive
    ok -> ok;
    error -> error
  end;
remove_value(Name,Date,Type) ->
  server ! {remove_value, {Name,Date,Type}, self()},
  receive
    ok -> ok;
    error -> error
  end.

get_one_value({Lat,Lon},Date,Type) ->
  server ! {get_one_value, {{Lat,Lon},Date,Type}, self()},
  receive Res -> Res end;
get_one_value(Name,Date,Type) ->
  server ! {get_one_value, {Name,Date,Type}, self()},
  receive Res -> Res end.

get_station_min({Lat,Lon},Type) ->
  server ! {get_station_min, {{Lat,Lon},Type}, self()},
  receive Res -> Res end;
get_station_min(Name,Type) ->
  server ! {get_station_min, {Name,Type}, self()},
  receive Res -> Res end.

get_daily_mean(Type,Date) ->
  server ! {get_daily_mean, {Type,Date}, self()},
  receive Res -> Res end.

get_minimum_pollution_station(Type) ->
  server ! {get_minimum_pollution_station, {Type}, self()},
  receive Res -> Res end.

%--------------------------Helpers--------------------------

check_error_chng(Monitor,New_monitor,PID) ->
  case New_monitor of
    {error,Msg} ->
      io:format(Msg),
      PID ! error,
      server_loop(Monitor);
    _ ->
      PID ! ok,
      server_loop(New_monitor)
  end.

check_error(Monitor,Result,PID) ->
  case Result of
    {err,Msg} -> io:format(Msg);
    _ -> ok
  end,
  PID ! Result,
  server_loop(Monitor).

%--------------------------Loop--------------------------

server_loop(Monitor) ->
  receive
    init ->
      case Monitor of
        none ->
          New_monitor = pollution:create_monitor(),
          server_loop(New_monitor);
        _ ->
          io:format("Monitor already exist"),
          server_loop(Monitor)
      end;

    kill -> ok;

    {add_station,{Name,Cords},PID} ->
      New_monitor = pollution:add_station(Name,Cords,Monitor),
      check_error_chng(Monitor,New_monitor,PID);

    {add_value, {{Lat,Lon},Date,Type,Val},PID} ->
      New_monitor = pollution:add_value({Lat,Lon},Date,Type,Val,Monitor),
      check_error_chng(Monitor,New_monitor, PID);

    {add_value, {Name,Date,Type,Val},PID} ->
      New_monitor = pollution:add_value(Name,Date,Type,Val,Monitor),
      check_error_chng(Monitor,New_monitor,PID);

    {remove_value, {{Lat,Lon},Date,Type},PID} ->
      New_monitor = pollution:remove_value({Lat,Lon},Date,Type,Monitor),
      check_error_chng(Monitor,New_monitor,PID);

    {remove_value, {Name,Date,Type},PID} ->
      New_monitor = pollution:remove_value(Name,Date,Type,Monitor),
      check_error_chng(Monitor,New_monitor,PID);

    {get_one_value,{{Lat,Lon},Date,Type},PID} ->
      Res = pollution:get_one_value({Lat,Lon},Date,Type,Monitor),
      check_error(Monitor,Res,PID);

    {get_one_value,{Name,Date,Type},PID} ->
      Res = pollution:get_one_value(Name,Date,Type,Monitor),
      check_error(Monitor,Res,PID);

    {get_station_min, {{Lat,Lon},Type}, PID} ->
      Res = pollution:get_station_min({Lat,Lon},Type,Monitor),
      check_error(Monitor,Res,PID);

    {get_station_min, {Name,Type}, PID} ->
      Res = pollution:get_station_min(Name,Type,Monitor),
      check_error(Monitor,Res,PID);

    {get_daily_mean,{Type,Date},PID} ->
      Res = pollution:get_daily_mean(Type,Date,Monitor),
      check_error(Monitor,Res,PID);

    {get_minimum_pollution_station, {Type}, PID} ->
      Res = pollution:get_minimum_pollution_station(Type,Monitor),
      check_error(Monitor,Res,PID)
  end.