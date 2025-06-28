-module(pollution).
-author("Mateusz").

-export([create_monitor/0,add_station/3,add_value/5,remove_value/4,get_one_value/4,get_station_min/3,get_daily_mean/3,get_minimum_pollution_station/2]).

%%Na początku chciałem zmienić monitor na zwykłą tablicę, ale z drugiej strony można zostawić to w postaci rekordu z myślą o rozbudowie takiej struktury
-record(monitor, {stations=[]}).
-record(station, {name,cords,readings=[]}).
-record(reading, {type,value,date}).

create_monitor() -> #monitor{}.

add_station(Station_name,{Lat,Lon} = Cords,Monitor) ->
  case {is_integer(Lat),is_integer(Lon)} of
    {true,true} ->
      Is_bad = lists:any(fun(Station) -> Station#station.name =:= Station_name end,Monitor#monitor.stations) orelse
        lists:any(fun(Station) -> Station#station.cords =:= Cords end,Monitor#monitor.stations),

      case Is_bad of
        true -> {error,"Name or cords already exists"};
        false ->
          New_station = #station{name=Station_name,cords=Cords},
          Monitor#monitor{stations = [New_station|Monitor#monitor.stations]}
      end;
    _ ->
      {error,"Invalid cords format or range"}
  end.

add_value(Cords={Lat,Lon},Date,Type,Val,Monitor) ->
  case {is_integer(Lat),is_integer(Lon)} of
    {true,true} ->
      Updated_stations = lists:map(
        fun(S) ->
          case S#station.cords =:= Cords of
            true ->
              case lists:any(fun(R) -> R#reading.type =:= Type andalso R#reading.date =:= Date end,S#station.readings) of
                true -> S;
                false ->
                  New_reading = #reading{type=Type,value=Val,date=Date},
                  S#station{readings = [New_reading|S#station.readings]}
              end;
            false -> S
          end
        end, Monitor#monitor.stations),
      case Updated_stations =:= Monitor#monitor.stations of
        true -> {error,"Data already exists or invalid station name"};
        false -> Monitor#monitor{stations = Updated_stations}
      end;
    _ ->
      {error,"Invalid cords"}
  end;
add_value(Station_name,Date,Type,Val,Monitor) ->
  Updated_stations = lists:map(
    fun(S) ->
      case S#station.name =:= Station_name of
        true ->
          case lists:any(fun(R) -> R#reading.type =:= Type andalso R#reading.date =:= Date end,S#station.readings) of
            true -> S;
            false ->
              New_reading = #reading{type=Type,value=Val,date=Date},
              S#station{readings = [New_reading|S#station.readings]}
          end;
        false -> S
      end
    end, Monitor#monitor.stations),
  case Updated_stations =:= Monitor#monitor.stations of
    true -> {error,"Data already exists or invalid station name"};
    false -> Monitor#monitor{stations = Updated_stations}
  end.

remove_value(Cords={Lat,Lon},Date,Type,Monitor) ->
  case {is_integer(Lat),is_integer(Lon)} of
    {true,true} ->
      Updated_stations = lists:map(
        fun(S) ->
          case S#station.cords =:= Cords of
            true ->
              Filtred = lists:filter(fun(R) -> not (R#reading.date =:= Date andalso R#reading.type =:= Type) end,S#station.readings),
              S#station{readings = Filtred};
            false -> S
          end
        end,Monitor#monitor.stations),
      case Updated_stations =:= Monitor#monitor.stations of
        true -> {error,"Data already exists or invalid station name"};
        false -> Monitor#monitor{stations = Updated_stations}
      end;
    _ ->
      {error, "Invalid cords"}
  end;

remove_value(Station_name,Date,Type,Monitor) ->
  Updated_stations = lists:map(
    fun(S) ->
      case S#station.name =:= Station_name of
        true ->
          Filtred = lists:filter(fun(R) -> not (R#reading.date =:= Date andalso R#reading.type =:= Type) end,S#station.readings),
          S#station{readings = Filtred};
        false -> S
      end
    end,Monitor#monitor.stations),
  case Updated_stations =:= Monitor#monitor.stations of
    true -> {error,"Data already exists or invalid station name"};
    false -> Monitor#monitor{stations = Updated_stations}
  end.

get_one_value(Cords={Lat,Lon},Date,Type,Monitor) ->
  case {is_integer(Lat),is_integer(Lon)} of
    {true,true} ->
      Station_arr = lists:filter(
        fun(S) -> S#station.cords =:= Cords end,
        Monitor#monitor.stations),
      case Station_arr of
        [] -> {error,"Station does not exist"};
        [S|_] ->
          Reading = lists:filter(fun(R) -> R#reading.type =:= Type andalso R#reading.date =:= Date end,S#station.readings),
          case Reading of
            [] -> {error,"Reading does not exist"};
            [Out|_] -> Out#reading.value
          end
      end;
    _ ->
      {error,"Invalid cords"}
  end;
get_one_value(Name,Date,Type,Monitor) ->
  Station_arr = lists:filter(
    fun(S) -> S#station.name =:= Name end,
    Monitor#monitor.stations),
  case Station_arr of
    [] -> {error,"Station does not exist"};
    [S|_] ->
      Reading = lists:filter(fun(R) -> R#reading.type =:= Type andalso R#reading.date =:= Date end,S#station.readings),
      case Reading of
        [] -> {error,"Reading does not exist"};
        [Out|_] -> Out#reading.value
      end
  end.


get_station_min(Cords={Lat,Lon},Type,Monitor) ->
  case {is_integer(Lat),is_integer(Lon)} of
    {true, true} ->
      Station_arr = lists:filter(fun(S) -> S#station.cords =:= Cords end,Monitor#monitor.stations),
      case Station_arr of
        [] -> {error,"Station does not exist"};
        [S|_] ->
          Type_arr = lists:filter(fun(R) -> R#reading.type =:= Type end,S#station.readings),
          case Type_arr of
            [] -> {error,"There is no readings"};
            _ ->
              Values = [R#reading.value || R <- Type_arr],
              lists:min(Values)
          end
      end;
    _ -> {error,"Invalid cords"}
end;
get_station_min(Name,Type,Monitor) ->
  Station_arr = lists:filter(fun(S) -> S#station.name =:= Name end,Monitor#monitor.stations),
  case Station_arr of
    [] -> {error,"Station does not exist"};
    [S|_] ->
      Type_arr = lists:filter(fun(R) -> R#reading.type =:= Type end,S#station.readings),
      case Type_arr of
        [] -> {error,"There is no readings"};
        _ ->
          Values = [R#reading.value || R <- Type_arr],
          lists:min(Values)
      end
  end.

get_daily_mean(Type,Date,Monitor) ->
  Values = [R#reading.value || S <- Monitor#monitor.stations,R <- S#station.readings,R#reading.type=:=Type,element(1,R#reading.date)=:=Date],
  case Values of
    [] -> {error,"0 values"};
    _ -> lists:sum(Values)/length(Values)
  end.

get_minimum_pollution_station(Type,Monitor) ->
  Stations = Monitor#monitor.stations,
  case Stations of
    [] -> {error, "There is no stations"};
    _ ->
      Type_arr = [{S#station.name,R#reading.value} || S <- Stations,R <- S#station.readings,R#reading.type=:=Type],
      [Head|_] = lists:sort(
        fun(A,B) ->
          element(2,A) =< element(2,B) end,
        Type_arr),
      element(1,Head)
end.