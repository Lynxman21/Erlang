%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,handle_call/3,handle_cast/2,terminate/2,crash/0]).
-export([add_value/4,add_station/2,remove_value/3]).
-export([get_one_value/3,get_station_min/2,get_daily_mean/2,get_minimum_pollution_station/1]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, pollution:create_monitor()}.

add_station(Name, {Lat,Lon}) ->
  gen_server:cast(?MODULE, {add_station,Name,{Lat,Lon}}).

add_value({Lat,Lon},Date,Type,Val) ->
  gen_server:cast(?MODULE, {add_value,{Lat,Lon},Date,Type,Val});
add_value(Name,Date,Type,Val) ->
  gen_server:cast(?MODULE, {add_value,Name,Date,Type,Val}).

remove_value({Lat,Lon},Date,Type) ->
  gen_server:cast(?MODULE, {remove_value,{Lat,Lon},Date,Type});
remove_value(Name,Date,Type) ->
  gen_server:cast(?MODULE, {remove_value,Name,Date,Type}).

get_one_value({Lat,Lon},Date,Type) ->
  gen_server:call(?MODULE,{get_one_value,{Lat,Lon},Date,Type});
get_one_value(Name,Date,Type) ->
  gen_server:call(?MODULE,{get_one_value,Name,Date,Type}).

get_station_min({Lat,Lon},Type) ->
  gen_server:call(?MODULE,{get_station_min,{Lat,Lon},Type});
get_station_min(Name,Type) ->
  gen_server:call(?MODULE,{get_station_min,Name,Type}).

get_daily_mean(Type,Date) ->
  gen_server:call(?MODULE,{get_daily_mean,Type,Date}).

get_minimum_pollution_station(Type) ->
  gen_server:call(?MODULE,{get_minimum_pollution_station,Type}).

handle_cast(Args,Monitor) ->
  Res = case Args of
          {add_station,Name,{Lat,Lon}} -> pollution:add_station(Name,{Lat,Lon},Monitor);
          {add_value,{Lat,Lon},Date,Type,Val} -> pollution:add_value({Lat,Lon},Date,Type,Val,Monitor);
          {add_value,Name,Date,Type,Val} -> pollution:add_value(Name,Date,Type,Val,Monitor);
          {remove_value,{Lat,Lon},Date,Type} -> pollution:remove_value({Lat,Lon},Date,Type,Monitor);
          {remove_value,Name,Date,Type} -> pollution:remove_value(Name,Date,Type,Monitor);
          _ -> {error, unknown_value}
        end,
  case Res of
    {error,Msg} ->
      io:format("Error: ~w. ~n",[Msg]),
      {noreply, Monitor};
    _ ->
      {noreply, Res}
  end.

handle_call(Args,_From,Monitor) ->
  Res = case Args of
          {get_one_value,{Lat,Lon},Date,Type} -> pollution:get_one_value({Lat,Lon},Date,Type,Monitor);
          {get_one_value,Name,Date,Type} -> pollution:get_one_value(Name,Date,Type,Monitor);
          {get_station_min,{Lat,Lon},Type} -> pollution:get_station_min({Lat,Lon},Type,Monitor);
          {get_station_min,Name,Type} -> pollution:get_station_min(Name,Type,Monitor);
          {get_daily_mean,Type,Date} -> pollution:get_daily_mean(Type,Date,Monitor);
          {get_minimum_pollution_station,Type} -> pollution:get_minimum_pollution_station(Type,Monitor);
          _ -> {error, unknown_value}
        end,

  case Res of
    {error, Msg} ->
      io:format("Error: ~w. ~n",[Msg]),
      {reply,{error, Msg}, Monitor};
    _ ->
      {reply,Res,Monitor}
  end.

crash() ->
  no:exist().

terminate(Reason,State) ->
  io:format("ERROR: ~w. ~n", [Reason]),
  io:format("Last state was: ~w. ~n", [State]),
  ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
