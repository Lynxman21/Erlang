%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. maj 2025 00:50
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("Mateusz").

-behaviour(gen_statem).

%% API
-export([start_link/0]).
-export([no_station/3, station_set/3]).

%% gen_statem callbacks
-export([init/1, terminate/3,callback_mode/0]).
-export([set_station/2,add_value/3,store_date/0]).

-define(SERVER, ?MODULE).

-record(state, {monitor, station_name=none,state_cords=none,new_readings=[]}).
-record(reading, {type,value,date}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
    Monitor = pollution:create_monitor(),
    gen_statem:start_link({local, ?SERVER}, ?MODULE, Monitor, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init(Monitor) ->
  {ok, no_station, #state{monitor = Monitor}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
    state_functions.

set_station(Station_name,Station_cords) ->
    gen_statem:cast(?SERVER,{set_station,Station_name,Station_cords}).

add_value(Type,Value,Date) ->
    gen_statem:cast(?SERVER,{add_value,Type,Value,Date}).

store_date() ->
    gen_statem:cast(?SERVER,{store_date}).

no_station(_, {set_station, Station_name, Station_cords}, State = #state{}) ->
    {next_state, station_set, State#state{station_name = Station_name, state_cords =  Station_cords}}.

station_set(_, {add_value, Type, Value, Date}, State = #state{}) ->
    New_reading = #reading{type = Type,value = Value, date = Date},
    New_list = [New_reading | State#state.new_readings],
    {keep_state,State#state{new_readings = New_list}};
station_set(_, {store_date}, State = #state{}) ->
    case commit_proccess(State) of
        error ->
            error;
        UpdatedState ->
            {next_state, no_station,UpdatedState}
    end.

commit_proccess(State) ->
    case pollution:is_station_present(State#state.station_name,State#state.monitor) of
        true -> commit(State);
        false ->
            New_monitor = pollution:add_station(State#state.station_name,State#state.state_cords,State#state.monitor),
            New_state = State#state{monitor = New_monitor},
            commit(New_state)
    end.

commit(#state{new_readings = []} = State) ->
    State;
commit(#state{new_readings = [Head | Tail]} = State) when is_record(Head,reading) ->
    case pollution:add_value(State#state.station_name,Head#reading.date,Head#reading.type,Head#reading.value,State#state.monitor) of
        {error,Msg} ->
            io:format(Msg),
            New_state = State#state{new_readings = Tail},
            commit(New_state);
        New_monitor ->
            New_state = State#state{monitor = New_monitor,new_readings = Tail},
            commit(New_state)
    end.

terminate(Reason, StateName, State) ->
    io:format("ERROR: ~w. ~n",[Reason]),
    io:format("In state: ~w. ~n",[StateName]),
    io:format("With data stored: ~w. ~n",[State]),
    ok.
