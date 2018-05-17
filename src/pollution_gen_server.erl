%%%-------------------------------------------------------------------
%%% @author ppeczek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2018 13:52
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("ppeczek").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%%  API
-export([start_link/0, stop/0, getOneValue/3, addValue/4, addStation/2, removeValue/4]).
-export([importFromCSV/1, crash/0, getStationMean/2, getDailyMean/2]).


start_link() ->
  gen_server:start_link({local, pollution_gen_server}, ?MODULE, [], []).

stop() -> gen_server:call(pollution_gen_server, {stop}).

getOneValue(NameOrCoords, Date, Type) ->
  gen_server:call(pollution_gen_server, {get_one_value, NameOrCoords, Date, Type}).

getStationMean(NameOrCoords, Type) ->
  gen_server:call(pollution_gen_server, {get_station_mean, NameOrCoords, Type}).

getDailyMean(DateDay, Type) ->
  gen_server:call(pollution_gen_server, {get_daily_mean, DateDay, Type}).

addStation(Name, Coords) ->
  gen_server:cast(pollution_gen_server, {add_station, Name, Coords}).

addValue(NameOrCoords, Date, Type, Value) ->
  gen_server:cast(pollution_gen_server, {add_value, NameOrCoords, Date, Type, Value}).

removeValue(NameOrCoords, Date, Type, Value) ->
  gen_server:cast(pollution_gen_server, {remove_value, NameOrCoords, Date, Type, Value}).

importFromCSV(FileName) ->
  gen_server:cast(pollution_gen_server, {import_from_csv, FileName}).


crash() ->
  gen_server:cast(pollution_gen_server, {crash}).



%%  IMPLEMENTATION

init([]) -> {ok, pollution:createMonitor()}.


handle_call({stop}, _, LoopData) -> {stop, normal, ok, LoopData};
handle_call({get_one_value, NameOrCoords, Date, Type}, _, LoopData) ->
  Value = pollution:getOneValue(LoopData, NameOrCoords, Date, Type),
  dispatchValueResponse(Value, LoopData);
handle_call({get_station_mean, NameOrCoords, Type}, _, LoopData) ->
  Value = pollution:getStationMean(LoopData, NameOrCoords, Type),
  dispatchValueResponse(Value, LoopData);
handle_call({get_daily_mean, DateDay, Type}, _, LoopData) ->
  Value = pollution:getDailyMean(LoopData, DateDay, Type),
  dispatchValueResponse(Value, LoopData).

handle_cast({add_station, Name, Coords}, LoopData) ->
  NewMonitor = pollution:addStation(LoopData, Name, Coords),
  dispatchStatusResponse(NewMonitor, LoopData);
handle_cast({add_value, NameOrCoords, Date, Type, Value}, LoopData) ->
  NewMonitor = pollution:addValue(LoopData, NameOrCoords, Date, Type, Value),
  dispatchStatusResponse(NewMonitor, LoopData);
handle_cast({remove_value, NameOrCoords, Date, Type, Value}, LoopData) ->
  NewMonitor = pollution:addValue(LoopData, NameOrCoords, Date, Type, Value),
  dispatchStatusResponse(NewMonitor, LoopData);
handle_cast({import_from_csv, FileName}, LoopData) ->
  NewMonitor = pollution:importFromCSV(LoopData, FileName),
  dispatchStatusResponse(NewMonitor, LoopData);
handle_cast({crash}, _) ->  1 / 0.

dispatchStatusResponse({error, _}, LoopData) ->
  {noreply, LoopData};
dispatchStatusResponse(NewMonitor, _) ->
  {noreply, NewMonitor}.

dispatchValueResponse(Value, LoopData) ->
  {reply, Value, LoopData}.

terminate(_Reason, _State) ->
  ok.