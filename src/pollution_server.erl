%%%-------------------------------------------------------------------
%%% @author ppeczek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. maj 2018 11:33
%%%-------------------------------------------------------------------
-module(pollution_server).
-import(pollution, [createMonitor/0, addStation/3, addValue/5,
removeValue/4, getOneValue/4, getStationMean/3,
getDailyMean/3, importFromCSV/2]).
-author("ppeczek").
-export([start/0, stop/0, addStation/2, addValue/4, removeValue/3,
        getOneValue/3, getStationMean/2, getDailyMean/2, importFromCSV/1]).
-export([initialize/0]).

%% API
start() ->
  register(server, spawn(?MODULE, initialize, [])).

stop() -> callHandler(stop, []).

addStation(Name, Coords) -> callHandler(add_station, [Name, Coords]).

addValue(NameOrCoords, Date, Type, Value) ->
  callHandler(add_value, [NameOrCoords, Date, Type, Value]).

removeValue(NameOrCoords, Date, Type) ->
  callHandler(remove_value, [NameOrCoords, Date, Type]).

getOneValue(NameOrCoords, Date, Type) ->
  callHandler(get_one_value, [NameOrCoords, Date, Type]).

getStationMean(NameOrCoords, Type) ->
  callHandler(get_station_mean, [NameOrCoords, Type]).

getDailyMean(DateDay, Type) ->
  callHandler(get_daily_mean, [DateDay, Type]).

importFromCSV(FileName) ->
  callHandler(import_from_csv, [FileName]).

%% API END


callHandler(RequestType, Args) when is_list(Args) ->
  server ! {request, self(), RequestType, Args},
  receive
    Message -> Message
  after
    1000 -> {error, timeout}
  end.


initialize() ->
  Monitor = pollution:createMonitor(),
  mainLoop(Monitor).

mainLoop(Monitor) ->
  receive
    {request, Pid, add_station, [Name, Coords]} ->
      NewMonitor = addStation(Monitor, Name, Coords),
      handleStatusRequest(Pid, Monitor, NewMonitor);
    {request, Pid, add_value, [NameOrCoords, Date, Type, Value]} ->
      NewMonitor = addValue(Monitor, NameOrCoords, Date, Type, Value),
      handleStatusRequest(Pid, Monitor, NewMonitor);
    {request, Pid, remove_value, [NameOrCoords, Date, Type]} ->
      NewMonitor = removeValue(Monitor, NameOrCoords, Date, Type),
      handleStatusRequest(Pid, Monitor, NewMonitor);
    {request, Pid, get_one_value, [NameOrCoords, Date, Type]} ->
      Value = getOneValue(Monitor, NameOrCoords, Date, Type),
      handleValueRequest(Pid, Value, Monitor);
    {request, Pid, get_station_mean, [NameOrCoords, Type]} ->
      Value = getStationMean(Monitor, NameOrCoords, Type),
      handleValueRequest(Pid, Value, Monitor);
    {request, Pid, get_daily_mean, [DateDay, Type]} ->
      Value = getDailyMean(Monitor, DateDay, Type),
      handleValueRequest(Pid, Value, Monitor);
    {request, Pid, import_from_csv, [FileName]} ->
      NewMonitor = importFromCSV(Monitor, FileName),
      handleStatusRequest(Pid, Monitor, NewMonitor);
    {request, Pid, stop, []} ->
      Pid ! {reply, ok};
    {request, Pid, _, _} ->
      Pid ! {reply, error, badRequest},
      mainLoop(Monitor)
  end.

handleStatusRequest(Pid, OldMonitor, NewMonitor) ->
  sendStatusResponse(Pid, NewMonitor),
  mainLoop(actualizeMonitor(OldMonitor, NewMonitor)).

handleValueRequest(Pid, Value, Monitor) ->
  sendValueResponse(Pid, Value),
  mainLoop(Monitor).

sendStatusResponse(Pid, {error, Message}) ->
  Pid ! {reply, error, Message};
sendStatusResponse(Pid, _) ->
  Pid ! {reply, ok}.

actualizeMonitor(OldMonitor, {error, _}) ->
  OldMonitor;
actualizeMonitor(_, NewMonitor) ->
  NewMonitor.

sendValueResponse(Pid, {error, Message}) ->
  Pid ! {reply, error, Message};
sendValueResponse(Pid, Value) ->
  Pid ! {reply, ok, Value}.
