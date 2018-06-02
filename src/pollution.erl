%%%-------------------------------------------------------------------
%%% @author ppeczek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. kwi 2018 13:10
%%%-------------------------------------------------------------------
-module(pollution).
-author("ppeczek").
-include_lib("monitor_header.hrl").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4,
         getStationMean/3, getDailyMean/3, importFromCSV/2]).


%% Exported functions

createMonitor() -> #monitor{}.


addStation(M, Name, {CoordX, CoordY}) when is_integer(CoordX) or is_integer(CoordX) ->
  addStation(M, Name, {float(CoordX), float(CoordY)});
addStation(M, Name, {CoordX, CoordY})
  when (CoordX >= 0) and (CoordY >= 0) and (CoordX =< 90) and (CoordY =< 180)
  and is_float(CoordX) and is_float(CoordY)->
    case isStationAvailable(M, Name, {CoordX, CoordY}) of
      true -> addContentToMonitor(M, Name, {CoordX, CoordY});
      false -> {error, alredyExists};
      badArguments -> {error, badArguments}
    end;
addStation(_, _, _) -> {error, badArguments}.

addValue(M, {CoordX, CoordY}, Date, Type, Value) when is_integer(CoordX) or is_integer(CoordY) ->
  addValue(M, {float(CoordX), float(CoordY)}, Date, Type, Value);
addValue(M, NameOrCoords, Date, Type, Value) ->
    case doesStationExists(M, NameOrCoords) of
      false -> {error, stationDeosntExist};
      true -> procedFiveArgRequest(M, NameOrCoords, Date, Type, Value, fun insertNewValueIntoMonitor/6);
      badArguments -> {error, badArguments}
    end.

removeValue(M, {CoordX, CoordY}, Date, Type) when is_integer(CoordX) or is_integer(CoordY) ->
  removeValue(M, {float(CoordX), float(CoordY)}, Date, Type);
removeValue(M, NameOrCoord, Date, Type) ->
  case doesStationExists(M, NameOrCoord) of
    false -> {error, stationDeosntExist};
    true -> procedFourArgRequest(M, NameOrCoord, Date, Type, fun deleteValueFromMonitor/5);
    badArguments -> {error, badArguments}
  end.


getOneValue(M, {CoordX, CoordY}, Date, Type) when is_integer(CoordX) or is_integer(CoordY) ->
  getOneValue(M, {float(CoordX), float(CoordY)}, Date, Type);
getOneValue(M, NameOrCoord, Date, Type) ->
  case doesStationExists(M, NameOrCoord) of
    false -> {error, stationDeosntExist};
    true -> procedFourArgRequest(M, NameOrCoord, Date, Type, fun findValueInMonitor/5);
    badArguments -> {error, badArguments}
  end.


getStationMean(M, {CoordX, CoordY}, Type) when is_integer(CoordX) or is_integer(CoordY) ->
  getStationMean(M, {float(CoordX), float(CoordY)}, Type);
getStationMean(M, NameOrCoord, Type) ->
  case doesStationExists(M, NameOrCoord) of
    false -> {error, stationDeosntExist};
    true -> proceedThreeArgRequest(M, NameOrCoord, Type, fun countMeanOfMeasurFromGivenStation/4);
    badArguments -> {error, badArguments}
  end.

getDailyMean(M, DateDay, Type) -> countDailyMean(M, DateDay, Type).

importFromCSV(M, FileName) -> procedImportProcess(M, FileName).

%% Exported functions END



%% Internal functions

addContentToMonitor(#monitor{names = N, coordinates = C, stations = S}, Name, {CoordX, CoordY}) ->
  #monitor{
    names = N#{Name => {CoordX, CoordY}},
    coordinates = C#{{CoordX, CoordY} => Name},
    stations = S#{{Name, {CoordX, CoordY}} => #measurements{}}}.


isStationAvailable(#monitor{names = N, coordinates = C, stations = _}, Name, {CoordX, CoordY}) ->
  isNameAndCoordsFree(maps:is_key(Name, N), maps:is_key({CoordX, CoordY}, C));
isStationAvailable(_, _, _) -> badArguments.


isNameAndCoordsFree(false, false) -> true;
isNameAndCoordsFree(_, _) -> false.


doesStationExists(#monitor{names = _, coordinates = C, stations = _}, {CoordX, CoordY}) ->
  maps:is_key({CoordX, CoordY}, C);
doesStationExists(#monitor{names = N, coordinates = _, stations = _}, Name) ->
  maps:is_key(Name, N);
doesStationExists(_, _) -> badArguments.


depackNameOrCoordArg(#monitor{names = _, coordinates = C, stations = _}, {CoordX, CoordY}) ->
  {maps:get({CoordX, CoordY}, C), {CoordX, CoordY}};
depackNameOrCoordArg(#monitor{names = N, coordinates = _, stations = _}, Name) ->
  {Name, maps:get(Name, N)}.

insertNewValueIntoMonitor(#monitor{names = N, coordinates = C, stations = S}, Name, Coords, Date, Type, Value) ->
  Measurements = maps:get({Name, Coords}, S),
  case isMeasurementSlotFree(Measurements, Date, Type) of
    true -> #monitor{
                      names = N,
                      coordinates = C,
                      stations = S#{{Name, Coords} := #measurements{
                        values = maps:put({Type, Date}, Value, Measurements#measurements.values)
                    }}};
    false -> {error, correspondingValueExists};
    badDateFormat -> {error, badDateFormat}
  end;
insertNewValueIntoMonitor(_, _, _, _, _, _) -> {error, badArguments}.

isMeasurementSlotFree(#measurements{values = Values}, {{Y, Mo, D}, {H, M, S}}, Type) ->
  case maps:is_key({Type, {{Y, Mo, D}, {H, M, S}}}, Values) of
    true -> false;
    false -> true
  end;
isMeasurementSlotFree(_, _, _) -> badDateFormat.
procedFiveArgRequest(M, NameOrCoords, Date, Type, Value, F) when is_function(F) ->
  {Name, Coords} = depackNameOrCoordArg(M, NameOrCoords),
  F(M, Name, Coords, Date, Type, Value).

procedFourArgRequest(M, NameOrCoords, Date, Type, F) when is_function(F) ->
  {Name, Coords} = depackNameOrCoordArg(M, NameOrCoords),
  F(M, Name, Coords, Date, Type).

proceedThreeArgRequest(M, NameOrCoords, DateOrType, F) when is_function(F) ->
  {Name, Coords} = depackNameOrCoordArg(M, NameOrCoords),
  F(M, Name, Coords, DateOrType).


deleteValueFromMonitor(#monitor{names = N, coordinates = C, stations = S}, Name, Coords, Date, Type) ->
  Measurements = maps:get({Name, Coords}, S),
  case doesMeasurementExists(Measurements, Date, Type) of
    true -> #monitor{
              names = N,
              coordinates = C,
              stations = S#{{Name, Coords} := #measurements{
                values = maps:remove({Type, Date}, Measurements#measurements.values)
            }}};
    false -> {error, valueDoesntExist};
    badDateFormat -> {error, badDateFormat}
  end.

doesMeasurementExists(#measurements{values = Values}, {{Y, Mo, D}, {H, M, S}}, Type) ->
  maps:is_key({Type, {{Y, Mo, D}, {H, M, S}}}, Values);
doesMeasurementExists(_, _, _) -> badDateFormat.


findValueInMonitor(#monitor{names = _, coordinates = _, stations = S}, Name, Coords, Date, Type) ->
  Measurements = maps:get({Name, Coords}, S),
  #measurements{values = Values} = Measurements,
  case doesMeasurementExists(Measurements, Date, Type) of
    true -> maps:get({Type, Date}, Values);
    false -> {error, valueDoesntExist};
    badDateFormat -> {error, badDateFormat}
  end.


countMeanOfMeasurFromGivenStation(#monitor{names = _, coordinates = _, stations = S}, Name, Coords, Type) ->
  #measurements{values = Values} = maps:get({Name, Coords}, S),
  Filtered = maps:filter(fun({T, _}, _) -> T == Type end, Values),
  saveAverage(maps:fold(fun(_, V, Acc) -> Acc + V end, 0, Filtered), maps:size(Filtered)).

saveAverage(_, Denom) when (Denom == 0) -> {error, noMeasurementsOfGivenTypeOrDate};
saveAverage(Num, Denom) -> Num / Denom.

countDailyMean(#monitor{names = _, coordinates = _, stations = S}, {Y, M, D}, Type) ->
  ListOfValues = maps:fold(fun(_, #measurements{values = V}, Acc) -> maps:to_list(V) ++ Acc end, [], S),
  Filtered =
    lists:filter(
      fun({{ET, {{EY, EM, ED}, {_, _, _}}}, _}) -> (ET == Type) and (Y == EY) and (M == EM) and (D == ED) end,
      ListOfValues),
  saveAverage(lists:foldl(fun({_, X}, Sum) -> X + Sum end, 0, Filtered), length(Filtered));
countDailyMean(_, _, _) -> {error, badArguments}.

procedImportProcess(M, FileName) ->
  case file:open(FileName, read) of
    {ok, Handler} ->
      Data = readLines(Handler, []),
      Splits = splitAllLines(Data, []),
      Tuples = lists:map(fun transformIntoTuple/1, Splits),
      batchTransform(M, Tuples);
    {error, Reason} -> {error, Reason}
  end.

readLines(Handler, Acc) ->
  case file:read_line(Handler) of
    eof -> Acc;
    {error, Reason} -> {error, Reason};
    {ok, Data} -> readLines(Handler, Acc ++ [Data])
  end.

splitAllLines([], Acc) -> Acc;
splitAllLines([H | T], Acc) -> splitAllLines(T, Acc ++ [splitOnComma(H, [])]).

splitOnComma([], Acc) -> Acc;
splitOnComma([$, | List], Acc) -> splitOnComma(List, Acc);
splitOnComma(Data, Acc) when is_list(Data) ->
  TmpList = lists:takewhile(fun(C) -> (C /= $,) end, Data),
  splitOnComma(Data -- TmpList, Acc ++ [TmpList]).


transformIntoTuple([Name, CoordX, CoordY| []]) ->
  {
    Name,
    {toFl(CoordX), toFl(CoordY)}
  };
transformIntoTuple([Name, Y, Mo, D, H, M, S, Type, Value | []]) ->
  {
    Name,
    {{toI(Y), toI(Mo), toI(D)}, {toI(H), toI(M), toI(S)}},
    Type,
    toFl(lists:filter(fun lfPredicate/1,Value))
  };
transformIntoTuple([CoordX, CoordY, Y, Mo, D, H, M, S, Type, Value | []]) ->
  {
    {toFl(CoordX), toFl(CoordY)},
    {{toI(Y), toI(Mo), toI(D)}, {toI(H), toI(M), toI(S)}},
    Type,
    toFl(lists:filter(fun lfPredicate/1,Value))
  }.


lfPredicate(X) -> (X /= 10).

toI(Expr) when is_list(Expr) -> list_to_integer(Expr).

toFl(Expr) when is_list(Expr) ->
  case string:to_float(Expr) of
    {error, no_float} -> list_to_integer(Expr);
    {F, _Rest} -> F
  end.


batchTransform({error, Type}, _) -> {error, Type};
batchTransform(M, []) -> M;
batchTransform(M, [{Name, {CoordX, CoordY}} | T]) ->
  batchTransform(addStation(M, Name, {CoordX, CoordY}), T);
batchTransform(M, [{NameOrCoords, Date, Type, Value} | T]) ->
  batchTransform(addValue(M, NameOrCoords, Date, Type, Value), T).

%% Internal functions END