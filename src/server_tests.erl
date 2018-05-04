%%%-------------------------------------------------------------------
%%% @author ppeczek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. maj 2018 14:01
%%%-------------------------------------------------------------------
-module(server_tests).
-author("ppeczek").

-record(monitor, {names = #{}, coordinates = #{}, stations = #{}}).
-record(measurements, {values = #{}}).

-include_lib("eunit/include/eunit.hrl").
-import(pollution_server, [start/0, stop/0, addStation/2, addValue/4, removeValue/3,
                          getOneValue/3, getStationMean/2, getDailyMean/2, importFromCSV/1]).

given_one_station_added() ->
  start(),
  addStation("kijowska", {21, 37}).

given_two_stations_added() ->
  given_one_station_added(),
  addStation("kawiory", {21.33, 37.19}).

given_three_values_added() ->
  given_two_stations_added(),
  addValue("kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 37),
  addValue("kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 30),
  addValue("kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 37).

given_four_values_added() ->
  given_three_values_added(),
  addValue({21.33, 37.19}, {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 30).

given_values_for_mean_added() ->
  given_two_stations_added(),
  addValue("kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 40),
  addValue("kijowska", {{2018, 4, 15}, {21, 10, 0}}, "PM10", 30),
  addValue("kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 40),
  addValue("kawiory", {{2018, 4, 15}, {21, 10, 0}}, "PM10", 30),
  addValue("kawiory", {{2018, 4, 15}, {21, 20, 0}}, "PM10", 35).

given_values_for_daily_mean_added() ->
  given_two_stations_added(),
  addValue("kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 40),
  addValue("kijowska", {{2018, 4, 15}, {21, 10, 0}}, "PM10", 30),
  addValue("kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 40),
  addValue("kawiory", {{2018, 4, 15}, {21, 10, 0}}, "PM10", 30),
  addValue("kawiory", {{2018, 4, 16}, {21, 20, 0}}, "PM10", 135).

adding_stations_to_monitor_ok_test() ->
  given_one_station_added(),
  %%  when
  Res = addStation("kawiory", {21.33, 37.19}),
  %%  then
  ?assertMatch({reply, ok}, Res),
  stop().

adding_stations_to_monitor_the_same_name_coords_test() ->
  given_two_stations_added(),
  %%  when
  Res = addStation("kawiory", {21.33, 37.19}),
  %%  Then
  ?assertMatch({reply, error, alredyExists}, Res),
  stop().

adding_stations_to_monitor_the_same_name_test() ->
  given_two_stations_added(),
  %%  when
  Val = addStation("kawiory", {22.33, 31.19}),
  %% then
  ?assertMatch({reply, error, alredyExists}, Val),
  stop().

adding_stations_to_monitor_the_same_coords_test() ->
  given_two_stations_added(),
  %% when
  Val = addStation("another_name", {21.33, 37.19}),
  %% then
  ?assertMatch({reply, error, alredyExists}, Val),
  stop().

adding_stations_to_monitor_name_no_string_test() ->
  given_two_stations_added(),
  ?assertMatch({reply, error, badArguments}, addStation(3, {21.33, 37.19})),
  stop().


adding_values_to_stations_success_test() ->
  given_three_values_added(),
  %% when
  Val = addValue({21.33, 37.19}, {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 30),
  %% then
  ?assertMatch({reply, ok}, Val),
  stop().

adding_values_to_stations_noexist_station_test() ->
  given_four_values_added(),
  %% when
  Val = addValue("no_station", {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 30),
  %% then
  ?assertMatch({reply, error, stationDeosntExist}, Val),
  stop().

adding_values_to_stations_bad_date_test() ->
  given_four_values_added(),
  %% when
  Val = addValue("kawiory", {{2018, 4, 16}, bad_date}, "PM2.5", 30),
  %% then
  ?assertMatch({reply, error, badDateFormat}, Val),
  stop().

adding_values_to_stations_corresponding_value_exists_test() ->
  given_four_values_added(),
  %% when
  Val = addValue("kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 60),
  %% then
  ?assertMatch({reply, error, correspondingValueExists}, Val),
  stop().

adding_values_to_stations_bad_type_test() ->
  given_four_values_added(),
  %% when
  Val = addValue("kawiory", {{2018, 4, 16}, {21, 0, 0}}, 3, 60),
  %% then
  ?assertMatch({reply, error, badArguments}, Val),
  stop().

adding_values_to_stations_noexist_station_corrupt_format_name_test() ->
  given_four_values_added(),
  %% when
  Val = addValue({a, b, c}, {{2018, 4, 16}, {21, 0, 0}}, 3, 60),
  %% then
  ?assertMatch({reply, error, stationDeosntExist}, Val),
  stop().

getting_value_by_coords_test() ->
  given_four_values_added(),
  %% when
  Val = getOneValue({21, 37}, {{2018, 4, 15}, {21, 0, 0}}, "PM10"),
  %% then
  ?assertEqual({reply, ok, 37}, Val),
  stop().

getting_value_by_name_test() ->
  given_four_values_added(),
  %% when
  Val = getOneValue("kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM10"),
  %% then
  ?assertEqual({reply, ok, 37}, Val),
  stop().

getting_value_bad_date_test() ->
  given_four_values_added(),
  %% when
  Val = getOneValue({21, 37}, {{2018, 4, 15}}, "PM10"),
  %% then
  ?assertMatch({reply, error, badDateFormat}, Val),
  stop().

getting_value_noexists_station_test() ->
  given_four_values_added(),
  %% when
  Val = getOneValue(scam, {{2018, 4, 15}, {21, 0, 0}}, "PM10"),
  %% then
  ?assertMatch({reply, error, stationDeosntExist}, Val),
  stop().

removing_value_by_name_test() ->
  given_four_values_added(),
  %% when
  Val = removeValue("kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM2.5"),
  %% then
  ?assertMatch({reply, ok}, Val),
  stop().

removing_value_by_coords_test() ->
  given_four_values_added(),
  %% when
  Val = removeValue({21.33, 37.19}, {{2018, 4, 15}, {21, 0, 0}}, "PM2.5"),
  %% then
  ?assertMatch({reply, ok}, Val),
  stop().

removing_value_that_doesnt_exist_test() ->
  given_four_values_added(),
  %% when
  Val = removeValue("kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM7.5"),
  %% then
  ?assertMatch({reply, error,valueDoesntExist}, Val),
  stop().

getting_station_mean_by_name_test() ->
  given_values_for_mean_added(),
  %% when
  Val = getStationMean("kawiory", "PM10"),
  %% then
  ?assertEqual({reply, ok, 35.0}, Val),
  stop().

getting_station_mean_by_coords_test() ->
  given_values_for_mean_added(),
  %% when
  Val = getStationMean({21, 37}, "PM10"),
  %% then
  ?assertEqual({reply, ok, 35.0}, Val),
  stop().

getting_station_mean_noexist_value_date_name_test() ->
  given_values_for_mean_added(),
  %% when
  Val = getStationMean({21, 37}, true),
  %% then
  ?assertEqual({reply, error, noMeasurementsOfGivenTypeOrDate}, Val),
  stop().

getting_daily_mean_ok1_test() ->
  given_values_for_daily_mean_added(),
  %% when
  Val = getDailyMean({2018, 4, 15}, "PM10"),
  %% then
  ?assertEqual({reply, ok, 35.0}, Val),
  stop().

getting_daily_mean_ok2_test() ->
  given_values_for_daily_mean_added(),
  %% when
  Val = getDailyMean({2018, 4, 16}, "PM10"),
  %% then
  ?assertEqual({reply, ok, 135.0}, Val),
  stop().

getting_daily_mean_noexist_date_test() ->
  given_values_for_daily_mean_added(),
  %% when
  Val = getDailyMean({2018, 4, 17}, "PM10"),
  %% then
  ?assertMatch({reply, error, noMeasurementsOfGivenTypeOrDate}, Val),
  stop().

getting_daily_mean_bad_date_format_test() ->
  given_values_for_daily_mean_added(),
  %% when
  Val = getDailyMean({2018, 4, 17, a}, "PM10"),
  %% then
  ?assertMatch({reply, error, badArguments}, Val),
  stop().
