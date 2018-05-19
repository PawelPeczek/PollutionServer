%%%-------------------------------------------------------------------
%%% @author ppeczek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. kwi 2018 23:18
%%%-------------------------------------------------------------------
-module(monitor_tests).
-author("ppeczek").

-include_lib("eunit/include/eunit.hrl").
-include_lib("../src/monitor_header.hrl").


given_one_station_added() ->
  M = pollution:createMonitor(),
  pollution:addStation(M, "kijowska", {21, 37}).

given_two_stations_added() ->
  M1 = given_one_station_added(),
  pollution:addStation(M1, "kawiory", {21.33, 37.19}).

given_three_values_added() ->
  M2 = given_two_stations_added(),
  M3 = pollution:addValue(M2, "kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 37),
  M4 = pollution:addValue(M3, "kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 30),
  pollution:addValue(M4, "kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 37).

given_four_values_added() ->
  M5 = given_three_values_added(),
  pollution:addValue(M5, {21.33, 37.19}, {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 30).

given_values_for_mean_added() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "kijowska", {21, 37}),
  M2 = pollution:addStation(M1, "kawiory", {21.33, 37.19}),
  M3 = pollution:addValue(M2, "kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 40),
  M4 = pollution:addValue(M3, "kijowska", {{2018, 4, 15}, {21, 10, 0}}, "PM10", 30),
  M5 = pollution:addValue(M4, "kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 40),
  M6 = pollution:addValue(M5, "kawiory", {{2018, 4, 15}, {21, 10, 0}}, "PM10", 30),
  pollution:addValue(M6, "kawiory", {{2018, 4, 15}, {21, 20, 0}}, "PM10", 35).

given_values_for_daily_mean_added() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation(M, "kijowska", {21, 37}),
  M2 = pollution:addStation(M1, "kawiory", {21.33, 37.19}),
  M3 = pollution:addValue(M2, "kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 40),
  M4 = pollution:addValue(M3, "kijowska", {{2018, 4, 15}, {21, 10, 0}}, "PM10", 30),
  M5 = pollution:addValue(M4, "kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM10", 40),
  M6 = pollution:addValue(M5, "kawiory", {{2018, 4, 15}, {21, 10, 0}}, "PM10", 30),
  pollution:addValue(M6, "kawiory", {{2018, 4, 16}, {21, 20, 0}}, "PM10", 135).

empty_monitor_creation_test() ->
  %% when
  Val = pollution:createMonitor(),
  %% then
  ?assertEqual(#monitor{}, Val).

adding_stations_to_monitor_ok_test() ->
  M1 = given_one_station_added(),
  %%  when
  M2 = pollution:addStation(M1, "kawiory", {21.33, 37.19}),
  %%  then
  ?assertMatch(
    #monitor{
      names = #{"kijowska" := {21.0, 37.0}, "kawiory" := {21.33, 37.19}},
      coordinates = #{{21.0, 37.0} := "kijowska", {21.33, 37.19} := "kawiory"},
      stations = #{{"kijowska", {21.0, 37.0}} := #measurements{}, {"kawiory",  {21.33, 37.19}} := #measurements{}}
    }
    , M2).

adding_stations_to_monitor_the_same_name_coords_test() ->
  M2 = given_two_stations_added(),
  %%  when
  Val = pollution:addStation(M2, "kawiory", {21.33, 37.19}),
  %%  Then
  ?assertMatch({error, alredyExists}, Val).

adding_stations_to_monitor_the_same_name_test() ->
  M2 = given_two_stations_added(),
  %%  when
  Val = pollution:addStation(M2, "kawiory", {22.33, 31.19}),
  %% then
  ?assertMatch({error, alredyExists}, Val).

adding_stations_to_monitor_the_same_coords_test() ->
  M2 = given_two_stations_added(),
  %% when
  Val = pollution:addStation(M2, "another_name", {21.33, 37.19}),
  %% then
  ?assertMatch({error, alredyExists}, Val).

adding_stations_to_monitor_bad_monitor_test() ->
  %% when
  Val = pollution:addStation(dumn_arg, "kawiory", {21.33, 37.19}),
  %% then
  ?assertMatch({error, badArguments}, Val).

adding_stations_to_monitor_name_no_string_test() ->
  M2 = given_two_stations_added(),
  ?assertMatch({error, badArguments}, pollution:addStation(M2, 3, {21.33, 37.19})).


adding_values_to_stations_success_test() ->
  M5 = given_three_values_added(),
  %% when
  M6 = pollution:addValue(M5, {21.33, 37.19}, {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 30),
  %% then
  ?assertMatch(
    #monitor{
      names = #{"kijowska" := {21.0, 37.0}, "kawiory" := {21.33, 37.19}},
      coordinates = #{{21.0, 37.0} := "kijowska", {21.33, 37.19} := "kawiory"},
      stations = #{{"kijowska", {21.0, 37.0}} := #measurements{
        values = #{
          {"PM10", {{2018, 4, 15}, {21, 0, 0}}} := 37,
          {"PM2.5", {{2018, 4, 15}, {21, 0, 0}}} := 30
        }
      },
        {"kawiory",  {21.33, 37.19}} := #measurements{ values = #{
          {"PM10", {{2018, 4, 15}, {21, 0, 0}}} := 37,
          {"PM2.5", {{2018, 4, 15}, {21, 0, 0}}} := 30
        }}}
    }
    , M6).

adding_values_to_stations_noexist_station_test() ->
  M6 = given_four_values_added(),
  %% when
  M7 = pollution:addValue(M6, "no_station", {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 30),
  %% then
  ?assertMatch({error, stationDeosntExist}, M7).

adding_values_to_stations_bad_monitor_test() ->
  %% when
  M7 = pollution:addValue(bad_arg, "kawiory", {{2018, 4, 16}, {1, 2, 3}}, "PM2.5", 30),
  %% then
  ?assertMatch({error, badArguments}, M7).

adding_values_to_stations_bad_date_test() ->
  M6 = given_four_values_added(),
  %% when
  M7 = pollution:addValue(M6, "kawiory", {{2018, 4, 16}, bad_date}, "PM2.5", 30),
  %% then
  ?assertMatch({error, badDateFormat}, M7).

adding_values_to_stations_corresponding_value_exists_test() ->
  M6 = given_four_values_added(),
  %% when
  M7 = pollution:addValue(M6, "kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM2.5", 60),
  %% then
  ?assertMatch({error, correspondingValueExists}, M7).

adding_values_to_stations_bad_type_test() ->
  M6 = given_four_values_added(),
  %% when
  M7 = pollution:addValue(M6, "kawiory", {{2018, 4, 16}, {21, 0, 0}}, 3, 60),
  %% then
  ?assertMatch({error, badArguments}, M7).

adding_values_to_stations_noexist_station_corrupt_format_name_test() ->
  M6 = given_four_values_added(),
  %% when
  M7 = pollution:addValue(M6, {a, b, c}, {{2018, 4, 16}, {21, 0, 0}}, 3, 60),
  %% then
  ?assertMatch({error, stationDeosntExist}, M7).

getting_value_by_coords_test() ->
  M6 = given_four_values_added(),
  %% when
  Val = pollution:getOneValue(M6, {21, 37}, {{2018, 4, 15}, {21, 0, 0}}, "PM10"),
  %% then
  ?assertEqual(37, Val).

getting_value_by_name_test() ->
  M6 = given_four_values_added(),
  %% when
  Val = pollution:getOneValue(M6, "kijowska", {{2018, 4, 15}, {21, 0, 0}}, "PM10"),
  %% then
  ?assertEqual(37, Val).

getting_value_bad_date_test() ->
  M6 = given_four_values_added(),
  %% when
  Val = pollution:getOneValue(M6, {21, 37}, {{2018, 4, 15}}, "PM10"),
  %% then
  ?assertMatch({error, badDateFormat}, Val).

getting_value_noexists_station_test() ->
  M6 = given_four_values_added(),
  %% when
  Val = pollution:getOneValue(M6, scam, {{2018, 4, 15}, {21, 0, 0}}, "PM10"),
  %% then
  ?assertMatch({error, stationDeosntExist}, Val).

getting_value_bad_monitor_test() ->
  %% when
  Val = pollution:getOneValue(d, {21, 37}, {{2018, 4, 15}, {21, 0, 0}}, "PM10"),
  %% then
  ?assertMatch({error, badArguments}, Val).

removing_value_by_name_test() ->
  M6 = given_four_values_added(),
  %% when
  M7 = pollution:removeValue(M6, "kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM2.5"),
  %% then
  ?assertMatch(
    #monitor{
      names = #{"kijowska" := {21.0, 37.0}, "kawiory" := {21.33, 37.19}},
      coordinates = #{{21.0, 37.0} := "kijowska", {21.33, 37.19} := "kawiory"},
      stations = #{{"kijowska", {21.0, 37.0}} := #measurements{
        values = #{
          {"PM10", {{2018, 4, 15}, {21, 0, 0}}} := 37,
          {"PM2.5", {{2018, 4, 15}, {21, 0, 0}}} := 30
        }
      },
        {"kawiory",  {21.33, 37.19}} := #measurements{ values = #{
          {"PM10", {{2018, 4, 15}, {21, 0, 0}}} := 37
        }}}
    }
    , M7).

removing_value_by_coords_test() ->
  M6 = given_four_values_added(),
  %% when
  M7 = pollution:removeValue(M6, {21.33, 37.19}, {{2018, 4, 15}, {21, 0, 0}}, "PM2.5"),
  %% then
  ?assertMatch(
    #monitor{
      names = #{"kijowska" := {21.0, 37.0}, "kawiory" := {21.33, 37.19}},
      coordinates = #{{21.0, 37.0} := "kijowska", {21.33, 37.19} := "kawiory"},
      stations = #{{"kijowska", {21.0, 37.0}} := #measurements{
        values = #{
          {"PM10", {{2018, 4, 15}, {21, 0, 0}}} := 37,
          {"PM2.5", {{2018, 4, 15}, {21, 0, 0}}} := 30
        }
      },
        {"kawiory",  {21.33, 37.19}} := #measurements{ values = #{
          {"PM10", {{2018, 4, 15}, {21, 0, 0}}} := 37
        }}}
    }
    , M7).

removing_value_that_deosnt_exist_test() ->
  M6 = given_four_values_added(),
  %% when
  M7 = pollution:removeValue(M6, "kawiory", {{2018, 4, 15}, {21, 0, 0}}, "PM7.5"),
  %% then
  ?assertMatch({error,valueDoesntExist}, M7).


getting_station_mean_by_name_test() ->
  M7 = given_values_for_mean_added(),
  %% when
  Val = pollution:getStationMean(M7, "kawiory", "PM10"),
  %% then
  ?assertEqual(35.0, Val).

getting_station_mean_by_coords_test() ->
  M7 = given_values_for_mean_added(),
  %% when
  Val = pollution:getStationMean(M7, {21, 37}, "PM10"),
  %% then
  ?assertEqual(35.0, Val).

getting_station_mean_noexist_value_date_name_test() ->
  M7 = given_values_for_mean_added(),
  %% when
  Val = pollution:getStationMean(M7, {21, 37}, true),
  %% then
  ?assertEqual({error, noMeasurementsOfGivenTypeOrDate}, Val).

getting_daily_mean_ok1_test() ->
  M7 = given_values_for_daily_mean_added(),
  %% when
  Val = pollution:getDailyMean(M7, {2018, 4, 15}, "PM10"),
  %% then
  ?assertEqual(35.0, Val).

getting_daily_mean_ok2_test() ->
  M7 = given_values_for_daily_mean_added(),
  %% when
  Val = pollution:getDailyMean(M7, {2018, 4, 16}, "PM10"),
  %% then
  ?assertEqual(135.0, Val).

getting_daily_mean_noexist_date_test() ->
  M7 = given_values_for_daily_mean_added(),
  %% when
  Val = pollution:getDailyMean(M7, {2018, 4, 17}, "PM10"),
  %% then
  ?assertMatch({error, noMeasurementsOfGivenTypeOrDate}, Val).

getting_daily_mean_bad_date_format_test() ->
  M7 = given_values_for_daily_mean_added(),
  %% when
  Val = pollution:getDailyMean(M7, {2018, 4, 17, a}, "PM10"),
  %% then
  ?assertMatch({error, badArguments}, Val).