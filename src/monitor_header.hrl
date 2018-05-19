%%%-------------------------------------------------------------------
%%% @author ppeczek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. maj 2018 17:20
%%%-------------------------------------------------------------------
-author("ppeczek").

-record(monitor, {names = #{}, coordinates = #{}, stations = #{}}).
-record(measurements, {values = #{}}).