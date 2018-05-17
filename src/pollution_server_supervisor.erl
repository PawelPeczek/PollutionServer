%%%-------------------------------------------------------------------
%%% @author ppeczek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. maj 2018 13:20
%%%-------------------------------------------------------------------
-module(pollution_server_supervisor).
-author("ppeczek").

%% API
-export([init/0, supervisor/0]).

init() -> spawn(?MODULE, supervisor, []).

supervisor() ->
  process_flag(trap_exit, true),
  register(server, spawn_link(pollution_server, initialize, [])),
  receive
    {'EXIT', _, normal} -> ok;
    {'EXIT', _, _} ->
      io:format("Server is respawned.~n"),
      supervisor()
  end.