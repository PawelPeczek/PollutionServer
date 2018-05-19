%%%-------------------------------------------------------------------
%%% @author ppeczek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. maj 2018 17:04
%%%-------------------------------------------------------------------
-module(pollution_gen_server_supervisor).
-author("ppeczek").
-behaviour(supervisor).

-include_lib("monitor_header.hrl").

%% API
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, supervisor}, ?MODULE, []).

init([]) ->
  {ok, {
    {one_for_one, 10, 1000},
    [ {persistance_server,
      {pollution_persistence_server, start_link, []},
      permanent, 5000, worker, [pollution_persistence_server]
      },
      {pollution_gen_server,
        {pollution_gen_server, start_link, []},
        permanent, 5000, worker, [pollution_gen_server]
      }
    ]}
  }.