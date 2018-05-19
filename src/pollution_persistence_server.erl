%%%-------------------------------------------------------------------
%%% @author ppeczek
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. maj 2018 17:23
%%%-------------------------------------------------------------------
-module(pollution_persistence_server).
-author("ppeczek").
-behaviour(gen_server).

-include_lib("monitor_header.hrl").

%% API
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([update_state/1, stop/0, get_state/0, start_link/0]).

start_link() ->
  gen_server:start_link({local, persistance_server}, ?MODULE, #monitor{}, []).

update_state(M) when erlang:is_record(M, monitor) ->
  gen_server:cast(persistance_server, {update_state, M}).

stop() -> gen_server:cast(persistance_server, {stop}).

get_state() -> gen_server:call(persistance_server, {get_state}).

init(M) when is_record(M, monitor) ->  {ok, M}.

handle_cast({update_state, M}, _) when erlang:is_record(M, monitor) -> {noreply, M};
handle_cast({stop}, LoopData) -> {stop, normal, LoopData}.

handle_call({get_state}, _, LoopData) -> {reply, LoopData, LoopData}.

terminate(_Reason, _State) ->
  ok.