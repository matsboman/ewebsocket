%% @private
-module(ewebsocket_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%% supervisor.
%%init([]) ->
%%  Procs = [],
%%  {ok, {{one_for_one, 10, 10}, Procs}}.
init([]) ->
  {ok, {{one_for_one, 10, 10},
    [{ch3, {game_handler, start_link, []},
      permanent, brutal_kill, worker, [ch3]}]}}.
