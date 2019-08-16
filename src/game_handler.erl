-module(game_handler).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%======================================================================================================
% API
%======================================================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  io:fwrite("start_link ~p~n", [?MODULE]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%======================================================================================================
% gen_server stuff
%======================================================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, Pid1} = planet_object:start_link({"sun", {0.0, 0.0, 0.0, 0, 0, 1}}),
  {ok, Pid2} = planet_object:start_link({"earth", {1.0, 0.0, 0.0, 10, 0, 5}}),
  {ok, Pid3} = planet_object:start_link({"venus", {1.0, 0.0, 0.0, 40, math:pi(), 10}}),
  {ok, [Pid1, Pid2, Pid3]}.

handle_call(_Request, _From, PidList) ->
  {ok, Positions} = getObjectPositions(PidList, []),
  {reply, Positions, PidList}.

handle_cast(Info, PidList) ->
  io:fwrite("handle_cast game_handler...~p~n", [Info]),
  {ok, Pid} = ship:start_link(Info),
  {noreply, [Pid | PidList]}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%======================================================================================================
% Internal
%======================================================================================================

getObjectPositions([], Positions) ->
  {ok, Positions};
getObjectPositions([Pid | T], Positions) ->
  Pos = gen_server:call(Pid, []),
%%  io:fwrite("getObjectPositions...~p~n", [Pos]),
  getObjectPositions(T, [Pos | Positions]).