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
  {ok, [{planet, Pid1}, {planet, Pid2}, {planet, Pid3}]}.

handle_call(_Request, _From, PidList) ->
  {ok, ObjectStatus} = getObjectStatus(PidList, []),
%%  io:fwrite("ObjectStatus: ~p~n", [ObjectStatus]),
  {reply, ObjectStatus, PidList}.

handle_cast(yaw_right, PidList) ->
  handle_ship_action(yaw_right, PidList);
handle_cast(yaw_left, PidList) ->
  handle_ship_action(yaw_left, PidList);
handle_cast(Info, PidList) ->
  {ok, Pid} = ship:start_link(Info),
  {noreply, [{ship, Pid} | PidList]}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%======================================================================================================
% Internal
%======================================================================================================

handle_ship_action(Action, PidList) ->
  lists:foreach(fun({Object, Pid}) -> case Object of
                                        ship -> gen_server:cast(Pid, Action);
                                        _ -> ok
                                      end
                end, PidList),
  {noreply, PidList}.

getObjectStatus([], Positions) ->
  {ok, Positions};
getObjectStatus([{_, Pid} | T], Positions) ->
  Pos = gen_server:call(Pid, []),
  getObjectStatus(T, [Pos | Positions]).
