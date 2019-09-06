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
  {ok, ObjectStatus, NewPidList} = getObjectStatus(PidList, [], []),
  check_collisions(ObjectStatus),
%%  io:fwrite("ObjectStatus: ~p~n", [ObjectStatus]),
  {reply, ObjectStatus, NewPidList}.

handle_cast({yaw_right, _Name} = Action, PidList) ->
  handle_ship_action(Action, PidList);
handle_cast({yaw_left, _Name} = Action, PidList) ->
  handle_ship_action(Action, PidList);
handle_cast({fire, Name}, PidList) ->
  handle_ship_action({fire, Name}, PidList);
handle_cast(Info, PidList) ->
  {ok, Pid} = ship:start_link(Info),
  {noreply, [{ship, {Pid, {shot_pids, []}}} | PidList]}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%======================================================================================================
% Internal
%======================================================================================================

check_collisions(StatusList) ->
%%  io:fwrite("check_collisions #############################: ~p~n", [StatusList]),
  check_collisions(StatusList, StatusList).
check_collisions([], StatusList) ->
  ok;
check_collisions([Status | T], StatusList) ->
  check_collisions_impl(Status, StatusList),
  check_collisions(T, StatusList).

check_collisions_impl(_Status, []) ->
  ok;
check_collisions_impl(Status, [Peer | T]) ->
  is_collision(Status, Peer),
  check_collisions_impl(Status, T).

is_collision(#{<<"name">> := Name},  #{<<"name">> := Name}) ->
  ok;
is_collision(#{<<"type">> := <<"planet">>},  #{<<"type">> := <<"planet">>}) ->
%%  io:fwrite("peer planets are not checked for collision...~n", []),
  ok;
is_collision(#{<<"name">> := Name1, <<"position">> := #{<<"x">> := X1, <<"y">> := Y1, <<"z">> := Z1}},
    #{<<"name">> := Name2, <<"position">> := #{<<"x">> := X2, <<"y">> := Y2, <<"z">> := Z2}}) ->
  Distance = math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2) + math:pow(Z2 - Z1, 2)),
  if
    Distance < 2 -> io:fwrite("Distance: ~p~n", [{Name1, Name2, Distance}]);
    true -> ok
  end.

handle_ship_action({fire, Name}, PidList) ->
  NewPidList = call_to_ship({fire, Name}, PidList),
%%  io:fwrite("NewPidList: ~p~n", [NewPidList]),
  {noreply, NewPidList};
handle_ship_action(Action, PidList) ->
  cast_to_ship(Action, PidList),
  {noreply, PidList}.

getObjectStatus([], Statuses, NewPidList) ->
  {ok, Statuses, NewPidList};
getObjectStatus([{Id, {Pid, {shot_pids, ShotPidList}}} | T], Statuses, NewPidList) ->
  {ok, ShotsPositions, NewShotPidList} = getShotsStatus(ShotPidList, [], []),
  Status = gen_server:call(Pid, []),
  getObjectStatus(T, Statuses ++ [Status | ShotsPositions], [{Id, {Pid, {shot_pids, NewShotPidList}}} | NewPidList]);
getObjectStatus([{Id, Pid} | T], Statuses, NewPidList) ->
  Status = gen_server:call(Pid, []),
  getObjectStatus(T, [Status | Statuses], [{Id, Pid} | NewPidList]).

getShotsStatus([], Statuses, NewPidList) ->
  {ok, Statuses, NewPidList};
getShotsStatus([Pid | T], Statuses, NewPidList) ->
  getShotsStatusHandler([Pid | T], gen_server:call(Pid, []), Statuses, NewPidList).

getShotsStatusHandler([Pid | T], #{<<"message">> := <<"terminated">>} = Status, Statuses, NewPidList) ->
  gen_server:stop(Pid),
  getShotsStatus(T, [Status | Statuses], NewPidList);
getShotsStatusHandler([Pid | T], Status, Statuses, NewPidList) ->
  getShotsStatus(T, [Status | Statuses], [Pid | NewPidList]).


cast_to_ship(Action, PidList) ->
  lists:foreach(fun({Object,  Pid}) -> case Object of
                                        ship ->
                                          {ShipPid, {shot_pids, _ShotPidList}} = Pid,
                                          gen_server:cast(ShipPid, Action);
                                        _ -> ok
                                      end
                end, PidList).

call_to_ship(Action, PidList) ->
  lists:map(fun({Object, Pid}) -> case Object of
                                        ship ->
                                          {ShipPid, {shot_pids, ShotPidList}} = Pid,
                                          case gen_server:call(ShipPid, Action) of
                                            no_action ->
                                              io:fwrite("fire no_action...~n", []),
                                              {Object, {ShipPid, {shot_pids, ShotPidList}}};
                                            ShotPid ->
                                              io:fwrite("fired shot...~p~n", [ShotPid]),
                                              {Object, {ShipPid, {shot_pids, [ShotPid | ShotPidList]}}}
                                          end;
                                        _ -> {Object, Pid}
                                      end
                end, PidList).
