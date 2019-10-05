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
  {ok, Pid1} = planet:start_link(
    #{<<"name">> => <<"sun">>, <<"x">> => 0.0, <<"y">> => 0.0, <<"z">> => 0.0,
      <<"orbit_radius">> => 0, <<"t0">> => 0, <<"t">> => 1, <<"radius">> => 4}),
  {ok, Pid2} = planet:start_link(
    #{<<"name">> => <<"earth">>, <<"x">> => 1.0, <<"y">> => 0.0, <<"z">> => 0.0,
      <<"orbit_radius">> => 10, <<"t0">> => 0, <<"t">> => 5, <<"radius">> => 0.4}),
  {ok, Pid3} = planet:start_link(
    #{<<"name">> => <<"venus">>, <<"x">> => 1.0, <<"y">> => 0.0, <<"z">> => 0.0,
      <<"orbit_radius">> => 40, <<"t0">> => math:pi(), <<"t">> => 10, <<"radius">> => 1.4}),
  {ok, [{planet, <<"sun">>, Pid1}, {planet, <<"earth">>, Pid2}, {planet, <<"venus">>, Pid3}]}.

handle_call({new_ship, Info}, _From, PidList) ->
  handle_new_ship(Info, PidList, PidList);
handle_call(_Request, _From, PidList) ->
  {ok, ObjectStatus, NewPidList} = getObjectStatus(PidList, [], []),
  check_collisions(ObjectStatus, PidList),
  {reply, ObjectStatus, NewPidList}.

handle_cast({fire, _Name} = Action, PidList) ->
  {noreply, call_to_ship(Action, PidList)};
handle_cast(Action, PidList) ->
  handle_action(Action, PidList).

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%======================================================================================================
% Internal
%======================================================================================================

handle_new_ship(#{<<"name">> := Name} = Info, PidList, []) ->
  {ok, Pid} = ship:start_link(Info),
  {reply, ok, [{ship, Name, Pid} | PidList]};
handle_new_ship(#{<<"name">> := Name}, PidList, [{ship, Name, _} | _]) ->
  {reply, {error, already_exists}, PidList};
handle_new_ship(Info, PidList, [_ | T]) ->
  handle_new_ship(Info, PidList, T).

check_collisions(StatusList, PidList) ->
  check_collisions(StatusList, StatusList, PidList).
check_collisions([], _StatusList, _PidList) ->
  ok;
check_collisions([Status | T], StatusList, PidList) ->
  check_collisions_impl(Status, StatusList, PidList),
  check_collisions(T, StatusList, PidList).

check_collisions_impl(_Status, [], _PidList) ->
  ok;
check_collisions_impl(Status, [Peer | T], PidList) ->
  case is_collision(Status, Peer) of
    {true, {Name1, Name2}} ->
      handle_action({collision, {Name1, Name2}}, PidList);
    _ -> ok
  end,
  check_collisions_impl(Status, T, PidList).

is_collision(#{<<"message">> := <<"died">>}, _) ->
  ok;
is_collision(_, #{<<"message">> := <<"died">>}) ->
  ok;
is_collision(#{<<"fired_by">> := Name}, #{<<"name">> := Name}) ->
  ok;
is_collision(#{<<"name">> := Name}, #{<<"fired_by">> := Name}) ->
  ok;
is_collision(#{<<"name">> := Name}, #{<<"name">> := Name}) ->
  ok;
is_collision(#{<<"type">> := <<"planet">>}, #{<<"type">> := <<"planet">>}) ->
  ok;
is_collision(#{<<"name">> := Name1, <<"position">> := #{<<"x">> := X1, <<"y">> := Y1, <<"z">> := Z1}, <<"radius">> := R1},
    #{<<"name">> := Name2, <<"position">> := #{<<"x">> := X2, <<"y">> := Y2, <<"z">> := Z2}, <<"radius">> := R2}) ->
  Distance = math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2) + math:pow(Z2 - Z1, 2)),
  if
    Distance < (R1 + R2) ->
      io:fwrite("Distance: ~p~n", [{Name1, Name2, R1, R2, Distance}]),
      {true, {Name1, Name2}};
    true -> ok
  end.

handle_action({fire, Name}, PidList) ->
  {ok, NewPidList} = call_to_ship({fire, Name}, PidList),
  {noreply, NewPidList};
handle_action(Action, PidList) ->
  cast_to_entity(Action, PidList),
  {noreply, PidList}.

getObjectStatus([], Statuses, NewPidList) ->
  {ok, Statuses, NewPidList};
getObjectStatus([{planet, _Name, Pid} = PidInfo | T], Statuses, NewPidList) ->
  getObjectStatus(T, [gen_server:call(Pid, []) | Statuses], [PidInfo | NewPidList]);
getObjectStatus([{_Id, _Name, Pid} = PidInfo | T], Statuses, NewPidList) ->
  case gen_server:call(Pid, []) of
    #{<<"message">> := <<"terminated">>} ->
      gen_server:stop(Pid),
      getObjectStatus(T, Statuses, NewPidList);
    Status ->
      getObjectStatus(T, [Status | Statuses], [PidInfo | NewPidList])
  end.

cast_to_entity(_, []) ->
  ok;
cast_to_entity(Action, [{planet, _, _} | T]) ->
  cast_to_entity(Action, T);
cast_to_entity({collision, {Name, _}} = Action, [{_, Name, Pid} | T]) ->
  gen_server:cast(Pid, collision),
  cast_to_entity(Action, T);
cast_to_entity({collision, {_, Name}} = Action, [{_, Name, Pid} | T]) ->
  gen_server:cast(Pid, collision),
  cast_to_entity(Action, T);
cast_to_entity({collision, _} = Action, [_ | T]) ->
  cast_to_entity(Action, T);
cast_to_entity({Action, Name}, [{_, Name, Pid} | _]) ->
  gen_server:cast(Pid, Action),
  cast_to_entity(done, []);
cast_to_entity(Action, [_ | T]) ->
  cast_to_entity(Action, T).

call_to_ship(Action, PidList) ->
  call_to_ship(Action, PidList, PidList).
call_to_ship(_Action, [], NewPidList) ->
  NewPidList;
call_to_ship({fire, Name}, [{ship, Name, Pid} | _], PidList) ->
  {ShotName, ShotPid} = gen_server:call(Pid, fire),
  [{shot, ShotName, ShotPid} | PidList];
call_to_ship(Action, [_ | T], PidList) ->
  call_to_ship(Action, T, PidList).

