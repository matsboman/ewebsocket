%%%-------------------------------------------------------------------
%%% @author mb189v
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2019 2:01 PM
%%%-------------------------------------------------------------------
-module(ship).
-author("mb189v").
-behaviour(gen_server).

%% API.
-export([start_link/1]).

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

-spec start_link(tuple) -> {ok, pid()}.
start_link(Seed) ->
  io:fwrite("start_link ~p~n", [Seed]),
  gen_server:start_link(?MODULE, Seed, []).

%======================================================================================================
% gen_server stuff
%======================================================================================================

init(Seed) ->
  io:fwrite("init ship~p~n", [Seed]),
  erlang:send_after(20, self(), timeout_tick),
  {ok, Seed}.

handle_call({fire, Name}, _From, #{<<"name">> := Name} = Map) ->
  io:fwrite("ship firing...~n", []),
  ShotsFired = maps:get(<<"shots_fired">>, Map) + 1,
  ShotMap = Map#{<<"fired_by">> => Name, <<"message">> => <<"new">>, <<"speed">> => maps:get(<<"speed">>, Map) * 10,
    <<"name">> => list_to_binary(binary_to_list(Name) ++ integer_to_list(ShotsFired))},
  {ok, Pid} = shot:start_link(ShotMap),
  {reply, Pid, Map#{<<"shots_fired">> := ShotsFired}};
handle_call({fire, _}, _From, Map) ->
  {reply, no_action, Map};
handle_call(_Request, _From, State) ->
  JSONObject = State#{<<"type">> => <<"ship">>},
  {reply, JSONObject, State}.

handle_cast(_, #{<<"message">> := <<"died">>} = State) ->
  io:fwrite("handle_cast no_action died already: ~p~n", [State]),
  {noreply, State};
handle_cast({yaw_right, Name}, #{<<"direction">> := #{<<"i">> := DirI, <<"j">> := DirJ, <<"k">> := DirK}, <<"name">> := Name} = Map) ->
  [NewDirI, NewDirJ, NewDirK] = yaw_right({DirI, DirJ, DirK}),
  {noreply, Map#{<<"direction">> := #{<<"i">> => NewDirI, <<"j">> => NewDirJ, <<"k">> => NewDirK}}};
handle_cast({yaw_left, Name}, #{<<"direction">> := #{<<"i">> := DirI, <<"j">> := DirJ, <<"k">> := DirK}, <<"name">> := Name} = Map) ->
  [NewDirI, NewDirJ, NewDirK] = yaw_left({DirI, DirJ, DirK}),
  {noreply, Map#{<<"direction">> := #{<<"i">> => NewDirI, <<"j">> => NewDirJ, <<"k">> => NewDirK}}};
handle_cast({collision, {Name1, _Name2}}, #{<<"name">> := Name1} = State) ->
  io:fwrite("handle_cast collision: ~p~n", [State]),
  erlang:send_after(5000, self(), terminate),
  {noreply, State#{<<"message">> := <<"died">>}};
handle_cast({collision, {_Name1, Name2}}, #{<<"name">> := Name2} = State) ->
  io:fwrite("handle_cast collision: ~p~n", [State]),
  erlang:send_after(5000, self(), terminate),
  {noreply, State#{<<"message">> := <<"died">>}};
handle_cast(Info, State) ->
  io:fwrite("handle_cast no_action: ~p~n", [Info]),
  {noreply, State}.

handle_info(timeout_tick, #{<<"direction">> := #{<<"i">> := DirI, <<"j">> := DirJ, <<"k">> := DirK},
  <<"position">> := #{<<"x">> := PosX, <<"y">> := PosY, <<"z">> := PosZ}, <<"speed">> := Speed} = Map) ->
  erlang:send_after(20, self(), timeout_tick),
  {NewPosX, NewPosY, NewPosZ} = forward({PosX, PosY, PosZ}, {DirI, DirJ, DirK}, Speed),
  {noreply, Map#{<<"position">> := #{<<"x">> => NewPosX, <<"y">> => NewPosY, <<"z">> => NewPosZ}, <<"speed">> := Speed}};
handle_info(terminate, State) ->
  io:fwrite("handle_info terminate: ~p~n", [State]),
  {noreply, State#{<<"message">> := <<"terminated">>}};
handle_info(_Info, State) ->
  io:fwrite("handle_info: ~p~n", [State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%======================================================================================================
% Internal
%======================================================================================================

forward({PosX, PosY, PosZ}, {DirI, DirJ, DirK}, Speed) ->
  vector_math:point_plus_vector({PosX, PosY, PosZ},
    vector_math:multiply_vector_by_scalar({DirI, DirJ, DirK}, Speed)).

yaw_right({DirI, DirJ, DirK}) ->
%%  rotate the forward and right vectors around the up vector axis for yaw
  vector_math:rotate_around_vector([DirI, DirJ, DirK], [0, 1, 0], -math:pi() / 100).
%% rightVec = forwardVec.cross(Vector(0,1,0)) - not needed right now

yaw_left({DirI, DirJ, DirK}) ->
%%  rotate the forward and right vectors around the up vector axis for yaw
  vector_math:rotate_around_vector([DirI, DirJ, DirK], [0, 1, 0], math:pi() / 100).
%% rightVec = forwardVec.cross(Vector(0,1,0)) - not needed right now
