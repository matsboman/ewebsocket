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
  erlang:send_after(10, self(), timeout_tick),
  {ok, Seed}.

handle_call(_Request, _From, #{<<"directionI">> := _, <<"directionJ">> := _,
  <<"directionK">> := _, <<"message">> := _,
  <<"positionX">> := PosX, <<"positionY">> := PosY,
  <<"positionZ">> := PosZ, <<"speed">> := _Speed, <<"name">> := Name} = State) ->
%%  io:fwrite("handle_call ~p~n", [{{Name, {PosX, PosY, PosZ, 0, 0, 0}}, 0}]),
  {reply, {{binary_to_list(Name), {PosX, PosY, PosZ, 0, 0, 0}}, 0}, State}.

handle_cast(_Info, State) ->
  io:fwrite("handle_cast ~p~n", [?MODULE]),
  {noreply, State}.

handle_info(timeout_tick, #{<<"directionI">> := DirI, <<"directionJ">> := DirJ,
  <<"directionK">> := DirK, <<"message">> := _,
  <<"positionX">> := PosX, <<"positionY">> := PosY,
  <<"positionZ">> := PosZ, <<"speed">> := Speed, <<"name">> := Name} = Map) ->
  erlang:send_after(10, self(), timeout_tick),
  {NewPosX, NewPosY, NewPosZ} = forward({PosX, PosY, PosZ}, {DirI, DirJ, DirK}, Speed),
%%  io:fwrite("handle_info ship ~p~n", [{NewPosX, NewPosY, NewPosZ}]),
  {noreply, Map#{<<"positionX">> := NewPosX, <<"positionY">> := NewPosY, <<"positionZ">> := NewPosZ}};
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
  point:point_plus_vector({PosX, PosY, PosZ}, vector:multiply({DirI, DirJ, DirK}, Speed)).