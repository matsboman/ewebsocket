%%%-------------------------------------------------------------------
%%% @author mb189v
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2019 2:01 PM
%%%-------------------------------------------------------------------
-module(shot).
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
  io:fwrite("init shot~p~n", [Seed]),
  erlang:send_after(20, self(), timeout_tick),
  erlang:send_after(10000, self(), die),
  erlang:send_after(15000, self(), terminate),
  {ok, Seed}.

handle_call(_Request, _From, State) ->
  JSONObject = State#{<<"type">> => <<"shot">>},
  {reply, JSONObject, State}.

handle_cast(_Info, State) ->
  {noreply, State}.

handle_info(timeout_tick, #{<<"direction">> := #{<<"i">> := DirI, <<"j">> := DirJ, <<"k">> := DirK},
  <<"position">> := #{<<"x">> := PosX,<<"y">> := PosY,<<"z">> := PosZ}, <<"speed">> := Speed} = Map) ->
  erlang:send_after(20, self(), timeout_tick),
  {NewPosX, NewPosY, NewPosZ} = forward({PosX, PosY, PosZ}, {DirI, DirJ, DirK}, Speed),
  {noreply, Map#{<<"position">> := #{<<"x">> => NewPosX,<<"y">> => NewPosY,<<"z">> => NewPosZ}}};
handle_info(die, State) ->
  io:fwrite("handle_info die: ~p~n", [State]),
  {noreply, State#{<<"message">> := <<"died">>}};
handle_info(terminate, State) ->
  io:fwrite("handle_info die: ~p~n", [State]),
  {noreply, State#{<<"message">> := <<"terminated">>}};
handle_info(_Info, State) ->
  io:fwrite("handle_info: ~p~n", [State]),
  {noreply, State}.

terminate(_Reason, State) ->
  io:fwrite("terminate: ~p~n", [State]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%======================================================================================================
% Internal
%======================================================================================================

forward({PosX, PosY, PosZ}, {DirI, DirJ, DirK}, Speed) ->
  vector_math:point_plus_vector({PosX, PosY, PosZ},
    vector_math:multiply_vector_by_scalar({DirI, DirJ, DirK}, Speed)).

