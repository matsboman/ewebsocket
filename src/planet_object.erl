%%%-------------------------------------------------------------------
%%% @author mb189v
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2019 2:01 PM
%%%-------------------------------------------------------------------
-module(planet_object).
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
  io:fwrite("init planet_object~p~n", [Seed]),
  erlang:send_after(20, self(), timeout_tick),
  {ok, {Seed, 0}}.

handle_call(_Request, _From, {{Id, {X, Y, Z, _R, _T0, _T}}, _Tau} = State) ->
  JSONObject = #{<<"type">> => <<"planet">>, <<"name">> => list_to_binary(Id),
    <<"position">> =>
    #{<<"x">> => X,
      <<"y">> => Y,
      <<"z">> => Z}},
  {reply, JSONObject, State}.

handle_cast(Info, State) ->
%%  io:fwrite("handle_cast ~p~n", [Info]),
  {noreply, State}.

handle_info(timeout_tick, {{Id, {_X, Y, _Z, R, T0, T}}, Tau}) ->
  erlang:send_after(20, self(), timeout_tick),
  NewTau = Tau + 0.01,
  NewX = R * math:cos(2 * math:pi() * (NewTau - T0) / T),
  NewZ = R * math:sin(2 * math:pi() * (NewTau - T0) / T),
  {noreply, {{Id, {NewX, Y, NewZ, R, T0, T}}, NewTau}};
handle_info(_Info, State) ->
  io:fwrite("handle_info: ~p~n", [State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.