%%%-------------------------------------------------------------------
%%% @author mb189v
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2019 2:01 PM
%%%-------------------------------------------------------------------
-module(game_object).
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
  io:fwrite("init game_object~p~n", [Seed]),
  erlang:send_after(10, self(), timeout_tick),
  {ok, Seed}.

handle_call(_Request, _From, State) ->
%%  io:fwrite("handle_call ~p~n", [State]),
  {reply, State, State}.

handle_cast(_Info, State) ->
  io:fwrite("handle_cast ~p~n", [?MODULE]),
  {noreply, State}.

handle_info(timeout_tick, {Id, Position, _}) when Position > 400 ->
  erlang:send_after(10, self(), timeout_tick),
  {noreply, {Id, Position + 1 * -1, -1}};
handle_info(timeout_tick, {Id, Position, _}) when Position < -400 ->
  erlang:send_after(10, self(), timeout_tick),
  {noreply, {Id, Position + 1, 1}};
handle_info(timeout_tick, {Id, Position, Direction}) ->
  erlang:send_after(10, self(), timeout_tick),
  {noreply, {Id, Position + 1 * Direction, Direction}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.