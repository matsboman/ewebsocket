-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  io:fwrite("init~n", []),
  {cowboy_websocket, Req, Opts}.

websocket_init(_State) ->
  io:fwrite("websocket_init~p~n", [self()]),
  erlang:start_timer(20, self(), <<"Hello!">>),
  {ok, 0}.

websocket_handle({text, Msg}, State) ->
  io:fwrite("websocket_handle ~p~n", [State]),
  {reply, {text, << "That is what she said!", Msg/binary >>}, State};
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, _Msg}, State) ->
  erlang:start_timer(20, self(), <<"How' you doin'?">>),
  Result = gen_server:call(game_handler, []),
%%  io:fwrite("game_handler result: ~p~n", [Result]),
  {ok, JSONResult} = formatJSON(Result, []),
%%  io:fwrite("JSONResult ~p~n", [JSONResult]),
  {reply, {text, JSONResult}, State + 1};
websocket_info(_Info, State) ->
  {ok, State}.

%======================================================================================================
% Internal
%======================================================================================================

formatJSON([], JSONResult) ->
  {ok, jsone:encode(JSONResult)};
formatJSON([{Id, Pos, _} | T], JSONResult) ->
  JSONObject = #{<<"object">> => list_to_binary(Id), <<"position">> => list_to_binary(integer_to_list(Pos))},
  formatJSON(T, [JSONObject | JSONResult]).
