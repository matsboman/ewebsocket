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
  MsgMap = jsone:decode(Msg),
  io:fwrite("websocket_handle ~p~n", [{State, MsgMap}]),
  handle_message(MsgMap, State);
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

handle_message(#{<<"message">> := <<"keepalive">>}, State) ->
  {reply, {text, jsone:encode(#{<<"message">> => <<"ping">>})}, State};
handle_message(#{<<"message">> := <<"newship">>} = MsgMap, State) ->
  gen_server:cast(game_handler, MsgMap),
  {reply, {text, jsone:encode(#{<<"message">> => <<"new ship created">>, <<"values">> => MsgMap})}, State}.

formatJSON([], JSONResult) ->
  {ok, jsone:encode(#{<<"message">> => <<"objects">>, <<"values">> => JSONResult})};

formatJSON([{{Id, {X, Y, Z, _, _, _}}, _} | T], JSONResult) ->
  JSONObject = #{<<"object">> => list_to_binary(Id),
    <<"position">> =>
    #{<<"x">> => float_to_binary(X * 1.0),
      <<"y">> => float_to_binary(Y * 1.0),
      <<"z">> => float_to_binary(Z * 1.0)}},
  formatJSON(T, [JSONObject | JSONResult]).
