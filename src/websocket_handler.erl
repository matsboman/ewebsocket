-module(websocket_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(_State) ->
  io:fwrite("websocket_init~p~n", [self()]),
  erlang:start_timer(30, self(), <<"Hello!">>),
  {ok, 0}.

websocket_handle({text, Msg}, State) ->
  MsgMap = jsone:decode(Msg),
  handle_message(MsgMap, State);
websocket_handle(_Data, State) ->
  {ok, State}.

websocket_info({timeout, _Ref, _Msg}, State) ->
  erlang:start_timer(30, self(), <<"How' you doin'?">>),
  Result = gen_server:call(game_handler, []),
  {reply, {text, jsone:encode(#{<<"message">> => <<"status">>, <<"values">> => Result})}, State + 1};
websocket_info(_Info, State) ->
  {ok, State}.

%======================================================================================================
% Internal
%======================================================================================================
handle_message(#{<<"name">> := Name, <<"message">> := <<"yaw_right">>}, State) ->
  gen_server:cast(game_handler, {yaw_right, Name}),
  {reply, {text, jsone:encode(#{<<"message">> => <<"yaw_right done">>})}, State};
handle_message(#{<<"name">> := Name, <<"message">> := <<"yaw_left">>}, State) ->
  gen_server:cast(game_handler, {yaw_left, Name}),
  {reply, {text, jsone:encode(#{<<"message">> => <<"yaw_left done">>})}, State};
handle_message(#{<<"name">> := Name, <<"message">> := <<"fire">>}, State) ->
  gen_server:cast(game_handler, {fire, Name}),
  {reply, {text, jsone:encode(#{<<"message">> => <<"fired!">>})}, State};
handle_message(#{<<"message">> := <<"keepalive">>}, State) ->
  {reply, {text, jsone:encode(#{<<"message">> => <<"ping">>})}, State};
handle_message(#{<<"message">> := <<"newship">>} = Msg, State) ->
  case gen_server:call(game_handler, {new_ship, Msg}) of
    ok ->
      {reply, {text, jsone:encode(#{<<"message">> => <<"new ship created">>, <<"values">> => Msg})}, State};
    {error, already_exists} ->
      {reply, {text, jsone:encode(#{<<"message">> => <<"error, ship already exists">>})}, State}
  end.

