%%%-------------------------------------------------------------------
%% @doc ewebsocket public API
%% @end
%%%-------------------------------------------------------------------

-module(ewebsocket_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, websocket, "index.html"}},
            {"/websocket", websocket_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, websocket, "static"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 9090}], #{
        env => #{dispatch => Dispatch}
    }),
    ewebsocket_sup:start_link().

stop(_State) ->
    ok.
