%%%-------------------------------------------------------------------
%% @doc webrtc_erlang public API
%% @end
%%%-------------------------------------------------------------------

-module(webrtc_erlang_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/js/[...]", cowboy_static, {priv_dir, webrtc_erlang, "/js"}},
                                           {"/css/[...]", cowboy_static, {priv_dir, webrtc_erlang, "/css"}},
                                           {"/websocket", ws_handler, []},
                                           {'_', cowboy_static, {priv_file, webrtc_erlang, "/index.html"}}
                                           ]}
                                   ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
                               [{port, 8080}],
                               #{env => #{dispatch => Dispatch}}
                              ),
  webrtc_erlang_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
