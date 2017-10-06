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
                                           {"/websocket/:room", ws_handler, []},
                                           {'_', cowboy_static, {priv_file, webrtc_erlang, "/index.html"}}
                                           ]}
                                   ]),
  {ok, _} = cowboy:start_clear(my_http_listener,
                               [{port, 8080}],
                               #{env => #{dispatch => Dispatch}}
                              ),
  syn:init(),
  stun_listener:add_listener(3478, udp, [{use_turn, true},
                                         {auth_type, user},
                                         {auth_realm, <<"localhost">>}, %% FIXME remove hardcoding of this
                                         {auth_fun, fun(_User, _Realm) -> <<"credential">> end}]),
  webrtc_erlang_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
