-module(ws_handler).

-export([init/2,
         websocket_handle/2]).

init(Req, State) ->
  {cowboy_websocket, Req, State}.

websocket_handle(Frame = {text, _}, State) ->
  lager:info("Received text frame ~p", [Frame]),
  {reply, Frame, State};
websocket_handle(Frame, State) ->
  lager:info("Received non text frame ~p", [Frame]),
  {ok, State}.
