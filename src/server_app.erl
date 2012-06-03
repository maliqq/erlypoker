-module(server_app).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
  application:start(mongodb),
	application:start(riakc),
	application:start(cowboy),
  application:start(server).

start(_, _) ->
  Dispatch = [{'_', [{'_', handler, []}]}],
  cowboy:start_listener(my_http_listener, 100,
    cowboy_tcp_transport, [{port, 8080}],
    cowboy_http_protocol, [{dispatch, Dispatch}]
  ),
  server_sup:start_link().

stop(_) ->
  ok.
