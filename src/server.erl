-module(server).
-behaviour(gen_server).

-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
  gen_server:start_link({local, server}, server, [], []).

alloc() ->
  gen_server:call(server, alloc).

free(Channel) ->
  gen_server:cast(server, {free, Channel}).

init(_) ->
  {ok, channels()}.

handle_call(alloc, _, Channels) ->
  {Channel, Allocated} = alloc(Channels),
  {reply, Channel, Allocated}.

handle_cast({free, Channel}, Channels) ->
  Free = free(Channel, Channels),
  {noreply, Free}.

terminate(_, _) ->
  ok.
