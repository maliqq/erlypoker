-module(secure).

-export([random/0, random/1]).

random() ->
  random:uniform().

random(N) ->
  random:uniform(N).
