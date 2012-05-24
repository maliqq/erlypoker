-module(secure).

-export([random/0]).

random() ->
  random:uniform().
