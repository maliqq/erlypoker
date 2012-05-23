-module(secure).

-export([init_random/0, get_random/0]).

init_random() ->
  random:seed(erlang:now()).

get_random() ->
  random:uniform().
