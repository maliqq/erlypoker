%%
-module(rake).
-export([test/0, rake/2, rake/3]).

rnd(Rake) -> erlang:trunc(Rake * 100) / 100.

rake(Amount, Percentage, Range) when is_tuple(Range) ->
  {Min, Max} = Range,
  Rake = Amount * Percentage,
  if
    Rake < Min ->
      0;
    Rake > Max ->
      Max;
    true ->
      rnd(Rake)
  end;

rake(Amount, Percentage, Max) ->
  rake(Amount, Percentage, {0, Max}).

rake(Amount, Percentage) ->
  rake(Amount, Percentage, Amount).

test() ->
  io:format("5% rake for $4.5 = ~p~n", [rake(4.5, 0.05)]),
  io:format("5% rake for $4.5 with 0.25 min = ~p~n", [rake(4.5, 0.05, {0.25, 1})]),
  io:format("5% rake for $4599 with 5 cap = ~p~n", [rake(4599, 0.05, 5)]).
