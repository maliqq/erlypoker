%%
-module(rake).

-export([rake/2, rake/3]).

-include_lib("eunit/include/eunit.hrl").

rake(R) ->
  erlang:round(R * 100) / 100.

rake(Amount, Percentage, Range) when is_tuple(Range) ->
  {Min, Max} = Range,
  R = Amount * Percentage / 100,
  if
    R < Min ->
      0;
    R > Max ->
      Max;
    true ->
      rake(R)
  end;

rake(Amount, Percentage, Max) ->
  rake(Amount, Percentage, {0, Max}).

rake(Amount, Percentage) ->
  rake(Amount, Percentage, Amount).

rake_test() ->
  ?assertEqual(0.23, rake(4.5, 5)),
  ?assertEqual(0, rake(4.5, 5, {0.25, 1})),
  ?assertEqual(5, rake(4599, 3.33333, 5)).
