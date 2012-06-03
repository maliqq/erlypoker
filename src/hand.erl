-module(hand).

-export([main/0, format/1, hand/2, compare/3]).

-include("card.hrl").
-include("hand.hrl").
-include("hand/high.hrl").
-include("hand/badugi.hrl").
-include("hand/low.hrl").

%%
hand(high, Cards) ->
  hand_high(Cards);
hand(badugi, Cards) ->
  hand_badugi(Cards);
hand(Low, Cards) ->
  hand_low(Low, Cards).

%% 0 - equal
compare(high, Hand1, Hand2) ->
  compare_high(Hand1, Hand2);
compare(badugi, Hand1, Hand2) ->
  compare_badugi(Hand1, Hand2);
compare(Low, Hand1, Hand2) ->
  compare_low(Low, Hand1, Hand2).

%%
format(_) ->
  ok.

main() ->
  {_, [[Arg]]} = init:get_argument(cards),
  Cards = card:wrap(Arg),
  {_, [[Rank]]} = init:get_argument(rank),
  Hand = hand(list_to_atom(Rank), Cards),
  io:format(format(Hand)),
  io:format("~n").
