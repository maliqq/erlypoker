-module(hand).

-export([main/0, to_string/1, hand/2, compare/3]).

-include("poker.hrl").
-include("card.hrl").
-include("hand.hrl").
-include("hand_high.erl").
-include("hand_badugi.erl").
-include("hand_low.erl").

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
to_string(_) ->
  ok.

main() ->
  {_, [[Arg]]} = init:get_argument(cards),
  Cards = card:wrap(Arg),
  {_, [[Rank]]} = init:get_argument(rank),
  Hand = hand(list_to_atom(Rank), Cards),
  io:format(to_string(Hand)),
  io:format("~n").
