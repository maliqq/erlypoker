-module(hand).

-export([main/0, to_string/1, high_card/1, badugi_hand/1, ace5_low/1, ace5_low8/1, ace6_low/1, deuce7_low/1]).

-record(hand, {
    id,
    rank,
    value,
    high = none,
    kicker = []
  }).

-include("poker.hrl").
-include("card.hrl").
-include("hand.hrl").
-include("hand_high.erl").
-include("hand_badugi.erl").
-include("hand_low.erl").

hand(high, Cards) ->
  hand_high(Cards);
hand(badugi, Cards) ->
  hand_badugi(Cards);
hand(ace_five, Cards) ->
  hand_low(ace_five, Cards);
hand(ace_six, Cards) ->
  hand_low(ace_six, Cards);
hand(ace_six_better, Cards) ->
  hand_low(ace_six_better, Cards);
hand(deuce_seven, Cards) ->
  hand_low(deuce_seven, Cards);

%%
to_string(Hand) ->
  case Hand of
    false -> false;
    _Else ->
      {N, W} = Hand#hand.rank,
      {_, Name} = lists:keyfind(N, 1, ?NAMES),
      What = if
        N == ?FLUSH ->
          card:suit_to_string(W);
        true ->
          card:kind_to_string(W)
      end,
      io_lib:format("~s (~s) [~ts ]", [Name, What, card:to_string(Hand#hand.value)])
  end.

main() ->
  {_, [[Arg]]} = init:get_argument(cards),
  Cards = card:wrap(Arg),
  {_, [[Rank]]} = init:get_argument(rank),
  Hand = hand(list_to_atom(Rank), Cards),
  io:format(to_string(Hand)),
  io:format("~n").
