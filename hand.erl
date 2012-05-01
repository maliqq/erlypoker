-record(hand, {
    id,
    rank,
    value,
    high = none,
    kicker = []
  }).

-include("hand/high_card.erl").
-include("hand/badugi_hand.erl").
-include("hand/low_hand.erl").

-define(RANKS, [{"high", fun high_card/1}, {"badugi", fun badugi_hand/1}, {"low", fun ace5_low/1}, {"low8", fun ace5_low8/1}, {"deuce7", fun deuce7_low/1}, {"ace6", fun ace6_low/1}]).
-define(RANK_NAMES, [
    %% high card
    {0, "high_card"}, {1, "one_pair"}, {2, "two_pair"}, {3, "three_kind"}, {4, "straight"}, {5, "flush"}, {6, "full_house"}, {7, "four_kind"}, {8, "straight_flush"},
    %% badugi
    {9, "one_card"}, {10, "two_card"}, {11, "three_card"}, {12, "four_card"}
  ]).

%%
hand_to_string(Hand) ->
  case Hand of
    false -> "[no hand]";
    _Else ->
      {N, W} = Hand#hand.rank,
      {_, Name} = lists:keyfind(N, 1, ?RANK_NAMES),
      What = if
        N == ?FLUSH ->
          suit_to_string(W);
        true ->
          kind_to_string(W)
      end,
      io_lib:format("~s (~s) [~ts ]", [Name, What, cards_to_string(Hand#hand.value)])
  end.

test_hand() ->
  {_, [[Arg]]} = init:get_argument(cards),
  Cards = parse_cards(Arg),
  {_, [[Rank]]} = init:get_argument(rank),
  {_, F} = lists:keyfind(Rank, 1, ?RANKS),
  Hand = F(Cards),
  io:format(hand_to_string(Hand)),
  io:format("~n").
