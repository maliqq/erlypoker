%% hand ranking: from high card to straight flush
-define(HIGH_CARD, 0).
-define(ONE_PAIR, 1).
-define(TWO_PAIR, 2).
-define(THREE_KIND, 3).
-define(STRAIGHT, 4).
-define(FLUSH, 5).
-define(FULL_HOUSE, 6).
-define(FOUR_KIND, 7).
-define(STRAIGHT_FLUSH, 8).

-define(HIGH, "high").
-define(RANK_HIGH, [{0, "high_card"}, {1, "one_pair"}, {2, "two_pair"}, {3, "three_kind"}, {4, "straight"}, {5, "flush"}, {6, "full_house"}, {7, "four_kind"}, {8, "straight_flush"}]).

%% 5 (or more) cards in a row
%% TODO ace to five straight
is_straight(Cards) ->
  Rows = [Row || Row <- split_rows(Cards), erlang:length(Row#card_group.value) >= 5],
  if
    erlang:length(Rows) == 0 -> false;
    true ->
      [Straight | _] = Rows, %% we have max 7 cards per hand, one straight is possible
      Highest = highest_card(Straight#card_group.value),
      #hand{id = ?RANK_HIGH, rank = {?STRAIGHT, Straight#card_group.kind}, value = Straight#card_group.value, high = Highest}
  end.

%% 5 (or more) cards with same suit
is_flush(Cards) ->
  Suits = [Group || Group <- group_suits(Cards), erlang:length(Group#card_group.value) >= 5],
  if
    erlang:length(Suits) == 0 -> false;
    true ->
      [Flush | _] = Suits, %% we have max 7 cards per hand, one flush is possible
      Highest = highest_card(Flush#card_group.value),
      #hand{id = ?RANK_HIGH, rank = {?FLUSH, Flush#card_group.suit}, value = Flush#card_group.value, high = Highest}
  end.

%% 5 cards with same suit in a row
is_straight_flush(Cards) ->
  case is_flush(Cards) of
    false -> high_card(Cards, [fun is_four_kind/1, fun is_full_house/1, fun is_straight/1]); %% skip flush
    Flush ->
      case is_straight(Flush#hand.value) of
        false -> high_card(Cards, [fun is_four_kind/1, fun is_full_house/1, fun is_flush/1, fun is_straight/1]);
        Straight ->
          {_, Kind} = Straight#hand.rank, {_, Suit} = Flush#hand.rank,
          #hand{id = ?RANK_HIGH, rank = {?STRAIGHT_FLUSH, Kind, Suit}, value = Flush#hand.value, high = build_card(Kind, Suit)}
      end
  end.

%% AAAAB, "four of A with B kicker"
is_four_kind(Cards) ->
  Repeats = card_frequency(Cards, 4),
  if
    erlang:length(Repeats) == 0 -> false;
    true ->
      [FourKind | _] = Repeats, %% we have max 7 cards per hand, one four kind is possible
      #hand{id = ?RANK_HIGH, rank = {?FOUR_KIND, FourKind#card_group.kind}, value = FourKind#card_group.value, kicker = kicker_cards(Cards, FourKind#card_group.value, 1)}
  end.

%% AAABC, "three of A with B and C kicker"
is_three_kind(Cards) ->
  Repeats = card_frequency(Cards, 3),
  if
    erlang:length(Repeats) == 1 ->
      [ThreeKind | _] = Repeats,
      #hand{id = ?RANK_HIGH, rank = {?THREE_KIND, ThreeKind#card_group.kind}, value = ThreeKind#card_group.value, kicker = kicker_cards(Cards, ThreeKind#card_group.value, 2)};
    true -> false
  end.

%% AAABB, "A's full of B", no kickers
is_full_house(Cards) ->
  ThreeKind = card_frequency(Cards, 3),
  Pairs = card_frequency(Cards, 2),
  if
    erlang:length(ThreeKind) == 0 -> false;
    erlang:length(ThreeKind) > 0, erlang:length(Pairs) == 0 -> false;
    true ->
      {Major, Minor} = if
        erlang:length(ThreeKind) >= 2 -> %% two "tripses" are one full house
          [Higher, Lower | _] = high_cards(ThreeKind),
          {Higher, Lower};
        true ->
          {highest_card(ThreeKind), highest_card(Pairs)} %% one trips plus highest pair
      end,
      Value = Major#card_group.value ++ Minor#card_group.value,
      #hand{id = ?RANK_HIGH, rank = {?FULL_HOUSE, [Major#card_group.kind, Minor#card_group.kind]}, value = Value}
  end.

%% AABBC, "two pair of A's and B's with C kicker"
is_two_pair(Cards) ->
  Pairs = card_frequency(Cards, 2),
  if
    erlang:length(Pairs) >= 2 ->
      [Major, Minor | _] = high_cards(Pairs),
      Value = Major#card_group.value ++ Minor#card_group.value,
      #hand{id = ?RANK_HIGH, rank = {?TWO_PAIR, [Major#card_group.kind, Minor#card_group.kind]}, value = Value, kicker = kicker_cards(Cards, Value, 1)};
    true -> false
  end.

%% AABCD, "one pair of A with B, C, D kicker"
is_one_pair(Cards) ->
  Pairs = card_frequency(Cards, 2),
  if
    erlang:length(Pairs) == 1 ->
      [Pair | _] = Pairs,
      [First | _] = Pair#card_group.value,
      #hand{id = ?RANK_HIGH, rank = {?ONE_PAIR, First#card.kind}, value = Pair#card_group.value, kicker = kicker_cards(Cards, Pair#card_group.value, 3)};
    true -> false
  end.

%% ABCDE, "high card A with B, C, D, E kicker"
is_high_card(Cards) ->
  Highest = highest_card(Cards),
  #hand{id = ?RANK_HIGH, rank = {?HIGH_CARD, Highest#card.kind}, value = [Highest], kicker = kicker_cards(Cards, [Highest], 4)}.

%% detect high card rank of the hand
high_card([]) -> throw("no cards given for hand");
high_card(Cards) when is_list(Cards) and erlang:length(Cards) > 7 -> throw("7 cards per hand allowed");
high_card(Cards) when is_list(Cards) ->
  high_card(Cards, [fun is_straight_flush/1, fun is_three_kind/1, fun is_two_pair/1, fun is_one_pair/1, fun is_high_card/1]).

high_card(_, []) -> false;
high_card(Cards, [F|List]) when is_function(F) ->
  case F(Cards) of
    false -> high_card(Cards, List);
    Hand -> Hand
  end.

test_high_card() ->
  ok.

