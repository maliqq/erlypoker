-record(high_card, {rank, value, high = none, kicker = []}).

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

-define(HIGH_CARDS, [{0, "high_card"}, {1, "one_pair"}, {2, "two_pair"}, {3, "three_kind"}, {4, "straight"}, {5, "flush"}, {6, "full_house"}, {7, "four_kind"}, {8, "straight_flush"}]).

%%
high_card_to_string(Hand) ->
  {N, What} = Hand#high_card.rank,
  {_, Name} = lists:keyfind(N, 1, ?HIGH_CARDS),
  io_lib:format("~s (~s) [~ts]", [Name, What, cards_to_string(Hand#high_card.value)]).

%% 5 (or more) cards in a row
%% TODO ace to five straight
is_straight(Cards) ->
	Rows = [Row || Row <- split_rows(Cards), erlang:length(Row#straight_row.value) >= 5],
	if
		erlang:length(Rows) == 0 -> false;
		true ->
			[Straight | _] = Rows, %% we have max 7 cards per hand, one straight is possible
			Highest = highest_card(Straight#straight_row.value),
			#high_card{rank = {?STRAIGHT, Straight#straight_row.to}, value = Straight#straight_row.value, high = Highest}
	end.

%% 5 (or more) cards with same suit
is_flush(Cards) ->
	Suits = [Group || Group <- group_suits(Cards), erlang:length(Group#group_suit.value) >= 5],
	if
		erlang:length(Suits) == 0 -> false;
		true ->
			[Flush | _] = Suits, %% we have max 7 cards per hand, one flush is possible
			Highest = highest_card(Flush#group_suit.value),
			#high_card{rank = {?FLUSH, Flush#group_suit.suit}, value = Flush#group_suit.value, high = Highest}
	end.

%% 5 cards with same suit in a row
is_straight_flush(Cards) ->
	case is_flush(Cards) of
	  false -> high_card(Cards, [fun is_four_kind/1, fun is_full_house/1, fun is_straight/1]); %% skip flush
	  Flush ->
	    case is_straight(Flush#high_card.value) of
	      false -> high_card(Cards, [fun is_four_kind/1, fun is_full_house/1, fun is_flush/1, fun is_straight/1]);
	      Straight ->
	        {_, Kind} = Straight#high_card.rank, {_, Suit} = Flush#high_card.rank,
	        #high_card{rank = {?STRAIGHT_FLUSH, Kind, Suit}, value = Flush#high_card.value, high = build_card(Kind, Suit)}
	    end
  end.

%% AAAAB, "four of A with B kicker"
is_four_kind(Cards) ->
	Repeats = card_frequency(Cards, 4),
	if
		erlang:length(Repeats) == 0 -> false;
		true ->
			[FourKind | _] = Repeats, %% we have max 7 cards per hand, one four kind is possible
			#high_card{rank = {?FOUR_KIND, FourKind#group_kind.kind}, value = FourKind#group_kind.value, kicker = kicker_cards(Cards, FourKind#group_kind.value, 1)}
	end.

%% AAABC, "three of A with B and C kicker"
is_three_kind(Cards) ->
	Repeats = card_frequency(Cards, 3),
	if
		erlang:length(Repeats) == 1 ->
			[ThreeKind | _] = Repeats,
			#high_card{rank = {?THREE_KIND, ThreeKind#group_kind.kind}, value = ThreeKind#group_kind.value, kicker = kicker_cards(Cards, ThreeKind#group_kind.value, 2)};
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
					[Higher, Lower | _] = sorted_cards(ThreeKind),
					{Higher, Lower};
				true ->
				  {highest_card(ThreeKind), highest_card(Pairs)} %% one trips plus highest pair
			end,
			Value = Major#group_kind.value ++ Minor#group_kind.value,
			#high_card{rank = {?FULL_HOUSE, [Major#group_kind.kind, Minor#group_kind.kind]}, value = Value}
	end.

%% AABBC, "two pair of A's and B's with C kicker"
is_two_pair(Cards) ->
	Pairs = card_frequency(Cards, 2),
	if
		erlang:length(Pairs) >= 2 ->
			[Major, Minor | _] = sorted_cards(Pairs),
			Value = Major#group_kind.value ++ Minor#group_kind.value,
			#high_card{rank = {?TWO_PAIR, [Major#group_kind.kind, Minor#group_kind.kind]}, value = Value, kicker = kicker_cards(Cards, Value, 1)};
		true -> false
	end.

%% AABCD, "one pair of A with B, C, D kicker"
is_one_pair(Cards) ->
	Pairs = card_frequency(Cards, 2),
	if
		erlang:length(Pairs) == 1 ->
			[Pair | _] = Pairs,
			[First | _] = Pair#group_kind.value,
			#high_card{rank = {?ONE_PAIR, First#card.kind}, value = Pair#group_kind.value, kicker = kicker_cards(Cards, Pair#group_kind.value, 3)};
		true -> false
	end.

%% ABCDE, "high card A with B, C, D, E kicker"
is_high_card(Cards) ->
	Highest = highest_card(Cards),
	#high_card{rank = {?HIGH_CARD, Highest#card.kind}, value = [Highest], kicker = kicker_cards(Cards, [Highest], 4)}.

%% detect high card rank of the hand
high_card([]) -> throw("no cards given for hand");
high_card(Cards) when is_list(Cards) and erlang:length(Cards) > 7 -> throw("7 cards per hand allowed");
high_card(Cards) when is_list(Cards) ->
  high_card(Cards, [fun is_straight_flush/1, fun is_three_kind/1, fun is_two_pair/1, fun is_one_pair/1, fun is_high_card/1]).

high_card(Cards, []) -> false;
high_card(Cards, [F|List]) when is_function(F) ->
	case F(Cards) of
		false -> high_card(Cards, List);
		Hand -> Hand
	end.

test_high_card() ->
  ok.

