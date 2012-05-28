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

%% detect high card rank of the hand
hand_high(Cards) when is_list(Cards) and (erlang:length(Cards) =< 7) and (erlang:length(Cards) > 0) ->
  hand_high(Cards, [
  	fun is_straight_flush/1, fun is_three_kind/1, fun is_two_pair/1, fun is_one_pair/1, fun is_high_card/1
	]).

hand_high(_, []) ->
	false;
hand_high(Cards, [F|List]) when is_function(F) ->
	case F(Cards) of
		false -> hand_high(Cards, List);
		Hand -> Hand
	end.

%% 5 (or more) cards in a row
%% TODO A-5 straight
is_straight(Cards) ->
	Rows = [Row || Row <- card:split_rows(Cards), erlang:length(Row#card_group.value) >= 5],
	if
		erlang:length(Rows) == 0 ->
			false;
		true ->
			[Straight | _] = Rows, %% we have max 7 cards per hand, one straight is possible
			Highest = card:highest(Straight#card_group.value),
			#hand{rank = {?STRAIGHT, Straight#card_group.kind}, value = Straight#card_group.value, high = Highest}
	end.

%% 5 (or more) cards with same suit
is_flush(Cards) ->
	Suits = [Group || Group <- card:group_suits(Cards), erlang:length(Group#card_group.value) >= 5],
	if
		erlang:length(Suits) == 0 ->
			false;
		true ->
			[Flush | _] = Suits, %% we have max 7 cards per hand, one flush is possible
			Highest = card:highest(Flush#card_group.value),
			#hand{rank = {?FLUSH, Flush#card_group.suit}, value = Flush#card_group.value, high = Highest}
	end.

%% 5 cards with same suit in a row
is_straight_flush(Cards) ->
	case is_flush(Cards) of
	  false ->
	  	hand_high(Cards, [fun is_four_kind/1, fun is_full_house/1, fun is_straight/1]); %% skip flush
	  Flush ->
	    case is_straight(Flush#hand.value) of
	      false -> hand_high(Cards, [fun is_four_kind/1, fun is_full_house/1]); %% skip straight & flush
	      Straight ->
	        {_, Kind} = Straight#hand.rank, {_, Suit} = Flush#hand.rank,
	        #hand{rank = {?STRAIGHT_FLUSH, Kind}, value = Flush#hand.value, high = card:new(Kind, Suit)}
	    end
  end.

%% AAAAB, "four of A with B kicker"
is_four_kind(Cards) ->
	Repeats = card:freq(Cards, 4),
	if
		erlang:length(Repeats) == 0 ->
			false;
		true ->
			[FourKind | _] = Repeats, %% we have max 7 cards per hand, one four kind is possible
			#hand{rank = {?FOUR_KIND, FourKind#card_group.kind}, value = FourKind#card_group.value, kicker = card:kickers(Cards, FourKind#card_group.value, 1)}
	end.

%% AAABC, "three of A with B and C kicker"
is_three_kind(Cards) ->
	Repeats = card:freq(Cards, 3),
	if
		erlang:length(Repeats) == 1 ->
			[ThreeKind | _] = Repeats,
			#hand{rank = {?THREE_KIND, ThreeKind#card_group.kind}, value = ThreeKind#card_group.value, kicker = card:kickers(Cards, ThreeKind#card_group.value, 2)};
		true ->
			false
	end.

%% AAABB, "A's full of B", no kickers
is_full_house(Cards) ->
	ThreeKind = card:freq(Cards, 3),
	Pairs = card:freq(Cards, 2),
	if
		erlang:length(ThreeKind) == 0 ->
			false;
		erlang:length(ThreeKind) > 0, erlang:length(Pairs) == 0 ->
			false;
		true ->
			{Major, Minor} = if
				erlang:length(ThreeKind) >= 2 -> %% two "tripses" are one full house
					[Higher, Lower | _] = card:arrange(ThreeKind),
					{Higher, Lower};
				true ->
				  {card:highest(ThreeKind), card:highest(Pairs)} %% one trips plus highest pair
			end,
			Value = Major#card_group.value ++ Minor#card_group.value,
			#hand{rank = {?FULL_HOUSE, [Major#card_group.kind, Minor#card_group.kind]}, value = Value}
	end.

%% AABBC, "two pair of A's and B's with C kicker"
is_two_pair(Cards) ->
	Pairs = card:freq(Cards, 2),
	if
		erlang:length(Pairs) >= 2 ->
			[Major, Minor | _] = card:arrange(Pairs),
			Value = Major#card_group.value ++ Minor#card_group.value,
			#hand{rank = {?TWO_PAIR, [Major#card_group.kind, Minor#card_group.kind]}, value = Value, kicker = card:kickers(Cards, Value, 1)};
		true ->
			false
	end.

%% AABCD, "one pair of A with B, C, D kicker"
is_one_pair(Cards) ->
	Pairs = card:freq(Cards, 2),
	if
		erlang:length(Pairs) == 1 ->
			[Pair | _] = Pairs,
			[First | _] = Pair#card_group.value,
			#hand{rank = {?ONE_PAIR, First#card.kind}, value = Pair#card_group.value, kicker = card:kickers(Cards, Pair#card_group.value, 3)};
		true ->
			false
	end.

%% ABCDE, "high card A with B, C, D, E kicker"
is_hand_high(Cards) ->
	Highest = card:highest(Cards),
	#hand{rank = {?HIGH_CARD, Highest#card.kind}, value = [Highest], kicker = card:kickers(Cards, [Highest], 4)}.

high_card_test() ->
  ok.
