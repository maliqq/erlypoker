-define(RANK_HIGH, 0).
-define(RANK_LOW, 1).
-define(RANK_LOW8, 2).
-define(RANK_DEUCE7, 3).
-define(RANK_ACE6, 4).
-define(RANK_BADUGI, 5).

-record(hand, {pot, deck, bets}).

-include("hand/high_card.erl").
-include("hand/badugi_hand.erl").

test_hand() ->
	{_, [Args]} = init:get_argument(cards),
	[Cards] = Args,
	io:format(high_card_to_string(high_card(parse_cards(Cards)))).
