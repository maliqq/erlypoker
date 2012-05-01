-record(hand, {id, rank, value, high = none, kicker = []}).

-include("hand/high_card.erl").
-include("hand/badugi_hand.erl").

-define(RANKS, [{"high", ?RANK_HIGH, fun high_card/1}, {"badugi", ?RANK_BADUGI, fun badugi_hand/1}]).

%%
hand_to_string(Hand) ->
	case Hand of
		false -> "[no hand]";
		_Else ->
			{N, What} = Hand#hand.rank,
			Rank = Hand#hand.id,
			{_, Names, _} = lists:keyfind(Rank, 1, ?RANKS),
			{_, Name} = lists:keyfind(N, 1, Names),
			io_lib:format("~s (~s) [~ts]", [Name, What, cards_to_string(Hand#hand.value)])
	end.

test_hand() ->
	{_, [[Arg]]} = init:get_argument(cards),
	Cards = parse_cards(Arg),
	{_, [[Rank]]} = init:get_argument(rank),
	{_, _, F} = lists:keyfind(Rank, 1, ?RANKS),
	Hand = F(Cards),
	io:format(hand_to_string(Hand)),
	io:format("~n").
