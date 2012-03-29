-module(poker).
-export([poker/0]).

%% suits
-define(SUIT_SPADE, 1).
-define(SUIT_CLUB, 2).
-define(SUIT_HEART, 3).
-define(SUIT_DIAMOND, 4).

-define(SUITS, [{?SUIT_SPADE, "s"}, {?SUIT_CLUB, "c"}, {?SUIT_HEART, "h"}, {?SUIT_DIAMOND, "d"}]).

%% kinds
-define(KIND_DEUCE, 1).
-define(KIND_THREE, 2).
-define(KIND_FOUR, 3).
-define(KIND_FIVE, 4).
-define(KIND_SIX, 5).
-define(KIND_SEVEN, 6).
-define(KIND_EIGHT, 7).
-define(KIND_NINE, 8).
-define(KIND_TEN, 9).
-define(KIND_JACK, 10).
-define(KIND_QUEEN, 11).
-define(KIND_KING, 12).
-define(KIND_ACE, 13).

-define(KINDS, [{?KIND_DEUCE, "2"}, {?KIND_THREE, "3"}, {?KIND_FOUR, "4"}, {?KIND_FIVE, "5"}, {?KIND_SIX, "6"}, {?KIND_SEVEN, "7"}, {?KIND_NINE, "9"}, {?KIND_TEN, "T"}, {?KIND_JACK, "J"}, {?KIND_QUEEN, "Q"}, {?KIND_KING, "K"}, {?KIND_ACE, "A"}]).

%% types
-define(TYPE_HOLDEM, 1).
-define(TYPE_7CARD, 2).
-define(TYPE_DRAW, 3).

%% hand ranks
-define(RANK_HIGH, 1).
-define(RANK_LOW, 2).
-define(RANK_LOW8, 3).
-define(RANK_ACE6, 4).
-define(RANK_DEUCE7, 5).
-define(RANK_BADUGI, 6).

%% high card combinations
-define(HIGH_CARD, 1).
-define(STRAIGHT_FLUSH, 2).
-define(FOUR_KIND, 3).
-define(FULL_HOUSE, 4).
-define(FLUSH, 5).
-define(STRAIGHT, 6).
-define(THREE_KIND, 7).
-define(TWO_PAIR, 8).
-define(ONE_PAIR, 9).

%% games
-define(GAME_TEXAS, 1).
-define(GAME_OMAHA, 2).
-define(GAME_OMAHA8, 3).
-define(GAME_STUD, 4).
-define(GAME_STUD8, 5).
-define(GAME_RAZZ, 6).
-define(GAME_LONDON, 7).
-define(GAME_DRAW5, 8).
-define(GAME_SINGLE27, 9).
-define(GAME_TRIPLE27, 10).
-define(GAME_BADUGI, 11).

%% limits
-define(LIMIT_FIXED, 1).
-define(LIMIT_POT, 2).
-define(LIMIT_NO, 3).

cards() ->
	[new_card(Kind, Suit) || Kind <- ?KINDS, Suit <- ?SUITS].

shuffle(Cards) ->
	lists:map(fun(Elem) -> element(2, Elem) end, lists:keysort(1, lists:map(fun(Card) -> {random:uniform(), Card} end, Cards))).

new_deck() ->
	shuffle(cards()).

deck_to_string(Deck) ->
	lists:concat(lists:map(fun(Card) -> card_to_string(Card) end, Deck)).

suit_to_string(Suit) when is_integer(Suit) ->
	case Suit of
		?SUIT_SPADE -> "spade";
		?SUIT_CLUB -> "club";
		?SUIT_HEART -> "heart";
		?SUIT_DIAMOND -> "diamond"
	end.

suit_to_char(Suit) when is_integer(Suit) ->
	case Suit of
		?SUIT_SPADE -> "♠";
		?SUIT_HEART -> "♥";
		?SUIT_DIAMOND -> "♦";
		?SUIT_CLUB -> "♣"
	end.

kind_to_string(Kind) when is_integer(Kind) ->
	case Kind of
		?KIND_DEUCE -> "deuce";
		?KIND_THREE -> "three";
		?KIND_FOUR -> "four";
		?KIND_FIVE -> "five";
		?KIND_SIX -> "six";
		?KIND_SEVEN -> "seven";
		?KIND_EIGHT -> "eight";
		?KIND_NINE -> "nine";
		?KIND_TEN -> "ten";
		?KIND_JACK -> "jack";
		?KIND_QUEEN -> "queen";
		?KIND_KING -> "king";
		?KIND_ACE -> "ace"
	end.

kind_to_char(Kind) when is_integer(Kind) ->
	element(2, lists:keyfind(Kind, 1, ?KINDS)).

kind_from_string(String) ->
	element(1, lists:keyfind(String, 2, ?KINDS)).

suit_from_string(String) ->
	element(1, lists:keyfind(String, 2, ?SUITS)).

new_card(Kind, Suit) when is_integer(Kind), is_integer(Suit) ->
	{Kind, Suit};

new_card(Kind, Suit) ->
	new_card(kind_from_string(Kind), suit_from_string(Suit)).

cards_from_string(String) ->
	lists:map(fun(Match1) ->
		[K, S] = re:split(string:substr(String, element(1, Match1) + 1, element(2, Match1) + 1), "", [{return, list}, {parts, 2}]),
		new_card(K, S)
	end, element(2, re:run(String, "[AKQJT0-9]{1}[schd]{1}"))).

card_to_string(Card) ->
	kind_to_char(element(1, Card)) ++ suit_to_char(element(2, Card)).

hand_to_string(Hand) ->
	case Hand of
		?FULL_HOUSE -> "full house";
		?HIGH_CARD -> "high card"
	end.

rank_high(Cards) ->
	rank_high(Cards, [fun is_straight_flush/1, fun is_four_kind/1, fun is_full_house/1, fun is_flush/1, fun is_straight/1, fun is_three_kind/1, fun is_two_pair/1, fun is_one_pair/1, fun is_high_card/1]).

rank_high(Cards, []) ->
	?HIGH_CARD;

rank_high(Cards, [F|List]) ->
	case F(Cards) of
		none -> rank_high(Cards, List);
		Hand -> hand_to_string(Hand)
	end.

is_straight_flush(Cards) ->
	none.

is_four_kind(Cards) ->
	none.

is_full_house(Cards) ->
	none.

paired(Cards) ->
	[].

suited(Cards) ->
	[].

is_flush(Cards) ->
	none.

is_straight(Cards) ->
	none.

is_three_kind(Cards) ->
	none.

is_two_pair(Cards) ->
	none.

is_one_pair(Cards) ->
	none.

is_high_card(Cards) ->
	?HIGH_CARD.

rank_low(Cards) ->
	ok.

rank_low8(Cards) ->
	ok.

rank_ace6(Cards) ->
	ok.

rank_deuce7(Cards) ->
	ok.

rank_badugi(Cards) ->
	ok.

new_hand(Cards, Rank) ->
	case Rank of
		?RANK_HIGH -> rank_high(Cards);
		?RANK_LOW -> rank_low(Cards);
		?RANK_LOW8 -> rank_low8(Cards);
		?RANK_ACE6 -> rank_ace6(Cards);
		?RANK_DEUCE7 -> rank_deuce7(Cards);
		?RANK_BADUGI -> rank_badugi(Cards)
	end.

-define(CARD, "Ah").

poker() ->
	io:format(card_to_string(lists:nth(1, cards_from_string(?CARD)))).
	%%io:format(deck_to_string(new_deck())).

