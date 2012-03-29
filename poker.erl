-module(poker).
-export([poker/0]).

%% suits
-define(SUIT_SPADE, 1).
-define(SUIT_CLUB, 2).
-define(SUIT_HEART, 3).
-define(SUIT_DIAMOND, 4).

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
		?SUIT_CLUB -> "♥";
		?SUIT_HEART -> "♦";
		?SUIT_DIAMOND -> "♣"
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
	case Kind of
		?KIND_DEUCE -> "2";
		?KIND_THREE -> "3";
		?KIND_FOUR -> "4";
		?KIND_FIVE -> "5";
		?KIND_SIX -> "6";
		?KIND_SEVEN -> "7";
		?KIND_EIGHT -> "8";
		?KIND_NINE -> "9";
		?KIND_TEN -> "T";
		?KIND_JACK -> "J";
		?KIND_QUEEN -> "Q";
		?KIND_KING -> "K";
		?KIND_ACE -> "A"
	end.

card(Kind, Suit) ->
	{Kind, Suit}.

card_to_string(Card) ->
	kind_to_char(element(1, Card)) ++ suit_to_char(element(2, Card)).

poker() ->
	io:format(card_to_string(card(11, 2))).

