-module(poker).
-export([poker/0]).

%% suits
-define(SUIT_SPADE, 0).
-define(SUIT_CLUB, 1).
-define(SUIT_HEART, 2).
-define(SUIT_DIAMOND, 3).

%% cards
-define(CARD_DEUCE, 2).
-define(CARD_THREE, 3).
-define(CARD_FOUR, 4).
-define(CARD_FIVE, 5).
-define(CARD_SIX, 6).
-define(CARD_SEVEN, 7).
-define(CARD_EIGHT, 8).
-define(CARD_NINE, 9).
-define(CARD_TEN, 10).
-define(CARD_JACK, 11).
-define(CARD_QUEEN, 12).
-define(CARD_KING, 13).
-define(CARD_ACE, 14).

%% types
-define(TYPE_HOLDEM, 0).
-define(TYPE_7CARD, 1).
-define(TYPE_DRAW, 2).

%% hand ranks
-define(RANK_HIGH, 0).
-define(RANK_LOW, 1).
-define(RANK_LOW8, 2).
-define(RANK_ACE6, 3).
-define(RANK_DEUCE7, 4).
-define(RANK_BADUGI, 5).

%% games
-define(GAME_TEXAS, 0).
-define(GAME_OMAHA, 1).
-define(GAME_OMAHA8, 2).
-define(GAME_STUD, 3).
-define(GAME_STUD8, 4).
-define(GAME_RAZZ, 5).
-define(GAME_LONDON, 6).
-define(GAME_DRAW5, 7).
-define(GAME_SINGLE27, 8).
-define(GAME_TRIPLE27, 9).
-define(GAME_BADUGI, 10).

%% limits
-define(LIMIT_FIXED, 0).
-define(LIMIT_POT, 1).
-define(LIMIT_NO, 2).

poker() ->
	io:format("Hello, world!\n").

