-record(game, {type, limit}).

-record(limit, {
    type,
    ante = 0,
    bring_in = 0,
    small_blind = 0,
    big_blind = 0,
    big_bet = 0
  }).

%% limits
-define(NO_LIMIT, 0). %% NL
-define(POT_LIMIT, 1). %% PL
-define(FIXED_LIMIT, 2). %% FL

%% holdem
-define(TEXAS, 1).
-define(OMAHA, 2).
-define(OMAHA8, 3).
%% seven card
-define(STUD, 4).
-define(STUD8, 5).
-define(RAZZ, 6).
-define(LONDON, 7).
%% 5 card draw
-define(FIVE_CARD, 8).
-define(SINGLE27, 9).
-define(TRIPLE27, 10).
%% 4 card draw
-define(BADUGI, 11).
%% mixes
-define(HORSE, 12).
-define(HOSE, 13).
-define(EIGHT_GAME, 14).
-define(NINE_GAME, 15).

-define(MIXED_HOLDEM, 16).
-define(MIXED_STUD, 17).
-define(MIXED_DRAW, 18).

-define(MIXED_SEVEN_CARD, 19).
-define(MIXED_FIVE_CARD, 20).

-define(HOLDEM_OMAHA, 21).

%% type groups
-define(HOLDEM, ?TEXAS bor ?OMAHA bor ?OMAHA8). %% holdem poker
-define(SEVEN_CARD, ?STUD bor ?STUD8 bor ?RAZZ bor ?LONDON). %% 7 card poker
-define(DRAW, ?FIVE_CARD bor ?SINGLE27 bor ?TRIPLE27 bor ?BADUGI). %% draw poker
-define(SINGLE_DRAW, ?FIVE_CARD bor ?SINGLE27).
-define(TRIPLE_DRAW, ?TRIPLE27 bor ?BADUGI).
-define(MIX, %% mixed poker
    ?HORSE bor ?HOSE bor ?EIGHT_GAME bor ?NINE_GAME bor
    ?MIXED_HOLDEM bor ?MIXED_STUD bor ?MIXED_DRAW bor
    ?MIXED_SEVEN_CARD bor ?MIXED_FIVE_CARD bor ?HOLDEM_OMAHA).

-define(limits(), ["no_limit", "pot_limit", "fixed_limit"]).
-define(games(), [
    "texas", "omaha", "omaha8",
    "stud", "stud8", "razz", "london",
    "card5", "single27", "triple27", "badugi"
  ]).

-include("poker.hrl").
-include("bet.hrl").
-include("game/settings.hrl").
-include("game/stages.hrl").
-include("game/betting.hrl").
