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
-define(NO_LIMIT, 1). %% NL
-define(POT_LIMIT, 2). %% PL
-define(FIXED_LIMIT, 3). %% FL

%% holdem
-define(TEXAS, 1).
-define(OMAHA, 2).
-define(OMAHA8, 3).
%% seven card
-define(STUD, 4).
-define(STUD8, 5).
-define(RAZZ, 6).
-define(LONDON, 7).
%% draw
-define(CARD5, 8).
-define(SINGLE27, 9).
-define(TRIPLE27, 10).
-define(BADUGI, 11).
%% mixes
-define(HORSE, 12).
-define(HOSE, 13).
-define(MIX7, 14).
-define(MIX8, 15).
-define(MIX9, 16).
-define(MIXED_HOLDEM, 17).
-define(MIXED_STUD, 18).
-define(MIXED_DRAW, 19).
%% type groups
-define(HOLDEM, ?TEXAS bor ?OMAHA bor ?OMAHA8). %% holdem poker
-define(CARD7, ?STUD bor ?STUD8 bor ?RAZZ bor ?LONDON). %% 7 card poker
-define(DRAW, ?CARD5 bor ?SINGLE27 bor ?TRIPLE27 bor ?BADUGI). %% draw poker
-define(MIX, ?HORSE bor ?HOSE bor ?MIX7 bor ?MIX8 bor ?MIXED_HOLDEM bor ?MIXED_STUD bor ?MIXED_DRAW). %% mixed poker
