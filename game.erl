%% types
-define(HOLDEM, 0). %% community cards poker
-define(SEVEN_CARD, 1). %% 7 card poker
-define(DRAW, 2). %% draw poker

%% limits
-define(NO_LIMIT, 0). %% NL
-define(POT_LIMIT, 1). %% PL
-define(FIXED_LIMIT, 2). %% FL

%% games
-define(TEXAS, 0).
-define(OMAHA, 1).
-define(OMAHA8, 2).
-define(STUD, 3).
-define(STUD8, 4).
-define(RAZZ, 5).
-define(LONDON, 6).
-define(FIVE_CARD, 7).
-define(SINGLE27, 8).
-define(TRIPLE27, 9).
-define(BADUGI, 10).

-record(game, {id, type, limit}).

%% new_game(?TEXAS, ?HOLDEM, ?NO_LIMIT).
new_game(Game, Type, Limit) -> #game{id = Game, type = Type, limit = Limit}.
