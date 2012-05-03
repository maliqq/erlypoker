-module(game).
-record(game, {id, type, limit}).

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

%% new_game(?TEXAS, ?HOLDEM, ?NO_LIMIT).
new(Game, Type, Limit) -> #game{id = Game, type = Type, limit = Limit}.

globals() ->
  [
    {cap, 4},
    {small_blind, 0.5},
    {big_bet, 2},
    {ante, 0.125},
    {bring_in, 0.25}
  ].
stages(Game) when Game#game.type == ?HOLDEM ->
  [
    antes,
    blinds,
    {street, "pre", [{deal, hole}, bets]},
    {street, "flop", [{deal, 3, board}, bets]},
    {street, "turn", [{deal, 1, board}, bets]},
    {street, "river", [{deal, 1, board}, bets]},
    showdown
  ];
stages(Game) when Game#game.type == ?SEVEN_CARD ->
  [
    antes,
    {street, "3rd", [{deal, 2, hole}, {deal, 1, door}, bring_in, bets]},
    {street, "4th", [{deal, 1, door}, bets]},
    big_bet,
    {street, "5th", [{deal, 1, door}, bets]},
    {street, "6th", [{deal, 1, door}, bets]},
    {street, "7th", [{deal, 1, hole}, bets]},
    showdown
  ];
stages(Game) when Game#game.type == ?DRAW ->
  [
    antes,
    blinds,
    {street, "pre", [deal, bets]},
    {streets, [discards, bets]},
    showdown
  ].
defaults(Game) when Game#game.type == ?SEVEN_CARD ->
  [
    {vela, true}, %% 1 card board
    {hole_cards, 7},
    {limit, ?FIXED_LIMIT},
    {ante, true},
    {bring_in, true},
    {big_bets, true},
    {table_max, 8}
  ];
defaults(Game) when Game#game.type == ?HOLDEM ->
  [
    {board, true},
    {table_max, 10}
  ];
defaults(Game) when Game#game.type == ?DRAW ->
  [
    {discards, true},
    {limit, ?FIXED_LIMIT},
    {table_max, 6},
    {reshuffle, true}
  ].
options(Game) when Game#game.id == ?TEXAS ->
  [
    {ranking, fun hand:high_card/1},
    {hole_cards, 2}
  ];
options(Game) when Game#game.id == ?OMAHA ->
  [
    {ranking, fun hand:high_card/1},
    {hole_cards, 4},
    {limit, ?POT_LIMIT}
  ];
options(Game) when Game#game.id == ?OMAHA8 ->
  [
    {ranking, fun hand:high_card/1},
    {ranking, fun hand:ace5_low8/1},
    {hole_cards, 4},
    {limit, ?POT_LIMIT}
  ];
options(Game) when Game#game.id == ?STUD ->
  [
    {ranking, fun hand:high_card/1}
  ];
options(Game) when Game#game.id == ?STUD8 ->
  [
    {ranking, fun hand:high_card/1},
    {ranking, fun hand:ace5_low8/1}
  ];
options(Game) when Game#game.id == ?RAZZ ->
  [
    {ranking, fun hand:ace5_low/1}
  ];
options(Game) when Game#game.id == ?LONDON ->
  [
    {ranking, fun hand:ace6_low/1}
  ];
options(Game) when Game#game.id == ?FIVE_CARD ->
  [
    {ranking, fun hand:high_card/1},
    {hole_cards, 5}
  ];
options(Game) when Game#game.id == ?SINGLE27 ->
  [
    {ranking, fun hand:deuce7_low/1},
    {hole_cards, 5},
    {streets, 1}
  ];
options(Game) when Game#game.id == ?TRIPLE27 ->
  [
    {ranking, fun hand:deuce7_low/1},
    {hole_cards, 5},
    {streets, 3}
  ];
options(Game) when Game#game.id == ?BADUGI ->
  [
    {ranking, fun hand:badugi_hand/1},
    {hole_cards, 4},
    {streets, 3},
    {table_max, 8}
  ].
