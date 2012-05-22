-module(game).
-record(game, {id, type, limit}).
-export([new/2, mix/1, stages/1, globals/0, defaults/1, options/1]).

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

%% new_game(?TEXAS, ?NO_LIMIT).
new(Type, Limit) -> #game{type = Type, limit = Limit}.

%% mixed game options
mix(Type) when Type == ?HORSE ->
  {[?HOLDEM, ?OMAHA8, ?RAZZ, ?STUD, ?STUD8], ?FIXED_LIMIT};
mix(Type) when Type == ?HOSE ->
  {[?HOLDEM, ?OMAHA8, ?STUD, ?STUD8], ?FIXED_LIMIT};
mix(Type) when Type == ?MIX8 -> %% 8-game
  [
    {[?TRIPLE27, ?HOLDEM, ?OMAHA8, ?RAZZ, ?STUD, ?STUD8], ?FIXED_LIMIT},
    {?HOLDEM, ?NO_LIMIT},
    {?OMAHA, ?POT_LIMIT}
  ];
mix(Type) when Type == ?MIX9 ->
  [
    {[?BADUGI, ?TRIPLE27, ?HOLDEM, ?OMAHA8, ?RAZZ, ?STUD, ?STUD8], ?FIXED_LIMIT},
    {?HOLDEM, ?NO_LIMIT},
    {?OMAHA, ?POT_LIMIT}
  ];
mix(Type) when Type == ?MIXED_HOLDEM ->
  [{?HOLDEM, ?FIXED_LIMIT}, {?HOLDEM, ?NO_LIMIT}, {?OMAHA8, ?FIXED_LIMIT}, {?OMAHA, ?POT_LIMIT}];
mix(Type) when Type == ?MIXED_STUD ->
  {[?STUD, ?STUD8, ?RAZZ], ?FIXED_LIMIT};
mix(Type) when Type == ?MIX7 ->
  {[?STUD, ?STUD8, ?RAZZ, ?LONDON], ?FIXED_LIMIT};
mix(Type) when Type == ?MIXED_DRAW ->
  {[?BADUGI, ?CARD5, ?SINGLE27, ?TRIPLE27], ?FIXED_LIMIT}.

%% global options for all game types
globals() ->
  [
    {cap, 4},
    {small_blind, 0.5},
    {big_bet, 2},
    {ante, 0.125},
    {bring_in, 0.25}
  ].

%% games stages for specified game type
stages(Type) when Type band ?HOLDEM == ?HOLDEM ->
  [
    antes,
    blinds,
    {street, "pre", [{deal, hole}, bets]},
    {street, "flop", [{deal, 3, board}, bets]},
    {street, "turn", [{deal, 1, board}, bets]},
    {street, "river", [{deal, 1, board}, bets]},
    showdown
  ];
stages(Type) when Type band ?CARD7 == ?CARD7 ->
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
stages(Type) when Type band ?DRAW == ?DRAW ->
  [
    antes,
    blinds,
    {street, "pre", [deal, bets]},
    {streets, [discards, bets]},
    showdown
  ].

%% game default options for specific game type
defaults(Type) when Type band ?CARD7 == ?CARD7 ->
  [
    {vela, true}, %% 1 card board
    {hole_cards, 7},
    {limit, ?FIXED_LIMIT},
    {ante, true},
    {bring_in, true},
    {big_bets, true},
    {table_max, 8}
  ];
defaults(Type) when Type band ?HOLDEM == ?HOLDEM ->
  [
    {board, true},
    {table_max, 10}
  ];
defaults(Type) when Type band ?DRAW == ?DRAW ->
  [
    {discards, true},
    {limit, ?FIXED_LIMIT},
    {table_max, 6},
    {reshuffle, true}
  ].

%% game options for specific game type
options(Type) when Type == ?TEXAS ->
  [
    {ranking, fun hand:high_card/1},
    {hole_cards, 2}
  ];
options(Type) when Type == ?OMAHA ->
  [
    {ranking, fun hand:high_card/1},
    {hole_cards, 4},
    {limit, ?POT_LIMIT}
  ];
options(Type) when Type == ?OMAHA8 ->
  [
    {ranking, fun hand:high_card/1},
    {ranking, fun hand:ace5_low8/1},
    {hole_cards, 4},
    {limit, ?POT_LIMIT}
  ];
options(Type) when Type == ?STUD ->
  [
    {ranking, fun hand:high_card/1}
  ];
options(Type) when Type == ?STUD8 ->
  [
    {ranking, fun hand:high_card/1},
    {ranking, fun hand:ace5_low8/1}
  ];
options(Type) when Type == ?RAZZ ->
  [
    {ranking, fun hand:ace5_low/1}
  ];
options(Type) when Type == ?LONDON ->
  [
    {ranking, fun hand:ace6_low/1}
  ];
options(Type) when Type == ?CARD5 ->
  [
    {ranking, fun hand:high_card/1},
    {hole_cards, 5}
  ];
options(Type) when Type == ?SINGLE27 ->
  [
    {ranking, fun hand:deuce7_low/1},
    {hole_cards, 5},
    {streets, 1}
  ];
options(Type) when Type == ?TRIPLE27 ->
  [
    {ranking, fun hand:deuce7_low/1},
    {hole_cards, 5},
    {streets, 3}
  ];
options(Type) when Type == ?BADUGI ->
  [
    {ranking, fun hand:badugi_hand/1},
    {hole_cards, 4},
    {streets, 3},
    {table_max, 8}
  ].
