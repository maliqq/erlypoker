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

%% games stages for specified game type
stages(Type) when Type band ?HOLDEM == ?HOLDEM ->
  [
    antes,
    blinds,
    {street,  "preflop",      [{deal, hole}, bets]},
    {street,  "flop",         [{deal, board, 3}, bets]},
    {street,  "turn",         [{deal, board, 1}, bets]},
    {street,  "river",        [{deal, board, 1}, bets]},
    showdown
  ];
stages(Type) when Type band ?CARD7 == ?CARD7 ->
  [
    antes,
    {street,  "third",        [{deal, hole, 2}, {deal, door, 1}, bring_in, bets]},
    {street,  "fourth",       [{deal, door, 1}, bets]},
    big_bets,
    {street,  "fifth",        [{deal, door, 1}, bets]},
    {street,  "sixth",        [{deal, door, 1}, bets]},
    {street,  "seventh",      [{deal, hole, 1}, bets]},
    showdown
  ];
stages(Type) when Type band ?SINGLE_DRAW == ?SINGLE_DRAW ->
  [
    antes,
    blinds,
    {street,  "predraw",      [{deal, hole}, bets]},
    {street,  "draw",         [discards, bets]},
    showdown
  ];
stages(Type) when Type band ?TRIPLE_DRAW == ?TRIPLE_DRAW ->
  [
    antes,
    blinds,
    {street,  "predraw",      [{deal, hole}, bets]},
    {street,  "first_draw",   [discards, bets]},
    {street,  "second_draw",  [discards, bets]},
    {street,  "third_draw",   [discards, bets]},
    showdown
  ].

%%
globals() ->
  [
    %% bet sizing
    {cap,           4},
    {small_blind,   0.5},
    {big_bet,       2},
    {ante,          0.125},
    {bring_in,      0.25}
  ].

%% game default options for specific game type
defaults(Type) when Type band ?CARD7 == ?CARD7 ->
  [
    {vela,          true}, %% 1 card board
    {hole_cards,    7},
    {limit,         fixed_limit},
    {ante,          true},
    {bring_in,      true},
    {big_bets,      true},
    {table_max,     8}
  ];
defaults(Type) when Type band ?HOLDEM == ?HOLDEM ->
  [
    {board,         true},
    {table_max,     10}
  ];
defaults(Type) when Type band ?DRAW == ?DRAW ->
  [
    {discards,      true},
    {limit,         fixed_limit},
    {table_max,     6},
    {reshuffle,     true}
  ].

%% game options for specific game type
options(Type) when Type == ?TEXAS ->
  [
    {ranking,       high},
    {hole_cards,    2},
    {limit,         no_limit}
  ];
options(Type) when Type == ?OMAHA ->
  [
    {ranking,       high},
    {hole_cards,    4},
    {limit,         pot_limit}
  ];
options(Type) when Type == ?OMAHA8 ->
  [
    {ranking,       [high, ace_five_better]},
    {hole_cards,    4},
    {limit,         pot_limit}
  ];
options(Type) when Type == ?STUD ->
  [
    {ranking,       high}
  ];
options(Type) when Type == ?STUD8 ->
  [
    {ranking,       [high, ace_five_better]}
  ];
options(Type) when Type == ?RAZZ ->
  [
    {ranking,       ace_five}
  ];
options(Type) when Type == ?LONDON ->
  [
    {ranking,       ace_six}
  ];
options(Type) when Type == ?CARD5 ->
  [
    {ranking,       high},
    {hole_cards,    5}
  ];
options(Type) when Type == ?SINGLE27 ->
  [
    {ranking,       deuce_seven},
    {hole_cards,    5}
  ];
options(Type) when Type == ?TRIPLE27 ->
  [
    {ranking,       deuce_seven},
    {hole_cards,    5}
  ];
options(Type) when Type == ?BADUGI ->
  [
    {ranking,       badugi},
    {hole_cards,    4},
    {streets,       3},
    {table_max,     8}
  ].
