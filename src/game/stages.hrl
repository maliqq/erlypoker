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
stages(Type) when Type band ?SEVEN_CARD == ?SEVEN_CARD ->
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
