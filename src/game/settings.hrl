%% mixed game options
mix(Type) when Type == ?HORSE ->
  {fixed_limit, [?HOLDEM, ?OMAHA8, ?RAZZ, ?STUD, ?STUD8]};

mix(Type) when Type == ?HOSE ->
  {fixed_limit, [?HOLDEM, ?OMAHA8, ?STUD, ?STUD8]};

mix(Type) when Type == ?EIGHT_GAME -> %% 8-game
  [
    {fixed_limit, [?TRIPLE27, ?HOLDEM, ?OMAHA8, ?RAZZ, ?STUD, ?STUD8]},
    {no_limit, ?HOLDEM},
    {pot_limit, ?OMAHA}
  ];

mix(Type) when Type == ?NINE_GAME ->
  [
    {fixed_limit, [?BADUGI, ?TRIPLE27, ?HOLDEM, ?OMAHA8, ?RAZZ, ?STUD, ?STUD8]},
    {no_limit, ?HOLDEM},
    {pot_limit, ?OMAHA}
  ];

mix(Type) when Type == ?MIXED_HOLDEM ->
  [
    {fixed_limit, ?HOLDEM},
    {no_limit, ?HOLDEM},
    {pot_limit, ?OMAHA8},
    {pot_limit, ?OMAHA}
  ];

mix(Type) when Type == ?HOLDEM_OMAHA ->
  [
    {no_limit, ?HOLDEM},
    {pot_limit, ?OMAHA}
  ];

mix(Type) when Type == ?MIXED_STUD ->
  {fixed_limit, [?STUD, ?STUD8, ?RAZZ]};

mix(Type) when Type == ?MIXED_SEVEN_CARD ->
  {fixed_limit, [?STUD, ?STUD8, ?RAZZ, ?LONDON]};

mix(Type) when Type == ?MIXED_DRAW ->
  {fixed_limit, [?BADUGI, ?FIVE_CARD, ?SINGLE27, ?TRIPLE27]}.

mix(Type) when Type == ?MIXED_FIVE_CARD ->
  {fixed_limit, [?FIVE_CARD, ?SINGLE27, ?TRIPLE27]}.

%% game default options for specific game type
defaults(Type) when Type band ?SEVEN_CARD == ?SEVEN_CARD ->
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
options(Type) when Type == ?FIVE_CARD ->
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
