%%

-record(forced_bet, {
    ante = 0,
    bring_in = 0,
    small_blind = 0,
    big_blind = 0,
    guest_blind = 0,
    straddle = 0
  }).

-record(bet, {
    raise = false,
    call = false,
    fold = false,
    all_in = false
  }).

-record(balance, { %% tournament balance
    buy_in = 0,
    rebuy_limit = 0,
    addon_limit = 0,
    bounty_players = []
  }).
-record(player, {
    id,
    name,
    state,
    balance
  }).

%% player states
-define(WAIT, 1). %% waiting next deal or BB
-define(IDLE, 2). %% sit out
-define(AWAY, 3). %% disconnected
-define(FOLD, 4). %% folded hand
-define(PLAY, 5). %% currently in deal
-define(ACTIVE, ?FOLD bor ?PLAY). %% active

-define(EMPTY, 1).
-define(RESERVED, 2).
-define(BUSY, 3).

-record(chips, {
    play,
    put,
    cap
  }).

-record(seat, {
    state = ?EMPTY,
    player,
    chips,
    cards,
    timer,
    action
  }).

-record(table, {
    id,
    game, type, state,
    seats, players, waiting, watching,
    max = 9,
    current %% current deal
  }).

%% table types
-define(NORMAL_TABLE, 1). %% cash game
-define(RANDOM_TABLE, 2). %% random game
-define(BATTLE_TABLE, 3). %% tournament game
