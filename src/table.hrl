-define(EMPTY, 1).
-define(RESERVED, 2).
-define(BUSY, 3).

%% table types
-define(NORMAL_TABLE, 1). %% cash game
-define(RANDOM_TABLE, 2). %% random game
-define(BATTLE_TABLE, 3). %% tournament game

%%
-record(balance, { %% tournament balance
    buy_in = 0,
    rebuy_limit = 0,
    addon_limit = 0,
    bounty_players = []
  }).

%%
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
-define(FOLDED, 4). %% folded hand
-define(PLAY, 5). %% currently in deal
-define(ACTIVE, ?FOLD bor ?PLAY). %% active

%%
-record(chips, {
    play,
    put,
    cap
  }).


-record(seat, {
    state = ?EMPTY,
    index,
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
    max = 9, button = 1,
    current %% current deal
  }).

-record(prize, {
    amount,
    rebuys,
    addons,
    places_paid,
    payout_structure,
    results
  }).

-record(battle, {
    game,
    type,
    state,    %% registering, canceled, paused, running
    buy_in,   %% {100.0, 10000}
    rake,     %% 9.0
    rebuy,    %% {100.0, 10000}
    rebuy_limit,
    addon,    %% {100.0, 50000}
    bounty,   %% 0.0
    
    prize_pool,

    max,

    start,
    finish,
    level_period, %% 15 min
    rebuy_levels, %% 4 levels
    break_period, %% 5 min
    late_reg_period, %% 8 levels

    levels,
    tables,
    players
  }).

-include("table/position.hrl").
-include("table/seat.hrl").
-include("table/player.hrl").

