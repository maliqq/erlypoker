-module(player).

-record(player, {
    id,
    name,
    state,
    chips,
    cards,
    timer
  }).

%% player states
-define(WAITING, 1). %% waiting next deal
-define(IDLE, 2). %% sit out
-define(AWAY, 3). %% disconnected
-define(ACTIVE, 4). %% active
