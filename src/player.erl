-module(player).

-record(player, {
    id,
    name
  }).

%% player states
-define(WAIT, 1). %% waiting next deal or BB
-define(IDLE, 2). %% sit out
-define(AWAY, 3). %% disconnected
-define(FOLD, 4). %% folded hand
-define(PLAY, 5). %% currently in deal

-define(ACTIVE, ?FOLD bor ?PLAY). %% active
