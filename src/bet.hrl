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

%% forced bets
-define(ANTE, 1).
-define(SMALL_BLIND, 2).
-define(BIG_BLIND, 3).
-define(GUEST_BLIND, 4).
-define(BRING_IN, 5).
-define(STRADDLE, 6).

%% bet actions
-define(FOLD, 7).
-define(CHECK, 8).
-define(RAISE, 9).
-define(CALL, 10).

%% draw actions
-define(DISCARD, 11).
-define(STAND_PAT, 12).

%% card actions
-define(MUCK, 13).

%% buy-in actions
-define(REBUY, 14).
-define(DOUBLE_REBUY, 15).
-define(ADDON, 18).

%%
-define(sizing(), [
    %% bet sizing
    {cap,           4},
    {small_blind,   0.5},
    {big_bet,       2},
    {ante,          0.125},
    {bring_in,      0.25}
  ]).
