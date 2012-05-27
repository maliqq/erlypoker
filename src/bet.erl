%%
-module(bet).

%% forced bets
-define(ANTE, 1).
-define(BRING_IN, 2).
-define(SMALL_BLIND, 3).
-define(BIG_BLIND, 4).
-define(GUEST_BLIND, 5).

-define(FORCED_BET, ?ANTE bor ?BRING_IN bor ?SMALL_BLIND bor ?BIG_BLIND bor ?GUEST_BLIND).

%% action bets
-define(FOLD, 6).
-define(CHECK, 7).
-define(CALL, 8).
-define(RAISE, 9).
-define(PUSH, 10).
