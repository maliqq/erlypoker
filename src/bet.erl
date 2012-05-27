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
-define(FOLD, fold).
-define(CHECK, check).
-define(CALL, call).
-define(RAISE, raise).
-define(ALL_IN, all_in).

-define(DISCARD_CARDS, discard_cards).
-define(STAND_PAT, stand_pat).
-define(MUCK_CARDS, muck_cards).
-define(SHOW_CARDS, show_cards).
