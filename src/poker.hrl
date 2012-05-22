%%
-record(card, {kind, suit}).
-record(card_group, {kind = none, suit = none, value}).

-record(bet, {
    amount,
    call = false,
    fold = false,
    all_in = false,
    forced = false
  }).

-record(context, {
    table,
    actor,
    action,
    stage
  }).

-define(SUITS, ["s", "h", "d", "c"]).
-define(KINDS, ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"]).
