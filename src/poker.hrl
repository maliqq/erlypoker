%%
-record(card, {kind, suit}).
-record(card_group, {kind = none, suit = none, value}).

-record(bet, {
    amount,
    call = false,
    fold = false,
    all_in = false,
    forced = false %% blinds and ante
  }).

-record(player, {
    id,
    name
  }).

-record(turn, {
    seat,
    bet
  }).

-define(EMPTY, 1).
-define(RESERVED, 2).
-define(BUSY, 3).

-record(seat, {
    state = ?EMPTY,
    player,
    chips,
    cards,
    timer,
    action
  }).

-define(SUITS, ["s", "h", "d", "c"]).
-define(KINDS, ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"]).
