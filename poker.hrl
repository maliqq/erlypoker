-record(card, {kind, suit}).
-record(card_group, {kind = none, suit = none, value}).

-define(SUITS, ["s", "h", "d", "c"]).
-define(KINDS, ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"]).
