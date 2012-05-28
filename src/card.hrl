-define(kinds(), lists:seq(0, 12)).
-define(suits(), lists:seq(0, 3)).
-define(card(Kind, Suit), (Kind bsl 2 + Suit)).
-define(suit(Mask), (Mask & 2#11)).
-define(kind(Mask), (Mask bsr 2)).
-define(cards(), [card(Kind, Suit) || Kind <- kinds(), Suit <- suits()]).

