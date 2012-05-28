-define(kinds(), lists:seq(0, 12)).
-define(suits(), lists:seq(0, 3)).
-define(cards(), [card(Kind, Suit) || Kind <- kinds(), Suit <- suits()]).

-define(kind(Mask), (Mask bsr 2)).
-define(suit(Mask), (Mask & 2#11)).
-define(card(Kind, Suit), (Kind bsl 2 + Suit)).

-define(card_chars(), [$2, $3, $4, $5, $6, $7, $8, $9, $T, $J, $Q, $K, $A]).
-define(card_names(), ["deuce", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "jack", "queen", "king", "ace"]).

-define(suit_chars(), ["s", "h", "d", "c"]).
-define(suit_chars_utf8(), [
    <<"♠"/utf8>>,
    <<"♥"/utf8>>,
    <<"♦"/utf8>>,
    <<"♣"/utf8>>
  ]).
-define(suit_names(), ["spade", "heart", "diamond", "club"]).
