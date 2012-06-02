%%
-define(ACE, 12).
-define(ACE_LOW, 0).

%%
-define(kinds(), lists:seq(0, ?ACE)).
-define(suits(), lists:seq(0, 3)).
-define(cards(), [?card(Kind, Suit) || Kind <- ?kinds(), Suit <- ?suits()]).

%%
-define(kind(Mask), (Mask bsr 2)).
-define(suit(Mask), (Mask band 2#11)).
-define(card(Kind, Suit), (Kind bsl 2 + Suit)).

%%
-define(kind_chars(), "23456789TJQKA").
-define(kind_names(), [
    "deuce", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "jack", "queen", "king", "ace"
  ]).

%%
-define(suit_chars(), "shdc").
-define(suit_utf8(), [
    <<"♠"/utf8>>,
    <<"♥"/utf8>>,
    <<"♦"/utf8>>,
    <<"♣"/utf8>>
  ]).
-define(suit_names(), ["spade", "heart", "diamond", "club"]).
