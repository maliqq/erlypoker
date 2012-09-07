-module(poker).

-include_lib("eunit/include/eunit.hrl").

-define(suits(), lists:seq(0, 3)).
-define(kinds(), lists:seq(0, 12)).
-define(card(Kind, Suit), (Kind bsl 2 + Suit)).
-define(kind(Card), (Card bsr 2)).
-define(suit(Card), (Card band 2#11)).
-define(cards(), [?card(Kind, Suit) || Kind <- ?kinds(), Suit <- ?suits()]).

suits_test() ->
  ?assertEqual([0, 1, 2, 3], ?suits()).

kinds_test() ->
  ?assertEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], ?kinds()).

card_test() ->
  ?assertEqual(0, ?card(0, 0)),
  ?assertEqual(51, ?card(12, 3)).

kind_test() ->
  ?assertEqual(3, ?kind(?card(3, 0))).

suit_test() ->
  ?assertEqual(2, ?suit(?card(3, 2))).

cards_test() ->
  ?assertEqual(52, erlang:length(?cards())).



