%% hand ranking: from high card to straight flush
-define(HIGH_CARD, 0).
-define(ONE_PAIR, 1).
-define(TWO_PAIR, 2).
-define(THREE_KIND, 3).
-define(STRAIGHT, 4).
-define(FLUSH, 5).
-define(FULL_HOUSE, 6).
-define(FOUR_KIND, 7).
-define(STRAIGHT_FLUSH, 8).

-define(BADUGI_ONE, 9).
-define(BADUGI_TWO, 10).
-define(BADUGI_THREE, 11).
-define(BADUGI_FOUR, 12). %% complete badugi card

-define(LOW_ACE5, 13).
-define(LOW_ACE5_BETTER, 14).
-define(LOW_DEUCE7, 15).
-define(LOW_ACE6, 16).

-record(hand, {
    rank,
    cards, %% all cards
    rows, kinds, suits,
    high = none, value = [], kickers = []
  }).
