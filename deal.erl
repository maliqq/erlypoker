-record(deal, {
    deck,
    pot,
    hands
  }).

%%
new_deck() ->
  card:shuffle(card:all()).

reshuffle_deck(_) -> ok.

deal_discarding_cards() -> ok.

deal_hole_cards() -> ok.

deal_community_cards() -> ok.

showdown_cards() -> ok.

post_blinds() -> ok.

post_antes() -> ok.

new_deal(_, _) -> ok.

%%
test_deck() ->
  io:format("random deck: "),
  [io:format("~ts ", [card:to_string(Card)]) || Card <- new_deck()],
  io:format("~n").
