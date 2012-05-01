-record(deal, {
    deck,
    pot,
    hands
  }).

reshuffle_deck(_) -> ok.

deal_discarding_cards() -> ok.

deal_hole_cards() -> ok.

deal_community_cards() -> ok.

showdown_cards() -> ok.

post_blinds() -> ok.

post_antes() -> ok.

new_deal(_, _) -> ok.
