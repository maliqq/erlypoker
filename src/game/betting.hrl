antes(Deal) ->
  Players = player:get_active(Deal#deal.players),
  post_antes(Deal, Players).

post_antes(Deal, []) ->
  Deal;

post_antes(Deal, [Player | Left]) ->
  Ante = bet:sizing(Deal, ?ANTE),
  post_antes(forced_bet(Deal, Player, Ante), Left).

blinds(Deal) ->
  Deal1 = post_blind(Deal, ?SMALL_BLIND, ?SB),
  post_blind(Deal1, ?BIG_BLIND, ?BB).

post_blind(Deal, Type, Position) ->
  Amount = bet:sizing(Deal, Type),
  Player = position:get(Deal, Position),
  forced_bet(Deal, Player, Amount).

forced_bet(Deal, Player, Amount) ->
  Deal.

big_bets(Deal) ->
  ok.
