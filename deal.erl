-module(deal).
-export([test/0, new/0, burn/2, discard/2, hole/2, board/2, vela/1]).

-record(deal, {
    deck,
    board = [],
    burned = [],
    pot,
    hands = []
  }).

%%

new() ->
  #deal{deck = card:deck(), pot = pot:new()}.

grant(Deal, Num) ->
  lists:sublits(Deal#deal.deck, 1, Num).

burn(Deal, Num) ->
  Burned = grant(Deal, Num),
  {Deal#deal{deck = Deal#deal.deck -- Burned, burned = Deal#deal.burned ++ Burned}, Burned}.

discard(Deal, Cards) ->
  New = grant(Deal, erlang:length(Cards)),
  {Deal#deal{deck = Deal#deal.deck -- New, burned = Deal#deal.burned ++ New}, New}.

hole(Deal, Num) ->
  Cards = grant(Deal, Num),
  {Deal#deal{deck = Deal#deal.deck -- Cards}, Cards}.

board(Deal, Num) ->
  Cards = grant(Deal, Num),
  {Deal#deal{board = Deal#deal.board ++ Cards}, Cards}.

vela(Deal) ->
  board(Deal, 1).

%%
test() ->
  io:format("random deck: "),
  [io:format("~ts ", [card:to_string(Card)]) || Card <- card:deck()],
  io:format("~n").
