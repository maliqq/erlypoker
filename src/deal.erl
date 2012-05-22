-module(deal).
-export([test/0, new/0, burn/2, discard/2, hole/2, board/2, vela/1]).

%%
-record(deal, {
    deck,
    board = [],
    burned = [],
    pot,
    hands = []
  }).

-include("poker.hrl").

%% blank deal
new() ->
  #deal{deck = card:deck(), pot = pot:new()}.

%%
draw(Deal, Num) when is_record(Deal, deal) ->
  lists:sublist(Deal#deal.deck, 1, Num).

%%
burn(Deal, Num) when is_record(Deal, deal) ->
  Burned = draw(Deal, Num),
  {Deal#deal{deck = Deal#deal.deck -- Burned, burned = Deal#deal.burned ++ Burned}, Burned}.

%%
discard(Deal, Cards) when is_record(Deal, deal) ->
  New = draw(Deal, erlang:length(Cards)),
  {Deal#deal{deck = Deal#deal.deck -- New, burned = Deal#deal.burned ++ New}, New}.
discard(Deal, Player, Cards) -> ok.

%%
hole(Deal, Num) when is_record(Deal, deal) ->
  Cards = draw(Deal, Num),
  {Deal#deal{deck = Deal#deal.deck -- Cards}, Cards}.

hole(Deal, Player, Num) ->
  ok.

%%
board(Deal, Num) when is_record(Deal, deal) ->
  Cards = draw(Deal, Num),
  {Deal#deal{board = Deal#deal.board ++ Cards}, Cards}.

%%
vela(Deal) when is_record(Deal, deal) ->
  board(Deal, 1).

%%
test() ->
  io:format("random deck: "),
  [io:format("~ts ", [card:to_string(Card)]) || Card <- card:deck()],
  io:format("~n").
