-module(deal).
-export([test/0, new/0, burn/2, discard/2, hole/2, board/2, vela/1, bet/3]).

-record(deal, {
    deck,
    board = [],
    burned = [],
    pot,
    hands = []
  }).

-include("poker.hrl").

%%
new() ->
  #deal{deck = card:deck(), pot = pot:new()}.

%%
grant(Deal, Num) when is_record(Deal, deal) ->
  lists:sublits(Deal#deal.deck, 1, Num).

%%
burn(Deal, Num) when is_record(Deal, deal) ->
  Burned = grant(Deal, Num),
  {Deal#deal{deck = Deal#deal.deck -- Burned, burned = Deal#deal.burned ++ Burned}, Burned}.

%%
discard(Deal, Cards) when is_record(Deal, deal) ->
  New = grant(Deal, erlang:length(Cards)),
  {Deal#deal{deck = Deal#deal.deck -- New, burned = Deal#deal.burned ++ New}, New}.
discard(Deal, Player, Cards) -> ok.

%%
hole(Deal, Num) when is_record(Deal, deal) ->
  Cards = grant(Deal, Num),
  {Deal#deal{deck = Deal#deal.deck -- Cards}, Cards}.
hole(Deal, Player, Num) -> ok.

%%
board(Deal, Num) when is_record(Deal, deal) ->
  Cards = grant(Deal, Num),
  {Deal#deal{board = Deal#deal.board ++ Cards}, Cards}.

%%
vela(Deal) when is_record(Deal, deal) ->
  board(Deal, 1).

%%
bet(Deal, Player, Bet) when is_record(Deal, deal), is_record(Bet, bet) ->
  if
    Bet#bet.amount > 0 -> %% RAISE, CALL
      Deal#deal{pot = pot:append(Deal#deal.pot, Player, Bet#bet.amount)};
    Bet#bet.fold -> %% FOLD
      Deal#deal{pot = pot:erase(Deal#deal.pot, Player)}
  end.

%%
test() ->
  io:format("random deck: "),
  [io:format("~ts ", [card:to_string(Card)]) || Card <- card:deck()],
  io:format("~n").
