-module(deal).
-export([test/0, new/0, burn/2, discard/2, hole/2, board/2]).

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
take(Deal, Num) when is_record(Deal, deal) ->
  lists:sublist(Deal#deal.deck, 1, Num).

%%
burn(Deal, Num) when is_record(Deal, deal) ->
  Burned = take(Deal, Num),
  {Burned, Deal#deal{deck = Deal#deal.deck -- Burned, burned = Deal#deal.burned ++ Burned}}.

%%
discard(Deal, Cards) when is_record(Deal, deal) ->
  New = take(Deal, erlang:length(Cards)),
  {New, Deal#deal{deck = Deal#deal.deck -- New, burned = Deal#deal.burned ++ New}}.

%%
hole(Deal, Num) when is_record(Deal, deal) ->
  Cards = take(Deal, Num),
  {Cards, Deal#deal{deck = Deal#deal.deck -- Cards}}.

%%
board(Deal, Num) when is_record(Deal, deal) ->
  Cards = take(Deal, Num),
  {Cards, Deal#deal{board = Deal#deal.board ++ Cards}}.

deal(Deal, Seats, Num) ->
  lists:mapfoldl(fun(Seat, D) ->
    {Cards, D1} = hole(D, Num),
    {Seat#seat{cards = Cards}, D1}
  end, Deal, Seats).

%%
test() ->
  random:seed(erlang:now()),

  io:format("new random deck: "),
  Deal = new(),
  io:format("~ts~n", [card:to_string(Deal#deal.deck)]),
  {Seats, _} = deal(Deal, [#seat{}, #seat{}, #seat{}, #seat{}, #seat{}, #seat{}, #seat{}, #seat{}], 2),
  lists:foreach(fun(Seat) -> io:format("Hand ~ts~n", [card:to_string(Seat#seat.cards)]) end, Seats),
  io:format("~n").
