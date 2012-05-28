-module(dealer).
-export([run/0, new/0, burn/2, discard/2, hole/2, board/2, deal/3, reshuffle/1]).

%%
-record(dealer, {
    deck, %% live cards
    board = [],
    burned = [], %% for reshuffle deck
    dealt = [] %% dealt cards
  }).

-include("poker.hrl").
-include_lib("eunit/include/eunit.hrl").

%% blank deal
new() ->
  #dealer{deck = card:deck()}.

%%
take(Dealer, Num) when is_record(Dealer, deal) ->
  lists:sublist(Dealer#dealer.deck, 1, Num).

%%
burn(Dealer, Num) when is_record(Dealer, deal) ->
  Burned = take(Dealer, Num),
  {Burned, Dealer#dealer{
    deck = Dealer#dealer.deck -- Burned,
    burned = Dealer#dealer.burned ++ Burned
  }}.

reshuffle(Dealer) ->
  Dealer#dealer{
    deck = card:shuffle(Dealer#dealer.deck ++ Dealer#dealer.burned),
    burned = []
  }.

%%
discard(Dealer, Cards) when is_record(Dealer, deal) ->
  New = take(Dealer, erlang:length(Cards)),
  {New, Dealer#dealer{
    deck = Dealer#dealer.deck -- New,
    burned = Dealer#dealer.burned ++ Cards,
    dealt = Dealer#dealer.dealt -- Cards ++ New
  }}.

%%
hole(Dealer, Num) when is_record(Dealer, deal) ->
  Cards = take(Dealer, Num),
  {Cards, Dealer#dealer{
    deck = Dealer#dealer.deck -- Cards,
    dealt = Dealer#dealer.dealt ++ Cards
  }}.

%%
board(D, Num) when is_record(D, deal) ->
  {_, Dealer} = burn(D, 1), % burn one card
  Cards = take(Dealer, Num),
  {Cards, Dealer#dealer{
    board = Dealer#dealer.board ++ Cards
  }}.

deal(Dealer, Seats, Num) ->
  lists:mapfoldl(fun(Seat, D) ->
    {Cards, D1} = hole(D, Num),
    {Seat#seat{cards = Cards}, D1}
  end, Dealer, Seats).

%%
dealer_test() ->
  Dealer = new(),
  
  Cards = take(Dealer, 2),
  ?assertEqual(2, erlang:length(Cards)),
  
  {Burned, Dealer1} = burn(Dealer, 2),
  ?assertEqual(2, erlang:length(Burned)),
  ?assertEqual([], Dealer1#dealer.burned -- Burned),
  
  Dealer2 = reshuffle(Dealer1),
  ?assertEqual(erlang:length(Dealer1#dealer.deck) + 2, erlang:length(Dealer2#dealer.deck)),
  
  {Hole, Dealer3} = hole(Dealer, 2),
  ?assertEqual([], Dealer3#dealer.dealt -- Hole),
  {Hole2, Dealer4} = discard(Dealer3, Hole),
  ?assertEqual([], Dealer4#dealer.dealt -- Hole2),

  ?assertEqual([], Dealer4#dealer.burned -- Hole),
  {Board, Dealer5} = board(Dealer, 3),
  ?assertEqual([], Dealer5#dealer.board -- Board).

main() ->
  random:seed(erlang:now()),
  io:format("new random deck: "),
  Dealer = new(),
  io:format("~ts~n", [card:to_string(Dealer#dealer.deck)]),
  {Seats, _} = deal(Dealer, [#seat{}, #seat{}, #seat{}, #seat{}, #seat{}, #seat{}, #seat{}, #seat{}], 2),
  lists:foreach(fun(Seat) -> io:format("Hand ~ts~n", [card:to_string(Seat#seat.cards)]) end, Seats),
  io:format("~n").
