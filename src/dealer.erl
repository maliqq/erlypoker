-module(dealer).
-export([new/0, burn_cards/2, discard_cards/2, hole_cards/2, board_cards/2, reshuffle_cards/1]).

%%
-record(dealer, {
    deck, %% live cards
    board = [],
    burned = [], %% for reshuffle deck
    dealt = [] %% dealt cards
  }).

-include_lib("eunit/include/eunit.hrl").

%% blank deal
new() ->
  #dealer{deck = card:deck()}.

%%
get_cards(Dealer, Num) ->
  lists:sublist(Dealer#dealer.deck, 1, Num).

%%
burn_cards(Dealer, Num) ->
  Burned = get_cards(Dealer, Num),
  {Burned, Dealer#dealer{
    deck = Dealer#dealer.deck -- Burned,
    burned = Dealer#dealer.burned ++ Burned
  }}.

%%
reshuffle_cards(Dealer) ->
  Dealer#dealer{
    deck = card:shuffle(Dealer#dealer.deck ++ Dealer#dealer.burned),
    burned = []
  }.

%%
discard_cards(Dealer, Cards) ->
  New = get_cards(Dealer, erlang:length(Cards)),
  {New, Dealer#dealer{
    deck = Dealer#dealer.deck -- New,
    burned = Dealer#dealer.burned ++ Cards,
    dealt = Dealer#dealer.dealt -- Cards ++ New
  }}.

%%
hole_cards(Dealer, Num) ->
  Cards = get_cards(Dealer, Num),
  {Cards, Dealer#dealer{
    deck = Dealer#dealer.deck -- Cards,
    dealt = Dealer#dealer.dealt ++ Cards
  }}.

%%
board_cards(D, Num) ->
  {_, Dealer} = burn_cards(D, 1), % burn one card
  Cards = get_cards(Dealer, Num),
  {Cards, Dealer#dealer{
    board = Dealer#dealer.board ++ Cards
  }}.

%%
dealer_test() ->
  Dealer = new(),
  
  Cards = get_cards(Dealer, 2),
  ?assertEqual(2, erlang:length(Cards)),
  
  {Burned, Dealer1} = burn_cards(Dealer, 2),
  ?assertEqual(2, erlang:length(Burned)),
  ?assertEqual([], Dealer1#dealer.burned -- Burned),
  
  Dealer2 = reshuffle_cards(Dealer1),
  ?assertEqual(erlang:length(Dealer1#dealer.deck) + 2, erlang:length(Dealer2#dealer.deck)),
  
  {Hole, Dealer3} = hole_cards(Dealer, 2),
  ?assertEqual([], Dealer3#dealer.dealt -- Hole),
  {Hole2, Dealer4} = discard_cards(Dealer3, Hole),
  ?assertEqual([], Dealer4#dealer.dealt -- Hole2),

  ?assertEqual([], Dealer4#dealer.burned -- Hole),
  {Board, Dealer5} = board_cards(Dealer, 3),
  ?assertEqual([], Dealer5#dealer.board -- Board)
  .
