-module(deal).

-export([new/0]).

-record(deal, {
    id,
    table_id,
    dealer,
    game,
    players,
    positions,    %% positions refs
    chip_count,   %% chip count refs
    betting,      %% action log
    pot,
    showdown,
    results %% winners, losers
  }).

new() ->
  #deal{dealer = dealer:new(), pot = pot:new()}.

json_data(Deal) ->
  .

start(Game, Seats) ->
  .
