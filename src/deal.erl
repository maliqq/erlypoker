-module(deal).

-export([new/0]).

-record(deal, {
    id,
    game,
    start,
    stage,
    dealer,
    players,
    betting, pot, showdown, results, timing
  }).

-include_lib("eunit/include/eunit.hrl").

new() ->
  #deal{dealer = dealer:new(), pot = pot:new()}.

new(Table) when is_record(Table, table) ->
  New = new(),
  Seats = lists:from_tuple(Table#table.seats),
  New#deal{
    game = Table#table.game,
    players = lists:map(fun(Seat) -> Seat#seat.player end, Seats),
    positions = lists:map(fun(Seat) ->
      {Seat#seat.index, Seat#seat.chips, seat#seat.player_id}
    end, Seats)
  }.

deal(hole, N, Deal) ->
  ok.

deal(door, N, Deal) ->
  ok.

deal(open, N, Deal) ->
  ok.

deal(board, N, Deal) ->
  ok.

to_string(Deal) ->
  "".

json_data(Deal) ->
  {}.

deal_test() ->
  ok.
