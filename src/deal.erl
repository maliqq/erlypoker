-module(deal).

-export([new/0, new/1, deal/3, to_string/1, json_data/1]).

-include("poker.hrl").
-include("table.hrl").
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
      {Seat#seat.index, Seat#seat.chips, Seat#seat.player}
    end, Seats)
  }.

deal(hole, _, _) ->
  ok;

deal(door, _, _) ->
  ok;

deal(open, _, _) ->
  ok;

deal(board, _, _) ->
  ok.

to_string(_) ->
  "".

json_data(_) ->
  {}.

deal_test() ->
  Deal = new()
  .
