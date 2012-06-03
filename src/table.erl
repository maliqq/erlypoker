-module(table).

-export([new/1]).

-include("table.hrl").
-include("game.hrl").
-include_lib("eunit/include/eunit.hrl").

new(Max) ->
	#table{seats = create_seats(Max), players = gb_trees:empty(), waiting = gb_sets:empty()}.

json_data(Table) ->
	{}.

increase_blinds(Table, Blinds, Ante) ->
	ok.

next_deal(Table) ->
	ok.

switch_game(Table, Game) ->
	ok.

close(Table) ->
	ok.

pause(Table) ->
	ok.

position_test() ->
  ?assertEqual(move_position(1, 9), 1),
  ?assertEqual(move_position(10, 9), 1),
  Table = #table{max = 9},
  Table1 = move_button(Table),
  ?assertEqual(Table1#table.button, 2),
  Table2 = #table{button = 9, max = 9},
  Table3 = move_button(Table2),
  ?assertEqual(Table3#table.button, 1),
  Table4 = #table{button = 1, max = 9},
  ?assert(is_after_button(Table4, 2)),
  ?assert(is_after_button(Table4, 3)),
  ?assertNot(is_after_button(Table4, 6)).

%%
seat_test() ->
  ?assertEqual(3, erlang:tuple_size(create_seats(3))),
  Seat = element(1, create_seats(3)),
  ?assertEqual(seat, element(1, Seat)),

  T = table:new(6),

  Player1 = #player{id = 1, name = "maliqq"},
  Player2 = #player{id = 2, name = "k8ldykbala"},

  Seat = element(1, T#table.seats),
  ?assertEqual(?EMPTY, Seat#seat.state),

  {Result, T1} = join(T, Player1, 1),

  Seat1 = element(1, T1#table.seats),
  ?assertEqual(?RESERVED, Seat1#seat.state),

  ?assertEqual(ok, Result),

  {Result, T2} = buy_in(T1, Player1, 10000),

  ?assertEqual(ok, Result),
  Seat2 = element(1, T2#table.seats),
  ?assertEqual(10000, Seat2#seat.chips),
  ?assertEqual(?BUSY, Seat2#seat.state),

  {Result3, T3} = join(T1, Player2, 1),
  ?assertEqual(taken, Result3)
  .

table_test() ->
	ok.
