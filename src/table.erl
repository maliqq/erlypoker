-module(table).

-export([new/1]).

-include("poker.hrl").
-include("game.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("table/seat.erl").
-include("table/player.erl").
-include("table/position.erl").

new(Max) ->
	#table{seats = seat:create(Max), players = gb_trees:empty(), waiting = gb_sets:empty()}.

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
  ?assertEqual(cycle(1, 9), 1),
  ?assertEqual(cycle(10, 9), 1),
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

table_test() ->
	.
