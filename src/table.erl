-module(table).

-export([new/1]).

-include("poker.hrl").
-include("game.hrl").
-include_lib("eunit/include/eunit.hrl").

new(Max) ->
	#table{seats = seat:create(Max), players = gb_trees:empty(), waiting = gb_sets:empty()}.

json_data(Table) ->
	.

increase_blinds(Table, Blinds, Ante) ->
	.

next_deal(Table) ->
	.

rotate_game(Table, Game) ->
	.

cast_message(Table) ->
	.

emit_message(Table, Player) ->
	.

close(Table) ->
	Table#table{state = closed}.

pause(Table) ->
	Table#table{state = paused}.

table_test() ->
	.