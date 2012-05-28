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
	cast_message({
		blinds, increase, []
	}).

next_deal(Table) ->
	cast_message({
		deal, start, deal:json_data(Deal)
	}).

switch_game(Table, Game) ->
	cast_message({
		game, switch, game:json_data(Game)
	}).

cast_message(Table) ->
	.

emit_message(Table, Player) ->
	.

close(Table) ->
	Table#table{state = closed},
	cast_message({
		table, close
	}).

pause(Table) ->
	Table#table{state = paused},
	cast_message({
		table, pause
	}).

table_test() ->
	.