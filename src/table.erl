-module(table).

-export([new/1]).

-include("poker.hrl").
-include("game.hrl").
-include_lib("eunit/include/eunit.hrl").

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

table_test() ->
	.