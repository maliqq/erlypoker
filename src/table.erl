%%
-module(table).

%% table types
-define(NORMAL, 1). %% cash game
-define(RANDOM, 2). %% random game
-define(BATTLE, 3). %% tournament game

-record(table, {
		id,
	  game, %% Texas Holdem No Limit
	  type,
	  state,
	  blinds, %% 50/100
	  ante = 0,
	  bring_in = 0,
	  seats = [],
	  players = [], %%
	  max = 9,
	  button = 1, %% current button position
	  current
  }).

-include("game.erl").
-include("deal.erl").
-include("bet.erl").

cycled_position(N, Max) ->
	if
		N > Max ->
			Max - N;
		true ->
			N
	end.

is_after_button(Table, Seat) ->
	Button = Table#table.button,
	Max = Table#table.max,
	Middle = cycled_position(Button + Max div 2, Max),
	Opposite = cycled_position(Seat + Max div 2, Max),
	Opposite > Middle.

%% cyclic increment #table.button
move_button(Table) when is_record(Table, table) ->
	Table#table{button = cycled_position(Table#table.button + 1, Table#table.max)}.
