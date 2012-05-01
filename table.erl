-define(CASH, 0).
-define(TOURNAMENT, 1).
-define(RANDOM, 2).

-record(table, {
	game, %% Texas Holdem No Limit
	blinds, %% 50/100
	ante = 0,
	bring_in = 0,
	seats,
	players, %% [{seat1, player1}, {seat2, player2}, ...]
	button = 1 %% current button position
	}).

%% cyclic increment #table.button
move_button(Table) ->
	Position = Table#table.button + 1,
	if
		Position > Table#table.seats -> Table#table{button = 1};
		false -> Table#table{button = Position}
	end.
