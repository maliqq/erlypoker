-define(WAITING, 0). %% waiting next deal
-define(IDLE, 1). %% sit out
-define(AWAY, 2). %% disconnected
-define(ACTIVE, 3). %% active

-include("game.erl").
-include("deal.erl").

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

-record(player, {
		id,
		name,
		state,
		chips,
		cards,
		timer
	}).

new_table(Game, BigBlind) when is_integer(BigBlind) ->
	#table{
		game = Game,
		blinds = [BigBlind div 2, BigBlind]
	}.

new_player(Name, Chips) ->
	#player{name = Name, chips = Chips}.

change_player_state(Table, Player, State) when is_record(Table, table), is_record(Player, player) ->
	case lists:keyfind(Player#player.id, 1, Table#table.players) of
		{_, Player} ->
			ChangedPlayer = Player#player{state = State},
			Players = lists:keyreplace(Player#player.id, 1, Table#table.players, {Player#player.id, ChangedPlayer}),
			Table#table{players = Players};
		_Else ->
			Table
	end.

cycled_position(N, Max) ->
	if
		N > Max ->
			Max - N;
		true ->
			N
	end.

is_before_button(Table, Player) ->
	Button = Table#table.button,
	Max = Table#table.max,
	{Seat, _} = lists:keyfind(Player#player.id, 2, Table#table.seats),
	Middle = cycled_position(Button + Max div 2, Max),
	SeatN = cycled_position(Seat + Max div 2, Max),
	if
		SeatN =< Middle -> true;
		SeatN > Middle -> false
	end.

activate_waiting(Table) when is_record(Table, table) ->
	Waiting = [Player || {_, Player} <- Table#table.players, Player#player.state == ?WAITING],
	lists:foldl(fun(P, T) ->
		case is_before_button(T, P) of
			true ->
				change_player_state(T, P, ?ACTIVE);
			_Else ->
				T
		end
	end, Table, Waiting).

can_deal(Table) when is_record(Table, table) ->
	ActivePlayers = [Player || {_, Player} <- Table#table.players, Player#player.state == ?ACTIVE],
	erlang:length(ActivePlayers) > 1 andalso ActivePlayers.

start_deal(Table) ->
	case can_deal(Table) of
		false -> false;
		ActivePlayers ->
			T = activate_waiting(move_button(Table)),
			Deal = new_deal(T#table.game, ActivePlayers),
			T#table{current = Deal}
	end.

%% cyclic increment #table.button
move_button(Table) when is_record(Table, table) ->
  NewPosition = Table#table.button + 1,
	Table#table{button = cycled_position(NewPosition, Table#table.max)}.

add_player(Table, Player) when is_record(Table, table), is_record(Player, player) ->
	Table#table{players = lists:append({Player#player.id, Player}, Table#table.players)}.

add_player(Table, Seat, Player) when is_record(Table, table), is_record(Player, player) ->
	if
		erlang:length(Table#table.seats) + 1 > Table#table.max ->
			throw("all seats are busy");
		true ->
			case lists:keyfind(Seat, 1, Table#table.seats) of
				false ->
					Waiting = Player#player{state = ?WAITING},
					T = add_player(Table, Waiting),
					T#table{seats = lists:append({Seat, Player#player.id}, T#table.seats)};
				{_, _} ->
					throw("this seat is busy")
			end
	end.

test_table() ->
	G = new_game(?TEXAS, ?HOLDEM, ?NO_LIMIT),
	T = new_table(G, 100),
	P1 = new_player("malik", 10000),
	P2 = new_player("kairat", 10000),
	T2 = add_player(T, 1, P1),
	T3 = add_player(T2, 2, P2),
	start_deal(T3).
