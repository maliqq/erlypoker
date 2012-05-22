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
	  waiting = [], %%

	  max = 9,
	  
	  button = 1, %% current button position
	  
	  current %% current deal
  }).

-include("game.erl").
-include("deal.erl").
-include("bet.erl").

new(Game, BigBlind) when is_integer(BigBlind) ->
	#table{
		game = Game,
		blinds = [BigBlind div 2, BigBlind]
	}.

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

is_after_button(Table, Seat) ->
	Button = Table#table.button,
	Max = Table#table.max,
	Middle = cycled_position(Button + Max div 2, Max),
	Opposite = cycled_position(Seat + Max div 2, Max),
	Opposite > Middle.

%% cyclic increment #table.button
move_button(Table) when is_record(Table, table) ->
	Table#table{button = cycled_position(Table#table.button + 1, Table#table.max)}.

players_with_state(Table, State) ->
	[Player || {_, Player} <- Table#table.players, Player#player.state == State].

activate_waiting(Table) when is_record(Table, table) ->
	Waiting = players_with_state(Table, ?WAITING),
	lists:foldl(fun(P, T) ->
		case is_after_button(T, P) of
			true ->
				change_player_state(T, P, ?ACTIVE);
			_Else ->
				T
		end
	end, Table, Waiting).

can_deal(Table) when is_record(Table, table) ->
	ActivePlayers = players_with_state(Table, ?ACTIVE),
	erlang:length(ActivePlayers) > 1 andalso ActivePlayers.

deal(Table) ->
	case can_deal(Table) of
		false -> false;
		ActivePlayers ->
			T = activate_waiting(move_button(Table)),
			Deal = deal:new(T#table.game, ActivePlayers),
			T#table{current = Deal}
	end.

sit(Table, Player) when is_record(Table, table), is_record(Player, player) ->
	Table#table{players = lists:append({Player#player.id, Player}, Table#table.players)}.

sit(Table, Seat, Player) when is_record(Table, table), is_record(Player, player) ->
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
	G = game:new(?TEXAS, ?NO_LIMIT),
	T = new(G, 100),
	
	P1 = #player{id = 9999, name = "malik"},
	P2 = #player{id = 8888, name = "kairat"},

	T1 = sit(T, 1, P1, 10000),
	T2 = sit(T1, 2, P2, 10000),

	deal(T2).
