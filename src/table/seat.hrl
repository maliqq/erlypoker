%%
create_seats(Max) ->
  Seats = erlang:make_tuple(Max, none),
  create_seats(Seats, Max).

create_seats(Seats, 0) ->
  Seats;

create_seats(Seats, Index) ->
  Seat = #seat{
    player = none,
    chips = 0,
    index = Index,
    state = ?EMPTY
  },
  create_seats(setelement(Index, Seats, Seat), Index - 1).

%%
reserve_seat(Table, Player, Position) when is_record(Table, table) ->
  Seat = element(Position, Table#table.seats),
  if
    Seat#seat.state == ?EMPTY ->
      {ok,
        Table#table{seats =
          setelement(Position, Table#table.seats, Seat#seat{
            player = Player,
            state = ?RESERVED
          })
        }
      };
    true ->
      {taken, Table}
  end.

%%
sit(Table, Player, Position, Amount) when is_record(Table, table) ->
  Seat = element(Position, Table#table.seats),
  if
    Seat#seat.state =:= ?EMPTY ->
      {ok,
        Table#table{seats =
          setelement(Position, Table#table.seats, Seat#seat{
            player = Player,
            chips = Amount,
            state = ?BUSY
          })
        }
      };
    true ->
      {taken, Table}
  end.

%%
sit(Table, Player, Position) when is_record(Table, table) ->
  if
    Table#table.type =:= ?BATTLE_TABLE ->
      Balance = Player#player.balance,
      sit(Table, Player, Position, Balance#balance.buy_in);
    true ->
      reserve_seat(Table, Player, Position)
  end.

%%
join(T, Player, Position) when is_record(T, table) ->
  {Result, Table} = sit(T, Player, Position),
  if
    Result == ok ->
      {Result, add_player(Table, Player, Position)};
    true ->
      {Result, Table}
  end.

%%
buy_in(Table, Player, Amount) when is_record(Table, table) ->
  Position = gb_trees:get(Player#player.id, Table#table.players),
  Seat = element(Position, Table#table.seats),
  SeatPlayer = Seat#seat.player,
  if
    (Seat#seat.state =:= ?RESERVED) and (SeatPlayer#player.id == Player#player.id) ->
      {ok, Table#table{seats =
        setelement(Position, Table#table.seats, Seat#seat{
          %%player = Player#player{state = ?WAIT},
          state = ?BUSY,
          chips = Amount
        })
      }};
    true ->
      {error, Table}
  end.
