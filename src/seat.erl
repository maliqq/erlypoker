%%
-module(seat).

-export([create/1, reserve/3, take/4, take/3, join/3, buy_in/3]).

-include("poker.hrl").
-include_lib("eunit/include/eunit.hrl").

create(Max) ->
  Seats = erlang:make_tuple(Max, none),
  create(Seats, Max).

create(Seats, 0) ->
  Seats;

create(Seats, Index) ->
  Seat = #seat{
    player = none,
    chips = 0,
    index = Index,
    state = ?EMPTY
  },
  create(setelement(Index, Seats, Seat), Index - 1).

reserve(Table, Player, Position) when is_record(Table, table) ->
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

sit(Table, Player, Position) when is_record(Table, table) ->
  if
    Table#table.type =:= ?TBL_BATTLE ->
      Balance = Player#player.balance,
      sit(Table, Player, Position, Balance#balance.buy_in);
    true ->
      reserve(Table, Player, Position)
  end.

join(T, Player, Position) when is_record(T, table) ->
  {Result, Table} = take(T, Player, Position),
  if
    Result == ok ->
      {Result, player:add(Table, Player, Position)};
    true ->
      {Result, Table}
  end.

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

seat_test() ->
  ?assertEqual(3, erlang:tuple_size(create(3))),
  Seat = element(1, create(3)),
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
