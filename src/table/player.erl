-define(MAX_WAITING, 100).

json_data(Player) ->
  {}.

add_player(T, P, Position) when is_record(T, table) ->
  Table = remove_waiting(T, P),
  Player = P#player{state = ?WAIT},
  Players = gb_trees:insert(Player#player.id, Position, Table#table.players),
  Table#table{players = Players}.

add_waiting(Table, Player) ->
  Size = gb_sets:size(Table#table.waiting),
  if
    Size < MAX_WAITING ->
      {ok, Table#table{waiting = gb_sets:add(Player#player.id, Table#table.waiting)}};
    true ->
      {limit_exceed, Table}
  end.

remove_waiting(Table Player) ->
  Table#table{waiting = gb_sets:delete_any(Player#player.id, Table#table.waiting)}.
