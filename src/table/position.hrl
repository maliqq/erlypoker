-define(SB, 1).
-define(BB, 2).
-define(UTG, 3).
-define(UTG1, 4).
-define(UTG2, 5).
-define(MP, 6).
-define(MP1, 7).
-define(MP2, 8).
-define(CO, 9).
-define(BU, 10).

%% position alias
position_aliases(Max) ->
  Pos = [?SB, ?BB],
  Pos1 = if
    Max >= 4 -> Pos ++ [?UTG];
    true -> Pos
  end,
  Pos2 = Pos1 ++ case Max of
    5 -> [?MP];
    6 -> [?MP];
    7 -> [?MP, ?MP1];
    8 -> [?UTG1, ?MP, ?MP1];
    9 -> [?UTG1, ?MP, ?MP1, ?MP2];
    10 ->[?UTG1, ?UTG2, ?MP, ?MP1, ?MP2];
    _Else -> Pos1
  end,
  Pos3 = if
    Max >= 6 -> Pos2 ++ [?CO];
    true -> Pos2
  end,
  if
    Max >= 3 -> Pos3 ++ [?BU];
    true -> Pos3
  end.

move_position(N, Max) ->
  if
    N > Max ->
      N - Max;
    true ->
      N
  end.

is_after_button(Table, Seat) when is_record(Table, table) ->
  Button = Table#table.button,
  Max = Table#table.max,
  Middle = move_position(Button + Max div 2, Max),
  Opposite = move_position(Seat + Max div 2, Max),
  Opposite > Middle.

move_button(Table) when is_record(Table, table) ->
  Table#table{button = move_position(Table#table.button + 1, Table#table.max)}.
