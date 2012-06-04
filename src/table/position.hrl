%%
position_names(Max) ->
  Pos = [<<"SB">>, <<"BB">>],
  Pos1 = if
    Max >= 4 -> Pos ++ [<<"UTG">>];
    true -> Pos
  end,
  Pos2 = Pos1 ++ case Max of
    5 -> [<<"MP">>];
    6 -> [<<"MP">>];
    7 -> [<<"MP1">>, <<"MP2">>];
    8 -> [<<"UTG+1">>, <<"MP1">>, <<"MP2">>];
    9 -> [<<"UTG+1">>, <<"MP1">>, <<"MP2">>,  <<"MP3">>];
    10 ->[<<"UTG+1">>, <<"UTG+2">>, <<"MP1">>, <<"MP2">>, <<"MP3">>];
    _Else -> []
  end,
  Pos3 = if
    Max >= 6 -> Pos2 ++ [<<"CO">>];
    true -> Pos2
  end,
  if
    Max >= 3 -> Pos3 ++ [<<"BTN">>];
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
