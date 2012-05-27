-module(position).

-export([alias/1, move_button/1]).

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
alias(Max) ->
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

cycle(N, Max) ->
  if
    N > Max ->
      N - Max;
    true ->
      N
  end.

is_after_button(Table, Seat) when is_record(Table, table) ->
  Button = Table#table.button,
  Max = Table#table.max,
  Middle = cycle(Button + Max div 2, Max),
  Opposite = cycle(Seat + Max div 2, Max),
  Opposite > Middle.

move_button(Table) when is_record(Table, table) ->
  Table#table{button = cycle(Table#table.button + 1, Table#table.max)}.

position_test() ->
  ?assertEqual(cycle(1, 9), 1),
  ?assertEqual(cycle(10, 9), 1),
  Table = #table{max = 9},
  Table1 = move_button(Table),
  ?assertEqual(Table1#table.button, 2),
  Table2 = #table{button = 9, max = 9},
  Table3 = move_button(Table2),
  ?assertEqual(Table3#table.button, 1),
  Table4 = #table{button = 1, max = 9},
  ?assert(is_after_button(Table4, 2)),
  ?assert(is_after_button(Table4, 3)),
  ?assertNot(is_after_button(Table4, 6)).
