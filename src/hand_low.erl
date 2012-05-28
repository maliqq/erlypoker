-define(LOW_ACE5, 13).
-define(LOW_DEUCE7, 14).
-define(LOW_ACE6, 15).

%% A-2-3-4-5
%% any low cards; repeats are ignored
hand_low(ace_five, Cards) ->
  Groups = card:group_kinds(Cards),
  if
    erlang:length(Groups) >= 5 ->
      Value = card:arrange_low(Groups, 5),
      #hand{rank={?LOW_ACE5}, value = Value};
    true -> false
  end;

%% 4-5-6-7-8
%% any low cards with 8 qualifier; repeats are ignored
hand_low(ace_six_better, Cards) ->
  Low8 = lists:filter(fun(C) -> card:index(C#card.kind, true) >= card:index("8", true) end, Cards),
  Hand = hand(ace_six, Low8),
  if
    Hand == false ->
      false;
    true ->
      Hand#hand{rank={?LOW_ACE5}}
  end;

%% A-2-3-4-6
%% 5 card rows with one gap, ace is low card; straights, repeats and flushes are ignored
hand_low(ace_six, Cards) ->
  Hand = card:arrange_low(Cards),
  {Rank, _} = hand(high, Hand),
  case Rank of
    ?HIGH_CARD ->
      Hand#hand{rank = {?LOW_ACE6}};
    _Else ->
      Hand
  end;

%% 2-3-4-5-7
%% 5 card rows with one gap, eq: 23467, 23567, 24567, 34568; straights, repeats and flushes are ignored

hand_low(deuce_seven, Cards) when erlang:length(Cards) == 5 ->
  Hand = card:arrange(Cards),
  {Rank, _} = hand(high, Hand),
  case Rank of
    ?HIGH_CARD ->
      Hand#hand{rank = {?LOW_DEUCE7}};
    _Else ->
      Hand
  end.

hand_low_test() ->
  ok.
