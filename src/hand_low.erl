-define(ACE5, 13).
-define(DEUCE7, 14).
-define(ACE6, 15).

%% A-2-3-4-5
%% any low cards; repeats are ignored
ace5_low(Cards) ->
  Groups = card:group_kinds(Cards),
  if
    erlang:length(Groups) >= 5 ->
      Value = card:arrange_low(Groups, 5),
      #hand{rank={?ACE5}, value = Value};
    true -> false
  end.

%% 4-5-6-7-8
%% any low cards with 8 qualifier; repeats are ignored
ace5_low8(Cards) ->
  Low8 = lists:filter(fun(C) -> card:index(C#card.kind, true) >= card:index("8", true) end, Cards),
  Hand = ace5_low(Low8),
  if
    Hand == false ->
      false;
    true ->
      Hand#hand{rank={?ACE5}}
  end.

%% A-2-3-4-6
%% 5 card rows with one gap, ace is low card; straights, repeats and flushes are ignored
ace6_low(Cards) ->
  Hand = card:arrange_low(Cards),
  {Rank, _} = high_card(Hand),
  case Rank of
    ?HIGH_CARD ->
      Hand#hand{rank = {?ACE6}};
    _Else ->
      Hand
  end.

%% 2-3-4-5-7
%% 5 card rows with one gap, eq: 23467, 23567, 24567, 34568; straights, repeats and flushes are ignored

deuce7_low(Cards) when erlang:length(Cards) == 5 ->
  Hand = card:arrange(Cards),
  {Rank, _} = high_card(Hand),
  case Rank of
    ?HIGH_CARD ->
      Hand#hand{rank = {?DEUCE7}};
    _Else ->
      Hand
  end.

test_low_hand() ->
  ok.