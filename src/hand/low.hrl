%%
compare_low(_, _, _) ->
  0.

%% A-2-3-4-5
%% any low cards; repeats are ignored
hand_low(ace_five, Cards) ->
  Groups = card:group_kinds(Cards),
  if
    erlang:length(Groups) >= 5 ->
      #hand{rank = ?LOW_ACE5, value = card:arrange_groups(low, Groups, 5)};
    
    true -> false
  end;

%% 4-5-6-7-8
%% any low cards with 8 qualifier; repeats are ignored
hand_low(ace_five_better, Cards) ->
  Eight = lists:filter(fun(C) ->
    card:index(C, true) >= card:index("8", true)
  end, Cards),
  
  Hand = hand(ace_five, Eight),
  if
    Hand == false ->
      false;
    
    true ->
      Hand#hand{rank = ?LOW_ACE5_BETTER}
  end;

%% A-2-3-4-6
%% 5 card rows with one gap, ace is low card; straights, repeats and flushes are ignored
hand_low(ace_six, Cards) ->
  Hand = card:arrange_low(Cards),
  {Rank, _} = hand(high, Hand),
  
  case Rank of
    ?HIGH_CARD ->
      Hand#hand{rank = ?LOW_ACE6};
  
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
      Hand#hand{rank = ?LOW_DEUCE7};
  
    _Else ->
      Hand
  end.
