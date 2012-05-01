-define(ACE5, 13).
-define(DEUCE7, 14).
-define(ACE6, 15).

ace5_low(Cards) ->
  Groups = group_kinds(Cards),
  if
    erlang:length(Groups) >= 5 ->
      Value = low_cards(Groups, 5),
      #hand{rank={?ACE5}, value = Value};
    true -> false
  end.

ace5_low8(Cards) ->
  Low8 = lists:filter(fun(C) -> card_index(C#card.kind, true) >= card_index("8", true) end, Cards),
  Hand = ace5_low(Low8),
  if
    Hand == false ->
      false;
    true ->
      Hand#hand{rank={?ACE5}}
  end.

ace6_low(_) -> ok.

deuce7_low(Cards) when erlang:length(Cards) /= 5 -> throw("2-7 ranking allows 5 cards only");
deuce7_low(Cards) ->
  Hand = high_card(Cards),
  {Rank, _} = Hand,
  case Rank of
    ?HIGH_CARD ->
      Hand#hand{rank = {?DEUCE7}};
    _Else ->
      Hand
  end.
