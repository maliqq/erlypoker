-define(LOW, "low").
-define(RANK_LOW, [{0, "A-5"}, {1, "A-6"}, {2, "2-7"}]).

low(Cards) ->
  Groups = group_kinds(Cards),
  if
    erlang:length(Groups) >= 5 ->
      Value = low_cards(Groups, 5),
      #hand{id="low", value = Value}
    true -> false
  end.

low8(Cards) ->
  Low8 = lists:filter(fun(C) -> card_index(C#card.kind, true) >= card_index("8", true) end, Cards),
  Hand = low(Low8),
  if
    Hand == false -> false;
    true -> Hand#hand{id="low8"}
  end.

ace_six(Cards) -> ok.

deuce_seven(Cards) when erlang:length(Cards) /= 5 -> throw("2-7 ranking allows 5 cards only");
deuce_seven(Cards) -> ok.
