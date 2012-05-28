-define(BADUGI_ONE, 9).
-define(BADUGI_TWO, 10).
-define(BADUGI_THREE, 11).
-define(BADUGI_FOUR, 12). %% complete badugi card

%%
hand_badugi(Cards) when erlang:length(Cards) == 4 ->
  hand_badugi(Cards, [fun is_badugi4/1, fun is_badugi3/1, fun is_badugi2/1, fun is_badugi1/1]).
hand_badugi(_, []) ->
  false;
hand_badugi(Cards, [F|Tail]) when is_function(F) ->
  case F(Cards) of
    false -> hand_badugi(Cards, Tail);
    Hand -> Hand
  end.

%%
is_badugi4(Cards) ->
  case has_rainbow(Cards, 4) of
    true ->
      Highest = card:highest(Cards),
      #hand{rank = {?BADUGI_FOUR, Highest#card.kind}, value = card:arrange_low(Cards)};
    _Else ->
      false
  end.

%% 2 cards same suit OR 2 cards same kind
%% not paired cards are different suit
is_badugi3(Cards) ->
  Value = case {
      has_paired(Cards, 2) and not_suited(Cards) or has_rainbow(Cards, 3) and not has_rainbow_paired(Cards),
      has_suited(Cards, 2) and not_paired(Cards)
  } of
    {true, _} -> %% AsAhJd3c
      [Paired | _] = [Group || Group <- card:group_kinds(Cards), erlang:length(Group#card_group.value) == 2],
      [A | _] = Paired#card_group.value,
      lists:filter(fun(C) -> C#card.kind /= A#card.kind end, Cards) ++ [A];
    {_, true} -> %% AsJs2d3c
      [Suited | _] = [Group || Group <- card:group_suits(Cards), erlang:length(Group#card_group.value) == 2],
      Lowest = card:lowest(Suited#card_group.value),
      lists:filter(fun(C) -> C#card.suit /= Lowest#card.suit end, Cards) ++ [Lowest];
    _Else ->
      false
  end,
  case Value of
    false ->
      false;
    _Then ->
      Highest = card:highest(Value),
      #hand{rank = {?BADUGI_THREE, Highest#card.kind}, value = card:arrange_low(Value)}
  end

%% 3 cards same suit OR 3 cards same kind
%% 2 cards same kind AND 2 cards same suit
%% not paired cards are same suit 
is_badugi2(Cards) ->
  Value = case {
      has_paired(Cards, 3),
      has_suited(Cards, 3),
      has_paired(Cards, 2) and has_suited(Cards, 2),
      has_rainbow(Cards, 3) and has_rainbow_paired(Cards)
  } of
    {true, _, _, _} -> %% 2s2h2dAc
      [ThreeKind | _] = card:freq(Cards, 3),
      [A | _] = lists:filter(fun(C) -> C#card.kind /= ThreeKind#card_group.kind end, Cards),
      [B | _] = lists:filter(fun(C) -> C#card.suit /= A#card.suit end, ThreeKind#card_group.value),
      [A, B];
    {_, true, _, _} -> %% 3d4d5dAc
      [Flush | _] = [Group || Group <- card:group_suits(Cards), erlang:length(Group#card_group.value) == 3],
      [A | _] = lists:filter(fun(C) -> C#card.suit /= Flush#card_group.suit end, Cards),
      [B | _] = lists:filter(fun(C) -> C#card.kind /= A#card.kind end, Flush#card_group.value),
      [A, B];
    {_, _, true, _} ->
      [Flush | _] = [Group || Group <- card:group_suits(Cards), erlang:length(Group#card_group.value) == 2],
      [A | _] = Flush,
      [B | _] = lists:filter(fun(C) -> (C#card.kind /= A#card.kind) and (C#card.suit /= A#card.suit) end, Cards),
      [A, B];
    {_, _, _, true} ->
      [Flush | _] = [Group || Group <- card:group_suits(Cards), erlang:length(Group#card_group.value) == 2],
      [A | _] = Flush,
      [Pairs | _] = card:freq(Cards, 2),
      [B | _] = Pairs,
      [A, B];
    _Else ->
      false
  end,
  case Value of
    false ->
      false;
    _Then ->
      Highest = card:highest(Value),
      #hand{rank = {?BADUGI_TWO, Highest#card.kind}, value = card:arrange_low(Value)}
  end.

%% all cards same suit OR all cards same kind
is_badugi1(Cards) ->
  case has_paired(Cards, 4) or has_suited(Cards, 4) of
    true ->
      Lowest = card:lowest(Cards),
      #hand{rank = {?BADUGI_ONE, Lowest#card.kind}, value = Lowest};
    _Else ->
      false
  end.

%%
has_paired(Cards, N) when is_integer(N) ->
  Grouped = [Group || Group <- card:group_kinds(Cards), erlang:length(Group#card_group.value) == N],
  erlang:length(Grouped) == 1.

%%
has_suited(Cards, N) ->
  Grouped = [Group || Group <- card:group_suits(Cards), erlang:length(Group#card_group.value) == N],
  erlang:length(Grouped) == 1.

%%
not_paired(Cards) ->
  Grouped = [Group || Group <- card:group_kinds(Cards), erlang:length(Group#card_group.value) > 1],
  erlang:length(Grouped) == 0.
%%
not_suited(Cards) ->
  Grouped = [Group || Group <- card:group_suits(Cards), erlang:length(Group#card_group.value) > 1],
  erlang:length(Grouped) == 0.

%% all different kind and suit
has_rainbow(Cards, N) when is_integer(N) ->
  (erlang:length(card:group_kinds(Cards)) == N) and (erlang:length(card:group_suits(Cards)) == N).
%%
has_rainbow_paired(Cards) ->
  case erlang:length(card:group_suits(Cards)) == 4 of
    true ->
      false;
    _Else ->
      Grouped = [Group || Group <- card:group_suits(Cards), erlang:length(Group#card_group.value) > 1],
      [A | _] = Grouped,
      [B | _] = lists:filter(fun(C) -> C#card.suit /= A#card_group.suit end, Cards),
      A#card_group.kind == B#card.kind
  end.

hand_badugi_test() ->
  io:format("~ts~n", [to_string(hand_badugi(card:wrap("2d3c4s4s")))]).
