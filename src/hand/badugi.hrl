%%
hand_badugi(Cards) when erlang:length(Cards) == 4 ->
  hand_badugi(
    new(Cards), [
      fun is_badugi4/1,
      fun is_badugi3/1,
      fun is_badugi2/1,
      fun is_badugi1/1
    ]).

hand_badugi(_, []) ->
  false;

hand_badugi(H, [F|Tail]) when is_function(F) ->
  case F(H) of
    false ->
      hand_badugi(H, Tail);
    Hand ->
      Hand
  end.

%%
compare_badugi(_, _) ->
  0.

%%
is_badugi4(Hand) ->
  case has_rainbow(Hand, 4) of
    true ->
      Hand#hand{rank = ?BADUGI_FOUR, high = card:highest(Hand#hand.cards), value = card:arrange_low(Hand#hand.cards)};
    _Else ->
      false
  end.

%% 2 cards same suit OR 2 cards same kind
%% not paired cards are different suit
test_badugi3(Hand) ->
  {
    has_paired(Hand, 2) and not_suited(Hand) or
    has_rainbow(Hand, 3) and not has_rainbow_paired(Hand),
    has_suited(Hand, 2) and not_paired(Hand)
  }.

is_badugi3(Hand) ->
  Value = case test_badugi3(Hand) of
    {true, _} -> %% AsAhJd3c
  
      [Paired | _] = [G || G <- Hand#hand.kinds, erlang:length(G) == 2],
      A = hd(Paired),
      lists:filter(fun(C) -> ?kind(C) /= ?kind(A) end, Hand#hand.cards) ++ [A];
  
    {_, true} -> %% AsJs2d3c
  
      [Suited | _] = [G || G <- Hand#hand.suits, erlang:length(G) == 2],
      Lowest = card:lowest(Suited),
      lists:filter(fun(C) -> ?suit(C) /= ?suit(Lowest) end, Hand#hand.cards) ++ [Lowest];
  
    _Else ->
      false
  end,
  
  case Value of
    false ->
      false;
    _Then ->
      Hand#hand{rank = ?BADUGI_THREE, high = card:highest(Value), value = card:arrange_low(Value)}
  end.

%% 3 cards same suit OR 3 cards same kind
%% 2 cards same kind AND 2 cards same suit
%% not paired cards are same suit 
test_badugi2(Hand) ->
  {
    has_paired(Hand, 3), has_suited(Hand, 3),
    has_paired(Hand, 2) and has_suited(Hand, 2),
    has_rainbow(Hand, 3) and has_rainbow_paired(Hand)
  }.

is_badugi2(Hand) ->
  Value = case test_badugi2(Hand) of
    {true, _, _, _} -> %% 2s2h2dAc
    
      [ThreeKind | _] = repeats(Hand#hand.kinds, 3),
      
      A = lists:filter(fun(C) -> ?kind(C) /= ?kind(hd(ThreeKind)) end, Hand#hand.cards),
      B = lists:filter(fun(C) -> ?suit(C) /= ?suit(hd(A)) end, ThreeKind),
      
      [hd(A), hd(B)];
    
    {_, true, _, _} -> %% 3d4d5dAc
    
      [Flush | _] = [G || G <- Hand#hand.suits, erlang:length(G) == 3],
      
      A = lists:filter(fun(C) -> ?suit(C) /= ?suit(hd(Flush)) end, Hand#hand.cards),
      B = lists:filter(fun(C) -> ?kind(C) /= ?kind(hd(A)) end, Flush),

      [hd(A), hd(B)];
    
    {_, _, true, _} ->
    
      [Flush | _] = [G || G <- Hand#hand.suits, erlang:length(G) == 2],
      
      A = hd(Flush),
      B = lists:filter(fun(C) -> (?kind(C) /= ?kind(A)) and (?suit(C) /= ?suit(A)) end, Hand#hand.cards),
      
      [A, hd(B)];
    
    {_, _, _, true} ->
      
      [Flush | _] = [G || G <- Hand#hand.suits, erlang:length(G) == 2],
      [hd(Flush), hd(hd(repeats(Hand#hand.kinds, 2)))];
    
    _Else ->
      false
  end,

  case Value of
    false ->
      false;
    _Then ->
      Hand#hand{rank = ?BADUGI_TWO, high = card:highers(Value), value = card:arrange_low(Value)}
  end.

%% all cards same suit OR all cards same kind
is_badugi1(Hand) ->
  case has_paired(Hand, 4) or has_suited(Hand, 4) of
    true ->
      
      Lowest = card:lowest(Hand#hand.cards),
      Hand#hand{rank = ?BADUGI_ONE, high = Lowest, value = [Lowest]};
    
    _Else ->
      false
  end.

%%
has_paired(Hand, N) when is_integer(N) ->
  Ged = [G || G <- Hand#hand.kinds, erlang:length(G) == N],
  erlang:length(Ged) == 1.

%%
has_suited(Hand, N) ->
  Ged = [G || G <- Hand#hand.suits, erlang:length(G) == N],
  erlang:length(Ged) == 1.

%%
not_paired(Hand) ->
  Ged = [G || G <- Hand#hand.kinds, erlang:length(G) > 1],
  erlang:length(Ged) == 0.
%%
not_suited(Hand) ->
  Ged = [G || G <- Hand#hand.suits, erlang:length(G) > 1],
  erlang:length(Ged) == 0.

%% all different kind and suit
has_rainbow(Hand, N) when is_integer(N) ->
  (erlang:length(Hand#hand.kinds) == N) and (erlang:length(Hand#hand.suits) == N).
%%
has_rainbow_paired(Hand) ->
  case erlang:length(Hand#hand.suits) == 4 of
    true ->
      false;
    _Else ->
    
      Ged = [G || G <- Hand#hand.suits, erlang:length(G) > 1],
      
      A = hd(Ged),
      B = lists:filter(fun(Card) -> ?suit(Card) /= ?suit(hd(A)) end, Hand#hand.cards),
      
      ?kind(hd(A)) == ?kind(hd(B))
  end.
