%%
new(Hand) when is_record(Hand, hand) ->
  Hand;
new(Cards) ->
  #hand{
    cards = Cards,
    kinds = card:group_kinds(Cards),
    suits = card:group_suits(Cards)
  }.

%%
hand_high(Cards) when is_list(Cards) and (erlang:length(Cards) =< 7) and (erlang:length(Cards) > 0) ->
  hand_high(
    new(Cards), [

    fun is_straight_flush/1,
    fun is_three_kind/1,
    fun is_two_pair/1,
    fun is_one_pair/1,
    fun is_high_card/1
  ]).

hand_high(_, []) ->
  false;

hand_high(H, [F|List]) when is_function(F) ->
  case F(H) of
    false -> hand_high(H, List);
    Hand -> Hand
  end.

%%
compare_high(_, _) ->
  0.

%%
repeats(Hand, Num) ->
  [Repeat || Repeat <- Hand#hand.kinds, erlang:length(Repeat) == Num].

%% 5 (or more) cards in a row
is_straight(H) ->
  Hand = H#hand{rows = card:split_rows(H#hand.cards)},
  Rows = [Row || Row <- Hand#hand.rows, erlang:length(Row) >= 5],
  if
    erlang:length(Rows) == 0 ->
      false;
    
    true ->
      Straight = hd(Rows), %% we have max 7 cards per hand, one straight is possible
      Highest = card:highest(Straight),
      
      Hand#hand{rank = ?STRAIGHT, value = Straight, high = Highest}
  end.

%% 5 (or more) cards with same suit
is_flush(Hand) ->
  Suits = [Group || Group <- Hand#hand.suits, erlang:length(Group) >= 5],
  if
    erlang:length(Suits) == 0 ->
      false;
    
    true ->
      Flush = hd(Suits), %% we have max 7 cards per hand, one flush is possible
      Highest = card:highest(Flush),
      
      Hand#hand{rank = ?FLUSH, value = Flush, high = Highest}
  end.

%% 5 cards with same suit in a row
is_straight_flush(Hand) ->
  case is_flush(Hand) of
    false ->
      hand_high(Hand, [fun is_four_kind/1, fun is_full_house/1, fun is_straight/1]); %% skip flush
    
    Flush ->
      case is_straight(Flush#hand{cards = Flush#hand.value}) of
        false ->
          hand_high(Hand, [fun is_four_kind/1, fun is_full_house/1]); %% skip straight & flush
        
        Straight ->
          Straight#hand{rank = ?STRAIGHT_FLUSH, cards = Hand#hand.cards}
      end
  end.

%% AAAAB, "four of A with B kicker"
is_four_kind(Hand) ->
  Repeats = repeats(Hand, 4),
  if
    erlang:length(Repeats) == 0 ->
      false;
    
    true ->
      FourKind = hd(Repeats), %% we have max 7 cards per hand, one four kind is possible
      Hand#hand{rank = ?FOUR_KIND, value = FourKind,
        kickers = card:kickers(Hand#hand.cards, FourKind, 1)}
  end.

%% AAABC, "three of A with B and C kicker"
is_three_kind(Hand) ->
  Repeats = repeats(Hand, 3),
  if
    erlang:length(Repeats) == 1 ->
      
      ThreeKind = hd(Repeats),
      #hand{rank = ?THREE_KIND, value = ThreeKind,
        kickers = card:kickers(Hand#hand.cards, ThreeKind, 2)};
    
    true ->
      false
  end.

%% AAABB, "A's full of B", no kickers
is_full_house(Hand) ->
  ThreeKind = repeats(Hand, 3),
  Pairs = repeats(Hand, 2),
  if
    erlang:length(ThreeKind) == 0 ->
      false;
    
    erlang:length(ThreeKind) > 0, erlang:length(Pairs) == 0 ->
      false;
    
    true ->
      {Major, Minor} = if
        erlang:length(ThreeKind) >= 2 -> %% two "tripses" are one full house
    
          [Higher, Lower | _] = card:arrange_groups(ThreeKind),
          {Higher, Lower};
    
        true ->
          {card:highest(ThreeKind), card:highest(Pairs)} %% one trips plus highest pair
      end,

      Value = Major ++ Minor,
      Hand#hand{rank = ?FULL_HOUSE, value = Value, high = [hd(Major), hd(Minor)]}
  end.

%% AABBC, "two pair of A's and B's with C kicker"
is_two_pair(Hand) ->
  Pairs = repeats(Hand, 2),
  if
    erlang:length(Pairs) >= 2 ->
    
      [Major, Minor | _] = card:arrange_groups(Pairs),
      Value = Major ++ Minor,
      Hand#hand{rank = ?TWO_PAIR, value = Value,
        kickers = card:kickers(Hand#hand.cards, Value, 1), high = [hd(Major), hd(Minor)]};
    
    true ->
      false
  end.

%% AABCD, "one pair of A with B, C, D kicker"
is_one_pair(Hand) ->
  Pairs = repeats(Hand, 2),
  if
    erlang:length(Pairs) == 1 ->

      Pair = hd(Pairs),
      Hand#hand{rank = ?ONE_PAIR, value = Pair,
        kickers = card:kickers(Hand#hand.cards, Pair, 3)};
    
    true ->
      false
  end.

%% ABCDE, "high card A with B, C, D, E kicker"
is_high_card(Hand) ->
  Highest = card:highest(Hand#hand.cards),
  Hand#hand{rank = ?HIGH_CARD, value = [Highest],
    kickers = card:kickers(Hand#hand.cards, [Highest], 4)}.
