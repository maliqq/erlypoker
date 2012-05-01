-record(card, {kind, suit}).
-record(card_group, {kind = none, suit = none, value}).

-define(SUITS, ["s", "h", "d", "c"]).
-define(KINDS, ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"]).

-define(KIND_NAMES, [{"A", "ace"}, {"K", "king"}, {"Q", "queen"}, {"J", "jack"}, {"T", "ten"}, {"9", "nine"}, {"8", "eight"}, {"7", "seven"}, {"6", "six"}, {"5", "five"}, {"4", "four"}, {"3", "three"}, {"2", "deuce"}]).

%% some hardcode
suit_to_char(Suit) ->
  case Suit of
    "s" -> <<"♠"/utf8>>;
    "h" -> <<"♥"/utf8>>;
    "d" -> <<"♦"/utf8>>;
    "c" -> <<"♣"/utf8>>
  end.

%% some hardcode
suit_index(Suit) ->
  case Suit of
    "s" -> 0;
    "h" -> 1;
    "d" -> 2;
    "c" -> 3
  end.

suit_to_string("s") ->
  "spade";
suit_to_string("h") ->
  "heart";
suit_to_string("d") ->
  "diamond";
suit_to_string("c") ->
  "club".

kind_to_string(Kind) -> {_, Name} = lists:keyfind(Kind, 1, ?KIND_NAMES), Name.

%% new card
build_card(Kind, Suit) ->
  #card{kind = Kind, suit = Suit}.
build_card(N) when is_integer(N) ->
  build_card(lists:nth(N rem erlang:length(?KINDS), ?KINDS), lists:nth(N div erlang:length(?KINDS) + 1, ?SUITS));
build_card(String) ->
  [Kind, Suit] = re:split(String, "", [{return, list}, {parts, 2}]), build_card(Kind, Suit).

%% build cards from string values like "KhJd8s9s2s"
parse_cards(String) ->
  {_, Result} = re:run(String, "([akqjt2-9]{1})([shdc]{1})", [global, caseless, {capture, [1, 2], list}]),
  lists:map(fun([Kind, Suit]) -> build_card(Kind, Suit) end, Result).
cards(String) ->
  parse_cards(String).

%% all cards
cards() ->
  [build_card(Kind, Suit) || Kind <- ?KINDS, Suit <- ?SUITS].

%%
card_to_string(Card) when is_record(Card, card) ->
  io_lib:format("~s~ts", [Card#card.kind, suit_to_char(Card#card.suit)]);
card_to_string(Card) when is_record(Card, card_group) ->
  io_lib:format("~s{~ts}", [Card#card_group.kind, string:join(lists:map(fun(C) -> suit_to_char(C#card.suit) end, Card#card_group.value), "")]).

%%
cards_to_string(Cards) ->
  string:join(lists:map(fun(Card) -> card_to_string(Card) end, Cards), " ").

%%
card_to_integer(Card) ->
  card_index(Card#card.kind) + erlang:length(?KINDS) * suit_index(Card#card.suit).

%%
card_index(N, _) when is_integer(N) ->
  N rem erlang:length(?KINDS);
card_index(Card, Low) when is_record(Card, card) ->
  card_index(Card#card.kind, Low);
card_index(Card, Low) when is_record(Card, card_group) ->
  card_index(Card#card_group.kind, Low);
card_index(Card, Low) ->
  Kinds = if
    Low -> [Ace | Tail] = ?KINDS, Tail ++ [Ace];
    true -> ?KINDS
  end,
  {Map, _} = lists:mapfoldl(fun(X, I) -> {{X, I}, I + 1} end, 1, Kinds),
  {_, Index} = lists:keyfind(Card, 1, Map),
  Index.
card_index(Card) ->
  card_index(Card, false).

%%
compare_cards(A, B) ->
  card_index(A) =< card_index(B).
compare_cards_low(A, B) ->
  card_index(B, true) =< card_index(A, true).

%%
diff_cards(A, B) ->
  erlang:abs(card_index(A) - card_index(B)).

%% from A to 2
high_cards(Cards) ->
  lists:sort(fun compare_cards/2, Cards).
high_cards(Cards, N) when is_integer(N) ->
  lists:sublist(high_cards(Cards), N).
%%
low_cards(Cards) ->
  lists:sort(fun compare_cards_low/2, Cards).
low_cards(Cards, N) when is_integer(N) ->
  lists:sublist(low_cards(Cards), N).

%%
highest_card(Cards) ->
  [Highest | _] = lists:sort(fun compare_cards/2, Cards), Highest.
lowest_card(Cards) ->
  [Lowest | _] = lists:sort(fun compare_cards_low/2, Cards), Lowest.
%%
kicker_cards(Cards, Except, N) when is_integer(N) ->
  Kinds = lists:map(fun(X) -> X#card.kind end, Except),
  Kickers = lists:filter(fun(X) -> not lists:member(X#card.kind, Kinds) end, Cards),
  high_cards(Kickers, N).

%% group cards
group_cards(_, [], Buf, F) when is_function(F) ->
  [Buf];
group_cards(Prev, [Next|Tail], Buf, F) when is_function(F) ->
  case F(Prev, Next) of
    true -> group_cards(Next, Tail, Buf ++ [Next], F);
    _Else -> [Buf] ++ group_cards(Next, Tail, [Next], F) 
  end.
group_cards(F, [H|T]) when is_function(F) ->
  group_cards(H, T, [H], F).

%% split cards into rows
%% e.q.
%%  Ah4sAd2cQdQhTc8cJsKs3c
%% split result:
%%  Group1: [A{h,d}, Ks, Q{d,h}, Js, Tc] Group2: [8c] Group3: [2c, 3c, 4d]
split_rows(Cards) ->
  Grouped = group_cards(fun(Prev, Next) -> diff_cards(Prev, Next) < 2 end, high_cards(Cards)),
  lists:map(fun(G) -> [First | _] = G, #card_group{kind = First#card.kind, value = G} end, Grouped).

%% cards with same kind
group_suits(Cards) ->
  Sorted = lists:keysort(1, lists:map(fun(C) -> {suit_index(C#card.suit), C} end, Cards)),
  Grouped = group_cards(fun(Prev, Next) -> Prev#card.suit == Next#card.suit end, lists:map(fun({_, C}) -> C end, Sorted)),
  lists:map(fun(G) -> [First | _] = G, #card_group{suit = First#card.suit, value = G} end, Grouped).

%% cards with same suit
group_kinds(Cards) ->
  Grouped = group_cards(fun(Prev, Next) -> Prev#card.kind == Next#card.kind end, high_cards(Cards)),
  lists:map(fun(G) -> [First | _] = G, #card_group{kind = First#card.kind, value = G} end, Grouped).

%%
card_frequency(Cards, Num) ->
  [Repeat || Repeat <- group_kinds(Cards), erlang:length(Repeat#card_group.value) == Num].

%%
shuffle_cards(Cards) ->
  random:seed(erlang:now()),
  lists:sort(fun(_, _) -> random:uniform() =< 0.5 end, Cards).

%%
new_deck() ->
  shuffle_cards(cards()).

%%
test_suit() ->
  io:format("suits: ~p~n", [?SUITS]),
  io:format("spade: ~ts~n", [suit_to_char("s")]).

%%
test_kind() ->
  io:format("all kinds: ~p~n", [?KINDS]).

%%
test_card() ->
  io:format("parsed from string: "),
  [io:format("~ts ", [card_to_string(Card)]) || Card <- parse_cards("AhJd")],
  io:format("~nall cards: "),
  [io:format("~ts ", [card_to_string(Card)]) || Card <- cards()],
  io:format("~ncard indexes: "),
  [io:format("~s(~p) ", [Kind, card_index(Kind)]) || Kind <- ?KINDS],
  io:format("~nsorted cards: "),
  [io:format("~ts ", [card_to_string(Card)]) || Card <- high_cards(parse_cards("Ah7dJcTsKs"))],
  io:format("~n"),
  io:format("card integer of Kd: ~p; card of 28: ~ts~n", [card_to_integer(build_card("K", "d")), card_to_string(build_card(28))]),
  io:format("~n"),
  io:format("rows: ~p~n", [split_rows(parse_cards("Ah4sAd2cQdQhTc8cJsKs3c"))]),
  io:format("suits: ~p~n", [group_suits(parse_cards("Ah4sAd2cQdQhTc8cJsKs3c"))]),
  io:format("kinds: ~p~n", [group_kinds(parse_cards("Ah4sAd2cQdQhTc8cJsKs3c"))]).

%%
test_deck() ->
  io:format("random deck: "),
  [io:format("~ts ", [card_to_string(Card)]) || Card <- new_deck()],
  io:format("~n").
