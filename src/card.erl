-module(card).
-export([
    test/0, to_string/1, wrap/1, deck/0,
    group_kinds/1, group_suits/1, all/0, shuffle/1, pack/1, suit_to_string/1, kind_to_string/1,
    arrange/1, arrange/2, arrange_low/1, arrange_low/2,
    highest/1, lowest/1, kickers/3, freq/2
  ]).

-include("poker.hrl").

-define(NAMES, [
    {"A", "ace"},   {"K", "king"},  {"Q", "queen"}, {"J", "jack"},  {"T", "ten"},
    {"9", "nine"},  {"8", "eight"}, {"7", "seven"}, {"6", "six"},   {"5", "five"},
    {"4", "four"},  {"3", "three"}, {"2", "deuce"}
  ]).

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

kind_to_string(Kind) -> {_, Name} = lists:keyfind(Kind, 1, ?NAMES), Name.

%% new card
new(Kind, Suit) ->
  #card{kind = Kind, suit = Suit}.
new(N) when is_integer(N), N > 0, N < 53 ->
  new(lists:nth(N rem erlang:length(?KINDS), ?KINDS), lists:nth(N div erlang:length(?KINDS) + 1, ?SUITS));
new(String) ->
  [Kind, Suit] = re:split(String, "", [{return, list}, {parts, 2}]), new(Kind, Suit).

%% build cards from binary packed value
parse(Binary) when is_binary(Binary) ->
  Bytes = binary:bin_to_list(Binary),
  [new(Byte) || Byte <- Bytes];
%% build cards from string values like "KhJd8s9s2s"
parse(String) ->
  {_, Result} = re:run(String, "([akqjt2-9]{1})([shdc]{1})", [global, caseless, {capture, [1, 2], list}]),
  lists:map(fun([Kind, Suit]) -> new(Kind, Suit) end, Result).
wrap(String) ->
  parse(String).

%%
to_string(Card) when is_record(Card, card) ->
  io_lib:format("~s~ts", [Card#card.kind, suit_to_char(Card#card.suit)]);
to_string(Card) when is_record(Card, card_group) ->
  io_lib:format("~s{~ts}", [Card#card_group.kind, string:join(lists:map(fun(C) -> suit_to_char(C#card.suit) end, Card#card_group.value), "")]);
to_string(Cards) when is_list(Cards) ->
  string:join(lists:map(fun(Card) -> to_string(Card) end, Cards), " ").

%%
to_integer(Card) ->
  index(Card#card.kind) + erlang:length(?KINDS) * suit_index(Card#card.suit).
pack(Cards) ->
  binary:list_to_binary([to_integer(Card) || Card <- Cards]).

%%
index(N, _) when is_integer(N) ->
  N rem erlang:length(?KINDS);
index(Card, Low) when is_record(Card, card) ->
  index(Card#card.kind, Low);
index(Card, Low) when is_record(Card, card_group) ->
  index(Card#card_group.kind, Low);
index(Card, Low) ->
  Kinds = if
    Low -> [Ace | Tail] = ?KINDS, Tail ++ [Ace];
    true -> ?KINDS
  end,
  {Map, _} = lists:mapfoldl(fun(X, I) -> {{X, I}, I + 1} end, 1, Kinds),
  {_, Index} = lists:keyfind(Card, 1, Map),
  Index.
index(Card) ->
  index(Card, false).

%%
compare_(A, B) ->
  index(A) =< index(B).
compare_low(A, B) ->
  index(B, true) =< index(A, true).

%%
diff(A, B) ->
  erlang:abs(index(A) - index(B)).

%% from A to 2
arrange(Cards) ->
  lists:sort(fun compare_/2, Cards).
arrange(Cards, N) when is_integer(N) ->
  lists:sublist(arrange(Cards), N).
%%
arrange_low(Cards) ->
  lists:sort(fun compare_low/2, Cards).
arrange_low(Cards, N) when is_integer(N) ->
  lists:sublist(arrange_low(Cards), N).

%%
highest(Cards) ->
  [Highest | _] = lists:sort(fun compare_/2, Cards), Highest.
lowest(Cards) ->
  [Lowest | _] = lists:sort(fun compare_low/2, Cards), Lowest.
%%
kickers(Cards, Except, N) when is_integer(N) ->
  Kinds = lists:map(fun(X) -> X#card.kind end, Except),
  Kickers = lists:filter(fun(X) -> not lists:member(X#card.kind, Kinds) end, Cards),
  arrange(Kickers, N).

%% group cards
group(_, [], Buf, F) when is_function(F) ->
  [Buf];
group(Prev, [Next|Tail], Buf, F) when is_function(F) ->
  case F(Prev, Next) of
    true -> group(Next, Tail, Buf ++ [Next], F);
    _Else -> [Buf] ++ group(Next, Tail, [Next], F) 
  end.
group(F, [H|T]) when is_function(F) ->
  group(H, T, [H], F).

%% split cards into rows
%% e.q.
%%  Ah4sAd2cQdQhTc8cJsKs3c
%% split result:
%%  Group1: [A{h,d}, Ks, Q{d,h}, Js, Tc] Group2: [8c] Group3: [2c, 3c, 4d]
split_rows(Cards) ->
  Grouped = group(fun(Prev, Next) -> diff(Prev, Next) < 2 end, arrange(Cards)),
  lists:map(fun(G) -> [First | _] = G, #card_group{kind = First#card.kind, value = G} end, Grouped).

%% cards with same kind
group_suits(Cards) ->
  Sorted = lists:keysort(1, lists:map(fun(C) -> {suit_index(C#card.suit), C} end, Cards)),
  Grouped = group(fun(Prev, Next) -> Prev#card.suit == Next#card.suit end, lists:map(fun({_, C}) -> C end, Sorted)),
  lists:map(fun(G) -> [First | _] = G, #card_group{suit = First#card.suit, value = G} end, Grouped).

%% cards with same suit
group_kinds(Cards) ->
  Grouped = group(fun(Prev, Next) -> Prev#card.kind == Next#card.kind end, arrange(Cards)),
  lists:map(fun(G) -> [First | _] = G, #card_group{kind = First#card.kind, value = G} end, Grouped).

%%
freq(Cards, Num) ->
  [Repeat || Repeat <- group_kinds(Cards), erlang:length(Repeat#card_group.value) == Num].

%% all cards
all() ->
  [new(Kind, Suit) || Kind <- ?KINDS, Suit <- ?SUITS].

%%
shuffle(Cards) ->
  random:seed(erlang:now()),
  lists:sort(fun(_, _) -> random:uniform() =< 0.5 end, Cards).

%%
deck() ->
  shuffle(all()).

%%
test_suit() ->
  io:format("suits: ~p~n", [?SUITS]),
  io:format("spade: ~ts~n", [suit_to_char("s")]).

%%
test_kind() ->
  io:format("all kinds: ~p~n", [?KINDS]).

%%
test() ->
  test_kind(), test_suit(),
  io:format("parsed from string: "),
  [io:format("~ts ", [to_string(Card)]) || Card <- wrap("AhJd")],
  io:format("~ncard indexes: "),
  [io:format("~s(~p) ", [Kind, index(Kind)]) || Kind <- ?KINDS],
  io:format("~nsorted cards: "),
  [io:format("~ts ", [to_string(Card)]) || Card <- arrange(parse("Ah7dJcTsKs"))],
  io:format("~n"),
  io:format("card integer of Kd: ~p; card of 28: ~ts~n", [to_integer(new("K", "d")), to_string(new(28))]),
  io:format("~n"),
  io:format("rows: ~p~n", [split_rows(parse("Ah4sAd2cQdQhTc8cJsKs3c"))]),
  io:format("suits: ~p~n", [group_suits(parse("Ah4sAd2cQdQhTc8cJsKs3c"))]),
  io:format("kinds: ~p~n", [group_kinds(parse("Ah4sAd2cQdQhTc8cJsKs3c"))]).
