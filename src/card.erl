-module(card).

-export([
    to_string/1, wrap/1, deck/0,
    split_rows/1, group_kinds/1, group_suits/1, all/0, shuffle/1, pack/1, suit_to_string/1, kind_to_string/1,
    arrange/1, arrange/2, arrange_low/1, arrange_low/2,
    highest/1, lowest/1, kickers/3, freq/2
  ]).

-include("poker.hrl").
-include("card.hrl").
-include_lib("eunit/include/eunit.hrl").

%% suit is number 0..3
suit_to_utf8(Suit) when is_integer(Suit) ->
  lists:nth(Suit + 1, ?suit_utf8());
suit_to_utf8(Suit) ->
  suit_to_utf8(suit_index(Suit)).

suit_to_char(Suit) when is_integer(Suit) ->
  string:substr(?suit_chars(), Suit + 1, 1);
suit_to_char(Suit) ->
  Suit.

suit_to_string(Suit) when is_integer(Suit) ->
  lists:nth(Suit + 1, ?suit_names());
suit_to_string(Suit) ->
  suit_to_string(suit_index(Suit)).

suit_index(Suit) ->
  string:str(?suit_chars(), Suit) - 1.

%% kind is number 0..12
kind_to_char(Kind) when is_integer(Kind) ->
  string:substr(?kind_chars(), Kind + 1, 1);
kind_to_char(Kind) ->
  Kind.

kind_to_string(Kind) when is_integer(Kind) ->
  lists:nth(Kind + 1, ?kind_names());
kind_to_string(Kind) ->
  kind_to_string(kind_index(Kind)).

kind_index(Kind, Low) ->
  Index = kind_index(kind_to_char(Kind)),
  if
    Low ->
      if
        Kind == ?ACE -> 0;
        true -> Index + 1
      end;
    true -> Index
  end.

kind_index(Kind) ->
  string:str(?kind_chars(), Kind) - 1.

%% card is tuple of kind and suit
new(Kind, Suit) when is_integer(Kind), is_integer(Suit) ->
  #card{kind = Kind, suit = Suit};
new(Kind, Suit) ->
  new(kind_index(Kind), suit_index(Suit)).
new(N) when is_integer(N) ->
  new(?kind(N), ?suit(N));
new(String) ->
  [Kind, Suit] = re:split(String, "", [{return, list}, {parts, 2}]),
  new(Kind, Suit).

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
to_string(Kind, Suit) ->
  io_lib:format("~s~ts", [
    kind_to_char(Kind),
    suit_to_char(Suit)
  ]);
to_string(Card) when is_integer(Card) ->
  to_string(?kind(Card), ?suit(Card))
to_string(Card) when is_record(Card, card) ->
  to_string(Card#card.kind, Card#card.suit);
to_string(Card) when is_record(Card, card_group) ->
  io_lib:format("~s{~ts}", [
    kind_to_char(Card#card_group.kind),
    string:join(lists:map(fun(C) -> suit_to_char(C#card.suit) end, Card#card_group.value), "")
  ]);
to_string(Cards) when is_list(Cards) ->
  string:join(lists:map(fun(Card) -> to_string(Card) end, Cards), " ").

%%
to_integer(Card) when is_integer(Card) ->
  Card;
to_integer(Card) when is_record(Card, card) ->
  ?card(Card#card.kind, Card#card.suit).
pack(Cards) ->
  binary:list_to_binary([to_integer(Card) || Card <- Cards]).

%%
index(Card, Low) when is_integer(Card) ->
  kind_index(?kind(Card), Low);
index(Card, Low) when is_record(Card, card) ->
  kind_index(Card#card.kind, Low);
index(Card, Low) when is_record(Card, card_group) ->
  kind_index(Card#card_group.kind, Low).
index(Card) ->
  index(Card, false).

%%
compare(A, B) ->
  index(A) =< index(B).
compare_low(A, B) ->
  index(B, true) =< index(A, true).

%%
diff(A, B) ->
  erlang:abs(index(A) - index(B)).

%% from A to 2
arrange(Cards) ->
  lists:sort(fun compare/2, Cards).
arrange(Cards, N) when is_integer(N) ->
  lists:sublist(arrange(Cards), N).
%%
arrange_low(Cards) ->
  lists:sort(fun compare_low/2, Cards).
arrange_low(Cards, N) when is_integer(N) ->
  lists:sublist(arrange_low(Cards), N).

%%
highest(Cards) ->
  [Highest | _] = lists:sort(fun compare/2, Cards), Highest.
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
  Aces = lists:filter(fun(C) -> C#card.kind == ?ACE end, Cards),
  Grouped = group(fun(Prev, Next) -> (diff(Prev, Next) < 2) or (diff(Prev, Next) >= 12) end, arrange(Cards) ++ Aces),
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
  randomize(round(math:log(length(Cards)) + 0.5), Cards).

randomize(1, List) ->
  randomize(List);
randomize(T, List) ->
  lists:foldl(fun(_, Acc) ->
    randomize(Acc)
  end, randomize(List), lists:seq(1, (T - 1))).
randomize(List) ->
   lists:sort(fun(_, _) ->
      secure:random() =< 0.5
   end, List).

%%
deck() ->
  shuffle(all()).

%%
suit_test() ->
  ?assertEqual("d", suit_to_char(2)),
  ?assertEqual("d", suit_to_char("d")),
  ?assertEqual("spade", suit_to_string(0)),
  ?assertEqual("spade", suit_to_string("s"))
  .
%%
kind_test() ->
  ?assertEqual("K", kind_to_char(11)),
  ?assertEqual("K", kind_to_char("K")),
  ?assertEqual("queen", kind_to_string(10)),
  ?assertEqual("queen", kind_to_string("Q"))
  .
%%
card_test() ->
  ?assertEqual(0, index(new("A", "d"), true)),
  ?assertEqual(?ACE, index(new("A", "h")))
  .

run() ->
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
