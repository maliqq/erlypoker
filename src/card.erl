-module(card).

-export([
    new/1, new/2, parse/1, wrap/1, to_string/1, to_binary/1, to_tuple/1,
    compare/2, compare_low/2, diff/2, diff/3, arrange/1, arrange/2, arrange_groups/1, arrange_groups/2,
    highest/1, lowest/1, kickers/2, kickers/3,
    group/2, split_rows/1, group_suits/1, group_kinds/1,
    shuffle/1, deck/0,
    main/1
  ]).

-include("poker.hrl").
-include("card.hrl").
-include_lib("eunit/include/eunit.hrl").

%% suit
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

suit_index(Suit) when is_integer(Suit) ->
  Suit;
suit_index(Suit) ->
  string:str(?suit_chars(), Suit) - 1.

%% kind
kind_to_char(Kind) when is_integer(Kind) ->
  string:substr(?kind_chars(), Kind + 1, 1);
kind_to_char(Kind) ->
  Kind.

kind_to_string(Kind) when is_integer(Kind) ->
  lists:nth(Kind + 1, ?kind_names());
kind_to_string(Kind) ->
  kind_to_string(kind_index(Kind)).

kind_index(low, Kind) ->
  Index = kind_index(Kind),
  if
    Kind == ?ACE -> 0;
    true -> Index + 1
  end.

kind_index(Kind) when is_integer(Kind) ->
  Kind;
kind_index(Kind) ->
  string:str(?kind_chars(), Kind) - 1.

%% card

new(Card) ->
  wrap(Card).
new(Kind, Suit) when is_integer(Kind), is_integer(Suit) ->
  ?card(Kind, Suit);
new(Kind, Suit) ->
  new(kind_index(Kind), suit_index(Suit)).

%%
parse(Binary) when is_binary(Binary) ->
  [wrap(Byte) || Byte <- binary:bin_to_list(Binary)];
parse(String) ->
  {_, Result} = re:run(String, "([akqjt2-9]{1})([shdc]{1})", [global, caseless, {capture, [1, 2], list}]),
  lists:map(fun([Kind, Suit]) -> new(Kind, Suit) end, Result).

%%
wrap(Card) when is_integer(Card) ->
  Card;
wrap(Card) when is_tuple(Card) ->
  {Kind, Suit} = Card,
  new(Kind, Suit);
wrap(String) ->
  [Kind, Suit] = re:split(String, "", [{return, list}, {parts, 2}]),
  new(Kind, Suit).

%%
to_string(Kind, Suit) ->
  lists:flatten(io_lib:format("~s~ts", [kind_to_char(Kind), suit_to_utf8(Suit)])).
to_string(Card) when is_integer(Card) ->
  to_string(?kind(Card), ?suit(Card));
to_string(Cards) when is_list(Cards) ->
  string:join(lists:map(fun(Card) -> to_string(Card) end, Cards), "").

%%
to_binary(Cards) ->
  binary:list_to_bin([wrap(Card) || Card <- Cards]).

%%
to_tuple(Card) when is_integer(Card) ->
  {?kind(Card), ?suit(Card)}.

%%
index(low, Card) ->
  kind_index(low, ?kind(wrap(Card))).
index(Card) ->
  index(Card, false).

%%
compare(A, B) ->
  index(A) =< index(B).
compare_low(A, B) ->
  index(low, B) =< index(low, A).

%%
diff(A, B) ->
  erlang:abs(index(A) - index(B)).
diff(low, A, B) ->
  erlang:abs(index(low, A) - index(low, B)).

%%
arrange(Cards) ->
  lists:sort(fun compare/2, Cards).
arrange(low, Cards) ->
  lists:sort(fun compare_low/2, Cards).
arrange_groups(Groups) ->
  Cards = lists:map(fun(Group) -> hd(Group) end, Groups),
  arrange(Cards).
arrange_groups(low, Groups) ->
  Cards = lists:map(fun(Group) -> hd(Group) end, Groups),
  arrange(low, Cards).
%%
highest(Cards) ->
  hd(arrange(Cards)).
lowest(Cards) ->
  hd(arrange(low, Cards)).

%%
kickers(Cards, Except) ->
  arrange(Cards -- Except).
kickers(Cards, Except, N) when is_integer(N) ->
  lists:sublist(kickers(Cards, Except), 1, N).

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

%%
split_rows(Cards) ->
  Aces = lists:filter(fun(Card) ->
    Card == ?ACE
  end, Cards),
  
  group(fun(Prev, Next) ->
    (diff(Prev, Next) < 2) or (diff(Prev, Next) >= 12)
  end, arrange(Cards) ++ Aces).

%%
group_suits(Cards) ->
  Sorted = lists:keysort(1, lists:map(fun(Card) ->
    {?suit(wrap(Card)), Card}
  end, Cards)),

  group(fun(Prev, Next) ->
    ?suit(wrap(Prev)) == ?suit(wrap(Next))
  end, lists:map(fun({_, Card}) -> Card end, Sorted)).

%%
group_kinds(Cards) ->
  group(fun(Prev, Next) ->
    ?kind(wrap(Prev)) == ?kind(wrap(Next))
  end, arrange(Cards)).

%%
shuffle(Cards) ->
  randomize(round(math:log(length(Cards)) + 0.5), Cards).

randomize(1, List) ->
  randomize(List);
randomize(N, List) ->
  lists:foldl(fun(_, L) ->
    randomize(L)
  end, randomize(List), lists:seq(1, N - 1)).
randomize(List) ->
   lists:sort(fun(_, _) ->
      secure:random() =< 0.5
   end, List).

%%
deck() ->
  shuffle(?cards()).

%%
suit_test() ->
  ?assertEqual(3, suit_index(3)),
  ?assertEqual(3, suit_index("c")),
  ?assertEqual("d", suit_to_char(2)),
  ?assertEqual("d", suit_to_char("d")),
  ?assertEqual("spade", suit_to_string(0)),
  ?assertEqual("spade", suit_to_string("s")),
  ?assertEqual(<<"♥"/utf8>>, suit_to_utf8(1)),
  ?assertEqual(<<"♥"/utf8>>, suit_to_utf8("h"))
  .
%%
kind_test() ->
  ?assertEqual("K", kind_to_char(11)),
  ?assertEqual("K", kind_to_char("K")),
  ?assertEqual("queen", kind_to_string(10)),
  ?assertEqual("queen", kind_to_string("Q")),
  ?assertEqual(?ACE, kind_index(12)),
  ?assertEqual(?ACE_LOW, kind_index(low, 12)),
  ?assertEqual(11, kind_index("K")),
  ?assertEqual(12, kind_index(low, "K"))
  .
%%
card_test() ->
  ?assertEqual(50, new("A", "d")),
  ?assertEqual([new("Ad"), new("Kh")], parse("AdKh")),
  ?assertEqual(50, wrap("Ad")),
  ?assertEqual(50, wrap({"A", "d"})),
  ?assertEqual(50, wrap(new("A", "d"))),
  ?assertEqual("Ad", to_string(50)),
  ?assertEqual({12, 2}, to_tuple(50)),
  ?assertEqual(0, index(low, new("A", "d"))),
  ?assertEqual(?ACE, index(new("A", "h")))
  .

main(_) ->
  random:seed(erlang:now()),
  io:format("~s~n", [to_string(deck())]),
  io:format("~ts~n", [to_binary(?cards())])
  .
