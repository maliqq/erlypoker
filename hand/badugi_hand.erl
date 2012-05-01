-record(badugi_hand, {rank, value}).

-define(BADUGI1, 0).
-define(BADUGI2, 1).
-define(BADUGI3, 2).
-define(BADUGI4, 3). %% complete badugi card

%%
is_badugi4(Cards) ->
  case has_rainbow(Cards, 4) of
    true -> #badugi_hand{rank = ?BADUGI4, value = sorted_cards(Cards)};
    _Else -> false
  end.

%% 2 cards same suit OR 2 cards same kind
%% paired cards are different suit
is_badugi3(Cards) ->
	case {has_paired(Cards, 2) and no_suited(Cards), has_suited(Cards, 2) and no_paired(Cards), has_rainbow(Cards, 3) and no_suited_paired(Cards)} of
		{true, _, _} -> true;
		{_, true, _} -> true;
		{_, _, true} -> true;
		_Else -> false
	end.

%% 3 cards same suit OR 3 cards same kind
%% 2 cards same kind AND 2 cards same suit
%% paired cards are same suit 
is_badugi2(Cards) ->
	case {has_paired(Cards, 3), has_suited(Cards, 3), has_paired(Cards, 2) and has_suited(Cards, 2), has_rainbow(Cards, 3) and is_suited_paired(Cards)} of
		{true, _, _, _} -> true;
		{_, true, _, _} -> true;
		{_, _, true, _} -> true;
		{_, _, _, true} -> true;
		_Else -> false
	end.

%% all cards same suit OR all cards same kind
is_badugi1(Cards) ->
  case has_paired(Cards, 4) or has_suited(Cards, 4) of
    true ->
    	Highest = highest_card(Cards),
    	#badugi_hand{rank = {?BADUGI1, Highest#card.kind}, value = Cards};
    _Else -> false
  end.

%%
has_paired(Cards, N) when is_integer(N) ->
	Grouped = [Group || Group <- group_kinds(Cards), erlang:length(Group#group_kind.value) == N],
	erlang:length(Grouped) == 1.

%%
has_suited(Cards, N) ->
	Grouped = [Group || Group <- group_suits(Cards), erlang:length(Group#group_suit.value) == N],
	erlang:length(Grouped) == 1.

%%
no_paired(Cards) ->
	Grouped = [Group || Group <- group_kinds(Cards), erlang:length(Group#group_kind.value) > 1],
	erlang:length(Grouped) == 0.
%%
no_suited(Cards) ->
	Grouped = [Group || Group <- group_suits(Cards), erlang:length(Group#group_suit.value) > 1],
	erlang:length(Grouped) == 0.

%% all different kind and suit
has_rainbow(Cards, N) when is_integer(N) ->
	(erlang:length(group_kinds(Cards)) == N) and (erlang:length(group_suits(Cards)) == N).
%%
no_suited_paired(Cards) -> ok.
%%
is_suited_paired(Cards) -> ok.

test_badugi() -> ok.
