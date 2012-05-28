%%
-module(bet).

-include("poker.hrl").
-include_lib("eunit/include/eunit.hrl").

to_string(Bet, Message) when Bet#bet.all_in =:= true ->
  lists:flatten(io_lib:format("~s and is all-in", [lists:flatten(Message)]));
to_string(_, Message) ->
  lists:flatten(Message).

to_string(Bet) when is_record(Bet, bet), Bet#bet.fold =:= true ->
  "folds";

to_string(Bet) when is_record(Bet, bet), Bet#bet.raise =:= false, Bet#bet.call =:= false ->
  "checks";

to_string(Bet) when is_record(Bet, bet), is_integer(Bet#bet.call), Bet#bet.call > 0 ->
  to_string(Bet, io_lib:format("calls ~p", [Bet#bet.call]));

to_string(Bet) when is_record(Bet, bet), is_tuple(Bet#bet.raise) ->
  to_string(Bet, io_lib:format("raises ~p to ~p", tuple_to_list(Bet#bet.raise)));

to_string(Bet) when is_record(Bet, bet), is_integer(Bet#bet.raise), Bet#bet.raise > 0 ->
  to_string(Bet, io_lib:format("bets ~p", [Bet#bet.raise])).

%% simple structure?

bet_test() ->
  ?assertEqual("checks", to_string(#bet{})),
  ?assertEqual("folds", to_string(#bet{fold = true})),
  ?assertEqual("calls 40", to_string(#bet{call = 40})),
  ?assertEqual("bets 100", to_string(#bet{raise = 100})),
  ?assertEqual("bets 100 and is all-in", to_string(#bet{raise = 100, all_in = true})),
  ?assertEqual("raises 85 to 195", to_string(#bet{raise = {85, 195}})),
  ?assertEqual("raises 85 to 195 and is all-in", to_string(#bet{raise = {85, 195}, all_in = true}))
  .
