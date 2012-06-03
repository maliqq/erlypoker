%%
-module(bet).

-include("bet.hrl").
-include_lib("eunit/include/eunit.hrl").

to_string(Bet, Data) when is_record(Bet, bet), Bet#bet.all_in =:= true ->
  lists:flatten(io_lib:format("~s (all in)", [lists:flatten(to_string(Data))]));
to_string(_, Data) ->
  lists:flatten(to_string(Data)).

to_string({?ANTE, N}) ->
  io_lib:format("posts the ante ~p", [N]);
to_string({?SMALL_BLIND, N}) ->
  io_lib:format("posts small blind ~p", [N]);
to_string({?BIG_BLIND, N}) ->
  io_lib:format("posts big blind ~p", [N]);
to_string({?GUEST_BLIND, N}) ->
  io_lib:format("posts guest blind ~p", [N]);
to_string({?BRING_IN, N}) ->
  io_lib:format("posts bring-in ~p", [N]);
to_string({?STRADDLE, N}) ->
  io_lib:format("posts straddle ~p", [N]);

to_string({?FOLD}) ->
  "folds";
to_string({?CHECK}) ->
  "checks";
to_string({?STAND_PAT}) ->
  "stands pat";
to_string({?MUCK}) ->
  "mucks hand";
to_string({?DISCARD, N}) ->
  io_lib:format("discards ~p cards", [N]);
to_string({?REBUY, Chips, Money}) ->
  io_lib:format("rebuys ~p chips for ~p", [Chips, Money]);
to_string({?DOUBLE_REBUY, Chips, Money}) ->
  io_lib:format("double rebuys ~p chips for ~p", [Chips, Money]);
to_string({?ADDON, Chips, Money}) ->
  io_lib:format("add-ons ~p chips for ~p", [Chips, Money]);

to_string({?RAISE, N}) ->
  io_lib:format("bets ~p", [N]);
to_string({?RAISE, M, N}) ->
  io_lib:format("raises ~p to ~p", [M, N]);
to_string({?CALL, N}) ->
  io_lib:format("calls ~p", [N]);

to_string(Bet) when is_record(Bet, bet), Bet#bet.fold =:= true ->
  to_string({?FOLD});

to_string(Bet) when is_record(Bet, bet), Bet#bet.raise =:= false, Bet#bet.call =:= false ->
  to_string({?CHECK});

to_string(Bet) when is_record(Bet, bet), is_integer(Bet#bet.call), Bet#bet.call > 0 ->
  to_string(Bet, {?CALL, Bet#bet.call});

to_string(Bet) when is_record(Bet, bet), is_tuple(Bet#bet.raise) ->
  {Bet, To} = Bet#bet.raise,
  to_string(Bet, {?RAISE, Bet, To});

to_string(Bet) when is_record(Bet, bet), is_integer(Bet#bet.raise), Bet#bet.raise > 0 ->
  to_string(Bet, {?RAISE, Bet#bet.raise}).

%% simple structure?

bet_test() ->
  ?assertEqual("checks", to_string(#bet{})),
  ?assertEqual("folds", to_string(#bet{fold = true})),
  ?assertEqual("calls 40", to_string(#bet{call = 40})),
  ?assertEqual("calls 199 (all in)", to_string(#bet{call = 199, all_in = true}))
  ?assertEqual("bets 100", to_string(#bet{raise = 100})),
  ?assertEqual("bets 100 (all in)", to_string(#bet{raise = 100, all_in = true})),
  ?assertEqual("raises 85 to 195", to_string(#bet{raise = {85, 195}})),
  ?assertEqual("raises 85 to 195 (all in)", to_string(#bet{raise = {85, 195}, all_in = true}))
  .
