%% forced bets
-define(ANTE, 1).
-define(BRING_IN, 2).
-define(SMALL_BLIND, 3).
-define(BIG_BLIND, 4).
-define(GUEST_BLIND, 5).

-define(FORCED_BET, ?ANTE bor ?BRING_IN bor ?SMALL_BLIND bor ?BIG_BLIND bor ?GUEST_BLIND).

%% action bets
-define(?FOLD, 6).
-define(?CHECK, 7).
-define(?CALL, 8).
-define(?RAISE, 9).
-define(?PUSH, 10).

bet(Context, Type) when Type band ?FORCED_BET == ?FORCED_BET ->
  Stage = table:current_stage(Context#context.table),
  Stage == forced_bets andalso {Context, #bet{amount = table:bet_size(Context#context.table, Type), call = true};

bet(Context, Type) when Type == ?FOLD ->
  {Context, #bet{amount = 0, fold = true};

bet(Context, Type) when Type == ?CHECK ->
  {_, Bet} = table:last_bet(Context#context.table),
  if
    Bet#bet.amount > 0 ->
      {Context, #bet{amount = 0, fold = true}}; %% fold this
    true ->
      {Context, #bet{amount = 0}}
  end;

bet(Context, Type) when Type == ?CALL ->
  {_, Bet} = table:last_bet(Context#context.table),
  PlayerStack = player:stack(Context#context.player),
  if
    Bet#bet.amount == 0 ->
      {Context, #bet{amount = 0}}; %% just check
    PlayerStack > Bet#bet.amount ->
      {Context, #bet{amount = Bet#bet.amount, call = true}}; %% just call
    true ->
      {Context, #bet{amount = PlayerStack, call = true, all_in = true}} %% call and go all-in
  end;

bet(Context, Type) when Type == ?RAISE ->
  {Context, #bet{amount = Context#context.bet}};

bet(Context, Type) when Type == ?PUSH ->
  {Context, #bet{amount = Context#context.bet, all_in = true}}.
