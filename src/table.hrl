-record(prize_pool, {
    amount,
    rebuys,
    addons,
    places_paid,
    payout_structure,
    results
  }).

-record(battle, {
    game,
    type,
    state,    %% registering, canceled, paused, running
    buy_in,   %% {100.0, 10000}
    rake,     %% 9.0
    rebuy,    %% {100.0, 10000}
    rebuy_limit,
    addon,    %% {100.0, 50000}
    bounty,   %% 0.0
    
    prize_pool,

    max,

    start,
    finish,
    level_period, %% 15 min
    rebuy_levels, %% 4 levels
    break_period, %% 5 min
    late_reg_period, %% 8 levels

    levels,
    tables,
    players
  }).
