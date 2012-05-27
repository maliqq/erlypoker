-record(prize_pool, {
    amount,
    rebuys,
    addons,
    places_paid,
    structure,
    results
  }).

-record(battle, {
    game,
    type,
    state,    %% registering, canceled, paused, active, bubble
    
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
    rebuy_period, %% 4 levels
    level_period, %% 15 min
    break_period,
    late_reg_period,

    levels,

    tables,
    players
  }).
