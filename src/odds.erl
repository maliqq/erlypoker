-module(odds).

-include_lib("eunit/include/eunit.hrl").

%% some math
fact(1) ->
  1;
fact(N) ->
  N * fact(N - 1).

perm(N, M) ->
  erlang:round(fact(N) / fact(N - M)).

comb(N, M) ->
  erlang:round(fact(N) / (fact(M) * fact(N - M))).

comb_fast(N, M) ->
  erlang:round(gosper(N) / (gosper(M) * gosper(N - M))).

gosper(N) ->
  erlang:round(
    math:sqrt((2 * N + 1/3) * math:pi()) *
    math:pow(N / math:exp(1), N)
  ).

odds_test() ->
  ?assertEqual(3628800, fact(10)),
  ?assertEqual(24, perm(4, 3)),
  ?assertEqual(1326, comb(52, 2)),
  ?assertEqual(1326, comb_fast(52, 2)),
  ?assertEqual(3629, erlang:round(gosper(10) / 1000)). %% accuracy
