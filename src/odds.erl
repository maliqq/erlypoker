-module(odds).

%% some math
fact(1) ->
  1;
fact(N) ->
  N * fact(N - 1).

perm(N, M) ->
  fact(N) / fact(N - M).

comb(N, M) ->
  fact(N) / (fact(M) * fact(N - M)).

comb_fast(N, M) ->
  gosper(N) / (gosper(M) * gosper(N - M)).

gosper(N) ->
  erlang:round(
    math:sqrt((2 * N + 1/3) * math:pi()) *
    math:pow(N / math:exp(1), N)
  ).
