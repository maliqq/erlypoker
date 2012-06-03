-module(game).
-export([new/2, mix/1, stages/1, defaults/1, options/1]).

-include_lib("eunit/include/eunit.hrl").
-include("table.hrl").
-include("game.hrl").

%%
new(Type, Limit) ->
  #game{type = Type, limit = Limit}.

to_string(Game) when is_record(Game, game) ->
  io_lib:format("", []).

game_test() ->
  ok.
