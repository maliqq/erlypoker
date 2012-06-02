-module(game).
-export([new/2, mix/1, stages/1, globals/0, defaults/1, options/1]).

-include("game.hrl").
-include("game/settings.erl").
-include("game/stages.erl").

%%
new(Type, Limit) ->
  #game{type = Type, limit = Limit}.

to_string(Game) when is_record(Gane, game) ->
  io_lib:format("", []).

game_test() ->
  ok.
