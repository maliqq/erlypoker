-module(game).
-export([new/2, mix/1, stages/1, globals/0, defaults/1, options/1]).

-include("game.hrl").
-include("game_settings.erl").

%% new_game(?TEXAS, ?NO_LIMIT).
new(Type, Limit) -> #game{type = Type, limit = Limit}.
