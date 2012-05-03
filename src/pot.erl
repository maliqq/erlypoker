%% pot manipulations - append, split

-module(pot).
-export([test/0, new/0, erase/2, append/3]).

-record(side_pot, {
    amount = 0,
    cap = 0,
    members
  }).
-record(pot, {
    amount = 0, %% amount in the pot
    main,
    side = [] %% side pots, last is main pot
  }).

%% blank pot
new_side_pot() -> #side_pot{members = orddict:new()}.
new() -> #pot{main = new_side_pot()}.

to_string(Pot) when is_record(Pot, side_pot) ->
  io_lib:format("(total = ~p cap=~p members = ~p)", [total(Pot), Pot#side_pot.cap, Pot#side_pot.members]);
to_string(Pot) when is_record(Pot, pot) ->
  io_lib:format("Total: ~p~n\tCurrent Pot: ~ts~n\tSide pots: ~ts~n", [total(Pot), to_string(Pot#pot.main),
    string:join(lists:map(fun(P) -> to_string(P) end, Pot#pot.side), "")
    ]).

is_active(Pot) when is_record(Pot, side_pot) ->
  (orddict:size(Pot#side_pot.members) > 0) and (total(Pot) > 0).

release(Pot) when is_record(Pot, side_pot) ->
  Pot#side_pot{amount = total(Pot)}.

total(Pot) when is_record(Pot, side_pot) ->
  orddict:fold(fun(_, Amount, Total) -> Total + Amount end, 0, Pot#side_pot.members);

total(Pot) when is_record(Pot, pot) ->
  lists:foldl(fun(Side, Total) ->
    Total + if
      Side#side_pot.amount > 0 ->
        Side#side_pot.amount;
      true ->
        total(Side)
    end
  end, 0, side_pots(Pot)).

side_pots(Pot) when is_record(Pot, pot) ->
  [Pot#pot.main] ++ Pot#pot.side.

erase(Pot, Member) -> ok.

append(P, Member, Amount) when is_record(P, side_pot) ->
  Pot = ensure_member(Member, P),

  %% main bet size
  Value = orddict:fetch(Member, Pot#side_pot.members),

  %% max bet size for covering All-in member
  Cap = Pot#side_pot.cap,

  if
    Cap > Value ->
      {Pot#side_pot{members = orddict:store(Member, Cap, Pot#side_pot.members)}, Amount + Value - Cap};
    Cap > 0 ->
      {Pot, Amount};
    true -> %% just add bet to this pot
      {Pot#side_pot{members = orddict:update_counter(Member, Amount, Pot#side_pot.members)}, 0}
  end;

append(Pot, Member, Amount) when is_record(Pot, pot) ->
  append(Pot, Member, Amount, false).

ensure_member(Member, P) when is_record(P, side_pot) ->
  case orddict:is_key(Member, P#side_pot.members) of %% check member
    false ->
      P#side_pot{members = orddict:store(Member, 0, P#side_pot.members)};
    true ->
      P
  end.

split(Pot, Member) when is_record(Pot, pot) ->
  Current = Pot#pot.main,
  %%Current = C#side_pot{members = orddict:update_counter(Member, Balance, C#side_pot.members)},
  Value = orddict:fetch(Member, Current#side_pot.members),

  Members = Current#side_pot.members,
  
  NewMembers = orddict:map(fun(_, V) ->
      V - Value
    end, orddict:filter(
      fun(K, V) ->
        (K /= Member) and (V > Value)
      end, Members)),
  NewPot = Current#side_pot{members = NewMembers},

  OldMembers = orddict:map(
    fun(_, V) ->
      if
        V > Value -> Value;
        true -> V
      end
    end, Members),
  OldPot = release(Current#side_pot{members = OldMembers, cap = Value}),

  Pot#pot{main = NewPot, side = lists:append(Pot#pot.side, [OldPot])}.

append(Pot, Member, Amount, Cap) when is_record(Pot, pot) ->
  {[_|Side], Balance} = lists:mapfoldl(fun(P, B) ->
    {P1, N} = append(P, Member, Amount),
    {P1, if
      N == 0 -> B;
      true -> N
    end}
  end, Amount, side_pots(Pot)),
  
  {Appended, _} = append(Pot#pot.main, Member, Balance),
  P = Pot#pot{main = Appended, side = Side},

  if
    Cap ->
      split(P, Member);
    true ->
      P
  end.

test() ->
  Pot = new(),
  Pot1 = append(Pot, "A", 27, true),
  Pot2 = append(Pot1, "B", 100),
  Pot3 = append(Pot2, "C", 100),
  io:format(to_string(Pot3)).
