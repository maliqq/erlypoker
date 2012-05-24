%%
-module(bot).

%% style
-define(LOOSE, 1).
-define(TIGHT, 2).
-define(AGGRESSIVE, 3).
-define(PASSIVE, 4).

%% position
-define(EARLY, 1). %% UTG, UTG+1
-define(MIDDLE, 2). %% MP1, MP2, MP3
-define(LATE, 3). %% CO, BU
-define(BLINDS, 4). %% SB, BB

%% starting hand classification
-define(TOP_PAIR, 1).         %% AA KK QQ JJ TT
-define(MIDDLE_PAIR, 2).      %% 99 88 77 66
-define(LOW_PAIR, 3).         %% 55 44 33 22

-define(TOP_ACE, 4).          %% AK AQ AJ AT
-define(MIDDLE_ACE, 5).       %% A6 A6 A8 A9
-define(LOW_ACE, 6).          %% A2 A3 A4 A5

-define(TOP_CONNECTOR, 7).    %% KQ KJ QJ KT QT JT
-define(MIDDLE_CONNECTOR, 8). %% T9 98 87 76
-define(LOW_CONNECTOR, 9).    %% 65 54 43 32

-define(GAP_CONNECTOR, 10).

-define(ANY_PAIR, ?TOP_PAIR bor ?MIDDLE_PAIR bor ?LOW_PAIR).
-define(ANY_ACE, ?TOP_ACE bor ?MIDDLE_ACE bor ?LOW_ACE).

-define(ANY_KING, 11).
-define(ANY_QUEEN, 12).
-define(ANY_JACK, 13).
-define(ANY_SUITED, 14).

range(Style, Position) when Style band ?LOOSE == ?LOOSE ->
  case Position of
    ?EARLY -> ?TOP_PAIR bor ?MIDDLE_PAIR bor ?TOP_ACE bor ?MIDDLE_ACE;
    ?MIDDLE -> ?ANY_PAIR bor ?ANY_ACE bor ?ANY_CONNECTOR;
    ?LATE -> ?ANY_PAIR bor ?ANY_ACE bor ?ANY_CONNECTOR bor ?ANY_KING bor ?ANY_SUITED
  end


%% 1. preflop strategy - ranges, position, N-bets
%%   1.1. early position
%%   1.2. middle position
%%   1.3. late position
%%   1.4. on blinds, blind vs blind
%% 2. flop strategy - heads-up, multipots
%%   2.1. out of position - contbets, donkbets, check/raises
%%   2.2. in position - reraises
%% 3. turn strategy - check-check, second barrel
%% 4. river strategy - value/bluff bets
%% 5. stats
%% 6. stop-and-go, check-call, check-fold
%%
%%
%%
%%
%%