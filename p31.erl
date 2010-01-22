-module(p31).

-export([answer/0]).

%% In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
%% 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
%% It is possible to make £2 in the following way:
%%  1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
%% How many different ways can £2 be made using any number of coins?

-compile(export_all).

answer() ->
    Currencies = [1, 2, 5, 10, 20, 50, 100, 200],
    combos(Currencies, 200).

combos(_, 0) -> 1;
combos([], _) -> 0;
combos(_, N) when N < 0 -> 0;
combos([C|Cs], N) ->
    combos(Cs, N) + combos([C|Cs], N-C).
