-module(p40).

-export([answer/0]).

%% An irrational decimal fraction is created by concatenating the positive integers:
%% 0.12345678910[1]112131415161718192021...
%% It can be seen that the 12^(th) digit of the fractional part is 1.
%% If d(n) represents the n^(th) digit of the fractional part, find the value of the following expression.
%% d(1) × d(10) × d(100) × d(1000) × d(10000) × d(100000) × d(1000000)

-compile(export_all).

answer() ->
    Dec = lists:flatten([ integer_to_list(X) || X <- lists:seq(1, 333333) ]),
    Pows = lists:map(fun(X) -> round(math:pow(10, X)) end, lists:seq(0, 6)),
    Ds = lists:map(fun(P) -> lists:nth(P, Dec) - 48 end, Pows),
    lists:foldl(fun(D, Acc) -> D * Acc end, 1, Ds).
