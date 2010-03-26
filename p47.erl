-module(p47).

-export([answer/0]).

%% The first two consecutive numbers to have two distinct prime factors are:
%%   14 = 2 × 7
%%   15 = 3 × 5
%% The first three consecutive numbers to have three distinct prime factors are:
%%   644 = 2² × 7 × 23
%%   645 = 3 × 5 × 43
%%   646 = 2 × 17 × 19.
%% Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?

-compile(export_all).

answer() ->
    prime_server:start_link(200000),
    Ans = find(2*3*5*7, 4, 0),
    prime_server:stop(),
    Ans.

find(X, Cnt, Cnt) -> X-Cnt;
find(X, Cnt, Fnd) ->
    case prime_server:count_factors(X) =:= Cnt of
        true ->
            find(X+1, Cnt, Fnd+1);
        false ->
            find(X+1, Cnt, 0)
    end.
