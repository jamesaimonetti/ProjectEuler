-module(p97).

-export([answer/0]).

%% The first known prime found to exceed one million digits was discovered in 1999,
%% and is a Mersenne prime of the form 2^(6972593)−1; it contains exactly 2,098,960 digits.
%% Subsequently other Mersenne primes, of the form 2^(p)−1, have been found which contain more digits.
%% However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: 28433×2^(7830457)+1.
%% Find the last ten digits of this prime number.

-compile(export_all).

answer() ->
    D = last_n_digits(2, 7830457, 10),
    A = integer_to_list(D * 28433 + 1),
    list_to_integer(lists:nthtail(length(A)-10, A)).

last_n_digits(X, Pow, Len) ->
    lists:foldl(fun(_P, Acc) ->
                        V = integer_to_list(Acc * X),
                        Tail = case length(V) > Len of
                                   true -> lists:nthtail(length(V)-Len, V);
                                   false -> V
                               end,
                        list_to_integer(Tail)
                end, 1, lists:seq(1, Pow)).
