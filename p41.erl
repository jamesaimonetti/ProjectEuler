-module(p41).

-export([answer/0]).

%% We shall say that an n-digit number is pandigital if it makes use of all the digits
%% 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
%% What is the largest n-digit pandigital prime that exists?

-compile(export_all).

answer() -> find_big(9).

find_big(N) -> find_big(N, variations(N)).

find_big(N, []) -> find_big(N-1, variations(N-1));
find_big(N, [H|T]) ->
    case is_pandigital(H) of
        true -> list_to_integer(H);
        false -> find_big(N, T)
    end.

is_pandigital(N) ->
    lists:all(fun(D) -> lists:member(D+$0, N) end, lists:seq(1, length(N)))
        andalso primes:is_prime(list_to_integer(N)).

variations(N) ->
    lists:map(fun(L) -> lists:map(fun(Ns) -> Ns + $0 end, L) end, my_math:perms(lists:seq(N,1,-1))).
