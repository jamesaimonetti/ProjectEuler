-module(p37).

-export([answer/0]).

%% The number 3797 has an interesting property. Being prime itself, it is possible to continuously
%% remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly
%% we can work from right to left: 3797, 379, 37, and 3.
%% Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
%% NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

-compile(export_all).

answer() -> find_p(primes:lazy_sieve(), [], []).

find_p([{P, _Pos} | Ps], PrimesSoFar, Truncatable) when P < 10 -> find_p(Ps(), [P | PrimesSoFar], Truncatable);
find_p(_, _, Truncatable) when length(Truncatable) =:= 11 -> lists:sum(Truncatable);
find_p([{P, _Pos} | Ps], PrimesSoFar, Truncatable) ->
    PSF = [P | PrimesSoFar],
    IsT = is_trunc(P, PSF),
    case IsT of
        true -> find_p(Ps(), PSF, [P | Truncatable]);
        false ->find_p(Ps(), PSF, Truncatable)
    end.

is_trunc(P, PSF) ->
    Ps = lists:merge(rtrunc(integer_to_list(P)), ltrunc(integer_to_list(P))),
    Primes = lists:filter(fun(X) -> lists:member(list_to_integer(X), PSF) end, Ps),
    length(Ps) =:= length(Primes).

ltrunc(Ns) -> ltrunc(Ns, [Ns]).
ltrunc(Ns, Ls) when length(Ns) == 1 -> lists:reverse(Ls);
ltrunc([_H|T], Ls) -> ltrunc(T, [ T | Ls]).

rtrunc(Ns) -> lists:map(fun lists:reverse/1, ltrunc(lists:reverse(Ns))).
