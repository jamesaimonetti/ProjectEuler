-module(p3).

-export([answer/0]).

%% The prime factors of 13195 are 5, 7, 13 and 29.
%% What is the largest prime factor of the number 600851475143?

answer() ->
    N = 600851475143,
    lists:max(lists:filter(fun(X) -> N rem X =:= 0 end, primes:queue(round(math:sqrt(N))))).
