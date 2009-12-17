-module(p10).

-export([answer/0]).

%% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
%% Find the sum of all the primes below two million.

answer() ->
    lists:sum(primes:queue(2000000)).
