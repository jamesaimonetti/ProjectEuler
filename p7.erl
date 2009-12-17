-module(p7).

-export([answer/0]).

%% By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6^(th) prime is 13.
%% What is the 10001st prime number?

answer() ->
    primes:nth(10001).
