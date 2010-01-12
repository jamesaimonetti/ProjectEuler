-module(my_math).

-export([gcd/1, gcd/2,
         lcm/1, lcm/2,
         perms/2,
         fac/1,
         fib/1,
         fib_generator/0,
         d/1,
         proper_divisors/1]).

-define(GOLD_RATIO, (1 + math:sqrt(5)) / 2).

%% Euler's Algorithm
gcd(A, 0) -> A;
gcd(A, B) when A < 0 orelse B < 0 -> gcd(abs(A), abs(B));
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, B) -> gcd(B, A - B * (A div B)).

%% gcd(A, B, C) = gcd( gcd(a,b), c)
gcd([A, B | L]) ->
    lists:foldl(fun(X, Acc) -> gcd(X, Acc) end, gcd(A, B), L).

lcm(A, B) ->
    A * B div gcd(A, B).
%% lcm(A, B, C) = lcm( lcm(a,b), c)
lcm([A, B | L]) ->
    lists:foldl(fun(X, Acc) -> lcm(X, Acc) end, lcm(A, B), L).

%% the number of permutations of a set of N in a groups of R
perms(N, R) ->
    fac(N) div fac(N-R).

fac(X) ->
    fac(X, 1).

fac(0, F) -> F;
fac(1, F) -> F;
fac(X, F) -> fac(X-1, F*X).

%% restricted divisor function
d(N) -> lists:sum(proper_divisors(N)).

proper_divisors(N) -> [ X || X <- lists:seq(1, N-1), N rem X =:= 0].

%% returns the Nth fibonacci number where
%% {F1, 1}, {F2, 1}, {F3, 2}, {F4, 3},...
fib(Nth) ->
    round((math:pow(?GOLD_RATIO, Nth) - math:pow(( 1 - ?GOLD_RATIO ), Nth)) / math:sqrt(5)).

fib_generator() ->
    fib_generator(0, 1, 1).

fib_generator(A, B, N) ->
    [{N,B} | fun() -> fib_generator(B, A+B, N+1) end ].
