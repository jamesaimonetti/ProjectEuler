-module(my_math).

-export([gcd/1, gcd/2, lcm/1, lcm/2, perms/2, fac/1]).

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
    
               
