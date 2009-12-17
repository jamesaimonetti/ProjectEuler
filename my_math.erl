-module(my_math).

-export([gcd/1, gcd/2, lcm/1, lcm/2]).

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