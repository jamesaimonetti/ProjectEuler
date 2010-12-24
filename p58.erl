-module(p58).

-export([answer/0]).

%% Starting with 1 and spiralling anticlockwise in the following way, a square
%% spiral with side length 7 is formed.

%% 37 36 35 34 33 32 31
%% 38 17 16 15 14 13 30
%% 39 18  5  4  3 12 29
%% 40 19  6  1  2 11 28
%% 41 20  7  8  9 10 27
%% 42 21 22 23 24 25 26
%% 43 44 45 46 47 48 49

%% It is interesting to note that the odd squares lie along the bottom right
%% diagonal, but what is more interesting is that 8 out of the 13 numbers lying
%% along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
%% If one complete new layer is wrapped around the spiral above, a square spiral
%% with side length 9 will be formed. If this process is continued, what is the
%% side length of the square spiral for which the ratio of primes along both
%% diagonals first falls below 10%?

%% from forum, the four corners are N*N, N*N-N+1, N*N -2N+2, N*N-3N+3, which reduces to 4*N*N - 6*N + 6, for N > 1
answer() ->
    p58(3, 5, 5).

p58(NumP, Tot, N) when NumP / Tot < 0.100000000 -> N-2;
p58(NumP, Tot, N) ->
    C = N * N,
    Corners = [C - N+1
	       ,C - (2*N) + 2
	       ,C - (3*N) + 3
	      ],
    Ps = length(lists:filter(fun primes:is_prime/1, Corners)),
    p58(NumP+Ps, Tot+4, N+2).
