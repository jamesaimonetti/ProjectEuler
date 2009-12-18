-module(p15).

-export([answer/0]).

%% Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.
%% How many routes are there through a 20x20 grid?

answer() ->
    a(20).

a(1) -> 2;
%a(N+1) -> ( 2 * ( 2 * N + 1 ) * a(N-1) ) div ( N + 1 ).
a(N) -> ( 4 * N -2 ) * a(N-1) div N.
