-module(p28).

-export([answer/0, answer2/0]).

%% Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
%% 21 22 23 24 25
%% 20  7  8  9 10
%% 19  6  1  2 11
%% 18  5  4  3 12
%% 17 16 15 14 13
%% It can be verified that the sum of the numbers on the diagonals is 101.
%% What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

answer() ->
    spiral(1001, 1, 1, 0).

spiral(N, N, LastD, Sum) -> Sum + lists:sum(quartet(N, LastD));
spiral(N, Cur, LastD, Sum) ->
    Seq = quartet(Cur, LastD),
    spiral(N, Cur+2, lists:last(Seq), Sum + lists:sum(Seq)).
    
quartet(N, LastD) -> lists:seq(LastD + N - 1, N*N, N-1).

%% from forum, the four corners are N*N, N*N-N+1, N*N -2N+2, N*N-3N+3, which reduces to 4*N*N - 6*N + 6, for N > 1
answer2() ->
    lists:foldl(fun(N, Acc) -> Acc + ( (4*N*N) - (6*N) + 6) end, 1, lists:seq(3, 1001, 2)).
