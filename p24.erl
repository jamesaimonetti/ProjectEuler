-module(p24).

-export([answer/0]).

%% A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4.
%% If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.
%% The lexicographic permutations of 0, 1 and 2 are:
%% 012   021   102   120   201   210
%% What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

answer() ->
    Len = 10,
    find_answer(999999, Len, lists:seq(0, Len-1), []).

%% C is the lexi-perm we're looking for
%% N is how many numbers are left to lookup
%% Nums are the available Numbers to choose from
%% A is a reverse-order list containing the C-th permutation

find_answer(_C, 0, _Nums, A) -> lists:reverse(A);
find_answer(C, N, Nums, A) ->
    PerN = my_math:perms(N-1, N-1),
    Num = lists:nth(C div PerN + 1, Nums),
    find_answer(C rem PerN, N-1, lists:subtract(Nums, [Num]), [Num | A]).
