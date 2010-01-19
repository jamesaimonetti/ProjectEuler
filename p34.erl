-module(p34).

-export([answer/0]).

%% 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
%% Find the sum of all numbers which are equal to the sum of the factorial of their digits.
%% Note: as 1! = 1 and 2! = 2 are not sums they are not included.

answer() ->
    lists:sum(lists:filter(fun is_sum_of_fac/1, lists:seq(10, 100000))).

is_sum_of_fac(N) ->
    lists:sum(lists:map(fun(D) -> my_math:fac(D-48) end, integer_to_list(N))) =:= N.
