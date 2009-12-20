-module(p20).

-export([answer/0]).

%% n! means n × (n − 1) × ... × 3 × 2 × 1
%% Find the sum of the digits in the number 100!

answer() ->
    lists:foldl(fun(X, Acc) -> (X-48) + Acc end, 0, integer_to_list(my_math:fac(100))).
