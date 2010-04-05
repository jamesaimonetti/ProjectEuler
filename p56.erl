-module(p56).

-export([answer/0]).

%% A googol (10^100) is a massive number: one followed by one-hundred zeros;
%% 100^100 is almost unimaginably large: one followed by two-hundred zeros.
%% Despite their size, the sum of the digits in each number is only 1.
%% Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?

-compile(export_all).

answer() -> 
    lists:max([ digital_sum(round(math:pow(A, B))) || A <- lists:seq(90,99),
                                                      B <- lists:seq(90,99) ]).

digital_sum(N) -> lists:foldl(fun(D, Acc) -> (D-$0) + Acc end, 0, integer_to_list(N)).
