-module(p16).

-export([answer/0]).

%% 2^(15) = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
%% What is the sum of the digits of the number 2^(1000)?

%% $1 = 49
answer() ->
    lists:foldl(fun(X, Acc) -> X + Acc - 48 end, 0, integer_to_list(round(math:pow(2, 1000)))).
