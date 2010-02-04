-module(p53).

-export([answer/0, answer1/0]).

%% There are exactly ten ways of selecting three from five, 12345:
%% 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
%% In combinatorics, we use the notation,  5(C)3 = 10.
%% In general, n(C)r = n! r! (n−r)!, where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
%% It is not until n = 23, that a value exceeds one-million: 23(C)10 = 1144066.
%% How many, not necessarily distinct, values of n(C)r, for 1 ≤ n ≤ 100, are greater than one-million?

answer() ->
    Facs = fac_list(100),
    Combo = fun(N, C) ->
                    Nfac = proplists:get_value(N, Facs),
                    Cfac = proplists:get_value(C, Facs),
                    NCfac = proplists:get_value(N-C, Facs),
                    Nfac / ( Cfac * NCfac )
            end,
    length([ {N, C} || N <- lists:seq(1,100),
                       C <- lists:seq(1,N),
                       Combo(N, C) > 1000000]).

fac_list(N) -> fac_list(1, N, 1, [{0, 1}]).
fac_list(N, N, Fac, L) -> lists:reverse([{N, Fac*N} | L]);
fac_list(X, N, Fac, L) -> fac_list(X+1, N, Fac*X, [{X, Fac*X} | L]).

%% From the forums, using the symmetry of Pascal's Triangle
answer1() ->
    Facs = fac_list(100),
    Combo = fun(N, C) ->
                    Nfac = proplists:get_value(N, Facs),
                    Cfac = proplists:get_value(C, Facs),
                    NCfac = proplists:get_value(N-C, Facs),
                    Nfac / ( Cfac * NCfac )
            end,
    2 * length([ {N, R} || N <- lists:seq(1, 100),
                           R <- lists:seq(1, N div 2),
                           Combo(N, R) > 1000000]) -
        length([ {N, N div 2} || N <- lists:seq(2, 100, 2),
                                 Combo(N, N div 2) > 1000000]).
