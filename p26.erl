-module(p26).

-export([answer/0]).

%% A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
%% 1/2 = 0.5
%% 1/3 = 0.(3)
%% 1/4 = 0.25
%% 1/5 = 0.2
%% 1/6 = 0.1(6)
%% 1/7 = 0.(142857)
%% 1/8 = 0.125
%% 1/9 = 0.(1)
%% 1/10 = 0.1
%% Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
%% Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

% taken from http://excelicious.wordpress.com/2009/08/18/project-euler-26/ cause I was a fish out of water.
answer() ->
    Ns = primes:queue(20), % all primes < 1000
    lists:foldl(fun(P, {P1, L1}) -> L = recur_len(1, [], P), case L > L1 of true -> {P, L}; false -> {P1, L1} end end, {1,1}, Ns).

recur_len(_I, _Rems, D) when D =< 5 -> 1;
recur_len(I, Rems, D) ->
    R = div_by(I, D),
    io:format("I: ~w D: ~w R: ~w Rems: ~w~n", [I, D, R, Rems]),
    case lists:member(R, Rems) of
        true -> length(Rems) - pos(Rems, R);
        false -> recur_len(R, Rems ++ [R], D)
    end.

div_by(R, D) ->
    Y = lists:min([ X || X <- lists:seq(1, 5), R * round(math:pow(10, X)) > D ]),
    R * round(math:pow(10, Y)) rem D.

pos(L, M) -> pos(L, M, 0).     

pos([H|_L], H, I) -> I;
pos([], _M, _I) -> false;
pos([_H|L], M, I) -> pos(L, M, I+1).
