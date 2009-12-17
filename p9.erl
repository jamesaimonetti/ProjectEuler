-module(p9).

-export([answer/0]).

%% A Pythagorean triplet is a set of three natural numbers, a < b < c, for which, a^(2) + b^(2) = c^(2)
%% For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).
%% There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product a*b*c.

answer() ->
    [{X, Y, Z} | _Rest] = [ {A, B, C} || C <- lists:seq(1, 1000),
                                         B <- lists:seq(1, 499),
                                         A <- lists:seq(1, 498),
                                         (A*A + B*B) =:= (C*C),
                                         (A+B+C) =:= 1000,
                                         A < B ],
    X*Y*Z.
