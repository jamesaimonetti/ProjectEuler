-module(p33).

-export([answer/0]).

%% The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify
%% it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
%% We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
%% There are exactly four non-trivial examples of this type of fraction,  less than one in value,
%% and containing two digits in the numerator and denominator.
%% If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

-compile(export_all).

answer() ->
    IncFrac = [ {N,D} || D <- lists:seq(10, 99), N <- lists:seq(10, D), is_div_inc(N, D)],
    {Num, Den} = lists:foldl(fun({N,D}, {An, Ad}) -> {N*An, D*Ad} end, {1,1}, IncFrac),
    GCD = my_math:gcd(Num, Den),
    Den div GCD.

is_div_inc(N, D) -> is_div_inc(integer_to_list(N), integer_to_list(D), N/D).

%% AA / AA -> false
is_div_inc(A, A, _) -> false;
%% AB / BD -> A/D
is_div_inc([A, B], [B, D], Res) when D > $0 -> (A-48) / (D-48) == Res;
%% AB / CB -> A/C when B =/= 0
is_div_inc([A, B], [C, B], Res) when B > $0 andalso C > $0 -> (A-48) / (C-48) == Res;
%% AB / AC -> B/C
is_div_inc([A, B], [A, C], Res) when C > $0 -> (B-48) / (C-48) == Res;
is_div_inc(_,_,_) -> false.

%% List comprehension from Forum
answer2() ->
    IncFrac = [ {10*X+Y,10*Y+Z} || X <- lists:seq(1,9),
                                   Y <- lists:seq(1,9),
                                   Z <- lists:seq(1,9),
                                   X =/= Y,
                                   (9*X*Z) + (Y*Z) == (10*X*Y)],
    {Num, Den} = lists:foldl(fun({N,D}, {An, Ad}) -> {N*An, D*Ad} end, {1,1}, IncFrac),
    GCD = my_math:gcd(Num, Den),
    Den div GCD.
