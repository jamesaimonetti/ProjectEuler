-module(p32).

-export([answer/0]).

%% We shall say that an n-digit number is pandigital if it makes use of all the
%% digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
%% The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand,
%% multiplier, and product is 1 through 9 pandigital.
%% Find the sum of all products whose multiplicand/multiplier/product identity can be written
%% as a 1 through 9 pandigital.
%% HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

-compile(export_all).

answer() ->
    Pdig = [ X*Y || X <- lists:seq(1,100),
                    Y <- lists:seq(101,2000),
                    is_pandigital(lists:flatten([integer_to_list(X), integer_to_list(Y), integer_to_list(X*Y)])) ],
    lists:sum(lists:usort(Pdig)).

is_pandigital(N) when length(N) =:= 9 ->
    lists:all(fun(D) -> lists:member(D, N) end, lists:seq($1, $9));
is_pandigital(_) -> false.
