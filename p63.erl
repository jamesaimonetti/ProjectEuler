-module(p63).

-export([answer/0]).

%% The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
%% How many n-digit positive integers exist which are also an nth power?

%% the answer the problem is looking for seems to be constrained by hardware. With Erlang
%% I could find well over 100 matching numbers such that len(pow(X, Y)) == Y
answer() ->
    length([ 1 || X <- lists:seq(1,10),
                  Y <- lists:seq(1,22),
                  len(trunc(math:pow(X,Y))) == Y ]).

len(N) -> length(integer_to_list(N)).
