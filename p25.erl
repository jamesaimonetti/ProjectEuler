-module(p25).

-export([answer/0]).

%% The Fibonacci sequence is defined by the recurrence relation: F(n) = F(n−1) + F(n−2), where F_(1) = 1 and F_(2) = 1.
%% Hence the first 12 terms will be:
%%    F_(1) = 1
%%    F_(2) = 1
%%    F_(3) = 2
%%    F_(4) = 3
%%    F_(5) = 5
%%    F_(6) = 8
%%    F_(7) = 13
%%    F_(8) = 21
%%    F_(9) = 34
%%    F_(10) = 55
%%    F_(11) = 89
%%    F_(12) = 144
%% The 12th term, F_(12), is the first term to contain three digits.
%% What is the first term in the Fibonacci sequence to contain 1000 digits?

-compile(export_all).

answer() ->
    find_term_by_length(1000).

%% find the first term that has L digits
find_term_by_length(L) ->
    Fibs = my_math:fib_generator(),
    find_term(L, Fibs).

find_term(L, [{Nth, F} | Next]) ->
    case digits(F) >= L of
        true -> Nth;
        false -> find_term(L, Next())
    end.

digits(N) ->
    length(integer_to_list(N)).
