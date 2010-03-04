-module(p55).

-export([answer/0]).

%% If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
%% Not all numbers produce palindromes so quickly. For example,
%%   349 + 943 = 1292,
%%   1292 + 2921 = 4213
%%   4213 + 3124 = 7337
%% That is, 349 took three iterations to arrive at a palindrome.
%% Although no one has proved it yet, it is thought that some numbers, like 196,
%% never produce a palindrome. A number that never forms a palindrome through the
%% reverse and add process is called a Lychrel number. Due to the theoretical nature
%% of these numbers, and for the purpose of this problem, we shall assume that a number
%% is Lychrel until proven otherwise. In addition you are given that for every number
%% below ten-thousand, it will either (i) become a palindrome in less than fifty iterations,
%% or, (ii) no one, with all the computing power that exists, has managed so far to map it
%% to a palindrome. In fact, 10677 is the first number to be shown to require over fifty
%% iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).
%% Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
%% How many Lychrel numbers are there below ten-thousand?

-define(MAX_ITERATIONS, 50).

-compile(export_all).

answer() -> length(lists:filter(fun is_lychrel/1, lists:seq(1,10000))).

is_lychrel(N) -> is_lychrel(rev_add(N), 0).

is_lychrel(_N, ?MAX_ITERATIONS) -> true;
is_lychrel(N, I) ->
    case my_math:is_palindrome(N) of
        true -> false;
        false -> is_lychrel(rev_add(N), I+1)
    end.

rev_add(N) ->
    N2 = list_to_integer(lists:reverse(integer_to_list(N))),
    N+N2.
