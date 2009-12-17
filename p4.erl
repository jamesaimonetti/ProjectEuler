-module(p4).

-export([answer/0]).

%% A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
%% Find the largest palindrome made from the product of two 3-digit numbers.

answer() ->
    find_palindrome(lists:seq(100,999), 0).

find_palindrome([], P) -> P;
find_palindrome([N | T], Palindrome) ->
    Prods = lists:map(fun(X) -> N * X end, T),
    Palins = lists:filter(fun is_palindrome/1, [1 | Prods]), % ensure 1 palindrome
    Palin = lists:max(Palins),
    P = case Palin > Palindrome of
            true -> Palin;
            _Else  -> Palindrome
        end,
    find_palindrome(T, P).

is_palindrome(N) ->
    Str = integer_to_list(N),
    Str =:= lists:reverse(Str).
