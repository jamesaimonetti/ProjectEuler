-module(p36).

-export([answer/0]).

%% The decimal number, 585 = 1001001001(binary), is palindromic in both bases.
%% Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
%% (Please note that the palindromic number, in either base, may not include leading zeros.)

answer() ->
    find_answer(1, 1000000, 0).

find_answer(Max, Max, Sum) -> Sum;
find_answer(N, Max, Sum) ->
    case is_palindrome(integer_to_list(N)) andalso is_palindrome(to_base_2(N)) of
        true -> find_answer(N+1, Max, Sum+N);
        false -> find_answer(N+1, Max, Sum)
    end.

to_base_2(N) ->
    hd(io_lib:format("~.2B", [N])).

is_palindrome(L) -> lists:reverse(L) =:= L.
