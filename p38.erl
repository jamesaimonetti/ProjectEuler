-module(p38).

-export([answer/0]).

%% Take the number 192 and multiply it by each of 1, 2, and 3:
%%    192 × 1 = 192
%%    192 × 2 = 384
%%    192 × 3 = 576
%% By concatenating each product we get the 1 to 9 pandigital, 192384576.
%% We will call 192384576 the concatenated product of 192 and (1,2,3)
%% The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5,
%% giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
%% What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated
%% product of an integer with (1,2, ... , n) where n > 1?

-compile(export_all).

answer() ->
    SetLengths = lists:seq(2,5),
    TestNs = lists:seq(1,10000),
    find_max(TestNs, SetLengths, 0).

find_max(_, [], Max) -> Max;
find_max(TestNs, [Len | SetT], Max) ->
    Set = lists:seq(1,Len),
    M = lists:max(
          lists:map(fun(N) -> case is_candidate(N, Set) of
                                  false -> 0;
                                  P -> P
                              end
                    end,
                    TestNs)
         ),
    case M > Max of
        true ->  find_max(TestNs, SetT, M);
        false -> find_max(TestNs, SetT, Max)
    end.
%% N is integer
%% L is list of multipliers
is_candidate(N, L) ->
    Prod = lists:reverse(
             lists:flatten(
               lists:foldl(fun(X, A) -> [ lists:reverse(integer_to_list(N*X)) | A] end, [], L)
              )
            ),
    case is_pandigital(Prod) of
        true -> list_to_integer(Prod);
        false -> false
    end.

is_pandigital(N) when length(N) =:= 9 ->
    lists:all(fun(D) -> lists:member(D, N) end, lists:seq($1, $9));
is_pandigital(_) -> false.
