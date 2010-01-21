-module(p52).

-export([answer/0]).

%% It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
%% Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

-compile(export_all).

answer() -> find_x(from(100000, 1)).

find_x([X|Xs]) ->
    Terms = lists:map(fun(N) -> integer_to_list(N*X) end, lists:seq(1, 6)),
    case is_answer(Terms) of
        false -> find_x(Xs());
        _ -> X
    end.

is_answer(Terms) ->
    lists:foldl( fun(S, Acc) ->
                         case lists:sort(S) == Acc of
                             true -> Acc;
                             false -> false
                         end
                 end,
                 lists:sort(hd(Terms)),
                 Terms).

from(K, Inc) -> [K|fun()-> from(K+Inc, Inc) end].
