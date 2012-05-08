-module(my_lists).

-export([unfold/4]).

unfold(Seed, ExpandFun, SeedFun, UntilFun) ->
    unfold(Seed, ExpandFun, SeedFun, UntilFun, []).


unfold(Seed, ExpandFun, SeedFun, UntilFun, Acc) ->
    case UntilFun(Seed) of
        true -> Acc;
        false ->
            unfold(SeedFun(Seed), ExpandFun, SeedFun, UntilFun, [ExpandFun(Seed) | Acc])
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unfold_simple_test() ->
    ?assertEqual([0,1,2,3], unfold(4
                                   ,fun(S) -> S-1 end
                                   ,fun(S) -> S-1 end
                                   ,fun(S) -> S =:= 0 end
                                  )).

unfold_fold_test() ->
    Unfold = unfold(42
                    ,fun(S) -> S div 2 end
                    ,fun(S) -> S div 2 end
                    ,fun(S) -> S < 1 end
                   ),
    Map = [(N rem 2) + $0 || N <- Unfold],
    Fold = lists:foldl(fun(S, Acc) -> [S | Acc] end, "", Map),
    ?assertEqual("101010", Fold).
-endif.
