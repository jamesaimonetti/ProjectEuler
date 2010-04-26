-module(p49).

-export([answer/0]).

%% The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
%% (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
%% There are no arithmetic sequences made up of three 1-,2-,or 3-digit primes,exhibiting this property,
%% but there is one other 4-digit increasing sequence.
%% What 12-digit number do you form by concatenating the three terms in this sequence?

-compile(export_all).

answer() ->
    [P | Ps] = lists:filter(fun(P) -> P > 999 andalso P < 10000 end, primes:queue(9999)),
    Perms = find_perms(P, Ps, []),
    io:format("Seqs: ~p~n", find_seq(Perms, [])).

find_seq([], Seqs) -> Seqs;
find_seq([{P, [P1 | Perms]} | Ps], Seqs) ->
    case is_seq(P1, Perms, P1 - P, 1) of
        true -> find_seq(Ps, [lists:flatten([P, P1, Perms]) | Seqs]);
        false -> find_seq(Ps, Seqs)
    end.

is_seq(P, [P1 | Ps], Diff, _Cnt) when P1 - P =/= Diff ->
    is_seq(P1, Ps, P1-P, 0);
is_seq(_, [P1 | Ps], Diff, Cnt) ->
    is_seq(P1, Ps, Diff, Cnt+1);
is_seq(_, [], _, Cnt) -> Cnt > 1.

find_perms(_, [], Perms) -> Perms;
find_perms(P, Ps, Perms) ->
    Perm = lists:filter(fun(Pr) -> is_perm(P, Pr) end, Ps),
    [P1 | Ps1] = Ps,
    case length(Perm) >= 2 of
        true ->
            find_perms(P1, Ps1, [{P, lists:sort(Perm)} | Perms]);
        false ->
            find_perms(P1, Ps1, Perms)
    end.
            

is_perm(X, Y) ->
    lists:member(integer_to_list(Y), my_math:perms(integer_to_list(X))).
