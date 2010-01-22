-module(p39).

-export([answer/0]).

%% If p is the perimeter of a right angle triangle with integral length sides, {a,b,c},
%% there are exactly three solutions for p = 120.
%% {20,48,52}, {24,45,51}, {30,40,50}
%% For which value of p ≤ 1000, is the number of solutions maximised?

-compile(export_all).

answer() ->
    Tris = [ A+B+C || C <- lists:seq(1, 500),
                      B <- lists:seq(1, C-1),
                      A <- lists:seq(1, B),
                      (A*A + B*B) =:= (C*C),
                      (A+B+C) =< 1000 ],
    Sorted = lists:sort(Tris),
    find_p(Sorted, hd(Sorted), 0, 0, hd(Sorted)).

find_p([], _, _, _, MaxP) -> MaxP;
find_p([P | Ps], P, C, Max, MaxP) -> find_p(Ps, P, C+1, Max, MaxP);
find_p([P | Ps], CurP, C, Max, MaxP) ->
    case C > Max of
        true -> find_p(Ps, P, 0, C, CurP);
        false -> find_p(Ps, P, 0, Max, MaxP)
    end.
