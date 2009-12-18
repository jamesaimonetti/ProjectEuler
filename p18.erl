-module(p18).

-export([answer/0, calc_routes/4]).

%% By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

%% 3
%% 7 4
%% 2 4 6
%% 8 5 9 3
%% That is, 3 + 7 + 4 + 9 = 23.

%% Find the maximum total from top to bottom of the triangle below:

%% 75
%% 95 64
%% 17 47 82
%% 18 35 87 10
%% 20 04 82 47 65
%% 19 01 23 75 03 34
%% 88 02 77 73 07 63 67
%% 99 65 04 28 06 16 70 92
%% 41 41 26 56 83 40 80 70 33
%% 41 48 72 33 47 32 37 16 94 29
%% 53 71 44 65 25 43 91 52 97 51 14
%% 70 11 33 28 77 73 17 78 39 68 17 57
%% 91 71 52 38 17 14 91 43 58 50 27 29 48
%% 63 66 04 68 89 53 67 30 73 16 69 87 40 31
%% 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

%% NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67,
%% is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)

-define(TRIANGLE, [75, 95, 64, 17, 47, 82, 18, 35, 87, 10, 20, 04, 82, 47, 65, 19, 01, 23, 75, 03, 34, 88, 02, 77, 73, 07, 63, 67,
                    99, 65, 04, 28, 06, 16, 70, 92, 41, 41, 26, 56, 83, 40, 80, 70, 33, 41, 48, 72, 33, 47, 32, 37, 16, 94, 29, 53,
                    71, 44, 65, 25, 43, 91, 52, 97, 51, 14, 70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57, 91, 71, 52, 38, 17, 14,
                    91, 43, 58, 50, 27, 29, 48, 63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31, 04, 62, 98, 27, 23, 09, 70,
                    98, 73, 93, 38, 53, 60, 04, 23]).

%-define(SMALL, [3, 7, 4, 2, 4, 6, 8, 5, 9, 3]).

answer() ->
    Next = p12:triangle_iterator(),
    lists:max(calc_routes([hd(?TRIANGLE)], 1, length(?TRIANGLE), Next() )).

% {Row,Col} is the Current Node we're looking at
% We can sum it with the one below and one below and to the right, when stacked
calc_routes(Routes, _R, L, [L| _Next]) -> Routes;
calc_routes(Routes, R, L, [_T| Next]) ->
    RawRoutes = lists:map(fun (C) -> node_route(R+1, C, lists:nth(C, Routes)) end , lists:seq(1, R)),
    NewRoutes = reduce(RawRoutes),
    calc_routes(NewRoutes, R+1, L, Next()).

reduce(L) ->
    lists:reverse(lists:foldl(fun reduce/2, [], L)).
% build backwards to take advantage of pattern matching
reduce([L, R], []) ->
    [R,L];
reduce([L, R], [H | Acc]) ->
    B = case L > H of
            true -> L;
            _Else -> H
        end,
    [R, B | Acc].


%% adds the lower two nodes to the current route's value
node_route(R, C, V) ->
    [V + get_value(R, C), V + get_value(R, C+1)].

get_value(R, C) ->
    Pos = coor_to_pos(R, C),
    case Pos > length(?TRIANGLE) of 
        true -> error;
        _Else -> lists:nth(Pos, ?TRIANGLE)
    end.

%% the first # in the row R is at (R^2 - R + 2) / 2 in the list
coor_to_pos(R, C) ->
    ((R*R - R + 2) div 2) + (C-1).
