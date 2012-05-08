-module(p81).

-export([answer/0]).

%% In the 5 by 5 matrix below, the minimal path sum from the top left to the
%% bottom right, by only moving to the right and down, is indicated in bold red
%% and is equal to 2427.
%%   *131	673	234	103	18
%%   *201	*96	*342	965	150
%%   630	803	*746	*422	111
%%   537	699	497	*121	956
%%   805	732	524	*37	*331
%% Find the minimal path sum, in matrix.txt, a 31K text file containing a 80 by
%% 80 matrix, from the top left to the bottom right by only moving right and down.

answer() ->
    %% {ok, Bin} = file:read_file("matrix.txt"),
    %% Str = binary_to_list(Bin),
    %% M = lists:map(fun erlang:list_to_integer/1, string:tokens(Str, [$\r, $\n, $,])),
    %% Len = round(math:sqrt(length(M))),
    M = [131,673,234,103,18
    	 ,201,96,342,965,150
    	 ,630,803,746,422,111
    	 ,537,699,497,121,956
    	 ,805,732,524,37,331
    	],
    Next = p12:triangle_iterator(),
    Len = round(math:sqrt(length(M))),
    M1 = to_tri(lists:reverse(M), Len),
    Len1 = length(M1)+1,
    io:format("M1: ~p~n~n", [M1]),
    lists:min(calc_routes(M1, [hd(M1)], 1, Len, Len1, Next() )).

to_tri(M0, Len) ->
    Take = ((Len*Len + Len) div 2),
    Unchanged = lists:sublist(M0, Take), % get the first N^2 + N / 2 numbers, then add gaps
    {_, M} = lists:foldl(fun(X, {Take0, Acc}) ->
				 Nulls = lists:duplicate(Len - X, 1000000),
				 Vals = lists:reverse(lists:sublist(M0, Take0, X)),
				 Line = Vals ++ Nulls,
				 {Take0 + X, Line ++ Acc}
			 end, {Take+1, lists:reverse(Unchanged)}, lists:seq(Len-1, 1, -1)),
    lists:reverse(M).
    

% {Row,Col} is the Current Node we're looking at
% We can sum it with the one below and one below and to the right, when stacked
calc_routes(_M, Routes, _R, L, L1, [L1| _Next]) ->
    io:format("Final Routes ~p~n", [Routes]),
    Routes;
calc_routes(M, Routes, R, L, L1, [T| Next]) when R > L ->
    io:format("Routes: ~p, R: ~p L: ~p L1: ~p T: ~p~n", [Routes, R, L, L1, T]),
    RawRoutes = lists:map(fun (C) -> node_route(M, R+1, C, lists:nth(C, Routes)) end , lists:seq(1, L)),
    NewRoutes = reduce(RawRoutes),
    calc_routes(M, NewRoutes, R+1, L, L1, Next());
calc_routes(M, Routes, R, L, L1, [T| Next]) ->
    io:format("Routes: ~p, R: ~p L: ~p T: ~p~n", [Routes, R, L, T]),
    RawRoutes = lists:map(fun (C) -> node_route(M, R+1, C, lists:nth(C, Routes)) end , lists:seq(1, R)),
    NewRoutes = reduce(RawRoutes),
    calc_routes(M, NewRoutes, R+1, L, L1, Next()).

reduce(L) ->
    lists:reverse(lists:foldl(fun reduce/2, [], L)).
% build backwards to take advantage of pattern matching
reduce([L, R], []) ->
    [R,L];
reduce([L, R], [H | Acc]) ->
    B = case L < H of
            true -> L;
            _Else -> H
        end,
    [R, B | Acc].


%% adds the lower two nodes to the current route's value
node_route(M, R, C, V) ->
    io:format("node_route(~p, ~p, ~p)~n", [R, C, V]),
    [V + get_value(M, R, C), V + get_value(M, R, C+1)].

get_value(M, R, C) ->
    Pos = coor_to_pos(R, C),
    case Pos > length(M) of 
        true -> io:format("Died on {~p, ~p} -> ~p~n", [R, C, Pos]), error;
        _Else -> V = lists:nth(Pos, M),
		 io:format("(~p, ~p) = ~p~n", [R, C, V]),
		 V
    end.

%% the first # in the row R is at (R^2 - R + 2) / 2 in the list
coor_to_pos(R, C) ->
    ((R*R - R + 2) div 2) + (C-1).
