-module(p57).

-export([answer/0]).

%% It is possible to show that the square root of two can be expressed as an infinite continued fraction.
%% √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
%% By expanding this for the first four iterations, we get:
%% 1 + 1/2 = 3/2 = 1.5
%% 1 + 1/(2 + 1/2) = 7/5 = 1.4
%% 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
%% 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
%% The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985,
%% is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.
%% In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?

-compile(export_all).

answer() ->
    {_, _, C} = lists:foldl(fun(_, {N, D, C}) ->
                                    {N1, D1} = next(N, D),
                                    case len(N1+D1) > len(D1) of
                                        true -> {N1, D1, C+1};
                                        false -> {N1, D1, C}
                                    end
                            end,
                            {1, 2, 0}, lists:seq(1,1000)),
    C.

next(N, D) -> {D, D*2 + N}.

len(N) -> length(integer_to_list(N)).
