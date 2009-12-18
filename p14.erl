-module(p14).

-export([answer/0]).

%% The following iterative sequence is defined for the set of positive integers:
%%   n → n/2 (n is even)
%%   n → 3n + 1 (n is odd)
%% Using the rule above and starting with 13, we generate the following sequence:
%% 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
%% It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem),
%% it is thought that all starting numbers finish at 1.
%% Which starting number, under one million, produces the longest chain?
%% NOTE: Once the chain starts the terms are allowed to go above one million.

answer() ->
    find_longest_chain(lists:seq(2,999999), {1, 1}).

find_longest_chain([], { Start, _Len }) -> Start;
find_longest_chain([N | R], {_Start, Len}=W) ->
    N_len = chain_length(N, 1),
    Winner = case N_len > Len of
                 true -> { N, N_len };
                 _Else -> W
             end,
    find_longest_chain(R, Winner).

chain_length(1, L) -> L;
chain_length(N, L) when N rem 2 =:= 0 ->
    chain_length(N div 2, L+1);
chain_length(N, L) ->
    chain_length(3*N+1, L+1).
    
