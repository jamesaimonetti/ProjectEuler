-module(p42).

-export([answer/0]).

%% The nth term of the sequence of triangle numbers is given by, t_(n) = ½n(n+1); so the first ten triangle numbers are:
%% 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
%% By converting each letter in a word to a number corresponding to its alphabetical position and adding these values
%% we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t(10). If the word value is a
%% triangle number then we shall call the word a triangle word.
%% Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common
%% English words, how many are triangle words?

-compile(export_all).

answer() ->
    {ok, F} = file:read_file("words.txt"),
    WordsWithQuotes = re:split(F, ","),
    Words = lists:map(fun(Wwq) -> S = binary_to_list(Wwq), string:substr(S, 2, string:len(S)-2) end, WordsWithQuotes),
    Trinums = list_trinums(200), % highest word score is 192
    length(lists:filter(fun(W) -> is_tri_word(W, Trinums) end, Words)).

is_tri_word(Word, Trinums) -> lists:member(tscore(Word), Trinums).

list_trinums(Max) -> list_trinums(1, Max, []).
list_trinums(N, Max, L) ->
    T = N * ( N + 1 ) div 2,
    case T > Max of
        true -> lists:reverse(L);
        false -> list_trinums(N+1, Max, [T | L])
    end.

tscore(Word) -> lists:sum(lists:map(fun letter_to_integer/1, Word)).

letter_to_integer([L]) -> L - 64;
letter_to_integer(L) -> L - 64.
    
