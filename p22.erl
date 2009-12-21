-module(p22).

-export([answer/0]).

%% Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names,
%% begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value
%% by its alphabetical position in the list to obtain a name score.
%% For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the
%% 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
%% What is the total of all the name scores in the file?

answer() ->
    {ok, Binary} = file:read_file("names.txt"),
    Names = string:tokens(binary_to_list(Binary), "\",\""),
    Sorted = lists:sort(Names),
    lists:sum(my_map(fun(Name, Idx) -> name_score(Name, Idx) end, Sorted)).

my_map(F, L) ->
    my_map(F, L, 1, []).

my_map(_F, [], _Idx, Res) -> lists:reverse(Res);
my_map(F, [H | L], Idx, Res) ->
    my_map(F, L, Idx+1, [F(H, Idx) | Res]).

name_score(Name, Idx) ->
    lists:sum(lists:map(fun(X) -> X - 64 end, Name)) * Idx.
