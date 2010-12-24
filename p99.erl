-module(p99).

-export([answer/0]).

%% Comparing two numbers written in index form like 2^(11) and 3^(7) is not
%% difficult, as any calculator would confirm that 2^(11) = 2048 < 3^(7) = 2187.
%% However, confirming that 632382^(518061) > 519432^(525806) would be much more
%% difficult, as both numbers contain over three million digits.
%% Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text
%% file containing one thousand lines with a base/exponent pair on each line,
%% determine which line number has the greatest numerical value.
%% NOTE: The first two lines in the file represent the numbers in the example given above.

answer() ->
    p99().

p99() ->
    {ok, B} = file:read_file("base_exp.txt"),
    Lines = string:tokens(binary_to_list(B), [$\r,$\n]),
    BaseExps = lists:map(fun(S) -> string:tokens(S, ",") end, Lines),
    p99(0, 0, 1, BaseExps).

p99(_, MaxLineNo, _, []) -> MaxLineNo;
p99(MaxVal, MaxLineNo, LineNo, [[Base, Exp] | BEs]) -> 
    V = list_to_integer(Exp) * math:log(list_to_integer(Base)),
    case V > MaxVal of
	true -> p99(V, LineNo, LineNo+1, BEs);
	false -> p99(MaxVal, MaxLineNo, LineNo+1, BEs)
    end.
