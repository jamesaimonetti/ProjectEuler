-module(prob).

-export([answer/0]).

%% You have the numbers 123456789, in that order. Between each number, you must
%% insert either nothing, a plus sign, or a multiplication sign, so that the
%% resulting expression equals 2002. Operator precedence does apply.

answer() -> find(lists:seq($1,$9)).

find(L) -> build(L, []).

build([], E) -> Ex = lists:reverse([$., $2, $0, $0, $2, $=,$:,$= | E]),
                case eval(Ex) of
                    {value, true, _} ->
                        io:format("E: ~p~n", [Ex]);
                    _Else -> false
                end;
build([H|T], []) -> build(T, [H]);
build([H|T], E) ->
    build(T, [H, $+ | E]),
    build(T, [H, $* | E]),
    build(T, [H | E]).

eval(S) ->
    {ok,Scanned,_} = erl_scan:string(S),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed,[]).
