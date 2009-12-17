%%% Adpated from : http://www.erlang.org/pipermail/erlang-questions/2007-August/028769.html
%%% File    : skew_kv.erl
%%% Author  :  <olopierpa@gmail.com>
%%% Description : Skew heaps using Key->Value structure
%%% Created : 30 May 2003 by Pierpaolo BERNARDI <olopierpa@gmail.com>
%%% Modified : 17 Dec 2009 by James Aimonetti <james.aimonetti@gmail.com>

-module(skew_kv).

-export([empty/0,
         is_empty/1,
         min/1,
         delete_min/1,
         insert/3,
         merge/2
        ]).

%% Aggressive inlining - will increase code size.
%%-compile(inline).
%%-compile({inline_size,100}).

%%-define(TESTING,true).

-ifdef(TESTING).
-compile(export_all).
-endif.

-define(THE_EMPTY_HEAP, the_empty_heap).

empty() ->
    ?THE_EMPTY_HEAP.

is_empty(?THE_EMPTY_HEAP) -> true;
is_empty(_) -> false.

min({K, V, _, _}) -> {K, V}.

delete_min({_K, _V, A, B}) -> merge(A, B).

insert(Key, Value, A) ->
    merge({Key, Value, ?THE_EMPTY_HEAP, ?THE_EMPTY_HEAP}, A).

merge(A, ?THE_EMPTY_HEAP) ->
    A;
merge(?THE_EMPTY_HEAP, B) ->
    B;
merge({KeyA, ValueA, LA, RA}, B={KeyB, _ValueB, _, _}) when KeyA =< KeyB ->
    {KeyA, ValueA, RA, merge(LA, B)};
merge(A, {KeyB, ValueB, L, R}) ->
    {KeyB, ValueB, R, merge(L, A)}.
