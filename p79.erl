-module(p79).

-export([answer/0]).

%% A common security method used for online banking is to ask the user for three random
%% characters from a passcode. For example, if the passcode was 531278, they may ask
%% for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.
%% The text file, keylog.txt, contains fifty successful login attempts.
%% Given that the three characters are always asked for in order,
%% analyse the file so as to determine the shortest possible secret passcode of unknown length.

-compile(export_all).

answer() -> 
    {ok, IoDevice} = file:open("keylog.txt", [read]),
    process(IoDevice, []).

process(IoDevice, L) ->
    case file:read_line(IoDevice) of
        {ok, [N1, N2, N3 | _Rest]} ->
            process(IoDevice, update([N1, N2, N3], L));
        eof -> L;
        {error, R} -> io:format("Error reading from file: ~w~n", [R]),
                      undefined
    end.

update(L, []) -> L;
update([], L) -> L;
update([H | T], L) ->
    case lists:member(H, L) of
        % split the list so hd(TailL) =:= H
        true -> {HeadL, TailL} = lists:splitwith(fun(N) -> N =/= H end, L),
                update(H, T, HeadL, TailL);
        false -> update(T, lists:reverse([H | L]))
    end.

% h exists as hd(TailL)
update(_H, [], HeadL, TailL) -> update([], lists:flatten([HeadL | TailL]));
update(H, [N | T], HeadL, TailL) ->
    case lists:member(N, HeadL) of
        true -> % if N is in HeadL, and since N comes after H, swap N and H
            update(T, new_list(H, N, HeadL, TailL));
        false -> % if N is not in HeadL, reconstruct the list and evaluate from N
            update([N|T], lists:flatten([HeadL | TailL]))
    end.

%% HeadL = [...B...], TailL = [A...]
new_list(A, B, HeadL, [A | TailL]) ->
    % Left = [...], Right = [B...]
    {Left, Right} = lists:splitwith(fun(N) -> N =/= B end, HeadL),
    lists:flatten([Left, A, tl(Right), B | TailL]).
