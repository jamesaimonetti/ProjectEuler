-module(p59).

-export([answer/0]).

%% Each character on a computer is assigned a unique code and the preferred standard is ASCII
%% (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk
%% (*) = 42, and lowercase k = 107.
%% A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each
%% byte with a given value, taken from a secret key. The advantage with the XOR function is that
%% using the same encryption key on the cipher text, restores the plain text; for example,
%% 65 XOR 42 = 107, then 107 XOR 42 = 65.
%% For unbreakable encryption, the key is the same length as the plain text message, and the key
%% is made up of random bytes. The user would keep the encrypted message and the encryption key in
%% different locations, and without both "halves", it is impossible to decrypt the message.
%% Unfortunately, this method is impractical for most users, so the modified method is to use a
%% password as a key. If the password is shorter than the message, which is likely, the key is
%% repeated cyclically throughout the message. The balance for this method is using a sufficiently
%% long password key for security, but short enough to be memorable.
%% Your task has been made easy, as the encryption key consists of three lower case characters.
%% Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted
%% ASCII codes, and the knowledge that the plain text must contain common English words, decrypt
%% the message and find the sum of the ASCII values in the original text.

-compile(export_all).

answer() ->
    {ok, Binary} = file:read_file("cipher1.txt"),
    Digits = [ list_to_integer(N) || N <- string:tokens(binary_to_list(Binary), ",\r\n") ],
    Candidates = lists:filter(fun(Msg) -> is_msg(Msg) end, lists:map(fun(Key) -> msg(Digits, Key) end, gen_keys(length(Digits)))),
    lists:map(fun(Msg) -> {sum, lists:sum(Msg), msg, Msg} end, Candidates).

msg(Digits, Key) -> lists:zipwith(fun(D, K) -> D bxor K end, Digits, Key).

is_msg(Msg) -> lists:all(fun(M) -> M > 31 andalso M < 123 end, Msg).

repeat(Xs, N) -> repeat(Xs, N, []).

repeat(_, 0, L) -> lists:reverse(L);
repeat([X|Xs], N, L) ->
    repeat(lists:flatten([Xs, [X]]), N-1, [X | L]).

gen_keys(Len) ->
    Range = lists:seq($a, $z),
    [ repeat([A, B, C], Len) || A <- Range, B <- Range, C <- Range].
