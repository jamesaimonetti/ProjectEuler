-module(p46).

-export([answer/0]).

%% It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
%%   9 = 7 + 2×1^2
%%  15 = 7 + 2×2^2
%%  21 = 3 + 2×3^2
%%  25 = 7 + 2×3^2
%%  27 = 19 + 2×2^2
%%  33 = 31 + 2×1^2
%% It turns out that the conjecture was false.
%% What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

answer() ->
    prime_server:start_link(1000000),
    OddComposites = odd_composites(3, prime_server:sieve(999999)),
    find_first(OddComposites).

find_first({O, _}) when O > 200000 -> io:format("Over 200000~n"),
                                      0;
find_first({O, N}) ->
    case is_goldbach(O) of
        true -> find_first(N());
        false -> O
    end.

odd_composites(Odd, Primes) ->
    case lists:member(Odd, Primes) of
        true ->
            odd_composites(Odd+2, Primes);
        false ->
            { Odd, fun() -> odd_composites(Odd+2, Primes) end}
    end.

is_goldbach(N) ->
    Primes = prime_server:sieve(N),
    is_goldbach(N, Primes, [ X*X*2 || X <- lists:seq(1, N-2) ] ).

is_goldbach(_, [], _) -> false;
is_goldbach(N, [P|Ps], Sqrs) ->
    case length([ X || X <- Sqrs, P+X =:= N ]) > 0 of
        true -> true;
        false -> is_goldbach(N, Ps, Sqrs)
    end.
            
