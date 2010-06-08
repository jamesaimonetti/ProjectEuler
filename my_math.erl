-module(my_math).

-export([gcd/1, gcd/2,
         lcm/1, lcm/2,
         perms/1, perms/2,
         powerset/1,
         fac/1,
         fib/1,
         fib_generator/0,
         d/1,
         modpow/3,
         totient/1,
         floor/1,
         ceiling/1,
         is_palindrome/1,
         is_pandigital/1,
         proper_divisors/1]).

-define(GOLD_RATIO, (1 + math:sqrt(5)) / 2).

%% Euler's Algorithm
gcd(A, 0) -> A;
gcd(A, B) when A < 0 orelse B < 0 -> gcd(abs(A), abs(B));
gcd(A, B) when A < B -> gcd(B, A);
gcd(A, B) -> gcd(B, A - B * (A div B)).

%% gcd(A, B, C) = gcd( gcd(a,b), c)
gcd([A, B | L]) ->
    lists:foldl(fun(X, Acc) -> gcd(X, Acc) end, gcd(A, B), L).

lcm(A, B) ->
    A * B div gcd(A, B).
%% lcm(A, B, C) = lcm( lcm(a,b), c)
lcm([A, B | L]) ->
    lists:foldl(fun(X, Acc) -> lcm(X, Acc) end, lcm(A, B), L).

%% the number of permutations of a set of N in a groups of R
perms(L, N) when is_list(L) ->
    lists:filter(fun(P) -> length(P) =:= N end, powerset(L));
perms(N, R) ->
    fac(N) div fac(N-R).

perms([]) -> [[]];
perms(L) ->
    [ [H|T] || H <- L, T <- perms(L--[H]) ].

%% powerset of [1,2,3] => [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]
powerset(L) ->
    N = length(L),
    Max = trunc(math:pow(2,N)),
    [[lists:nth(Pos+1,L) || Pos <- lists:seq(0,N-1),
                            I band (1 bsl Pos) =/= 0]
     || I <- lists:seq(0,Max-1)].

fac(X) ->
    fac(X, 1).

fac(0, F) -> F;
fac(1, F) -> F;
fac(X, F) -> fac(X-1, F*X).

%% restricted divisor function
d(N) -> lists:sum(proper_divisors(N)).

proper_divisors(N) -> [ X || X <- lists:seq(1, N-1), N rem X =:= 0].

%% returns the Nth fibonacci number where
%% {F1, 1}, {F2, 1}, {F3, 2}, {F4, 3},...
fib(Nth) ->
    round((math:pow(?GOLD_RATIO, Nth) - math:pow(( 1 - ?GOLD_RATIO ), Nth)) / math:sqrt(5)).

fib_generator() ->
    fib_generator(0, 1, 1).

fib_generator(A, B, N) ->
    [{N,B} | fun() -> fib_generator(B, A+B, N+1) end ].

%% finds C in the equation C ≡ B^E rem M
modpow(B, E, M) ->
    modpow(B, E, M, 1).

modpow(_B, E, _M, R) when E =< 0 -> R;
modpow(B, E, M, R) ->
    R1 = case E band 1 =:= 1 of
             true -> (R * B) rem M;
             false  -> R
         end,
    modpow( (B*B) rem M, E bsr 1, M, R1).

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

% Euler's phi-function
totient(1) -> 1;
totient(N) ->
    case primes:is_prime(N) of
        false ->
            Factors = primes:prime_factors(N),
            round(lists:foldl(fun({P, _C}, Acc) -> Acc * (1 - ( 1 / P ) ) end, N, Factors));
        true ->
            N-1
    end.

is_palindrome(N) when is_integer(N) ->
    is_palindrome(integer_to_list(N));
is_palindrome(L) ->
    L =:= lists:reverse(L).

is_pandigital(N) when length(N) =:= 9 ->
    lists:all(fun(D) -> lists:member(D, N) end, lists:seq($1, $9));
is_pandigital(_) -> false.
