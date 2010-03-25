-module(primes).

-export([queue/1,
         nth/1,
         is_prime/1,
         lazy_sieve/0,
         prime_factors/1,
         prime_factors/2,
         count_factors/1,
         count_factors/2
        ]).

%% use a priority queue, or skew heap, to store interators for primes
queue(2) -> [2];
queue(N) when N < 2 -> [];
queue(N) ->
    sieve_queue(lists:seq(2, N)).

sieve_queue([]) ->
    [];
sieve_queue([X|XS]) ->
    Table = insert_prime(X, skew_kv:empty()),
    [X | sieve_queue(XS, Table)].

insert_prime(P, Table) ->
    skew_kv:insert(P*P, from(P*P, P), Table).

sieve_queue([], _Table) ->
    [];
sieve_queue([X|XS], Table) ->
    {NextComposite, _Value} = skew_kv:min(Table),
    case  NextComposite =< X of
        true -> sieve_queue(XS, adjust(Table, X));
        _Else -> [X | sieve_queue(XS, insert_prime(X, Table))]
    end.

adjust(Table, X) ->
    {N, [Nprime | NS]} = skew_kv:min(Table),
    case N =< X of
        true ->
            T = skew_kv:delete_min(Table),
            T2 = skew_kv:insert(Nprime, NS(), T),
            adjust(T2, X);
        _Else -> Table
    end.

%% from http://www.erlang.org/cgi-bin/ezmlm-cgi?4:mss:177:khdaceipfabbicmifdhf
%% a lazy list that starts at K and increments by Inc
from(K, Inc) ->
    [ K | fun()-> from(K+Inc, Inc) end ].

%% prime number iterator
%% [ { Prime, Position } | Rest() ]
lazy_sieve() ->
    Table = insert_prime(2, skew_kv:empty()),
    [ { 2, 1 } | fun() -> lazy_sieve({ 3, 2 }, Table) end ].

lazy_sieve({ X, Pos }, Table) ->
    {NextComposite, _Value} = skew_kv:min(Table),
    case  NextComposite =< X of
        true -> lazy_sieve({X+1, Pos}, adjust(Table, X));
        _Else -> [ {X, Pos} | fun() -> lazy_sieve({X+1, Pos+1}, insert_prime(X,Table)) end]
    end.

%% returns the nth prime
%% nth(1) -> 2
%% nth(2) -> 3
%% nth(3) -> 5
nth(N) ->
    nth(lazy_sieve(), N).

% iterate through the lazy_sieve until the prime's position equals N
nth([ {Prime, N} | _Next], N) -> Prime;
nth([ {_Prime, _Pos} | Next], N) ->
    nth(Next(), N).

%% primality test of N
is_prime(2) -> true;
is_prime(3) -> true;
is_prime(N) when N rem 2 =:= 0 orelse N rem 3 =:= 0 -> false;
is_prime(N) ->
    Sqrt = round(math:sqrt(N)),
    is_prime(N, 1, Sqrt+1).

is_prime(N, K, Sqrt) when 6*K+1 =< Sqrt ->
    K1 = 6*K+1,
    K2 = 6*K-1,
    case N rem K1 =:= 0 orelse N rem K2 =:= 0 of
        true -> false;
        _Else -> is_prime(N, K+1, Sqrt)
    end;
is_prime(_N, _K, _Sqrt) -> true.

prime_factors(N, CandidatePrimes) ->
    PrimeFactors = [ X || X <- CandidatePrimes, N rem X =:= 0 ],
    find_factors(PrimeFactors, N, 0, []).

prime_factors(N) ->
    prime_factors(N, primes:queue(N div 2)).

count_factors(N, CandidatePrimes) ->
    length([ X || X <- CandidatePrimes, N rem X =:= 0 ]).

count_factors(N) ->
    count_factors(N, primes:queue(N div 2)).

%% X is a prime factor, N is what's left, C is the count for X, L is the prime factor and count of exponents
%% returns a list of primes and their exponents of the factorization
%% so if 108 factors to 2^2 * 3^3, first arg will be [2, 3] and return [{2,2},{3,3}]
find_factors([], _N, _C, L) -> lists:reverse(L);
find_factors([X | _T], 1, C, L) -> lists:reverse([{X, C} | L]);
find_factors([X | T], N, C, L) when N rem X =:= 0 ->
    find_factors([X | T], N div X, C+1, L);
find_factors([X | T], N, C, L) ->
    find_factors(T, N, 0, [{X, C} | L]).
