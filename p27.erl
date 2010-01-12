-module(p27).

-export([answer/0]).

%% Euler published the remarkable quadratic formula:
%% n² + n + 41
%% It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However,
%% when n = 40, 40^(2) + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41,
%% 41² + 41 + 41 is clearly divisible by 41.
%% Using computers, the incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for
%% the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.
%% Considering quadratics of the form:
%%    n² + an + b,
%% where |a| < 1000 and |b| < 1000 
%% where |n| is the modulus/absolute value of n
%%    e.g. |11| = 11 and |−4| = 4
%% 
%% Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum
%% number of primes for consecutive values of n, starting with n = 0.

answer() ->
    As = lists:seq(-999,999),
    Bs = lists:seq(-999,999),
    find_answer(As, Bs, {0, 0, 0}).

find_answer([], _Bs, {A, B, _PrimeCount}) -> A*B;
find_answer([A|As], Bs, Ans) ->
    Ps = lists:map(fun(B) -> {A, B, primes(0, A, B, 0)} end, Bs),
    NewAns = lists:foldl(fun({_A1, _B1, P1}=New, {_A2, _B2, P2}=Old) ->
                                 case P1 > P2 of
                                     true -> New;
                                     false -> Old
                                 end
                         end, Ans, Ps),
    find_answer(As, Bs, NewAns).

primes(N, A, B, P) ->
    Eq = round(abs(eq(N, A, B))),
    case primes:is_prime(Eq) of
        true -> primes(N+1, A, B, P+1);
        false -> P-1
    end.

eq(N, A, B) -> (N*N) + (A*N) + B.
    
