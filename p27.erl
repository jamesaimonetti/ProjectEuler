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

%% My Notes
%% From the forum, we learn that B must be prime (0^2+0*A+B = B) to start a sequence off
%% Next, we learn that A must be odd, so that N*N + A*N is always even, which, when summed with
%% B, will be odd - required to continue primality in the sequence.

answer() ->
    As = lists:seq(-999,999, 2),
    B = 971,
    Ps = lists:map(fun(A) -> { A, primes(0, A, B, 0) } end, As),
    {A, _Count } = lists:foldl(fun({_NewA, NewP}=New, {_CurA, CurP}=Cur) ->
                                       case NewP > CurP of
                                           true -> New;
                                           false -> Cur
                                       end
                               end, {0, 0}, Ps),
    A*B.

% count how many consecutive primes, starting at N=0
primes(N, A, B, P) ->
    Eq = round(abs(eq(N, A, B))),
    case primes:is_prime(Eq) of
        true -> primes(N+1, A, B, P+1);
        false -> P-1
    end.

eq(N, A, B) -> (N*N) + (A*N) + B.
    
