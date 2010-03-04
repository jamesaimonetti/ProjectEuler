-module(p48).

-export([answer/0]).

%% The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
%% Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.


-compile(export_all).

answer() -> 
    TenTenth = round(math:pow(10,10)),
    lists:sum([ my_math:modpow(N, N, TenTenth) || N <- lists:seq(1,1000)]) rem TenTenth.

