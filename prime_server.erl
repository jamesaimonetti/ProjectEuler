-module(prime_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, start_link/1, stop/0,
         sieve/1, factors/1, count_factors/1,
         iterator/0, is_prime/1, nth/1]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link(N) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [N], []).

%% return a sieve with primes =< N
sieve(N) -> gen_server:call(?MODULE, {sieve, N}).

%% return prime factors of N
factors(N) -> gen_server:call(?MODULE, {factors, N}).

%% return the count of factors of N
count_factors(N) -> gen_server:call(?MODULE, {count_factors, N}).

%% return an iterator (stream) of primes
iterator() -> gen_server:call(?MODULE, {iterator}).

%% is a number prime?
is_prime(N) -> gen_server:call(?MODULE, {is_prime, N}).

%% what is the nth prime?
nth(N) -> gen_server:call(?MODULE, {nth, N}).

stop() -> gen_server:cast(?MODULE, stop).

init([]) -> {ok, { 1000000, primes:queue(1000000)}};
init([N]) -> {ok, { N, primes:queue(N)}}.

handle_call({sieve, N}, _From, {UpTo, _Queue}) when N > UpTo ->
    NewQ = primes:queue(N),
    {reply, NewQ, {N, NewQ}};
handle_call({sieve, N}, _From, {_UpTo, Queue}=State) ->
    {reply, [ P || P <- Queue, P =< N], State};
handle_call({factors, N}, _From, {UpTo, _Queue}) when N div 2 > UpTo ->
    NewQ = primes:queue(N),
    Factors = primes:prime_factors(N, NewQ),
    {reply, Factors, {N, NewQ}};
handle_call({factors, N}, _From, {_UpTo, Queue}=State) ->
    Factors = primes:prime_factors(N, [ P || P <- Queue, P =< N div 2]),
    {reply, Factors, State};
handle_call({count_factors, N}, _From, {UpTo, _Queue}) when N div 2 > UpTo ->
    NewQ = primes:queue(N),
    Factors = primes:count_factors(N, NewQ),
    {reply, Factors, {N, NewQ}};
handle_call({count_factors, N}, _From, {_UpTo, Queue}=State) ->
    Factors = primes:count_factors(N, [ P || P <- Queue, P =< N div 2]),
    {reply, Factors, State};
handle_call({is_prime, N}, _From, {UpTo, _Queue}=State) when N > UpTo ->
    {reply, primes:is_prime(N), State};
handle_call({is_prime, N}, _From, {_UpTo, Queue}=State) ->
    {reply, lists:member(N, Queue), State};
handle_call({nth, N}, _From, State) ->
    {reply, primes:nth(N), State};
handle_call({iterator}, _From, State) ->
    {reply, primes:lazy_sieve(), State}. 

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
