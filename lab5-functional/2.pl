% prolog

merge(IN1, IN2, OUT):-
    freeze(IN1,  % odraczasmy wykononanie do czasu gdy IN1 przyjmuje wartość
    (   IN1 = [H1 | T1]
    -> freeze(IN2,
        (   IN2 = [H2 | T2]
        -> ( H1 =< H2
            -> OUT = [H1 | R],
               merge(T1, IN2, R)
            ;  OUT = [H2 | R],
               merge(IN1, T2, R)
            )
        ;   OUT = IN1
        ))
    ;   OUT = IN2
    )).

split(IN, OUT1, OUT2):-
    freeze(IN,
    (   IN = [H | T]
    ->  (   OUT1 = [H | T1],
            split(T, OUT2, T1)
        )
    ;   (
            OUT1 = [],
            OUT2 = []
        )
    )).


merge_sort(IN, OUT):-
    freeze(IN,
    (   IN = [_ | T]
        ->  freeze(T,
            (   T = [_ | _]
            ->  (   split(IN, X1, X2),
                    merge_sort(X1, Y1),
                    merge_sort(X2, Y2),
                    merge(Y1, Y2, OUT)
                )
            ;   IN = OUT
            ))
        ;   OUT = []
    )).

%%%

extended_gcd(A, 0, 1, 0, A).
extended_gcd(A, B, X, Y, G) :-
    B \= 0,
    R is A mod B,
    Q is A // B,
    extended_gcd(B, R, X1, Y1, G),
    X is Y1,
    Y is X1 - Q * Y1.

de(A, B, X, Y, Z) :-
    extended_gcd(A, B, X, Y, Z).


prime_factors(N, X) :- 
    prime_factors(N, 2, X).

prime_factors(1, _, []).

prime_factors(N, Current, [Current | Fs]) :-
    N > 1,
    0 is N mod Current,
    NextN is N // Current,
    prime_factors(NextN, Current, Fs).

prime_factors(N, Current, Fs) :- 
    N > 1,
    N mod Current =\= 0,
    NextCurrent is Current + 1,
    prime_factors(N, NextCurrent, Fs).

%%%

gcd(0, B, B) :- B > 0.
gcd(A, 0, A) :- A > 0.
gcd(A, B, G) :- 
    A > 0, B > 0, 
    R is A mod B, 
    gcd(B, R, G).

relatively_prime(A, B) :-
    gcd(A, B, 1).

totient(N, T) :- 
    N > 0,
    totient(N, N, 0, T).

totient(_, 0, Acc, Acc).
totient(N, I, Acc, T) :-
    I > 0,
    (relatively_prime(N, I) -> Acc1 is Acc + 1 ; Acc1 is Acc),
    I1 is I - 1,
    totient(N, I1, Acc1, T).

sieve([], []).
sieve([P|Xs], [P|Ys]) :-
    exclude(multiple_of(P), Xs, Zs),
    sieve(Zs, Ys).

multiple_of(P, X) :-
    X mod P =:= 0.

numlist(2, N, List) :- findall(X, between(2, N, X), List).

primes(N, Primes) :-
    N >= 2,
    numlist(2, N, List),
    sieve(List, Primes).
