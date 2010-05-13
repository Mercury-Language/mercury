% vim: ts=4 sw=4 et ft=mercury

:- module primes.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    primes(limit, Out),
    print_list(Out, !IO).

:- func limit = int.

limit = 98.

:- pred primes(int::in, list(int)::out) is det.

primes(Limit, Primes) :-
    integers(2, Limit, Integers),
    sift(Integers, Primes).

:- pred integers(int::in, int::in, list(int)::out) is det.

integers(Low, High, Result) :-
    ( Low =< High ->
        NextLow = Low + 1,
        integers(NextLow, High, Rest),
        Result = [Low | Rest]
    ;
        Result = []
    ).

:- pred sift(list(int)::in, list(int)::out) is det.

sift([], []).
sift([Integer | Integers], [Integer | Ps]) :-
    remove_multiples(Integer, Integers, New),
    sift(New, Ps).

:- pred remove_multiples(int::in, list(int)::in, list(int)::out) is det.

remove_multiples(_Prime, [], []).
remove_multiples(Prime, [I | Is], Result) :-
    ( I mod Prime = 0 ->
        remove_multiples(Prime, Is, TailResult),
        Result = TailResult
    ;
        remove_multiples(Prime, Is, TailResult),
        Result = [I | TailResult]
    ).

:- pred print_list(list(int)::in, io::di, io::uo) is det.

print_list(Xs, !IO) :-
    (
        Xs = [],
        io.write_string("[]\n", !IO)
    ;
        Xs = [H | T],
        io.write_string("[", !IO),
        print_list_elements(H, T, !IO),
        io.write_string("]\n", !IO)
    ).

:- pred print_list_elements(int::in, list(int)::in, io::di, io::uo) is det.

print_list_elements(X, Xs, !IO) :-
    io.write_int(X, !IO),
    (
        Xs = []
    ;
        Xs = [H | T],
        io.write_string(", ", !IO),
        print_list_elements(H, T, !IO)
    ).
