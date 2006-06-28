:- module indep_par_nested.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    (
        A = 100,
        (
            B = A + 1
        &
            C = A + 2
        &
            D = A + 3
        )
    &
        E = 200,
        (
            F = E + 1
        &
            G = E + 2
        &
            H = E + 3
        )
    &
        I = 300,
        (
            J = I + 1
        &
            K = I + 2
        &
            L = I + 3
        )
    ),
    io.print({A,B,C,D,E,F,G,H,I,J,K,L}, !IO),
    io.nl(!IO).
