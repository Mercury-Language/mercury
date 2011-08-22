%-----------------------------------------------------------------------------%

:- module float_ground_term.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type mono
    --->    mono(float).

:- type list_of_mono
    --->    []
    ;       [mono | list_of_mono].

:- type poly(T)
    --->    poly(T).

main(!IO) :-
    T1 = [
        mono(1.1), mono(1.1), mono(1.1), mono(1.1), mono(1.1),
        mono(1.1), mono(1.1), mono(1.1), mono(1.1), mono(1.1),
        mono(1.1), mono(1.1), mono(1.1), mono(1.1), mono(1.1)
    ] : list(mono),
    T2 = [
        mono(2.2), mono(2.2), mono(2.2), mono(2.2), mono(2.2),
        mono(2.2), mono(2.2), mono(2.2), mono(2.2), mono(2.2),
        mono(2.2), mono(2.2), mono(2.2), mono(2.2), mono(2.2)
    ] : list_of_mono,
    T3 =[
        poly(3.3), poly(3.3), poly(3.3), poly(3.3), poly(3.3),
        poly(3.3), poly(3.3), poly(3.3), poly(3.3), poly(3.3),
        poly(3.3), poly(3.3), poly(3.3), poly(3.3), poly(3.3)
    ],
    io.write(T1, !IO),
    io.nl(!IO),
    io.write(T2, !IO),
    io.nl(!IO),
    io.write(T3, !IO),
    io.nl(!IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
