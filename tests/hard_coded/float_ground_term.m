%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module float_ground_term.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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
    io.write_line(T1, !IO),
    io.write_line(T2, !IO),
    io.write_line(T3, !IO).
