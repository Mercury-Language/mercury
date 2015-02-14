%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lookup_switch_simple_opt.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module pair.
:- import_module string.

:- type e ---> e1 ; e2 ; e3 ; e4 ; e5 ; e6 ; e7.
:- type t ---> t(e, int).

main(!IO) :-
    test(t(e1, 11), !IO),
    test(t(e2, 12), !IO),
    test(t(e3, 13), !IO),
    test(t(e4, 14), !IO),
    test(t(e5, 15), !IO),
    test(t(e6, 16), !IO),
    test(t(e7, 17), !IO).

:- pred test(t::in, io::di, io::uo) is det.

test(T, !IO) :-
    p(T, N),
    io.write(T, !IO),
    io.write_string(" => ", !IO),
    io.write_int(N, !IO),
    io.write_string("\n", !IO).

:- pred p(t::in, int::out) is det.

p(T, N) :-
    T = t(E, _N),
    (
        E = e1,
        N = 1
    ;
        E = e2,
        N = 2
    ;
        E = e3,
        N = 3
    ;
        E = e4,
        N = 4
    ;
        E = e5,
        N = 5
    ;
        E = e6,
        N = 6
    ;
        E = e7,
        N = 7
    ).
