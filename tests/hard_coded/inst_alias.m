%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module inst_alias.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module string.

main(!IO) :-
    p1(1, T1),
    q(T1, S1),
    io.write_string(S1, !IO),
    io.nl(!IO),

    p2("abc", T2),
    q(T2, S2),
    io.write_string(S2, !IO),
    io.nl(!IO).

:- type t
    --->    f1(int)
    ;       f2(string).

:- inst i1 == i2.

:- inst i2
    --->    f1(ground)
    ;       f2(i3).

:- inst i3 == i4.

:- inst i4
    --->    "abc".

:- pred p1(int::in, t::out(i1)) is det.

p1(N, f1(N)).

:- pred p2(string::in(i3), t::out(i1)) is det.

p2(S, f2(S)).

:- pred q(t::in(i1), string::out) is det.

q(T, S) :-
    (
        T = f1(N),
        string.int_to_string(N, S)
    ;
        T = f2(S)
    ).
