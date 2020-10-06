%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test to check the proper functioning of the code in switch_detect.m
% that looks for switch unifications inside nested disjunctions.
%
% We also test that the compiler doesn't give a warning for the if-then-else
% condition containing X = h(I, F): that unification is equivalent to false
% in one arm of the generated switch and to true in the other, but it is
% not redundant in the source code.
%

:- module switch_detect.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module require.

:- type t
    --->    f
    ;       g(int)
    ;       h(int, float).

main(!IO) :-
    read_t(X, !IO),
    (
        X = f,
        io.write_string("f\n", !IO)
    ;
        ( X = g(_) ; X = h(_, _) ),
        io.write_line(X, !IO),
        ( if X = h(I, F) then
            io.write_string("h: ", !IO),
            io.write_int(I, !IO),
            io.write_string(" ", !IO),
            io.write_float(F, !IO),
            io.nl(!IO)
        else
            true
        )
    ),
    read_t(Y, !IO),
    (
        Y = f,
        Num = 42
    ;
        Z = Y,
        (
            Z = g(Num)
        ;
            W = Z,
            W = h(Num0, _),
            Num = Num0 + 5
        )
    ),
    io.write_int(Num, !IO),
    io.nl(!IO).

:- pred read_t(t::out, io::di, io::uo) is det.

read_t(X, !IO) :-
    io.read(Res, !IO),
    (
        Res = ok(X)
    ;
        Res = error(_, _),
        error("cannot read")
    ;
        Res = eof,
        error("eof")
    ).
