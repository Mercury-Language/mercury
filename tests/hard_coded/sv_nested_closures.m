%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module sv_nested_closures.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    some [!X] (
        !:X = 10,
        Foo =
            ( pred(I::in, !.IO::di, !:IO::uo) is det :-
                Bar =
                    ( pred(J::in, !.IO::di, !:IO::uo) is det :-
                        Result = J + I + !.X,
                        io.write_int(Result, !IO),
                        io.nl(!IO)
                    ),
                Bar(4, !IO)
            ),
        Foo(3, !IO)
    ),
    baz(-200, X, !IO),
    io.write_int(X, !IO),
    io.nl(!IO).

:- pred baz(int::in, int::out, io::di, io::uo) is det.

baz(!Y, !IO) :-
    io.format("(baz) !.Y = %d\n", [i(!.Y)], !IO),
    BazFoo =
        ( pred(!.Y::in, !:Y::out, !.IO::di, !:IO::uo) is det :-
            io.format("(BazFoo) !.Y = %d\n", [i(!.Y)], !IO),
            BazBar =
                ( pred(!.Z::in, !:Z::out, !.IO::di, !:IO::uo) is det :-
                    io.format("(BazBar) !.Y = %d\n", [i(!.Y)], !IO)
                ),
            BazBar(400, _, !IO)
        ),
    BazFoo(700, _, !IO).
