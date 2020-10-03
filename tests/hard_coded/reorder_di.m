%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% regression test - Mercury 0.7 failed this test
%

:- module reorder_di.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module require.

main(!IO) :-
    r(Y),
    q(X),
    io.write_string(Str, !IO),
    ( if Y = 1 then
        io.write_string(Str, !IO),
        io.write_string("quux\n", !IO)
    else
        error("qux")
    ),
    io.write_string("bar\n", !IO),
    ( X = 1, Str = "foo\n"
    ; X = 2, Str = "baz\n"
    ).

:- pred q(int::(free >> bound(1 ; 2))) is det.

q(1).

:- pred r(int::(free >> bound(1 ; 2))) is det.

r(1).
