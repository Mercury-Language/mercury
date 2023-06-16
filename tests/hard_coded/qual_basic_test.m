%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test to ensure qualified predicates, function calls
% and higher-order constants are parsed correctly.

:- module qual_basic_test.

:- interface.

:- import_module io.

:- pred qual_basic_test.main(io.state::di, io.state::uo) is det.

:- implementation.

:- import_module int.

qual_basic_test.main(!IO) :-
    io.write_string("Gotcha1!\n", !IO),
    A = qual_basic_test.test,
    X = int.(A + 2),
    io.write_int(X, !IO),
    io.write_string("\n", !IO),
    Pred = int.max,
    call(Pred, 1, 2, Y),
    io.write_int(Y, !IO),
    io.write_string("\n", !IO).

:- func test = int.

test = 2.
