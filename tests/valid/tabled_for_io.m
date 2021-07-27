%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test for the case where we have pragma(c_code, ...) decs for different
% modes of the same pred.

:- module tabled_for_io.

:- interface.

:- import_module io.

:- pred test(int::in, int::out, io::di, io::uo) is det.

:- implementation.

:- pragma foreign_proc("C",
    test(A::in, B::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
    B = A;
    IO = IO0;
").
test(X, X, !IO).
