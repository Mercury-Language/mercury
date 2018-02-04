% Check for int64 literals in inst definitions and the error messages
% concerning them.

:- module invalid_int64_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 7i64,
    X = foo(N),
    io.print_line(X, !IO).

:- func foo(int64::in(bound(2i64 ; 4i64 ; 6i64))) = (int64::out) is det.

foo(2i64) = 4i64.
foo(4i64) = 9i64.
foo(6i64) = 12i64.
