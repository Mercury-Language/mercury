% Check for int32 literals in inst definitions and the error messages
% concerning them.

:- module invalid_int32_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 7i32,
    X = foo(N),
    io.print_line(X, !IO).

:- func foo(int32::in(bound(2i32 ; 4i32 ; 6i32))) = (int32::out) is det.

foo(2i32) = 4i32.
foo(4i32) = 9i32.
foo(6i32) = 12i32.
