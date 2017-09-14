% Check for int8 literals in inst definitions and the error messages concerning
% them.

:- module invalid_int8_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 7u8,
    X = foo(N),
    io.print_line(X, !IO).

:- func foo(int8::in(bound(2i8 ; 4i8 ; 6i8))) = (int8::out) is det.

foo(2i8) = 4i8.
foo(4i8) = 9i8.
foo(6i8) = 12i8.
