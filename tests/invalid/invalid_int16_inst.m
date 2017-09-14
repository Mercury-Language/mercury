% Check for int16 literals in inst definitions and the error messages
% concerning them.

:- module invalid_int16_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 7i16,
    X = foo(N),
    io.print_line(X, !IO).

:- func foo(int16::in(bound(2i16 ; 4i16 ; 6i16))) = (int16::out) is det.

foo(2i16) = 4i16.
foo(4i16) = 9i16.
foo(6i16) = 12i16.
