% Check for uint16 literals in inst definitions and the error messages concerning
% them.

:- module invalid_uint16_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 7u16,
    X = foo(N),
    io.print_line(X, !IO).

:- func foo(uint16::in(bound(2u16 ; 4u16 ; 6u16))) = (uint16::out) is det.

foo(2u16) = 4u16.
foo(4u16) = 9u16.
foo(6u16) = 12u16.
