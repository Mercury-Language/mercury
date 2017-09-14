% Check for uint8 literals in inst definitions and the error messages concerning
% them.

:- module invalid_uint8_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 7u8,
    X = foo(N),
    io.print_line(X, !IO).

:- func foo(uint8::in(bound(2u8 ; 4u8 ; 6u8))) = (uint8::out) is det.

foo(2u8) = 4u8.
foo(4u8) = 9u8.
foo(6u8) = 12u8.
