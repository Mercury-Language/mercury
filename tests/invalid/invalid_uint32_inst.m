% Check for uint32 literals in inst definitions and the error messages
% concerning them.

:- module invalid_uint32_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 7u32,
    X = foo(N),
    io.print_line(X, !IO).

:- func foo(uint32::in(bound(2u32 ; 4u32 ; 6u32))) = (uint32::out) is det.

foo(2u32) = 4u32.
foo(4u32) = 9u32.
foo(6u32) = 12u32.
