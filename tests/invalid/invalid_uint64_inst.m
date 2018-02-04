% Check for uint64 literals in inst definitions and the error messages
% concerning them.

:- module invalid_uint64_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 7u64,
    X = foo(N),
    io.print_line(X, !IO).

:- func foo(uint64::in(bound(2u64 ; 4u64 ; 6u64))) = (uint64::out) is det.

foo(2u64) = 4u64.
foo(4u64) = 9u64.
foo(6u64) = 12u64.
