% Check for uint literals in inst definitions and the error messages concerning
% them.

:- module invalid_uint_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    N = 7u,
    X = foo(N),
    io.print_line(X, !IO).

:- func foo(uint::in(bound(2u ; 4u ; 6u))) = (uint::out) is det.

foo(2u) = 4u.
foo(4u) = 9u.
foo(6u) = 12u.
