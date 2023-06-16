%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module promise_eqv_solns_typeclasses.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- typeclass foo(X) where [].
:- instance foo(int) where [].

main(!IO) :-
    ( if foo(2, N) then
        io.write_int(N, !IO),
        io.nl(!IO)
    else
        io.write_string("no solution\n", !IO)
    ).

:- pred foo(X::in, X::out) is semidet <= foo(X).

foo(X, Y) :-
    % The calls to reverse() are necessary so things aren't optimized away...
    promise_equivalent_solutions [Bar] (
        list.reverse([X], Bar)
    ),
    (
        Bar = [],
        false
    ;
        Bar = [Z | L],
        list.reverse(L, _),
        Y = Z
    ).
