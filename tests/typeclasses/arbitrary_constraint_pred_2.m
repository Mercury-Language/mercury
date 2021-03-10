%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module arbitrary_constraint_pred_2.
:- interface.

:- import_module float.
:- import_module io.
:- import_module list.

:- typeclass solver_for(B, S) where [
    func coerce(B) = S
].

:- pred mg(T, T) <= solver_for(list(T), T).
:- mode mg(in, out) is det.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util.

:- instance solver_for(list(T), float) where [
    coerce(_) = 42.0
].

mg(S0, S) :-
    ( semidet_succeed ->
        S = (coerce)([S0])
    ;
        S = S0
    ).

main -->
    { mg(1.0, S) },
    io__print(S),
    io__nl.
