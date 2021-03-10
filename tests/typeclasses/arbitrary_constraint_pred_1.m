%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module arbitrary_constraint_pred_1.
:- interface.

:- import_module float.
:- import_module io.
:- import_module string.

:- typeclass solver_for(B, S) where [
    func coerce(B) = S
].

:- pred mg(T, T) <= solver_for(float, T).
:- mode mg(in, out) is det.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util.

:- instance solver_for(float, string) where [
    coerce(Float) = string__float_to_string(Float)
].

mg(S0, S) :-
    ( semidet_succeed ->
        S = (coerce)(0.0)
    ;
        S = S0
    ).

main -->
    { mg("1.0", S) },
    io__write_string(S),
    io__nl.
