%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module arbitrary_constraint_pred_1.
:- interface.

:- import_module float.
:- import_module io.
:- import_module string.

:- pred main(io::di, io::uo) is det.

:- typeclass solver_for(B, S) where [
    func coerce(B) = S
].

:- pred mg(T, T) <= solver_for(float, T).
:- mode mg(in, out) is det.

:- implementation.
:- import_module std_util.

main(!IO) :-
    mg("1.0", S),
    io.write_string(S, !IO),
    io.nl(!IO).

:- instance solver_for(float, string) where [
    coerce(Float) = string.float_to_string(Float)
].

mg(S0, S) :-
    ( if semidet_succeed then
        S = (coerce)(0.0)
    else
        S = S0
    ).
