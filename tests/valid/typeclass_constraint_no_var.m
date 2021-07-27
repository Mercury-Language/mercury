%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_constraint_no_var.
:- interface.

:- import_module float.
:- import_module io.
:- import_module list.

:- typeclass solver_for(B, S) where [
    func coerce(B) = S
].

:- pred mg(T, T) <= solver_for(list(float), float).
:- mode mg(in, out) is det.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util.

:- instance solver_for(list(T), float) where [
    coerce(_) = 42.0
].

main(!IO) :-
    mg(1.0, S),
    io.print_line(S, !IO).

mg(S0, S) :-
    ( if semidet_succeed then
        S = S0
    else
        S = S0
    ).
