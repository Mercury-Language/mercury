%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_constraint_extra_var.
:- interface.

:- import_module io.
:- import_module list.

:- typeclass solver_for(B, S) where [
    func coerce(B) = S
].

:- pred main(io::di, io::uo) is det.

:- pred mg(T::in, T::out) is det <= solver_for(list(U), T).

:- implementation.
:- import_module float.
:- import_module std_util.

:- instance solver_for(list(T), float) where [
    coerce(_) = 42.0
].

main(!IO) :-
    mg(1.0, S),
    io.print_line(S, !IO).

mg(S0, S) :-
    ( if semidet_succeed then
        S = coerce([S0])
    else
        S = S0
    ).
