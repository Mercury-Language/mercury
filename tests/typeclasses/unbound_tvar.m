%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unbound_tvar.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(a(1)).

:- typeclass c(T) where [
    pred p(T),
    mode p(in) is det
].

:- type t(A, B)
    --->    a(A)
    ;       b(B).

:- instance c(t(A, B)) where [
    pred(p/1) is x
].

:- pred x(t(A, B)::in) is det.

x(_).
