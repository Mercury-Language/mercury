%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module abstract_typeclass.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module abstract_typeclass_helper_1.
:- import_module list.

main(!IO) :-
    p(43, !IO),
    p("Forty-three", !IO),
    p([43], !IO),
    p([[[], [43]]], !IO),
    q(X),
    p(X, !IO).
