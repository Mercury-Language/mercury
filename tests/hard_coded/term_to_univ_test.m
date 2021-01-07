%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module term_to_univ_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module term.
:- import_module term_conversion.
:- import_module term_io.
:- import_module univ.
:- import_module varset.

main(!IO) :-
    X = 4,
    type_to_univ(X, Univ0),
    type_to_univ(Univ0, Univ),
    univ_to_term(Univ, Term),
    io.write_line(Term, !IO),
    varset.init(VarSet),
    write_term(VarSet, Term, !IO),
    io.nl(!IO).
