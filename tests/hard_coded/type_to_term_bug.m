%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module type_to_term_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module queue.
:- import_module term.
:- import_module term_conversion.

main(!IO) :-
    queue.init(Q1),
    queue.put(1, Q1, _Q2),
    term_conversion.type_to_term(Q1, Term3),
    term.generic_term(Term3),
    io.write_line(Term3, !IO).
