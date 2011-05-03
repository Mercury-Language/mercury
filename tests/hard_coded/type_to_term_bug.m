:- module type_to_term_bug.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module term.

:- import_module queue.

main -->
        { queue__init(Q1) },
        { queue__put(1, Q1, _Q2) },
        { term__type_to_term(Q1, Term3) },
        { term__generic_term(Term3) },
        write(Term3),
	nl.


