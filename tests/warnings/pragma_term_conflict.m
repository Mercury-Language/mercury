
	% Check termination pragmas do not conflict with each other when
	% analysing mutually recursive predicates.
:- module pragma_term_conflict.

:- interface.

:- pred a is det.

:- implementation.

:- pred b is det.

:- pred c is det.

:- pragma terminates(a/0).
:- pragma does_not_terminate(b/0).

a :- b.
b :- c.
c :- a.
