%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Check that pragmas that assert facts about the termination of
% mutually recursive predicates do not conflict with each other.
% Also check that we get a warning for a decl pragma for an exported predicate
% that is itself not exported.
%---------------------------------------------------------------------------%

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
