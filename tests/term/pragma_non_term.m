%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pragma_non_term.

:- interface.

:- pred a is det.

:- implementation.

:- pragma does_not_terminate(a/0).

a :- true.
