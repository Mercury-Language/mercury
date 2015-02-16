%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pragma_term.

:- interface.

:- pred a is det.

:- implementation.

:- pragma terminates(a/0).

a :- a.
