%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% rotd-2006-09-18 and before would not compile the following because
% the fact that the inst varsets attached to the clauses and the mode
% declarations are different meant that it couldn't work out which mode
% belonged to which clause.  The fix is to allow for a renaming between
% inst variables.
%
% A similar thing occurs with the inst varset attached to foreign_export
% pragmas.
%
:- module param_mode_bug.
:- interface.

:- type list(T)
    --->    []
    ;       [T | list(T)].

:- pred foo(list(T), list(T)).
:- mode foo(in(I),  out(I)) is det.
:- mode foo(out(I), in(I)) is det.

:- pred bar(list(T)::in(I), list(T)::out(I)) is det.

:- implementation.

:- pragma promise_equivalent_clauses(foo/2).

foo(X::in(I), X::out(I)).
foo(Y::out(I), X::in(I)) :-
    foo_2(Y, X).

:- pred foo_2(list(T)::out(I), list(T)::in(I)) is det.
:- pragma foreign_proc("C",
    foo_2(Y::out(I), X::in(I)),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    Y = X;
").
:- pragma foreign_proc("Java",
    foo_2(Y::out(I), X::in(I)),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    Y = X;
").

:- pragma foreign_export("C", bar(in(I), out(I)), "BAR").
bar(X, X).
