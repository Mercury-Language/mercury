:- module spurious_purity_warning.
:- interface.

:- impure pred foo(int::out) is det.
:- impure pred bar(int::in) is det.

:- implementation.
:- import_module require.

foo(_::out) :-
	error("foo/1").
bar(_::in) :-
	error("bar/1").

:- pragma foreign_proc("C", foo(X::out),
	[will_not_call_mercury, thread_safe],
"
	X = 0;
").

:- pragma foreign_proc("C", bar(_X::in),
	[will_not_call_mercury, thread_safe],
"
").
