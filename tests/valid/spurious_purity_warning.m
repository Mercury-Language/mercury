:- module spurious_purity_warning.
:- interface.

:- impure pred foo(int::out) is det.
:- impure pred bar(int::in) is det.

:- implementation.
:- import_module require, std_util.

foo(X::out) :-
	( semidet_succeed ->
		error("foo/1")
	;
		X = 5
	).
bar(_::in) :-
	( semidet_succeed ->
		error("bar/1")
	;
		true
	).

:- pragma foreign_proc("C", foo(X::out),
	[will_not_call_mercury, thread_safe],
"
	X = 0;
").

:- pragma foreign_proc("C", bar(_X::in),
	[will_not_call_mercury, thread_safe],
"
").
