:- module promise_will_not_throw.

:- interface.

:- pred foo(int::in, int::out) is det.

:- pred bar(int::in, int::out) is det.

:- pred baz(int::in, int::out) is det.

:- pred quux(int::in, int::out) is det.

:- implementation.

:- pragma foreign_proc("C",
	foo(X::in, Y::out),
	[may_call_mercury, promise_pure, will_not_throw_exception],
"
	X = Y;
").

:- pragma foreign_proc("C",
	bar(X::in, Y::out),
	[may_call_mercury, promise_pure],
"
	X = Y;
").

:- pragma foreign_proc("C",
	baz(X::in, Y::out),
	[will_not_call_mercury, promise_pure, will_not_throw_exception],
"
	X = Y;
").

:- pragma foreign_proc("C",
	quux(X::in, Y::out),
	[will_not_call_mercury, promise_pure],
"
	X = Y;
").
