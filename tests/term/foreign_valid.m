:- module foreign_valid.

:- interface.

:- import_module int.

:- pred test1(int::out) is det.
:- pred test2(int::out) is det.
:- pred test3(int::out) is det.
:- pred test4(int::out) is det.
:- pred test5(int::out) is det.
:- pred test6(int::out) is det.
:- pred test7(int::out) is det.
:- pred test8(int::out) is det.
:- pred test9(int::out) is det.
:- pred test10(int::out) is det.

:- implementation.

:- pragma foreign_proc("C", test1(X::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	X = (MR_Integer) 3;
").

:- pragma foreign_proc("C", test2(X::out),
	[may_call_mercury, promise_pure, thread_safe],
"
	X = (MR_Integer) 3;
").

:- pragma foreign_proc("C", test3(X::out),
	[will_not_call_mercury, promise_pure, thread_safe, does_not_terminate], "
	X = (MR_Integer) 3;
").

:- pragma foreign_proc("C", test4(X::out),
	[may_call_mercury, promise_pure, thread_safe, terminates],
"
	X = (MR_Integer) 3;
").

:- pragma foreign_proc("C", test5(X::out),
	[will_not_call_mercury, promise_pure, thread_safe, terminates],
"
	X = (MR_Integer) 3;
").

:- pragma foreign_proc("C", test6(X::out),
	[may_call_mercury, promise_pure, thread_safe, does_not_terminate],
"
	X = (MR_Integer) 3;
").

:- pragma terminates(test7/1).
:- pragma foreign_proc("C", test7(X::out),
	[may_call_mercury, promise_pure, thread_safe],
"
	X = (MR_Integer) 3;
").

:- pragma does_not_terminate(test8/1).
:- pragma foreign_proc("C", test8(X::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	X = (MR_Integer) 3;
").

:- pragma terminates(test9/1).
:- pragma foreign_proc("C", test9(X::out),
	[may_call_mercury, promise_pure, thread_safe, terminates],
"
	X = (MR_Integer) 3;
").

:- pragma does_not_terminate(test10/1).
:- pragma foreign_proc("C", test10(X::out),
	[will_not_call_mercury, promise_pure, thread_safe, does_not_terminate], "
	X = (MR_Integer) 3;
").

:- end_module foreign_valid.
