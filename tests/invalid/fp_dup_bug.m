% rotd-2007-02-06 and before silently ignored the face that
% there were multiple foreign clauses for the same language. 
:- module fp_dup_bug.
:- interface.

:- pred foo(int::in, int::out) is det. 

:- implementation.

:- pragma foreign_proc("C",
     foo(X::in, Y::out),
     [will_not_call_mercury, promise_pure],
"
	X = Y;
").

:- pragma foreign_proc("C",
     foo(X::in, Y::out),
     [will_not_call_mercury, promise_pure],
"
	X = Y;
").

:- pragma foreign_proc("C",
     foo(X::in, Y::out),
     [will_not_call_mercury, promise_pure],
"
	X = Y;
").
