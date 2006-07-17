:- module pragma_c_code_no_det.

:- interface.

:- pred test(int::out) is det.

:- implementation.

test(Int) :-
	c_code(Int).

:- pred c_code(int::out).
:- pragma foreign_proc("C",
	c_code(X::out),
	[promise_pure, will_not_call_mercury],
"
	X = 1;
").
