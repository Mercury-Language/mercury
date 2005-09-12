% A test for the case where we have pragma(c_code, ...) decs for different
% modes of the same pred.

:- module two_pragma_c_codes.

:- interface.

:- pred c_int_unify(int, int).

:- implementation.

:- mode c_int_unify(in, out) is det.

:- mode c_int_unify(out, in) is det.

:- pragma promise_pure(c_int_unify/2).

:- pragma foreign_proc("C",
	c_int_unify(Int0::in, Int::out),
	[promise_pure, will_not_call_mercury],
"
	Int = Int0;
").

:- pragma foreign_proc("C",
	c_int_unify(Int::out, Int0::in),
	[promise_pure, will_not_call_mercury],
"	
	Int = Int0;
").

c_int_unify(X, X).

