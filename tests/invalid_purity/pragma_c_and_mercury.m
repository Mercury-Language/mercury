% A test that we report an error for procedures defined with both
% c_code and Mercury clauses, but without `pragma promise_pure'.

% At the time of writing, we fail this test in IL and Java grades.

:- module pragma_c_and_mercury.

:- interface.

:- pred c_int_unify(int, int).


:- implementation.

:- mode c_int_unify(in, out) is det.

:- mode c_int_unify(out, in) is det.

:- pragma(c_code, c_int_unify(Int0::in, Int::out), "Int = Int0;").

:- pragma(c_code, c_int_unify(Int::out, Int0::in), "Int = Int0;").

c_int_unify(X, X).
