% A test for the case where we have pragma(c_code, ...) decs for different
% modes of the same pred.

:- module two_pragma_c_codes.

:- interface.

:- pred c_int_unify(int, int).


:- implementation.

:- mode c_int_unify(in, out) is det.

:- mode c_int_unify(out, in) is det.

:- pragma(c_code, c_int_unify(Int0::in, Int::out), "Int = Int0;").

:- pragma(c_code, c_int_unify(Int::out, Int0::in), "Int = Int0;").

c_int_unify(X, X).

