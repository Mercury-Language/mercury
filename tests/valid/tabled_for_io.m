% A test for the case where we have pragma(c_code, ...) decs for different
% modes of the same pred.

:- module tabled_for_io.

:- interface.

:- import_module io.

:- pred test(int::in, int::out, io__state::di, io__state::uo) is det.

:- implementation.

:- pragma c_code(test(A::in, B::out, IO0::di, IO::uo),
	[will_not_call_mercury, tabled_for_io],
"
	B = A;
	IO = IO0;
").
test(X, X) --> [].
