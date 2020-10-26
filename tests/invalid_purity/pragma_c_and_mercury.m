%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test that we report an error for procedures defined with both foreign and
% Mercury clauses, but without `pragma promise_pure'.

:- module pragma_c_and_mercury.

:- interface.

:- pred c_int_unify(int, int).

:- mode c_int_unify(in, out) is det.

:- mode c_int_unify(out, in) is det.

:- implementation.

:- pragma foreign_proc("C",
    c_int_unify(Int0::in, Int::out), [promise_pure], "Int = Int0;").

:- pragma foreign_proc("C",
    c_int_unify(Int::out, Int0::in), [promise_pure], "Int = Int0;").

:- pragma foreign_proc("Java",
    c_int_unify(Int0::in, Int::out), [promise_pure], "Int = Int0;").

:- pragma foreign_proc("Java",
    c_int_unify(Int::out, Int0::in), [promise_pure], "Int = Int0;").

:- pragma foreign_proc("C#",
    c_int_unify(Int0::in, Int::out), [promise_pure], "Int = Int0;").

:- pragma foreign_proc("C#",
    c_int_unify(Int::out, Int0::in), [promise_pure], "Int = Int0;").

c_int_unify(X, X).
