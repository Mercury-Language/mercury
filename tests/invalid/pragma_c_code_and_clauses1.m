:- module pragma_c_code_and_clauses1.

:- interface.

:- pred foo(int::in) is multidet.

foo(42).

:- pragma(c_code, foo(X::in), "some_function(X);").
