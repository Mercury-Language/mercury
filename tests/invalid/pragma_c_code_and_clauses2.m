:- module pragma_c_code_and_clauses2.

:- interface.

:- pred foo(int::in) is multidet.

:- pragma(c_code, foo(X::in), "some_function(X);").

foo(42).
