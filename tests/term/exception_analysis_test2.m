:- module exception_analysis_test2.
:- interface.

	% Test for correct handling of enumerations with
	% user-defined equality.
	%
:- pred test1(foo::in, foo::in) is semidet.

:- pred test2(bar::in, bar::in) is semidet.

:- type foo
	---> 	foo0
	;	foo1
	;	foo2
	;	foo3
	where equality is foo_equals.

:- pred my_unify(T::in, T::in) is semidet.

:- pred foo_equals(foo::in, foo::in) is semidet.

:- type bar
	--->	bar1
	;	bar2(int)
	;	bar3(foo).

:- implementation.

:- import_module exception.

test1(A, B) :- my_unify(A, B).
test2(A, B) :- my_unify(A, B).

my_unify(A, B) :- unify(B, A).

foo_equals(_, _) :-
	throw("Cannot compare values of type foo").
