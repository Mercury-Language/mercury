:- module non_term_user_special.

:- interface.

:- import_module list.

:- type myset(T) ---> myset(list(T))
	where equality is my_set_equals,
	comparison is my_set_compare.

:- pred my_set_equals(myset(T)::in, myset(T)::in) is semidet.

:- pred my_set_compare(builtin.comparison_result::uo, myset(T)::in,
	myset(T)::in) is det.

:- solver type foo(T).

:- implementation.

my_set_equals(_, _) :- loop.

:- pred loop is semidet.

loop :- a.

:- pred a is semidet.
:- pred b is semidet.

a :- b.
b :- a.

my_set_compare(Res, _, _) :- 
	( loop ->
		Res = (=)
	;
		Res = (=)
	).

:- solver type foo
	where	representation is int,	
		initialisation is init_foo,
		ground         is ground,
		any            is ground.

:- pragma promise_pure(init_foo/1).
:- pred init_foo(foo::out(any)) is det.
init_foo(X) :-
	( loop ->
		Y = 42
	;	
		Y = 43
	),
	impure X = 'representation to any foo/0'(Y).
