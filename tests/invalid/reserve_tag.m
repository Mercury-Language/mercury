% tests/invalid/reserve_tag.m:
%	test some invalid uses of the `:- reserve_tag' pragma.

:- module reserve_tag.
:- interface.
:- import_module int.

:- type exported_type ---> foo(int).
:- type abstract_type.

:- type exported_type2 ---> foo(int).
:- pragma reserve_tag(exported_type2/0). % OK

:- func mkfoo(int) = exported_type2.

:- implementation.
:- import_module list.

mkfoo(X) = foo(X).

:- pragma reserve_tag(undefined_type/1). % error: undefined type

:- pragma reserve_tag(list__list / 1).   % error: syntax error
:- pragma reserve_tag(list / 1).   	 % error: undefined type
:- pragma reserve_tag(exported_type/0).  % error: visibility mismatch

:- type abstract_type ---> foo(int).
:- pragma reserve_tag(abstract_type/0).	 % OK

:- type invalid_arity ---> invalid_arity.
:- type invalid_arity(T1, T2) ---> another_invalid_arity.
:- pragma reserve_tag(invalid_arity/1).	 % error: undef type (incorrect arity)

:- pragma reserve_tag(expr/0).		 % OK
:- type expr
	--->	number(int)
	;	plus(expr, expr)
	;       minus(expr, expr)
	;       times(expr, expr)
	;       div(expr, expr).

:- pragma reserve_tag(foo/0).	 % OK.
:- type foo ---> foo.
:- pragma reserve_tag(foo/0).	 % warning: duplicate pragma

