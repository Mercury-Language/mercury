% tests/valid/reserve_tag.m:
%	test some valid uses of the `:- reserve_tag' pragma.

:- module reserve_tag.
:- interface.
:- import_module int.

:- type exported_type ---> foo(int).
:- type exported_type2 ---> foo2(int).
:- type abstract_type.

:- pragma reserve_tag(exported_type2/0). % OK

:- func mkfoo(int) = exported_type.
:- func mkfoo2(int) = exported_type2.
:- func mkfoo3(int) = abstract_type.

:- implementation.
:- import_module list.

:- type abstract_type ---> foo3(int).
:- pragma reserve_tag(abstract_type/0).	 % OK

mkfoo(X) = foo(X).
mkfoo2(X) = foo2(X).
mkfoo3(X) = foo3(X).

:- pragma reserve_tag(expr/0).		 % OK
:- type expr
	--->	number(int)
	;	plus(expr, expr)
	;       minus(expr, expr)
	;       times(expr, expr)
	;       div(expr, expr).
