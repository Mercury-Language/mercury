:- module empty_bound_inst_list.

:- interface.

:- pred p is failure.

:- implementation.
:- import_module maybe.

:- type some_functors ---> foo ; bar ; baz.

p :-
	( Y = yes(bar), Z = foo
	; Y = yes(baz), Z = foo
	),
	Y = yes(Z).

