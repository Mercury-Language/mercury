:- module constructor_warning.

:- interface.

:- type foo.

:- pred bar(foo::in) is semidet.

:- implementation.

:- type foo --->	foo_baz(int)
		;	foo_baz(int, int)
		;	fanta_fanta_fanta.


bar(foo_baz).
bar(fanta_fanta_fanta(1)).
