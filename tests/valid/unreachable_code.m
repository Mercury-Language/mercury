:- module unreachable_code.
:- interface.

:- type foo ---> foo.

:- pred p(foo::in, foo::in) is semidet.

:- implementation.
:- import_module require.

p(X, Y) :-
	error("err"),
	X = Y.

%-----------------------------------------------------------------------------%
