:- module type_spec_modes.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.

:- pred my_unify(T, T).
:- mode my_unify(in, in) is semidet.
:- mode my_unify(in, out) is det.
:- mode my_unify(out, in) is det.
:- pragma type_spec(my_unify(in, in), T = list(int)).
:- pragma type_spec(my_unify(in, in), T = int).
:- pragma type_spec(my_unify(in, out), T = int).
:- pragma type_spec(my_unify(out, in), T = list(int)).

:- pragma no_inline(my_unify/2).

my_unify(X, X).

main -->
	( { my_unify(1, 1) } ->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	),
	( { my_unify([1, 2, 3], [1, 2, 6]) } ->
		io__write_string("no\n")
	;
		io__write_string("yes\n")
	),
	{ my_unify(X, [1, 2, 3]) },
	( { X = [1, 2, 3] } ->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

