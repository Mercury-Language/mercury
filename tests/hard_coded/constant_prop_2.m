:- module constant_prop_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, int, float, private_builtin, std_util.

main -->
	( { "abc" ++ "xyz" = "abcxyz" } ->
		io.write_string("yes"), io.nl
	;
		link_error
	),
	( { 5 * 10000 + 4 * 1000 + 3 * 100 + 2 * 10 + 1 = 54321 } ->
		io.write_string("yes"), io.nl
	;
		link_error
	),
	( { 4.0 * 1000.0 + 3.0 * 100.0 + 2.0 * 10.0 + 1.0 = 4321.0 } ->
		io.write_string("yes"), io.nl
	;
		link_error
	),
	( { private_builtin.typed_unify(42, 42) } ->
		io.write_string("yes"), io.nl
	;
		link_error
	),
	( { private_builtin.typed_unify(1, 2) } ->
		link_error
	;
		io.write_string("no"), io.nl
	),
	( { private_builtin.typed_unify(43, X1) } ->
		io.write_int(X1), io.nl
	;
		link_error
	),
	( { private_builtin.typed_unify(44, _ `with_type` string) } ->
		link_error
	;
		io.write_string("no"), io.nl
	),
	( { std_util.dynamic_cast(45, X2) } ->
		io.write_int(X2), io.nl
	;
		link_error
	),
	( { std_util.dynamic_cast(46, _ `with_type` string) } ->
		link_error
	;
		io.write_string("no"), io.nl
	).

	% We should be able to optimize away all calls to this procedure
	% at compile time, so we should not even emit a reference to it.
:- pred link_error(io::di, io::uo) is det.
:- external(link_error/2).
