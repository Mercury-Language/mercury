
:- module impure_func_t4.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int, require.

:- pragma promise_pure(main/2).

main -->
	print("X = "), 
	{ X = get_counter },
	print(X), 
	nl.

:- semipure func get_counter = int is det.

:- pragma foreign_decl("C", "extern MR_Integer counter;").
:- pragma foreign_code("C", "MR_Integer counter = 0;").
:- pragma foreign_proc("C",
	get_counter = (X::out),
	[will_not_call_mercury, promise_semipure],
"
	X = counter;
").
get_counter = 0.
