
:- module impure_func_t2.

:- interface.

:- import_module io.

:- pred main(state::di, state::uo) is det.

:- implementation.
:- import_module int, require.

:- pragma promise_pure(main/2).

main -->
	{ X = get_counter },
	print("X = "), 
	print(X), 
	nl.

:- impure func get_counter = int is det.

:- pragma c_header_code("extern Integer counter;").
:- pragma c_code("Integer counter = 0;").
:- pragma c_code(get_counter = (X::out), will_not_call_mercury, "X = counter;").
get_counter = 0.
