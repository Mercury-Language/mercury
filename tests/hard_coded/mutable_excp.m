% Test mutables whose initial value expressions throw exceptions.
% 
:- module mutable_excp.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module exception. 

:- mutable(foo, int, init_foo, ground, [untrailed, attach_to_io_state]).
:- mutable(bar, int, init_bar, ground, [untrailed, attach_to_io_state]).
:- mutable(baz, int, init_baz, ground, [untrailed, attach_to_io_state]).

:- func init_foo = int.

init_foo = X :-
	promise_equivalent_solutions [X] (
		try(get_string(561), Result),
		(
			Result = succeeded(_),
			X = 80
		;
			Result = exception(_),
			X = 40
		),
		trace [io(!IO)] (
			io.write_string("init_foo: X = ", !IO),
			io.write_int(X, !IO),
			io.nl(!IO),
			io.flush_output(!IO)
		)
	).

:- func init_bar = int.

init_bar = 
	( get_magic_number(3) ->
		throw(magic_number_exception)
	;
		561
	).

:- func init_baz = int.

init_baz = X :-
	trace [io(!IO)] (
	    io.write_string("init_baz: I should not be running.\n", !IO)
	),
	X = 10.

:- pred get_string(int::in, string::out) is det.

get_string(X, Str) :-
	( X = 561 ->
		throw(magic_number_exception)
	;
		Str = "not 561"
	).	

:- type magic_number_exception ---> magic_number_exception.

:- pred get_magic_number(int::out) is det.

get_magic_number(3).

main(!IO) :-
	io.write_string("main: I should not be running.\n", !IO).
