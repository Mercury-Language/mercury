:- module use_abstract_instance.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module abstract_instance, list.

main -->
	run(42),
	run("hello world"),
	run([5,4,3,2,1]),
	run(["hello", "world"]),
	run([[[[0]]]]).
