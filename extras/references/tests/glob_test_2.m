:- module glob_test_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module std_util, reference.

:- pragma c_header_code("
	#include ""c_reference.h""
").

:- pragma promise_pure(main/2).

main(!IO) :-
	(
		impure update(trailed_global, 2),
		semidet_fail
	->
		write_string("I didn't expect that to succeed!\n", !IO)
	;
		semipure value(trailed_global, Value),
		write_int(Value, !IO),
		nl(!IO)
	).

:- func trailed_global = reference(int).
:- pragma memo(trailed_global/0).
:- pragma promise_pure(trailed_global/0).

trailed_global = Ref :-
	impure new_reference(1, Ref).

