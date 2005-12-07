:- module typed_unify.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module std_util.

main(!IO) :-
	U = univ(1),
	( type_to_univ(I, U) ->
		io.write_int(I, !IO)
	;
		true
	),
	nl(!IO).
