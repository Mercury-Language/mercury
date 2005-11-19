:- module dummy_type_construct.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.

:- import_module construct, type_desc, list, string, deconstruct.

:- type dummy ---> dummy.

main(!IO) :-
	( DummyVal1 = construct.construct(type_of(_:dummy), 0, []) ->
		io.write(DummyVal1, !IO)
	;
		io.write_string("Unable to construct type.", !IO)
	),
	io.nl(!IO),
	( DummyVal2 = construct.construct(type_of(_:dummy), 1, []) ->
		io.write(DummyVal2, !IO)
	;
		io.write_string("Unable to construct type.", !IO)
	),
	io.nl(!IO),
	( get_functor_ordinal(type_of(_:dummy), 0, Ordinal1) ->
		io.write_int(Ordinal1, !IO)
	;
		io.write_string("get_functor_ordinal failed.", !IO)
	),
	io.nl(!IO),
	( get_functor_ordinal(type_of(_:dummy), 1, Ordinal2) ->
		io.write_int(Ordinal2, !IO)
	;
		io.write_string("get_functor_ordinal failed.", !IO)
	),
	io.nl(!IO),
	deconstruct.deconstruct(dummy, canonicalize, Functor, Arity, Args),
	io.format("Functor = %s, Arity = %i, Args = %s\n", [
		s(Functor), i(Arity), s(string.string(Args))], !IO).
