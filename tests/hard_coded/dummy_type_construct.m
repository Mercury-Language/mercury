%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module dummy_type_construct.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module construct.
:- import_module deconstruct.
:- import_module list.
:- import_module string.
:- import_module type_desc.

:- type dummy
    --->    dummy.

main(!IO) :-
    ( if DummyVal1 = construct.construct(type_of(_:dummy), 0, []) then
        io.write(DummyVal1, !IO)
    else
        io.write_string("Unable to construct type.", !IO)
    ),
    io.nl(!IO),
    ( if DummyVal2 = construct.construct(type_of(_:dummy), 1, []) then
        io.write(DummyVal2, !IO)
    else
        io.write_string("Unable to construct type.", !IO)
    ),
    io.nl(!IO),
    ( if get_functor_ordinal(type_of(_:dummy), 0, Ordinal1) then
        io.write_int(Ordinal1, !IO)
    else
        io.write_string("get_functor_ordinal failed.", !IO)
    ),
    io.nl(!IO),
    ( if get_functor_ordinal(type_of(_:dummy), 1, Ordinal2) then
        io.write_int(Ordinal2, !IO)
    else
        io.write_string("get_functor_ordinal failed.", !IO)
    ),
    io.nl(!IO),
    deconstruct.deconstruct(dummy, canonicalize, Functor, Arity, Args),
    io.format("Functor = %s, Arity = %i, Args = %s\n",
        [s(Functor), i(Arity), s(string.string(Args))], !IO).
