%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module repeated_singleton.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    test_a({1, "foo"}, {2, "bar"}, !IO),
    test_a({1, "foo"}, {2, "foo"}, !IO),
    test_a({11, "foo"}, {2, "bar"}, !IO),
    test_b({1, "foo"}, {2, "bar"}, !IO),
    test_b({1, "foo"}, {2, "foo"}, !IO),
    test_b({11, "foo"}, {2, "bar"}, !IO).

:- pred test_a({int, T}::in, {int, T}::in, io::di, io::uo) is det.

test_a(A, B, !IO) :-
    ( if
        A = {AInt, _AT},
        % This scope should NOT disable the warning for "_AT".
        disable_warning [singleton_vars] ( B = {BInt, _AT} ),
        CInt = AInt + BInt,
        CInt < 10
    then
        AStr = string.string(A),
        BStr = string.string(B),
        io.format("%s %s %d\n", [s(AStr), s(BStr), i(CInt)], !IO)
    else
        io.write_string("failed\n", !IO)
    ).

:- pred test_b({int, T}::in, {int, T}::in, io::di, io::uo) is det.

test_b(A, B, !IO) :-
    ( if
        A = {AInt, _AT},
        % This scope SHOULD disable the warning for "_AT".
        disable_warning [repeated_singleton_vars] ( B = {BInt, _AT} ),
        CInt = AInt + BInt,
        CInt < 10
    then
        AStr = string.string(A),
        BStr = string.string(B),
        io.format("%s %s %d\n", [s(AStr), s(BStr), i(CInt)], !IO)
    else
        io.write_string("failed\n", !IO)
    ).
