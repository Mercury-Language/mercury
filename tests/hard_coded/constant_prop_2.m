%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module constant_prop_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module float.
:- import_module int.
:- import_module string.
:- import_module uint.

main(!IO) :-
    ( if "abc" ++ "xyz" = "abcxyz" then
        io.write_string("yes", !IO), io.nl(!IO)
    else
        link_error(!IO)
    ),
    ( if 5 * 10000 + 4 * 1000 + 3 * 100 + 2 * 10 + 1 = 54321 then
        io.write_string("yes", !IO), io.nl(!IO)
    else
        link_error(!IO)
    ),
    ( if 4.0 * 1000.0 + 3.0 * 100.0 + 2.0 * 10.0 + 1.0 = 4321.0 then
        io.write_string("yes", !IO), io.nl(!IO)
    else
        link_error(!IO)
    ),
    ( if 8u * 1000u + 6u * 100u + 4u * 10u + 2u = 8642u then
        io.write_string("yes", !IO), io.nl(!IO)
    else
        link_error(!IO)
    ),
    ( if private_builtin.typed_unify(42, 42) then
        io.write_string("yes", !IO), io.nl(!IO)
    else
        link_error(!IO)
    ),
    ( if private_builtin.typed_unify(1, 2) then
        link_error(!IO)
    else
        io.write_string("no", !IO), io.nl(!IO)
    ),
    ( if private_builtin.typed_unify(43, X1) then
        io.write_int(X1, !IO), io.nl(!IO)
    else
        link_error(!IO)
    ),
    ( if private_builtin.typed_unify(44, _ `with_type` string) then
        link_error(!IO)
    else
        io.write_string("no", !IO), io.nl(!IO)
    ),
    ( if dynamic_cast(45, X2) then
        io.write_int(X2, !IO), io.nl(!IO)
    else
        link_error(!IO)
    ),
    ( if dynamic_cast(46, _ `with_type` string) then
        link_error(!IO)
    else
        io.write_string("no", !IO), io.nl(!IO)
    ).

    % We should be able to optimize away all calls to this procedure
    % at compile time, so we should not even emit a reference to it.
:- pred link_error(io::di, io::uo) is det.
:- pragma external_pred(link_error/2).
