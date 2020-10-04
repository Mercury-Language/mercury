%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module constant_prop_1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module float.
:- import_module string.
:- import_module uint.

main(!IO) :-
    io.write_string("foo" ++ "bar", !IO),
    io.nl(!IO),
    io.write_int(1 * 1000 + 2 * 100 + 3 * 10 + 4, !IO),
    io.nl(!IO),
    io.write_float(5.0 * 1000.0 + 6.0 * 100.0 + 7.0 * 10.0 + 8.0, !IO),
    io.nl(!IO),
    io.write_uint(2u * 1000u + 4u * 100u + 6u * 10u + 8u, !IO),
    io.nl(!IO).
