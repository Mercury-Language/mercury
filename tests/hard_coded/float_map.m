%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Some versions of the compiler fail it
% because of an improper implementation of comparison for floats.
%

:- module float_map.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module map.
:- import_module float.

main(!IO) :-
    map.init(M1),
    map.det_insert(1.0, 5, M1, M2),
    ( if map.search(M2, 14.5, _) then
        io.write_string("found it: bug\n", !IO)
    else
        io.write_string("did not find it: ok\n", !IO)
    ).
