%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test. Some versions of the compiler fail it
% because of an improper implementation of comparison for floats.

:- module float_map.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module map.
:- import_module float.

main(S0, S) :-
    map__init(M1),
    map__det_insert(1.0, 5, M1, M2),
    ( map__search(M2, 14.5, _) ->
        io__write_string("found it: bug\n", S0, S)
    ;
        io__write_string("did not find it: ok\n", S0, S)
    ).
