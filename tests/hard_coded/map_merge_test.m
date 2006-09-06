% vim: ft=mercury ts=4 sw=4 et
%
% Check that map.merge/3 throws an exception if the sets of keys
% are not disjoint.

:- module map_merge_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module assoc_list.
:- import_module exception.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module unit.

main(!IO) :-
    try_io(main_2, Result, !IO),
    (
        Result = succeeded(_),
        io.write_string("FAILURE", !IO)
    ;
        Result = exception(_),
        io.write_string("SUCCESS", !IO)
    ).

:- pred main_2(map(int, int)::out, io::di, io::uo) is det.

main_2(MapC, !IO) :-
    MapA = map.from_assoc_list([10 - 10, 20 - 20, 30 - 30]),
    MapB = map.from_assoc_list([5 - 5, 20 - 20, 35 - 35]),
    map.merge(MapA, MapB, MapC).
