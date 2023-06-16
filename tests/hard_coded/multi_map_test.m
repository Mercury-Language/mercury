%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multi_map_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module assoc_list.
:- import_module list.
:- import_module multi_map.

main(!IO) :-
    multi_map.init(EmptyMap : multi_map(int, int)),
    Map = multi_map.from_corresponding_lists([1,  2,  1], [11, 22, 13]),

    % Test multi_map.lookup.
    Vs = multi_map.lookup(Map, 1) : list(int),
    io.write_line(Vs, !IO),

    % Test is_empty.
    ( if multi_map.is_empty(EmptyMap) then
        io.write_string("PASSED: is_empty(EmptyMap) succeeded\n", !IO)
    else
        io.write_string("FAILED: is_empty(EmptyMap) failed\n", !IO)
    ),
    ( if multi_map.is_empty(Map) then
        io.write_string("FAILED: is_empty(Map) succeeded\n", !IO)
    else
        io.write_string("PASSED: is_empty(Map) failed\n", !IO)
    ),

    % Test insertion.
    multi_map.det_insert(3, 34, Map, InsertMap),
    multi_map.to_assoc_list(InsertMap, AL),
    io.write_string("det_insert/4: ", !IO),
    io.write_line(AL, !IO),

    ( if multi_map.insert(3, 34, Map, InsertMap2) then
        multi_map.to_assoc_list(InsertMap2, AL2),
        io.write_string("insert/4 (test 1): PASSED: ", !IO),
        io.write_line(AL2, !IO)
    else
        io.write_string("insert/4 test 1): FAILED\n", !IO)
    ),

    ( if multi_map.insert(1, 14, Map, InsertMap3) then
        multi_map.to_assoc_list(InsertMap3, AL3),
        io.write_string("insert/4 (test 2) FAILED: ", !IO),
        io.write_line(AL3, !IO)
    else
        io.write_string("insert/4 (test 2) PASSED\n", !IO)
    ),

    % Test update.
    multi_map.det_update(1, 14, Map, UpdateMap),
    multi_map.to_assoc_list(UpdateMap, UAL),
    io.write_string("det_update/4: ", !IO),
    io.write_line(UAL, !IO),

    ( if multi_map.update(1, 14, Map, UpdateMap1) then
        multi_map.to_assoc_list(UpdateMap1, UAL1),
        io.write_string("update/4 (test 1): PASSED: ", !IO),
        io.write_line(UAL1, !IO)
    else
        io.write_string("update/4 (test 1): FAILED\n", !IO)
    ),

    ( if multi_map.update(1, 14, EmptyMap, UpdateMap2) then
        multi_map.to_assoc_list(UpdateMap2, UAL2),
        io.write_string("update/4 (test 2): FAILED: ", !IO),
        io.write_line(UAL2, !IO)
    else
        io.write_string("update/4 (test 2): PASSED\n", !IO)
    ),

    % Test replace.
    multi_map.det_replace(1, [561, 562, 563], Map, ReplaceMap),
    multi_map.to_assoc_list(ReplaceMap, RAL),
    io.write_string("det_replace/4: ", !IO),
    io.write_line(RAL, !IO),

    ( if multi_map.replace(1, [561, 562, 563], Map, ReplaceMap1) then
        multi_map.to_assoc_list(ReplaceMap1, RAL1),
        io.write_string("replace/4 (test 1): PASSED: ", !IO),
        io.write_line(RAL1, !IO)
    else
        io.write_string("replace/4 (test 1): FAILED\n", !IO)
    ),

    ( if multi_map.replace(1, [561, 562, 563], EmptyMap, ReplaceMap2) then
        multi_map.to_assoc_list(ReplaceMap2, RAL2),
        io.write_string("replace/4 (test 2): FAILED: ", !IO),
        io.write_line(RAL2, !IO)
    else
        io.write_string("replace/4 (test 2): PASSED\n", !IO)
    ),

    % Test set.
    multi_map.set(1, 12, EmptyMap, SetMap),
    multi_map.to_assoc_list(SetMap, SAL),
    io.write_string("set/4 (test 1): ", !IO),
    io.write_line(SAL, !IO),

    multi_map.set(1, 14, Map, SetMap2),
    multi_map.to_assoc_list(SetMap2, SAL2),
    io.write_string("set/4 (test 2): ", !IO),
    io.write_line(SAL2, !IO).
