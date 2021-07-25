%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% map_equal.m
% Paul Bone
%---------------------------------------------------------------------------%

:- module map_equal.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    List1 = [1 - 1, 2 - 2, 3 - 3, 5 - 5, 6 - 6,
        7 - 7, 9 - 9, 10 - 10],
    map.from_assoc_list(List1, Map1),
    test("Map1 = Map1", Map1, Map1, !IO),
    copy(Map1, Map1Copy),
    test("Map1 = Map1Copy", Map1, Map1Copy, !IO),
    set(7, 49, Map1, Map2),
    test("Map1 = Map2", Map1, Map2, !IO),
    test("Map2 = Map1", Map2, Map1, !IO),

    EmptyMap = map.init,
    test("Map1 = empty", Map1, EmptyMap, !IO),
    test("empty = Map1", EmptyMap, Map1, !IO),
    test("empty = empty", EmptyMap, EmptyMap, !IO),
    copy(EmptyMap, EmptyMap2),
    test("empty = copy(empty)", EmptyMap, EmptyMap2, !IO),

    map.from_assoc_list(List1, Map3),
    map.from_assoc_list(reverse(List1), Map4),
    test("Map3 = Map4", Map3, Map4, !IO),
    test("Map4 = Map3", Map3, Map4, !IO).

:- pred test(string::in, map(K, V)::in, map(K, V)::in, io::di, io::uo) is det.

test(Name, A, B, !IO) :-
    ( if map.equal(A, B) then
        EqualRes = "equal"
    else
        EqualRes = "not equal"
    ),
    ( if unify(A, B) then
        UnifyRes = "unifiable"
    else
        UnifyRes = "not unifiable"
    ),
    io.format("%s: %s, %s\n", [s(Name), s(UnifyRes), s(EqualRes)], !IO).

%---------------------------------------------------------------------------%
