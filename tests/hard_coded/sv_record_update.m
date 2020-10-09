%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module sv_record_update.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module map.

:- type type1
    --->    type1(
                field1  :: int,
                field2  :: int
            ).

:- type type2
    --->    type2(
                type1   :: type1,
                field3  :: int
            ).

main(!IO) :-
    test1(type1(3, 4), Type1),
    io.write(Type1, !IO),
    io.nl(!IO),
    test2(type2(Type1, 5), Type2),
    io.write(Type2, !IO),
    io.nl(!IO),
    test3(map.init, Map),
    map.to_assoc_list(Map, AssocList),
    io.write(AssocList, !IO),
    io.nl(!IO).

:- pred test1(type1::in, type1::out) is det.

test1(!Type1) :-
    !Type1 ^ field2 := 12.

:- pred test2(type2::in, type2::out) is det.

test2(!Type2) :-
    !Type2 ^ type1 ^ field1 := 5,
    !Type2 ^ field3 := 13.

:- pred test3(map(string, int)::in, map(string, int)::out) is det.

test3(!Map) :-
    !Map ^ elem("1") := 1,
    !Map ^ elem("2") := 2.
