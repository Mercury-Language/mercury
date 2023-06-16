% vim: ft=mercury ts=4 sw=4 et
%
% Test map.keys_and_values/3.
%
:- module test_keys_and_values.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module pair.

main(!IO) :-
    map.init(Map0),
    Map1 = map.singleton("one", 1),
    Map2 = map.from_assoc_list(["one" - 1, "two" - 2]),
    Map3 = map.from_assoc_list(["one" - 1, "two" - 2, "three" - 3]),
    Map4 = map.from_assoc_list(["one" - 1, "two" - 2, "three" - 3, "four" - 4]),
    do_test(Map0, !IO),
    do_test(Map1, !IO),
    do_test(Map2, !IO),
    do_test(Map3, !IO),
    do_test(Map4, !IO).

:- pred do_test(map(string, int)::in, io::di, io::uo) is det.

do_test(Map, !IO) :-
    Keys = map.keys(Map),
    Values = map.values(Map),
    map.keys_and_values(Map, KeysPrime, ValuesPrime),
    ( if Keys = KeysPrime, Values = ValuesPrime then
        io.write_string("KeysPrime = ", !IO),
        io.write(KeysPrime, !IO),
        io.write_string("\nValuesPrime = ", !IO),
        io.write_line(ValuesPrime, !IO)
    else
        io.write_string("ERROR: Map = ", !IO),
        io.write_line(Map, !IO)
    ).
