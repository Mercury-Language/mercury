%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the filtering of the contents of a map.
%
%---------------------------------------------------------------------------%

:- module test_map_filter.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module map.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    fill_map(1, 30, map.init, Map0),
    map.filter_map_values(filter_map_func, Map0, Map),
    ( if map.search(Map, 14, Val14) then
        io.write_line(Val14, !IO)
    else
        io.write_string("14 not found\n", !IO)
    ),
    map.to_sorted_assoc_list(Map, AL),
    list.foldl(io.write_line, AL, !IO).

:- pred fill_map(int::in, int::in,
    map(int, string)::in, map(int, string)::out) is det.

fill_map(Cur, Max, !Map) :-
    ( if Cur < Max then
        map.det_insert(Cur, string.int_to_string(Cur), !Map),
        fill_map(Cur + 1, Max, !Map)
    else
        true
    ).

:- pred filter_map_func(int::in, string::in, string::out) is semidet.

filter_map_func(Key, Value0, Value) :-
    Key mod 3 > 0,
    Value = "x" ++ Value0.
