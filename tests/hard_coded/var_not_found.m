%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test -- previous versions of the Mercury compiler
% (Oct 98) got a `var not found' error for this test case, due to
% mode analysis producing incorrect instmap delta annotations
% for the complicated unification in the implied mode in the
% first call to map.from_assoc_list in next/3.
%

:- module var_not_found.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.
:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module std_util.
:- import_module string.

main(!IO) :-
    map.from_assoc_list([int(1) - int(2)], Map),
    ( if next(Map, Map, Next) then
        io.write_line(Next, !IO)
    else
        io.write_string("failed (as we should)\n", !IO)
    ).

:- type data
    --->    int(int)
    ;       flt(float)
    ;       str(string)
    ;       array(map(data, data))
    ;       void.

:- pred next(map(data, data)::in, map(data, data)::in, map(data, data)::out)
    is semidet.

next(Thing, Array, Next) :-
    map.to_assoc_list(Thing, [int(0) - Key, int(1) - _]),
    map.to_assoc_list(Array, List),
    next_pair(List, Key, NewKey - NewValue),
    map.from_assoc_list([int(0) - NewKey, int(1) - NewValue], Next).

:- pred next_pair(list(pair(data))::in, data::in, pair(data)::out) is semidet.

next_pair([Pair0 | Pairs], Key, Pair) :-
    ( if Pair0 = Key - _ then
        Pairs = [Pair | _]
    else
        next_pair(Pairs, Key, Pair)
    ).
