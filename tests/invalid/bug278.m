%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bug278.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module map.

main(!IO) :-
    io.write_string("Hello\n", !IO).

:- type memo_table
    --->    memo_table.

:- pred build_map(pred(K, V, memo_table, memo_table), list(K), map(K, V),
    memo_table, memo_table).
:- mode build_map(pred(in, out, in, out) is semidet, in, out, in, out) is det.

build_map(KeyToValue, Keys, Map, !MemoTable) :-
    list.foldl2(
        % The bug is in the spelling of !:MemoTable1 here, it crashes the
        % compiler.
        ( pred(K::in, !.M::in, !:M::out, !.MemoTable1::in, !:MemoTable::out)
                is det :-
            ( if KeyToValue(K, V, !MemoTable1) then
                map.det_insert(K, V, !M)
            else
                true
            )
        ), Keys, map.init, Map, !MemoTable).
