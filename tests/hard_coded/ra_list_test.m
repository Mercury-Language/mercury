%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test of the operations of the ra_list module.
%

:- module ra_list_test.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module ra_list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    ra_list.init(Nil),
    ra_list.cons(1.0, Nil,  RA_1),
    ra_list.cons(2.0, RA_1, RA_2),
    ra_list.cons(3.0, RA_2, RA_3),
    ra_list.cons(4.0, RA_3, RA_4),

    ra_list.append(RA_4, RA_4, RA_8),
    ra_list.append(RA_8, RA_8, RA_16),

    RALists = [RA_1, RA_2, RA_3, RA_4, RA_8, RA_16],

    Searches = [0, 1, 2, 3, 4, 7, 8, 9, 14, 15, 16],
    Drops = [0, 1, 2, 5, 9],

    list.foldl(test_ops(Searches, Drops), RALists, !IO).

:- pred test_ops(list(int)::in, list(int)::in, ra_list(float)::in,
    io::di, io::uo) is det.

test_ops(Searches, Drops, RAList, !IO) :-
    ra_list_to_list(RAList, List),
    io.write_line(List, !IO),
    ( if ra_list.head_tail(RAList, RAHead, RATail) then
        ra_list_to_list(RATail, Tail),
        io.format("head: %.1f\n", [f(RAHead)], !IO),
        io.format("tail: ", [], !IO),
        io.write_line(Tail, !IO)
    else
        io.format("ra_list.head_tail failed\n", [], !IO)
    ),
    list.foldl(test_index(RAList), Searches, !IO),
    list.foldl(test_drop(RAList), Drops, !IO),
    io.nl(!IO).

:- pred test_index(ra_list(float)::in, int::in, io::di, io::uo) is det.

test_index(RAList, Index, !IO) :-
    ( if ra_list.index0(RAList, Index, Val) then
        io.format("at index0 %d: %.1f\n", [i(Index), f(Val)], !IO)
    else
        io.format("at index0 %d: _\n", [i(Index)], !IO)
    ).

:- pred test_drop(ra_list(float)::in, int::in, io::di, io::uo) is det.

test_drop(RAList0, Drop, !IO) :-
    io.format("drop %d: ", [i(Drop)], !IO),
    ( if ra_list.drop(Drop, RAList0, RAList) then
        ra_list_to_list(RAList, List),
        io.write_line(List, !IO)
    else
        io.format("failed\n", [], !IO)
    ).

%---------------------------------------------------------------------------%
