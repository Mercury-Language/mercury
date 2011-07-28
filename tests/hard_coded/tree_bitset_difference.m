%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
%-----------------------------------------------------------------------------%
% This is a regression test for Mantis bug #207.
%-----------------------------------------------------------------------------%

:- module tree_bitset_difference.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module set.
:- import_module tree_bitset.

%-----------------------------------------------------------------------------%

main(!IO) :-
    % The values of X and Y (or 2X and 2Y) are intended to end up
    % in near the start and the end of one interior node, while Z (or 2Z)
    % ends up near the start of the next interior node. The bug was in how
    % the difference operation handled interior nodes at the same level
    % but not at the same starting address.

    X = 532,
    Y = 32431,
    Z = 32794,

    % This version of the test failed on 32 bit systems.
    ListA_1 = [X, Y],
    ListB_1 = [Z],
    test(ListA_1, ListB_1, !IO),

    % This version of the test failed on 64 bit systems.
    ListA_2 = [X * 2, Y * 2],
    ListB_2 = [Z * 2],
    test(ListA_2, ListB_2, !IO).

:- pred test(list(int)::in, list(int)::in, io::di, io::uo) is det.

test(ListA, ListB, !IO) :-
    SetA = set.from_list(ListA),
    SetB = set.from_list(ListB),
    set.difference(SetA, SetB, SetC),
    set.to_sorted_list(SetC, ListC_set),

    BitSetA = tree_bitset.list_to_set(ListA),
    BitSetB = tree_bitset.list_to_set(ListB),
    tree_bitset.difference(BitSetA, BitSetB, BitSetC),
    ListC_bitset = tree_bitset.to_sorted_list(BitSetC),

    io.write_string("list A:                 ", !IO),
    io.write(ListA, !IO),
    io.nl(!IO),
    io.write_string("list B:                 ", !IO),
    io.write(ListB, !IO),
    io.nl(!IO),
    io.write_string("set difference:         ", !IO),
    io.write(ListC_set, !IO),
    io.nl(!IO),
    io.write_string("tree_bitset difference: ", !IO),
    io.write(ListC_bitset, !IO),
    io.nl(!IO).
