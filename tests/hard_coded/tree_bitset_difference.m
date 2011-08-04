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
:- import_module pair.
:- import_module set.
:- import_module tree_bitset.

%-----------------------------------------------------------------------------%

main(!IO) :-
    Tests = [
        % The values of X and Y (or 2X and 2Y) are intended to end up
        % in near the start and the end of one interior node, while Z (or 2Z)
        % ends up near the start of the next interior node. The bug was in how
        % the difference operation handled interior nodes at the same level
        % but not at the same starting address.

        [532, 32431] -      [32794],
        [1, 29424] -        [1, 2, 3, 35701],
        [1] -               [2, 35701],
        [101, 102] -        [1, 2, 3, 35699, 35700, 35701],
        [36696, 35702, 35703, 35705] -
                            [1, 2, 3, 33416, 334283]
    ],
    list.foldl(test_32_64, Tests, !IO).

:- pred test_32_64(pair(list(int), list(int))::in, io::di, io::uo) is det.

test_32_64(ListA - ListB, !IO) :-
    test(ListA, ListB, !IO),
    test(list.map(double, ListA), list.map(double, ListB), !IO).

:- func double(int) = int.

double(X) = 2 * X.

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

    ( ListC_set = ListC_bitset ->
        true
    ;
        io.write_string("DIFFERENCE:\n", !IO),
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
        io.nl(!IO)
    ).
