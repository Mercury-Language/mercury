%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% test_pretty_printer_defaults.m
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Tue Aug  7 15:29:20 EST 2007
%
% Test the default pretty_printer formatters.
%
%---------------------------------------------------------------------------%

:- module test_pretty_printer_defaults.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module char.
:- import_module fat_sparse_bitset.
:- import_module fatter_sparse_bitset.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module one_or_more.
:- import_module pretty_printer.
:- import_module sparse_bitset.
:- import_module string.
:- import_module tree_bitset.
:- import_module uint.
:- import_module version_array.

%---------------------------------------------------------------------------%

main(!IO) :-
    pretty_printer.get_default_formatter_map(FMap, !IO),
    % We print only the type names. Even if we could get them, there would be
    % no point in printing the corresponding formatters, since they would
    % all be printed as just "<<function>>".
    FMapTypes = get_formatter_map_entry_types(FMap),
    io.write_string("The types in the default formatter map:\n", !IO),
    list.foldl(io.write_line, FMapTypes, !IO),
    io.nl(!IO),

    format_and_write_items("ints", 42, -123, !IO),
    format_and_write_items("floats", 3.141, -10.0, !IO),
    format_and_write_item("chars", [a, '*', '\n'], !IO),
    format_and_write_item("string", "this is a string", !IO),
    format_and_write_item("tuple", {1, '2', 3.0, "four"}, !IO),

    A = array(L),
    VA = version_array(L),
    L = 1..100,
    OoM = one_or_more(1, 2..100),
    map.from_corresponding_lists(L, L, M),

    format_and_write_item("array", A, !IO),
    format_and_write_item("version_array", VA, !IO),
    format_and_write_item("list", L, !IO),
    format_and_write_item("one_or_more", OoM, !IO),
    format_and_write_item("map", M, !IO),

    Primes = [2u, 3u, 5u, 7u, 11u, 13u, 17u, 19u, 23u, 29u, 31u, 37u, 41u, 43u,
        47u, 53u, 59u, 61u, 67u, 71u, 73u, 79u, 83u, 89u, 97u],
    sparse_bitset.list_to_set(Primes, SparseBitset),
    fat_sparse_bitset.list_to_set(Primes, FatSparseBitset),
    fatter_sparse_bitset.list_to_set(Primes, FatterSparseBitset),
    tree_bitset.list_to_set(Primes, TreeBitset),

    format_and_write_item("sparse_bitset", SparseBitset, !IO),
    format_and_write_item("fat_sparse_bitset", FatSparseBitset, !IO),
    format_and_write_item("fatter_sparse_bitset", FatterSparseBitset, !IO),
    format_and_write_item("tree_bitset", TreeBitset, !IO).

:- pred format_and_write_item(string::in, T::in, io::di, io::uo) is det.

format_and_write_item(Label, Item, !IO) :-
    Doc = docs([str(Label ++ ":"), nl, format(Item), nl]),
    pretty_printer.write_doc(Doc, !IO),
    io.nl(!IO).

:- pred format_and_write_items(string::in, T::in, T::in, io::di, io::uo)
    is det.

format_and_write_items(Label, ItemA, ItemB, !IO) :-
    Doc = docs([str(Label ++ ":"), nl,
        format(ItemA), str(" "), format(ItemB), nl]),
    pretty_printer.write_doc(Doc, !IO),
    io.nl(!IO).
