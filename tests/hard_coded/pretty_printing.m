%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% pretty_printing.m
% Ralph Becket <rbeck@microsoft.com>
% Fri Oct 26 12:57:35 EST 2001
%
% Test code for pprint.
%
%---------------------------------------------------------------------------%

:- module pretty_printing.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pprint.
:- import_module rbtree.
:- import_module solutions.
:- import_module string.
:- import_module version_array.

:- type tree(T)
    --->    branch(tree(T), T, tree(T))
    ;       leaf.

%---------------------------------------------------------------------------%

:- func list_doc(int, int) = doc.

list_doc(M, D) = to_doc(D, 1 `..` M).

%---------------------------------------------------------------------------%

:- func array_doc(int, int, int) = doc.

array_doc(M, N, D) = to_doc(D, array(duplicate(M, 1 `..` N))).

%---------------------------------------------------------------------------%

:- func version_array_doc(int, int, int) = doc.

version_array_doc(M, N, D) = to_doc(D, version_array(duplicate(M, 1 ..N))).

%---------------------------------------------------------------------------%

:- func tuple_doc_0(int) = doc.
:- func tuple_doc_1(int) = doc.
:- func tuple_doc_2(int) = doc.
:- func tuple_doc_3(int) = doc.
:- func tuple_doc_10(int) = doc.

tuple_doc_0(D)  = to_doc(D, {}).
tuple_doc_1(D)  = to_doc(D, {1}).
tuple_doc_2(D)  = to_doc(D, {1, 2}).
tuple_doc_3(D)  = to_doc(D, {1, 2, 3}).
tuple_doc_10(D) = to_doc(D, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}).

%---------------------------------------------------------------------------%

:- func map_doc(int, int, int) = doc.

map_doc(M, N, D) = to_doc(D, gen_map(1, M, 1 `..` N, map.init)).

:- func gen_map(int, int, list(int), map(int, list(int))) =
    map(int, list(int)).

gen_map(I, M, Ns, Map) =
    ( if M < I then
        Map
    else
        gen_map(I + 1, M, Ns, Map ^ elem(I) := Ns)
    ).

%---------------------------------------------------------------------------%

:- func tree_doc(int, int) = doc.

tree_doc(M, D) = to_doc(D, gen_tree(M)).

:- func gen_tree(int) = tree(int).

gen_tree(M) =
    ( if M =< 0 then
        leaf
    else
        branch(gen_tree(M - 1), M, gen_tree(M - 1))
    ).

%---------------------------------------------------------------------------%

:- func word_wrapped_doc = doc.

word_wrapped_doc = word_wrapped("
    In Xanadu did Kubla Khan
    A stately pleasure-dome decree:
    Where Alph, the sacred river, ran
    Through caverns measureless to man
    Down to a sunless sea.
    So twice five miles of fertile ground
    With walls and towers were girdled round:
    And here were gardens bright with sinuous rills
    Where blossomed many an incense-bearing tree;
    And here were forests ancient as the hills,
    Enfolding sunny spots of greenery.
    But oh! that deep romantic chasm which slanted
    Down the green hill athwart a cedarn cover!
    A savage place! as holy and enchanted
    As e'er beneath a waning moon was haunted
    By woman wailing for her demon-lover!
").

%---------------------------------------------------------------------------%

:- func test_string({int, int, int}) = string.

test_string({D, S, W}) = to_string(W, test_doc(D, S, W)).

:- func test_doc(int, int, int) = doc.

test_doc(D, S, W) =
    line `<>`
    line `<>`
    text(string.format("depth %d, size %d, width %d", [i(D), i(S), i(W)])) `<>`
    line `<>`
    list_doc(S, D) `<>` line `<>`
    array_doc(S, 1, D) `<>` line `<>`
    array_doc(S, S, D) `<>` line `<>`
    version_array_doc(S, 1, D) `<>` line `<>`
    version_array_doc(S, S, D) `<>` line `<>`
    tuple_doc_0(D) `<>` line `<>`
    tuple_doc_1(D) `<>` line `<>`
    tuple_doc_2(D) `<>` line `<>`
    tuple_doc_3(D) `<>` line `<>`
    tuple_doc_10(D) `<>` line `<>`
    map_doc(S, 1, D) `<>` line `<>`
    map_doc(S, S, D) `<>` line `<>`
    ( if 10 < S then nil else tree_doc(S, D) `<>` line ) `<>`
    word_wrapped_doc `<>` line.

%---------------------------------------------------------------------------%

:- pred depth_size_width({int, int, int}::out) is multi.

depth_size_width({D, S, W}) :-
    depth(D),
    size(S),
    width(W).

:- pred depth(int::out) is multi.

depth(0).
depth(5).
depth(1000).

:- pred size(int::out) is multi.

size(0).
size(1).
size(3).
size(5).

:- pred width(int::out) is multi.

width(20).
width(40).
width(80).

%---------------------------------------------------------------------------%

main(!IO) :-
    foldl(io.write_string, map(test_string, solutions(depth_size_width)), !IO).

%---------------------------------------------------------------------------%
