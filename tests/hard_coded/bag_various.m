%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Basic test of some bag predicates.
%---------------------------------------------------------------------------%

:- module bag_various.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bag.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.

main(!IO) :-
    Bag = bag.from_list([1, 1, 1, 2, 3, 3, 4]),
    dump("bag.to_list: ", []++bag.to_list(Bag), !IO),
    dump("bag.to_assoc_list: ", []++bag.to_assoc_list(Bag), !IO),
    dump("bag.count: ", 0+bag.count(Bag), !IO),
    dump("bag.count_unique: ", 0+bag.count_unique(Bag), !IO),
    ( if bag.member(4, Bag)
    then dump("bag.member(4): ", yes, !IO)
    else dump("bag.member(4): ", no, !IO)
    ),
    ( if bag.member(5, Bag)
    then dump("bag.member(5): ", yes, !IO)
    else dump("bag.member(5): ", no, !IO)
    ),
    unsorted_solutions(
    (pred(O::out) is nondet:-
        bag.member(M, Bag, Rest),
        O = {M, []++to_list(Rest)}
    ), Sols),
    dump("unsorted_solutions(bag.member/3): ", Sols, !IO),

    test_insert_duplicates(5, bag.init, !IO),
    test_insert_duplicates(0, bag.init, !IO),
    test_insert_duplicates(-1, bag.init, !IO),
    test_insert_duplicates(4, bag.from_list(["foo"]), !IO),

    true.

:- pred test_insert_duplicates(int::in, bag(string)::in, io::di, io::uo)
    is det.

test_insert_duplicates(N, Bag0, !IO) :-
    Prefix = "bag.insert_duplicates(" ++ int_to_string(N) ++ ", \"foo\"): ",
    ( if bag.insert_duplicates(N, "foo", Bag0, Bag) then
        bag.to_list(Bag, List),
        dump(Prefix, List, !IO)
    else
        dump(Prefix, "fail", !IO)
    ).

:- pred dump(string::in, T::in, io::di, io::uo) is det.

dump(Msg, T, !IO) :-
    io.write_string(Msg, !IO),
    io.write_line(T, !IO).
