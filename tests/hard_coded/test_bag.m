%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_bag.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bag.
:- import_module list.

main(!IO) :-
    Bag111 = bag.from_list([11, 12, 13]) : bag(int),
    % Bag122 = bag.from_list([11, 12, 12, 13, 13]) : bag(int),
    Bag212 = bag.from_list([11, 11, 12, 13, 13]) : bag(int),
    % Bag221 = bag.from_list([11, 11, 12, 13, 13]) : bag(int),
    Bag222 = bag.from_list([11, 11, 12, 12, 13, 13]) : bag(int),
    BagS   = bag.from_list([5, 5, 6]) : bag(int),
    BagL   = bag.from_list([25, 26, 26]) : bag(int),
    BagSM  = bag.from_list([5, 5, 6, 12]) : bag(int),
    BagSL  = bag.from_list([5, 5, 6, 25, 26, 26]) : bag(int),
    BagML  = bag.from_list([12, 25, 26, 26]) : bag(int),
    BagSML = bag.from_list([5, 5, 6, 25, 12, 26, 26]) : bag(int),

    AllBags = [Bag111, Bag212, Bag222,
        BagS, BagL, BagSL, BagSM, BagML, BagSML],
    test_bag_combinations_outer_loop(AllBags, AllBags, !IO).

:- pred test_bag_combinations_outer_loop(
    list(bag(int))::in, list(bag(int))::in, 
    io::di, io::uo) is det.

test_bag_combinations_outer_loop([], _, !IO).
test_bag_combinations_outer_loop([OuterBag | OuterBags], InnerBags, !IO) :-
    test_bag_combinations_inner_loop(OuterBag, InnerBags, !IO),
    test_bag_combinations_outer_loop(OuterBags, InnerBags, !IO).

:- pred test_bag_combinations_inner_loop(bag(int)::in, list(bag(int))::in, 
    io::di, io::uo) is det.

test_bag_combinations_inner_loop(_OuterBag, [], !IO).
test_bag_combinations_inner_loop(OuterBag, [InnerBag | InnerBags], !IO) :-
    test_bag_combination(OuterBag, InnerBag, !IO),
    test_bag_combinations_inner_loop(OuterBag, InnerBags, !IO).

:- pred test_bag_combination(bag(int)::in, bag(int)::in, 
    io::di, io::uo) is det.

test_bag_combination(BagA, BagB, !IO) :-
    bag.to_list(BagA, ListA),
    bag.to_list(BagB, ListB),
    io.write_string("-----------------------------------------", !IO),
    io.nl(!IO),
    io.write_string("BagA =                   ", !IO),
    io.write(ListA, !IO),
    io.nl(!IO),
    io.write_string("BagB =                   ", !IO),
    io.write(ListB, !IO),
    io.nl(!IO),

    bag.subtract(BagA, BagB, BagAmB),
    bag.subtract_small(BagA, BagB, BagAmBS),
    bag.to_list(BagAmB, ListAmB),
    bag.to_list(BagAmBS, ListAmBS),

    io.write_string("A subtract B:            ", !IO),
    io.write(ListAmB, !IO),
    io.write_string(maybe_agree(ListAmB, ListAmBS), !IO),
    io.nl(!IO),

    bag.least_upper_bound(BagA, BagB, BagAlubB),
    bag.least_upper_bound_small(BagA, BagB, BagAlubBS),
    bag.to_list(BagAlubB, ListAlubB),
    bag.to_list(BagAlubBS, ListAlubBS),

    io.write_string("A least_upper_bound B:   ", !IO),
    io.write(ListAlubB, !IO),
    io.write_string(maybe_agree(ListAlubB, ListAlubBS), !IO),
    io.nl(!IO),

    bag.union(BagA, BagB, BagAuB),
    bag.union_small(BagA, BagB, BagAuBS),
    bag.to_list(BagAuB, ListAuB),
    bag.to_list(BagAuBS, ListAuBS),

    io.write_string("A union B:               ", !IO),
    io.write(ListAuB, !IO),
    io.write_string(maybe_agree(ListAuB, ListAuBS), !IO),
    io.nl(!IO),

    bag.intersect(BagA, BagB, BagAiB),
    bag.intersect_small(BagA, BagB, BagAiBS),
    bag.to_list(BagAiB, ListAiB),
    bag.to_list(BagAiBS, ListAiBS),

    io.write_string("A intersect B:           ", !IO),
    io.write(ListAiB, !IO),
    io.write_string(maybe_agree(ListAiB, ListAiBS), !IO),
    io.nl(!IO),

    io.write_string("A ", !IO),
    ( if bag.subset_compare(CmpRes, BagA, BagB) then
        io.write(CmpRes, !IO)
    else
        io.write_string("incomparable", !IO)
    ),
    io.write_string(" B", !IO),
    io.nl(!IO),

    io.write_string("-----------------------------------------", !IO),
    io.nl(!IO).

:- func maybe_agree(list(int), list(int)) = string.

maybe_agree(ListA, ListB) = Str :-
    ( if ListA = ListB then
        Str = ""
    else
        Str = " disagree"
    ).
