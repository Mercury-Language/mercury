%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression tests for set_bbbtree.

% Author benyi.

:- module set_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module list.
:- import_module set_bbbtree.
:- import_module string.

main(!IO) :-
    set_bbbtree.init(Set0),
    ( if
        set_bbbtree.is_empty(Set0),
        set_bbbtree.count(Set0, 0)
    then
        true
    else
        io.write_string("empty/size failed on empty set.\n", !IO)
    ),
    ( if
        set_bbbtree.is_member(5, Set0, no)
    then
        true
    else
        io.write_string("is_member should have failed on empty set\n", !IO)
    ),
    set_bbbtree.insert(5, Set0, Set10),
    ( if
        set_bbbtree.is_member(5, Set10, yes),
        set_bbbtree.is_member(6, Set10, no),
        set_bbbtree.singleton_set(5, Set10),
        set_bbbtree.least(Set10, 5),
        set_bbbtree.largest(Set10, 5)
    then
        true
    else
        io.write_string("is_member/singleton_set/least/largest failed\n", !IO)
    ),
    set_bbbtree.delete(5, Set10, Set20),
    ( if
        set_bbbtree.is_empty(Set20)
    then
        true
    else
        io.write_string("empty Set20 failed\n", !IO)
    ),
    set_bbbtree.delete(5, Set20, Set30),
    ( if
        set_bbbtree.is_empty(Set30)
    then
        true
    else
        io.write_string("empty Set30 failed\n", !IO)
    ),

    set_bbbtree.delete_list([0, 1, 2, 3, 4, 5, 6, 100000], Set10, Set40),
    ( if
        set_bbbtree.is_empty(Set40)
    then
        true
    else
        io.write_string("empty Set40 failed\n", !IO)
    ),
    set_bbbtree.list_to_set([-2, 10, 0, -1, 2, -2, -2, 0], Set50),
    ( if
        set_bbbtree.count(Set50, 5),
        set_bbbtree.remove_least(-2, Set50, TempSet)
    then
        Set60 = TempSet,
        ( if
            set_bbbtree.is_member(-2, Set60, no)
        then
            true
        else
            io.write_string("is_member failed\n", !IO)
        ),
        set_bbbtree.delete_list([-2, -2, -2, 10], Set60, Set70),
        ( if
            set_bbbtree.is_member(-2, Set70, no),
            set_bbbtree.is_member(10, Set70, no),
            set_bbbtree.is_member(10000, Set70, no),
            set_bbbtree.is_member(0, Set70, yes),
            set_bbbtree.largest(Set70, 2),
            set_bbbtree.least(Set70, -1)
        then
            true
        else
            io.write_string("is_member/largest/least failed\n", !IO)
        )
    else
        io.write_string("size/remove_least failed\n", !IO)
    ),

    set_bbbtree.list_to_set([4, -1, 0], Set80),
    set_bbbtree.to_sorted_list(Set80, List),
    ( if
        List = [-1, 0, 4]
    then
        true
    else
        io.write_string("conversion of list to set and back failed.\n", !IO)
    ),

    set_bbbtree.list_to_set([1, 2, 3], Set90),
    set_bbbtree.list_to_set([3, 4, 3], Set100),
    set_bbbtree.list_to_set([1, 2, 3, 4], SetUnion),
    set_bbbtree.list_to_set([3], SetIntersection),
    set_bbbtree.list_to_set([1, 2], SetDifference),
    set_bbbtree.union(Set90, Set100, SetUnion0),
    ( if
        set_bbbtree.equal(SetUnion0, SetUnion)
    then
        true
    else
        io.write_string("union/equal failed\n", !IO)
    ),
    set_bbbtree.intersect(Set90, Set100, SetIntersection0),
    ( if
        set_bbbtree.equal(SetIntersection0, SetIntersection)
    then
        true
    else
        io.write_string("intersect/equal failed\n", !IO)
    ),
    set_bbbtree.difference(Set90, Set100, SetDifference0),
    ( if
        set_bbbtree.equal(SetDifference0, SetDifference)
    then
        true
    else
        io.write_string("difference/equal failed\n", !IO)
    ),

    ( if
        set_bbbtree.subset(SetIntersection, Set90),
        set_bbbtree.subset(SetIntersection, Set100)
    then
        true
    else
        io.write_string("subset failed\n", !IO)
    ),

    set_bbbtree.init(Set110),
    set_bbbtree.insert(Set90, Set110, Set120),
    set_bbbtree.insert(Set100, Set120, Set130),
    set_bbbtree.insert(SetIntersection, Set130, Set140),
    set_bbbtree.insert(SetDifference, Set140, Set150),
    set_bbbtree.power_union(Set150, SetUnion10),
    ( if
        set_bbbtree.equal(SetUnion10, SetUnion)
    then
        true
    else
        io.write_string("power_union/equal failed\n", !IO)
    ),
    set_bbbtree.power_intersect(Set150, SetIntersection10),
    ( if
        set_bbbtree.equal(SetIntersection10, SetIntersection)
    then
        true
    else
        io.write_string("power_intersect/equal failed\n", !IO)
    ).
