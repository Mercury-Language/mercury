%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2011-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Test operations on bitsets by comparing their output with the output
% from an ordinary set.
%
%---------------------------------------------------------------------------%

:- module test_bitset.

:- interface.

:- import_module enum.
:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- type test_bitset(T).

:- type bitset_error(T)
    --->    zero_argument(string,
                test_bitset(T))
    ;       one_argument(string,
                test_bitset(T), test_bitset(T))
    ;       two_arguments(string,
                test_bitset(T), test_bitset(T), test_bitset(T)).

%---------------------------------------------------------------------------%
%
% Initial creation of sets.
%

:- func init = test_bitset(T).
:- pred init(test_bitset(T)::out) is det.

:- func singleton_set(T) = test_bitset(T) <= uenum(T).
:- pred singleton_set(test_bitset(T)::out, T::in) is det <= uenum(T).

:- func make_singleton_set(T) = test_bitset(T) <= uenum(T).
:- pred make_singleton_set(test_bitset(T)::out, T::in) is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Emptiness and singleton-ness tests.
%

:- pred is_empty(test_bitset(T)::in) is semidet.

:- pred is_non_empty(test_bitset(T)::in) is semidet.

:- pred is_singleton(test_bitset(T)::in, T::out) is semidet <= uenum(T).

%---------------------------------------------------------------------------%
%
% Membership tests.
%

:- pred member(T, test_bitset(T)) <= uenum(T).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

:- pred contains(test_bitset(T)::in, T::in) is semidet <= uenum(T).

%---------------------------------------------------------------------------%
%
% Insertions and deletions.
%

:- pred insert(T::in, test_bitset(T)::in, test_bitset(T)::out)
    is det <= uenum(T).
:- pred insert_new(T::in, test_bitset(T)::in, test_bitset(T)::out)
    is semidet <= uenum(T).
:- pred insert_list(list(T)::in, test_bitset(T)::in, test_bitset(T)::out)
    is det <= uenum(T).

:- pred delete(T::in, test_bitset(T)::in, test_bitset(T)::out)
    is det <= uenum(T).
:- pred delete_list(list(T)::in, test_bitset(T)::in, test_bitset(T)::out)
    is det <= uenum(T).

:- pred remove(T::in, test_bitset(T)::in, test_bitset(T)::out)
    is semidet <= uenum(T).
:- pred remove_list(list(T)::in, test_bitset(T)::in, test_bitset(T)::out)
    is semidet <= uenum(T).
:- pred remove_least(T::out, test_bitset(T)::in, test_bitset(T)::out)
    is semidet <= uenum(T).

:- pred remove_leq(test_bitset(T)::in, T::in, test_bitset(T)::out)
    is det <= uenum(T).
:- pred remove_gt(test_bitset(T)::in, T::in, test_bitset(T)::out)
    is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Comparisons between sets.
%

:- pred equal(test_bitset(T)::in, test_bitset(T)::in) is semidet <= uenum(T).

:- pred subset(test_bitset(T)::in, test_bitset(T)::in) is semidet.

:- pred superset(test_bitset(T)::in, test_bitset(T)::in) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on two or more sets.
%

:- func union(test_bitset(T), test_bitset(T)) = test_bitset(T) <= uenum(T).
:- pred union(test_bitset(T)::in,
    test_bitset(T)::in, test_bitset(T)::out) is det <= uenum(T).

:- func union_list(list(test_bitset(T))) = test_bitset(T) <= uenum(T).
:- pred union_list(list(test_bitset(T))::in, test_bitset(T)::out) is det
    <= uenum(T).

:- func intersect(test_bitset(T), test_bitset(T)) = test_bitset(T) <= uenum(T).
:- pred intersect(test_bitset(T)::in,
    test_bitset(T)::in, test_bitset(T)::out) is det <= uenum(T).

:- func intersect_list(list(test_bitset(T))) = test_bitset(T) <= uenum(T).
:- pred intersect_list(list(test_bitset(T))::in, test_bitset(T)::out) is det
    <= uenum(T).

:- func difference(test_bitset(T), test_bitset(T)) = test_bitset(T)
    <= uenum(T).
:- pred difference(test_bitset(T)::in,
    test_bitset(T)::in, test_bitset(T)::out) is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Operations that divide a set into two parts.
%

:- pred divide(pred(T)::in(pred(in) is semidet), test_bitset(T)::in,
    test_bitset(T)::out, test_bitset(T)::out) is det <= uenum(T).

:- pred divide_by_set(test_bitset(T)::in, test_bitset(T)::in,
    test_bitset(T)::out, test_bitset(T)::out) is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Converting lists to sets.
%

:- func list_to_set(list(T)) = test_bitset(T) <= uenum(T).
:- pred list_to_set(list(T)::in, test_bitset(T)::out) is det <= uenum(T).

:- func sorted_list_to_set(list(T)) = test_bitset(T) <= uenum(T).
:- pred sorted_list_to_set(list(T)::in, test_bitset(T)::out) is det
    <= uenum(T).

%---------------------------------------------------------------------------%
%
% Converting sets to lists.
%

:- func to_sorted_list(test_bitset(T)) = list(T) <= uenum(T).
:- pred to_sorted_list(test_bitset(T)::in, list(T)::out) is det <= uenum(T).

%---------------------------------------------------------------------------%
%
% Converting between different kinds of sets.
%

:- func set_to_bitset(set(T)) = test_bitset(T) <= uenum(T).
:- func from_set(set(T)) = test_bitset(T) <= uenum(T).

:- func bitset_to_set(test_bitset(T)) = set(T) <= uenum(T).
:- func to_set(test_bitset(T)) = set(T) <= uenum(T).

%---------------------------------------------------------------------------%
%
% Counting.
%

:- func count(test_bitset(T)) = int <= uenum(T).

%---------------------------------------------------------------------------%
%
% Standard higher order functions on collections.
%

    % all_true(Pred, Set) succeeds iff Pred(Element) succeeds
    % for all the elements of Set.
    %
:- pred all_true(pred(T)::in(pred(in) is semidet), test_bitset(T)::in)
    is semidet <= uenum(T).

:- func filter(pred(T)::in(pred(in) is semidet), test_bitset(T)::in)
    = (test_bitset(T)::out) is det <= uenum(T).
:- pred filter(pred(T)::in(pred(in) is semidet),
    test_bitset(T)::in, test_bitset(T)::out, test_bitset(T)::out)
    is det <= uenum(T).

:- func foldl(func(T, Acc) = Acc, test_bitset(T), Acc) = Acc <= uenum(T).
:- mode foldl((func(in, in) = out is det), in, in) = out is det.

:- pred foldl(pred(T, Acc, Acc), test_bitset(T), Acc, Acc) <= uenum(T).
:- mode foldl(in(pred(in, in, out) is det), in, in, out) is det.
:- mode foldl(in(pred(in, in, out) is semidet), in, in, out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module fat_sparse_bitset.
:- import_module fatter_sparse_bitset.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set_ordlist.
:- import_module solutions.
:- import_module sparse_bitset.
:- import_module string.
:- import_module tree_bitset.
:- import_module uint.

:- type test_bitset(T)
    --->    tb(
                tree_bitset(T),
                sparse_bitset(T),
                fat_sparse_bitset(T),
                fatter_sparse_bitset(T),
                set_ordlist(T)
            ).

%---------------------------------------------------------------------------%

init =
    tb(
        tree_bitset.init,
        sparse_bitset.init,
        fat_sparse_bitset.init,
        fatter_sparse_bitset.init,
        set_ordlist.init).

init(init).

singleton_set(A) =
    tb(
        tree_bitset.make_singleton_set(A),
        sparse_bitset.make_singleton_set(A),
        fat_sparse_bitset.make_singleton_set(A),
        fatter_sparse_bitset.make_singleton_set(A),
        set_ordlist.make_singleton_set(A)).

singleton_set(test_bitset.singleton_set(A), A).

make_singleton_set(A) =
    tb(
        tree_bitset.make_singleton_set(A),
        sparse_bitset.make_singleton_set(A),
        fat_sparse_bitset.make_singleton_set(A),
        fatter_sparse_bitset.make_singleton_set(A),
        set_ordlist.make_singleton_set(A)).

make_singleton_set(test_bitset.make_singleton_set(A), A).

%---------------------------------------------------------------------------%

is_empty(tb(A, B, C, D, S)) :-
    ( if tree_bitset.is_empty(A) then EA = yes else EA = no),
    ( if sparse_bitset.is_empty(B) then EB = yes else EB = no),
    ( if fat_sparse_bitset.is_empty(C) then EC = yes else EC = no),
    ( if fatter_sparse_bitset.is_empty(D) then ED = yes else ED = no),
    ( if set_ordlist.is_empty(S) then ES = yes else ES = no),
    ( if EA = ES, EB = ES, EC = ES, ED = ES then
        ES = yes
    else
        unexpected($pred, "failed")
    ).

is_non_empty(tb(A, B, C, D, S)) :-
    ( if tree_bitset.is_non_empty(A) then NEA = yes else NEA = no),
    ( if sparse_bitset.is_non_empty(B) then NEB = yes else NEB = no),
    ( if fat_sparse_bitset.is_non_empty(C) then NEC = yes else NEC = no),
    ( if fatter_sparse_bitset.is_non_empty(D) then NED = yes else NED = no),
    ( if set_ordlist.is_non_empty(S) then NES = yes else NES = no),
    ( if NEA = NES, NEB = NES, NEC = NES, NED = NES then
        NES = yes
    else
        unexpected($pred, "failed")
    ).

is_singleton(tb(A, B, C, D, S), R) :-
    ( if tree_bitset.is_singleton(A, AR) then
        ResA = yes(AR)
    else
        ResA = no
    ),
    ( if sparse_bitset.is_singleton(B, BR) then
        ResB = yes(BR)
    else
        ResB = no
    ),
    ( if fat_sparse_bitset.is_singleton(C, CR) then
        ResC = yes(CR)
    else
        ResC = no
    ),
    ( if fatter_sparse_bitset.is_singleton(D, DR) then
        ResD = yes(DR)
    else
        ResD = no
    ),
    ( if set_ordlist.is_singleton(S, SR) then
        ResS = yes(SR)
    else
        ResS = no
    ),
    ( if ResA = ResS, ResB = ResS, ResC = ResS, ResD = ResS then
        ResS = yes(R)
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%

:- pragma promise_equivalent_clauses(pred(member/2)).

member(E::in, tb(SetA, SetB, SetC, SetD, SetS)::in) :-
    ( if tree_bitset.member(E, SetA) then InA = yes else InA = no),
    ( if sparse_bitset.member(E, SetB) then InB = yes else InB = no),
    ( if fat_sparse_bitset.member(E, SetC) then InC = yes else InC = no),
    ( if fatter_sparse_bitset.member(E, SetD) then InD = yes else InD = no),
    ( if set_ordlist.member(E, SetS) then InS = yes else InS = no),
    ( if InA = InS, InB = InS, InC = InS, InD = InS then
        InS = yes
    else
        unexpected($pred, "failed (in, in)")
    ).

member(E::out, tb(SetA, SetB, SetC, SetD, SetS)::in) :-
    PredA = (pred(EA::out) is nondet :- tree_bitset.member(EA, SetA)),
    PredB = (pred(EB::out) is nondet :- sparse_bitset.member(EB, SetB)),
    PredC = (pred(EC::out) is nondet :- fat_sparse_bitset.member(EC, SetC)),
    PredD = (pred(ED::out) is nondet :- fatter_sparse_bitset.member(ED, SetD)),
    PredS = (pred(ES::out) is nondet :- set_ordlist.member(ES, SetS)),
    solutions(PredA, SolA),
    solutions(PredB, SolB),
    solutions(PredC, SolC),
    solutions(PredD, SolD),
    solutions(PredS, SolS),
    ( if SolA = SolS, SolB = SolS, SolC = SolS, SolD = SolS then
        set_ordlist.member(E, SetS)
    else
        unexpected($pred, "failed (out, in)")
    ).

contains(tb(SetA, SetB, SetC, SetD, SetS), E) :-
    ( if tree_bitset.contains(SetA, E) then InA = yes else InA = no),
    ( if sparse_bitset.contains(SetB, E) then InB = yes else InB = no),
    ( if fat_sparse_bitset.contains(SetC, E) then InC = yes else InC = no),
    ( if fatter_sparse_bitset.contains(SetD, E) then InD = yes else InD = no),
    ( if set_ordlist.contains(SetS, E) then InS = yes else InS = no),
    ( if InA = InS, InB = InS, InC = InS, InD = InS then
        InS = yes
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%

insert(E, In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Result) :-
    tree_bitset.insert(E, SetA0, SetA),
    sparse_bitset.insert(E, SetB0, SetB),
    fat_sparse_bitset.insert(E, SetC0, SetC),
    fatter_sparse_bitset.insert(E, SetD0, SetD),
    set_ordlist.insert(E, SetS0, SetS),
    check1("insert", In, tb(SetA, SetB, SetC, SetD, SetS), Result).

insert_new(E, In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Result) :-
    ( if tree_bitset.insert_new(E, SetA0, SetA) then
        MaybeA = yes(SetA)
    else
        MaybeA = no
    ),
    ( if sparse_bitset.insert_new(E, SetB0, SetB) then
        MaybeB = yes(SetB)
    else
        MaybeB = no
    ),
    ( if fat_sparse_bitset.insert_new(E, SetC0, SetC) then
        MaybeC = yes(SetC)
    else
        MaybeC = no
    ),
    ( if fatter_sparse_bitset.insert_new(E, SetD0, SetD) then
        MaybeD = yes(SetD)
    else
        MaybeD = no
    ),
    ( if set_ordlist.insert_new(E, SetS0, SetS) then
        MaybeS = yes(SetS)
    else
        MaybeS = no
    ),
    ( if
        MaybeA = yes(A),
        MaybeB = yes(B),
        MaybeC = yes(C),
        MaybeD = yes(D),
        MaybeS = yes(S)
    then
        check1("insert_new", In, tb(A, B, C, D, S), Result)
    else if
        MaybeA = no,
        MaybeB = no,
        MaybeC = no,
        MaybeD = no,
        MaybeS = no
    then
        fail
    else
        unexpected($pred, "failed")
    ).

insert_list(Es, In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Result) :-
    tree_bitset.insert_list(Es, SetA0, SetA),
    sparse_bitset.insert_list(Es, SetB0, SetB),
    fat_sparse_bitset.insert_list(Es, SetC0, SetC),
    fatter_sparse_bitset.insert_list(Es, SetD0, SetD),
    set_ordlist.insert_list(Es, SetS0, SetS),
    check1("insert_list", In, tb(SetA, SetB, SetC, SetD, SetS), Result).

delete(E, In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Result) :-
    tree_bitset.delete(E, SetA0, SetA),
    sparse_bitset.delete(E, SetB0, SetB),
    fat_sparse_bitset.delete(E, SetC0, SetC),
    fatter_sparse_bitset.delete(E, SetD0, SetD),
    set_ordlist.delete(E, SetS0, SetS),
    check1("delete", In, tb(SetA, SetB, SetC, SetD, SetS), Result).

delete_list(Es, In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Result) :-
    tree_bitset.delete_list(Es, SetA0, SetA),
    sparse_bitset.delete_list(Es, SetB0, SetB),
    fat_sparse_bitset.delete_list(Es, SetC0, SetC),
    fatter_sparse_bitset.delete_list(Es, SetD0, SetD),
    set_ordlist.delete_list(Es, SetS0, SetS),
    check1("delete_list", In, tb(SetA, SetB, SetC, SetD, SetS), Result).

remove(E, In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Result) :-
    ( if tree_bitset.remove(E, SetA0, SetA) then
        MaybeA = yes(SetA)
    else
        MaybeA = no
    ),
    ( if sparse_bitset.remove(E, SetB0, SetB) then
        MaybeB = yes(SetB)
    else
        MaybeB = no
    ),
    ( if fat_sparse_bitset.remove(E, SetC0, SetC) then
        MaybeC = yes(SetC)
    else
        MaybeC = no
    ),
    ( if fatter_sparse_bitset.remove(E, SetD0, SetD) then
        MaybeD = yes(SetD)
    else
        MaybeD = no
    ),
    ( if set_ordlist.remove(E, SetS0, SetS) then
        MaybeS = yes(SetS)
    else
        MaybeS = no
    ),
    ( if
        MaybeA = yes(A),
        MaybeB = yes(B),
        MaybeC = yes(C),
        MaybeD = yes(D),
        MaybeS = yes(S)
    then
        check1("remove", In, tb(A, B, C, D, S), Result)
    else if
        MaybeA = no,
        MaybeB = no,
        MaybeC = no,
        MaybeD = no,
        MaybeS = no
    then
        fail
    else
        unexpected($pred, "failed")
    ).

remove_list(Es, In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Result) :-
    ( if tree_bitset.remove_list(Es, SetA0, SetA) then
        MaybeA = yes(SetA)
    else
        MaybeA = no
    ),
    ( if sparse_bitset.remove_list(Es, SetB0, SetB) then
        MaybeB = yes(SetB)
    else
        MaybeB = no
    ),
    ( if fat_sparse_bitset.remove_list(Es, SetC0, SetC) then
        MaybeC = yes(SetC)
    else
        MaybeC = no
    ),
    ( if fatter_sparse_bitset.remove_list(Es, SetD0, SetD) then
        MaybeD = yes(SetD)
    else
        MaybeD = no
    ),
    ( if set_ordlist.remove_list(Es, SetS0, SetS) then
        MaybeS = yes(SetS)
    else
        MaybeS = no
    ),
    ( if
        MaybeA = yes(A),
        MaybeB = yes(B),
        MaybeC = yes(C),
        MaybeD = yes(D),
        MaybeS = yes(S)
    then
        check1("remove", In, tb(A, B, C, D, S), Result)
    else if
        MaybeA = no,
        MaybeB = no,
        MaybeC = no,
        MaybeD = no,
        MaybeS = no
    then
        fail
    else
        unexpected($pred, "failed")
    ).

remove_least(Least, In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Result) :-
    ( if tree_bitset.remove_least(LeastA, SetA0, SetA) then
        MaybeA = yes(LeastA - SetA)
    else
        MaybeA = no
    ),
    ( if sparse_bitset.remove_least(LeastB, SetB0, SetB) then
        MaybeB = yes(LeastB - SetB)
    else
        MaybeB = no
    ),
    ( if fat_sparse_bitset.remove_least(LeastC, SetC0, SetC) then
        MaybeC = yes(LeastC - SetC)
    else
        MaybeC = no
    ),
    ( if fatter_sparse_bitset.remove_least(LeastD, SetD0, SetD) then
        MaybeD = yes(LeastD - SetD)
    else
        MaybeD = no
    ),
    ( if set_ordlist.remove_least(LeastS, SetS0, SetS) then
        MaybeS = yes(LeastS - SetS)
    else
        MaybeS = no
    ),
    ( if
        MaybeA = yes(LA - SA),
        MaybeB = yes(LB - SB),
        MaybeC = yes(LC - SC),
        MaybeD = yes(LD - SD),
        MaybeS = yes(LS - SS),
        LA = LS, LB = LS, LC = LS, LD = LS
    then
        Least = LS,
        check1("remove_least", In, tb(SA, SB, SC, SD, SS), Result)
    else if
        MaybeA = no,
        MaybeB = no,
        MaybeC = no,
        MaybeD = no,
        MaybeS = no
    then
        fail
    else
        unexpected($pred, "failed")
    ).

remove_leq(In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Hurdle, Result) :-
    tree_bitset.remove_leq(Hurdle, SetA0, SetA),
    sparse_bitset.remove_leq(Hurdle, SetB0, SetB),
    fat_sparse_bitset.remove_leq(Hurdle, SetC0, SetC),
    fatter_sparse_bitset.remove_leq(Hurdle, SetD0, SetD),
    RemoveLeq =
        ( pred(Item::in) is semidet :-
            Index = to_uint(Item),
            HurdleIndex = to_uint(Hurdle),
            not (Index =< HurdleIndex)
        ),
    set.filter(RemoveLeq, SetS0, SetS),
    check1("remove_leq", In, tb(SetA, SetB, SetC, SetD, SetS), Result).

remove_gt(In @ tb(SetA0, SetB0, SetC0, SetD0, SetS0), Hurdle, Result) :-
    tree_bitset.remove_gt(Hurdle, SetA0, SetA),
    sparse_bitset.remove_gt(Hurdle, SetB0, SetB),
    fat_sparse_bitset.remove_gt(Hurdle, SetC0, SetC),
    fatter_sparse_bitset.remove_gt(Hurdle, SetD0, SetD),
    RemoveGt =
        ( pred(Item::in) is semidet :-
            Index = to_uint(Item),
            HurdleIndex = to_uint(Hurdle),
            not (Index > HurdleIndex)
        ),
    set.filter(RemoveGt, SetS0, SetS),
    check1("remove_gt", In, tb(SetA, SetB, SetC, SetD, SetS), Result).

%---------------------------------------------------------------------------%

equal(InL, InR) :-
    InL = tb(SetAL, SetBL, SetCL, SetDL, SetSL),
    InR = tb(SetAR, SetBR, SetCR, SetDR, SetSR),
    ( if tree_bitset.equal(SetAL, SetAR) then A = yes else A = no),
    ( if sparse_bitset.equal(SetBL, SetBR) then B = yes else B = no),
    ( if fat_sparse_bitset.equal(SetCL, SetCR) then C = yes else C = no),
    ( if fatter_sparse_bitset.equal(SetDL, SetDR) then D = yes else D = no),
    ( if set_ordlist.equal(SetSL, SetSR) then S = yes else S = no),
    ( if A = S, B = S, C = S, D = S then
        S = yes
    else
        unexpected($pred, "failed")
    ).

subset(InL, InR) :-
    InL = tb(SetAL, SetBL, SetCL, SetDL, SetSL),
    InR = tb(SetAR, SetBR, SetCR, SetDR, SetSR),
    ( if tree_bitset.subset(SetAL, SetAR) then A = yes else A = no),
    ( if sparse_bitset.subset(SetBL, SetBR) then B = yes else B = no),
    ( if fat_sparse_bitset.subset(SetCL, SetCR) then C = yes else C = no),
    ( if fatter_sparse_bitset.subset(SetDL, SetDR) then D = yes else D = no),
    ( if set_ordlist.subset(SetSL, SetSR) then S = yes else S = no),
    ( if A = S, B = S, C = S, D = S then
        S = yes
    else
        unexpected($pred, "failed")
    ).

superset(InL, InR) :-
    InL = tb(SetAL, SetBL, SetCL, SetDL, SetSL),
    InR = tb(SetAR, SetBR, SetCR, SetDR, SetSR),
    ( if tree_bitset.superset(SetAL, SetAR) then A = yes else A = no),
    ( if sparse_bitset.superset(SetBL, SetBR) then B = yes else B = no),
    ( if fat_sparse_bitset.superset(SetCL, SetCR) then C = yes else C = no),
    ( if fatter_sparse_bitset.superset(SetDL, SetDR) then D = yes else D = no),
    ( if set_ordlist.superset(SetSL, SetSR) then S = yes else S = no),
    ( if A = S, B = S, C = S, D = S then
        S = yes
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%

union(InL, InR) = Result :-
    InL = tb(SetAL, SetBL, SetCL, SetDL, SetSL),
    InR = tb(SetAR, SetBR, SetCR, SetDR, SetSR),
    tree_bitset.union(SetAL, SetAR, SetA),
    sparse_bitset.union(SetBL, SetBR, SetB),
    fat_sparse_bitset.union(SetCL, SetCR, SetC),
    fatter_sparse_bitset.union(SetDL, SetDR, SetD),
    set_ordlist.union(SetSL, SetSR, SetS),
    check2("union", InL, InR, tb(SetA, SetB, SetC, SetD, SetS), Result).

union(A, B, test_bitset.union(A, B)).

union_list(SetsABCS) = Result :-
    get_sets("union_list", SetsABCS, SetsA, SetsB, SetsC, SetsD, SetsS),
    SetA = tree_bitset.union_list(SetsA),
    SetB = sparse_bitset.union_list(SetsB),
    SetC = fat_sparse_bitset.union_list(SetsC),
    SetD = fatter_sparse_bitset.union_list(SetsD),
    SetS = set_ordlist.union_list(SetsS),
    check0("union_list", tb(SetA, SetB, SetC, SetD, SetS), Result).

union_list(Sets, test_bitset.union_list(Sets)).

intersect(InL, InR) = Result :-
    InL = tb(SetAL, SetBL, SetCL, SetDL, SetSL),
    InR = tb(SetAR, SetBR, SetCR, SetDR, SetSR),
    tree_bitset.intersect(SetAL, SetAR, SetA),
    sparse_bitset.intersect(SetBL, SetBR, SetB),
    fat_sparse_bitset.intersect(SetCL, SetCR, SetC),
    fatter_sparse_bitset.intersect(SetDL, SetDR, SetD),
    set_ordlist.intersect(SetSL, SetSR, SetS),
    check2("intersect", InL, InR, tb(SetA, SetB, SetC, SetD, SetS), Result).

intersect(A, B, test_bitset.intersect(A, B)).

intersect_list(SetsABCS) = Result :-
    get_sets("intersect_list", SetsABCS, SetsA, SetsB, SetsC, SetsD, SetsS),
    SetA = tree_bitset.intersect_list(SetsA),
    SetB = sparse_bitset.intersect_list(SetsB),
    SetC = fat_sparse_bitset.intersect_list(SetsC),
    SetD = fatter_sparse_bitset.intersect_list(SetsD),
    SetS = set_ordlist.intersect_list(SetsS),
    check0("intersect_list", tb(SetA, SetB, SetC, SetD, SetS), Result).

intersect_list(Sets, test_bitset.intersect_list(Sets)).

difference(InL, InR) = Result :-
    InL = tb(SetAL, SetBL, SetCL, SetDL, SetSL),
    InR = tb(SetAR, SetBR, SetCR, SetDR, SetSR),
    tree_bitset.difference(SetAL, SetAR, SetA),
    sparse_bitset.difference(SetBL, SetBR, SetB),
    fat_sparse_bitset.difference(SetCL, SetCR, SetC),
    fatter_sparse_bitset.difference(SetDL, SetDR, SetD),
    set_ordlist.difference(SetSL, SetSR, SetS),
    check2("difference", InL, InR, tb(SetA, SetB, SetC, SetD, SetS), Result).

difference(A, B, test_bitset.difference(A, B)).

%---------------------%

:- pred get_sets(string::in, list(test_bitset(T))::in,
    list(tree_bitset(T))::out, list(sparse_bitset(T))::out,
    list(fat_sparse_bitset(T))::out, list(fatter_sparse_bitset(T))::out,
    list(set_ordlist(T))::out) is det <= uenum(T).

get_sets(_, [], [], [], [], [], []).
get_sets(Op, [tb(SetA, SetB, SetC, SetD, SetS) | SetsABCDS],
        [SetA | SetsA], [SetB | SetsB], [SetC | SetsC], [SetD | SetsD],
        [SetS | SetsS]) :-
    tree_bitset.to_sorted_list(SetA, ListA),
    sparse_bitset.to_sorted_list(SetB, ListB),
    fat_sparse_bitset.to_sorted_list(SetC, ListC),
    fatter_sparse_bitset.to_sorted_list(SetD, ListD),
    set_ordlist.to_sorted_list(SetS, ListS),
    ( if ListA = ListS, ListB = ListS, ListC = ListS, ListD = ListS then
        get_sets(Op, SetsABCDS, SetsA, SetsB, SetsC, SetsD, SetsS)
    else
        unexpected($pred, "unequal sets in " ++ Op ++ " arg list")
    ).

%---------------------------------------------------------------------------%

divide(Pred, tb(SetA, SetB, SetC, SetD, SetS), ResultIn, ResultOut) :-
    tree_bitset.divide(Pred, SetA, InSetA, OutSetA),
    sparse_bitset.divide(Pred, SetB, InSetB, OutSetB),
    fat_sparse_bitset.divide(Pred, SetC, InSetC, OutSetC),
    fatter_sparse_bitset.divide(Pred, SetD, InSetD, OutSetD),
    set_ordlist.divide(Pred, SetS, InSetS, OutSetS),

    tree_bitset.to_sorted_list(SetA, ListA),
    tree_bitset.to_sorted_list(InSetA, InListA),
    tree_bitset.to_sorted_list(OutSetA, OutListA),
    sparse_bitset.to_sorted_list(SetB, ListB),
    sparse_bitset.to_sorted_list(InSetB, InListB),
    sparse_bitset.to_sorted_list(OutSetB, OutListB),
    fat_sparse_bitset.to_sorted_list(SetC, ListC),
    fat_sparse_bitset.to_sorted_list(InSetC, InListC),
    fat_sparse_bitset.to_sorted_list(OutSetC, OutListC),
    fatter_sparse_bitset.to_sorted_list(SetD, ListD),
    fatter_sparse_bitset.to_sorted_list(InSetD, InListD),
    fatter_sparse_bitset.to_sorted_list(OutSetD, OutListD),
    set_ordlist.to_sorted_list(SetS, ListS),
    set_ordlist.to_sorted_list(InSetS, InListS),
    set_ordlist.to_sorted_list(OutSetS, OutListS),
    ( if
        ListA = ListS,
        ListB = ListS,
        ListC = ListS,
        ListD = ListS,
        InListA = InListS,
        InListB = InListS,
        InListC = InListS,
        InListD = InListS,
        OutListA = OutListS,
        OutListB = OutListS,
        OutListC = OutListS,
        OutListD = OutListS
    then
        ResultIn = tb(InSetA, InSetB, InSetC, InSetD, InSetS),
        ResultOut = tb(OutSetA, OutSetB, OutSetC, OutSetD, OutSetS)
    else
        unexpected($pred, "failed")
    ).

divide_by_set(DivBy, Set, ResultIn, ResultOut) :-
    DivBy = tb(DivByA, DivByB, DivByC, DivByD, DivByS),
    Set = tb(SetA, SetB, SetC, SetD, SetS),
    tree_bitset.divide_by_set(DivByA, SetA, InSetA, OutSetA),
    sparse_bitset.divide_by_set(DivByB, SetB, InSetB, OutSetB),
    fat_sparse_bitset.divide_by_set(DivByC, SetC, InSetC, OutSetC),
    fatter_sparse_bitset.divide_by_set(DivByD, SetD, InSetD, OutSetD),
    set_ordlist.divide_by_set(DivByS, SetS, InSetS, OutSetS),

    tree_bitset.to_sorted_list(DivByA, DivListA),
    tree_bitset.to_sorted_list(SetA, ListA),
    tree_bitset.to_sorted_list(InSetA, InListA),
    tree_bitset.to_sorted_list(OutSetA, OutListA),
    sparse_bitset.to_sorted_list(DivByB, DivListB),
    sparse_bitset.to_sorted_list(SetB, ListB),
    sparse_bitset.to_sorted_list(InSetB, InListB),
    sparse_bitset.to_sorted_list(OutSetB, OutListB),
    fat_sparse_bitset.to_sorted_list(DivByC, DivListC),
    fat_sparse_bitset.to_sorted_list(SetC, ListC),
    fat_sparse_bitset.to_sorted_list(InSetC, InListC),
    fat_sparse_bitset.to_sorted_list(OutSetC, OutListC),
    fatter_sparse_bitset.to_sorted_list(DivByD, DivListD),
    fatter_sparse_bitset.to_sorted_list(SetD, ListD),
    fatter_sparse_bitset.to_sorted_list(InSetD, InListD),
    fatter_sparse_bitset.to_sorted_list(OutSetD, OutListD),
    set_ordlist.to_sorted_list(DivByS, DivListS),
    set_ordlist.to_sorted_list(SetS, ListS),
    set_ordlist.to_sorted_list(InSetS, InListS),
    set_ordlist.to_sorted_list(OutSetS, OutListS),
    ( if
        DivListA = DivListS,
        DivListB = DivListS,
        DivListC = DivListS,
        DivListD = DivListS,
        ListA = ListS,
        ListB = ListS,
        ListC = ListS,
        ListD = ListS,
        InListA = InListS,
        InListB = InListS,
        InListC = InListS,
        InListD = InListS,
        OutListA = OutListS,
        OutListB = OutListS,
        OutListC = OutListS,
        OutListD = OutListS
    then
        ResultIn = tb(InSetA, InSetB, InSetC, InSetD, InSetS),
        ResultOut = tb(OutSetA, OutSetB, OutSetC, OutSetD, OutSetS)
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%

list_to_set(List) = Result :-
    SetA = tree_bitset.list_to_set(List),
    SetB = sparse_bitset.list_to_set(List),
    SetC = fat_sparse_bitset.list_to_set(List),
    SetD = fatter_sparse_bitset.list_to_set(List),
    SetS = set_ordlist.list_to_set(List),
    check0("list_to_set", tb(SetA, SetB, SetC, SetD, SetS), Result).

list_to_set(A, test_bitset.list_to_set(A)).

sorted_list_to_set(List) = Result :-
    SetA = tree_bitset.sorted_list_to_set(List),
    SetB = sparse_bitset.sorted_list_to_set(List),
    SetC = fat_sparse_bitset.sorted_list_to_set(List),
    SetD = fatter_sparse_bitset.sorted_list_to_set(List),
    SetS = set_ordlist.sorted_list_to_set(List),
    check0("sorted_list_to_set", tb(SetA, SetB, SetC, SetD, SetS), Result).

sorted_list_to_set(A, test_bitset.sorted_list_to_set(A)).

%---------------------------------------------------------------------------%

to_sorted_list(tb(A, B, C, D, S)) = List :-
    ListA = tree_bitset.to_sorted_list(A),
    ListB = sparse_bitset.to_sorted_list(B),
    ListC = fat_sparse_bitset.to_sorted_list(C),
    ListD = fatter_sparse_bitset.to_sorted_list(D),
    ListS = set_ordlist.to_sorted_list(S),
    ( if ListA = ListS, ListB = ListS, ListC = ListS, ListD = ListS then
        List = ListS
    else
        unexpected($pred, "failed")
    ).

to_sorted_list(A, test_bitset.to_sorted_list(A)).

%---------------------------------------------------------------------------%

set_to_bitset(Set) = Result :-
    set.to_sorted_list(Set, SortedList),
    Result = test_bitset.sorted_list_to_set(SortedList).

from_set(Set) = set_to_bitset(Set).

bitset_to_set(TestBitset) = Set :-
    SortedList = test_bitset.to_sorted_list(TestBitset),
    set.sorted_list_to_set(SortedList, Set).

to_set(Set) = bitset_to_set(Set).

%---------------------------------------------------------------------------%

count(tb(SetA, SetB, SetC, SetD, SetS)) = Cnt :-
    CntA = tree_bitset.count(SetA),
    CntB = sparse_bitset.count(SetB),
    CntC = fat_sparse_bitset.count(SetC),
    CntD = fatter_sparse_bitset.count(SetD),
    CntS = set_ordlist.count(SetS),
    ( if CntA = CntS, CntB = CntS, CntC = CntS, CntD = CntS then
        Cnt = CntS
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%

all_true(Pred, tb(SetA, SetB, SetC, SetD, SetS)) :-
    ( if tree_bitset.all_true(Pred, SetA) then
        MaybeA = yes
    else
        MaybeA = no
    ),
    ( if sparse_bitset.all_true(Pred, SetB) then
        MaybeB = yes
    else
        MaybeB = no
    ),
    ( if fat_sparse_bitset.all_true(Pred, SetC) then
        MaybeC = yes
    else
        MaybeC = no
    ),
    ( if fatter_sparse_bitset.all_true(Pred, SetD) then
        MaybeD = yes
    else
        MaybeD = no
    ),
    ( if set_ordlist.all_true(Pred, SetS) then
        MaybeS = yes
    else
        MaybeS = no
    ),
    ( if
        MaybeA = MaybeS, MaybeB = MaybeS, MaybeC = MaybeS, MaybeD = MaybeS
    then
        MaybeS = yes
    else
        unexpected($pred, "failed")
    ).

filter(Pred, tb(SetA, SetB, SetC, SetD, SetS)) = Result :-
    tree_bitset.to_sorted_list(SetA, ListA),
    sparse_bitset.to_sorted_list(SetB, ListB),
    fat_sparse_bitset.to_sorted_list(SetC, ListC),
    fatter_sparse_bitset.to_sorted_list(SetD, ListD),
    set_ordlist.to_sorted_list(SetS, ListS),

    InSetA = tree_bitset.filter(Pred, SetA),
    InSetB = sparse_bitset.filter(Pred, SetB),
    InSetC = fat_sparse_bitset.filter(Pred, SetC),
    InSetD = fatter_sparse_bitset.filter(Pred, SetD),
    InSetS = set_ordlist.filter(Pred, SetS),

    tree_bitset.to_sorted_list(InSetA, InListA),
    sparse_bitset.to_sorted_list(InSetB, InListB),
    fat_sparse_bitset.to_sorted_list(InSetC, InListC),
    fatter_sparse_bitset.to_sorted_list(InSetD, InListD),
    set_ordlist.to_sorted_list(InSetS, InListS),

    ( if
        ListA = ListS,
        ListB = ListS,
        ListC = ListS,
        ListD = ListS,
        InListA = InListS,
        InListB = InListS,
        InListC = InListS,
        InListD = InListS
    then
        Result = tb(InSetA, InSetB, InSetC, InSetD, InSetS)
    else
        unexpected($pred, "failed")
    ).

filter(Pred, tb(SetA, SetB, SetC, SetD, SetS), ResultIn, ResultOut) :-
    tree_bitset.to_sorted_list(SetA, ListA),
    sparse_bitset.to_sorted_list(SetB, ListB),
    fat_sparse_bitset.to_sorted_list(SetC, ListC),
    fatter_sparse_bitset.to_sorted_list(SetD, ListD),
    set_ordlist.to_sorted_list(SetS, ListS),

    tree_bitset.filter(Pred, SetA, InSetA, OutSetA),
    sparse_bitset.filter(Pred, SetB, InSetB, OutSetB),
    fat_sparse_bitset.filter(Pred, SetC, InSetC, OutSetC),
    fatter_sparse_bitset.filter(Pred, SetD, InSetD, OutSetD),
    set_ordlist.filter(Pred, SetS, InSetS, OutSetS),

    tree_bitset.to_sorted_list(InSetA, InListA),
    tree_bitset.to_sorted_list(OutSetA, OutListA),
    sparse_bitset.to_sorted_list(InSetB, InListB),
    sparse_bitset.to_sorted_list(OutSetB, OutListB),
    fat_sparse_bitset.to_sorted_list(InSetC, InListC),
    fat_sparse_bitset.to_sorted_list(OutSetC, OutListC),
    fatter_sparse_bitset.to_sorted_list(InSetD, InListD),
    fatter_sparse_bitset.to_sorted_list(OutSetD, OutListD),
    set_ordlist.to_sorted_list(InSetS, InListS),
    set_ordlist.to_sorted_list(OutSetS, OutListS),

    ( if
        ListA = ListS,
        ListB = ListS,
        ListC = ListS,
        ListD = ListS,
        InListA = InListS,
        InListB = InListS,
        InListC = InListS,
        InListD = InListS,
        OutListA = OutListS,
        OutListB = OutListS,
        OutListC = OutListS,
        OutListD = OutListS
    then
        ResultIn = tb(InSetA, InSetB, InSetC, InSetD, InSetS),
        ResultOut = tb(OutSetA, OutSetB, OutSetC, OutSetD, OutSetS)
    else
        unexpected($pred, "failed")
    ).

foldl(Func, tb(SetA, SetB, SetC, SetD, SetS), Acc0) = Acc :-
    tree_bitset.to_sorted_list(SetA, ListA),
    sparse_bitset.to_sorted_list(SetB, ListB),
    fat_sparse_bitset.to_sorted_list(SetC, ListC),
    fatter_sparse_bitset.to_sorted_list(SetD, ListD),
    set_ordlist.to_sorted_list(SetS, ListS),

    tree_bitset.foldl(Func, SetA, Acc0) = AccA,
    sparse_bitset.foldl(Func, SetB, Acc0) = AccB,
    fat_sparse_bitset.foldl(Func, SetC, Acc0) = AccC,
    fatter_sparse_bitset.foldl(Func, SetD, Acc0) = AccD,
    set_ordlist.fold(Func, SetS, Acc0) = AccS,

    ( if
        ListA = ListS, ListB = ListS, ListC = ListS, ListD = ListS,
        AccA = AccS, AccB = AccS, AccC = AccS, AccD = AccS
    then
        Acc = AccS
    else
        unexpected($pred, "failed")
    ).

foldl(Pred, tb(SetA, SetB, SetC, SetD, SetS), Acc0, Acc) :-
    tree_bitset.to_sorted_list(SetA, ListA),
    sparse_bitset.to_sorted_list(SetB, ListB),
    fat_sparse_bitset.to_sorted_list(SetC, ListC),
    fatter_sparse_bitset.to_sorted_list(SetD, ListD),
    set_ordlist.to_sorted_list(SetS, ListS),

    tree_bitset.foldl(Pred, SetA, Acc0, AccA),
    sparse_bitset.foldl(Pred, SetB, Acc0, AccB),
    fat_sparse_bitset.foldl(Pred, SetC, Acc0, AccC),
    fatter_sparse_bitset.foldl(Pred, SetD, Acc0, AccD),
    set_ordlist.fold(Pred, SetS, Acc0, AccS),

    ( if
        ListA = ListS, ListB = ListS, ListC = ListS, ListD = ListS,
        AccA = AccS, AccB = AccS, AccC = AccS, AccD = AccS
    then
        Acc = AccS
    else
        unexpected($pred, "failed")
    ).

%---------------------------------------------------------------------------%
%
% The integrity test operations.
%

:- pred check0(string::in, test_bitset(T)::in, test_bitset(T)::out) is det
    <= uenum(T).

check0(Op, TestIn, Result) :-
    TestIn = tb(InSetA, InSetB, InSetC, InSetD, InSetS),
    tree_bitset.to_sorted_list(InSetA, ListA),
    sparse_bitset.to_sorted_list(InSetB, ListB),
    fat_sparse_bitset.to_sorted_list(InSetC, ListC),
    fatter_sparse_bitset.to_sorted_list(InSetD, ListD),
    set_ordlist.to_sorted_list(InSetS, ListS),
    ( if ListA = ListS, ListB = ListS, ListC = ListS, ListD = ListS then
        Result = TestIn
    else
        throw(zero_argument(Op, TestIn))
    ).

:- pred check1(string::in, test_bitset(T)::in, test_bitset(T)::in,
    test_bitset(T)::out) is det <= uenum(T).

check1(Op, TestIn, TestOut, Result) :-
    TestIn = tb(InSetA, InSetB, InSetC, InSetD, InSetS),
    TestOut = tb(OutSetA, OutSetB, OutSetC, OutSetD, OutSetS),
    tree_bitset.to_sorted_list(InSetA, InsA),
    tree_bitset.to_sorted_list(OutSetA, OutsA),
    sparse_bitset.to_sorted_list(InSetB, InsB),
    sparse_bitset.to_sorted_list(OutSetB, OutsB),
    fat_sparse_bitset.to_sorted_list(InSetC, InsC),
    fat_sparse_bitset.to_sorted_list(OutSetC, OutsC),
    fatter_sparse_bitset.to_sorted_list(InSetD, InsD),
    fatter_sparse_bitset.to_sorted_list(OutSetD, OutsD),
    set_ordlist.to_sorted_list(InSetS, InsS),
    set_ordlist.to_sorted_list(OutSetS, OutsS),
    ( if
        InsA = InsS, InsB = InsS, InsC = InsS, InsD = InsS,
        OutsA = OutsS, OutsB = OutsS, OutsC = OutsS, OutsD = OutsS
    then
        Result = TestOut
    else
        throw(one_argument(Op, TestIn, TestOut))
    ).

:- pred check2(string::in, test_bitset(T)::in, test_bitset(T)::in,
    test_bitset(T)::in, test_bitset(T)::out) is det <= uenum(T).

check2(Op, TestInL, TestInR, TestOut, Result) :-
    TestInL = tb(InSetLA, InSetLB, InSetLC, InSetLD, InSetLS),
    TestInR = tb(InSetRA, InSetRB, InSetRC, InSetRD, InSetRS),
    TestOut = tb(OutSetA, OutSetB, OutSetC, OutSetD, OutSetS),
    tree_bitset.to_sorted_list(InSetLA, InsLA),
    tree_bitset.to_sorted_list(InSetRA, InsRA),
    tree_bitset.to_sorted_list(OutSetA, OutsA),
    sparse_bitset.to_sorted_list(InSetLB, InsLB),
    sparse_bitset.to_sorted_list(InSetRB, InsRB),
    sparse_bitset.to_sorted_list(OutSetB, OutsB),
    fat_sparse_bitset.to_sorted_list(InSetLC, InsLC),
    fat_sparse_bitset.to_sorted_list(InSetRC, InsRC),
    fat_sparse_bitset.to_sorted_list(OutSetC, OutsC),
    fatter_sparse_bitset.to_sorted_list(InSetLD, InsLD),
    fatter_sparse_bitset.to_sorted_list(InSetRD, InsRD),
    fatter_sparse_bitset.to_sorted_list(OutSetD, OutsD),
    set_ordlist.to_sorted_list(InSetLS, InsLS),
    set_ordlist.to_sorted_list(InSetRS, InsRS),
    set_ordlist.to_sorted_list(OutSetS, OutsS),
    ( if
        InsLA = InsLS, InsLB = InsLS, InsLC = InsLS, InsLD = InsLS,
        InsRA = InsRS, InsRB = InsRS, InsRC = InsRS, InsRD = InsRS,
        OutsA = OutsS, OutsB = OutsS, OutsC = OutsS, OutsD = OutsS
    then
        Result = TestOut
    else
        throw(two_arguments(Op, TestInL, TestInR, TestOut))
    ).

%---------------------------------------------------------------------------%
:- end_module test_bitset.
%---------------------------------------------------------------------------%
