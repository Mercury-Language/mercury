%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_injection.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module assoc_list.
:- import_module exception.
:- import_module injection.
:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module univ.

:- type test_inj == injection(int, int).
:- type test_data == assoc_list(int, int).

main(!IO) :-
    injection.init(J0),
    test(is_empty_test(J0), "is_empty", !IO),
    test_data_1(Data1),
    injection.det_insert_from_assoc_list(Data1, J0, J1),
    test(not_is_empty_test(J1), "not is_empty", !IO),
    test(set_same_test(J1, Data1), "set same", !IO),
    test_data_2(Data2),
    test(update_new(J1, Data2), "update new", !IO),
    J = list.foldl((func(K - V, X) = injection.det_update(X, K, V)),
        Data2, J1),
    test_data(Data),
    test(forward_search_succ_1(J, Data), "forward_search succ 1", !IO),
    test(forward_search_succ_2(J, Data), "forward_search succ 2", !IO),
    test(forward_search_fail(J, -1), "forward_search fail", !IO),
    AllData = Data1 ++ Data2,
    test(reverse_search_succ_1(J, AllData), "reverse_search succ 1", !IO),
    test(reverse_search_succ_2(J, AllData), "reverse_search succ 2", !IO),
    test(reverse_search_fail(J, -1), "reverse_search fail", !IO),
    test(lookup_throw(J, 10), "lookup throw", !IO),
    test(reverse_lookup_throw(J, 1), "reverse_lookup throw", !IO),
    test(keys_test(J, Data), "keys", !IO),
    test(values_test(J, AllData), "values", !IO),
    test(insert_fail(J, 1, -1), "insert fail on key", !IO),
    test(insert_fail(J, -1, 10), "insert fail on value 1", !IO),
    test(insert_fail(J, -1, 11), "insert fail on value 2", !IO),
    test(insert_throw(J, 2, -1), "insert throw on key", !IO),
    test(insert_throw(J, -1, 20), "insert throw on value 1", !IO),
    test(insert_throw(J, -1, 21), "insert throw on value 2", !IO),
    test(update_fail(J, -1, -1), "update fail on key", !IO),
    test(update_fail(J, 1, 10), "update fail on value 1", !IO),
    test(update_fail(J, 1, 11), "update fail on value 2", !IO),
    test(update_fail(J, 1, 20), "update fail on value 3", !IO),
    test(update_fail(J, 1, 21), "update fail on value 4", !IO),
    test(update_throw(J, -1, -1), "update throw on key", !IO),
    test(update_throw(J, 2, 20), "update throw on value 1", !IO),
    test(update_throw(J, 2, 21), "update throw on value 2", !IO),
    test(update_throw(J, 2, 30), "update throw on value 3", !IO),
    test(update_throw(J, 2, 31), "update throw on value 4", !IO),
    test(set_succeed(J, 3, 30), "set succeed 1", !IO),
    test(set_succeed(J, 3, 31), "set succeed 2", !IO),
    test(set_succeed(J, 3, -1), "set succeed 3", !IO),
    test(set_fail(J, 1, 20), "set fail 1", !IO),
    test(set_fail(J, 1, 21), "set fail 2", !IO),
    test(set_throw(J, 2, 10), "set throw 1", !IO),
    test(set_throw(J, 2, 11), "set throw 2", !IO),
    test(delete_key_test(J, 3), "delete_key", !IO),
    test(delete_value_succ(J, 30), "delete_value succ", !IO),
    test(delete_value_throw(J, 31), "delete_value throw", !IO),

    % Tests of merging and overlaying injections. Some are these
    % are designed to test that an exception is thrown if the
    % calling conditions are not met. However, we don't actually
    % throw an exception in these cases since the underlying map
    % implementation doesn't either. Hence these cases are
    % disabled for the moment.
    %
    merge_test_data_no_overlap(NoOverlapData),
    injection.det_set_from_assoc_list(NoOverlapData, J0, NoOverlapJ),
    merge_test_data_key_overlap(KeyOverlapData),
    injection.det_set_from_assoc_list(KeyOverlapData, J0, KeyOverlapJ),
    % merge_test_data_value_overlap_1(ValOverlapData1),
    % injection.det_set_from_assoc_list(ValOverlapData1, J0, ValOverlapJ1),
    % merge_test_data_value_overlap_2(ValOverlapData2),
    % injection.det_set_from_assoc_list(ValOverlapData2, J0, ValOverlapJ2),
    test(merge_succ(J, NoOverlapJ), "merge no overlap", !IO),
    % test(merge_throw(J, KeyOverlapJ), "merge key overlap", !IO),
    % test(merge_throw(J, ValOverlapJ1), "merge value overlap 1", !IO),
    % test(merge_throw(J, ValOverlapJ2), "merge value overlap 2", !IO),
    test(overlay_succ(J, NoOverlapJ), "overlay no overlap", !IO),
    test(overlay_succ(J, KeyOverlapJ), "overlay key overlap", !IO),
    % test(overlay_throw(J, ValOverlapJ1), "overlay value overlap 1", !IO),
    % test(overlay_throw(J, ValOverlapJ2), "overlay value overlap 2", !IO),

    InjectiveMap = (func(_, V) = -V),
    NonInjectiveMap = (func(_, _) = 0),
    test(map_keys_test(J, InjectiveMap), "map_keys injective", !IO),
    test(map_keys_test(J, NonInjectiveMap), "map_keys non-injective", !IO),
    test(map_values_test(J, InjectiveMap), "map_values injective", !IO),
    test(map_values_test(J, NonInjectiveMap), "map_values non-injective",
        !IO).

:- pred test_data_1(test_data::out) is det.

test_data_1([1 - 10, 2 - 20, 3 - 30, 4 - 40, 5 - 50]).

:- pred test_data_2(test_data::out) is det.

test_data_2([1 - 11, 2 - 21, 3 - 31]).

:- pred test_data(test_data::out) is det.

test_data([1 - 11, 2 - 21, 3 - 31, 4 - 40, 5 - 50]).

:- pred merge_test_data_no_overlap(test_data::out) is det.

merge_test_data_no_overlap([6 - 60, 7 - 70, 8 - 80]).

:- pred merge_test_data_key_overlap(test_data::out) is det.

merge_test_data_key_overlap([5 - 150, 6 - 160, 7 - 170]).

:- pred merge_test_data_value_overlap_1(test_data::out) is det.

merge_test_data_value_overlap_1([6 - 60, 7 - 10, 8 - 80]).

:- pred merge_test_data_value_overlap_2(test_data::out) is det.

merge_test_data_value_overlap_2([6 - 60, 7 - 21, 8 - 80]).

:- pred test(pred(string)::in(pred(out) is semidet), string::in,
    io::di, io::uo) is cc_multi.

test(Pred, Name, !IO) :-
    io.write_strings(["Test """, Name, """ "], !IO),
    try(Pred, Result),
    (
        Result = succeeded(Msg),
        io.write_strings(["did not pass: ", Msg, ".\n"], !IO)
    ;
        Result = failed,
        io.write_string("passed.\n", !IO)
    ;
        Result = exception(Univ),
        io.write_string("threw exception: ", !IO),
        io.write(univ_value(Univ), !IO),
        io.write_string("\n", !IO)
    ).

:- pred validate_injection(test_inj::in) is semidet.

validate_injection(J) :-
    ( if
        not validate_condition_1(J)
    then
        throw("first invariant violated")
    else if
        not validate_condition_2(J)
    then
        throw("second invariant violated")
    else
        semidet_succeed
    ).

:- pred validate_condition_1(test_inj::in) is semidet.

validate_condition_1(J) :-
    injection.keys(J, Ks),
    all [K, V] (
        (
            list.member(K, Ks),
            injection.lookup(J, K, V)
        ) =>
        injection.reverse_search(J, K, V)
    ).

:- pred validate_condition_2(test_inj::in) is semidet.

validate_condition_2(J) :-
    injection.values(J, Vs),
    all [K, V] (
        (
            list.member(V, Vs),
            injection.reverse_lookup(J, K, V)
        ) =>
        injection.forward_search(J, K, _)
    ).

:- pred semidet_succeed(T::in) is semidet.

semidet_succeed(_) :-
    semidet_succeed.

:- pred semidet_fail(T::in) is semidet.

semidet_fail(_) :-
    semidet_fail.

%---------------------------------------------------------------------------%

:- pred is_empty_test(test_inj::in, string::out) is semidet.

is_empty_test(J, "reported not empty") :-
    not is_empty(J).

:- pred not_is_empty_test(test_inj::in, string::out) is semidet.

not_is_empty_test(J, "reported empty") :-
    is_empty(J).

:- pred set_same_test(test_inj::in, test_data::in, string::out) is semidet.

set_same_test(J, Data, "set failed") :-
    not (
        injection.set_from_assoc_list(Data, J, NewJ),
        validate_injection(NewJ)
    ).

:- pred update_new(test_inj::in, test_data::in, string::out) is semidet.

update_new(J, Data, "update failed") :-
    some [K, V] (
        list.member(K - V, Data),
        not (
            injection.update(J, K, V, NewJ),
            validate_injection(NewJ)
        )
    ).

:- pred forward_search_succ_1(test_inj::in, test_data::in, string::out)
    is semidet.

forward_search_succ_1(J, Data, "key not found") :-
    some [K] (
        list.member(K - _, Data),
        not injection.forward_search(J, K, _)
    ).

:- pred forward_search_succ_2(test_inj::in, test_data::in, string::out)
    is semidet.

forward_search_succ_2(J, Data, "wrong value") :-
    some [K, V] (
        list.member(K - V, Data),
        not injection.forward_search(J, K, V)
    ).

:- pred forward_search_fail(test_inj::in, int::in, string::out) is semidet.

forward_search_fail(J, K, "wrongly succeeded") :-
    injection.forward_search(J, K, _).

:- pred reverse_search_succ_1(test_inj::in, test_data::in, string::out)
    is semidet.

reverse_search_succ_1(J, Data, "value not found") :-
    some [V] (
        list.member(_ - V, Data),
        not injection.reverse_search(J, _, V)
    ).

:- pred reverse_search_succ_2(test_inj::in, test_data::in, string::out)
    is semidet.

reverse_search_succ_2(J, Data, "wrong key") :-
    some [K, V] (
        list.member(K - V, Data),
        not injection.reverse_search(J, K, V)
    ).

:- pred reverse_search_fail(test_inj::in, int::in, string::out) is semidet.

reverse_search_fail(J, V, "wrongly succeeded") :-
    injection.reverse_search(J, _, V).

:- pred lookup_throw(test_inj::in, int::in, string::out) is semidet.

lookup_throw(J, K, "wrongly succeeded") :-
    injection.lookup(J, K, V),
    semidet_succeed(V).

:- pred reverse_lookup_throw(test_inj::in, int::in, string::out) is semidet.

reverse_lookup_throw(J, V, "wrongly succeeded") :-
    injection.reverse_lookup(J, K, V),
    semidet_succeed(K).

:- pred keys_test(test_inj::in, test_data::in, string::out) is semidet.

keys_test(J, Data, "keys did not match") :-
    injection.keys(J, Ks),
    not (all [K] (
        list.member(K - _, Data) <=> list.member(K, Ks)
    )).

:- pred values_test(test_inj::in, test_data::in, string::out) is semidet.

values_test(J, Data, "values did not match") :-
    injection.values(J, Vs),
    not (
        all [V] (
            list.member(_ - V, Data) <=> list.member(V, Vs)
        )
    ).

:- pred insert_fail(test_inj::in, int::in, int::in, string::out) is semidet.

insert_fail(J, K, V, "succeeded with duplicate") :-
    injection.insert(J, K, V, _).

:- pred insert_throw(test_inj::in, int::in, int::in, string::out) is semidet.

insert_throw(J, K, V, "succeeded with duplicate") :-
    injection.det_insert(J, K, V, NewJ),
    validate_injection(NewJ).

:- pred update_fail(test_inj::in, int::in, int::in, string::out) is semidet.

update_fail(J, K, V, "wrongly succeeded") :-
    injection.update(J, K, V, _).

:- pred update_throw(test_inj::in, int::in, int::in, string::out) is semidet.

update_throw(J, K, V, "wrongly succeeded") :-
    injection.det_update(J, K, V, NewJ),
    validate_injection(NewJ).

:- pred set_succeed(test_inj::in, int::in, int::in, string::out) is semidet.

set_succeed(J, K, V, "failed with valid value") :-
    not (
        injection.set(J, K, V, NewJ),
        validate_injection(NewJ)
    ).

:- pred set_fail(test_inj::in, int::in, int::in, string::out) is semidet.

set_fail(J, K, V, "succeeded with duplicate value") :-
    injection.set(J, K, V, _).

:- pred set_throw(test_inj::in, int::in, int::in, string::out) is semidet.

set_throw(J, K, V, "succeeded with duplicate value") :-
    injection.det_set(J, K, V, NewJ),
    validate_injection(NewJ).

:- pred delete_key_test(test_inj::in, int::in, string::out) is semidet.

delete_key_test(J, K, "dangling reference in reverse map") :-
    injection.delete_key(K, J, NewJ),
    validate_injection(NewJ),
    injection.values(NewJ, Vs),
    some [V] (
        list.member(V, Vs),
        injection.reverse_lookup(NewJ, K, V)
    ).

:- pred delete_value_succ(test_inj::in, int::in, string::out) is semidet.

delete_value_succ(J, V, "if you see this the test driver is broken") :-
    injection.delete_value(V, J, NewJ),
    validate_injection(NewJ),
    semidet_fail(NewJ).

:- pred delete_value_throw(test_inj::in, int::in, string::out) is semidet.

delete_value_throw(J, V, "dangling reference in forward map") :-
    injection.delete_value(V, J, NewJ),
    validate_injection(NewJ).

:- pred merge_succ(test_inj::in, test_inj::in, string::out) is semidet.

merge_succ(J, M, "if you see this the test driver is broken") :-
    injection.merge(J, M, NewJ),
    validate_injection(NewJ),
    semidet_fail(NewJ).

:- pred merge_throw(test_inj::in, test_inj::in, string::out) is semidet.

merge_throw(J, M, "duplicates accepted") :-
    injection.merge(J, M, NewJ),
    validate_injection(NewJ).

:- pred overlay_succ(test_inj::in, test_inj::in, string::out) is semidet.

overlay_succ(J, M, "if you see this the test driver is broken") :-
    injection.overlay(J, M, NewJ),
    validate_injection(NewJ),
    semidet_fail(NewJ).

:- pred overlay_throw(test_inj::in, test_inj::in, string::out) is semidet.

overlay_throw(J, M, "duplicates accepted") :-
    injection.overlay(J, M, NewJ),
    validate_injection(NewJ).

:- pred map_keys_test(test_inj::in, (func(int, int) = int)::in, string::out)
    is semidet.

map_keys_test(J, F, "bad transformation") :-
    NewJ = injection.map_keys(F, J),
    validate_injection(NewJ),
    injection.values(J, Vs),
    some [K, V] (
        list.member(V, Vs),
        injection.reverse_lookup(J, K, V),
        not injection.reverse_lookup(NewJ, F(V, K), V)
    ).

:- pred map_values_test(test_inj::in, (func(int, int) = int)::in, string::out)
    is semidet.

map_values_test(J, F, "bad transformation") :-
    NewJ = injection.map_values(F, J),
    validate_injection(NewJ),
    injection.keys(J, Ks),
    injection.values(J, Vs),
    some [K, V] (
        list.member(K, Ks),
        injection.lookup(J, K, V),
        not injection.lookup(NewJ, K, F(K, V))
    ;
        list.member(V, Vs),
        injection.reverse_lookup(J, K, V),
        not injection.reverse_lookup(NewJ, K, F(K, V))
    ).
