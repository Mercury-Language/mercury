%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018-2019, 2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_promise.m.
%
% This module defines the part of the HLDS that deals with promises:
% properties of predicates that the users promises are true.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_promise.
:- interface.

:- import_module hlds.hlds_pred.

:- import_module list.

%---------------------------------------------------------------------------%

    % A table that records all the assertions in the system.
    % An assertion is a goal that will always evaluate to true,
    % subject to the constraints imposed by the quantifiers.
    % For example, the assertion
    %
    %   :- promise all [A] some [B] (B > A)
    %
    % states that for all possible values of A, there will exist
    % at least one value for B such that B is greater than A.
    %
:- type assert_id.
:- type assertion_table.

:- pred assertion_table_init(assertion_table::out) is det.

:- pred assertion_table_add_assertion(pred_id::in, assert_id::out,
    assertion_table::in, assertion_table::out) is det.

:- pred assertion_table_lookup(assertion_table::in, assert_id::in,
    pred_id::out) is det.

:- pred assertion_table_pred_ids(assertion_table::in,
    list(pred_id)::out) is det.

%---------------------------------------------------------------------------%

    % A table recording exclusivity declarations (i.e. promise_exclusive
    % and promise_exclusive_exhaustive).
    %
    % e.g. :- all [X]
    %       promise_exclusive
    %       some [Y]
    %       (
    %           p(X, Y)
    %       ;
    %           q(X)
    %       ).
    %
    % promises that only one of p(X, Y) and q(X) can succeed at a time,
    % although whichever one succeeds may have multiple solutions.
    % See notes/promise_ex.html for details of the declarations.
    %
    % Note: the exclusive_table is not yet used.

    % An exclusive_id is the pred_id of an exclusivity declaration,
    % and is useful in distinguishing between the arguments of the
    % operations below on the exclusive_table.
    %
:- type exclusive_id.
:- type exclusive_table.

    % Initialise the exclusive_table.
    %
:- pred exclusive_table_init(exclusive_table::out) is det.

    % exclusive_table_add_exclusive(PromisePredId, ExclusivePredIds, !Table):
    %
    % The promise whose pred_id is PromisePredId lists ExclusivePredIds
    % as being exclusive. Add this fact to the table.
    %
:- pred exclusive_table_add_exclusive(pred_id::in, list(pred_id)::in,
    exclusive_table::in, exclusive_table::out) is det.

    % Search the exclusive table and return the list of exclusivity
    % declarations that use the predicate given by pred_id.
    %
:- pred exclusive_table_search(exclusive_table::in, pred_id::in,
    list(exclusive_id)::out) is semidet.

    % As for search, but aborts if no exclusivity declarations are found.
    %
:- pred exclusive_table_lookup(exclusive_table::in, pred_id::in,
    list(exclusive_id)::out) is det.

    % Optimises the exclusive_table.
    %
:- pred exclusive_table_optimize(exclusive_table::in, exclusive_table::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module map.
:- import_module multi_map.

%---------------------------------------------------------------------------%

:- type assert_id
    --->    assert_id(int).

:- type assertion_table
    --->    assertion_table(
                % A counter to be used in allocating the next assert_id.
                counter,

                % Maps the id of an assertion to the pred_id of the predicate
                % that represents the assertion.
                map(assert_id, pred_id)
            ).

assertion_table_init(AssertionTable) :-
    counter.init(0, Counter),
    map.init(AssertionMap),
    AssertionTable = assertion_table(Counter, AssertionMap).

assertion_table_add_assertion(AssertionPredId, AssertId, !AssertionTable) :-
    !.AssertionTable = assertion_table(Counter0, AssertionMap0),
    counter.allocate(AssertIdInt, Counter0, Counter),
    AssertId = assert_id(AssertIdInt),
    map.det_insert(AssertId, AssertionPredId, AssertionMap0, AssertionMap),
    !:AssertionTable = assertion_table(Counter, AssertionMap).

assertion_table_lookup(AssertionTable, AssertId, AssertionPredId) :-
    AssertionTable = assertion_table(_Counter, AssertionMap),
    map.lookup(AssertionMap, AssertId, AssertionPredId).

assertion_table_pred_ids(assertion_table(_, AssertionMap), PredIds) :-
    map.values(AssertionMap, PredIds).

%---------------------------------------------------------------------------%

:- type exclusive_id
    --->    exclusive_id(int).

:- type exclusive_table
    --->    exclusive_table(
                % A counter to be used in allocating the next exclusive_id.
                counter,

                % Maps the id of an assertion to the pred_id of the predicate
                % that represents the assertion.
                multi_map(pred_id, exclusive_id)
            ).

exclusive_table_init(ExclusiveTable) :-
    counter.init(0, Counter),
    multi_map.init(ExclusiveMap),
    ExclusiveTable = exclusive_table(Counter, ExclusiveMap).

exclusive_table_add_exclusive(_PromisePredId, ExclusivePredIds,
        !ExclusiveTable) :-
    !.ExclusiveTable = exclusive_table(Counter0, ExclusiveMap0),
    counter.allocate(ExclusiveIdInt, Counter0, Counter),
    ExclusiveId = exclusive_id(ExclusiveIdInt),
    RecordExclusiveId =
        ( pred(PId::in, Map0::in, Map::out) is det :-
            multi_map.add(PId, ExclusiveId, Map0, Map)
        ),
    list.foldl(RecordExclusiveId, ExclusivePredIds,
        ExclusiveMap0, ExclusiveMap),
    !:ExclusiveTable = exclusive_table(Counter, ExclusiveMap).

exclusive_table_search(ExclusiveTable, PredId, ExclusiveIds) :-
    ExclusiveTable = exclusive_table(_Counter, ExclusiveMap),
    multi_map.search(ExclusiveMap, PredId, ExclusiveIds).

exclusive_table_lookup(ExclusiveTable, PredId, ExclusiveIds) :-
    ExclusiveTable = exclusive_table(_Counter, ExclusiveMap),
    multi_map.lookup(ExclusiveMap, PredId, ExclusiveIds).

exclusive_table_optimize(!ExclusiveTable) :-
    !.ExclusiveTable = exclusive_table(Counter, ExclusiveMap0),
    multi_map.optimize(ExclusiveMap0, ExclusiveMap),
    !:ExclusiveTable = exclusive_table(Counter, ExclusiveMap).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_promise.
%---------------------------------------------------------------------------%
