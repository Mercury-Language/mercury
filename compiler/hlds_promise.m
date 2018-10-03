%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
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

:- implementation.

:- import_module counter.
:- import_module map.
:- import_module multi_map.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- interface.

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

:- implementation.

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
%---------------------------------------------------------------------------%

:- interface.

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

    % An exclusive_id is the pred_id of an exclusivity declaration,
    % and is useful in distinguishing between the arguments of the
    % operations below on the exclusive_table.
    %
:- type exclusive_id  == pred_id.
:- type exclusive_ids == list(pred_id).

:- type exclusive_table.

    % Initialise the exclusive_table.
    %
:- pred exclusive_table_init(exclusive_table::out) is det.

    % Search the exclusive table and return the list of exclusivity
    % declarations that use the predicate given by pred_id.
    %
:- pred exclusive_table_search(exclusive_table::in, pred_id::in,
    exclusive_ids::out) is semidet.

    % As for search, but aborts if no exclusivity declarations are found.
    %
:- pred exclusive_table_lookup(exclusive_table::in, pred_id::in,
    exclusive_ids::out) is det.

    % Optimises the exclusive_table.
    %
:- pred exclusive_table_optimize(exclusive_table::in, exclusive_table::out)
    is det.

    % Add to the exclusive table that pred_id is used in the exclusivity
    % declaration exclusive_id.
    %
:- pred exclusive_table_add(pred_id::in, exclusive_id::in,
    exclusive_table::in, exclusive_table::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- type exclusive_table == multi_map(pred_id, exclusive_id).

exclusive_table_init(ExclusiveTable) :-
    multi_map.init(ExclusiveTable).

exclusive_table_lookup(ExclusiveTable, PredId, ExclusiveIds) :-
    multi_map.lookup(ExclusiveTable, PredId, ExclusiveIds).

exclusive_table_search(ExclusiveTable, Id, ExclusiveIds) :-
    multi_map.search(ExclusiveTable, Id, ExclusiveIds).

exclusive_table_optimize(!ExclusiveTable) :-
    multi_map.optimize(!ExclusiveTable).

exclusive_table_add(ExclusiveId, PredId, !ExclusiveTable) :-
    multi_map.set(PredId, ExclusiveId, !ExclusiveTable).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_promise.
%---------------------------------------------------------------------------%
