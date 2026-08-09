%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_data_rare.m.
%
% This module defines types that are
%
% - are part of the parse tree of a module;
% - are needed even after the HLDS has been built from the parse tree;
% - but are so needed only by very few modules.
%
% It is the last part that is the reason for taking these types
% out of the prog_data.m module.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_data_rare.
:- interface.

%---------------------------------------------------------------------------%
%
% Predicates.
%

    % The kinds of auxiliary predicates we may need to generate
    % to implement a mutable.
    %
    % The first group represent the public predicates, the predicates
    % that user programs may call. The usual (non-constant) kind of mutable
    % will have the standard get and set predicates, and if attached
    % to the I/O state, will have the I/O get and set predicates as well.
    % Constant mutables will have the constant get and set predicates instead
    % (see below).
    %
    % The second group represent the private predicates, the predicates
    % that user programs should not call (and which are not documented).
    % The unsafe get and set predicates may be needed to implement the other,
    % user-visible get and set predicates, and the lock and unlock predicates
    % have the same role. The initialization predicate is called by the
    % implementation itself at program startup, and it may need the help
    % of the preinit predicate.
    %
    % Note that we need a set predicate even for constant mutables.
    % The reason is that the init predicate needs to do two things:
    % execute arbitrary Mercury code (call functions etc) to generate
    % the initial (and for constant mutables, also final) value of the mutable,
    % and then store this value in persistent storage. However, even if
    % we could create an item that contains both Mercury code and backend
    % (e.g. C) code, which is currently not possible, this would require
    % the second part to be a foreign_proc goal. Such goals include a reference
    % to the predicate they implement. That predicate would be equivalent
    % to the set predicate.
    %
    % In these circumstances, avoiding the need for a set predicate
    % would require significant changes to the structures of items.
    % It is much simpler to use a predicate and give it a name that
    % makes it clear people that they shouldn't use it.
    %
:- type mutable_pred_kind
    --->    mutable_pred_std_get
    ;       mutable_pred_std_set
    ;       mutable_pred_io_get
    ;       mutable_pred_io_set
    ;       mutable_pred_constant_get
    ;       mutable_pred_constant_secret_set

    ;       mutable_pred_unsafe_get
    ;       mutable_pred_unsafe_set
    ;       mutable_pred_lock
    ;       mutable_pred_unlock
    ;       mutable_pred_pre_init
    ;       mutable_pred_init.

:- type tabling_aux_pred_kind
    --->    tabling_aux_pred_stats
    ;       tabling_aux_pred_reset.

:- type solver_type_pred_kind
    --->    solver_type_to_ground_pred
    ;       solver_type_to_any_pred
    ;       solver_type_from_ground_pred
    ;       solver_type_from_any_pred.

%---------------------------------------------------------------------------%
%
% Trailing and minimal model tabling analysis.
%

:- type trailing_status
    --->    trail_may_modify
    ;       trail_will_not_modify
    ;       trail_conditional.

:- type mm_tabling_status
    --->    mm_tabled_may_call
    ;       mm_tabled_will_not_call
    ;       mm_tabled_conditional.

%---------------------------------------------------------------------------%
%
% Parts of items that are needed beyond the construction of the HLDS.
%

    % What kind of promise does a promise item contain?
    %
:- type promise_type
    --->    promise_type_exclusive
            % A promise that given two disjuncts, at most one is true.

    ;       promise_type_exhaustive
            % A promise that given two disjuncts, at least one is true.

    ;       promise_type_exclusive_exhaustive
            % A promise that given two disjuncts, exactly one is true.

    ;       promise_type_true.
            % A promise that the given goal is true.

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_data_rare.
%---------------------------------------------------------------------------%
