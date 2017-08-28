%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2007, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: table_builtin.m.
% Main authors: zs, fjh, ohutch.
% Stability: low.
%
% This file is automatically imported, as if via `use_module', into every
% module that contains a tabling pragma (`pragma loopcheck', `pragma memo',
% or `pragma minimal_model'). It is intended for the builtin procedures
% that the compiler generates implicit calls to when implementing tabling.
% This is separated from private_builtin.m, partly for modularity, but
% mostly to improve compilation speed for programs that don't use tabling.
%
% The *_shortcut predicates are dummies. They do not ever get called directly;
% their purpose is to serve as hooks on which to hang foreign_procs generated
% directly by the compiler.
%
% This module is a private part of the Mercury implementation; user modules
% should never explicitly import this module. The interface for this module
% does not get included in the Mercury library reference manual.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module table_builtin.
:- interface.

% This section of the module contains the predicates that are
% automatically inserted by the table_gen pass of the compiler
% into predicates that use tabling, and the types they use.
%
% The predicates fall into the following categories:
%
% - Predicates that manage loop checking.
%
% - Predicates that manage memoization in general.
%
% - Predicates that manage memoization of I/O procedures.
%
% - Predicates that manage minimal model tabling using stack copying.
%
% - Predicates that manage minimal model tabling using own stacks.
%
% - Utility predicates that are needed in the tabling of all predicates.
%
% The utility predicates that handle tries are combined lookup/insert
% operations; if the item being searched for is not already in the trie,
% they insert it. These predicates are used to implement both call tables,
% in which case the items inserted are input arguments of a tabled predicate,
% and answer tables, in which case the items inserted are output arguments
% of a tabled predicate.
%
% The call table trie is used for detecting duplicate calls,
% while the answer table trie is used for detecting duplicate answers.
% However, storing answers only in the answer table trie is not sufficient,
% for two reasons. First, while the trie encodes the values of the output
% arguments, this encoding is not in the form of the native Mercury
% representations of those arguments. Second, for model_non subgoals we
% want a chronological list of answers, to allow us to separate out
% answers we have returned already from answers we have not yet returned.
% To handle the first problem, we save each answer not only in the
% answer table trie but also in an answer block, which is a vector of N
% elements, where N is the number of output arguments of the procedure
% concerned. To handle the second problem, for model_non procedures
% we chain these answer blocks together in a chronological list.
%
% For loopcheck goals, the word at the end of the call table trie
% is used as a status indication (of type loop_status).
%
% For memo goals, the word at the end of the call table trie is used
% both as a status indication (of type memo_status) and as a pointer to
% an answer block (if the goal succeeded). This is OK, because we can
% distinguish the two, and because an answer block pointer can be
% associated with only one status value (memo_succeeded).
%
% For minimal model goals with stack copying, the word at the end of the call
% table trie always points to a subgoal structure, with several fields. The
% status of the subgoal and the list of answers are two of these fields. Other
% fields, described in runtime/mercury_minimal_model.h, are used in the
% implementation of the minimal model.
%
% For minimal model goals with own stacks, the word at the end of the call
% table trie always points to a consumer structure, with several fields. The
% status of the consumer and the list of answers are two of these fields. Other
% fields, described in runtime/mercury_mm_own_stack.h, are used in the
% implementation of the minimal model.
%
% All of the predicates here with the impure declaration modify the tabling
% structures. Because the structures are persistent through backtracking,
% this causes the predicates to become impure. The predicates with the semipure
% directive only examine the tabling structures, but do not modify them.
%
% At the moment, tabling is supported only by the LLDS and MLDS C backends, so
% in the next three type definitions, only the C definition is useful.  The
% Mercury definitions are placeholders only, required to make this module
% compile cleanly on the non-C backends respectively.

    % This type represents the interior pointers of both call
    % tables and answer tables.
    %
:- type ml_trie_node.

    % This type represents the blocks we use to store sets of output arguments.
    %
:- type ml_answer_block.

    % This type represents the data structure the implementation uses to store
    % information related to a given procedure.
    %
:- type ml_proc_table_info.

    % N.B. interface continued below

%---------------------------------------------------------------------------%

:- implementation.

    % This type represents the interior pointers of both call tables
    % and answer tables.
    %
:- type ml_trie_node
    --->   ml_trie_node(c_pointer).
:- pragma foreign_type("C", ml_trie_node, "MR_TrieNode",
    [can_pass_as_mercury_type]).

    % This type represents a block of memory that contains one word
    % for each output argument of a procedure.
    %
:- type ml_answer_block --->    ml_answer_block(c_pointer).
:- pragma foreign_type("C", ml_answer_block, "MR_AnswerBlock",
    [can_pass_as_mercury_type]).

:- type ml_proc_table_info ---> ml_proc_table_info(c_pointer).
:- pragma foreign_type("C", ml_proc_table_info, "MR_ProcTableInfoPtr",
    [can_pass_as_mercury_type]).

%---------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage loop checking.
%

    % This type should correspond exactly to the MR_LOOPCHECK_* #defines
    % in runtime/mercury_tabling.h.
    %
:- type loop_status
    --->    loop_inactive
    ;       loop_active.

:- impure pred table_loop_setup(ml_trie_node::in, loop_status::out) is det.

:- impure pred table_loop_setup_shortcut(ml_trie_node::in, ml_trie_node::out,
    loop_status::out) is det.

    % Mark the call represented by the given table as currently
    % not being evaluated.
    %
:- impure pred table_loop_mark_as_inactive(ml_trie_node::in) is det.

    % Mark the call represented by the given table as currently
    % not being evaluated, and fail.
    %
:- impure pred table_loop_mark_as_inactive_and_fail(ml_trie_node::in)
    is failure.

    % Mark the call represented by the given table as currently
    % being evaluated, and fail.
    %
:- impure pred table_loop_mark_as_active_and_fail(ml_trie_node::in) is failure.

    % N.B. interface continued below

%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_proc("C",
    table_loop_setup(T::in, Status::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_loop_setup(MR_TABLE_DEBUG_BOOL, MR_FALSE, T, Status);
").

:- pragma foreign_proc("C",
    table_loop_setup_shortcut(T0::in, T::out, Status::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_loop_setup_shortcut(T0, T, Status);
").

:- pragma foreign_proc("C",
    table_loop_mark_as_inactive(T::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_loop_mark_as_inactive(MR_TABLE_DEBUG_BOOL, T);
").

:- pragma foreign_proc("C",
    table_loop_mark_as_inactive_and_fail(T::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_loop_mark_as_inactive_and_fail(MR_TABLE_DEBUG_BOOL, T);
").

:- pragma foreign_proc("C",
    table_loop_mark_as_active_and_fail(T::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_loop_mark_as_active_and_fail(MR_TABLE_DEBUG_BOOL, T);
").

table_loop_setup(_, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_loop_setup").

table_loop_setup_shortcut(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_loop_setup_shortcut").

table_loop_mark_as_inactive(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_loop_mark_as_inactive").

table_loop_mark_as_inactive_and_fail(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_loop_mark_as_inactive"),
    fail.

table_loop_mark_as_active_and_fail(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_loop_mark_as_active"),
    fail.

%---------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage memoization.
%

    % This type should correspond exactly to the MR_MEMO_DET_* #defines
    % in runtime/mercury_tabling.h.
    %
:- type memo_det_status
    --->    memo_det_inactive
    ;       memo_det_active
    ;       memo_det_succeeded.

    % This type should correspond exactly to the MR_MEMO_SEMI_* #defines
    % in runtime/mercury_tabling.h.
    %
:- type memo_semi_status
    --->    memo_semi_inactive
    ;       memo_semi_active
    ;       memo_semi_succeeded
    ;       memo_semi_failed.

    % This type should correspond exactly to the MR_MemoNonStatus type
    % in runtime/mercury_tabling.h.
    %
:- type memo_non_status
    --->    memo_non_inactive
    ;       memo_non_active
    ;       memo_non_incomplete
    ;       memo_non_complete.

:- type memo_non_record.

:- impure pred table_memo_det_setup(ml_trie_node::in, memo_det_status::out)
    is det.

:- impure pred table_memo_det_setup_shortcut(ml_trie_node::in,
    ml_trie_node::out, memo_det_status::out) is det.

:- impure pred table_memo_semi_setup(ml_trie_node::in, memo_semi_status::out)
    is det.

:- impure pred table_memo_semi_setup_shortcut(ml_trie_node::in,
    ml_trie_node::out, memo_semi_status::out) is det.

:- impure pred table_memo_non_setup(ml_trie_node::in,
    memo_non_record::out, memo_non_status::out) is det.

    % Save the fact that the call has failed in the given table.
    %
:- impure pred table_memo_mark_as_failed(ml_trie_node::in) is failure.

    % Save the fact that the call has succeeded in the given table.
    %
:- impure pred table_memo_mark_as_succeeded(ml_trie_node::in) is det.

    % Set the status of the given call to incomplete.
    %
:- impure pred table_memo_mark_as_incomplete(memo_non_record::in) is det.

    % Set the status of the given call to active, and fail.
    %
:- impure pred table_memo_mark_as_active_and_fail(memo_non_record::in)
    is failure.

    % Set the status of the given call to complete, and fail.
    %
:- impure pred table_memo_mark_as_complete_and_fail(memo_non_record::in)
    is failure.

    % Create an answer block with the given number of slots and add it
    % to the given table.
    %
:- impure pred table_memo_create_answer_block(ml_trie_node::in, int::in,
    ml_answer_block::out) is det.
:- impure pred table_memo_fill_answer_block_shortcut(ml_trie_node::in) is det.

    % Return the answer block for the given call.
    %
:- semipure pred table_memo_get_answer_block(ml_trie_node::in,
    ml_answer_block::out) is det.
:- semipure pred table_memo_get_answer_block_shortcut(ml_trie_node::in) is det.

    % Return the table of answers already returned to the given nondet
    % table.
    %
:- semipure pred table_memo_non_get_answer_table(memo_non_record::in,
    ml_trie_node::out) is det.

    % If the answer represented by the given answer table
    % has not been generated before by this subgoal,
    % succeed and remember the answer as having been generated.
    % If the answer has been generated before, fail.
    %
:- impure pred table_memo_non_answer_is_not_duplicate(ml_trie_node::in)
    is semidet.
:- impure pred table_memo_non_answer_is_not_duplicate_shortcut(
    memo_non_record::in) is semidet.

    % Add an answer block to the given table.
    %
:- impure pred table_memo_non_create_answer_block_shortcut(memo_non_record::in)
    is det.

    % Return all the answer blocks for the given model_non call.
    %
:- semipure pred table_memo_return_all_answers_nondet(memo_non_record::in,
    ml_answer_block::out) is nondet.
:- semipure pred table_memo_return_all_answers_multi(memo_non_record::in,
    ml_answer_block::out) is multi.
:- semipure pred table_memo_non_return_all_shortcut(memo_non_record::in)
    is det.

    % N.B. interface continued below

%---------------------------------------------------------------------------%

:- implementation.

:- type memo_non_record --->        memo_non_record(c_pointer).
:- pragma foreign_type("C", memo_non_record, "MR_MemoNonRecordPtr",
    [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    table_memo_det_setup(T::in, Status::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_det_setup(MR_TABLE_DEBUG_BOOL, MR_FALSE, T, Status);
").

:- pragma foreign_proc("C",
    table_memo_det_setup_shortcut(T0::in, T::out, Status::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_det_setup_shortcut(T0, T, Status);
").

:- pragma foreign_proc("C",
    table_memo_semi_setup(T::in, Status::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_semi_setup(MR_TABLE_DEBUG_BOOL, MR_FALSE, T, Status);
").

:- pragma foreign_proc("C",
    table_memo_semi_setup_shortcut(T0::in, T::out, Status::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_semi_setup_shortcut(T0, T, Status);
").

:- pragma foreign_proc("C",
    table_memo_non_setup(T0::in, Record::out, Status::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_non_setup(MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, Record, Status);
").

:- pragma foreign_proc("C",
    table_memo_mark_as_failed(T::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_mark_as_failed(MR_TABLE_DEBUG_BOOL, T);
").

:- pragma foreign_proc("C",
    table_memo_mark_as_succeeded(T::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_mark_as_succeeded(MR_TABLE_DEBUG_BOOL, T);
").

:- pragma foreign_proc("C",
    table_memo_mark_as_incomplete(R::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_mark_as_incomplete(MR_TABLE_DEBUG_BOOL, R);
").

:- pragma foreign_proc("C",
    table_memo_mark_as_active_and_fail(R::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_mark_as_active_and_fail(MR_TABLE_DEBUG_BOOL, R);
").

:- pragma foreign_proc("C",
    table_memo_mark_as_complete_and_fail(R::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_mark_as_complete_and_fail(MR_TABLE_DEBUG_BOOL, R);
").

:- pragma foreign_proc("C",
    table_memo_create_answer_block(T::in, Size::in, AnswerBlock::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_create_answer_block(MR_TABLE_DEBUG_BOOL,
        T, Size, AnswerBlock);
").

:- pragma foreign_proc("C",
    table_memo_fill_answer_block_shortcut(T::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_fill_answer_block_shortcut(T);
").

:- pragma foreign_proc("C",
    table_memo_get_answer_block(T::in, AnswerBlock::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_memo_get_answer_block(MR_TABLE_DEBUG_BOOL, T, AnswerBlock);
").

:- pragma foreign_proc("C",
    table_memo_get_answer_block_shortcut(T::in),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_memo_get_answer_block_shortcut(T);
").

:- pragma foreign_proc("C",
    table_memo_non_get_answer_table(R::in, AT::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_memo_non_get_answer_table(MR_TABLE_DEBUG_BOOL, R, AT);
").

:- pragma foreign_proc("C",
    table_memo_non_answer_is_not_duplicate(T::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_non_answer_is_not_duplicate(MR_TABLE_DEBUG_BOOL,
        T, SUCCESS_INDICATOR);
").

:- pragma foreign_proc("C",
    table_memo_non_answer_is_not_duplicate_shortcut(R::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_non_answer_is_not_duplicate_shortcut(R,
        SUCCESS_INDICATOR);
").

:- pragma foreign_proc("C",
    table_memo_non_create_answer_block_shortcut(R::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_memo_non_create_answer_block_shortcut(R::in);
").

:- pragma foreign_proc("C",
    table_memo_non_return_all_shortcut(R::in),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_memo_non_return_all_shortcut(R);
").

:- pragma external_pred(table_memo_return_all_answers_nondet/2).
:- pragma external_pred(table_memo_return_all_answers_multi/2).

table_memo_det_setup(_, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_det_setup").

table_memo_det_setup_shortcut(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_det_setup_shortcut").

table_memo_semi_setup(_, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_semi_setup").

table_memo_semi_setup_shortcut(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_semi_setup_shortcut").

table_memo_mark_as_failed(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_mark_as_failed"),
    fail.

table_memo_mark_as_succeeded(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_mark_as_succeeded").

table_memo_mark_as_incomplete(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_mark_as_incomplete").

table_memo_mark_as_active_and_fail(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_mark_as_active_and_fail").

table_memo_mark_as_complete_and_fail(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_mark_as_complete_and_fail").

table_memo_create_answer_block(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_create_answer_block").

table_memo_fill_answer_block_shortcut(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_fill_answer_block_shortcut").

table_memo_get_answer_block(_, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_memo_get_answer_block").

table_memo_non_return_all_shortcut(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_memo_non_return_all_shortcut").

table_memo_get_answer_block_shortcut(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_memo_get_answer_block_shortcut").

table_memo_non_get_answer_table(_, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_memo_non_get_answer_table").

table_memo_non_answer_is_not_duplicate(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_non_answer_is_not_duplicate").

table_memo_non_answer_is_not_duplicate_shortcut(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry(
        "table_memo_non_answer_is_not_duplicate_shortcut").

table_memo_non_create_answer_block_shortcut(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_memo_non_create_answer_block_shortcut").

%---------------------------------------------------------------------------%

:- interface.

:- import_module io.

    % This procedure should be called exactly once for each I/O action.
    % If I/O tabling is enabled, this predicate will increment the I/O
    % action counter, and will check if this action should be tabled.
    % If not, it fails. If yes, it succeeds, and binds the output
    % arguments, which are, in order:
    %
    % - The root trie node for all I/O actions. This is similar to
    %   the per-procedure tabling pointers, but it is shared by all
    %   I/O actions.
    % - the I/O action number of this action.
    % - The I/O action number of the first action in the tabled range.
    %
    % After the first tabled action, the root trie node will point to a
    % (dynamically expandable) array of trie nodes. The trie node for
    % I/O action number Counter is at offset Counter - Start in this array,
    % where Start is the I/O action number of the first tabled action.
    % The three output parameters together specify this location.
    %
:- impure pred table_io_in_range(ml_trie_node::out, int::out, int::out)
    is semidet.

    % This procedure should be called exactly once for each I/O action
    % for which table_io_in_range returns true. Given the trie node
    % for a given I/O action number, it returns true iff that action has
    % been carried out before (i.e. the action is now being reexecuted
    % after a retry command in the debugger).
    %
:- impure pred table_io_has_occurred(ml_trie_node::in) is semidet.

    % This predicate simply copies the input I/O state to become the output
    % I/O state. It is used only because it is easier to get the insts
    % right by calling this procedure than by hand-writing insts for a
    % unification.
    %
:- pred table_io_copy_io_state(io.state::di, io.state::uo) is det.

    % Calls to these predicates bracket the code of foreign_procs with
    % the tabled_for_io_unitize annotation. The left bracket procedure
    % returns the current value of MR_debug_enabled, and then turns off
    % both MR_debug_enabled and MR_io_tabling_enabled. (We don't need to
    % save MR_io_tabling_enabled because we only get to this code if it
    % contains true.) The right bracket code takes the value returned by
    % the left bracket as input and restores both globals to the values
    % they had before the call to the left bracket.
    %
:- impure pred table_io_left_bracket_unitized_goal(int::out) is det.
:- impure pred table_io_right_bracket_unitized_goal(int::in) is det.

    % N.B. interface continued below

%---------------------------------------------------------------------------%

:- implementation.

% For purposes of I/O tabling, we divide the program's execution into four
% phases.
%
% Phase UNINIT consists of Mercury code executed prior to the first debugger
% event. Even if main/2 is traced, this will include the initialization of the
% I/O system itself. During this phase, MR_io_tabling_enabled will be MR_FALSE.
%
% Phase BEFORE consists of Mercury code during whose execution the user does
% not need safe retry across I/O, probably because he/she does not require
% retry at all. During this phase, MR_io_tabling_enabled will be MR_TRUE while
% we ensure that table_io_range returns MR_FALSE by setting MR_io_tabling_start
% to the highest possible value.
%
% Phase DURING consists of Mercury code during whose execution the user does
% need safe retry across I/O. During this phase, MR_io_tabling_enabled will be
% MR_TRUE, and MR_io_tabling_start will be set to the value of
% MR_io_tabling_counter on entry to phase DURING. We will ensure that
% table_io_in_range returns MR_TRUE by setting MR_io_tabling_end to the highest
% possible value.
%
% Phase AFTER again consists of Mercury code during whose execution the user
% does not need safe retry across I/O. During this phase, MR_io_tabling_enabled
% will be MR_TRUE, MR_io_tabling_start will contain the value of
% MR_io_tabling_counter at the time of the entry to phase DURING, while
% MR_io_tabling_end will contain the value of MR_io_tabling_counter at the end
% of phase DURING, thus ensuring that table_io_in_range again returns MR_FALSE.
%
% The transition from phase UNINIT to phase BEFORE will occur during the
% initialization of the debugger, at the first trace event.
%
% The transition from phase BEFORE to phase DURING will occur when the user
% issues the "table_io start" command, while the transition from phase DURING
% to phase AFTER will occur when the user issues the "table_io end" command.
% The user may automate entry into phase DURING by putting "table_io start"
% into a .mdbrc file. Of course the program will never enter phase DURING or
% phase AFTER if the user never gives the commands that start those phases.
%
% The debugger itself invokes Mercury code e.g. to print the values of
% variables. During such calls it will set MR_io_tabling_enabled to MR_FALSE,
% since the I/O actions executed during such times do not belong to the user
% program.

:- pragma foreign_decl("C", "
    #include ""mercury_tabling.h""      /* for MR_copy_table_steps */
    #include ""mercury_trace_base.h""   /* for MR_io_tabling_* */
").

:- pragma foreign_proc("C",
    table_io_in_range(T::out, Counter::out, Start::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_io_in_range(MR_TABLE_DEBUG_BOOL, T, Counter, Start,
        SUCCESS_INDICATOR);
").

:- pragma foreign_proc("C",
    table_io_has_occurred(T::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_io_has_occurred(MR_TABLE_DEBUG_BOOL, T, SUCCESS_INDICATOR);
").

table_io_copy_io_state(IO, IO).

:- pragma foreign_proc("C",
    table_io_left_bracket_unitized_goal(TraceEnabled::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_io_left_bracket_unitized_goal(TraceEnabled);
").

:- pragma foreign_proc("C",
    table_io_right_bracket_unitized_goal(TraceEnabled::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_io_right_bracket_unitized_goal(TraceEnabled);
").

table_io_in_range(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_io_in_range").

table_io_has_occurred(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_io_has_occurred").

table_io_left_bracket_unitized_goal(_TraceEnabled) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_io_left_bracket_unitized_goal").

table_io_right_bracket_unitized_goal(_TraceEnabled) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_io_right_bracket_unitized_goal").

%---------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage the stack copy model of minimal model tabling.
%

    % This type should correspond exactly to the type MR_SubgoalStatus
    % defined in runtime/mercury_tabling.h.
    %
:- type mm_status
    --->    mm_inactive
    ;       mm_active
    ;       mm_complete.

    % This type represents the data structure at the tips of the call table
    % in the stack copy implementation of minimal model tabling.
    %
:- type ml_subgoal.

    % Check if this minimal_model subgoal has been called before.
    % If not, return inactive as the old status, set up the subgoal's
    % structure, and mark it as active, since the caller is about to become
    % the subgoal's generator. If yes, return the status currently recorded
    % in the subgoal structure.
    %
:- impure pred table_mm_setup(ml_trie_node::in, ml_subgoal::out,
    mm_status::out) is det.

    % Save the state of the current subgoal and fail. Sometime later,
    % when the subgoal has some solutions, table_mm_completion will
    % restore the saved state. At the time, table_mm_suspend_consumer
    % will succeed, and return an answer block as its second argument.
    %
:- impure pred table_mm_suspend_consumer(ml_subgoal::in, ml_answer_block::out)
    is nondet.

    % Resume all suspended subgoal calls. This predicate will resume each
    % of the suspended subgoals that depend on it in turn until it reaches
    % a fixed point, at which all depended suspended subgoals have had
    % all available answers returned to them.
    %
:- impure pred table_mm_completion(ml_subgoal::in) is det.

    % Return the table of answers already returned to the given nondet
    % table.
    %
:- semipure pred table_mm_get_answer_table(ml_subgoal::in, ml_trie_node::out)
    is det.

    % If the answer represented by the given answer table
    % has not been generated before by this subgoal,
    % succeed and remember the answer as having been generated.
    % If the answer has been generated before, fail.
    %
:- impure pred table_mm_answer_is_not_duplicate(ml_trie_node::in) is semidet.
:- impure pred table_mm_answer_is_not_duplicate_shortcut(ml_subgoal::in)
    is semidet.

    % Create a new slot in the answer list of the subgoal, create a new
    % answer block of the given size, and put the answer block in the new
    % slot.
    %
:- impure pred table_mm_create_answer_block(ml_subgoal::in, int::in,
    ml_answer_block::out) is det.

:- impure pred table_mm_fill_answer_block_shortcut(ml_subgoal::in) is det.

    % Return all of the answer blocks stored in the given table.
    %
:- semipure pred table_mm_return_all_nondet(ml_subgoal::in,
    ml_answer_block::out) is nondet.
:- semipure pred table_mm_return_all_multi(ml_subgoal::in,
    ml_answer_block::out) is multi.
:- semipure pred table_mm_return_all_shortcut(ml_answer_block::in) is det.

    % N.B. interface continued below

%---------------------------------------------------------------------------%

:- implementation.

:- type ml_subgoal --->     ml_subgoal(c_pointer).
:- pragma foreign_type("C", ml_subgoal, "MR_SubgoalPtr",
    [can_pass_as_mercury_type]).

    % This type represents a list of answers of a model_non predicate.
    %
:- type ml_answer_list ---> ml_answer_list(c_pointer).
:- pragma foreign_type("C", ml_answer_list, "MR_AnswerList",
    [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    table_mm_setup(T::in, Subgoal::out, Status::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_mm_setup(MR_TABLE_DEBUG_BOOL, MR_FALSE, T, Subgoal, Status);
").

    % The definitions of these two predicates are in the runtime system,
    % in runtime/mercury_minimal_model.c.
    %
:- pragma external_pred(table_mm_suspend_consumer/2).
:- pragma external_pred(table_mm_completion/1).
:- pragma external_pred(table_mm_return_all_nondet/2).
:- pragma external_pred(table_mm_return_all_multi/2).

:- pragma foreign_proc("C",
    table_mm_return_all_shortcut(AnswerBlock::in),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_mm_return_all_shortcut(AnswerBlock);
").

:- pragma foreign_proc("C",
    table_mm_get_answer_table(Subgoal::in, AnswerTable::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_mm_get_answer_table(MR_TABLE_DEBUG_BOOL, Subgoal, AnswerTable);
").

:- pragma foreign_proc("C",
    table_mm_answer_is_not_duplicate(TrieNode::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_mm_answer_is_not_duplicate(MR_TABLE_DEBUG_BOOL, TrieNode,
        SUCCESS_INDICATOR);
").

:- pragma foreign_proc("C",
    table_mm_answer_is_not_duplicate_shortcut(Subgoal::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    /*
    ** The body of this predicate doesn't matter, because it will never be
    ** referred to. When the compiler creates references to this predicate,
    ** it always overrides the predicate body.
    */
    /* mention Subgoal to shut up the warning */
    MR_fatal_error(""table_mm_answer_is_not_duplicate_shortcut: direct call"");
").

:- pragma foreign_proc("C",
    table_mm_create_answer_block(Subgoal::in, Size::in, AnswerBlock::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_mm_create_answer_block(MR_TABLE_DEBUG_BOOL,
        Subgoal, Size, AnswerBlock);
").

:- pragma foreign_proc("C",
    table_mm_fill_answer_block_shortcut(Subgoal::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_mm_fill_answer_block_shortcut(Subgoal);
").

table_mm_return_all_shortcut(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_mm_return_all_shortcut").

table_mm_get_answer_table(_, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_mm_get_answer_table").

table_mm_answer_is_not_duplicate(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_mm_answer_is_not_duplicate").

table_mm_answer_is_not_duplicate_shortcut(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_mm_answer_is_not_duplicate_shortcut").

table_mm_create_answer_block(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_mm_create_answer_block").

table_mm_fill_answer_block_shortcut(_) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_mm_fill_answer_block_shortcut").

%---------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage the own stack model of minimal model tabling.
%

    % This type represents the data structure at the tips of the call table
    % in the own stack implementation of minimal model tabling.
    %
:- type ml_consumer.

    % This type represents the generators in the own stack implementation
    % of minimal model tabling.
    %
:- type ml_generator.

    % Save the information that will be needed later about this subgoal
    % in a temporary data structure in the runtime.
    %
:- impure pred table_mmos_save_inputs is det.

    % The given trie node should be at the tip of a call table,
    % indicating a subgoal. Create a generator for this subgoal if
    % necessary (if the trie node indicates it has not been called before),
    % and then register this call as a consumer of the answers of the
    % generator.
    %
:- impure pred table_mmos_setup_consumer(ml_trie_node::in, c_pointer::in,
    ml_consumer::out) is det.

    % If the answer represented by the given answer table
    % has not been generated before by this subgoal,
    % succeed and remember the answer as having been generated.
    % If the answer has been generated before, fail.
    %
:- impure pred table_mmos_answer_is_not_duplicate(ml_trie_node::in) is semidet.

    % This is a version of table_mmos_answer_is_not_duplicate
    % in intended for bundling with other operations.
    %
:- impure pred table_mmos_answer_is_not_duplicate_shortcut(ml_generator::in)
    is semidet.

    % This predicate checks whether there is a next answer already listed
    % for the consumer. If yes, it returns it. If not, it wakes up the
    % generator to create more answers if possible.
    %
:- impure pred table_mmos_consume_next_answer_nondet(ml_consumer::in,
    ml_answer_block::out) is nondet.
:- impure pred table_mmos_consume_next_answer_multi(ml_consumer::in,
    ml_answer_block::out) is multi.

    % Return all the answers of a consumers, in extra arguments hung
    % on a foreign_proc invocation of this predicate.
    %
:- semipure pred table_mmos_restore_answers(ml_answer_block::in) is det.

    % Pickup the input arguments of the generator from where the consumer
    % put them.
    %
:- impure pred table_mmos_pickup_inputs(ml_generator::out) is det.

    % Create an answer block of the given size for a new answer of the
    % given generator.
    %
:- impure pred table_mmos_create_answer_block(ml_generator::in,
    int::in, ml_answer_block::out) is det.

    % Return the given answer to the consumer(s) that requested it.
    %
:- impure pred table_mmos_return_answer(ml_generator::in, ml_answer_block::in)
    is det.

    % Resume all suspended subgoal calls. This predicate will resume each
    % of the suspended subgoals that depend on it in turn until it reaches
    % a fixed point, at which all depended suspended subgoals have had
    % all available answers returned to them.
    %
:- impure pred table_mmos_completion(ml_generator::in) is failure.

:- implementation.

    % This type represents the data structure at the tips of the call table
    % in the own stack implementation of minimal model tabling.
    %
:- type ml_consumer --->    ml_consumer(c_pointer).
:- pragma foreign_type("C", ml_consumer, "MR_ConsumerPtr",
    [can_pass_as_mercury_type]).

    % This type represents the generators in the own stack implementation
    % of minimal model tabling.
    %
:- type ml_generator --->   ml_generator(c_pointer).
:- pragma foreign_type("C", ml_generator, "MR_GeneratorPtr",
    [can_pass_as_mercury_type]).

:- pragma foreign_proc("C",
    table_mmos_save_inputs,
    [will_not_call_mercury, does_not_affect_liveness],
"
    /*
    ** The body of this predicate doesn't matter, because it will never be
    ** referred to. When the compiler creates references to this predicate,
    ** it always overrides the predicate body.
    */
    MR_fatal_error(""table_mmos_save_inputs: direct call"");
").

:- pragma foreign_proc("C",
    table_mmos_setup_consumer(T::in, GeneratorPred::in, Consumer::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    /*
    ** The body of this predicate doesn't matter, because it will never be
    ** referred to. When the compiler creates references to this predicate,
    ** it always overrides the predicate body.
    */
    /* mention T, GeneratorPred, Consumer to shut up the warning */
    MR_fatal_error(""table_mmos_setup_consumer: direct call"");
").

:- pragma foreign_proc("C",
    table_mmos_answer_is_not_duplicate(T::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    /*
    ** The body of this predicate doesn't matter, because it will never be
    ** referred to. When the compiler creates references to this predicate,
    ** it always overrides the predicate body.
    */
    /* mention T to shut up the warning */
    MR_fatal_error(""table_mmos_answer_is_not_duplicate: direct call"");
").

:- pragma foreign_proc("C",
    table_mmos_answer_is_not_duplicate_shortcut(G::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    /*
    ** The body of this predicate doesn't matter, because it will never be
    ** referred to. When the compiler creates references to this predicate,
    ** it always overrides the predicate body.
    */
    /* mention G to shut up the warning */
    MR_fatal_error(""table_mmos_answer_is_not_duplicate_shortcut: direct call"");
").

:- pragma external_pred(table_mmos_consume_next_answer_nondet/2).
:- pragma external_pred(table_mmos_consume_next_answer_multi/2).

:- pragma foreign_proc("C",
    table_mmos_restore_answers(AnswerBlock::in),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    /*
    ** The body of this predicate doesn't matter, because it will never be
    ** referred to. When the compiler creates references to this predicate,
    ** it always overrides the predicate body.
    */
    /* mention AnswerBlock to shut up the warning */
    MR_fatal_error(""table_mmos_restore_answers: direct call"");
").

:- pragma foreign_proc("C",
    table_mmos_pickup_inputs(Generator::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    /*
    ** The body of this predicate doesn't matter, because it will never be
    ** referred to. When the compiler creates references to this predicate,
    ** it always overrides the predicate body.
    */
    /* mention Generator to shut up the warning */
    MR_fatal_error(""table_mmos_pickup_inputs: direct call"");
").

:- pragma foreign_proc("C",
    table_mmos_create_answer_block(Generator::in, BlockSize::in,
        AnswerBlock::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    /*
    MR_tbl_mmos_create_answer_block(Generator, BlockSize, AnswerBlock);
    */
").

:- pragma foreign_proc("C",
    table_mmos_return_answer(Generator::in, AnswerBlock::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    /*
    MR_tbl_mmos_return_answer(Generator, AnswerBlock);
    */
").

:- pragma foreign_proc("C",
    table_mmos_completion(Generator::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    /*
    MR_tbl_mmos_completion(Generator);
    */
").

table_mmos_save_inputs :-
    impure private_builtin.imp.

table_mmos_setup_consumer(_, _, Consumer) :-
    impure private_builtin.imp,
    % Required only to avoid warnings; never executed.
    pretend_to_generate_value(Consumer).

table_mmos_create_answer_block(_, _, AnswerBlock) :-
    impure private_builtin.imp,
    % Required only to avoid warnings; never executed.
    pretend_to_generate_value(AnswerBlock).

table_mmos_return_answer(_, _) :-
    impure private_builtin.imp.

table_mmos_completion(_) :-
    impure private_builtin.imp,
    fail.

    % Required only to avoid warnings; never executed.
    %
:- pred pretend_to_generate_value(T::out) is det.

pretend_to_generate_value(Bogus) :-
    % The following code will throw an exception if executed.
    det_univ_to_type(univ(0), Bogus).

%---------------------------------------------------------------------------%

:- interface.

%
% Utility predicates that are needed in the tabling of all kinds of subgoals.
%

%
% The following table_lookup_insert... predicates lookup or insert the second
% argument into the trie pointed to by the first argument. The value returned
% is a pointer to the leaf of the trie reached by the lookup. From the
% returned leaf another trie may be connected.
%

    % Lookup or insert an integer in the given table.
    %
:- impure pred table_lookup_insert_int(ml_trie_node::in, int::in,
    ml_trie_node::out) is det.

    % Lookup or insert an unsigned integer in the given table.
    %
:- impure pred table_lookup_insert_uint(ml_trie_node::in, uint::in,
    ml_trie_node::out) is det.

    % Lookup or insert a signed 8-bit integer in the given table.
    %
:- impure pred table_lookup_insert_int8(ml_trie_node::in, int8::in,
    ml_trie_node::out) is det.

    % Lookup or insert an unsigned 8-bit integer in the given table.
    %
:- impure pred table_lookup_insert_uint8(ml_trie_node::in, uint8::in,
    ml_trie_node::out) is det.

    % Lookup or insert a signed 16-bit integer in the given table.
    %
:- impure pred table_lookup_insert_int16(ml_trie_node::in, int16::in,
    ml_trie_node::out) is det.

    % Lookup or insert an unsigned 16-bit integer in the given table.
    %
:- impure pred table_lookup_insert_uint16(ml_trie_node::in, uint16::in,
    ml_trie_node::out) is det.

    % Lookup or insert a signed 32-bit integer in the given table.
    %
:- impure pred table_lookup_insert_int32(ml_trie_node::in, int32::in,
    ml_trie_node::out) is det.

    % Lookup or insert an unsigned 32-bit integer in the given table.
    %
:- impure pred table_lookup_insert_uint32(ml_trie_node::in, uint32::in,
    ml_trie_node::out) is det.

    % Lookup or insert an integer in the given table.
    %
:- impure pred table_lookup_insert_start_int(ml_trie_node::in, int::in,
    int::in, ml_trie_node::out) is det.

    % Lookup or insert a character in the given trie.
    %
:- impure pred table_lookup_insert_char(ml_trie_node::in, character::in,
    ml_trie_node::out) is det.

    % Lookup or insert a string in the given trie.
    %
:- impure pred table_lookup_insert_string(ml_trie_node::in, string::in,
    ml_trie_node::out) is det.

    % Lookup or insert a float in the current trie.
    %
:- impure pred table_lookup_insert_float(ml_trie_node::in, float::in,
    ml_trie_node::out) is det.

    % Lookup or insert an enumeration type in the given trie.
    %
:- impure pred table_lookup_insert_enum(ml_trie_node::in, int::in, T::in,
    ml_trie_node::out) is det.

    % Lookup or insert a foreign enumeration type in the given trie.
    %
:- impure pred table_lookup_insert_foreign_enum(ml_trie_node::in, T::in,
    ml_trie_node::out) is det.

    % Lookup or insert a monomorphic general type in the given trie.
    %
:- impure pred table_lookup_insert_gen(ml_trie_node::in, T::in,
    ml_trie_node::out) is det.

    % Lookup or insert a monomorphic general type in the given trie,
    % tabling terms without traversing them. This makes the operation fast,
    % but if a term was inserted previously, we will catch it only if the
    % insert was the exact same memory cells. (This is the "loose" part.)
    %
:- impure pred table_lookup_insert_gen_addr(ml_trie_node::in, T::in,
    ml_trie_node::out) is det.

    % Lookup or insert a polymorphic general type in the given trie.
    %
:- impure pred table_lookup_insert_gen_poly(ml_trie_node::in, T::in,
    ml_trie_node::out) is det.

    % Lookup or insert a polymorphic general type in the given trie,
    % tabling terms without traversing them. This makes the operation fast,
    % but if a term was inserted previously, we will catch it only if the
    % insert was the exact same memory cells. (This is the "loose" part.)
    %
:- impure pred table_lookup_insert_gen_poly_addr(ml_trie_node::in, T::in,
    ml_trie_node::out) is det.

    % Lookup or insert a type_info in the given trie.
    %
:- impure pred table_lookup_insert_typeinfo(ml_trie_node::in,
    private_builtin.type_info::in, ml_trie_node::out) is det.

    % Lookup or insert a typeclass_info in the given trie.
    %
:- impure pred table_lookup_insert_typeclassinfo(ml_trie_node::in,
    private_builtin.typeclass_info::in, ml_trie_node::out) is det.

    % Save an integer answer in the given answer block at the given
    % offset.
    %
:- impure pred table_save_int_answer(ml_answer_block::in, int::in, int::in)
    is det.

    % Save an unsigned integer answer in the given answer block at the given
    % offset.
    %
:- impure pred table_save_uint_answer(ml_answer_block::in, int::in, uint::in)
    is det.

    % Save a signed 8-bit integer answer in the given answer block at the
    % given offset.
    %
:- impure pred table_save_int8_answer(ml_answer_block::in, int::in, int8::in)
    is det.

    % Save an unsigned 8-bit integer answer in the given answer block at the
    % given offset.
    %
:- impure pred table_save_uint8_answer(ml_answer_block::in, int::in, uint8::in)
    is det.

    % Save a signed 16-bit integer answer in the given answer block at the
    % given offset.
    %
:- impure pred table_save_int16_answer(ml_answer_block::in, int::in, int16::in)
    is det.

    % Save an unsigned 16-bit integer answer in the given answer block at the
    % given offset.
    %
:- impure pred table_save_uint16_answer(ml_answer_block::in, int::in, uint16::in)
    is det.

    % Save a signed 32-bit integer answer in the given answer block at the
    % given offset.
    %
:- impure pred table_save_int32_answer(ml_answer_block::in, int::in, int32::in)
    is det.

    % Save an unsigned 32-bit integer answer in the given answer block at the
    % given offset.
    %
:- impure pred table_save_uint32_answer(ml_answer_block::in, int::in, uint32::in)
    is det.

    % Save a character answer in the given answer block at the given
    % offset.
    %
:- impure pred table_save_char_answer(ml_answer_block::in, int::in,
    character::in) is det.

    % Save a string answer in the given answer block at the given
    % offset.
    %
:- impure pred table_save_string_answer(ml_answer_block::in, int::in,
    string::in) is det.

    % Save a float answer in the given answer block at the given
    % offset.
    %
:- impure pred table_save_float_answer(ml_answer_block::in, int::in, float::in)
    is det.

    % Save an I/O state in the given answer block at the given offset.
    %
:- impure pred table_save_io_state_answer(ml_answer_block::in, int::in,
    io.state::ui) is det.

    % Save any type of answer in the given answer block at the given
    % offset.
    %
:- impure pred table_save_any_answer(ml_answer_block::in, int::in, T::in)
    is det.

    % Restore an integer answer from the given answer block at the
    % given offset.
    %
:- semipure pred table_restore_int_answer(ml_answer_block::in, int::in,
    int::out) is det.

    % Restore an unsigned integer answer from the given answer block at
    % the given offset.
    %
:- semipure pred table_restore_uint_answer(ml_answer_block::in, int::in,
    uint::out) is det.

    % Restore a signed 8-bit integer answer from the given answer block at the
    % given offset.
    %
:- semipure pred table_restore_int8_answer(ml_answer_block::in, int::in,
    int8::out) is det.

    % Restore an unsigned 8-bit integer answer from the given answer block at
    % the given offset.
    %
:- semipure pred table_restore_uint8_answer(ml_answer_block::in, int::in,
    uint8::out) is det.

    % Restore a signed 16-bit integer answer from the given answer block at the
    % given offset.
    %
:- semipure pred table_restore_int16_answer(ml_answer_block::in, int::in,
    int16::out) is det.

    % Restore an unsigned 16-bit integer answer from the given answer block at
    % the given offset.
    %
:- semipure pred table_restore_uint16_answer(ml_answer_block::in, int::in,
    uint16::out) is det.

    % Restore a signed 32-bit integer answer from the given answer block at the
    % given offset.
    %
:- semipure pred table_restore_int32_answer(ml_answer_block::in, int::in,
    int32::out) is det.

    % Restore an unsigned 32-bit integer answer from the given answer block at
    % the given offset.
    %
:- semipure pred table_restore_uint32_answer(ml_answer_block::in, int::in,
    uint32::out) is det.

    % Restore a character answer from the given answer block at the
    % given offset.
    %
:- semipure pred table_restore_char_answer(ml_answer_block::in, int::in,
    character::out) is det.

    % Restore a string answer from the given answer block at the
    % given offset.
    %
:- semipure pred table_restore_string_answer(ml_answer_block::in, int::in,
    string::out) is det.

    % Restore a float answer from the given answer block at the
    % given offset.
    %
:- semipure pred table_restore_float_answer(ml_answer_block::in, int::in,
    float::out) is det.

    % Restore an I/O state from the given answer block at the given offset.
    %
:- semipure pred table_restore_io_state_answer(ml_answer_block::in, int::in,
    io.state::uo) is det.

    % Restore any type of answer from the given answer block at the
    % given offset.
    %
:- semipure pred table_restore_any_answer(ml_answer_block::in, int::in, T::out)
    is det.

    % Report an error message.
    %
:- pred table_error(string::in) is erroneous.

    % Report statistics on the operation of the tabling system to stderr.
    %
:- impure pred table_report_statistics is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module univ.

:- pragma foreign_decl("C", "

#include ""mercury_misc.h""         /* for MR_fatal_error(); */
#include ""mercury_type_info.h""    /* for MR_TypeCtorInfo_Struct; */
#include ""mercury_tabling.h""      /* for MR_TrieNode, etc. */

MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_NAME(io, state, 0));

").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    table_lookup_insert_int(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_int(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_uint(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_uint(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_start_int(T0::in, S::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_start_int(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE,
        T0, S, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_int8(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_int8(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_uint8(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_uint8(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_int16(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_int16(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_uint16(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_uint16(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_int32(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_int32(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_uint32(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_uint32(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_char(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_char(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_string(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_string(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_float(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_float(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_enum(T0::in, R::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_enum(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0,
        R, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_foreign_enum(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_foreign_enum(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0,
        V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_gen(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_gen(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0,
        TypeInfo_for_T, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_gen_addr(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_gen_addr(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0,
        TypeInfo_for_T, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_gen_poly(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_gen_poly(NULL, MR_TABLE_DEBUG_BOOL, MR_FALSE, T0,
        TypeInfo_for_T, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_gen_poly_addr(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_gen_poly_addr(NULL, MR_TABLE_DEBUG_BOOL,
        MR_FALSE, T0, TypeInfo_for_T, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_typeinfo(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_typeinfo(NULL, MR_TABLE_DEBUG_BOOL,
        MR_FALSE, T0, V, T);
").

:- pragma foreign_proc("C",
    table_lookup_insert_typeclassinfo(T0::in, V::in, T::out),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_lookup_insert_typeclassinfo(NULL, MR_TABLE_DEBUG_BOOL,
        MR_FALSE, T0, V, T);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    table_save_int_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_int_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_uint_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_uint_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_int8_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_int8_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_uint8_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_uint8_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_int16_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_int16_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_uint16_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_uint16_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_int32_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_int32_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_uint32_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_uint32_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_char_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_char_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_string_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_string_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_float_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_float_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_io_state_answer(AB::in, Offset::in, V::ui),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_io_state_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_save_any_answer(AB::in, Offset::in, V::in),
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_tbl_save_any_answer(MR_TABLE_DEBUG_BOOL, AB, Offset,
        TypeInfo_for_T, V);
").

:- pragma foreign_proc("C",
    table_restore_int_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_int_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_uint_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_uint_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_int8_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_int8_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_uint8_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_uint8_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_int16_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_int16_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_uint16_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_uint16_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_int32_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_int32_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_uint32_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_uint32_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_char_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_char_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_string_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_string_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_float_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_float_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_io_state_answer(AB::in, Offset::in, V::uo),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_io_state_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

:- pragma foreign_proc("C",
    table_restore_any_answer(AB::in, Offset::in, V::out),
    [will_not_call_mercury, promise_semipure, does_not_affect_liveness],
"
    MR_tbl_restore_any_answer(MR_TABLE_DEBUG_BOOL, AB, Offset, V);
").

table_error(Message) :-
    error(Message).

:- pragma foreign_proc("C",
    table_report_statistics,
    [will_not_call_mercury, does_not_affect_liveness],
"
    MR_table_report_statistics(stderr);
").

table_lookup_insert_int(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_int").

table_lookup_insert_uint(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_uint").

table_lookup_insert_start_int(_, _, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_start_int").

table_lookup_insert_char(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_char").

table_lookup_insert_string(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_string").

table_lookup_insert_float(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_float").

table_lookup_insert_enum(_, _, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_enum").

table_lookup_insert_gen(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_gen").

table_lookup_insert_gen_poly(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_gen_poly").

table_lookup_insert_gen_addr(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_gen_addr").

table_lookup_insert_gen_poly_addr(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_lookup_insert_gen_poly_addr").

table_save_int_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_save_int_answer").

table_save_uint_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_save_uint_answer").

table_save_char_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_save_char_answer").

table_save_string_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_save_string_answer").

table_save_float_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_save_float_answer").

table_save_io_state_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_save_io_state_answer").

table_save_any_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_save_any_answer").

table_restore_int_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_restore_int_answer").

table_restore_char_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_restore_char_answer").

table_restore_string_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_restore_string_answer").

table_restore_float_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_restore_float_answer").

table_restore_io_state_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_restore_io_state_answer").

table_restore_any_answer(_, _, _) :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    semipure private_builtin.semip,
    private_builtin.sorry("table_restore_any_answer").

table_report_statistics :-
    % This version is only used for back-ends for which there is no
    % matching foreign_proc version.
    impure private_builtin.imp,
    private_builtin.sorry("table_report_statistics").

%---------------------------------------------------------------------------%

:- pragma foreign_code("Erlang", "

    % These stubs are needed so that the Erlang compiler doesn't complain
    % about missing definitions for exported functions.

    table_memo_return_all_answers_multi_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(""tabling in Erlang backend"").
    table_memo_return_all_answers_nondet_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(""tabling in Erlang backend"").
    table_mm_completion_1_p_0(_) ->
        mercury__private_builtin:sorry_1_p_0(""tabling in Erlang backend"").
    table_mm_return_all_multi_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(""tabling in Erlang backend"").
    table_mm_return_all_nondet_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(""tabling in Erlang backend"").
    table_mm_suspend_consumer_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(""tabling in Erlang backend"").
    table_mmos_consume_next_answer_multi_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(""tabling in Erlang backend"").
    table_mmos_consume_next_answer_nondet_2_p_0(_, _) ->
        mercury__private_builtin:sorry_1_p_0(""tabling in Erlang backend"").
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
