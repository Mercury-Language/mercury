%---------------------------------------------------------------------------%
% Copyright (C) 1998-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: table_builtin.m.
% Main authors: fjh, ohutch, zs.
% Stability: low.

% This file is automatically imported, as if via `use_module', into every
% module that contains a tabling pragma (`pragma memo', `pragma loopcheck',
% or `pragma minimal_model').  It is intended for the builtin procedures
% that the compiler generates implicit calls to when implementing tabling.
% This is separated from private_builtin.m, partly for modularity, but
% mostly to improve compilation speed for programs that don't use tabling.

% This module is a private part of the Mercury implementation;
% user modules should never explicitly import this module.
% The interface for this module does not get included in the
% Mercury library reference manual.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module table_builtin.

%-----------------------------------------------------------------------------%

:- interface.

% This section of the module contains the predicates that are
% automatically inserted by the table_gen pass of the compiler
% into predicates that use tabling, and the types they use.
%
% The predicates fall into three categories:
%
% (1)	Predicates that manage the tabling of simple subgoals.
%	A subgoal is simple if its predicate is model_det or model_semi,
%	which means that its evaluation method must be something
%	other than minimal model.
%
% (2)	Predicates that manage the tabling of model_non subgoals,
%	which usually means that its evaluation method is minimal model.
%
% (3)	Utility predicates that are needed in the tabling of both
%	simple and nondet subgoals.
%
% The utility predicates that handle tries are combined lookup/insert
% operations; if the item being searched for is not already in the trie,
% they insert it. These predicates are used to implement both subgoal tables,
% in which case the items inserted are input arguments of a tabled predicate,
% and answer tables, in which case the items inserted are output arguments
% of a tabled predicate.
%
% The subgoal table trie is used for detecting duplicate calls,
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
% For simple goals, the word at the end of the subgoal table trie is used
% first as a status indication (of type MR_SimpletableStatus), and later on
% as a pointer to an answer block (if the goal succeeded). This is OK, because
% we can distinguish the two, and because an answer block pointer can be
% associated with only one status value.
%
% For nondet goals, the word at the end of the subgoal table trie always
% points to a subgoal structure, with several fields. The status of the
% subgoal and the list of answers are two of these fields. Other fields,
% described in runtime/mercury_tabling.h, are used in the implementation
% of the minimal model.
%
% All of the predicates here with the impure declaration modify the tabling
% structures. Because the structures are persistent through backtracking,
% this causes the predicates to become impure. The predicates with the semipure
% directive only examine the tabling structures, but do not modify them.

	% This type is used as a generic table: it can in fact represent two
	% types, either a subgoal_table or an answer_table. The subgoal_table
	% and answer_table types are differentiated by what they have at the
	% table nodes but not by the actual underlying trie structure.
:- type ml_table.

	% This type is used in contexts where a node of a subgoal table is
	% expected.
:- type ml_subgoal_table_node.

	% This type is used in contexts where a node of an answer table is
	% expected.
:- type ml_answer_table_node.

	% This type is used in contexts where an answer slot is expected.
:- type ml_answer_slot.

	% This type is used in contexts where an answer block is expected.
:- type ml_answer_block.

	% These equivalences should be local to private_builtin. However,
	% at the moment table_gen.m assumes that it can use a single variable
	% sometimes as an ml_table and other times as an ml_subgoal_table_node
	% (e.g. by giving the output of table_lookup_insert_int as input to
	% table_have_all_ans). The proper fix would be for table_gen.m to
	% use additional variables and insert unsafe casts. However, this
	% would require significant work for no real gain, so for now
	% we fix the problem by exposing the equivalences to code generated
	% by table_gen.m.
:- type ml_subgoal_table_node == ml_table.
:- type ml_answer_table_node == ml_table.
:- type ml_answer_slot == ml_table.
:- type ml_answer_block == ml_table.
:- type ml_table == c_pointer.

	% N.B. interface continued below

:- implementation.

% This equivalence should be private. However, polymorphism gets an
% internal error when compiling tests/tabling/boyer.m if it is.
% :- type ml_table == c_pointer.

%-----------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage the tabling of simple subgoals.
%

	% Return true if the subgoal represented by the given table has an
	% answer.
:- semipure pred table_simple_is_complete(ml_subgoal_table_node::in)
	is semidet.

	% Return true if the subgoal represented by the given table has a
	% true answer.
:- semipure pred table_simple_has_succeeded(ml_subgoal_table_node::in)
	is semidet.

	% Return true if the subgoal represented by the given table has
	% failed.
:- semipure pred table_simple_has_failed(ml_subgoal_table_node::in) is semidet.

	% Return true if the subgoal represented by the given table is
	% currently being evaluated (working on an answer).
:- semipure pred table_simple_is_active(ml_subgoal_table_node::in) is semidet.

	% Return false if the subgoal represented by the given table is
	% currently being evaluated (working on an answer).
:- semipure pred table_simple_is_inactive(ml_subgoal_table_node::in)
	is semidet.

	% Save the fact the the subgoal has succeeded in the given table.
:- impure pred table_simple_mark_as_succeeded(ml_subgoal_table_node::in)
	is det.

	% Save the fact the the subgoal has failed in the given table.
:- impure pred table_simple_mark_as_failed(ml_subgoal_table_node::in) is det.

	% Mark the subgoal represented by the given table as currently
	% being evaluated (working on an answer).
:- impure pred table_simple_mark_as_active(ml_subgoal_table_node::in) is det.

	% Mark the subgoal represented by the given table as currently
	% not being evaluated (working on an answer).
:- impure pred table_simple_mark_as_inactive(ml_subgoal_table_node::in) is det.

	% N.B. interface continued below

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_proc("C",
	table_simple_is_complete(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if simple %p is complete: %ld (%lx)\\n"",
			table, (long) table->MR_simpletable_status,
			(long) table->MR_simpletable_status);
	}
#endif
	SUCCESS_INDICATOR = 
		((table->MR_simpletable_status == MR_SIMPLETABLE_FAILED)
		|| (table->MR_simpletable_status >= MR_SIMPLETABLE_SUCCEEDED));
").

:- pragma foreign_proc("C",
	table_simple_has_succeeded(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if simple %p is succeeded: %ld (%lx)\\n"",
			table, (long) table->MR_simpletable_status,
			(long) table->MR_simpletable_status);
	}
#endif
	SUCCESS_INDICATOR =
		(table->MR_simpletable_status >= MR_SIMPLETABLE_SUCCEEDED);
").

:- pragma foreign_proc("C",
	table_simple_has_failed(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if simple %p is failed: %ld (%lx)\\n"",
			table, (long) table->MR_simpletable_status,
			(long) table->MR_simpletable_status);
	}
#endif
	SUCCESS_INDICATOR =
		(table->MR_simpletable_status == MR_SIMPLETABLE_FAILED);
").

:- pragma foreign_proc("C",
	table_simple_is_active(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if simple %p is active: %ld (%lx)\\n"",
			table, (long) table->MR_simpletable_status,
			(long) table->MR_simpletable_status);
	}
#endif
	SUCCESS_INDICATOR =
		(table->MR_simpletable_status == MR_SIMPLETABLE_WORKING);
").

:- pragma foreign_proc("C",
	table_simple_is_inactive(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if simple %p is inactive: %ld (%lx)\\n"",
			table, (long) table->MR_simpletable_status,
			(long) table->MR_simpletable_status);
	}
#endif
	SUCCESS_INDICATOR =
		(table->MR_simpletable_status != MR_SIMPLETABLE_WORKING);
").

:- pragma foreign_proc("C",
	table_simple_mark_as_succeeded(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""marking %p as succeeded\\n"", table);
	}
#endif
	table->MR_simpletable_status = MR_SIMPLETABLE_SUCCEEDED;
").

:- pragma foreign_proc("C",
	table_simple_mark_as_failed(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""marking %p as failed\\n"", table);
	}
#endif
	table->MR_simpletable_status = MR_SIMPLETABLE_FAILED;
").

:- pragma foreign_proc("C",
	table_simple_mark_as_active(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""marking %p as working\\n"", table);
	}
#endif
	table->MR_simpletable_status = MR_SIMPLETABLE_WORKING;
").

:- pragma foreign_proc("C",
	table_simple_mark_as_inactive(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""marking %p as uninitialized\\n"", table);
	}
#endif
	table->MR_simpletable_status = MR_SIMPLETABLE_UNINITIALIZED;
").


:- pragma promise_semipure(table_simple_is_complete/1).
table_simple_is_complete(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_simple_is_complete").

:- pragma promise_semipure(table_simple_has_succeeded/1).
table_simple_has_succeeded(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_simple_has_succeeded").

:- pragma promise_semipure(table_simple_has_failed/1).
table_simple_has_failed(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_simple_has_failed").

:- pragma promise_semipure(table_simple_is_active/1).
table_simple_is_active(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_simple_is_active").

:- pragma promise_semipure(table_simple_is_inactive/1).
table_simple_is_inactive(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_simple_is_inactive").

table_simple_mark_as_succeeded(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_simple_mark_as_succeeded").

table_simple_mark_as_failed(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_simple_mark_as_failed").

table_simple_mark_as_active(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_simple_mark_as_active").

table_simple_mark_as_inactive(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_simple_mark_as_inactive").
%-----------------------------------------------------------------------------%

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

:- impure pred table_io_in_range(ml_table::out, int::out, int::out) is semidet.

	% This procedure should be called exactly once for each I/O action
	% for which table_io_in_range returns true. Given the trie node
	% for a given I/O action number, it returns true iff that action has
	% been carried out before (i.e. the action is now being reexecuted
	% after a retry command in the debugger).

:- impure pred table_io_has_occurred(ml_table::in) is semidet.

	% This predicate simply copies the input I/O state to become the output
	% I/O state. It is used only because it is easier to get the insts
	% right by calling this procedure than by hand-writing insts for a
	% unification.

:- pred table_io_copy_io_state(io__state::di, io__state::uo) is det.

	% Calls to these predicates bracket the code of foreign_procs with
	% the tabled_for_io_unitize annotation. The left bracket procedure
	% returns the current value of MR_trace_enabled, and then turns off
	% both MR_trace_enabled and MR_io_tabling_enabled. (We don't need to
	% save MR_io_tabling_enabled because we only get to this code if it
	% contains true.) The right bracket code takes the value returned by
	% the left bracket as input and restores both globals to the values
	% they had before the call to the left bracket.

:- impure pred table_io_left_bracket_unitized_goal(int::out) is det.
:- impure pred table_io_right_bracket_unitized_goal(int::in) is det.

	% N.B. interface continued below

%-----------------------------------------------------------------------------%

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
	#include ""mercury_trace_base.h""	/* for MR_io_tabling_* */
").

:- pragma foreign_proc("C",
	table_io_in_range(T::out, Counter::out, Start::out),
	[will_not_call_mercury],
"
	if (MR_io_tabling_enabled) {
		MR_Unsigned	old_counter;

#ifdef	MR_DEBUG_RETRY
		if (MR_io_tabling_debug) {
			printf(""checking table_io_in_range: ""
				""prev %d, start %d, hwm %d"",
				MR_io_tabling_counter, MR_io_tabling_start,
				MR_io_tabling_counter_hwm);
		}
#endif

		old_counter = MR_io_tabling_counter;

		MR_io_tabling_counter++;

		if (MR_io_tabling_start < MR_io_tabling_counter 
			&& MR_io_tabling_counter <= MR_io_tabling_end)
		{
			T = (MR_Word) &MR_io_tabling_pointer;
			Counter = (MR_Word) old_counter;
			Start = MR_io_tabling_start;
			if (MR_io_tabling_counter > MR_io_tabling_counter_hwm)
			{
				MR_io_tabling_counter_hwm =
					MR_io_tabling_counter;
			}

#ifdef	MR_DEBUG_RETRY
			if (MR_io_tabling_debug) {
				printf("" in range\n"");
			}
#endif

			SUCCESS_INDICATOR = MR_TRUE;
		} else {

#ifdef	MR_DEBUG_RETRY
			if (MR_io_tabling_debug) {
				printf("" not in range\n"");
			}
#endif
			SUCCESS_INDICATOR = MR_FALSE;
		}
	} else {
		SUCCESS_INDICATOR = MR_FALSE;
	}
").

:- pragma foreign_proc("C", table_io_has_occurred(T::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking %p for previous execution: %p\\n"",
			table, table->MR_answerblock);
	}
#endif
	SUCCESS_INDICATOR = (table->MR_answerblock != NULL);
").

:- pragma foreign_proc("C", table_io_copy_io_state(S0::di, S::uo),
	[will_not_call_mercury, promise_pure],
"
	S = S0;
").

:- pragma foreign_proc("C",
	table_io_left_bracket_unitized_goal(TraceEnabled::out),
	[will_not_call_mercury],
"
	TraceEnabled = MR_trace_enabled;
	MR_trace_enabled = MR_FALSE;
	MR_io_tabling_enabled = MR_FALSE;
").

:- pragma foreign_proc("C",
	table_io_right_bracket_unitized_goal(TraceEnabled::in),
	[will_not_call_mercury],
"
	MR_io_tabling_enabled = MR_TRUE;
	MR_trace_enabled = TraceEnabled;
").

table_io_in_range(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_io_in_range").

table_io_has_occurred(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_io_has_occurred").

table_io_copy_io_state(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("table_io_copy_io_state").

%-----------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage the tabling of model_non subgoals.
%

	% Save the information that will be needed later about this
	% nondet subgoal in a data structure. If we have already seen
	% this subgoal before, do nothing.
:- impure pred table_nondet_setup(ml_subgoal_table_node::in,
	ml_subgoal_table_node::out) is det.

	% Save the state of the current subgoal and fail. Sometime later,
	% when the subgoal has some solutions, table_nondet_resume will
	% restore the saved state. At the time, table_nondet_suspend will
	% succeed, and return an answer block as its second argument.
:- impure pred table_nondet_suspend(ml_subgoal_table_node::in,
	ml_answer_block::out) is nondet.

	% Resume all suspended subgoal calls. This predicate will resume each
	% of the suspended subgoals that depend on it in turn until it reaches
	% a fixed point, at which all depended suspended subgoals have had
	% all available answers returned to them.
:- impure pred table_nondet_resume(ml_subgoal_table_node::in) is det.

	% Succeed if we have finished generating all answers for
	% the given nondet subgoal.
:- semipure pred table_nondet_is_complete(ml_subgoal_table_node::in)
	is semidet.

	% Succeed if the given nondet subgoal is active,
	% i.e. the process of computing all its answers is not yet complete.
:- semipure pred table_nondet_is_active(ml_subgoal_table_node::in) is semidet.

	% Mark a table as being active.
:- impure pred table_nondet_mark_as_active(ml_subgoal_table_node::in) is det.

	% Return the table of answers already return to the given nondet
	% table.
:- impure pred table_nondet_get_ans_table(ml_subgoal_table_node::in,
	ml_table::out) is det.

	% If the answer represented by the given answer table
	% has not been generated before by this subgoal,
	% succeed and remember the answer as having been generated.
	% If the answer has been generated before, fail.
:- impure pred table_nondet_answer_is_not_duplicate(ml_answer_table_node::in)
	is semidet.

	% Create a new slot in the answer list.
:- impure pred table_nondet_new_ans_slot(ml_subgoal_table_node::in,
	ml_answer_slot::out) is det.

	% Return all of the answer blocks stored in the given table.
:- semipure pred table_nondet_return_all_ans(ml_subgoal_table_node::in,
	ml_answer_block::out) is nondet.
:- semipure pred table_multi_return_all_ans(ml_subgoal_table_node::in,
	ml_answer_block::out) is multi.

	% N.B. interface continued below

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_proc("C",
	table_nondet_setup(T0::in, T::out),
	[will_not_call_mercury],
"
#ifndef	MR_USE_MINIMAL_MODEL
	MR_fatal_error(""minimal model code entered when not enabled"");
#else
#ifdef	MR_THREAD_SAFE
#error ""Sorry, not yet implemented: mixing minimal model tabling and threads""
#endif
	MR_TrieNode	table;

	table = (MR_TrieNode) T0;

	/*
	** Initialize the subgoal if this is the first time we see it.
	** If the subgoal structure already exists but is marked inactive,
	** then it was left by a previous generator that couldn't
	** complete the evaluation of the subgoal due to a commit.
	** In that case, we want to forget all about the old generator.
	*/

	if (table->MR_subgoal == NULL) {
		MR_Subgoal	*subgoal;

		subgoal = MR_TABLE_NEW(MR_Subgoal);

		subgoal->status = MR_SUBGOAL_INACTIVE;
		subgoal->leader = NULL;
		subgoal->followers = MR_TABLE_NEW(MR_SubgoalListNode);
		subgoal->followers->item = subgoal;
		subgoal->followers->next = NULL;
		subgoal->followers_tail = &(subgoal->followers->next);
		subgoal->answer_table = (MR_Word) NULL;
		subgoal->num_ans = 0;
		subgoal->answer_list = NULL;
		subgoal->answer_list_tail = &subgoal->answer_list;
		subgoal->consumer_list = NULL;
		subgoal->consumer_list_tail = &subgoal->consumer_list;

#ifdef	MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf(""setting up table %p -> %p, answer slot %p\\n"",
				table, subgoal, subgoal->answer_list_tail);
		}

		if (MR_maxfr != MR_curfr) {
			MR_fatal_error(
				""MR_maxfr != MR_curfr at table setup\\n"");
		}
#endif
#ifdef MR_HIGHLEVEL_CODE
 		MR_fatal_error(""sorry, not implemented: ""
			""minimal_model tabling with --high-level-code"");
#else
		subgoal->generator_maxfr = MR_prevfr_slot(MR_maxfr);
		subgoal->generator_sp = MR_sp;
#endif
		table->MR_subgoal = subgoal;
	}
	T = T0;
#endif /* MR_USE_MINIMAL_MODEL */
").

table_nondet_setup(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_nondet_setup").

	% The definitions of these two predicates are in the runtime system,
	% in runtime/mercury_tabling.c.
:- external(table_nondet_suspend/2).
:- external(table_nondet_resume/1).

/*

XXX :- external stops us from using this

:- pragma foreign_proc("MC++",
	table_nondet_suspend(_A::in, _B::out), [will_not_call_mercury, promise_pure],
	local_vars(""),
	first_code(""),
	retry_code(""),
	common_code("
		mercury::runtime::Errors::SORRY(
			""foreign code for this function"");
	")
).

:- pragma foreign_proc("MC++",
	table_nondet_resume(_A::in), [will_not_call_mercury, promise_pure], "
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
").

*/

:- pragma foreign_proc("C",
	table_nondet_is_complete(T::in), [will_not_call_mercury], "
#ifdef	MR_USE_MINIMAL_MODEL
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

	SUCCESS_INDICATOR = (table->MR_subgoal->status == MR_SUBGOAL_COMPLETE);
#else
	MR_fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma foreign_proc("C",
	table_nondet_is_active(T::in),
	[will_not_call_mercury],
"
#ifdef	MR_USE_MINIMAL_MODEL
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

	SUCCESS_INDICATOR = (table->MR_subgoal->status == MR_SUBGOAL_ACTIVE);
#else
	MR_fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma foreign_proc("C",
	table_nondet_mark_as_active(T::in),
	[will_not_call_mercury],
"
#ifdef	MR_USE_MINIMAL_MODEL
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

	MR_push_generator(MR_curfr, table);
	MR_register_generator_ptr(table);
	table->MR_subgoal->status = MR_SUBGOAL_ACTIVE;
#else
	MR_fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma foreign_proc("C",
	table_nondet_get_ans_table(T::in, AT::out),
	[will_not_call_mercury],
"
#ifdef	MR_USE_MINIMAL_MODEL
	MR_TrieNode	table;

	table = (MR_TrieNode) T;

	AT = (MR_Word) &(table->MR_subgoal->answer_table);
#else
	MR_fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma foreign_proc("C",
	table_nondet_answer_is_not_duplicate(T::in),
	[will_not_call_mercury],
"
#ifndef	MR_USE_MINIMAL_MODEL
	MR_fatal_error(""minimal model code entered when not enabled"");
#else
	MR_TrieNode	table;
	MR_bool		is_new_answer;

	table = (MR_TrieNode) T;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""checking if %p is a duplicate answer: %ld\\n"",
			table, (long) table->MR_integer);
	}
#endif

	is_new_answer = (table->MR_integer == 0);
	table->MR_integer = 1;	/* any nonzero value will do */
	SUCCESS_INDICATOR = is_new_answer;
#endif
").

:- pragma foreign_proc("C",
	table_nondet_new_ans_slot(T::in, Slot::out),
	[will_not_call_mercury],
"
#ifndef	MR_USE_MINIMAL_MODEL
	MR_fatal_error(""minimal model code entered when not enabled"");
#else
	MR_TrieNode		table;
	MR_Subgoal		*subgoal;
	MR_AnswerListNode	*answer_node;

	table = (MR_TrieNode) T;
	subgoal = table->MR_subgoal;
	subgoal->num_ans++;

	/*
	**
	** We fill in the answer_data slot with a dummy value.
	** This slot will be filled in by the next piece of code
	** to be executed after we return, which is why we return its address.
	*/

	answer_node = MR_TABLE_NEW(MR_AnswerListNode);
	answer_node->answer_num = subgoal->num_ans;
	answer_node->answer_data.MR_integer = 0;
	answer_node->next_answer = NULL;

#ifdef	MR_TABLE_DEBUG
	if (MR_tabledebug) {
		printf(""new answer slot %d at %p(%p), storing into %p\\n"",
			subgoal->num_ans, answer_node,
			&answer_node->answer_data, subgoal->answer_list_tail);
	}
#endif

	*(subgoal->answer_list_tail) = answer_node;
	subgoal->answer_list_tail = &(answer_node->next_answer);

	Slot = (MR_Word) &(answer_node->answer_data);
#endif
").

table_nondet_return_all_ans(TrieNode, Answer) :-
	semipure pickup_answer_list(TrieNode, CurNode0),
	semipure table_nondet_return_all_ans_2(CurNode0, Answer).

table_multi_return_all_ans(TrieNode, Answer) :-
	semipure pickup_answer_list(TrieNode, CurNode0),
	( semipure return_next_answer(CurNode0, FirstAnswer, CurNode1) ->
		(
			Answer = FirstAnswer
		;
			semipure table_nondet_return_all_ans_2(CurNode1,
				Answer)
		)
	;
		error("table_multi_return_all_ans: no first answer")
	).

:- semipure pred table_nondet_return_all_ans_2(c_pointer::in,
	ml_answer_block::out) is nondet.

table_nondet_return_all_ans_2(CurNode0, Answer) :-
	semipure return_next_answer(CurNode0, FirstAnswer, CurNode1),
	(
		Answer = FirstAnswer
	;
		semipure table_nondet_return_all_ans_2(CurNode1, Answer)
	).

:- semipure pred pickup_answer_list(ml_subgoal_table_node::in, c_pointer::out)
	is det.

:- pragma foreign_proc("C", pickup_answer_list(T::in, CurNode::out),
	[will_not_call_mercury], "
#ifdef MR_USE_MINIMAL_MODEL
		MR_TrieNode	table;

		table = (MR_TrieNode) T;
	CurNode = (MR_Word) table->MR_subgoal->answer_list;

  #ifdef MR_TABLE_DEBUG
		if (MR_tabledebug) {
			printf(""restoring all answers in %p -> %p\\n"",
				table, table->MR_subgoal);
		}
  #endif
#endif
").

:- semipure pred return_next_answer(c_pointer::in, ml_answer_block::out,
	c_pointer::out) is semidet.

:- pragma foreign_proc("C",
	return_next_answer(CurNode0::in, AnswerBlock::out, CurNode::out),
	[will_not_call_mercury], "
#ifdef MR_USE_MINIMAL_MODEL
	MR_AnswerList	cur_node0;

	cur_node0 = (MR_AnswerList *) CurNode0;
	if (cur_node0 == NULL) {
		SUCCESS_INDICATOR = MR_FALSE;
		} else {
		AnswerBlock = (MR_Word) &cur_node0->answer_data;
		CurNode = (MR_Word) cur_node0->next_answer;
		SUCCESS_INDICATOR = MR_TRUE;
		}
#else
		MR_fatal_error(""minimal model code entered when not enabled"");
#endif
").

:- pragma promise_semipure(table_nondet_is_complete/1).
table_nondet_is_complete(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_nondet_is_complete").

:- pragma promise_semipure(table_nondet_is_active/1).
table_nondet_is_active(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_nondet_is_active").
	
table_nondet_mark_as_active(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_nondet_mark_as_active").

table_nondet_get_ans_table(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_nondet_get_ans_table").

table_nondet_answer_is_not_duplicate(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_nondet_answer_is_not_duplicate").

table_nondet_new_ans_slot(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_nondet_new_ans_slot").

:- pragma promise_semipure(pickup_answer_list/2).
pickup_answer_list(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("pickup_answer_list").

:- pragma promise_semipure(return_next_answer/3).
return_next_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("return_next_answer").

%-----------------------------------------------------------------------------%

:- interface.

%
% Utility predicates that are needed in the tabling of both
% simple and nondet subgoals.
%

%
% The following table_lookup_insert... predicates lookup or insert the second
% argument into the trie pointed to by the first argument. The value returned
% is a pointer to the leaf of the trie reached by the lookup. From the
% returned leaf another trie may be connected.
%

	% Lookup or insert an integer in the given table.
:- impure pred table_lookup_insert_int(ml_table::in, int::in, ml_table::out)
	is det.

	% Lookup or insert an integer in the given table.
:- impure pred table_lookup_insert_start_int(ml_table::in, int::in, int::in,
	ml_table::out) is det.

	% Lookup or insert a character in the given trie.
:- impure pred table_lookup_insert_char(ml_table::in, character::in,
	ml_table::out) is det.

	% Lookup or insert a string in the given trie.
:- impure pred table_lookup_insert_string(ml_table::in, string::in,
	ml_table::out) is det.

	% Lookup or insert a float in the current trie.
:- impure pred table_lookup_insert_float(ml_table::in, float::in,
	ml_table::out) is det.

	% Lookup or inert an enumeration type in the given trie.
:- impure pred table_lookup_insert_enum(ml_table::in, int::in, T::in,
	ml_table::out) is det.

	% Lookup or insert a monomorphic user defined type in the given trie.
:- impure pred table_lookup_insert_user(ml_table::in, T::in, ml_table::out)
	is det.

	% Lookup or insert a polymorphic user defined type in the given trie.
:- impure pred table_lookup_insert_poly(ml_table::in, T::in, ml_table::out)
	is det.

	% Save an integer answer in the given answer block at the given
	% offset.
:- impure pred table_save_int_ans(ml_answer_block::in, int::in, int::in)
	is det.

	% Save a character answer in the given answer block at the given
	% offset.
:- impure pred table_save_char_ans(ml_answer_block::in, int::in, character::in)
	is det.

	% Save a string answer in the given answer block at the given
	% offset.
:- impure pred table_save_string_ans(ml_answer_block::in, int::in, string::in)
	is det.

	% Save a float answer in the given answer block at the given
	% offset.
:- impure pred table_save_float_ans(ml_answer_block::in, int::in, float::in)
	is det.

	% Save an I/O state in the given answer block at the given offset.
:- impure pred table_save_io_state_ans(ml_answer_block::in, int::in,
	io__state::ui) is det.

	% Save any type of answer in the given answer block at the given
	% offset.
:- impure pred table_save_any_ans(ml_answer_block::in, int::in, T::in) is det.

	% Restore an integer answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_int_ans(ml_answer_block::in, int::in, int::out)
	is det.

	% Restore a character answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_char_ans(ml_answer_block::in, int::in,
	character::out) is det.

	% Restore a string answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_string_ans(ml_answer_block::in, int::in,
	string::out) is det.

	% Restore a float answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_float_ans(ml_answer_block::in, int::in,
	float::out) is det.

	% Restore an I/O state from the given answer block at the given offset.
:- semipure pred table_restore_io_state_ans(ml_answer_block::in, int::in,
	io__state::uo) is det.

	% Restore any type of answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_any_ans(ml_answer_block::in, int::in, T::out)
	is det.

	% Report an error message about the current subgoal looping.
:- pred table_loopcheck_error(string::in) is erroneous.

	% Create an answer block with the given number of slots and add it
	% to the given table.
:- impure pred table_create_ans_block(ml_subgoal_table_node::in, int::in,
	ml_answer_block::out) is det.

	% Report statistics on the operation of the tabling system to stderr.
:- impure pred table_report_statistics is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

:- pragma foreign_decl("C", "

#include ""mercury_misc.h""		/* for MR_fatal_error(); */
#include ""mercury_type_info.h""	/* for MR_TypeCtorInfo_Struct; */
#include ""mercury_tabling.h""		/* for MR_TrieNode, etc. */

#ifdef MR_HIGHLEVEL_CODE
  #define MR_TYPE_CTOR_INFO_INT	      \
  	mercury__builtin__builtin__type_ctor_info_int_0
  #define MR_TYPE_CTOR_INFO_STRING    \
  	mercury__builtin__builtin__type_ctor_info_string_0
  #define MR_TYPE_CTOR_INFO_FLOAT     \
  	mercury__builtin__builtin__type_ctor_info_float_0
  #define MR_TYPE_CTOR_INFO_CHAR      \
  	mercury__builtin__builtin__type_ctor_info_character_0
  #define MR_TYPE_CTOR_INFO_IO_STATE  \
  	mercury__io__io__type_ctor_info_state_0
#else
  #define MR_TYPE_CTOR_INFO_INT	      mercury_data___type_ctor_info_int_0
  #define MR_TYPE_CTOR_INFO_STRING    mercury_data___type_ctor_info_string_0
  #define MR_TYPE_CTOR_INFO_FLOAT     mercury_data___type_ctor_info_float_0
  #define MR_TYPE_CTOR_INFO_CHAR      mercury_data___type_ctor_info_character_0
  #define MR_TYPE_CTOR_INFO_IO_STATE  mercury_data_io__type_ctor_info_state_0
#endif

MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_INT);
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_STRING);
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_FLOAT);
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_CHAR);
MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_IO_STATE);

").

:- pragma foreign_proc("C", table_lookup_insert_int(T0::in, I::in, T::out),
	[will_not_call_mercury],
"
	MR_TrieNode	table0, table;

	table0 = (MR_TrieNode) T0;
	MR_DEBUG_NEW_TABLE_INT(table, table0, (MR_Integer) I);
	T = (MR_Word) table;
").

:- pragma foreign_proc("C",
	table_lookup_insert_start_int(T0::in, S::in, I::in, T::out),
	[will_not_call_mercury],
"
	MR_TrieNode	table0, table;

	table0 = (MR_TrieNode) T0;
	MR_DEBUG_NEW_TABLE_START_INT(table, table0,
		(MR_Integer) S, (MR_Integer) I);
	T = (MR_Word) table;
").

:- pragma foreign_proc("C",
	table_lookup_insert_char(T0::in, C::in, T::out),
	[will_not_call_mercury],
"
	MR_TrieNode	table0, table;

	table0 = (MR_TrieNode) T0;
	MR_DEBUG_NEW_TABLE_CHAR(table, table0, (MR_Integer) C);
	T = (MR_Word) table;
").

:- pragma foreign_proc("C",
	table_lookup_insert_string(T0::in, S::in, T::out),
	[will_not_call_mercury],
"
	MR_TrieNode	table0, table;

	table0 = (MR_TrieNode) T0;
	MR_DEBUG_NEW_TABLE_STRING(table, table0, (MR_String) S);
	T = (MR_Word) table;
").

:- pragma foreign_proc("C",
	table_lookup_insert_float(T0::in, F::in, T::out),
	[will_not_call_mercury],
"
	MR_TrieNode	table0, table;

	table0 = (MR_TrieNode) T0;
	MR_DEBUG_NEW_TABLE_FLOAT(table, table0, F);
	T = (MR_Word) table;
").

:- pragma foreign_proc("C", 
	table_lookup_insert_enum(T0::in, R::in, V::in, T::out),
	[will_not_call_mercury],
"
	MR_TrieNode	table0, table;

	table0 = (MR_TrieNode) T0;
	MR_DEBUG_NEW_TABLE_ENUM(table, table0, R, V);
	T = (MR_Word) table;
").

:- pragma foreign_proc("C",
	table_lookup_insert_user(T0::in, V::in, T::out),
	[will_not_call_mercury],
"
	MR_TrieNode	table0, table;

	table0 = (MR_TrieNode) T0;
	MR_DEBUG_NEW_TABLE_ANY(table, table0, (MR_TypeInfo) TypeInfo_for_T, V);
	T = (MR_Word) table;
").

:- pragma foreign_proc("C",
	table_lookup_insert_poly(T0::in, V::in, T::out),
	[will_not_call_mercury],
"
	MR_TrieNode	table0, table;

	table0 = (MR_TrieNode) T0;
	MR_DEBUG_NEW_TABLE_ANY(table, table0, (MR_TypeInfo) TypeInfo_for_T, V);
	T = (MR_Word) table;
").

:- pragma foreign_proc("C",
	table_save_int_ans(T::in, Offset::in, I::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	MR_TABLE_SAVE_ANSWER(table, Offset, I, &MR_TYPE_CTOR_INFO_INT);
").

:- pragma foreign_proc("C",
	table_save_char_ans(T::in, Offset::in, C::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	MR_TABLE_SAVE_ANSWER(table, Offset, C, &MR_TYPE_CTOR_INFO_CHAR);
").

:- pragma foreign_proc("C",
	table_save_string_ans(T::in, Offset::in, S::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	MR_TABLE_SAVE_ANSWER(table, Offset, (MR_Word) S,
		&MR_TYPE_CTOR_INFO_STRING);
").

:- pragma foreign_proc("C",
	table_save_float_ans(T::in, Offset::in, F::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
#ifdef MR_HIGHLEVEL_CODE
	MR_TABLE_SAVE_ANSWER(table, Offset, (MR_Word) MR_box_float(F),
		&MR_TYPE_CTOR_INFO_FLOAT);
#else
	MR_TABLE_SAVE_ANSWER(table, Offset, MR_float_to_word(F),
		&MR_TYPE_CTOR_INFO_FLOAT);
#endif
").

:- pragma foreign_proc("C",
	table_save_io_state_ans(T::in, Offset::in, S::ui),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	MR_TABLE_SAVE_ANSWER(table, Offset, (MR_Word) S,
		&MR_TYPE_CTOR_INFO_IO_STATE);
").

:- pragma foreign_proc("C", 
	table_save_any_ans(T::in, Offset::in, V::in),
	[will_not_call_mercury],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	MR_TABLE_SAVE_ANSWER(table, Offset, V, TypeInfo_for_T);
").

:- pragma foreign_proc("C",
	table_restore_int_ans(T::in, Offset::in, I::out),
	[will_not_call_mercury, promise_semipure],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	I = (MR_Integer) MR_TABLE_GET_ANSWER(table, Offset);
").

:- pragma foreign_proc("C",
	table_restore_char_ans(T::in, Offset::in, C::out),
	[will_not_call_mercury, promise_semipure],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	C = (MR_Char) MR_TABLE_GET_ANSWER(table, Offset);
").

:- pragma foreign_proc("C",
	table_restore_string_ans(T::in, Offset::in, S::out),
	[will_not_call_mercury, promise_semipure],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	S = (MR_String) MR_TABLE_GET_ANSWER(table, Offset);
").

:- pragma foreign_proc("C",
	table_restore_float_ans(T::in, Offset::in, F::out),
	[will_not_call_mercury, promise_semipure],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
#ifdef MR_HIGHLEVEL_CODE
	F = MR_unbox_float(MR_TABLE_GET_ANSWER(table, Offset));
#else
	F = MR_word_to_float(MR_TABLE_GET_ANSWER(table, Offset));
#endif
").

:- pragma foreign_proc("C",
	table_restore_io_state_ans(T::in, Offset::in, V::uo),
	[will_not_call_mercury, promise_semipure],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	V = (MR_Word) MR_TABLE_GET_ANSWER(table, Offset);
").

:- pragma foreign_proc("C",
	table_restore_any_ans(T::in, Offset::in, V::out),
	[will_not_call_mercury, promise_semipure],
"
	MR_TrieNode	table;

	table = (MR_TrieNode) T;
	V = (MR_Word) MR_TABLE_GET_ANSWER(table, Offset);
").

:- pragma foreign_proc("C",
	table_create_ans_block(T0::in, Size::in, T::out),
	[will_not_call_mercury],
"
	MR_TrieNode	table0;

	table0 = (MR_TrieNode) T0;
	MR_TABLE_CREATE_ANSWER_BLOCK(table0, Size);
	T = T0;
").

table_loopcheck_error(Message) :-
	error(Message).

:- pragma foreign_proc("C",
	table_report_statistics, [will_not_call_mercury], "
	MR_table_report_statistics(stderr);
").


table_lookup_insert_int(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_lookup_insert_int").

table_lookup_insert_start_int(_, _, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_lookup_insert_start_int").

table_lookup_insert_char(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_lookup_insert_char").

table_lookup_insert_string(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_lookup_insert_string").

table_lookup_insert_float(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_lookup_insert_float").

table_lookup_insert_enum(_, _, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_lookup_insert_enum").

table_lookup_insert_user(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_lookup_insert_user").

table_lookup_insert_poly(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_lookup_insert_poly").

table_save_int_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_int_ans").

table_save_char_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_char_ans").

table_save_string_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_string_ans").

table_save_float_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_float_ans").

table_save_io_state_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_io_state_ans").

table_save_any_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_any_ans").

:- pragma promise_semipure(table_restore_int_ans/3).
table_restore_int_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_restore_int_ans").

:- pragma promise_semipure(table_restore_char_ans/3).
table_restore_char_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_restore_char_ans").

:- pragma promise_semipure(table_restore_string_ans/3).
table_restore_string_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_restore_string_ans").

:- pragma promise_semipure(table_restore_float_ans/3).
table_restore_float_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_restore_float_ans").

:- pragma promise_semipure(table_restore_io_state_ans/3).
table_restore_io_state_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_restore_io_state_ans").

:- pragma promise_semipure(table_restore_any_ans/3).
table_restore_any_ans(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_restore_any_ans").

table_create_ans_block(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_create_ans_block").

table_report_statistics :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_report_statistics").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
