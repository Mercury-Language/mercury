%---------------------------------------------------------------------------%
% Copyright (C) 1998-2004 The University of Melbourne.
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
% The predicates fall into five categories:
%
% (1)	Predicates that manage loop checking.
%
% (2)	Predicates that manage memoization in general.
%
% (3)	Predicates that manage memoization of I/O procedures.
%
% (4)	Predicates that manage minimal model tabling.
%
% (5)	Utility predicates that are needed in the tabling of all predicates.
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
% For minimal model goals, the word at the end of the call table trie always
% points to a subgoal structure, with several fields. The status of the
% subgoal and the list of answers are two of these fields. Other fields,
% described in runtime/mercury_minimal_model.h, are used in the implementation
% of the minimal model.
%
% All of the predicates here with the impure declaration modify the tabling
% structures. Because the structures are persistent through backtracking,
% this causes the predicates to become impure. The predicates with the semipure
% directive only examine the tabling structures, but do not modify them.
%
% At the moment, tabling is supported only by the LLDS and MLDS C backends,
% so in the next three type definitions, only the C definition is useful.
% The Mercury and IL definitions are placeholders only, required to make
% this module compile cleanly on the Java and .NET backends respectively.
%
% XXX
% All the types defined ought to be abstract types. The only reason why their
% implementation is exported is that if they aren't, C code inside and
% outside the module would end up using different C types to represent
% values of these Mercury types, and lcc treats those type disagreements
% as errors.

	% This type represents the interior pointers of both call
	% tables and answer tables.
:- type ml_trie_node.

	% This type represents the blocks we use to store sets of output
	% arguments.
:- type ml_answer_block.

	% N.B. interface continued below

%-----------------------------------------------------------------------------%

:- implementation.

	% This type represents the interior pointers of both call
	% tables and answer tables.
:- type ml_trie_node --->	ml_trie_node(c_pointer).
:- pragma foreign_type("C", ml_trie_node, "MR_TrieNode",
	[can_pass_as_mercury_type]).
:- pragma foreign_type(il,  ml_trie_node, "class [mscorlib]System.Object").

	% This type represents a block of memory that contains one word
	% for each output argument of a procedure.
:- type ml_answer_block --->	ml_answer_block(c_pointer).
:- pragma foreign_type("C", ml_answer_block, "MR_AnswerBlock",
	[can_pass_as_mercury_type]).
:- pragma foreign_type(il,  ml_answer_block, "class [mscorlib]System.Object").

%-----------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage loop checking.
%

	% This type should correspond exactly to the MR_LOOPCHECK_* #defines
	% in runtime/mercury_tabling.h.

:- type loop_status
	--->	loop_inactive
	;	loop_active.

:- impure pred table_loop_setup(ml_trie_node::in, loop_status::out) is det.

	% Mark the call represented by the given table as currently
	% not being evaluated (working on an answer).
:- impure pred table_loop_mark_as_inactive(ml_trie_node::in) is det.

	% N.B. interface continued below

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_proc("C",
	table_loop_setup(T::in, Status::out),
	[will_not_call_mercury],
"
	MR_table_loop_setup(T, Status);
").

:- pragma foreign_proc("C",
	table_loop_mark_as_inactive(T::in),
	[will_not_call_mercury],
"
	MR_table_loop_mark_as_inactive(T);
").

table_loop_setup(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_loop_setup").

table_loop_mark_as_inactive(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_loop_mark_as_inactive").

%-----------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage memoization.
%

	% This type should correspond exactly to the MR_MEMO_DET_* #defines
	% in runtime/mercury_tabling.h.

:- type memo_det_status
	--->	memo_det_inactive
	;	memo_det_active
	;	memo_det_succeeded.

	% This type should correspond exactly to the MR_MEMO_SEMI_* #defines
	% in runtime/mercury_tabling.h.

:- type memo_semi_status
	--->	memo_semi_inactive
	;	memo_semi_active
	;	memo_semi_succeeded
	;	memo_semi_failed.

:- impure pred table_memo_det_setup(ml_trie_node::in, memo_det_status::out)
	is det.

:- impure pred table_memo_semi_setup(ml_trie_node::in, memo_semi_status::out)
	is det.

	% Save the fact that the call has failed in the given table.
:- impure pred table_memo_mark_as_failed(ml_trie_node::in) is failure.

	% Save the fact that the call has succeeded in the given table.
:- impure pred table_memo_mark_as_succeeded(ml_trie_node::in) is det.

	% Create an answer block with the given number of slots and add it
	% to the given table.
:- impure pred table_memo_create_answer_block(ml_trie_node::in, int::in,
	ml_answer_block::out) is det.

	% Return the answer block for the given call.
:- semipure pred table_memo_get_answer_block(ml_trie_node::in,
	ml_answer_block::out) is det.

	% N.B. interface continued below

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_proc("C",
	table_memo_det_setup(T::in, Status::out),
	[will_not_call_mercury],
"
	MR_table_memo_det_setup(T, Status);
").

:- pragma foreign_proc("C",
	table_memo_semi_setup(T::in, Status::out),
	[will_not_call_mercury],
"
	MR_table_memo_semi_setup(T, Status);
").

:- pragma foreign_proc("C",
	table_memo_mark_as_failed(T::in),
	[will_not_call_mercury],
"
	MR_table_memo_mark_as_failed(T);
").

:- pragma foreign_proc("C",
	table_memo_mark_as_succeeded(T::in),
	[will_not_call_mercury],
"
	MR_table_memo_mark_as_succeeded(T);
").

:- pragma foreign_proc("C",
	table_memo_create_answer_block(T::in, Size::in, AnswerBlock::out),
	[will_not_call_mercury],
"
	MR_table_memo_create_answer_block(T, Size, AnswerBlock);
").

:- pragma foreign_proc("C",
	table_memo_get_answer_block(T::in, AnswerBlock::out),
	[will_not_call_mercury, promise_semipure],
"
	MR_table_memo_get_answer_block(T, AnswerBlock);
").

table_memo_det_setup(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_memo_det_setup").

table_memo_semi_setup(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_memo_semi_setup").

table_memo_mark_as_failed(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_memo_mark_as_failed"),
	fail.

table_memo_mark_as_succeeded(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_memo_mark_as_succeeded").

table_memo_create_answer_block(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_memo_create_answer_block").

table_memo_get_answer_block(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__semip,
	private_builtin__sorry("table_memo_get_answer_block").

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

:- impure pred table_io_in_range(ml_trie_node::out, int::out, int::out)
	is semidet.

	% This procedure should be called exactly once for each I/O action
	% for which table_io_in_range returns true. Given the trie node
	% for a given I/O action number, it returns true iff that action has
	% been carried out before (i.e. the action is now being reexecuted
	% after a retry command in the debugger).

:- impure pred table_io_has_occurred(ml_trie_node::in) is semidet.

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
	MR_table_io_in_range(T, Counter, Start, SUCCESS_INDICATOR);
").

:- pragma foreign_proc("C",
	table_io_has_occurred(T::in),
	[will_not_call_mercury],
"
	MR_table_io_has_occurred(T, SUCCESS_INDICATOR);
").

table_io_copy_io_state(IO, IO).

:- pragma foreign_proc("C",
	table_io_left_bracket_unitized_goal(TraceEnabled::out),
	[will_not_call_mercury],
"
	MR_table_io_left_bracket_unitized_goal(TraceEnabled);
").

:- pragma foreign_proc("C",
	table_io_right_bracket_unitized_goal(TraceEnabled::in),
	[will_not_call_mercury],
"
	MR_table_io_right_bracket_unitized_goal(TraceEnabled);
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

table_io_left_bracket_unitized_goal(_TraceEnabled) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_io_left_bracket_unitized_goal").

table_io_right_bracket_unitized_goal(_TraceEnabled) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_io_right_bracket_unitized_goal").

%-----------------------------------------------------------------------------%

:- interface.

%
% Predicates that manage the tabling of model_non subgoals.
%

	% This type should correspond exactly to the type MR_SubgoalStatus
	% defined in runtime/mercury_tabling.h.

:- type mm_status
	--->	mm_inactive
	;	mm_active
	;	mm_complete.

	% This type represents the data structure at the tips of the call table
	% in minimal_model predicates.
:- type ml_subgoal.

	% Check if this minimal_model subgoal has been called before.
	% If not, return inactive as the old status, set up the subgoal's
	% structure, and mark it as active, since the caller is about to become
	% the subgoal's generator. If yes, return the status currently recorded
	% in the subgoal structure.
:- impure pred table_mm_setup(ml_trie_node::in, ml_subgoal::out,
	mm_status::out) is det.

	% Save the state of the current subgoal and fail. Sometime later,
	% when the subgoal has some solutions, table_mm_completion will
	% restore the saved state. At the time, table_mm_suspend_consumer
	% will succeed, and return an answer block as its second argument.
:- impure pred table_mm_suspend_consumer(ml_subgoal::in, ml_answer_block::out)
	is nondet.

	% Resume all suspended subgoal calls. This predicate will resume each
	% of the suspended subgoals that depend on it in turn until it reaches
	% a fixed point, at which all depended suspended subgoals have had
	% all available answers returned to them.
:- impure pred table_mm_completion(ml_subgoal::in) is det.

	% Return the table of answers already returned to the given nondet
	% table.
:- semipure pred table_mm_get_answer_table(ml_subgoal::in, ml_trie_node::out)
	is det.

	% If the answer represented by the given answer table
	% has not been generated before by this subgoal,
	% succeed and remember the answer as having been generated.
	% If the answer has been generated before, fail.
:- impure pred table_mm_answer_is_not_duplicate(ml_trie_node::in) is semidet.

	% Create a new slot in the answer list of the subgoal, create a new
	% answer block of the given size, and put the answer block in the new
	% slot.
:- impure pred table_mm_create_answer_block(ml_subgoal::in, int::in,
	ml_answer_block::out) is det.

	% Return all of the answer blocks stored in the given table.
:- semipure pred table_mm_return_all_nondet(ml_subgoal::in,
	ml_answer_block::out) is nondet.
:- semipure pred table_mm_return_all_multi(ml_subgoal::in,
	ml_answer_block::out) is multi.

	% N.B. interface continued below

%-----------------------------------------------------------------------------%

:- implementation.

:- type ml_subgoal --->		ml_subgoal(c_pointer).
:- pragma foreign_type("C", ml_subgoal, "MR_SubgoalPtr",
	[can_pass_as_mercury_type]).
:- pragma foreign_type(il,  ml_subgoal, "class [mscorlib]System.Object").

	% This type represents a list of answers of a model_non predicate.
:- type ml_answer_list --->	ml_answer_list(c_pointer).
:- pragma foreign_type("C", ml_answer_list, "MR_AnswerList",
	[can_pass_as_mercury_type]).
:- pragma foreign_type(il,  ml_answer_list, "class [mscorlib]System.Object").

:- pragma foreign_proc("C",
	table_mm_setup(T::in, Subgoal::out, Status::out),
	[will_not_call_mercury],
"
	MR_table_mm_setup(T, Subgoal, Status);
").

	% The definitions of these two predicates are in the runtime system,
	% in runtime/mercury_minimal_model.c.
:- external(table_mm_suspend_consumer/2).
:- external(table_mm_completion/1).
:- external(table_mm_answer_is_not_duplicate/1).
:- external(table_mm_return_all_nondet/2).
:- external(table_mm_return_all_multi/2).

:- pragma foreign_proc("C",
	table_mm_get_answer_table(Subgoal::in, AnswerTable::out),
	[will_not_call_mercury, promise_semipure],
"
	MR_table_mm_get_answer_table(Subgoal, AnswerTable);
").

:- pragma foreign_proc("C",
	table_mm_create_answer_block(Subgoal::in, Size::in, AnswerBlock::out),
	[will_not_call_mercury],
"
	MR_table_mm_create_answer_block(Subgoal, Size, AnswerBlock);
").

table_mm_get_answer_table(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__semip,
	private_builtin__sorry("table_mm_get_answer_table").

table_mm_create_answer_block(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_mm_create_answer_block").

%-----------------------------------------------------------------------------%

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
:- impure pred table_lookup_insert_int(ml_trie_node::in, int::in,
	ml_trie_node::out) is det.

	% Lookup or insert an integer in the given table.
:- impure pred table_lookup_insert_start_int(ml_trie_node::in, int::in,
	int::in, ml_trie_node::out) is det.

	% Lookup or insert a character in the given trie.
:- impure pred table_lookup_insert_char(ml_trie_node::in, character::in,
	ml_trie_node::out) is det.

	% Lookup or insert a string in the given trie.
:- impure pred table_lookup_insert_string(ml_trie_node::in, string::in,
	ml_trie_node::out) is det.

	% Lookup or insert a float in the current trie.
:- impure pred table_lookup_insert_float(ml_trie_node::in, float::in,
	ml_trie_node::out) is det.

	% Lookup or inert an enumeration type in the given trie.
:- impure pred table_lookup_insert_enum(ml_trie_node::in, int::in, T::in,
	ml_trie_node::out) is det.

	% Lookup or insert a monomorphic user defined type in the given trie.
:- impure pred table_lookup_insert_user(ml_trie_node::in, T::in,
	ml_trie_node::out) is det.

	% Lookup or insert a polymorphic user defined type in the given trie.
:- impure pred table_lookup_insert_poly(ml_trie_node::in, T::in,
	ml_trie_node::out) is det.

	% Save an integer answer in the given answer block at the given
	% offset.
:- impure pred table_save_int_answer(ml_answer_block::in, int::in, int::in)
	is det.

	% Save a character answer in the given answer block at the given
	% offset.
:- impure pred table_save_char_answer(ml_answer_block::in, int::in,
	character::in) is det.

	% Save a string answer in the given answer block at the given
	% offset.
:- impure pred table_save_string_answer(ml_answer_block::in, int::in,
	string::in) is det.

	% Save a float answer in the given answer block at the given
	% offset.
:- impure pred table_save_float_answer(ml_answer_block::in, int::in, float::in)
	is det.

	% Save an I/O state in the given answer block at the given offset.
:- impure pred table_save_io_state_answer(ml_answer_block::in, int::in,
	io__state::ui) is det.

	% Save any type of answer in the given answer block at the given
	% offset.
:- impure pred table_save_any_answer(ml_answer_block::in, int::in, T::in)
	is det.

	% Restore an integer answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_int_answer(ml_answer_block::in, int::in,
	int::out) is det.

	% Restore a character answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_char_answer(ml_answer_block::in, int::in,
	character::out) is det.

	% Restore a string answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_string_answer(ml_answer_block::in, int::in,
	string::out) is det.

	% Restore a float answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_float_answer(ml_answer_block::in, int::in,
	float::out) is det.

	% Restore an I/O state from the given answer block at the given offset.
:- semipure pred table_restore_io_state_answer(ml_answer_block::in, int::in,
	io__state::uo) is det.

	% Restore any type of answer from the given answer block at the
	% given offset.
:- semipure pred table_restore_any_answer(ml_answer_block::in, int::in, T::out)
	is det.

	% Report an error message.
:- pred table_error(string::in) is erroneous.

	% Report statistics on the operation of the tabling system to stderr.
:- impure pred table_report_statistics is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require.

:- pragma foreign_decl("C", "

#include ""mercury_misc.h""		/* for MR_fatal_error(); */
#include ""mercury_type_info.h""	/* for MR_TypeCtorInfo_Struct; */
#include ""mercury_tabling.h""		/* for MR_TrieNode, etc. */

MR_DECLARE_TYPE_CTOR_INFO_STRUCT(MR_TYPE_CTOR_INFO_NAME(io, state, 0));

").

:- pragma foreign_proc("C",
	table_lookup_insert_int(T0::in, I::in, T::out),
	[will_not_call_mercury],
"
	MR_DEBUG_NEW_TABLE_INT(T, T0, (MR_Integer) I);
").

:- pragma foreign_proc("C",
	table_lookup_insert_start_int(T0::in, S::in, I::in, T::out),
	[will_not_call_mercury],
"
	MR_DEBUG_NEW_TABLE_START_INT(T, T0, (MR_Integer) S, (MR_Integer) I);
").

:- pragma foreign_proc("C",
	table_lookup_insert_char(T0::in, C::in, T::out),
	[will_not_call_mercury],
"
	MR_DEBUG_NEW_TABLE_CHAR(T, T0, (MR_Integer) C);
").

:- pragma foreign_proc("C",
	table_lookup_insert_string(T0::in, S::in, T::out),
	[will_not_call_mercury],
"
	MR_DEBUG_NEW_TABLE_STRING(T, T0, (MR_String) S);
").

:- pragma foreign_proc("C",
	table_lookup_insert_float(T0::in, F::in, T::out),
	[will_not_call_mercury],
"
	MR_DEBUG_NEW_TABLE_FLOAT(T, T0, F);
").

:- pragma foreign_proc("C", 
	table_lookup_insert_enum(T0::in, R::in, V::in, T::out),
	[will_not_call_mercury],
"
	MR_DEBUG_NEW_TABLE_ENUM(T, T0, R, V);
").

:- pragma foreign_proc("C",
	table_lookup_insert_user(T0::in, V::in, T::out),
	[will_not_call_mercury],
"
	MR_DEBUG_NEW_TABLE_ANY(T, T0, (MR_TypeInfo) TypeInfo_for_T, V);
").

:- pragma foreign_proc("C",
	table_lookup_insert_poly(T0::in, V::in, T::out),
	[will_not_call_mercury],
"
	MR_DEBUG_NEW_TABLE_ANY(T, T0, (MR_TypeInfo) TypeInfo_for_T, V);
").

:- pragma foreign_proc("C",
	table_save_int_answer(AB::in, Offset::in, I::in),
	[will_not_call_mercury],
"
	MR_TABLE_SAVE_ANSWER(AB, Offset, I,
		&MR_TYPE_CTOR_INFO_NAME(builtin, int, 0));
").

:- pragma foreign_proc("C",
	table_save_char_answer(AB::in, Offset::in, C::in),
	[will_not_call_mercury],
"
	MR_TABLE_SAVE_ANSWER(AB, Offset, C,
		&MR_TYPE_CTOR_INFO_NAME(builtin, character, 0));
").

:- pragma foreign_proc("C",
	table_save_string_answer(AB::in, Offset::in, S::in),
	[will_not_call_mercury],
"
	MR_TABLE_SAVE_ANSWER(AB, Offset, (MR_Word) S,
		&MR_TYPE_CTOR_INFO_NAME(builtin, string, 0));
").

:- pragma foreign_proc("C",
	table_save_float_answer(AB::in, Offset::in, F::in),
	[will_not_call_mercury],
"
#ifdef MR_HIGHLEVEL_CODE
	MR_TABLE_SAVE_ANSWER(AB, Offset, (MR_Word) MR_box_float(F),
		&MR_TYPE_CTOR_INFO_NAME(builtin, float, 0));
#else
	MR_TABLE_SAVE_ANSWER(AB, Offset, MR_float_to_word(F),
		&MR_TYPE_CTOR_INFO_NAME(builtin, float, 0));
#endif
").

:- pragma foreign_proc("C",
	table_save_io_state_answer(AB::in, Offset::in, S::ui),
	[will_not_call_mercury],
"
	MR_TABLE_SAVE_ANSWER(AB, Offset, (MR_Word) S,
		&MR_TYPE_CTOR_INFO_NAME(io, state, 0));
").

:- pragma foreign_proc("C", 
	table_save_any_answer(AB::in, Offset::in, V::in),
	[will_not_call_mercury],
"
	MR_TABLE_SAVE_ANSWER(AB, Offset, V, TypeInfo_for_T);
").

:- pragma foreign_proc("C",
	table_restore_int_answer(AB::in, Offset::in, I::out),
	[will_not_call_mercury, promise_semipure],
"
	I = (MR_Integer) MR_TABLE_GET_ANSWER(AB, Offset);
").

:- pragma foreign_proc("C",
	table_restore_char_answer(AB::in, Offset::in, C::out),
	[will_not_call_mercury, promise_semipure],
"
	C = (MR_Char) MR_TABLE_GET_ANSWER(AB, Offset);
").

:- pragma foreign_proc("C",
	table_restore_string_answer(AB::in, Offset::in, S::out),
	[will_not_call_mercury, promise_semipure],
"
	S = (MR_String) MR_TABLE_GET_ANSWER(AB, Offset);
").

:- pragma foreign_proc("C",
	table_restore_float_answer(AB::in, Offset::in, F::out),
	[will_not_call_mercury, promise_semipure],
"
#ifdef MR_HIGHLEVEL_CODE
	F = MR_unbox_float(MR_TABLE_GET_ANSWER(AB, Offset));
#else
	F = MR_word_to_float(MR_TABLE_GET_ANSWER(AB, Offset));
#endif
").

:- pragma foreign_proc("C",
	table_restore_io_state_answer(AB::in, Offset::in, V::uo),
	[will_not_call_mercury, promise_semipure],
"
	V = (MR_Word) MR_TABLE_GET_ANSWER(AB, Offset);
").

:- pragma foreign_proc("C",
	table_restore_any_answer(AB::in, Offset::in, V::out),
	[will_not_call_mercury, promise_semipure],
"
	V = (MR_Word) MR_TABLE_GET_ANSWER(AB, Offset);
").

table_error(Message) :-
	error(Message).

:- pragma foreign_proc("C",
	table_report_statistics, [will_not_call_mercury],
"
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

table_save_int_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_int_answer").

table_save_char_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_char_answer").

table_save_string_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_string_answer").

table_save_float_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_float_answer").

table_save_io_state_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_io_state_answer").

table_save_any_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_save_any_answer").

table_restore_int_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__semip,
	private_builtin__sorry("table_restore_int_answer").

table_restore_char_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__semip,
	private_builtin__sorry("table_restore_char_answer").

table_restore_string_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__semip,
	private_builtin__sorry("table_restore_string_answer").

table_restore_float_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__semip,
	private_builtin__sorry("table_restore_float_answer").

table_restore_io_state_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__semip,
	private_builtin__sorry("table_restore_io_state_answer").

table_restore_any_answer(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__semip,
	private_builtin__sorry("table_restore_any_answer").

table_report_statistics :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	impure private_builtin__imp,
	private_builtin__sorry("table_report_statistics").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
