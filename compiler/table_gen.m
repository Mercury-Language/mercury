%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main authors: ohutch, zs.
%
% This module transforms HLDS code to implement loop detection, memoing,
% minimal model evaluation, or I/O idempotence. The transformation involves
% adding calls to predicates defined in library/table_builtin.m and in
% runtime/mercury_minimal_model.c.
%
% The loop detection transformation adds code to a procedure that allows
% early detection of infinite loops. If such loops are detected the program
% will terminate with a helpful error message.
%
% The memo transformation adds code that allows a procedure to "memo"
% (remember) answers once they have been generated using program clause
% resolution.
%
% The minimal model transformation changes the semantics of the procedure
% being transformed. See the PhD thesis of K. Sagonas: `The SLG-WAM: A
% Search-Efficient Engine for Well-Founded Evaluation of Normal Logic
% Programs' from SUNY at Stony Brook in 1996 for a description of
% the semantics behind the transformation. Currently only SLGd is
% implemented.
%
% The current implementation does not attempt to do anything special
% to handle cases where tabling interacts with if-then-else, solutions,
% and negated contexts in general. Such situations are not even detected,
% since the cost of such detection is quite substantial, and this cost
% would be "distributed fat". Therefore in such cases the system may
% silently produce incorrect answers. For time being, this is not a bug,
% but a missing feature :-)
%
% Example of transformation for semidet minimal_model :
%
%	Given the following code for left recursive transitive closure :
%
%	:- pred p(int, int).
%	:- mode p(in, in) is semidet.
%
%	p(A, B) :- e(A, B).
%	p(A, B) :- p(A, C), e(C, B).
%
%	The transformed code would be :
%
%	p(A, B) :-
%			% Get a handle on the table.
%		T0 = <table pointer for p/2>,
%
%			% Look up the input arguments.
%		impure table_lookup_insert_int(T0, A, T1),
%		impure table_lookup_insert_int(T1, B, T2),
%		(if
%			semipure table_simple_is_complete(T2)
%		then
%				% If the subgoal has already succeeded,
%				% just return.
%			semipure table_simple_has_succeeded(T2)
%		else
%		   	(if
%					% If we are already working on
%					% an answer for this subgoal, fail.
%				semipure table_simple_is_inactive(T2),
%
%					% Mark this subgoal as being evaluated.
%				impure table_simple_mark_as_active(T2),
%
%				(
%					%
%					% Original goals
%					%
%				)
%			then
%				impure table_simple_mark_as_succeeded(T2)
%			else
%				impure table_simple_mark_as_failed(T2),
%				fail
%			)
%		).
%
% Example of transformation for nondet minimal_model :
%
%	Given the following code for left recursive transitive closure :
%
%	:- pred p(int, int).
%	:- mode p(in, out) is nondet
%
%	p(A, B) :- e(A, B).
%	p(A, B) :- p(A, C), e(C, B).
%
%	The transformed code would be :
%
%	p(A, B) :-
%			% Get a handle on the table.
%		T0 = <table pointer for p/2>,
%
%			% Look up the input arguments, and set up the table.
%		impure table_lookup_insert_int(T0, A, T1),
%		impure table_nondet_setup(T1, T2),
%		impure table_subgoal_status(T2, Status),
%		(
%			Status = complete,
%				% Return all the answers from the complete
%				% table.
%			impure table_nondet_return_all_ans(T2, Ans),
%			impure table_restore_int_ans(Ans, 0, B)
%		;
%			Status = active,
%				% Suspend the current computational branch.
%			impure table_nondet_suspend(T2, Ans),
%			impure table_restore_int_ans(Ans, 0, B)
%		;
%			Status = inactive,
%		   	(
%					% Mark that this subgoal is being
%					% evaluated.
%				impure table_nondet_mark_as_active(T2),
%
%				(
%					%
%					% Original goals
%					%
%				),
%
%					% Check for duplicate answers.
%				impure table_nondet_get_ans_table(T2, AT0),
%				impure table_lookup_insert_int(AT0, B, AT1),
%
%					% The following pred is semidet;
%					% it will fail if the answer is
%					% already in the table.
%				semipure
%				    table_nondet_answer_is_not_duplicate(AT1),
%
%					% Save the new answer in the table.
%				impure table_nondet_new_ans_slot(T2, AS),
%				impure table_create_ans_block(AS, 1, AB),
%				impure table_save_int_ans(AB, 0, B)
%			;
%					% Mark this subgoal as completely
%					% evaluated, modulo any dependencies
%					% on other subgoals.
%				impure table_nondet_resume(T2),
%				fail
%			)
%		).
%
% The memo and loopcheck transformations are very similar to the above
% transformations except that for the memo case the code for handling
% loops (fail in the semi_det case, suspend in the nondet case) is changed to
% a loop check. And in the loop_check case the code for memoing answers is
% dropped and the loop handling code is modified to call an error predicate.
%
% Example of transformation for tabling I/O, for I/O primitives (i.e.
% predicates defined by pragma c_code that take an input/output pair of
% io_state arguments) that have the tabled_for_io feature:
%
%	:- pred p(int, string, io__state, io__state).
%	:- mode p(in, out, di, uo) is det.
%
%	p(A, B, S0, S) :- $pragma(...)
%
%	The transformed code would be :
%
%	p(A, B, S0, S) :-
%		(if
%				% Get the global I/O table, the global I/O
%				% counter, and the starting point for tabling
%				% I/O actions, if we are in the tabled range.
%			table_io_in_range(T0, Counter, Start)
%		then
%				% Look up the input arguments.
%			impure table_lookup_insert_start_int(T0, Counter,
%				Start, T),
%			(if
%				semipure table_io_has_occurred(T)
%			then
%				impure table_restore_string_ans(T, 0, B)
%				table_io_copy_io_state(S0, S)
%			else
%				(
%					%
%					% Call original procedure
%					%
%				),
%					% Save the answers in the table.
%				impure table_io_create_ans_block(T, 1, AnsBl),
%				impure table_save_string_ans(AnsBl, 0, B)
%			)
%		else
%			%
%			% Call original procedure
%			%
%		).
%
% Note that copying the I/O state works only because variables of type
% io__state don't actually contain any information; the information is actually
% stored in global variables. However, if this ever changes, the transformation
% can be fixed simply by changing the value of --trace-table-io-states to yes,
% which will cause such values to be tabled along with the other output
% arguments.
%
% For I/O primitives that do not have tabled_for_io, we should require that
% they do not do any I/O in their own code, meaning that all their I/O is
% inside any Mercury code they call. We can then leave such primitives
% untransformed; the I/O primitives called from the inner Mercury engine
% will do the right thing. For now, this requirement is not enforced,
% which means that enabling I/O tabling (with --trace-table-io) does not
% guarantee that *all* I/O actions are tabled. This can cause inconsistent
% behavior after retry commands in mdb. This is the reason why retry across
% I/O is experimental for now.
%
% The reason why we require I/O primitives to be marked manually by a
% programmer with the tabled_for_io feature is to get the programmer to make
% sure that the primitive meets the requirement. Unfortunately, this cannot be
% automated, since automation would require analysis of arbitrary C code.
%
% The transformation for tabling I/O for declarative debugging is a variant
% of the usual transformation of I/O primitives. In this variant, the answer
% block contains not only the answers, but also a pointer to the primitive's
% proc layout structure and the values of the input aruments. The code we
% generate will fill in the slots containing this extra information before
% it executes the original goal.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds__table_gen.

:- interface.

:- import_module hlds__hlds_module.

:- import_module io.

:- pred table_gen__process_module(module_info::in, module_info::out,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs__code_model.
:- import_module backend_libs__rtti.
:- import_module check_hlds__det_analysis.
:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__modes.
:- import_module check_hlds__polymorphism.
:- import_module check_hlds__purity.
:- import_module check_hlds__type_util.
:- import_module hlds__goal_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_error_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_out.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module hlds__passes_aux.
:- import_module hlds__quantification.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module ll_backend__code_aux.
:- import_module ll_backend__code_util.
:- import_module ll_backend__continuation_info.
:- import_module ll_backend__follow_code.
:- import_module ll_backend__llds.
:- import_module parse_tree__error_util.
:- import_module parse_tree__inst.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module transform_hlds__const_prop.

:- import_module term, varset.
:- import_module bool, int, string, list, assoc_list.
:- import_module set, map, require, std_util.

%-----------------------------------------------------------------------------%

	% NOTE: following preds seem to duplicate the code in passes_aux.m.
	% The reason for this duplication is that this module needs a variant
	% of this code that is able to handle passing a module_info to
	% polymorphism and getting an updated module_info back.
table_gen__process_module(ModuleInfo0, ModuleInfo, S0, S) :-
	module_info_preds(ModuleInfo0, Preds0),
	map__keys(Preds0, PredIds),
	table_gen__process_preds(PredIds, ModuleInfo0, ModuleInfo, S0, S).

:- pred table_gen__process_preds(list(pred_id)::in,
	module_info::in, module_info::out, io__state::di, io__state::uo) is det.

table_gen__process_preds([], ModuleInfo, ModuleInfo, S, S).
table_gen__process_preds([PredId | PredIds], ModuleInfo0, ModuleInfo, S0, S) :-
	table_gen__process_pred(PredId, ModuleInfo0, ModuleInfo1, S0, S1),
	table_gen__process_preds(PredIds, ModuleInfo1, ModuleInfo, S1, S).

:- pred table_gen__process_pred(pred_id::in, module_info::in, module_info::out,
	io__state::di, io__state::uo) is det.

table_gen__process_pred(PredId, !ModuleInfo, !IO) :-
	module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
	ProcIds = pred_info_procids(PredInfo),
	table_gen__process_procs(PredId, ProcIds, !ModuleInfo, !IO).

:- pred table_gen__process_procs(pred_id::in, list(proc_id)::in,
	module_info::in, module_info::out, io__state::di, io__state::uo)
	is det.

table_gen__process_procs(_PredId, [], !ModuleInfo, !IO).
table_gen__process_procs(PredId, [ProcId | ProcIds], !ModuleInfo, !IO) :-
	module_info_preds(!.ModuleInfo, PredTable),
	map__lookup(PredTable, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo0),
	table_gen__process_proc(PredId, ProcId, ProcInfo0, PredInfo,
		!ModuleInfo, !IO),
	table_gen__process_procs(PredId, ProcIds, !ModuleInfo, !IO).

:- pred table_gen__process_proc(pred_id::in, proc_id::in, proc_info::in,
	pred_info::in, module_info::in, module_info::out, io__state::di,
	io__state::uo) is det.

table_gen__process_proc(PredId, ProcId, ProcInfo0, PredInfo0,
		!ModuleInfo, !IO) :-
	proc_info_eval_method(ProcInfo0, EvalMethod),
	( eval_method_requires_tabling_transform(EvalMethod) = yes ->
		table_gen__transform_proc(EvalMethod, PredId, ProcId,
			ProcInfo0, _, PredInfo0, _, !ModuleInfo)
	;
		module_info_globals(!.ModuleInfo, Globals),
		globals__lookup_bool_option(Globals, trace_table_io, yes),
		proc_info_has_io_state_pair(!.ModuleInfo, ProcInfo0,
			_InArgNum, _OutArgNum)
	->
% XXX We can't include this sanity checking code, because it fails on
% globals:io_lookup_bool_option.
%
%		proc_info_interface_code_model(ProcInfo0, CodeModel),
%		( CodeModel = model_det ->
%			true
%		;
%			pred_id_to_int(PredId, PredIdInt),
%			Msg = string__format(
%				"I/O procedure pred id %d not model_det",
%				[i(PredIdInt)]),
%			error(Msg)
%		),
		globals__lookup_bool_option(Globals, trace_table_io_all,
			TransformAll),
		globals__lookup_bool_option(Globals, trace_table_io_require,
			Require),
		proc_info_goal(ProcInfo0, BodyGoal),
		predicate_module(!.ModuleInfo, PredId, PredModuleName),
		should_io_procedure_be_transformed(TransformAll, Require,
			BodyGoal, PredModuleName, AnnotationIsMissing,
			TransformPrimitive),
		(
			AnnotationIsMissing = yes,
			report_missing_tabled_for_io(!.ModuleInfo, PredInfo0,
				PredId, ProcId, !IO),
			module_info_incr_errors(!ModuleInfo)
		;
			AnnotationIsMissing = no
		),
		(
			TransformPrimitive = no
		;
			TransformPrimitive = yes(Unitize),
			globals__lookup_bool_option(Globals,
				trace_table_io_only_retry,
				TraceTableIoOnlyRetry),
			(
				TraceTableIoOnlyRetry = no,
				Decl = table_io_decl
			;
				TraceTableIoOnlyRetry = yes,
				Decl = table_io_proc
			),
			TableIoMethod = eval_table_io(Decl, Unitize),
			proc_info_set_eval_method(TableIoMethod,
				ProcInfo0, ProcInfo1),
			table_gen__transform_proc(TableIoMethod,
				PredId, ProcId, ProcInfo1, _, PredInfo0, _,
				!ModuleInfo)
		)
	;
		true
	).

%-----------------------------------------------------------------------------%

:- pred should_io_procedure_be_transformed(bool::in, bool::in, hlds_goal::in,
	sym_name::in, bool::out, maybe(table_io_is_unitize)::out) is det.

should_io_procedure_be_transformed(TransformAll, Require, BodyGoal,
		PredModuleName, AnnotationIsMissing, TransformInfo) :-
	tabled_for_io_attributes(BodyGoal, TabledForIoAttrs),
	( TabledForIoAttrs = [] ->
		AnnotationIsMissing = no,
		TransformInfo = no
	; TabledForIoAttrs = [TabledForIoAttr] ->
		(
			TabledForIoAttr = not_tabled_for_io,
			(
				Require = yes,
				\+ any_mercury_builtin_module(PredModuleName)
			->
				AnnotationIsMissing = yes,
				TransformInfo = no
			;
				AnnotationIsMissing = no,
				(
					TransformAll = no,
					TransformInfo = no
				;
					TransformAll = yes,
					may_call_mercury_attributes(BodyGoal,
						MayCallMercuryAttrs),
					(
						MayCallMercuryAttrs =
							[may_call_mercury]
					->
						TransformInfo = no
					;
						TransformInfo =
							yes(table_io_alone)
					)
				)
			)
		;
			TabledForIoAttr = tabled_for_descendant_io,
			AnnotationIsMissing = no,
			% The procedure itself doesn't do any I/O, so don't
			% transform it.
			TransformInfo = no
		;
			TabledForIoAttr = tabled_for_io,
			AnnotationIsMissing = no,
			TransformInfo = yes(table_io_alone)
		;
			TabledForIoAttr = tabled_for_io_unitize,
			AnnotationIsMissing = no,
			TransformInfo = yes(table_io_unitize)
		)
	;
		% Since table_gen is run before inlining, each procedure
		% should contain at most one foreign_proc goal.
		error("should_io_procedure_be_transformed: different tabled_for_io attributes in one procedure")
	).

:- pred may_call_mercury_attributes(hlds_goal::in, list(may_call_mercury)::out)
	is det.

may_call_mercury_attributes(Goal, MayCallMercuryAttrs) :-
	solutions(subgoal_may_call_mercury_attribute(Goal),
		MayCallMercuryAttrs).

:- pred subgoal_may_call_mercury_attribute(hlds_goal::in,
	may_call_mercury::out) is nondet.

subgoal_may_call_mercury_attribute(Goal, MayCallMercuryAttr) :-
	some [SubGoal,Attrs] (
		goal_contains_goal(Goal, SubGoal),
		SubGoal = foreign_proc(Attrs, _,_,_,_,_,_) - _,
		MayCallMercuryAttr = may_call_mercury(Attrs)
	).

:- pred tabled_for_io_attributes(hlds_goal::in, list(tabled_for_io)::out)
	is det.

tabled_for_io_attributes(Goal, TabledForIoAttrs) :-
	solutions(subgoal_tabled_for_io_attribute(Goal), TabledForIoAttrs).

:- pred subgoal_tabled_for_io_attribute(hlds_goal::in, tabled_for_io::out)
	is nondet.

subgoal_tabled_for_io_attribute(Goal, TabledForIoAttr) :-
	some [SubGoal,Attrs] (
		goal_contains_goal(Goal, SubGoal),
		SubGoal = foreign_proc(Attrs, _,_,_,_,_,_) - _,
		TabledForIoAttr = tabled_for_io(Attrs),
		\+ TabledForIoAttr = not_tabled_for_io
	).

:- pred report_missing_tabled_for_io(module_info::in, pred_info::in,
	pred_id::in, proc_id::in, io__state::di, io__state::uo) is det.

report_missing_tabled_for_io(ModuleInfo, PredInfo, PredId, ProcId) -->
	{ pred_info_context(PredInfo, Context) },
	{ describe_one_proc_name(ModuleInfo, proc(PredId, ProcId), Name) },
	{ Msg = [fixed(Name), words("contains untabled I/O primitive.")] },
	error_util__write_error_pieces(Context, 0, Msg).

%-----------------------------------------------------------------------------%

:- pred table_gen__transform_proc(eval_method::in, pred_id::in, proc_id::in,
	proc_info::in, proc_info::out, pred_info::in, pred_info::out,
	module_info::in, module_info::out) is det.

table_gen__transform_proc(EvalMethod, PredId, ProcId, !ProcInfo, !PredInfo,
		!ModuleInfo) :-
	table_info_init(!.ModuleInfo, PredId, ProcId, !.PredInfo, !.ProcInfo,
		TableInfo0),

	% grab the appropriate fields from the pred_info and proc_info
	proc_info_interface_determinism(!.ProcInfo, Detism),
	determinism_to_code_model(Detism, CodeModel),
	proc_info_headvars(!.ProcInfo, HeadVars),
	proc_info_varset(!.ProcInfo, VarSet0),
	proc_info_vartypes(!.ProcInfo, VarTypes0),
	proc_info_goal(!.ProcInfo, OrigGoal),
	proc_info_argmodes(!.ProcInfo, ArgModes),

	( EvalMethod = eval_table_io(Decl, Unitize) ->
		module_info_globals(!.ModuleInfo, Globals),
		globals__lookup_bool_option(Globals, trace_table_io_states,
			TableIoStates),
		table_gen__create_new_io_goal(OrigGoal, Decl, Unitize,
			TableIoStates, HeadVars, ArgModes, VarTypes0, VarTypes,
			VarSet0, VarSet, TableInfo0, TableInfo, Goal,
			MaybeProcTableInfo),
		MaybeCallTableTip = no
	;
		(
			CodeModel = model_det,
			table_gen__create_new_det_goal(EvalMethod, Detism,
				OrigGoal, PredId, ProcId, HeadVars, ArgModes,
				VarTypes0, VarTypes, VarSet0, VarSet,
				TableInfo0, TableInfo, CallTableTip, Goal,
				ProcTableInfo)
		;
			CodeModel = model_semi,
			table_gen__create_new_semi_goal(EvalMethod, Detism,
				OrigGoal, PredId, ProcId, HeadVars, ArgModes,
				VarTypes0, VarTypes, VarSet0, VarSet,
				TableInfo0, TableInfo, CallTableTip, Goal,
				ProcTableInfo)
		;
			CodeModel = model_non,
			table_gen__create_new_non_goal(EvalMethod, Detism,
				OrigGoal, PredId, ProcId, HeadVars, ArgModes,
				VarTypes0, VarTypes, VarSet0, VarSet,
				TableInfo0, TableInfo, CallTableTip, Goal,
				ProcTableInfo)
		),
		MaybeCallTableTip = yes(CallTableTip),
		MaybeProcTableInfo = yes(ProcTableInfo)
	),

	table_info_extract(TableInfo, !:ModuleInfo, _, _,
		!:PredInfo, !:ProcInfo),

	% set the new values of the fields in proc_info and pred_info
	% and save in the module info
	proc_info_set_goal(Goal, !ProcInfo),
	proc_info_set_varset(VarSet, !ProcInfo),
	proc_info_set_vartypes(VarTypes, !ProcInfo),
	proc_info_set_call_table_tip(MaybeCallTableTip, !ProcInfo),
	proc_info_set_maybe_proc_table_info(MaybeProcTableInfo, !ProcInfo),

	% Some of the instmap_deltas generated in this module
	% are pretty dodgy (especially those for if-then-elses), so
	% recompute them here.

	RecomputeAtomic = no,
	recompute_instmap_delta_proc(RecomputeAtomic, !ProcInfo,
		!ModuleInfo),

	pred_info_procedures(!.PredInfo, ProcTable1),
	map__det_update(ProcTable1, ProcId, !.ProcInfo, ProcTable),
	pred_info_set_procedures(ProcTable, !PredInfo),

	%
	% The transformation doesn't pay attention to the purity
	% of compound goals, so recompute the purity here.
	%
	repuritycheck_proc(!.ModuleInfo, proc(PredId, ProcId), !PredInfo),
	module_info_preds(!.ModuleInfo, PredTable1),
	map__det_update(PredTable1, PredId, !.PredInfo, PredTable),
	module_info_set_preds(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

		%
		% Transform procedures that do I/O.
		%

:- pred table_gen__create_new_io_goal(hlds_goal::in, table_io_is_decl::in,
	table_io_is_unitize::in, bool::in, list(prog_var)::in, list(mode)::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	hlds_goal::out, maybe(proc_table_info)::out) is det.

table_gen__create_new_io_goal(OrigGoal, TableDecl, Unitize, TableIoStates,
		HeadVars, HeadVarModes, !VarTypes, !VarSet, !TableInfo,
		Goal, MaybeProcTableInfo) :-
	OrigGoal = _ - OrigGoalInfo,
	goal_info_get_nonlocals(OrigGoalInfo, OrigNonLocals),
	goal_info_get_context(OrigGoalInfo, Context),
	ModuleInfo = !.TableInfo ^ table_module_info,
	get_input_output_vars(HeadVars, HeadVarModes, ModuleInfo, InputVars,
		OutputVars),
	(
		TableIoStates = yes,
		IoStateAssignToVars = [],
		IoStateAssignFromVars = [],
		SavedOutputVars = OutputVars,
		SavedHeadVars = HeadVars
	;
		TableIoStates = no,
		list__filter(table_gen__var_is_io_state(!.VarTypes),
			OutputVars, IoStateAssignToVars, SavedOutputVars),
		list__filter(table_gen__var_is_io_state(!.VarTypes),
			InputVars, IoStateAssignFromVars, _SavedInputVars),
		list__filter(table_gen__var_is_io_state(!.VarTypes),
			HeadVars, _, SavedHeadVars)
	),
	generate_new_table_var("TableVar", trie_node_type, !VarTypes, !VarSet,
		TableVar),
	generate_new_table_var("CounterVar", int_type, !VarTypes, !VarSet,
		CounterVar),
	generate_new_table_var("StartVar", int_type, !VarTypes, !VarSet,
		StartVar),
	generate_call("table_io_in_range", [TableVar, CounterVar, StartVar],
		semidet, yes(impure), [TableVar - ground(shared, none),
		CounterVar - ground(shared, none),
		StartVar - ground(shared, none)],
		ModuleInfo, Context, InRangeGoal),
	generate_new_table_var("TipVar", trie_node_type, !VarTypes, !VarSet,
		TipVar),
	generate_call("table_lookup_insert_start_int",
		[TableVar, StartVar, CounterVar, TipVar],
		det, yes(impure), [TipVar - ground(unique, none)],
		ModuleInfo, Context, LookupGoal),
	generate_call("table_io_has_occurred", [TipVar], semidet,
		yes(semipure), [], ModuleInfo, Context, OccurredGoal),
	(
		TableDecl = table_io_decl,
		PredId = !.TableInfo ^ table_cur_pred_id,
		ProcId = !.TableInfo ^ table_cur_proc_id,
		RttiProcLabel = rtti__make_rtti_proc_label(ModuleInfo,
			PredId, ProcId),
		TableIoDeclConsId = table_io_decl(RttiProcLabel),
		make_const_construction(TableIoDeclConsId, c_pointer_type,
			yes("TableIoDeclPtr"), TableIoDeclGoal,
			TableIoDeclPtrVar, !VarTypes, !VarSet),
		allocate_slot_numbers(SavedHeadVars, 1, NumberedSavedHeadVars),
		NumberedSaveVars = [TableIoDeclPtrVar - 0 |
			NumberedSavedHeadVars],
		list__filter(key_belong_to_list(SavedOutputVars),
			NumberedSaveVars, NumberedSavedOutputVars),
		NumberedRestoreVars = NumberedSavedOutputVars,

		ProcInfo0 = !.TableInfo ^ table_cur_proc_info,
		continuation_info__generate_table_arg_type_info(ProcInfo0,
			NumberedSavedHeadVars, TableArgTypeInfo),
		ProcTableInfo = table_io_decl_info(TableArgTypeInfo),
		MaybeProcTableInfo = yes(ProcTableInfo)
	;
		TableDecl = table_io_proc,
		true_goal(TableIoDeclGoal),
		allocate_slot_numbers(SavedOutputVars, 0,
			NumberedSavedOutputVars),
		NumberedRestoreVars = NumberedSavedOutputVars,
		NumberedSaveVars = NumberedSavedOutputVars,
		MaybeProcTableInfo = no
	),
	list__length(NumberedSaveVars, BlockSize),
	generate_simple_restore_goal(NumberedRestoreVars, TipVar,
		ModuleInfo, Context, !VarTypes, !VarSet, RestoreAnsGoal0),
	(
		TableIoStates = yes,
		RestoreAnsGoal = RestoreAnsGoal0
	;
		TableIoStates = no,
		(
			IoStateAssignFromVars = [IoStateAssignFromVarPrime],
			IoStateAssignToVars = [IoStateAssignToVarPrime]
		->
			IoStateAssignFromVar = IoStateAssignFromVarPrime,
			IoStateAssignToVar = IoStateAssignToVarPrime
		;
			% The call to proc_info_has_io_state_pair in 
			% table_gen__process_procs should ensure that we
			% never get here.
			error("create_new_io_goal: one in / one out violation")
		),
		generate_call("table_io_copy_io_state",
			[IoStateAssignFromVar, IoStateAssignToVar], det, no,
			[IoStateAssignFromVar - ground(clobbered, none),
			IoStateAssignToVar - ground(unique, none)],
			ModuleInfo, Context, IoStateAssignGoal),
		RestoreAnsGoalEx = conj([RestoreAnsGoal0, IoStateAssignGoal]),
		create_instmap_delta([RestoreAnsGoal0, IoStateAssignGoal],
			RestoreAnsInstMapDelta0),
		RestoreAnsGoal0 = _ - RestoreAnsGoal0Info,
		goal_info_get_nonlocals(RestoreAnsGoal0Info,
			RestoreAns0NonLocals),
		set__insert_list(RestoreAns0NonLocals,
			[IoStateAssignFromVar, IoStateAssignToVar],
			RestoreAnsNonLocals),
		instmap_delta_restrict(RestoreAnsInstMapDelta0,
			RestoreAnsNonLocals, RestoreAnsInstMapDelta),
		goal_info_init(RestoreAnsNonLocals, RestoreAnsInstMapDelta,
			det, semipure, Context, RestoreAnsGoalInfo0),
		goal_info_add_feature(RestoreAnsGoalInfo0, hide_debug_event,
			RestoreAnsGoalInfo),
		RestoreAnsGoal = RestoreAnsGoalEx - RestoreAnsGoalInfo
	),
	generate_save_goal(NumberedSaveVars, TipVar, BlockSize,
		Context, !VarTypes, !VarSet, !TableInfo, SaveAnsGoal),
	(
		Unitize = table_io_alone,
		CallSaveAnsGoalList = [OrigGoal, TableIoDeclGoal, SaveAnsGoal]
	;
		Unitize = table_io_unitize,
		generate_new_table_var("SavedTraceEnabled", int_type,
			!VarTypes, !VarSet, SavedTraceEnabledVar),
		generate_call("table_io_left_bracket_unitized_goal",
			[SavedTraceEnabledVar], det, yes(impure),
			[SavedTraceEnabledVar - ground(unique, none)],
			ModuleInfo, Context, LeftBracketGoal),
		generate_call("table_io_right_bracket_unitized_goal",
			[SavedTraceEnabledVar], det, yes(impure), [],
			ModuleInfo, Context, RightBracketGoal),
		CallSaveAnsGoalList = [LeftBracketGoal, OrigGoal,
			RightBracketGoal, TableIoDeclGoal, SaveAnsGoal]
	),
	CallSaveAnsGoalEx = conj(CallSaveAnsGoalList),
	create_instmap_delta(CallSaveAnsGoalList, CallSaveAnsInstMapDelta0),
	set__insert(OrigNonLocals, TipVar, CallSaveAnsNonLocals),
	instmap_delta_restrict(CallSaveAnsInstMapDelta0,
		CallSaveAnsNonLocals, CallSaveAnsInstMapDelta),
	goal_info_init(CallSaveAnsNonLocals, CallSaveAnsInstMapDelta, det,
		impure, Context, CallSaveAnsGoalInfo0),
	goal_info_add_feature(CallSaveAnsGoalInfo0, hide_debug_event,
		CallSaveAnsGoalInfo),
	CallSaveAnsGoal = CallSaveAnsGoalEx - CallSaveAnsGoalInfo,

	GenIfNecGoalEx = if_then_else([], OccurredGoal,
		RestoreAnsGoal, CallSaveAnsGoal),
	create_instmap_delta([OccurredGoal, RestoreAnsGoal,
		CallSaveAnsGoal], GenIfNecInstMapDelta0),
	set__insert(OrigNonLocals, TipVar, GenIfNecNonLocals),
	instmap_delta_restrict(GenIfNecInstMapDelta0, GenIfNecNonLocals,
		GenIfNecInstMapDelta),
	goal_info_init(GenIfNecNonLocals, GenIfNecInstMapDelta, det, impure,
		Context, GenIfNecGoalInfo0),
	goal_info_add_feature(GenIfNecGoalInfo0, hide_debug_event,
		GenIfNecGoalInfo),
	GenIfNecGoal = GenIfNecGoalEx - GenIfNecGoalInfo,

	CheckAndGenAnsGoalEx = conj([LookupGoal, GenIfNecGoal]),
	create_instmap_delta([LookupGoal, GenIfNecGoal],
		CheckAndGenAnsInstMapDelta0),
	set__insert_list(OrigNonLocals, [TableVar, CounterVar, StartVar],
		CheckAndGenAnsNonLocals),
	instmap_delta_restrict(CheckAndGenAnsInstMapDelta0,
		CheckAndGenAnsNonLocals, CheckAndGenAnsInstMapDelta),
	goal_info_init(CheckAndGenAnsNonLocals, CheckAndGenAnsInstMapDelta,
		det, impure, Context, CheckAndGenAnsGoalInfo0),
	goal_info_add_feature(CheckAndGenAnsGoalInfo0, hide_debug_event,
		CheckAndGenAnsGoalInfo),
	CheckAndGenAnsGoal = CheckAndGenAnsGoalEx - CheckAndGenAnsGoalInfo,

	BodyGoalEx = if_then_else([], InRangeGoal, CheckAndGenAnsGoal,
		OrigGoal),
	create_instmap_delta([InRangeGoal, CheckAndGenAnsGoal, OrigGoal],
		BodyInstMapDelta0),
	instmap_delta_restrict(BodyInstMapDelta0, OrigNonLocals,
		BodyInstMapDelta),
	goal_info_init(OrigNonLocals, BodyInstMapDelta, det, impure,
		Context, BodyGoalInfo0),
	goal_info_add_feature(BodyGoalInfo0, hide_debug_event, BodyGoalInfo),
	Goal = BodyGoalEx - BodyGoalInfo.

		%
		% Transform deterministic procedures.
		%
:- pred table_gen__create_new_det_goal(eval_method::in, determinism::in,
	hlds_goal::in, pred_id::in, proc_id::in, list(prog_var)::in,
	list(mode)::in, map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	prog_var::out, hlds_goal::out, proc_table_info::out) is det.

table_gen__create_new_det_goal(EvalMethod, Detism, OrigGoal, PredId, ProcId,
		HeadVars, HeadVarModes, !VarTypes, !VarSet, !TableInfo,
		TableTipVar, Goal, ProcTableInfo) :-
	% even if the original goal doesn't use all of the headvars,
	% the code generated by the tabling transformation does,
	% so we need to compute the nonlocals from the headvars rather
	% than getting it from the nonlocals field in the original goal
	set__list_to_set(HeadVars, OrigNonLocals),
	OrigGoal = _ - OrigGoalInfo,
	goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
	goal_info_get_context(OrigGoalInfo, Context),

	ModuleInfo = !.TableInfo ^ table_module_info,

	get_input_output_vars(HeadVars, HeadVarModes, ModuleInfo, InputVars,
		OutputVars),

	generate_simple_call_table_lookup_goal(InputVars, PredId, ProcId,
		Context, !VarTypes, !VarSet, !TableInfo, TableTipVar,
		LookUpGoal, Steps),
	generate_call("table_simple_is_complete", [TableTipVar], semidet,
		yes(semipure), [], ModuleInfo, Context, CompleteCheckGoal),
	allocate_slot_numbers(OutputVars, 0, NumberedOutputVars),
	list__length(NumberedOutputVars, BlockSize),
	generate_save_goal(NumberedOutputVars, TableTipVar, BlockSize,
		Context, !VarTypes, !VarSet, !TableInfo, SaveAnsGoal0),
	generate_simple_restore_goal(NumberedOutputVars, TableTipVar,
		ModuleInfo, Context, !VarTypes, !VarSet, RestoreAnsGoal0),
	generate_call("table_simple_mark_as_inactive", [TableTipVar], det,
		yes(impure), [], ModuleInfo, Context, MarkAsInactiveGoal),
	generate_loop_error_goal(!.TableInfo, Context, infinite_recursion_msg,
		!VarTypes, !VarSet, LoopErrorGoal),
	( Detism = erroneous ->
		% In this case, the RestoreAnsGoal will never be reached,
		% but to ensure that the generated code is determinism-correct,
		% we need to make sure that RestoreAnsGoal's determinism is
		% erroneous.  So we use the LoopErrorGoal (which calls error/1)
		% rather than RestoreAnsGoal0.
		RestoreAnsGoal = LoopErrorGoal
	;
		RestoreAnsGoal = RestoreAnsGoal0
	),

	set__insert(OrigNonLocals, TableTipVar, GenAnsNonLocals),

	( EvalMethod = eval_loop_check ->
		SaveAnsGoal = MarkAsInactiveGoal
	; EvalMethod = eval_memo ->
		SaveAnsGoal = SaveAnsGoal0
	;
		error("table_gen__create_new_det_goal: " ++
			"unsupported evaluation model")
	),

	generate_call("table_simple_is_active", [TableTipVar], semidet,
		yes(semipure), [], ModuleInfo, Context, ActiveCheckGoal),
	generate_call("table_simple_mark_as_active", [TableTipVar], det,
		yes(impure), [], ModuleInfo, Context, MarkAsActiveGoal),

	NoLoopGenAnsGoalEx = conj([MarkAsActiveGoal, OrigGoal, SaveAnsGoal]),
	create_instmap_delta([MarkAsActiveGoal, OrigGoal, SaveAnsGoal],
		NoLoopGenInstMapDelta0),
	instmap_delta_restrict(NoLoopGenInstMapDelta0, GenAnsNonLocals,
		NoLoopGenInstMapDelta),
	goal_info_init(GenAnsNonLocals, NoLoopGenInstMapDelta, det, impure,
		Context, NoLoopGenGoalInfo0),
	goal_info_add_feature(NoLoopGenGoalInfo0, hide_debug_event,
		NoLoopGenGoalInfo),
	NoLoopGenAnsGoal = NoLoopGenAnsGoalEx - NoLoopGenGoalInfo,

	GenAnsGoalEx = if_then_else([], ActiveCheckGoal,
		LoopErrorGoal, NoLoopGenAnsGoal),
	create_instmap_delta([ActiveCheckGoal, LoopErrorGoal,
		NoLoopGenAnsGoal], GenAnsInstMapDelta0),
	instmap_delta_restrict(GenAnsInstMapDelta0, GenAnsNonLocals,
		GenAnsInstMapDelta),
	goal_info_init(GenAnsNonLocals, GenAnsInstMapDelta, det, impure,
		Context, GenAnsGoalInfo0),
	goal_info_add_feature(GenAnsGoalInfo0, hide_debug_event,
		GenAnsGoalInfo),
	GenAnsGoal = GenAnsGoalEx - GenAnsGoalInfo,

	ITEGoalEx = if_then_else([], CompleteCheckGoal, RestoreAnsGoal,
		GenAnsGoal),
	create_instmap_delta([CompleteCheckGoal, RestoreAnsGoal, GenAnsGoal],
		ITEInstMapDelta0),
	instmap_delta_restrict(ITEInstMapDelta0, GenAnsNonLocals,
		ITEInstMapDelta),
	goal_info_init(GenAnsNonLocals, ITEInstMapDelta, det, impure,
		Context, ITEGoalInfo0),
	goal_info_add_feature(ITEGoalInfo0, hide_debug_event, ITEGoalInfo),
	ITEGoal = ITEGoalEx - ITEGoalInfo,

	GoalEx = conj([LookUpGoal, ITEGoal]),
	goal_info_init(OrigNonLocals, OrigInstMapDelta, det, impure,
		Context, GoalInfo0),
	goal_info_add_feature(GoalInfo0, hide_debug_event, GoalInfo),
	Goal = GoalEx - GoalInfo,
	generate_gen_proc_table_info(!.TableInfo, Steps, InputVars, OutputVars,
		ProcTableInfo).

%-----------------------------------------------------------------------------%

		%
		% Transform semi deterministic procedures
		%
:- pred table_gen__create_new_semi_goal(eval_method::in, determinism::in,
	hlds_goal::in, pred_id::in, proc_id::in, list(prog_var)::in,
	list(mode)::in, map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	prog_var::out, hlds_goal::out, proc_table_info::out) is det.

table_gen__create_new_semi_goal(EvalMethod, Detism, OrigGoal, PredId, ProcId,
		HeadVars, HeadVarModes, !VarTypes, !VarSet, !TableInfo,
		TableTipVar, Goal, ProcTableInfo) :-
	% even if the original goal doesn't use all of the headvars,
	% the code generated by the tabling transformation does,
	% so we need to compute the nonlocals from the headvars rather
	% than getting it from the nonlocals field in the original goal
	set__list_to_set(HeadVars, OrigNonLocals),
	OrigGoal = _ - OrigGoalInfo,
	goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
	goal_info_get_context(OrigGoalInfo, Context),

	ModuleInfo = !.TableInfo ^ table_module_info,
	get_input_output_vars(HeadVars, HeadVarModes, ModuleInfo, InputVars,
		OutputVars),

	generate_simple_call_table_lookup_goal(InputVars, PredId, ProcId,
		Context, !VarTypes, !VarSet, !TableInfo, TableTipVar,
		LookUpGoal, Steps),
	generate_call("table_simple_is_complete", [TableTipVar], semidet,
		yes(semipure), [], ModuleInfo, Context, CompleteCheckGoal),
	allocate_slot_numbers(OutputVars, 0, NumberedOutputVars),
	list__length(NumberedOutputVars, BlockSize),
	generate_save_goal(NumberedOutputVars, TableTipVar, BlockSize,
		Context, !VarTypes, !VarSet, !TableInfo, SaveAnsGoal0),
	generate_simple_restore_goal(NumberedOutputVars, TableTipVar,
		ModuleInfo, Context, !VarTypes, !VarSet, RestoreTrueAnsGoal),
	generate_loop_error_goal(!.TableInfo, Context, infinite_recursion_msg,
		!VarTypes, !VarSet, LoopErrorGoal),
	generate_call("table_simple_mark_as_failed", [TableTipVar],
		det, yes(impure), [], ModuleInfo, Context, MarkAsFailedGoal0),
	append_fail(MarkAsFailedGoal0, MarkAsFailedGoal),
	generate_call("table_simple_has_succeeded", [TableTipVar], semidet,
		yes(semipure), [], ModuleInfo, Context, HasSucceededCheckGoal),
	generate_call("table_simple_mark_as_inactive", [TableTipVar],
		det, yes(impure), [], ModuleInfo, Context, MarkAsInactiveGoal),

	set__insert(OrigNonLocals, TableTipVar, GenAnsNonLocals),

	(
		(
			EvalMethod = eval_loop_check
		;
			EvalMethod = eval_memo
		)
	->
		( EvalMethod = eval_loop_check ->
			SaveAnsGoal = MarkAsInactiveGoal
		;
			SaveAnsGoal = SaveAnsGoal0
		),
		generate_call("table_simple_is_active", [TableTipVar],
			semidet, yes(semipure), [], ModuleInfo, Context,
			ActiveCheckGoal),
		generate_call("table_simple_mark_as_active", [TableTipVar],
			det, yes(impure), [], ModuleInfo, Context,
			MarkAsActiveGoal),

		NoLoopGenAnsGoalEx = conj([MarkAsActiveGoal, OrigGoal]),
		create_instmap_delta([MarkAsActiveGoal, OrigGoal],
			NoLoopGenInstMapDelta0),
		instmap_delta_restrict(NoLoopGenInstMapDelta0, GenAnsNonLocals,
			NoLoopGenInstMapDelta),
		goal_info_init(GenAnsNonLocals, NoLoopGenInstMapDelta, semidet,
			impure, Context, NoLoopGenGoalInfo0),
		goal_info_add_feature(NoLoopGenGoalInfo0, hide_debug_event,
			NoLoopGenGoalInfo),
		NoLoopGenAnsGoal = NoLoopGenAnsGoalEx - NoLoopGenGoalInfo,

		GenTrueAnsGoalEx = if_then_else([], ActiveCheckGoal,
			LoopErrorGoal, NoLoopGenAnsGoal),
		create_instmap_delta([ActiveCheckGoal, LoopErrorGoal,
			NoLoopGenAnsGoal], GenTrueAnsInstMapDelta0),
		instmap_delta_restrict(GenTrueAnsInstMapDelta0,
			GenAnsNonLocals, GenTrueAnsInstMapDelta),
		goal_info_init(GenAnsNonLocals, GenTrueAnsInstMapDelta,
			semidet, impure, Context, GenTrueAnsGoalInfo0),
		goal_info_add_feature(GenTrueAnsGoalInfo0, hide_debug_event,
			GenTrueAnsGoalInfo),
		GenTrueAnsGoal = GenTrueAnsGoalEx - GenTrueAnsGoalInfo
	;
		EvalMethod = eval_minimal
	->
		SaveAnsGoal = SaveAnsGoal0,
		generate_call("table_simple_is_inactive", [TableTipVar],
			semidet, yes(semipure), [], ModuleInfo, Context,
			InactiveCheckGoal),
		generate_call("table_simple_mark_as_active", [TableTipVar],
			det, yes(impure), [], ModuleInfo, Context,
			MarkAsActiveGoal),
		GenTrueAnsGoalEx = conj([InactiveCheckGoal,
			MarkAsActiveGoal, OrigGoal]),

		create_instmap_delta([InactiveCheckGoal, MarkAsActiveGoal,
			OrigGoal, SaveAnsGoal], GenTrueAnsInstMapDelta0),
		instmap_delta_restrict(GenTrueAnsInstMapDelta0,
			GenAnsNonLocals, GenTrueAnsInstMapDelta),
		goal_info_init(GenAnsNonLocals, GenTrueAnsInstMapDelta,
			semidet, impure, Context, GenTrueAnsGoalInfo0),
		goal_info_add_feature(GenTrueAnsGoalInfo0, hide_debug_event,
			GenTrueAnsGoalInfo),
		GenTrueAnsGoal = GenTrueAnsGoalEx - GenTrueAnsGoalInfo
	;
		error("table_gen__create_new_semi_goal: " ++
			"unsupported evaluation model")
	),

	( Detism = failure ->
		% We need to treat this case specially,
		% otherwise the code that we generate would not
		% be statically determinism-correct: there would
		% be a path through the procedure which determinism
		% analysis thinks might succeed.  In fact that path
		% would never be executed, but if this case were not
		% treated specially, the determinism analysis algorithm
		% could not prove that.  So to ensure that the generated
		% code is determinism-correct, we replace the bit which
		% retrieves an answer from the table with an
		% explicit `fail'.
		fail_goal(Context, RestoreAnsGoal)
	;
		RestAnsGoalEx =
			conj([HasSucceededCheckGoal, RestoreTrueAnsGoal]),
		set__singleton_set(RestNonLocals0, TableTipVar),
		set__insert_list(RestNonLocals0, OutputVars, RestNonLocals),
		create_instmap_delta([HasSucceededCheckGoal,
			RestoreTrueAnsGoal], RestInstMapDelta0),
		instmap_delta_restrict(RestInstMapDelta0, RestNonLocals,
			RestInstMapDelta),
		goal_info_init(RestNonLocals, RestInstMapDelta, semidet,
			semipure, Context, RestAnsGoalInfo0),
		goal_info_add_feature(RestAnsGoalInfo0, hide_debug_event,
			RestAnsGoalInfo),
		RestoreAnsGoal = RestAnsGoalEx - RestAnsGoalInfo
	),

	GenAnsGoalEx = if_then_else([], GenTrueAnsGoal, SaveAnsGoal,
		MarkAsFailedGoal),
	create_instmap_delta([GenTrueAnsGoal, SaveAnsGoal, MarkAsFailedGoal],
		GenAnsGoalInstMapDelta0),
	instmap_delta_restrict(GenAnsGoalInstMapDelta0, GenAnsNonLocals,
		GenAnsGoalInstMapDelta),
	goal_info_init(GenAnsNonLocals, GenAnsGoalInstMapDelta, semidet,
		impure, Context, GenAnsGoalInfo0),
	goal_info_add_feature(GenAnsGoalInfo0, hide_debug_event,
		GenAnsGoalInfo),
	GenAnsGoal = GenAnsGoalEx - GenAnsGoalInfo,

	ITEGoalEx = if_then_else([], CompleteCheckGoal, RestoreAnsGoal,
		GenAnsGoal),
	create_instmap_delta([CompleteCheckGoal, RestoreAnsGoal, GenAnsGoal],
		ITEInstMapDelta0),
	instmap_delta_restrict(ITEInstMapDelta0, GenAnsNonLocals,
		ITEInstMapDelta),
	goal_info_init(GenAnsNonLocals, ITEInstMapDelta, semidet, impure,
		Context, ITEGoalInfo0),
	goal_info_add_feature(ITEGoalInfo0, hide_debug_event, ITEGoalInfo),
	ITEGoal = ITEGoalEx - ITEGoalInfo,

	GoalEx = conj([LookUpGoal, ITEGoal]),
	goal_info_init(OrigNonLocals, OrigInstMapDelta, semidet, impure,
		Context, GoalInfo0),
	goal_info_add_feature(GoalInfo0, hide_debug_event, GoalInfo),
	Goal = GoalEx - GoalInfo,
	generate_gen_proc_table_info(!.TableInfo, Steps, InputVars, OutputVars,
		ProcTableInfo).

%-----------------------------------------------------------------------------%

		%
		% Transform non deterministic procedures
		%
:- pred table_gen__create_new_non_goal(eval_method::in, determinism::in,
	hlds_goal::in, pred_id::in, proc_id::in, list(prog_var)::in,
	list(mode)::in, map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	prog_var::out, hlds_goal::out, proc_table_info::out) is det.

table_gen__create_new_non_goal(EvalMethod, Detism, OrigGoal, PredId, ProcId,
		HeadVars, HeadVarModes, !VarTypes, !VarSet, !TableInfo,
		TableTipVar, Goal, ProcTableInfo) :-
	% even if the original goal doesn't use all of the headvars,
	% the code generated by the tabling transformation does,
	% so we need to compute the nonlocals from the headvars rather
	% than getting it from the nonlocals field in the original goal
	set__list_to_set(HeadVars, OrigNonLocals),
	OrigGoal = _ - OrigGoalInfo,
	goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
	goal_info_get_context(OrigGoalInfo, Context),

	ModuleInfo = !.TableInfo ^ table_module_info,
	get_input_output_vars(HeadVars, HeadVarModes, ModuleInfo, InputVars,
		OutputVars),
	allocate_slot_numbers(OutputVars, 0, NumberedOutputVars),
	list__length(NumberedOutputVars, BlockSize),
	generate_non_call_table_lookup_goal(InputVars, PredId, ProcId, Context,
		!VarTypes, !VarSet, !TableInfo, TableTipVar, SubgoalVar,
		LookUpGoal, Steps),
	generate_new_table_var("Status", status_type, !VarTypes, !VarSet,
		StatusVar),
	generate_call("table_subgoal_status", [SubgoalVar, StatusVar], det,
		yes(semipure), [StatusVar - ground(unique, none)],
		ModuleInfo, Context, StatusGoal),
	generate_non_save_goal(NumberedOutputVars, SubgoalVar, BlockSize,
		Context, !VarTypes, !VarSet, !TableInfo, SaveAnsGoal),
	generate_restore_all_goal(Detism, NumberedOutputVars, SubgoalVar,
		ModuleInfo, Context, !VarTypes, !VarSet, RestoreAllAnsGoal),
	generate_loop_error_goal(!.TableInfo, Context, infinite_recursion_msg,
		!VarTypes, !VarSet, LoopErrorGoal),
	generate_call("table_nondet_mark_as_active", [SubgoalVar], det,
		yes(impure), [], ModuleInfo, Context, MarkAsActiveGoal),
	true_goal(TrueGoal),
	(
		EvalMethod = eval_memo,
		AfterOrigGoal = SaveAnsGoal,
		ActiveGoal = LoopErrorGoal,
		CompleteGoal = RestoreAllAnsGoal
	;
		EvalMethod = eval_loop_check,
		generate_loop_error_goal(!.TableInfo, Context,
			"detected internal error", !VarTypes, !VarSet,
			LoopInternalErrorGoal),
		AfterOrigGoal = TrueGoal,
		ActiveGoal = LoopErrorGoal,
		CompleteGoal = LoopInternalErrorGoal
	;
		EvalMethod = eval_minimal,
		generate_suspend_goal(NumberedOutputVars, SubgoalVar,
			ModuleInfo, Context, !VarTypes, !VarSet, SuspendGoal),
		AfterOrigGoal = SaveAnsGoal,
		ActiveGoal = SuspendGoal,
		CompleteGoal = RestoreAllAnsGoal
	;
		EvalMethod = eval_table_io(_, _),
		error("table_gen__create_new_non_goal: table_io")
	;
		EvalMethod = eval_normal,
		error("table_gen__create_new_non_goal: normal")
	),
	MainEx = conj([MarkAsActiveGoal, OrigGoal, AfterOrigGoal]),
	set__insert(OrigNonLocals, SubgoalVar, MainNonLocals),
	create_instmap_delta([MarkAsActiveGoal, OrigGoal, SaveAnsGoal],
		MainIMD0),
	instmap_delta_restrict(MainIMD0, MainNonLocals, MainIMD),
	goal_info_init(MainNonLocals, MainIMD, nondet, impure, Context,
		MainGoalInfo0),
	goal_info_add_feature(MainGoalInfo0, hide_debug_event, MainGoalInfo),
	MainGoal = MainEx - MainGoalInfo,
	( EvalMethod = eval_minimal ->
		generate_call("table_nondet_resume", [SubgoalVar], det,
			yes(impure), [], ModuleInfo, Context, ResumeGoal0),
		append_fail(ResumeGoal0, ResumeGoal),
		InactiveEx = disj([MainGoal, ResumeGoal]),
		InactiveGoal = InactiveEx - MainGoalInfo
	;
		InactiveGoal = MainGoal
	),

	mercury_table_builtin_module(TB),
	SwitchArms = [
		case(cons(qualified(TB, "inactive"), 0), InactiveGoal),
		case(cons(qualified(TB, "complete"), 0), CompleteGoal),
		case(cons(qualified(TB, "active"), 0), ActiveGoal)],
	SwitchEx = switch(StatusVar, cannot_fail, SwitchArms),
	goal_info_add_feature(MainGoalInfo, hide_debug_event, SwitchGoalInfo),
	SwitchGoal = SwitchEx - SwitchGoalInfo,

	GoalEx = conj([LookUpGoal, StatusGoal, SwitchGoal]),
	goal_info_init(OrigNonLocals, OrigInstMapDelta, nondet, impure,
		Context, GoalInfo0),
	goal_info_add_feature(GoalInfo0, hide_debug_event, GoalInfo),
	Goal = GoalEx - GoalInfo,
	generate_gen_proc_table_info(!.TableInfo, Steps, InputVars, OutputVars,
		ProcTableInfo).

%-----------------------------------------------------------------------------%

:- pred table_gen__var_is_io_state(map(prog_var, type)::in, prog_var::in)
	is semidet.

table_gen__var_is_io_state(VarTypes, Var) :-
	map__lookup(VarTypes, Var, VarType),
	type_util__type_is_io_state(VarType).

%-----------------------------------------------------------------------------%

:- pred generate_gen_proc_table_info(table_info::in, list(table_trie_step)::in,
	list(prog_var)::in, list(prog_var)::in, proc_table_info::out) is det.

generate_gen_proc_table_info(TableInfo, Steps, InputVars, OutputVars,
		ProcTableInfo) :-
	ProcInfo = TableInfo ^ table_cur_proc_info,
	list__append(InputVars, OutputVars, InOutHeadVars),
	allocate_slot_numbers(InOutHeadVars, 1, NumberedInOutHeadVars),
	continuation_info__generate_table_arg_type_info(ProcInfo,
		NumberedInOutHeadVars, TableArgTypeInfo),
	NumInputs = list__length(InputVars),
	NumOutputs = list__length(OutputVars),
	ProcTableInfo = table_gen_info(NumInputs, NumOutputs, Steps,
		TableArgTypeInfo).

%-----------------------------------------------------------------------------%

:- pred generate_get_table_goal(pred_id::in, proc_id::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out,
	prog_var::out, hlds_goal::out) is det.

generate_get_table_goal(PredId, ProcId, !VarTypes, !VarSet, PredTableVar,
		Goal) :-
	generate_new_table_var("PredTable", trie_node_type,
		!VarTypes, !VarSet, PredTableVar),
	ConsId = tabling_pointer_const(PredId, ProcId),
	make_const_construction(PredTableVar, ConsId, GoalExpr - GoalInfo0),
	goal_info_add_feature(GoalInfo0, impure, GoalInfo),
	Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

:- pred generate_simple_call_table_lookup_goal(list(prog_var)::in,
	pred_id::in, proc_id::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	prog_var::out, hlds_goal::out, list(table_trie_step)::out) is det.

generate_simple_call_table_lookup_goal(Vars, PredId, ProcId, Context,
		!VarTypes, !VarSet, !TableInfo, TableTipVar, Goal, Steps) :-
	generate_get_table_goal(PredId, ProcId, !VarTypes, !VarSet,
		PredTableVar, GetTableGoal0),
	(
		Vars = [],
		Steps = [],
		TableTipVar = PredTableVar,
		attach_call_table_tip(GetTableGoal0, GetTableGoal),
		LookupGoals = []
	;
		Vars = [FirstVar | LaterVars],
		GetTableGoal = GetTableGoal0,
		generate_call_table_lookup_goals(FirstVar, LaterVars, Context,
			PredTableVar, TableTipVar, !VarTypes, !VarSet,
			!TableInfo, LookupGoals, Steps)
	),
	GoalExpr = conj([GetTableGoal | LookupGoals]),
	set__singleton_set(NonLocals0, TableTipVar),
	set__insert_list(NonLocals0, Vars, NonLocals),
	instmap_delta_from_assoc_list([TableTipVar - ground(unique, none)],
		InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, impure, Context,
		GoalInfo),
	Goal = GoalExpr - GoalInfo.

:- pred generate_non_call_table_lookup_goal(list(prog_var)::in,
	pred_id::in, proc_id::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	prog_var::out, prog_var::out, hlds_goal::out,
	list(table_trie_step)::out) is det.

generate_non_call_table_lookup_goal(Vars, PredId, ProcId, Context, !VarTypes,
		!VarSet, !TableInfo, TableTipVar, SubgoalVar, Goal, Steps) :-
	generate_simple_call_table_lookup_goal(Vars, PredId, ProcId, Context,
		!VarTypes, !VarSet, !TableInfo, TableTipVar, LookupGoal,
		Steps),
	ModuleInfo = !.TableInfo ^ table_module_info,
	generate_new_table_var("SubgoalVar", subgoal_type, !VarTypes, !VarSet,
		SubgoalVar),
	generate_call("table_nondet_setup", [TableTipVar, SubgoalVar],
		det, yes(impure), [SubgoalVar - ground(unique, none)],
		ModuleInfo, Context, SetupGoal),
	GoalExpr = conj([LookupGoal, SetupGoal]),
	set__singleton_set(NonLocals0, SubgoalVar),
	set__insert_list(NonLocals0, Vars, NonLocals),
	instmap_delta_from_assoc_list([SubgoalVar - ground(unique, none)],
		InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, impure, Context,
		GoalInfo),
	Goal = GoalExpr - GoalInfo.

:- pred generate_call_table_lookup_goals(prog_var::in, list(prog_var)::in,
	term__context::in, prog_var::in, prog_var::out,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	list(hlds_goal)::out, list(table_trie_step)::out) is det.

generate_call_table_lookup_goals(Var, Vars, Context, TableVar0, TableTipVar,
		!VarTypes, !VarSet, !TableInfo, [Goal | RestGoals],
		[Step | Steps]) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	map__lookup(!.VarTypes, Var, VarType),
	classify_type(ModuleInfo, VarType) = TypeCat,
	gen_lookup_call_for_type(TypeCat, VarType, TableVar0, Var, Context,
		!VarTypes, !VarSet, !TableInfo, TableVar1, Goal0, Step),
	(
		Vars = [],
		TableTipVar = TableVar1,
		attach_call_table_tip(Goal0, Goal),
		RestGoals = [],
		Steps = []
	;
		Vars = [FirstVar | LaterVars],
		Goal = Goal0,
		generate_call_table_lookup_goals(FirstVar, LaterVars,
			Context, TableVar1, TableTipVar, !VarTypes, !VarSet,
			!TableInfo, RestGoals, Steps)
	).

:- pred attach_call_table_tip(hlds_goal::in, hlds_goal::out) is det.

attach_call_table_tip(GoalExpr - GoalInfo0, GoalExpr - GoalInfo) :-
	goal_info_get_features(GoalInfo0, Features0),
	set__insert(Features0, call_table_gen, Features),
	goal_info_set_features(GoalInfo0, Features, GoalInfo).

:- pred generate_answer_table_lookup_goals(list(prog_var)::in,
	term__context::in, prog_var::in, prog_var::out,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	list(hlds_goal)::out) is det.

generate_answer_table_lookup_goals([], _Context, TableVar, TableVar,
		!VarTypes, !VarSet, !TableInfo, []).
generate_answer_table_lookup_goals([Var | Vars], Context, TableVar0,
		TableTipVar, !VarTypes, !VarSet, !TableInfo,
		[Goal | RestGoals]) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	map__lookup(!.VarTypes, Var, VarType),
	classify_type(ModuleInfo, VarType) = TypeCat,
	gen_lookup_call_for_type(TypeCat, VarType, TableVar0, Var, Context,
		!VarTypes, !VarSet, !TableInfo, TableVar1, Goal, _),
	generate_answer_table_lookup_goals(Vars, Context,
		TableVar1, TableTipVar, !VarTypes, !VarSet,
		!TableInfo, RestGoals).

:- pred gen_lookup_call_for_type(type_category::in, (type)::in,
	prog_var::in, prog_var::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	prog_var::out, hlds_goal::out, table_trie_step::out) is det.

gen_lookup_call_for_type(TypeCat, Type, TableVar, ArgVar, Context,
		!VarTypes, !VarSet, !TableInfo, NextTableVar, Goal, Step) :-
	ModuleInfo = !.TableInfo ^ table_module_info,

	( TypeCat = enum_type ->
		( type_to_ctor_and_args(Type, TypeCtor, _) ->
			module_info_types(ModuleInfo, TypeDefnTable),
			map__lookup(TypeDefnTable, TypeCtor, TypeDefn),
			hlds_data__get_type_defn_body(TypeDefn, TypeBody),
			(
				Ctors = TypeBody ^ du_type_ctors,
				TypeBody ^ du_type_is_enum = yes,
				TypeBody ^ du_type_usereq = no
			->
				list__length(Ctors, EnumRange)
			;
				error("gen_lookup_call_for_type: " ++
					"enum type is not du_type?")
			),
			gen_int_construction("RangeVar", EnumRange, !VarTypes,
				!VarSet, RangeVar, RangeUnifyGoal),
			generate_new_table_var("TableNodeVar", trie_node_type,
				!VarTypes, !VarSet, NextTableVar),
			generate_call("table_lookup_insert_enum",
				[TableVar, RangeVar, ArgVar, NextTableVar],
				det, yes(impure),
				[NextTableVar - ground(unique, none)],
				ModuleInfo, Context, LookupGoal),
			set__init(NonLocals0),
			set__insert_list(NonLocals0, [TableVar, ArgVar],
				NonLocals),
			instmap_delta_from_assoc_list([], InstMapDelta),
			goal_info_init(NonLocals, InstMapDelta, det, impure,
				Context, GoalInfo),
			Goal = conj([RangeUnifyGoal, LookupGoal]) - GoalInfo,
			Step = table_trie_step_enum(EnumRange)
		;
			error("gen_lookup_call_for_type: unexpected enum type")
		)
	;
		generate_new_table_var("TableNodeVar", trie_node_type,
			!VarTypes, !VarSet, NextTableVar),
		InstMapAL = [NextTableVar - ground(unique, none)],
		lookup_tabling_category(TypeCat, MaybeCatStringStep),
		(
			MaybeCatStringStep = no,
			( type_util__vars(Type, []) ->
				LookupPredName = "table_lookup_insert_user",
				Step = table_trie_step_user(Type)
			;
				LookupPredName = "table_lookup_insert_poly",
				Step = table_trie_step_poly
			),
			make_type_info_var(Type, Context, !VarTypes, !VarSet,
				!TableInfo, TypeInfoVar, ExtraGoals),
			generate_call(LookupPredName,
				[TypeInfoVar, TableVar, ArgVar, NextTableVar],
				det, yes(impure), InstMapAL, ModuleInfo,
				Context, CallGoal),
			list__append(ExtraGoals, [CallGoal], ConjList),
			CallGoal = _ - GoalInfo,
			conj_list_to_goal(ConjList, GoalInfo, Goal)
		;
			MaybeCatStringStep = yes(CatString - Step),
			string__append("table_lookup_insert_", CatString,
				LookupPredName),
			generate_call(LookupPredName,
				[TableVar, ArgVar, NextTableVar],
				det, yes(impure), InstMapAL, ModuleInfo,
				Context, Goal)
		)
	).

%-----------------------------------------------------------------------------%

:- pred generate_save_goal(assoc_list(prog_var, int)::in,
	prog_var::in, int::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	hlds_goal::out) is det.

generate_save_goal(NumberedVars, TableVar, BlockSize, Context,
		!VarTypes, !VarSet, !TableInfo, Goal) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	( BlockSize > 0 ->
		gen_int_construction("BlockSize", BlockSize, !VarTypes,
			!VarSet, BlockSizeVar, BlockSizeVarUnifyGoal),
		generate_new_table_var("AnswerTableVar", answer_block_type,
			!VarTypes, !VarSet, AnsTableVar),
		generate_call("table_create_ans_block",
			[TableVar, BlockSizeVar, AnsTableVar], det,
			yes(impure), [AnsTableVar - ground(unique, none)],
			ModuleInfo, Context, CreateAnsBlockGoal),
		generate_save_goals(NumberedVars, AnsTableVar, Context,
			!VarTypes, !VarSet, !TableInfo, SaveGoals),
		GoalEx = conj([BlockSizeVarUnifyGoal, CreateAnsBlockGoal |
			SaveGoals]),
		set__singleton_set(NonLocals0, TableVar),
		assoc_list__keys(NumberedVars, Vars),
		set__insert_list(NonLocals0, Vars, NonLocals),
		create_instmap_delta([BlockSizeVarUnifyGoal, CreateAnsBlockGoal
			| SaveGoals], InstMapDelta0),
		instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta),
		goal_info_init(NonLocals, InstMapDelta, det, impure, Context,
			GoalInfo),
		Goal = GoalEx - GoalInfo
	;
		generate_call("table_simple_mark_as_succeeded", [TableVar],
			det, yes(impure), [], ModuleInfo, Context, Goal)
	).

:- pred generate_non_save_goal(assoc_list(prog_var, int)::in,
	prog_var::in, int::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	hlds_goal::out) is det.

generate_non_save_goal(NumberedOutputVars, TableVar, BlockSize, Context,
		!VarTypes, !VarSet, !TableInfo, Goal) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	generate_new_table_var("AnswerTableVar", trie_node_type,
		!VarTypes, !VarSet, AnsTableVar0),
	generate_call("table_nondet_get_ans_table", [TableVar, AnsTableVar0],
		det, yes(impure), [AnsTableVar0 - ground(unique, none)],
		ModuleInfo, Context, GetAnsTableGoal),
	assoc_list__keys(NumberedOutputVars, OutputVars),
	generate_answer_table_lookup_goals(OutputVars, Context,
		AnsTableVar0, AnsTableVar1, !VarTypes, !VarSet, !TableInfo,
		LookupAnsGoals),
	generate_call("table_nondet_answer_is_not_duplicate", [AnsTableVar1],
		semidet, yes(impure), [], ModuleInfo, Context,
		DuplicateCheckGoal),
	generate_new_table_var("AnswerSlotVar", trie_node_type,
		!VarTypes, !VarSet, AnsSlotVar),
	generate_call("table_nondet_new_ans_slot", [TableVar, AnsSlotVar], det,
		yes(impure), [AnsSlotVar - ground(unique, none)],
		ModuleInfo, Context, NewAnsSlotGoal),
	gen_int_construction("BlockSize", BlockSize, !VarTypes, !VarSet,
		BlockSizeVar, BlockSizeVarUnifyGoal),
	generate_new_table_var("AnswerBlock", answer_block_type,
		!VarTypes, !VarSet, AnsBlockVar),
	generate_call("table_create_ans_block",
		[AnsSlotVar, BlockSizeVar, AnsBlockVar], det, yes(impure),
		[AnsBlockVar - ground(unique, none)],
		ModuleInfo, Context, CreateAnsBlockGoal),
	generate_save_goals(NumberedOutputVars, AnsBlockVar, Context,
		!VarTypes, !VarSet, !TableInfo, SaveGoals),
	list__append([GetAnsTableGoal | LookupAnsGoals],
		[DuplicateCheckGoal, NewAnsSlotGoal, BlockSizeVarUnifyGoal,
		CreateAnsBlockGoal | SaveGoals], Goals),
	GoalEx = conj(Goals),
	set__singleton_set(NonLocals0, TableVar),
	set__insert_list(NonLocals0, OutputVars, NonLocals),
	create_instmap_delta(Goals, InstMapDelta0),
	instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, semidet,
		impure, Context, GoalInfo),
	Goal = GoalEx - GoalInfo.

:- pred generate_save_goals(assoc_list(prog_var, int)::in, prog_var::in,
	term__context::in, map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	list(hlds_goal)::out) is det.

generate_save_goals([], _TableVar, _Context, !VarTypes, !VarSet, !TableInfo,
		[]).
generate_save_goals([NumberedVar | NumberedRest], TableVar, Context,
		!VarTypes, !VarSet, !TableInfo, Goals) :-
	NumberedVar = Var - Offset,
	gen_int_construction("OffsetVar", Offset, !VarTypes,
		!VarSet, OffsetVar, OffsetUnifyGoal),
	ModuleInfo = !.TableInfo ^ table_module_info,
	map__lookup(!.VarTypes, Var, VarType),
	classify_type(ModuleInfo, VarType) = TypeCat,
	gen_save_call_for_type(TypeCat, VarType, TableVar, Var, OffsetVar,
		Context, !VarTypes, !VarSet, !TableInfo, CallGoal),
	generate_save_goals(NumberedRest, TableVar, Context,
		!VarTypes, !VarSet, !TableInfo, RestGoals),
	Goals =	[OffsetUnifyGoal, CallGoal | RestGoals].

:- pred gen_save_call_for_type(type_category::in, (type)::in,
	prog_var::in, prog_var::in, prog_var::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	hlds_goal::out) is det.

gen_save_call_for_type(TypeCat, Type, TableVar, Var, OffsetVar, Context,
		!VarTypes, !VarSet, !TableInfo, Goal) :-
	ModuleInfo = !.TableInfo ^ table_module_info,
	( type_util__type_is_io_state(Type) ->
		LookupPredName = "table_save_io_state_ans",
		generate_call(LookupPredName, [TableVar, OffsetVar, Var],
			det, yes(impure), [], ModuleInfo, Context, Goal)
	; builtin_type(TypeCat) = no ->
		make_type_info_var(Type, Context, !VarTypes, !VarSet,
			!TableInfo, TypeInfoVar, ExtraGoals),

		generate_call("table_save_any_ans",
			[TypeInfoVar, TableVar, OffsetVar, Var],
			det, yes(impure), [], ModuleInfo, Context, CallGoal),

		list__append(ExtraGoals, [CallGoal], ConjList),
		CallGoal = _ - GoalInfo,
		conj_list_to_goal(ConjList, GoalInfo, Goal)
	;
		type_save_category(TypeCat, CatString),
		string__append_list(["table_save_", CatString, "_ans"],
			LookupPredName),
		generate_call(LookupPredName, [TableVar, OffsetVar, Var],
			det, yes(impure), [], ModuleInfo, Context, Goal)
	).

%-----------------------------------------------------------------------------%

:- pred generate_simple_restore_goal(assoc_list(prog_var, int)::in,
	prog_var::in, module_info::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, hlds_goal::out) is det.

generate_simple_restore_goal(NumberedOutputVars, TipVar, ModuleInfo, Context,
		!VarTypes, !VarSet, Goal) :-
	generate_new_table_var("RestoreBlockVar", answer_block_type,
		!VarTypes, !VarSet, RestoreBlockVar),
	generate_call("table_simple_get_answer_block",
		[TipVar, RestoreBlockVar], det, yes(semipure),
		[RestoreBlockVar - ground(unique, none)],
		ModuleInfo, Context, GetBlockGoal),
	generate_restore_goals(NumberedOutputVars, RestoreBlockVar,
		ModuleInfo, Context, !VarTypes, !VarSet, RestoreGoals),
	GoalEx = conj([GetBlockGoal | RestoreGoals]),
	set__singleton_set(NonLocals0, TipVar),
	assoc_list__keys(NumberedOutputVars, OutputVars),
	set__insert_list(NonLocals0, OutputVars, NonLocals),
	create_instmap_delta(RestoreGoals, InstMapDelta0),
	instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, det, semipure, Context,
		GoalInfo),
	Goal = GoalEx - GoalInfo.

:- pred generate_restore_all_goal(determinism::in,
	assoc_list(prog_var, int)::in, prog_var::in,
	module_info::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, hlds_goal::out) is det.

generate_restore_all_goal(Detism, NumberedOutputVars, SubgoalVar,
		ModuleInfo, Context, !VarTypes, !VarSet, Goal) :-
	generate_new_table_var("AnswerTable", answer_block_type,
		!VarTypes, !VarSet, AnsTableVar),
	( Detism = multidet ->
		ReturnAllAns = "table_multi_return_all_ans"
	; Detism = nondet ->
		ReturnAllAns = "table_nondet_return_all_ans"
	;
		error("generate_restore_all_goal: invalid determinism")
	),
	generate_call(ReturnAllAns, [SubgoalVar, AnsTableVar],
		Detism, yes(semipure), [AnsTableVar - ground(unique, none)],
		ModuleInfo, Context, ReturnAnsBlocksGoal),
	generate_restore_goals(NumberedOutputVars, AnsTableVar, ModuleInfo,
		Context, !VarTypes, !VarSet, RestoreGoals),
	GoalEx = conj([ReturnAnsBlocksGoal | RestoreGoals]),
	set__singleton_set(NonLocals0, SubgoalVar),
	assoc_list__keys(NumberedOutputVars, OutputVars),
	set__insert_list(NonLocals0, OutputVars, NonLocals),
	create_instmap_delta([ReturnAnsBlocksGoal | RestoreGoals],
		InstMapDelta0),
	instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, nondet, semipure, Context,
		GoalInfo),
	Goal = GoalEx - GoalInfo.

:- pred generate_restore_goals(assoc_list(prog_var, int)::in, prog_var::in,
	module_info::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, list(hlds_goal)::out) is det.

generate_restore_goals([], _AnswerBlockVar, _ModuleInfo, _Context,
		!VarTypes, !VarSet, []).
generate_restore_goals([NumberedVar | NumberedRest], AnswerBlockVar,
		ModuleInfo, Context, !VarTypes, !VarSet,
		[OffsetUnifyGoal, CallGoal | RestGoals]) :-
	NumberedVar = Var - Offset,
	gen_int_construction("OffsetVar", Offset, !VarTypes, !VarSet,
		OffsetVar, OffsetUnifyGoal),
	map__lookup(!.VarTypes, Var, VarType),
	classify_type(ModuleInfo, VarType) = TypeCat,
	gen_restore_call_for_type(TypeCat, VarType, AnswerBlockVar, Var,
		OffsetVar, ModuleInfo, Context, CallGoal),
	generate_restore_goals(NumberedRest, AnswerBlockVar, ModuleInfo,
		Context, !VarTypes, !VarSet, RestGoals).

:- pred gen_restore_call_for_type(type_category::in, (type)::in,
	prog_var::in, prog_var::in, prog_var::in, module_info::in,
	term__context::in, hlds_goal::out) is det.

gen_restore_call_for_type(TypeCat, Type, TableVar, Var, OffsetVar,
		ModuleInfo, Context, Goal) :-
	( type_util__type_is_io_state(Type) ->
		LookupPredName = "table_restore_io_state_ans"
	; builtin_type(TypeCat) = no ->
		LookupPredName = "table_restore_any_ans"
	;
		type_save_category(TypeCat, CatString),
		string__append_list(["table_restore_", CatString, "_ans"],
			LookupPredName)
	),
	generate_call(LookupPredName, [TableVar, OffsetVar, Var], det,
		yes(impure), [Var - ground(shared, none)],
		ModuleInfo, Context, Goal).

%-----------------------------------------------------------------------------%

:- pred generate_suspend_goal(assoc_list(prog_var, int)::in, prog_var::in,
	module_info::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, hlds_goal::out) is det.

generate_suspend_goal(NumberedOutputVars, TableVar, ModuleInfo, Context,
		!VarTypes, !VarSet, Goal) :-

	generate_new_table_var("AnswerTable", answer_block_type,
		!VarTypes, !VarSet, AnsTableVar),
	generate_call("table_nondet_suspend", [TableVar, AnsTableVar],
		nondet, yes(semipure), [AnsTableVar - ground(unique, none)],
		ModuleInfo, Context, ReturnAnsBlocksGoal),

	generate_restore_goals(NumberedOutputVars, AnsTableVar,
		ModuleInfo, Context, !VarTypes, !VarSet, RestoreGoals),

	GoalEx = conj([ReturnAnsBlocksGoal | RestoreGoals]),
	set__singleton_set(NonLocals0, TableVar),
	assoc_list__keys(NumberedOutputVars, OutputVars),
	set__insert_list(NonLocals0, OutputVars, NonLocals),
	create_instmap_delta([ReturnAnsBlocksGoal | RestoreGoals],
		InstMapDelta0),
	instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, nondet, impure, Context,
		GoalInfo),
	Goal = GoalEx - GoalInfo.

%-----------------------------------------------------------------------------%

:- func infinite_recursion_msg = string.

infinite_recursion_msg = "detected infinite recursion".

:- pred generate_loop_error_goal(table_info::in, term__context::in,
	string::in, map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, hlds_goal::out) is det.

generate_loop_error_goal(TableInfo, Context, Msg, !VarTypes, !VarSet, Goal) :-
	ModuleInfo = TableInfo ^ table_module_info,
	PredInfo = TableInfo ^ table_cur_pred_info,

	Module = pred_info_module(PredInfo),
	Name = pred_info_name(PredInfo),
	Arity = pred_info_arity(PredInfo),
	PredOrFunc = pred_info_is_pred_or_func(PredInfo),
	hlds_out__pred_or_func_to_str(PredOrFunc, PredOrFuncS),
	prog_out__sym_name_to_string(qualified(Module, Name), NameS),
	string__int_to_string(Arity, ArityS),
	string__append_list([Msg, " in ", PredOrFuncS,
		" ", NameS, "/", ArityS], Message),

	gen_string_construction("MessageS", Message, !VarTypes, !VarSet,
		MessageVar, MessageConsGoal),
	generate_call("table_loopcheck_error", [MessageVar], erroneous,
		no, [], ModuleInfo, Context, CallGoal),

	GoalEx = conj([MessageConsGoal, CallGoal]),
	set__init(NonLocals),
	create_instmap_delta([MessageConsGoal, CallGoal], InstMapDelta0),
	instmap_delta_restrict(InstMapDelta0, NonLocals, InstMapDelta),
	goal_info_init(NonLocals, InstMapDelta, erroneous, impure,
		Context, GoalInfo),
	Goal = GoalEx - GoalInfo.

%-----------------------------------------------------------------------------%

:- pred generate_new_table_var(string::in, (type)::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, prog_var::out) is det.

generate_new_table_var(Name, Type, !VarTypes, !VarSet, Var) :-
	varset__new_named_var(!.VarSet, Name, Var, !:VarSet),
	map__set(!.VarTypes, Var, Type, !:VarTypes).

:- pred generate_call(string::in, list(prog_var)::in, determinism::in,
	maybe(goal_feature)::in, assoc_list(prog_var, inst)::in,
	module_info::in, term__context::in, hlds_goal::out) is det.

generate_call(PredName, Args, Detism, MaybeFeature, InstMap,
		ModuleInfo, Context, CallGoal) :-
	mercury_table_builtin_module(BuiltinModule),
	goal_util__generate_simple_call(BuiltinModule, PredName, predicate,
		Args, only_mode, Detism, MaybeFeature, InstMap, ModuleInfo,
		Context, CallGoal).

:- pred append_fail(hlds_goal::in, hlds_goal::out) is det.

append_fail(Goal, GoalAndThenFail) :-
	Goal = _ - GoalInfo,
	goal_info_get_nonlocals(GoalInfo, NonLocals),
	goal_info_get_context(GoalInfo, Context),
	instmap_delta_init_unreachable(UnreachInstMapDelta),
	goal_info_init(NonLocals, UnreachInstMapDelta, failure, impure,
		Context, ConjGoalInfo),
	fail_goal(FailGoal),
	GoalAndThenFail = conj([Goal, FailGoal]) - ConjGoalInfo.

:- pred gen_int_construction(string::in, int::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out,
	prog_var::out, hlds_goal::out) is det.

gen_int_construction(VarName, VarValue, !VarTypes, !VarSet, Var, Goal) :-
	make_int_const_construction(VarValue, yes(VarName), Goal, Var,
		!VarTypes, !VarSet).

:- pred gen_string_construction(string::in, string::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out,
	prog_var::out, hlds_goal::out) is det.

gen_string_construction(VarName, VarValue, !VarTypes, !VarSet, Var, Goal) :-
	make_string_const_construction(VarValue, yes(VarName), Goal, Var,
		!VarTypes, !VarSet).

:- func trie_node_type = (type).

trie_node_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "ml_trie_node") - 0, [], Type).

:- func subgoal_type = (type).

subgoal_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "ml_subgoal") - 0, [], Type).

:- func answer_block_type = (type).

answer_block_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "ml_answer_block") - 0, [], Type).

:- func status_type = (type).

status_type = Type :-
	mercury_table_builtin_module(TB),
	construct_type(qualified(TB, "subgoal_status") - 0, [], Type).

:- pred get_input_output_vars(list(prog_var)::in, list(mode)::in,
	module_info::in, list(prog_var)::out, list(prog_var)::out) is det.

get_input_output_vars([], [], _, [], []).
get_input_output_vars([_|_], [], _, _, _) :-
	error("get_input_output_vars: lists not same length").
get_input_output_vars([], [_|_], _, _, _) :-
	error("get_input_output_vars: lists not same length").
get_input_output_vars([Var | RestV], [Mode | RestM], ModuleInfo,
		InVars, OutVars) :-
	( mode_is_fully_input(ModuleInfo, Mode) ->
		get_input_output_vars(RestV, RestM, ModuleInfo,
			InVars0, OutVars),
		InVars = [Var | InVars0]
	; mode_is_fully_output(ModuleInfo, Mode) ->
		get_input_output_vars(RestV, RestM, ModuleInfo,
			InVars, OutVars0),
		OutVars = [Var | OutVars0]
	;
		error("Only fully input/output arguments are allowed " ++
			"in tabled code!")
	).

:- pred create_instmap_delta(hlds_goals::in, instmap_delta::out) is det.

create_instmap_delta([], IMD) :-
	instmap_delta_from_assoc_list([], IMD).
create_instmap_delta([Goal | Rest], IMD) :-
	Goal = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, IMD0),
	create_instmap_delta(Rest, IMD1),
	instmap_delta_apply_instmap_delta(IMD0, IMD1, IMD).

:- func builtin_type(type_category) = bool.

% For backward compatibility, we treat type_info_type as user_type. However,
% this makes the tabling of type_infos more expensive than necessary, since
% we essentially table the information in the type_info twice, once by tabling
% the type represented by the type_info (since this is the value of the type
% argument of the type constructor private_builtin.type_info/1, and then
% tabling the type_info itself.

builtin_type(int_type) = yes.
builtin_type(char_type) = yes.
builtin_type(str_type) = yes.
builtin_type(float_type) = yes.
builtin_type(void_type) = yes.
builtin_type(type_info_type) = no.
builtin_type(type_ctor_info_type) = yes.
builtin_type(typeclass_info_type) = yes.
builtin_type(base_typeclass_info_type) = yes.
builtin_type(higher_order_type) = no.
builtin_type(enum_type) = no.
builtin_type(variable_type) = no.
builtin_type(tuple_type) = no.
builtin_type(user_ctor_type) = no.

% Figure out what kind of data structure implements the lookup table for values
% of a given builtin type.

:- pred lookup_tabling_category(type_category::in,
	maybe(pair(string, table_trie_step))::out) is det.

lookup_tabling_category(int_type,   yes("int" -    table_trie_step_int)).
lookup_tabling_category(char_type,  yes("char" -   table_trie_step_char)).
lookup_tabling_category(str_type,   yes("string" - table_trie_step_string)).
lookup_tabling_category(float_type, yes("float" -  table_trie_step_float)).
lookup_tabling_category(void_type, _) :-
	error("lookup_tabling_category: void").
lookup_tabling_category(type_info_type, no).
lookup_tabling_category(type_ctor_info_type, no).
lookup_tabling_category(typeclass_info_type, no).
lookup_tabling_category(base_typeclass_info_type, no).
lookup_tabling_category(enum_type, no).
lookup_tabling_category(higher_order_type, no).
lookup_tabling_category(tuple_type, no).
lookup_tabling_category(variable_type, no).
lookup_tabling_category(user_ctor_type, no).

% Figure out which save and restore predicates in library/table_builtin.m
% we need to use for values of types belonging the type category given by
% the first argument. The returned value replaces CAT in table_save_CAT_ans
% and table_restore_CAT_ans.

:- pred type_save_category(type_category::in, string::out) is det.

type_save_category(enum_type,		"enum").
type_save_category(int_type,		"int").
type_save_category(char_type,		"char").
type_save_category(str_type,		"string").
type_save_category(float_type,		"float").
type_save_category(higher_order_type,	"pred").
type_save_category(tuple_type,		"any").
type_save_category(user_ctor_type,	"any").		% could do better
type_save_category(variable_type,	"any").		% could do better
type_save_category(void_type, _) :-
	error("type_save_category: void").
type_save_category(type_info_type, "any").		% could do better
type_save_category(type_ctor_info_type, _) :-
	error("type_save_category: type_ctor_info").
type_save_category(typeclass_info_type, _) :-
	error("type_save_category: typeclass_info").
type_save_category(base_typeclass_info_type, _) :-
	error("type_save_category: base_typeclass_info").

%-----------------------------------------------------------------------------%

:- pred table_gen__make_type_info_var((type)::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	prog_var::out, list(hlds_goal)::out) is det.

table_gen__make_type_info_var(Type, Context, !VarTypes, !VarSet, !TableInfo,
		TypeInfoVar, TypeInfoGoals) :-
	table_gen__make_type_info_vars([Type], Context, !VarTypes, !VarSet,
		!TableInfo, TypeInfoVars, TypeInfoGoals),
	( TypeInfoVars = [TypeInfoVar0] ->
		TypeInfoVar = TypeInfoVar0
	;
		error("table_gen__make_type_info_var: list length != 1")
	).

:- pred table_gen__make_type_info_vars(list(type)::in, term__context::in,
	map(prog_var, type)::in, map(prog_var, type)::out,
	prog_varset::in, prog_varset::out, table_info::in, table_info::out,
	list(prog_var)::out, list(hlds_goal)::out) is det.

table_gen__make_type_info_vars(Types, Context, !VarTypes, !VarSet, !TableInfo,
		TypeInfoVars, TypeInfoGoals) :-
	%
	% Extract the information from table_info
	%
	table_info_extract(!.TableInfo, ModuleInfo0, PredId, ProcId,
		PredInfo0, ProcInfo0),

	%
	% Put the varset and vartypes from the simplify_info
	% back in the proc_info
	%
	proc_info_set_vartypes(!.VarTypes, ProcInfo0, ProcInfo1),
	proc_info_set_varset(!.VarSet, ProcInfo1, ProcInfo2),

	%
	% Call polymorphism.m to create the type_infos
	%
	create_poly_info(ModuleInfo0, PredInfo0, ProcInfo2, PolyInfo0),
	polymorphism__make_type_info_vars(Types, Context,
		TypeInfoVars, TypeInfoGoals, PolyInfo0, PolyInfo),
	poly_info_extract(PolyInfo, PredInfo0, PredInfo,
		ProcInfo0, ProcInfo, ModuleInfo),

	%
	% Get the new varset and vartypes from the proc_info
	%
	proc_info_vartypes(ProcInfo, !:VarTypes),
	proc_info_varset(ProcInfo, !:VarSet),

	%
	% Put the new module_info, pred_info, and proc_info back in the
	% table_info.
	%
	table_info_init(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
		!:TableInfo).

%-----------------------------------------------------------------------------%

:- pred allocate_slot_numbers(list(prog_var)::in, int::in,
	assoc_list(prog_var, int)::out) is det.

allocate_slot_numbers([], _, []).
allocate_slot_numbers([Var | Vars], Offset0, [Var - Offset0 | NumberedVars]) :-
	allocate_slot_numbers(Vars, Offset0 + 1, NumberedVars).

:- pred key_belong_to_list(list(T)::in, pair(T, U)::in) is semidet.

key_belong_to_list(List, Key - _Value) :-
	list__member(Key, List).

%-----------------------------------------------------------------------------%

:- type table_info
	---> table_info(
			table_module_info	:: module_info,
			table_cur_pred_id	:: pred_id,
			table_cur_proc_id	:: proc_id,
			table_cur_pred_info	:: pred_info,
			table_cur_proc_info	:: proc_info
		).

:- pred table_info_init(module_info::in, pred_id::in, proc_id::in,
	pred_info::in, proc_info::in, table_info::out) is det.

:- pred table_info_extract(table_info::in, module_info::out,
	pred_id::out, proc_id::out, pred_info::out, proc_info::out) is det.

table_info_init(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo, TableInfo) :-
	TableInfo = table_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo).

table_info_extract(TableInfo, ModuleInfo, PredId, ProcId, PredInfo, ProcInfo)
		:-
	TableInfo = table_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo).

%-----------------------------------------------------------------------------%
