%----------------------------------------------------------------------------%
% Copyright (C) 1997-2001, 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% termination.m
%
% Main author: crs.
% Significant modifications by zs.
%
% This termination analysis is based on the algorithm given by Gerhard Groeger
% and Lutz Plumer in their paper "Handling of Mutual Recursion in Automatic
% Termination Proofs for Logic Programs"  which was printed in JICSLP '92
% (the proceedings of the Joint International Conference and Symposium on
% Logic Programming 1992) pages 336 - 350.
%
% Details about this implementation are covered in:
% Chris Speirs, Zoltan Somogyi, and Harald Sondergaard. Termination
% analysis for Mercury. In P. Van Hentenryck, editor, Static Analysis:
% Proceedings of the 4th International Symposium, Lecture Notes in Computer
% Science. Springer, 1997.  A more detailed version is available for
% download from http://www.cs.mu.oz.au/publications/tr_db/mu_97_09.ps.gz
%
% It also fails to prove termination for any predicate that involves higher
% order calls.
%
% The termination analysis may use a number of different norms to calculate
% the size of a term.  These are set by using the --termination-norm string
% option.  To add a new norm, the following files must be modified:
%
% globals.m 		To change the termination_norm type and
% 			convert_termination_norm predicate.
%
% handle_options.m 	To change the error message that is produced when
% 			an incorrect argument is given to --termination-norm.
%
% term_norm.m		To change the functor_norm predicate and change the
% 			functor_alg type.
%
% termination.m		To change the set_functor_info predicate.
%
%----------------------------------------------------------------------------%

:- module transform_hlds__termination.

:- interface.

:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.

:- import_module io.

	% Perform termination analysis on the module.

:- pred termination__pass(module_info::in, module_info::out,
	io::di, io::uo) is det.

	% Write out a termination_info pragma for the predicate if it
	% is exported, it is not a builtin and it is not a predicate used
	% to force type specialization.
:- pred termination__write_pred_termination_info(module_info::in, pred_id::in,
	io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds__inst_match.
:- import_module check_hlds__mode_util.
:- import_module check_hlds__type_util.
:- import_module hlds__hlds_data.
:- import_module hlds__hlds_error_util.
:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_out.
:- import_module hlds__passes_aux.
:- import_module hlds__special_pred.
:- import_module libs__globals.
:- import_module libs__options.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module transform_hlds__dependency_graph.
:- import_module transform_hlds__post_term_analysis.
:- import_module transform_hlds__term_errors.
:- import_module transform_hlds__term_norm.
:- import_module transform_hlds__term_pass1.
:- import_module transform_hlds__term_pass2.
:- import_module transform_hlds__term_util.

:- import_module bool, std_util, list.
:- import_module map, int, char, string, relation.
:- import_module require, bag, set, term.
:- import_module varset, svmap.

%----------------------------------------------------------------------------%

termination__pass(!Module, !IO) :-

		% Find out what norm we should use, and set up for using it
	globals__io_get_termination_norm(TermNorm, !IO),
	set_functor_info(TermNorm, !.Module, FunctorInfo),
	globals__io_lookup_int_option(termination_error_limit, MaxErrors, !IO),
	globals__io_lookup_int_option(termination_path_limit, MaxPaths, !IO),
	PassInfo = pass_info(FunctorInfo, MaxErrors, MaxPaths),

		% Process builtin and compiler-generated predicates,
		% and user-supplied pragmas.
	module_info_predids(!.Module, PredIds),
	check_preds(PredIds, !Module, !IO),

		% Process all the SCCs of the call graph in a bottom up order.
	module_info_ensure_dependency_info(!Module),
	module_info_dependency_info(!.Module, DepInfo),
	hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),

		% Set the termination status of foreign_procs.
	check_foreign_code_attributes(SCCs, !Module, !IO),

		% Ensure that termination pragmas for a proc. do conflict
		% with termination pragmas for other procs. in the same SCC.
	check_pragmas_are_consistent(SCCs, !Module, !IO),

	list__foldl2(process_scc(PassInfo), SCCs, !Module, !IO),

	globals__io_lookup_bool_option(make_optimization_interface,
		MakeOptInt, !IO),
	( MakeOptInt = yes ->
		termination__make_opt_int(PredIds, !.Module, !IO)
	;
		true
	),
	post_term_analysis__process_module(!.Module, !IO).

%----------------------------------------------------------------------------%
%
% Handle foreign code attributes.
%

% Set the termination status for any procedures implemented using the
% foreign language interface.  If the terminates/does_not_terminate
% attribute has been set then we set the termination status of the procedure
% accordingly.  Otherwise the procedure is considered to be terminating
% if it does not call Mercury and non-terminating if it does.
%
% We also check that the foreign code attributes do not conflict with any
% termination pragmas that have been supplied for the procedure.

:- pred check_foreign_code_attributes(list(list(pred_proc_id))::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

check_foreign_code_attributes(SCCs, !Module, !IO) :-
	list__foldl2(check_foreign_code_attributes_2, SCCs, !Module, !IO).

:- pred check_foreign_code_attributes_2(list(pred_proc_id)::in, module_info::in,
	module_info::out, io::di, io::uo) is det.

	% This case shouldn't happen.
check_foreign_code_attributes_2([], _, _, _, _) :-
	unexpected(this_file, "check_foreign_code_attributes_2/5: empty SCC.").
check_foreign_code_attributes_2([PPId], !Module, !IO) :-
	module_info_pred_proc_info(!.Module, PPId, PredInfo, ProcInfo0),
	(
		proc_info_goal(ProcInfo0, Goal),
		fst(Goal) = foreign_proc(Attributes, _, _, _, _, _)
	->
		proc_info_get_maybe_termination_info(ProcInfo0,
			MaybeTermination),
		proc_info_context(ProcInfo0, Context),
		(
			MaybeTermination = no,
			( attributes_imply_termination(Attributes) ->
				proc_info_set_maybe_termination_info(
					yes(cannot_loop), ProcInfo0, ProcInfo)
			;
				TermErr = Context - does_not_term_foreign(PPId),
				proc_info_set_maybe_termination_info(
					yes(can_loop([TermErr])), ProcInfo0,
					ProcInfo)
			)
		;
			% If there was a `pragma terminates' declaration
			% for this procedure then check that the foreign
			% code attributes do not contradict this.
			MaybeTermination = yes(cannot_loop),
			( terminates(Attributes) = does_not_terminate ->
				TermErr = Context - inconsistent_annotations,
				proc_info_set_maybe_termination_info(
					yes(can_loop([TermErr])), ProcInfo0,
					ProcInfo),
				ProcNamePieces =
					describe_one_proc_name(!.Module,
						should_module_qualify, PPId),
				Piece1 = words("has a `pragma terminates'"),
				Piece2 = words("declaration but also has the"),
				Piece3 = words("`does_not_terminate' foreign"),
				Piece4 = words("code attribute set."),
				Components = [words("Warning:")] ++
					ProcNamePieces ++
					[Piece1, Piece2, Piece3, Piece4],
				error_util__report_warning(Context, 0,
					Components, !IO)
			;
				ProcInfo = ProcInfo0
			)
		;
			% In this case there was a `pragma does_not_terminate'
			% declaration - check that the foreign code attribute
			% does not contradict this.
			MaybeTermination = yes(can_loop(TermErrs0)),
			( terminates(Attributes) = terminates ->
			    TermErr = Context - inconsistent_annotations,
			    TermErrs = [TermErr | TermErrs0 ],
			    proc_info_set_maybe_termination_info(
			        yes(can_loop(TermErrs)),
			        ProcInfo0, ProcInfo),
			    ProcNamePieces = describe_one_proc_name(!.Module,
				    should_module_qualify, PPId),
			    Piece1 = words("has a `pragma does_not_terminate'"),
			    Piece2 = words("declaration but also has the"),
			    Piece3 = words("`terminates' foreign code"),
			    Piece4 = words("attribute set."),
			    Components = [words("Warning:")] ++
			    	ProcNamePieces ++
				[Piece1, Piece2, Piece3, Piece4],
			    error_util__report_warning(Context, 0, Components,
			        !IO)
			;
			    ProcInfo = ProcInfo0
			)
		),
		module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo,
			!Module)
	;
		true
	).
check_foreign_code_attributes_2([_, _ | _], !Module, !IO).

%----------------------------------------------------------------------------%
% Check that any user-supplied termination information (from pragma
% terminates/does_not_terminate) is consistent for each SCC in the program.
%
% The information will not be consistent if:
% (1) One or more procs. in the SCC has a terminates pragma *and* one or more
%     procs. in the SCC has a does_not_terminate pragma.
% (2) One or more procs. in the SCC has a termination pragma (of either sort),
%     *and* the termination status of one or more procs. in the SCC is
%     unknown.  (We check this after checking for the first case, so the
%     termination info. for the known procs. will be consistent.)
%
% In the first case set the termination for all procs. in the SCC to
% can_loop and emit a warning.  In the second, set the termination of any
% procs. whose termination status is unknown to be the same as those whose
% termination status is known.

:- pred check_pragmas_are_consistent(list(list(pred_proc_id))::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

check_pragmas_are_consistent(SCCs, !Module, !IO) :-
	list__foldl2(check_scc_pragmas_are_consistent, SCCs, !Module, !IO).

:- pred check_scc_pragmas_are_consistent(list(pred_proc_id)::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

check_scc_pragmas_are_consistent(SCC, !Module, !IO) :-
	list__filter(is_termination_known(!.Module), SCC, SCCTerminationKnown,
		SCCTerminationUnknown),
	(
		SCCTerminationKnown = []
	;
		SCCTerminationKnown = [KnownPPId | _],
		module_info_pred_proc_info(!.Module, KnownPPId, _,
			KnownProcInfo),
		proc_info_get_maybe_termination_info(KnownProcInfo,
			MaybeKnownTerm),
		(
			MaybeKnownTerm = no,
			unexpected(this_file, "No termination info. found.")
		;
			MaybeKnownTerm  = yes(KnownTermStatus)
		),
		(
			check_procs_known_term(KnownTermStatus,
				SCCTerminationKnown, !.Module)
		->
			    % Force any procs. in the SCC whose termination
			    % status is unknown to have the same termination
			    % status as those that are known.
			set_termination_infos(SCCTerminationUnknown,
				KnownTermStatus, !Module)
		;
			    % There is a conflict between the user-supplied
			    % termination information for two or more procs.
			    % in this SCC.  Emit a warning and then assume
			    % that they all loop.
			get_context_from_scc(SCCTerminationKnown, !.Module,
				Context),
			NewTermStatus =
				can_loop([Context - inconsistent_annotations]),
			set_termination_infos(SCC, NewTermStatus, !Module),

			PredIds = list__map((func(proc(PredId, _)) = PredId),
				SCCTerminationKnown),
			PredNamesPieces = describe_several_pred_names(!.Module,
				should_module_qualify, PredIds),
			Piece1 = words(
				"are mutually recursive but some of their"),
			Piece2 = words(
				"termination pragmas are inconsistent."),
			Components = [words("Warning:")] ++ PredNamesPieces ++
				[Piece1, Piece2],
			error_util__report_warning(Context, 0, Components, !IO)
		)
	).

	% Check that all procedures in an SCC whose termination status is known
	% have the same termination status.
:- pred check_procs_known_term(termination_info::in, list(pred_proc_id)::in,
	module_info::in) is semidet.

check_procs_known_term(_, [], _).
check_procs_known_term(Status, [PPId | PPIds], Module) :-
	module_info_pred_proc_info(Module, PPId, _, ProcInfo),
	proc_info_get_maybe_termination_info(ProcInfo, MaybeTerm),
	(
		MaybeTerm = no,
		unexpected(this_file, "No termination info for procedure.")
	;
		MaybeTerm = yes(PPIdStatus)
	),
	(
		Status = cannot_loop,
		PPIdStatus = cannot_loop
	;
		Status = can_loop(_),
		PPIdStatus = can_loop(_)
	),
	check_procs_known_term(Status, PPIds, Module).

%----------------------------------------------------------------------------%

	% For each SCC, we first find out the relationships among
	% the sizes of the arguments of the procedures of the SCC,
	% and then attempt to prove termination of the procedures.

:- pred termination__process_scc(pass_info::in, list(pred_proc_id)::in,
	module_info::in, module_info::out, io::di, io::uo) is det.

termination__process_scc(PassInfo, SCC, !Module, !IO) :-
	IsArgSizeKnown = (pred(PPId::in) is semidet :-
		module_info_pred_proc_info(!.Module, PPId, _, ProcInfo),
		proc_info_get_maybe_arg_size_info(ProcInfo, yes(_))
	),
	list__filter(IsArgSizeKnown, SCC, _SCCArgSizeKnown, SCCArgSizeUnknown),
	( SCCArgSizeUnknown = [] ->
		ArgSizeErrors = [],
		TermErrors = []
	;
		find_arg_sizes_in_scc(SCCArgSizeUnknown, !.Module, PassInfo,
			ArgSizeResult, TermErrors, !IO),
		(
			ArgSizeResult = ok(Solutions, OutputSupplierMap),
			set_finite_arg_size_infos(Solutions,
				OutputSupplierMap, !Module),
			ArgSizeErrors = []
		;
			ArgSizeResult = error(Errors),
			set_infinite_arg_size_infos(SCCArgSizeUnknown,
				infinite(Errors), !Module),
			ArgSizeErrors = Errors
		)
	),
	list__filter(is_termination_known(!.Module), SCC,
		_SCCTerminationKnown, SCCTerminationUnknown),
	( SCCTerminationUnknown = [] ->
			%
			% We may possibly have encountered inconsistent
			% terminates/does_not_terminate pragmas for this SCC,
			% so we need to report errors here as well.
		true
	;
		IsFatal = (pred(ContextError::in) is semidet :-
			ContextError = _Context - Error,
			( Error = horder_call
			; Error = horder_args(_, _)
			; Error = imported_pred
			)
		),
		list__filter(IsFatal, ArgSizeErrors, FatalErrors),
		list__append(TermErrors, FatalErrors, BothErrors),
		( BothErrors = [_ | _] ->
			% These errors prevent pass 2 from proving termination
			% in any case, so we may as well not prove it quickly.
			PassInfo = pass_info(_, MaxErrors, _),
			list__take_upto(MaxErrors, BothErrors,
				ReportedErrors),
			TerminationResult = can_loop(ReportedErrors)
		;
			globals__io_lookup_int_option(termination_single_args,
				SingleArgs, !IO),
			prove_termination_in_scc(SCCTerminationUnknown,
				!.Module, PassInfo, SingleArgs,
				TerminationResult)
		),
		set_termination_infos(SCCTerminationUnknown,
			TerminationResult, !Module),
		( TerminationResult = can_loop(TerminationErrors) ->
			report_termination_errors(SCC, TerminationErrors,
				!Module, !IO)
		;
			true
		)
	).

%----------------------------------------------------------------------------%

% This predicate takes the results from solve_equations
% and inserts these results into the module info.

:- pred set_finite_arg_size_infos(list(pair(pred_proc_id, int))::in,
	used_args::in, module_info::in, module_info::out) is det.

set_finite_arg_size_infos([], _, !Module).
set_finite_arg_size_infos([Soln | Solns], OutputSupplierMap, !Module) :-
	Soln = PPId - Gamma,
	PPId = proc(PredId, ProcId),
	module_info_preds(!.Module, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),
	map__lookup(OutputSupplierMap, PPId, OutputSuppliers),
	ArgSizeInfo = finite(Gamma, OutputSuppliers),
	proc_info_set_maybe_arg_size_info(yes(ArgSizeInfo),
		ProcInfo, ProcInfo1),
	map__set(ProcTable, ProcId, ProcInfo1, ProcTable1),
	pred_info_set_procedures(ProcTable1, PredInfo, PredInfo1),
	map__set(PredTable0, PredId, PredInfo1, PredTable),
	module_info_set_preds(PredTable, !Module),
	set_finite_arg_size_infos(Solns, OutputSupplierMap, !Module).

:- pred set_infinite_arg_size_infos(list(pred_proc_id)::in,
	arg_size_info::in, module_info::in, module_info::out) is det.

set_infinite_arg_size_infos([], _, !Module).
set_infinite_arg_size_infos([PPId | PPIds], ArgSizeInfo, !Module) :-
	PPId = proc(PredId, ProcId),
	module_info_preds(!.Module, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo),
	pred_info_procedures(PredInfo, ProcTable),
	map__lookup(ProcTable, ProcId, ProcInfo),
	proc_info_set_maybe_arg_size_info(yes(ArgSizeInfo),
		ProcInfo, ProcInfo1),
	map__set(ProcTable, ProcId, ProcInfo1, ProcTable1),
	pred_info_set_procedures(ProcTable1, PredInfo, PredInfo1),
	map__set(PredTable0, PredId, PredInfo1, PredTable),
	module_info_set_preds(PredTable, !Module),
	set_infinite_arg_size_infos(PPIds, ArgSizeInfo, !Module).

%----------------------------------------------------------------------------%

:- pred set_termination_infos(list(pred_proc_id)::in, termination_info::in,
	module_info::in, module_info::out) is det.

set_termination_infos([], _, !Module).
set_termination_infos([PPId | PPIds], TerminationInfo, !Module) :-
	PPId = proc(PredId, ProcId),
	module_info_preds(!.Module, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_procedures(PredInfo0, ProcTable0),
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_set_maybe_termination_info(yes(TerminationInfo),
		ProcInfo0, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
	pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
	map__det_update(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(PredTable, !Module),
	set_termination_infos(PPIds, TerminationInfo, !Module).

:- pred report_termination_errors(list(pred_proc_id)::in,
	list(term_errors__error)::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

report_termination_errors(SCC, Errors, !Module, !IO) :-
	globals__io_lookup_bool_option(check_termination,
		NormalErrors, !IO),
	globals__io_lookup_bool_option(verbose_check_termination,
		VerboseErrors, !IO),
	(
		IsCheckTerm = (pred(PPId::in) is semidet :-
			module_info_pred_proc_info(!.Module, PPId, PredInfo, _),
			\+ pred_info_is_imported(PredInfo),
			pred_info_get_markers(PredInfo, Markers),
			check_marker(Markers, check_termination)
		),
		list__filter(IsCheckTerm, SCC, CheckTermPPIds),
		CheckTermPPIds = [_ | _]
	->
		% If any procedure in the SCC has a check_terminates pragma,
		% print out one error message for the whole SCC and indicate
		% an error.
		term_errors__report_term_errors(SCC, Errors, !.Module, !IO),
		io__set_exit_status(1, !IO),
		module_info_incr_errors(!Module)
	;
		IsNonImported = (pred(PPId::in) is semidet :-
			module_info_pred_proc_info(!.Module, PPId, PredInfo, _),
			\+ pred_info_is_imported(PredInfo)
		),
		list__filter(IsNonImported, SCC, NonImportedPPIds),
		NonImportedPPIds = [_ | _],

		% Don't emit non-termination warnings for the compiler
		% generated wrapper predicates for solver type initialisation
		% predicates.  If they don't terminate there's nothing the user
		% can do about it anyway - the problem is with the
		% initialisation predicate specified by the user, not the
		% wrapper.
		list__all_false(is_solver_init_wrapper_pred(!.Module), SCC),

		% Only output warnings of non-termination for direct
		% errors.  If there are no direct errors then output
		% the indirect errors - this is better than giving
		% no reason at all.  If verbose errors is enabled then
		% output both sorts of error.
		% (See term_errors.m for details of direct and indirect
		%  errors).

		( VerboseErrors = yes ->
			PrintErrors = Errors
		; NormalErrors = yes ->
			IsNonSimple = (pred(ContextError::in) is semidet :-
				ContextError = _Context - Error,
				\+ indirect_error(Error)
			),
			list__filter(IsNonSimple, Errors, PrintErrors0),
				% If there were no direct errors then use
				% the indirect errors.
			( if 	PrintErrors0 = []
			  then	PrintErrors = Errors
			  else	PrintErrors = PrintErrors0
			)
		;
			fail
		)
	->
		term_errors__report_term_errors(SCC, PrintErrors, !.Module, !IO)
	;
		true
	).

	% Succeeds iff the given PPId is a compiler generated wrapper
	% predicate for a solver type initialisation predicate.
	%
:- pred is_solver_init_wrapper_pred(module_info::in, pred_proc_id::in)
	is semidet.

is_solver_init_wrapper_pred(ModuleInfo, proc(PredId, _)) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_get_origin(PredInfo, PredOrigin),
	PredOrigin = special_pred(SpecialPredId - _),
	SpecialPredId = initialise.

%----------------------------------------------------------------------------%

:- pred check_preds(list(pred_id)::in, module_info::in, module_info::out,
	io::di, io::uo) is det.

% This predicate processes each predicate and sets the termination property
% if possible.  This is done as follows:  Set the termination to yes if:
% - there is a terminates pragma defined for the predicate
% - there is a `check_termination' pragma defined for the predicate, and it
% 	is imported, and the compiler is not currently generating the
% 	intermodule optimization file.
% - the predicate is a builtin predicate or is compiler generated (This
% 	also sets the termination constant and UsedArgs).
%
% Set the termination to dont_know if:
% - there is a `does_not_terminate' pragma defined for this predicate.
% - the predicate is imported and there is no other source of information
% 	about it (termination_info pragmas, terminates pragmas,
% 	check_termination pragmas, builtin/compiler generated).

check_preds([], !Module, !IO).
check_preds([PredId | PredIds], !Module, !IO) :-
	write_pred_progress_message("% Checking ", PredId, !.Module, !IO),
	globals__io_lookup_bool_option(make_optimization_interface,
		MakeOptInt, !IO),
	module_info_preds(!.Module, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_import_status(PredInfo0, ImportStatus),
	pred_info_context(PredInfo0, Context),
	pred_info_procedures(PredInfo0, ProcTable0),
	pred_info_get_markers(PredInfo0, Markers),
	ProcIds = pred_info_procids(PredInfo0),
	(
		% It is possible for compiler generated/mercury builtin
		% predicates to be imported or locally defined, so they
		% must be covered here, separately.
		set_compiler_gen_terminates(PredInfo0, ProcIds, PredId,
			!.Module, ProcTable0, ProcTable1)
	->
		ProcTable2 = ProcTable1
	;
		status_defined_in_this_module(ImportStatus, yes)
	->
		( check_marker(Markers, terminates) ->
			change_procs_termination_info(ProcIds, yes,
				cannot_loop, ProcTable0, ProcTable2)
		;
			ProcTable2 = ProcTable0
		)
	;
		% Not defined in this module.

		% All of the predicates that are processed in this section
		% are imported in some way.
		% With imported predicates, any 'check_termination'
		% pragmas will be checked by the compiler when it compiles
		% the relevant source file (that the predicate was imported
		% from).  When making the intermodule optimizations, the
		% check_termination will not be checked when the relevant
		% source file is compiled, so it cannot be depended upon.
		(
			(
				check_marker(Markers, terminates)
			;
				MakeOptInt = no,
				check_marker(Markers, check_termination)
			)
		->
			change_procs_termination_info(ProcIds, yes,
				cannot_loop, ProcTable0, ProcTable1)
		;
			TerminationError = Context - imported_pred,
			TerminationInfo = can_loop([TerminationError]),
			change_procs_termination_info(ProcIds, no,
				TerminationInfo, ProcTable0, ProcTable1)
		),
		ArgSizeError = imported_pred,
		ArgSizeInfo = infinite([Context - ArgSizeError]),
		change_procs_arg_size_info(ProcIds, no, ArgSizeInfo,
			ProcTable1, ProcTable2)
	),
	( check_marker(Markers, does_not_terminate) ->
		RequestError = Context - does_not_term_pragma(PredId),
		RequestTerminationInfo = can_loop([RequestError]),
		change_procs_termination_info(ProcIds, yes,
			RequestTerminationInfo, ProcTable2, ProcTable)
	;
		ProcTable = ProcTable2
	),
	pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(PredTable, !Module),
	check_preds(PredIds, !Module, !IO).

%----------------------------------------------------------------------------%

% This predicate checks each ProcId in the list to see if it is a compiler
% generated predicate, or a predicate from builtin.m or private_builtin.m.
% If it is, then the compiler sets the termination property of the ProcIds
% accordingly.

% We assume that user-defined special predicates terminate.  This 
% assumption is checked later during the post_term_analysis pass.

:- pred set_compiler_gen_terminates(pred_info::in, list(proc_id)::in,
	pred_id::in, module_info::in, proc_table::in, proc_table::out)
	is semidet.

set_compiler_gen_terminates(PredInfo, ProcIds, PredId, Module, !ProcTable) :-
	(
		pred_info_is_builtin(PredInfo)
	->
		set_builtin_terminates(ProcIds, PredId, PredInfo, Module,
			!ProcTable)
	;
		(
			ModuleName = pred_info_module(PredInfo),
			Name = pred_info_name(PredInfo),
			Arity = pred_info_orig_arity(PredInfo),
			special_pred_name_arity(SpecPredId0, Name, Arity),
			any_mercury_builtin_module(ModuleName)
		->
			SpecialPredId = SpecPredId0
		;
			pred_info_get_origin(PredInfo, Origin),
			Origin = special_pred(SpecialPredId - _)
		)
	->
		set_generated_terminates(ProcIds, SpecialPredId, !ProcTable)
	;
		fail
	).

:- pred set_generated_terminates(list(proc_id)::in, special_pred_id::in,
	proc_table::in, proc_table::out) is det.

set_generated_terminates([], _, !ProcTable).
set_generated_terminates([ProcId | ProcIds], SpecialPredId, !ProcTable) :-
	%
	% We don't need to do anything special for solver type initialisation 
	% predicates.  Leaving it up to the analyser may result in better
	% argument size information anyway.
	% 
	( SpecialPredId \= initialise -> 
		map__lookup(!.ProcTable, ProcId, ProcInfo0),
		proc_info_headvars(ProcInfo0, HeadVars),
		special_pred_id_to_termination(SpecialPredId, HeadVars,
			ArgSize, Termination),
		proc_info_set_maybe_arg_size_info(yes(ArgSize), ProcInfo0,
			ProcInfo1),
		proc_info_set_maybe_termination_info(yes(Termination),
			ProcInfo1, ProcInfo),
		svmap__det_update(ProcId, ProcInfo, !ProcTable)
	;
		true
	),
	set_generated_terminates(ProcIds, SpecialPredId, !ProcTable).

	% XXX The ArgSize argument for unify predicates may not be correct
	% in the case where the type has user-defined equality.
	%
:- pred special_pred_id_to_termination(special_pred_id::in,
	list(prog_var)::in, arg_size_info::out, termination_info::out) is det.

special_pred_id_to_termination(compare, HeadVars, ArgSize, Termination) :-
	term_util__make_bool_list(HeadVars, [no, no, no], OutList),
	ArgSize = finite(0, OutList),
	Termination = cannot_loop.
special_pred_id_to_termination(unify, HeadVars, ArgSize, Termination) :-
	term_util__make_bool_list(HeadVars, [yes, yes], OutList),
	ArgSize = finite(0, OutList),
	Termination = cannot_loop.
special_pred_id_to_termination(index, HeadVars, ArgSize, Termination) :-
	term_util__make_bool_list(HeadVars, [no, no], OutList),
	ArgSize = finite(0, OutList),
	Termination = cannot_loop.
special_pred_id_to_termination(initialise, _, _, _) :-
	unexpected(this_file, "special_pred_id_to_termination/4 " ++
		"initialise predicate").

% The list of proc_ids must refer to builtin predicates.  This predicate
% sets the termination information of builtin predicates.

:- pred set_builtin_terminates(list(proc_id)::in, pred_id::in, pred_info::in,
	module_info::in, proc_table::in, proc_table::out) is det.

set_builtin_terminates([], _, _, _, !ProcTable).
set_builtin_terminates([ProcId | ProcIds], PredId, PredInfo, Module,
		!ProcTable) :-
	map__lookup(!.ProcTable, ProcId, ProcInfo0),
	( all_args_input_or_zero_size(Module, PredInfo, ProcInfo0) ->
		% The size of the output arguments will all be 0,
		% independent of the size of the input variables.
		% UsedArgs should be set to yes([no, no, ...]).
		proc_info_headvars(ProcInfo0, HeadVars),
		term_util__make_bool_list(HeadVars, [], UsedArgs),
		ArgSizeInfo = yes(finite(0, UsedArgs))
	;
		pred_info_context(PredInfo, Context),
		Error = is_builtin(PredId),
		ArgSizeInfo = yes(infinite([Context - Error]))
	),
	proc_info_set_maybe_arg_size_info(ArgSizeInfo, ProcInfo0, ProcInfo1),
	proc_info_set_maybe_termination_info(yes(cannot_loop), ProcInfo1,
		ProcInfo),
	map__det_update(!.ProcTable, ProcId, ProcInfo, !:ProcTable),
	set_builtin_terminates(ProcIds, PredId, PredInfo, Module, !ProcTable).

:- pred all_args_input_or_zero_size(module_info::in, pred_info::in,
	proc_info::in) is semidet.

all_args_input_or_zero_size(Module, PredInfo, ProcInfo) :-
	pred_info_arg_types(PredInfo, TypeList),
	proc_info_argmodes(ProcInfo, ModeList),
	all_args_input_or_zero_size_2(TypeList, ModeList, Module).

:- pred all_args_input_or_zero_size_2(list(type)::in, list(mode)::in,
	module_info::in) is semidet.

all_args_input_or_zero_size_2([], [], _).
all_args_input_or_zero_size_2([], [_|_], _) :-
	unexpected(this_file, "all_args_input_or_size_2/3 - unmatched lists.").
all_args_input_or_zero_size_2([_|_], [], _) :-
	unexpected(this_file, "all_args_input_or_size_2/3 - unmatched lists.").
all_args_input_or_zero_size_2([Type | Types], [Mode | Modes], Module) :-
	( mode_is_input(Module, Mode) ->
		% The variable is an input variables, so its size is
		% irrelevant.
		all_args_input_or_zero_size_2(Types, Modes, Module)
	;
		zero_size_type(Type, Module),
		all_args_input_or_zero_size_2(Types, Modes, Module)
	).

%----------------------------------------------------------------------------%

% This predicate sets the arg_size_info property of the given list
% of procedures.
%
% change_procs_arg_size_info(ProcList, Override, TerminationInfo,
% 		ProcTable, ProcTable)
%
% If Override is yes, then this predicate overrides any existing arg_size
% information. If Override is no, then it leaves the proc_info of a procedure
% unchanged unless the proc_info had no arg_size information (i.e. the
% maybe(arg_size_info) field was set to "no").

:- pred change_procs_arg_size_info(list(proc_id)::in, bool::in,
	arg_size_info::in, proc_table::in, proc_table::out) is det.

change_procs_arg_size_info([], _, _, !ProcTable).
change_procs_arg_size_info([ProcId | ProcIds], Override, ArgSize, !ProcTable) :-
	map__lookup(!.ProcTable, ProcId, ProcInfo0),
	(
		(
			Override = yes
		;
			proc_info_get_maybe_arg_size_info(ProcInfo0, no)
		)
	->
		proc_info_set_maybe_arg_size_info(yes(ArgSize),
			ProcInfo0, ProcInfo),
		map__det_update(!.ProcTable, ProcId, ProcInfo, !:ProcTable)
	;
		true
	),
	change_procs_arg_size_info(ProcIds, Override, ArgSize, !ProcTable).

% This predicate sets the termination_info property of the given list
% of procedures.
%
% change_procs_termination_info(ProcList, Override, TerminationInfo,
% 		ProcTable, ProcTable)
%
% If Override is yes, then this predicate overrides any existing termination
% information. If Override is no, then it leaves the proc_info of a procedure
% unchanged unless the proc_info had no termination information (i.e. the
% maybe(termination_info) field was set to "no").

:- pred change_procs_termination_info(list(proc_id)::in, bool::in,
	termination_info::in, proc_table::in, proc_table::out) is det.

change_procs_termination_info([], _, _, !ProcTable).
change_procs_termination_info([ProcId | ProcIds], Override, Termination,
		!ProcTable) :-
	map__lookup(!.ProcTable, ProcId, ProcInfo0),
	(
		(
			Override = yes
		;
			proc_info_get_maybe_termination_info(ProcInfo0, no)
		)
	->
		proc_info_set_maybe_termination_info(yes(Termination),
			ProcInfo0, ProcInfo),
		map__det_update(!.ProcTable, ProcId, ProcInfo, !:ProcTable)
	;
		true
	),
	change_procs_termination_info(ProcIds, Override, Termination,
		!ProcTable).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

% These predicates are used to add the termination_info pragmas to the .opt
% file.  It is often better to use the .trans_opt file, as it gives
% much better accuracy.  The two files are not mutually exclusive, and
% termination information may be stored in both.

:- pred termination__make_opt_int(list(pred_id)::in, module_info::in,
	io::di, io::uo) is det.

termination__make_opt_int(PredIds, Module, !IO) :-
	module_info_name(Module, ModuleName),
	module_name_to_file_name(ModuleName, ".opt.tmp", no, OptFileName, !IO),
	globals__io_lookup_bool_option(verbose, Verbose, !IO),
	maybe_write_string(Verbose,
		"% Appending termination_info pragmas to `", !IO),
	maybe_write_string(Verbose, OptFileName, !IO),
	maybe_write_string(Verbose, "'...", !IO),
	maybe_flush_output(Verbose, !IO),

	io__open_append(OptFileName, OptFileRes, !IO),
	(
		OptFileRes = ok(OptFile),
		io__set_output_stream(OptFile, OldStream, !IO),
		list__foldl(termination__write_pred_termination_info(Module),
			PredIds, !IO),
		io__set_output_stream(OldStream, _, !IO),
		io__close_output(OptFile, !IO),
		maybe_write_string(Verbose, " done.\n", !IO)
	;
		OptFileRes = error(IOError),
		% failed to open the .opt file for processing
		maybe_write_string(Verbose, " failed!\n", !IO),
		io__error_message(IOError, IOErrorMessage),
		io__write_strings(["Error opening file `",
			OptFileName, "' for output: ", IOErrorMessage], !IO),
		io__set_exit_status(1, !IO)
	).

termination__write_pred_termination_info(Module, PredId, !IO) :-
	module_info_pred_info(Module, PredId, PredInfo),
	pred_info_import_status(PredInfo, ImportStatus),
	module_info_type_spec_info(Module, TypeSpecInfo),
	TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),

	(
		(
			ImportStatus = exported
		;
			ImportStatus = opt_exported
		),
		\+ is_unify_or_compare_pred(PredInfo),

		% XXX These should be allowed, but the predicate
		% declaration for the specialized predicate is not produced
		% before the termination pragmas are read in, resulting
		% in an undefined predicate error.
		\+ set__member(PredId, TypeSpecForcePreds)
	->
		PredName = pred_info_name(PredInfo),
		ProcIds = pred_info_procids(PredInfo),
		PredOrFunc = pred_info_is_pred_or_func(PredInfo),
		ModuleName = pred_info_module(PredInfo),
		pred_info_procedures(PredInfo, ProcTable),
		pred_info_context(PredInfo, Context),
		SymName = qualified(ModuleName, PredName),
		termination__make_opt_int_procs(PredId, ProcIds, ProcTable,
			PredOrFunc, SymName, Context, !IO)
	;
		true
	).

:- pred termination__make_opt_int_procs(pred_id::in, list(proc_id)::in,
	proc_table::in, pred_or_func::in, sym_name::in, prog_context::in,
	io::di, io::uo) is det.

termination__make_opt_int_procs(_PredId, [], _, _, _, _, !IO).
termination__make_opt_int_procs(PredId, [ ProcId | ProcIds ], ProcTable,
		PredOrFunc, SymName, Context, !IO) :-
	map__lookup(ProcTable, ProcId, ProcInfo),
	proc_info_get_maybe_arg_size_info(ProcInfo, ArgSize),
	proc_info_get_maybe_termination_info(ProcInfo, Termination),
	proc_info_declared_argmodes(ProcInfo, ModeList),
	write_pragma_termination_info(PredOrFunc, SymName,
		ModeList, Context, ArgSize, Termination, !IO),
	termination__make_opt_int_procs(PredId, ProcIds, ProcTable,
		PredOrFunc, SymName, Context, !IO).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "termination.m".

%----------------------------------------------------------------------------%
:- end_module termination.
%----------------------------------------------------------------------------%
