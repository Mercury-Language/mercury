%-----------------------------------------------------------------------------%
%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% term_errors.m
% Main author: crs.
% 
% This module prints out the various error messages that are produced by
% termination.m
%
%-----------------------------------------------------------------------------%

:- module term_errors.

:- interface.
:- import_module io, bag, std_util, term, list, bool.
:- import_module hlds_module.

% term_errors__output(PredId, ProcId, Module, Success, IO0, IO).
% 	This is used to print out the errors found by termination analysis.
% 	Success returns yes if an error was successfully printed out.
% 	Success will be no if there was no termination error for that
% 	procedure.
:- pred term_errors__output(pred_id, proc_id, module_info, bool,
		io__state, io__state).
:- mode term_errors__output(in, in, in, out, di, uo) is det.


% term_errors__output_const_error(PredId, ProcId, Module, Success, IO0, IO).
% 	This prints out any errors which occured when trying to set the
% 	termination constant.  An error message will only be available if
% 	the termination constant is set to 'inf'.
% 	Success returns yes if an error was successfully printed out.
% 	Success will be no if there was no termination error for that
% 	procedure.
:- pred term_errors__output_const_error(pred_id, proc_id, module_info, bool,
		io__state, io__state).
:- mode term_errors__output_const_error(in, in, in, out, di, uo) is det.

% This is used to print out error messages for the hlds dumps.  These are
% much more concise than the normal error messages.
:- pred term_errors__output_hlds(pred_id, proc_id, module_info, 
	io__state, io__state).
:- mode term_errors__output_hlds(in, in, in, di, uo) is det.

:- type term_errors__error == pair(term__context, termination_error).
% With these error messages, they do not necessarily need to involve the
% procedure that they are assigned to.  It is possible that an error
% occured when processing other predicates in the same SCC, and therefore
% the termination (or termination constant) of this predicate was set to
% dont_know (or infinity), even though the error occured in a different
% predicate.
:- type termination_error
			% A recursive call is made with variables that are 
			% strictly larger than in the head.  Note that 
			% the recursive call may be indirect, so this does 
			% not neccessarily indicate non-termination.
			% The first PPId is the calling proc.  The second
			% PPId is the called procedure.
	--->	positive_value(pred_proc_id, pred_proc_id)
	;	horder_call
	;	pragma_c_code
	;	imported_pred
			% dont_know_proc_called(CallerProc, CalleeProc)  
			% A call was made from CallerProc to CalleeProc,
			% where the termination constant of the CalleeProc
			% is set to dont_know.
	;	dont_know_proc_called(pred_proc_id, pred_proc_id)
			% horder_args(CallerProc, CalleeProc)
			% This error message indicates that the CallerProc
			% called the CalleeProc where some of the arguments
			% are of a higher order type.
	;	horder_args(pred_proc_id, pred_proc_id)
	;	inf_termination_const(pred_proc_id, pred_proc_id)
			% not_subset(Proc, SupplierVariables, InHeadVariables)
			% This error occurs when the Supplier variables
			% (either Recursive-input suppliers or Output
			% suppliers, depending on whether the error was
			% associated with a dont_know or with a constant of
			% infinity) is not a subset of the input head
			% variables.
	;	not_subset(pred_proc_id, bag(var), bag(var))
	;	not_dag
	;	no_eqns
	;	lpsolve_failed
	;	call_in_single_arg(pred_proc_id)
			% single argument analysis did not find a head
			% variable that was decreasing in size. 
	;	single_arg_failed(term_errors__error)
			% single_arg_failed(ReasonForNormalAnalysisFailing,
			% 	ReasonForSingleArgAnalysisFailing)
	;	single_arg_failed(term_errors__error, term_errors__error)
			% the termination constant of a builtin predicate
			% is set to infinity if the types of the builtin
			% predicate may have a norm greater than 0.
	;	is_builtin
	;	does_not_term_pragma(pred_id).

% eqn_soln are used to report the results from solving the equations
% created in the first pass.  The first 4 (optimal - failure) represent
% output states from lp_solve. 
:- type eqn_soln
	---> 	optimal		
	;	infeasible
	;	unbounded
	;	failure
	;	fatal_error 	% unable to open a file, or make a system call
	;	parse_error	% unable to parse the output from lp_solve
	;	solved(list(pair(pred_proc_id, int))).

% An error is considered a simple error if it is likely that the error is
% caused by the analysis failing, instead of being due to a programming
% error.
:- pred simple_error(term_errors__termination_error).
:- mode simple_error(in) is semidet.

:- implementation.

:- import_module hlds_out, prog_out, hlds_pred, passes_aux, require.
:- import_module mercury_to_mercury, term_util, bag, options, globals.

simple_error(horder_call).
simple_error(pragma_c_code).
simple_error(imported_pred).
simple_error(dont_know_proc_called(_, _)).
simple_error(call_in_single_arg(_)).
simple_error(horder_args(_, _)).
simple_error(single_arg_failed(_Context - Error)) :- simple_error(Error).
simple_error(single_arg_failed(_Con1 - Err1, _Con2 - Err2)) :-
	simple_error(Err1), simple_error(Err2).
simple_error(does_not_term_pragma(_)).

term_errors__output(PredId, ProcId, Module, Success) -->
	{ module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo) },
	{ proc_info_termination(ProcInfo, Termination) },
	{ Termination = term(_Const, _Terminates, _UsedArgs, MaybeError) },
	( { MaybeError = yes(Context - Error) } ->
		prog_out__write_context(Context),
		io__write_string("Termination of "),
		hlds_out__write_pred_id(Module, PredId),
		io__nl,
		prog_out__write_context(Context),
		io__write_string("  not proved because "),
		{ ConstErrorOutput = no },
		{ ForHLDSDump = no },
		term_errors__output_2(PredId, ProcId, Module, ConstErrorOutput,
			ForHLDSDump, Context - Error),
		{ Success = yes }
	;
		{ Success = no }
	).

term_errors__output_const_error(PredId, ProcId, Module, Success) -->
	{ module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo) },
	{ proc_info_termination(ProcInfo, Termination) },
	{ Termination = term(Const, _Terminates, _UsedArgs, _MaybeError) },
	( { Const = inf(Context - Error) } ->
		prog_out__write_context(Context),
		io__write_string("Termination constant of "),
		hlds_out__write_pred_id(Module, PredId),
		io__nl,
		prog_out__write_context(Context),
		io__write_string("  set to infinity because "),
		{ ConstErrorOutput = yes },
		{ ForHLDSDump = no },
		term_errors__output_2(PredId, ProcId, Module, ConstErrorOutput,
			ForHLDSDump, Context - Error),
		{ Success = yes }
	; 
		{ Success = no }
	).

term_errors__output_hlds(PredId, ProcId, Module) -->
	{ module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo) },
	{ proc_info_termination(ProcInfo, Termination) },
	{ Termination = term(Const, _Terminates, _UsedArgs, MaybeError) },
	{ ForHLDSDump = yes },
	( { MaybeError = yes(TermContext - TermError) } ->
		io__write_string("% "),
		prog_out__write_context(TermContext),
		io__write_string("Termination not proved because "),
		{ ConstErrorOutput0 = no },
		term_errors__output_2(PredId,ProcId, Module, ConstErrorOutput0,
			ForHLDSDump, TermContext - TermError)
	;
		[]
	),
	( { Const = inf(ConstContext - ConstError) } ->
		io__write_string("% "),
		prog_out__write_context(ConstContext),
		io__write_string("Termination const set to inf because "),
		{ ConstErrorOutput1 = yes },
		term_errors__output_2(PredId, ProcId, Module, ConstErrorOutput1,
			ForHLDSDump, ConstContext - ConstError)
	;
		[]
	).
	

:- pred term_errors__output_same_SCC(pred_id, module_info, term__context,
	bool, io__state, io__state).
:- mode term_errors__output_same_SCC(in, in, in, in, di, uo) is det.
term_errors__output_same_SCC(PredId, Module, Context, ForHLDSDump) -->
	io__write_string("it is in the same SCC as the\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_pred_id(Module, PredId).


% term_errors__output_2(PredId, ProcId, Module, ConstErrorOutput,
% 	ForHLDSDump, Error, IO0, IO).
% If this predicate is called from term_errors__output_const_error, then
% ConstErrorOutput should be set to `yes' to indicate that the error
% message should describe why the constant was set to infinity.
% If ConstErrorOutput is set to `no' then term_errors__output_2 describes
% why the analysis could not prove termination.
%
% If ForHLDSDump is set to yes, then a % must be placed at the beginning of
% each line, because the output is for the HLDS dump.
% 
% This predicate is used by both term_errors__output() and
% term_errors__output_const_error() to print out the reason for the error.
% Before calling output_2, term_errors__output prints out:
% myfile.m:300: Termination of predicate `myfile:yourpredicate/3' 
% myfile.m:300:   not proved because 
%
% and term_errors__output_const_error prints out:
% myfile.m:300: Termination constant of function `myfile:myfunction/6' 
% myfile.m:300:   set to infinity because 
%
:- pred term_errors__output_2(pred_id, proc_id, module_info, bool, bool,
	term_errors__error, io__state, io__state).
:- mode term_errors__output_2(in, in, in, in, in, in, di, uo) is det.
term_errors__output_2(PredId, _ProcId, Module, _ConstErrorOutput, ForHLDSDump,
		Context - positive_value(CallerPPId, CalledPPId)) -->	
	{ CalledPPId = proc(CalledPredId, _CalledProcId) },
	{ CallerPPId = proc(CallerPredId, _CallerProcId) },
	( { PredId = CallerPredId } ->
		io__write_string("it contains a ")
	;
		term_errors__output_same_SCC(PredId, Module, Context, 
			ForHLDSDump),
		io__write_string(" which contains a ")
	),
	( { PredId = CalledPredId } ->
		io__write_string("directly\n"),
		maybe_write_string(ForHLDSDump, "% "),
		prog_out__write_context(Context),
		io__write_string("  recursive call ")
	;
		io__write_string("recursive\n"),
		maybe_write_string(ForHLDSDump, "% "),
		prog_out__write_context(Context),
		io__write_string("call to "),
		hlds_out__write_pred_id(Module, CalledPredId),
		io__nl,
		maybe_write_string(ForHLDSDump, "% "),
		prog_out__write_context(Context),
		io__write_string("  ")
	),
	io__write_string("with the size of the variables increased.\n").

term_errors__output_2(_PredId, _ProcId, _Module, _ConstErrOutput, _ForHLDSDump,
		_Context - horder_call) -->
	io__write_string("it contains a higher order call\n").

term_errors__output_2(_PredId, _ProcId, _Module, _ConstErrOutput, _ForHLDSDump,
		_Context - pragma_c_code) -->
	io__write_string("it contains a pragma c_code() declaration\n").

term_errors__output_2(PredId, _ProcId, Module, _ConstErrorOutput, ForHLDSDump,
		Context - dont_know_proc_called(CallerPPId, CalleePPId)) -->
	{ CallerPPId = proc(CallerPredId, _CallerProcId) },
	{ CalleePPId = proc(CalleePredId, _CalleeProcId) },
	( { PredId = CallerPredId } ->
		io__write_string("it calls the ")
	;
		term_errors__output_same_SCC(CallerPredId, Module, Context,
			ForHLDSDump),
		io__write_string("which calls the ")
	),
	io__nl,
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_pred_id(Module, CalleePredId),
	io__nl,
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  which could not be proved to terminate\n").

term_errors__output_2(_PredId, _ProcId, _Module, _ConstErrOutput, _ForHLDSDump,
		_Context - imported_pred) -->
	io__write_string("it was imported.\n").

term_errors__output_2(PredId, _ProcId, Module, _ConstErrorOutput, ForHLDSDump,
		Context - horder_args(CallerPPId, CalleePPId)) -->
	% OtherPPId may refer to the current Predicate, or it may refer to
	% another predicate in the same SCC
	{ CallerPPId = proc(CallerPredId, _CallerProcId) },
	{ CalleePPId = proc(CalleePredId, _CalleeProcId) },

	( { PredId = CallerPredId } ->
		io__write_string("it calls the ")
	;
		term_errors__output_same_SCC(CallerPredId, Module, Context,
			ForHLDSDump),
		io__write_string("which calls the")
	),
	io__nl,
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_pred_id(Module, CalleePredId),
	io__nl,
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string(
		"  where the call contains higher order argument(s)\n").

term_errors__output_2(PredId, _ProcId, Module, ConstErrorOutput, ForHLDSDump,
		Context - inf_termination_const(CallerPPId, CalleePPId)) -->
	{ CallerPPId = proc(CallerPredId, _CallerProcId) },
	{ CalleePPId = proc(CalleePredId, CalleeProcId) },
	( { PredId = CallerPredId } ->
		io__write_string("it calls the ")
	;
		term_errors__output_same_SCC(CallerPredId, Module, Context,
			ForHLDSDump),
		io__write_string("which calls the ")
	),
	io__nl,
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_pred_id(Module, CalleePredId),
	io__nl,
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string(
		"  which has a termination constant of infinity\n"),
	( 
		{ ForHLDSDump = no },
		{ ConstErrorOutput = no }
	->
		{ module_info_pred_proc_info(Module, CalleePredId, 
			CalleeProcId, _, CalleeProcInfo) },
		{ proc_info_termination(CalleeProcInfo, CalleeTermination) },
		{ CalleeTermination = term(CalleeConst, _, _, _) },
		globals__io_lookup_bool_option(verbose_check_termination, 
			VerboseErrors),
		(
			{ CalleeConst = inf(CalleeContext - CalleeConstError)},
			( 
				{ \+ simple_error(CalleeConstError) }
			;	
				{ VerboseErrors = yes }
			)
		->
			maybe_write_string(ForHLDSDump, "% "),
			prog_out__write_context(CalleeContext),
			io__write_string("  The termination constant of that predicate was set to\n"),
			maybe_write_string(ForHLDSDump, "% "),
			prog_out__write_context(CalleeContext),
			io__write_string("  infinity because "),
			{ NewConstErrorOutput = yes },
			term_errors__output_2(CalleePredId, CalleeProcId, 
				Module, NewConstErrorOutput, ForHLDSDump, 
				CalleeContext - CalleeConstError)
		;
			[]
		) 
	;
		[]
	).

% If hlds dump, should print out variables in numerical form
% If verbose errors requested (-E), should list variables
term_errors__output_2(PredId, _ProcId, Module, ConstErrorOutput, ForHLDSDump,
		Context - not_subset(OtherPPId, 
		SupplierVarsBag, HeadVarsBag)) -->
	{ OtherPPId = proc(OtherPredId, OtherProcId) },
	{ module_info_pred_proc_info(Module, OtherPredId, OtherProcId, 
		OtherPredInfo, OtherProcInfo) },
	{ pred_info_get_is_pred_or_func(OtherPredInfo, OtherPredOrFunc) },
	( { OtherPredId = PredId } ->
		[]
	;
		term_errors__output_same_SCC(OtherPredId, Module, Context,
			ForHLDSDump),
		( { ConstErrorOutput = yes } ->
			io__write_string(
				". The termination constant of that\n"),
			maybe_write_string(ForHLDSDump, "% "),
			prog_out__write_context(Context),
			io__write_string("  "),
			hlds_out__write_pred_or_func(OtherPredOrFunc),
			io__write_string(" was set to infinity because ")
		;
			io__write_string(". Termination of that\n"),
			maybe_write_string(ForHLDSDump, "% "),
			prog_out__write_context(Context),
			io__write_string("  "),
			hlds_out__write_pred_or_func(OtherPredOrFunc),
			io__write_string(" could not be proved because ")
		)
	),
	io__write_string("the analysis\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  found that the set of "),
	( { ConstErrorOutput = yes } ->
		io__write_string("output ")
	;
		io__write_string("recursive input ")
	),
	io__write_string("supplier variables\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  was not a subset of the head variables of the "),
	hlds_out__write_pred_or_func(OtherPredOrFunc),
	io__write_string(".\n"),
		% Print out the variables as calculated.
	{ proc_info_variables(OtherProcInfo, VarSet) },
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  The "),
	( { ConstErrorOutput = yes } ->
		io__write_string("output ")
	;
		io__write_string("recursive input ")
	),
	io__write_string("supplier variables are\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  "),
	{ bag__to_list(SupplierVarsBag, SupplierVars) },
	mercury_output_vars(SupplierVars, VarSet, no),
	io__nl,
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  The input head variables are\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  "),
	{ bag__to_list(HeadVarsBag, HeadVars) },
	mercury_output_vars(HeadVars, VarSet, no),
	io__nl.

term_errors__output_2(_PredId, _ProcId, _Module, _ConstErrorOutput, ForHLDSDump,
		Context - not_dag) -->
	io__write_string("there was a cycle in the call graph of\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string(
		"  this SCC where the variables did not decrease in size\n").

term_errors__output_2(_PredId, _ProcId, _Module, ConstErrorOutput, ForHLDSDump,
		Context - no_eqns)-->
	{ require(unify(ConstErrorOutput, yes),
		"Unexpected value in term_errors__output_2(no_eqns)") },
	io__write_string("the analysis was unable to form any\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string(
		"  constraints between the arguments of the predicates\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  and functions in this SCC\n").

term_errors__output_2(_PredId, _ProcId, _Module, _ConstErrorOutput, ForHLDSDump,
		Context - lpsolve_failed) -->
	io__write_string("the constraint solver "),
	io__write_string("found the\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  constraints that the analysis produced to be infeasible\n").

% call_in_single_arg will only be printed out as the second part of the
% single_arg_failed(NormErr, SingleErr) error.  Therefore, the following
% lines have already been printed out:
% Single argument termination analysis failed to
% prove termination because 
term_errors__output_2(_PredId, _ProcId, Module, _ConstErrorOutput, ForHLDSDump,
		Context - call_in_single_arg(PPId)) -->
	{ PPId = proc(CallPredId, _CallProcId) },
	io__write_string("it encountered a call to\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  "),
	hlds_out__write_pred_id(Module, CallPredId),
	io__nl,
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  which could not be processed.\n").

term_errors__output_2(PredId, ProcId, Module, ConstErrorOutput, ForHLDSDump,
		Context - single_arg_failed(Error)) -->
	term_errors__output_2(PredId, ProcId, Module, ConstErrorOutput, 
		ForHLDSDump, Error),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string(
		"  Single argument analysis failed to find a head variable\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  that was always decreasing in size.\n").

term_errors__output_2(PredId, ProcId, Module, ConstErrorOutput, ForHLDSDump, 
		_Context - single_arg_failed(NormErr, SingleArgErr)) -->
	{ NormErr = NormalContext - NormalError }, 
	{ SingleArgErr = SingleArgContext - SingleArgError },

		% single argument analysis is independent of finding the
		% termination constant.  The error for a termination
		% constant should never be single_arg_failed.
	{ require(unify(ConstErrorOutput, no),
		"Unexpected value in term_errors__output_2") },

	term_errors__output_2(PredId, ProcId, Module, ConstErrorOutput, 
		ForHLDSDump, NormalContext - NormalError),
	globals__io_lookup_bool_option(verbose_check_termination,
		VerboseErrors),
	( 
		{ SingleArgError \= NormalError },
		{ VerboseErrors = yes }
	->
		% Only output a single argument error if verbose errors are
		% enabled, and it is different from the normal error
		% message.
		maybe_write_string(ForHLDSDump, "% "),
		prog_out__write_context(SingleArgContext),
		io__write_string(
			"  Single argument termination analysis failed to\n"),
		maybe_write_string(ForHLDSDump, "% "),
		prog_out__write_context(SingleArgContext),
		io__write_string("  prove termination because "),
		term_errors__output_2(PredId, ProcId, Module, ConstErrorOutput, 
			ForHLDSDump, SingleArgContext - SingleArgError)
	;
		[]
	).

term_errors__output_2(_PredId, _ProcId, _Module, ConstErrorOutput, _ForHLDSDump,
		_Context - is_builtin) -->
	{ require(unify(ConstErrorOutput, yes), 
		"Unexpected value in term_errors:output_2") },
	io__write_string("it is a builtin predicate\n").

term_errors__output_2(PredId, _ProcId, Module, ConstErrorOutput, ForHLDSDump,
		Context - does_not_term_pragma(OtherPredId)) -->
	{ require(unify(ConstErrorOutput, no), 
		"Unexpected value in term_errors:output_2") },
	io__write_string("there was a `does_not_terminate'\n"),
	maybe_write_string(ForHLDSDump, "% "),
	prog_out__write_context(Context),
	io__write_string("  pragma defined on "),
	( { PredId = OtherPredId } ->
		{ module_info_pred_info(Module, PredId, PredInfo) },
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		io__write_string("this "),
		hlds_out__write_pred_or_func(PredOrFunc),
		io__nl
	;
		io__write_string("the "),
		hlds_out__write_pred_id(Module, OtherPredId),
		io__nl
	).

