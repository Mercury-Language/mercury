%-----------------------------------------------------------------------------
%
% Copyright (C) 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------
%
% termination.m
% Main author: crs.
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
% Currently, this implementation assumes that all c_code terminates.
% It also fails to prove termination for any predicate that involves higher
% order calls.
%
% The termination analysis may use a number of different norms to calculate 
% the size of a term.  These are set by using the --termination-norm string
% option.  To add a new norm, the following files must be modified:
% globals.m 		To change the termination_norm type and
% 			convert_termination_norm predicate.
% handle_options.m 	To change the error message that is produced when
% 			an incorrect argument is given to
% 			--termination-norm.
% term_util.m		To change the functor_norm predicate and change the
% 			functor_alg type.
% termination.m		To change the set_functor_info predicate.
% 			
%-----------------------------------------------------------------------------

:- module termination.

:- interface.

:- import_module io.
:- import_module hlds_module, term_util.

% The top level predicate.  This controls all of the termination analysis
:- pred termination__pass(module_info, module_info, io__state, io__state).
:- mode termination__pass(in, out, di, uo) is det.

% This predicate prints out a termination structure.
:- pred termination__out(termination, io__state, io__state).
:- mode termination__out(in, di, uo) is det.

% This predicate prints out the used_args structure
:- pred termination__out_used_args(termination, io__state, io__state). 
:- mode termination__out_used_args(in, di, uo) is det.

% This predicate prints out the termination constant
:- pred termination__out_const(termination, io__state, io__state).
:- mode termination__out_const(in, di, uo) is det.

% This predicate prints out whether or not a predicate terminates
% The possible values are: (yes/dont_know/not_set).
:- pred termination__out_terminates(termination, io__state, io__state).
:- mode termination__out_terminates(in, di, uo) is det.

% This predicate outputs termination_info pragmas which are used in .trans_opt
% and .opt files.
:- pred termination__output_pragma_termination_info(pred_or_func, sym_name,
	argument_modes, termination, term__context, io__state, io__state).
:- mode termination__output_pragma_termination_info(in, in, in, in, in, 
	di, uo) is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module map, std_util, bool, int, char, string, relation.
:- import_module list, require, bag, set, term.

:- import_module inst_match, passes_aux, options, globals, prog_data.
:- import_module hlds_data, hlds_pred, hlds_goal, dependency_graph.
:- import_module mode_util, hlds_out, code_util, prog_out.
:- import_module mercury_to_mercury, varset, type_util, special_pred.
:- import_module term_pass1, term_pass2, term_errors.

%-----------------------------------------------------------------------------%

termination__pass(Module0, Module) -->
	globals__io_get_termination_norm(TermNorm),
	{ module_info_ensure_dependency_info(Module0, Module1) },
	{ module_info_predids(Module1, PredIds) },

	% Process builtin predicates.
	check_preds(PredIds, Module1, Module2),

	% Find the functor_info.
	{ set_functor_info(TermNorm, Module2, FunctorInfo) },

	proc_inequalities(Module2, FunctorInfo, Module3),

	termination(Module3, FunctorInfo, Module),
	
	globals__io_lookup_bool_option(make_optimization_interface,
		MakeOptInt),
	( { MakeOptInt = yes } ->
		termination__make_opt_int(PredIds, Module)
	;
		[]
	).

% This predicate sets the functor info depending on the value of the
% termination_norm option. The functor info field stores the weight which
% is associated with each functor, and may contain information about which
% subterms contribute to the size of that functor.
:- pred set_functor_info(globals__termination_norm, module_info, functor_info).
:- mode set_functor_info(in, in, out) is det.
set_functor_info(total, _Module, total).
set_functor_info(simple, _Module, simple).
set_functor_info(num_data_elems, Module, use_map_and_args(WeightMap)) :-
	find_weights(Module, WeightMap).
set_functor_info(size_data_elems, Module, use_map(WeightMap)) :-
	find_weights(Module, WeightMap).

%-----------------------------------------------------------------------------%
% These termination__out* predicates are used to print out the
% termination_info pragmas.  If they are changed, then prog_io_pragma.m
% must also be changed so that it can parse the resulting pragma
% termination_info declarations.
% XXX could these predicates be replaced by calls to io__write?

termination__out(Termination) -->
	termination__out_terminates(Termination),
	io__write_string("("),
	termination__out_const(Termination),
	io__write_string(")").

termination__out_used_args(term(_Const, _Term, MaybeUsedArgs, _)) -->
	( 	
		{ MaybeUsedArgs = yes([]) },
		io__write_string("yes([])") 
	;
		{ MaybeUsedArgs = yes([UsedArg | UsedArgs]) },
		io__write_string("yes([ "),
		io__write(UsedArg),
		termination__out_used_args_2(UsedArgs),
		io__write_string(" ])")
	;
		{ MaybeUsedArgs = no }, 
		io__write_string("no")
	).

:- pred termination__out_used_args_2(list(bool), io__state, io__state). 
:- mode termination__out_used_args_2(in, di, uo) is det.
termination__out_used_args_2([]) --> [].
termination__out_used_args_2([ UsedArg | UsedArgs ]) -->
	io__write_string(", "),
	io__write(UsedArg),
	termination__out_used_args_2(UsedArgs).

termination__out_terminates(term(_, Term, _, _)) -->
	termination__out_terminates_2(Term).

:- pred termination__out_terminates_2(terminates, io__state, io__state).
:- mode termination__out_terminates_2(in, di, uo) is det.
termination__out_terminates_2(not_set) --> 
	io__write_string("not_set").
termination__out_terminates_2(dont_know) --> 
	io__write_string("dont_know").
termination__out_terminates_2(yes) --> 
	io__write_string("yes").

termination__out_const(term(Const, _, _, _)) --> 
	termination__out_const_2(Const).

:- pred termination__out_const_2(term_util__constant, io__state, io__state).
:- mode termination__out_const_2(in, di, uo) is det.
termination__out_const_2(inf(_)) -->
	io__write_string("infinite").
termination__out_const_2(not_set) -->
	io__write_string("not_set").
termination__out_const_2(set(Int)) -->
	io__write_string("set("),
	io__write_int(Int),
	io__write_string(")").

termination__output_pragma_termination_info(PredOrFunc, SymName,
		argument_modes(InstTable, ModeList), Termination, Context) -->
	io__write_string(":- pragma termination_info("),
	{ varset__init(InitVarSet) },
	( 
		{ PredOrFunc = predicate },
		mercury_output_pred_mode_subdecl(InitVarSet, SymName, 
			ModeList, no, Context, InstTable)
	;
		{ PredOrFunc = function },
		{ pred_args_to_func_args(ModeList, FuncModeList, RetMode) },
		mercury_output_func_mode_subdecl(InitVarSet, SymName, 
			FuncModeList, RetMode, no, Context, InstTable)
	),
	io__write_string(", "),
	termination__out_const(Termination),
	io__write_string(", "),
	termination__out_terminates(Termination),
	io__write_string(", "),
	termination__out_used_args(Termination),
	io__write_string(").\n").
	
%-----------------------------------------------------------------------------%

:- pred check_preds(list(pred_id), module_info, module_info, 
	io__state, io__state).
:- mode check_preds(in, in, out, di, uo) is det.

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
check_preds([], Module, Module, State, State).
check_preds([PredId | PredIds] , Module0, Module, State0, State) :-
	globals__io_lookup_bool_option(make_optimization_interface,
		MakeOptInt, State0, State1),
	module_info_preds(Module0, PredTable0),
	map__lookup(PredTable0, PredId, PredInfo0),
	pred_info_import_status(PredInfo0, ImportStatus),
	pred_info_context(PredInfo0, Context),
	pred_info_procedures(PredInfo0, ProcTable0),
	pred_info_get_markers(PredInfo0, Markers),
	map__keys(ProcTable0, ProcIds),
	( 
		% It is possible for compiler generated/mercury builtin
		% predicates to be imported or locally defined, so they
		% must be covered here, seperatly.
		set_compiler_gen_terminates(ProcIds, PredInfo0, 
			Module0, ProcTable0, ProcTable1)
	->
		ProcTable2 = ProcTable1
	; % else if
		( ImportStatus = exported
		; ImportStatus = local
		; ImportStatus = pseudo_exported
		)
	->
		( check_marker(Markers, terminates) ->
			MaybeFind = no,
			ReplaceTerminate = yes,
			MaybeError = no,
			change_procs_terminate(ProcIds, MaybeFind, 
				ReplaceTerminate, MaybeError, 
				ProcTable0, ProcTable2)
		;
			ProcTable2 = ProcTable0
		)
	; %else if
		( ImportStatus = imported
		; ImportStatus = opt_imported
		; ImportStatus = pseudo_imported  % should this be here?
		)
	->
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
			change_procs_terminate(ProcIds, no, yes, no, 
				 ProcTable0, ProcTable1)
		;
			% go through, changing each 'not_set' to 'dont_know'
			MaybeFind = yes(not_set),
			ReplaceTerminate = dont_know,
			MaybeError = yes(Context - imported_pred),
			change_procs_terminate(ProcIds, MaybeFind,
				ReplaceTerminate, MaybeError, ProcTable0,
				ProcTable1)
				
		),
		MaybeFindConst = yes(not_set),
		ConstError = imported_pred,
		ReplaceConst = inf(Context - ConstError),
		change_procs_const(ProcIds, MaybeFindConst, ReplaceConst,
			ProcTable1, ProcTable2)
	;
%		( ImportStatus = abstract_imported
%		; ImportStatus = abstract_exported
%		),
		% This should not happen, as procedures are being processed
		% here, and these import_status' refer to abstract types.
		error("termination__check_preds: Unexpected import status of a predicate")
	),
	( check_marker(Markers, does_not_terminate) ->
		MaybeFind1 = no,
		ReplaceTerminate1 = dont_know,
		MaybeError1 = yes(Context - does_not_term_pragma(PredId)),
		change_procs_terminate(ProcIds, MaybeFind1,
			ReplaceTerminate1, MaybeError1, ProcTable2, ProcTable)
	;
		ProcTable = ProcTable2
	),
	pred_info_set_procedures(PredInfo0, ProcTable, PredInfo),
	map__set(PredTable0, PredId, PredInfo, PredTable),
	module_info_set_preds(Module0, PredTable, Module1),
	check_preds(PredIds, Module1, Module, State1, State).

% This predicate checks each ProcId in the list to see if it is a compiler
% generated predicate, or a mercury_builtin predicate.  If it is, then the
% compiler sets the termination property of the ProcIds accordingly.
:- pred set_compiler_gen_terminates(list(proc_id), pred_info, module_info,
	proc_table, proc_table).
:- mode set_compiler_gen_terminates(in, in, in, in, out) is semidet.
set_compiler_gen_terminates([], _PredInfo, _Module, ProcTable, ProcTable).
set_compiler_gen_terminates([ ProcId | ProcIds ], PredInfo, Module, 
		ProcTable0, ProcTable) :-
	( code_util__predinfo_is_builtin(PredInfo) ->
		set_builtin_terminates([ ProcId | ProcIds ], PredInfo, 
			Module, ProcTable0, ProcTable)
	;
		pred_info_name(PredInfo, Name),
		pred_info_arity(PredInfo, Arity),
		(
			special_pred_name_arity(SpecPredId0, Name, _, Arity),
			pred_info_module(PredInfo, ModuleName),
			ModuleName = "mercury_builtin"
		->
			SpecialPredId = SpecPredId0
		;
			special_pred_name_arity(SpecialPredId, _, Name, Arity)
		),
		map__lookup(ProcTable0, ProcId, ProcInfo0),
		proc_info_headvars(ProcInfo0, HeadVars),
		termination__special_pred_id_to_termination(SpecialPredId, 
			HeadVars, Termination),
		% The procedure is a compiler generated procedure, so enter the
		% data in the proc_info.
		proc_info_set_termination(ProcInfo0, Termination, ProcInfo),
		map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable1),
		set_compiler_gen_terminates(ProcIds, PredInfo, Module,
			ProcTable1, ProcTable)
	).

:- pred termination__special_pred_id_to_termination(special_pred_id, 
	list(var), termination).
:- mode termination__special_pred_id_to_termination(in, in, out) is det.
termination__special_pred_id_to_termination(compare, HeadVars, Termination) :-
	term_util__make_bool_list(HeadVars, [no, no, no], OutList),
	Termination = term(set(0), yes, yes(OutList), no).
termination__special_pred_id_to_termination(unify, HeadVars, Termination) :-
	term_util__make_bool_list(HeadVars, [yes, yes], OutList),
	Termination = term(set(0), yes, yes(OutList), no).
termination__special_pred_id_to_termination(index, HeadVars, Termination) :-
	term_util__make_bool_list(HeadVars, [no, no], OutList),
	Termination = term(set(0), yes, yes(OutList), no).

% The list of proc_ids must refer to builtin predicates.  This predicate
% sets the termination information of builtin predicates.
:- pred set_builtin_terminates(list(proc_id), pred_info, module_info, 
	proc_table, proc_table).
:- mode set_builtin_terminates(in, in, in, in, out) is det.
set_builtin_terminates([], _, _, ProcTable, ProcTable).
set_builtin_terminates([ProcId | ProcIds], PredInfo, Module, ProcTable0, 
			ProcTable) :-
	map__lookup(ProcTable0, ProcId, ProcInfo0), 
	( attempt_set_proc_const(Module, PredInfo, ProcInfo0) ->
		% For attempt_set_proc_const to succeed on a procedure, the
		% output variables of that procedure must all be of a type
		% whose norm will be zero.  Hence the size of the output
		% variables will all be 0, independent of the size of the
		% input variables.  Therefore, as the size of the output
		% variables do not depend on the size of the input
		% variables, UsedArgs should be set to yes([no, no, ...]).
		Const = set(0),
		proc_info_headvars(ProcInfo0, HeadVars),
		term_util__make_bool_list(HeadVars, [], Bools),
		UsedArgs = yes(Bools)
	;
		pred_info_context(PredInfo, Context),
		Const = inf(Context - is_builtin),
		UsedArgs = no
	),
	Term = term(Const, yes, UsedArgs, no),
	proc_info_set_termination(ProcInfo0, Term, ProcInfo),
	map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable1),
	set_builtin_terminates(ProcIds, PredInfo, Module, ProcTable1, 
		ProcTable).


% For attempt_set_proc_const to succeed, the output variables of
% that procedure must all be of a type that will always have a norm of 0.
% Hence the size of the output vars will always be 0, independent of the
% size of the input variables.   Therefore, if attempt_set_proc_const
% succeeds on a predicate, then the termination constant of that predicate
% can be set to 0, independantly of what the predicate does.
:- pred attempt_set_proc_const(module_info, pred_info, proc_info).
:- mode attempt_set_proc_const(in, in, in) is semidet.
attempt_set_proc_const(Module, PredInfo, ProcInfo) :-
	pred_info_arg_types(PredInfo, _, TypeList),
	proc_info_argmodes(ProcInfo, argument_modes(InstTable, Modes)),
	attempt_set_proc_const_2(TypeList, Modes, InstTable, Module). 

:- pred attempt_set_proc_const_2(list(type), list(mode), inst_table,
		module_info).
:- mode attempt_set_proc_const_2(in, in, in, in) is semidet.
attempt_set_proc_const_2([], [], _, _).
attempt_set_proc_const_2([], [_|_], _, _) :- 
	error("termination__attempt_set_proc_const_2: Unmatched variables.").
attempt_set_proc_const_2([_|_], [], _, _) :- 
	error("termination:attempt_set_proc_const_2: Unmatched variables").
attempt_set_proc_const_2([Type | Types], [Mode | Modes], InstTable, Module) :-
	( mode_is_input(InstTable, Module, Mode) ->
		% The variable is an input variables, so its size is
		% irrelevant.
		attempt_set_proc_const_2(Types, Modes, InstTable, Module)
	;
		classify_type(Type, Module, TypeCategory),
		% User_type could be a type_info, which should be called
		% size 0.  This is not a big problem, as most type_infos
		% are input.  
		TypeCategory \= user_type, 
		% This could be changed, by looking up the polymorphic type, 
		% and seeing if it is recursive, or could it?
		TypeCategory \= polymorphic_type, 
		TypeCategory \= pred_type,
		attempt_set_proc_const_2(Types, Modes, InstTable, Module)
	).
		
% This predicate changes the terminate property of a list of procedures.
% change_procs_terminate(ProcList,MaybeFind, Replace, MaybeError,
% 		ProcTable, ProcTable).
% If MaybeFind is no, then this predicate changes the terminates and
% MaybeError field of all the procedures passed to it.  If MaybeFind is set
% to yes(OldTerminates) then the terminates and MaybeError field of a
% procedure is only changed if the Terminates field was OldTerminates.
:- pred change_procs_terminate(list(proc_id), maybe(terminates), terminates,
	maybe(term_errors__error), proc_table, proc_table).
:- mode change_procs_terminate(in, in, in, in, in, out) is det.
change_procs_terminate([], _Find, _Replace, _, ProcTable, ProcTable).
change_procs_terminate([ProcId | ProcIds], MaybeFind, Replace, MaybeError, 
		ProcTable0, ProcTable) :-
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_termination(ProcInfo0, Termination),
	( 
		Termination = term(Const, Terminates, UsedArgs, _Error),
		( 
			MaybeFind = yes(Terminates)
		;
			MaybeFind = no
		)

	->
		Termination1 = term(Const, Replace, UsedArgs, MaybeError),
		proc_info_set_termination(ProcInfo0, Termination1, ProcInfo),
		map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable1)
	;
		ProcTable1 = ProcTable0
	),
	change_procs_terminate(ProcIds, MaybeFind, Replace, MaybeError,
		ProcTable1, ProcTable).

% This predicate changes the termination constant of a list of procedures.
% change_procs_const(ProcList,MaybeFind, Replace, ProcTable, ProcTable).
% If MaybeFind is no, then this predicate changes the constant
% field of all the procedures passed to it.  If MaybeFind is set
% to yes(OldConst) then the termination constant of a procedure is
% only changed if the Constant field was OldConst.
:- pred change_procs_const(list(proc_id), maybe(term_util__constant),
	term_util__constant, proc_table, proc_table).
:- mode change_procs_const(in, in, in, in, out) is det.
change_procs_const([], _Find, _Replace, ProcTable, ProcTable).
change_procs_const([ProcId | ProcIds], MaybeFind, Replace, ProcTable0, 
		ProcTable) :-
	map__lookup(ProcTable0, ProcId, ProcInfo0),
	proc_info_termination(ProcInfo0, Termination),
	( 
		Termination = term(Const, Term, UsedArgs, MaybeError),
		(
			MaybeFind = yes(Const)
		;
			MaybeFind = no
		)
	->
		Termination1 = term(Replace, Term, UsedArgs, MaybeError),
		proc_info_set_termination(ProcInfo0, Termination1, ProcInfo),
		map__det_update(ProcTable0, ProcId, ProcInfo, ProcTable1)
	;
		ProcTable1 = ProcTable0
	),
	change_procs_const(ProcIds, MaybeFind, Replace, ProcTable1, ProcTable).


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% These predicates are used to add the termination_info pragmas to the .opt
% file.  It is oftern better to use the .trans_opt file, as it gives
% much better accuracy.  The two files are not mutually exclusive, and
% termination information may be stored in both.

:- pred termination__make_opt_int(list(pred_id), module_info, io__state, 
		io__state).
:- mode termination__make_opt_int(in, in, di, uo) is det.
termination__make_opt_int(PredIds, Module) -->
	{ module_info_name(Module, ModuleName) },
	{ string__append(ModuleName, ".opt.tmp", OptFileName) },
	io__open_append(OptFileName, OptFileRes),
	( { OptFileRes = ok(OptFile) } ->
		io__set_output_stream(OptFile, OldStream),
		termination__make_opt_int_preds(PredIds, Module),
		io__set_output_stream(OldStream, _),
		io__close_output(OptFile)
	;
		% failed to open the .opt file for processing
		io__write_strings(["Cannot open `",
			OptFileName, "' for output\n"]),
		io__set_exit_status(1)
	).

:- pred termination__make_opt_int_preds(list(pred_id), module_info, 
	io__state, io__state).
:- mode termination__make_opt_int_preds(in, in, di, uo) is det.
termination__make_opt_int_preds([], _Module) --> [].
termination__make_opt_int_preds([ PredId | PredIds ], Module) -->
	{ module_info_preds(Module, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_import_status(PredInfo, ImportStatus) },
	( 
		{ ImportStatus = exported },
		{ \+ code_util__compiler_generated(PredInfo) }
	->
		{ pred_info_name(PredInfo, PredName) },
		{ pred_info_procedures(PredInfo, ProcTable) },
		{ pred_info_procids(PredInfo, ProcIds) },
		{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
		{ pred_info_module(PredInfo, ModuleName) },
		{ pred_info_context(PredInfo, Context) },
		{ SymName = qualified(ModuleName, PredName) },
		termination__make_opt_int_procs(PredId, ProcIds, ProcTable, 
			PredOrFunc, SymName, Context)
	;
		[]
	),
	termination__make_opt_int_preds(PredIds, Module).
	
:- pred termination__make_opt_int_procs(pred_id, list(proc_id), proc_table,
	pred_or_func, sym_name, term__context, io__state, io__state).
:- mode termination__make_opt_int_procs(in, in, in, in, in, in, di, uo) is det.
termination__make_opt_int_procs(_PredId, [], _, _, _, _) --> [].
termination__make_opt_int_procs(PredId, [ ProcId | ProcIds ], ProcTable, 
		PredOrFunc, SymName, Context) -->
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	{ proc_info_termination(ProcInfo, Termination) },
	{ proc_info_declared_argmodes(ProcInfo, ArgModes) },
	termination__output_pragma_termination_info(PredOrFunc, SymName,
		ArgModes, Termination, Context),
	termination__make_opt_int_procs(PredId, ProcIds, ProcTable, 
		PredOrFunc, SymName, Context).

