%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mercury_compile.m.
% Main authors: fjh, zs.

% This is the top-level of the Mercury compiler.

% This module invokes the different passes of the compiler as appropriate.

%-----------------------------------------------------------------------------%

:- module mercury_compile.
:- interface.
:- import_module bool, string, io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

	% library modules
:- import_module int, list, map, set, std_util, dir, tree234, require.
:- import_module library, getopt, term, varset.

	% the main compiler passes (in order of execution)
:- import_module handle_options, prog_io, modules, make_hlds, hlds.
:- import_module undef_types, typecheck, undef_modes, modes.
:- import_module switch_detection, cse_detection, det_analysis, unique_modes.
:- import_module (lambda), polymorphism, higher_order, inlining, common.
:- import_module constraint, unused_args, dead_proc_elim, excess, liveness.
:- import_module follow_code, follow_vars, live_vars, arg_info, store_alloc.
:- import_module code_gen, optimize, llds.

	% miscellaneous compiler modules
:- import_module prog_util, hlds_out, dependency_graph.
:- import_module mercury_to_c, mercury_to_mercury, mercury_to_goedel.
:- import_module garbage_out, shapes.
:- import_module options, globals, passes_aux.

%-----------------------------------------------------------------------------%

main -->
	handle_options(MaybeError, Args, Link),
	main_2(MaybeError, Args, Link).

%-----------------------------------------------------------------------------%

:- pred main_2(maybe(string), list(string), bool, io__state, io__state).
:- mode main_2(in, in, in, di, uo) is det.

main_2(yes(ErrorMessage), _, _) -->
	usage_error(ErrorMessage).
main_2(no, Args, Link) -->
	globals__io_lookup_bool_option(help, Help),
	( { Help = yes } ->
		long_usage
	; { Args = [] } ->
		usage
	;
		{ strip_module_suffixes(Args, ModuleNames) },
		process_module_list(ModuleNames),
		io__get_exit_status(ExitStatus),
		( { ExitStatus = 0 } ->
			( { Link = yes } ->
				mercury_compile__link_module_list(ModuleNames)
			;
				[]
			)
		;
			% If we found some errors, but the user didn't enable
			% the `-E' (`--verbose-errors') option, give them a
			% hint about it.

			globals__io_lookup_bool_option(verbose_errors,
				VerboseErrors),
			( { VerboseErrors = no } ->
				io__write_string("For more information, try recompiling with `-E'.\n")
			;
				[]
			)
		)
	).

	% Process a list of module names.
	% Remove any `.m' extension extension before processing
	% the module name.

:- pred strip_module_suffixes(list(string), list(string)).
:- mode strip_module_suffixes(in, out) is det.

strip_module_suffixes([], []).
strip_module_suffixes([Module0 | Modules0], [Module | Modules]) :-
	(
		string__remove_suffix(Module0, ".m", Module1)
	->
		Module = Module1
	;
		Module = Module0
	),
	strip_module_suffixes(Modules0, Modules).

:- pred process_module_list(list(string), io__state, io__state).
:- mode process_module_list(in, di, uo) is det.

process_module_list([]) --> [].
process_module_list([Module | Modules]) -->
	process_module(Module),
	process_module_list(Modules).

	% Open the file and process it.

:- pred process_module(string, io__state, io__state).
:- mode process_module(in, di, uo) is det.

process_module(Module) -->
	 	% All messages go to stderr
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, _),

	globals__io_lookup_bool_option(generate_dependencies, GenerateDeps),
	( { GenerateDeps = yes } ->
		generate_dependencies(Module)
	;
		process_module_2(Module)
	).

:- pred process_module_2(string, io__state, io__state).
:- mode process_module_2(in, di, uo) is det.

process_module_2(ModuleName) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Parsing `"),
	maybe_write_string(Verbose, ModuleName),
	maybe_write_string(Verbose, ".m' and imported interfaces...\n"),
	io__gc_call(read_mod(ModuleName, ".m", "Reading module",
			Items0, Error)),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),

	globals__io_lookup_bool_option(halt_at_syntax_errors, HaltSyntax),
	globals__io_lookup_bool_option(make_interface, MakeInterface),
	globals__io_lookup_bool_option(convert_to_mercury, ConvertToMercury),
	globals__io_lookup_bool_option(convert_to_goedel, ConvertToGoedel),
	( { Error = fatal } ->
		[]
	; { Error = yes, HaltSyntax = yes } ->
		[]
	; { MakeInterface = yes } ->
		make_interface(ModuleName, Items0)
	; { ConvertToMercury = yes } ->
		{ string__append(ModuleName, ".ugly", OutputFileName) },
		convert_to_mercury(ModuleName, OutputFileName, Items0)
	; { ConvertToGoedel = yes } ->
		convert_to_goedel(ModuleName, Items0)
	;
		grab_imported_modules(ModuleName, Items0, Module, Error2),
		( { Error2 \= fatal } ->
			mercury_compile(Module)
		;
			[]
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given a fully expanded module (i.e. a module name and a list
	% of all the items in the module and any of its imports),
	% compile it.

:- pred mercury_compile(module_imports, io__state, io__state).
:- mode mercury_compile(in, di, uo) is det.

%	The predicate that invokes all the different passes of the
% 	compiler.  This is written using NU-Prolog hacks to avoid
%	running out of memory.

#if NU_PROLOG
:- type mc ---> mc.
:- type ref.
:- pred putprop(mc, mc, T).
:- mode putprop(in, in, in) is det.
:- pred getprop(mc, mc, T, ref).
:- mode getprop(in, in, out, out) is det.
:- pred erase(ref).
:- mode erase(in) is det.
#endif

%-----------------------------------------------------------------------------%

mercury_compile(Module) -->
	{ Module = module_imports(ModuleName, _, _, _, _) },
	mercury_compile__pre_hlds_pass(Module, HLDS1, Errors1),
	mercury_compile__semantic_pass(HLDS1, HLDS7, Errors2),
	globals__io_lookup_bool_option(errorcheck_only, ErrorCheckOnly),
	( { Errors1 = no }, { Errors2 = no } ->
	    mercury_compile__maybe_write_dependency_graph(HLDS7, HLDS7a),
	    ( { ErrorCheckOnly = yes } ->
		% we may still want to run `unused_args' so that we get
		% the appropriate warnings
		globals__io_set_option(optimize_unused_args, bool(no)),
		mercury_compile__maybe_unused_args(HLDS7a, _)
	    ;
		mercury_compile__maybe_output_prof_call_graph(HLDS7a, HLDS7b),
		mercury_compile__middle_pass(HLDS7b, HLDS12),
		globals__io_lookup_bool_option(highlevel_c, HighLevelC),
		( { HighLevelC = yes } ->
			{ string__append(ModuleName, ".c", C_File) },
			mercury_compile__gen_hlds(C_File, HLDS12),
			globals__io_lookup_bool_option(compile_to_c,
				CompileToC),
			( { CompileToC = no } ->
				mercury_compile__single_c_to_obj(ModuleName,
					_CompileOK)
			;
				[]
			)
		;
			mercury_compile__backend_pass(HLDS12, HLDS19, LLDS2),
			mercury_compile__output_pass(HLDS19, LLDS2,
				ModuleName, _CompileErrors)
		)
	    )
	;
	    []
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__pre_hlds_pass(module_imports, module_info, bool,
	io__state, io__state).
:- mode mercury_compile__pre_hlds_pass(in, out, out, di, uo) is det.

mercury_compile__pre_hlds_pass(module_imports(Module, ShortDeps, LongDeps,
		Items0, _), HLDS1, FoundError) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	write_dependency_file(Module, ShortDeps, LongDeps),

	mercury_compile__expand_equiv_types(Items0, Items),
	maybe_report_stats(Statistics),

	mercury_compile__make_hlds(Module, Items, HLDS0, FoundError),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS0, "1", "initial"),

	( { FoundError = yes } ->
		{ module_info_incr_errors(HLDS0, HLDS1) }
	;	
		{ HLDS1 = HLDS0 }
	),

	maybe_report_stats(Statistics),

#if NU_PROLOG
	{ putprop(mc, mc, HLDS1 - Proceed), fail }.
mercury_compile__pre_hlds_pass(_, HLDS1, Proceed) -->
	{ getprop(mc, mc, HLDS1 - Proceed, Ref), erase(Ref) },
#endif
	{ true }.

:- pred mercury_compile__expand_equiv_types(item_list, item_list,
	io__state, io__state).
:- mode mercury_compile__expand_equiv_types(in, out, di, uo) is det.

mercury_compile__expand_equiv_types(Items0, Items) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Expanding equivalence types..."),
	maybe_flush_output(Verbose),
	{ prog_util__expand_eqv_types(Items0, Items) },
	maybe_write_string(Verbose, " done.\n").

:- pred mercury_compile__make_hlds(module_name, item_list,
	module_info, bool, io__state, io__state).
:- mode mercury_compile__make_hlds(in, in, out, out, di, uo) is det.

mercury_compile__make_hlds(Module, Items, HLDS, FoundSemanticError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Converting parse tree to hlds...\n"),
	{ Prog = module(Module, Items) },
	parse_tree_to_hlds(Prog, HLDS),
	{ module_info_num_errors(HLDS, NumErrors) },
	( { NumErrors > 0 } ->
		{ FoundSemanticError = yes },
		io__set_exit_status(1)
	;
		{ FoundSemanticError = no }
	),
	maybe_write_string(Verbose, "% done.\n").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__semantic_pass(module_info, module_info, bool,
					io__state, io__state).
% :- mode mercury_compile__semantic_pass(di, uo, in, out, di, uo) is det.
:- mode mercury_compile__semantic_pass(in, out, out, di, uo) is det.

mercury_compile__semantic_pass(HLDS1, HLDS, FoundError) -->
	%
	% First check for undefined types.
	% We can't continue after an undefined type error, since
	% typecheck would get internal errors
	%
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Type-checking...\n"),
	mercury_compile__check_undef_types(HLDS1, HLDS1b, FoundUndefTypeError),
	( { FoundUndefTypeError = yes } ->
	    { HLDS = HLDS1b },
	    { FoundError = yes },
	    maybe_write_string(Verbose,
		    "% Program contains undefined type error(s).\n"),
	    io__set_exit_status(1)
	;

	    %
	    % Next typecheck the clauses.
	    %
	    typecheck(HLDS1b, HLDS2, FoundTypeError),
	    ( { FoundTypeError = yes } ->
		maybe_write_string(Verbose,
			"% Program contains type error(s).\n"),
		io__set_exit_status(1)
	    ;
		maybe_write_string(Verbose, "% Program is type-correct.\n")
	    ),
	    mercury_compile__maybe_dump_hlds(HLDS2, "2", "typecheck"),

	    %
	    % Now continue, even if we got a type error,
	    % unless `--typecheck-only' was specified.
	    %
	    globals__io_lookup_bool_option(typecheck_only, TypecheckOnly),
	    ( { TypecheckOnly = yes } ->
		{ HLDS = HLDS2 },
		{ FoundError = FoundTypeError }
	    ;
		%
		% Check for undefined insts and modes.
	        % We can't continue after an undefined insts/mode error, since
	        % mode analysis would get internal errors
		%
	        mercury_compile__check_undef_modes(HLDS2, HLDS2b,
		    FoundUndefModeError),
	        ( { FoundUndefModeError = yes } ->
	            { HLDS = HLDS2b },
	            { FoundError = yes },
	            maybe_write_string(Verbose,
	"% Program contains undefined inst or undefined mode error(s).\n"),
	            io__set_exit_status(1)
	        ;
		    %
		    % Now go ahead and do the rest of mode checking and
		    % determinism analysis
		    %
		    mercury_compile__semantic_pass_2_by_phases(HLDS2, HLDS,
			    FoundModeOrDetError),
		    { bool__or(FoundTypeError, FoundModeOrDetError,
			    FoundError) }
		)
	    )
	),

#if NU_PROLOG
	{ putprop(mc, mc, HLDS - Proceed), fail }.
mercury_compile__semantic_pass(_, HLDS, _, Proceed) -->
	{ getprop(mc, mc, HLDS - Proceed, Ref), erase(Ref) },
#endif

	{ true }.

:- pred mercury_compile__semantic_pass_2(module_info, module_info,
	bool, io__state, io__state).
% :- mode mercury_compile__semantic_pass_2(di, uo, out, di, uo) is det.
:- mode mercury_compile__semantic_pass_2(in, out, out, di, uo) is det.

mercury_compile__semantic_pass_2(HLDS2, HLDS7, FoundError) -->
	globals__io_lookup_bool_option(trad_passes, TradPasses),
	(
		{ TradPasses = no },
		mercury_compile__semantic_pass_2_by_phases(HLDS2, HLDS7,
			FoundError)
	;
		{ TradPasses = yes },
		mercury_compile__semantic_pass_2_by_preds(HLDS2, HLDS7,
			FoundError)
	).

:- pred mercury_compile__semantic_pass_2_by_phases(module_info, module_info,
	bool, io__state, io__state).
% :- mode mercury_compile__semantic_pass_2_by_phases(di, uo, out, di, uo)
% is det.
:- mode mercury_compile__semantic_pass_2_by_phases(in, out, out, di, uo) is det.

mercury_compile__semantic_pass_2_by_phases(HLDS2, HLDS7, FoundError) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	mercury_compile__modecheck(HLDS2, HLDS3, FoundModeError),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS3, "3", "modecheck"),

	mercury_compile__detect_switches(HLDS3, HLDS4),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS4, "4", "switch_detect"),

	mercury_compile__detect_cse(HLDS4, HLDS5),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS5, "5", "cse"),

	mercury_compile__check_determinism(HLDS5, HLDS6, FoundDetError),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS6, "6", "determinism"),

	mercury_compile__check_unique_modes(HLDS6, HLDS7, FoundUniqError),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS7, "7", "unique_modes"),

	    %
	    % work out whether we encountered any errors
	    %
	(
	    { FoundModeError = no },
	    { FoundDetError = no },
	    { FoundUniqError = no }
	->
	    { FoundError = no }
	;
	    { FoundError = yes }
	).

:- pred mercury_compile__semantic_pass_2_by_preds(module_info, module_info,
	bool, io__state, io__state).
% :- mode mercury_compile__semantic_pass_2_by_preds(di, uo, out, di, uo)
%	is det.
:- mode mercury_compile__semantic_pass_2_by_preds(in, out, out, di, uo)
	is det.

mercury_compile__semantic_pass_2_by_preds(HLDS1, HLDS8, FoundError) -->
	mercury_compile__semantic_pass_2_by_phases(HLDS1, HLDS8, FoundError).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__middle_pass(module_info, module_info,
					io__state, io__state).
% :- mode mercury_compile__middle_pass(di, uo, di, uo) is det.
:- mode mercury_compile__middle_pass(in, out, di, uo) is det.

mercury_compile__middle_pass(HLDS7, HLDS15) -->
	%
	% polymorphism affects the pred declarations, so it must be done
	% in one phase, not per-pred.
	%
	globals__io_lookup_bool_option(statistics, Statistics),
	mercury_compile__maybe_polymorphism(HLDS7, HLDS8),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS8, "8", "polymorphism"),

	globals__io_lookup_bool_option(trad_passes, TradPasses),
	( { TradPasses = no } ->
		mercury_compile__middle_pass_by_phases(HLDS8, HLDS15)
	;
		mercury_compile__middle_pass_by_preds(HLDS8, HLDS15)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__middle_pass_by_phases(module_info, module_info,
						io__state, io__state).
% :- mode mercury_compile__middle_pass_by_phases(di, uo, di, uo) is det.
:- mode mercury_compile__middle_pass_by_phases(in, out, di, uo) is det.

mercury_compile__middle_pass_by_phases(HLDS8, HLDS15) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	mercury_compile__maybe_optimize_higher_order(HLDS8, HLDS9),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS9, "9", "higher_order"),

	mercury_compile__maybe_do_inlining(HLDS9, HLDS10),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS10, "10", "inlining"),

	mercury_compile__maybe_detect_common_struct(HLDS10, HLDS11),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS11, "11", "common"),

	mercury_compile__maybe_propagate_constraints(HLDS11, HLDS12),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS12, "12", "constraint"),

	mercury_compile__maybe_unused_args(HLDS12, HLDS13),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS13, "13", "unused_args"),

	mercury_compile__maybe_eliminate_dead_procs(HLDS13, HLDS14),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS14, "14", "dead_procs"),

	mercury_compile__maybe_remove_excess_assigns(HLDS14, HLDS15),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS15, "15", "excessassign").

%-----------------------------------------------------------------------------%

:- pred mercury_compile__middle_pass_by_preds(module_info, module_info,
	io__state, io__state).
% :- mode mercury_compile__middle_pass_by_preds(di, uo, di, uo) is det.
:- mode mercury_compile__middle_pass_by_preds(in, out, di, uo) is det.

mercury_compile__middle_pass_by_preds(HLDS8, HLDS15) -->
	mercury_compile__middle_pass_by_phases(HLDS8, HLDS15).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass(module_info, module_info,
	list(c_procedure), io__state, io__state).
% :- mode mercury_compile__backend_pass(di, uo, out, di, uo) is det.
:- mode mercury_compile__backend_pass(in, out, out, di, uo) is det.

mercury_compile__backend_pass(HLDS15, HLDS22, LLDS2) -->
	%
	% map_args_to_regs affects the interface to a predicate,
	% so it must be done in one phase before code generation
	%
	mercury_compile__map_args_to_regs(HLDS15, HLDS16),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS16, "16", "args_to_regs"),

	globals__io_lookup_bool_option(trad_passes, TradPasses),
	(
		{ TradPasses = no },
		mercury_compile__backend_pass_by_phases(HLDS16, HLDS22, LLDS2)
	;
		{ TradPasses = yes },
		mercury_compile__backend_pass_by_preds(HLDS16, HLDS22, LLDS2)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass_by_phases(module_info, module_info,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__backend_pass_by_phases(in, out, out, di, uo) is det.

mercury_compile__backend_pass_by_phases(HLDS16, HLDS22, LLDS2) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	mercury_compile__maybe_migrate_followcode(HLDS16, HLDS17),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS17, "17", "followcode"),

	mercury_compile__compute_liveness(HLDS17, HLDS18),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS18, "18", "liveness"),

	mercury_compile__maybe_compute_followvars(HLDS18, HLDS19),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS19, "19", "followvars"),

#if NU_PROLOG
	{ putprop(mc, mc, HLDS19), fail }.
mercury_compile__backend_pass_by_phases(_, _, _) -->
	{ getprop(mc, mc, HLDS19, Ref), erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
#endif

	mercury_compile__compute_stack_vars(HLDS19, HLDS20),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS20, "20", "stackvars"),

	mercury_compile__allocate_store_map(HLDS20, HLDS21),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS21, "21", "store_map"),

	maybe_report_sizes(HLDS21),

#if NU_PROLOG
	{ putprop(mc, mc, HLDS21), fail }.
mercury_compile__backend_pass_by_phases(_, _, _) -->
	{ getprop(mc, mc, HLDS21, Ref), erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
#endif

	mercury_compile__generate_code(HLDS21, HLDS22, LLDS1),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS22, "22", "codegen"),
	mercury_compile__maybe_dump_hlds(HLDS22, "99", "final"),

#if NU_PROLOG
	{ putprop(mc, mc, HLDS22 - LLDS1), fail }.
mercury_compile__backend_pass_by_phases(_, HLDS22, LLDS2) -->
	{ getprop(mc, mc, HLDS22 - LLDS1, Ref), erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
#endif

	mercury_compile__maybe_do_optimize(LLDS1, LLDS2),
	maybe_report_stats(Statistics).

:- pred mercury_compile__backend_pass_by_preds(module_info, module_info,
	list(c_procedure), io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds(di, uo, out, di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds(in, out, out, di, uo) is det.

mercury_compile__backend_pass_by_preds(HLDS16, HLDS22, LLDS2) -->
	{ module_info_predids(HLDS16, PredIds) },
	mercury_compile__backend_pass_by_preds_2(PredIds, HLDS16, HLDS22,
		LLDS2).

:- pred mercury_compile__backend_pass_by_preds_2(list(pred_id),
	module_info, module_info, list(c_procedure), io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds_2(in, di, uo, out, di, uo)
% 	is det.
:- mode mercury_compile__backend_pass_by_preds_2(in, in, out, out, di, uo)
	is det.

mercury_compile__backend_pass_by_preds_2([], ModuleInfo, ModuleInfo, [])
	--> [].
mercury_compile__backend_pass_by_preds_2([PredId | PredIds], ModuleInfo0,
		ModuleInfo, Code) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( { ProcIds = [] } ->
		{ ModuleInfo1 = ModuleInfo0 },
		{ Code1 = [] }
	;
		globals__io_lookup_bool_option(verbose, Verbose),
		( { Verbose = yes } ->
			io__write_string("% Processing "),
			hlds_out__write_pred_id(ModuleInfo0, PredId),
			io__write_string(" ...\n"),
			io__flush_output
		;
			[]
		),
		mercury_compile__backend_pass_by_preds_3(ProcIds, PredId,
			PredInfo, ModuleInfo0, ModuleInfo1, Code1),
		( { Verbose = yes } ->
			io__write_string("% done.\n"),
			io__flush_output
		;
			[]
		)
	),
	mercury_compile__backend_pass_by_preds_2(PredIds,
		ModuleInfo1, ModuleInfo, Code2),
	{ list__append(Code1, Code2, Code) }.

:- pred mercury_compile__backend_pass_by_preds_3(list(proc_id), pred_id,
	pred_info, module_info, module_info, list(c_procedure),
	io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds_3(in, in, in, di, uo, out,
% 	di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds_3(in, in, in, in, out, out,
	di, uo) is det.

mercury_compile__backend_pass_by_preds_3([], _, _, ModuleInfo, ModuleInfo, [])
		--> [].
mercury_compile__backend_pass_by_preds_3([ProcId | ProcIds], PredId, PredInfo,
		ModuleInfo0, ModuleInfo, [Proc | Procs]) -->
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	mercury_compile__backend_pass_by_preds_4(ProcInfo, ProcId, PredId,
		ModuleInfo0, ModuleInfo1, Proc),
	mercury_compile__backend_pass_by_preds_3(ProcIds, PredId, PredInfo,
		ModuleInfo1, ModuleInfo, Procs).

:- pred mercury_compile__backend_pass_by_preds_4(proc_info, proc_id, pred_id,
	module_info, module_info, c_procedure, io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds_4(in, in, in, di, uo,
% 	out, di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds_4(in, in, in, in, out,
	out, di, uo) is det.

mercury_compile__backend_pass_by_preds_4(ProcInfo0, ProcId, PredId,
		ModuleInfo0, ModuleInfo, Proc) -->
	globals__io_lookup_bool_option(excess_assign, ExcessAssign),
	( { ExcessAssign = yes } ->
		{ excess_assignments_proc(ProcInfo0, ModuleInfo0, ProcInfo1) }
	;
		{ ProcInfo1 = ProcInfo0 }
	),
	globals__io_lookup_bool_option(follow_code, FollowCode),
	globals__io_lookup_bool_option(prev_code, PrevCode),
	( { FollowCode = yes ; PrevCode = yes } ->
		{ move_follow_code_in_proc(ProcInfo1, ProcInfo2,
			FollowCode - PrevCode, ModuleInfo0, ModuleInfo1) }
	;
		{ ProcInfo2 = ProcInfo1 },
		{ ModuleInfo1 = ModuleInfo0 }
	),
	{ detect_liveness_proc(ProcInfo2, ModuleInfo1, ProcInfo3) },
	globals__io_lookup_bool_option(follow_vars, FollowVars),
	( { FollowVars = yes } ->
		{ find_follow_vars_in_proc(ProcInfo3, ModuleInfo1, ProcInfo4) }
	;
		{ ProcInfo4 = ProcInfo3 }
	),
	{ detect_live_vars_in_proc(ProcInfo4, ModuleInfo1, ProcInfo5) },
	{ store_alloc_in_proc(ProcInfo5, ModuleInfo1, ProcInfo6) },
	{ module_info_get_shapes(ModuleInfo1, Shapes0) },
	generate_proc_code(ProcInfo6, ProcId, PredId, ModuleInfo1,
		Shapes0, Shapes, Proc0),
	{ module_info_set_shapes(ModuleInfo1, Shapes, ModuleInfo) },
	globals__io_lookup_bool_option(optimize, Optimize),
	( { Optimize = yes } ->
		optimize__proc(Proc0, Proc)
	;
		{ Proc = Proc0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__check_undef_types(module_info, module_info, bool,
	io__state, io__state).
:- mode mercury_compile__check_undef_types(in, out, out, di, uo) is det.

mercury_compile__check_undef_types(HLDS0, HLDS, FoundError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_write_string(Verbose, "% Checking for undefined types...\n"),
	check_undefined_types(HLDS0, HLDS, FoundError),
	maybe_report_stats(Statistics).

:- pred mercury_compile__check_undef_modes(module_info, module_info, bool,
	io__state, io__state).
:- mode mercury_compile__check_undef_modes(in, out, out, di, uo) is det.

mercury_compile__check_undef_modes(HLDS0, HLDS, FoundError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_write_string(Verbose, "% Mode-checking...\n"),
	maybe_write_string(Verbose, "% Checking for undefined insts and modes...\n"),
	check_undefined_modes(HLDS0, HLDS, FoundError),
	maybe_report_stats(Statistics).

:- pred mercury_compile__modecheck(module_info, module_info, bool,
	io__state, io__state).
:- mode mercury_compile__modecheck(in, out, out, di, uo) is det.

mercury_compile__modecheck(HLDS0, HLDS, FoundModeError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	{ module_info_num_errors(HLDS0, NumErrors0) },
	modecheck(HLDS0, HLDS),
	{ module_info_num_errors(HLDS, NumErrors) },
	( { NumErrors \= NumErrors0 } ->
		{ FoundModeError = yes },
		maybe_write_string(Verbose,
			"% Program contains mode error(s).\n"),
		io__set_exit_status(1)
	;
		{ FoundModeError = no },
		maybe_write_string(Verbose,
			"% Program is mode-correct.\n")
	).

:- pred mercury_compile__detect_switches(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__detect_switches(in, out, di, uo) is det.

mercury_compile__detect_switches(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Detecting switches..."),
	maybe_flush_output(Verbose),
	detect_switches(HLDS0, HLDS),
	maybe_write_string(Verbose, " done.\n").

:- pred mercury_compile__detect_cse(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__detect_cse(in, out, di, uo) is det.

mercury_compile__detect_cse(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(common_goal, CommonGoal),
	( { CommonGoal = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Detecting common deconstructions...\n"),
		detect_cse(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n")
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__check_determinism(module_info, module_info, bool,
	io__state, io__state).
:- mode mercury_compile__check_determinism(in, out, out, di, uo) is det.

mercury_compile__check_determinism(HLDS0, HLDS, FoundError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	{ module_info_num_errors(HLDS0, NumErrors0) },
	determinism_pass(HLDS0, HLDS),
	{ module_info_num_errors(HLDS, NumErrors) },
	( { NumErrors \= NumErrors0 } ->
		{ FoundError = yes },
		maybe_write_string(Verbose,
			"% Program contains determinism error(s).\n"),
		io__set_exit_status(1)
	;
		{ FoundError = no },
		maybe_write_string(Verbose,
			"% Program is determinism-correct.\n")
	).

:- pred mercury_compile__check_unique_modes(module_info, module_info, bool,
	io__state, io__state).
:- mode mercury_compile__check_unique_modes(in, out, out, di, uo) is det.

mercury_compile__check_unique_modes(HLDS0, HLDS, FoundError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose,
		"% Checking for backtracking over unique modes...\n"),
	io__get_exit_status(OldStatus),
	io__set_exit_status(0),
	unique_modes__check_module(HLDS0, HLDS),
	io__get_exit_status(NewStatus),
	( { NewStatus \= 0 } ->
		{ FoundError = yes },
		maybe_write_string(Verbose,
			"% Program contains unique mode error(s).\n")
	;
		{ FoundError = no },
		maybe_write_string(Verbose,
			"% Program is unique-mode-correct.\n"),
		io__set_exit_status(OldStatus)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_write_dependency_graph(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_write_dependency_graph(in, out, di, uo) is det.

mercury_compile__maybe_write_dependency_graph(ModuleInfo0, ModuleInfo) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(show_dependency_graph, ShowDepGraph),
	( { ShowDepGraph = yes } ->
		maybe_write_string(Verbose, "% Writing dependency graph..."),
		{ module_info_name(ModuleInfo0, Name) },
		{ string__append(Name, ".dependency_graph", WholeName) },
		io__tell(WholeName, Res),
		( { Res = ok } ->
			dependency_graph__write_dependency_graph(ModuleInfo0,
							ModuleInfo),
			io__told,
			maybe_write_string(Verbose, " done.\n")
		;
			report_error("unable to write dependency graph."),
			{ ModuleInfo0 = ModuleInfo }
		)
	;
		{ ModuleInfo0 = ModuleInfo }
	).

        % Output's the file <module_name>.prof, which contains the static
        % call graph in terms of label names, if the profiling flag enabled.
:- pred mercury_compile__maybe_output_prof_call_graph(module_info, module_info,
						        io__state, io__state).
:- mode mercury_compile__maybe_output_prof_call_graph(in, out, di, uo) is det.

mercury_compile__maybe_output_prof_call_graph(ModuleInfo0, ModuleInfo) -->
        globals__io_lookup_bool_option(profiling, Profiling),
        (
                { Profiling = yes }
        ->
                globals__io_lookup_bool_option(verbose, Verbose),
                maybe_write_string(Verbose, "% Outputing profiling call graph..."),
                maybe_flush_output(Verbose),
                { module_info_name(ModuleInfo0, Name) },
                { string__append(Name, ".prof", WholeName) },
                io__tell(WholeName, Res),
                (
                        { Res = ok }
                ->
                        dependency_graph__write_prof_dependency_graph(
						ModuleInfo0, ModuleInfo),
                        io__told
               ;
                        report_error("unable to write profiling static call graph."),
			{ ModuleInfo = ModuleInfo0 }
                ),
                maybe_write_string(Verbose, " done.\n")
        ;
		{ ModuleInfo = ModuleInfo0 }
        ).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_polymorphism(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_polymorphism(in, out, di, uo) is det.

mercury_compile__maybe_polymorphism(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(polymorphism, Polymorphism),
	( { Polymorphism = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose,
			"% Transforming polymorphic unifications..."),
		maybe_flush_output(Verbose),
		{ polymorphism__process_module(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__maybe_detect_common_struct(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_detect_common_struct(in, out, di, uo) is det.

mercury_compile__maybe_detect_common_struct(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(common_struct, CommonStruct),
	( { CommonStruct = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Detecting common structures..."),
		maybe_flush_output(Verbose),
		{ common__optimise_common_subexpressions(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_optimize_higher_order(module_info, module_info,
						io__state, io__state).
:- mode mercury_compile__maybe_optimize_higher_order(in, out, di, uo) is det.

mercury_compile__maybe_optimize_higher_order(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(optimize_higher_order, Optimize),
	( { Optimize = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose,
				"% Specializing higher-order predicates...\n"),
		maybe_flush_output(Verbose),
		specialize_higher_order(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n")
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_do_inlining(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_do_inlining(in, out, di, uo) is det.

mercury_compile__maybe_do_inlining(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(inlining, Inlining),
	globals__io_lookup_bool_option(errorcheck_only, ErrorCheckOnly),
	(
		{ Inlining = yes, ErrorCheckOnly = no }
	->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Inlining..."),
		maybe_flush_output(Verbose),
		{ inlining(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__maybe_propagate_constraints(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_propagate_constraints(in, out, di, uo) is det.

mercury_compile__maybe_propagate_constraints(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(constraint_propagation, ConstraintProp),
	( { ConstraintProp = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Propagating constraints..."),
		maybe_flush_output(Verbose),
		constraint_propagation(HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_unused_args(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_unused_args(in, out, di, uo) is det.

mercury_compile__maybe_unused_args(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(optimize_unused_args, Optimize),
	globals__io_lookup_bool_option(warn_unused_args, Warn),
        ( { Optimize = yes; Warn = yes } ->
                globals__io_lookup_bool_option(verbose, Verbose),
                maybe_write_string(Verbose, "% Finding unused arguments ...\n"),
                maybe_flush_output(Verbose),
                unused_args__process_module(HLDS0, HLDS),
                maybe_write_string(Verbose, "% done.\n")
        ;
                { HLDS0 = HLDS }
        ).
	

:- pred mercury_compile__maybe_eliminate_dead_procs(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_eliminate_dead_procs(in, out, di, uo) is det.

mercury_compile__maybe_eliminate_dead_procs(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(optimize_dead, Dead),
	( { Dead = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Eliminating dead procedures...\n"),
		maybe_flush_output(Verbose),
		dead_proc_elim(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n")
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_remove_excess_assigns(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_remove_excess_assigns(in, out, di, uo) is det.

mercury_compile__maybe_remove_excess_assigns(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(excess_assign, ExcessAssign),
	( { ExcessAssign = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Removing excess assignments..."),
		maybe_flush_output(Verbose),
		{ excess_assignments(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS0 = HLDS }
	).

%-----------------------------------------------------------------------------%

% The backend passes

:- pred mercury_compile__map_args_to_regs(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__map_args_to_regs(in, out, di, uo) is det.

mercury_compile__map_args_to_regs(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Mapping args to regs..."),
	maybe_flush_output(Verbose),
	globals__io_get_args_method(Args),
	{ generate_arg_info(HLDS0, Args, HLDS) },
	maybe_write_string(Verbose, " done.\n").

:- pred mercury_compile__maybe_migrate_followcode(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_migrate_followcode(in, out, di, uo) is det.

mercury_compile__maybe_migrate_followcode(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(follow_code, FollowCode),
	globals__io_lookup_bool_option(prev_code, PrevCode),
	( { FollowCode = yes ; PrevCode = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Migrating branch code..."),
		maybe_flush_output(Verbose),
		{ move_follow_code(HLDS0, FollowCode - PrevCode, HLDS) },
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__compute_liveness(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__compute_liveness(in, out, di, uo) is det.

mercury_compile__compute_liveness(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Computing liveness..."),
	maybe_flush_output(Verbose),
	{ detect_liveness(HLDS0, HLDS) },
	maybe_write_string(Verbose, " done.\n").

:- pred mercury_compile__maybe_compute_followvars(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_compute_followvars(in, out, di, uo) is det.

mercury_compile__maybe_compute_followvars(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(follow_vars, FollowVars),
	( { FollowVars = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Computing followvars..."),
		maybe_flush_output(Verbose),
		{ find_follow_vars(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__compute_stack_vars(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__compute_stack_vars(in, out, di, uo) is det.

mercury_compile__compute_stack_vars(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Computing stack vars..."),
	maybe_flush_output(Verbose),
	{ detect_live_vars(HLDS0, HLDS) },
	maybe_write_string(Verbose, " done.\n").

:- pred mercury_compile__allocate_store_map(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__allocate_store_map(in, out, di, uo) is det.

mercury_compile__allocate_store_map(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Allocating store map..."),
	maybe_flush_output(Verbose),
	{ store_alloc(HLDS0, HLDS) },
	maybe_write_string(Verbose, " done.\n").

:- pred mercury_compile__generate_code(module_info, module_info,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__generate_code(in, out, out, di, uo) is det.

mercury_compile__generate_code(HLDS0, HLDS, LLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Generating code...\n"),
	maybe_flush_output(Verbose),
	generate_code(HLDS0, HLDS, LLDS),
	maybe_write_string(Verbose, "% done.\n").

:- pred mercury_compile__maybe_do_optimize(list(c_procedure), list(c_procedure),
	io__state, io__state).
:- mode mercury_compile__maybe_do_optimize(in, out, di, uo) is det.

mercury_compile__maybe_do_optimize(LLDS0, LLDS) -->
	globals__io_lookup_bool_option(optimize, Optimize),
	( { Optimize = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose,
			"% Doing optimizations...\n"),
		maybe_flush_output(Verbose),
		optimize__main(LLDS0, LLDS),
		maybe_write_string(Verbose, "% done.\n")
	;
		{ LLDS = LLDS0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The LLDS output pass

:- pred mercury_compile__output_pass(module_info, list(c_procedure), string,
					bool, io__state, io__state).
:- mode mercury_compile__output_pass(in, in, in, out, di, uo) is det.

mercury_compile__output_pass(HLDS16, LLDS2, ModuleName, CompileErrors) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	mercury_compile__chunk_llds(HLDS16, LLDS2, LLDS3, NumChunks),
	mercury_compile__output_llds(ModuleName, LLDS3),
	maybe_report_stats(Statistics),

	mercury_compile__maybe_find_abstr_exports(HLDS16, HLDS17),
	maybe_report_stats(Statistics),

	{ module_info_shape_info(HLDS17, Shape_Info) },
	mercury_compile__maybe_write_gc(ModuleName, Shape_Info, LLDS3),
	maybe_report_stats(Statistics),

	globals__io_lookup_bool_option(compile_to_c, CompileToC),
	( { CompileToC = no } ->
		mercury_compile__c_to_obj(ModuleName, NumChunks, CompileOK),
		{ bool__not(CompileOK, CompileErrors) }
	;
		{ CompileErrors = no }
	).

	% Split the code up into bite-size chunks for the C compiler.

:- pred mercury_compile__chunk_llds(module_info, list(c_procedure), c_file, int,
	io__state, io__state).
% :- mode mercury_compile__chunk_llds(in, di, uo, out, di, uo) is det.
:- mode mercury_compile__chunk_llds(in, in, out, out, di, uo) is det.

mercury_compile__chunk_llds(HLDS, Procedures, c_file(Name, C_HeaderCode,
		ModuleList), NumChunks) -->
	{ module_info_name(HLDS, Name) },
	{ string__append(Name, "_module", ModName) },
	globals__io_lookup_int_option(procs_per_c_function, ProcsPerFunc),
	{ module_info_get_c_header(HLDS, C_HeaderCode) },
	{ module_info_get_c_body_code(HLDS, C_BodyCode0) },
	{ get_c_body_code(C_BodyCode0, C_BodyCode) },
	( { ProcsPerFunc = 0 } ->
		% ProcsPerFunc = 0 really means infinity -
		% we store all the procs in a single function.
		{ ModuleList0 = [c_module(ModName, Procedures)] }
	;
		{ list__chunk(Procedures, ProcsPerFunc, ChunkList) },
		{ mercury_compile__combine_chunks(ChunkList, ModName,
			ModuleList0) }
	),
	{ list__append(C_BodyCode, ModuleList0, ModuleList) },
	{ list__length(ModuleList, NumChunks) }.


:- pred get_c_body_code(c_body_info, list(c_module)).
:- mode get_c_body_code(in, out) is det.

get_c_body_code([], []).
get_c_body_code([Code - Context | CodesAndContexts],
			[c_code(Code, Context) | C_Modules]) :-
	get_c_body_code(CodesAndContexts, C_Modules).

:- pred mercury_compile__combine_chunks(list(list(c_procedure)), string,
	list(c_module)).
:- mode mercury_compile__combine_chunks(in, in, out) is det.

mercury_compile__combine_chunks(ChunkList, ModName, Modules) :-
	mercury_compile__combine_chunks_2(ChunkList, ModName, 0, Modules).

:- pred mercury_compile__combine_chunks_2(list(list(c_procedure)), string, int,
	list(c_module)).
:- mode mercury_compile__combine_chunks_2(in, in, in, out) is det.

mercury_compile__combine_chunks_2([], _ModName, _N, []).
mercury_compile__combine_chunks_2([Chunk|Chunks], ModName, Num,
		[Module | Modules]) :-
	string__int_to_string(Num, NumString),
	string__append(ModName, NumString, ThisModuleName),
	Module = c_module(ThisModuleName, Chunk),
	Num1 is Num + 1,
	mercury_compile__combine_chunks_2(Chunks, ModName, Num1, Modules).

:- pred mercury_compile__output_llds(module_name, c_file, io__state, io__state).
:- mode mercury_compile__output_llds(in, in, di, uo) is det.

mercury_compile__output_llds(ModuleName, LLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose,
		"% Writing output to `"),
	maybe_write_string(Verbose, ModuleName),
	maybe_write_string(Verbose, ".c'..."),
	maybe_flush_output(Verbose),
	output_c_file(LLDS),
	maybe_write_string(Verbose, " done.\n").

:- pred mercury_compile__maybe_write_gc(module_name, shape_info, c_file,
	io__state, io__state).
:- mode mercury_compile__maybe_write_gc(in, in, in, di, uo) is det.

mercury_compile__maybe_write_gc(ModuleName, Shape_Info, LLDS) -->
	globals__io_get_gc_method(GarbageCollectionMethod),
	( { GarbageCollectionMethod = accurate } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Writing gc info to `"),
		maybe_write_string(Verbose, ModuleName),
		maybe_write_string(Verbose, ".garb'..."),
		maybe_flush_output(Verbose),
		garbage_out__do_garbage_out(Shape_Info, LLDS),
		maybe_write_string(Verbose, " done.\n")
	;
		[]
	).

:- pred mercury_compile__maybe_find_abstr_exports(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_find_abstr_exports(in, out, di, uo) is det.
mercury_compile__maybe_find_abstr_exports(HLDS0, HLDS) -->
	globals__io_get_gc_method(GarbageCollectionMethod),
	(
		{ GarbageCollectionMethod = accurate }
	->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Looking up abstract type "),
		maybe_write_string(Verbose, "exports..."),
		maybe_flush_output(Verbose),
		{ shapes__do_abstract_exports(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS = HLDS0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The `--high-level-C' alternative backend

:- pred mercury_compile__gen_hlds(string, module_info, io__state, io__state).
:- mode mercury_compile__gen_hlds(in, in, di, uo) is det.

mercury_compile__gen_hlds(DumpFile, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_write_string(Verbose, "% Dumping out HLDS to `"),
	maybe_write_string(Verbose, DumpFile),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__tell(DumpFile, Res),
	( { Res = ok } ->
		io__gc_call(mercury_to_c__gen_hlds(0, HLDS)),
		io__told,
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Statistics)
	;
		maybe_write_string(Verbose, "\n"),
		{ string__append_list(["can't open file `",
			DumpFile, "' for output."], ErrorMessage) },
		report_error(ErrorMessage)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type compiler_type ---> gcc ; lcc ; unknown.

:- pred mercury_compile__c_to_obj(string, int, bool, io__state, io__state).
:- mode mercury_compile__c_to_obj(in, in, out, di, uo) is det.

mercury_compile__c_to_obj(ModuleName, NumChunks, Succeeded) -->
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = yes } ->
		mercury_compile__c_to_obj_list(ModuleName, 0, NumChunks,
			Succeeded)
	;
		mercury_compile__single_c_to_obj(ModuleName, Succeeded)
	).

:- pred mercury_compile__c_to_obj_list(string, int, int, bool,
					io__state, io__state).
:- mode mercury_compile__c_to_obj_list(in, in, in, out, di, uo) is det.

	% compile each of the C files in `<module>.dir'

mercury_compile__c_to_obj_list(ModuleName, Chunk, NumChunks, Succeeded) -->
	( { Chunk > NumChunks } ->
		{ Succeeded = yes }
	;
		{ dir__basename(ModuleName, BaseName) },
		{ string__format("%s.dir/%s_%03d",
			[s(BaseName), s(BaseName), i(Chunk)], NewName) },
		mercury_compile__single_c_to_obj(NewName, Succeeded0),
		( { Succeeded0 = no } ->
			{ Succeeded = no }
		;
			{ Chunk1 is Chunk + 1 },
			mercury_compile__c_to_obj_list(ModuleName,
				Chunk1, NumChunks, Succeeded)
		)
	).

:- pred mercury_compile__single_c_to_obj(string, bool, io__state, io__state).
:- mode mercury_compile__single_c_to_obj(in, out, di, uo) is det.

mercury_compile__single_c_to_obj(ModuleName, Succeeded) -->
	{ string__append(ModuleName, ".c", C_File) },
	{ string__append(ModuleName, ".o", O_File) },
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Compiling `"),
	maybe_write_string(Verbose, C_File),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(cc, CC),
	globals__io_lookup_string_option(cflags, CFLAGS),
	globals__io_lookup_string_option(c_include_directory, C_INCL),
	{ C_INCL = "" ->
		InclOpt = ""
	;
		string__append_list(["-I", C_INCL, " "], InclOpt)
	},
	globals__io_lookup_bool_option(split_c_files, Split_C_Files),
	{ Split_C_Files = yes ->
		SplitOpt = "-DSPLIT_C_FILES "
	;
		SplitOpt = ""
	},
	globals__io_lookup_bool_option(gcc_global_registers, GCC_Regs),
	( { GCC_Regs = yes } ->
		globals__io_lookup_string_option(cflags_for_regs,
			CFLAGS_FOR_REGS),
		{ RegOpt = " -DUSE_GCC_GLOBAL_REGISTERS " }
	;
		{ CFLAGS_FOR_REGS = "" },
		{ RegOpt = "" }
	),
	globals__io_lookup_bool_option(gcc_non_local_gotos, GCC_Gotos),
	( { GCC_Gotos = yes } ->
		{ GotoOpt = " -DUSE_GCC_NONLOCAL_GOTOS " },
		globals__io_lookup_string_option(cflags_for_gotos,
			CFLAGS_FOR_GOTOS)
	;
		{ GotoOpt = "" },
		{ CFLAGS_FOR_GOTOS = "" }
	),
	globals__io_lookup_bool_option(asm_labels, ASM_Labels),
	{ ASM_Labels = yes ->
		AsmOpt = "-DUSE_ASM_LABELS "
	;
		AsmOpt = ""
	},
	globals__io_get_gc_method(GC_Method),
	{ GC_Method = conservative ->
		GC_Opt = "-DCONSERVATIVE_GC "
	;
		GC_Opt = ""
	},
	globals__io_lookup_bool_option(profiling, Profiling),
	{ Profiling = yes ->
		ProfileOpt = "-DPROFILE_CALLS -DPROFILE_TIME "
	;
		ProfileOpt = ""
	},
	globals__io_get_tags_method(Tags_Method),
	{ Tags_Method = high ->
		TagsOpt = "-DHIGHTAGS "
	;
		TagsOpt = ""
	},
	globals__io_lookup_int_option(num_tag_bits, NumTagBits),
	{ string__int_to_string(NumTagBits, NumTagBitsString) },
	{ string__append_list(
		["-DTAGBITS=", NumTagBitsString, " "], NumTagBitsOpt) },
	globals__io_lookup_bool_option(debug, Debug),
	{ Debug = yes ->
		DebugOpt = "-g "
	;
		DebugOpt = "-DSPEED "
	},
	{ string__sub_string_search(CC, "gcc", _) ->
		CompilerType = gcc
	; string__sub_string_search(CC, "lcc", _) ->
		CompilerType = lcc
	;
		CompilerType = unknown
	},
	globals__io_lookup_bool_option(c_optimize, C_optimize),
	{ C_optimize = yes, Debug = no ->
		( CompilerType = gcc ->
			OptimizeOpt = "-O2 -fomit-frame-pointer "
		; CompilerType = lcc ->
			OptimizeOpt = ""
		;
			OptimizeOpt = "-O "
		)
	;
		OptimizeOpt = ""
	},
	{ CompilerType = gcc ->
		WarningOpt = "-Wall -Wwrite-strings -Wpointer-arith -Wcast-qual -Wtraditional -Wshadow -Wstrict-prototypes -Wmissing-prototypes -Wno-unused "
	; CompilerType = lcc ->
		WarningOpt = "-w "
	;
		WarningOpt = ""
	},
	{ string__append_list([CC, " ", InclOpt, SplitOpt,
		CFLAGS_FOR_REGS, RegOpt, CFLAGS_FOR_GOTOS, GotoOpt, AsmOpt,
		GC_Opt, ProfileOpt, TagsOpt, NumTagBitsOpt, DebugOpt,
		OptimizeOpt, WarningOpt, CFLAGS,
		" -c ", C_File, " -o ", O_File], Command) },
	invoke_system_command(Command, Succeeded),
	( { Succeeded = no } ->
		report_error("problem compiling C file.")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__link_module_list(list(string), io__state, io__state).
:- mode mercury_compile__link_module_list(in, di, uo) is det.

mercury_compile__link_module_list(Modules) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Statistics),
	globals__io_lookup_string_option(output_file_name, OutputFile0),
	( { OutputFile0 = "" } ->
	    ( { Modules = [Module | _] } ->
		{ dir__basename(Module, OutputFile) }
	    ;
	        { error("link_module_list: no modules") }
	    )
	;
	    { OutputFile = OutputFile0 }
	),
	{ dir__basename(OutputFile, OutputFileBase) },

	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = yes } ->
	    { join_module_list(Modules, ".dir/*.o ", [], ObjectList) },
	    { list__append(
	        ["ar cr ", OutputFileBase, ".a " | ObjectList],
	        [" && ranlib ", OutputFileBase, ".a"],
		MakeLibCmdList) },
	    { string__append_list(MakeLibCmdList, MakeLibCmd) },
	    invoke_system_command(MakeLibCmd, MakeLibCmdOK),
	    { Objects = [OutputFileBase, ".a"] }
        ;
	    { MakeLibCmdOK = yes },
	    { join_module_list(Modules, ".o ", [], Objects) }
        ),
	( { MakeLibCmdOK = no } ->
	    report_error("creation of object file library failed.")
	;
	    % create the initialization C file
	    maybe_write_string(Verbose, "% Creating initialization file...\n"),
	    { string__append(OutputFileBase, "_init", C_Init_Base) },
	    { join_module_list(Modules, ".m ", ["> ", C_Init_Base, ".c"],
				MkInitCmd0) },
	    { string__append_list(["c2init " | MkInitCmd0], MkInitCmd) },
	    invoke_system_command(MkInitCmd, MkInitOK),
	    maybe_report_stats(Statistics),
	    ( { MkInitOK = no } ->
		report_error("creation of init file failed.")
	    ;
		% compile it
	        maybe_write_string(Verbose,
			"% Compiling initialization file...\n"),
		mercury_compile__single_c_to_obj(C_Init_Base, CompileOK),
	        maybe_report_stats(Statistics),
		( { CompileOK = no } ->
		    report_error("compilation of init file failed.")
		;
	            maybe_write_string(Verbose, "% Linking...\n"),
		    globals__io_lookup_string_option(grade, Grade),
		    globals__io_lookup_string_option(link_flags, LinkFlags),
		    { string__append_list(
			["ml --grade ", Grade, " -o ", OutputFile, " ",
			LinkFlags, " ",
			OutputFileBase, "_init.o " | Objects],
			LinkCmd) },
		    invoke_system_command(LinkCmd, LinkCmdOK),
		    maybe_report_stats(Statistics),
		    ( { LinkCmdOK = no } ->
			report_error("link failed.")
		    ;
			[]
		    )
		)
	    )
	).

:- pred join_module_list(list(string), string, list(string), list(string)).
:- mode join_module_list(in, in, in, out) is det.

join_module_list([], _Separator, Terminator, Terminator).
join_module_list([Module0 | Modules0], Separator, Terminator,
			[Basename0, Separator | Rest]) :-
	dir__basename(Module0, Basename0),
	join_module_list(Modules0, Separator, Terminator, Rest).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_dump_hlds(module_info, string, string,
	io__state, io__state).
:- mode mercury_compile__maybe_dump_hlds(in, in, in, di, uo) is det.

mercury_compile__maybe_dump_hlds(HLDS, StageNum, StageName) -->
	globals__io_lookup_accumulating_option(dump_hlds, DumpStages),
	(
		{ list__member(StageNum, DumpStages)
		; list__member(StageName, DumpStages)
		}
	->
		{ module_info_name(HLDS, ModuleName) },
		{ string__append_list(
			[ModuleName, ".hlds_dump.", StageNum, "-", StageName],
			DumpFile) },
		mercury_compile__dump_hlds(DumpFile, HLDS)
	;
		[]
	).

:- pred mercury_compile__dump_hlds(string, module_info, io__state, io__state).
:- mode mercury_compile__dump_hlds(in, in, di, uo) is det.

mercury_compile__dump_hlds(DumpFile, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_write_string(Verbose, "% Dumping out HLDS to `"),
	maybe_write_string(Verbose, DumpFile),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__tell(DumpFile, Res),
	( { Res = ok } ->
		io__gc_call(hlds_out__write_hlds(0, HLDS)),
		io__told,
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Statistics)
	;
		maybe_write_string(Verbose, "\n"),
		{ string__append_list(["can't open file `",
			DumpFile, "' for output."], ErrorMessage) },
		report_error(ErrorMessage)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
