%-----------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
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
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

	% library modules
:- import_module int, list, map, set, std_util, dir, require, string, bool.
:- import_module library, getopt, term, set_bbbtree, varset.

	% the main compiler passes (in order of execution)
:- import_module handle_options, prog_io, prog_out, modules, module_qual.
:- import_module equiv_type, make_hlds, typecheck, purity, modes.
:- import_module switch_detection, cse_detection, det_analysis, unique_modes.
:- import_module stratify, check_typeclass, simplify, intermod, trans_opt.
:- import_module table_gen.
:- import_module bytecode_gen, bytecode.
:- import_module (lambda), polymorphism, termination, higher_order, inlining.
:- import_module deforest, dnf, constraint, unused_args, dead_proc_elim.
:- import_module lco, saved_vars, liveness.
:- import_module follow_code, live_vars, arg_info, store_alloc, goal_path.
:- import_module code_gen, optimize, export, base_type_info, base_type_layout.
:- import_module llds_common, llds_out, continuation_info, stack_layout.

	% miscellaneous compiler modules
:- import_module prog_data, hlds_module, hlds_pred, hlds_out, llds.
:- import_module mercury_to_c, mercury_to_mercury, mercury_to_goedel.
:- import_module dependency_graph.
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
		),
		globals__io_lookup_bool_option(statistics, Statistics),
		( { Statistics = yes } ->
			io__report_full_memory_stats
		;
			[]
		)
	).

	% Process a list of module names.
	% Remove any `.m' extension before processing
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
	process_module(Module), !,
	process_module_list(Modules).

	% Open the file and process it.

:- pred process_module(string, io__state, io__state).
:- mode process_module(in, di, uo) is det.

process_module(PathName) -->
	 	% All messages go to stderr
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, _),

	{ dir__split_name(PathName, DirName, BaseFileName) },
	( { dir__this_directory(DirName) } ->
		{ file_name_to_module_name(BaseFileName, ModuleName) },

		globals__io_lookup_bool_option(generate_dependencies,
			GenerateDeps),
		( { GenerateDeps = yes } ->
			generate_dependencies(ModuleName)
		;
			process_module_2(ModuleName)
		)
	;
		% Currently we don't allow directory names in the
		% command-line arguments, because it would confuse
		% the mapping between module names and file names.
		io__progname("mercury_compile", ProgName),
		io__write_strings([
			ProgName, ": Error in command-line argument `",
			PathName, "':\n",
			"arguments may not contain directory names.\n"]),
		io__set_exit_status(1)
	).

:- pred process_module_2(module_name, io__state, io__state).
:- mode process_module_2(in, di, uo) is det.

process_module_2(ModuleName) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Parsing `"),
	module_name_to_file_name(ModuleName, ".m", no, FileName),
	maybe_write_string(Verbose, FileName),
	maybe_write_string(Verbose, "' and imported interfaces...\n"),
	read_mod(ModuleName, ".m", "Reading module", yes, Items0, Error), !,
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_report_stats(Stats),

	globals__io_lookup_bool_option(halt_at_syntax_errors, HaltSyntax),
	globals__io_lookup_bool_option(make_interface, MakeInterface),
	globals__io_lookup_bool_option(make_short_interface,
						MakeShortInterface),
	globals__io_lookup_bool_option(make_private_interface,
						MakePrivateInterface),
	globals__io_lookup_bool_option(convert_to_mercury, ConvertToMercury),
	globals__io_lookup_bool_option(convert_to_goedel, ConvertToGoedel),
	( { Error = fatal } ->
		[]
	; { Error = yes, HaltSyntax = yes } ->
		[]
	; { MakeInterface = yes } ->
		{ split_into_submodules(ModuleName, Items0, SubModuleList) },
		list__foldl(make_interface, SubModuleList)
	; { MakeShortInterface = yes } ->
		{ split_into_submodules(ModuleName, Items0, SubModuleList) },
		list__foldl(make_short_interface, SubModuleList)
	; { MakePrivateInterface = yes } ->
		{ split_into_submodules(ModuleName, Items0, SubModuleList) },
		list__foldl(make_private_interface, SubModuleList)
	; { ConvertToMercury = yes } ->
		module_name_to_file_name(ModuleName, ".ugly", yes,
					OutputFileName),
		convert_to_mercury(ModuleName, OutputFileName, Items0)
	; { ConvertToGoedel = yes } ->
		convert_to_goedel(ModuleName, Items0)
	;
		{ split_into_submodules(ModuleName, Items0, SubModuleList) },
		list__foldl(compile, SubModuleList)

		% XXX it would be better to do something like
		%
		%	list__map_foldl(compile_to_llds, SubModuleList,
		%		LLDS_FragmentList),
		%	merge_llds_fragments(LLDS_FragmentList, LLDS),
		%	output_pass(LLDS_FragmentList)
		%
		% i.e. compile nested modules to a single C file.
	).

:- pred make_interface(pair(module_name, item_list), io__state, io__state).
:- mode make_interface(in, di, uo) is det.

make_interface(Module - Items) -->
	make_interface(Module, Items).

:- pred make_short_interface(pair(module_name, item_list),
				io__state, io__state).
:- mode make_short_interface(in, di, uo) is det.

make_short_interface(Module - Items) -->
	make_short_interface(Module, Items).

:- pred make_private_interface(pair(module_name, item_list),
				io__state, io__state).
:- mode make_private_interface(in, di, uo) is det.

make_private_interface(Module - Items) -->
	make_private_interface(Module, Items).

%-----------------------------------------------------------------------------%

	% Given a fully expanded module (i.e. a module name and a list
	% of all the items in the module and any of its imports),
	% compile it.

	% Stage number assignments:
	%
	%	 1 to 25	front end pass
	%	26 to 50	middle pass
	%	51 to 99	back end pass
	%
	% The initial arrangement has the stage numbers increasing by three
	% so that new stages can be slotted in without too much trouble.

:- pred compile(pair(module_name, item_list), io__state, io__state).
:- mode compile(in, di, uo) is det.

compile(ModuleName - Items0) -->
	grab_imported_modules(ModuleName, Items0, Module, Error2),
	( { Error2 \= fatal } ->
		mercury_compile(Module)
	;
		[]
	).

:- pred mercury_compile(module_imports, io__state, io__state).
:- mode mercury_compile(in, di, uo) is det.

mercury_compile(Module) -->
	globals__io_lookup_bool_option(typecheck_only, TypeCheckOnly),
	globals__io_lookup_bool_option(errorcheck_only, ErrorCheckOnly),
	{ bool__or(TypeCheckOnly, ErrorCheckOnly, DontWriteDFile) },
	% If we are only typechecking or error checking, then we should not
	% modify any files, this includes writing to .d files.
	mercury_compile__pre_hlds_pass(Module, DontWriteDFile,
		HLDS1, UndefTypes, UndefModes, Errors1), !,
	mercury_compile__frontend_pass(HLDS1, HLDS20, UndefTypes,
		UndefModes, Errors2), !,
	( { Errors1 = no }, { Errors2 = no } ->
	    globals__io_lookup_bool_option(verbose, Verbose),
	    globals__io_lookup_bool_option(statistics, Stats),
	    mercury_compile__maybe_write_dependency_graph(HLDS20,
		Verbose, Stats, HLDS21),
	    globals__io_lookup_bool_option(make_optimization_interface,
		MakeOptInt),
	    globals__io_lookup_bool_option(make_transitive_opt_interface,
		MakeTransOptInt),
	    ( { TypeCheckOnly = yes } ->
		[]
	    ; { ErrorCheckOnly = yes } ->
		% we may still want to run `unused_args' so that we get
		% the appropriate warnings
		globals__io_set_option(optimize_unused_args, bool(no)),
		mercury_compile__maybe_unused_args(HLDS21, Verbose, Stats, _)
	    ; { MakeOptInt = yes } ->
		% only run up to typechecking when making the .opt file
	    	[]
	    ; { MakeTransOptInt = yes } ->
	    	mercury_compile__output_trans_opt_file(HLDS21)
	    ;
		{ module_imports_get_module_name(Module, ModuleName) },
		mercury_compile__maybe_output_prof_call_graph(HLDS21,
			Verbose, Stats, HLDS25),
		mercury_compile__middle_pass(ModuleName, HLDS25, HLDS50), !,
		globals__io_lookup_bool_option(highlevel_c, HighLevelC),
		( { HighLevelC = yes } ->
			module_name_to_file_name(ModuleName, ".c", no, C_File),
			mercury_compile__gen_hlds(C_File, HLDS50),
			globals__io_lookup_bool_option(compile_to_c,
				CompileToC),
			( { CompileToC = no } ->
				module_name_to_file_name(ModuleName, ".o", yes,
					O_File),
				mercury_compile__single_c_to_obj(
					C_File, O_File, _CompileOK)
			;
				[]
			)
		;
			mercury_compile__backend_pass(HLDS50, HLDS70, LLDS), !,
			mercury_compile__output_pass(HLDS70, LLDS,
				ModuleName, _CompileErrors)
		)
	    )
	;
	    []
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__pre_hlds_pass(module_imports, bool,
		module_info, bool, bool, bool, io__state, io__state).
:- mode mercury_compile__pre_hlds_pass(in, in, out, out, out, out,
		di, uo) is det.

mercury_compile__pre_hlds_pass(ModuleImports0, DontWriteDFile,
		HLDS1, UndefTypes, UndefModes, FoundError) -->
	globals__io_lookup_bool_option(statistics, Stats),
	globals__io_lookup_bool_option(verbose, Verbose),

	{ module_imports_get_module_name(ModuleImports0, Module) },

	( { DontWriteDFile = yes } ->
		% The only time the TransOptDeps are required is when
		% creating the .trans_opt file.  If DontWriteDFile is yes,
		% then error check only or type-check only is enabled, so
		% we cant be creating the .trans_opt file.
		{ MaybeTransOptDeps = no }
	;
		maybe_read_dependency_file(Module, MaybeTransOptDeps), !,
		write_dependency_file(ModuleImports0, MaybeTransOptDeps), !
	),

	% Errors in .opt and .trans_opt files result in software errors.
	mercury_compile__maybe_grab_optfiles(ModuleImports0, Verbose,
		MaybeTransOptDeps, ModuleImports1, IntermodError), !,

	{ module_imports_get_items(ModuleImports1, Items1) },
	mercury_compile__module_qualify_items(Items1, Items2, Module, Verbose,
				Stats, MQInfo, _, UndefTypes0, UndefModes0), !,

	mercury_compile__expand_equiv_types(Items2, Verbose, Stats,
				Items, CircularTypes, EqvMap), !,
	{ bool__or(UndefTypes0, CircularTypes, UndefTypes1) },

	mercury_compile__make_hlds(Module, Items, MQInfo, EqvMap, Verbose, 
			Stats, HLDS0, UndefTypes2, UndefModes2, FoundError), !,

	{ bool__or(UndefTypes1, UndefTypes2, UndefTypes) },
	{ bool__or(UndefModes0, UndefModes2, UndefModes) },

	mercury_compile__maybe_dump_hlds(HLDS0, "01", "initial"), !,

	% Only stop on syntax errors in .opt files.
	( { FoundError = yes ; IntermodError = yes } ->
		{ module_info_incr_errors(HLDS0, HLDS1) }
	;	
		globals__io_lookup_bool_option(intermodule_optimization,
			Intermod),
		globals__io_lookup_bool_option(make_optimization_interface,
			MakeOptInt),
		( { Intermod = yes, MakeOptInt = no } ->
			% Eliminate unnecessary clauses from `.opt' files,
			% to speed up compilation.
			{ dead_pred_elim(HLDS0, HLDS1) }
		;
			{ HLDS1 = HLDS0 }
		)
	).

:- pred mercury_compile__module_qualify_items(item_list, item_list,
		module_name, bool, bool, mq_info, int, bool, bool,
		io__state, io__state).
:- mode mercury_compile__module_qualify_items(in, out, in, in, in, out, out,
		out, out, di, uo) is det. 

mercury_compile__module_qualify_items(Items0, Items, ModuleName, Verbose, 
			Stats, MQInfo, NumErrors, UndefTypes, UndefModes) -->
	maybe_write_string(Verbose, "% Module qualifying items...\n"),
	maybe_flush_output(Verbose),
	module_qual__module_qualify_items(Items0, Items, ModuleName, yes,
				MQInfo, NumErrors, UndefTypes, UndefModes),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_grab_optfiles(module_imports, bool,
	 maybe(list(module_name)), module_imports, bool, io__state, io__state).
:- mode mercury_compile__maybe_grab_optfiles(in, in, in, out, out, 
	di, uo) is det.

mercury_compile__maybe_grab_optfiles(Imports0, Verbose, MaybeTransOptDeps, 
		Imports, Error) -->
	globals__io_lookup_bool_option(intermodule_optimization, UseOptInt),
	globals__io_lookup_bool_option(make_optimization_interface,
						MakeOptInt),
	globals__io_lookup_bool_option(transitive_optimization, TransOpt),
	globals__io_lookup_bool_option(make_transitive_opt_interface,
		MakeTransOptInt),
	( { UseOptInt = yes, MakeOptInt = no } ->
		maybe_write_string(Verbose, "% Reading .opt files...\n"),
		maybe_flush_output(Verbose),
		intermod__grab_optfiles(Imports0, Imports1, Error1),
		maybe_write_string(Verbose, "% Done.\n")
	;
		{ Imports1 = Imports0 },
		{ Error1 = no }
	),
	( { MakeTransOptInt = yes } ->
		( { MaybeTransOptDeps = yes(TransOptDeps) } ->
			% When creating the trans_opt file, only import the
			% trans_opt files which are lower in the ordering.
			trans_opt__grab_optfiles(Imports1, TransOptDeps, 
				Imports, Error2)
		;
			{ Imports = Imports1 },
			{ Error2 = no },
			{ module_imports_get_module_name(Imports,
				ModuleName) },
			globals__io_lookup_bool_option(
				warn_missing_trans_opt_deps,
				WarnNoTransOptDeps),
			( { WarnNoTransOptDeps = yes } ->
				{ prog_out__sym_name_to_string(ModuleName,
					ModuleString) },
				io__write_strings([
				    "Warning: cannot read trans-opt ",
				    "dependencies for module `",
				    ModuleString, "'.\n",
				    "  You need to remake ",
				    "the dependencies.\n"]),
				globals__io_lookup_bool_option(halt_at_warn,
					Halt),
				( { Halt = yes } ->
					io__set_exit_status(1)
				;
					[]
				)
			;
				[]
			)
		)
	;
		( { TransOpt = yes } ->
			% If transitive optimization is enabled, but we are
			% not creating the trans opt file, then import the
			% trans_opt files for all the modules that are
			% imported (or used), and for all ancestor modules.
			{ Imports0 = module_imports(_Module, Ancestors,
				InterfaceImports, ImplementationImports,
				_IndirectImports, _PublicChildren, _FactDeps,
				_Items, _Error) },
			{ list__condense([Ancestors, InterfaceImports,
				ImplementationImports], TransOptFiles) },
			trans_opt__grab_optfiles(Imports1, TransOptFiles,
				Imports, Error2)
		;
			{ Imports = Imports1 },
			{ Error2 = no }
		)
	),

	{ bool__or(Error1, Error2, Error) }.

:- pred mercury_compile__expand_equiv_types(item_list, bool, bool, item_list,
	bool, eqv_map, io__state, io__state).
:- mode mercury_compile__expand_equiv_types(in, in, in, out,
	out, out, di, uo) is det.

mercury_compile__expand_equiv_types(Items0, Verbose, Stats,
		Items, CircularTypes, EqvMap) -->
	maybe_write_string(Verbose, "% Expanding equivalence types..."),
	maybe_flush_output(Verbose),
	equiv_type__expand_eqv_types(Items0, Items, CircularTypes, EqvMap),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__make_hlds(module_name, item_list, mq_info, eqv_map, 
	bool, bool, module_info, bool, bool, bool, io__state, io__state).
:- mode mercury_compile__make_hlds(in, in, in, in, in, in,
	out, out, out, out, di, uo) is det.

mercury_compile__make_hlds(Module, Items, MQInfo, EqvMap, Verbose, Stats,
		HLDS, UndefTypes, UndefModes, FoundSemanticError) -->
	maybe_write_string(Verbose, "% Converting parse tree to hlds...\n"),
	{ Prog = module(Module, Items) },
	parse_tree_to_hlds(Prog, MQInfo, EqvMap, HLDS, UndefTypes, UndefModes),
	{ module_info_num_errors(HLDS, NumErrors) },
	( { NumErrors > 0 } ->
		{ FoundSemanticError = yes },
		io__set_exit_status(1)
	;
		{ FoundSemanticError = no }
	),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__frontend_pass(module_info, module_info, bool,
					bool, bool, io__state, io__state).
% :- mode mercury_compile__frontend_pass(di, uo, in, in, out, di, uo) is det.
:- mode mercury_compile__frontend_pass(in, out, in, in, out, di, uo) is det.

mercury_compile__frontend_pass(HLDS1, HLDS, FoundUndefTypeError,
		FoundUndefModeError, FoundError) -->
	%
	% We can't continue after an undefined type error, since
	% typecheck would get internal errors
	%
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Type-checking...\n"),
	( { FoundUndefTypeError = yes } ->
	    { HLDS = HLDS1 },
	    { FoundError = yes },
	    maybe_write_string(Verbose,
		    "% Program contains undefined type error(s).\n"),
	    io__set_exit_status(1)
	;
		
	    maybe_write_string(Verbose, 
		"% Checking typeclass instances...\n"),
	    check_typeclass__check_instance_decls(HLDS1, HLDS2,
		FoundTypeclassError),
	    mercury_compile__maybe_dump_hlds(HLDS2, "2", "typeclass"), !,

	    %
	    % Next typecheck the clauses.
	    %
	    typecheck(HLDS2, HLDS3, FoundUndefModeError, FoundTypeError), !,
	    ( { FoundTypeError = yes } ->
		maybe_write_string(Verbose,
			"% Program contains type error(s).\n"),
		io__set_exit_status(1)
	    ;
		maybe_write_string(Verbose, "% Program is type-correct.\n")
	    ),
	    mercury_compile__maybe_dump_hlds(HLDS3, "03", "typecheck"),

	    %
	    % Now continue, even if we got a type error,
	    % unless `--typecheck-only' was specified.
	    %
	    globals__io_lookup_bool_option(typecheck_only, TypecheckOnly),
	    ( { TypecheckOnly = yes } ->
		{ HLDS = HLDS3 },
		{ bool__or(FoundTypeError, FoundTypeclassError, FoundError) }
	    ;
		% only write out the `.opt' file if there are no type errors
		globals__io_lookup_bool_option(make_optimization_interface,
			MakeOptInt),
		( { FoundTypeError = no } ->
			mercury_compile__maybe_write_optfile(MakeOptInt,
		    		HLDS3, HLDS4), !
		;
			{ HLDS4 = HLDS3 }
		),
		% if our job was to write out the `.opt' file, then we're done
		( { MakeOptInt = yes } ->
		    	{ HLDS = HLDS4 },
			{ bool__or(FoundTypeError, FoundTypeclassError,
				FoundError) }
		;
			%
			% We can't continue after an undefined inst/mode
			% error, since mode analysis would get internal errors
			%
			( { FoundUndefModeError = yes } ->
			    { FoundError = yes },
			    { HLDS = HLDS4 },
			    maybe_write_string(Verbose,
		"% Program contains undefined inst or undefined mode error(s).\n"),
			    io__set_exit_status(1)
			;
			    %
			    % Now go ahead and do the rest of mode checking and
			    % determinism analysis
			    %
			    mercury_compile__frontend_pass_2_by_phases(HLDS4,
			    		HLDS, FoundModeOrDetError),
			    { bool__or(FoundTypeError, FoundModeOrDetError,
					FoundError0) },
			    { bool__or(FoundError0, FoundTypeclassError,
				FoundError) }
			)
		)
	    )
	).

:- pred mercury_compile__maybe_write_optfile(bool::in, module_info::in,
		module_info::out, io__state::di, io__state::uo) is det.

mercury_compile__maybe_write_optfile(MakeOptInt, HLDS0, HLDS) -->
	globals__io_lookup_bool_option(intermod_unused_args, IntermodArgs),
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	globals__io_lookup_bool_option(termination, Termination),

	( { MakeOptInt = yes } ->
		intermod__write_optfile(HLDS0, HLDS1),

		% If intermod_unused_args is being performed, run mode and
		% determinism analysis and polymorphism, then run unused_args
		% to append the unused argument information to the `.opt.tmp' 
		% file written above.
		( { IntermodArgs = yes ; Termination = yes } ->
			mercury_compile__frontend_pass_2_by_phases(
				HLDS1, HLDS2, FoundModeError),
			( { FoundModeError = no } ->
				mercury_compile__maybe_polymorphism(HLDS2,
					Verbose, Stats, HLDS3),
				( { IntermodArgs = yes } ->
					mercury_compile__maybe_unused_args(
						HLDS3, Verbose, Stats, HLDS4)
				;
					{ HLDS4 = HLDS3 }
				),
				( { Termination = yes } ->
					mercury_compile__maybe_termination(
						HLDS4, Verbose, Stats, HLDS)
				;
					{ HLDS = HLDS4 }
				)
					
			;
				io__set_exit_status(1),
				{ HLDS = HLDS2 }
			)
		;
			{ HLDS = HLDS1 }
		),
		{ module_info_name(HLDS, ModuleName) },
		module_name_to_file_name(ModuleName, ".opt", yes, OptName),
		update_interface(OptName),
		touch_interface_datestamp(ModuleName, ".optdate")
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__output_trans_opt_file(module_info, 
	io__state, io__state).
:- mode mercury_compile__output_trans_opt_file(in, di, uo) is det.
mercury_compile__output_trans_opt_file(HLDS25) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__maybe_polymorphism(HLDS25, Verbose, Stats, HLDS26),
	mercury_compile__maybe_dump_hlds(HLDS26, "26", "polymorphism"), !,

	mercury_compile__maybe_termination(HLDS26, Verbose, Stats, HLDS28),
	mercury_compile__maybe_dump_hlds(HLDS28, "28", "termination"), !,

	trans_opt__write_optfile(HLDS28).
	
:- pred mercury_compile__frontend_pass_2(module_info, module_info,
	bool, io__state, io__state).
% :- mode mercury_compile__frontend_pass_2(di, uo, out, di, uo) is det.
:- mode mercury_compile__frontend_pass_2(in, out, out, di, uo) is det.

mercury_compile__frontend_pass_2(HLDS0, HLDS, FoundError) -->
	globals__io_lookup_bool_option(trad_passes, TradPasses),
	(
		{ TradPasses = no },
		mercury_compile__frontend_pass_2_by_phases(HLDS0, HLDS,
			FoundError)
	;
		{ TradPasses = yes },
		mercury_compile__frontend_pass_2_by_preds(HLDS0, HLDS,
			FoundError)
	).

:- pred mercury_compile__frontend_pass_2_by_phases(module_info, module_info,
	bool, io__state, io__state).
% :- mode mercury_compile__frontend_pass_2_by_phases(di, uo, out, di, uo)
% is det.
:- mode mercury_compile__frontend_pass_2_by_phases(in, out, out, di, uo) is det.

mercury_compile__frontend_pass_2_by_phases(HLDS3, HLDS20, FoundError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__puritycheck(HLDS3, Verbose, Stats, HLDS4),
	mercury_compile__maybe_dump_hlds(HLDS4, "04", "puritycheck"),

	mercury_compile__modecheck(HLDS4, Verbose, Stats, HLDS5,
		FoundModeError, UnsafeToContinue),
	mercury_compile__maybe_dump_hlds(HLDS5, "05", "modecheck"),

	( { UnsafeToContinue = yes } ->
		{ FoundError = yes },
		{ HLDS13 = HLDS5 }
	;
		mercury_compile__detect_switches(HLDS5, Verbose, Stats, HLDS6),
		!,
		mercury_compile__maybe_dump_hlds(HLDS6, "06", "switch_detect"),
		!,

		mercury_compile__detect_cse(HLDS6, Verbose, Stats, HLDS7), !,
		mercury_compile__maybe_dump_hlds(HLDS7, "07", "cse"), !,

		mercury_compile__check_determinism(HLDS7, Verbose, Stats, HLDS8,
			FoundDetError), !,
		mercury_compile__maybe_dump_hlds(HLDS8, "08", "determinism"),
		!,

		mercury_compile__check_unique_modes(HLDS8, Verbose, Stats,
			HLDS9, FoundUniqError), !,
		mercury_compile__maybe_dump_hlds(HLDS9, "09", "unique_modes"),
		!,

		mercury_compile__check_stratification(HLDS9, Verbose, Stats, 
			HLDS11, FoundStratError), !,
		mercury_compile__maybe_dump_hlds(HLDS11, "11",
			"stratification"), !,

		mercury_compile__simplify(HLDS11, yes, no,
			Verbose, Stats, HLDS12), !,
		mercury_compile__maybe_dump_hlds(HLDS12, "12", "simplify"), !,

		%
		% work out whether we encountered any errors
		%
		io__get_exit_status(ExitStatus),
		(
			{ FoundModeError = no },
			{ FoundDetError = no },
			{ FoundUniqError = no },
			{ FoundStratError = no },
			% Strictly speaking, we shouldn't need to check
			% the exit status.  But the values returned for
			% FoundModeError etc. aren't always correct.
			{ ExitStatus = 0 }
		->
			{ FoundError = no },
			globals__io_lookup_bool_option(intermodule_optimization,
								Intermod),
			globals__io_lookup_bool_option(
				make_optimization_interface, MakeOptInt),
			{ Intermod = yes, MakeOptInt = no ->
				intermod__adjust_pred_import_status(HLDS12,
					HLDS13), !
			;
				HLDS13 = HLDS12
			}
		;
			{ FoundError = yes },
			{ HLDS13 = HLDS12 }
		)
	),

	{ HLDS20 = HLDS13 },
	mercury_compile__maybe_dump_hlds(HLDS20, "20", "front_end").

:- pred mercury_compile__frontend_pass_2_by_preds(module_info, module_info,
	bool, io__state, io__state).
% :- mode mercury_compile__frontend_pass_2_by_preds(di, uo, out, di, uo)
%	is det.
:- mode mercury_compile__frontend_pass_2_by_preds(in, out, out, di, uo)
	is det.

mercury_compile__frontend_pass_2_by_preds(HLDS0, HLDS, FoundError) -->
	mercury_compile__frontend_pass_2_by_phases(HLDS0, HLDS, FoundError).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__middle_pass(module_name, module_info, module_info,
					io__state, io__state).
% :- mode mercury_compile__middle_pass(in, di, uo, di, uo) is det.
:- mode mercury_compile__middle_pass(in, in, out, di, uo) is det.

mercury_compile__middle_pass(ModuleName, HLDS24, HLDS50) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__tabling(HLDS24, Verbose, HLDS25),
	mercury_compile__maybe_dump_hlds(HLDS25, "25", "tabling"), !,

	mercury_compile__maybe_polymorphism(HLDS25, Verbose, Stats, HLDS26),
	mercury_compile__maybe_dump_hlds(HLDS26, "26", "polymorphism"), !,

	%
	% Uncomment the following code to check that unique mode analysis
	% works after polymorphism has been run. Currently it does not
	% because common.m does not preserve unique mode correctness
	% (this test fails on about five modules in the compiler and library).
	% It is important that unique mode analysis work most of the time
	% after optimizations and polymorphism because deforestation reruns it.
	%

	{ HLDS27 = HLDS26 },
	%mercury_compile__check_unique_modes(HLDS26, Verbose, Stats,
	%		HLDS27, FoundUniqError), !,
	%
	%{ FoundUniqError = yes ->
	%	error("unique modes failed")
	%;
	%	true
	%},

	mercury_compile__maybe_termination(HLDS27, Verbose, Stats, HLDS28),
	mercury_compile__maybe_dump_hlds(HLDS28, "28", "termination"), !,

	mercury_compile__maybe_base_type_infos(HLDS28, Verbose, Stats, HLDS29),
	!,
	mercury_compile__maybe_dump_hlds(HLDS29, "29", "base_type_infos"), !,

	mercury_compile__maybe_base_type_layouts(HLDS29, Verbose, Stats,HLDS30),
	!,
	mercury_compile__maybe_dump_hlds(HLDS30, "30", "base_type_layouts"), !,

	mercury_compile__maybe_bytecodes(HLDS30, ModuleName, Verbose, Stats),
	!,

	% stage number 31 is used by mercury_compile__maybe_bytecodes

	mercury_compile__maybe_higher_order(HLDS30, Verbose, Stats, HLDS32), !,
	mercury_compile__maybe_dump_hlds(HLDS32, "32", "higher_order"), !,

	mercury_compile__maybe_do_inlining(HLDS32, Verbose, Stats, HLDS34), !,
	mercury_compile__maybe_dump_hlds(HLDS34, "34", "inlining"), !,

	mercury_compile__maybe_deforestation(HLDS34, 
			Verbose, Stats, HLDS36), !,
	mercury_compile__maybe_dump_hlds(HLDS36, "36", "deforestation"), !,

	% dnf transformations should be after inlining
	% magic sets transformations should be before constraints
	mercury_compile__maybe_transform_dnf(HLDS36, Verbose, Stats, HLDS38), !,
	mercury_compile__maybe_dump_hlds(HLDS38, "38", "dnf"), !,

	mercury_compile__maybe_constraints(HLDS38, Verbose, Stats, HLDS40), !,
	mercury_compile__maybe_dump_hlds(HLDS40, "40", "constraint"), !,

	mercury_compile__maybe_unused_args(HLDS40, Verbose, Stats, HLDS43), !,
	mercury_compile__maybe_dump_hlds(HLDS43, "43", "unused_args"), !,

	mercury_compile__maybe_dead_procs(HLDS43, Verbose, Stats, HLDS46), !,
	mercury_compile__maybe_dump_hlds(HLDS46, "46", "dead_procs"), !,

	mercury_compile__maybe_lco(HLDS46, Verbose, Stats, HLDS47), !,
	mercury_compile__maybe_dump_hlds(HLDS47, "47", "lco"), !,

	% map_args_to_regs affects the interface to a predicate,
	% so it must be done in one phase immediately before code generation

	mercury_compile__map_args_to_regs(HLDS47, Verbose, Stats, HLDS49), !,
	mercury_compile__maybe_dump_hlds(HLDS49, "49", "args_to_regs"), !,

	{ HLDS50 = HLDS49 },
	mercury_compile__maybe_dump_hlds(HLDS49, "50", "middle_pass").

%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass(module_info, module_info,
	list(c_procedure), io__state, io__state).
% :- mode mercury_compile__backend_pass(di, uo, out, di, uo) is det.
:- mode mercury_compile__backend_pass(in, out, out, di, uo) is det.

mercury_compile__backend_pass(HLDS0, HLDS, LLDS) -->
	globals__io_lookup_bool_option(trad_passes, TradPasses),
	(
		{ TradPasses = no },
		mercury_compile__backend_pass_by_phases(HLDS0, HLDS, LLDS)
	;
		{ TradPasses = yes },
		mercury_compile__backend_pass_by_preds(HLDS0, HLDS, LLDS)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass_by_phases(module_info, module_info,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__backend_pass_by_phases(in, out, out, di, uo) is det.

mercury_compile__backend_pass_by_phases(HLDS50, HLDS99, LLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__maybe_followcode(HLDS50, Verbose, Stats, HLDS52), !,
	mercury_compile__maybe_dump_hlds(HLDS52, "52", "followcode"), !,

	mercury_compile__simplify(HLDS52, no, yes, Verbose, Stats, HLDS53), !,
	mercury_compile__maybe_dump_hlds(HLDS53, "53", "simplify2"), !,

	mercury_compile__maybe_saved_vars(HLDS53, Verbose, Stats, HLDS56), !,
	mercury_compile__maybe_dump_hlds(HLDS56, "56", "savedvars"), !,

	mercury_compile__compute_liveness(HLDS56, Verbose, Stats, HLDS59), !,
	mercury_compile__maybe_dump_hlds(HLDS59, "59", "liveness"), !,

	mercury_compile__compute_stack_vars(HLDS59, Verbose, Stats, HLDS65), !,
	mercury_compile__maybe_dump_hlds(HLDS65, "65", "stackvars"), !,

	mercury_compile__allocate_store_map(HLDS65, Verbose, Stats, HLDS68), !,
	mercury_compile__maybe_dump_hlds(HLDS68, "68", "store_map"), !,

	mercury_compile__maybe_goal_paths(HLDS68, Verbose, Stats, HLDS72), !,
	mercury_compile__maybe_dump_hlds(HLDS72, "72", "goal_path"), !,

	maybe_report_sizes(HLDS72),

	{ HLDS90 = HLDS72 },
	mercury_compile__maybe_dump_hlds(HLDS90, "90", "precodegen"), !,

	mercury_compile__generate_code(HLDS90, Verbose, Stats, HLDS95, LLDS1),
	!,
	mercury_compile__maybe_dump_hlds(HLDS95, "95", "codegen"), !,

	mercury_compile__maybe_generate_stack_layouts(HLDS95, LLDS1, Verbose, 
		Stats, HLDS97), !,
	mercury_compile__maybe_dump_hlds(HLDS97, "97", "stack_layout"), !,

	{ HLDS99 = HLDS97 },
	mercury_compile__maybe_dump_hlds(HLDS99, "99", "final"), !,

	mercury_compile__maybe_do_optimize(LLDS1, Verbose, Stats, LLDS).

:- pred mercury_compile__backend_pass_by_preds(module_info, module_info,
	list(c_procedure), io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds(di, uo, out, di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds(in, out, out, di, uo) is det.

mercury_compile__backend_pass_by_preds(HLDS0, HLDS, LLDS) -->
	{ module_info_predids(HLDS0, PredIds) },
	mercury_compile__backend_pass_by_preds_2(PredIds, HLDS0, HLDS, LLDS).

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
			io__write_string("% Generating code for "),
			hlds_out__write_pred_id(ModuleInfo0, PredId),
			io__write_string("\n")
		;
			[]
		),
		mercury_compile__backend_pass_by_preds_3(ProcIds, PredId,
			PredInfo, ModuleInfo0, ModuleInfo1, Code1), !
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
	globals__io_get_globals(Globals),
	{ globals__lookup_bool_option(Globals, follow_code, FollowCode) },
	{ globals__lookup_bool_option(Globals, prev_code, PrevCode) },
	( { FollowCode = yes ; PrevCode = yes } ->
		{ move_follow_code_in_proc(ProcInfo0, ProcInfo1,
			ModuleInfo0, ModuleInfo1) }
	;
		{ ProcInfo1 = ProcInfo0 },
		{ ModuleInfo1 = ModuleInfo0 }
	),
	{ simplify__find_simplifications(no, Globals, Simplifications) },
	simplify__proc([do_once | Simplifications], PredId, ProcId,
		ModuleInfo1, ModuleInfo2, ProcInfo1, ProcInfo2, _, _),
	{ globals__lookup_bool_option(Globals, optimize_saved_vars,
		SavedVars) },
	( { SavedVars = yes } ->
		saved_vars_proc(PredId, ProcId, ProcInfo2, ProcInfo3,
			ModuleInfo2, ModuleInfo3)
	;
		{ ProcInfo3 = ProcInfo2 },
		{ ModuleInfo3 = ModuleInfo2 }
	),
	write_proc_progress_message("% Computing liveness in ", PredId, ProcId,
		ModuleInfo3),
	{ detect_liveness_proc(ProcInfo3, ModuleInfo3, ProcInfo4) },
	write_proc_progress_message("% Allocating stack slots in ", PredId,
		                ProcId, ModuleInfo3),
	{ allocate_stack_slots_in_proc(ProcInfo4, ModuleInfo3, ProcInfo5) },
	write_proc_progress_message(
		"% Allocating storage locations for live vars in ",
				PredId, ProcId, ModuleInfo3),
	{ store_alloc_in_proc(ProcInfo5, ModuleInfo3, ProcInfo6) },
	globals__io_get_trace_level(TraceLevel),
	( { TraceLevel = interface ; TraceLevel = full } ->
		write_proc_progress_message(
			"% Calculating goal paths in ",
					PredId, ProcId, ModuleInfo3),
		{ goal_path__fill_slots(ProcInfo6, ModuleInfo3, ProcInfo7) }
	;
		{ ProcInfo7 = ProcInfo6 }
	),
	write_proc_progress_message(
		"% Generating low-level (LLDS) code for ",
				PredId, ProcId, ModuleInfo3),
	{ module_info_get_continuation_info(ModuleInfo3, ContInfo0) },
	{ module_info_get_cell_count(ModuleInfo3, CellCount0) },
	{ generate_proc_code(ProcInfo7, ProcId, PredId, ModuleInfo3, Globals,
		ContInfo0, CellCount0, ContInfo1, CellCount, Proc0) },
	{ module_info_set_continuation_info(ModuleInfo3, ContInfo1, 
		ModuleInfo4) },
	{ module_info_set_cell_count(ModuleInfo4, CellCount, ModuleInfo5) },
	{ globals__lookup_bool_option(Globals, optimize, Optimize) },
	( { Optimize = yes } ->
		optimize__proc(Proc0, Proc)
	;
		{ Proc = Proc0 }
	),
	{ globals__lookup_bool_option(Globals, basic_stack_layout,
		BasicStackLayout) },
	( { BasicStackLayout = yes } ->
		{ Proc = c_procedure(_, _, PredProcId, Instructions) },
		{ module_info_get_continuation_info(ModuleInfo5, ContInfo2) },
		{ globals__lookup_bool_option(Globals, agc_stack_layout,
			AgcStackLayout) },
		write_proc_progress_message(
			"% Generating call continuation information for ",
				PredId, ProcId, ModuleInfo5),
		{ continuation_info__process_instructions(PredProcId,
			Instructions, AgcStackLayout, ContInfo2, ContInfo3) },
		{ module_info_set_continuation_info(ModuleInfo5, ContInfo3, 
			ModuleInfo) }
	;
		{ ModuleInfo = ModuleInfo5 }
	).
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__puritycheck(module_info, bool, bool,
				module_info, io__state, io__state).
:- mode mercury_compile__puritycheck(in, in, in, out, di, uo) is det.

mercury_compile__puritycheck(HLDS0, Verbose, Stats, HLDS) -->
	{ module_info_num_errors(HLDS0, NumErrors0) },
	puritycheck(HLDS0, HLDS),
	{ module_info_num_errors(HLDS, NumErrors) },
	( { NumErrors \= NumErrors0 } ->
		maybe_write_string(Verbose,
			"% Program contains purity error(s).\n"),
		io__set_exit_status(1)
	;
		maybe_write_string(Verbose,
			"% Program is purity-correct.\n")
	),
	maybe_report_stats(Stats).

:- pred mercury_compile__modecheck(module_info, bool, bool,
				module_info, bool, bool, io__state, io__state).
:- mode mercury_compile__modecheck(in, in, in, out, out, out, di, uo) is det.

mercury_compile__modecheck(HLDS0, Verbose, Stats, HLDS, FoundModeError,
		UnsafeToContinue) -->
	{ module_info_num_errors(HLDS0, NumErrors0) },
	modecheck(HLDS0, HLDS, UnsafeToContinue),
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
	),
	maybe_report_stats(Stats).

:- pred mercury_compile__detect_switches(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__detect_switches(in, in, in, out, di, uo) is det.

mercury_compile__detect_switches(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Detecting switches...\n"),
	maybe_flush_output(Verbose),
	detect_switches(HLDS0, HLDS),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__detect_cse(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__detect_cse(in, in, in, out, di, uo) is det.

mercury_compile__detect_cse(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(common_goal, CommonGoal),
	( { CommonGoal = yes } ->
		maybe_write_string(Verbose, "% Detecting common deconstructions...\n"),
		detect_cse(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__check_determinism(module_info, bool, bool,
	module_info, bool, io__state, io__state).
:- mode mercury_compile__check_determinism(in, in, in, out, out, di, uo) is det.

mercury_compile__check_determinism(HLDS0, Verbose, Stats, HLDS, FoundError) -->
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
	),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_termination(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_termination(in, in, in, out, di, uo) is det.

mercury_compile__maybe_termination(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(polymorphism, Polymorphism),
	globals__io_lookup_bool_option(termination, Termination),
	% Termination analysis requires polymorphism to be run,
	% as termination analysis does not handle complex unification
	( 
		{ Polymorphism = yes },
		{ Termination = yes }
	->
		maybe_write_string(Verbose, "% Detecting termination...\n"),
		termination__pass(HLDS0, HLDS),
		maybe_write_string(Verbose, "% Termination checking done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__check_unique_modes(module_info, bool, bool,
	module_info, bool, io__state, io__state).
:- mode mercury_compile__check_unique_modes(in, in, in, out, out, di, uo)
	is det.

mercury_compile__check_unique_modes(HLDS0, Verbose, Stats, HLDS, FoundError) -->
	maybe_write_string(Verbose,
		"% Checking for backtracking over unique modes...\n"),
	{ module_info_num_errors(HLDS0, NumErrors0) },
	unique_modes__check_module(HLDS0, HLDS),
	{ module_info_num_errors(HLDS, NumErrors) },
	( { NumErrors \= NumErrors0 } ->
		{ FoundError = yes },
		maybe_write_string(Verbose,
			"% Program contains unique mode error(s).\n"),
		io__set_exit_status(1)
	;
		{ FoundError = no },
		maybe_write_string(Verbose,
			"% Program is unique-mode-correct.\n")
	),
	maybe_report_stats(Stats).

:- pred mercury_compile__check_stratification(module_info, bool, bool,
	module_info, bool, io__state, io__state).
:- mode mercury_compile__check_stratification(in, in, in, out, out, di, uo)
	is det.

mercury_compile__check_stratification(HLDS0, Verbose, Stats, HLDS, 
		FoundError) -->
	{ module_info_stratified_preds(HLDS0, StratifiedPreds) },
	globals__io_lookup_bool_option(warn_non_stratification, Warn),
	(
		  { \+ set__empty(StratifiedPreds)
		  ; Warn = yes }
	->
		maybe_write_string(Verbose,
			"% Checking stratification...\n"),
		io__get_exit_status(OldStatus),
		io__set_exit_status(0),
		stratify__check_stratification(HLDS0, HLDS),
		io__get_exit_status(NewStatus),
		( { NewStatus \= 0 } ->
			{ FoundError = yes },
			maybe_write_string(Verbose,
				"% Program contains stratification error(s).\n")
		;
			{ FoundError = no },
			maybe_write_string(Verbose,
				"% done.\n"),
			io__set_exit_status(OldStatus)
		),
		maybe_report_stats(Stats)
	;
		{ FoundError = no },
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__simplify(module_info, bool, bool, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__simplify(in, in, in, in, in, out, di, uo) is det.

mercury_compile__simplify(HLDS0, Warn, Once, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Simplifying goals...\n"),
	maybe_flush_output(Verbose),
	globals__io_get_globals(Globals),
	{ simplify__find_simplifications(Warn, Globals, Simplifications0) },
	( { Once = yes } ->
		{ Simplifications = [do_once | Simplifications0] }
	;
		{ Simplifications = Simplifications0 }
	),
	process_all_nonimported_procs(
		update_proc_error(simplify__proc(Simplifications)),
		HLDS0, HLDS),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_write_dependency_graph(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_write_dependency_graph(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_write_dependency_graph(ModuleInfo0, Verbose, Stats,
		ModuleInfo) -->
	globals__io_lookup_bool_option(show_dependency_graph, ShowDepGraph),
	( { ShowDepGraph = yes } ->
		maybe_write_string(Verbose, "% Writing dependency graph..."),
		{ module_info_name(ModuleInfo0, ModuleName) },
		module_name_to_file_name(ModuleName, ".dependency_graph", yes,
			FileName),
		io__tell(FileName, Res),
		( { Res = ok } ->
			dependency_graph__write_dependency_graph(ModuleInfo0,
							ModuleInfo),
			io__told,
			maybe_write_string(Verbose, " done.\n")
		;
			report_error("unable to write dependency graph."),
			{ ModuleInfo0 = ModuleInfo }
		),
		maybe_report_stats(Stats)
	;
		{ ModuleInfo0 = ModuleInfo }
	).

	% Outputs the file <module_name>.prof, which contains the static
	% call graph in terms of label names, if the profiling flag is enabled.

:- pred mercury_compile__maybe_output_prof_call_graph(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_output_prof_call_graph(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_output_prof_call_graph(ModuleInfo0, Verbose, Stats,
		ModuleInfo) -->
	globals__io_lookup_bool_option(profile_calls, ProfileCalls),
	globals__io_lookup_bool_option(profile_time, ProfileTime),
	(
		{ ProfileCalls = yes ; ProfileTime = yes }
	->
		maybe_write_string(Verbose, "% Outputing profiling call graph..."),
		maybe_flush_output(Verbose),
		{ module_info_name(ModuleInfo0, ModuleName) },
		module_name_to_file_name(ModuleName, ".prof", yes,
						ProfFileName),
		io__tell(ProfFileName, Res),
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
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ ModuleInfo = ModuleInfo0 }
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__tabling(module_info, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__tabling(in, in, out, di, uo) is det.

mercury_compile__tabling(HLDS0, Verbose, HLDS) -->
	maybe_write_string(Verbose,
		"% Transforming tabled predicates..."),
	maybe_flush_output(Verbose),
	{ table_gen__process_module(HLDS0, HLDS) },
	maybe_write_string(Verbose, " done.\n").

%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_polymorphism(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_polymorphism(in, in, in, out, di, uo) is det.

mercury_compile__maybe_polymorphism(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(polymorphism, Polymorphism),
	( { Polymorphism = yes } ->
		maybe_write_string(Verbose,
			"% Transforming polymorphic unifications..."),
		maybe_flush_output(Verbose),
		polymorphism__process_module(HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__maybe_base_type_infos(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_base_type_infos(in, in, in, out, di, uo) is det.

mercury_compile__maybe_base_type_infos(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose,
		"% Generating base_type_info structures..."),
	maybe_flush_output(Verbose),
	{ base_type_info__generate_hlds(HLDS0, HLDS) },
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

	% We only add base_type_layouts if shared-one-or-two-cell
	% type_infos are being used (the layouts refer to the
	% base_type_infos, so will fail to link without them).

:- pred mercury_compile__maybe_base_type_layouts(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_base_type_layouts(in, in, in, out, di, uo) is det.

mercury_compile__maybe_base_type_layouts(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(type_layout, TypeLayoutOption),
	( 
		{ TypeLayoutOption = yes } 
	->
		maybe_write_string(Verbose,
			"% Generating base_type_layout structures..."),
		maybe_flush_output(Verbose),
		{ base_type_layout__generate_hlds(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__maybe_bytecodes(module_info, module_name, bool, bool,
	io__state, io__state).
:- mode mercury_compile__maybe_bytecodes(in, in, in, in, di, uo) is det.

mercury_compile__maybe_bytecodes(HLDS0, ModuleName, Verbose, Stats) -->
	globals__io_lookup_bool_option(generate_bytecode, GenBytecode),
	( { GenBytecode = yes } ->
		mercury_compile__map_args_to_regs(HLDS0, Verbose, Stats, HLDS1),
		mercury_compile__maybe_dump_hlds(HLDS1, "31",
			"bytecode_args_to_regs"),
		maybe_write_string(Verbose,
			"% Generating bytecodes...\n"),
		maybe_flush_output(Verbose),
		bytecode_gen__module(HLDS1, Bytecode),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats),
		module_name_to_file_name(ModuleName, ".bytedebug", yes,
			BytedebugFile),
		maybe_write_string(Verbose,
			"% Writing bytecodes to `"),
		maybe_write_string(Verbose, BytedebugFile),
		maybe_write_string(Verbose, "'..."),
		maybe_flush_output(Verbose),
		debug_bytecode_file(BytedebugFile, Bytecode),
		maybe_write_string(Verbose, " done.\n"),
		module_name_to_file_name(ModuleName, ".mbc", yes, BytecodeFile),
		maybe_write_string(Verbose,
			"% Writing bytecodes to `"),
		maybe_write_string(Verbose, BytecodeFile),
		maybe_write_string(Verbose, "'..."),
		maybe_flush_output(Verbose),
		output_bytecode_file(BytecodeFile, Bytecode),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		[]
	).

:- pred mercury_compile__maybe_higher_order(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_higher_order(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_higher_order(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(optimize_higher_order, Optimize),
	( { Optimize = yes } ->
		maybe_write_string(Verbose,
				"% Specializing higher-order predicates...\n"),
		maybe_flush_output(Verbose),
		specialize_higher_order(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_do_inlining(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_do_inlining(in, in, in, out, di, uo) is det.

mercury_compile__maybe_do_inlining(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(errorcheck_only, ErrorCheckOnly),
	globals__io_lookup_bool_option(inline_simple, Simple),
	globals__io_lookup_bool_option(inline_single_use, SingleUse),
	globals__io_lookup_int_option(inline_compound_threshold, Threshold),
	(
		{ ErrorCheckOnly = no },
		{ Simple = yes ; SingleUse = yes ; Threshold > 0 }
	->
		maybe_write_string(Verbose, "% Inlining...\n"),
		maybe_flush_output(Verbose),
		inlining(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__maybe_deforestation(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_deforestation(in, in, in, out, di, uo) is det.

mercury_compile__maybe_deforestation(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(deforestation, Deforest),
	( { Deforest = yes } ->
		maybe_write_string(Verbose, "% Deforestation...\n"),
		maybe_flush_output(Verbose),
		deforestation(HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_transform_dnf(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_transform_dnf(in, in, in, out, di, uo) is det.

mercury_compile__maybe_transform_dnf(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(aditi, Aditi),
	( { Aditi = yes } ->
		maybe_write_string(Verbose, "% Disjunctive normal form transformation..."),
		maybe_flush_output(Verbose),
		{ dnf__transform_module(HLDS0, no, no, HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_constraints(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_constraints(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_constraints(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(constraint_propagation, ConstraintProp),
	( { ConstraintProp = yes } ->
		maybe_write_string(Verbose, "% Propagating constraints..."),
		maybe_flush_output(Verbose),
		constraint_propagation(HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_unused_args(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_unused_args(in, in, in, out, di, uo) is det.

mercury_compile__maybe_unused_args(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(intermod_unused_args, Intermod),
	globals__io_lookup_bool_option(optimize_unused_args, Optimize),
	globals__io_lookup_bool_option(warn_unused_args, Warn),
	( { Optimize = yes; Warn = yes ; Intermod = yes } ->
		maybe_write_string(Verbose, "% Finding unused arguments ...\n"),
		maybe_flush_output(Verbose),
		unused_args__process_module(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_dead_procs(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_dead_procs(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_dead_procs(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(optimize_dead_procs, Dead),
	( { Dead = yes } ->
		maybe_write_string(Verbose, "% Eliminating dead procedures...\n"),
		maybe_flush_output(Verbose),
		dead_proc_elim(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_lco(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_lco(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_lco(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(optimize_constructor_last_call, LCO),
	( { LCO = yes } ->
		maybe_write_string(Verbose, "% Looking for LCO modulo constructor application ...\n"),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(
			update_proc_io(lco_modulo_constructors), HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

%-----------------------------------------------------------------------------%

% The backend passes

:- pred mercury_compile__map_args_to_regs(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__map_args_to_regs(in, in, in, out, di, uo) is det.

mercury_compile__map_args_to_regs(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Mapping args to regs..."),
	maybe_flush_output(Verbose),
	{ generate_arg_info(HLDS0, HLDS) },
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_saved_vars(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_saved_vars(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_saved_vars(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(optimize_saved_vars, SavedVars),
	( { SavedVars = yes } ->
		maybe_write_string(Verbose, "% Reordering to minimize variable saves...\n"),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(update_module_io(
			saved_vars_proc), HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_followcode(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_followcode(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_followcode(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(follow_code, FollowCode),
	globals__io_lookup_bool_option(prev_code, PrevCode),
	( { FollowCode = yes ; PrevCode = yes } ->
		maybe_write_string(Verbose, "% Migrating branch code..."),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(update_module(
			move_follow_code_in_proc), HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__compute_liveness(module_info, bool, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__compute_liveness(in, in, in, out, di, uo) is det.

mercury_compile__compute_liveness(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Computing liveness...\n"),
	maybe_flush_output(Verbose),
	process_all_nonimported_procs(update_proc(detect_liveness_proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__compute_stack_vars(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__compute_stack_vars(in, in, in, out, di, uo) is det.

mercury_compile__compute_stack_vars(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Computing stack vars..."),
	maybe_flush_output(Verbose),
	process_all_nonimported_procs(update_proc(allocate_stack_slots_in_proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__allocate_store_map(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__allocate_store_map(in, in, in, out, di, uo) is det.

mercury_compile__allocate_store_map(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Allocating store map..."),
	maybe_flush_output(Verbose),
	process_all_nonimported_procs(update_proc(store_alloc_in_proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_goal_paths(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_goal_paths(in, in, in, out, di, uo) is det.

mercury_compile__maybe_goal_paths(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_get_trace_level(TraceLevel),
	( { TraceLevel = interface ; TraceLevel = full } ->
		maybe_write_string(Verbose, "% Calculating goal paths..."),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(
			update_proc(goal_path__fill_slots),
			HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__generate_code(module_info, bool, bool, module_info,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__generate_code(in, in, in, out, out, di, uo) is det.

mercury_compile__generate_code(HLDS0, Verbose, Stats, HLDS, LLDS) -->
	maybe_write_string(Verbose, "% Generating code...\n"),
	maybe_flush_output(Verbose),
	generate_code(HLDS0, HLDS, LLDS),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_do_optimize(list(c_procedure), bool, bool,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__maybe_do_optimize(in, in, in, out, di, uo) is det.

mercury_compile__maybe_do_optimize(LLDS0, Verbose, Stats, LLDS) -->
	globals__io_lookup_bool_option(optimize, Optimize),
	( { Optimize = yes } ->
		maybe_write_string(Verbose,
			"% Doing optimizations...\n"),
		maybe_flush_output(Verbose),
		optimize_main(LLDS0, LLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ LLDS = LLDS0 }
	).

:- pred mercury_compile__maybe_generate_stack_layouts(module_info, 
	list(c_procedure), bool, bool, module_info, io__state, io__state).
:- mode mercury_compile__maybe_generate_stack_layouts(in, in, in, in, out, 
	di, uo) is det.

mercury_compile__maybe_generate_stack_layouts(ModuleInfo0, LLDS0, Verbose, 
		Stats, ModuleInfo) -->
	globals__io_lookup_bool_option(basic_stack_layout, BasicStackLayout),
	( { BasicStackLayout = yes } ->
		maybe_write_string(Verbose,
			"% Generating call continuation information..."),
		maybe_flush_output(Verbose),
		globals__io_lookup_bool_option(agc_stack_layout, 
			AgcStackLayout),
		{ module_info_get_continuation_info(ModuleInfo0, ContInfo0) },
		{ continuation_info__process_llds(LLDS0, AgcStackLayout,
			ContInfo0, ContInfo) },
		{ module_info_set_continuation_info(ModuleInfo0, ContInfo,
			ModuleInfo) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ ModuleInfo = ModuleInfo0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%
% Gather together the information from the HLDS that is
% used for the C interface.  This stuff mostly just gets
% passed directly to the LLDS unchanged, but we do do
% a bit of code generation -- for example, we call
% export__get_pragma_exported_procs here, which does the
% generation of C code for `pragma export' declarations.
%

:- pred get_c_interface_info(module_info, c_interface_info).
:- mode get_c_interface_info(in, out) is det.

get_c_interface_info(HLDS, C_InterfaceInfo) :-
	module_info_name(HLDS, ModuleName),
	module_info_get_c_header(HLDS, C_HeaderCode),
	module_info_get_c_body_code(HLDS, C_BodyCode),
	export__get_c_export_decls(HLDS, C_ExportDecls),
	export__get_c_export_defns(HLDS, C_ExportDefns),
	C_InterfaceInfo = c_interface_info(ModuleName,
		C_HeaderCode, C_BodyCode, C_ExportDecls, C_ExportDefns).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The LLDS output pass

:- pred mercury_compile__output_pass(module_info, list(c_procedure),
		module_name, bool, io__state, io__state).
:- mode mercury_compile__output_pass(in, in, in, out, di, uo) is det.

mercury_compile__output_pass(HLDS0, LLDS0, ModuleName, CompileErrors) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	globals__io_lookup_bool_option(basic_stack_layout, BasicStackLayout),

	{ base_type_info__generate_llds(HLDS0, BaseTypeInfos) },
	{ base_type_layout__generate_llds(HLDS0, HLDS1, BaseTypeLayouts) },
	{ BasicStackLayout = yes ->
		stack_layout__generate_llds(HLDS1, HLDS,
			StackLayouts, StackLayoutLabelMap),
		list__append(StackLayouts, BaseTypeLayouts, StaticData0)
	;
		HLDS = HLDS1,
		set_bbbtree__init(StackLayoutLabelMap),
		StaticData0 = BaseTypeLayouts
	},
	{ get_c_interface_info(HLDS, C_InterfaceInfo) },

	{ llds_common(LLDS0, StaticData0, ModuleName, LLDS1, 
		StaticData, CommonData) },

	{ list__append(BaseTypeInfos, StaticData, AllData) },
	mercury_compile__chunk_llds(C_InterfaceInfo, LLDS1, AllData,
		CommonData, LLDS2, NumChunks),
	mercury_compile__output_llds(ModuleName, LLDS2, StackLayoutLabelMap,
		Verbose, Stats),

	{ C_InterfaceInfo = c_interface_info(_ModuleName,
		_C_headerCode, _C_BodyCode, C_ExportDecls, _C_ExportDefns) },
	export__produce_header_file(C_ExportDecls, ModuleName),

	globals__io_lookup_bool_option(compile_to_c, CompileToC),
	( { CompileToC = no } ->
		mercury_compile__c_to_obj(ModuleName, NumChunks, CompileOK),
		{ bool__not(CompileOK, CompileErrors) }
	;
		{ CompileErrors = no }
	).

	% Split the code up into bite-size chunks for the C compiler.

:- pred mercury_compile__chunk_llds(c_interface_info, list(c_procedure),
	list(c_module), list(c_module), c_file, int, io__state, io__state).
% :- mode mercury_compile__chunk_llds(in, di, di, uo, out, di, uo) is det.
:- mode mercury_compile__chunk_llds(in, in, in, in, out, out, di, uo) is det.

mercury_compile__chunk_llds(C_InterfaceInfo, Procedures, BaseTypeData,
		CommonDataModules,
		c_file(ModuleName, C_HeaderCode, ModuleList), NumChunks) -->
	{ C_InterfaceInfo = c_interface_info(ModuleName,
		C_HeaderCode0, C_BodyCode0, C_ExportDecls, C_ExportDefns) },
	{ llds_out__sym_name_mangle(ModuleName, MangledModName) },
	{ string__append(MangledModName, "_module", ModName) },
	globals__io_lookup_int_option(procs_per_c_function, ProcsPerFunc),
	{ get_c_body_code(C_BodyCode0, C_BodyCode) },
	( { ProcsPerFunc = 0 } ->
		% ProcsPerFunc = 0 really means infinity -
		% we store all the procs in a single function.
		{ ProcModules = [c_module(ModName, Procedures)] }
	;
		{ list__chunk(Procedures, ProcsPerFunc, ChunkList) },
		{ mercury_compile__combine_chunks(ChunkList, ModName,
			ProcModules) }
	),
	maybe_add_header_file_include(C_ExportDecls, ModuleName, C_HeaderCode0,
		C_HeaderCode1),
	globals__io_get_trace_level(TraceLevel),
	( { TraceLevel = interface ; TraceLevel = full } ->
		{ term__context_init(Context) },
		{ TraceInclude = "#include ""mercury_trace.h""\n" - Context },
		{ list__append(C_HeaderCode1, [TraceInclude], C_HeaderCode) }
	;
		{ C_HeaderCode = C_HeaderCode1 }
	),
	{ list__condense([C_BodyCode, BaseTypeData, CommonDataModules,
		ProcModules, [c_export(C_ExportDefns)]], ModuleList) },
	{ list__length(ModuleList, NumChunks) }.

:- pred maybe_add_header_file_include(c_export_decls, module_name,
	c_header_info, c_header_info, io__state, io__state).
:- mode maybe_add_header_file_include(in, in, in, out, di, uo) is det.

maybe_add_header_file_include(C_ExportDecls, ModuleName, 
		C_HeaderCode0, C_HeaderCode) -->
	(
		{ C_ExportDecls = [] },
		{ C_HeaderCode = C_HeaderCode0 }
	;
		{ C_ExportDecls = [_|_] },
		module_name_to_file_name(ModuleName, ".h", no, HeaderFileName),
                globals__io_lookup_bool_option(split_c_files, SplitFiles),
                { 
			SplitFiles = yes,
                        string__append_list(
                                ["#include ""../", HeaderFileName, """\n"],
				Include0)
                ;
			SplitFiles = no,
                        string__append_list(
				["#include """, HeaderFileName, """\n"],
				Include0)
                },

		{ term__context_init(Context) },
		{ Include = Include0 - Context },

			% We put the new include at the end since the list is
			% stored in reverse, and we want this include to come
			% first.
		{ list__append(C_HeaderCode0, [Include], C_HeaderCode) }
	).

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

:- pred mercury_compile__combine_chunks_2(list(list(c_procedure)),
		string, int, list(c_module)).
:- mode mercury_compile__combine_chunks_2(in, in, in, out) is det.

mercury_compile__combine_chunks_2([], _ModName, _N, []).
mercury_compile__combine_chunks_2([Chunk|Chunks], ModName, Num,
		[Module | Modules]) :-
	string__int_to_string(Num, NumString),
	string__append(ModName, NumString, ThisModuleName),
	Module = c_module(ThisModuleName, Chunk),
	Num1 is Num + 1,
	mercury_compile__combine_chunks_2(Chunks, ModName, Num1, Modules).

:- pred mercury_compile__output_llds(module_name, c_file, set_bbbtree(label),
	bool, bool, io__state, io__state).
:- mode mercury_compile__output_llds(in, in, in, in, in, di, uo) is det.

mercury_compile__output_llds(ModuleName, LLDS, StackLayoutLabels,
		Verbose, Stats) -->
	maybe_write_string(Verbose,
		"% Writing output to `"),
	module_name_to_file_name(ModuleName, ".c", yes, FileName),
	maybe_write_string(Verbose, FileName),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	output_c_file(LLDS, StackLayoutLabels),
	maybe_write_string(Verbose, " done.\n"),
	maybe_flush_output(Verbose),
	maybe_report_stats(Stats).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The `--high-level-C' alternative backend

:- pred mercury_compile__gen_hlds(string, module_info, io__state, io__state).
:- mode mercury_compile__gen_hlds(in, in, di, uo) is det.

mercury_compile__gen_hlds(DumpFile, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_write_string(Verbose, "% Dumping out HLDS to `"),
	maybe_write_string(Verbose, DumpFile),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__tell(DumpFile, Res),
	( { Res = ok } ->
		mercury_to_c__gen_hlds(0, HLDS),
		io__told,
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		maybe_write_string(Verbose, "\n"),
		{ string__append_list(["can't open file `",
			DumpFile, "' for output."], ErrorMessage) },
		report_error(ErrorMessage)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type compiler_type ---> gcc ; lcc ; unknown.

:- pred mercury_compile__c_to_obj(module_name, int, bool, io__state, io__state).
:- mode mercury_compile__c_to_obj(in, in, out, di, uo) is det.

mercury_compile__c_to_obj(ModuleName, NumChunks, Succeeded) -->
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = yes } ->
		mercury_compile__c_to_obj_list(ModuleName, 0, NumChunks,
			Succeeded)
	;
		module_name_to_file_name(ModuleName, ".c", no, C_File),
		module_name_to_file_name(ModuleName, ".o", yes, O_File),
		mercury_compile__single_c_to_obj(C_File, O_File, Succeeded)
	).

:- pred mercury_compile__c_to_obj_list(module_name, int, int, bool,
					io__state, io__state).
:- mode mercury_compile__c_to_obj_list(in, in, in, out, di, uo) is det.

	% compile each of the C files in `<module>.dir'

mercury_compile__c_to_obj_list(ModuleName, Chunk, NumChunks, Succeeded) -->
	( { Chunk > NumChunks } ->
		{ Succeeded = yes }
	;
		module_name_to_split_c_file_name(ModuleName, Chunk,
			".c", C_File),
		module_name_to_split_c_file_name(ModuleName, Chunk,
			".o", O_File),
		mercury_compile__single_c_to_obj(C_File, O_File, Succeeded0),
		( { Succeeded0 = no } ->
			{ Succeeded = no }
		;
			{ Chunk1 is Chunk + 1 },
			mercury_compile__c_to_obj_list(ModuleName,
				Chunk1, NumChunks, Succeeded)
		)
	).

:- pred mercury_compile__single_c_to_obj(string, string, bool,
					io__state, io__state).
:- mode mercury_compile__single_c_to_obj(in, in, out, di, uo) is det.

mercury_compile__single_c_to_obj(C_File, O_File, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Compiling `"),
	maybe_write_string(Verbose, C_File),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(cc, CC),
	globals__io_lookup_accumulating_option(cflags, C_Flags_List),
	{ join_string_list(C_Flags_List, "", "", " ", CFLAGS) },

	globals__io_lookup_bool_option(use_subdirs, UseSubdirs),
	{ UseSubdirs = yes ->
		% the file will be compiled in the "Mercury/cs" subdir,
		% so we need to add `-I.' so it can
		% include header files in the source directory.
		SubDirInclOpt = "-I. "
	;
		SubDirInclOpt = ""
	},
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
		{ RegOpt = "-DUSE_GCC_GLOBAL_REGISTERS " }
	;
		{ CFLAGS_FOR_REGS = "" },
		{ RegOpt = "" }
	),
	globals__io_lookup_bool_option(gcc_non_local_gotos, GCC_Gotos),
	( { GCC_Gotos = yes } ->
		{ GotoOpt = "-DUSE_GCC_NONLOCAL_GOTOS " },
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
	; GC_Method = accurate ->
		GC_Opt = "-DNATIVE_GC "
	;
		GC_Opt = ""
	},
	globals__io_lookup_bool_option(profile_calls, ProfileCalls),
	{ ProfileCalls = yes ->
		ProfileCallsOpt = "-DPROFILE_CALLS "
	;
		ProfileCallsOpt = ""
	},
	globals__io_lookup_bool_option(profile_time, ProfileTime),
	{ ProfileTime = yes ->
		ProfileTimeOpt = "-DPROFILE_TIME "
	;
		ProfileTimeOpt = ""
	},
	globals__io_lookup_bool_option(profile_memory, ProfileMemory),
	{ ProfileMemory = yes ->
		ProfileMemoryOpt = "-DPROFILE_MEMORY "
	;
		ProfileMemoryOpt = ""
	},
	globals__io_lookup_bool_option(pic_reg, PIC_Reg),
	{ PIC_Reg = yes ->
		PIC_Reg_Opt = "-DPIC_REG "
	;
		PIC_Reg_Opt = ""
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
	globals__io_lookup_bool_option(require_tracing, RequireTracing),
	{ RequireTracing = yes ->
		RequireTracingOpt = "-DMR_REQUIRE_TRACING "
	;
		RequireTracingOpt = ""
	},
	globals__io_lookup_bool_option(stack_trace, StackTrace),
	{ StackTrace = yes ->
		StackTraceOpt = "-DMR_STACK_TRACE "
	;
		StackTraceOpt = ""
	},
	globals__io_lookup_bool_option(c_debug, C_Debug),
	{ C_Debug = yes ->
		C_DebugOpt = "-g "
	;
		C_DebugOpt = ""
	},
	globals__io_lookup_bool_option(low_level_debug, LL_Debug),
	{ LL_Debug = yes ->
		LL_DebugOpt = "-DMR_LOW_LEVEL_DEBUG "
	;
		LL_DebugOpt = ""
	},
	{ string__sub_string_search(CC, "gcc", _) ->
		CompilerType = gcc
	; string__sub_string_search(CC, "lcc", _) ->
		CompilerType = lcc
	;
		CompilerType = unknown
	},
	globals__io_lookup_bool_option(use_trail, UseTrail),
	{ UseTrail = yes ->
		UseTrailOpt = "-DMR_USE_TRAIL "
	;
		UseTrailOpt = ""
	},
	globals__io_get_args_method(ArgsMethod),
	{ ArgsMethod = compact ->
		ArgsOpt = "-DCOMPACT_ARGS "
	;
		ArgsOpt = ""
	},
	globals__io_lookup_bool_option(type_layout, TypeLayoutOption),
	{ TypeLayoutOption = no ->
		TypeLayoutOpt = "-DNO_TYPE_LAYOUT "
	;
		TypeLayoutOpt = ""
	},
	globals__io_lookup_bool_option(c_optimize, C_optimize),
	{ C_optimize = yes ->
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
	globals__io_lookup_bool_option(inline_alloc, InlineAlloc),
	{ InlineAlloc = yes ->
		InlineAllocOpt = "-DINLINE_ALLOC -DSILENT "
	;
		InlineAllocOpt = ""
	},
	{ CompilerType = gcc ->
		% if --inline-alloc is enabled, don't enable missing-prototype
		% warnings, since gc_inline.h is missing lots of prototypes
		( InlineAlloc = yes ->
			WarningOpt = "-Wall -Wwrite-strings -Wpointer-arith -Wcast-qual -Wtraditional -Wshadow -Wmissing-prototypes -Wno-unused -Wno-uninitialized "
		;
			WarningOpt = "-Wall -Wwrite-strings -Wpointer-arith -Wcast-qual -Wtraditional -Wshadow -Wmissing-prototypes -Wno-unused -Wno-uninitialized -Wstrict-prototypes "
		)
	; CompilerType = lcc ->
		WarningOpt = "-w "
	;
		WarningOpt = ""
	},
	% Be careful with the order here!  Some options override others,
	% e.g. CFLAGS_FOR_REGS must come after OptimizeOpt so that
	% it can override -fomit-frame-pointer with -fno-omit-frame-pointer.
	% Also be careful that each option is separated by spaces.
	{ string__append_list([CC, " ", SubDirInclOpt, InclOpt,
		SplitOpt, OptimizeOpt,
		RegOpt, GotoOpt, AsmOpt,
		CFLAGS_FOR_REGS, " ", CFLAGS_FOR_GOTOS, " ",
		GC_Opt, ProfileCallsOpt, ProfileTimeOpt, ProfileMemoryOpt,
		PIC_Reg_Opt, TagsOpt, NumTagBitsOpt,
		C_DebugOpt, LL_DebugOpt,
		StackTraceOpt, RequireTracingOpt,
		UseTrailOpt, ArgsOpt, TypeLayoutOpt,
		InlineAllocOpt, WarningOpt, CFLAGS,
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
	globals__io_lookup_bool_option(statistics, Stats),
	globals__io_lookup_string_option(output_file_name, OutputFileName0),
	( { OutputFileName0 = "" } ->
	    ( { Modules = [Module | _] } ->
		{ OutputFileName = Module }
	    ;
		{ error("link_module_list: no modules") }
	    )
	;
	    { OutputFileName = OutputFileName0 }
	),

	{ file_name_to_module_name(OutputFileName, ModuleName) },
	module_name_to_file_name(ModuleName, "_init.c", yes, InitCFileName),
	module_name_to_file_name(ModuleName, "_init.o", yes, InitObjFileName),

	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { SplitFiles = yes } ->
	    module_name_to_file_name(ModuleName, ".a", yes, SplitLibFileName),
	    join_module_list(Modules, ".dir/*.o", [], ObjectList),
	    { list__append(
		["ar cr ", SplitLibFileName, " " | ObjectList],
		[" && ranlib ", SplitLibFileName],
		MakeLibCmdList) },
	    { string__append_list(MakeLibCmdList, MakeLibCmd) },
	    invoke_system_command(MakeLibCmd, MakeLibCmdOK),
	    { Objects = SplitLibFileName }
	;
	    { MakeLibCmdOK = yes },
	    join_module_list(Modules, ".o", [], ObjectsList),
	    { string__append_list(ObjectsList, Objects) }
	),
	( { MakeLibCmdOK = no } ->
	    report_error("creation of object file library failed.")
	;
	    % create the initialization C file
	    maybe_write_string(Verbose, "% Creating initialization file...\n"),
	    join_module_list(Modules, ".m", ["> ", InitCFileName], MkInitCmd0),
	    { string__append_list(["c2init " | MkInitCmd0], MkInitCmd) },
	    invoke_system_command(MkInitCmd, MkInitOK),
	    maybe_report_stats(Stats),
	    ( { MkInitOK = no } ->
		report_error("creation of init file failed.")
	    ;
		% compile it
		maybe_write_string(Verbose,
			"% Compiling initialization file...\n"),
		mercury_compile__single_c_to_obj(InitCFileName, InitObjFileName,
			CompileOK),
		maybe_report_stats(Stats),
		( { CompileOK = no } ->
		    report_error("compilation of init file failed.")
		;
		    maybe_write_string(Verbose, "% Linking...\n"),
		    globals__io_get_globals(Globals),
		    { compute_grade(Globals, Grade) },
		    globals__io_lookup_accumulating_option(link_flags,
				LinkFlagsList),
		    { join_string_list(LinkFlagsList, "", "", " ", LinkFlags) },
		    globals__io_lookup_accumulating_option(
				link_library_directories,
				LinkLibraryDirectoriesList),
		    { join_string_list(LinkLibraryDirectoriesList, "-L", "",
				" ", LinkLibraryDirectories) },
		    globals__io_lookup_accumulating_option(link_libraries,
				LinkLibrariesList),
		    { join_string_list(LinkLibrariesList, "-l", "", " ",
				LinkLibraries) },
		    globals__io_lookup_accumulating_option(link_objects,
				LinkObjectsList),
		    { join_string_list(LinkObjectsList, "", "", " ",
				LinkObjects) },
		    { string__append_list(
			["ml --grade ", Grade, " ", LinkFlags,
			" -o ", OutputFileName, " ",
			InitObjFileName, " ", Objects, " ",
			LinkObjects, " ",
			LinkLibraryDirectories, " ", LinkLibraries],
			LinkCmd) },
		    invoke_system_command(LinkCmd, LinkCmdOK),
		    maybe_report_stats(Stats),
		    ( { LinkCmdOK = no } ->
			report_error("link failed.")
		    ;
			[]
		    )
		)
	    )
	).

	% join_string_list(Strings, Prefix, Suffix, Serarator, Result)
	%
	% Appends the strings in the list `Strings' together into the
	% string Result. Each string is prefixed by Prefix, suffixed by
	% Suffix and separated by Separator.

:- pred join_string_list(list(string), string, string, string, string).
:- mode join_string_list(in, in, in, in, out) is det.

join_string_list([], _Prefix, _Suffix, _Separator, "").
join_string_list([String | Strings], Prefix, Suffix, Separator, Result) :-
	( Strings = [] ->
		string__append_list([Prefix, String, Suffix], Result)
	;
		join_string_list(Strings, Prefix, Suffix, Separator, Result0),
		string__append_list([Prefix, String, Suffix, Separator,
			Result0], Result)
	).

	% join_module_list(ModuleNames, Extension, Terminator, Result)
	%
	% The list of strings `Result' is computed from the list of strings
	% `ModuleNames', by removing any directory paths, and
	% converting the strings to file names and then back,
	% adding the specified Extension.  (This conversion ensures
	% that we follow the usual file naming conventions.)
	% Each file name is separated by a space from the next one, 
	% and the result is followed by the list of strings `Terminator'.

:- pred join_module_list(list(string), string, list(string), list(string),
			io__state, io__state).
:- mode join_module_list(in, in, in, out, di, uo) is det.

join_module_list([], _Extension, Terminator, Terminator) --> [].
join_module_list([Module | Modules], Extension, Terminator,
			[FileName, " " | Rest]) -->
	{ dir__basename(Module, BaseName) },
	{ file_name_to_module_name(BaseName, ModuleName) },
	module_name_to_file_name(ModuleName, Extension, no, FileName),
	join_module_list(Modules, Extension, Terminator, Rest).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_dump_hlds(module_info, string, string,
	io__state, io__state).
:- mode mercury_compile__maybe_dump_hlds(in, in, in, di, uo) is det.

mercury_compile__maybe_dump_hlds(HLDS, StageNum, StageName) -->
	globals__io_lookup_accumulating_option(dump_hlds, DumpStages),
	(
		{
			list__member(StageNum, DumpStages)
		;
			list__member(StageName, DumpStages)
		;
			list__member("all", DumpStages)
		;
			string__append("0", StrippedStageNum, StageNum),
			list__member(StrippedStageNum, DumpStages)
		}
	->
		{ module_info_name(HLDS, ModuleName) },
		module_name_to_file_name(ModuleName, ".hlds_dump", yes,
			BaseFileName),
		{ string__append_list(
			[BaseFileName, ".", StageNum, "-", StageName],
			DumpFile) },
		mercury_compile__dump_hlds(DumpFile, HLDS)
	;
		[]
	).

:- pred mercury_compile__dump_hlds(string, module_info, io__state, io__state).
:- mode mercury_compile__dump_hlds(in, in, di, uo) is det.

mercury_compile__dump_hlds(DumpFile, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_write_string(Verbose, "% Dumping out HLDS to `"),
	maybe_write_string(Verbose, DumpFile),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__tell(DumpFile, Res),
	( { Res = ok } ->
		hlds_out__write_hlds(0, HLDS),
		io__told,
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		maybe_write_string(Verbose, "\n"),
		{ string__append_list(["can't open file `",
			DumpFile, "' for output."], ErrorMessage) },
		report_error(ErrorMessage)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
