%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001 The University of Melbourne.
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

	%
	% the main compiler passes (mostly in order of execution)
	%

	% semantic analysis
:- import_module handle_options, prog_io, prog_out, modules, module_qual.
:- import_module equiv_type, make_hlds, typecheck, purity, polymorphism, modes.
:- import_module switch_detection, cse_detection, det_analysis, unique_modes.
:- import_module stratify, simplify.

	% high-level HLDS transformations
:- import_module check_typeclass, intermod, trans_opt, table_gen, (lambda).
:- import_module type_ctor_info, termination, higher_order, accumulator.
:- import_module inlining, deforest, dnf, magic, dead_proc_elim.
:- import_module delay_construct, unused_args, unneeded_code, lco.
:- import_module deep_profiling.

	% the LLDS back-end
:- import_module saved_vars, liveness.
:- import_module follow_code, live_vars, arg_info, store_alloc, goal_path.
:- import_module code_gen, optimize, foreign, export.
:- import_module base_typeclass_info.
:- import_module llds_common, transform_llds, llds_out.
:- import_module continuation_info, stack_layout.

	% the Aditi-RL back-end
:- import_module rl_gen, rl_opt, rl_out.

	% the bytecode back-end
:- import_module bytecode_gen, bytecode.

	% the MLDS back-end
:- import_module add_trail_ops.			% HLDS -> HLDS
:- import_module mark_static_terms.		% HLDS -> HLDS
:- import_module mlds.				% MLDS data structure
:- import_module ml_code_gen, rtti_to_mlds.	% HLDS/RTTI -> MLDS
:- import_module ml_elim_nested, ml_tailcall.	% MLDS -> MLDS
:- import_module ml_optimize.			% MLDS -> MLDS
:- import_module mlds_to_c.			% MLDS -> C
:- import_module mlds_to_java.			% MLDS -> Java
:- import_module mlds_to_ilasm.			% MLDS -> IL assembler
:- import_module maybe_mlds_to_gcc.		% MLDS -> GCC back-end
:- import_module ml_util.			% MLDS utility predicates 

	% miscellaneous compiler modules
:- import_module prog_data, hlds_module, hlds_pred, hlds_out, llds, rl.
:- import_module mercury_to_mercury, hlds_data.
:- import_module layout, dependency_graph, prog_util, rl_dump, rl_file.
:- import_module options, globals, trace_params, passes_aux.
:- import_module recompilation, recompilation_usage, recompilation_check.
:- import_module timestamp.

	% library modules
:- import_module int, list, map, set, std_util, dir, require, string, bool.
:- import_module library, getopt, set_bbbtree, term, varset, assoc_list.
:- import_module gc.

%-----------------------------------------------------------------------------%

main -->
	gc_init,
	handle_options(MaybeError, Args, Link),
	main_2(MaybeError, Args, Link).

%-----------------------------------------------------------------------------%

:- pred gc_init(io__state::di, io__state::uo) is det.

:- pragma c_code(gc_init(_IO0::di, _IO::uo), [will_not_call_mercury], "
#ifdef CONSERVATIVE_GC
	/*
	** Explicitly force the initial heap size to be at least 4 Mb.
	**
	** This works around a bug in the Boehm collector (for versions up
	** to at least 6.2) where the collector would sometimes abort with
	** the message `unexpected mark stack overflow' (e.g. in grade hlc.gc
	** on dec-alpha-osf3.2).
	**
	** Doing this should also improve performance slightly by avoiding
	** frequent garbage collection during start-up.
	*/
	GC_expand_hp(4 * 1024 * 1024);
#endif
").

%-----------------------------------------------------------------------------%

:- pred main_2(maybe(string), list(string), bool, io__state, io__state).
:- mode main_2(in, in, in, di, uo) is det.

main_2(yes(ErrorMessage), _, _) -->
	usage_error(ErrorMessage).
main_2(no, Args, Link) -->
	globals__io_lookup_bool_option(help, Help),
	globals__io_lookup_bool_option(output_grade_string, OutputGrade),
	globals__io_lookup_bool_option(filenames_from_stdin,
		FileNamesFromStdin),
	( { Help = yes } ->
		long_usage
	; { OutputGrade = yes } ->
		globals__io_get_globals(Globals),
		{ compute_grade(Globals, Grade) },
		io__write_string(Grade),
		io__write_string("\n")
	; { Args = [], FileNamesFromStdin = no } ->
		usage
	;
		process_all_args(Args, ModulesToLink),
		io__get_exit_status(ExitStatus),
		( { ExitStatus = 0 } ->
			( { Link = yes } ->
				mercury_compile__link_module_list(
					ModulesToLink)
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
			io__report_stats("full_memory_stats")
		;
			[]
		)
	).

:- pred process_all_args(list(string), list(string), io__state, io__state).
:- mode process_all_args(in, out, di, uo) is det.

process_all_args(Args, ModulesToLink) -->
	% Because of limitations in the GCC back-end,
	% we can only call the GCC back-end once (per process),
	% to generate a single assembler file, rather than
	% calling it multiple times to generate individual
	% assembler files for each module.
	% So if we're generating code using the GCC back-end,
	% we need to call run_gcc_backend here at the top level.
	globals__io_get_globals(Globals),
	( { compiling_to_asm(Globals) } ->
	    ( { Args = [FirstArg | OtherArgs] }  ->
		globals__io_lookup_bool_option(smart_recompilation, Smart),
		( { Smart = yes } ->
		    ( { OtherArgs = [] } ->
			% With smart recompilation we need to delay
			% starting the gcc backend to avoid overwriting
			% the output assembler file even if recompilation 
			% is found to be unnecessary.
		    	process_args(Args, ModulesToLink)
		    ;
			io__write_string(
"Sorry, not implemented: `--target asm' with `--smart-recompilation'\n"),
			io__write_string(
"with more than one module to compile.\n"),
			io__set_exit_status(1),
			{ ModulesToLink = [] }
		    )
		;
		    compile_using_gcc_backend(
		    	string_to_file_or_module(FirstArg),
			process_args(Args), ModulesToLink)
	        )
	    ;
		io__write_string(
"Sorry, not implemented: `--target asm' with `--filenames-from-stdin\n"),
		io__set_exit_status(1),
		{ ModulesToLink = [] }
	    )
	;
		% If we're NOT using the GCC back-end,
		% then we can just call process_args directly,
		% rather than via GCC.
	    process_args(Args, ModulesToLink)
	).

:- pred compiling_to_asm(globals::in) is semidet.
compiling_to_asm(Globals) :-
	globals__get_target(Globals, asm),
	% even if --target asm is specified,
	% it can be overridden by other options:
	OptionList = [convert_to_mercury,
		generate_dependencies, make_interface,
		make_short_interface, make_private_interface,
		make_optimization_interface,
		make_transitive_opt_interface,
		typecheck_only, errorcheck_only],
	BoolList = map((func(Opt) = Bool :-
		globals__lookup_bool_option(Globals, Opt, Bool)),
		OptionList),
	bool__or_list(BoolList) = no.

:- pred compile_using_gcc_backend(file_or_module,
		frontend_callback(list(string)),
		list(string), io__state, io__state).
:- mode compile_using_gcc_backend(in, in(frontend_callback),
		out, di, uo) is det.

compile_using_gcc_backend(FirstFileOrModule, CallBack, ModulesToLink) -->
	% The name of the assembler file that we generate
	% is based on name of the first module named
	% on the command line.  (Mmake requires this.)
	%
	% There's two cases:
	% (1) If the argument ends in ".m", we assume
	% that the argument is a file name.
	% To find the corresponding module name,
	% we would need to read in the file
	% (at least up to the first item);
	% this is needed to handle the case where
	% the module name does not match the file
	% name, e.g. file "browse.m" containing
	% ":- module mdb__browse." as its first item.
	% Rather than reading in the source file here,
	% we just pick a name
	% for the asm file based on the file name argument,
	% (e.g. "browse.s") and if necessary rename it later
	% (e.g. to "mdb.browse.s").
	%
	% (2) If the argument doesn't end in `.m',
	% then we assume it is a module name.
	% (Is it worth checking that the name doesn't
	% contain directory seperators, and issuing
	% a warning or error in that case?)
	%
	{
		FirstFileOrModule = file(FirstFileName),
		file_name_to_module_name(FirstFileName, FirstModuleName)
	;
		FirstFileOrModule = module(FirstModuleName)
	},

	% Invoke run_gcc_backend.  It will call us back,
	% and then we'll continue with the normal work of
	% the compilation, which will be done by the callback
	% function (`process_args').
	maybe_mlds_to_gcc__run_gcc_backend(FirstModuleName,
		CallBack, ModulesToLink),

	% Now we know what the real module name was, so we
	% can rename the assembler file if needed (see above).
	( { ModulesToLink = [Module | _] } ->
		{ file_name_to_module_name(Module, ModuleName) },
		globals__io_lookup_bool_option(pic, Pic),
		{ AsmExt = (Pic = yes -> ".pic_s" ; ".s") },
		module_name_to_file_name(ModuleName, AsmExt, no,
			AsmFile),
		(
			{ ModuleName \= FirstModuleName }
		->
			module_name_to_file_name(FirstModuleName,
				AsmExt, no, FirstAsmFile),
			do_rename_file(FirstAsmFile, AsmFile, Result)
		;
			{ Result = ok }
		),

		% Invoke the assembler to produce an object file,
		% if needed.
		globals__io_lookup_bool_option(target_code_only, 
				TargetCodeOnly),
		( { Result = ok, TargetCodeOnly = no } ->
			globals__io_lookup_string_option(object_file_extension,
				Obj),
			module_name_to_file_name(ModuleName, Obj,
				yes, O_File),
			mercury_compile__asm_to_obj(
				AsmFile, O_File, _AssembleOK)
		;
			[]
		)
	;
		% This can happen if smart recompilation decided
		% that nothing needed to be compiled.
		[]
	).

:- pred do_rename_file(string::in, string::in, io__res::out,
		io__state::di, io__state::uo) is det.

do_rename_file(OldFileName, NewFileName, Result) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(Verbose, "% Renaming `"),
	maybe_write_string(Verbose, OldFileName),
	maybe_write_string(Verbose, "' as `"),
	maybe_write_string(Verbose, NewFileName),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__rename_file(OldFileName, NewFileName, Result0),
	( { Result0 = error(Error0) } ->
		maybe_write_string(VeryVerbose, " failed.\n"),
		maybe_flush_output(VeryVerbose),
		{ io__error_message(Error0, ErrorMsg0) },
		% On some systems, we need to remove the existing file
		% first, if any.  So try again that way.
		maybe_write_string(VeryVerbose, "% Removing `"),
		maybe_write_string(VeryVerbose, OldFileName),
		maybe_write_string(VeryVerbose, "'..."),
		maybe_flush_output(VeryVerbose),
		io__remove_file(NewFileName, Result1),
		( { Result1 = error(Error1) } ->
			maybe_write_string(Verbose, " failed.\n"),
			maybe_flush_output(Verbose),
			{ io__error_message(Error1, ErrorMsg1) },
			{ string__append_list(["can't rename file `",
				OldFileName, "' as `", NewFileName, "': ",
				ErrorMsg0, "; and can't remove file `",
				NewFileName, "': ", ErrorMsg1], Message) },
			report_error(Message),
			{ Result = Result1 }
		;
			maybe_write_string(VeryVerbose, " done.\n"),
			maybe_write_string(VeryVerbose, "% Renaming `"),
			maybe_write_string(VeryVerbose, OldFileName),
			maybe_write_string(VeryVerbose, "' as `"),
			maybe_write_string(VeryVerbose, NewFileName),
			maybe_write_string(VeryVerbose, "' again..."),
			maybe_flush_output(VeryVerbose),
			io__rename_file(OldFileName, NewFileName, Result2),
			( { Result2 = error(Error2) } ->
				maybe_write_string(Verbose,
					" failed.\n"),
				maybe_flush_output(Verbose),
				{ io__error_message(Error2, ErrorMsg) },
				{ string__append_list(
					["can't rename file `", OldFileName,
					"' as `", NewFileName, "': ",
					ErrorMsg], Message) },
				report_error(Message)
			;
				maybe_write_string(Verbose, " done.\n")
			),
			{ Result = Result2 }
		)
	;
		maybe_write_string(Verbose, " done.\n"),
		{ Result = Result0 }
	).

:- pred process_args(list(string), list(string), io__state, io__state).
:- mode process_args(in, out, di, uo) is det.

process_args(Args, ModulesToLink) -->
	globals__io_lookup_bool_option(filenames_from_stdin,
		FileNamesFromStdin),
	( { FileNamesFromStdin = yes } ->
		process_stdin_arg_list([], ModulesToLink)
	;
		process_arg_list(Args, ModulesToLink)
	).

:- pred process_stdin_arg_list(list(string), list(string), 
		io__state, io__state).
:- mode process_stdin_arg_list(in, out, di, uo) is det.

process_stdin_arg_list(Modules0, Modules) -->
	( { Modules0 \= [] } -> garbage_collect ; [] ),
	io__read_line_as_string(FileResult),
	( 
		{ FileResult = ok(Line) },
		{ string__remove_suffix(Line, "\n", Arg0) ->
			Arg = Arg0
		;
			Arg = Line
		},
		process_arg(Arg, Module),
		{ list__append(Module, Modules0, Modules1) },
		process_stdin_arg_list(Modules1, Modules)
	;
		{ FileResult = eof },
		{ Modules = Modules0 }
	;
		{ FileResult = error(Error) },
		{ io__error_message(Error, Msg) },
		io__write_string("Error reading module name: "),
		io__write_string(Msg),
		{ Modules = Modules0 },
		io__set_exit_status(1)
	).

:- pred process_arg_list(list(string), list(string), io__state, io__state).
:- mode process_arg_list(in, out, di, uo) is det.

process_arg_list(Args, Modules) -->
	process_arg_list_2(Args, ModulesList),
	{ list__condense(ModulesList, Modules) }.

:- pred process_arg_list_2(list(string), list(list(string)),
			io__state, io__state).
:- mode process_arg_list_2(in, out, di, uo) is det.

process_arg_list_2([], []) --> [].
process_arg_list_2([Arg | Args], [Modules | ModulesList]) -->
	process_arg(Arg, Modules),
	( { Args \= [] } -> garbage_collect ; [] ),
	process_arg_list_2(Args, ModulesList).

	% Figure out whether the argument is a module name or a file name.
	% Open the specified file or module, and process it.
	% Return the list of modules (including sub-modules,
	% if they were compiled to seperate object files)
	% that should be linked into the final executable.

:- pred process_arg(string, list(string), io__state, io__state).
:- mode process_arg(in, out, di, uo) is det.

process_arg(Arg, ModulesToLink) -->
	 	% All messages go to stderr
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, _),

	{ FileOrModule = string_to_file_or_module(Arg) },
	globals__io_lookup_bool_option(generate_dependencies, GenerateDeps),
	( { GenerateDeps = yes } ->
		{ ModulesToLink = [] },
		(
			{ FileOrModule = file(FileName) },
			generate_file_dependencies(FileName)
		;
			{ FileOrModule = module(ModuleName) },
			generate_module_dependencies(ModuleName)
		)
	;
		process_module(FileOrModule, ModulesToLink)
	).

:- type file_or_module
	--->	file(file_name)
	;	module(module_name)
	.

:- func string_to_file_or_module(string) = file_or_module.

string_to_file_or_module(String) = FileOrModule :-
	( string__remove_suffix(String, ".m", FileName) ->
		% If the argument name ends in `.m', then we assume it is
		% a file name.
		FileOrModule = file(FileName)
	;
		% If it doesn't end in `.m', then we assume it is
		% a module name.  (Is it worth checking that the
		% name doesn't contain directory seperators, and issuing
		% a warning or error in that case?)
		file_name_to_module_name(String, ModuleName),
		FileOrModule = module(ModuleName) 			
	).

:- pred read_module(file_or_module, bool, module_name, file_name,
		maybe(timestamp), item_list, module_error,
		read_modules, read_modules, io__state, io__state).
:- mode read_module(in, in, out, out, out, out, out, in, out, di, uo) is det.

read_module(module(ModuleName), ReturnTimestamp, ModuleName, FileName,
		MaybeTimestamp, Items, Error, ReadModules0, ReadModules) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Parsing module `"),
	{ prog_out__sym_name_to_string(ModuleName, ModuleNameString) },
	maybe_write_string(Verbose, ModuleNameString),
	maybe_write_string(Verbose, "' and imported interfaces...\n"),
	(
		% Avoid rereading the module if it was already read
		% by recompilation_version.m.
		{ find_read_module(ReadModules0, ModuleName, ".m",
			ReturnTimestamp, Items0, MaybeTimestamp0,
			Error0, FileName0) }
	->
		{ map__delete(ReadModules0, ModuleName - ".m", ReadModules) },
		{ FileName = FileName0 },
		{ Items = Items0 },
		{ Error = Error0 },
		{ MaybeTimestamp = MaybeTimestamp0 }
	;
		{ ReadModules = ReadModules0 },
		read_mod(ModuleName, ".m", "Reading module", yes,
			ReturnTimestamp, Items, Error, FileName,
			MaybeTimestamp)
	),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_report_stats(Stats).
read_module(file(FileName), ReturnTimestamp, ModuleName, SourceFileName,
		MaybeTimestamp, Items, Error, ReadModules0, ReadModules) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Parsing file `"),
	maybe_write_string(Verbose, FileName),
	maybe_write_string(Verbose, "' and imported interfaces...\n"),

	{ file_name_to_module_name(FileName, DefaultModuleName) },
	(
		% Avoid rereading the module if it was already read
		% by recompilation_version.m.
		{ find_read_module(ReadModules0, DefaultModuleName, ".m",
			ReturnTimestamp, Items0, MaybeTimestamp0, Error0, _) }
	->
		{ map__delete(ReadModules0, ModuleName - ".m", ReadModules) },
		{ ModuleName = DefaultModuleName },
		{ Items = Items0 },
		{ Error = Error0 },
		{ MaybeTimestamp = MaybeTimestamp0 }
	;
		{ ReadModules = ReadModules0 },
		read_mod_from_file(FileName, ".m", "Reading file", yes,
			ReturnTimestamp, Items, Error, ModuleName,
			MaybeTimestamp),

		%
		% XXX If the module name doesn't match the file name
		% the compiler won't be able to find the `.used'
		% file (the name of the `.used' file is derived from
		% the module name not the file name).
		% This will be fixed when mmake functionality
		% is moved into the compiler.
		%
		globals__io_lookup_bool_option(smart_recompilation, Smart),
		( { Smart = yes, ModuleName \= DefaultModuleName } ->
			globals__io_lookup_bool_option(
				warn_smart_recompilation, Warn),
			globals__io_lookup_bool_option(
				halt_at_warn, Halt),
			( { Warn = yes } ->
				io__write_string(
		"Warning: module name does not match file name:\n"),
				io__write_string("  "),
				io__write_string(FileName),
				io__write_string(" contains module `"),
				prog_out__write_sym_name(ModuleName),	
				io__write_string(".\n"),
				io__write_string(
		"  Smart recompilation will not work with this module.\n"),
				( { Halt = yes } ->
					io__set_exit_status(1)
				;
					[]
				)
			;
				[]	
			),
			globals__io_set_option(smart_recompilation, bool(no))
		;
			[]
		)
	),

	globals__io_lookup_bool_option(statistics, Stats),
	maybe_report_stats(Stats),
	{ string__append(FileName, ".m", SourceFileName) }.

:- pred process_module(file_or_module, list(string), io__state, io__state).
:- mode process_module(in, out, di, uo) is det.

process_module(FileOrModule, ModulesToLink) -->
	globals__io_lookup_bool_option(halt_at_syntax_errors, HaltSyntax),
	globals__io_lookup_bool_option(make_interface, MakeInterface),
	globals__io_lookup_bool_option(make_short_interface,
						MakeShortInterface),
	globals__io_lookup_bool_option(make_private_interface,
						MakePrivateInterface),
	globals__io_lookup_bool_option(convert_to_mercury, ConvertToMercury),
	globals__io_lookup_bool_option(generate_item_version_numbers,
		GenerateVersionNumbers),
	( 
		{ MakeInterface = yes ->
			ProcessModule = make_interface,
			ReturnTimestamp = GenerateVersionNumbers
		; MakeShortInterface = yes ->
			ProcessModule = make_short_interface,
			ReturnTimestamp = no
		; MakePrivateInterface = yes ->
			ProcessModule = make_private_interface,
			ReturnTimestamp = GenerateVersionNumbers
		;
			fail
		}
	->
		read_module(FileOrModule, ReturnTimestamp, ModuleName,
			FileName, MaybeTimestamp, Items, Error, map__init, _),
		( { halt_at_module_error(HaltSyntax, Error) } ->
			[]
		;
			split_into_submodules(ModuleName,
				Items, SubModuleList),
			list__foldl(
				(pred(SubModule::in, di, uo) is det -->
					ProcessModule(FileName, MaybeTimestamp,
						SubModule)
				),
				SubModuleList)
		),
		{ ModulesToLink = [] }
	;
		{ ConvertToMercury = yes }
	->
		read_module(FileOrModule, no, ModuleName, _, _,
			Items, Error, map__init, _),
			
		( { halt_at_module_error(HaltSyntax, Error) } ->
			[]
		;
			module_name_to_file_name(ModuleName, ".ugly", yes,
					OutputFileName),
			convert_to_mercury(ModuleName, OutputFileName, Items)
		),
		{ ModulesToLink = [] }
	;
		globals__io_lookup_bool_option(smart_recompilation, Smart),
		globals__io_get_target(Target),
		( { Smart = yes } ->
			{
				FileOrModule = module(ModuleName)
			;
				FileOrModule = file(FileName),
				% XXX This won't work if the module name
				% doesn't match the file name -- such
				% modules will always be recompiled.
				%
				% This problem will be fixed when mmake
				% functionality is moved into the compiler.
				% The file_name->module_name mapping
				% will be explicitly recorded.
				file_name_to_module_name(FileName, ModuleName)
			},

			globals__io_get_globals(Globals),
			{ find_smart_recompilation_target_files(ModuleName,
				Globals, FindTargetFiles) },
			{ find_timestamp_files(ModuleName, Globals,
				FindTimestampFiles) },
			recompilation_check__should_recompile(ModuleName,
				FindTargetFiles, FindTimestampFiles,
				ModulesToRecompile0, ReadModules),
			{
				Target = asm,
				ModulesToRecompile0 = some([_ | _])
			->
				% 
				% With `--target asm', if one module
				% needs to be recompiled, all need to be
				% recompiled because they are all compiled
				% into a single object file.
				%
				ModulesToRecompile = (all)
			;
				ModulesToRecompile = ModulesToRecompile0
			}
		;
			{ map__init(ReadModules) },
			{ ModulesToRecompile = (all) }
		),
		( { ModulesToRecompile = some([]) } ->
			% XXX Currently smart recompilation is disabled
			% if mmc is linking the executable because it
			% doesn't know how to check whether all the
			% necessary intermediate files are present
			% and up-to-date.
			{ ModulesToLink = [] }
		;
			( { Target = asm, Smart = yes } ->
			    % See the comment in process_all_args.
			    compile_using_gcc_backend(FileOrModule,
				process_module_2(FileOrModule,
					ModulesToRecompile, ReadModules),
				ModulesToLink)
			;
			    process_module_2(FileOrModule, ModulesToRecompile,
			    	ReadModules, ModulesToLink)
			)
		)
	).

:- pred process_module_2(file_or_module, modules_to_recompile,
		read_modules, list(string), io__state, io__state).
:- mode process_module_2(in, in, in, out, di, uo) is det.

process_module_2(FileOrModule, MaybeModulesToRecompile, ReadModules0,
		ModulesToLink) -->
	read_module(FileOrModule, yes, ModuleName, FileName,
		MaybeTimestamp, Items, Error, ReadModules0, ReadModules),
	globals__io_lookup_bool_option(halt_at_syntax_errors, HaltSyntax),
	( { halt_at_module_error(HaltSyntax, Error) } ->
		{ ModulesToLink = [] }
	;
		split_into_submodules(ModuleName, Items, SubModuleList0),
		{ MaybeModulesToRecompile = some(ModulesToRecompile) ->
			list__filter(
				(pred((SubModule - _)::in) is semidet :-
					list__member(SubModule,
						ModulesToRecompile)
				),
				SubModuleList0, SubModuleListToCompile)
		;
				SubModuleListToCompile = SubModuleList0	
		},
		{ assoc_list__keys(SubModuleList0, NestedSubModules0) },
		{ list__delete_all(NestedSubModules0,
			ModuleName, NestedSubModules) },

		globals__io_get_globals(Globals),
		{ find_timestamp_files(ModuleName, Globals,
			FindTimestampFiles) },
		(
			{ any_mercury_builtin_module(ModuleName) }
		->
			% Some predicates in the builtin modules are missing
			% typeinfo arguments, which means that execution
			% tracing will not work on them. Predicates defined
			% there should never be part of an execution trace
			% anyway; they are effectively language primitives.
			% (They may still be parts of stack traces.)
			globals__io_lookup_bool_option(trace_stack_layout,
				TSL),
			globals__io_get_trace_level(TraceLevel),

			globals__io_set_option(trace_stack_layout, bool(no)),
			globals__io_set_trace_level_none,

			compile_all_submodules(FileName,
				ModuleName - NestedSubModules,
				MaybeTimestamp, ReadModules,
				FindTimestampFiles, SubModuleListToCompile,
				ModulesToLink),

			globals__io_set_option(trace_stack_layout, bool(TSL)),
			globals__io_set_trace_level(TraceLevel)
		;
			compile_all_submodules(FileName,
				ModuleName - NestedSubModules,
				MaybeTimestamp, ReadModules,
				FindTimestampFiles, SubModuleListToCompile,
				ModulesToLink)
		)
	).

	% For the MLDS->C and LLDS->C back-ends, we currently
	% compile each sub-module to its own C file.
	% XXX it would be better to do something like
	%
	%	list__map2_foldl(compile_to_llds, SubModuleList,
	%		LLDS_FragmentList),
	%	merge_llds_fragments(LLDS_FragmentList, LLDS),
	%	output_pass(LLDS_FragmentList)
	%
	% i.e. compile nested modules to a single C file.

:- pred compile_all_submodules(string, pair(module_name, list(module_name)),
	maybe(timestamp), read_modules, find_timestamp_file_names,
	list(pair(module_name, item_list)),
	list(string), io__state, io__state).
:- mode compile_all_submodules(in, in, in, in, in(find_timestamp_file_names),
	in, out, di, uo) is det.

compile_all_submodules(FileName, NestedSubModules, MaybeTimestamp, ReadModules,
		FindTimestampFiles, SubModuleList, ModulesToLink) -->
	list__foldl(
		compile(FileName, NestedSubModules, MaybeTimestamp,
			ReadModules, FindTimestampFiles),
		SubModuleList),
	list__map_foldl(module_to_link, SubModuleList, ModulesToLink).

:- pred make_interface(file_name, maybe(timestamp),
		pair(module_name, item_list), io__state, io__state).
:- mode make_interface(in, in, in, di, uo) is det.

make_interface(SourceFileName, MaybeTimestamp, ModuleName - Items) -->
	make_interface(SourceFileName, ModuleName, MaybeTimestamp, Items).

:- pred make_short_interface(file_name, maybe(timestamp),
		pair(module_name, item_list), io__state, io__state).
:- mode make_short_interface(in, in, in, di, uo) is det.

make_short_interface(SourceFileName, _, ModuleName - Items) -->
	make_short_interface(SourceFileName, ModuleName, Items).

:- pred make_private_interface(file_name, maybe(timestamp),
		pair(module_name, item_list), io__state, io__state).
:- mode make_private_interface(in, in, in, di, uo) is det.

make_private_interface(SourceFileName, MaybeTimestamp, ModuleName - Items) -->
	make_private_interface(SourceFileName, ModuleName,
		MaybeTimestamp, Items).

:- pred halt_at_module_error(bool, module_error).
:- mode halt_at_module_error(in, in) is semidet.

halt_at_module_error(_, fatal_module_errors).
halt_at_module_error(HaltSyntax, some_module_errors) :- HaltSyntax = yes.

:- pred module_to_link(pair(module_name, item_list), string,
			io__state, io__state).
:- mode module_to_link(in, out, di, uo) is det.

module_to_link(ModuleName - _Items, ModuleToLink) -->
	module_name_to_file_name(ModuleName, "", no, ModuleToLink).

%-----------------------------------------------------------------------------%

	% Return a closure which will work out what the target files
	% are for a module, so recompilation_check.m can check that
	% they are up-to-date which deciding whether compilation is
	% necessary.
	% Note that `--smart-recompilation' only works with
	% `--target-code-only', which is always set when the
	% compiler is invoked by mmake. Using smart recompilation
	% without using mmake is not a sensible thing to do.
	% handle_options.m will disable smart recompilation if
	% `--target-code-only' is not set.
:- pred find_smart_recompilation_target_files(module_name, globals,
		find_target_file_names).
:- mode find_smart_recompilation_target_files(in, in,
		out(find_target_file_names)) is det.

find_smart_recompilation_target_files(TopLevelModuleName,
		Globals, FindTargetFiles) :-
	globals__get_target(Globals, CompilationTarget),
	(
		CompilationTarget = c,
		globals__lookup_bool_option(Globals, split_c_files, yes)
	->
		FindTargetFiles =
		    (pred(ModuleName::in, [FileName]::out, di, uo) is det -->
			% We don't know how many chunks there should
			% be, so just check the first.
			{ Chunk = 0 },
			globals__io_lookup_string_option(object_file_extension,
				Obj),
			module_name_to_split_c_file_name(ModuleName, Chunk,
				Obj, FileName)
		)
	;
		( CompilationTarget = c, TargetSuffix = ".c"
		; CompilationTarget = il, TargetSuffix = ".il"
		; CompilationTarget = java, TargetSuffix = ".java"
		; CompilationTarget = asm, TargetSuffix = ".s"
		),
		FindTargetFiles =
		    (pred(ModuleName::in, TargetFiles::out, di, uo) is det -->
			% XXX Should we check the generated header files?
			(
				{ CompilationTarget = asm },
				{ ModuleName \= TopLevelModuleName }
			->
				% With `--target asm' all the nested
				% sub-modules are placed in the `.s' file
				% of the top-level module.
				{ TargetFiles = [] }
			;
				module_name_to_file_name(ModuleName,
					TargetSuffix, yes, FileName),
				{ TargetFiles = [FileName] }
			)
		    )
	).

:- pred find_timestamp_files(module_name, globals, find_timestamp_file_names).
:- mode find_timestamp_files(in, in, out(find_timestamp_file_names)) is det.

find_timestamp_files(TopLevelModuleName, Globals, FindTimestampFiles) :-
	globals__lookup_bool_option(Globals, pic, Pic),
	globals__get_target(Globals, CompilationTarget),
	( CompilationTarget = c, TimestampSuffix = ".c_date"
	; CompilationTarget = il, TimestampSuffix = ".il_date"
	; CompilationTarget = java, TimestampSuffix = ".java_date"
	; CompilationTarget = asm,
		TimestampSuffix = (Pic = yes -> ".pic_s_date" ; ".s_date")
	),
	FindTimestampFiles =
	    (pred(ModuleName::in, TimestampFiles::out, di, uo) is det -->
		(
			{ CompilationTarget = asm },
			{ ModuleName \= TopLevelModuleName }
		->
			% With `--target asm' all the nested
			% sub-modules are placed in the `.s' file
			% of the top-level module.
			{ TimestampFiles = [] }
		;
			module_name_to_file_name(ModuleName,
				TimestampSuffix, yes, FileName),
			{ TimestampFiles = [FileName] }
		)
	    ).

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

:- pred compile(file_name, pair(module_name, list(module_name)),
	maybe(timestamp), read_modules, find_timestamp_file_names,
	pair(module_name, item_list), io__state, io__state).
:- mode compile(in, in, in, in, in(find_timestamp_file_names),
	in, di, uo) is det.

compile(SourceFileName, RootModuleName - NestedSubModules0,
		MaybeTimestamp, ReadModules, FindTimestampFiles,
		ModuleName - Items) -->
	check_for_no_exports(Items, ModuleName),
	grab_imported_modules(SourceFileName, ModuleName, ReadModules,
		MaybeTimestamp, Items, Module, Error2),
	( { Error2 \= fatal_module_errors } ->
		{ ModuleName = RootModuleName ->
			NestedSubModules = NestedSubModules0
		;
			NestedSubModules = []
		},
		mercury_compile(Module, NestedSubModules, FindTimestampFiles)
	;
		[]
	).

:- pred mercury_compile(module_imports, list(module_name),
		find_timestamp_file_names, io__state, io__state).
:- mode mercury_compile(in, in, in(find_timestamp_file_names), di, uo) is det.

mercury_compile(Module, NestedSubModules, FindTimestampFiles) -->
	{ module_imports_get_module_name(Module, ModuleName) },
	% If we are only typechecking or error checking, then we should not
	% modify any files, this includes writing to .d files.
	globals__io_lookup_bool_option(typecheck_only, TypeCheckOnly),
	globals__io_lookup_bool_option(errorcheck_only, ErrorCheckOnly),
	{ bool__or(TypeCheckOnly, ErrorCheckOnly, DontWriteDFile) },
	mercury_compile__pre_hlds_pass(Module, DontWriteDFile,
		HLDS1, QualInfo, MaybeTimestamps, UndefTypes, UndefModes,
		Errors1),
	mercury_compile__frontend_pass(HLDS1, QualInfo, UndefTypes,
		UndefModes, HLDS20, Errors2),
	( { Errors1 = no }, { Errors2 = no } ->
	    globals__io_lookup_bool_option(verbose, Verbose),
	    globals__io_lookup_bool_option(statistics, Stats),
	    mercury_compile__maybe_write_dependency_graph(HLDS20,
		Verbose, Stats, HLDS21),
	    mercury_compile__maybe_generate_schemas(HLDS21, Verbose),
	    globals__io_lookup_bool_option(make_optimization_interface,
		MakeOptInt),
	    globals__io_lookup_bool_option(make_transitive_opt_interface,
		MakeTransOptInt),
	    ( { TypeCheckOnly = yes } ->
		[]
	    ; { ErrorCheckOnly = yes } ->
		% we may still want to run `unused_args' so that we get
		% the appropriate warnings
		globals__io_lookup_bool_option(warn_unused_args, UnusedArgs),
		( { UnusedArgs = yes } ->
			globals__io_set_option(optimize_unused_args, bool(no)),
			mercury_compile__maybe_unused_args(HLDS21,
				Verbose, Stats, HLDS22)
	    	;
			{ HLDS22 = HLDS21 }
	    	),
		% magic sets can report errors.
		mercury_compile__maybe_transform_dnf(HLDS22,
			Verbose, Stats, HLDS23),
		mercury_compile__maybe_magic(HLDS23, Verbose, Stats, _)
	    ; { MakeOptInt = yes } ->
		% only run up to typechecking when making the .opt file
		[]
	    ; { MakeTransOptInt = yes } ->
	    	mercury_compile__output_trans_opt_file(HLDS21)
	    ;
		mercury_compile__maybe_output_prof_call_graph(HLDS21,
			Verbose, Stats, HLDS25),
		mercury_compile__middle_pass(ModuleName, HLDS25, HLDS50,
			DeepProfilingStructures),
		globals__io_lookup_bool_option(highlevel_code, HighLevelCode),
		globals__io_lookup_bool_option(aditi_only, AditiOnly),
		globals__io_get_target(Target),
		globals__io_lookup_bool_option(target_code_only, 
				TargetCodeOnly),

		%
		% Remove any existing `.used' file before writing the
		% output file file. This avoids leaving the old `used'
		% file lying around if compilation is interrupted after
		% the new output file is written but before the new
		% `.used' file is written.
		%
		module_name_to_file_name(ModuleName, ".used", no,
			UsageFileName),
		io__remove_file(UsageFileName, _),

		% magic sets can report errors.
		{ module_info_num_errors(HLDS50, NumErrors) },
		( { NumErrors = 0 } ->
		    mercury_compile__maybe_generate_rl_bytecode(HLDS50,
				Verbose, MaybeRLFile),
		    ( { AditiOnly = yes } ->
		    	{ HLDS = HLDS50 }
		    ; { Target = il } ->
			{ HLDS = HLDS50 },
			mercury_compile__mlds_backend(HLDS, MLDS),
			( { TargetCodeOnly = yes } ->
				mercury_compile__mlds_to_il_assembler(MLDS)
			;
				{ HasMain = mercury_compile__mlds_has_main(
					MLDS) },
				mercury_compile__mlds_to_il_assembler(MLDS),
				mercury_compile__il_assemble(ModuleName,
					HasMain)
			)
		    ; { Target = java } ->
			{ HLDS = HLDS50 },
			mercury_compile__mlds_backend(HLDS, MLDS),
			mercury_compile__mlds_to_java(MLDS),
			( { TargetCodeOnly = yes } ->
				[]
			;
				mercury_compile__compile_java_file(ModuleName)
			)
		    ; { Target = asm } ->
		    	% compile directly to assembler using the gcc back-end
			{ HLDS = HLDS50 },
			mercury_compile__mlds_backend(HLDS, MLDS),
			mercury_compile__maybe_mlds_to_gcc(MLDS,
				ContainsCCode),
			( { TargetCodeOnly = yes } ->
				[]
			;
				% We don't invoke the assembler to produce an
				% object file yet -- that is done at
				% the top level.
				%
				% But if the module contained `pragma c_code',
				% then we will have compiled that to a
				% separate C file.  We need to invoke the
				% C compiler on that.
				%
				( { ContainsCCode = yes } ->
					module_name_to_file_name(ModuleName,
						".c", no, CCode_C_File),
					globals__io_lookup_string_option(
						object_file_extension, Obj),
					module_name_to_file_name(ModuleName,
						"__c_code" ++ Obj,
						yes, CCode_O_File),
					mercury_compile__single_c_to_obj(
						CCode_C_File, CCode_O_File,
						_CompileOK),
					% add this object file to the list
					% of extra object files to link in
					globals__io_lookup_accumulating_option(
						link_objects, LinkObjects),
					globals__io_set_option(link_objects,
						accumulating([CCode_O_File |
						LinkObjects]))
				;
					[]
				)
			)
		    ; { HighLevelCode = yes } ->
			{ HLDS = HLDS50 },
			mercury_compile__mlds_backend(HLDS, MLDS),
			mercury_compile__mlds_to_high_level_c(MLDS),
			( { TargetCodeOnly = yes } ->
				[]
			;
				module_name_to_file_name(ModuleName, ".c", no,
					C_File),
				globals__io_lookup_string_option(
					object_file_extension, Obj),
				module_name_to_file_name(ModuleName, Obj, yes,
					O_File),
				mercury_compile__single_c_to_obj(
					C_File, O_File, _CompileOK)
			)
		    ;
			mercury_compile__backend_pass(HLDS50, HLDS,
				DeepProfilingStructures, GlobalData, LLDS),
			mercury_compile__output_pass(HLDS, GlobalData, LLDS,
				MaybeRLFile, ModuleName, _CompileErrors)
		    ),
		    recompilation_usage__write_usage_file(HLDS,
		    	NestedSubModules, MaybeTimestamps),
		    FindTimestampFiles(ModuleName, TimestampFiles),
		    list__foldl(touch_datestamp, TimestampFiles)
		;
		    	% If the number of errors is > 0, make sure that
			% the compiler exits with a non-zero exit
			% status.
		    io__get_exit_status(ExitStatus),
		    ( { ExitStatus = 0 } ->
		    	io__set_exit_status(1)
		    ;
		    	[]
		    )
		)
	    )
	;
	    []
	).

	% return `yes' iff this module defines the main/2 entry point.
:- func mercury_compile__mlds_has_main(mlds) = bool.
mercury_compile__mlds_has_main(MLDS) =
	(
		MLDS = mlds(_, _, _, Defns),
		defns_contain_main(Defns)
	->
		yes
	;
		no
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__pre_hlds_pass(module_imports, bool,
		module_info, qual_info, maybe(module_timestamps),
		bool, bool, bool, io__state, io__state).
:- mode mercury_compile__pre_hlds_pass(in, in, out, out, out, out, out, out,
		di, uo) is det.

mercury_compile__pre_hlds_pass(ModuleImports0, DontWriteDFile,
		HLDS1, QualInfo, MaybeTimestamps,
		UndefTypes, UndefModes, FoundError) -->
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
		maybe_read_dependency_file(Module, MaybeTransOptDeps)
	),

	% Errors in .opt and .trans_opt files result in software errors.
	mercury_compile__maybe_grab_optfiles(ModuleImports0, Verbose,
		MaybeTransOptDeps, ModuleImports1, IntermodError),

	{ module_imports_get_items(ModuleImports1, Items1) },
	{ MaybeTimestamps = ModuleImports1 ^ maybe_timestamps },

	mercury_compile__module_qualify_items(Items1, Items2, Module, Verbose,
				Stats, MQInfo0, _, UndefTypes0, UndefModes0),

	{ mq_info_get_recompilation_info(MQInfo0, RecompInfo0) },
	mercury_compile__expand_equiv_types(Module, Items2, Verbose, Stats,
				Items, CircularTypes, EqvMap,
				RecompInfo0, RecompInfo),
	{ mq_info_set_recompilation_info(MQInfo0, RecompInfo, MQInfo) }, 
	{ bool__or(UndefTypes0, CircularTypes, UndefTypes1) },

	mercury_compile__make_hlds(Module, Items, MQInfo, EqvMap, Verbose, 
			Stats, HLDS0, QualInfo,
			UndefTypes2, UndefModes2, FoundError),

	{ bool__or(UndefTypes1, UndefTypes2, UndefTypes) },
	{ bool__or(UndefModes0, UndefModes2, UndefModes) },

	mercury_compile__maybe_dump_hlds(HLDS0, "01", "initial"),

	( { DontWriteDFile = yes } ->
		[]
	;
		{ module_info_get_all_deps(HLDS0, AllDeps) },
		write_dependency_file(ModuleImports0, AllDeps,
			MaybeTransOptDeps)
	),

	% Only stop on syntax errors in .opt files.
	( { FoundError = yes ; IntermodError = yes } ->
		{ module_info_incr_errors(HLDS0, HLDS1) }
	;	
		{ HLDS1 = HLDS0 }
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
	globals__io_lookup_bool_option(intermodule_optimization, IntermodOpt),
	globals__io_lookup_bool_option(use_opt_files, UseOptInt),
	globals__io_lookup_bool_option(make_optimization_interface,
						MakeOptInt),
	globals__io_lookup_bool_option(transitive_optimization, TransOpt),
	globals__io_lookup_bool_option(make_transitive_opt_interface,
		MakeTransOptInt),
	( { (UseOptInt = yes ; IntermodOpt = yes), MakeOptInt = no } ->
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
	; { MakeOptInt = yes } ->
		% If we're making the `.opt' file, then we can't
		% read any `.trans_opt' files, since `.opt' files 
		% aren't allowed to depend on `.trans_opt' files.
		{ Imports = Imports1 },
		{ Error2 = no }
	;
		( { TransOpt = yes } ->
			% If transitive optimization is enabled, but we are
			% not creating the .opt or .trans opt file, then import
			% the trans_opt files for all the modules that are
			% imported (or used), and for all ancestor modules.
			{ Imports0 = module_imports(_File, _Module, Ancestors,
				InterfaceImports, ImplementationImports,
				_IndirectImports, _PublicChildren, _FactDeps,
				_ForeignCode, _Items, _Error, _Timestamps) },
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

:- pred mercury_compile__expand_equiv_types(module_name, item_list,
	bool, bool, item_list, bool, eqv_map, maybe(recompilation_info),
	maybe(recompilation_info), io__state, io__state).
:- mode mercury_compile__expand_equiv_types(in, in, in, in, out,
	out, out, in, out, di, uo) is det.

mercury_compile__expand_equiv_types(ModuleName, Items0, Verbose, Stats,
		Items, CircularTypes, EqvMap, RecompInfo0, RecompInfo) -->
	maybe_write_string(Verbose, "% Expanding equivalence types..."),
	maybe_flush_output(Verbose),
	equiv_type__expand_eqv_types(ModuleName, Items0, Items, CircularTypes,
		EqvMap, RecompInfo0, RecompInfo),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__make_hlds(module_name, item_list, mq_info, eqv_map, 
	bool, bool, module_info, qual_info, bool, bool, bool,
	io__state, io__state).
:- mode mercury_compile__make_hlds(in, in, in, in, in, in,
	out, out, out, out, out, di, uo) is det.

mercury_compile__make_hlds(Module, Items, MQInfo, EqvMap, Verbose, Stats,
		HLDS, QualInfo, UndefTypes, UndefModes, FoundSemanticError) -->
	maybe_write_string(Verbose, "% Converting parse tree to hlds...\n"),
	{ Prog = module(Module, Items) },
	parse_tree_to_hlds(Prog, MQInfo, EqvMap, HLDS, QualInfo,
		UndefTypes, UndefModes),
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

:- pred mercury_compile__frontend_pass(module_info, qual_info, bool, bool,
		module_info, bool, io__state, io__state).
% :- mode mercury_compile__frontend_pass(di, in, in, in, uo, out, di, uo)
%	is det.
:- mode mercury_compile__frontend_pass(in, in, in, in, out, out, di, uo)
	is det.

mercury_compile__frontend_pass(HLDS1, QualInfo0, FoundUndefTypeError,
		FoundUndefModeError, HLDS, FoundError) -->
	%
	% We can't continue after an undefined type error, since
	% typecheck would get internal errors
	%
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
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
	    check_typeclass__check_instance_decls(HLDS1, QualInfo0, HLDS2,
		QualInfo, FoundTypeclassError),
	    mercury_compile__maybe_dump_hlds(HLDS2, "02", "typeclass"),
	    { make_hlds__set_module_recompilation_info(QualInfo,
	    	HLDS2, HLDS2a) },

	    globals__io_lookup_bool_option(intermodule_optimization, Intermod),
	    globals__io_lookup_bool_option(use_opt_files, UseOptFiles),
	    globals__io_lookup_bool_option(make_optimization_interface,
		MakeOptInt),
	    ( { (Intermod = yes; UseOptFiles = yes), MakeOptInt = no } ->
		% Eliminate unnecessary clauses from `.opt' files,
		% to speed up compilation. This must be done after
		% typeclass instances have been checked, since that
		% fills in which pred_ids are needed by instance decls.
		{ dead_pred_elim(HLDS2a, HLDS2b) },
		mercury_compile__maybe_dump_hlds(HLDS2b, "02b",
				"dead_pred_elim")
	    ;
		{ HLDS2b = HLDS2a }
	    ),

	    %
	    % Next typecheck the clauses.
	    %
	    typecheck(HLDS2b, HLDS3, FoundTypeError,
	    		ExceededTypeCheckIterationLimit),
	    ( { FoundTypeError = yes } ->
		maybe_write_string(Verbose,
			"% Program contains type error(s).\n"),
		io__set_exit_status(1)
	    ;
		maybe_write_string(Verbose, "% Program is type-correct.\n")
	    ),
	    mercury_compile__maybe_dump_hlds(HLDS3, "03", "typecheck"),

	    %
	    % We can't continue after an undefined inst/mode
	    % error, since propagate_types_into_proc_modes
	    % (in post_typecheck.m -- called by purity.m)
	    % and mode analysis would get internal errors.
	    %
	    % We can't continue if the type inference iteration
	    % limit was exceeeded because the code to resolve
	    % overloading in post_typecheck.m (called by purity.m)
	    % could abort.
	    ( { FoundUndefModeError = yes } ->
		{ FoundError = yes },
		{ HLDS = HLDS3 },
		maybe_write_string(Verbose,
	"% Program contains undefined inst or undefined mode error(s).\n"),
		io__set_exit_status(1)
	    ; { ExceededTypeCheckIterationLimit = yes } ->
		% FoundTypeError will always be true here, so we've already
		% printed a message about the program containing type errors.
		{ FoundError = yes },
		{ HLDS = HLDS3 },
		io__set_exit_status(1)
	    ;
	        %
	        % Run purity checking
	        %
		mercury_compile__puritycheck(FoundTypeError, HLDS3,
			Verbose, Stats, HLDS4, FoundPostTypecheckError),
		mercury_compile__maybe_dump_hlds(HLDS4, "04", "puritycheck"),

	        %
	        % Stop here if `--typecheck-only' was specified.
	        %
	        globals__io_lookup_bool_option(typecheck_only, TypecheckOnly),
	        ( { TypecheckOnly = yes } ->
		    { HLDS = HLDS4 },
		    { bool__or(FoundTypeError, FoundTypeclassError,
		    	FoundError) }
	        ; 
		    { FoundTypeError = yes ; FoundPostTypecheckError = yes 
		    	; FoundTypeclassError = yes } 
		->
		    %
		    % XXX it would be nice if we could go on and mode-check
		    % the predicates which didn't have type errors, but
		    % we need to run polymorphism before running mode
		    % analysis, and currently polymorphism may get internal
		    % errors if any of the predicates are not type-correct.
		    %
		    { HLDS = HLDS4 },
		    { FoundError = yes }
		;
		    % only write out the `.opt' file if there are no type errors
		    % or undefined modes
		    ( { FoundTypeError = no, FoundUndefModeError = no } ->
			    mercury_compile__maybe_write_optfile(MakeOptInt,
		    		    HLDS4, HLDS5)
		    ;
			    { HLDS5 = HLDS4 }
		    ),
		    % if our job was to write out the `.opt' file,
		    % then we're done
		    ( { MakeOptInt = yes } ->
		    	    { HLDS = HLDS5 },
			    { bool__or(FoundTypeError, FoundTypeclassError,
				    FoundError) }
		    ;
			    %
			    % Now go ahead and do the rest of mode checking and
			    % determinism analysis
			    %
			    mercury_compile__frontend_pass_2_by_phases(HLDS5,
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
	globals__io_lookup_bool_option(intermodule_optimization, Intermod),
	globals__io_lookup_bool_option(intermod_unused_args, IntermodArgs),
	globals__io_lookup_accumulating_option(intermod_directories,
		IntermodDirs),
	globals__io_lookup_bool_option(use_opt_files, UseOptFiles),
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	globals__io_lookup_bool_option(termination, Termination),

	( { MakeOptInt = yes } ->
		intermod__write_optfile(HLDS0, HLDS1),

		% If intermod_unused_args is being performed, run polymorphism,
		% mode analysis and determinism analysis, then run unused_args
		% to append the unused argument information to the `.opt.tmp' 
		% file written above.
		( { IntermodArgs = yes ; Termination = yes } ->
			mercury_compile__frontend_pass_2_by_phases(
				HLDS1, HLDS2, FoundModeError),
			( { FoundModeError = no } ->
				( { IntermodArgs = yes } ->
					mercury_compile__maybe_unused_args(
						HLDS2, Verbose, Stats, HLDS3)
				;
					{ HLDS3 = HLDS2 }
				),
				( { Termination = yes } ->
					mercury_compile__maybe_termination(
						HLDS3, Verbose, Stats, HLDS)
				;
					{ HLDS = HLDS3 }
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
		% If there is a `.opt' file for this module the import
		% status of items in the `.opt' file needs to be updated.
		( { Intermod = yes } ->
			{ UpdateStatus = yes }
		; { UseOptFiles = yes } ->
			{ module_info_name(HLDS0, ModuleName) },
			module_name_to_file_name(ModuleName,
				".opt", no, OptName),
			search_for_file(IntermodDirs, OptName, Found),
			( { Found = yes } ->
				{ UpdateStatus = yes },
				io__seen
			;
				{ UpdateStatus = no }
			)
		;
			{ UpdateStatus = no }
		),	
		{ is_bool(UpdateStatus) },
		( { UpdateStatus = yes } ->
			intermod__adjust_pred_import_status(HLDS0, HLDS)
		;
			{ HLDS = HLDS0 }
		)
	).

:- pred is_bool(bool::in) is det.
is_bool(_).

:- pred mercury_compile__output_trans_opt_file(module_info, 
	io__state, io__state).
:- mode mercury_compile__output_trans_opt_file(in, di, uo) is det.
mercury_compile__output_trans_opt_file(HLDS25) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__maybe_termination(HLDS25, Verbose, Stats, HLDS28),
	mercury_compile__maybe_dump_hlds(HLDS28, "28", "termination"),

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

mercury_compile__frontend_pass_2_by_phases(HLDS4, HLDS20, FoundError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__maybe_polymorphism(HLDS4, Verbose, Stats, HLDS5),
	mercury_compile__maybe_dump_hlds(HLDS5, "05", "polymorphism"),

	mercury_compile__modecheck(HLDS5, Verbose, Stats, HLDS6,
		FoundModeError, UnsafeToContinue),
	mercury_compile__maybe_dump_hlds(HLDS6, "06", "modecheck"),

	( { UnsafeToContinue = yes } ->
		{ FoundError = yes },
		{ HLDS12 = HLDS6 }
	;
		mercury_compile__detect_switches(HLDS6, Verbose, Stats, HLDS7),
		mercury_compile__maybe_dump_hlds(HLDS7, "07", "switch_detect"),

		mercury_compile__detect_cse(HLDS7, Verbose, Stats, HLDS8),
		mercury_compile__maybe_dump_hlds(HLDS8, "08", "cse"),

		mercury_compile__check_determinism(HLDS8, Verbose, Stats, HLDS9,
			FoundDetError),
		mercury_compile__maybe_dump_hlds(HLDS9, "09", "determinism"),

		mercury_compile__check_unique_modes(HLDS9, Verbose, Stats,
			HLDS10, FoundUniqError),
		mercury_compile__maybe_dump_hlds(HLDS10, "10", "unique_modes"),

		mercury_compile__check_stratification(HLDS10, Verbose, Stats, 
			HLDS11, FoundStratError),
		mercury_compile__maybe_dump_hlds(HLDS11, "11",
			"stratification"),

		mercury_compile__simplify(HLDS11, yes, no, Verbose, Stats,
			process_all_nonimported_procs, HLDS12),
		mercury_compile__maybe_dump_hlds(HLDS12, "12", "simplify"),

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
			{ FoundError = no }
		;
			{ FoundError = yes }
		)
	),

	{ HLDS20 = HLDS12 },
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
	list(layout_data), io__state, io__state).
% :- mode mercury_compile__middle_pass(in, di, uo, out, di, uo) is det.
:- mode mercury_compile__middle_pass(in, in, out, out, di, uo) is det.

mercury_compile__middle_pass(ModuleName, HLDS24, HLDS50,
		DeepProfilingStructures) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__tabling(HLDS24, Verbose, HLDS25),
	mercury_compile__maybe_dump_hlds(HLDS25, "25", "tabling"),

	mercury_compile__process_lambdas(HLDS25, Verbose, HLDS26),
	mercury_compile__maybe_dump_hlds(HLDS26, "26", "lambda"),

	%
	% Uncomment the following code to check that unique mode analysis
	% works after simplification has been run. Currently it does not
	% because common.m does not preserve unique mode correctness
	% (this test fails on about five modules in the compiler and library).
	% It is important that unique mode analysis work most of the time
	% after optimizations because deforestation reruns it.
	%

	{ HLDS27 = HLDS26 },
	%mercury_compile__check_unique_modes(HLDS26, Verbose, Stats,
	%		HLDS27, FoundUniqError),
	%
	%{ FoundUniqError = yes ->
	%	error("unique modes failed")
	%;
	%	true
	%},

	mercury_compile__maybe_termination(HLDS27, Verbose, Stats, HLDS28),
	mercury_compile__maybe_dump_hlds(HLDS28, "28", "termination"),

	mercury_compile__maybe_type_ctor_infos(HLDS28, Verbose, Stats, HLDS30),
	mercury_compile__maybe_dump_hlds(HLDS30, "30", "type_ctor_infos"),

	mercury_compile__maybe_bytecodes(HLDS30, ModuleName, Verbose, Stats),

	% stage number 31 is used by mercury_compile__maybe_bytecodes

	mercury_compile__maybe_higher_order(HLDS30, Verbose, Stats, HLDS32),
	mercury_compile__maybe_dump_hlds(HLDS32, "32", "higher_order"),

	mercury_compile__maybe_introduce_accumulators(HLDS32,
			Verbose, Stats, HLDS33),
	mercury_compile__maybe_dump_hlds(HLDS33, "33", "accum"),

	mercury_compile__maybe_do_inlining(HLDS33, Verbose, Stats, HLDS34),
	mercury_compile__maybe_dump_hlds(HLDS34, "34", "inlining"),

	mercury_compile__maybe_deforestation(HLDS34, Verbose, Stats, HLDS36),
	mercury_compile__maybe_dump_hlds(HLDS36, "36", "deforestation"),

	mercury_compile__maybe_delay_construct(HLDS36, Verbose, Stats, HLDS37),
	mercury_compile__maybe_dump_hlds(HLDS37, "37", "delay_construct"),

	mercury_compile__maybe_unused_args(HLDS37, Verbose, Stats, HLDS39),
	mercury_compile__maybe_dump_hlds(HLDS39, "39", "unused_args"),

	mercury_compile__maybe_unneeded_code(HLDS39, Verbose, Stats, HLDS40),
	mercury_compile__maybe_dump_hlds(HLDS40, "40", "unneeded_code"),

	mercury_compile__maybe_lco(HLDS40, Verbose, Stats, HLDS42), !,
	mercury_compile__maybe_dump_hlds(HLDS42, "42", "lco"), !,

	% DNF transformations should be after inlining.
	mercury_compile__maybe_transform_dnf(HLDS40, Verbose, Stats, HLDS44),
	mercury_compile__maybe_dump_hlds(HLDS44, "44", "dnf"),

	% Magic sets should be the last thing done to Aditi procedures
	% before RL code generation, and must come immediately after DNF.
	mercury_compile__maybe_magic(HLDS44, Verbose, Stats, HLDS46),
	mercury_compile__maybe_dump_hlds(HLDS46, "46", "magic"),

	mercury_compile__maybe_dead_procs(HLDS46, Verbose, Stats, HLDS48),
	mercury_compile__maybe_dump_hlds(HLDS48, "48", "dead_procs"),

	% Deep profiling transformation should be done late in the piece
	% since it munges the code a fair amount and introduces strange
	% disjunctions that might confuse other hlds->hlds transformations.
	mercury_compile__maybe_deep_profiling(HLDS48, Verbose, Stats, HLDS49,
		DeepProfilingStructures),
	mercury_compile__maybe_dump_hlds(HLDS49, "49", "deep_profiling"),

	{ HLDS49 = HLDS50 },
	mercury_compile__maybe_dump_hlds(HLDS50, "50", "middle_pass").

%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_generate_rl_bytecode(module_info, bool,
		maybe(rl_file), io__state, io__state).
:- mode mercury_compile__maybe_generate_rl_bytecode(in, in,
		out, di, uo) is det.

mercury_compile__maybe_generate_rl_bytecode(ModuleInfo,
		Verbose, MaybeRLFile) -->
	globals__io_lookup_bool_option(aditi, Aditi),
	(
		{ Aditi = yes },
		{ module_info_get_do_aditi_compilation(ModuleInfo,
			AditiCompile) },
		(
			{ AditiCompile = do_aditi_compilation },

			%
			% Generate the RL procedures.
			%
			maybe_write_string(Verbose, "% Generating RL...\n"),
			maybe_flush_output(Verbose),
			rl_gen__module(ModuleInfo, RLProcs0),
			mercury_compile__maybe_dump_rl(RLProcs0,
				ModuleInfo, "", ""),

			%
			% Optimize the RL procedures.
			%
			rl_opt__procs(ModuleInfo, RLProcs0, RLProcs),
			mercury_compile__maybe_dump_rl(RLProcs,
				ModuleInfo, "", ".opt"),

			%
			% Convert the RL procedures to bytecode.
			%
			rl_out__generate_rl_bytecode(ModuleInfo,
				RLProcs, MaybeRLFile)
		;
			{ AditiCompile = no_aditi_compilation },
			{ MaybeRLFile = no },

			globals__io_lookup_bool_option(aditi_only, AditiOnly),
			(
				{ AditiOnly = yes },

				% Always generate a `.rlo' file if compiling
				% with `--aditi-only'.
				{ RLProcs = [] },
				rl_out__generate_rl_bytecode(ModuleInfo,
					RLProcs, _)
			;
				{ AditiOnly = no }
			)
		)
	;
		{ Aditi = no },
		{ MaybeRLFile = no }
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass(module_info, module_info,
	list(layout_data), global_data, list(c_procedure),
	io__state, io__state).
% :- mode mercury_compile__backend_pass(di, uo, out, out, di, uo) is det.
:- mode mercury_compile__backend_pass(in, out, in, out, out, di, uo) is det.

mercury_compile__backend_pass(HLDS50, HLDS, DeepProfilingStructures,
		GlobalData, LLDS) -->
	{ global_data_init(DeepProfilingStructures, GlobalData0) },

	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	% map_args_to_regs affects the interface to a predicate,
	% so it must be done in one phase immediately before code generation

	mercury_compile__map_args_to_regs(HLDS50, Verbose, Stats, HLDS51),
	mercury_compile__maybe_dump_hlds(HLDS51, "51", "args_to_regs"),

	globals__io_lookup_bool_option(trad_passes, TradPasses),
	(
		{ TradPasses = no },
		mercury_compile__backend_pass_by_phases(HLDS51, HLDS,
			GlobalData0, GlobalData, LLDS)
	;
		{ TradPasses = yes },
		mercury_compile__backend_pass_by_preds(HLDS51, HLDS,
			GlobalData0, GlobalData, LLDS)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass_by_phases(module_info, module_info,
	global_data, global_data, list(c_procedure),
	io__state, io__state).
:- mode mercury_compile__backend_pass_by_phases(in, out, in, out, out, di, uo)
	is det.

mercury_compile__backend_pass_by_phases(HLDS51, HLDS99,
		GlobalData0, GlobalData, LLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__maybe_followcode(HLDS51, Verbose, Stats, HLDS52),
	mercury_compile__maybe_dump_hlds(HLDS52, "52", "followcode"),

	mercury_compile__simplify(HLDS52, no, yes, Verbose, Stats, 
		process_all_nonimported_nonaditi_procs, HLDS53),
	mercury_compile__maybe_dump_hlds(HLDS53, "53", "simplify2"),

	mercury_compile__maybe_saved_vars(HLDS53, Verbose, Stats, HLDS56),
	mercury_compile__maybe_dump_hlds(HLDS56, "56", "savedvars"),

	mercury_compile__compute_liveness(HLDS56, Verbose, Stats, HLDS59),
	mercury_compile__maybe_dump_hlds(HLDS59, "59", "liveness"),

	mercury_compile__compute_stack_vars(HLDS59, Verbose, Stats, HLDS65),
	mercury_compile__maybe_dump_hlds(HLDS65, "65", "stackvars"),

	mercury_compile__allocate_store_map(HLDS65, Verbose, Stats, HLDS68),
	mercury_compile__maybe_dump_hlds(HLDS68, "68", "store_map"),

	mercury_compile__maybe_goal_paths(HLDS68, Verbose, Stats, HLDS72),
	mercury_compile__maybe_dump_hlds(HLDS72, "72", "goal_path"),

	maybe_report_sizes(HLDS72),

	{ HLDS90 = HLDS72 },
	mercury_compile__maybe_dump_hlds(HLDS90, "90", "precodegen"),

	mercury_compile__generate_code(HLDS90, GlobalData0, Verbose, Stats,
		HLDS99, GlobalData1, LLDS1),
	mercury_compile__maybe_dump_hlds(HLDS99, "99", "codegen"),

	mercury_compile__maybe_generate_stack_layouts(HLDS99, GlobalData1,
		LLDS1, Verbose, Stats, GlobalData),
	% mercury_compile__maybe_dump_global_data(GlobalData),

	mercury_compile__maybe_do_optimize(LLDS1, GlobalData, Verbose, Stats,
		LLDS).

:- pred mercury_compile__backend_pass_by_preds(module_info, module_info,
	global_data, global_data, list(c_procedure),
	io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds(di, uo, in, out, out, di, uo)
%	is det.
:- mode mercury_compile__backend_pass_by_preds(in, out, in, out, out, di, uo)
	is det.

mercury_compile__backend_pass_by_preds(HLDS0, HLDS, GlobalData0, GlobalData,
		LLDS) -->
	{ module_info_predids(HLDS0, PredIds) },
	mercury_compile__backend_pass_by_preds_2(PredIds, HLDS0, HLDS,
		GlobalData0, GlobalData, LLDS).

:- pred mercury_compile__backend_pass_by_preds_2(list(pred_id),
	module_info, module_info, global_data, global_data, list(c_procedure),
	io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds_2(in, di, uo, in, out, out,
%	di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds_2(in, in, out, in, out, out,
	di, uo) is det.

mercury_compile__backend_pass_by_preds_2([], ModuleInfo, ModuleInfo,
		GlobalData, GlobalData, []) --> [].
mercury_compile__backend_pass_by_preds_2([PredId | PredIds], ModuleInfo0,
		ModuleInfo, GlobalData0, GlobalData, Code) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	{ pred_info_non_imported_procids(PredInfo, ProcIds) },
	( 
		{ ProcIds = []
		; hlds_pred__pred_info_is_aditi_relation(PredInfo)
		}
	->
		{ ModuleInfo3 = ModuleInfo0 },
		{ GlobalData1 = GlobalData0 },
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
		(
			{ pred_info_module(PredInfo, PredModule) },
			{ pred_info_name(PredInfo, PredName) },
                        { pred_info_arity(PredInfo, PredArity) },
                        { no_type_info_builtin(PredModule, PredName,
				PredArity) }
		->
				% These predicates should never be traced,
				% since they do not obey typeinfo_liveness.
				% Since they may be opt_imported into other
				% modules, we must switch off the tracing
				% of such preds on a pred-by-pred basis.
			{ module_info_globals(ModuleInfo0, Globals0) },
			{ globals__get_trace_level(Globals0, TraceLevel) },
			{ globals__set_trace_level_none(Globals0, Globals1) },
			{ module_info_set_globals(ModuleInfo0, Globals1,
				ModuleInfo1) },
			{ copy(Globals1, Globals1Unique) },
			globals__io_set_globals(Globals1Unique),
			mercury_compile__backend_pass_by_preds_3(ProcIds,
				PredId, PredInfo, ModuleInfo1, ModuleInfo2,
				GlobalData0, GlobalData1, Code1),
			{ module_info_globals(ModuleInfo2, Globals2) },
			{ globals__set_trace_level(Globals2, TraceLevel,
				Globals) },
			{ module_info_set_globals(ModuleInfo2, Globals,
				ModuleInfo3) },
			{ copy(Globals, GlobalsUnique) },
			globals__io_set_globals(GlobalsUnique)
		;
			mercury_compile__backend_pass_by_preds_3(ProcIds,
				PredId, PredInfo, ModuleInfo0, ModuleInfo3,
				GlobalData0, GlobalData1, Code1)
		)
	),
	mercury_compile__backend_pass_by_preds_2(PredIds,
		ModuleInfo3, ModuleInfo, GlobalData1, GlobalData, Code2),
	{ list__append(Code1, Code2, Code) }.

:- pred mercury_compile__backend_pass_by_preds_3(list(proc_id), pred_id,
	pred_info, module_info, module_info, global_data, global_data,
	list(c_procedure), io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds_3(in, in, in, di, uo, in, out,
%	out, di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds_3(in, in, in, in, out, in, out,
	out, di, uo) is det.

mercury_compile__backend_pass_by_preds_3([], _, _, ModuleInfo, ModuleInfo,
		GlobalData, GlobalData, []) --> [].
mercury_compile__backend_pass_by_preds_3([ProcId | ProcIds], PredId, PredInfo,
		ModuleInfo0, ModuleInfo, GlobalData0, GlobalData,
		[Proc | Procs]) -->
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	mercury_compile__backend_pass_by_preds_4(PredInfo, ProcInfo,
		ProcId, PredId, ModuleInfo0, ModuleInfo1,
		GlobalData0, GlobalData1, Proc),
	mercury_compile__backend_pass_by_preds_3(ProcIds, PredId, PredInfo,
		ModuleInfo1, ModuleInfo, GlobalData1, GlobalData, Procs).

:- pred mercury_compile__backend_pass_by_preds_4(pred_info, proc_info,
	proc_id, pred_id, module_info, module_info, global_data, global_data,
	c_procedure, io__state, io__state).
% :- mode mercury_compile__backend_pass_by_preds_4(in, in, in, in, di, uo,
% 	in, out, out, di, uo) is det.
:- mode mercury_compile__backend_pass_by_preds_4(in, in, in, in, in, out,
	in, out, out, di, uo) is det.

mercury_compile__backend_pass_by_preds_4(PredInfo, ProcInfo0, ProcId, PredId,
		ModuleInfo0, ModuleInfo, GlobalData0, GlobalData, Proc) -->
	{ module_info_globals(ModuleInfo0, Globals) },
	{ globals__lookup_bool_option(Globals, follow_code, FollowCode) },
	{ globals__lookup_bool_option(Globals, prev_code, PrevCode) },
	( { FollowCode = yes ; PrevCode = yes } ->
		{ move_follow_code_in_proc(PredInfo, ProcInfo0, ProcInfo1,
			ModuleInfo0, ModuleInfo1) }
	;
		{ ProcInfo1 = ProcInfo0 },
		{ ModuleInfo1 = ModuleInfo0 }
	),
	{ simplify__find_simplifications(no, Globals, Simplifications) },
	simplify__proc([do_once | Simplifications], PredId, ProcId,
		ModuleInfo1, ModuleInfo2, ProcInfo1, ProcInfo2),
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
	detect_liveness_proc(PredId, ProcId, ModuleInfo3,
		ProcInfo3, ProcInfo4),
	write_proc_progress_message("% Allocating stack slots in ", PredId,
		                ProcId, ModuleInfo3),
	{ allocate_stack_slots_in_proc(ProcInfo4, PredId, ModuleInfo3,
		ProcInfo5) },
	write_proc_progress_message(
		"% Allocating storage locations for live vars in ",
				PredId, ProcId, ModuleInfo3),
	{ store_alloc_in_proc(ProcInfo5, PredId, ModuleInfo3, ProcInfo6) },
	globals__io_get_trace_level(TraceLevel),
	( { trace_level_is_none(TraceLevel) = no } ->
		write_proc_progress_message(
			"% Calculating goal paths in ",
					PredId, ProcId, ModuleInfo3),
		{ goal_path__fill_slots(ProcInfo6, ModuleInfo3, ProcInfo) }
	;
		{ ProcInfo = ProcInfo6 }
	),
	write_proc_progress_message(
		"% Generating low-level (LLDS) code for ",
				PredId, ProcId, ModuleInfo3),
	{ module_info_get_cell_counter(ModuleInfo3, CellCounter0) },
	{ generate_proc_code(PredInfo, ProcInfo, ProcId, PredId, ModuleInfo3,
		GlobalData0, GlobalData1, CellCounter0, CellCounter, Proc0) },
	{ globals__lookup_bool_option(Globals, optimize, Optimize) },
	( { Optimize = yes } ->
		optimize__proc(Proc0, GlobalData1, Proc)
	;
		{ Proc = Proc0 }
	),
	{ Proc = c_procedure(_, _, PredProcId, Instructions, _, _, _) },
	write_proc_progress_message(
		"% Generating call continuation information for ",
			PredId, ProcId, ModuleInfo3),
	{ continuation_info__maybe_process_proc_llds(Instructions, PredProcId,
		ModuleInfo3, GlobalData1, GlobalData) },
	{ module_info_set_cell_counter(ModuleInfo3, CellCounter, ModuleInfo) }.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__puritycheck(bool, module_info, bool, bool,
				module_info, bool, io__state, io__state).
:- mode mercury_compile__puritycheck(in, in, in, in, out, out, di, uo) is det.

mercury_compile__puritycheck(FoundTypeError, HLDS0, Verbose, Stats,
		HLDS, FoundPostTypecheckError) -->
	{ module_info_num_errors(HLDS0, NumErrors0) },
	puritycheck(FoundTypeError, HLDS0, FoundPostTypecheckError, HLDS),
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
	pred(task, module_info, module_info, io__state, io__state), 
	module_info, io__state, io__state).
:- mode mercury_compile__simplify(in, in, in, in, in, 
	pred(task, in, out, di, uo) is det, out, di, uo) is det.

mercury_compile__simplify(HLDS0, Warn, Once, Verbose, Stats, Process, HLDS) -->
	maybe_write_string(Verbose, "% Simplifying goals...\n"),
	maybe_flush_output(Verbose),
	globals__io_get_globals(Globals),
	{ simplify__find_simplifications(Warn, Globals, Simplifications0) },
	( { Once = yes } ->
		{ Simplifications = [do_once | Simplifications0] }
	;
		{ Simplifications = Simplifications0 }
	),
	call(Process, update_pred_error(simplify__pred(Simplifications)),
		HLDS0, HLDS),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_mark_static_terms(module_info, bool, bool,
		module_info, io__state, io__state).
:- mode mercury_compile__maybe_mark_static_terms(in, in, in, out, di, uo)
		is det.

mercury_compile__maybe_mark_static_terms(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(static_ground_terms, StaticGroundTerms),
	( { StaticGroundTerms = yes } ->
		maybe_write_string(Verbose,
			"% Marking static ground terms...\n"),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(update_proc(mark_static_terms),
			HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__maybe_add_trail_ops(module_info, bool, bool,
		module_info, io__state, io__state).
:- mode mercury_compile__maybe_add_trail_ops(in, in, in, out, di, uo)
		is det.

mercury_compile__maybe_add_trail_ops(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(use_trail, UseTrail),
	( { UseTrail = yes } ->
		maybe_write_string(Verbose,
			"% Adding trailing operations...\n"),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(update_proc(add_trail_ops),
			HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS = HLDS0 }
	).

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

:- pred mercury_compile__maybe_generate_schemas(module_info, bool,
		io__state, io__state).
:- mode mercury_compile__maybe_generate_schemas(in, in, di, uo) is det.

mercury_compile__maybe_generate_schemas(ModuleInfo, Verbose) -->
	globals__io_lookup_bool_option(generate_schemas, Generate),
	( { Generate = yes } ->
		maybe_write_string(Verbose, "% Writing schema file..."),
		rl_out__generate_schema_file(ModuleInfo),
		maybe_write_string(Verbose, " done.\n")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__tabling(module_info, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__tabling(in, in, out, di, uo) is det.

mercury_compile__tabling(HLDS0, Verbose, HLDS) -->
	maybe_write_string(Verbose,
		"% Transforming tabled predicates..."),
	maybe_flush_output(Verbose),
	{ table_gen__process_module(HLDS0, HLDS) },
	maybe_write_string(Verbose, " done.\n").

%-----------------------------------------------------------------------------%

:- pred mercury_compile__process_lambdas(module_info, bool, module_info,
	io__state, io__state).
:- mode mercury_compile__process_lambdas(in, in, out, di, uo) is det.

mercury_compile__process_lambdas(HLDS0, Verbose, HLDS) -->
	maybe_write_string(Verbose,
		"% Transforming lambda expressions..."),
	maybe_flush_output(Verbose),
	{ lambda__process_module(HLDS0, HLDS) },
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
		% The --no-polymorphism option really doesn't make much
		% sense anymore, because the polymorphism pass is necessary
		% for the proper mode analysis of code using existential
		% types.
		{ error("sorry, `--no-polymorphism' is no longer supported") }
	).

:- pred mercury_compile__maybe_type_ctor_infos(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_type_ctor_infos(in, in, in, out, di, uo) is det.

mercury_compile__maybe_type_ctor_infos(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(type_ctor_info, TypeCtorInfoOption),
	( 
		{ TypeCtorInfoOption = yes } 
	->
		maybe_write_string(Verbose,
			"% Generating type_ctor_info structures..."),
		maybe_flush_output(Verbose),
		{ type_ctor_info__generate_hlds(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
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
	globals__io_lookup_bool_option(optimize_higher_order, HigherOrder),
	% --type-specialization implies --user-guided-type-specialization.
	globals__io_lookup_bool_option(user_guided_type_specialization, Types),

	% Always produce the specialized versions for which
	% `:- pragma type_spec' declarations exist, because
	% importing modules might call them.
	{ module_info_type_spec_info(HLDS0, TypeSpecInfo) },
	{ TypeSpecInfo = type_spec_info(TypeSpecPreds, _, _, _) },

	( { HigherOrder = yes ; Types = yes ; \+ set__empty(TypeSpecPreds) } ->
		maybe_write_string(Verbose,
		"% Specializing higher-order and polymorphic predicates...\n"),
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
	globals__io_lookup_bool_option(inline_simple, Simple),
	globals__io_lookup_bool_option(inline_single_use, SingleUse),
	globals__io_lookup_int_option(inline_compound_threshold, Threshold),
	(
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

	% --constraint-propagation implies --local-constraint-propagation.
	globals__io_lookup_bool_option(local_constraint_propagation,
		Constraints),
	( { Deforest = yes ; Constraints = yes } ->
		{ Deforest = no, Constraints = no, 
			error("mercury_compile__maybe_deforestation")
		; Deforest = yes, Constraints = yes,
			Msg = "% Deforestation and constraint propagation...\n"
		; Deforest = yes, Constraints = no,
			Msg = "% Deforestation...\n"
		; Deforest = no, Constraints = yes,
			Msg = "% Constraint propagation...\n"
		},
		maybe_write_string(Verbose, Msg),
		maybe_flush_output(Verbose),
		deforestation(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_transform_dnf(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_transform_dnf(in, in, in, out, di, uo) is det.

mercury_compile__maybe_transform_dnf(HLDS0, Verbose, Stats, HLDS) -->
	{ module_info_get_do_aditi_compilation(HLDS0, Aditi) },
	( { Aditi = do_aditi_compilation } ->
		maybe_write_string(Verbose, "% Disjunctive normal form transformation..."),
		maybe_flush_output(Verbose),
		{ module_info_predids(HLDS0, PredIds) },
		{ set__init(AditiPreds0) },
		{ list__foldl(add_aditi_procs(HLDS0),
			PredIds, AditiPreds0, AditiPreds) },
		{ dnf__transform_module(HLDS0, no, yes(AditiPreds), HLDS) },
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).
	
:- pred add_aditi_procs(module_info, pred_id, 
		set(pred_proc_id), set(pred_proc_id)).
:- mode add_aditi_procs(in, in, in, out) is det.

add_aditi_procs(HLDS0, PredId, AditiPreds0, AditiPreds) :-
	module_info_pred_info(HLDS0, PredId, PredInfo),
	( hlds_pred__pred_info_is_aditi_relation(PredInfo) ->
		pred_info_procids(PredInfo, ProcIds),
		AddProc = 
		    lambda([ProcId::in, Preds0::in, Preds::out] is det, (
			set__insert(Preds0, proc(PredId, ProcId), Preds)
		    )),
		list__foldl(AddProc, ProcIds, AditiPreds0, AditiPreds)
	;
		AditiPreds = AditiPreds0
	).

:- pred mercury_compile__maybe_delay_construct(module_info::in,
	bool::in, bool::in, module_info::out, io__state::di, io__state::uo)
	is det.

mercury_compile__maybe_delay_construct(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(delay_construct, DelayConstruct),
	( { DelayConstruct = yes } ->
		maybe_write_string(Verbose,
			"% Delaying construction unifications ...\n"),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(
			update_proc_io(delay_construct_proc),
			HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
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

:- pred mercury_compile__maybe_unneeded_code(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_unneeded_code(in, in, in, out, di, uo) is det.

mercury_compile__maybe_unneeded_code(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(unneeded_code, UnneededCode),
	( { UnneededCode = yes } ->
		maybe_write_string(Verbose, "% Removing unneeded code from procedure bodies...\n"),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(
			update_module_io(unneeded_code__process_proc_msg),
			HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_magic(module_info, bool, bool,
		module_info, io__state, io__state).
:- mode mercury_compile__maybe_magic(in, in, in, out, di, uo) is det.

mercury_compile__maybe_magic(HLDS0, Verbose, Stats, HLDS) -->
	{ module_info_get_do_aditi_compilation(HLDS0, Aditi) },
	( { Aditi = do_aditi_compilation } ->
		maybe_write_string(Verbose, "% Supplementary magic sets transformation..."),
		maybe_flush_output(Verbose),
		magic__process_module(HLDS0, HLDS),
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
		maybe_write_string(Verbose,
			"% Eliminating dead procedures...\n"),
		maybe_flush_output(Verbose),
		dead_proc_elim(HLDS0, HLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_deep_profiling(module_info, bool, bool,
	module_info, list(layout_data), io__state, io__state).
:- mode mercury_compile__maybe_deep_profiling(in, in, in, out, out, di, uo)
	is det.

mercury_compile__maybe_deep_profiling(HLDS0, Verbose, Stats, HLDS,
		DeepProfilingStructures) -->
	globals__io_lookup_bool_option(profile_deep, Dead),
	( { Dead = yes } ->
		maybe_write_string(Verbose,
			"% Applying deep profiling transformation...\n"),
		maybe_flush_output(Verbose),
		apply_deep_profiling_transformation(HLDS0, HLDS,
			DeepProfilingStructures),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ HLDS0 = HLDS },
		{ DeepProfilingStructures = [] }
	).

:- pred mercury_compile__maybe_introduce_accumulators(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_introduce_accumulators(in, in, in, out, di, uo)
	is det.

mercury_compile__maybe_introduce_accumulators(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_lookup_bool_option(introduce_accumulators, Optimize),
	( { Optimize = yes } ->
		maybe_write_string(Verbose,
				"% Attempting to introduce accumulators...\n"),
		maybe_flush_output(Verbose),
		process_all_nonimported_procs(
			update_module_io(accumulator__process_proc),
			HLDS0, HLDS),
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
		process_all_nonimported_nonaditi_procs(
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
		process_all_nonimported_nonaditi_procs(update_module(
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
	process_all_nonimported_nonaditi_procs(
		update_proc_io(detect_liveness_proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__compute_stack_vars(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__compute_stack_vars(in, in, in, out, di, uo) is det.

mercury_compile__compute_stack_vars(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Computing stack vars..."),
	maybe_flush_output(Verbose),
	process_all_nonimported_nonaditi_procs(
		update_proc_predid(allocate_stack_slots_in_proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__allocate_store_map(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__allocate_store_map(in, in, in, out, di, uo) is det.

mercury_compile__allocate_store_map(HLDS0, Verbose, Stats, HLDS) -->
	maybe_write_string(Verbose, "% Allocating store map..."),
	maybe_flush_output(Verbose),
	process_all_nonimported_nonaditi_procs(
		update_proc_predid(store_alloc_in_proc),
		HLDS0, HLDS),
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_goal_paths(module_info, bool, bool,
	module_info, io__state, io__state).
:- mode mercury_compile__maybe_goal_paths(in, in, in, out, di, uo) is det.

mercury_compile__maybe_goal_paths(HLDS0, Verbose, Stats, HLDS) -->
	globals__io_get_trace_level(TraceLevel),
	( { trace_level_is_none(TraceLevel) = no } ->
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

:- pred mercury_compile__generate_code(module_info, global_data, bool, bool,
	module_info, global_data, list(c_procedure), io__state, io__state).
:- mode mercury_compile__generate_code(in, in, in, in, out, out, out, di, uo)
	is det.

mercury_compile__generate_code(HLDS0, GlobalData0, Verbose, Stats,
		HLDS, GlobalData, LLDS) -->
	maybe_write_string(Verbose, "% Generating code...\n"),
	maybe_flush_output(Verbose),
	generate_code(HLDS0, HLDS, GlobalData0, GlobalData, LLDS),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_do_optimize(list(c_procedure), global_data,
	bool, bool, list(c_procedure), io__state, io__state).
:- mode mercury_compile__maybe_do_optimize(in, in, in, in, out, di, uo) is det.

mercury_compile__maybe_do_optimize(LLDS0, GlobalData, Verbose, Stats, LLDS) -->
	globals__io_lookup_bool_option(optimize, Optimize),
	( { Optimize = yes } ->
		maybe_write_string(Verbose,
			"% Doing optimizations...\n"),
		maybe_flush_output(Verbose),
		optimize_main(LLDS0, GlobalData, LLDS),
		maybe_write_string(Verbose, "% done.\n"),
		maybe_report_stats(Stats)
	;
		{ LLDS = LLDS0 }
	).

:- pred mercury_compile__maybe_generate_stack_layouts(module_info, global_data,
	list(c_procedure), bool, bool, global_data, io__state, io__state).
:- mode mercury_compile__maybe_generate_stack_layouts(in, in, in, in, in, out, 
	di, uo) is det.

mercury_compile__maybe_generate_stack_layouts(ModuleInfo0, GlobalData0, LLDS0,
		Verbose, Stats, GlobalData) -->
	maybe_write_string(Verbose,
		"% Generating call continuation information..."),
	maybe_flush_output(Verbose),
	{ continuation_info__maybe_process_llds(LLDS0, ModuleInfo0,
		GlobalData0, GlobalData) },
	maybe_write_string(Verbose, " done.\n"),
	maybe_report_stats(Stats).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

%
% Gather together the information from the HLDS, given the foreign
% language we are going to use, that is used for the foreign language
% interface.  
% This stuff mostly just gets passed directly to the LLDS unchanged, but
% we do do a bit of code generation -- for example, we call
% export__get_foreign_export_{decls,defns} here, which do the generation
% of C code for `pragma export' declarations.
%

:- pred get_c_interface_info(module_info, foreign_language, 
		foreign_interface_info).
:- mode get_c_interface_info(in, in, out) is det.

get_c_interface_info(HLDS, UseForeignLanguage, Foreign_InterfaceInfo) :-
	module_info_name(HLDS, ModuleName),
	module_info_get_foreign_decl(HLDS, ForeignDecls),
	module_info_get_foreign_body_code(HLDS, ForeignBodyCode),
	foreign__filter_decls(UseForeignLanguage, ForeignDecls, 
		WantedForeignDecls, _OtherDecls),
	foreign__filter_bodys(UseForeignLanguage, ForeignBodyCode,
		WantedForeignBodys, _OtherBodys),
	export__get_foreign_export_decls(HLDS, Foreign_ExportDecls),
	export__get_foreign_export_defns(HLDS, Foreign_ExportDefns),
	Foreign_InterfaceInfo = foreign_interface_info(ModuleName,
		WantedForeignDecls, WantedForeignBodys,
		Foreign_ExportDecls, Foreign_ExportDefns).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The LLDS output pass

:- pred mercury_compile__output_pass(module_info, global_data,
	list(c_procedure), maybe(rl_file), module_name, bool,
	io__state, io__state).
:- mode mercury_compile__output_pass(in, in, in, in, in, out, di, uo) is det.

mercury_compile__output_pass(HLDS0, GlobalData, Procs0, MaybeRLFile,
		ModuleName, CompileErrors) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	globals__io_lookup_bool_option(common_data, CommonData),
	%
	% Here we generate the LLDS representations for
	% various data structures used for RTTI, type classes,
	% and stack layouts.
	% XXX this should perhaps be part of backend_pass
	% rather than output_pass.
	%
	{ type_ctor_info__generate_rtti(HLDS0, TypeCtorRttiData) },
	{ base_typeclass_info__generate_rtti(HLDS0, TypeClassInfoRttiData) },
	{ list__map(llds__wrap_rtti_data, TypeCtorRttiData, TypeCtorTables) },
	{ list__map(llds__wrap_rtti_data, TypeClassInfoRttiData,
		TypeClassInfos) },
	{ stack_layout__generate_llds(HLDS0, HLDS, GlobalData,
		PossiblyDynamicLayouts, StaticLayouts, LayoutLabels) },
	%
	% Here we perform some optimizations on the LLDS data.
	% XXX this should perhaps be part of backend_pass
	% rather than output_pass.
	%
	% XXX We assume that the foreign language we use is C
	{ get_c_interface_info(HLDS, c, C_InterfaceInfo) },
	{ global_data_get_all_proc_vars(GlobalData, GlobalVars) },
	{ global_data_get_all_non_common_static_data(GlobalData,
		NonCommonStaticData) },
	{ global_data_get_all_closure_layouts(GlobalData, ClosureLayouts) },
	{ CommonableData0 = StaticLayouts },
	( { CommonData = yes } ->
		{ llds_common(Procs0, CommonableData0, ModuleName, Procs1,
			CommonableData) }
	;
		{ CommonableData = CommonableData0 },
		{ Procs1 = Procs0 }
	),

	%
	% Next we put it all together and output it to one or more C files.
	%
	{ list__condense([CommonableData, NonCommonStaticData, ClosureLayouts,
		TypeCtorTables, TypeClassInfos, PossiblyDynamicLayouts],
		AllData) },
	mercury_compile__construct_c_file(C_InterfaceInfo, Procs1, GlobalVars,
		AllData, CFile, NumChunks),
	mercury_compile__output_llds(ModuleName, CFile, LayoutLabels,
		MaybeRLFile, Verbose, Stats),

	{ C_InterfaceInfo = foreign_interface_info(_, _, _, C_ExportDecls, _) },
	export__produce_header_file(C_ExportDecls, ModuleName),

	%
	% Finally we invoke the C compiler to compile it.
	%
	globals__io_lookup_bool_option(target_code_only, TargetCodeOnly),
	( { TargetCodeOnly = no } ->
		mercury_compile__c_to_obj(ModuleName, NumChunks, CompileOK),
		{ bool__not(CompileOK, CompileErrors) }
	;
		{ CompileErrors = no }
	).

	% Split the code up into bite-size chunks for the C compiler.

:- pred mercury_compile__construct_c_file(foreign_interface_info,
	list(c_procedure), list(comp_gen_c_var), list(comp_gen_c_data),
	c_file, int, io__state, io__state).
:- mode mercury_compile__construct_c_file(in, in, in, in, out, out, di, uo)
	is det.

mercury_compile__construct_c_file(C_InterfaceInfo, Procedures, GlobalVars,
		AllData, CFile, ComponentCount) -->
	{ C_InterfaceInfo = foreign_interface_info(ModuleSymName,
		C_HeaderCode0, C_BodyCode0, C_ExportDecls, C_ExportDefns) },
	{ llds_out__sym_name_mangle(ModuleSymName, MangledModuleName) },
	{ string__append(MangledModuleName, "_module", ModuleName) },
	globals__io_lookup_int_option(procs_per_c_function, ProcsPerFunc),
	{ get_c_body_code(C_BodyCode0, C_BodyCode) },
	( { ProcsPerFunc = 0 } ->
		% ProcsPerFunc = 0 really means infinity -
		% we store all the procs in a single function.
		{ ChunkedModules = [comp_gen_c_module(ModuleName,
			Procedures)] }
	;
		{ list__chunk(Procedures, ProcsPerFunc, ChunkedProcs) },
		{ mercury_compile__combine_chunks(ChunkedProcs, ModuleName,
			ChunkedModules) }
	),
	maybe_add_header_file_include(C_ExportDecls, ModuleSymName,
		C_HeaderCode0, C_HeaderCode),
	{ CFile = c_file(ModuleSymName, C_HeaderCode, C_BodyCode,
		C_ExportDefns, GlobalVars, AllData, ChunkedModules) },
	{ list__length(C_BodyCode, UserCCodeCount) },
	{ list__length(C_ExportDefns, ExportCount) },
	{ list__length(GlobalVars, CompGenVarCount) },
	{ list__length(AllData, CompGenDataCount) },
	{ list__length(ChunkedModules, CompGenCodeCount) },
	{ ComponentCount is UserCCodeCount + ExportCount
		+ CompGenVarCount + CompGenDataCount + CompGenCodeCount }.

:- pred maybe_add_header_file_include(foreign_export_decls, module_name,
	foreign_decl_info, foreign_decl_info, io__state, io__state).
:- mode maybe_add_header_file_include(in, in, in, out, di, uo) is det.

maybe_add_header_file_include(C_ExportDecls, ModuleName, 
		C_HeaderCode0, C_HeaderCode) -->
	(
		{ C_ExportDecls = [] },
		{ C_HeaderCode = C_HeaderCode0 }
	;
		{ C_ExportDecls = [_|_] },
		module_name_to_file_name(ModuleName, ".h", no, HeaderFileName),
		{ string__append_list(
			["#include """, HeaderFileName, """\n"],
			IncludeString) },

		{ term__context_init(Context) },
		{ Include = foreign_decl_code(c, IncludeString, Context) },

			% We put the new include at the end since the list is
			% stored in reverse, and we want this include to come
			% first.
		{ list__append(C_HeaderCode0, [Include], C_HeaderCode) }
	).

:- pred get_c_body_code(foreign_body_info, list(user_foreign_code)).
:- mode get_c_body_code(in, out) is det.

get_c_body_code([], []).
get_c_body_code([foreign_body_code(Lang, Code, Context) | CodesAndContexts],
		[user_foreign_code(Lang, Code, Context) | C_Modules]) :-
	get_c_body_code(CodesAndContexts, C_Modules).

:- pred mercury_compile__combine_chunks(list(list(c_procedure)), string,
	list(comp_gen_c_module)).
:- mode mercury_compile__combine_chunks(in, in, out) is det.

mercury_compile__combine_chunks(ChunkList, ModName, Modules) :-
	mercury_compile__combine_chunks_2(ChunkList, ModName, 0, Modules).

:- pred mercury_compile__combine_chunks_2(list(list(c_procedure)),
	string, int, list(comp_gen_c_module)).
:- mode mercury_compile__combine_chunks_2(in, in, in, out) is det.

mercury_compile__combine_chunks_2([], _ModName, _N, []).
mercury_compile__combine_chunks_2([Chunk | Chunks], ModuleName, Num,
		[Module | Modules]) :-
	string__int_to_string(Num, NumString),
	string__append(ModuleName, NumString, ThisModuleName),
	Module = comp_gen_c_module(ThisModuleName, Chunk),
	Num1 is Num + 1,
	mercury_compile__combine_chunks_2(Chunks, ModuleName, Num1, Modules).

:- pred mercury_compile__output_llds(module_name, c_file,
	map(llds__label, llds__data_addr), maybe(rl_file), bool, bool,
	io__state, io__state).
:- mode mercury_compile__output_llds(in, in, in, in, in, in, di, uo) is det.

mercury_compile__output_llds(ModuleName, LLDS0, StackLayoutLabels, MaybeRLFile,
		Verbose, Stats) -->
	maybe_write_string(Verbose,
		"% Writing output to `"),
	module_name_to_file_name(ModuleName, ".c", yes, FileName),
	maybe_write_string(Verbose, FileName),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	transform_llds(LLDS0, LLDS),
	output_llds(LLDS, StackLayoutLabels, MaybeRLFile),
	maybe_write_string(Verbose, " done.\n"),
	maybe_flush_output(Verbose),
	maybe_report_stats(Stats).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The MLDS-based alternative backend

:- pred mercury_compile__mlds_backend(module_info, mlds, io__state, io__state).
:- mode mercury_compile__mlds_backend(in, out, di, uo) is det.

mercury_compile__mlds_backend(HLDS51, MLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	mercury_compile__simplify(HLDS51, no, yes, Verbose, Stats, 
		process_all_nonimported_nonaditi_procs, HLDS53),
	mercury_compile__maybe_dump_hlds(HLDS53, "53", "simplify2"),

	mercury_compile__maybe_add_trail_ops(HLDS53, Verbose, Stats,
		HLDS55),
	mercury_compile__maybe_dump_hlds(HLDS55, "55", "add_trail_ops"),

	mercury_compile__maybe_mark_static_terms(HLDS55, Verbose, Stats,
		HLDS60),
	mercury_compile__maybe_dump_hlds(HLDS60, "60", "mark_static"),

	{ HLDS = HLDS60 },
	mercury_compile__maybe_dump_hlds(HLDS, "99", "final"),

	maybe_write_string(Verbose, "% Converting HLDS to MLDS...\n"),
	ml_code_gen(HLDS, MLDS0),
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats),
	mercury_compile__maybe_dump_mlds(MLDS0, "0", "initial"),

	maybe_write_string(Verbose, "% Generating RTTI data...\n"),
	{ mercury_compile__mlds_gen_rtti_data(HLDS, MLDS0, MLDS10) },
	maybe_write_string(Verbose, "% done.\n"),
	maybe_report_stats(Stats),
	mercury_compile__maybe_dump_mlds(MLDS10, "10", "rtti"),

	globals__io_lookup_bool_option(optimize_tailcalls, OptimizeTailCalls),
	( { OptimizeTailCalls = yes } ->
		maybe_write_string(Verbose, 
			"% Detecting tail calls...\n"),
		ml_mark_tailcalls(MLDS10, MLDS20),
		maybe_write_string(Verbose, "% done.\n")
	;
		{ MLDS10 = MLDS20 }
	),
	maybe_report_stats(Stats),
	mercury_compile__maybe_dump_mlds(MLDS20, "20", "tailcalls"),

	globals__io_lookup_bool_option(gcc_nested_functions, NestedFuncs),
	( { NestedFuncs = no } ->
		maybe_write_string(Verbose,
			"% Flattening nested functions...\n"),
		ml_elim_nested(MLDS20, MLDS30),
		maybe_write_string(Verbose, "% done.\n")
	;
		{ MLDS30 = MLDS20 }
	),
	maybe_report_stats(Stats),
	mercury_compile__maybe_dump_mlds(MLDS30, "30", "nested_funcs"),

	globals__io_lookup_bool_option(optimize, Optimize),
	( { Optimize = yes } ->
		maybe_write_string(Verbose, "% Optimizing MLDS...\n"),
		ml_optimize__optimize(MLDS30, MLDS40),
		maybe_write_string(Verbose, "% done.\n")
	;
		{ MLDS40 = MLDS30 }
	),
	maybe_report_stats(Stats),
	mercury_compile__maybe_dump_mlds(MLDS40, "40", "optimize"),

	{ MLDS = MLDS40 },
	mercury_compile__maybe_dump_mlds(MLDS, "99", "final").


:- pred mercury_compile__mlds_gen_rtti_data(module_info, mlds, mlds).
:- mode mercury_compile__mlds_gen_rtti_data(in, in, out) is det.

mercury_compile__mlds_gen_rtti_data(HLDS, MLDS0, MLDS) :-
	type_ctor_info__generate_rtti(HLDS, TypeCtorRtti),
	base_typeclass_info__generate_rtti(HLDS, TypeClassInfoRtti),
	list__append(TypeCtorRtti, TypeClassInfoRtti, RttiData),
	RttiDefns = rtti_data_list_to_mlds(HLDS, RttiData),
	MLDS0 = mlds(ModuleName, ForeignCode, Imports, Defns0),
	list__append(RttiDefns, Defns0, Defns),
	MLDS = mlds(ModuleName, ForeignCode, Imports, Defns).

% The `--high-level-C' MLDS output pass

:- pred mercury_compile__mlds_to_high_level_c(mlds, io__state, io__state).
:- mode mercury_compile__mlds_to_high_level_c(in, di, uo) is det.

mercury_compile__mlds_to_high_level_c(MLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	maybe_write_string(Verbose, "% Converting MLDS to C...\n"),
	mlds_to_c__output_mlds(MLDS, ""),
	maybe_write_string(Verbose, "% Finished converting MLDS to C.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__mlds_to_java(mlds, io__state, io__state).
:- mode mercury_compile__mlds_to_java(in, di, uo) is det.

mercury_compile__mlds_to_java(MLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	maybe_write_string(Verbose, "% Converting MLDS to Java...\n"),
	mlds_to_java__output_mlds(MLDS),
	maybe_write_string(Verbose, "% Finished converting MLDS to Java.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__maybe_mlds_to_gcc(mlds, bool, io__state, io__state).
:- mode mercury_compile__maybe_mlds_to_gcc(in, out, di, uo) is det.

mercury_compile__maybe_mlds_to_gcc(MLDS, ContainsCCode) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	maybe_write_string(Verbose,
		"% Passing MLDS to GCC and compiling to assembler...\n"),
	maybe_mlds_to_gcc__compile_to_asm(MLDS, ContainsCCode),
	maybe_write_string(Verbose, "% Finished compiling to assembler.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__mlds_to_il_assembler(mlds, io__state, io__state).
:- mode mercury_compile__mlds_to_il_assembler(in, di, uo) is det.

mercury_compile__mlds_to_il_assembler(MLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),

	maybe_write_string(Verbose, "% Converting MLDS to IL...\n"),
	mlds_to_ilasm__output_mlds(MLDS),
	maybe_write_string(Verbose, "% Finished converting MLDS to IL.\n"),
	maybe_report_stats(Stats).

:- pred mercury_compile__il_assemble(module_name, bool, io__state, io__state).
:- mode mercury_compile__il_assemble(in, in, di, uo) is det.

mercury_compile__il_assemble(ModuleName, HasMain) -->
	module_name_to_file_name(ModuleName, ".il", no, IL_File),
	module_name_to_file_name(ModuleName, ".dll", no, DLL_File),
	module_name_to_file_name(ModuleName, ".exe", no, EXE_File),
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Assembling `"),
	maybe_write_string(Verbose, IL_File),
	maybe_write_string(Verbose, "':\n"),
	{ Verbose = yes ->
		VerboseOpt = ""
	;
		VerboseOpt = "/quiet "
	},
	globals__io_lookup_bool_option(target_debug, Debug),
	{ Debug = yes ->
		DebugOpt = "/debug "
	;
		DebugOpt = ""
	},
	{ HasMain = yes ->
		TargetOpt = "",
		TargetFile = EXE_File
	;	
		TargetOpt = "/dll ",
		TargetFile = DLL_File
	},
	{ OutputOpt = "/out=" },
	{ string__append_list(["ilasm ", VerboseOpt, DebugOpt, TargetOpt,
		OutputOpt, TargetFile, " ", IL_File], Command) },
	invoke_system_command(Command, Succeeded),
	( { Succeeded = no } ->
		report_error("problem assembling IL file.")
	;
		[]
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
		globals__io_lookup_string_option(object_file_extension, Obj),
		module_name_to_file_name(ModuleName, ".c", no, C_File),
		module_name_to_file_name(ModuleName, Obj, yes, O_File),
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
		globals__io_lookup_string_option(object_file_extension, Obj),
		module_name_to_split_c_file_name(ModuleName, Chunk,
			".c", C_File),
		module_name_to_split_c_file_name(ModuleName, Chunk,
			Obj, O_File),
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

% WARNING: The code here duplicates the functionality of scripts/mgnuc.in.
% Any changes there may also require changes here, and vice versa.

mercury_compile__single_c_to_obj(C_File, O_File, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_string_option(c_flag_to_name_object_file,
			NameObjectFile),
	maybe_write_string(Verbose, "% Compiling `"),
	maybe_write_string(Verbose, C_File),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(cc, CC),
	globals__io_lookup_accumulating_option(cflags, C_Flags_List),
	{ join_string_list(C_Flags_List, "", "", " ", CFLAGS) },

	globals__io_lookup_bool_option(use_subdirs, UseSubdirs),
	globals__io_lookup_bool_option(split_c_files, SplitCFiles),
	{ (UseSubdirs = yes ; SplitCFiles = yes) ->
		% the source file (foo.c) will be compiled in a subdirectory
		% (either Mercury/cs, foo.dir, or Mercury/dirs/foo.dir,
		% depending on which of these two options is set)
		% so we need to add `-I.' so it can
		% include header files in the source directory.
		SubDirInclOpt = "-I. "
	;
		SubDirInclOpt = ""
	},
	globals__io_lookup_accumulating_option(c_include_directory,
	 	C_Incl_Dirs),
	{ InclOpt = string__append_list(list__condense(list__map(
	 	(func(C_INCL) = ["-I", C_INCL, " "]), C_Incl_Dirs))) },
	globals__io_lookup_bool_option(split_c_files, Split_C_Files),
	{ Split_C_Files = yes ->
		SplitOpt = "-DSPLIT_C_FILES "
	;
		SplitOpt = ""
	},
	globals__io_lookup_bool_option(highlevel_code, HighLevelCode),
	( { HighLevelCode = yes } ->
		{ HighLevelCodeOpt = "-DMR_HIGHLEVEL_CODE " }
	;
		{ HighLevelCodeOpt = "" }
	),
	globals__io_lookup_bool_option(gcc_nested_functions,
		GCC_NestedFunctions),
	( { GCC_NestedFunctions = yes } ->
		{ NestedFunctionsOpt = "-DMR_USE_GCC_NESTED_FUNCTIONS " }
	;
		{ NestedFunctionsOpt = "" }
	),
	globals__io_lookup_bool_option(highlevel_data, HighLevelData),
	( { HighLevelData = yes } ->
		{ HighLevelDataOpt = "-DMR_HIGHLEVEL_DATA " }
	;
		{ HighLevelDataOpt = "" }
	),
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
	globals__io_lookup_bool_option(parallel, Parallel),
	( { Parallel = yes } ->
		globals__io_lookup_string_option(cflags_for_threads,
			CFLAGS_FOR_THREADS)
	;
		{ CFLAGS_FOR_THREADS = "" }
	),
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
		ProfileCallsOpt = "-DMR_MPROF_PROFILE_CALLS "
	;
		ProfileCallsOpt = ""
	},
	globals__io_lookup_bool_option(profile_time, ProfileTime),
	{ ProfileTime = yes ->
		ProfileTimeOpt = "-DMR_MPROF_PROFILE_TIME "
	;
		ProfileTimeOpt = ""
	},
	globals__io_lookup_bool_option(profile_memory, ProfileMemory),
	{ ProfileMemory = yes ->
		ProfileMemoryOpt = "-DMR_MPROF_PROFILE_MEMORY "
	;
		ProfileMemoryOpt = ""
	},
	globals__io_lookup_bool_option(profile_deep, ProfileDeep),
	{ ProfileDeep = yes ->
		ProfileDeepOpt = "-DMR_DEEP_PROFILING "
	;
		ProfileDeepOpt = ""
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
	globals__io_lookup_bool_option(target_debug, Target_Debug),
	{ Target_Debug = yes ->
		Target_DebugOpt = "-g "
	;
		Target_DebugOpt = ""
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
	globals__io_lookup_bool_option(use_minimal_model, MinimalModel),
	{ MinimalModel = yes ->
		MinimalModelOpt = "-DMR_USE_MINIMAL_MODEL "
	;
		MinimalModelOpt = ""
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
		% We don't enable `-Wpointer-arith', because it causes
		% too many complaints in system header files.
		% This is fixed in gcc 3.0, though, so at some
		% point we should re-enable this.
		%
		% If --inline-alloc is enabled, don't enable missing-prototype
		% warnings, since gc_inline.h is missing lots of prototypes.
		%
		% For a full list of the other gcc warnings that we don't
		% enable, and why, see scripts/mgnuc.in.
		( InlineAlloc = yes ->
			WarningOpt = "-Wall -Wwrite-strings -Wshadow -Wmissing-prototypes -Wno-unused -Wno-uninitialized "
		;
			WarningOpt = "-Wall -Wwrite-strings -Wshadow -Wmissing-prototypes -Wno-unused -Wno-uninitialized -Wstrict-prototypes "
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
		HighLevelCodeOpt, NestedFunctionsOpt, HighLevelDataOpt,
		RegOpt, GotoOpt, AsmOpt,
		CFLAGS_FOR_REGS, " ", CFLAGS_FOR_GOTOS, " ",
		CFLAGS_FOR_THREADS, " ",
		GC_Opt, ProfileCallsOpt, ProfileTimeOpt, ProfileMemoryOpt,
		ProfileDeepOpt, PIC_Reg_Opt, TagsOpt, NumTagBitsOpt,
		Target_DebugOpt, LL_DebugOpt,
		StackTraceOpt, RequireTracingOpt,
		UseTrailOpt, MinimalModelOpt, TypeLayoutOpt,
		InlineAllocOpt, WarningOpt, CFLAGS,
		" -c ", C_File, " ", NameObjectFile, O_File], Command) },
	invoke_system_command(Command, Succeeded),
	( { Succeeded = no } ->
		report_error("problem compiling C file.")
	;
		[]
	).

:- pred mercury_compile__compile_java_file(module_name, io__state, io__state).
:- mode mercury_compile__compile_java_file(in, di, uo) is det.

mercury_compile__compile_java_file(ModuleName) -->
	module_name_to_file_name(ModuleName, ".java", no, JavaFile),
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Compiling `"),
	maybe_write_string(Verbose, JavaFile),
	maybe_write_string(Verbose, "':\n"),
	globals__io_lookup_string_option(java_compiler, JavaCompiler),
	globals__io_lookup_accumulating_option(java_flags, JavaFlagsList),
	{ join_string_list(JavaFlagsList, "", "", " ", JAVAFLAGS) },

	globals__io_lookup_accumulating_option(java_classpath,
	 	Java_Incl_Dirs),
	( { Java_Incl_Dirs = [] } ->
		{ InclOpt = "" }
	;
		% XXX PathSeparator should be ";" on Windows
		{ PathSeparator = ":" },
		{ join_string_list(Java_Incl_Dirs, "", "",
			PathSeparator, ClassPath) },
		{ InclOpt = string__append_list([
			"-classpath ", ClassPath, " "]) }
	),
	globals__io_lookup_bool_option(target_debug, Target_Debug),
	{ Target_Debug = yes ->
		Target_DebugOpt = "-g "
	;
		Target_DebugOpt = ""
	},
	% Be careful with the order here!  Some options may override others.
	% Also be careful that each option is separated by spaces.
	{ string__append_list([JavaCompiler, " ", InclOpt,
		Target_DebugOpt, JAVAFLAGS, JavaFile], Command) },
	invoke_system_command(Command, Succeeded),
	( { Succeeded = no } ->
		report_error("problem compiling Java file.")
	;
		[]
	).

:- pred mercury_compile__asm_to_obj(string, string, bool,
					io__state, io__state).
:- mode mercury_compile__asm_to_obj(in, in, out, di, uo) is det.

mercury_compile__asm_to_obj(AsmFile, ObjFile, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Assembling `"),
	maybe_write_string(Verbose, AsmFile),
	maybe_write_string(Verbose, "':\n"),
	% XXX should we use new asm_* options rather than
	% reusing cc, cflags, c_flag_to_name_object_file?
	globals__io_lookup_string_option(cc, CC),
	globals__io_lookup_string_option(c_flag_to_name_object_file,
			NameObjectFile),
	globals__io_lookup_accumulating_option(cflags, C_Flags_List),
	{ join_string_list(C_Flags_List, "", "", " ", CFLAGS) },
	% Be careful with the order here.
	% Also be careful that each option is separated by spaces.
	{ string__append_list([CC, " ", CFLAGS,
		" -c ", AsmFile, " ", NameObjectFile, ObjFile], Command) },
	invoke_system_command(Command, Succeeded),
	( { Succeeded = no } ->
		report_error("problem assembling the assembler file.")
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
	globals__io_lookup_string_option(object_file_extension, Obj),
	{ string__append("_init", Obj, InitObj) },
	module_name_to_file_name(ModuleName, "_init.c", yes, InitCFileName),
	module_name_to_file_name(ModuleName, InitObj, yes, InitObjFileName),

	globals__io_get_target(Target),
	globals__io_lookup_bool_option(split_c_files, SplitFiles),
	( { Target = asm } ->
	    % for --target asm, we generate everything into a single object file
	    ( { Modules = [FirstModule | _] } ->
		join_module_list([FirstModule], Obj, [], ObjectsList),
		{ string__append_list(ObjectsList, Objects) }
	    ;
		{ error("link_module_list: no modules") }
	    ),
	    { MakeLibCmdOK = yes }
	; { SplitFiles = yes } ->
	    module_name_to_file_name(ModuleName, ".a", yes, SplitLibFileName),
	    { string__append(".dir/*", Obj, DirObj) },
	    join_module_list(Modules, DirObj, [], ObjectList),
	    { list__append(
		["ar cr ", SplitLibFileName, " " | ObjectList],
		[" && ranlib ", SplitLibFileName],
		MakeLibCmdList) },
	    { string__append_list(MakeLibCmdList, MakeLibCmd) },
	    invoke_system_command(MakeLibCmd, MakeLibCmdOK),
	    { Objects = SplitLibFileName }
	;
	    { MakeLibCmdOK = yes },
	    join_module_list(Modules, Obj, [], ObjectsList),
	    { string__append_list(ObjectsList, Objects) }
	),
	( { MakeLibCmdOK = no } ->
	    report_error("creation of object file library failed.")
	;
	    % create the initialization C file
	    maybe_write_string(Verbose, "% Creating initialization file...\n"),
	    globals__io_get_trace_level(TraceLevel),
            { trace_level_is_none(TraceLevel) = no ->
		TraceOpt = "--trace "
	    ;
		TraceOpt = ""
	    },
	    join_module_list(Modules, ".c", ["-o ", InitCFileName], MkInitCmd0),
	    { string__append_list(["c2init ", TraceOpt | MkInitCmd0],
	    	MkInitCmd) },
	    invoke_shell_command(MkInitCmd, MkInitOK),
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
		    globals__io_lookup_bool_option(target_debug, Target_Debug),
		    { Target_Debug = yes ->
		    	Target_Debug_Opt = "--no-strip "
		    ;
		    	Target_Debug_Opt = ""
		    },
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
			["ml --grade ", Grade, " ",
			Target_Debug_Opt, TraceOpt, LinkFlags,
			" -o ", OutputFileName, " ",
			InitObjFileName, " ", Objects, " ",
			LinkObjects, " ",
			LinkLibraryDirectories, " ", LinkLibraries],
			LinkCmd) },
		    invoke_shell_command(LinkCmd, LinkCmdOK),
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

:- pred mercury_compile__maybe_dump_mlds(mlds, string, string,
	io__state, io__state).
:- mode mercury_compile__maybe_dump_mlds(in, in, in, di, uo) is det.

mercury_compile__maybe_dump_mlds(MLDS, StageNum, StageName) -->
	globals__io_lookup_accumulating_option(dump_mlds, DumpStages),
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
		{ ModuleName = mlds__get_module_name(MLDS) },
		module_name_to_file_name(ModuleName, ".mlds_dump", yes,
			BaseFileName),
		{ string__append_list(
			[BaseFileName, ".", StageNum, "-", StageName],
			DumpFile) },
		mercury_compile__dump_mlds(DumpFile, MLDS),

		{ string__append_list(["_dump.", StageNum, "-", StageName],
			DumpSuffix) },
		mlds_to_c__output_mlds(MLDS, DumpSuffix)
	;
		[]
	).

:- pred mercury_compile__dump_mlds(string, mlds, io__state, io__state).
:- mode mercury_compile__dump_mlds(in, in, di, uo) is det.

mercury_compile__dump_mlds(DumpFile, MLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Stats),
	maybe_write_string(Verbose, "% Dumping out MLDS to `"),
	maybe_write_string(Verbose, DumpFile),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__tell(DumpFile, Res),
	( { Res = ok } ->
		% XXX the following doesn't work, due to performance bugs
		% in pprint:
		%	pprint__write(80, pprint__to_doc(MLDS)),
		io__print(MLDS), io__nl,
		io__told,
		maybe_write_string(Verbose, " done.\n"),
		maybe_report_stats(Stats)
	;
		maybe_write_string(Verbose, "\n"),
		{ string__append_list(["can't open file `",
			DumpFile, "' for output."], ErrorMessage) },
		report_error(ErrorMessage)
	).

:- pred mercury_compile__maybe_dump_rl(list(rl_proc), module_info,
		string, string, io__state, io__state).
:- mode mercury_compile__maybe_dump_rl(in, in, in, in, di, uo) is det.

mercury_compile__maybe_dump_rl(Procs, ModuleInfo, _StageNum, StageName) -->
	globals__io_lookup_bool_option(dump_rl, Dump),
	( { Dump = yes } ->
		{ module_info_name(ModuleInfo, ModuleName0) },
		{ prog_out__sym_name_to_string(ModuleName0, ModuleName) },
		{ string__append_list([ModuleName, ".rl_dump", StageName],
			DumpFile) },
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Dumping out RL to `"),
		maybe_write_string(Verbose, DumpFile),
		maybe_write_string(Verbose, "'..."),
		maybe_flush_output(Verbose),
		io__tell(DumpFile, Res),
		( { Res = ok } ->
			list__foldl(rl_dump__write_procedure(ModuleInfo), 
				Procs),
			io__told,
			maybe_write_string(Verbose, " done.\n")
		;
			maybe_write_string(Verbose, "\n"),
			{ string__append_list(["can't open file `",
				DumpFile, "' for output."], ErrorMessage) },
			report_error(ErrorMessage)
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
