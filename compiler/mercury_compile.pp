%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: mercury-compile.m.
% Main authors: fjh, zs

% This is the top-level of the Mercury compiler.

% Imports and exports are handled here.

%-----------------------------------------------------------------------------%

:- module mercury_compile.
:- interface.
:- import_module list, string, io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module prog_io, make_hlds, typecheck, modes, switch_detection.
:- import_module cse_detection, polymorphism, garbage_out, shapes.
:- import_module liveness, det_analysis, follow_code, follow_vars, live_vars.
:- import_module arg_info, store_alloc, code_gen, optimize, llds, inlining.
:- import_module negation, call_graph.
:- import_module common.

:- import_module prog_out, prog_util, hlds_out.
:- import_module mercury_to_mercury, mercury_to_goedel.
:- import_module getopt, options, globals.
:- import_module int, map, set, std_util, dir, tree234, term, varset, hlds.
:- import_module require.

:- import_module debugger.

%-----------------------------------------------------------------------------%

main -->
	io__command_line_arguments(Args0),
	{ getopt__process_options(Args0, Args, Result0) },
	postprocess_options(Result0, Result), 
	main_2(Result, Args).

% Convert string-valued options into the appropriate enumeration types,
% and process implications among the options.

:- pred postprocess_options(maybe_option_table, maybe(string),
	io__state, io__state).
:- mode postprocess_options(in, out, di, uo) is det.

postprocess_options(error(ErrorMessage), yes(ErrorMessage)) --> [].
postprocess_options(ok(OptionTable0), Error) -->
	{ map__lookup(OptionTable0, grade, GradeOpt) },
	(   
		{ GradeOpt = string(GradeString) },
		{ convert_gc_grade_option(GradeString, OptionTable0,
			OptionTable) }
	->
		{ map__lookup(OptionTable, gc, GC_Method0) },
		(
			{ GC_Method0 = string(GC_Method_String) },
			{ convert_gc_method(GC_Method_String, GC_Method) }
		->
			{ map__lookup(OptionTable, tags, Tags_Method0) },
			( 
				{ Tags_Method0 = string(Tags_Method_String) },
				{ convert_tags_method(Tags_Method_String,
					Tags_Method) }
			->
				% work around for NU-Prolog problems
				( { map__search(OptionTable0, heap_space,
					int(HeapSpace)) }
				->
					io__preallocate_heap_space(HeapSpace)
				;
					[]
				),
				globals__io_init(OptionTable, GC_Method,
					Tags_Method),
				% --gc conservative implies --no-reclaim-heap-*
				( { GC_Method = conservative } ->
					globals__io_set_option(
						reclaim_heap_on_semidet_failure,
						bool(no)
					),
					globals__io_set_option(
						reclaim_heap_on_nondet_failure,
						bool(no)
					)
				;
					[]
				),
				% --tags none implies --num-tag-bits 0
				( { Tags_Method = none } ->
					globals__io_set_option(num_tag_bits,
						int(0))
				;	
					[]
				),
				% --very-verbose implies --verbose
				globals__io_lookup_bool_option(very_verbose,
					VeryVerbose),
				( { VeryVerbose = yes } ->
					globals__io_set_option(verbose,
						bool(yes))
				;	
					[]
				),
				% --debugger implies --nocompile
				globals__io_lookup_bool_option(debugger,
					Debugger),
				( { Debugger = yes } ->
					globals__io_set_option(link,
						bool(no)),
					globals__io_set_option(compile,
						bool(no)),
					globals__io_set_option(generate_code,
						bool(no)),
					globals__io_set_option(inlining,
						bool(no))
				;
					[]
				),
				% dump and flags statistics
				% require compilation by phases
				globals__io_lookup_accumulating_option(dump_hlds, DumpStages),
				globals__io_lookup_bool_option(statistics, Statistics),
				( { DumpStages \= [] ; Statistics = yes } ->
					globals__io_set_option(trad_passes,
						bool(no))
				;
					[]
				),
				% --link implies --compile
				globals__io_lookup_bool_option(link, Link),
				( { Link = yes } ->
					globals__io_set_option(compile,
						bool(yes))
				;	
					[]
				),
				% --compile implies --generate-code
				globals__io_lookup_bool_option(compile,
					Compile),
				( { Compile = yes } ->
					globals__io_set_option(generate_code,
						bool(yes))
				;	
					[]
				),
				{ Error = no }
			;
				{ Error = yes("Invalid tags option (must be `none', `low' or `high')") }
			)
		;
			{ Error = yes("Invalid GC option (must be `none', `conservative' or `accurate')") }
		)
	;
		{ Error = yes("Invalid grade option") }
	).

:- pred convert_gc_grade_option(string::in, option_table::in, option_table::out)
	is semidet.

convert_gc_grade_option(GC_Grade) -->
	( { string__remove_suffix(GC_Grade, ".gc", Grade) } ->
		set_string_opt(gc, "conservative"),
		convert_grade_option(Grade)
	;
		set_string_opt(gc, "none"),
		convert_grade_option(GC_Grade)
	).

:- pred convert_grade_option(string::in, option_table::in, option_table::out)
	is semidet.

convert_grade_option("asm_fast") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, yes),
	set_bool_opt(asm_labels, yes).
convert_grade_option("fast") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, yes),
	set_bool_opt(asm_labels, no).
convert_grade_option("asm_jump") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, yes).
convert_grade_option("jump") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, yes),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, no).
convert_grade_option("reg") -->
	set_bool_opt(debug, no),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, no),
	set_bool_opt(gcc_global_registers, yes),
	set_bool_opt(asm_labels, no).
convert_grade_option("none") -->
	set_bool_opt(debug, yes),
	set_bool_opt(c_optimize, yes),
	set_bool_opt(gcc_non_local_gotos, no),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, no).
convert_grade_option("debug") -->
	set_bool_opt(debug, yes),
	set_bool_opt(c_optimize, no),
	set_bool_opt(gcc_non_local_gotos, no),
	set_bool_opt(gcc_global_registers, no),
	set_bool_opt(asm_labels, no).

:- pred set_bool_opt(option, bool, option_table, option_table).
:- mode set_bool_opt(in, in, in, out) is det.

set_bool_opt(Option, Value, OptionTable0, OptionTable) :-
	map__set(OptionTable0, Option, bool(Value), OptionTable).

:- pred set_string_opt(option, string, option_table, option_table).
:- mode set_string_opt(in, in, in, out) is det.

set_string_opt(Option, Value, OptionTable0, OptionTable) :-
	map__set(OptionTable0, Option, string(Value), OptionTable).

	% Display error message and then usage message
:- pred usage_error(string::in, io__state::di, io__state::uo) is det.
usage_error(ErrorMessage) -->
	io__progname_base("mercury_compile", ProgName),
	io__stderr_stream(StdErr),
	io__write_string(StdErr, ProgName),
	io__write_string(StdErr, ": "),
	io__write_string(StdErr, ErrorMessage),
	io__write_string(StdErr, "\n"),
	io__set_exit_status(1),
	usage.

:- pred report_error(string::in, io__state::di, io__state::uo) is det.
report_error(ErrorMessage) -->
	io__write_string("Error: "),
	io__write_string(ErrorMessage),
	io__write_string("\n"),
	io__set_exit_status(1).

	% Display usage message
:- pred usage(io__state::di, io__state::uo) is det.
usage -->
	io__progname_base("mercury_compile", ProgName),
	io__stderr_stream(StdErr),
 	io__write_string(StdErr, "Mercury compiler version 0.2.5\n"),
	io__write_string(StdErr, "Usage: "),
	io__write_string(StdErr, ProgName),
	io__write_string(StdErr, " [<options>] <module>\n"),
	io__write_string(StdErr, "Use `"),
	io__write_string(StdErr, ProgName),
	io__write_string(StdErr, " --help' for more information.\n").

:- pred long_usage(io__state::di, io__state::uo) is det.
long_usage -->
	io__progname_base("mercury_compile", ProgName),
 	io__write_string("Mercury compiler version 0.2.5\n"),
	io__write_string("Usage: "),
	io__write_string(ProgName),
	io__write_string(" [<options>] <module>\n"),
	io__write_string("Options:\n"),
	options_help.

%-----------------------------------------------------------------------------%

:- pred main_2(maybe(string), list(string), io__state, io__state).
:- mode main_2(in, in, di, uo) is det.

main_2(yes(ErrorMessage), _) -->
	usage_error(ErrorMessage).
main_2(no, Args) -->
	globals__io_lookup_bool_option(help, Help),
	globals__io_lookup_bool_option(debugger,Debugger),
	( { Help = yes } ->
		long_usage
	; { Args = [] } ->
		usage
        ; { Debugger = yes } ->
% Now display message to user that he/she have selected the debugger
io__write_string("\nYou have selected the Debugger for Mercury.\n"),
io__write_string("Unfortunately the debugger is still under construction,\n"),
io__write_string("in other words, it doesn't work!\n"),
io__write_string("Even if it works, it's a figment of your\n"),
io__write_string("imagination because officially it doesn't work.\n"),
		{ strip_module_suffixes(Args,ModuleNames) },
		dprocess_module_list(ModuleNames,HLDS_OUT),
		mercury_compile__dump_hlds("debugger.hlds.output",
			HLDS_OUT),
		the_debugger(HLDS_OUT)
	;
		{ strip_module_suffixes(Args, ModuleNames) },
		process_module_list(ModuleNames),
		globals__io_lookup_bool_option(link, Link),
		io__get_exit_status(ExitStatus),
		( { Link = yes, ExitStatus = 0 } ->
			mercury_compile__link_module_list(ModuleNames)
		;
			[]
		)
	).

%----------------------------------------------------------------------------%
% Here lies, old ye implementation of zee Debugger

:- pred dprocess_module_list(list(string),module_info,io__state,io__state).
:- mode dprocess_module_list(in,out,di,uo) is det.

dprocess_module_list(ModuleNames,HLDS_OUT) -->
	% obtain the list of items from all the modules
	% (including imported modules)
	globals__io_lookup_string_option(builtin_module,BuiltinModule),
	dprocess_module_list2([BuiltinModule|ModuleNames], [], [], ListItems) ,
	{ varset__init(VarSet) }, % start with an empty varset
	{ term__context_init(Context) }, % dummy context
	{ list__append(ListItems,[module_defn(VarSet,imported) - Context],
	  ListItems1) },
	{ Module = module("debugger123",[],[],ListItems1,no) },
	mercury_compile__pre_hlds_pass(Module,HLDS1,Proceed1),
	mercury_compile__semantic_pass(HLDS1,HLDS9,Proceed1,Proceed2),
	( { Proceed2 = no },
	    { module_info_init("Nothing",HLDS_OUT) },
	    io__write_string("Errors detected by semantic_pass\n")
	;
	    { Proceed2 = yes },
	    mercury_compile__check_determinism(HLDS9,HLDS_OUT,FoundError),
            % Test for error
	    ( { FoundError = yes },
	      io__write_string("Errors detected by check_determinism\n")
	    ;
	      { FoundError = no }
	    )
	).

% Takes a list of module names and obtain items from each module
% XXX should return an error indicator!

:- pred dprocess_module_list2(list(string), list(string), item_list, item_list,
	io__state, io__state).
:- mode dprocess_module_list2(in, in, in, out, di, uo) is det.

dprocess_module_list2([], _Modules, Items, Items) --> [].

dprocess_module_list2([M|Ms], ModulesRead, Items0, Items) -->
	% check whether we've already read this module in
	( { list__member(M, ModulesRead) } ->
		dprocess_module_list2(Ms, ModulesRead, Items0, Items)
	;
		io__gc_call(read_mod(M, ".m", "Reading module",
				ModuleItems, Error)),
		{ get_dependencies(ModuleItems, ImportedModules) },
		( { Error = yes } ->
			io__write_string("read_mod detects errors\n")
		;
		  { Error = fatal } ->
			io__write_string("read_mod detects fatal errors\n")
		;
			[]
		),
		{ list__append(ModuleItems, Items0, Items1) },
		{ list__append(ImportedModules, Ms, Ms1) },
		dprocess_module_list2(Ms1, [M | ModulesRead], Items1, Items)
	).

%----------------------------------------------------------------------------%

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
	maybe_write_string(Verbose, "% Parsing...\n"),
	io__gc_call(read_mod(ModuleName, ".m", "Reading module",
			Items0, Error)),
	globals__io_lookup_bool_option(statistics, Statistics),
	maybe_report_stats(Statistics),

	globals__io_lookup_bool_option(make_interface, MakeInterface),
	globals__io_lookup_bool_option(convert_to_mercury, ConvertToMercury),
	globals__io_lookup_bool_option(convert_to_goedel, ConvertToGoedel),
	( { Error = fatal } ->
		[]
	; { MakeInterface = yes } ->
		{ get_interface(Items0, InterfaceItems0) },
		check_for_clauses_in_interface(InterfaceItems0, InterfaceItems),
		write_interface_file(ModuleName, ".int", InterfaceItems),
		{ get_short_interface(InterfaceItems, ShortInterfaceItems) },
		write_interface_file(ModuleName, ".int2", ShortInterfaceItems),
		touch_datestamp(ModuleName)
	; { ConvertToMercury = yes } ->
		{ string__append(ModuleName, ".ugly", OutputFileName) },
		convert_to_mercury(ModuleName, OutputFileName, Items0)
	; { ConvertToGoedel = yes } ->
		convert_to_goedel(ModuleName, Items0)
	;
		{ get_dependencies(Items0, ImportedModules) },

			% Note that the module `mercury_builtin' is always
			% automatically imported.  (Well, the actual name
			% is overrideable using the `--builtin-module' option.) 
		globals__io_lookup_string_option(builtin_module, BuiltinModule),
			% we add a pseudo-declaration `:- imported' at the end
			% of the item list, so that make_hlds knows which items
			% are imported and which are defined in the main module
		{ varset__init(VarSet) },
		{ term__context_init(ModuleName, 0, Context) },
		{ list__append(Items0,
			[module_defn(VarSet, imported) - Context], Items1) },
		{ dir__basename(ModuleName, BaseModuleName) },
		{ Module0 = module(BaseModuleName, [], [], Items1, no) },
		process_module_interfaces([BuiltinModule | ImportedModules], 
			[], Module0, Module),
		{ Module = module(_, _, _, _, Error2) },
		( { Error = no, Error2 = no } ->
			mercury_compile(Module)
		;
			[]
		)
	).

%-----------------------------------------------------------------------------%

:- pred write_interface_file(string, string, item_list, io__state, io__state).
:- mode write_interface_file(in, in, in, di, uo) is det.

write_interface_file(ModuleName, Suffix, InterfaceItems) -->

		% create <Module>.int.tmp

	{ string__append(ModuleName, Suffix, OutputFileName) },
	{ string__append(OutputFileName, ".tmp", TmpOutputFileName) },
	{ dir__basename(ModuleName, BaseModuleName) },
	convert_to_mercury(BaseModuleName, TmpOutputFileName, InterfaceItems),

		% invoke the shell script `mercury_update_interface'
		% to update <Module>.int from <Module>.int.tmp if
		% necessary

	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Updating interface:\n"),
	( { Verbose = yes } ->
		{ Command = "mercury_update_interface -v " }
	;
		{ Command = "mercury_update_interface " }
	),
	{ string__append(Command, OutputFileName, ShellCommand) },
	mercury_compile__invoke_system_command(ShellCommand, Succeeded),
	( { Succeeded = no } ->
		report_error("problem updating interface files")
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% touch the datestamp file `<Module>.date'

:- pred touch_datestamp(string, io__state, io__state).
:- mode touch_datestamp(in, di, uo) is det.

touch_datestamp(ModuleName) -->
	{ string__append(ModuleName, ".date", OutputFileName) },

	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Touching `"),
	maybe_write_string(Verbose, OutputFileName),
	maybe_write_string(Verbose, "'... "),
	maybe_flush_output(Verbose),
	io__open_output(OutputFileName, Result),
	( { Result = ok(OutputStream) } ->
		io__write_string(OutputStream, "\n"),
		io__close_output(OutputStream),
		maybe_write_string(Verbose, " done.\n")
	;
		io__write_string("\nError opening `"),
		io__write_string(OutputFileName),
		io__write_string("' for output\n")
	).

%-----------------------------------------------------------------------------%

:- pred write_dependency_file(string, list(string), list(string),
				io__state, io__state).
:- mode write_dependency_file(in, in, in, di, uo) is det.

write_dependency_file(ModuleName, LongDeps, ShortDeps) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	{ string__append(ModuleName, ".d", DependencyFileName) },
	maybe_write_string(Verbose, "% Writing auto-dependency file `"),
	maybe_write_string(Verbose, DependencyFileName),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__open_output(DependencyFileName, Result),
	( { Result = ok(DepStream) } ->
		io__write_string(DepStream, ModuleName),
		io__write_string(DepStream, ".err : "),
		io__write_string(DepStream, ModuleName),
		io__write_string(DepStream, ".m"),
		write_dependencies_list(LongDeps, ".int", DepStream),
		write_dependencies_list(ShortDeps, ".int2", DepStream),
		io__write_string(DepStream, "\n"),

		io__write_string(DepStream, ModuleName),
		io__write_string(DepStream, ".c : "),
		io__write_string(DepStream, ModuleName),
		io__write_string(DepStream, ".m"),
		write_dependencies_list(LongDeps, ".int", DepStream),
		write_dependencies_list(ShortDeps, ".int2", DepStream),
		io__write_string(DepStream, "\n"),

		io__close_output(DepStream),
		maybe_write_string(Verbose, " done.\n")
	;
		{ string__append_list(["can't open file `", DependencyFileName,
				"' for output"], Message) },
		report_error(Message)
	).

%-----------------------------------------------------------------------------%

:- pred generate_dependencies(string, io__state, io__state).
:- mode generate_dependencies(in, di, uo) is det.

generate_dependencies(Module) -->
	{ string__append(Module, ".dep", DepFileName) },
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Creating auto-dependency file `"),
	maybe_write_string(Verbose, DepFileName),
	maybe_write_string(Verbose, "'...\n"),
	io__open_output(DepFileName, Result),
	( { Result = ok(DepStream) } ->
		{ map__init(DepsMap) },
		generate_dependencies_2([Module], Module, DepsMap, DepStream),
		io__close_output(DepStream),
		maybe_write_string(Verbose, "% done\n")
	;
		{ string__append_list(["can't open file `", DepFileName,
				"' for output"], Message) },
		report_error(Message)
	).

:- type deps_map == map(string, deps).
:- type deps
	---> deps(
		bool,		% have we processed this module yet?
		module_error,	% if we did, where there any errors?
		list(string),	% interface dependencies
		list(string)	% implementation dependencies
	).

:- pred generate_dependencies_2(list(string), string, deps_map,
				io__output_stream, io__state, io__state).
:- mode generate_dependencies_2(in, in, in, in, di, uo) is det.

generate_dependencies_2([], ModuleName, DepsMap, DepStream) -->
	io__write_string(DepStream,
		"# automatically generated dependencies for module `"),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, "'.\n"),

	{ map__keys(DepsMap, Modules0) },
	{ select_ok_modules(Modules0, DepsMap, Modules) },

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".srcs = "),
	write_dependencies_list(Modules, ".m", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nos = "),
	write_dependencies_list(Modules, ".no", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".qls = "),
	write_dependencies_list(Modules, ".ql", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".cs = "),
	write_dependencies_list(Modules, ".c", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".os = "),
	write_dependencies_list(Modules, ".o", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ss = "),
	write_dependencies_list(Modules, ".s", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".errs = "),
	write_dependencies_list(Modules, ".err", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".dates = "),
	write_dependencies_list(Modules, ".date", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".deps = "),
	write_dependencies_list(Modules, ".d", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ints = "),
	write_dependencies_list(Modules, ".int", DepStream),
	write_dependencies_list(Modules, ".int2", DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, " : $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".os) "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, "_init.o\n"),
	io__write_string(DepStream, "\t$(ML) -s $(GRADE) $(MLFLAGS) -o "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, " "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, "_init.o $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".os)\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, "_init.c : $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".cs)\n"),
	io__write_string(DepStream, "\t$(C2INIT) $(C2INITFLAGS) $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".cs) > "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, "_init.c\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nu : $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nos)\n"),
	io__write_string(DepStream, "\t$(MNL) $(MNLFLAGS) -o "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nu $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nos)\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nu.debug : $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nos)\n"),
	io__write_string(DepStream, "\t$(MNL) --debug $(MNLFLAGS) -o "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nu.debug $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nos)\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".sicstus : $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".qls)\n"),
	io__write_string(DepStream, "\t$(MSL) $(MSLFLAGS) -o "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".sicstus $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".qls)\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".sicstus.debug : $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".qls)\n"),
	io__write_string(DepStream, "\t$(MSL) --debug $(MSLFLAGS) -o "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".sicstus.debug $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".qls)\n\n"),

	io__write_string(DepStream, "clean_progs: clean_prog_"),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, "clean_prog_"),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ":\n"),
	io__write_string(DepStream, "\t-rm -f "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, " "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nu "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".nu.debug "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".sicstus "),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".sicstus.debug"),
	io__write_string(DepStream, "\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".check : $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".errs)\n\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ints : $("),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".dates)\n").

generate_dependencies_2([Module | Modules], ModuleName, DepsMap0, DepStream) -->
	%
	% Look up the module's dependencies, and determine whether
	% it has been processed yet.
	%
	lookup_dependencies(Module, DepsMap0, Done, Error, ImplDeps, IntDeps,
				DepsMap1),
	%
	% If the module hadn't been processed yet, compute its
	% transitive dependencies (we already know its primary dependencies),
	% output a line for this module to the dependency file
	% (if the file exists),
	% add its imports to the list of dependencies we need to
	% generate, and mark it as having been processed.
	%
	( { Done = no } ->
		{ map__set(DepsMap1, Module,
			deps(yes, Error, IntDeps, ImplDeps), DepsMap2) },
		transitive_dependencies(IntDeps, DepsMap2, SecondaryDeps,
			DepsMap),
		( { Error \= fatal } ->
			write_dependency_file(Module, ImplDeps, SecondaryDeps)
		;
			[]
		),
		{ list__append(ImplDeps, Modules, Modules2) }
	;
		{ DepsMap = DepsMap1 },
		{ Modules2 = Modules }
	),
	%
	% Recursively process the remaining modules
	%
	generate_dependencies_2(Modules2, ModuleName, DepsMap, DepStream).

%-----------------------------------------------------------------------------%

:- pred select_ok_modules(list(string), deps_map, list(string)).
:- mode select_ok_modules(in, in, out) is det.

select_ok_modules([], _, []).
select_ok_modules([Module | Modules0], DepsMap, Modules) :-
	map__lookup(DepsMap, Module, deps(_, Error, _, _)),
	( Error = fatal ->
		Modules = Modules1
	;
		Modules = [Module | Modules1]
	),
	select_ok_modules(Modules0, DepsMap, Modules1).

%-----------------------------------------------------------------------------%

	% Given a list of modules, return a list of those modules
	% and all their transitive interface dependencies.

:- pred transitive_dependencies(list(string), deps_map, list(string), deps_map,
				io__state, io__state).
:- mode transitive_dependencies(in, in, out, out, di, uo) is det.

transitive_dependencies(Modules, DepsMap0, Dependencies, DepsMap) -->
	{ set__init(Dependencies0) },
	transitive_dependencies_2(Modules, Dependencies0, DepsMap0,
		Dependencies1, DepsMap),
	{ set__to_sorted_list(Dependencies1, Dependencies) }.

:- pred transitive_dependencies_2(list(string), set(string), deps_map,
				set(string), deps_map,
				io__state, io__state).
:- mode transitive_dependencies_2(in, in, in, out, out, di, uo) is det.

transitive_dependencies_2([], Deps, DepsMap, Deps, DepsMap) --> [].
transitive_dependencies_2([Module | Modules0], Deps0, DepsMap0, Deps, DepsMap)
		-->
	( { set__member(Module, Deps0) } ->
		{ Deps1 = Deps0 },
		{ DepsMap1 = DepsMap0 },
		{ Modules1 = Modules0 }
	;
		{ set__insert(Deps0, Module, Deps1) },
		lookup_dependencies(Module, DepsMap0,
					_, _, IntDeps, _, DepsMap1),
		{ list__append(IntDeps, Modules0, Modules1) }
	),
	transitive_dependencies_2(Modules1, Deps1, DepsMap1, Deps, DepsMap).

%-----------------------------------------------------------------------------%

	% Look up a module in the dependency map
	% If we don't know its dependencies, read the
	% module and save the dependencies in the dependency map.

:- pred lookup_dependencies(string, deps_map,
		bool, module_error, list(string), list(string), deps_map,
		io__state, io__state).
:- mode lookup_dependencies(in, in, out, out, out, out, out, di, uo) is det.

lookup_dependencies(Module, DepsMap0, Done, Error, IntDeps, ImplDeps, DepsMap)
		-->
	(
		{ map__search(DepsMap0, Module,
			deps(Done0, Error0, IntDeps0, ImplDeps0)) }
	->
		{ Done = Done0 },
		{ Error = Error0 },
		{ IntDeps = IntDeps0 },
		{ ImplDeps = ImplDeps0 },
		{ DepsMap = DepsMap0 }
	;
		read_dependencies(Module, ImplDeps, IntDeps, Error),
		{ map__set(DepsMap0, Module, deps(no, Error, IntDeps, ImplDeps),
			DepsMap) },
		{ Done = no }
	).

	% Read a module to determine its dependencies.
	
:- pred read_dependencies(string, list(string), list(string), module_error,
				io__state, io__state).
:- mode read_dependencies(in, out, out, out, di, uo) is det.

read_dependencies(Module, InterfaceDeps, ImplementationDeps, Error) -->
	io__gc_call(read_mod_ignore_errors(Module, ".m",
			"Getting dependencies for module", Items, Error)),
	{ get_dependencies(Items, ImplementationDeps0) },
	{ get_interface(Items, InterfaceItems) },
	{ get_dependencies(InterfaceItems, InterfaceDeps) },
		% Note that the module `mercury_builtin' is always
		% automatically imported.  (Well, the actual name
		% is overrideable using the `--builtin-module' option.) 
	globals__io_lookup_string_option(builtin_module, BuiltinModule),
	{ ImplementationDeps = [BuiltinModule | ImplementationDeps0] }.

%-----------------------------------------------------------------------------%

:- pred write_dependencies_list(list(string), string, io__output_stream,
				io__state, io__state).
:- mode write_dependencies_list(in, in, in, di, uo) is det.

write_dependencies_list([], _, _) --> [].
write_dependencies_list([Module | Modules], Suffix, DepStream) -->
	io__write_string(DepStream, " \\\n\t"),
	io__write_string(DepStream, Module),
	io__write_string(DepStream, Suffix),
	write_dependencies_list(Modules, Suffix, DepStream).

%-----------------------------------------------------------------------------%

:- pred check_for_clauses_in_interface(item_list, item_list,
					io__state, io__state).
:- mode check_for_clauses_in_interface(in, out, di, uo) is det.

check_for_clauses_in_interface([], []) --> [].
check_for_clauses_in_interface([Item0 | Items0], Items) -->
	( { Item0 = clause(_,_,_,_) - Context } ->
		prog_out__write_context(Context),
		io__write_string("Warning: clause in module interface.\n"),
		check_for_clauses_in_interface(Items0, Items)
	;
		{ Items = [Item0 | Items1] },
		check_for_clauses_in_interface(Items0, Items1)
	).

%-----------------------------------------------------------------------------%

:- pred read_mod(string, string, string, item_list, module_error,
		io__state, io__state).
:- mode read_mod(in, in, in, out, out, di, uo) is det.

read_mod(ModuleName, Extension, Descr, Items, Error) -->
	{ dir__basename(ModuleName, Module) },
	{ string__append(ModuleName, Extension, FileName) },
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(VeryVerbose, "% "),
	maybe_write_string(VeryVerbose, Descr),
	maybe_write_string(VeryVerbose, " `"),
	maybe_write_string(VeryVerbose, FileName),
	maybe_write_string(VeryVerbose, "'... "),
	maybe_flush_output(VeryVerbose),
	prog_io__read_module(FileName, Module, Error, Messages, Items),
	( { Error = fatal } ->
		maybe_write_string(VeryVerbose, "fatal error(s).\n"),
		io__set_exit_status(1)
	; { Error = yes } ->
		maybe_write_string(VeryVerbose, "parse error(s).\n"),
		io__set_exit_status(1)
	;
		maybe_write_string(VeryVerbose, "successful parse.\n")
	),
	prog_out__write_messages(Messages).

:- pred read_mod_ignore_errors(string, string, string, item_list, module_error,
		io__state, io__state).
:- mode read_mod_ignore_errors(in, in, in, out, out, di, uo) is det.

read_mod_ignore_errors(ModuleName, Extension, Descr, Items, Error) -->
	{ dir__basename(ModuleName, Module) },
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(VeryVerbose, "% "),
	maybe_write_string(VeryVerbose, Descr),
	maybe_write_string(VeryVerbose, " `"),
	maybe_write_string(VeryVerbose, Module),
	maybe_write_string(VeryVerbose, "'... "),
	maybe_flush_output(VeryVerbose),
	{ string__append(ModuleName, Extension, FileName) },
	prog_io__read_module(FileName, Module, Error, _Messages, Items),
	maybe_write_string(VeryVerbose, "done.\n").

:- pred read_mod_short_interface(string, string, item_list, module_error,
				io__state, io__state).
:- mode read_mod_short_interface(in, in, out, out, di, uo) is det.

read_mod_short_interface(Module, Descr, Items, Error) -->
	read_mod(Module, ".int2", Descr, Items, Error).

:- pred read_mod_interface(string, string, item_list, module_error,
				io__state, io__state).
:- mode read_mod_interface(in, in, out, out, di, uo) is det.

read_mod_interface(Module, Descr, Items, Error) -->
	read_mod(Module, ".int", Descr, Items, Error).

%-----------------------------------------------------------------------------%

:- type (module) --->
	module(
		string,		% The primary module name
		list(string),	% The list of modules it directly imports
		list(string),	% The list of modules it indirectly imports
		item_list,	% The contents of the module and its imports
		bool		% Whether an error has been encountered
	).

:- pred process_module_interfaces(list(string), list(string), module, module,
				io__state, io__state).
:- mode process_module_interfaces(in, in, in, out, di, uo) is det.

process_module_interfaces([], IndirectImports, Module0, Module) -->
	process_module_short_interfaces(IndirectImports, Module0, Module).
	
process_module_interfaces([Import | Imports], IndirectImports0, Module0, Module)
		-->
	{ Module0 = module(ModuleName, DirectImports0, _, Items0, Error0) },
	(
		{ Import = ModuleName }
	->
		globals__io_lookup_string_option(builtin_module, BuiltinModule),
		( { ModuleName = BuiltinModule } ->
			[]
		;
			{ term__context_init(ModuleName, 1, Context) },
			prog_out__write_context(Context),
			io__write_string("Warning: module imports itself!\n")
		),
		process_module_interfaces(Imports, IndirectImports0,
					Module0, Module)
	;
		{ list__member(Import, DirectImports0) }
	->
		process_module_interfaces(Imports, IndirectImports0,
					Module0, Module)
	;
		io__gc_call(
			read_mod_interface(Import,
				"Reading interface for module", Items1, Error1)
		),
		{ ( Error1 \= no ->
			Error2 = yes
		;
			Error2 = Error0
		) },

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		{ get_dependencies(Items1, IndirectImports1) },
		( { Error1 = fatal } ->
			{ DirectImports1 = DirectImports0 }
		;
			{ DirectImports1 = [Import | DirectImports0] }
		),
		{ list__append(IndirectImports0, IndirectImports1,
			IndirectImports2) },
		{ list__append(Items0, Items1, Items2) },
		{ Module1 = module(ModuleName, DirectImports1, [],
					Items2, Error2) },
		process_module_interfaces(Imports, IndirectImports2,
				Module1, Module)
	).

%-----------------------------------------------------------------------------%

:- pred process_module_short_interfaces(list(string), module, module,
					io__state, io__state).
:- mode process_module_short_interfaces(in, in, out, di, uo)
	is det.

process_module_short_interfaces([], Module, Module) --> [].
	
process_module_short_interfaces([Import | Imports], Module0, Module) -->
	{ Module0 = module(ModuleName, DirectImports, IndirectImports0,
			Items0, Error0) },
	(
		% check if the imported module has already been imported
		{ Import = ModuleName
		; list__member(Import, DirectImports)
		; list__member(Import, IndirectImports0)
		}
	->
		process_module_short_interfaces(Imports, Module0, Module)
	;
		io__gc_call(
			read_mod_short_interface(Import,
				"Reading short interface for module",
					Items1, Error1)
		),
		{ ( Error1 \= no ->
			Error2 = yes
		;
			Error2 = Error0
		) },

		globals__io_lookup_bool_option(statistics, Statistics),
		maybe_report_stats(Statistics),

		{ get_dependencies(Items1, Imports1) },
		{ list__append(Imports, Imports1, Imports2) },
		{ list__append(Items0, Items1, Items2) },
		{ IndirectImports1 = [Import | IndirectImports0] },
		{ Module1 = module(ModuleName, DirectImports, IndirectImports1,
					Items2, Error2) },
		process_module_short_interfaces(Imports2, Module1, Module)
	).

%-----------------------------------------------------------------------------%

	% Given a module (well, a list of items),
	% determine all the modules that it depends upon
	% (both interface dependencies and also implementation dependencies).

:- pred get_dependencies(item_list, list(string)).
:- mode get_dependencies(in, out) is det.

get_dependencies(Items, Deps) :-
	get_dependencies_2(Items, [], Deps).

:- pred get_dependencies_2(item_list, list(string), list(string)).
:- mode get_dependencies_2(in, in, out) is det.

get_dependencies_2([], Deps, Deps).
get_dependencies_2([Item - _Context | Items], Deps0, Deps) :-
	( Item = module_defn(_VarSet, import(module(Modules))) ->
		list__append(Deps0, Modules, Deps1)
	;
		Deps1 = Deps0
	),
	get_dependencies_2(Items, Deps1, Deps).

%-----------------------------------------------------------------------------%

	% Given a module (well, a list of items), extract the interface
	% part of that module, i.e. all the items between `:- interface'
	% and `:- implementation'.

:- pred get_interface(item_list, item_list).
:- mode get_interface(in, out) is det.

get_interface(Items0, Items) :-
	get_interface_2(Items0, no, [], RevItems),
	list__reverse(RevItems, Items).

:- pred get_interface_2(item_list, bool, item_list, item_list).
:- mode get_interface_2(in, in, in, out) is det.

get_interface_2([], _, Items, Items).
get_interface_2([Item - Context | Rest], InInterface0, Items0, Items) :-
	( Item = module_defn(_, interface) ->
		Items1 = Items0,
		InInterface1 = yes
	; Item = module_defn(_, implementation) ->
		Items1 = Items0,
		InInterface1 = no
	;
		( InInterface0 = yes ->
			Items1 = [Item - Context | Items0]
		;
			Items1 = Items0
		),
		InInterface1 = InInterface0
	),
	get_interface_2(Rest, InInterface1, Items1, Items).

	% Given a module (well, a list of items), extract the short interface
	% part of that module, i.e. the exported type/inst/mode declarations,
	% but not the exported pred or constructor declarations.

:- pred get_short_interface(item_list, item_list).
:- mode get_short_interface(in, out) is det.

get_short_interface(Items0, Items) :-
	get_short_interface_2(Items0, [], RevItems),
	list__reverse(RevItems, Items).

:- pred get_short_interface_2(item_list, item_list, item_list).
:- mode get_short_interface_2(in, in, out) is det.

get_short_interface_2([], Items, Items).
get_short_interface_2([ItemAndContext | Rest], Items0, Items) :-
	ItemAndContext = Item0 - Context,
	( make_abstract_type_defn(Item0, Item1) ->
		Items1 = [Item1 - Context | Items0]
	; include_in_short_interface(Item0) ->
		Items1 = [ItemAndContext | Items0]
	;
		Items1 = Items0
	),
	get_short_interface_2(Rest, Items1, Items).

:- pred include_in_short_interface(item).
:- mode include_in_short_interface(in) is semidet.

include_in_short_interface(type_defn(_, _, _)).
include_in_short_interface(inst_defn(_, _, _)).
include_in_short_interface(mode_defn(_, _, _)).
include_in_short_interface(module_defn(_, _)).

:- pred make_abstract_type_defn(item, item).
:- mode make_abstract_type_defn(in, out) is semidet.

make_abstract_type_defn(type_defn(VarSet, du_type(Name, Args, _Ctors), Cond),
			type_defn(VarSet, abstract_type(Name, Args), Cond)).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Given a fully expanded module (i.e. a module name and a list
	% of all the items in the module and any of its imports),
	% compile it.

:- pred mercury_compile(module, io__state, io__state).
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
	{ Module = module(ModuleName, _, _, _, _) },
	mercury_compile__pre_hlds_pass(Module, HLDS1, Proceed1),
	mercury_compile__semantic_pass(HLDS1, HLDS9, Proceed1, Proceed2),
	(
		{ Proceed2 = yes },
		mercury_compile__middle_pass(HLDS9, HLDS11, Proceed3),
		globals__io_lookup_bool_option(generate_code, GenerateCode),
		{ bool__and(GenerateCode, Proceed3, Proceed4) },
		(
			{ Proceed4 = yes },
			mercury_compile__backend_pass(HLDS11, HLDS16, LLDS2),
			mercury_compile__output_pass(HLDS16, LLDS2, ModuleName)
		;
			{ Proceed4 = no }
		)
	;
		{ Proceed2 = no }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__pre_hlds_pass(module, module_info, bool,
	io__state, io__state).
:- mode mercury_compile__pre_hlds_pass(in, out, out, di, uo) is det.

mercury_compile__pre_hlds_pass(module(Module, ShortDeps, LongDeps, Items0, _),
		HLDS1, Proceed) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	write_dependency_file(Module, ShortDeps, LongDeps),

	negation__transform(Items0, Items1),
	mercury_compile__expand_equiv_types(Items1, Items),
	maybe_report_stats(Statistics),

	mercury_compile__make_hlds(Module, Items, HLDS0, FoundError),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS0, "0", "initial"),

	( { FoundError = yes } ->
		{ module_info_incr_errors(HLDS0, HLDS1) },
		{ Proceed = no }
	;	
		{ HLDS1 = HLDS0 },
		{ Proceed = yes }
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

:- pred mercury_compile__semantic_pass(module_info, module_info,
	bool, bool, io__state, io__state).
:- mode mercury_compile__semantic_pass(di, uo, in, out, di, uo) is det.

mercury_compile__semantic_pass(HLDS1, HLDS9, Proceed0, Proceed) -->
	globals__io_lookup_bool_option(trad_passes, TradPasses),
	(
		{ TradPasses = no },
		mercury_compile__semantic_pass_by_phases(HLDS1, HLDS9,
			Proceed0, Proceed)
	;
		{ TradPasses = yes },
		mercury_compile__semantic_pass_by_preds(HLDS1, HLDS9,
			Proceed0, Proceed)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__semantic_pass_by_phases(module_info, module_info,
	bool, bool, io__state, io__state).
:- mode mercury_compile__semantic_pass_by_phases(di, uo, in, out, di, uo)
	is det.

mercury_compile__semantic_pass_by_phases(HLDS1, HLDS9, Proceed0, Proceed) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	mercury_compile__typecheck(HLDS1, HLDS2, FoundTypeError),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS2, "2", "typecheck"),
	{ bool__not(FoundTypeError, Proceed1) },

	globals__io_lookup_bool_option(modecheck, DoModeCheck),
	( { DoModeCheck = yes } ->
		mercury_compile__modecheck(HLDS2, HLDS3, FoundModeError),
		maybe_report_stats(Statistics),
		mercury_compile__maybe_dump_hlds(HLDS3, "3", "modecheck"),
		{ bool__not(FoundModeError, Proceed2) },

		globals__io_lookup_bool_option(make_call_graph, MakeCallGraph),
		( { MakeCallGraph = yes } ->
			mercury_compile__make_call_graph(HLDS3),
			maybe_report_stats(Statistics)
		;
			[]
		),

		{ bool__and_list([Proceed0, Proceed1, Proceed2], Proceed) },
		( { Proceed = yes } ->
			mercury_compile__maybe_polymorphism(HLDS3, HLDS4),
			maybe_report_stats(Statistics),
			mercury_compile__maybe_dump_hlds(HLDS4, "4", "polymorphism"),

			mercury_compile__detect_switches(HLDS4, HLDS5),
			maybe_report_stats(Statistics),
			mercury_compile__maybe_dump_hlds(HLDS5, "5", "switch_detect"),

			mercury_compile__detect_cse(HLDS5, HLDS6),
			maybe_report_stats(Statistics),
			mercury_compile__maybe_dump_hlds(HLDS6, "6", "cse"),

			mercury_compile__maybe_detect_common_struct(HLDS6, HLDS7),
			maybe_report_stats(Statistics),
			mercury_compile__maybe_dump_hlds(HLDS7, "7", "common"),

			mercury_compile__maybe_do_inlining(HLDS7, HLDS8),
			maybe_report_stats(Statistics),
			mercury_compile__maybe_dump_hlds(HLDS8, "8", "inlining"),

			mercury_compile__maybe_migrate_followcode(HLDS8, HLDS9),
			maybe_report_stats(Statistics),
			mercury_compile__maybe_dump_hlds(HLDS9, "9", "followcode")
		;
			{ HLDS9 = HLDS3 }
		)
	;
		{ HLDS9 = HLDS2 },
		{ Proceed = no }
	),

#if NU_PROLOG
	{ putprop(mc, mc, HLDS9 - Proceed), fail }.
mercury_compile__semantic_pass_by_phases(_, HLDS9, _, Proceed) -->
	{ getprop(mc, mc, HLDS9 - Proceed, Ref), erase(Ref) },
#endif

	{ true }.

:- pred mercury_compile__typecheck(module_info, module_info, bool,
	io__state, io__state).
:- mode mercury_compile__typecheck(in, out, out, di, uo) is det.

mercury_compile__typecheck(HLDS0, HLDS, FoundTypeError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Type-checking...\n"),
	typecheck(HLDS0, HLDS, FoundTypeError),
	( { FoundTypeError = yes } ->
		maybe_write_string(Verbose,
			"% Program contains type error(s).\n"),
		io__set_exit_status(1)
	;
		maybe_write_string(Verbose, "% Program is type-correct.\n")
	).

:- pred mercury_compile__modecheck(module_info, module_info, bool,
	io__state, io__state).
:- mode mercury_compile__modecheck(in, out, out, di, uo) is det.

mercury_compile__modecheck(HLDS0, HLDS, FoundModeError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Mode-checking...\n"),
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

:- pred mercury_compile__make_call_graph(module_info,
	io__state, io__state).
:- mode mercury_compile__make_call_graph(in, di, uo) is det.

mercury_compile__make_call_graph(ModuleInfo) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Building call graph..."),
	maybe_flush_output(Verbose),
	{ call_graph__build_call_graph(ModuleInfo, CallGraph) },
	maybe_write_string(Verbose, "done.\n"),
	{ module_info_name(ModuleInfo, Name) },
	{ string__append(Name, ".call_graph", WholeName) },
	io__tell(WholeName, Res),
	( { Res = ok } ->
		call_graph__write_call_graph(CallGraph, ModuleInfo),
		io__told
	;
		report_error("unable to write call graph")
	).
	
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

:- pred mercury_compile__maybe_detect_common_struct(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_detect_common_struct(in, out, di, uo) is det.

mercury_compile__maybe_detect_common_struct(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(common_struct, CommonStruct),
	( { CommonStruct = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Detecting common structures..."),
		maybe_flush_output(Verbose),
		common__optimise_common_subexpressions(HLDS0, HLDS),
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS0 = HLDS }
	).

:- pred mercury_compile__maybe_do_inlining(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_do_inlining(in, out, di, uo) is det.

mercury_compile__maybe_do_inlining(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(inlining, Inlining),
	(
		{ Inlining = yes }
	->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Inlining..."),
		maybe_flush_output(Verbose),
		{ inlining(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS = HLDS0 }
	).

:- pred mercury_compile__maybe_migrate_followcode(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__maybe_migrate_followcode(in, out, di, uo) is det.

mercury_compile__maybe_migrate_followcode(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(follow_code, FollowCode),
	( { FollowCode = yes } ->
		globals__io_lookup_bool_option(verbose, Verbose),
		maybe_write_string(Verbose, "% Migrating followcode..."),
		maybe_flush_output(Verbose),
		{ move_follow_code(HLDS0, HLDS) },
		maybe_write_string(Verbose, " done.\n")
	;
		{ HLDS0 = HLDS }
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__semantic_pass_by_preds(module_info, module_info,
	bool, bool, io__state, io__state).
:- mode mercury_compile__semantic_pass_by_preds(di, uo, in, out, di, uo) is det.

mercury_compile__semantic_pass_by_preds(HLDS1, HLDS9, Proceed0, Proceed) -->
	mercury_compile__semantic_pass_by_phases(HLDS1, HLDS9,
		Proceed0, Proceed).

%-----------------------------------------------------------------------------%

% semantic_analyze_predicate(PredId, ModuleInfo0, ModuleInfo) -->
% 	{ module_info_preds(ModuleInfo0, Preds0) },
% 	{ map__lookup(Preds0, PredId, PredInfo0) },
% 	( { pred_info_is_imported(PredInfo0) } ->
% 		{ ModuleInfo = ModuleInfo0 }
% 	;
% 		semantic_analyze_predicate_1(PredId, ModuleInfo0, ModuleInfo)
% 	).
% 
% semantic_analyze_predicate_1(PredId, ModuleInfo0, ModuleInfo) -->
% 	write_progress_message("% Type-checking predicate ", PredId,
% 		ModuleInfo0),
% 	typecheck_pred_type(PredId, PredInfo0, ModuleInfo0, MaybePredInfo1),
% 	(
% 		{ MaybePredInfo1 = no },
% 		{ module_info_remove_predid(ModuleInfo0, PredId, ModuleInfo) }
% 	;
% 		{ MaybePredInfo1 = yes(PredInfo1) },
% 		{ map__set(Preds0, PredId, PredInfo1, Preds1) },
% 		{ module_info_set_preds(ModuleInfo0, Preds, ModuleInfo1) },
% 		semantic_analyze_predicate_2(PredId, PredInfo1, ModuleInfo1,
% 			ModuleInfo)
% 	).
% 
% semantic_analyze_predicate_2(PredId, PredInfo0, ModuleInfo0, ModuleInfo) -->
% 	write_progress_message("% Mode-checking predicate ", PredId,
% 		ModuleInfo0),
% 	modecheck_pred_mode(PredId, PredInfo0, ModuleInfo0, ModuleInfo1, Errs),
% 	( { Errs \= 0 } ->
% 		{ module_info_num_errors(ModuleInfo1, NumErrors1) },
% 		{ NumErrors is NumErrors1 + Errs },
% 		{ module_info_set_num_errors(ModuleInfo1, NumErrors,
% 			ModuleInfo2) },
% 		{ module_info_remove_predid(ModuleInfo2, PredId,
% 			ModuleInfo) }
% 	;
% 		semantic_analyze_predicate_3(PredId, ModuleInfo1, ModuleInfo)
% 	).
% 
% semantic_analyze_predicate_3(PredId, ModuleInfo1, ModuleInfo) -->

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__middle_pass(module_info, module_info,
	bool, io__state, io__state).
:- mode mercury_compile__middle_pass(di, uo, out, di, uo) is det.

mercury_compile__middle_pass(HLDS9, HLDS11, Proceed) -->
	globals__io_lookup_bool_option(trad_passes, TradPasses),
	globals__io_lookup_bool_option(generate_code, GenerateCode),
	(
		{ TradPasses = no },
		mercury_compile__middle_pass_by_phases(HLDS9, HLDS11,
			GenerateCode, Proceed)
	;
		{ TradPasses = yes },
		mercury_compile__middle_pass_by_preds(HLDS9, HLDS11,
			GenerateCode, Proceed)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__middle_pass_by_phases(module_info, module_info,
	bool, bool, io__state, io__state).
:- mode mercury_compile__middle_pass_by_phases(di, uo, in, out, di, uo) is det.

mercury_compile__middle_pass_by_phases(HLDS9, HLDS11, GenerateCode, Proceed) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	mercury_compile__check_determinism(HLDS9, HLDS10, FoundError),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS10, "10", "determinism"),
	(
		{ GenerateCode = yes },
		{ FoundError = no }
	->
		mercury_compile__map_args_to_regs(HLDS10, HLDS11),
		maybe_report_stats(Statistics),
		mercury_compile__maybe_dump_hlds(HLDS11, "11", "args_to_regs")
	;
		{ HLDS11 = HLDS10 }
	),
	{ bool__not(FoundError, Proceed) }.

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

:- pred mercury_compile__map_args_to_regs(module_info, module_info,
	io__state, io__state).
:- mode mercury_compile__map_args_to_regs(in, out, di, uo) is det.

mercury_compile__map_args_to_regs(HLDS0, HLDS) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Mapping args to regs..."),
	maybe_flush_output(Verbose),
	{ generate_arg_info(HLDS0, HLDS) },
	maybe_write_string(Verbose, " done.\n").

%-----------------------------------------------------------------------------%

:- pred mercury_compile__middle_pass_by_preds(module_info, module_info,
	bool, bool, io__state, io__state).
:- mode mercury_compile__middle_pass_by_preds(di, uo, in, out, di, uo) is det.

mercury_compile__middle_pass_by_preds(HLDS9, HLDS11, GenerateCode, Proceed) -->
	mercury_compile__middle_pass_by_phases(HLDS9, HLDS11, GenerateCode,
		Proceed).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass(module_info, module_info,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__backend_pass(di, uo, out, di, uo) is det.

mercury_compile__backend_pass(HLDS11, HLDS16, LLDS2) -->
	globals__io_lookup_bool_option(trad_passes, TradPasses),
	(
		{ TradPasses = no },
		mercury_compile__backend_pass_by_phases(HLDS11, HLDS16, LLDS2)
	;
		{ TradPasses = yes },
		mercury_compile__backend_pass_by_preds(HLDS11, HLDS16, LLDS2)
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__backend_pass_by_phases(module_info, module_info,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__backend_pass_by_phases(in, out, out, di, uo) is det.

mercury_compile__backend_pass_by_phases(HLDS11, HLDS16, LLDS2) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	mercury_compile__compute_liveness(HLDS11, HLDS12),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS12, "12", "liveness"),

	mercury_compile__maybe_compute_followvars(HLDS12, HLDS13),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS13, "13", "followvars"),

#if NU_PROLOG
	{ putprop(mc, mc, HLDS13), fail }.
mercury_compile__backend_pass_by_phases(_, _, _) -->
	{ getprop(mc, mc, HLDS13, Ref), erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
#endif

	mercury_compile__compute_stack_vars(HLDS13, HLDS14),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS14, "14", "stackvars"),

	mercury_compile__allocate_store_map(HLDS14, HLDS15),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS15, "15", "store_map"),

	mercury_compile__maybe_report_sizes(HLDS15),

#if NU_PROLOG
	{ putprop(mc, mc, HLDS15), fail }.
mercury_compile__backend_pass_by_phases(_, _, _) -->
	{ getprop(mc, mc, HLDS15, Ref), erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
#endif

	mercury_compile__generate_code(HLDS15, HLDS16, LLDS1),
	maybe_report_stats(Statistics),
	mercury_compile__maybe_dump_hlds(HLDS16, "99", "final"),

#if NU_PROLOG
	{ putprop(mc, mc, HLDS16 - LLDS1), fail }.
mercury_compile__backend_pass_by_phases(_, HLDS16, LLDS2) -->
	{ getprop(mc, mc, HLDS16 - LLDS1, Ref), erase(Ref) },
	globals__io_lookup_bool_option(statistics, Statistics),
#endif

	mercury_compile__maybe_do_optimize(LLDS1, LLDS2),
	maybe_report_stats(Statistics).

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

:- pred mercury_compile__backend_pass_by_preds(module_info, module_info,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__backend_pass_by_preds(di, uo, out, di, uo) is det.

mercury_compile__backend_pass_by_preds(HLDS10, HLDS16, LLDS2) -->
	{ module_info_predids(HLDS10, PredIds) },
	mercury_compile__backend_pass_by_preds_2(PredIds, HLDS10, HLDS16, LLDS2).

:- pred mercury_compile__backend_pass_by_preds_2(list(pred_id),
	module_info, module_info, list(c_procedure), io__state, io__state).
:- mode mercury_compile__backend_pass_by_preds_2(in, di, uo, out, di, uo)
	is det.

mercury_compile__backend_pass_by_preds_2([], ModuleInfo, ModuleInfo, [])
	--> [].
mercury_compile__backend_pass_by_preds_2([PredId | PredIds], ModuleInfo0,
		ModuleInfo, Code) -->
	{ module_info_preds(ModuleInfo0, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },
	( { pred_info_is_imported(PredInfo) } ->
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

		{ pred_info_procids(PredInfo, ProcIds) },
		{ module_info_shapes(ModuleInfo0, Shapes0) },
		mercury_compile__backend_pass_by_preds_3(ProcIds, PredId,
			PredInfo, ModuleInfo0, Shapes0, Shapes, Code1),
		{ module_info_set_shapes(ModuleInfo0, Shapes, ModuleInfo1) },

		( { Verbose = yes } ->
			io__write_string("% done\n"),
			io__flush_output
		;
			[]
		)
	),
	mercury_compile__backend_pass_by_preds_2(PredIds,
		ModuleInfo1, ModuleInfo, Code2),
	{ list__append(Code1, Code2, Code) }.

:- pred mercury_compile__backend_pass_by_preds_3(list(proc_id), pred_id,
	pred_info, module_info, shape_table, shape_table,
	list(c_procedure), io__state, io__state).
:- mode mercury_compile__backend_pass_by_preds_3(in, in, in, in, di, uo, out,
	di, uo) is det.

mercury_compile__backend_pass_by_preds_3([], _, _, _, Shapes, Shapes, [])
		--> [].
mercury_compile__backend_pass_by_preds_3([ProcId | ProcIds], PredId, PredInfo,
		ModuleInfo, Shapes0, Shapes, [Proc | Procs]) -->
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	mercury_compile__backend_pass_by_preds_4(ProcInfo, ProcId, PredId,
		ModuleInfo, Shapes0, Shapes1, Proc),
	mercury_compile__backend_pass_by_preds_3(ProcIds, PredId, PredInfo,
		ModuleInfo, Shapes1, Shapes, Procs).

:- pred mercury_compile__backend_pass_by_preds_4(proc_info, proc_id, pred_id,
	module_info, shape_table, shape_table, c_procedure,
	io__state, io__state).
:- mode mercury_compile__backend_pass_by_preds_4(in, in, in, in, di, uo, out,
	di, uo) is det.

mercury_compile__backend_pass_by_preds_4(ProcInfo0, ProcId, PredId, ModuleInfo,
		Shapes0, Shapes, Proc) -->
	{ detect_liveness_proc(ProcInfo0, ModuleInfo, ProcInfo1) },
	globals__io_lookup_bool_option(follow_vars, FollowVars),
	( { FollowVars = yes } ->
		{ find_follow_vars_in_proc(ProcInfo1, ModuleInfo, ProcInfo2) }
	;
		{ ProcInfo2 = ProcInfo1 }
	),
	{ detect_live_vars_in_proc(ProcInfo2, ModuleInfo, ProcInfo3) },
	{ store_alloc_in_proc(ProcInfo3, ModuleInfo, ProcInfo4) },
	generate_proc_code(ProcInfo4, ProcId, PredId, ModuleInfo,
		Shapes0, Shapes, Proc0),
	globals__io_lookup_bool_option(optimize, Optimize),
	( { Optimize = yes } ->
		optimize__proc(Proc0, Proc)
	;
		{ Proc = Proc0 }
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__output_pass(module_info, list(c_procedure), string,
	io__state, io__state).
:- mode mercury_compile__output_pass(in, in, in, di, uo) is det.

mercury_compile__output_pass(HLDS16, LLDS2, ModuleName) -->
	globals__io_lookup_bool_option(statistics, Statistics),

	mercury_compile__chunk_llds(HLDS16, LLDS2, LLDS3),
	mercury_compile__output_llds(ModuleName, LLDS3),
	maybe_report_stats(Statistics),

	mercury_compile__maybe_find_abstr_exports(HLDS16, HLDS17), 
	% maybe_report_stats(Statistics),

	{ module_info_shape_info(HLDS17, Shape_Info) },
	mercury_compile__maybe_write_gc(ModuleName, Shape_Info, LLDS3),
	% maybe_report_stats(Statistics),

	globals__io_lookup_bool_option(compile, Compile),
	( { Compile = yes } ->
		{ string__append(ModuleName, ".c", C_File) },
		mercury_compile__c_to_obj(C_File, _Compile_went_OK)
	;
		[]
	).

	% Split the code up into bite-size chunks for the C compiler.

:- pred mercury_compile__chunk_llds(module_info, list(c_procedure), c_file,
	io__state, io__state).
:- mode mercury_compile__chunk_llds(in, di, uo, di, uo) is det.

mercury_compile__chunk_llds(HLDS, Procedures, c_file(Name, ModuleList)) -->
	{ module_info_name(HLDS, Name) },
	{ string__append(Name, "_module", ModName) },
	globals__io_lookup_int_option(procs_per_c_function, ProcsPerFunc),
	( { ProcsPerFunc = 0 } ->
		% ProcsPerFunc = 0 really means infinity -
		% we store all the procs in a single function.
		{ ModuleList = [c_module(ModName, Procedures)] }
	;
		{ list__chunk(Procedures, ProcsPerFunc, ChunkList) },
		{ mercury_compile__combine_chunks(ChunkList, ModName,
			ModuleList) }
	).

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
	globals__io_lookup_string_option(gc, Garbage),
	( { Garbage = "accurate" } ->
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

:- pred mercury_compile__c_to_obj(string, bool, io__state, io__state).
:- mode mercury_compile__c_to_obj(in, out, di, uo) is det.

mercury_compile__c_to_obj(C_File, Succeeded) -->
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
	globals__io_lookup_bool_option(gcc_global_registers, GCC_Regs),
	{ GCC_Regs = yes ->
		RegOpt = "-DUSE_GCC_GLOBAL_REGISTERS "
	;
		RegOpt = ""
	},
	globals__io_lookup_bool_option(gcc_non_local_gotos, GCC_Gotos),
	{ GCC_Gotos = yes ->
		GotoOpt = "-DUSE_GCC_NONLOCAL_GOTOS "
	;
		GotoOpt = ""
	},
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
	globals__io_lookup_bool_option(c_optimize, C_optimize),
	{ C_optimize = yes, Debug = no ->
		OptimizeOpt = "-O2 -fomit-frame-pointer "
	;
		OptimizeOpt = ""
	},
	{ string__append_list([CC, " ", InclOpt, RegOpt, GotoOpt, AsmOpt,
		GC_Opt, TagsOpt, NumTagBitsOpt, DebugOpt, OptimizeOpt, CFLAGS,
		" -c ", C_File], Command) },
	mercury_compile__invoke_system_command(Command, Succeeded),
	( { Succeeded = no } ->
		report_error("problem compiling C file")
	;
		[]
	).

%-----------------------------------------------------------------------------%

:- pred mercury_compile__link_module_list(list(string), io__state, io__state).
:- mode mercury_compile__link_module_list(in, di, uo) is det.

mercury_compile__link_module_list(Modules) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	globals__io_lookup_bool_option(statistics, Statistics),
	( { Modules = [Module | _] } ->
	    % create the initialization C file
	    maybe_write_string(Verbose, "% Creating initialization file...\n"),
	    { string__append(Module, "_init.c", C_Init_File) },
	    { join_string_list(Modules, ".c ", ["> ", C_Init_File],
				MkInitCmd0) },
	    { string__append_list(["c2init " | MkInitCmd0], MkInitCmd) },
	    mercury_compile__invoke_system_command(MkInitCmd, MkInitOK),
	    maybe_report_stats(Statistics),
	    ( { MkInitOK = no } ->
		report_error("creation of init file failed")
	    ;
		% compile it
	        maybe_write_string(Verbose, "% Compiling initialization file...\n"),
		mercury_compile__c_to_obj(C_Init_File, CompileOK),
	        maybe_report_stats(Statistics),
		( { CompileOK = no } ->
		    report_error("compilation of init file failed")
		;
	            maybe_write_string(Verbose, "% Linking...\n"),
		    { join_string_list(Modules, ".o ", [], ObjectList) },
		    globals__io_lookup_string_option(grade, Grade),
		    { string__append_list(["ml -s ", Grade, " -o ", Module, " ",
				Module, "_init.o " | ObjectList], LinkCmd) },
		    mercury_compile__invoke_system_command(LinkCmd, LinkCmdOK),
		    maybe_report_stats(Statistics),
		    ( { LinkCmdOK = no } ->
			report_error("link failed")
		    ;
			[]
		    )
		)
	    )
	;
	    { error("link_module_list: no modules") }
	).

:- pred join_string_list(list(string), string, list(string), list(string)).
:- mode join_string_list(in, in, in, out) is det.

join_string_list([], _Separator, Terminator, Terminator).
join_string_list([String0 | Strings0], Separator, Terminator,
			[String0, Separator | Strings]) :-
	join_string_list(Strings0, Separator, Terminator, Strings).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred mercury_compile__invoke_system_command(string, bool,
	io__state, io__state).
:- mode mercury_compile__invoke_system_command(in, out, di, uo) is det.

mercury_compile__invoke_system_command(Command, Succeeded) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Invoking system command `"),
	maybe_write_string(Verbose, Command),
	maybe_write_string(Verbose, "'...\n"),
	io__call_system(Command, Result),
	( { Result = ok(0) } ->
		maybe_write_string(Verbose, "% done\n"),
		{ Succeeded = yes }
	; { Result = ok(_) } ->
		report_error("system command returned non-zero exit status"),
		{ Succeeded = no }
	;	
		report_error("unable to invoke system command"),
		{ Succeeded = no }
	).

:- pred mercury_compile__maybe_report_sizes(module_info, io__state, io__state).
:- mode mercury_compile__maybe_report_sizes(in, di, uo) is det.

mercury_compile__maybe_report_sizes(HLDS) -->
	globals__io_lookup_bool_option(statistics, Statistics),
	( { Statistics = yes } ->
		mercury_compile__report_sizes(HLDS)
	;
		[]
	).

:- pred mercury_compile__report_sizes(module_info, io__state, io__state).
:- mode mercury_compile__report_sizes(in, di, uo) is det.

mercury_compile__report_sizes(ModuleInfo) -->
	{ module_info_preds(ModuleInfo, Preds) },
	mercury_compile__tree_stats("Pred table", Preds),
	{ module_info_types(ModuleInfo, Types) },
	mercury_compile__tree_stats("Type table", Types),
	{ module_info_ctors(ModuleInfo, Ctors) },
	mercury_compile__tree_stats("Constructor table", Ctors).

:- pred mercury_compile__tree_stats(string, map(_K, _V), io__state, io__state).
:- mode mercury_compile__tree_stats(in, in, di, uo) is det.

mercury_compile__tree_stats(Description, Tree) -->
	{ tree234__count(Tree, Count) },
	%{ tree234__depth(Tree, Depth) },
	io__write_string(Description),
	io__write_string(": count = "),
	io__write_int(Count),
	%io__write_string(", depth = "),
	%io__write_int(Depth),
	io__write_string("\n").

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
		{ string__append_list( ["can't open file `",
			DumpFile, "' for output"], ErrorMessage) },
		report_error(ErrorMessage)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
