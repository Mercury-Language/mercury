%-----------------------------------------------------------------------------%

% file: modules.m
% main author: fjh

% This module contains all the code for handling module imports and exports,
% for computing module dependencies, and for generate makefile fragments to
% record those dependencies.

%-----------------------------------------------------------------------------%

:- module modules.
:- interface.
:- import_module string, list, io.
:- import_module prog_io.

	% read_mod(ModuleName, Extension, Descr, Items, Error):
	%	Given a module name and a file extension (e.g. `.m',
	%	`.int', or `int2'), read in the list of items in that file.
	%
:- pred read_mod(string, string, string, item_list, module_error,
		io__state, io__state).
:- mode read_mod(in, in, in, out, out, di, uo) is det.

	% make_interface(ModuleName, Items):
	%	Given a module name and the list of items in that module,
	%	output the long (`.int') and short (`.int2') interface files
	%	for the module.
	%
:- pred make_interface(string, item_list, io__state, io__state).
:- mode make_interface(in, in, di, uo) is det.

	% grab_imported_modules(ModuleName, Items, Module, Error)
	%	Given a module name and the list of items in that module,
	%	read in the full interface files for all the imported modules,
	%	and the short interface files for all the indirectly imported
	%	modules, and return a `module_imports' structure containing the
	%	relevant information.
	%
:- type module_imports --->
	module_imports(
		string,		% The primary module name
		list(string),	% The list of modules it directly imports
		list(string),	% The list of modules it indirectly imports
		item_list,	% The contents of the module and its imports
		module_error	% Whether an error has been encountered
	).

:- pred grab_imported_modules(string, item_list, module_imports, module_error,
				io__state, io__state).
:- mode grab_imported_modules(in, in, out, out, di, uo) is det.

	% write_dependency_file(ModuleName, LongDeps, ShortDeps):
	%	Write out the per-module makefile dependencies (`.d') file
	%	for a module `ModuleName' which depends directly on the
	% 	modules `LongDeps' and indirectly on the modules `ShortDeps'.
	%
:- pred write_dependency_file(string, list(string), list(string),
				io__state, io__state).
:- mode write_dependency_file(in, in, in, di, uo) is det.

	% generate_dependencies(ModuleName):
	%	Generate the per-program makefile dependencies (`.dep') file
	%	for a program whose top-level module is `ModuleName'.
	%	This involes first transitively reading in all imported
	%	modules.
	%
:- pred generate_dependencies(string, io__state, io__state).
:- mode generate_dependencies(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, set, map, term, varset, dir, std_util, library.
:- import_module globals, options, prog_out, mercury_to_mercury.
:- import_module mercury_compile.

make_interface(ModuleName, Items0) -->
	{ get_interface(Items0, InterfaceItems0) },
	check_for_clauses_in_interface(InterfaceItems0, InterfaceItems),
	write_interface_file(ModuleName, ".int", InterfaceItems),
	{ get_short_interface(InterfaceItems, ShortInterfaceItems) },
	write_interface_file(ModuleName, ".int2", ShortInterfaceItems),
	check_for_no_exports(ModuleName, InterfaceItems, ShortInterfaceItems),
	touch_interface_datestamp(ModuleName).

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

:- pred check_for_no_exports(string, item_list, item_list,
					io__state, io__state).
:- mode check_for_no_exports(in, in, in, di, uo) is det.

check_for_no_exports(ModuleName, InterfaceItems, ShortInterfaceItems) -->
	(
		{ InterfaceItems = ShortInterfaceItems }	% ???
	->
		warn_no_exports(ModuleName)
	;
		[]
	).

:- pred warn_no_exports(string, io__state, io__state).
:- mode warn_no_exports(in, di, uo) is det.

warn_no_exports(ModuleName) -->
	globals__io_lookup_bool_option(warn_nothing_exported, ExportWarning),
	( 	
		{ ExportWarning = yes }
	->
		report_warning(ModuleName, 1,
			"Interface does not export anything."),
		globals__io_lookup_bool_option(verbose_errors, VerboseErrors),
		(
			{ VerboseErrors = yes }
		->
			io__stderr_stream(StdErr),
			io__write_strings(StdErr, [ "\t\t",
	"To be useful, a module should export something.\n\t\t",
	"A file should contain at least one declaration other than\n\t\t",
	"`:- import_module' in its interface section(s).\n\t\t",
	"This would normally be a `:- pred', `:- type', `:- inst' or \n\t\t",
	"`:- mode' declaration.\n"
				])
		;
			[]
		)
	;
		[]
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
		report_error("problem updating interface files.")
	;
		[]
	).

%-----------------------------------------------------------------------------%

	% Touch the datestamp file `<Module>.date'.
	% This datestamp is used to record when the interface files
	% were last updated.

:- pred touch_interface_datestamp(string, io__state, io__state).
:- mode touch_interface_datestamp(in, di, uo) is det.

touch_interface_datestamp(ModuleName) -->
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
%-----------------------------------------------------------------------------%

grab_imported_modules(ModuleName, Items0, Module, Error) -->
	get_dependencies(ModuleName, Items0, ImportedModules),

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
	{ Module0 = module_imports(BaseModuleName, [], [], Items1, no) },
	process_module_interfaces([BuiltinModule | ImportedModules], 
		[], Module0, Module),
	{ Module = module_imports(_, _, _, _, Error) }.

%-----------------------------------------------------------------------------%

write_dependency_file(ModuleName, LongDeps0, ShortDeps0) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	{ string__append(ModuleName, ".d", DependencyFileName) },
	maybe_write_string(Verbose, "% Writing auto-dependency file `"),
	maybe_write_string(Verbose, DependencyFileName),
	maybe_write_string(Verbose, "'..."),
	maybe_flush_output(Verbose),
	io__open_output(DependencyFileName, Result),
	( { Result = ok(DepStream) } ->
		{ list__sort_and_remove_dups(LongDeps0, LongDeps) },
		{ list__sort_and_remove_dups(ShortDeps0, ShortDeps) },

		io__write_string(DepStream, ModuleName),
		io__write_string(DepStream, ".c "),
		io__write_string(DepStream, ModuleName),
		io__write_string(DepStream, ".err "),
		io__write_string(DepStream, ModuleName),
		io__write_string(DepStream, ".o : "),
		io__write_string(DepStream, ModuleName),
		io__write_string(DepStream, ".m"),
		write_dependencies_list(LongDeps, ".int", DepStream),
		write_dependencies_list(ShortDeps, ".int2", DepStream),
		io__write_string(DepStream, "\n"),

		io__close_output(DepStream),
		maybe_write_string(Verbose, " done.\n")
	;
		{ string__append_list(["can't open file `", DependencyFileName,
				"' for output."], Message) },
		report_error(Message)
	).

%-----------------------------------------------------------------------------%

generate_dependencies(Module) -->
	{ map__init(DepsMap0) },
	generate_deps_map([Module], DepsMap0, DepsMap),
	{ map__lookup(DepsMap, Module, deps(_, Error, _, _)) },
	( { Error = fatal } ->
	    { string__append_list(["fatal error reading module `",
				Module, "'."], Message) },
	    report_error(Message)
	;
	    { string__append(Module, ".dep", DepFileName) },
	    globals__io_lookup_bool_option(verbose, Verbose),
	    maybe_write_string(Verbose, "% Creating auto-dependency file `"),
	    maybe_write_string(Verbose, DepFileName),
	    maybe_write_string(Verbose, "'...\n"),
	    io__open_output(DepFileName, Result),
	    ( { Result = ok(DepStream) } ->
		generate_dep_file(Module, DepsMap, DepStream),
		io__close_output(DepStream),
		maybe_write_string(Verbose, "% done\n")
	    ;
		{ string__append_list(["can't open file `", DepFileName,
				"' for output."], Message) },
		report_error(Message)
	    )
	).

:- type deps_map == map(string, deps).
:- type deps
	---> deps(
		bool,		% have we processed this module yet?
		module_error,	% if we did, where there any errors?
		list(string),	% interface dependencies
		list(string)	% implementation dependencies
	).

:- pred generate_deps_map(list(string), deps_map, deps_map,
			io__state, io__state).
:- mode generate_deps_map(in, in, out, di, uo) is det.

generate_deps_map([], DepsMap, DepsMap) --> [].
generate_deps_map([Module | Modules], DepsMap0, DepsMap) -->
		% Look up the module's dependencies, and determine whether
		% it has been processed yet.
	lookup_dependencies(Module, DepsMap0, Done, Error, ImplDeps, IntDeps,
				DepsMap1),
		% If the module hadn't been processed yet, compute its
		% transitive dependencies (we already know its primary ones),
		% (1) output a line for this module to the dependency file
		% (if the file exists), (2) add its imports to the list of
		% dependencies we need to generate, and (3) mark it as having
		% been processed.
	( { Done = no } ->
		{ map__set(DepsMap1, Module,
			deps(yes, Error, IntDeps, ImplDeps), DepsMap2) },
		transitive_dependencies(ImplDeps, DepsMap2, SecondaryDeps,
			DepsMap3),
		( { Error \= fatal } ->
			write_dependency_file(Module, ImplDeps, SecondaryDeps)
		;
			[]
		),
		{ list__append(ImplDeps, Modules, Modules2) }
	;
		{ DepsMap3 = DepsMap1 },
		{ Modules2 = Modules }
	),
		% Recursively process the remaining modules
	generate_deps_map(Modules2, DepsMap3, DepsMap).


% write out the `.dep' file

:- pred generate_dep_file(string, deps_map, io__output_stream,
			io__state, io__state).
:- mode generate_dep_file(in, in, in, di, uo) is det.

generate_dep_file(ModuleName, DepsMap, DepStream) -->
	io__write_string(DepStream,
		"# Automatically generated dependencies for module `"),
	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, "'.\n"),
	{ library__version(Version) },
	io__write_string(DepStream,
		"# Generated by the Mercury compiler, version "),
	io__write_string(DepStream, Version),
	io__write_string(DepStream, ".\n\n"),

	{ map__keys(DepsMap, Modules0) },
	{ select_ok_modules(Modules0, DepsMap, Modules) },

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ms = "),
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
	io__write_string(DepStream, ".err2s = "),
	write_dependencies_list(Modules, ".err2", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".dates = "),
	write_dependencies_list(Modules, ".date", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ds = "),
	write_dependencies_list(Modules, ".d", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".ints = "),
	write_dependencies_list(Modules, ".int", DepStream),
	write_dependencies_list(Modules, ".int2", DepStream),
	io__write_string(DepStream, "\n\n"),

	io__write_strings(DepStream, [
		ModuleName, " : $(", ModuleName, ".os) ",
				ModuleName, "_init.o\n",
		"\t$(ML) -s $(GRADE) $(MLFLAGS) -o ", ModuleName, " ",
			ModuleName, "_init.o \\\n",
			"\t$(", ModuleName, ".os) $(MLLIBS)\n\n",

		ModuleName, "_init.c :\n",
		"\t$(C2INIT) $(C2INITFLAGS) $(", ModuleName, ".ms) > ",
			ModuleName, "_init.c\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".nu : $(", ModuleName, ".nos)\n",
		"\t$(MNL) $(MNLFLAGS) -o ", ModuleName, ".nu ",
			"$(", ModuleName, ".nos)\n\n",

		ModuleName, ".nu.debug : $(", ModuleName, ".nos)\n",
		"\t$(MNL) --debug $(MNLFLAGS) -o ", ModuleName, ".nu.debug ",
			"$(", ModuleName, ".nos)\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".sicstus : $(", ModuleName, ".qls)\n",
		"\t$(MSL) $(MSLFLAGS) -o ", ModuleName, ".sicstus ",
			"$(", ModuleName, ".qls)\n\n",

		ModuleName, ".sicstus.debug : $(", ModuleName, ".qls)\n",
			"\t$(MSL) --debug $(MSLFLAGS) -o ", ModuleName,
			".sicstus.debug $(", ModuleName, ".qls)\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".check : $(", ModuleName, ".errs)\n\n",

		ModuleName, ".ints : $(", ModuleName, ".dates)\n\n"
	]),

	io__write_strings(DepStream, [
		"clean: ", ModuleName, ".clean\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".clean :\n",
		"\t-rm -f $(", ModuleName, ".cs) ", ModuleName, "_init.c\n",
		"\t-rm -f $(", ModuleName, ".ss) ", ModuleName, "_init.s\n",
		"\t-rm -f $(", ModuleName, ".os) ", ModuleName, "_init.o\n",
		"\t-rm -f $(", ModuleName, ".nos)\n",
		"\t-rm -f $(", ModuleName, ".qls)\n",
		"\t-rm -f $(", ModuleName, ".errs)\n",
		"\t-rm -f $(", ModuleName, ".err2s)\n\n"
	]),

	io__write_strings(DepStream, [
		"realclean: ", ModuleName, ".realclean\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".realclean : ", ModuleName, ".clean\n",
		"\t-rm -f $(", ModuleName, ".dates)\n",
		"\t-rm -f $(", ModuleName, ".ints)\n",
		"\t-rm -f $(", ModuleName, ".ds)\n"
	]),
	io__write_strings(DepStream, [
		"\t-rm -f ",
			ModuleName, " ",
			ModuleName, ".nu ",
			ModuleName, ".nu.save ",
			ModuleName, ".nu.debug.save ",
			ModuleName, ".nu.debug ",
			ModuleName, ".sicstus ",
			ModuleName, ".sicstus.debug ",
			ModuleName, ".dep\n\n"
	]),
	io__write_strings(DepStream, [
		"clean_nu: ", ModuleName, ".clean_nu\n",
		ModuleName, ".clean_nu :\n",
		"\t-rm -f $(", ModuleName, ".nos)\n\n",

		"clean_sicstus: ", ModuleName, ".clean_sicstus\n",
		ModuleName, ".clean_sicstus :\n",
		"\t-rm -f $(", ModuleName, ".qls)\n\n"
	]).

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

/*
:- pred combine_module_errors(module_error, module_error, module_error).
:- mode combine_module_errors(in, in, out) is det.

combine_module_errors(fatal, _, fatal).
combine_module_errors(yes, fatal, fatal).
combine_module_errors(yes, yes, yes).
combine_module_errors(yes, no, yes).
combine_module_errors(no, Error, Error).
*/

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

:- pred process_module_interfaces(list(string), list(string),
				module_imports, module_imports,
				io__state, io__state).
:- mode process_module_interfaces(in, in, in, out, di, uo) is det.

process_module_interfaces([], IndirectImports, Module0, Module) -->
	process_module_short_interfaces(IndirectImports, Module0, Module).

process_module_interfaces([Import | Imports], IndirectImports0, Module0, Module)
		-->
	{ Module0 = module_imports(ModuleName, DirectImports0, _, Items0,
					Error0) },
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
		{ Module1 = module_imports(ModuleName, DirectImports1, [],
					Items2, Error2) },
		process_module_interfaces(Imports, IndirectImports2,
				Module1, Module)
	).

%-----------------------------------------------------------------------------%

:- pred process_module_short_interfaces(list(string),
		module_imports, module_imports, io__state, io__state).
:- mode process_module_short_interfaces(in, in, out, di, uo) is det.

process_module_short_interfaces([], Module, Module) --> [].
process_module_short_interfaces([Import | Imports], Module0, Module) -->
	{ Module0 = module_imports(ModuleName, DirectImports, IndirectImports0,
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
		{ Module1 = module_imports(ModuleName, DirectImports,
			IndirectImports1, Items2, Error2) },
		process_module_short_interfaces(Imports2, Module1, Module)
	).

%-----------------------------------------------------------------------------%

	% Given a module (well, a list of items),
	% determine all the modules that it depends upon
	% (both interface dependencies and also implementation dependencies).
	%
	% As this predicate does a full traversal of the module, I've bolted 
	% on a function to check that there is something exported in the 
	% interface section.
	%
	% The /2 predicate is the original, and the /5 has the warnings.
	% If the /5 predicate were called from some/too many places, there 
	% would be many repeated and/or inaccurate warnings.
	%
	% This does sacrifice some flexibility for speed.

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

:- pred get_dependencies(string, item_list, list(string), io__state, io__state).
:- mode get_dependencies(in, in, out, di, uo) is det.

get_dependencies(ModuleName, Items, Deps) -->
	{ get_dependencies_2_imp(Items, [], Deps, no, Found_useful_interface) },
	( 	
		{ Found_useful_interface = no }
	->
		warn_no_exports(ModuleName)
	;
		[]
	).

:- pred get_dependencies_2_imp(item_list, list(string), list(string), bool, bool).
:- mode get_dependencies_2_imp(in, in, out, in, out) is det.

get_dependencies_2_imp([], Deps, Deps, UsefulIntf, UsefulIntf).
get_dependencies_2_imp([Item - _Context | Items], Deps0, Deps, 
			UsefulIntf0, UsefulIntf) :-
	( 
		Item = module_defn(_VarSet, import(module(Modules))) 
	->
		list__append(Deps0, Modules, Deps1)
	;
		Deps1 = Deps0
	),
	( 
		Item = module_defn(_Varset, interface)
	->
		get_dependencies_2_int(Items, Deps1, Deps, UsefulIntf0,
				UsefulIntf)
	;
		get_dependencies_2_imp(Items, Deps1, Deps, UsefulIntf0, 
				UsefulIntf)
	).

:- pred get_dependencies_2_int(item_list, list(string), list(string),
				bool, bool).
:- mode get_dependencies_2_int(in, in, out, in, out) is det.

get_dependencies_2_int([], Deps, Deps, UsefulIntf, UsefulIntf).
get_dependencies_2_int([Item - _Context | Items], Deps0, Deps, 
			UsefulIntf0, UsefulIntf) :-
	(
		Item = module_defn(_VarSet, import(module(Modules)))
	->
		list__append(Deps0, Modules, Deps1),
		UsefulIntf1 = UsefulIntf0
	;
		Item = nothing
	->
		Deps1 = Deps0,
		UsefulIntf1 = UsefulIntf0
	;
		Deps1 = Deps0,
		UsefulIntf1 = yes 
	),
	( 
		Item = module_defn(_Varset, implementation)
	->
		get_dependencies_2_imp(Items, Deps1, Deps, UsefulIntf1,
				UsefulIntf)
	;
		get_dependencies_2_int(Items, Deps1, Deps, UsefulIntf1,
				UsefulIntf)
	).

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
