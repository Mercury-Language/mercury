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
	%	modules.  While we're at it, we also save the per-module
	%	makefile dependency (`.d') files for all those modules.
	%
:- pred generate_dependencies(string, io__state, io__state).
:- mode generate_dependencies(in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module bool, set, map, term, varset, dir, std_util, library.
:- import_module globals, options, passes_aux, prog_out, mercury_to_mercury.

make_interface(ModuleName, Items0) -->
	{ get_interface(Items0, InterfaceItems0) },
	check_for_clauses_in_interface(InterfaceItems0, InterfaceItems),
	write_interface_file(ModuleName, ".int", InterfaceItems),
	{ get_short_interface(InterfaceItems, ShortInterfaceItems) },
	write_interface_file(ModuleName, ".int2", ShortInterfaceItems),
	check_for_no_exports(InterfaceItems, ModuleName),
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

:- pred check_for_no_exports(item_list, string, io__state, io__state).
:- mode check_for_no_exports(in, in, di, uo) is det.

check_for_no_exports([], ModuleName) -->
	warn_no_exports(ModuleName).
check_for_no_exports([Item - _Context | Items], ModuleName) -->
	(
		{ Item = nothing
		; Item = module_defn(_,_)
		}
	->
		% nothing useful - keep searching
		check_for_no_exports(Items, ModuleName)
	;
		% we found something useful - don't issue the warning
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

		% we need to add a `:- interface' declaration at the start
		% of the item list
	{ varset__init(VarSet) },
	{ term__context_init(ModuleName, 0, Context) },
	{ InterfaceDeclaration = module_defn(VarSet, interface) - Context },
	{ InterfaceItems1 = [InterfaceDeclaration | InterfaceItems] },

	convert_to_mercury(BaseModuleName, TmpOutputFileName, InterfaceItems1),

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
	invoke_system_command(ShellCommand, Succeeded),
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
		{ list__sort_and_remove_dups(LongDeps0, LongDeps1) },
		{ list__delete_all(LongDeps1, ModuleName, LongDeps) },
		{ list__sort_and_remove_dups(ShortDeps0, ShortDeps1) },
		{ list__delete_elems(ShortDeps1, LongDeps, ShortDeps2) },
		{ list__delete_all(ShortDeps2, ModuleName, ShortDeps) },

		io__write_strings(DepStream, [
			ModuleName, ".c ",
			ModuleName, ".err ",
			ModuleName, ".o : ",
			ModuleName, ".m"
		] ),
		write_dependencies_list(LongDeps, ".int", DepStream),
		write_dependencies_list(ShortDeps, ".int2", DepStream),

		io__write_strings(DepStream, [
			"\n\n",
			ModuleName, ".dir/", ModuleName, "_000.o: ",
				ModuleName, ".m\n",
			"\trm -rf ", ModuleName, ".dir\n",
			"\t$(MCS) -s$(GRADE) $(MCSFLAGS) ", ModuleName, ".m\n"
		]),

		io__close_output(DepStream),
		maybe_write_string(Verbose, " done.\n")
	;
		{ string__append_list(["can't open file `", DependencyFileName,
				"' for output."], Message) },
		report_error(Message)
	).

%-----------------------------------------------------------------------------%

generate_dependencies(Module) -->
	%
	% first, build up a map of the dependencies (writing `.d' files as
	% we go)
	%
	{ map__init(DepsMap0) },
	generate_deps_map([Module], DepsMap0, DepsMap),
	%
	% check whether we couldn't read the main `.m' file
	%
	{ map__lookup(DepsMap, Module, deps(_, Error, _, _)) },
	( { Error = fatal } ->
	    { string__append_list(["fatal error reading module `",
				Module, "'."], Message) },
	    report_error(Message)
	;
	    %
	    % now, write the `.dep' file
	    %
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

% This is the data structure we use to record the dependencies.
% We keep a map from module name to information about the module.

:- type deps_map == map(string, deps).
:- type deps
	---> deps(
		bool,		% have we processed this module yet?
		module_error,	% if we did, where there any errors?
		list(string),	% interface dependencies
		list(string)	% implementation dependencies
	).

% This is the predicate which creates the above data structure.

:- pred generate_deps_map(list(string), deps_map, deps_map,
			io__state, io__state).
:- mode generate_deps_map(in, in, out, di, uo) is det.

generate_deps_map([], DepsMap, DepsMap) --> [].
generate_deps_map([Module | Modules], DepsMap0, DepsMap) -->
		% Look up the module's dependencies, and determine whether
		% it has been processed yet.
	lookup_dependencies(Module, DepsMap0, Done, Error, IntDeps, ImplDeps,
				DepsMap1),
		% If the module hadn't been processed yet, compute its
		% transitive dependencies (we already know its primary ones),
		% (1) output this module's dependencies to its `.d' file
		% (if the `.m' file exists), (2) add its imports to the list of
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


% Write out the `.dep' file, using the information collected in the
% deps_map data structure.

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
	io__write_string(DepStream, ".pic_os = "),
	write_dependencies_list(Modules, ".$(EXT_FOR_PIC_OBJECTS)", DepStream),
	io__write_string(DepStream, "\n"),

	io__write_string(DepStream, ModuleName),
	io__write_string(DepStream, ".dir_os = "),
	write_dependencies_list(Modules, ".dir/*.o", DepStream),
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
			"\t$(", ModuleName, ".os) $(MLLIBS)\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".split : ", ModuleName, ".split.a ",
				ModuleName, "_init.o\n",
		"\t$(ML) -s $(GRADE) $(MLFLAGS) -o ", ModuleName, ".split ",
			ModuleName, "_init.o \\\n",
			"\t", ModuleName, ".split.a $(MLLIBS)\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".split.a : $(", ModuleName, ".dir_os)\n",
		"\trm -f ", ModuleName, ".split.a\n",
		"\tar cr ", ModuleName, ".split.a ",
			"$(", ModuleName, ".dir_os)\n",
		"\tranlib ", ModuleName, ".split.a\n\n"
	]),

/************
% I decided to leave the rules for `foo.so' and `foo.a' out,
% mainly because the rule for `foo.so' is hard to make portable.
% The rules here would conflict with the one in library/Mmake.

	io__write_strings(DepStream, [
		ModuleName, ".so : $(", ModuleName, ".pic_os)\n",
		"\t$(LINK_SHARED_LIB) -o ", ModuleName, ".so ",
			"$(", ModuleName, ".pic_os)\n\n"
	]),

	io__write_strings(DepStream, [
		ModuleName, ".a : $(", ModuleName, ".os)\n",
		"\trm -f ", ModuleName, ".a\n",
		"\tar cr ", ModuleName, ".a ",
			"$(", ModuleName, ".os)\n",
		"\tranlib ", ModuleName, ".a\n\n"
	]),
************/

	io__write_strings(DepStream, [
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
		"\t-rm -rf ", ModuleName, ".dir\n",
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
			ModuleName, ".split ",
			ModuleName, ".split.a ",
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
					_, _, IntDeps, _ImplDeps, DepsMap1),
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
		read_dependencies(Module, IntDeps, ImplDeps, Error),
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
			"Getting dependencies for module", Items0, Error)),
	( { Items0 = [], Error = fatal } ->
		io__gc_call(read_mod_ignore_errors(Module, ".int",
		    "Getting dependencies for module interface", Items, _Error))
	;
		{ Items = Items0 }
	),
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
				"Reading interface for module",
				LongIntItems1, Error1)
		),
		% strip off the `:- interface' declaration at the start, if any
		{
			LongIntItems1 = [ FirstItem | LongIntItems2 ],
			FirstItem = module_defn(_, interface) - _
		->
			Items1 = LongIntItems2
		;
			Items1 = LongIntItems1
		},
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
					ShortIntItems1, Error1)
		),
		% strip off the `:- interface' declaration at the start, if any
		{
			ShortIntItems1 = [ FirstItem | ShortIntItems2 ],
			FirstItem = module_defn(_, interface) - _
		->
			Items1 = ShortIntItems2
		;
			Items1 = ShortIntItems1
		},
		{ Error1 \= no ->
			Error2 = yes
		;
			Error2 = Error0
		},

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

	% Given a module interface (well, a list of items), extract the
	% short interface part of that module, i.e. the exported
	% type/inst/mode declarations, but not the exported pred or
	% constructor declarations.  If the module interface imports
	% other modules, then the short interface only needs to include
	% those import_module declarations only if the short interface
	% contains some equivalence types or some mode or inst definitions
	% that might use declarations in the imported modules.
	% If the short interface is empty, or only contains abstract
	% type declarations, then it doesn't need any import_module
	% declarations.

:- pred get_short_interface(item_list, item_list).
:- mode get_short_interface(in, out) is det.

get_short_interface(Items0, Items) :-
	get_short_interface_2(Items0, [], [], no,
			RevItems, RevImports, NeedsImports),
	list__reverse(RevItems, Items1),
	( NeedsImports = yes ->
		list__reverse(RevImports, Imports1),
		list__append(Imports1, Items1, Items)
	;
		Items = Items1
	).

:- pred get_short_interface_2(item_list, item_list, item_list, bool,
				item_list, item_list, bool).
:- mode get_short_interface_2(in, in, in, in, out, out, out) is det.

get_short_interface_2([], Items, Imports, NeedsImports,
			Items, Imports, NeedsImports).
get_short_interface_2([ItemAndContext | Rest], Items0, Imports0, NeedsImports0,
			Items, Imports, NeedsImports) :-
	ItemAndContext = Item0 - Context,
	( Item0 = module_defn(_, import(_)) ->
		Items1 = Items0,
		Imports1 = [ItemAndContext | Imports0],
		NeedsImports1 = NeedsImports0
	; make_abstract_type_defn(Item0, Item1) ->
		Imports1 = Imports0,
		Items1 = [Item1 - Context | Items0],
		NeedsImports1 = NeedsImports0
	; include_in_short_interface(Item0) ->
		Imports1 = Imports0,
		Items1 = [ItemAndContext | Items0],
		NeedsImports1 = yes
	;
		Items1 = Items0,
		Imports1 = Imports0,
		NeedsImports1 = NeedsImports0
	),
	get_short_interface_2(Rest, Items1, Imports1, NeedsImports1,
				Items, Imports, NeedsImports).

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
make_abstract_type_defn(type_defn(VarSet, abstract_type(Name, Args), Cond),
			type_defn(VarSet, abstract_type(Name, Args), Cond)).

%-----------------------------------------------------------------------------%
