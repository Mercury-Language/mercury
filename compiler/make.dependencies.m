%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: make.dependencies.m
% Author: stayl
% 
% Code to find the dependencies for a particular target,
% e.g. module.c depends on module.m, import.int, etc.
%-----------------------------------------------------------------------------%
:- module make__dependencies.

:- interface.

	% find_module_deps(ModuleName, Succeeded, Deps, Info0, Info).
	%
	% The reason we don't return maybe(Deps) is that with `--keep-going'
	% we want to do as much work as possible.
:- type find_module_deps(T) ==
		pred(module_name, bool, set(T),
			make_info, make_info, io__state, io__state).
:- inst find_module_deps ==
		(pred(in, out, out, in, out, di, uo) is det).

:- type dependency_file
	--->	target(target_file)		% A target which could be made.
	;	file(file_name, maybe(option))	% An ordinary file which
						% `mmc --make' does not know
						% how to rebuild. The option
						% gives a list of directories
						% in which to search.
	.

	% Return a closure which will find the dependencies for
	% a target type given a module name.
:- func target_dependencies(globals, module_target_type) =
			find_module_deps(dependency_file).
:- mode target_dependencies(in, in) = out(find_module_deps) is det.

	% Union the output set of dependencies for a given module
	% with the accumulated set. This is used with
	% foldl3_maybe_stop_at_error to iterate over a list of
	% module_names to find all target files for those modules.
:- pred union_deps(find_module_deps(T)::in(find_module_deps),
	module_name::in, bool::out, set(T)::in, set(T)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

	% Find all modules in the current directory which are
	% reachable (by import) from the given module.
:- pred find_reachable_local_modules(module_name::in, bool::out,
		set(module_name)::out, make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- pred dependency_status(dependency_file::in, dependency_status::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- type dependencies_result
	--->	up_to_date
	;	out_of_date
	;	error
	.

	% Check that all the dependency targets are up-to-date. 
:- pred check_dependencies(string::in, maybe_error(timestamp)::in,
	list(dependency_file)::in, dependencies_result::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

	% Check that all the dependency files are up-to-date. 
:- pred check_dependency_timestamps(string::in,
	maybe_error(timestamp)::in, list(File)::in,
	pred(File, io__state, io__state)::(pred(in, di, uo) is det),
	list(maybe_error(timestamp))::in, dependencies_result::out,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%

:- type cached_direct_imports.
:- func init_cached_direct_imports = cached_direct_imports.

:- type cached_transitive_dependencies.
:- func init_cached_transitive_dependencies = cached_transitive_dependencies.

%-----------------------------------------------------------------------------%
:- implementation.

:- type deps_result(T) == pair(bool, set(T)).
:- type module_deps_result == deps_result(module_name).

union_deps(FindDeps, ModuleName, Success, Deps0,
		set__union(Deps0, Deps), Info0, Info) -->
	FindDeps(ModuleName, Success, Deps, Info0, Info).

	% Note that we go to some effort in this module to stop
	% dependency calculation as soon as possible if there
	% are errors. This is important because the calls to
	% get_module_dependencies from the dependency calculation
	% predicates can result in every module in the program being
	% read.
:- func combine_deps(find_module_deps(T), find_module_deps(T)) =
		find_module_deps(T).
:- mode combine_deps(in(find_module_deps), in(find_module_deps)) =
		out(find_module_deps) is det.

combine_deps(FindDeps1, FindDeps2) = 
	(pred(ModuleName::in, Success::out, Deps::out,
			Info0::in, Info::out, di, uo) is det -->
		FindDeps1(ModuleName, Success1, Deps1, Info0, Info1),
		( { Success1 = no, Info1 ^ keep_going = no } ->
			{ Info = Info1 },
			{ Success = no },
			{ Deps = Deps1 }
		;
			FindDeps2(ModuleName, Success2, Deps2, Info1, Info),
			{ Success = Success1 `and` Success2 },
			{ Deps = set__union(Deps1, Deps2) }
		)
	).

:- func combine_deps_list(list(find_module_deps(T))) =
		find_module_deps(T).
:- mode combine_deps_list(in(list_skel(find_module_deps))) =
		out(find_module_deps) is det.

combine_deps_list([]) = no_deps.
combine_deps_list([FindDeps | FindDepsList]) =
		( FindDepsList = [] ->
			FindDeps
		;
			combine_deps(FindDeps, combine_deps_list(FindDepsList))
		).

target_dependencies(_, source) = no_deps.
target_dependencies(Globals, errors) = compiled_code_dependencies(Globals).
target_dependencies(_, private_interface) = interface_file_dependencies.
target_dependencies(_, long_interface) = interface_file_dependencies.
target_dependencies(_, short_interface) = interface_file_dependencies.
target_dependencies(_, unqualified_short_interface) = source `of` self.
target_dependencies(Globals, aditi_code) = compiled_code_dependencies(Globals).
target_dependencies(Globals, c_header) = target_dependencies(Globals, c_code).
target_dependencies(Globals, c_code) = compiled_code_dependencies(Globals).
target_dependencies(Globals, il_code) = compiled_code_dependencies(Globals).
target_dependencies(_, il_asm) = il_code `of` self.
target_dependencies(Globals, java_code) = compiled_code_dependencies(Globals). 
target_dependencies(Globals, asm_code(_)) =
		compiled_code_dependencies(Globals).
target_dependencies(Globals, object_code(PIC)) = Deps :-
	globals__get_target(Globals, CompilationTarget),
	TargetCode = ( CompilationTarget = asm -> asm_code(PIC) ; c_code ),
	globals__lookup_bool_option(Globals, highlevel_code, HighLevelCode),

	%
	% For --highlevel-code, the `.c' file will #include the header
	% file for all imported modules.
	%
	HeaderDeps =
	    ( CompilationTarget = c, HighLevelCode = yes ->
		combine_deps_list([
		    c_header `of` direct_imports,
		    c_header `of` indirect_imports,
		    c_header `of` parents,
		    c_header `of` intermod_imports,
		    c_header `of` foreign_imports
		])
	    ;
		no_deps
	    ),
	Deps = combine_deps_list([
		TargetCode `of` self,
		c_header `of` foreign_imports,
		HeaderDeps
	]).
target_dependencies(_, intermodule_interface) =
		combine_deps_list([
			source `of` self,
			private_interface `of` parents,
			long_interface `of` non_intermod_direct_imports,
			short_interface `of` non_intermod_indirect_imports
		]).

:- func interface_file_dependencies =
	(find_module_deps(dependency_file)::out(find_module_deps)) is det.

interface_file_dependencies =
		combine_deps_list([
			source `of` self,
			private_interface `of` parents,
			unqualified_short_interface `of` direct_imports,
			unqualified_short_interface `of` indirect_imports
		]).

:- func compiled_code_dependencies(globals::in) =
	(find_module_deps(dependency_file)::out(find_module_deps)) is det.

compiled_code_dependencies(Globals) = Deps :-
	globals__lookup_bool_option(Globals,
		intermodule_optimization, Intermod),
	( Intermod = yes ->
			% XXX Handle inter-module optimization
			% with sub-modules properly. Currently
			% inter-module optimization is pretty much
			% disabled in the presence of sub-modules.
			% We need to read the `.int0' files for the parents
			% of all modules for which we read `.opt' files.
		Deps = combine_deps_list([
				intermodule_interface `of` self,
				intermodule_interface `of` intermod_imports,
				compiled_code_dependencies
			])
	;
		Deps = compiled_code_dependencies
	).

:- func compiled_code_dependencies =
	(find_module_deps(dependency_file)::out(find_module_deps)) is det.

compiled_code_dependencies = 
		combine_deps_list([
			source `of` self,
			fact_table `files_of` self,
			private_interface `of` parents,
			long_interface `of` direct_imports,
			short_interface `of` indirect_imports
		]).

:- func module_target_type `of` find_module_deps(module_name) =
		find_module_deps(dependency_file).
:- mode in `of` in(find_module_deps) = out(find_module_deps) is det.

FileType `of` FindDeps =
    (pred(ModuleName::in, Success::out, TargetFiles::out,
    		Info0::in, Info::out, di, uo) is det -->
	FindDeps(ModuleName, Success, ModuleNames, Info0, Info),
	{ TargetFiles = set__sorted_list_to_set(
		make_dependency_list(set__to_sorted_list(ModuleNames),
				FileType)) }
    ).

:- func find_module_deps(pair(file_name, maybe(option))) `files_of`
		find_module_deps(module_name) =
			find_module_deps(dependency_file).
:- mode in(find_module_deps) `files_of` in(find_module_deps)
		= out(find_module_deps) is det.

FindFiles `files_of` FindDeps =
    (pred(ModuleName::in, Success::out, DepFiles::out,
    		Info0::in, Info::out, di, uo) is det -->
	{ KeepGoing = Info0 ^ keep_going },

	FindDeps(ModuleName, Success0, ModuleNames, Info0, Info1),
	( { Success0 = no, KeepGoing = no } ->
		{ Success = no },
		{ Info = Info1 },
		{ DepFiles = set__init }
	;
		foldl3_maybe_stop_at_error(KeepGoing,
			union_deps(FindFiles),
			set__to_sorted_list(ModuleNames),
			Success1, set__init, FileNames, Info1, Info),
		{ Success = Success0 `and` Success1 },
		{ DepFiles = set__sorted_list_to_set(
			list__map(
			    (func(FileName - Option) = file(FileName, Option)),
			    set__to_sorted_list(FileNames))) }
    	)
    ).

:- pred no_deps(module_name::in, bool::out, set(T)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

no_deps(_, yes, set__init, Info, Info) --> [].

:- pred self(module_name::in, bool::out, set(module_name)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

self(ModuleName, yes, set__make_singleton_set(ModuleName), Info, Info) --> [].

:- pred parents(module_name::in, bool::out, set(module_name)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

parents(ModuleName, yes, set__list_to_set(Ancestors), Info, Info) -->
	{ get_ancestors(ModuleName, Ancestors) }.

%-----------------------------------------------------------------------------%

:- type cached_direct_imports == map(module_name, module_deps_result).

init_cached_direct_imports = map__init.

:- pred direct_imports(module_name::in, bool::out, set(module_name)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.
		
direct_imports(ModuleName, Success, Modules, Info0, Info) -->
    ( { Result0 = Info0 ^ cached_direct_imports ^ elem(ModuleName) } ->
	{ Result0 = Success - Modules },
	{ Info = Info0 }
    ;
	{ KeepGoing = Info0 ^ keep_going },

	non_intermod_direct_imports(ModuleName, Success0,
		Modules0, Info0, Info1),
	( { Success0 = no, KeepGoing = no } ->
	    { Info3 = Info1 },
	    { Success = no },
	    { Modules = set__init }
	;
		%
		% We also read `.int' files for modules imported
		% by `.opt' files.
		%
	    intermod_imports(ModuleName, Success1,
			IntermodModules, Info1, Info2),
	    ( { Success1 = no, KeepGoing = no } ->
		{ Info3 = Info2 },
		{ Success = no },
		{ Modules = set__init }
	    ;
		foldl3_maybe_stop_at_error(Info2 ^ keep_going,
			union_deps(non_intermod_direct_imports),
			set__to_sorted_list(IntermodModules), Success2,
			Modules0, Modules1, Info2, Info3),
		{ Success = Success0 `and` Success1 `and` Success2 },
		{ Modules = set__delete(Modules1, ModuleName) }
	    )
	),
	{ Info = Info3 ^ cached_direct_imports
			^ elem(ModuleName) := Success - Modules }
    ).

	% Return the modules for which `.int' files are read in a compilation
	% which does not use `--intermodule-optimization'.
:- pred non_intermod_direct_imports(module_name::in, bool::out,
	set(module_name)::out, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

non_intermod_direct_imports(ModuleName, Success, Modules, Info0, Info) -->
	get_module_dependencies(ModuleName, MaybeImports, Info0, Info1),
	(
		{ MaybeImports = yes(Imports) },

		%
		% Find the direct imports of this module (modules
		% for which we will read the `.int' files).
		%
		% Note that we need to do this both for the interface
		% imports of this module and for the *implementation*
		% imports of its ancestors.  This is because if this
		% module is defined in the implementation section of
		% its parent, then the interface of this module may
		% depend on things imported only by its parent's
		% implementation.
		%
		% If this module was actually defined in the interface
		% section of one of its ancestors, then it should only
		% depend on the interface imports of that ancestor,
		% so the dependencies added here are in fact more
		% conservative than they need to be in that case.
		% However, that should not be a major problem.
		% (This duplicates how this is handled by modules.m).
		% 
		{ Modules0 = set__union(set__list_to_set(Imports ^ impl_deps),
				set__list_to_set(Imports ^ int_deps)) },
		( { ModuleName = qualified(ParentModule, _) } ->
			non_intermod_direct_imports(ParentModule, Success,
				ParentImports, Info1, Info),
			{ Modules = set__union(ParentImports, Modules0) }
		;
			{ Success = yes },
			{ Modules = Modules0 },
			{ Info = Info1 }
		)
	;
		{ MaybeImports = no },
		{ Success = no },
		{ Modules = set__init },
		{ Info = Info1 }
	).

%-----------------------------------------------------------------------------%

	% Return the list of modules for which we should read `.int2' files.
:- pred indirect_imports(module_name::in, bool::out, set(module_name)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

indirect_imports(ModuleName, Success, Modules, Info0, Info) -->
	indirect_imports_2(direct_imports, ModuleName,
		Success, Modules, Info0, Info).

	% Return the list of modules for which we should read `.int2' files,
	% ignoring those which need to be read as a result of importing
	% modules imported by a `.opt' file.
:- pred non_intermod_indirect_imports(module_name::in, bool::out,
	set(module_name)::out, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

non_intermod_indirect_imports(ModuleName, Success, Modules, Info0, Info) -->
	indirect_imports_2(non_intermod_direct_imports, ModuleName,
		Success, Modules, Info0, Info).

:- pred indirect_imports_2(find_module_deps(module_name)::in(find_module_deps),
	module_name::in, bool::out, set(module_name)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

indirect_imports_2(FindDirectImports, ModuleName, Success, IndirectImports,
		Info0, Info) -->
	{ KeepGoing = Info1 ^ keep_going },

	FindDirectImports(ModuleName, DirectSuccess,
		DirectImports, Info0, Info1),
	( { DirectSuccess = no, KeepGoing = no } ->
		{ Success = no },
		{ IndirectImports = set__init },
		{ Info = Info1 }
	;
		foldl3_maybe_stop_at_error(Info1 ^ keep_going,
			union_deps(find_transitive_interface_imports),
			set__to_sorted_list(DirectImports), IndirectSuccess,
			set__init, IndirectImports0, Info1, Info),
		{ IndirectImports = set__difference(
			set__delete(IndirectImports0, ModuleName),
			DirectImports) },
		{ Success = DirectSuccess `and` IndirectSuccess }
	).

%-----------------------------------------------------------------------------%

	% Return the list of modules for which we should read `.opt' files.
:- pred intermod_imports(module_name::in, bool::out, set(module_name)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

intermod_imports(ModuleName, Success, Modules, Info0, Info) -->
	globals__io_lookup_bool_option(intermodule_optimization, Intermod),
	(
		{ Intermod = yes },
		% XXX Read `.opt' files transitively.
		non_intermod_direct_imports(ModuleName, Success,
			Modules, Info0, Info)
	;
		{ Intermod = no },
		{ Info = Info0 },
		{ Success = yes },
		{ Modules = set__init }
	).

%-----------------------------------------------------------------------------%

:- pred foreign_imports(module_name::in, bool::out, set(module_name)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

foreign_imports(ModuleName, Success, Modules, Info0, Info) -->
	%
	% The object file depends on the header files for the modules
	% mentioned in `:- pragma foreign_import_module' declarations
	% in the current module and the `.opt' files it imports.
	%
	globals__io_get_globals(Globals),
	{ globals__get_backend_foreign_languages(Globals, Languages) },
	intermod_imports(ModuleName, IntermodSuccess, IntermodModules,
		Info0, Info1),
	foldl3_maybe_stop_at_error(Info1 ^ keep_going,
		union_deps(find_module_foreign_imports(
				set__list_to_set(Languages))),
		[ModuleName | set__to_sorted_list(IntermodModules)],
		ForeignSuccess, set__init, Modules, Info1, Info),
	{ Success = IntermodSuccess `and` ForeignSuccess }.

:- pred find_module_foreign_imports(set(foreign_language)::in, module_name::in,
	bool::out, set(module_name)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.	

find_module_foreign_imports(Languages, ModuleName,
		Success, ForeignModules, Info0, Info) -->
	get_module_dependencies(ModuleName, MaybeImports, Info0, Info),
	{
		MaybeImports = yes(Imports),
		ForeignModules = set__list_to_set(
					get_foreign_imported_modules(Languages,
					Imports ^ foreign_import_module_info)),
		Success = yes
	;
		MaybeImports = no,
		ForeignModules = set__init,
		Success = no
	}.

:- func get_foreign_imported_modules(foreign_import_module_info) =
		list(module_name).

get_foreign_imported_modules(ForeignImportModules) =
	get_foreign_imported_modules_2(no, ForeignImportModules).

:- func get_foreign_imported_modules(set(foreign_language),
		foreign_import_module_info) = list(module_name).

get_foreign_imported_modules(Languages, ForeignImportModules) =
	get_foreign_imported_modules_2(yes(Languages), ForeignImportModules).

:- func get_foreign_imported_modules_2(maybe(set(foreign_language)),
		foreign_import_module_info) = list(module_name).

get_foreign_imported_modules_2(MaybeLanguages, ForeignImportModules) =
	list__filter_map(
	    (func(ForeignImportModule) = ForeignModule is semidet :-
		ForeignImportModule =
			foreign_import_module(Language, ForeignModule, _),
		(
		  	MaybeLanguages = yes(Languages),
			set__member(Language, Languages)
		;
			MaybeLanguages = no
		)
	    ), ForeignImportModules).

%-----------------------------------------------------------------------------%

:- pred fact_table(module_name::in,
	bool::out, set(pair(file_name, maybe(option)))::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

fact_table(ModuleName, Success, Files, Info0, Info) -->
	get_module_dependencies(ModuleName, MaybeImports, Info0, Info),
	{
		MaybeImports = yes(Imports),
		Success = yes,
		Files = set__list_to_set(
			make_target_list(Imports ^ fact_table_deps, no))
	;
		MaybeImports = no,
		Success = no,
		Files = set__init
	}.

%-----------------------------------------------------------------------------%

:- type transitive_dependencies_root
	---> transitive_dependencies_root(
		module_name,
		transitive_dependencies_type,
		module_locn
	).

:- type transitive_deps_result == pair(bool, set(module_name)).

:- type transitive_dependencies_type
	--->    interface_imports
	;       all_dependencies        % including parents and children
	.

:- type module_locn
	--->    local_module    % The source file for the module is in
				% the current directory.
	;       any_module
	.

:- type cached_transitive_dependencies ==
		map(transitive_dependencies_root, transitive_deps_result).

init_cached_transitive_dependencies = map__init.

find_reachable_local_modules(ModuleName, Success, Modules, Info0, Info) -->
	find_transitive_module_dependencies(all_dependencies, local_module,
		ModuleName, Success, Modules0, Info0, Info),
	{ Modules = set__insert(Modules0, ModuleName) }.

:- pred find_transitive_interface_imports(module_name::in, bool::out,
		set(module_name)::out, make_info::in, make_info::out,
		io__state::di, io__state::uo) is det.

find_transitive_interface_imports(ModuleName,
		Success, Modules, Info0, Info) -->
	find_transitive_module_dependencies(interface_imports, any_module,
		ModuleName, Success, Modules, Info0, Info).

:- pred find_transitive_module_dependencies(transitive_dependencies_type::in,
	module_locn::in, module_name::in, bool::out, set(module_name)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

find_transitive_module_dependencies(DependenciesType, ModuleLocn,
		ModuleName, Success, Modules, Info0, Info) -->
	globals__io_lookup_bool_option(keep_going, KeepGoing),
	find_transitive_module_dependencies_2(KeepGoing,
		DependenciesType, ModuleLocn, ModuleName,
		Success, set__init, Modules0, Info0, Info1),
	{ set__delete(Modules0, ModuleName, Modules) },
    	{ DepsRoot = transitive_dependencies_root(ModuleName,
			DependenciesType, ModuleLocn) },
	{ Info = Info1 ^ cached_transitive_dependencies
			^ elem(DepsRoot) := Success - Modules }.

:- pred find_transitive_module_dependencies_2(bool::in,
	transitive_dependencies_type::in, module_locn::in,
	module_name::in, bool::out, set(module_name)::in,
	set(module_name)::out, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

find_transitive_module_dependencies_2(KeepGoing, DependenciesType,
		ModuleLocn, ModuleName, Success, Modules0, Modules,
		Info0, Info) -->
    (
    	{ set__member(ModuleName, Modules0) }
    ->
	{ Success = yes },
    	{ Modules = Modules0 },
	{ Info = Info0 }
    ;
        { DepsRoot = transitive_dependencies_root(ModuleName,
				DependenciesType, ModuleLocn) },
        { Result0 = Info0 ^ cached_transitive_dependencies ^ elem(DepsRoot) }
    ->
	{ Result0 = Success - Modules1 },
	{ Modules = set__union(Modules0, Modules1) },
	{ Info = Info0 }
    ;
	get_module_dependencies(ModuleName, MaybeImports, Info0, Info1),
	(
	    { MaybeImports = yes(Imports) },
	    (
		{
			ModuleLocn = any_module
	    	;
			ModuleLocn = local_module,
			Imports ^ module_dir = dir__this_directory
		}
	    ->
		{
			% Parents don't need to be considered here.
			% Anywhere the interface of the child module
			% is needed, the parent must also have been
			% imported.
			DependenciesType = interface_imports,
			ImportsToCheck = Imports ^ int_deps
		;
			DependenciesType = all_dependencies,
			ImportsToCheck = 
			    list__condense([
				Imports ^ int_deps,
				Imports ^ impl_deps,
				Imports ^ parent_deps,
				Imports ^ children,
				get_foreign_imported_modules(
					Imports ^ foreign_import_module_info)
			    ])
		},
		foldl3_maybe_stop_at_error(KeepGoing,
			find_transitive_module_dependencies_2(KeepGoing,
				DependenciesType, ModuleLocn),
				ImportsToCheck, Success,
				set__insert(Modules0, ModuleName), Modules,
				Info1, Info)
	    ;
		{ Success = yes },
		{ Modules = Modules0 },
		{ Info = Info1 }
	    )
	;
	    { MaybeImports = no },
	    { Success = no } ,
	    { Modules = Modules0 },
	    { Info = Info1 }
	)
    ).

%-----------------------------------------------------------------------------%

check_dependencies(TargetFileName, MaybeTimestamp,
		DepFiles, DepsResult, Info0, Info) -->
	list__map_foldl2(dependency_status, DepFiles,
		DepStatusList, Info0, Info1),
	{ assoc_list__from_corresponding_lists(DepFiles,
		DepStatusList, DepStatusAL) },
	{ list__filter(
		(pred((_ - DepStatus)::in) is semidet :-
			DepStatus \= up_to_date
		), DepStatusAL, UnbuiltDependencies) },

	( { UnbuiltDependencies \= [] } ->
		{ Info = Info1 },
		debug_msg(
		    (pred(di, uo) is det -->
			io__write_string(TargetFileName),
			io__write_string(
				": dependencies could not be built.\n\t"),
			io__write_list(UnbuiltDependencies,
				",\n\t",
				(pred((DepTarget - DepStatus)::in,
					di, uo) is det -->
				    write_dependency_file(DepTarget),
				    io__write_string(" - "),
				    io__write(DepStatus)
				)),
			io__nl
		    )),
		{ DepsResult = error }
	;
		debug_msg(
		    (pred(di, uo) is det -->
			io__write_string(TargetFileName),
			io__write_string(": finished dependencies\n")
		    )),
		list__map_foldl2(get_dependency_timestamp, DepFiles,
			DepTimestamps, Info1, Info),

		check_dependency_timestamps(TargetFileName, MaybeTimestamp,
			DepFiles, write_dependency_file, DepTimestamps,
			DepsResult)
	).

check_dependency_timestamps(TargetFileName, MaybeTimestamp, DepFiles,
		WriteDepFile, DepTimestamps, DepsResult) -->
    ( 
	{ MaybeTimestamp = error(_) },
	{ DepsResult = out_of_date },
	debug_msg(
	    (pred(di, uo) is det -->
		io__write_string(TargetFileName),
		io__write_string("does not exist.\n")
	    ))
    ;
	{ MaybeTimestamp = ok(Timestamp) },
	globals__io_lookup_bool_option(rebuild, Rebuild),

	(
	    { list__member(MaybeDepTimestamp1, DepTimestamps) },
	    { MaybeDepTimestamp1 = error(_) }
	->
	    { DepsResult = error }
	;
	    { Rebuild = yes }
	->
		%
		% With `--rebuild', a target is always considered
		% to be out-of-date, regardless of the timestamps
		% of its dependencies.
		%
	    { DepsResult = out_of_date }
	;
	    { list__member(MaybeDepTimestamp2, DepTimestamps) },
	    { MaybeDepTimestamp2 = ok(DepTimestamp) },
	    { compare((>), DepTimestamp, Timestamp) }
	->
	    debug_newer_dependencies(TargetFileName, MaybeTimestamp,
		DepFiles, WriteDepFile, DepTimestamps),
	    { DepsResult = out_of_date }
	;
	    { DepsResult = up_to_date }
	)
    ).

:- pred debug_newer_dependencies(string::in, maybe_error(timestamp)::in,
	list(T)::in, pred(T, io__state, io__state)::(pred(in, di, uo) is det),
	list(maybe_error(timestamp))::in, io__state::di, io__state::uo) is det.

debug_newer_dependencies(TargetFileName, MaybeTimestamp,
		DepFiles, WriteDepFile, DepTimestamps) -->
	debug_msg(
	    (pred(di, uo) is det -->
		io__write_string(TargetFileName),
		io__write_string(": newer dependencies: "),
		{ assoc_list__from_corresponding_lists(DepFiles,
			DepTimestamps, DepTimestampAL) },
		{ solutions(
		    (pred(DepFile::out) is nondet :-
			list__member(DepFile - MaybeDepTimestamp,
				DepTimestampAL),
			(
				MaybeDepTimestamp = error(_)
			;
				MaybeDepTimestamp = ok(DepTimestamp),
				MaybeTimestamp = ok(Timestamp),
				compare((>), DepTimestamp, Timestamp)
			)), NewerDeps) },
		io__write_list(NewerDeps, ",\n\t", WriteDepFile),
		io__nl
	    )).

dependency_status(file(FileName, _) @ Dep, Status, Info0, Info) -->
	( { Status0 = Info0 ^ dependency_status ^ elem(Dep) } ->
		{ Info = Info0 },
		{ Status = Status0 }
	;
		get_dependency_timestamp(Dep, MaybeTimestamp, Info0, Info1),
		(
			{ MaybeTimestamp = ok(_) },
			{ Status = up_to_date }
		;
			{ MaybeTimestamp = error(Error) },
			{ Status = error },
			io__write_string("** Error: file `"),
			io__write_string(FileName),
			io__write_string("' not found: "),
			io__write_string(Error)
		),
		{ Info = Info1 ^ dependency_status ^ elem(Dep) := Status }
	).
dependency_status(target(Target) @ Dep, Status, Info0, Info) -->
    { Target = ModuleName - FileType },
    ( { FileType = source } ->
	% Source files are always up-to-date.
	{ Info = Info0 },
	{ Status = up_to_date }
    ; { Status0 = Info0 ^ dependency_status ^ elem(Dep) } ->
		{ Info = Info0 },
		{ Status = Status0 }
    ;
	get_module_dependencies(ModuleName, MaybeImports, Info0, Info1),
	(
	    { MaybeImports = no },
	    { Status = error },
	    { Info2 = Info1 }
	;
	    { MaybeImports = yes(Imports) },
	    ( { Imports ^ module_dir \= dir__this_directory } ->
		%
		% Targets from libraries are always
		% considered to be up-to-date if they
		% exist.
		%
		get_target_timestamp(Target, MaybeTimestamp, Info1, Info2),
		(
		    { MaybeTimestamp = ok(_) },
		    { Status = up_to_date }
		;
		    { MaybeTimestamp = error(Error) },
		    { Status = error },
		    io__write_string("** Error: file `"),
		    write_target_file(Target),
		    io__write_string("' not found: "),
		    io__write_string(Error)
	        )
	    ;
		{ Info2 = Info1 },
		{ Status = not_considered }
	    )
	),
	{ Info = Info2 ^ dependency_status ^ elem(Dep) := Status }
    ).

%-----------------------------------------------------------------------------%
