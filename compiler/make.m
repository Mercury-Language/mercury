%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: make.m
% Main author: stayl
%
% A builtin Mercury-specific make replacement.
%
% TODO:
% - `--split-c-files'
% - fix `--target il'
%	- check commands for compilation of foreign code files
%		in mercury_compile.m
%	- fix handling of the `.exe' file
% - library installation
% - transitive inter-module optimization (probably won't bother since
%   that is being rewritten anyway)
% - parallel/distributed builds
%
%-----------------------------------------------------------------------------%
:- module make.

:- interface.

:- include_module make__options_file.

:- import_module parse_tree.

:- import_module make__options_file, parse_tree__modules, parse_tree__prog_io.
:- import_module io, list.

	% make__process_args(OptionArgs, NonOptionArgs).
:- pred make__process_args(list(string)::in, list(file_name)::in,
		io__state::di, io__state::uo) is det.

:- pred make__write_module_dep_file(module_imports::in,
		io__state::di, io__state::uo) is det.

:- func make__module_dep_file_extension = string.

%-----------------------------------------------------------------------------%
:- implementation.

:- include_module make__dependencies, make__module_dep_file.
:- include_module make__module_target, make__program_target, make__util.

:- import_module hlds, libs, backend_libs.
:- import_module top_level. % XXX unwanted dependency

:- import_module make__dependencies, make__module_dep_file.
:- import_module make__module_target, make__program_target, make__util.

:- import_module parse_tree__prog_data, parse_tree__prog_io.
:- import_module parse_tree__modules, parse_tree__mercury_to_mercury.
:- import_module parse_tree__prog_out, parse_tree__prog_io_util.
:- import_module backend_libs__foreign, backend_libs__compile_target_code.
:- import_module libs__timestamp, libs__process_util.
:- import_module libs__globals, libs__options, libs__handle_options.
:- import_module top_level__mercury_compile. % XXX unwanted dependency

:- import_module assoc_list, bool, char, dir, exception, getopt, int, list.
:- import_module map, parser, require, set, std_util, string, term, term_io.

:- type make_info
	---> make_info(
			% The items field of each module_imports
			% structure should be empty -- we're not
			% trying to cache the items here.
		module_dependencies :: map(module_name, maybe(module_imports)),

		file_timestamps :: file_timestamps,

			% The original set of options passed to mmc,
			% not including the targets to be made.
		option_args :: list(string),
		
			% The contents of the Mercury.options file.
		options_variables :: options_variables,

		dependency_status :: map(dependency_file, dependency_status),

			% For each module, the set of modules for
			% which the `.int' files are read, excluding
			% those read as a result of reading `.opt' files.
			% The bool records whether there was an error
			% in the dependencies.
			% XXX Use a better representation for the sets.
		cached_direct_imports :: cached_direct_imports,

			% The boolean is `yes' if the result is complete.
			% XXX Use a better representation for the sets.
		cached_transitive_dependencies ::
				cached_transitive_dependencies,

			% Should the `.module_dep' files be rebuilt.
			% Set to `no' for `mmc --make clean'.
		rebuild_dependencies :: bool,

		keep_going :: bool,

			% Modules for which we have redirected output
			% to a `.err' file during this invocation of mmc.
		error_file_modules :: set(module_name),

			% Used for reporting which module imported
			% a nonexistent module.
		importing_module :: maybe(module_name)
	).

:- type make_error
	--->	target_error(target_file)
	;	dependencies_error(module_name)
	;	other(string)
	.

:- type compilation_task == pair(compilation_task_type, module_name).

:- type compilation_task_type
	--->	process_module(module_compilation_task_type)

			% The `pic' argument is only used for
			% `--target c' and `--target asm'.
	;	target_code_to_object_code(pic)
	.

:- type module_compilation_task_type
	--->	errorcheck
	;	make_short_interface
	;	make_interface
	;	make_private_interface
	;	make_optimization_interface
	;	make_transitive_optimization_interface
	;	compile_to_target_code
	.

:- type module_target_type
	--->	source
	;	errors
	;	private_interface
	;	long_interface
	;	short_interface
	;	unqualified_short_interface
	;	intermodule_interface
	;	aditi_code
	;	c_header(c_header_type)
	;	c_code
	;	il_code
	;	il_asm
	;	java_code
	;	asm_code(pic)
	;	object_code(pic)
	.

:- type c_header_type
	--->	mh	% For `:- pragma export' declarations.
	;	mih	% Declarations for hlc grades, for compiler use only.
	.

% :- type linked_target_type in mercury_compile.m.

:- type misc_target_type
	--->	clean
	;	realclean
	;	build_all(module_target_type)
	;	build_library
	;	install_library
	.

:- type file_timestamps == map(string, maybe_error(timestamp)).

:- type dependency_status
	--->	not_considered
	;	being_built
	;	up_to_date
	;	error
	.

:- type target_file == pair(module_name, module_target_type).
:- type linked_target_file == pair(module_name, linked_target_type).

%-----------------------------------------------------------------------------%

make__write_module_dep_file(Imports) -->
	make__module_dep_file__write_module_dep_file(Imports).

make__module_dep_file_extension = ".module_dep".

make__process_args(OptionArgs, Targets0) -->
    read_options_files(MaybeVariables),
    (
    	{ MaybeVariables = yes(Variables) },
	% Look up the MCFLAGS and GRADEFLAGS from the options file.
    	lookup_mmc_options(Variables, MaybeMCFlags),
        (
		{ MaybeMCFlags = yes(MCFlags) },
		handle_options(MCFlags ++ OptionArgs, MaybeError,
			_, _, _),
		(
			{ MaybeError = yes(OptionsError) },
			usage_error(OptionsError),
			{ Continue0 = no }
		;
			{ MaybeError = no },
			{ Continue0 = yes }
		)
	;
		{ MaybeMCFlags = no },
		{ Continue0 = no }
	)
    ;
	{ MaybeVariables = no },
	{ Variables = options_variables_init },
        { Continue0 = no }
    ),
    (
	{ Continue0 = yes },
	{ Targets0 = [] }
    ->
    	lookup_main_target(Variables, MaybeMAIN_TARGET),
	(
		{ MaybeMAIN_TARGET = yes(Targets) },
		(
			{ Targets = [_ | _] },
			{ Continue = yes }
		;
			{ Targets = [] },
			{ Continue = no },
			io__write_string(
	"** Error: no targets specified and `MAIN_TARGET' not defined.\n")
		)
	;
		{ MaybeMAIN_TARGET = no },
		{ Targets = [] },
		{ Continue = no }
	)
    ;
	{ Continue = Continue0 },
	{ Targets = Targets0 }
    ),
    ( { Continue = no } ->	
	io__set_exit_status(1)
    ;
	{ ShouldRebuildDeps = yes },
	globals__io_lookup_bool_option(keep_going, KeepGoing),
	{ MakeInfo0 = make_info(map__init, map__init,
		OptionArgs, Variables, map__init,
		init_cached_direct_imports,
		init_cached_transitive_dependencies,
		ShouldRebuildDeps, KeepGoing, set__init, no) },

	globals__io_get_globals(Globals),
	foldl2_maybe_stop_at_error(KeepGoing,
	    (pred(TargetStr::in, Success0::out,
	    		Info0::in, Info::out, di, uo) is det -->	
		(
			% Accept and ignore `.depend' targets.
			% `mmc --make' does not need a separate
			% make depend step. The dependencies for
			% each module are regenerated on demand.
			{ string__length(TargetStr, NameLength) },
			{ search_backwards_for_dot(TargetStr,
				NameLength - 1, DotLocn) },
			{ string__split(TargetStr, DotLocn, _, ".depend") }
		->
			{ Success0 = yes },
			{ Info = Info0 }
		;
			{ target_file(Globals, TargetStr,
				ModuleName, TargetType) }
		->
			(
			    { TargetType = module_target(ModuleTargetType) },
			    make_module_target(
			    	target(ModuleName - ModuleTargetType),
			    	Success0, Info0, Info)
			;
			    { TargetType = linked_target(ProgramTargetType) },
			    make_linked_target(
			    	ModuleName - ProgramTargetType, Success0,
			    	Info0, Info)
			;
			    { TargetType = misc_target(MiscTargetType) },
			    make_misc_target(ModuleName - MiscTargetType,
			    	Success0, Info0, Info)
			)
		;
			{ Info = Info0 },
			{ Success0 = no },
			io__write_string("** Unknown target: "),
			io__write_string(TargetStr),
			io__write_string(".\n")
		)
	    ), Targets, Success, MakeInfo0, _MakeInfo),

	( { Success = no } ->
		io__set_exit_status(1)
	;
		[]
	)
    ).

%-----------------------------------------------------------------------------%

:- type target_type
	--->	module_target(module_target_type)
	;	linked_target(linked_target_type)
	;	misc_target(misc_target_type)
	.

:- pred target_file(globals::in, string::in,
		module_name::out, target_type::out) is semidet.

target_file(Globals, FileName, ModuleName, TargetType) :-
    (
	string__length(FileName, NameLength),
	search_backwards_for_dot(FileName, NameLength - 1, DotLocn),
	string__split(FileName, DotLocn, ModuleNameStr0, Suffix),
	solutions(
	    (pred(TargetFile0::out) is nondet :-
		(
			Suffix = target_extension(Globals,
				ModuleTargetType)
		->
			ModuleNameStr = ModuleNameStr0,
			TargetType0 = module_target(ModuleTargetType)
		;
			globals__lookup_string_option(Globals,
				library_extension, Suffix),
			string__append("lib", ModuleNameStr1, ModuleNameStr0)
		->
			ModuleNameStr = ModuleNameStr1,
			TargetType0 = linked_target(static_library)
		;
			globals__lookup_string_option(Globals,
				shared_library_extension, Suffix),
			string__append("lib", ModuleNameStr1, ModuleNameStr0)
		->
			ModuleNameStr = ModuleNameStr1,
			TargetType0 = linked_target(shared_library)
		;
			globals__lookup_string_option(Globals,
				executable_file_extension, Suffix)
		->
			ModuleNameStr = ModuleNameStr0,
			TargetType0 = linked_target(executable)
		;
			string__append(Suffix1, "s", Suffix),
			Suffix1 = target_extension(Globals, ModuleTargetType),

			% Not yet implemented. `build_all' targets
			% are only used by tools/bootcheck, so it
			% doesn't really matter.
			ModuleTargetType \= c_header(_)
		->
			ModuleNameStr = ModuleNameStr0,
			TargetType0 = misc_target(
					build_all(ModuleTargetType))
		;
			Suffix = ".check"
		->
			ModuleNameStr = ModuleNameStr0,
			TargetType0 = misc_target(build_all(errors))
		;
			Suffix = ".clean"
		->
			ModuleNameStr = ModuleNameStr0,
			TargetType0 = misc_target(clean)
		;
			Suffix = ".realclean"
		->
			ModuleNameStr = ModuleNameStr0,
			TargetType0 = misc_target(realclean)
		;
			Suffix = ".install"
		->
			ModuleNameStr = ModuleNameStr0,
			TargetType0 = misc_target(install_library)
		;
			fail
		),
		file_name_to_module_name(ModuleNameStr, ModuleName0),
		TargetFile0 = ModuleName0 - TargetType0
	    ), TargetFiles),
	TargetFiles = [TargetFile]
    ->
	TargetFile = ModuleName - TargetType
    ;
	string__append("lib", ModuleNameStr, FileName)
    ->
	TargetType = misc_target(build_library),
	file_name_to_module_name(ModuleNameStr, ModuleName)
    ;
	globals__lookup_string_option(Globals, executable_file_extension, "")
    ->
	TargetType = linked_target(executable),
	file_name_to_module_name(FileName, ModuleName)
    ;
    	fail
    ).

:- pred search_backwards_for_dot(string::in, int::in, int::out) is semidet.

search_backwards_for_dot(String, Index, DotIndex) :-
	Index >= 0,
	( string__index_det(String, Index, '.') ->
		DotIndex = Index	
	;
		search_backwards_for_dot(String, Index - 1, DotIndex)
	).

%-----------------------------------------------------------------------------%
