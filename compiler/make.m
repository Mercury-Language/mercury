%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
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
% - transitive inter-module optimization (probably won't bother since
%   that is being rewritten anyway)
% - parallel/distributed builds
%
%-----------------------------------------------------------------------------%
:- module make.

:- interface.

:- include_module make__options_file.
:- include_module make__util.

:- import_module make__options_file.
:- import_module mdbcomp.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_io.

:- import_module io, list.

	% make__process_args(OptionArgs, NonOptionArgs).
:- pred make__process_args(options_variables::in, list(string)::in,
	list(file_name)::in, io::di, io::uo) is det.

:- pred make__write_module_dep_file(module_imports::in,
	io::di, io::uo) is det.

:- func make__module_dep_file_extension = string.

:- type make_info.

%-----------------------------------------------------------------------------%
:- implementation.

:- include_module make__dependencies.
:- include_module make__module_dep_file.
:- include_module make__module_target.
:- include_module make__program_target.

:- import_module hlds.
:- import_module libs.
:- import_module backend_libs.
:- import_module top_level. % XXX unwanted dependency

:- import_module make__dependencies.
:- import_module make__module_dep_file.
:- import_module make__module_target.
:- import_module make__program_target.
:- import_module make__util.

:- import_module backend_libs__compile_target_code.
:- import_module backend_libs__foreign.
:- import_module libs__globals.
:- import_module libs__handle_options.
:- import_module libs__options.
:- import_module libs__process_util.
:- import_module libs__timestamp.
:- import_module parse_tree__error_util.
:- import_module parse_tree__mercury_to_mercury.
:- import_module parse_tree__modules.
:- import_module parse_tree__prog_data.
:- import_module parse_tree__prog_io.
:- import_module parse_tree__prog_io_util.
:- import_module parse_tree__prog_out.
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
		importing_module :: maybe(module_name),

			% Targets specified on the command line.
		command_line_targets :: set(pair(module_name, target_type))

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
	;	foreign_code_to_object_code(pic, foreign_language)
	;	fact_table_code_to_object_code(pic, file_name)
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
	;	foreign_il_asm(foreign_language)
	;	foreign_object(pic, foreign_language)
	;	fact_table_object(pic, file_name)
	.

:- type c_header_type
	--->	mh	% For `:- pragma export' declarations.
	;	mih	% Declarations for hlc grades, for compiler use only.
	.

% :- type linked_target_type in compile_target_code.m.

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

make__process_args(Variables, OptionArgs, Targets0) -->
    (
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
	{ Continue = yes },
	{ Targets = Targets0 }
    ),
    ( { Continue = no } ->
	io__set_exit_status(1)
    ;
	globals__io_lookup_bool_option(keep_going, KeepGoing),
	globals__io_get_globals(Globals),

	%
	% Accept and ignore `.depend' targets.
	% `mmc --make' does not need a separate
	% make depend step. The dependencies for
	% each module are regenerated on demand.
	%
	{ NonDependTargets = list__filter(
		(pred(Target::in) is semidet :-
			\+ string__remove_suffix(Target, ".depend", _)
		), Targets) },

	%
	% Classify the remaining targets.
	%
	{ list__map(classify_target(Globals), NonDependTargets,
		ClassifiedTargets) },

	{ ShouldRebuildDeps = yes },
	{ MakeInfo0 = make_info(map__init, map__init,
		OptionArgs, Variables, map__init,
		init_cached_direct_imports,
		init_cached_transitive_dependencies,
		ShouldRebuildDeps, KeepGoing,
		set__init, no, set__list_to_set(ClassifiedTargets)) },

	%
	% Build the targets, stopping on any errors if
	% `--keep-going' was not set.
	%
	foldl2_maybe_stop_at_error(KeepGoing,
		(pred(Target::in, Success0::out, Info0::in, Info::out,
		    		di, uo) is det -->
		    { Target = ModuleName - TargetType },
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
		), ClassifiedTargets, Success, MakeInfo0, _MakeInfo),

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

:- pred classify_target(globals::in, string::in,
		pair(module_name, target_type)::out) is det.

classify_target(Globals, FileName, ModuleName - TargetType) :-
    (
	string__length(FileName, NameLength),
	search_backwards_for_dot(FileName, NameLength - 1, DotLocn),
	string__split(FileName, DotLocn, ModuleNameStr0, Suffix),
	solutions(
	    (pred(TargetFile0::out) is nondet :-
		(
			yes(Suffix) = target_extension(Globals,
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
			yes(Suffix1) = target_extension(Globals,
						ModuleTargetType),

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
			Suffix = ".install",
			string__append("lib", ModuleNameStr1, ModuleNameStr0)
		->
			ModuleNameStr = ModuleNameStr1,
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
	TargetType = linked_target(executable),
	file_name_to_module_name(FileName, ModuleName)
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
