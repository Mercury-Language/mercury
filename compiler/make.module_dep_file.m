%-----------------------------------------------------------------------------%
% Copyright (C) 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: make.module_dep_file.m
% Author: stayl
%
% Code to read and write the `<module>.module_dep' files, which contain
% information about inter-module dependencies.
%-----------------------------------------------------------------------------%
:- module make__module_dep_file.

:- interface.

:- import_module parse_tree__modules.
:- import_module std_util, io.

	% Get the dependencies for a given module.
	% Dependencies are generated on demand, not by a `mmc --make depend'
	% command, so this predicate may need to read the source for
	% the module.
:- pred get_module_dependencies(module_name::in, maybe(module_imports)::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

:- pred write_module_dep_file(module_imports::in,
	io__state::di, io__state::uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.

get_module_dependencies(ModuleName, MaybeImports, Info0, Info) -->
	{ RebuildDeps = Info0 ^ rebuild_dependencies },
	(
		{ ModuleName = unqualified(_) }
	->
		get_module_dependencies_2(RebuildDeps, ModuleName, MaybeImports,
			Info0, Info)
	;
		{ map__search(Info0 ^ module_dependencies,
			ModuleName, MaybeImports0) }
	->
		{ MaybeImports = MaybeImports0 },
		{ Info = Info0 }
	;
		%
		% For sub-modules, we need to generate the dependencies
		% for the parent modules first (make_module_dependencies
		% expects to be given the top-level module in a source file).
		% If the module is a nested module, its dependencies will be
		% generated as a side effect of generating the parent's
		% dependencies.
		%
		{ Ancestors = get_ancestors(ModuleName) },
		list__foldl3(
			generate_ancestor_dependencies(RebuildDeps),
				Ancestors, no, Error, Info0, Info1),
		(
			{ Error = yes },
			{ MaybeImports = no },
			{ Info = Info1 ^ module_dependencies
					^ elem(ModuleName) := MaybeImports }
		;
			{ Error = no },
			get_module_dependencies_2(RebuildDeps,
				ModuleName, MaybeImports, Info1, Info)
		)
	).

:- pred generate_ancestor_dependencies(bool::in, module_name::in,
	bool::in, bool::out, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

generate_ancestor_dependencies(_, ModuleName, yes, yes, Info,
		Info ^ module_dependencies ^ elem(ModuleName) := no) --> [].
generate_ancestor_dependencies(RebuildDeps, ModuleName,
		no, Error, Info0, Info) -->
	get_module_dependencies_2(RebuildDeps,
		ModuleName, MaybeImports, Info0, Info),
	{ MaybeImports = yes(_), Error = no
	; MaybeImports = no, Error = yes
	}.

:- pred get_module_dependencies_2(bool::in, module_name::in,
	maybe(module_imports)::out, make_info::in, make_info::out,
	io__state::di, io__state::uo) is det.

get_module_dependencies_2(RebuildDeps, ModuleName,
		MaybeImports, Info0, Info) -->
    (
	{ map__search(Info0 ^ module_dependencies, ModuleName, MaybeImports0) }
    ->
	{ MaybeImports = MaybeImports0 },
	{ Info = Info0 }
    ;
	% We can't just use
	%	`get_target_timestamp(ModuleName - source, ..)'
	% because that could recursively call get_module_dependencies,
	% leading to an infinite loop. Just using module_name_to_file_name
	% will fail if the module name doesn't match the file name, but
	% that case is handled below.
	module_name_to_file_name(ModuleName, ".m", no, SourceFileName),
	get_file_timestamp([dir__this_directory], SourceFileName,
		MaybeSourceFileTimestamp, Info0, Info2),

	module_name_to_file_name(ModuleName, module_dep_file_extension,
			no, DepFileName),
	globals__io_lookup_accumulating_option(search_directories, SearchDirs),
	get_file_timestamp(SearchDirs, DepFileName,
		MaybeDepFileTimestamp, Info2, Info3),

	(
		{ MaybeSourceFileTimestamp = ok(SourceFileTimestamp) },
		{ MaybeDepFileTimestamp = ok(DepFileTimestamp) },
		(
			{ RebuildDeps = no
			; compare((>), DepFileTimestamp, SourceFileTimestamp)
			}
		->
			read_module_dependencies(RebuildDeps,
				ModuleName, Info3, Info6)
		;
			make_module_dependencies(ModuleName, Info3, Info6)
		)
	;
		{ MaybeSourceFileTimestamp = error(_) },
		{ MaybeDepFileTimestamp = ok(DepFileTimestamp) },
		read_module_dependencies(RebuildDeps,
			ModuleName, Info3, Info4),

		%
		% Check for the case where the module name doesn't match
		% the source file name (e.g. parse.m contains module
		% mdb.parse). Get the correct source file name from
		% the module dependency file, then check whether the
		% module dependency file is up to date.
		%
		{ map__lookup(Info4 ^ module_dependencies,
			ModuleName, MaybeImports0) },
		(
		    { MaybeImports0 = yes(Imports0) },
		    { Imports0 ^ module_dir = dir__this_directory }
		->
		    { SourceFileName1 = Imports0 ^ source_file_name },
		    get_file_timestamp([dir__this_directory], SourceFileName1,
		    	MaybeSourceFileTimestamp1, Info4, Info5),
		    (
			{ MaybeSourceFileTimestamp1 =
				ok(SourceFileTimestamp1) },
			(
			    { RebuildDeps = no
			    ; compare((>), DepFileTimestamp,
					SourceFileTimestamp1)
			    }
			->
			    { Info6 = Info5 }
			;
			    make_module_dependencies(ModuleName, Info5, Info6)
			)
		    ;
			{ MaybeSourceFileTimestamp1 = error(Message) },
			io__write_string("** Error reading file `"),
			io__write_string(SourceFileName1),
			io__write_string("' to generate dependencies: "),
			io__write_string(Message),
			io__write_string(".\n"),
		    	{ Info6 = Info5 }
		    )
		;
		    { Info6 = Info4 }
		)
	;
		{ MaybeDepFileTimestamp = error(_)},

		%
		% Try to make the dependencies. This will succeed
		% when the module name doesn't match the file name
		% and the dependencies for this module haven't been
		% built before. It will fail if the source file is
		% in another directory.
		%
		( { RebuildDeps = yes } ->
			make_module_dependencies(ModuleName, Info3, Info6)
		;
			{ Info6 = Info3 ^ module_dependencies
					^ elem(ModuleName) := no }	
		)
	),

	{ MaybeImports1 = Info6 ^ module_dependencies ^ elem(ModuleName) ->
		Info = Info6,
		MaybeImports = MaybeImports1
	;
		MaybeImports = no,
		Info = Info6 ^ module_dependencies ^ elem(ModuleName) := no
    	}
    ).

%-----------------------------------------------------------------------------%

:- func module_dependencies_version_number = int.

module_dependencies_version_number = 1.

write_module_dep_file(Imports0) -->
	% Make sure all the required fields are filled in.
	globals__io_get_globals(Globals),
	{ strip_imported_items(Imports0 ^ items, Items) },
	{ init_dependencies(Imports0 ^ source_file_name,
		Imports0 ^ source_file_module_name,
		Imports0 ^ nested_children, no_module_errors, Globals,
		Imports0 ^ module_name - Items, Imports) },
	do_write_module_dep_file(Imports).

:- pred do_write_module_dep_file(module_imports::in,
	io__state::di, io__state::uo) is det.

do_write_module_dep_file(Imports) -->
	{ ModuleName = Imports ^ module_name },
	module_name_to_file_name(ModuleName, module_dep_file_extension,
		yes, ProgDepFile),
	io__open_output(ProgDepFile, ProgDepResult),
	(
		{ ProgDepResult = ok(ProgDepStream) },
		io__set_output_stream(ProgDepStream, OldOutputStream),
		io__write_string("module("),
		io__write_int(module_dependencies_version_number),
		io__write_string(", """),
		io__write_string(Imports ^ source_file_name),
		io__write_string(""",\n\t"),
		mercury_output_bracketed_sym_name(
			Imports ^ source_file_module_name),
		io__write_string(",\n\t{"),
		io__write_list(Imports ^ parent_deps,
			", ", mercury_output_bracketed_sym_name),
		io__write_string("},\n\t{"),
		io__write_list(Imports ^ int_deps,
			", ", mercury_output_bracketed_sym_name),
		io__write_string("},\n\t{"),
		io__write_list(Imports ^ impl_deps,
			", ", mercury_output_bracketed_sym_name),
		io__write_string("},\n\t{"),
		io__write_list(Imports ^ children,
			", ", mercury_output_bracketed_sym_name),
		io__write_string("},\n\t{"),
		io__write_list(Imports ^ nested_children,
			", ", mercury_output_bracketed_sym_name),
		io__write_string("},\n\t{"),
		io__write_list(Imports ^ fact_table_deps,
			", ", io__write),
		io__write_string("},\n\t{"),
		{
			Imports ^ foreign_code =
				contains_foreign_code(ForeignLanguages0)
		->
			ForeignLanguages = set__to_sorted_list(
						ForeignLanguages0)
		;
			ForeignLanguages = []	
		},
		io__write_list(ForeignLanguages, ", ",
			mercury_output_foreign_language_string),
		io__write_string("},\n\t{"),
		io__write_list(Imports  ^ foreign_import_module_info, ", ",
		    (pred(ForeignImportModule::in, di, uo) is det -->
			{ ForeignImportModule = foreign_import_module(
						Lang, ForeignImport, _) },
			mercury_output_foreign_language_string(Lang),
		    	io__write_string(" - "),
			mercury_output_bracketed_sym_name(ForeignImport)
		    )),
		io__write_string("},\n\t"),
		io__write(Imports ^ contains_foreign_export),
		io__write_string(",\n\t"),
		io__write(Imports ^ has_main),
		io__write_string("\n).\n"),
		io__set_output_stream(OldOutputStream, _),
		io__close_output(ProgDepStream)
	;
		{ ProgDepResult = error(Error) },
		{ io__error_message(Error, Msg) },
		io__write_strings(["Error opening ", ProgDepFile,
			"for output: ", Msg, "\n"]),
		io__set_exit_status(1)
	).

:- pred read_module_dependencies(bool::in, module_name::in, 
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

read_module_dependencies(RebuildDeps, ModuleName, Info0, Info) -->
    module_name_to_file_name(ModuleName, module_dep_file_extension,
    	no, ModuleDepFile),
    globals__io_lookup_accumulating_option(search_directories, SearchDirs),
    io__input_stream(OldInputStream),
    search_for_file_returning_dir(SearchDirs, ModuleDepFile, SearchResult),
    ( { SearchResult = ok(ModuleDir) } ->
	parser__read_term(ImportsTermResult),
	io__set_input_stream(OldInputStream, ModuleDepStream),
	io__close_input(ModuleDepStream),
	( 
		{ ImportsTermResult = term(_, ImportsTerm) },
		{ ImportsTerm = term__functor(term__atom("module"),
				ModuleArgs, _) },
		{ ModuleArgs = [
			VersionNumberTerm,
			SourceFileTerm,
			SourceFileModuleNameTerm,
			ParentsTerm,
			IntDepsTerm,
			ImplDepsTerm,
			ChildrenTerm,
			NestedChildrenTerm,
			FactDepsTerm,
			ForeignLanguagesTerm,
			ForeignImportsTerm,
			ContainsForeignExportTerm,
			HasMainTerm
		] },
		{ VersionNumberTerm = term__functor(
		    term__integer(module_dependencies_version_number),
		    [], _) },
		{ SourceFileTerm = term__functor(
			term__string(SourceFileName), [], _) },
		{ sym_name_and_args(SourceFileModuleNameTerm,
			SourceFileModuleName, []) },
		{ parse_sym_name_list(ParentsTerm, Parents) },
		{ parse_sym_name_list(IntDepsTerm, IntDeps) },
		{ parse_sym_name_list(ImplDepsTerm, ImplDeps) },
		{ parse_sym_name_list(ChildrenTerm, Children) },
		{ parse_sym_name_list(NestedChildrenTerm, NestedChildren) },
		{ FactDepsTerm = term__functor(term__atom("{}"),
					FactDepsStrings, _) },
		{ list__map(
		    (pred(StringTerm::in, String::out) is semidet :-
			StringTerm = term__functor(
				term__string(String), [], _)
		    ), FactDepsStrings, FactDeps) },
		{ ForeignLanguagesTerm = term__functor(
			term__atom("{}"), ForeignLanguagesTerms, _) },
		{ list__map(
		    (pred(LanguageTerm::in,
				Language::out) is semidet :-
			LanguageTerm = term__functor(
				term__string(LanguageString), [], _),
			globals__convert_foreign_language(
				LanguageString, Language)
		    ), ForeignLanguagesTerms, ForeignLanguages) },
		{ ForeignImportsTerm = term__functor(term__atom("{}"),
					ForeignImportTerms, _) },
		{ list__map(
		    (pred(ForeignImportTerm::in,
				ForeignImportModule::out) is semidet :-
			ForeignImportTerm = term__functor(term__atom("-"),
				[LanguageTerm, ImportedModuleTerm], _),
			LanguageTerm = term__functor(
				term__string(LanguageString), [], _),
			globals__convert_foreign_language(LanguageString,
				Language),
			sym_name_and_args(ImportedModuleTerm,
				ImportedModuleName, []),
			ForeignImportModule = foreign_import_module(
				Language, ImportedModuleName,
				term__context_init)
		    ), ForeignImportTerms, ForeignImports) },

		{ ContainsForeignExportTerm =
			term__functor(term__atom(ContainsForeignExportStr),
				[], _) },
		{ ContainsForeignExportStr = "contains_foreign_export",
			ContainsForeignExport = contains_foreign_export
		; ContainsForeignExportStr = "no_foreign_export",
			ContainsForeignExport = no_foreign_export
		},

		{ HasMainTerm = term__functor(term__atom(HasMainStr),
			[], _) },
		{ HasMainStr = "has_main", HasMain = has_main
		; HasMainStr = "no_main", HasMain = no_main
		}
	->
		{ ForeignLanguages = [] ->
			ContainsForeignCode = no_foreign_code
		;
			ContainsForeignCode = contains_foreign_code(
				set__list_to_set(ForeignLanguages))
		},
		{ Imports = module_imports(SourceFileName,
			SourceFileModuleName, ModuleName, Parents,
			IntDeps, ImplDeps, [], [], Children,
			NestedChildren, FactDeps, ContainsForeignCode,
			ForeignImports, ContainsForeignExport,
			[], no_module_errors, no, HasMain, ModuleDir) },
		{ Info1 = Info0 ^ module_dependencies
				^ elem(ModuleName) := yes(Imports) },

		%
		% Read the dependencies for the nested children.
		% If something goes wrong (for example one of the
		% files was removed), the dependencies for all
		% modules in the source file will be remade
		% (make_module_dependencies expects to be given
		% the top-level module in the source file).
		%
		{ SubRebuildDeps = no },
		list__foldl2(read_module_dependencies(SubRebuildDeps),
			NestedChildren, Info1, Info2),
		(
			{ list__member(NestedChild, NestedChildren) },
			{
				map__search(Info2 ^ module_dependencies,
					NestedChild, ChildImports)
			->
				ChildImports = no	
			;
				true
			}
		->
			read_module_dependencies_remake(RebuildDeps,
				ModuleName, "error in nested sub-modules",
				Info2, Info)
		;
			{ Info = Info2 }
		)
	;
		read_module_dependencies_remake(RebuildDeps,
			ModuleName, "parse error", Info0, Info)
	)
    ;
	read_module_dependencies_remake(RebuildDeps, ModuleName,
		"couldn't find `.module_dep' file", Info0, Info)
    ).

	% Something went wrong reading the dependencies, so just rebuild them.
:- pred read_module_dependencies_remake(bool::in, module_name::in, string::in,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

read_module_dependencies_remake(RebuildDeps, ModuleName, Msg, Info0, Info) -->
	( { RebuildDeps = yes } ->
		module_name_to_file_name(ModuleName,
			module_dep_file_extension, no, ModuleDepsFile),
		debug_msg(
		    (pred(di, uo) is det -->
			io__write_string("Error reading file `"),
			io__write_string(ModuleDepsFile),
			io__write_string("rebuilding: "),
			io__write_string(Msg),
			io__nl
		    )),
		make_module_dependencies(ModuleName, Info0, Info)
	;
		{ Info = Info0 }
	).

:- pred parse_sym_name_list(term::in, list(sym_name)::out) is semidet.

parse_sym_name_list(term__functor(term__atom("{}"), Args, _), SymNames) :-
	list__map(
		(pred(Arg::in, SymName::out) is semidet :-
			sym_name_and_args(Arg, SymName, [])
		), Args, SymNames).

	% The module_name given must be the top level module in
	% the source file. get_module_dependencies ensures this by
	% making the dependencies for all parent modules of the
	% requested module first.
:- pred make_module_dependencies(module_name::in, make_info::in,
		make_info::out, io__state::di, io__state::uo) is det.

make_module_dependencies(ModuleName, Info0, Info) -->
    { Search = no },
    { ReturnTimestamp = yes },

    redirect_output(ModuleName, MaybeErrorStream, Info0, Info1),
    (
	{ MaybeErrorStream = yes(ErrorStream) },
	io__set_output_stream(ErrorStream, OldOutputStream),
	read_mod(ModuleName, ".m",
		"Getting dependencies for module", Search, ReturnTimestamp,
		Items, Error, SourceFileName, _),
	( { Error = fatal_module_errors } ->
	    io__set_output_stream(OldOutputStream, _),
	    io__write_string("** Error: error reading file `"),
	    io__write_string(SourceFileName),
	    io__write_string("' to generate dependencies.\n"),

	    % Display the contents of the `.err' file, then remove it
	    % so we don't leave `.err' files lying around for nonexistent
	    % modules.
	    globals__io_lookup_int_option(output_compile_error_lines, Lines),
	    globals__io_set_option(output_compile_error_lines, int(10000)),
	    unredirect_output(ModuleName, ErrorStream, Info1, Info2),
	    globals__io_set_option(output_compile_error_lines, int(Lines)),
	    module_name_to_file_name(ModuleName, ".err", no, ErrFileName),
	    io__remove_file(ErrFileName, _),
	    { Info = Info2 ^ module_dependencies ^ elem(ModuleName) := no }
	;
	    io__set_exit_status(0),
	    io__set_output_stream(ErrorStream, _),
	    split_into_submodules(ModuleName, Items, SubModuleList),
	    io__set_output_stream(OldOutputStream, _),

	    globals__io_get_globals(Globals),
	    { assoc_list__keys(SubModuleList, SubModuleNames) },
	    { list__map(
		init_dependencies(SourceFileName, ModuleName,
			SubModuleNames, Error, Globals),
		SubModuleList, ModuleImportList) },
	    { list__foldl(
		(pred(ModuleImports::in, in, out) is det -->
		    { SubModuleName = ModuleImports ^ module_name },
		    ^ module_dependencies ^ elem(SubModuleName)
			:= yes(ModuleImports)
		), ModuleImportList, Info1, Info2) },

	    %
	    % If there were no errors, write out the `.int3' file
	    % while we have the contents of the module. The `int3'
	    % file doesn't depend on anything else.
	    %
	    ( { Error = no_module_errors } ->
		{ Target = ModuleName - unqualified_short_interface },
		maybe_make_target_message(OldOutputStream, Target),
		build_with_check_for_interrupt(
		    build_with_module_options(ModuleName,
			["--make-short-interface"],
			make_short_interfaces(ErrorStream,
				SourceFileName, SubModuleList)), 
		    cleanup_short_interfaces(SubModuleNames),
		    Succeeded, Info2, Info3)
	    ;
		{ Info3 = Info2 },
		{ Succeeded = no }
	    ),

	    build_with_check_for_interrupt(
	    	(pred(yes::out, MakeInfo::in, MakeInfo::out, di, uo) is det -->
	   		list__foldl(do_write_module_dep_file,
				ModuleImportList)
	  	), cleanup_module_dep_files(SubModuleNames),
		_, Info3, Info5),

	    record_made_target(ModuleName - unqualified_short_interface,
		process_module(make_short_interface), Succeeded, Info5, Info6),

	    unredirect_output(ModuleName, ErrorStream,
	    	Info6, Info)
	)
    ;
	{ MaybeErrorStream = no },
	{ Info = Info1 }
    ).

:- pred make_short_interfaces(io__output_stream::in, file_name::in,
	assoc_list(module_name, item_list)::in, list(string)::in, bool::out,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.
		
make_short_interfaces(ErrorStream, SourceFileName, SubModuleList,
		_, Succeeded, Info, Info) --> 
	io__set_output_stream(ErrorStream, OutputStream),
	list__foldl(
	    (pred(SubModule::in, di, uo) is det -->
		    { SubModule = SubModuleName - SubModuleItems },
		    modules__make_short_interface(SourceFileName,
			SubModuleName, SubModuleItems)
	    ), SubModuleList),
	io__set_output_stream(OutputStream, _),
	io__get_exit_status(ExitStatus),
	{ Succeeded = ( ExitStatus = 0 -> yes ; no ) }.

:- pred cleanup_short_interfaces(list(module_name)::in,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

cleanup_short_interfaces(SubModuleNames, Info0, Info) -->
	list__foldl2(
	    (pred(SubModuleName::in, Info1::in, Info2::out, di, uo) is det -->
		remove_target_file(SubModuleName, unqualified_short_interface,
			Info1, Info2)
	    ), SubModuleNames, Info0, Info).

:- pred cleanup_module_dep_files(list(module_name)::in,
	make_info::in, make_info::out, io__state::di, io__state::uo) is det.

cleanup_module_dep_files(SubModuleNames, Info0, Info) -->
	list__foldl2(
	    (pred(SubModuleName::in, Info1::in, Info2::out, di, uo) is det -->
		remove_file(SubModuleName, module_dep_file_extension,
			Info1, Info2)
	    ), SubModuleNames, Info0, Info).

%-----------------------------------------------------------------------------%
