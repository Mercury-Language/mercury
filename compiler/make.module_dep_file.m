%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make.module_dep_file.m.
% Author: stayl.
%
% Code to read and write the `<module>.module_dep' files, which contain
% information about inter-module dependencies.
%
%-----------------------------------------------------------------------------%

:- module make.module_dep_file.
:- interface.

:- import_module libs.globals.
:- import_module mdbcomp.prim_data.

:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Get the dependencies for a given module.
    % Dependencies are generated on demand, not by a `mmc --make depend'
    % command, so this predicate may need to read the source for
    % the module.
    %
:- pred get_module_dependencies(globals::in, module_name::in,
    maybe(module_and_imports)::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

:- pred write_module_dep_file(globals::in, module_and_imports::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.file_util.
:- import_module libs.process_util.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.read_modules.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_sym_name.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module cord.
:- import_module dir.
:- import_module getopt_io.
:- import_module parser.
:- import_module term.
:- import_module term_io.

    % The version 1 module_dep file format is the same as version 2 except that
    % it does not include a list of files included by `pragma foreign_decl' and
    % `pragma foreign_code'. We continue to write version 1 files when
    % possible.
    %
:- type module_dep_file_version
    --->    module_dep_file_v1
    ;       module_dep_file_v2.

:- pred version_number(module_dep_file_version, int).
:- mode version_number(in, out) is det.
:- mode version_number(out, in) is semidet.

version_number(module_dep_file_v1, 1).
version_number(module_dep_file_v2, 2).

%-----------------------------------------------------------------------------%

get_module_dependencies(Globals, ModuleName, MaybeImports, !Info, !IO) :-
    RebuildModuleDeps = !.Info ^ rebuild_module_deps,
    (
        ModuleName = unqualified(_),
        maybe_get_module_dependencies(Globals, RebuildModuleDeps, ModuleName,
            MaybeImports, !Info, !IO)
    ;
        ModuleName = qualified(_, _),
        (
            map.search(!.Info ^ module_dependencies, ModuleName,
                MaybeImportsPrime)
        ->
            MaybeImports = MaybeImportsPrime
        ;
            % For sub-modules, we need to generate the dependencies
            % for the parent modules first (make_module_dependencies
            % expects to be given the top-level module in a source file).
            % If the module is a nested module, its dependencies will be
            % generated as a side effect of generating the parent's
            % dependencies.

            Ancestors = get_ancestors(ModuleName),
            list.foldl3(
                generate_ancestor_dependencies(Globals, RebuildModuleDeps),
                Ancestors, no, Error, !Info, !IO),
            (
                Error = yes,
                MaybeImports = no,
                ModuleDepMap0 = !.Info ^ module_dependencies,
                % XXX Could this be map.det_update or map.det_insert?
                map.set(ModuleName, MaybeImports, ModuleDepMap0, ModuleDepMap),
                !Info ^ module_dependencies := ModuleDepMap
            ;
                Error = no,
                maybe_get_module_dependencies(Globals, RebuildModuleDeps,
                    ModuleName, MaybeImports, !Info, !IO)
            )
        )
    ).

:- pred generate_ancestor_dependencies(globals::in, rebuild_module_deps::in,
    module_name::in, bool::in, bool::out, make_info::in, make_info::out,
    io::di, io::uo) is det.

generate_ancestor_dependencies(_, _, ModuleName, yes, yes, Info,
        Info ^ module_dependencies ^ elem(ModuleName) := no, !IO).
generate_ancestor_dependencies(Globals, RebuildModuleDeps, ModuleName,
        no, Error, !Info, !IO) :-
    maybe_get_module_dependencies(Globals, RebuildModuleDeps, ModuleName,
        MaybeImports, !Info, !IO),
    (
        MaybeImports = yes(_),
        Error = no
    ;
        MaybeImports = no,
        Error = yes
    ).

:- pred maybe_get_module_dependencies(globals::in, rebuild_module_deps::in,
    module_name::in, maybe(module_and_imports)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

maybe_get_module_dependencies(Globals, RebuildModuleDeps, ModuleName,
        MaybeImports, !Info, !IO) :-
    ( map.search(!.Info ^ module_dependencies, ModuleName, MaybeImports0) ->
        MaybeImports = MaybeImports0
    ;
        do_get_module_dependencies(Globals, RebuildModuleDeps, ModuleName,
            MaybeImports, !Info, !IO)
    ).

:- pred do_get_module_dependencies(globals::in, rebuild_module_deps::in,
    module_name::in, maybe(module_and_imports)::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

do_get_module_dependencies(Globals, RebuildModuleDeps, ModuleName,
        !:MaybeImports, !Info, !IO) :-
    % We can't just use
    %   `get_target_timestamp(ModuleName - source, ..)'
    % because that could recursively call get_module_dependencies,
    % leading to an infinite loop. Just using module_name_to_file_name
    % will fail if the module name doesn't match the file name, but
    % that case is handled below.
    module_name_to_file_name(Globals, ModuleName, ".m", do_not_create_dirs,
        SourceFileName, !IO),
    get_file_timestamp([dir.this_directory], SourceFileName,
        MaybeSourceFileTimestamp, !Info, !IO),

    module_name_to_file_name(Globals, ModuleName,
        make_module_dep_file_extension, do_not_create_dirs, DepFileName, !IO),
    globals.lookup_accumulating_option(Globals, search_directories,
        SearchDirs),
    get_file_timestamp(SearchDirs, DepFileName, MaybeDepFileTimestamp,
        !Info, !IO),
    (
        MaybeSourceFileTimestamp = ok(SourceFileTimestamp),
        MaybeDepFileTimestamp = ok(DepFileTimestamp),
        (
            ( RebuildModuleDeps = do_not_rebuild_module_deps
            ; compare((>), DepFileTimestamp, SourceFileTimestamp)
            )
        ->
            % Since the source file was found in this directory, don't
            % use module_dep files which might be for installed copies
            % of the module.
            read_module_dependencies_no_search(Globals, RebuildModuleDeps,
                ModuleName, !Info, !IO)
        ;
            make_module_dependencies(Globals, ModuleName, !Info, !IO)
        )
    ;
        MaybeSourceFileTimestamp = error(_),
        MaybeDepFileTimestamp = ok(DepFileTimestamp),
        read_module_dependencies_search(Globals, RebuildModuleDeps,
            ModuleName, !Info, !IO),

        % Check for the case where the module name doesn't match the
        % source file name (e.g. parse.m contains module mdb.parse). Get
        % the correct source file name from the module dependency file,
        % then check whether the module dependency file is up to date.

        map.lookup(!.Info ^ module_dependencies, ModuleName, !:MaybeImports),
        (
            !.MaybeImports = yes(Imports0),
            Imports0 ^ mai_module_dir = dir.this_directory
        ->
            SourceFileName1 = Imports0 ^ mai_source_file_name,
            get_file_timestamp([dir.this_directory], SourceFileName1,
                MaybeSourceFileTimestamp1, !Info, !IO),
            (
                MaybeSourceFileTimestamp1 = ok(SourceFileTimestamp1),
                (
                    ( RebuildModuleDeps = do_not_rebuild_module_deps
                    ; compare((>), DepFileTimestamp, SourceFileTimestamp1)
                    )
                ->
                    true
                ;
                    make_module_dependencies(Globals, ModuleName, !Info, !IO)
                )
            ;
                MaybeSourceFileTimestamp1 = error(Message),
                io.write_string("** Error reading file `", !IO),
                io.write_string(SourceFileName1, !IO),
                io.write_string("' to generate dependencies: ", !IO),
                io.write_string(Message, !IO),
                io.write_string(".\n", !IO),
                maybe_write_importing_module(ModuleName,
                    !.Info ^ importing_module, !IO)
            )
        ;
            true
        )
    ;
        MaybeDepFileTimestamp = error(_),

        % Try to make the dependencies. This will succeed when the module name
        % doesn't match the file name and the dependencies for this module
        % haven't been built before. It will fail if the source file
        % is in another directory.
        (
            RebuildModuleDeps = do_rebuild_module_deps,
            make_module_dependencies(Globals, ModuleName, !Info, !IO)
        ;
            RebuildModuleDeps = do_not_rebuild_module_deps,
            ModuleDepMap0 = !.Info ^ module_dependencies,
            % XXX Could this be map.det_update or map.det_insert?
            map.set(ModuleName, no, ModuleDepMap0, ModuleDepMap1),
            !Info ^ module_dependencies := ModuleDepMap1
        )
    ),
    ModuleDepMap2 = !.Info ^ module_dependencies,
    ( map.search(ModuleDepMap2, ModuleName, MaybeImportsPrime) ->
        !:MaybeImports = MaybeImportsPrime
    ;
        !:MaybeImports = no,
        map.det_insert(ModuleName, no, ModuleDepMap2, ModuleDepMap),
        !Info ^ module_dependencies := ModuleDepMap
    ).

%-----------------------------------------------------------------------------%

write_module_dep_file(Globals, Imports0, !IO) :-
    % Make sure all the required fields are filled in.
    module_and_imports_get_results(Imports0, Items0, _Specs, _Errors),
    strip_imported_items(Items0, Items),
    init_dependencies(Imports0 ^ mai_source_file_name,
        Imports0 ^ mai_source_file_module_name,
        Imports0 ^ mai_nested_children,
        Imports0 ^ mai_specs, no_module_errors, Globals,
        Imports0 ^ mai_module_name - Items, Imports),
    do_write_module_dep_file(Globals, Imports, !IO).

:- pred do_write_module_dep_file(globals::in, module_and_imports::in,
    io::di, io::uo) is det.

do_write_module_dep_file(Globals, Imports, !IO) :-
    ModuleName = Imports ^ mai_module_name,
    module_name_to_file_name(Globals, ModuleName,
        make_module_dep_file_extension, do_create_dirs, ProgDepFile, !IO),
    io.open_output(ProgDepFile, ProgDepResult, !IO),
    (
        ProgDepResult = ok(ProgDepStream),
        io.set_output_stream(ProgDepStream, OldOutputStream, !IO),
        choose_module_dep_file_version(Imports, Version),
        do_write_module_dep_file_2(Imports, Version, !IO),
        io.set_output_stream(OldOutputStream, _, !IO),
        io.close_output(ProgDepStream, !IO)
    ;
        ProgDepResult = error(Error),
        io.error_message(Error, Msg),
        io.write_strings(["Error opening ", ProgDepFile,
            " for output: ", Msg, "\n"], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred choose_module_dep_file_version(module_and_imports::in,
    module_dep_file_version::out) is det.

choose_module_dep_file_version(Imports, Version) :-
    ForeignIncludeFiles = Imports ^ mai_foreign_include_files,
    (
        ForeignIncludeFiles = [],
        Version = module_dep_file_v1
    ;
        ForeignIncludeFiles = [_ | _],
        Version = module_dep_file_v2
    ).

:- pred do_write_module_dep_file_2(module_and_imports::in,
    module_dep_file_version::in, io::di, io::uo) is det.

do_write_module_dep_file_2(Imports, Version, !IO) :-
    io.write_string("module(", !IO),
    version_number(Version, VersionNumber),
    io.write_int(VersionNumber, !IO),
    io.write_string(", """, !IO),
    io.write_string(Imports ^ mai_source_file_name, !IO),
    io.write_string(""",\n\t", !IO),
    mercury_output_bracketed_sym_name(
        Imports ^ mai_source_file_module_name, !IO),
    io.write_string(",\n\t{", !IO),
    io.write_list(Imports ^ mai_parent_deps,
        ", ", mercury_output_bracketed_sym_name, !IO),
    io.write_string("},\n\t{", !IO),
    io.write_list(Imports ^ mai_int_deps,
        ", ", mercury_output_bracketed_sym_name, !IO),
    io.write_string("},\n\t{", !IO),
    io.write_list(Imports ^ mai_impl_deps,
        ", ", mercury_output_bracketed_sym_name, !IO),
    io.write_string("},\n\t{", !IO),
    io.write_list(Imports ^ mai_children,
        ", ", mercury_output_bracketed_sym_name, !IO),
    io.write_string("},\n\t{", !IO),
    io.write_list(Imports ^ mai_nested_children,
        ", ", mercury_output_bracketed_sym_name, !IO),
    io.write_string("},\n\t{", !IO),
    io.write_list(Imports ^ mai_fact_table_deps,
        ", ", io.write, !IO),
    io.write_string("},\n\t{", !IO),
    ( Imports ^ mai_has_foreign_code = contains_foreign_code(LangList) ->
        ForeignLanguages = set.to_sorted_list(LangList)
    ;
        ForeignLanguages = []
    ),
    io.write_list(ForeignLanguages,
        ", ", mercury_output_foreign_language_string, !IO),
    io.write_string("},\n\t{", !IO),
    io.write_list(Imports ^ mai_foreign_import_modules,
        ", ", write_foreign_import_module_info, !IO),
    io.write_string("},\n\t", !IO),
    contains_foreign_export_to_string(Imports ^ mai_contains_foreign_export,
        ContainsForeignExportStr),
    io.write_string(ContainsForeignExportStr, !IO),
    io.write_string(",\n\t", !IO),
    has_main_to_string(Imports ^ mai_has_main, HasMainStr),
    io.write_string(HasMainStr, !IO),
    (
        Version = module_dep_file_v1
    ;
        Version = module_dep_file_v2,
        io.write_string(",\n\t{", !IO),
        io.write_list(list.reverse(Imports ^ mai_foreign_include_files),
            ", ", write_foreign_include_file_info, !IO),
        io.write_string("}", !IO)
    ),
    io.write_string("\n).\n", !IO).

:- pred write_foreign_import_module_info(foreign_import_module_info::in,
    io::di, io::uo) is det.

write_foreign_import_module_info(ForeignImportModule, !IO) :-
    ForeignImportModule = foreign_import_module_info(Lang, ForeignImport, _),
    mercury_output_foreign_language_string(Lang, !IO),
    io.write_string(" - ", !IO),
    mercury_output_bracketed_sym_name(ForeignImport, !IO).

:- pred write_foreign_include_file_info(foreign_include_file_info::in,
    io::di, io::uo) is det.

write_foreign_include_file_info(ForeignInclude, !IO) :-
    ForeignInclude = foreign_include_file_info(Lang, FileName),
    mercury_output_foreign_language_string(Lang, !IO),
    io.write_string(" - ", !IO),
    term_io.quote_string(FileName, !IO).

:- pred contains_foreign_export_to_string(contains_foreign_export, string).
:- mode contains_foreign_export_to_string(in, out) is det.
:- mode contains_foreign_export_to_string(out, in) is semidet.

contains_foreign_export_to_string(ContainsForeignExport,
        ContainsForeignExportStr) :-
    (
        ContainsForeignExport = contains_foreign_export,
        ContainsForeignExportStr = "contains_foreign_export"
    ;
        ContainsForeignExport = contains_no_foreign_export,
        % Yes, without the "contains_" prefix.  Don't change it unless you mean
        % to break compatibility with older .module_dep files.
        ContainsForeignExportStr = "no_foreign_export"
    ).

:- pred has_main_to_string(has_main, string).
:- mode has_main_to_string(in, out) is det.
:- mode has_main_to_string(out, in) is semidet.

has_main_to_string(HasMain, HasMainStr) :-
    (
        HasMain = has_main,
        HasMainStr = "has_main"
    ;
        HasMain = no_main,
        HasMainStr = "no_main"
    ).

%-----------------------------------------------------------------------------%

:- pred read_module_dependencies_search(globals::in, rebuild_module_deps::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

read_module_dependencies_search(Globals, RebuildModuleDeps, ModuleName,
        !Info, !IO) :-
    globals.lookup_accumulating_option(Globals, search_directories,
        SearchDirs),
    read_module_dependencies_2(Globals, RebuildModuleDeps, SearchDirs,
        ModuleName, !Info, !IO).

:- pred read_module_dependencies_no_search(globals::in,
    rebuild_module_deps::in, module_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

read_module_dependencies_no_search(Globals, RebuildModuleDeps, ModuleName,
        !Info, !IO) :-
    read_module_dependencies_2(Globals, RebuildModuleDeps,
        [dir.this_directory], ModuleName, !Info, !IO).

:- pred read_module_dependencies_2(globals::in, rebuild_module_deps::in,
    list(dir_name)::in, module_name::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

read_module_dependencies_2(Globals, RebuildModuleDeps, SearchDirs, ModuleName,
        !Info, !IO) :-
    module_name_to_search_file_name(Globals, ModuleName,
        make_module_dep_file_extension, ModuleDepFile, !IO),
    io.input_stream(OldInputStream, !IO),
    search_for_file_returning_dir(open_file, SearchDirs, ModuleDepFile,
        SearchResult, !IO),
    (
        SearchResult = ok(ModuleDir),
        parser.read_term(TermResult, !IO),
        io.set_input_stream(OldInputStream, ModuleDepStream, !IO),
        io.close_input(ModuleDepStream, !IO),
        (
            TermResult = term(_, Term),
            read_module_dependencies_3(Globals, SearchDirs, ModuleName,
                ModuleDir, Term, Result, !Info, !IO)
        ;
            TermResult = eof,
            Result = error("unexpected eof")
        ;
            TermResult = error(ParseError, _),
            Result = error("parse error: " ++ ParseError)
        ),
        (
            Result = ok
        ;
            Result = error(Msg),
            read_module_dependencies_remake(Globals, RebuildModuleDeps,
                ModuleName, Msg, !Info, !IO)
        )
    ;
        SearchResult = error(_),
        % XXX should use the error message.
        read_module_dependencies_remake(Globals, RebuildModuleDeps, ModuleName,
            "couldn't find `.module_dep' file", !Info, !IO)
    ).

:- pred read_module_dependencies_3(globals::in, list(dir_name)::in,
    module_name::in, dir_name::in, term::in, maybe_error::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

read_module_dependencies_3(Globals, SearchDirs, ModuleName, ModuleDir,
        Term, Result, !Info, !IO) :-
    (
        atom_term(Term, "module", ModuleArgs),
        ModuleArgs = [
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
            | ModuleArgsTail
        ],

        version_number_term(VersionNumberTerm, Version),
        string_term(SourceFileTerm, SourceFileName),
        try_parse_sym_name_and_no_args(SourceFileModuleNameTerm,
            SourceFileModuleName),

        sym_names_term(ParentsTerm, Parents),
        sym_names_term(IntDepsTerm, IntDeps),
        sym_names_term(ImplDepsTerm, ImplDeps),
        sym_names_term(ChildrenTerm, Children),
        sym_names_term(NestedChildrenTerm, NestedChildren),

        braces_term(fact_dep_term, FactDepsTerm, FactDeps),
        braces_term(foreign_language_term, ForeignLanguagesTerm,
            ForeignLanguages),
        braces_term(foreign_import_term, ForeignImportsTerm, ForeignImports),

        contains_foreign_export_term(ContainsForeignExportTerm,
            ContainsForeignExport),

        has_main_term(HasMainTerm, HasMain),

        (
            Version = module_dep_file_v1,
            ModuleArgsTail = [],
            ForeignIncludes = []
        ;
            Version = module_dep_file_v2,
            ModuleArgsTail = [ForeignIncludesTerm],
            braces_term(foreign_include_term, ForeignIncludesTerm,
                ForeignIncludes)
        )
    ->
        ContainsForeignCode = contains_foreign_code(ForeignLanguages),
        IndirectDeps = [],
        PublicChildren = [],
        Items = cord.empty,
        Specs = [],
        Errors = no_module_errors,
        MaybeTimestamps = no,
        Imports = module_and_imports(SourceFileName, SourceFileModuleName,
            ModuleName, Parents, IntDeps, ImplDeps, IndirectDeps,
            Children, PublicChildren, NestedChildren, FactDeps,
            ContainsForeignCode, ForeignImports, ForeignIncludes,
            ContainsForeignExport,
            Items, Specs, Errors, MaybeTimestamps, HasMain, ModuleDir),

        ModuleDepMap0 = !.Info ^ module_dependencies,
        % XXX Could this be map.det_insert?
        map.set(ModuleName, yes(Imports), ModuleDepMap0, ModuleDepMap),
        !Info ^ module_dependencies := ModuleDepMap,

        % Read the dependencies for the nested children. If something
        % goes wrong (for example one of the files was removed), the
        % dependencies for all modules in the source file will be remade
        % (make_module_dependencies expects to be given the top-level
        % module in the source file).
        list.foldl2(
            read_module_dependencies_2(Globals, do_not_rebuild_module_deps,
                SearchDirs),
            NestedChildren, !Info, !IO),
        ( some_bad_module_dependency(!.Info, NestedChildren) ->
            Result = error("error in nested sub-modules")
        ;
            Result = ok
        )
    ;
        Result = error("failed to parse term")
    ).

:- pred version_number_term(term::in, module_dep_file_version::out) is semidet.

version_number_term(Term, Version) :-
    Term = term.functor(term.integer(Int), [], _),
    version_number(Version, Int).

:- pred string_term(term::in, string::out) is semidet.

string_term(Term, String) :-
    Term = term.functor(term.string(String), [], _).

:- pred atom_term(term::in, string::out, list(term)::out) is semidet.

atom_term(Term, Atom, Args) :-
    Term = term.functor(term.atom(Atom), Args, _).

:- pred braces_term(pred(term, U), term, list(U)).
:- mode braces_term(in(pred(in, out) is semidet), in, out) is semidet.

braces_term(P, Term, Args) :-
    atom_term(Term, "{}", ArgTerms),
    list.map(P, ArgTerms, Args).

:- pred sym_names_term(term::in, list(sym_name)::out) is semidet.

sym_names_term(Term, SymNames) :-
    braces_term(try_parse_sym_name_and_no_args, Term, SymNames).

:- pred fact_dep_term(term::in, string::out) is semidet.

fact_dep_term(Term, FactDep) :-
    string_term(Term, FactDep).

:- pred foreign_language_term(term::in, foreign_language::out) is semidet.

foreign_language_term(Term, Lang) :-
    string_term(Term, String),
    globals.convert_foreign_language(String, Lang).

:- pred foreign_import_term(term::in, foreign_import_module_info::out)
    is semidet.

foreign_import_term(Term, ForeignImport) :-
    atom_term(Term, "-", [LanguageTerm, ImportedModuleTerm]),
    foreign_language_term(LanguageTerm, Language),
    try_parse_sym_name_and_no_args(ImportedModuleTerm, ImportedModuleName),
    ForeignImport = foreign_import_module_info(Language, ImportedModuleName,
        term.context_init).

:- pred foreign_include_term(term::in, foreign_include_file_info::out)
    is semidet.

foreign_include_term(Term, ForeignInclude) :-
    atom_term(Term, "-", [LanguageTerm, FileNameTerm]),
    foreign_language_term(LanguageTerm, Language),
    string_term(FileNameTerm, FileName),
    ForeignInclude = foreign_include_file_info(Language, FileName).

:- pred contains_foreign_export_term(term::in, contains_foreign_export::out)
    is semidet.

contains_foreign_export_term(Term, ContainsForeignExport) :-
    atom_term(Term, Atom, []),
    contains_foreign_export_to_string(ContainsForeignExport, Atom).

:- func contains_foreign_code(list(foreign_language)) = contains_foreign_code.

contains_foreign_code([]) = contains_no_foreign_code.
contains_foreign_code(Langs) = contains_foreign_code(LangSet) :-
    Langs = [_ | _],
    LangSet = set.from_list(Langs).

:- pred has_main_term(term::in, has_main::out) is semidet.

has_main_term(Term, HasMain) :-
    atom_term(Term, String, []),
    has_main_to_string(HasMain, String).

:- pred some_bad_module_dependency(make_info::in, list(module_name)::in)
    is semidet.

some_bad_module_dependency(Info, ModuleNames) :-
    list.member(ModuleName, ModuleNames),
    map.search(Info ^ module_dependencies, ModuleName, no).

%-----------------------------------------------------------------------------%

    % Something went wrong reading the dependencies, so just rebuild them.
    %
:- pred read_module_dependencies_remake(globals::in, rebuild_module_deps::in,
    module_name::in, string::in, make_info::in, make_info::out,
    io::di, io::uo) is det.

read_module_dependencies_remake(Globals, RebuildModuleDeps, ModuleName, Msg,
        !Info, !IO) :-
    (
        RebuildModuleDeps = do_rebuild_module_deps,
        debug_msg(Globals,
            read_module_dependencies_remake_msg(Globals, ModuleName, Msg),
            !IO),
        make_module_dependencies(Globals, ModuleName, !Info, !IO)
    ;
        RebuildModuleDeps = do_not_rebuild_module_deps
    ).

:- pred read_module_dependencies_remake_msg(globals::in, module_name::in,
    string::in, io::di, io::uo) is det.

read_module_dependencies_remake_msg(Globals, ModuleName, Msg, !IO) :-
    module_name_to_file_name(Globals, ModuleName,
        make_module_dep_file_extension, do_not_create_dirs, ModuleDepsFile,
        !IO),
    io.write_string("Error reading file `", !IO),
    io.write_string(ModuleDepsFile, !IO),
    io.write_string("', rebuilding: ", !IO),
    io.write_string(Msg, !IO),
    io.nl(!IO).

    % The module_name given must be the top level module in the source file.
    % get_module_dependencies ensures this by making the dependencies
    % for all parent modules of the requested module first.
    %
:- pred make_module_dependencies(globals::in, module_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_module_dependencies(Globals, ModuleName, !Info, !IO) :-
    redirect_output(ModuleName, MaybeErrorStream, !Info, !IO),
    (
        MaybeErrorStream = yes(ErrorStream),
        io.set_output_stream(ErrorStream, OldOutputStream, !IO),
        % XXX Why ask for the timestamp if we then ignore it?
        read_module(Globals, ModuleName, ".m",
            "Getting dependencies for module",
            do_not_search, do_return_timestamp, Items, Specs0, Error,
            SourceFileName, _, !IO),
        (
            Error = fatal_module_errors,
            io.set_output_stream(ErrorStream, _, !IO),
            write_error_specs(Specs0, Globals, 0, _NumWarnings, 0, _NumErrors,
                !IO),
            io.set_output_stream(OldOutputStream, _, !IO),
            io.write_string("** Error: error reading file `", !IO),
            io.write_string(SourceFileName, !IO),
            io.write_string("' to generate dependencies.\n", !IO),
            maybe_write_importing_module(ModuleName, !.Info ^ importing_module,
                !IO),

            % Display the contents of the `.err' file, then remove it
            % so we don't leave `.err' files lying around for nonexistent
            % modules.
            globals.set_option(output_compile_error_lines, int(10000),
                Globals, UnredirectGlobals),
            unredirect_output(UnredirectGlobals, ModuleName, ErrorStream,
                !Info, !IO),
            module_name_to_file_name(Globals, ModuleName, ".err",
                do_not_create_dirs, ErrFileName, !IO),
            io.remove_file(ErrFileName, _, !IO),
            ModuleDepMap0 = !.Info ^ module_dependencies,
            % XXX Could this be map.det_update?
            map.set(ModuleName, no, ModuleDepMap0, ModuleDepMap),
            !Info ^ module_dependencies := ModuleDepMap
        ;
            ( Error = no_module_errors
            ; Error = some_module_errors
            ),
            io.set_output_stream(ErrorStream, _, !IO),
            split_into_submodules(ModuleName, Items, SubModuleList,
                Specs0, Specs),
            io.set_exit_status(0, !IO),
            write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors,
                !IO),
            io.set_output_stream(OldOutputStream, _, !IO),

            assoc_list.keys(SubModuleList, SubModuleNames),
            list.map(init_dependencies(SourceFileName, ModuleName,
                SubModuleNames, [], Error, Globals),
                SubModuleList, ModuleImportList),
            list.foldl(
                (pred(ModuleImports::in, Info0::in, Info::out) is det :-
                    SubModuleName = ModuleImports ^ mai_module_name,
                    Info = Info0 ^ module_dependencies ^ elem(SubModuleName)
                        := yes(ModuleImports)
                ), ModuleImportList, !Info),

            % If there were no errors, write out the `.int3' file
            % while we have the contents of the module. The `int3' file
            % does not depend on anything else.
            globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
            (
                Error = no_module_errors,
                Target = target_file(ModuleName,
                    module_target_unqualified_short_interface),
                maybe_make_target_message_to_stream(Globals, OldOutputStream,
                    Target, !IO),
                build_with_check_for_interrupt(VeryVerbose,
                    build_with_module_options(Globals, ModuleName,
                        ["--make-short-interface"],
                        make_short_interfaces(ErrorStream,
                            SourceFileName, SubModuleList)
                    ),
                    cleanup_short_interfaces(Globals, SubModuleNames),
                    Succeeded, !Info, !IO)
            ;
                Error = some_module_errors,
                Succeeded = no
            ),

            build_with_check_for_interrupt(VeryVerbose,
                (pred(yes::out, MakeInfo::in, MakeInfo::out, di, uo) is det -->
                    list.foldl(do_write_module_dep_file(Globals),
                        ModuleImportList)
                ),
                cleanup_module_dep_files(Globals, SubModuleNames), _,
                !Info, !IO),

            MadeTarget = target_file(ModuleName,
                module_target_unqualified_short_interface),
            record_made_target(Globals, MadeTarget,
                process_module(task_make_short_interface), Succeeded,
                !Info, !IO),
            unredirect_output(Globals, ModuleName, ErrorStream, !Info, !IO)
        )
    ;
        MaybeErrorStream = no
    ).

:- pred make_short_interfaces(io.output_stream::in, file_name::in,
    assoc_list(module_name, list(item))::in, globals::in, list(string)::in,
    bool::out, make_info::in, make_info::out, io::di, io::uo) is det.

make_short_interfaces(ErrorStream, SourceFileName, SubModuleList, Globals,
        _, Succeeded, !Info, !IO) :-
    io.set_output_stream(ErrorStream, OutputStream, !IO),
    list.foldl(
        (pred(SubModule::in, !.IO::di, !:IO::uo) is det :-
            SubModule = SubModuleName - SubModuleItems,
            modules.make_short_interface(Globals, SourceFileName,
                SubModuleName, SubModuleItems, !IO)
        ), SubModuleList, !IO),
    io.set_output_stream(OutputStream, _, !IO),
    io.get_exit_status(ExitStatus, !IO),
    Succeeded = ( ExitStatus = 0 -> yes ; no ).

:- pred cleanup_short_interfaces(globals::in, list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

cleanup_short_interfaces(Globals, SubModuleNames, !Info, !IO) :-
    list.foldl2(
        (pred(SubModuleName::in, !.Info::in, !:Info::out, !.IO::di, !:IO::uo)
                is det :-
            make_remove_target_file_by_name(Globals, very_verbose,
                SubModuleName, module_target_unqualified_short_interface,
                !Info, !IO)
        ), SubModuleNames, !Info, !IO).

:- pred cleanup_module_dep_files(globals::in, list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

cleanup_module_dep_files(Globals, SubModuleNames, !Info, !IO) :-
    list.foldl2(
        (pred(SubModuleName::in, !.Info::in, !:Info::out, !.IO::di, !:IO::uo)
                is det :-
            make_remove_module_file(Globals, verbose_make, SubModuleName,
                make_module_dep_file_extension, !Info, !IO)
        ), SubModuleNames, !Info, !IO).

:- pred maybe_write_importing_module(module_name::in, maybe(module_name)::in,
    io::di, io::uo) is det.

maybe_write_importing_module(_, no, !IO).
maybe_write_importing_module(ModuleName, yes(ImportingModuleName), !IO) :-
    io.write_string("** Module `", !IO),
    write_sym_name(ModuleName, !IO),
    io.write_string("' is imported or included by module `", !IO),
    write_sym_name(ImportingModuleName, !IO),
    io.write_string("'.\n", !IO).

%-----------------------------------------------------------------------------%
:- end_module make.module_dep_file.
%-----------------------------------------------------------------------------%
