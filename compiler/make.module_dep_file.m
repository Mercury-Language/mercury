%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2009, 2011 The University of Melbourne.
% Copyright (C) 2014-2017, 2019-2020 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.module_dep_file.m.
% Author: stayl.
%
% Code to read and write the `<module>.module_dep' files, which contain
% information about inter-module dependencies.
%
%---------------------------------------------------------------------------%

:- module make.module_dep_file.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_dep_info.

:- import_module io.

%---------------------------------------------------------------------------%

    % Exported to mercury_compile_make_hlds.m.
    %
:- pred write_module_dep_file(io.text_output_stream::in, globals::in,
    burdened_module::in, io::di, io::uo) is det.

    % Exported to make.get_module_dep_info.m.
    %
:- pred do_write_module_dep_file(io.text_output_stream::in, globals::in,
    burdened_module::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred read_module_dep_file(dir_name::in, file_name::in, string::in,
    module_name::in, maybe1(module_dep_summary, string)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.file_names.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module dir.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module mercury_term_parser.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_int.
:- import_module term_io.

%---------------------------------------------------------------------------%

    % The version 1 module_dep file format is the same as version 2 except that
    % it does not include a list of files included by `pragma foreign_decl' and
    % `pragma foreign_code'. We stopped generating version 1 .module_dep files
    % on 2021 Jul 31.
    %
    % XXX We should consider
    %
    % - adding a version 3 that differs from 2 in deleting the field
    %   that now *always* contains "no_main",
    % - replacing the braces that wrap the contents of each structured field
    %   with a function symbol that explicitly specifies the meaning of
    %   that contents, and
    % - switching to always generating version 3.
    %
    % XXX The precise on-disk representation of each (current) .module_dep
    % file format version should be explicitly documented. This documentation
    % should explain
    %
    % - what the meaning of each field is,
    % - what purposes does it serve, and
    % - what invariants (if any) apply to it.
    %
    % It should also have some examples to help readers understand it all.
    %
:- type module_dep_file_version
    --->    module_dep_file_v1
    ;       module_dep_file_v2.

:- pred version_number(module_dep_file_version, int).
:- mode version_number(in, out) is det.
:- mode version_number(out, in) is semidet.

version_number(module_dep_file_v1, 1).
version_number(module_dep_file_v2, 2).

%---------------------------------------------------------------------------%

write_module_dep_file(ProgressStream, Globals, BurdenedModule0, !IO) :-
    BurdenedModule0 = burdened_module(Baggage0, ParseTreeModuleSrc),
    Baggage0 = module_baggage(SourceFileName, _SourceFileDir,
        SourceFileModuleName, MaybeTopModule, _MaybeTimestampMap,
        _GrabbedFileMap, _Errors),

    MaybeTimestampMap = maybe.no,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    GrabbedFileMap = map.singleton(ModuleName, gf_src(ParseTreeModuleSrc)),
    Errors = init_read_module_errors,
    Baggage = module_baggage(SourceFileName, dir.this_directory,
        SourceFileModuleName, MaybeTopModule, MaybeTimestampMap,
        GrabbedFileMap, Errors),

    BurdenedModule = burdened_module(Baggage, ParseTreeModuleSrc),
    do_write_module_dep_file(ProgressStream, Globals, BurdenedModule, !IO).

do_write_module_dep_file(ProgressStream, Globals, BurdenedModule, !IO) :-
    BurdenedModule = burdened_module(Baggage, ParseTreeModuleSrc),
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    module_name_to_file_name_create_dirs(Globals, $pred,
        ext_cur_ngs(ext_cur_ngs_misc_module_dep),
        ModuleName, ProgDepFile, !IO),
    io.open_output(ProgDepFile, ProgDepResult, !IO),
    (
        ProgDepResult = ok(ProgDepStream),
        do_write_module_dep_file_to_stream(ProgDepStream, Globals,
            Baggage, ParseTreeModuleSrc, !IO),
        io.close_output(ProgDepStream, !IO)
    ;
        ProgDepResult = error(Error),
        io.error_message(Error, Msg),
        io.format(ProgressStream, "Error opening %s for output: %s\n",
            [s(ProgDepFile), s(Msg)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred do_write_module_dep_file_to_stream(io.text_output_stream::in,
    globals::in, module_baggage::in, parse_tree_module_src::in,
    io::di, io::uo) is det.

do_write_module_dep_file_to_stream(Stream, Globals,
        Baggage, ParseTreeModuleSrc, !IO) :-
    Version = module_dep_file_v2,
    version_number(Version, VersionNumber),
    SourceFileName = Baggage ^ mb_source_file_name,
    SourceFileModuleName = Baggage ^ mb_source_file_module_name,
    SourceFileModuleNameStr =
        mercury_bracketed_sym_name_to_string(SourceFileModuleName),
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    Ancestors = set.to_sorted_list(get_ancestors_set(ModuleName)),
    IncludeMap = ParseTreeModuleSrc ^ ptms_include_map,
    Children = map.keys(IncludeMap),
    parse_tree_module_src_get_int_imp_deps(ParseTreeModuleSrc,
        IntDepSet, ImpDepSet),
    set.to_sorted_list(IntDepSet, IntDeps),
    set.to_sorted_list(ImpDepSet, ImpDeps),
    MaybeTopModule = Baggage ^ mb_maybe_top_module,
    NestedSubModules = get_nested_children_list_of_top_module(MaybeTopModule),
    get_fact_tables(ParseTreeModuleSrc, FactTableFilesSet),
    FactTableFilesStrs = list.map(term_io.quoted_string,
        set.to_sorted_list(FactTableFilesSet)),
    globals.get_backend_foreign_languages(Globals, BackendLangsList),
    BackendLangs = set.list_to_set(BackendLangsList),
    get_foreign_code_langs(ParseTreeModuleSrc, CodeLangs),
    get_foreign_export_langs(ParseTreeModuleSrc, ExportLangs),
    set.intersect(BackendLangs, CodeLangs, BackendCodeLangs),
    set.intersect(BackendLangs, ExportLangs, BackendExportLangs),
    CodeLangStrs = list.map(mercury_foreign_language_to_string,
        set.to_sorted_list(BackendCodeLangs)),
    ( if set.is_empty(BackendExportLangs) then
        ContainsForeignExport = contains_no_foreign_export
    else
        ContainsForeignExport = contains_foreign_export
    ),
    get_fim_specs(ParseTreeModuleSrc, FIMSpecs),
    get_foreign_include_file_infos(ParseTreeModuleSrc, ForeignIncludeFiles),
    FIMSpecStrs = list.map(fim_spec_to_string, set.to_sorted_list(FIMSpecs)),
    FIFOStrs = list.map(foreign_include_file_info_to_string,
        set.to_sorted_list(ForeignIncludeFiles)),
    contains_foreign_export_to_string(ContainsForeignExport,
        ContainsForeignExportStr),
    io.format(Stream,
        "module(%d, ""%s"",\n" ++
            "\t%s,\n" ++
            "\t{%s},\n" ++
            "\t{%s},\n" ++
            "\t{%s},\n" ++
            "\t{%s},\n" ++
            "\t{%s},\n" ++
            "\t{%s},\n" ++
            "\t{%s},\n" ++
            "\t{%s},\n" ++
            "\t%s,\n" ++
            % The has_main/no_main slot is not needed anymore,
            % so we just put no_main in there always.
            "\tno_main,\n" ++
            "\t{%s}\n" ++
        ").\n",
        [i(VersionNumber), s(SourceFileName),
        s(SourceFileModuleNameStr),
        s(bracketed_sym_names_to_comma_list_string(Ancestors)),
        s(bracketed_sym_names_to_comma_list_string(IntDeps)),
        s(bracketed_sym_names_to_comma_list_string(ImpDeps)),
        s(bracketed_sym_names_to_comma_list_string(Children)),
        s(bracketed_sym_names_to_comma_list_string(NestedSubModules)),
        s(string.join_list(", ", FactTableFilesStrs)),
        s(string.join_list(", ", CodeLangStrs)),
        s(string.join_list(", ", FIMSpecStrs)),
        s(ContainsForeignExportStr),
        s(string.join_list(", ", FIFOStrs))],
        !IO).

:- func bracketed_sym_names_to_comma_list_string(list(sym_name)) = string.

bracketed_sym_names_to_comma_list_string(SymNames) = Str :-
    Strs = list.map(mercury_bracketed_sym_name_to_string, SymNames),
    Str = string.join_list(", ", Strs).

:- func fim_spec_to_string(fim_spec) = string.

fim_spec_to_string(FIMSpec) = Str :-
    FIMSpec = fim_spec(Lang, ForeignImport),
    LangStr = mercury_foreign_language_to_string(Lang),
    ForeignImportStr = mercury_bracketed_sym_name_to_string(ForeignImport),
    Str = LangStr ++ " - " ++ ForeignImportStr.

:- func foreign_include_file_info_to_string(foreign_include_file_info)
    = string.

foreign_include_file_info_to_string(ForeignInclude) = Str :-
    ForeignInclude = foreign_include_file_info(Lang, FileName),
    LangStr = mercury_foreign_language_to_string(Lang),
    Str = LangStr ++ " - " ++ term_io.quoted_string(FileName).

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
        % Yes, without the "contains_" prefix. Don't change it unless you mean
        % to break compatibility with older .module_dep files.
        ContainsForeignExportStr = "no_foreign_export"
    ).

%---------------------------------------------------------------------------%

read_module_dep_file(DepFileDir, DepFileName, DepFileContents, ModuleName,
        Result) :-
    % Since .module_dep files are automatically generated, errors in them
    % should be vanishingly rare. However, this also means that if and when
    % they do occur, any errors are likely to be quite obscure. In such
    % cases, knowing *for sure* exactly *which* .module_dep file has
    % the error would be one of the first pieces of information that
    % any person looking to track down the problem would wan.
    % This is why we include DepFileDir in the pathname we pass
    % to read_term_from_string.
    DepFilePathName = DepFileDir / DepFileName,
    mercury_term_parser.read_term_from_string(DepFilePathName,
        DepFileContents, _EndPos, TermResult),
    (
        TermResult = term(_, Term),
        ( if
            parse_module_dep_file_term(ModuleName, DepFileDir, Term,
                ModuleSummary)
        then
            Result = ok1(ModuleSummary)
        else
            Result = error1("failed to parse term")
        )
    ;
        TermResult = eof,
        Result = error1("unexpected eof")
    ;
        TermResult = error(Error, _),
        Result = error1("parse error: " ++ Error)
    ).

:- pred parse_module_dep_file_term(module_name::in, dir_name::in, term::in,
    module_dep_summary::out) is semidet.

parse_module_dep_file_term(ModuleName, DepFileDir, Term, ModuleSummary) :-
    atom_term(Term, "module", ModuleArgs),
    ModuleArgs = [
        VersionNumberTerm,
        SourceFileTerm,
        SourceFileModuleNameTerm,
        ParentsTerm,                % XXX Redundant term
        IntDepsTerm,
        ImpDepsTerm,
        ChildrenTerm,
        NestedSubModulesTerm,
        FactDepsTerm,
        ForeignLanguagesTerm,
        ForeignImportsTerm,
        ContainsForeignExportTerm,
        _HasMainTerm                % XXX Redundant term
        | ModuleArgsTail
    ],

    version_number_term(VersionNumberTerm, Version),
    string_term(SourceFileTerm, SourceFileName),
    try_parse_sym_name_and_no_args(SourceFileModuleNameTerm,
        SourceFileModuleName),

    sym_names_term(ParentsTerm, Parents),
    sym_names_term(IntDepsTerm, IntDeps),
    sym_names_term(ImpDepsTerm, ImpDeps),
    sym_names_term(ChildrenTerm, Children),
    sym_names_term(NestedSubModulesTerm, NestedSubModules0),

    braces_term(fact_dep_term, FactDepsTerm, FactDeps),
    braces_term(foreign_language_term, ForeignLanguagesTerm, ForeignLanguages),
    braces_term(foreign_import_term, ForeignImportsTerm, ForeignImports),

    contains_foreign_export_term(ContainsForeignExportTerm,
        ContainsForeignExport),

    (
        Version = module_dep_file_v1,
        ModuleArgsTail = [],
        ForeignIncludes = []
    ;
        Version = module_dep_file_v2,
        ModuleArgsTail = [ForeignIncludesTerm],
        braces_term(foreign_include_term, ForeignIncludesTerm, ForeignIncludes)
    ),

    require_det (
        ( if ModuleName = SourceFileModuleName then
            MaybeTopModule = top_module(set.list_to_set(NestedSubModules0))
        else
            MaybeTopModule = not_top_module,
            expect(unify(NestedSubModules0, []), $pred,
                "NestedSubModules0 != []")
        ),
        set.list_to_set(Parents, ParentsSet),
        AncestorsSet = get_ancestors_set(ModuleName),
        expect(set.equal(ParentsSet, AncestorsSet), $pred,
            "ParentsSet != AncestorsSet"),
        ContainsForeignCode =
            foreign_code_langs_known(set.list_to_set(ForeignLanguages)),
        ModuleSummary = module_dep_summary(SourceFileName, DepFileDir,
            SourceFileModuleName, ModuleName, set.list_to_set(Children),
            MaybeTopModule,
            set.list_to_set(IntDeps), set.list_to_set(ImpDeps),
            set.list_to_set(FactDeps), set.list_to_set(ForeignImports),
            set.list_to_set(ForeignIncludes),
            ContainsForeignCode, ContainsForeignExport)
    ).

:- pred atom_term(term::in, string::out, list(term)::out) is semidet.

atom_term(Term, Atom, Args) :-
    Term = term.functor(term.atom(Atom), Args, _).

:- pred version_number_term(term::in, module_dep_file_version::out) is semidet.

version_number_term(Term, Version) :-
    term_int.decimal_term_to_int(Term, Int),
    version_number(Version, Int).

:- pred string_term(term::in, string::out) is semidet.

string_term(Term, String) :-
    Term = term.functor(term.string(String), [], _).

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

:- pred foreign_import_term(term::in, fim_spec::out) is semidet.

foreign_import_term(Term, FIMSpec) :-
    atom_term(Term, "-", [LanguageTerm, ImportedModuleTerm]),
    foreign_language_term(LanguageTerm, Language),
    try_parse_sym_name_and_no_args(ImportedModuleTerm, ImportedModuleName),
    FIMSpec = fim_spec(Language, ImportedModuleName).

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

%---------------------------------------------------------------------------%
:- end_module make.module_dep_file.
%---------------------------------------------------------------------------%
