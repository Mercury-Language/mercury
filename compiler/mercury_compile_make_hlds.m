%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: mercury_compile_make_hlds.m.
%
% This module manages the parts of the compiler that build the HLDS
% and, if needed, write out updated .d files.
%
%---------------------------------------------------------------------------%

:- module top_level.mercury_compile_make_hlds.
:- interface.

:- import_module libs.
:- import_module libs.globals.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.

:- import_module hlds.make_hlds.qual_info.
:- import_module hlds.passes_aux.
:- import_module libs.op_mode.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.

:- type maybe_write_d_file
    --->    do_not_write_d_file
    ;       write_d_file.

:- pred make_hlds_pass(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, op_mode_augment::in, maybe_write_d_file::in,
    module_baggage::in, aug_compilation_unit::in,
    module_info::out, qual_info::out, maybe(module_timestamp_map)::out,
    bool::out, bool::out, bool::out,
    dump_info::in, dump_info::out, list(error_spec)::in, list(error_spec)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_defns.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module libs.file_util.
:- import_module libs.options.
:- import_module make.
:- import_module make.module_dep_file.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.grab_modules.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_used_modules.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.write_deps_file.
:- import_module parse_tree.write_error_spec.
:- import_module recompilation.

:- import_module char.
:- import_module library.
:- import_module set.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

make_hlds_pass(ProgressStream, ErrorStream, Globals, OpModeAugment,
        WriteDFile0, Baggage0, AugCompUnit0, HLDS0, QualInfo,
        MaybeTimestampMap, UndefTypes, UndefModes, PreHLDSErrors,
        !DumpInfo, !Specs, !HaveReadModuleMaps, !IO) :-
    globals.lookup_bool_option(Globals, statistics, Stats),
    globals.lookup_bool_option(Globals, verbose, Verbose),

    globals.lookup_bool_option(Globals, invoked_by_mmc_make, InvokedByMMCMake),
    ( if
        % Don't write the `.d' file when making the `.opt' file because
        % we can't work out the full transitive implementation dependencies.
        ( InvokedByMMCMake = yes
        ; OpModeAugment = opmau_make_plain_opt
        )
    then
        WriteDFile = do_not_write_d_file
    else
        WriteDFile = WriteDFile0
    ),

    ParseTreeModuleSrc = AugCompUnit0 ^ acu_module_src,
    maybe_warn_about_stdlib_shadowing(Globals, ParseTreeModuleSrc, !Specs),
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    (
        WriteDFile = do_not_write_d_file,
        MaybeTransOptDeps = no
    ;
        WriteDFile = write_d_file,
        % We need the MaybeTransOptDeps when creating the .trans_opt file.
        % However, we *also* need the MaybeTransOptDeps when writing out
        % .d files. In the absence of MaybeTransOptDeps, we will write out
        % a .d file that does not include the trans_opt_deps mmake rule,
        % which will require an "mmake depend" before the next rebuild.
        maybe_read_d_file_for_trans_opt_deps(ProgressStream, ErrorStream,
            Globals, ModuleName, MaybeTransOptDeps, !IO)
    ),

    % Errors in .opt and .trans_opt files result in software errors.
    maybe_grab_plain_and_trans_opt_files(ProgressStream, ErrorStream, Globals,
        OpModeAugment, Verbose, MaybeTransOptDeps, IntermodError,
        Baggage0, Baggage1, AugCompUnit0, AugCompUnit1,
        !HaveReadModuleMaps, !IO),
    MaybeTimestampMap = Baggage1 ^ mb_maybe_timestamp_map,

    !:Specs = get_read_module_specs(Baggage1 ^ mb_errors) ++ !.Specs,

    globals.lookup_string_option(Globals, event_set_file_name,
        EventSetFileName),
    ( if EventSetFileName = "" then
        EventSetName = "",
        EventSpecMap1 = map.init,
        EventSetErrors = no
    else
        read_event_set(EventSetFileName, EventSetName0, EventSpecMap0,
            EventSetSpecs, !IO),
        !:Specs = EventSetSpecs ++ !.Specs,
        EventSetErrors = contains_errors(Globals, EventSetSpecs),
        (
            EventSetErrors = no,
            EventSetName = EventSetName0,
            EventSpecMap1 = EventSpecMap0
        ;
            EventSetErrors = yes,
            EventSetName = "",
            EventSpecMap1 = map.init
        )
    ),

    pre_hlds_maybe_write_out_errors(ErrorStream, Verbose, Globals, !Specs, !IO),
    maybe_write_string(ProgressStream, Verbose,
        "% Module qualifying items...\n", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    module_qualify_aug_comp_unit(Globals, AugCompUnit1, AugCompUnit2,
        EventSpecMap1, EventSpecMap2, EventSetFileName, MQInfo0,
        MQUndefTypes, MQUndefInsts, MQUndefModes, MQUndefTypeClasses,
        [], QualifySpecs),
    !:Specs = QualifySpecs ++ !.Specs,
    pre_hlds_maybe_write_out_errors(ErrorStream, Verbose, Globals, !Specs, !IO),
    maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO),

    mq_info_get_recompilation_info(MQInfo0, RecompInfo0),
    maybe_write_string(ProgressStream, Verbose,
        "% Expanding equivalence types and insts...\n", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),
    expand_eqv_types_insts(AugCompUnit2, AugCompUnit,
        EventSpecMap2, EventSpecMap, TypeEqvMap, UsedModules,
        RecompInfo0, RecompInfo, ExpandSpecs),
    ExpandErrors = contains_errors(Globals, ExpandSpecs),
    !:Specs = ExpandSpecs ++ !.Specs,
    pre_hlds_maybe_write_out_errors(ErrorStream, Verbose, Globals, !Specs, !IO),
    maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO),
    mq_info_set_recompilation_info(RecompInfo, MQInfo0, MQInfo),

    EventSet = event_set(EventSetName, EventSpecMap),
    make_hlds(ProgressStream, ErrorStream, Globals, AugCompUnit, EventSet,
        MQInfo, TypeEqvMap, UsedModules, Verbose, Stats, HLDS0, QualInfo,
        MakeHLDSFoundInvalidType, MakeHLDSFoundInvalidInstOrMode,
        FoundSemanticError, !Specs, !IO),
    bool.or(FoundSemanticError, IntermodError, PreHLDSErrors),
    maybe_write_definitions(ProgressStream, ErrorStream,
        Verbose, Stats, HLDS0, !IO),
    maybe_write_definition_line_counts(ProgressStream, ErrorStream,
        Verbose, Stats, HLDS0, !IO),
    maybe_write_definition_extents(ProgressStream, ErrorStream,
        Verbose, Stats, HLDS0, !IO),

    ( if
        MQUndefTypes = did_not_find_undef_type,
        MQUndefTypeClasses = did_not_find_undef_typeclass,
        EventSetErrors = no,
        ExpandErrors = no,
        MakeHLDSFoundInvalidType = did_not_find_invalid_type
    then
        UndefTypes = no
    else
        UndefTypes = yes
    ),
    ( if
        MQUndefInsts = did_not_find_undef_inst,
        MQUndefModes = did_not_find_undef_mode,
        MakeHLDSFoundInvalidInstOrMode = did_not_find_invalid_inst_or_mode
    then
        UndefModes = no
    else
        UndefModes = yes
    ),

    maybe_dump_hlds(ProgressStream, HLDS0, 1, "initial", !DumpInfo, !IO),

    (
        WriteDFile = do_not_write_d_file
    ;
        WriteDFile = write_d_file,
        module_info_get_all_deps(HLDS0, AllDeps),
        % XXX When creating the .d and .module_dep files, why are we using
        % BurdenedAugCompUnit0 instead of BurdenedAugCompUnit1?
        BurdenedAugCompUnit0 = burdened_aug_comp_unit(Baggage0, AugCompUnit0),
        write_dependency_file(Globals, BurdenedAugCompUnit0,
            no_intermod_deps, AllDeps, MaybeTransOptDeps, !IO),
        globals.lookup_bool_option(Globals,
            generate_mmc_make_module_dependencies, OutputMMCMakeDeps),
        (
            OutputMMCMakeDeps = yes,
            BurdenedModule0 = burdened_module(Baggage0, ParseTreeModuleSrc),
            make.module_dep_file.write_module_dep_file(Globals,
                BurdenedModule0, !IO)
        ;
            OutputMMCMakeDeps = no
        )
    ).

:- pred maybe_warn_about_stdlib_shadowing(globals::in,
    parse_tree_module_src::in,
    list(error_spec)::in, list(error_spec)::out) is det.

maybe_warn_about_stdlib_shadowing(Globals, ParseTreeModuleSrc, !Specs) :-
    globals.lookup_bool_option(Globals, warn_stdlib_shadowing, WarnShadowing),
    (
        WarnShadowing = no
    ;
        WarnShadowing = yes,
        ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
        ModuleNameStr = sym_name_to_string(ModuleName),
        ( if
            stdlib_module_doc_undoc(ModuleNameStr, DocUndoc)
        then
            Pieces0 = [words("Warning: this module,"),
                qual_sym_name(ModuleName), suffix(","),
                words("has the same name"),
                words("as a module in the Mercury standard library."),
                words("A third module cannot import both,"),
                words("and you will likely have problems where"),
                words("a third module will want to import one"),
                words("but will get the other."), nl],
            maybe_mention_undoc(DocUndoc, Pieces0, Pieces),
            Context = ParseTreeModuleSrc ^ ptms_module_name_context,
            Spec = simplest_spec($pred, severity_warning, phase_read_files,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        else if
            GetStdlibModules =
                ( pred(LibModuleName::out) is multi :-
                    library.stdlib_module_doc_undoc(LibModuleNameStr,
                        _DocUndoc),
                    LibModuleName = string_to_sym_name(LibModuleNameStr)
                ),
            solutions.solutions(GetStdlibModules, LibModuleNames),
            IsShadowed =
                ( pred(LibModuleName::in) is semidet :-
                    partial_sym_name_is_part_of_full(LibModuleName, ModuleName)
                ),
            list.find_first_match(IsShadowed, LibModuleNames,
                ShadowedLibModuleName),
            ShadowedLibModuleNameStr =
                sym_name_to_string(ShadowedLibModuleName),
            stdlib_module_doc_undoc(ShadowedLibModuleNameStr, DocUndoc)
        then
            Pieces0 = [words("Warning: the name of this module,"),
                qual_sym_name(ModuleName), suffix(","),
                words("contains the name of a module,"),
                qual_sym_name(ShadowedLibModuleName), suffix(","),
                words("in the Mercury standard library."),
                words("A reference to the standard library in a third module"),
                words("will therefore be a (not fully qualified) reference"),
                words("to this module, which means that"),
                words("you will likely have problems where,"),
                words("especially in the absence of needed"),
                decl("import_module"), words("declarations,"),
                words("a reference intended to refer to"),
                words("the standard library module"),
                words("will be taken as a reference to this module,"),
                words("and vice versa."), nl],
            maybe_mention_undoc(DocUndoc, Pieces0, Pieces),
            Context = ParseTreeModuleSrc ^ ptms_module_name_context,
            Spec = simplest_spec($pred, severity_warning, phase_read_files,
                Context, Pieces),
            !:Specs = [Spec | !.Specs]
        else
            true
        )
    ).

:- pred maybe_mention_undoc(doc_or_undoc::in,
    list(format_piece)::in, list(format_piece)::out) is det.

maybe_mention_undoc(DocUndoc, Pieces0, Pieces) :-
    (
        DocUndoc = doc,
        Pieces = Pieces0
    ;
        DocUndoc = undoc,
        Pieces = Pieces0 ++
            [words("The Mercury standard library module in question"),
            words("is part of the Mercury implementation,"),
            words("and is not publically documented."), nl]
    ).

%---------------------%

    % maybe_read_d_file_for_trans_opt_deps(ProgressStream, ErrorStream,
    %   Globals, ModuleName, MaybeTransOptDeps, !IO):
    %
    % If transitive intermodule optimization has been enabled, then read
    % <ModuleName>.d to find the modules which <ModuleName>.trans_opt may
    % depend on. Otherwise return `no'.
    %
:- pred maybe_read_d_file_for_trans_opt_deps(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, module_name::in,
    maybe(list(module_name))::out, io::di, io::uo) is det.

maybe_read_d_file_for_trans_opt_deps(ProgressStream, ErrorStream, Globals,
        ModuleName, MaybeTransOptDeps, !IO) :-
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    (
        TransOpt = yes,
        globals.lookup_bool_option(Globals, verbose, Verbose),
        module_name_to_file_name(Globals, $pred, do_not_create_dirs,
            ext_other(other_ext(".d")), ModuleName, DependencyFileName, !IO),
        io.format(ProgressStream, "%% Reading auto-dependency file `%s'...", 
            [s(DependencyFileName)], !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        io.open_input(DependencyFileName, DepFileOpenResult, !IO),
        (
            DepFileOpenResult = ok(DepFileInStream),
            module_name_to_file_name(Globals, $pred, do_not_create_dirs,
                ext_other(other_ext(".trans_opt_date")),
                ModuleName, TransOptDateFileName, !IO),
            SearchPattern = TransOptDateFileName ++ " :",
            read_dependency_file_find_start(DepFileInStream, SearchPattern,
                FindResult, !IO),
            (
                FindResult = yes,
                read_dependency_file_get_modules(DepFileInStream,
                    TransOptDeps, !IO),
                MaybeTransOptDeps = yes(TransOptDeps)
            ;
                FindResult = no,
                % error reading .d file
                MaybeTransOptDeps = no
            ),
            io.close_input(DepFileInStream, !IO),
            maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
        ;
            DepFileOpenResult = error(IOError),
            maybe_write_string(ProgressStream, Verbose, " failed.\n", !IO),
            maybe_flush_output(ProgressStream, Verbose, !IO),
            io.error_message(IOError, IOErrorMessage),
            string.format("error opening file `%s for input: %s",
                [s(DependencyFileName), s(IOErrorMessage)], Message),
            report_error(ErrorStream, Message, !IO),
            MaybeTransOptDeps = no
        )
    ;
        TransOpt = no,
        MaybeTransOptDeps = no
    ).

    % Read lines from the dependency file (module.d) until one is found
    % which begins with SearchPattern.
    %
:- pred read_dependency_file_find_start(io.text_input_stream::in, string::in,
    bool::out, io::di, io::uo) is det.

read_dependency_file_find_start(InStream, SearchPattern, Success, !IO) :-
    io.read_line_as_string(InStream, Result, !IO),
    (
        Result = ok(Line),
        ( if string.prefix(Line, SearchPattern) then
            % Have found the start.
            Success = yes
        else
            read_dependency_file_find_start(InStream, SearchPattern,
                Success, !IO)
        )
    ;
        ( Result = error(_)
        ; Result = eof
        ),
        Success = no
    ).

    % Read lines until one is found which does not contain whitespace
    % followed by a word which ends in .trans_opt. Remove the .trans_opt
    % ending from all the words which are read in and return the resulting
    % list of modules.
    %
:- pred read_dependency_file_get_modules(io.text_input_stream::in,
    list(module_name)::out, io::di, io::uo) is det.

read_dependency_file_get_modules(InStream, TransOptDeps, !IO) :-
    io.read_line(InStream, Result, !IO),
    ( if
        Result = ok(CharList0),
        % Remove any whitespace from the beginning of the line,
        % then take all characters until another whitespace occurs.
        list.drop_while(char.is_whitespace, CharList0, CharList1),
        list.take_while_not(char.is_whitespace, CharList1, CharList),
        string.from_char_list(CharList, FileName0),
        string.remove_suffix(FileName0, ".trans_opt", FileName)
    then
        ( if string.append("Mercury/trans_opts/", BaseFileName, FileName) then
            ModuleFileName = BaseFileName
        else
            ModuleFileName = FileName
        ),
        file_name_to_module_name(ModuleFileName, Module),
        read_dependency_file_get_modules(InStream, TransOptDeps0, !IO),
        TransOptDeps = [Module | TransOptDeps0]
    else
        TransOptDeps = []
    ).

%---------------------%

:- pred maybe_grab_plain_and_trans_opt_files(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, op_mode_augment::in,
    bool::in, maybe(list(module_name))::in, bool::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

maybe_grab_plain_and_trans_opt_files(ProgressStream, ErrorStream, Globals,
        OpModeAugment, Verbose, MaybeTransOptDeps, Error,
        !Baggage, !AugCompUnit, !HaveReadModuleMaps, !IO) :-
    globals.lookup_bool_option(Globals, intermodule_optimization, IntermodOpt),
    globals.lookup_bool_option(Globals, use_opt_files, UseOptInt),
    globals.lookup_bool_option(Globals, transitive_optimization, TransOpt),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    ( if
        ( UseOptInt = yes
        ; IntermodOpt = yes
        ; IntermodAnalysis = yes
        ),
        OpModeAugment \= opmau_make_plain_opt
    then
        maybe_write_string(ProgressStream, Verbose,
            "% Reading .opt files...\n", !IO),
        maybe_flush_output(ProgressStream, Verbose, !IO),
        grab_plain_opt_and_int_for_opt_files(ProgressStream, ErrorStream,
            Globals, PlainOptError, !Baggage, !AugCompUnit,
            !HaveReadModuleMaps, !IO),
        maybe_write_string(ProgressStream, Verbose, "% Done.\n", !IO)
    else
        PlainOptError = no_opt_file_error
    ),
    (
        OpModeAugment = opmau_make_trans_opt,
        (
            MaybeTransOptDeps = yes(TransOptDeps),
            % When creating the trans_opt file, only import the
            % trans_opt files which are lower in the ordering.
            grab_trans_opt_files(ProgressStream, Globals,
                TransOptDeps, TransOptError, !Baggage, !AugCompUnit,
                !HaveReadModuleMaps, !IO)
        ;
            MaybeTransOptDeps = no,
            TransOptError = no_opt_file_error,
            ParseTreeModuleSrc = !.AugCompUnit ^ acu_module_src,
            ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
            globals.lookup_bool_option(Globals, warn_missing_trans_opt_deps,
                WarnNoTransOptDeps),
            (
                WarnNoTransOptDeps = yes,
                Pieces = [words("Warning: cannot read trans-opt dependencies"),
                    words("for module"), qual_sym_name(ModuleName),
                    suffix("."), nl,
                    words("You need to remake the dependencies."), nl],
                Spec = simplest_no_context_spec($pred, severity_warning,
                    phase_read_files, Pieces),
                write_error_spec(ErrorStream, Globals, Spec, !IO)
            ;
                WarnNoTransOptDeps = no
            )
        )
    ;
        OpModeAugment = opmau_make_plain_opt,
        % If we are making the `.opt' file, then we cannot read any
        % `.trans_opt' files, since `.opt' files aren't allowed to depend on
        % `.trans_opt' files.
        TransOptError = no_opt_file_error
    ;
        ( OpModeAugment = opmau_make_analysis_registry
        ; OpModeAugment = opmau_make_xml_documentation
        ; OpModeAugment = opmau_typecheck_only
        ; OpModeAugment = opmau_errorcheck_only
        ; OpModeAugment = opmau_generate_code(_)
        ),
        (
            TransOpt = yes,
            % If transitive optimization is enabled, but we are not creating
            % the .opt or .trans opt file, then import the trans_opt files
            % for all the modules that are imported (or used), and for all
            % ancestor modules.
            ParseTreeModuleSrc = !.AugCompUnit ^ acu_module_src,
            ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
            Ancestors = get_ancestors_set(ModuleName),
            Deps0 = map.keys_as_set(ParseTreeModuleSrc ^ ptms_import_use_map),
            % Some builtin modules can implicitly depend on themselves.
            % (For example, we consider every module to depend on both
            % builtin.m and private_builtin.m, so they "depend" on themselves.)
            % For those, we don't want to read in their .trans_opt file,
            % since we already have their .m file.
            set.delete(ModuleName, Deps0, Deps),
            TransOptFiles = set.union_list([Ancestors, Deps]),
            set.to_sorted_list(TransOptFiles, TransOptFilesList),
            grab_trans_opt_files(ProgressStream, Globals,
                TransOptFilesList, TransOptError, !Baggage, !AugCompUnit,
                !HaveReadModuleMaps, !IO)
        ;
            TransOpt = no,
            TransOptError = no_opt_file_error
        )
    ),
    ( if
        PlainOptError = no_opt_file_error,
        TransOptError = no_opt_file_error
    then
        Error = no
    else
        Error = yes
    ).

%---------------------%

:- pred make_hlds(io.text_output_stream::in, io.text_output_stream::in,
    globals::in, aug_compilation_unit::in, event_set::in, mq_info::in,
    type_eqv_map::in, used_modules::in, bool::in, bool::in,
    module_info::out, qual_info::out,
    found_invalid_type::out, found_invalid_inst_or_mode::out, bool::out,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

make_hlds(ProgressStream, ErrorStream, Globals, AugCompUnit, EventSet, MQInfo,
        TypeEqvMap, UsedModules, Verbose, Stats, !:HLDS, QualInfo,
        FoundInvalidType, FoundInvalidInstOrMode,
        FoundSemanticError, !Specs, !IO) :-
    pre_hlds_maybe_write_out_errors(ErrorStream, Verbose, Globals,
        !Specs, !IO),
    maybe_write_string(ProgressStream, Verbose,
        "% Converting parse tree to hlds...\n", !IO),
    ParseTreeModuleSrc = AugCompUnit ^ acu_module_src,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".hlds_dump")), ModuleName, DumpBaseFileName, !IO),
    parse_tree_to_hlds(AugCompUnit, Globals, DumpBaseFileName, MQInfo,
        TypeEqvMap, UsedModules, QualInfo,
        FoundInvalidType, FoundInvalidInstOrMode, !:HLDS, MakeSpecs),
    !:Specs = MakeSpecs ++ !.Specs,
    module_info_set_event_set(EventSet, !HLDS),
    io.get_exit_status(Status, !IO),
    SpecsErrors = contains_errors(Globals, !.Specs),
    ( if
        ( Status \= 0
        ; SpecsErrors = yes
        )
    then
        FoundSemanticError = yes,
        io.set_exit_status(1, !IO)
    else
        FoundSemanticError = no
    ),
    pre_hlds_maybe_write_out_errors(ErrorStream, Verbose, Globals,
        !Specs, !IO),
    maybe_write_string(ProgressStream, Verbose, "% done.\n", !IO),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------%

:- pred maybe_write_definitions(io.text_output_stream::in,
    io.text_output_stream::in, bool::in, bool::in, module_info::in,
    io::di, io::uo) is det.

maybe_write_definitions(ProgressStream, ErrorStream, Verbose, Stats,
        HLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, show_definitions, ShowDefns),
    (
        ShowDefns = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Writing definitions...", !IO),
        module_info_get_name(HLDS, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".defns")), ModuleName, FileName, !IO),
        io.open_output(FileName, Res, !IO),
        (
            Res = ok(FileStream),
            hlds.hlds_defns.write_hlds_defns(FileStream, HLDS, !IO),
            io.close_output(FileStream, !IO),
            maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
        ;
            Res = error(IOError),
            ErrorMsg = "unable to write definitions: " ++
                io.error_message(IOError),
            report_error(ErrorStream, ErrorMsg, !IO)
        ),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        ShowDefns = no
    ).

:- pred maybe_write_definition_line_counts(io.text_output_stream::in,
    io.text_output_stream::in, bool::in, bool::in, module_info::in,
    io::di, io::uo) is det.

maybe_write_definition_line_counts(ProgressStream, ErrorStream, Verbose, Stats,
        HLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, show_definition_line_counts,
        LineCounts),
    (
        LineCounts = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Writing definition line counts...", !IO),
        module_info_get_name(HLDS, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".defn_line_counts")),
            ModuleName, FileName, !IO),
        io.open_output(FileName, Res, !IO),
        (
            Res = ok(FileStream),
            hlds.hlds_defns.write_hlds_defn_line_counts(FileStream, HLDS, !IO),
            io.close_output(FileStream, !IO),
            maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
        ;
            Res = error(IOError),
            ErrorMsg = "unable to write definition line counts: " ++
                io.error_message(IOError),
            report_error(ErrorStream, ErrorMsg, !IO)
        ),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        LineCounts = no
    ).

:- pred maybe_write_definition_extents(io.text_output_stream::in,
    io.text_output_stream::in, bool::in, bool::in, module_info::in,
    io::di, io::uo) is det.

maybe_write_definition_extents(ProgressStream, ErrorStream, Verbose, Stats,
        HLDS, !IO) :-
    module_info_get_globals(HLDS, Globals),
    globals.lookup_bool_option(Globals, show_definition_extents, Extents),
    (
        Extents = yes,
        maybe_write_string(ProgressStream, Verbose,
            "% Writing definition extents...", !IO),
        module_info_get_name(HLDS, ModuleName),
        module_name_to_file_name(Globals, $pred, do_create_dirs,
            ext_other(other_ext(".defn_extents")), ModuleName, FileName, !IO),
        io.open_output(FileName, Res, !IO),
        (
            Res = ok(FileStream),
            hlds.hlds_defns.write_hlds_defn_extents(FileStream, HLDS, !IO),
            io.close_output(FileStream, !IO),
            maybe_write_string(ProgressStream, Verbose, " done.\n", !IO)
        ;
            Res = error(IOError),
            ErrorMsg = "unable to write definition extents: " ++
                io.error_message(IOError),
            report_error(ErrorStream, ErrorMsg, !IO)
        ),
        maybe_report_stats(ProgressStream, Stats, !IO)
    ;
        Extents = no
    ).

%---------------------------------------------------------------------------%
:- end_module top_level.mercury_compile_make_hlds.
%---------------------------------------------------------------------------%
