%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: grab_modules.m.
% Main author: fjh (original), zs (current).
%
% When the compiler is processing a module, it uses the code in this module
% to grab files that earlier compiler invocations have automatically generated
% for some of the *other* modules of the program.
%
% This module does this in four circumstances, each of which requires it
% to perform a different set of files from a different set of modules.
%
% 1     grab_unqual_imported_modules_make_int performs the task that is
%       chronologically first in the build-system lifecycle of a module,
%       named (say) module A. This task is to grab all the .int3 files needed
%       for the creation of A.int0, A.int, and A.int2. This task returns
%       its results as an aug_make_int_unit structure.
%
% 2     grab_qual_imported_modules_augment performs the next task
%       chronologically, which is to grab all the .int0, .int and .int2 files
%       needed for the creation of A.c, A.cs, A.java, A.opt, A.trans_opt,
%       and for all the other tasks described by op_mode_augment.
%       (Hence the suffix on the name.) This task returns its results
%       as an aug_compilation_unit structure.
%
% 3     grab_plain_opt_and_int_for_opt_files performs the third task
%       chronologically, which is to grab all the .opt files needed
%       for applying intermodule optimization to module A, together
%       with any .int0 and/or .int files needed to make sense of their
%       contents. This task records its results by updating an existing
%       aug_compilation_unit structure.
%
% 4     grab_trans_opt_files performs the fourth task chronologically,
%       which is to grab all the .trans_opt files needed for applying
%       *transitive* intermodule optimization to module A. It also records
%       its results by updating an existing aug_compilation_unit structure.
%
% The roles of the interface files (.int0, .int3, .int2 and .int) that
% this module reads in are documented (to the extent that they are documented
% anywhere) in the modules that create them, which are comp_unit_interface.m
% and (to a limited extent) write_module_interface_files.m.
%
% XXX The file notes/interface_files.html contains (a start on) some
% more comprehensive documentation.
%
%---------------------------------------------------------------------------%

:- module parse_tree.grab_modules.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % grab_qual_imported_modules_augment(ProgressStream, Globals,
    %   SourceFileName, SourceFileModuleName, MaybeTimestamp, MaybeTopModule,
    %   ParseTreeModuleSrc, Baggage, AugCompUnit, !HaveReadModuleMaps, !IO):
    %
    % Given ParseTreeModuleSrc, one of the modules stored in SourceFileName,
    % read in
    %
    % - the private interface files (.int0) for all the parent modules,
    % - the long interface files (.int) for all the imported modules, and
    % - the qualified short interface files (.in2) for all the indirectly
    %   imported modules.
    %
    % Return the aug_compilation_unit structure containing all the information
    % gathered this way.
    %
    % SourceFileModuleName is the top-level module name in SourceFileName.
    % ModuleTimestamp is the timestamp of the SourceFileName.
    % HaveReadModuleMaps contains the interface files read during
    % recompilation checking.
    %
    % Used when augmenting a module, which we do when asked to do
    % the tasks described by op_mode_augment. Most of the time, this is
    % generating target language code, but sometimes it may be e.g.
    % generating .opt and .trans_opt files.
    %
:- pred grab_qual_imported_modules_augment(io.text_output_stream::in,
    globals::in, file_name::in, module_name::in, maybe(timestamp)::in,
    maybe_top_module::in, parse_tree_module_src::in,
    module_baggage::out, aug_compilation_unit::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

    % grab_unqual_imported_modules_make_int(ProgressStream, Globals,
    %   SourceFileName, SourceFileModuleName, ParseTreeModuleSrc,
    %   Baggage, AugMakeIntUnit, !HaveReadModuleMaps, !IO):
    %
    % Similar to grab_imported_modules_augment, but only reads in the
    % unqualified short interfaces (.int3s), and the .int0 files for
    % parent modules, instead of reading the long interfaces and
    % qualified short interfaces (.int and int2s).
    %
    % Used when generating .int0 files, and when generating .int/.int2 files.
    %
:- pred grab_unqual_imported_modules_make_int(io.text_output_stream::in,
    globals::in, file_name::in, module_name::in,
    parse_tree_module_src::in, module_baggage::out, aug_make_int_unit::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

:- type maybe_opt_file_error
    --->    no_opt_file_error
    ;       opt_file_error.

    % grab_plain_opt_and_int_for_opt_files(ProgressStream, Globals, FoundError,
    %   !Baggage, !AugCompUnit, !HaveReadModuleMaps, !IO):
    %
    % Add the contents of the .opt files of imported modules, as well as
    % the .int files needed to make sense of them, to !AugCompUnit.
    %
:- pred grab_plain_opt_and_int_for_opt_files(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, maybe_opt_file_error::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

    % grab_trans_opt_files(ProgressStream, Globals, Modules, FoundError,
    %   !Baggage, !AugCompUnit, !HaveReadModuleMaps, !IO):
    %
    % Add the contents of the .trans_opt file of each module in Modules
    % to !AugCompUnit.
    %
:- pred grab_trans_opt_files(io.text_output_stream::in, globals::in,
    list(module_name)::in, maybe_opt_file_error::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.builtin_modules.
:- import_module parse_tree.check_import_accessibility.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.get_dependencies.
:- import_module parse_tree.item_util.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.write_error_spec.

:- import_module bool.
:- import_module cord.
:- import_module dir.
:- import_module map.
:- import_module one_or_more.
:- import_module set.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

grab_qual_imported_modules_augment(ProgressStream, Globals, SourceFileName,
        SourceFileModuleName, MaybeTimestamp, MaybeTopModule,
        ParseTreeModuleSrc0, !:Baggage, !:AugCompUnit,
        !HaveReadModuleMaps, !IO) :-
    % The predicates grab_imported_modules_augment and
    % grab_unqual_imported_modules_make_int have quite similar tasks.
    % Please keep the corresponding parts of these two predicates in sync.
    %
    % XXX ITEM_LIST Why aren't we updating !HaveReadModuleMaps?
    some [!IntIndirectImported, !ImpIndirectImported,
        !IntImpIndirectImported, !ImpImpIndirectImported]
    (
        % Construct the initial module import structure.
        ModuleName = ParseTreeModuleSrc0 ^ ptms_module_name,

        IntFIMs0 = ParseTreeModuleSrc0 ^ ptms_int_fims,
        ImpFIMs0 = ParseTreeModuleSrc0 ^ ptms_imp_fims,
        set.foldl(add_implicit_fim_for_module(ModuleName),
            ParseTreeModuleSrc0 ^ ptms_int_self_fim_langs, IntFIMs0, IntFIMs),
        set.foldl(add_implicit_fim_for_module(ModuleName),
            ParseTreeModuleSrc0 ^ ptms_imp_self_fim_langs, ImpFIMs0, ImpFIMs),
        ParseTreeModuleSrc1 = ParseTreeModuleSrc0 ^ ptms_int_fims := IntFIMs,
        ParseTreeModuleSrc  = ParseTreeModuleSrc1 ^ ptms_imp_fims := ImpFIMs,

        (
            MaybeTimestamp = yes(Timestamp),
            MaybeTimestampMap = yes(map.singleton(ModuleName,
                module_timestamp(fk_src, Timestamp, recomp_avail_src)))
        ;
            MaybeTimestamp = no,
            MaybeTimestampMap = no
        ),

        Errors0 = init_read_module_errors,
        init_aug_compilation_unit(ParseTreeModuleSrc, !:AugCompUnit),
        GrabbedFileMap0 =
            map.singleton(ModuleName, gf_src(ParseTreeModuleSrc)),
        !:Baggage = module_baggage(SourceFileName, dir.this_directory,
            SourceFileModuleName, MaybeTopModule, MaybeTimestampMap,
            GrabbedFileMap0, Errors0),

        SrcMap0 = !.HaveReadModuleMaps ^ hrmm_module_src,
        % XXX Should be map.det_insert.
        map.set(ModuleName, ParseTreeModuleSrc, SrcMap0, SrcMap),
        !HaveReadModuleMaps ^ hrmm_module_src := SrcMap,

        ImportUseMap = ParseTreeModuleSrc ^ ptms_import_use_map,
        import_and_or_use_map_to_module_name_contexts(ImportUseMap,
            IntImportMap, IntUseMap, ImpImportMap, ImpUseMap,
            IntUseImpImportMap),
        map.keys_as_set(IntImportMap, IntImports0),
        map.keys_as_set(IntUseMap, IntUses0),
        ImpImports = map.sorted_keys(ImpImportMap),
        ImpUses = map.sorted_keys(ImpUseMap),
        IntUseImpImports = map.sorted_keys(IntUseImpImportMap),

        % Get the .int0 files of the ancestor modules.
        %
        % Uses of the items declared in ancestor modules do not need
        % module qualifiers. Modules imported by ancestors are considered
        % to be visible in the current module.
        % XXX grab_unqual_imported_modules_make_int treats AncestorImported and
        % AncestorUsed slightly differently from !.IntImported and !.IntUsed.
        Ancestors = get_ancestors(ModuleName),
        grab_module_int0_files_for_acu(ProgressStream, Globals,
            "ancestors", rwi0_section,
            Ancestors, IntImports0, IntImports, IntUses0, IntUses,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),

        % Get the .int files of the modules imported using `import_module'.
        set.init(!:IntIndirectImported),
        set.init(!:ImpIndirectImported),
        set.init(!:IntImpIndirectImported),
        set.init(!:ImpImpIndirectImported),
        grab_module_int1_files(ProgressStream, Globals,
            "int_imported", rwi1_int_import,
            set.to_sorted_list(IntImports),
            !IntIndirectImported, !IntImpIndirectImported,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
        grab_module_int1_files(ProgressStream, Globals,
            "imp_imported", rwi1_imp_import,
            ImpImports,
            !ImpIndirectImported, !ImpImpIndirectImported,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),

        % Get the .int files of the modules imported using `use_module'.
        grab_module_int1_files(ProgressStream, Globals,
            "int_used", rwi1_int_use,
            set.to_sorted_list(IntUses),
            !IntIndirectImported, !IntImpIndirectImported,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
        grab_module_int1_files(ProgressStream, Globals,
            "imp_used", rwi1_imp_use,
            ImpUses,
            !ImpIndirectImported, !ImpImpIndirectImported,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),

        % Get the .int files of the modules imported using `use_module'
        % in the interface and `import_module' in the implementation.
        grab_module_int1_files(ProgressStream, Globals,
            "int_used_imp_imported", rwi1_int_use_imp_import,
            IntUseImpImports,
            !IntIndirectImported, !IntImpIndirectImported,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),

        % Get the .int2 files of the modules imported in .int files.
        grab_module_int2_files_transitively(ProgressStream, Globals,
            "int_indirect_imported", rwi2_int_use,
            !.IntIndirectImported, !IntImpIndirectImported,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
        grab_module_int2_files_transitively(ProgressStream, Globals,
            "imp_indirect_imported", rwi2_imp_use,
            !.ImpIndirectImported, !ImpImpIndirectImported,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),

        % Get the .int2 files of the modules indirectly imported
        % the implementation sections of .int/.int2 files.
        grab_module_int2_files_and_impls_transitively(ProgressStream, Globals,
            "int_imp_indirect_imported", rwi2_abstract,
            !.IntImpIndirectImported,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
        grab_module_int2_files_and_impls_transitively(ProgressStream, Globals,
            "imp_imp_indirect_imported", rwi2_abstract,
            !.ImpImpIndirectImported,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),

        globals.lookup_bool_option(Globals, experiment3, Experiment3),
        (
            Experiment3 = no
        ;
            Experiment3 = yes,
            % This compiler invocation can get type representation information
            % for all the types it has access to in the .int and/or .int2 files
            % of the modules it imported directly or indirectly, *except* for
            % the types it defines itself, and the types defined by its
            % ancestors (if any). For representation information for these
            % types, it must read its *own* .int file, and the .int file(s)
            % of its ancestors.
            grab_module_int1_file(ProgressStream, Globals, rwi1_type_repn,
                ModuleName, _IntUses, _ImpUses,
                !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
            list.map2_foldl4(
                grab_module_int1_file(ProgressStream, Globals, rwi1_type_repn),
                get_ancestors(ModuleName), _IntUsesList, _ImpUsesList,
                !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO)
        ),

        aug_comp_unit_get_import_accessibility_info(!.AugCompUnit,
            ImportAccessibilityInfo),
        check_import_accessibility(ParseTreeModuleSrc,
            ImportAccessibilityInfo, AccessSpecs),
        module_baggage_add_nonfatal_specs(AccessSpecs, !Baggage)
    ).

%---------------------------------------------------------------------------%

grab_unqual_imported_modules_make_int(ProgressStream, Globals, SourceFileName,
        SourceFileModuleName, ParseTreeModuleSrc,
        !:Baggage, !:AugMakeIntUnit, !HaveReadModuleMaps, !IO) :-
    % The predicates grab_imported_modules_augment and
    % grab_unqual_imported_modules_make_int have quite similar tasks.
    % Please keep the corresponding parts of these two predicates in sync.
    %
    % XXX ITEM_LIST Why aren't we updating !HaveReadModuleMaps?

    some [!IntIndirectImported, !ImpIndirectImported]
    (
        ImportAndOrUseMap = ParseTreeModuleSrc ^ ptms_import_use_map,
        import_and_or_use_map_to_module_name_contexts(ImportAndOrUseMap,
            IntImportMap, IntUseMap, ImpImportMap, ImpUseMap,
            IntUseImpImportMap),
        map.keys_as_set(IntImportMap, IntImports0),
        map.keys_as_set(IntUseMap, IntUses),
        map.keys_as_set(ImpImportMap, ImpImports),
        map.keys_as_set(ImpUseMap, ImpUses),
        map.keys_as_set(IntUseImpImportMap, IntUsesImpImports),
        set.insert(mercury_public_builtin_module, IntImports0, IntImports),

        ( if ParseTreeModuleSrc ^ ptms_module_name = SourceFileModuleName then
            % We lie about the set of modules nested inside this one;
            % the lie will be correct only by accident.
            MaybeTopModule = top_module(set.init)
        else
            MaybeTopModule = not_top_module
        ),
        MaybeTimestampMap = no,

        Errors0 = init_read_module_errors,
        init_aug_make_int_unit(ParseTreeModuleSrc, !:AugMakeIntUnit),
        GrabbedFileMap0 =
            map.singleton(ModuleName, gf_src(ParseTreeModuleSrc)),
        !:Baggage = module_baggage(SourceFileName, dir.this_directory,
            SourceFileModuleName, MaybeTopModule, MaybeTimestampMap,
            GrabbedFileMap0, Errors0),

        ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
        SrcMap0 = !.HaveReadModuleMaps ^ hrmm_module_src,
        map.set(ModuleName, ParseTreeModuleSrc, SrcMap0, SrcMap),
        !HaveReadModuleMaps ^ hrmm_module_src := SrcMap,

        % Get the .int0 files of the ancestor modules.
        Ancestors = get_ancestors(ModuleName),
        grab_module_int0_files_for_amiu(ProgressStream, Globals,
            "unqual_ancestors",
            Ancestors, set.init, AncestorImports, set.init, AncestorUses,
            !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO),

        % Get the .int3 files of the modules imported using `import_module'.
        set.init(!:IntIndirectImported),
        set.init(!:ImpIndirectImported),
        grab_module_int3_files(ProgressStream, Globals,
            "unqual_parent_imported", rwi3_direct_ancestor_import,
            set.to_sorted_list(AncestorImports),
            !IntIndirectImported, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO),
        grab_module_int3_files(ProgressStream, Globals,
            "unqual_int_imported", rwi3_direct_int_import,
            set.to_sorted_list(IntImports),
            !IntIndirectImported, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO),
        grab_module_int3_files(ProgressStream, Globals,
            "unqual_imp_imported", rwi3_direct_imp_import,
            set.to_sorted_list(ImpImports),
            !ImpIndirectImported, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO),

        % Get the .int3 files of the modules imported using `use_module'.
        grab_module_int3_files(ProgressStream, Globals,
            "unqual_parent_used", rwi3_direct_ancestor_use,
            set.to_sorted_list(AncestorUses),
            !IntIndirectImported, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO),
        grab_module_int3_files(ProgressStream, Globals,
            "unqual_int_used", rwi3_direct_int_use,
            set.to_sorted_list(IntUses),
            !IntIndirectImported, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO),
        grab_module_int3_files(ProgressStream, Globals,
            "unqual_imp_used", rwi3_direct_imp_use,
            set.to_sorted_list(ImpUses),
            !ImpIndirectImported, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO),

        % Get the .int3 files of the modules imported using `use_module'
        % in the interface and `import_module' in the implementation.
        grab_module_int3_files(ProgressStream, Globals,
            "unqual_int_used_imp_imported", rwi3_direct_int_use_imp_import,
            set.to_sorted_list(IntUsesImpImports),
            !IntIndirectImported, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO),

        % Get the .int3 files of the modules imported in .int3 files.
        grab_module_int3_files_transitively(ProgressStream, Globals,
            "unqual_int_indirect_imported", rwi3_indirect_int_use,
            !.IntIndirectImported, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO),
        grab_module_int3_files_transitively(ProgressStream, Globals,
            "unqual_imp_indirect_imported", rwi3_indirect_imp_use,
            !.ImpIndirectImported, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO),

        aug_make_int_unit_get_import_accessibility_info(!.AugMakeIntUnit,
            ImportAccessibilityInfo),
        check_import_accessibility(ParseTreeModuleSrc,
            ImportAccessibilityInfo, AccessSpecs),
        module_baggage_add_nonfatal_specs(AccessSpecs, !Baggage)
    ).

:- pred dump_modules(io.text_output_stream::in, set(module_name)::in,
    io::di, io::uo) is det.
:- pragma consider_used(pred(dump_modules/4)).

dump_modules(Stream, ModuleNames, !IO) :-
    ModuleNameStrs =
        set.to_sorted_list(set.map(sym_name_to_string, ModuleNames)),
    list.foldl(io.write_line(Stream), ModuleNameStrs, !IO).

%---------------------------------------------------------------------------%

grab_plain_opt_and_int_for_opt_files(ProgressStream, ErrorStream, Globals,
        FoundError, !Baggage, !AugCompUnit, !HaveReadModuleMaps, !IO) :-
    % Read in the .opt files for imported and ancestor modules.
    ParseTreeModuleSrc = !.AugCompUnit ^ acu_module_src,
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    Ancestors0 = get_ancestors_set(ModuleName),
    DirectDeps0 = map.keys_as_set(ParseTreeModuleSrc ^ ptms_import_use_map),
    % Some builtin modules can implicitly depend on themselves.
    % For those, we don't want to read in their .opt file, since we have
    % already in their .m file.
    set.delete(ModuleName, DirectDeps0, DirectDeps),
    OptModules = set.union(Ancestors0, DirectDeps),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    globals.lookup_bool_option(Globals, read_opt_files_transitively,
        ReadOptFilesTransitively),
    % Do not add to the queue either the modules in the initial queue,
    % or the module being compiled.
    set.insert(ModuleName, OptModules, DontQueueOptModules),
    read_plain_opt_files(ProgressStream, Globals, VeryVerbose,
        ReadOptFilesTransitively,
        set.to_sorted_list(OptModules), DontQueueOptModules,
        cord.empty, ParseTreePlainOptsCord0, set.init, ExplicitDeps,
        cord.empty, ImplicitNeedsCord, [], OptSpecs0,
        no_opt_file_error, OptError0, !IO),
    ParseTreePlainOpts0 = cord.list(ParseTreePlainOptsCord0),

    % Get the :- pragma unused_args(...) declarations created when writing
    % the .opt file for the current module. These are needed because we can
    % probably remove more arguments with intermod_unused_args, but the
    % interface for other modules must remain the same.
    %
    % Similarly for the  :- pragma structure_reuse(...) declarations. With more
    % information available when making the target code than when writing the
    % `.opt' file, it can turn out that a procedure which seemed to have
    % condition reuse actually has none. But we have to maintain the interface
    % for modules that use the conditional reuse information from the `.opt'
    % file.
    globals.get_opt_tuple(Globals, OptTuple),
    UnusedArgs = OptTuple ^ ot_opt_unused_args_intermod,
    globals.lookup_bool_option(Globals, structure_reuse_analysis,
        StructureReuse),
    ( if (UnusedArgs = opt_unused_args_intermod ; StructureReuse = yes) then
        read_module_plain_opt(ProgressStream, Globals, ModuleName,
            HaveReadOwnPlainOpt0, !IO),
        (
            HaveReadOwnPlainOpt0 = have_read_module(_, _,
                OwnParseTreePlainOpt0, OwnOptModuleErrors),
            % XXX We should store the whole parse_tree, with a note next to it
            % saying "keep only these two kinds of pragmas".
            keep_only_unused_and_reuse_pragmas_in_parse_tree_plain_opt(
                UnusedArgs, StructureReuse,
                OwnParseTreePlainOpt0, OwnParseTreePlainOpt),
            ParseTreePlainOpts = [OwnParseTreePlainOpt | ParseTreePlainOpts0],
            update_opt_error_status_on_success(OwnOptModuleErrors,
                OptSpecs0, OptSpecs1, OptError0, OptError)
        ;
            HaveReadOwnPlainOpt0 = have_not_read_module(OwnOptFileName, _),
            ParseTreePlainOpts = ParseTreePlainOpts0,
            update_opt_error_status_on_failure(Globals, warn_missing_opt_files,
                OwnOptFileName, OptSpecs0, OptSpecs1, OptError0, OptError)
        ),
        pre_hlds_maybe_write_out_errors(ErrorStream, VeryVerbose, Globals,
            OptSpecs1, OptSpecs, !IO)
    else
        ParseTreePlainOpts = ParseTreePlainOpts0,
        OptSpecs = OptSpecs0,
        OptError = OptError0
    ),

    list.foldl(aug_compilation_unit_add_plain_opt, ParseTreePlainOpts,
        !AugCompUnit),
    module_baggage_add_nonfatal_specs(OptSpecs, !Baggage),

    % Read .int0 files required by the `.opt' files, except the ones
    % we have already read as ancestors of ModuleName,
    % XXX This code reads in the .int0 files of the ancestors of *OptModules*,
    % but due to read_opt_files_transitively, ParseTreePlainOpts may contain
    % more the .opt files of modules that are *not* in OptModules.
    % This looks like a bug.
    OptModuleAncestors =
        set.power_union(set.map(get_ancestors_set, OptModules)),
    OldModuleAncestors = get_ancestors_set(ModuleName),
    set.insert(ModuleName, OldModuleAncestors, OldModuleAndAncestors),
    OptOnlyModuleAncestors =
        set.difference(OptModuleAncestors, OldModuleAndAncestors),

    grab_module_int0_files_for_acu(ProgressStream, Globals,
        "opt_int0s", rwi0_opt,
        set.to_sorted_list(OptOnlyModuleAncestors),
        set.init, OptAncestorImports, set.init, OptAncestorUses,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),

    % Figure out which .int files are implicitly needed by the .opt files.
    combine_implicit_needs(cord.list(ImplicitNeedsCord), AllImplicitNeeds),
    compute_implicit_avail_needs(Globals, AllImplicitNeeds, ImplicitDeps),
    NewDeps = set.union_list([ExplicitDeps, ImplicitDeps,
        OptAncestorImports, OptAncestorUses]),

    % Read in the .int, and .int2 files needed by the .opt files.
    grab_module_int1_files(ProgressStream, Globals,
        "opt_new_deps", rwi1_opt,
        set.to_sorted_list(NewDeps),
        set.init, NewIndirectDeps, set.init, NewImplIndirectDeps,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
    grab_module_int2_files_and_impls_transitively(ProgressStream, Globals,
        "opt_new_indirect_deps", rwi2_opt,
        set.union(NewIndirectDeps, NewImplIndirectDeps),
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),

    % Figure out whether anything went wrong.
    % XXX We should try to put all the relevant error indications into
    % !AugCompUnit, and let our caller figure out what to do with them.
    ModuleErrors = !.Baggage ^ mb_errors,
    ( if
        ( set.is_non_empty(ModuleErrors ^ rm_fatal_errors)
        ; set.is_non_empty(ModuleErrors ^ rm_nonfatal_errors)
        ; OptError = opt_file_error
        )
    then
        FoundError = opt_file_error
    else
        FoundError = no_opt_file_error
    ).

%---------------------------------------------------------------------------%

grab_trans_opt_files(ProgressStream, Globals, TransOptModuleNames, FoundError,
        !Baggage, !AugCompUnit, !HaveReadModuleMaps, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(ProgressStream, Verbose,
        "% Reading .trans_opt files..\n", !IO),
    maybe_flush_output(ProgressStream, Verbose, !IO),

    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    read_trans_opt_files(ProgressStream, Globals, VeryVerbose,
        TransOptModuleNames, cord.init, ParseTreeTransOptsCord,
        [], TransOptSpecs, no_opt_file_error, FoundError, !IO),
    list.foldl(aug_compilation_unit_add_trans_opt,
        cord.list(ParseTreeTransOptsCord), !AugCompUnit),
    module_baggage_add_nonfatal_specs(TransOptSpecs, !Baggage),
    % XXX why ignore any existing errors?
    !Baggage ^ mb_errors := init_read_module_errors,

    maybe_write_string(ProgressStream, Verbose, "% Done.\n", !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% XXX ITEM_LIST Document what the grab_module_int* predicates do
% more precisely, and document exactly WHY they do each of their actions.
% I (zs) think it likely that some of the interface files we now read in
% are read in unnecessarily.

%---------------------------------------------------------------------------%

:- pred grab_module_int2_files_and_impls_transitively(
    io.text_output_stream::in, globals::in,
    string::in, read_why_int2::in, set(module_name)::in,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    io::di, io::uo) is det.

grab_module_int2_files_and_impls_transitively(ProgressStream, Globals,
        Why, ReadWhy2, Modules, !HaveReadModuleMaps,
        !Baggage, !AugCompUnit, !IO) :-
    grab_module_int2_files_transitively(ProgressStream, Globals,
        Why, ReadWhy2, Modules, set.init, ImpIndirectImports,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
    ( if set.is_empty(ImpIndirectImports) then
        true
    else
        grab_module_int2_files_and_impls_transitively(ProgressStream, Globals,
            Why, ReadWhy2, ImpIndirectImports, !HaveReadModuleMaps,
            !Baggage, !AugCompUnit, !IO)
    ).

:- pred grab_module_int2_files_transitively(io.text_output_stream::in,
    globals::in, string::in, read_why_int2::in, set(module_name)::in,
    set(module_name)::in, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    io::di, io::uo) is det.

grab_module_int2_files_transitively(ProgressStream, Globals, Why, ReadWhy2,
        Modules, !ImpIndirectImports, !HaveReadModuleMaps,
        !Baggage, !AugCompUnit, !IO) :-
    grab_module_int2_files(ProgressStream, Globals, Why, ReadWhy2,
        set.to_sorted_list(Modules),
        set.init, IndirectImports, !ImpIndirectImports,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
    ( if set.is_empty(IndirectImports) then
        true
    else
        grab_module_int2_files_transitively(ProgressStream, Globals,
            Why, ReadWhy2, IndirectImports, !ImpIndirectImports,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO)
    ).

:- pred grab_module_int3_files_transitively(io.text_output_stream::in,
    globals::in, string::in, read_why_int3::in, set(module_name)::in,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_make_int_unit::in, aug_make_int_unit::out, io::di, io::uo) is det.

grab_module_int3_files_transitively(ProgressStream, Globals, Why, ReadWhy3,
        Modules, !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO) :-
    grab_module_int3_files(ProgressStream, Globals, Why, ReadWhy3,
        set.to_sorted_list(Modules), set.init, IndirectImports,
        !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO),
    ( if set.is_empty(IndirectImports) then
        true
    else
        grab_module_int3_files_transitively(ProgressStream, Globals,
            Why, ReadWhy3, IndirectImports, !HaveReadModuleMaps,
            !Baggage, !AugMakeIntUnit, !IO)
    ).

:- pred grab_module_int0_files_for_acu(io.text_output_stream::in, globals::in,
    string::in, read_why_int0::in, list(module_name)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    io::di, io::uo) is det.

grab_module_int0_files_for_acu(_ProgressStream, _Globals, _Why, _ReadWhy0, [],
        !DirectImports, !DirectUses,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO).
grab_module_int0_files_for_acu(ProgressStream, Globals, Why, ReadWhy0,
        [ModuleName | ModuleNames], !DirectImports, !DirectUses,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO) :-
    ( if should_read_interface(!.Baggage, ModuleName, ifk_int0) then
        maybe_log_augment_decision(ProgressStream, Why, ifk_int0, ReadWhy0,
            ModuleName, decided_to_read, !IO),
        grab_module_int0_file_for_acu(ProgressStream, Globals, ReadWhy0,
            ModuleName, IntImports, ImpImports, IntUses, ImpUses,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
        set.union(IntImports, !DirectImports),
        set.union(ImpImports, !DirectImports),
        set.union(IntUses, !DirectUses),
        set.union(ImpUses, !DirectUses)
    else
        maybe_log_augment_decision(ProgressStream, Why, ifk_int0, ReadWhy0,
            ModuleName, decided_not_to_read, !IO)
    ),
    grab_module_int0_files_for_acu(ProgressStream, Globals, Why, ReadWhy0,
        ModuleNames, !DirectImports, !DirectUses,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO).

:- pred grab_module_int0_files_for_amiu(io.text_output_stream::in, globals::in,
    string::in, list(module_name)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_make_int_unit::in, aug_make_int_unit::out,
    io::di, io::uo) is det.

grab_module_int0_files_for_amiu(_ProgressStream, _Globals, _Why, [],
        !DirectImports, !DirectUses,
        !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO).
grab_module_int0_files_for_amiu(ProgressStream, Globals, Why,
        [ModuleName | ModuleNames], !DirectImports, !DirectUses,
        !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO) :-
    ReadWhy0 = rwi0_section,
    ( if should_read_interface(!.Baggage, ModuleName, ifk_int0) then
        maybe_log_augment_decision(ProgressStream, Why, ifk_int0, ReadWhy0,
            ModuleName, decided_to_read, !IO),
        grab_module_int0_file_for_amiu(ProgressStream, Globals, ModuleName,
            IntImports, ImpImports, IntUses, ImpUses,
            !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO),
        set.union(IntImports, !DirectImports),
        set.union(ImpImports, !DirectImports),
        set.union(IntUses, !DirectUses),
        set.union(ImpUses, !DirectUses)
    else
        maybe_log_augment_decision(ProgressStream, Why, ifk_int0, ReadWhy0,
            ModuleName, decided_not_to_read, !IO)
    ),
    grab_module_int0_files_for_amiu(ProgressStream, Globals, Why, ModuleNames,
        !DirectImports, !DirectUses,
        !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO).

:- pred grab_module_int1_files(io.text_output_stream::in, globals::in,
    string::in, read_why_int1::in, list(module_name)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    io::di, io::uo) is det.

grab_module_int1_files(_ProgressStream, _Globals, _Why, _ReadWhy1,
        [], !IntIndirectImports, !ImpIndirectImports,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO).
grab_module_int1_files(ProgressStream, Globals, Why, ReadWhy1,
        [ModuleName | ModuleNames], !IntIndirectImports, !ImpIndirectImports,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO) :-
    ( if should_read_interface(!.Baggage, ModuleName, ifk_int1) then
        maybe_log_augment_decision(ProgressStream, Why, ifk_int1, ReadWhy1,
            ModuleName, decided_to_read, !IO),
        grab_module_int1_file(ProgressStream, Globals, ReadWhy1, ModuleName,
            IntUses, ImpUses, !HaveReadModuleMaps,
            !Baggage, !AugCompUnit, !IO),
        set.union(IntUses, !IntIndirectImports),
        set.union(ImpUses, !ImpIndirectImports)
    else
        maybe_log_augment_decision(ProgressStream, Why, ifk_int1, ReadWhy1,
            ModuleName, decided_not_to_read, !IO)
    ),
    grab_module_int1_files(ProgressStream, Globals, Why, ReadWhy1,
        ModuleNames, !IntIndirectImports, !ImpIndirectImports,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO).

:- pred grab_module_int2_files(io.text_output_stream::in, globals::in,
    string::in, read_why_int2::in, list(module_name)::in,
    set(module_name)::in, set(module_name)::out,
    set(module_name)::in, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    io::di, io::uo) is det.

grab_module_int2_files(_ProgressStream, _Globals, _Why, _ReadWhy2,
        [], !IntIndirectImports, !ImpIndirectImports,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO).
grab_module_int2_files(ProgressStream, Globals, Why, ReadWhy2,
        [ModuleName | ModuleNames], !IntIndirectImports, !ImpIndirectImports,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO) :-
    ( if should_read_interface(!.Baggage, ModuleName, ifk_int2) then
        maybe_log_augment_decision(ProgressStream, Why, ifk_int2, ReadWhy2,
            ModuleName, decided_to_read, !IO),
        grab_module_int2_file(ProgressStream, Globals, ReadWhy2, ModuleName,
            IntUses, ImpUses,
            !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO),
        set.union(IntUses, !IntIndirectImports),
        set.union(ImpUses, !ImpIndirectImports)
    else
        maybe_log_augment_decision(ProgressStream, Why, ifk_int2, ReadWhy2,
            ModuleName, decided_not_to_read, !IO)
    ),
    grab_module_int2_files(ProgressStream, Globals, Why, ReadWhy2,
        ModuleNames, !IntIndirectImports, !ImpIndirectImports,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO).

:- pred grab_module_int3_files(io.text_output_stream::in, globals::in,
    string::in, read_why_int3::in, list(module_name)::in,
    set(module_name)::in, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_make_int_unit::in, aug_make_int_unit::out, io::di, io::uo) is det.

grab_module_int3_files(_ProgressStream, _Globals, _Why, _ReadWhy3,
        [], !IntIndirectImports, !HaveReadModuleMaps,
        !Baggage, !AugMakeIntUnit, !IO).
grab_module_int3_files(ProgressStream, Globals, Why, ReadWhy3,
        [ModuleName | ModuleNames], !IntIndirectImports, !HaveReadModuleMaps,
        !Baggage, !AugMakeIntUnit, !IO) :-
    ( if should_read_interface(!.Baggage, ModuleName, ifk_int3) then
        maybe_log_augment_decision(ProgressStream, Why, ifk_int3, ReadWhy3,
            ModuleName, decided_to_read, !IO),
        grab_module_int3_file(ProgressStream, Globals, ReadWhy3, ModuleName,
            IntImports, !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO),
        set.union(IntImports, !IntIndirectImports)
    else
        maybe_log_augment_decision(ProgressStream, Why, ifk_int3, ReadWhy3,
            ModuleName, decided_not_to_read, !IO)
    ),
    grab_module_int3_files(ProgressStream, Globals, Why, ReadWhy3,
        ModuleNames, !IntIndirectImports, !HaveReadModuleMaps,
        !Baggage, !AugMakeIntUnit, !IO).

:- pred should_read_interface(module_baggage::in, module_name::in,
    int_file_kind::in) is semidet.

should_read_interface(Baggage, ModuleName, FileKind) :-
    GrabbedFileMap = Baggage ^ mb_grabbed_file_map,
    ( if map.search(GrabbedFileMap, ModuleName, OldGrabbedFile) then
        OldFileKind = grabbed_file_to_file_kind(OldGrabbedFile),
        ( if compare((<), fk_int(FileKind), OldFileKind) then
            true
        else
            fail
        )
    else
        true
    ).

:- func grabbed_file_to_file_kind(grabbed_file) = file_kind.

grabbed_file_to_file_kind(GrabbedWhy) = FileKind :-
    ( GrabbedWhy = gf_src(_),     FileKind = fk_src
    ; GrabbedWhy = gf_int0(_, _), FileKind = fk_int(ifk_int0)
    ; GrabbedWhy = gf_int1(_, _), FileKind = fk_int(ifk_int1)
    ; GrabbedWhy = gf_int2(_, _), FileKind = fk_int(ifk_int2)
    ; GrabbedWhy = gf_int3(_, _), FileKind = fk_int(ifk_int3)
    ).

%---------------------------------------------------------------------------%

:- pred grab_module_int0_file_for_acu(io.text_output_stream::in, globals::in,
    read_why_int0::in, module_name::in,
    set(module_name)::out, set(module_name)::out,
    set(module_name)::out, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    io::di, io::uo) is det.

grab_module_int0_file_for_acu(ProgressStream, Globals, ReadWhy0, ModuleName,
        IntImports, ImpImports, IntUses, ImpUses,
        !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO) :-
    do_we_need_timestamps(!.Baggage, ReturnTimestamp),
    maybe_read_module_int0(ProgressStream, Globals, do_search, ModuleName,
        ReturnTimestamp, HaveReadInt0, !HaveReadModuleMaps, !IO),
    (
        HaveReadInt0 = have_read_module(_FileName, MaybeTimestamp,
            ParseTreeInt0, Errors),
        GrabbedFile = gf_int0(ParseTreeInt0, ReadWhy0),
        module_baggage_add_grabbed_file(ModuleName, GrabbedFile, !Baggage),
        (
            ReadWhy0 = rwi0_section,
            AncestorIntSpec = ancestor_int0(ParseTreeInt0, ReadWhy0),
            aug_compilation_unit_add_ancestor_int_spec(AncestorIntSpec,
                !AugCompUnit)
        ;
            ReadWhy0 = rwi0_opt,
            IntForOptSpec = for_opt_int0(ParseTreeInt0, ReadWhy0),
            aug_compilation_unit_add_int_for_opt_spec(IntForOptSpec,
                !AugCompUnit)
        ),
        maybe_record_interface_timestamp(ModuleName, ifk_int0,
            recomp_avail_int_import, MaybeTimestamp, !Baggage),
        map.foldl4(get_imports_uses, ParseTreeInt0 ^ pti0_import_use_map,
            set.init, IntImports, set.init, ImpImports,
            set.init, IntUses, set.init, ImpUses),
        aug_compilation_unit_maybe_add_module_version_numbers(ModuleName,
            ParseTreeInt0 ^ pti0_maybe_version_numbers, !AugCompUnit)
    ;
        HaveReadInt0 = have_not_read_module(_FileName, Errors),
        set.init(IntImports),
        set.init(ImpImports),
        set.init(IntUses),
        set.init(ImpUses)
    ),
    module_baggage_add_errors(Errors, !Baggage).

:- pred grab_module_int0_file_for_amiu(io.text_output_stream::in,
    globals::in, module_name::in,
    set(module_name)::out, set(module_name)::out,
    set(module_name)::out, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_make_int_unit::in, aug_make_int_unit::out,
    io::di, io::uo) is det.

grab_module_int0_file_for_amiu(ProgressStream, Globals, ModuleName,
        IntImports, ImpImports, IntUses, ImpUses,
        !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO) :-
    do_we_need_timestamps(!.Baggage, ReturnTimestamp),
    maybe_read_module_int0(ProgressStream, Globals, do_search, ModuleName,
        ReturnTimestamp, HaveReadInt0, !HaveReadModuleMaps, !IO),
    (
        HaveReadInt0 = have_read_module(_FileName, MaybeTimestamp,
            ParseTreeInt0, Errors),
        GrabbedFile = gf_int0(ParseTreeInt0, rwi0_section),
        module_baggage_add_grabbed_file(ModuleName, GrabbedFile, !Baggage),
        aug_make_int_unit_add_ancestor_int(ParseTreeInt0, !AugMakeIntUnit),
        maybe_record_interface_timestamp(ModuleName, ifk_int0,
            recomp_avail_int_import, MaybeTimestamp, !Baggage),
        map.foldl4(get_imports_uses, ParseTreeInt0 ^ pti0_import_use_map,
            set.init, IntImports, set.init, ImpImports,
            set.init, IntUses, set.init, ImpUses),
        aug_make_int_unit_maybe_add_module_version_numbers(ModuleName,
            ParseTreeInt0 ^ pti0_maybe_version_numbers, !AugMakeIntUnit)
    ;
        HaveReadInt0 = have_not_read_module(_FileName, Errors),
        set.init(IntImports),
        set.init(ImpImports),
        set.init(IntUses),
        set.init(ImpUses)
    ),
    module_baggage_add_errors(Errors, !Baggage).

:- pred grab_module_int1_file(io.text_output_stream::in, globals::in,
    read_why_int1::in, module_name::in,
    set(module_name)::out, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    io::di, io::uo) is det.

grab_module_int1_file(ProgressStream, Globals, ReadWhy1, ModuleName,
        IntUses, ImpUses, !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO) :-
    do_we_need_timestamps(!.Baggage, ReturnTimestamp),
    maybe_read_module_int1(ProgressStream, Globals, do_search, ModuleName,
        ReturnTimestamp, HaveReadInt1, !HaveReadModuleMaps, !IO),
    (
        HaveReadInt1 = have_read_module(_FileName, MaybeTimestamp,
            ParseTreeInt1, Errors),
        GrabbedFile = gf_int1(ParseTreeInt1, ReadWhy1),
        module_baggage_add_grabbed_file(ModuleName, GrabbedFile, !Baggage),
        (
            (
                ReadWhy1 = rwi1_int_import,
                RecompAvail = recomp_avail_int_import
            ;
                ReadWhy1 = rwi1_int_use,
                RecompAvail = recomp_avail_int_use
            ;
                ReadWhy1 = rwi1_imp_import,
                RecompAvail = recomp_avail_imp_import
            ;
                ReadWhy1 = rwi1_imp_use,
                RecompAvail = recomp_avail_imp_use
            ;
                ReadWhy1 = rwi1_int_use_imp_import,
                RecompAvail = recomp_avail_int_use_imp_import
            ),
            DirectIntSpec = direct_int1(ParseTreeInt1, ReadWhy1),
            aug_compilation_unit_add_direct_int1_spec(DirectIntSpec,
                !AugCompUnit),
            module_baggage_add_errors(Errors, !Baggage)
        ;
            ReadWhy1 = rwi1_opt,
            RecompAvail = recomp_avail_imp_use,
            IntForOptSpec = for_opt_int1(ParseTreeInt1, ReadWhy1),
            aug_compilation_unit_add_int_for_opt_spec(IntForOptSpec,
                !AugCompUnit),
            module_baggage_add_errors(Errors, !Baggage)
        ;
            ReadWhy1 = rwi1_type_repn,
            RecompAvail = recomp_avail_int_import,
            TypeRepnSpec = type_repn_spec_int1(ParseTreeInt1),
            aug_compilation_unit_add_type_repn_spec(TypeRepnSpec, !AugCompUnit)
            % Do not add Errors to !AugCompUnit. Any error messages in there
            % will be reported when the source file of the affected module
            % is compiled to target code.
        ),
        map.foldl2(get_uses, ParseTreeInt1 ^ pti1_use_map,
            set.init, IntUses, set.init, ImpUses),
        maybe_record_interface_timestamp(ModuleName, ifk_int1, RecompAvail,
            MaybeTimestamp, !Baggage),
        aug_compilation_unit_maybe_add_module_version_numbers(ModuleName,
            ParseTreeInt1 ^ pti1_maybe_version_numbers, !AugCompUnit)
    ;
        HaveReadInt1 = have_not_read_module(_FileName, Errors),
        set.init(IntUses),
        set.init(ImpUses),
        module_baggage_add_errors(Errors, !Baggage)
    ).

:- pred grab_module_int2_file(io.text_output_stream::in, globals::in,
    read_why_int2::in, module_name::in,
    set(module_name)::out, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_compilation_unit::in, aug_compilation_unit::out,
    io::di, io::uo) is det.

grab_module_int2_file(ProgressStream, Globals, ReadWhy2, ModuleName,
        IntUses, ImpUses, !HaveReadModuleMaps, !Baggage, !AugCompUnit, !IO) :-
    do_we_need_timestamps(!.Baggage, ReturnTimestamp),
    maybe_read_module_int2(ProgressStream, Globals, do_search, ModuleName,
        ReturnTimestamp, HaveReadInt2, !HaveReadModuleMaps, !IO),
    (
        HaveReadInt2 = have_read_module(_FileName, MaybeTimestamp,
            ParseTreeInt2, Errors),
        GrabbedFile = gf_int2(ParseTreeInt2, ReadWhy2),
        module_baggage_add_grabbed_file(ModuleName, GrabbedFile, !Baggage),
        (
            ( ReadWhy2 = rwi2_int_use
            ; ReadWhy2 = rwi2_imp_use
            ; ReadWhy2 = rwi2_abstract
            ),
            IndirectIntSpec = indirect_int2(ParseTreeInt2, ReadWhy2),
            aug_compilation_unit_add_indirect_int2_spec(IndirectIntSpec,
                !AugCompUnit)
        ;
            ReadWhy2 = rwi2_opt,
            IntForOptSpec = for_opt_int2(ParseTreeInt2, ReadWhy2),
            aug_compilation_unit_add_int_for_opt_spec(IntForOptSpec,
                !AugCompUnit)
        ),
        map.foldl2(get_uses, ParseTreeInt2 ^ pti2_use_map,
            set.init, IntUses, set.init, ImpUses),
        maybe_record_interface_timestamp(ModuleName, ifk_int2,
            recomp_avail_imp_use, MaybeTimestamp, !Baggage),
        aug_compilation_unit_maybe_add_module_version_numbers(ModuleName,
            ParseTreeInt2 ^ pti2_maybe_version_numbers, !AugCompUnit)
    ;
        HaveReadInt2 = have_not_read_module(_FileName, Errors),
        set.init(IntUses),
        set.init(ImpUses)
    ),
    module_baggage_add_errors(Errors, !Baggage).

:- pred grab_module_int3_file(io.text_output_stream::in, globals::in,
    read_why_int3::in, module_name::in, set(module_name)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    module_baggage::in, module_baggage::out,
    aug_make_int_unit::in, aug_make_int_unit::out, io::di, io::uo) is det.

grab_module_int3_file(ProgressStream, Globals, ReadWhy3, ModuleName, IntImports,
        !HaveReadModuleMaps, !Baggage, !AugMakeIntUnit, !IO) :-
    do_we_need_timestamps(!.Baggage, ReturnTimestamp),
    maybe_read_module_int3(ProgressStream, Globals, do_search, ModuleName,
        ReturnTimestamp, HaveReadInt3, !HaveReadModuleMaps, !IO),
    (
        HaveReadInt3 = have_read_module(_FileName, MaybeTimestamp,
            ParseTreeInt3, Errors),
        GrabbedFile = gf_int3(ParseTreeInt3, ReadWhy3),
        module_baggage_add_grabbed_file(ModuleName, GrabbedFile, !Baggage),
        (
            (
                ReadWhy3 = rwi3_direct_ancestor_import,
                RecompAvail = recomp_avail_int_import
            ;
                ReadWhy3 = rwi3_direct_int_import,
                RecompAvail = recomp_avail_int_import
            ;
                ReadWhy3 = rwi3_direct_imp_import,
                RecompAvail = recomp_avail_imp_import
            ;
                ReadWhy3 = rwi3_direct_ancestor_use,
                RecompAvail = recomp_avail_int_use
            ;
                ReadWhy3 = rwi3_direct_int_use,
                RecompAvail = recomp_avail_int_use
            ;
                ReadWhy3 = rwi3_direct_imp_use,
                RecompAvail = recomp_avail_imp_use
            ;
                ReadWhy3 = rwi3_direct_int_use_imp_import,
                RecompAvail = recomp_avail_int_use_imp_import
            ),
            DirectIntSpec = direct_int3(ParseTreeInt3, ReadWhy3),
            aug_make_int_unit_add_direct_int3_spec(DirectIntSpec,
                !AugMakeIntUnit)
        ;
            (
                ReadWhy3 = rwi3_indirect_int_use,
                RecompAvail = recomp_avail_int_use
            ;
                ReadWhy3 = rwi3_indirect_imp_use,
                RecompAvail = recomp_avail_imp_use
            ),
            IndirectIntSpec = indirect_int3(ParseTreeInt3, ReadWhy3),
            aug_make_int_unit_add_indirect_int3_spec(IndirectIntSpec,
                !AugMakeIntUnit)
        ),
        IntImportMap = ParseTreeInt3 ^ pti3_int_import_map,
        map.keys_as_set(IntImportMap, IntImports),
        maybe_record_interface_timestamp(ModuleName, ifk_int3, RecompAvail,
            MaybeTimestamp, !Baggage)
    ;
        HaveReadInt3 = have_not_read_module(_FileName, Errors),
        set.init(IntImports)
    ),
    module_baggage_add_errors(Errors, !Baggage).

%---------------------------------------------------------------------------%

:- type read_decision
    --->    decided_not_to_read
    ;       decided_to_read.

:- pred maybe_log_augment_decision(io.text_output_stream::in, string::in,
    int_file_kind::in, T::in, module_name::in, read_decision::in,
    io::di, io::uo) is det.
% Inlining calls to this predicate effectively optimizes it away
% if the trace condition is not met, as it usually won't be.
:- pragma inline(pred(maybe_log_augment_decision/8)).

maybe_log_augment_decision(ProgressStream, Why, IntFileKind, ReadWhy,
        ModuleName, Read, !IO) :-
    trace [compile_time(flag("log_augment_decisions")),
        runtime(env("LOG_AUGMENT_DECISION")), io(!TIO)]
    (
        ModuleNameStr = sym_name_to_string(ModuleName),
        ( IntFileKind = ifk_int0, ExtStr = ".int0"
        % We print ".int1" instead of the correct ".int" for alignment.
        ; IntFileKind = ifk_int1, ExtStr = ".int1"
        ; IntFileKind = ifk_int2, ExtStr = ".int2"
        ; IntFileKind = ifk_int3, ExtStr = ".int3"
        ),
        WhyStr = string.string(ReadWhy),
        (
            Read = decided_not_to_read,
            ReadStr = "decided not to read"
        ;
            Read = decided_to_read,
            ReadStr = "decided to read"
        ),
        io.format(ProgressStream, "AUGMENT_LOG %s, %s, %s, %s: %s\n",
            [s(Why), s(ModuleNameStr), s(ExtStr), s(WhyStr), s(ReadStr)], !TIO)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_record_interface_timestamp(module_name::in, int_file_kind::in,
    recomp_avail::in, maybe(timestamp)::in,
    module_baggage::in, module_baggage::out) is det.

maybe_record_interface_timestamp(ModuleName, IntFileKind, RecompAvail,
        MaybeTimestamp, !Baggage) :-
    MaybeTimestampMap = !.Baggage ^ mb_maybe_timestamp_map,
    (
        MaybeTimestampMap = yes(TimestampMap0),
        (
            MaybeTimestamp = yes(Timestamp),
            FileKind = fk_int(IntFileKind),
            TimestampInfo = module_timestamp(FileKind, Timestamp, RecompAvail),
            map.set(ModuleName, TimestampInfo, TimestampMap0, TimestampMap),
            !Baggage ^ mb_maybe_timestamp_map := yes(TimestampMap)
        ;
            MaybeTimestamp = no
        )
    ;
        MaybeTimestampMap = no
    ).

:- pred do_we_need_timestamps(module_baggage::in,
    maybe_return_timestamp::out) is det.

do_we_need_timestamps(Baggage, MaybeReturnTimestamp) :-
    MaybeTimestampMap = Baggage ^ mb_maybe_timestamp_map,
    ( MaybeTimestampMap = yes(_), MaybeReturnTimestamp = do_return_timestamp
    ; MaybeTimestampMap = no,     MaybeReturnTimestamp = dont_return_timestamp
    ).

:- pred module_baggage_add_grabbed_file(module_name::in, grabbed_file::in,
    module_baggage::in, module_baggage::out) is det.

module_baggage_add_grabbed_file(ModuleName, FileWhy, !Baggage) :-
    GrabbedFileMap0 = !.Baggage ^ mb_grabbed_file_map,
    % We could be adding a new entry to the map, or overwriting an existing
    % entry. XXX Why would we overwrite an entry?
    map.set(ModuleName, FileWhy, GrabbedFileMap0, GrabbedFileMap),
    !Baggage ^ mb_grabbed_file_map := GrabbedFileMap.

:- pred module_baggage_add_nonfatal_specs(list(error_spec)::in,
    module_baggage::in, module_baggage::out) is det.

module_baggage_add_nonfatal_specs(NewSpecs, !Baggage) :-
    Errors0 = !.Baggage ^ mb_errors,
    NonFatalErrorSpecs0 = Errors0 ^ rm_nonfatal_error_specs,
    NonFatalErrorSpecs = NewSpecs ++ NonFatalErrorSpecs0,
    Errors = Errors0 ^ rm_nonfatal_error_specs := NonFatalErrorSpecs,
    !Baggage ^ mb_errors := Errors.

:- pred module_baggage_add_errors(read_module_errors::in,
    module_baggage::in, module_baggage::out) is det.

module_baggage_add_errors(Errors1, !Baggage) :-
    Errors0 = !.Baggage ^ mb_errors,
    Errors0 = read_module_errors(FatalErrors0, FatalErrorSpecs0,
        NonFatalErrors0, NonFatalErrorSpecs0, WarningSpecs0),
    Errors1 = read_module_errors(FatalErrors1, FatalErrorSpecs1,
        NonFatalErrors1, NonFatalErrorSpecs1, WarningSpecs1),
    FatalErrors = set.union(FatalErrors0, FatalErrors1),
    FatalErrorSpecs = FatalErrorSpecs0 ++ FatalErrorSpecs1,
    NonFatalErrors = set.union(NonFatalErrors0, NonFatalErrors1),
    NonFatalErrorSpecs = NonFatalErrorSpecs0 ++ NonFatalErrorSpecs1,
    WarningSpecs = WarningSpecs0 ++ WarningSpecs1,
    Errors = read_module_errors(FatalErrors, FatalErrorSpecs,
        NonFatalErrors, NonFatalErrorSpecs, WarningSpecs),
    !Baggage ^ mb_errors := Errors.

%---------------------------------------------------------------------------%

:- pred keep_only_unused_and_reuse_pragmas_in_parse_tree_plain_opt(
    maybe_opt_unused_args_intermod::in, bool::in,
    parse_tree_plain_opt::in, parse_tree_plain_opt::out) is det.

keep_only_unused_and_reuse_pragmas_in_parse_tree_plain_opt(
        KeepUnusedArgs, KeepReuses, ParseTreePlainOpt0, ParseTreePlainOpt) :-
    ParseTreePlainOpt0 = parse_tree_plain_opt(ModuleName, ModuleNameContext,
        _UsedModuleNames, _FIMSpecs, _TypeDefns, _ForeignEnums,
        _InstDefns, _ModeDefns, _TypeClasses, _Instances,
        _PredDecls, _ModeDecls, _Clauses, _ForeignProcs, _Promises,
        _DeclMarkers, _ImplMarkers, _TypeSpecs,
        UnusedArgs0, _TermInfos, _Term2Infos,
        _Exceptions, _Trailings, _MMTablings, _Sharings, Reuses0),
    (
        KeepUnusedArgs = opt_unused_args_intermod,
        UnusedArgs = UnusedArgs0
    ;
        KeepUnusedArgs = do_not_opt_unused_args_intermod,
        UnusedArgs = []
    ),
    (
        KeepReuses = yes,
        Reuses = Reuses0
    ;
        KeepReuses = no,
        Reuses = []
    ),
    ParseTreePlainOpt = parse_tree_plain_opt(ModuleName, ModuleNameContext,
        map.init, set.init, [], [], [], [], [], [], [], [], [], [], [],
        [], [], [], UnusedArgs, [], [], [], [], [], [], Reuses).

:- pred read_plain_opt_files(io.text_output_stream::in, globals::in,
    bool::in, bool::in, list(module_name)::in, set(module_name)::in,
    cord(parse_tree_plain_opt)::in, cord(parse_tree_plain_opt)::out,
    set(module_name)::in, set(module_name)::out,
    cord(implicit_avail_needs)::in, cord(implicit_avail_needs)::out,
    list(error_spec)::in, list(error_spec)::out,
    maybe_opt_file_error::in, maybe_opt_file_error::out,
    io::di, io::uo) is det.

read_plain_opt_files(_, _, _, _, [], _, !ParseTreePlainOptsCord,
        !ExplicitDeps, !ImplicitNeeds, !Specs, !OptError, !IO).
read_plain_opt_files(ProgressStream, Globals, VeryVerbose,
        ReadOptFilesTransitively, [ModuleName | ModuleNames0],
        DontQueueOptModules0, !ParseTreePlainOptsCord,
        !ExplicitDeps, !ImplicitNeeds, !Specs, !OptError, !IO) :-
    read_module_plain_opt(ProgressStream, Globals, ModuleName,
        HaveReadPlainOpt, !IO),
    (
        HaveReadPlainOpt = have_read_module(_FileName, _MaybeTimestamp,
            ParseTreePlainOpt, ModuleErrors),
        cord.snoc(ParseTreePlainOpt, !ParseTreePlainOptsCord),
        update_opt_error_status_on_success(ModuleErrors, !Specs, !OptError),

        get_explicit_and_implicit_avail_needs_in_parse_tree_plain_opt(
            ParseTreePlainOpt, ParseTreeExplicitDeps, ParseTreeImplicitNeeds),
        set.union(ParseTreeExplicitDeps, !ExplicitDeps),
        cord.snoc(ParseTreeImplicitNeeds, !ImplicitNeeds),
        (
            ReadOptFilesTransitively = yes,
            compute_implicit_avail_needs(Globals, ParseTreeImplicitNeeds,
                ParseTreeImplicitDeps),
            set.union(ParseTreeExplicitDeps, ParseTreeImplicitDeps,
                ParseTreeDeps),
            set.difference(ParseTreeDeps, DontQueueOptModules0, NewDeps),
            ModuleNames1 = set.to_sorted_list(NewDeps) ++ ModuleNames0,
            set.union(NewDeps, DontQueueOptModules0, DontQueueOptModules1)
        ;
            ReadOptFilesTransitively = no,
            ModuleNames1 = ModuleNames0,
            DontQueueOptModules1 = DontQueueOptModules0
        )
    ;
        HaveReadPlainOpt = have_not_read_module(FileName, _ModuleErrors),
        update_opt_error_status_on_failure(Globals, warn_missing_opt_files,
            FileName, !Specs, !OptError),
        ModuleNames1 = ModuleNames0,
        DontQueueOptModules1 = DontQueueOptModules0
    ),

    pre_hlds_maybe_write_out_errors(ProgressStream, VeryVerbose, Globals,
        !Specs, !IO),
    read_plain_opt_files(ProgressStream, Globals, VeryVerbose,
        ReadOptFilesTransitively, ModuleNames1, DontQueueOptModules1,
        !ParseTreePlainOptsCord, !ExplicitDeps, !ImplicitNeeds,
        !Specs, !OptError, !IO).

%---------------------------------------------------------------------------%

:- pred read_trans_opt_files(io.text_output_stream::in, globals::in,
    bool::in, list(module_name)::in,
    cord(parse_tree_trans_opt)::in, cord(parse_tree_trans_opt)::out,
    list(error_spec)::in, list(error_spec)::out,
    maybe_opt_file_error::in, maybe_opt_file_error::out,
    io::di, io::uo) is det.

read_trans_opt_files(_, _, _, [], !ParseTreeTransOpts, !Specs, !Error, !IO).
read_trans_opt_files(ProgressStream, Globals, VeryVerbose,
        [ModuleName | ModuleNames], !ParseTreeTransOptsCord,
        !Specs, !OptError, !IO) :-
    read_module_trans_opt(ProgressStream, Globals, ModuleName,
        HaveReadTransOpt, !IO),
    (
        HaveReadTransOpt = have_read_module(_FileName, _MaybeTimestamp,
            ParseTreeTransOpt, ModuleErrors),
        cord.snoc(ParseTreeTransOpt, !ParseTreeTransOptsCord),
        update_opt_error_status_on_success(ModuleErrors, !Specs, !OptError)
    ;
        HaveReadTransOpt = have_not_read_module(FileName, _Errors),
        update_opt_error_status_on_failure(Globals,
            warn_missing_trans_opt_files, FileName, !Specs, !OptError)
    ),
    pre_hlds_maybe_write_out_errors(ProgressStream, VeryVerbose, Globals,
        !Specs, !IO),

    read_trans_opt_files(ProgressStream, Globals, VeryVerbose,
        ModuleNames, !ParseTreeTransOptsCord, !Specs, !OptError, !IO).

%---------------------------------------------------------------------------%

    % update_opt_error_status_on_failure(Globals, WarnOption, FileName,
    %   !Specs, !Error):
    %
    % Process the failure of opening a .opt or .trans_opt file.
    %
    % Our caller does not pass us the error message for that failure,
    % since we always construct our own error message.
    % XXX This may not be the best course of action.
    %
    % A missing `.opt' or `.trans_opt' file is only a fatal error if
    % `--halt-at-warn' was passed the compiler together with
    % `--warn-missing-opt-files' or `--warn-missing-trans-opt-files'
    % respectively.
    %
:- pred update_opt_error_status_on_failure(globals::in, option::in,
    file_name::in, list(error_spec)::in, list(error_spec)::out,
    maybe_opt_file_error::in, maybe_opt_file_error::out) is det.

update_opt_error_status_on_failure(Globals, WarnOption, FileName,
        !Specs, !Error) :-
    % We get here if we couldn't find and/or open the file.
    % ModuleErrors ^ rm_fatal_error_specs will already contain
    % an error_severity error_spec about that, with more details
    % than the message we generate below, but the test case
    % hard_coded/intermod_unused_args insists on there being no error,
    % only a warning with the text below. That is why we do not add
    % any error_specs from ModuleErrors to !Specs here.
    %
    % I (zs) don't know whether we should add a version of ModuleSpecs
    % with downgraded severity to !Specs instead of the Spec we generate
    % below.
    globals.lookup_bool_option(Globals, WarnOption, WarnOptionValue),
    (
        WarnOptionValue = no
    ;
        WarnOptionValue = yes,
        Pieces = [words("Warning: cannot open"), quote(FileName),
            suffix("."), nl],
        Spec = simplest_no_context_spec($pred, severity_warning,
            phase_read_files, Pieces),
        !:Specs = [Spec | !.Specs]
    ).
    % NOTE: We do NOT update !Error, since a missing optimization
    % interface file is not necessarily an error.

    % update_opt_error_status_on_success(ModuleErrors, !Specs, !Error):
    %
    % Work out whether any errors have occurred while reading
    % a .opt or .trans_opt file, and update !Error accordingly.
    % Note that we can ignore *not finding* a .opt or .trans_opt file,
    % a situation handled by update_opt_error_status_on_failure,
    % finding any error, whether syntax or semantic, inside one of these files
    % that does exist is always fatal. This is because it indicates that
    % - either the Mercury compiler invocation that created it had a bug,
    % - or that the file has been tampered with later.
    %
:- pred update_opt_error_status_on_success(read_module_errors::in,
    list(error_spec)::in, list(error_spec)::out,
    maybe_opt_file_error::in, maybe_opt_file_error::out) is det.

update_opt_error_status_on_success(ModuleErrors, !Specs, !Error) :-
    FatalErrors = ModuleErrors ^ rm_fatal_errors,
    NonFatalErrors0 = ModuleErrors ^ rm_nonfatal_errors,
    set.delete(rme_nec, NonFatalErrors0, NonFatalErrors),
    ( if
        set.is_empty(FatalErrors),
        set.is_empty(NonFatalErrors)
    then
        % ModuleSpecs contains no errors we have traditionally recorded
        % as a separately classified error. It could contain
        %
        % - errors we have generated error messages for but for which
        %   we have not recorded an error category (these are the errors
        %   whose category we now record as Not Elsewhere Classified,
        %   or rme_nec), and
        %
        % - warnings.
        %
        % Not adding either of those to !Specs preserves old behavior.
        true
    else
        !:Specs = get_read_module_specs(ModuleErrors) ++ !.Specs,
        !:Error = opt_file_error
    ).

%---------------------------------------------------------------------------%

:- pred aug_compilation_unit_add_ancestor_int_spec(ancestor_int_spec::in,
    aug_compilation_unit::in, aug_compilation_unit::out) is det.
:- pred aug_compilation_unit_add_direct_int1_spec(direct_int1_spec::in,
    aug_compilation_unit::in, aug_compilation_unit::out) is det.
:- pred aug_compilation_unit_add_indirect_int2_spec(indirect_int2_spec::in,
    aug_compilation_unit::in, aug_compilation_unit::out) is det.
:- pred aug_compilation_unit_add_plain_opt(parse_tree_plain_opt::in,
    aug_compilation_unit::in, aug_compilation_unit::out) is det.
:- pred aug_compilation_unit_add_trans_opt(parse_tree_trans_opt::in,
    aug_compilation_unit::in, aug_compilation_unit::out) is det.
:- pred aug_compilation_unit_add_int_for_opt_spec(int_for_opt_spec::in,
    aug_compilation_unit::in, aug_compilation_unit::out) is det.
:- pred aug_compilation_unit_add_type_repn_spec(type_repn_spec::in,
    aug_compilation_unit::in, aug_compilation_unit::out) is det.
:- pred aug_compilation_unit_maybe_add_module_version_numbers(
    module_name::in, maybe_version_numbers::in,
    aug_compilation_unit::in, aug_compilation_unit::out) is det.

%---------------------%

aug_compilation_unit_add_ancestor_int_spec(X, !AugCompUnit) :-
    Map0 = !.AugCompUnit ^ acu_ancestor_int_specs,
    X = ancestor_int0(PT0, _),
    MN = PT0 ^ pti0_module_name,
    map.det_insert(MN, X, Map0, Map),
    !AugCompUnit ^ acu_ancestor_int_specs := Map.

aug_compilation_unit_add_direct_int1_spec(X, !AugCompUnit) :-
    Map0 = !.AugCompUnit ^ acu_direct_int1_specs,
    X = direct_int1(PT1, _), MN = PT1 ^ pti1_module_name,
    map.det_insert(MN, X, Map0, Map),
    !AugCompUnit ^ acu_direct_int1_specs := Map.

aug_compilation_unit_add_indirect_int2_spec(X, !AugCompUnit) :-
    Map0 = !.AugCompUnit ^ acu_indirect_int2_specs,
    X = indirect_int2(PT2, _), MN = PT2 ^ pti2_module_name,
    map.det_insert(MN, X, Map0, Map),
    !AugCompUnit ^ acu_indirect_int2_specs := Map.

aug_compilation_unit_add_plain_opt(X, !AugCompUnit) :-
    Map0 = !.AugCompUnit ^ acu_plain_opts,
    MN = X ^ ptpo_module_name,
    map.det_insert(MN, X, Map0, Map),
    !AugCompUnit ^ acu_plain_opts := Map.

aug_compilation_unit_add_trans_opt(X, !AugCompUnit) :-
    Map0 = !.AugCompUnit ^ acu_trans_opts,
    MN = X ^ ptto_module_name,
    map.det_insert(MN, X, Map0, Map),
    !AugCompUnit ^ acu_trans_opts := Map.

aug_compilation_unit_add_int_for_opt_spec(X, !AugCompUnit) :-
    Map0 = !.AugCompUnit ^ acu_int_for_opt_specs,
    ( X = for_opt_int0(PT0, _), MN = PT0 ^ pti0_module_name
    ; X = for_opt_int1(PT1, _), MN = PT1 ^ pti1_module_name
    ; X = for_opt_int2(PT2, _), MN = PT2 ^ pti2_module_name
    ),
    map.det_insert(MN, X, Map0, Map),
    !AugCompUnit ^ acu_int_for_opt_specs := Map.

aug_compilation_unit_add_type_repn_spec(X, !AugCompUnit) :-
    Map0 = !.AugCompUnit ^ acu_type_repn_specs,
    X = type_repn_spec_int1(PT1), MN = PT1 ^ pti1_module_name,
    map.det_insert(MN, X, Map0, Map),
    !AugCompUnit ^ acu_type_repn_specs := Map.

aug_compilation_unit_maybe_add_module_version_numbers(ModuleName,
        MaybeVersionNumbers, !AugCompUnit) :-
    (
        MaybeVersionNumbers = no_version_numbers
    ;
        MaybeVersionNumbers = version_numbers(VersionNumbers),
        ModuleVersionNumbersMap0 =
            !.AugCompUnit ^ acu_module_item_version_numbers_map,
        map.det_insert(ModuleName, VersionNumbers,
            ModuleVersionNumbersMap0, ModuleVersionNumbersMap),
        !AugCompUnit ^ acu_module_item_version_numbers_map :=
            ModuleVersionNumbersMap
    ).

%---------------------------------------------------------------------------%

:- pred init_aug_make_int_unit(parse_tree_module_src::in,
    aug_make_int_unit::out) is det.

init_aug_make_int_unit(ParseTreeModuleSrc, AugMakeIntUnit) :-
    map.init(AncestorIntSpecs),
    map.init(DirectIntSpecs),
    map.init(IndirectIntSpecs),
    map.init(VersionNumbers),
    AugMakeIntUnit = aug_make_int_unit(ParseTreeModuleSrc,
        AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs, VersionNumbers).

%---------------------%

:- pred aug_make_int_unit_add_ancestor_int(parse_tree_int0::in,
    aug_make_int_unit::in, aug_make_int_unit::out) is det.
:- pred aug_make_int_unit_add_direct_int3_spec(direct_int3_spec::in,
    aug_make_int_unit::in, aug_make_int_unit::out) is det.
:- pred aug_make_int_unit_add_indirect_int3_spec(indirect_int3_spec::in,
    aug_make_int_unit::in, aug_make_int_unit::out) is det.
:- pred aug_make_int_unit_maybe_add_module_version_numbers(
    module_name::in, maybe_version_numbers::in,
    aug_make_int_unit::in, aug_make_int_unit::out) is det.

%---------------------%

aug_make_int_unit_add_ancestor_int(PT0, !AugMakeIntUnit) :-
    Map0 = !.AugMakeIntUnit ^ amiu_ancestor_int_specs,
    MN = PT0 ^ pti0_module_name,
    map.det_insert(MN, PT0, Map0, Map),
    !AugMakeIntUnit ^ amiu_ancestor_int_specs := Map.

aug_make_int_unit_add_direct_int3_spec(X, !AugMakeIntUnit) :-
    Map0 = !.AugMakeIntUnit ^ amiu_direct_int3_specs,
    X = direct_int3(PT3, _), MN = PT3 ^ pti3_module_name,
    map.det_insert(MN, X, Map0, Map),
    !AugMakeIntUnit ^ amiu_direct_int3_specs := Map.

aug_make_int_unit_add_indirect_int3_spec(X, !AugMakeIntUnit) :-
    Map0 = !.AugMakeIntUnit ^ amiu_indirect_int3_specs,
    X = indirect_int3(PT3, _), MN = PT3 ^ pti3_module_name,
    map.det_insert(MN, X, Map0, Map),
    !AugMakeIntUnit ^ amiu_indirect_int3_specs := Map.

aug_make_int_unit_maybe_add_module_version_numbers(ModuleName,
        MaybeVersionNumbers, !AugMakeIntUnit) :-
    (
        MaybeVersionNumbers = no_version_numbers
    ;
        MaybeVersionNumbers = version_numbers(VersionNumbers),
        ModuleVersionNumbersMap0 =
            !.AugMakeIntUnit ^ amiu_module_item_version_numbers_map,
        map.det_insert(ModuleName, VersionNumbers,
            ModuleVersionNumbersMap0, ModuleVersionNumbersMap),
        !AugMakeIntUnit ^ amiu_module_item_version_numbers_map :=
            ModuleVersionNumbersMap
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.grab_modules.
%---------------------------------------------------------------------------%
