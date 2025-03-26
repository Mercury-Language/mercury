%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: recompilation.check.m.
% Main author: stayl.
%
% Check whether a source file should be recompiled.
%
% In the rare case where a source file contains not just a top module
% but also some nested submodules, find out which top and/or sub module
% should be recompiled.
%
%---------------------------------------------------------------------------%

:- module recompilation.check.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.read_modules.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- type file_components_to_recompile
    --->    all_file_components
    ;       some_file_components(list(module_name)).

    % what_file_components_should_we_recompile(ProgressStream, Globals,
    %   ModuleName, FileComponentsToRecompile, !HaveParseTreeMaps, !IO):
    %
    % Process the `.used'  files for the given module (which is named as an
    % argument of the current compiler invocation) and all its inline
    % submodules (if any) to find out which parts of that source file
    % need to be recompiled.
    %
    % NOTE ModuleName should be the top module in its source file, but
    % our caller does nothing to check this.
    %
    % `ReadModules' is the list of interface files read during
    % recompilation checking, returned to avoid rereading them
    % if recompilation is required.
    %
:- pred what_file_components_should_we_recompile(io.text_output_stream::in,
    globals::in, module_name::in, file_components_to_recompile::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_cons.    % for type field_access_type
:- import_module hlds.hlds_pred.    % for field_access_function_name, pred_id.
:- import_module libs.file_util.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.item_util.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_parse_tree.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.write_error_spec.
:- import_module recompilation.item_types.
:- import_module recompilation.record_uses.
:- import_module recompilation.used_file.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.file.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

what_file_components_should_we_recompile(ProgressStream, Globals, ModuleName,
        WhatToRecompile, HaveParseTreeMaps0, HaveParseTreeMaps, !IO) :-
    globals.lookup_bool_option(Globals, find_all_recompilation_reasons,
        FindAllReasons),
    ResolvedUsedItems0 = init_resolved_used_items,
    % Start by assuming that we need to recompile *nothing*,
    % and mark things for recompilation *only* when we discover
    % the need for such recompilation.
    Info0 = recompilation_check_info(FindAllReasons, HaveParseTreeMaps0,
        ResolvedUsedItems0, set.init, some_file_components([]), []),
    get_used_file_should_we_recompile_module(ProgressStream, Globals,
        compiler_arg_module, ModuleName, ReadUsedFileResult,
        Info0, Info1, !IO),
    (
        ReadUsedFileResult = used_file_ok(UsedFile),
        WhatToRecompile1 = Info1 ^ rci_what_to_recompile,
        (
            WhatToRecompile1 = all_file_components,
            % There is no point in checking whether we should recompile
            % all submodules of ModuleName; we have already decided that
            % we have to.
            Info = Info1
        ;
            WhatToRecompile1 = some_file_components(_),
            % So far, we have made a decision on whether to recompile
            % ModuleName; now make the same decision for its nested submodules,
            % if any.
            MaybeTopModule = UsedFile ^ uf_maybe_top_module,
            (
                MaybeTopModule = not_top_module_used_file,
                Info = Info1
            ;
                MaybeTopModule = top_module_used_file(NestedSubModuleNames),
                % Note that NestedSubModuleNames will *usually* be empty.
                list.map_foldl2(
                    get_used_file_should_we_recompile_module(ProgressStream,
                        Globals, submodule_of_compiler_arg_module),
                    NestedSubModuleNames, _, Info1, Info, !IO)
            )
        )
    ;
        ReadUsedFileResult = used_file_error(_),
        Info = Info1
    ),
    WhatToRecompile = Info ^ rci_what_to_recompile,
    HaveParseTreeMaps = Info ^ rci_have_parse_tree_maps.

:- type maybe_compiler_arg_module
    --->    compiler_arg_module
    ;       submodule_of_compiler_arg_module.

:- pred get_used_file_should_we_recompile_module(io.text_output_stream::in,
    globals::in, maybe_compiler_arg_module::in, module_name::in,
    used_file_result(used_file)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

get_used_file_should_we_recompile_module(ProgressStream, Globals,
        MaybeCompilerArg, ModuleName, ReadUsedFileResult, !Info, !IO) :-
    read_used_file_for_module(Globals, ModuleName, ReadUsedFileResult, !IO),
    (
        ReadUsedFileResult = used_file_ok(UsedFile),
        should_we_recompile_module(ProgressStream, Globals, ModuleName,
            UsedFile, MaybeCompilerArg, MaybeStoppingReason, !Info, !IO),
        record_and_maybe_report_recompilation_why_or_why_not(ProgressStream,
            Globals, ModuleName, MaybeStoppingReason, !Info, !IO)
    ;
        ReadUsedFileResult = used_file_error(UsedFileError),
        maybe_write_recompilation_message(ProgressStream, Globals,
            write_used_file_error(Globals, ModuleName, UsedFileError),
            !IO),
        !Info ^ rci_what_to_recompile := all_file_components
    ).

:- pred should_we_recompile_module(io.text_output_stream::in, globals::in,
    module_name::in, used_file::in, maybe_compiler_arg_module::in,
    maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

should_we_recompile_module(ProgressStream, Globals, ModuleName, UsedFile,
        MaybeCompilerArg, MaybeStoppingReason, !Info, !IO) :-
    % XXX The following warning may, or may not, have been obsoleted
    % by the commit that redesigned this module's operation away from
    % relying wholly on exceptions.
    % WARNING: any exceptions thrown before the sub_modules field is set
    % in the recompilation_check_info must set the modules_to_recompile field
    % to `all', or else the nested submodules will not be checked
    % and necessary recompilations may be missed.
    UsedFile = used_file(ModuleTimestamp, _MaybeTopModule,
        UsedItems, UsedClasses, UsedModules),
    ModuleTimestamp = module_timestamp(_, RecordedTimestamp, _),
    !Info ^ rci_used_items := UsedItems,
    !Info ^ rci_used_typeclasses := set.list_to_set(UsedClasses),
    (
        MaybeCompilerArg = submodule_of_compiler_arg_module,
        % For inline submodules, we don't need to check the module timestamp,
        % because we have already checked the timestamp for the parent module.
        MaybeStoppingReason0 = no
    ;
        MaybeCompilerArg = compiler_arg_module,
        % XXX Why aren't we searching !.Info ^ rci_have_parse_tree_maps?
        read_module_src(ProgressStream, Globals, rrm_std,
            do_search, ModuleName, [],
            do_not_read_module_if_match(RecordedTimestamp), HaveReadSrc, !IO),
        (
            HaveReadSrc = have_not_read_module(FileName, Errors),
            % If we did not read the source file because its timestamp
            % matched RecordedTimestamp, then there will be no errors.
            ( if there_are_some_errors(Errors) then
                MaybeStoppingReason0 =
                    yes(read_module_error_stopping_reason(FileName, Errors))
            else
                MaybeStoppingReason0 = no
            )
        ;
            HaveReadSrc = have_module(FileName, ParseTreeSrc, Source),
            Source = was_read(MaybeNewTimestamp, Errors),
            ( if
                MaybeNewTimestamp = yes(NewTimestamp),
                NewTimestamp \= RecordedTimestamp
            then
                % If the source file has changed, recompile all its components.
                record_read_file_src(ModuleName, FileName,
                    ModuleTimestamp ^ mts_timestamp := NewTimestamp,
                    ParseTreeSrc, Errors, !Info),
                !Info ^ rci_what_to_recompile := all_file_components,
                ChangedReason = recompile_for_file_changed(FileName),
                record_recompilation_reason(ChangedReason,
                    MaybeStoppingReason0, !Info)
            else if
                ( there_are_some_errors(Errors)
                ; MaybeNewTimestamp = no
                )
            then
                MaybeStoppingReason0 =
                    yes(read_module_error_stopping_reason(FileName, Errors))
            else
                % We are throwing away ModuleErrors. Since it should be
                % a repeat of the errors we saw when the file was first
                % read in, this should be OK.
                MaybeStoppingReason0 = no
            )
        )
    ),
    (
        MaybeStoppingReason0 = yes(_),
        MaybeStoppingReason = MaybeStoppingReason0
    ;
        MaybeStoppingReason0 = no,
        % Check whether the output files are present and up-to-date.
        module_name_to_target_file_name_create_dirs(Globals,
            ModuleName, TargetFile, !IO),
        require_recompilation_if_not_up_to_date(RecordedTimestamp, TargetFile,
            MaybeStoppingReason0, MaybeStoppingReason1, !Info, !IO),
        check_imported_modules(ProgressStream, Globals, UsedModules,
            MaybeStoppingReason1, MaybeStoppingReason, !Info, !IO)
    ).

:- pred record_and_maybe_report_recompilation_why_or_why_not(
    io.text_output_stream::in, globals::in, module_name::in,
    maybe(recompile_reason)::in,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

record_and_maybe_report_recompilation_why_or_why_not(ProgressStream, Globals,
        ModuleName, MaybeStoppingReason, !Info, !IO) :-
    (
        MaybeStoppingReason = no,
        Reasons = !.Info ^ rci_recompilation_reasons
    ;
        MaybeStoppingReason = yes(StoppingReason),
        % Ignoring the old contents of the rci_recompilation_reasons field
        % in favor of the stopping reason preserves old behavior.
        % (We used to throw an exception containing StoppingReason,
        % and we used to ignore the contents of rci_collect_all_reasons
        % when catching that exception.)
        Reasons = [StoppingReason]
    ),
    (
        Reasons = [],
        module_name_to_target_timestamp_file_name_create_dirs(Globals,
            ModuleName, TimestampFile, !IO),
        maybe_write_recompilation_message(ProgressStream, Globals,
            write_not_recompiling_message(ModuleName), !IO),
        % Record *in the filesystem* that this module has been found to be
        % up to date as of this time.
        touch_file_datestamp(Globals, ProgressStream, TimestampFile,
            _Succeeded, !IO)
    ;
        Reasons = [_ | _],
        add_module_to_recompile(ModuleName, !Info),
        maybe_write_recompilation_message(ProgressStream, Globals,
            write_reasons_message(Globals, ModuleName,
                list.reverse(Reasons)),
            !IO)
    ).

:- func read_module_error_stopping_reason(file_name, read_module_errors)
    = recompile_reason.

read_module_error_stopping_reason(FileName, _Errors) = FileReason :-
    % We are throwing away the error_specs in _Errors, even though they
    % could illuminate the cause of the problem. XXX Why is this OK?
    Pieces = [words("error reading file"), quote(FileName), suffix("."), nl],
    % XXX Some of the errors in Errors could be errors other than
    % syntax errors.
    FileReason = recompile_for_file_error(FileName, Pieces).

:- pred require_recompilation_if_not_up_to_date(timestamp::in, file_name::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

require_recompilation_if_not_up_to_date(RecordedTimestamp, TargetFile,
        !MaybeStoppingReason, !Info, !IO) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        io.file.file_modification_time(TargetFile, TargetModTimeResult, !IO),
        ( if
            TargetModTimeResult = ok(TargetModTime),
            compare(TargetModTimeCompare, time_t_to_timestamp(TargetModTime),
                RecordedTimestamp),
            TargetModTimeCompare = (>)
        then
            !:MaybeStoppingReason = no
        else
            Reason = recompile_for_output_file_not_up_to_date(TargetFile),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred check_imported_modules(io.text_output_stream::in, globals::in,
    list(recomp_used_module)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

check_imported_modules(_, _, [], !MaybeStoppingReason, !Info, !IO).
check_imported_modules(ProgressStream, Globals,
        [HeadUsedModule | TailUsedModules], !MaybeStoppingReason,
        !Info, !IO) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        check_imported_module(ProgressStream, Globals, HeadUsedModule,
            !:MaybeStoppingReason, !Info, !IO),
        check_imported_modules(ProgressStream, Globals, TailUsedModules,
            !MaybeStoppingReason, !Info, !IO)
    ).

    % Check whether the interface file read for a module in the last
    % compilation has changed, and if so whether the items have changed
    % in a way which should cause a recompilation.
    %
:- pred check_imported_module(io.text_output_stream::in, globals::in,
    recomp_used_module::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det.

check_imported_module(ProgressStream, Globals, UsedModule, MaybeStoppingReason,
        !Info, !IO) :-
    UsedModule = recomp_used_module(ImportedModuleName, ModuleTimestamp,
        MaybeUsedVersionNumbers),
    ModuleTimestamp =
        module_timestamp(FileKind, _RecordedTimestamp, _RecompAvail),
    % XXX We should look into using subtypes to avoid the need
    % for this switch.
    (
        FileKind = fk_int(IntFileKind)
    ;
        FileKind = fk_src,
        unexpected($pred, "fk_src")
    ;
        FileKind = fk_opt(_),
        unexpected($pred, "fk_opt")
    ),
    HaveParseTreeMaps = !.Info ^ rci_have_parse_tree_maps,
    % NOTE The calls to check_imported_module_intN are all identical
    % except for which field of HaveParseTreeMaps we select, *but*
    % the identical parts cannot be factored out. This is because the
    % different fields of HaveParseTreeMaps have different types,
    % and therefore the four calls below set the PT type variable in the
    % signature of check_imported_module_intN to four different values.
    (
        IntFileKind = ifk_int0,
        check_imported_module_intN(ProgressStream, Globals, ImportedModuleName,
            ModuleTimestamp, MaybeUsedVersionNumbers,
            HaveParseTreeMaps ^ hptm_int0, MaybeStoppingReason, !Info, !IO)
    ;
        IntFileKind = ifk_int1,
        check_imported_module_intN(ProgressStream, Globals, ImportedModuleName,
            ModuleTimestamp, MaybeUsedVersionNumbers,
            HaveParseTreeMaps ^ hptm_int1, MaybeStoppingReason, !Info, !IO)
    ;
        IntFileKind = ifk_int2,
        check_imported_module_intN(ProgressStream, Globals, ImportedModuleName,
            ModuleTimestamp, MaybeUsedVersionNumbers,
            HaveParseTreeMaps ^ hptm_int2, MaybeStoppingReason, !Info, !IO)
    ;
        IntFileKind = ifk_int3,
        check_imported_module_intN(ProgressStream, Globals, ImportedModuleName,
            ModuleTimestamp, MaybeUsedVersionNumbers,
            HaveParseTreeMaps ^ hptm_int3, MaybeStoppingReason, !Info, !IO)
    ).

:- pred check_imported_module_intN(io.text_output_stream::in, globals::in,
    module_name::in, module_timestamp::in,
    maybe(module_item_version_numbers)::in,
    have_parse_tree_map(PT)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out,
    io::di, io::uo) is det <= check_imported_module_int_file(PT).

check_imported_module_intN(ProgressStream, Globals, ImportedModuleName,
        ModuleTimestamp, MaybeUsedVersionNumbers, HPTM, MaybeStoppingReason,
        !Info, !IO) :-
    ModuleTimestamp =
        module_timestamp(_FileKind, RecordedTimestamp, _RecompAvail),
    ( if
        cim_search_mapN(HPTM, ImportedModuleName, HaveReadModuleIntNPrime)
    then
        Recorded = bool.yes,
        HaveReadModuleIntN = HaveReadModuleIntNPrime
    else
        Recorded = bool.no,
        cim_read_module_intN(ProgressStream, Globals, rrm_std, do_search,
            ImportedModuleName, do_not_read_module_if_match(RecordedTimestamp),
            HaveReadModuleIntN, !IO)
    ),
    (
        HaveReadModuleIntN = have_not_read_module(FileName, Errors),
        % If we did not read the interface file because its timestamp
        % matched RecordedTimestamp, then there will be no errors.
        ( if there_are_some_errors(Errors) then
            MaybeStoppingReason =
                yes(read_module_error_stopping_reason(FileName, Errors))
        else
            MaybeStoppingReason = no
        )
    ;
        HaveReadModuleIntN = have_module(FileName, ParseTreeIntN, Source),
        have_parse_tree_source_get_maybe_timestamp_errors(Source,
            MaybeNewTimestamp, Errors),
        ( if there_are_some_errors(Errors) then
            % We are throwing away Specs, even though some of its elements
            % could illuminate the cause of the problem. XXX Is this OK?
            MaybeStoppingReason =
                yes(read_module_error_stopping_reason(FileName, Errors))
        else if
            MaybeNewTimestamp = yes(NewTimestamp),
            NewTimestamp \= RecordedTimestamp
        then
            (
                Recorded = no,
                cim_record_read_file_intN(ImportedModuleName, FileName,
                    ModuleTimestamp ^ mts_timestamp := NewTimestamp,
                    ParseTreeIntN, Errors, !Info)
            ;
                Recorded = yes
            ),
            ( if
                MaybeUsedVersionNumbers = yes(UsedVersionNumbers),
                cim_get_version_numbersN(ParseTreeIntN, VersionNumbers)
            then
                cim_get_ambiguity_checkablesN(ParseTreeIntN, Checkables),
                check_module_used_items(ImportedModuleName,
                    ModuleTimestamp, UsedVersionNumbers, VersionNumbers,
                    Checkables, MaybeStoppingReason, !Info)
            else
                Reason = recompile_for_file_changed(FileName),
                record_recompilation_reason(Reason, MaybeStoppingReason,
                    !Info)
            )
        else
            % We are throwing away the error_specs in Errors. Since it
            % should be a repeat of the errors we saw when the file
            % was first read in, this should be OK.
            MaybeStoppingReason = no
        )
    ).

%---------------------------------------------------------------------------%

:- pred check_module_used_items(module_name::in, module_timestamp::in,
    module_item_version_numbers::in, module_item_version_numbers::in,
    ambiguity_checkables::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_module_used_items(ModuleName, OldModuleTimestamp,
        UsedVersionNumbers, NewVersionNumbers, ParseTreeCheckables,
        !:MaybeStoppingReason, !Info) :-
    OldModuleTimestamp = module_timestamp(_FK, OldTimestamp, RecompAvail),
    UsedVersionNumbers =
        module_item_version_numbers(UsedTypeNameMap, UsedTypeDefnMap,
            UsedInstMap, UsedModeMap, UsedClassMap, UsedInstanceMap,
            UsedPredMap, UsedFuncMap),
    NewVersionNumbers =
        module_item_version_numbers(NewTypeNameMap, NewTypeDefnMap,
            NewInstMap, NewModeMap, NewClassMap, NewInstanceMap,
            NewPredMap, NewFuncMap),

    !:MaybeStoppingReason = no,
    % Check whether any of the items which were used have changed.
    check_name_arity_version_numbers(ModuleName, recomp_type_name,
        UsedTypeNameMap, NewTypeNameMap, !MaybeStoppingReason, !Info),
    check_name_arity_version_numbers(ModuleName, recomp_type_defn,
        UsedTypeDefnMap, NewTypeDefnMap, !MaybeStoppingReason, !Info),
    check_name_arity_version_numbers(ModuleName, recomp_inst,
        UsedInstMap, NewInstMap, !MaybeStoppingReason, !Info),
    check_name_arity_version_numbers(ModuleName, recomp_mode,
        UsedModeMap, NewModeMap, !MaybeStoppingReason, !Info),
    check_name_arity_version_numbers(ModuleName, recomp_typeclass,
        UsedClassMap, NewClassMap, !MaybeStoppingReason, !Info),
    check_item_name_version_numbers(ModuleName,
        UsedInstanceMap, NewInstanceMap, !MaybeStoppingReason, !Info),
    check_name_arity_version_numbers(ModuleName, recomp_predicate,
        UsedPredMap, NewPredMap, !MaybeStoppingReason, !Info),
    check_name_arity_version_numbers(ModuleName, recomp_function,
        UsedFuncMap, NewFuncMap, !MaybeStoppingReason, !Info),

    % Check whether added or modified items could cause name resolution
    % ambiguities with items which were used.
    ParseTreeCheckables = ambiguity_checkables(ItemTypeDefns,
        ItemInstDefns, ItemModeDefns, ItemTypeClasses, ItemPredDecls),
    check_items_for_ambiguities(
        check_type_defn_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedVersionNumbers),
        ItemTypeDefns, !MaybeStoppingReason, !Info),
    check_items_for_ambiguities(
        check_inst_defn_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedVersionNumbers),
        ItemInstDefns, !MaybeStoppingReason, !Info),
    check_items_for_ambiguities(
        check_mode_defn_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedVersionNumbers),
        ItemModeDefns, !MaybeStoppingReason, !Info),
    check_items_for_ambiguities(
        check_typeclass_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedVersionNumbers),
        ItemTypeClasses, !MaybeStoppingReason, !Info),
    check_items_for_ambiguities(
        check_pred_decl_info_for_ambiguities(RecompAvail, OldTimestamp,
            UsedVersionNumbers),
        ItemPredDecls, !MaybeStoppingReason, !Info),

    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        % Check for new instances for used typeclasses.
        ModuleInstances = map.keys_as_set(NewInstanceMap),
        UsedInstances = map.keys_as_set(UsedInstanceMap),

        UsedClasses = !.Info ^ rci_used_typeclasses,
        set.difference(set.intersect(UsedClasses, ModuleInstances),
            UsedInstances, AddedInstances),
        AddedInstancesList = set.to_sorted_list(AddedInstances),
        (
            AddedInstancesList = [],
            !:MaybeStoppingReason = no
        ;
            AddedInstancesList = [FirstAddedInstance | _],
            Reason1 = recompile_for_changed_or_added_instance(ModuleName,
                FirstAddedInstance),
            record_recompilation_reason(Reason1, !:MaybeStoppingReason, !Info)
        )
    ).

:- func make_item_id(module_name, recomp_item_type, name_arity)
    = recomp_item_id.

make_item_id(Module, ItemType, name_arity(Name, Arity)) =
    recomp_item_id(ItemType, recomp_item_name(qualified(Module, Name), Arity)).

%---------------------------------------------------------------------------%

:- pred check_name_arity_version_numbers(module_name::in, recomp_item_type::in,
    name_arity_version_map::in, name_arity_version_map::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_name_arity_version_numbers(ModuleName, ItemType,
        UsedVersionMap, NewVersionMap, !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        map.foldl2(
            check_name_arity_version_number(ModuleName, ItemType,
                NewVersionMap),
            UsedVersionMap, !MaybeStoppingReason, !Info)
    ).

:- pred check_name_arity_version_number(module_name::in, recomp_item_type::in,
    name_arity_version_map::in, name_arity::in, version_number::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_name_arity_version_number(ModuleName, ItemType, NewVersionMap,
        NameArity, UsedVersionNumber, !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        ( if map.search(NewVersionMap, NameArity, NewVersionNumber) then
            ( if NewVersionNumber = UsedVersionNumber then
                true
            else
                ItemId = make_item_id(ModuleName, ItemType, NameArity),
                Reason = recompile_for_changed_item(ItemId),
                record_recompilation_reason(Reason, !:MaybeStoppingReason,
                    !Info)
            )
        else
            ItemId = make_item_id(ModuleName, ItemType, NameArity),
            Reason = recompile_for_removed_item(ItemId),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        )
    ).

:- pred check_item_name_version_numbers(module_name::in,
    recomp_item_name_version_map::in, recomp_item_name_version_map::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_item_name_version_numbers(ModuleName, UsedVersionMap, NewVersionMap,
        !MaybeStoppingReason, !Info) :-
    map.foldl2(
        check_item_name_version_number(ModuleName, NewVersionMap),
        UsedVersionMap, !MaybeStoppingReason, !Info).

:- pred check_item_name_version_number(module_name::in,
    recomp_item_name_version_map::in, recomp_item_name::in, version_number::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_item_name_version_number(ModuleName, NewVersionMap,
        ItemName, UsedVersionNumber, !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        ( if map.search(NewVersionMap, ItemName, NewVersionNumber) then
            ( if UsedVersionNumber = NewVersionNumber then
                true
            else
                % XXX RECOMP In the same circumstance, the name_arity version
                % of this predicate above returns recompile_for_changed_item,
                % with no "_or_added".
                Reason = recompile_for_changed_or_added_instance(ModuleName,
                    ItemName),
                record_recompilation_reason(Reason, !:MaybeStoppingReason,
                    !Info)
            )
        else
            Reason = recompile_for_removed_instance(ModuleName, ItemName),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        )
    ).

%---------------------------------------------------------------------------%

:- pred check_items_for_ambiguities(
    pred(T, maybe(recompile_reason), maybe(recompile_reason),
        recompilation_check_info, recompilation_check_info)
    ::in(pred(in, in, out, in, out) is det),
    list(T)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_items_for_ambiguities(_CheckPred, [], !MaybeStoppingReason, !Info).
check_items_for_ambiguities(CheckPred, [HeadItem | TailItems],
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        CheckPred(HeadItem, no, !:MaybeStoppingReason, !Info),
        check_items_for_ambiguities(CheckPred, TailItems,
            !MaybeStoppingReason, !Info)
    ).

%---------------------%

:- pred check_type_defn_info_for_ambiguities(recomp_avail::in, timestamp::in,
    module_item_version_numbers::in, item_type_defn_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_type_defn_info_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        ItemTypeDefn, !MaybeStoppingReason, !Info) :-
    ItemTypeDefn = item_type_defn_info(TypeSymName, TypeParams, TypeBody,
        _, _, _),
    list.length(TypeParams, TypeArity),
    check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
        VersionNumbers ^ mivn_type_names, recomp_type_name,
        TypeSymName, TypeArity, NeedsCheck, !MaybeStoppingReason, !Info),
    (
        NeedsCheck = yes,
        TypeCtor = type_ctor(TypeSymName, TypeArity),
        check_type_defn_ambiguity_with_functor(RecompAvail,
            TypeCtor, TypeBody, !MaybeStoppingReason, !Info)
    ;
        NeedsCheck = no
    ).

%---------------------%

:- pred check_inst_defn_info_for_ambiguities(recomp_avail::in, timestamp::in,
    module_item_version_numbers::in, item_inst_defn_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_inst_defn_info_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        ItemInstDefn, !MaybeStoppingReason, !Info) :-
    % XXX IFTC Do we need to check _MaybeForTypeCtor?
    ItemInstDefn = item_inst_defn_info(InstSymName, InstParams,
        _MaybeForTypeCtor, _, _, _, _),
    list.length(InstParams, InstArity),
    check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
        VersionNumbers ^ mivn_insts, recomp_inst, InstSymName, InstArity,
        _NeedsCheck, !MaybeStoppingReason, !Info).

%---------------------%

:- pred check_mode_defn_info_for_ambiguities(recomp_avail::in, timestamp::in,
    module_item_version_numbers::in, item_mode_defn_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_mode_defn_info_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        ItemModeDefn, !MaybeStoppingReason, !Info) :-
    ItemModeDefn = item_mode_defn_info(ModeSymName, ModeParams, _, _, _, _),
    list.length(ModeParams, ModeArity),
    check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
        VersionNumbers ^ mivn_modes, recomp_mode, ModeSymName, ModeArity,
        _NeedsCheck, !MaybeStoppingReason, !Info).

%---------------------%

:- pred check_typeclass_info_for_ambiguities(recomp_avail::in,
    timestamp::in, module_item_version_numbers::in, item_typeclass_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_typeclass_info_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        ItemTypeClass, !MaybeStoppingReason, !Info) :-
    ItemTypeClass = item_typeclass_info(TypeClassSymName, TypeClassParams,
        _, _, Interface, _, _, _),
    list.length(TypeClassParams, TypeClassArity),
    check_for_simple_item_ambiguity(RecompAvail, OldTimestamp,
        VersionNumbers ^ mivn_typeclasses, recomp_typeclass,
        TypeClassSymName, TypeClassArity,
        NeedsCheck, !MaybeStoppingReason, !Info),
    ( if
        NeedsCheck = yes,
        Interface = class_interface_concrete(ClassDecls)
    then
        list.foldl2(
            check_class_decl_for_ambiguities(RecompAvail, OldTimestamp,
                VersionNumbers),
            ClassDecls, !MaybeStoppingReason, !Info)
    else
        true
    ).

:- pred check_class_decl_for_ambiguities(recomp_avail::in,
    timestamp::in, module_item_version_numbers::in, class_decl::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_class_decl_for_ambiguities(RecompAvail, OldTimestamp, VersionNumbers,
        Decl, !MaybeStoppingReason, !Info) :-
    (
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        PredOrFuncInfo = class_pred_or_func_info(MethodName, PredOrFunc,
            MethodArgs, MethodWithType, _, _, _, _, _, _, _, _),
        check_for_pred_or_func_item_ambiguity(yes, RecompAvail,
            OldTimestamp, VersionNumbers, PredOrFunc, MethodName,
            MethodArgs, MethodWithType, !MaybeStoppingReason, !Info)
    ;
        Decl = class_decl_mode(_)
    ).

%---------------------%

:- pred check_pred_decl_info_for_ambiguities(recomp_avail::in,
    timestamp::in, module_item_version_numbers::in, item_pred_decl_info::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_pred_decl_info_for_ambiguities(RecompAvail, OldTimestamp,
        VersionNumbers, ItemPredDecl, !MaybeStoppingReason, !Info) :-
    ItemPredDecl = item_pred_decl_info(PredSymName, PredOrFunc, Args,
        WithType, _, _, _, _, _, _, _, _, _, _),
    check_for_pred_or_func_item_ambiguity(no, RecompAvail, OldTimestamp,
        VersionNumbers, PredOrFunc, PredSymName, Args, WithType,
        !MaybeStoppingReason, !Info).

%---------------------------------------------------------------------------%

:- pred check_for_simple_item_ambiguity(recomp_avail::in, timestamp::in,
    name_arity_version_map::in, recomp_item_type::in(recomp_simple),
    sym_name::in, arity::in, bool::out,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_simple_item_ambiguity(RecompAvail, UsedFileTimestamp,
        VersionMap, ItemType, SymName, Arity, NeedsCheck,
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_),
        % Since we have found a reason to recompile, we don't need to look
        % for more reasons.
        NeedsCheck = no
    ;
        !.MaybeStoppingReason = no,
        ( if
            item_is_new_or_changed(UsedFileTimestamp, VersionMap,
                SymName, Arity)
        then
            NeedsCheck = yes,
            UsedItems = !.Info ^ rci_used_items,
            (
                ItemType = recomp_type_name,
                UsedItemMap = UsedItems ^ rui_type_names
            ;
                ItemType = recomp_type_defn,
                unexpected($pred, "type_body_item")
            ;
                ItemType = recomp_inst,
                UsedItemMap = UsedItems ^ rui_insts
            ;
                ItemType = recomp_mode,
                UsedItemMap = UsedItems ^ rui_modes
            ;
                ItemType = recomp_typeclass,
                UsedItemMap = UsedItems ^ rui_typeclasses
            ),
            NameArity = name_arity(unqualify_name(SymName), Arity),
            ( if map.search(UsedItemMap, NameArity, MatchingQualifiers) then
                map.foldl2(
                    check_for_simple_item_ambiguity_2(ItemType,
                        RecompAvail, SymName, Arity),
                    MatchingQualifiers, !MaybeStoppingReason, !Info)
            else
                true
            )
        else
            NeedsCheck = no
        )
    ).

:- pred check_for_simple_item_ambiguity_2(recomp_item_type::in,
    recomp_avail::in, sym_name::in, arity::in,
    module_qualifier::in, module_name::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_simple_item_ambiguity_2(ItemType, RecompAvail, SymName, Arity,
        OldModuleQualifier, OldMatchingModuleName,
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        Name = unqualify_name(SymName),
        ( if
            % XXX RECOMP401 This logic is ancient, and may do the wrong thing
            % with most values of RecompAvail, since those values did not exist
            % when the original version of this code was written.
            ( RecompAvail = recomp_avail_int_use
            ; RecompAvail = recomp_avail_imp_use
            ),
            % XXX This is a bit conservative in the case of partially qualified
            % names but that hopefully won't come up too often.
            OldModuleQualifier = unqualified("")
        then
            true
        else if
            QualifiedName = module_qualify_name(OldModuleQualifier, Name),
            partial_sym_name_matches_full(QualifiedName, SymName),
            not SymName = qualified(OldMatchingModuleName, _)
        then
            OldMatchingSymName = qualified(OldMatchingModuleName, Name),
            ItemName = recomp_item_name(SymName, Arity),
            OldItemName = recomp_item_name(OldMatchingSymName, Arity),
            Reason = recompile_for_item_ambiguity(
                recomp_item_id(ItemType, ItemName),
                [recomp_item_id(ItemType, OldItemName)]),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        else
            true
        )
    ).

:- pred item_is_new_or_changed(timestamp::in, name_arity_version_map::in,
    sym_name::in, arity::in) is semidet.

item_is_new_or_changed(UsedFileTimestamp, UsedVersionMap, SymName, Arity) :-
    NameArity = name_arity(unqualify_name(SymName), Arity),
    ( if map.search(UsedVersionMap, NameArity, UsedVersionNumber) then
        % XXX This assumes that version numbers are timestamps.
        compare((>), UsedVersionNumber, UsedFileTimestamp)
    else
        true
    ).

:- pred check_for_pred_or_func_item_ambiguity(bool::in,
    recomp_avail::in, timestamp::in, module_item_version_numbers::in,
    pred_or_func::in, sym_name::in,
    types_and_maybe_modes::in, maybe(mer_type)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity(NeedsCheck, RecompAvail, OldTimestamp,
        VersionNumbers, PredOrFunc, SymName, ArgTypesAndMaybeModes, WithType,
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        PredFormArity = types_and_maybe_modes_arity(ArgTypesAndMaybeModes),
        PredFormArity = pred_form_arity(PredFormArityInt),
        (
            WithType = no,
            % XXX Given that we use pred_form_arity elsewhere
            % when we process resolved_functor_pred_or_func,
            % setting Arity here to the user_arity looks to be a bug.
            % (NOTE Comments elsewhere in the code seems to indicate
            % that there are supposed to be rules that govern when
            % we use pred_form arities and when we use user arities,
            % given that due to the presence of with_type/with_inst,
            % we cannot usefully use user arities everywhere. Unfortunately,
            % I (zs) don't know of any place where those rules, invented
            % by Simon ages ago, have been written down.)
            %
            % Unfortunately, ...
            adjust_func_arity(PredOrFunc, UserArityInt, PredFormArityInt)
        ;
            WithType = yes(_),
            % ... in the presence of with_type, we have no idea what even
            % the actual pred_form_arity is.
            UserArityInt = PredFormArityInt
        ),
        ( if
            (
                NeedsCheck = yes
            ;
                (
                    PredOrFunc = pf_predicate,
                    PredMap = VersionNumbers ^ mivn_predicates,
                    item_is_new_or_changed(OldTimestamp, PredMap,
                        SymName, UserArityInt)
                ;
                    PredOrFunc = pf_function,
                    FuncMap = VersionNumbers ^ mivn_functions,
                    item_is_new_or_changed(OldTimestamp, FuncMap,
                        SymName, UserArityInt)
                )
            )
        then
            UsedItems = !.Info ^ rci_used_items,
            (
                PredOrFunc = pf_predicate,
                ItemType = recomp_predicate,
                UsedItemMap = UsedItems ^ rui_predicates
            ;
                PredOrFunc = pf_function,
                ItemType = recomp_function,
                UsedItemMap = UsedItems ^ rui_functions
            ),
            Name = unqualify_name(SymName),
            ( if map.search(UsedItemMap, Name, MatchingArityList) then
                list.foldl2(
                    check_for_pred_or_func_item_ambiguity_1(WithType,
                        ItemType, RecompAvail, SymName, UserArityInt),
                    MatchingArityList, no, !:MaybeStoppingReason, !Info)
            else
                !:MaybeStoppingReason = no
            ),
            InvPredId = invalid_pred_id,
            (
                SymName = qualified(ModuleName, _),
                (
                    WithType = yes(_),
                    % We don't know the actual arity.
                    AritiesToMatch = match_arity_any
                ;
                    WithType = no,
                    AritiesToMatch =
                        match_arity_less_than_or_equal(UserArityInt)
                ),
                ResolvedFunctor = resolved_functor_pred_or_func(InvPredId,
                    PredOrFunc, ModuleName, pred_form_arity(PredFormArityInt)),
                check_functor_ambiguities_by_name(RecompAvail, SymName,
                    AritiesToMatch, ResolvedFunctor,
                    !MaybeStoppingReason, !Info)
            ;
                SymName = unqualified(_),
                unexpected($pred, "unqualified predicate name")
            )
        else
            !:MaybeStoppingReason = no
        )
    ).

:- pred check_for_pred_or_func_item_ambiguity_1(maybe(mer_type)::in,
    recomp_item_type::in, recomp_avail::in, sym_name::in, arity::in,
    pair(arity, map(sym_name, set(pair(pred_id, module_name))))::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity_1(WithType, ItemType, RecompAvail,
        SymName, Arity, MatchArity - MatchingQualifiers,
        !MaybeStoppingReason, !Info) :-
    ( if
        (
            WithType = yes(_),
            MatchArity >= Arity
        ;
            WithType = no,
            MatchArity = Arity
        )
    then
        map.foldl2(
            check_for_pred_or_func_item_ambiguity_2(ItemType, RecompAvail,
                SymName, MatchArity),
            MatchingQualifiers, !MaybeStoppingReason, !Info)
    else
        true
    ).

:- pred check_for_pred_or_func_item_ambiguity_2(recomp_item_type::in,
    recomp_avail::in, sym_name::in, arity::in, module_qualifier::in,
    set(pair(pred_id, module_name))::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_for_pred_or_func_item_ambiguity_2(ItemType, RecompAvail,
        SymName, Arity, OldModuleQualifier, OldMatchingModuleNames,
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        Name = unqualify_name(SymName),
        ( if
            % XXX RECOMP401 This logic is ancient, and may do the wrong thing
            % with most values of RecompAvail, since those values did not exist
            % when the original version of this was written.
            ( RecompAvail = recomp_avail_int_use
            ; RecompAvail = recomp_avail_imp_use
            ),
            % XXX This is a bit conservative in the case of partially qualified
            % names but that hopefully won't come up too often.
            OldModuleQualifier = unqualified("")
        then
            !:MaybeStoppingReason = no
        else if
            QualifiedName = module_qualify_name(OldModuleQualifier, Name),
            partial_sym_name_matches_full(QualifiedName, SymName),
            not (
                SymName = qualified(PredModuleName, _),
                set.member(_ - PredModuleName, OldMatchingModuleNames)
            )
        then
            AmbiguousDecls = list.map(
                ( func(_ - OldMatchingModule) = Item :-
                    OldMatchingName = qualified(OldMatchingModule, Name),
                    Item = recomp_item_id(ItemType,
                        recomp_item_name(OldMatchingName, Arity))
                ),
                set.to_sorted_list(OldMatchingModuleNames)),
            Reason = recompile_for_item_ambiguity(recomp_item_id(ItemType,
                recomp_item_name(SymName, Arity)), AmbiguousDecls),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        else
            !:MaybeStoppingReason = no
        )
    ).

    % Go over the constructors for a type which has changed and check whether
    % any of them could create an ambiguity with functors used during the
    % last compilation.
    %
:- pred check_type_defn_ambiguity_with_functor(recomp_avail::in,
    type_ctor::in, type_defn::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_type_defn_ambiguity_with_functor(RecompAvail, TypeCtor, TypeDefn,
        !MaybeStoppingReason, !Info) :-
    (
        ( TypeDefn = parse_tree_abstract_type(_)
        ; TypeDefn = parse_tree_eqv_type(_)
        ; TypeDefn = parse_tree_foreign_type(_)
        ; TypeDefn = parse_tree_solver_type(_)
        )
    ;
        (
            TypeDefn = parse_tree_du_type(DetailsDu),
            DetailsDu = type_details_du(Ctors, _, _)
        ;
            TypeDefn = parse_tree_sub_type(DetailsSub),
            DetailsSub = type_details_sub(_, Ctors)
        ),
        list.foldl2(check_functor_ambiguities(RecompAvail, TypeCtor),
            one_or_more_to_list(Ctors), !MaybeStoppingReason, !Info)
    ).

:- pred check_functor_ambiguities(recomp_avail::in, type_ctor::in,
    constructor::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities(RecompAvail, TypeCtor, Ctor,
        !MaybeStoppingReason, !Info) :-
    Ctor = ctor(_, _, SymName, Args, Arity, _),
    ResolvedCtor = resolved_functor_data_constructor(TypeCtor),
    check_functor_ambiguities_by_name(RecompAvail, SymName,
        match_arity_exact(Arity), ResolvedCtor, !MaybeStoppingReason, !Info),
    ConsCtor = cons_ctor(SymName, Arity, TypeCtor),
    FieldAccessResolvedCtor =
        resolved_functor_field_access_func(ConsCtor),
    list.foldl2(
        check_field_ambiguities(RecompAvail, FieldAccessResolvedCtor),
        Args, !MaybeStoppingReason, !Info).

:- pred check_field_ambiguities(recomp_avail::in,
    resolved_functor::in, constructor_arg::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_field_ambiguities(RecompAvail, ResolvedCtor, CtorArg,
        !MaybeStoppingReason, !Info) :-
    CtorArg = ctor_arg(MaybeCtorFieldName, _, _),
    (
        MaybeCtorFieldName = no
    ;
        MaybeCtorFieldName = yes(CtorFieldName),
        CtorFieldName = ctor_field_name(FieldName, _Ctxt),
        % XXX The arities to match below will need to change if we ever
        % allow taking the address of field access functions.
        field_access_function_name(get, FieldName, ExtractFuncSymName),
        field_access_function_name(set, FieldName, UpdateFuncSymName),
        check_functor_ambiguities_by_name(RecompAvail, ExtractFuncSymName,
            match_arity_exact(1), ResolvedCtor, !MaybeStoppingReason, !Info),
        check_functor_ambiguities_by_name(RecompAvail, UpdateFuncSymName,
            match_arity_exact(2), ResolvedCtor, !MaybeStoppingReason, !Info)
    ).

    % Predicates and functions used as functors can match any arity
    % less than or equal to the predicate or function's arity.
:- type functor_match_arity
    --->    match_arity_exact(arity)
    ;       match_arity_less_than_or_equal(arity)
    ;       match_arity_any.

:- pred check_functor_ambiguities_by_name(recomp_avail::in,
    sym_name::in, functor_match_arity::in, resolved_functor::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities_by_name(RecompAvail, SymName, MatchArity,
        ResolvedCtor, !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        UsedItems = !.Info ^ rci_used_items,
        Name = unqualify_name(SymName),
        UsedCtors = UsedItems ^ rui_functors,
        ( if map.search(UsedCtors, Name, UsedCtorAL) then
            check_functor_ambiguities_by_name_loop(RecompAvail, SymName,
                MatchArity, ResolvedCtor, UsedCtorAL,
                no, !:MaybeStoppingReason, !Info)
        else
            !:MaybeStoppingReason = no
        )
    ).

:- pred check_functor_ambiguities_by_name_loop(recomp_avail::in, sym_name::in,
    functor_match_arity::in, resolved_functor::in,
    assoc_list(arity, resolved_functor_map)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguities_by_name_loop(_, _, _, _, [],
        !MaybeStoppingReason, !Info).
check_functor_ambiguities_by_name_loop(RecompAvail, SymName, MatchArity,
        ResolvedCtor, [Arity - UsedCtorMap | UsedCtorAL],
        !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        (
            MatchArity = match_arity_exact(ArityToMatch),
            ( if ArityToMatch = Arity then
                Check = yes,
                Continue = no
            else
                Check = no,
                ( if Arity < ArityToMatch then
                    Continue = yes
                else
                    Continue = no
                )
            )
        ;
            MatchArity = match_arity_less_than_or_equal(ArityToMatch),
            ( if Arity =< ArityToMatch then
                Check = yes,
                Continue = yes
            else
                Check = no,
                Continue = no
            )
        ;
            MatchArity = match_arity_any,
            Check = yes,
            Continue = yes
        ),
        (
            Check = yes,
            map.foldl2(
                check_functor_ambiguity(RecompAvail, SymName, Arity,
                    ResolvedCtor),
                UsedCtorMap, no, !:MaybeStoppingReason, !Info)
        ;
            Check = no
        ),
        (
            Continue = yes,
            check_functor_ambiguities_by_name_loop(RecompAvail, SymName,
                MatchArity, ResolvedCtor, UsedCtorAL,
                !MaybeStoppingReason, !Info)
        ;
            Continue = no
        )
    ).

:- pred check_functor_ambiguity(recomp_avail::in,
    sym_name::in, arity::in, resolved_functor::in,
    module_qualifier::in, set(resolved_functor)::in,
    maybe(recompile_reason)::in, maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

check_functor_ambiguity(RecompAvail, SymName, Arity, ResolvedCtor,
        OldModuleQualifier, OldResolvedCtors, !MaybeStoppingReason, !Info) :-
    (
        !.MaybeStoppingReason = yes(_)
    ;
        !.MaybeStoppingReason = no,
        ( if
            % XXX RECOMP401 This logic is ancient, and may do the wrong thing
            % with most values of RecompAvail, since those values did not exist
            % when the original version of this was written.
            ( RecompAvail = recomp_avail_int_use
            ; RecompAvail = recomp_avail_imp_use
            ),
            % XXX This is a bit conservative in the case of partially qualified
            % names but that hopefully won't come up too often.
            OldModuleQualifier = unqualified("")
        then
            !:MaybeStoppingReason = no
        else if
            Name = unqualify_name(SymName),
            OldName = module_qualify_name(OldModuleQualifier, Name),
            partial_sym_name_matches_full(OldName, SymName),
            not set.member(ResolvedCtor, OldResolvedCtors)
        then
            OldModuleQualName = module_qualify_name(OldModuleQualifier, Name),
            Reason = recompile_for_functor_ambiguity(OldModuleQualName, Arity,
                ResolvedCtor, set.to_sorted_list(OldResolvedCtors)),
            record_recompilation_reason(Reason, !:MaybeStoppingReason, !Info)
        else
            !:MaybeStoppingReason = no
        )
    ).

%---------------------------------------------------------------------------%

:- type recompilation_check_info
    --->    recompilation_check_info(
                rci_collect_all_reasons     :: bool,
                rci_have_parse_tree_maps    :: have_parse_tree_maps,
                rci_used_items              :: resolved_used_items,
                rci_used_typeclasses        :: set(recomp_item_name),
                rci_what_to_recompile       :: file_components_to_recompile,
                rci_recompilation_reasons   :: list(recompile_reason)
            ).

:- type recompile_reason
    --->    recompile_for_file_error(
                file_name,
                list(format_piece)
            )
    ;       recompile_for_output_file_not_up_to_date(
                file_name
            )
    ;       recompile_for_file_changed(
                file_name
            )
    ;       recompile_for_item_ambiguity(
                recomp_item_id,                 % new item.
                list(recomp_item_id)            % ambiguous declarations.
            )
    ;       recompile_for_functor_ambiguity(
                sym_name,
                arity,
                resolved_functor,               % new item.
                list(resolved_functor)          % ambiguous declarations.
            )
    ;       recompile_for_changed_item(
                recomp_item_id
            )
    ;       recompile_for_removed_item(
                recomp_item_id
            )
    ;       recompile_for_changed_or_added_instance(
                module_name,
                recomp_item_name                % class name
            )
    ;       recompile_for_removed_instance(
                module_name,
                recomp_item_name                % class name
            ).

:- pred add_module_to_recompile(module_name::in, recompilation_check_info::in,
    recompilation_check_info::out) is det.

add_module_to_recompile(Module, !Info) :-
    ModulesToRecompile0 = !.Info ^ rci_what_to_recompile,
    (
        ModulesToRecompile0 = all_file_components
    ;
        ModulesToRecompile0 = some_file_components(Modules0),
        Modules = [Module | Modules0],
        !Info ^ rci_what_to_recompile := some_file_components(Modules)
    ).

%---------------------------------------------------------------------------%

:- pred record_recompilation_reason(recompile_reason::in,
    maybe(recompile_reason)::out,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_recompilation_reason(Reason, MaybeStoppingReason, !Info) :-
    CollectAllReasons = !.Info ^ rci_collect_all_reasons,
    (
        CollectAllReasons = yes,
        % XXX Note that many places in the code above record a stopping reason
        % *without* either calling this predicate or checking the value of
        % CollectAllReasons themselves, so CollectAllReasons being yes
        % does NOT guarantee that we in fact collect all reasons to recompile.
        MaybeStoppingReason = no,
        !Info ^ rci_recompilation_reasons :=
            [Reason | !.Info ^ rci_recompilation_reasons]
    ;
        CollectAllReasons = no,
        MaybeStoppingReason = yes(Reason)
    ).

%---------------------------------------------------------------------------%

:- type ambiguity_checkables
    --->    ambiguity_checkables(
                % NOTE We should consider making the types of the first
                % three fields type_ctor_defn_map, inst_ctor_defn_map and
                % mode_ctor_defn_map respectively. However, before we do that,
                % we need to decide exactly how we want to handle any entries
                % in the implementation section versions of those maps.
                % I (zs) think it is quite likely that the original code
                % of this module did not consider the treatment of such entries
                % thoroughly enough.
                %
                % Consider that the original motivation to put type
                % definitions into the implementation sections of .int files
                % was to give the compiler the information it needs to decide
                % on the correct representation of values of the type,
                % especially in the context of equivalence types involving
                % floats, which at the time were stored in two words
                % (as 64 bit entities on a 32 bit platform). However,
                % such type definition items specify non-user-visible
                % information, and as such should not be able to affect
                % which type names are ambiguous and which are not.
                % And yet the code of this module has always processed
                % type definition items without regard to which section
                % of an interface file they occurred in. (It is possible
                % that the reason for this is that when this code was first
                % written, interface files did not *have* implementation
                % sections.)
                list(item_type_defn_info),
                list(item_inst_defn_info),
                list(item_mode_defn_info),
                list(item_typeclass_info),
                list(item_pred_decl_info)
            ).

%---------------------%

:- typeclass check_imported_module_int_file(PT) where [
    pred cim_search_mapN(have_parse_tree_map(PT)::in,
        module_name::in, have_module(PT)::out) is semidet,
    pred cim_read_module_intN(io.text_output_stream::in, globals::in,
        read_reason_msg::in, maybe_search::in,
        module_name::in, read_module_and_timestamps::in,
        have_module(PT)::out, io::di, io::uo) is det,
    pred cim_record_read_file_intN(module_name::in, file_name::in,
        module_timestamp::in, PT::in, read_module_errors::in,
        recompilation_check_info::in, recompilation_check_info::out) is det,
    pred cim_get_version_numbersN(PT::in,
        module_item_version_numbers::out) is semidet,
    pred cim_get_ambiguity_checkablesN(PT::in,
        ambiguity_checkables::out) is det
].

:- instance check_imported_module_int_file(parse_tree_int0) where [
    ( cim_search_mapN(HPTM, ModuleName, HaveReadModule) :-
        map.search(HPTM, ModuleName, HaveReadModule)
    ),
    pred(cim_read_module_intN/9) is read_module_int0,
    pred(cim_record_read_file_intN/7) is record_read_file_int0,
    ( cim_get_version_numbersN(PT, VN) :-
        PT ^ pti0_maybe_version_numbers = version_numbers(VN)
    ),
    pred(cim_get_ambiguity_checkablesN/2) is get_ambiguity_checkables_int0
].

:- instance check_imported_module_int_file(parse_tree_int1) where [
    ( cim_search_mapN(HPTM, ModuleName, HaveReadModule) :-
        map.search(HPTM, ModuleName, HaveReadModule)
    ),
    pred(cim_read_module_intN/9) is read_module_int1,
    pred(cim_record_read_file_intN/7) is record_read_file_int1,
    ( cim_get_version_numbersN(PT, VN) :-
        PT ^ pti1_maybe_version_numbers = version_numbers(VN)
    ),
    pred(cim_get_ambiguity_checkablesN/2) is get_ambiguity_checkables_int1
].

:- instance check_imported_module_int_file(parse_tree_int2) where [
    ( cim_search_mapN(HPTM, ModuleName, HaveReadModule) :-
        map.search(HPTM, ModuleName, HaveReadModule)
    ),
    pred(cim_read_module_intN/9) is read_module_int2,
    pred(cim_record_read_file_intN/7) is record_read_file_int2,
    ( cim_get_version_numbersN(PT, VN) :-
        PT ^ pti2_maybe_version_numbers = version_numbers(VN)
    ),
    pred(cim_get_ambiguity_checkablesN/2) is get_ambiguity_checkables_int2
].

:- instance check_imported_module_int_file(parse_tree_int3) where [
    ( cim_search_mapN(HPTM, ModuleName, HaveReadModule) :-
        map.search(HPTM, ModuleName, HaveReadModule)
    ),
    pred(cim_read_module_intN/9) is read_module_int3,
    pred(cim_record_read_file_intN/7) is record_read_file_int3,
    ( cim_get_version_numbersN(_PT, _VN) :-
        fail
    ),
    pred(cim_get_ambiguity_checkablesN/2) is get_ambiguity_checkables_int3
].

%---------------------%

:- pred record_read_file_src(module_name::in, file_name::in,
    module_timestamp::in, parse_tree_src::in, read_module_errors::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_read_file_src(ModuleName, FileName, ModuleTimestamp,
        ParseTreeSrc, Errors, !Info) :-
    HaveParseTreeMaps0 = !.Info ^ rci_have_parse_tree_maps,
    HaveParseTreeMapSrc0 = HaveParseTreeMaps0 ^ hptm_src,
    ModuleTimestamp = module_timestamp(_, Timestamp, _),
    % XXX Make this map.det_insert once we have ensured that
    % we read ParseTreeSrc *only* if we do not already have it.
    map.set(ModuleName,
        have_module(FileName, ParseTreeSrc, was_read(yes(Timestamp), Errors)),
        HaveParseTreeMapSrc0, HaveParseTreeMapSrc),
    HaveParseTreeMaps =
        HaveParseTreeMaps0 ^ hptm_src := HaveParseTreeMapSrc,
    !Info ^ rci_have_parse_tree_maps := HaveParseTreeMaps.

:- pred record_read_file_int0(module_name::in, file_name::in,
    module_timestamp::in, parse_tree_int0::in, read_module_errors::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_read_file_int0(ModuleName, FileName, ModuleTimestamp, ParseTreeInt0,
        Errors, !Info) :-
    ModuleTimestamp = module_timestamp(_, Timestamp, _),
    HaveParseTreeMaps0 = !.Info ^ rci_have_parse_tree_maps,
    HPTM0 = HaveParseTreeMaps0 ^ hptm_int0,
    ReadResult = have_module(FileName, ParseTreeInt0,
        was_read(yes(Timestamp), Errors)),
    map.set(ModuleName, ReadResult, HPTM0, HPTM),
    HaveParseTreeMaps = HaveParseTreeMaps0 ^ hptm_int0 := HPTM,
    !Info ^ rci_have_parse_tree_maps := HaveParseTreeMaps.

:- pred record_read_file_int1(module_name::in, file_name::in,
    module_timestamp::in, parse_tree_int1::in, read_module_errors::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_read_file_int1(ModuleName, FileName, ModuleTimestamp, ParseTreeInt1,
        Errors, !Info) :-
    ModuleTimestamp = module_timestamp(_, Timestamp, _),
    HaveParseTreeMaps1 = !.Info ^ rci_have_parse_tree_maps,
    HPTM1 = HaveParseTreeMaps1 ^ hptm_int1,
    ReadResult = have_module(FileName, ParseTreeInt1,
        was_read(yes(Timestamp), Errors)),
    map.set(ModuleName, ReadResult, HPTM1, HPTM),
    HaveParseTreeMaps = HaveParseTreeMaps1 ^ hptm_int1 := HPTM,
    !Info ^ rci_have_parse_tree_maps := HaveParseTreeMaps.

:- pred record_read_file_int2(module_name::in, file_name::in,
    module_timestamp::in, parse_tree_int2::in, read_module_errors::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_read_file_int2(ModuleName, FileName, ModuleTimestamp, ParseTreeInt2,
        Errors, !Info) :-
    ModuleTimestamp = module_timestamp(_, Timestamp, _),
    HaveParseTreeMaps2 = !.Info ^ rci_have_parse_tree_maps,
    HPTM2 = HaveParseTreeMaps2 ^ hptm_int2,
    ReadResult = have_module(FileName, ParseTreeInt2,
        was_read(yes(Timestamp), Errors)),
    map.set(ModuleName, ReadResult, HPTM2, HPTM),
    HaveParseTreeMaps = HaveParseTreeMaps2 ^ hptm_int2 := HPTM,
    !Info ^ rci_have_parse_tree_maps := HaveParseTreeMaps.

:- pred record_read_file_int3(module_name::in, file_name::in,
    module_timestamp::in, parse_tree_int3::in, read_module_errors::in,
    recompilation_check_info::in, recompilation_check_info::out) is det.

record_read_file_int3(ModuleName, FileName, ModuleTimestamp, ParseTreeInt3,
        Errors, !Info) :-
    ModuleTimestamp = module_timestamp(_, Timestamp, _),
    HaveParseTreeMaps3 = !.Info ^ rci_have_parse_tree_maps,
    HPTM3 = HaveParseTreeMaps3 ^ hptm_int3,
    ReadResult = have_module(FileName, ParseTreeInt3,
        was_read(yes(Timestamp), Errors)),
    map.set(ModuleName, ReadResult, HPTM3, HPTM),
    HaveParseTreeMaps = HaveParseTreeMaps3 ^ hptm_int3 := HPTM,
    !Info ^ rci_have_parse_tree_maps := HaveParseTreeMaps.

%---------------------%

:- pred get_ambiguity_checkables_int0(parse_tree_int0::in,
    ambiguity_checkables::out) is det.

get_ambiguity_checkables_int0(ParseTreeInt0, Checkables) :-
    ParseTreeInt0 = parse_tree_int0(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _InclMap, _ImportUseMap, _IntFIMs, _ImpFIMs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, _IntInstances, IntPredDecls, _IntModeDecls,
        _IntDeclPragmas, _IntDeclMarkers, _IntPromises,
        ImpTypeClasses, _ImpInstances, ImpPredDecls, _ImpModeDecls,
        _ImpDeclPragmas, _ImpDeclMarkers, _ImpPromises),
    type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
        IntTypeDefns, ImpTypeDefns, _ImpForeignEnums),
    inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
        IntInstDefns, ImpInstDefns),
    mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
        IntModeDefns, ImpModeDefns),

    ItemTypeDefns = IntTypeDefns ++ ImpTypeDefns,
    ItemInstDefns = IntInstDefns ++ ImpInstDefns,
    ItemModeDefns = IntModeDefns ++ ImpModeDefns,
    ItemTypeClasses = IntTypeClasses ++ ImpTypeClasses,
    ItemPredDecls = IntPredDecls ++ ImpPredDecls,
    Checkables = ambiguity_checkables(ItemTypeDefns,
        ItemInstDefns, ItemModeDefns, ItemTypeClasses, ItemPredDecls).

:- pred get_ambiguity_checkables_int1(parse_tree_int1::in,
    ambiguity_checkables::out) is det.

get_ambiguity_checkables_int1(ParseTreeInt1, Checkables) :-
    ParseTreeInt1 = parse_tree_int1(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _InclMap, _ImportUseMap, _IntFIMs, _ImpFIMs,
        TypeDefnCheckedMap, InstDefnCheckedMap, ModeDefnCheckedMap,
        IntTypeClasses, _IntItemInstances, IntPredDecls, _IntModeDecls,
        _IntDeclPragmas, _IntDeclMarkers, _IntPromises,
        _IntTypeRepnMap, ImpTypeClasses),
    type_ctor_checked_map_get_src_defns(TypeDefnCheckedMap,
        IntTypeDefns, ImpTypeDefns, _ImpForeignEnums),
    inst_ctor_checked_map_get_src_defns(InstDefnCheckedMap,
        IntInstDefns, ImpInstDefns),
    mode_ctor_checked_map_get_src_defns(ModeDefnCheckedMap,
        IntModeDefns, ImpModeDefns),
    expect(unify(ImpInstDefns, []), $pred, "ImpInstDefns != []"),
    expect(unify(ImpModeDefns, []), $pred, "ImpModeDefns != []"),
    ItemTypeDefns = IntTypeDefns ++ ImpTypeDefns,
    ItemInstDefns = IntInstDefns,
    ItemModeDefns = IntModeDefns,
    ItemTypeClasses = IntTypeClasses ++ coerce(ImpTypeClasses),
    ItemPredDecls = IntPredDecls,
    Checkables = ambiguity_checkables(ItemTypeDefns,
        ItemInstDefns, ItemModeDefns, ItemTypeClasses, ItemPredDecls).

:- pred get_ambiguity_checkables_int2(parse_tree_int2::in,
    ambiguity_checkables::out) is det.

get_ambiguity_checkables_int2(ParseTreeInt2, Checkables) :-
    ParseTreeInt2 = parse_tree_int2(_ModuleName, _ModuleNameContext,
        _MaybeVersionNumbers, _InclMap, _ImportUseMap, _IntFIMs, _ImpFIMs,
        TypeDefnCheckedMap, InstDefnCheckedMap, ModeDefnCheckedMap,
        IntItemTypeClasses, _IntItemInstances, _IntTypeRepnMap),
    type_ctor_checked_map_get_src_defns(TypeDefnCheckedMap,
        IntTypeDefns, ImpTypeDefns, _ImpForeignEnums),
    inst_ctor_checked_map_get_src_defns(InstDefnCheckedMap,
        IntInstDefns, ImpInstDefns),
    mode_ctor_checked_map_get_src_defns(ModeDefnCheckedMap,
        IntModeDefns, ImpModeDefns),
    expect(unify(ImpInstDefns, []), $pred, "ImpInstDefns != []"),
    expect(unify(ImpModeDefns, []), $pred, "ImpModeDefns != []"),
    ItemTypeDefns = IntTypeDefns ++ ImpTypeDefns,
    ItemInstDefns = IntInstDefns,
    ItemModeDefns = IntModeDefns,
    ItemTypeClasses = IntItemTypeClasses,
    ItemPredDecls = [],
    Checkables = ambiguity_checkables(ItemTypeDefns,
        ItemInstDefns, ItemModeDefns, ItemTypeClasses, ItemPredDecls).

:- pred get_ambiguity_checkables_int3(parse_tree_int3::in,
    ambiguity_checkables::out) is det.

get_ambiguity_checkables_int3(ParseTreeInt3, Checkables) :-
    ParseTreeInt3 = parse_tree_int3(_ModuleName, _ModuleNameContext,
        _InclMap, _ImportUseMap,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, _IntInstances, _TypeRepnMap),
    type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
        IntTypeDefns, ImpTypeDefns, _ImpForeignEnums),
    inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
        IntInstDefns, ImpInstDefns),
    mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
        IntModeDefns, ImpModeDefns),
    expect(unify(ImpTypeDefns, []), $pred, "ImpTypeDefns != []"),
    expect(unify(ImpInstDefns, []), $pred, "ImpInstDefns != []"),
    expect(unify(ImpModeDefns, []), $pred, "ImpModeDefns != []"),
    IntPredDecls = [],
    Checkables = ambiguity_checkables(IntTypeDefns,
        IntInstDefns, IntModeDefns, coerce(IntTypeClasses), IntPredDecls).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred write_not_recompiling_message(module_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_not_recompiling_message(ModuleName, Stream, !IO) :-
    io.format(Stream, "Not recompiling module %s.\n",
        [s(escaped_sym_name_to_string(ModuleName))], !IO).

:- pred write_reasons_message(globals::in, module_name::in,
    list(recompile_reason)::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_reasons_message(Globals, ModuleName, Reasons, Stream, !IO) :-
    list.foldl(write_recompile_reason(Globals, Stream, ModuleName),
        Reasons, !IO).

:- pred write_used_file_error(globals::in, module_name::in,
    used_file_error::in, io.text_output_stream::in, io::di, io::uo) is det.

write_used_file_error(Globals, ModuleName, UsedFileError, Stream, !IO) :-
    PrefixPieces = [words("Recompiling module"), qual_sym_name(ModuleName),
        suffix(":"), nl],
    (
        UsedFileError = uf_read_error(FileName, _IOError),
        Pieces = [words("file"), quote(FileName), words("not found."), nl],
        Spec = no_ctxt_spec($pred, severity_informational,
            phase_read_files, PrefixPieces ++ Pieces)
    ;
        UsedFileError = uf_invalid_file_format(FileName),
        Pieces = [words("invalid version number in"), quote(FileName),
            suffix("."), nl],
        Spec = no_ctxt_spec($pred, severity_informational,
            phase_read_files, PrefixPieces ++ Pieces)
    ;
        UsedFileError = uf_syntax_error(Context, Message),
        AllPieces = PrefixPieces ++ [words(Message), suffix("."), nl],
        Spec = spec($pred, severity_informational, phase_read_files,
            Context, AllPieces)
    ;
        UsedFileError = uf_unreadable_used_items(UsedItemSpecs),
        list.map(extract_spec_msgs_and_maybe_add_id(Globals),
            UsedItemSpecs, MsgsList),
        list.condense(MsgsList, Msgs),
        % MaybeContext = find_first_context_in_msgs(Msgs),
        Spec = error_spec($pred, severity_informational, phase_read_files,
            Msgs)
    ),
    write_error_spec(Stream, Globals, Spec, !IO).

:- pred maybe_write_recompilation_message(io.text_output_stream::in,
    globals::in,
    pred(io.text_output_stream, io, io)::in(pred(in, di, uo) is det),
    io::di, io::uo) is det.

maybe_write_recompilation_message(ProgressStream, Globals, P, !IO) :-
    globals.lookup_bool_option(Globals, verbose_recompilation, Verbose),
    (
        Verbose = yes,
        P(ProgressStream, !IO)
    ;
        Verbose = no
    ).

:- pred write_recompile_reason(globals::in, io.text_output_stream::in,
    module_name::in, recompile_reason::in, io::di, io::uo) is det.

write_recompile_reason(Globals, Stream, ThisModuleName, Reason, !IO) :-
    PrefixPieces = [words("Recompiling module"),
        qual_sym_name(ThisModuleName), suffix(":"), nl],
    (
        Reason = recompile_for_file_error(_FileName, Pieces)
        % Pieces should mention FileName.
    ;
        Reason = recompile_for_output_file_not_up_to_date(FileName),
        Pieces = [words("output file"), quote(FileName),
            words("is not up to date."), nl]
    ;
        Reason = recompile_for_file_changed(FileName),
        Pieces = [words("file"), quote(FileName), words("has changed."), nl]
    ;
        Reason = recompile_for_item_ambiguity(Item, AmbiguousItems),
        ItemPieces = describe_item(Item),
        AmbiguousItemPieces = pieces_list_to_pieces("and",
            list.map(describe_item, AmbiguousItems)),
        Pieces = [words("addition of") | ItemPieces]
            ++ [words("could cause an ambiguity with")]
            ++ AmbiguousItemPieces ++ [suffix("."), nl]
    ;
        Reason = recompile_for_functor_ambiguity(SymName, Arity,
            Functor, AmbiguousFunctors),
        FunctorPieces = describe_resolved_functor(SymName, Arity, Functor),
        AmbiguousFunctorPieces = pieces_list_to_pieces("and",
            list.map(describe_resolved_functor(SymName, Arity),
                AmbiguousFunctors)),
        Pieces = [words("addition of") | FunctorPieces]
            ++ [words("could cause an ambiguity with")]
            ++ AmbiguousFunctorPieces ++ [suffix("."), nl]
    ;
        Reason = recompile_for_changed_item(Item),
        Pieces = describe_item(Item) ++ [words("was modified."), nl]
    ;
        Reason = recompile_for_removed_item(Item),
        Pieces = describe_item(Item) ++ [words("was removed."), nl]
    ;
        Reason = recompile_for_changed_or_added_instance(ModuleName,
            recomp_item_name(ClassName, ClassArity)),
        Pieces = [words("an instance for class"),
            qual_sym_name_arity(sym_name_arity(ClassName, ClassArity)),
            words("in module"), qual_sym_name(ModuleName),
            words("was added or modified."), nl]
    ;
        Reason = recompile_for_removed_instance(ModuleName,
            recomp_item_name(ClassName, ClassArity)),
        Pieces = [words("an instance for class "),
            qual_sym_name_arity(sym_name_arity(ClassName, ClassArity)),
            words("in module"), qual_sym_name(ModuleName),
            words("was removed."), nl]
    ),
    AllPieces = PrefixPieces ++ Pieces,
    Spec = no_ctxt_spec($pred, severity_informational,
        phase_read_files, AllPieces),
    % Since these messages are informational, there should be no warnings
    % or errors.
    write_error_spec(Stream, Globals, Spec, !IO).

:- func describe_item(recomp_item_id) = list(format_piece).

describe_item(ItemId) = Pieces :-
    ItemId = recomp_item_id(ItemType0, ItemName),
    ( if is_body_of_item(ItemType0, ItemType1) then
        string_to_recomp_item_type(ItemTypeStr, ItemType1),
        ItemPieces = [words("body of"), words(ItemTypeStr)]
    else
        string_to_recomp_item_type(ItemTypeStr, ItemType0),
        ItemPieces = [words(ItemTypeStr)]
    ),
    ItemName = recomp_item_name(SymName, Arity),
    Pieces = ItemPieces ++
        [qual_sym_name_arity(sym_name_arity(SymName, Arity))].

:- pred is_body_of_item(recomp_item_type::in, recomp_item_type::out)
    is semidet.

is_body_of_item(recomp_type_defn, recomp_type_name).

:- func describe_resolved_functor(sym_name, arity, resolved_functor) =
    list(format_piece).

describe_resolved_functor(SymName, Arity, ResolvedFunctor) = Pieces :-
    (
        ResolvedFunctor = resolved_functor_pred_or_func(_, PredOrFunc,
            ModuleName, PredArity),
        Name = unqualify_name(SymName),
        PFStr = pred_or_func_to_full_str(PredOrFunc),
        user_arity_pred_form_arity(PredOrFunc, user_arity(UserArity),
            PredArity),
        SNA = sym_name_arity(qualified(ModuleName, Name), UserArity),
        Pieces = [words(PFStr), qual_sym_name_arity(SNA)]
    ;
        ResolvedFunctor = resolved_functor_data_constructor(TypeCtor),
        SNA = sym_name_arity(SymName, Arity),
        Pieces = [words("constructor"), unqual_sym_name_arity(SNA),
            words("of type"), qual_type_ctor(TypeCtor)]
    ;
        ResolvedFunctor = resolved_functor_field_access_func(ConsCtor),
        SNA = sym_name_arity(SymName, Arity),
        ConsCtor = cons_ctor(ConsName, ConsArity, TypeCtor),
        ConsSNA = sym_name_arity(ConsName, ConsArity),
        Pieces = [words("field access function"), unqual_sym_name_arity(SNA),
            words("for constructor"), unqual_sym_name_arity(ConsSNA),
            words("of type"), qual_type_ctor(TypeCtor)]
    ).

%---------------------------------------------------------------------------%
:- end_module recompilation.check.
%---------------------------------------------------------------------------%
