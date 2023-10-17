%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: write_module_interface_files.m.
% Main author: fjh (when this code was in modules.m).
%
% This module writes the automatically generated .int3, .int2 and .int files
% (and if needed, the .int0 file) for each Mercury source module.
%
% The interface file system works as follows:
%
% 1. a .int3 file is written, which contains all the types, typeclasses, insts
% and modes defined in the interface. Equivalence types, solver types, insts
% and modes are written in full, others are written in abstract form. These
% are module qualified as far as possible given the information present in the
% current module.
%
% 2. The .int and .int2 files are created, using the .int3 files
% of imported modules to fully module qualify all items.
% The .int2 file is mostly just a fully qualified version of the .int3 file,
% however it also includes some extra information, such as functors for
% discriminated union types, which may be needed for mode analysis.
%
% 3. The .int0 file is similar to the .int file except that it also
% includes declarations (but not clauses) from the implementation section.
% It is generated only for modules that have submodules.
%
% NOTE The above is only a summary, and may be out of date. An attempt
% at an up-to-date and much more detailed description can be found in
% notes/interface_files.html.
%
%---------------------------------------------------------------------------%
%
% The datestamp on the .date3 file gives the last time
% the .int3 file was checked for consistency.
%
% The datestamp on the .date file gives the last time
% the .int and .int2 files were checked for consistency.
%
% The datestamp on the .date0 file gives the last time
% the .int0 file was checked for consistency.
%
%---------------------------------------------------------------------------%

:- module parse_tree.write_module_interface_files.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Each of the predicates
    %
    %   write_short_interface_file_int3
    %   write_private_interface_file_int0
    %   write_interface_file_int1_int2
    %
    % has an argument of this type. Their callers can set this argument to
    % do_add_new_to_hrmm to tell the predicate to add the interface file(s)
    % it has constructed to !HaveReadModuleMaps.
    %
:- type maybe_add_to_hrmm
    --->    do_not_add_new_to_hrmm
    ;       do_add_new_to_hrmm.

    % write_short_interface_file_int3(ProgressStream, ErrorStream, Globals,
    %   AddToHrmm, ParseTreeModuleSrc, Succeeded, Specs,
    %   !HaveReadModuleMaps, !IO):
    %
    % Output the unqualified short interface file to <module>.int3.
    %
    % Specs will contain any error and/or warning messages resulting
    % - from the process of constructing the contents of the .int3 file,
    % - from the process of writing it out, and
    % - updating the associated timestamp file.
    %
    % We bind Succeeded to "succeeded" only if all three of those processes
    % succeeded without errors. (Specs may contain warnings even then.)
    %
    % We add the contents of the .int3 file to !HaveReadModuleMaps if we could
    % construct it without any errors, *and* AddToHrmm says we should.
    %
:- pred write_short_interface_file_int3(
    io.text_output_stream::in, io.text_output_stream::in, globals::in,
    maybe_add_to_hrmm::in, parse_tree_module_src::in,
    maybe_succeeded::out, list(error_spec)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

    % write_private_interface_file_int0(ProgressStream, ErrorStream, Globals,
    %   AddToHrmm, SourceFileName, SourceFileModuleName, MaybeTimestamp,
    %   ParseTreeModuleSrc0, Succeeded, Specs, !HaveReadModuleMaps, !IO):
    %
    % Given a source file name, the timestamp of the source file, and the
    % representation of a module in that file, output the private (`.int0')
    % interface file for the module. (The private interface contains all the
    % declarations in the module, including those in the `implementation'
    % section; it is used when compiling submodules.)
    %
:- pred write_private_interface_file_int0(
    io.text_output_stream::in, io.text_output_stream::in, globals::in,
    maybe_add_to_hrmm::in, file_name::in, module_name::in,
    maybe(timestamp)::in, parse_tree_module_src::in,
    maybe_succeeded::out, list(error_spec)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.
:- pred write_private_interface_file_int0_burdened_module(
    io.text_output_stream::in, io.text_output_stream::in, globals::in,
    maybe_add_to_hrmm::in, burdened_module::in,
    maybe_succeeded::out, list(error_spec)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

    % write_interface_file_int1_int2(ProgressStream, ErrorStream, Globals,
    %   AddToHrmm, SourceFileName, SourceFileModuleName, MaybeTimestamp,
    %   ParseTreeModuleSrc0, Succeeded, Specs, !HaveReadModuleMaps, !IO):
    %
    % Given a source file name, the timestamp of the source file, and the
    % representation of a module in that file, output the long (`.int')
    % and short (`.int2') interface files for the module.
    %
:- pred write_interface_file_int1_int2(
    io.text_output_stream::in, io.text_output_stream::in, globals::in,
    maybe_add_to_hrmm::in, file_name::in, module_name::in,
    maybe(timestamp)::in, parse_tree_module_src::in,
    maybe_succeeded::out, list(error_spec)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.
:- pred write_interface_file_int1_int2_burdened_module(
    io.text_output_stream::in, io.text_output_stream::in, globals::in,
    maybe_add_to_hrmm::in, burdened_module::in,
    maybe_succeeded::out, list(error_spec)::out,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.comp_unit_interface.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.grab_modules.           % undesirable dependency
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_tree_out.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module bool.
:- import_module getopt.
:- import_module io.file.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%
%
% Write out .int3 files.
%

write_short_interface_file_int3(ProgressStream, ErrorStream, Globals,
        AddToHrmm, ParseTreeModuleSrc, Succeeded, Specs,
        !HaveReadModuleMaps, !IO) :-
    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.
    generate_short_interface_int3(Globals, ParseTreeModuleSrc, ParseTreeInt3,
        [], Specs0),
    filter_interface_generation_specs(Globals, Specs0, Specs1),
    EffectivelyErrors =
        contains_errors_or_warnings_treated_as_errors(Globals, Specs1),
    ModuleName = ParseTreeModuleSrc ^ ptms_module_name,
    (
        EffectivelyErrors = no,
        ExtraSuffix = "",
        construct_int_file_name(Globals, ModuleName, ifk_int3, ExtraSuffix,
            FileName, TmpFileName, !IO),
        actually_write_interface_file3(ProgressStream, ErrorStream,
            Globals, ParseTreeInt3, FileName, TmpFileName, no,
            OutputSucceeded, !IO),
        touch_module_ext_datestamp(Globals, ProgressStream,
            ModuleName, ext_cur_ngs(ext_cur_ngs_int_date_int3),
            TouchSucceeded, !IO),
        Succeeded = OutputSucceeded `and` TouchSucceeded,
        Specs = Specs1,
        (
            AddToHrmm = do_not_add_new_to_hrmm
        ;
            AddToHrmm = do_add_new_to_hrmm,
            Int3Map0 = !.HaveReadModuleMaps ^ hrmm_int3,
            % XXX If needed for smart recompilation, we could get
            % the actual timestamp of the int0 file.
            MaybeTimestampInt = maybe.no,
            % XXX ReadModuleErrors
            ReadModuleErrors = read_module_errors(set.init, [],
                set.init, [], []),
            HRM = have_read_module(FileName, MaybeTimestampInt,
                ParseTreeInt3, ReadModuleErrors),
            map.set(ModuleName, HRM, Int3Map0, Int3Map),
            !HaveReadModuleMaps ^ hrmm_int3 := Int3Map
        )
    ;
        EffectivelyErrors = yes,
        report_file_not_written(Globals, [], ModuleName,
            ext_cur_ngs(ext_cur_ngs_int_int3), no,
            ext_cur_ngs(ext_cur_ngs_int_date_int3), Specs1, Specs, !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
%
% Write out .int0 files.
%

write_private_interface_file_int0(ProgressStream, ErrorStream, Globals,
        AddToHrmm, SourceFileName, SourceFileModuleName, MaybeTimestamp,
        ParseTreeModuleSrc0, Succeeded, Specs, !HaveReadModuleMaps, !IO) :-
    ModuleName = ParseTreeModuleSrc0 ^ ptms_module_name,
    grab_unqual_imported_modules_make_int(ProgressStream, Globals,
        SourceFileName, SourceFileModuleName, ParseTreeModuleSrc0,
        Baggage, AugMakeIntUnit1, !HaveReadModuleMaps, !IO),

    % Check whether we succeeded.
    GetErrors = Baggage ^ mb_errors,
    GetSpecs = get_read_module_specs(GetErrors),
    GetSpecsEffectivelyErrors =
        contains_errors_or_warnings_treated_as_errors(Globals, GetSpecs),
    ( if
        GetSpecsEffectivelyErrors = no,
        there_are_no_errors(GetErrors)
    then
        % Module-qualify all items.
        % XXX ITEM_LIST We don't need grab_unqual_imported_modules
        % to include in ModuleAndImports and thus in AugMakeIntUnit1
        % any items that (a) generate_private_interface_int0 below
        % will throw away, and (b) which don't help the module qualification
        % of the items that it keeps.
        module_qualify_aug_make_int_unit(Globals,
            AugMakeIntUnit1, AugMakeIntUnit, [], QualSpecs),
        filter_interface_generation_specs(Globals,
            GetSpecs ++ QualSpecs, EffectiveGetQualSpecs),
        (
            EffectiveGetQualSpecs = [],
            % Construct the `.int0' file.
            generate_private_interface_int0(AugMakeIntUnit, ParseTreeInt0,
                [], GenerateSpecs),
            filter_interface_generation_specs(Globals,
                EffectiveGetQualSpecs ++ GenerateSpecs, Specs),
            % Write out the `.int0' file.
            ExtraSuffix = "",
            construct_int_file_name(Globals, ModuleName, ifk_int0, ExtraSuffix,
                FileName, TmpFileName, !IO),
            actually_write_interface_file0(ProgressStream, ErrorStream,
                Globals, ParseTreeInt0, FileName, TmpFileName, MaybeTimestamp,
                OutputSucceeded, !IO),
            touch_module_ext_datestamp(Globals, ProgressStream,
                ModuleName, ext_cur_ngs(ext_cur_ngs_int_date_int0),
                TouchSucceeded, !IO),
            Succeeded = OutputSucceeded `and` TouchSucceeded,
            (
                AddToHrmm = do_not_add_new_to_hrmm
            ;
                AddToHrmm = do_add_new_to_hrmm,
                Int0Map0 = !.HaveReadModuleMaps ^ hrmm_int0,
                % XXX If needed for smart recompilation, we could get
                % the actual timestamp of the int0 file.
                MaybeTimestampInt = maybe.no,
                % XXX ReadModuleErrors
                ReadModuleErrors = read_module_errors(set.init, [],
                    set.init, [], []),
                HRM = have_read_module(FileName, MaybeTimestampInt,
                    ParseTreeInt0, ReadModuleErrors),
                map.set(ModuleName, HRM, Int0Map0, Int0Map),
                !HaveReadModuleMaps ^ hrmm_int0 := Int0Map
            )
        ;
            EffectiveGetQualSpecs = [_ | _],
            report_file_not_written(Globals, [], ModuleName,
                ext_cur_ngs(ext_cur_ngs_int_int0), no,
                ext_cur_ngs(ext_cur_ngs_int_date_int0),
                EffectiveGetQualSpecs, Specs, !IO),
            Succeeded = did_not_succeed
        )
    else
        % The negative indent is to let the rest of the error_spec
        % start at the left margin.
        PrefixPieces = [words("Error reading interface files."),
            nl_indent_delta(-1)],
        report_file_not_written(Globals, PrefixPieces, ModuleName,
            ext_cur_ngs(ext_cur_ngs_int_int0), no,
            ext_cur_ngs(ext_cur_ngs_int_date_int0), GetSpecs, Specs, !IO),
        Succeeded = did_not_succeed
    ).

write_private_interface_file_int0_burdened_module(ProgressStream, ErrorStream,
        Globals, AddToHrmm, BurdenedModule, Succeeded, Specs,
        !HaveReadModuleMaps, !IO) :-
    BurdenedModule = burdened_module(Baggage, ParseTreeModuleSrc),
    SourceFileName = Baggage ^ mb_source_file_name,
    SourceFileModuleName = Baggage ^ mb_source_file_module_name,
    MaybeTimestamp = Baggage ^ mb_maybe_timestamp,
    write_private_interface_file_int0(ProgressStream, ErrorStream, Globals,
        AddToHrmm, SourceFileName, SourceFileModuleName, MaybeTimestamp,
        ParseTreeModuleSrc, Succeeded, Specs, !HaveReadModuleMaps, !IO).

%---------------------------------------------------------------------------%
%
% Write out .int and .int2 files.
%

write_interface_file_int1_int2(ProgressStream, ErrorStream, Globals,
        AddToHrmm, SourceFileName, SourceFileModuleName, MaybeTimestamp,
        ParseTreeModuleSrc0, Succeeded, Specs, !HaveReadModuleMaps, !IO) :-
    ModuleName = ParseTreeModuleSrc0 ^ ptms_module_name,
    generate_pre_grab_pre_qual_interface_for_int1_int2(ParseTreeModuleSrc0,
        IntParseTreeModuleSrc),

    % Get the .int3 files for imported modules.
    grab_unqual_imported_modules_make_int(ProgressStream, Globals,
        SourceFileName, SourceFileModuleName, IntParseTreeModuleSrc,
        Baggage, AugMakeIntUnit1, !HaveReadModuleMaps, !IO),

    % Check whether we succeeded.
    GetErrors = Baggage ^ mb_errors,
    GetSpecs = get_read_module_specs(GetErrors),
    GetSpecsEffectivelyErrors =
        contains_errors_or_warnings_treated_as_errors(Globals, GetSpecs),
    ( if
        GetSpecsEffectivelyErrors = no,
        there_are_no_errors(GetErrors)
    then
        % Module-qualify the aug_make_int_unit.
        %
        % Note that doing this only if the condition above succeeds avoids
        % the generation of avalanche error messages, which is good,
        % but it also prevents us from generating useful, non-avalanche
        % error messages, e.g. in tests/invalid_make_int/test_nested.m,
        % we would be able to report that the fourth argument of predicate
        % "foo" refers to a nonexistent type.
        %
        % In the absence of a sure way to filter out all avalanche errors
        % from QualSpecs, we have to decide between generating some avalanche
        % error messages or foregoing the generation of some non-avalanche
        % error messages. This position of this call makes the latter choice.
        module_qualify_aug_make_int_unit(Globals,
            AugMakeIntUnit1, AugMakeIntUnit, [], QualSpecs),
        filter_interface_generation_specs(Globals,
            GetSpecs ++ QualSpecs, EffectiveGetQualSpecs),
        (
            EffectiveGetQualSpecs = [],
            % Construct the `.int' and `.int2' files.
            generate_interfaces_int1_int2(Globals, AugMakeIntUnit,
                ParseTreeInt1, ParseTreeInt2, [], GenerateSpecs),
            filter_interface_generation_specs(Globals,
                EffectiveGetQualSpecs ++ GenerateSpecs, Specs),
            % Write out the `.int' and `.int2' files.
            ExtraSuffix = "",
            construct_int_file_name(Globals, ModuleName, ifk_int1, ExtraSuffix,
                FileName1, TmpFileName1, !IO),
            construct_int_file_name(Globals, ModuleName, ifk_int2, ExtraSuffix,
                FileName2, TmpFileName2, !IO),
            actually_write_interface_file1(ProgressStream, ErrorStream,
                Globals, ParseTreeInt1, FileName1, TmpFileName1,
                MaybeTimestamp, OutputSucceeded1, !IO),
            actually_write_interface_file2(ProgressStream, ErrorStream,
                Globals, ParseTreeInt2, FileName2, TmpFileName2,
                MaybeTimestamp, OutputSucceeded2, !IO),
            touch_module_ext_datestamp(Globals, ProgressStream,
                ModuleName, ext_cur_ngs(ext_cur_ngs_int_date_int12),
                TouchSucceeded, !IO),
            Succeeded = OutputSucceeded1 `and` OutputSucceeded2 `and`
                TouchSucceeded,
            (
                AddToHrmm = do_not_add_new_to_hrmm
            ;
                AddToHrmm = do_add_new_to_hrmm,
                Int1Map0 = !.HaveReadModuleMaps ^ hrmm_int1,
                Int2Map0 = !.HaveReadModuleMaps ^ hrmm_int2,
                % XXX If needed for smart recompilation, we could get
                % the actual timestamps of the .int and .int2 files.
                MaybeTimestampInt = maybe.no,
                % XXX ReadModuleErrors
                ReadModuleErrors = read_module_errors(set.init, [],
                    set.init, [], []),
                HRM1 = have_read_module(FileName1, MaybeTimestampInt,
                    ParseTreeInt1, ReadModuleErrors),
                HRM2 = have_read_module(FileName2, MaybeTimestampInt,
                    ParseTreeInt2, ReadModuleErrors),
                map.set(ModuleName, HRM1, Int1Map0, Int1Map),
                map.set(ModuleName, HRM2, Int2Map0, Int2Map),
                !HaveReadModuleMaps ^ hrmm_int1 := Int1Map,
                !HaveReadModuleMaps ^ hrmm_int2 := Int2Map
            )
        ;
            EffectiveGetQualSpecs = [_ | _],
            report_file_not_written(Globals, [], ModuleName,
                ext_cur_ngs(ext_cur_ngs_int_int1),
                yes(ext_cur_ngs(ext_cur_ngs_int_int2)),
                ext_cur_ngs(ext_cur_ngs_int_date_int12),
                EffectiveGetQualSpecs, Specs, !IO),
            Succeeded = did_not_succeed
        )
    else
        % The negative indent is to let the rest of the error_spec
        % start at the left margin.
        PrefixPieces = [words("Error reading .int3 files."),
            nl_indent_delta(-1)],
        report_file_not_written(Globals, PrefixPieces, ModuleName,
            ext_cur_ngs(ext_cur_ngs_int_int1),
            yes(ext_cur_ngs(ext_cur_ngs_int_int2)),
            ext_cur_ngs(ext_cur_ngs_int_date_int12),
            GetSpecs, Specs, !IO),
        Succeeded = did_not_succeed
    ).

write_interface_file_int1_int2_burdened_module(ProgressStream, ErrorStream,
        Globals, AddToHrmm, BurdenedModule, Succeeded, Specs,
        !HaveReadModuleMaps, !IO) :-
    BurdenedModule = burdened_module(Baggage, ParseTreeModuleSrc),
    SourceFileName = Baggage ^ mb_source_file_name,
    SourceFileModuleName = Baggage ^ mb_source_file_module_name,
    MaybeTimestamp = Baggage ^ mb_maybe_timestamp,
    write_interface_file_int1_int2(ProgressStream, ErrorStream, Globals,
        AddToHrmm, SourceFileName, SourceFileModuleName, MaybeTimestamp,
        ParseTreeModuleSrc, Succeeded, Specs, !HaveReadModuleMaps, !IO).

%---------------------------------------------------------------------------%

:- pred actually_write_interface_file0(
    io.text_output_stream::in, io.text_output_stream::in, globals::in,
    parse_tree_int0::in, string::in, string::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file0(ProgressStream, ErrorStream, Globals,
        ParseTreeInt0, FileName, TmpFileName, MaybeTimestamp,
        Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    % We handle any failure to read in the old interface version as
    % every item in the module source being brand new.
    maybe_read_old_int0_and_compare_for_smart_recomp(ProgressStream,
        NoLineNumGlobals, ParseTreeInt0, MaybeTimestamp,
        MaybeVersionNumbers, !IO),
    ParseTreeInt0V = ParseTreeInt0 ^ pti0_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int0(ProgressStream, ErrorStream, NoLineNumGlobals,
        TmpFileName, ParseTreeInt0V, OutputSucceeded, !IO),
    copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
        ".int0", FileName, UpdateSucceeded, !IO),
    Succeeded = OutputSucceeded `and` UpdateSucceeded.

:- pred actually_write_interface_file1(
    io.text_output_stream::in, io.text_output_stream::in, globals::in,
    parse_tree_int1::in, string::in, string::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file1(ProgressStream, ErrorStream, Globals,
        ParseTreeInt1, FileName, TmpFileName, MaybeTimestamp,
        Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    % We handle any failure to read in the old interface version as
    % every item in the module source being brand new.
    maybe_read_old_int1_and_compare_for_smart_recomp(ProgressStream,
        NoLineNumGlobals, ParseTreeInt1, MaybeTimestamp,
        MaybeVersionNumbers, !IO),
    ParseTreeInt1V = ParseTreeInt1 ^ pti1_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int1(ProgressStream, ErrorStream, NoLineNumGlobals,
        TmpFileName, ParseTreeInt1V, OutputSucceeded, !IO),
    copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
        ".int", FileName, UpdateSucceeded, !IO),
    Succeeded = OutputSucceeded `and` UpdateSucceeded.

:- pred actually_write_interface_file2(
    io.text_output_stream::in, io.text_output_stream::in, globals::in,
    parse_tree_int2::in, string::in, string::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file2(ProgressStream, ErrorStream, Globals,
        ParseTreeInt2, FileName, TmpFileName, MaybeTimestamp,
        Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    maybe_read_old_int2_and_compare_for_smart_recomp(ProgressStream,
        NoLineNumGlobals, ParseTreeInt2, MaybeTimestamp,
        MaybeVersionNumbers, !IO),
    ParseTreeInt2V = ParseTreeInt2 ^ pti2_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int2(ProgressStream, ErrorStream, NoLineNumGlobals,
        TmpFileName, ParseTreeInt2V, OutputSucceeded, !IO),
    copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
        ".int2", FileName, UpdateSucceeded, !IO),
    Succeeded = OutputSucceeded `and` UpdateSucceeded.

:- pred actually_write_interface_file3(
    io.text_output_stream::in, io.text_output_stream::in, globals::in,
    parse_tree_int3::in, string::in, string::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file3(ProgressStream, ErrorStream, Globals,
        ParseTreeInt3, FileName, TmpFileName, _MaybeTimestamp,
        Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    output_parse_tree_int3(ProgressStream, ErrorStream, NoLineNumGlobals,
        TmpFileName, ParseTreeInt3, OutputSucceeded, !IO),
    copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
        ".int3", FileName, UpdateSucceeded, !IO),
    Succeeded = OutputSucceeded `and` UpdateSucceeded.

%---------------------------------------------------------------------------%

:- pred construct_int_file_name(globals::in,
    module_name::in, int_file_kind::in, string::in,
    string::out, string::out, io::di, io::uo) is det.

construct_int_file_name(Globals, ModuleName, IntFileKind, ExtraSuffix,
        OutputFileName, TmpOutputFileName, !IO) :-
    int_file_kind_to_extension(IntFileKind, _ExtStr, Ext),
    module_name_to_file_name_create_dirs(Globals, $pred, Ext,
        ModuleName, OutputFileName0, !IO),
    OutputFileName = OutputFileName0 ++ ExtraSuffix,
    TmpOutputFileName = OutputFileName ++ ".tmp".

:- pred disable_all_line_numbers(globals::in, globals::out) is det.

disable_all_line_numbers(Globals, NoLineNumGlobals) :-
    globals.set_option(line_numbers, bool(no),
        Globals, NoLineNumGlobals0),
    globals.set_option(line_numbers_around_foreign_code, bool(no),
        NoLineNumGlobals0, NoLineNumGlobals).

%---------------------------------------------------------------------------%

:- pred maybe_read_old_int0_and_compare_for_smart_recomp(
    io.text_output_stream::in, globals::in, parse_tree_int0::in,
    maybe(timestamp)::in, maybe_version_numbers::out, io::di, io::uo) is det.

maybe_read_old_int0_and_compare_for_smart_recomp(ProgressStream,
        NoLineNumGlobals, ParseTreeInt0, MaybeTimestamp,
        MaybeVersionNumbers, !IO) :-
    should_generate_item_version_numbers(NoLineNumGlobals,
        WantVersionNumbers, !IO),
    (
        WantVersionNumbers = generate_version_numbers,
        ModuleName = ParseTreeInt0 ^ pti0_module_name,
        % Find the timestamp of the current module.
        insist_on_timestamp(MaybeTimestamp, Timestamp),
        % Read in the previous version of the file.
        read_module_int0(ProgressStream, NoLineNumGlobals,
            rrm_old, ignore_errors, do_search, ModuleName,
            always_read_module(dont_return_timestamp), HaveReadInt0, !IO),
        (
            HaveReadInt0 = have_read_module(_FN, _MTS,
                OldParseTreeInt0, OldModuleErrors),
            ( if there_are_no_errors(OldModuleErrors) then
                MaybeOldParseTreeInt0 = yes(OldParseTreeInt0)
            else
                % If we can't read in the old file, the timestamps will
                % all be set to the modification time of the source file.
                MaybeOldParseTreeInt0 = no
            )
        ;
            HaveReadInt0 = have_not_read_module(_, _),
            MaybeOldParseTreeInt0 = no
        ),
        recompilation.version.compute_version_numbers_int0(
            MaybeOldParseTreeInt0, Timestamp, ParseTreeInt0, VersionNumbers),
        MaybeVersionNumbers = version_numbers(VersionNumbers)
    ;
        WantVersionNumbers = do_not_generate_version_numbers,
        MaybeVersionNumbers = no_version_numbers
    ).

:- pred maybe_read_old_int1_and_compare_for_smart_recomp(
    io.text_output_stream::in, globals::in, parse_tree_int1::in,
    maybe(timestamp)::in, maybe_version_numbers::out, io::di, io::uo) is det.

maybe_read_old_int1_and_compare_for_smart_recomp(ProgressStream,
        NoLineNumGlobals, ParseTreeInt1, MaybeTimestamp,
        MaybeVersionNumbers, !IO) :-
    should_generate_item_version_numbers(NoLineNumGlobals,
        WantVersionNumbers, !IO),
    (
        WantVersionNumbers = generate_version_numbers,
        ModuleName = ParseTreeInt1 ^ pti1_module_name,
        % Find the timestamp of the current module.
        insist_on_timestamp(MaybeTimestamp, Timestamp),
        % Read in the previous version of the file.
        read_module_int1(ProgressStream, NoLineNumGlobals,
            rrm_old, ignore_errors, do_search, ModuleName,
            always_read_module(dont_return_timestamp), HaveReadInt1, !IO),
        (
            HaveReadInt1 = have_read_module(_FN, _MTS,
                OldParseTreeInt1, OldModuleErrors),
            ( if there_are_no_errors(OldModuleErrors) then
                MaybeOldParseTreeInt1 = yes(OldParseTreeInt1)
            else
                % If we can't read in the old file, the timestamps will
                % all be set to the modification time of the source file.
                MaybeOldParseTreeInt1 = no
            )
        ;
            HaveReadInt1 = have_not_read_module(_, _),
            MaybeOldParseTreeInt1 = no
        ),
        recompilation.version.compute_version_numbers_int1(
            MaybeOldParseTreeInt1, Timestamp, ParseTreeInt1, VersionNumbers),
        MaybeVersionNumbers = version_numbers(VersionNumbers)
    ;
        WantVersionNumbers = do_not_generate_version_numbers,
        MaybeVersionNumbers = no_version_numbers
    ).

:- pred maybe_read_old_int2_and_compare_for_smart_recomp(
    io.text_output_stream::in, globals::in, parse_tree_int2::in,
    maybe(timestamp)::in, maybe_version_numbers::out, io::di, io::uo) is det.

maybe_read_old_int2_and_compare_for_smart_recomp(ProgressStream,
        NoLineNumGlobals, ParseTreeInt2, MaybeTimestamp,
        MaybeVersionNumbers, !IO) :-
    should_generate_item_version_numbers(NoLineNumGlobals,
        WantVersionNumbers, !IO),
    (
        WantVersionNumbers = generate_version_numbers,
        ModuleName = ParseTreeInt2 ^ pti2_module_name,
        % Find the timestamp of the current module.
        insist_on_timestamp(MaybeTimestamp, Timestamp),
        % Read in the previous version of the file.
        read_module_int2(ProgressStream, NoLineNumGlobals,
            rrm_old, ignore_errors, do_search, ModuleName,
            always_read_module(dont_return_timestamp), HaveReadInt2, !IO),
        (
            HaveReadInt2 = have_read_module(_FN, _MTS,
                OldParseTreeInt2, OldModuleErrors),
            ( if there_are_no_errors(OldModuleErrors) then
                MaybeOldParseTreeInt2 = yes(OldParseTreeInt2)
            else
                % If we can't read in the old file, the timestamps will
                % all be set to the modification time of the source file.
                MaybeOldParseTreeInt2 = no
            )
        ;
            HaveReadInt2 = have_not_read_module(_, _),
            MaybeOldParseTreeInt2 = no
        ),
        recompilation.version.compute_version_numbers_int2(
            MaybeOldParseTreeInt2, Timestamp, ParseTreeInt2, VersionNumbers),
        MaybeVersionNumbers = version_numbers(VersionNumbers)
    ;
        WantVersionNumbers = do_not_generate_version_numbers,
        MaybeVersionNumbers = no_version_numbers
    ).

%---------------------------------------------------------------------------%

:- type maybe_generate_version_numbers
    --->    do_not_generate_version_numbers
    ;       generate_version_numbers.

:- pred should_generate_item_version_numbers(globals::in,
    maybe_generate_version_numbers::out, io::di, io::uo) is det.

should_generate_item_version_numbers(Globals, ShouldGenVersionNumbers, !IO) :-
    globals.lookup_bool_option(Globals, generate_item_version_numbers,
        GenerateVersionNumbers),
    io_get_disable_generate_item_version_numbers(DisableVersionNumbers, !IO),
    ( if
        GenerateVersionNumbers = yes,
        DisableVersionNumbers = do_not_disable_item_version_numbers
    then
        ShouldGenVersionNumbers = generate_version_numbers
    else
        ShouldGenVersionNumbers = do_not_generate_version_numbers
    ).

:- pred insist_on_timestamp(maybe(timestamp)::in, timestamp::out) is det.

insist_on_timestamp(MaybeTimestamp, Timestamp) :-
    % Find the timestamp of the current module.
    (
        MaybeTimestamp = no,
        unexpected($pred, "timestamp not read with `--smart-recompilation'")
    ;
        MaybeTimestamp = yes(Timestamp)
    ).

%---------------------------------------------------------------------------%

:- pred report_file_not_written(globals::in, list(format_piece)::in,
    module_name::in, ext::in, maybe(ext)::in, ext::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

report_file_not_written(Globals, PrefixPieces, ModuleName,
        ExtA, MaybeExtB, ExtDate, Specs0, Specs, !IO) :-
    % We use write_error_spec to print the message the interface file or
    % files not being written in order to wrap the message if it is
    % longer than the line length.
    module_name_to_file_name(Globals, $pred,
        ExtA, ModuleName, IntAFileName),
    module_name_to_file_name(Globals, $pred,
        ExtDate, ModuleName, DateFileName),
    (
        MaybeExtB = no,
        NotWrittenPieces = [quote(IntAFileName), words("not written."), nl],
        ToRemoveFileNames = [IntAFileName, DateFileName]
    ;
        MaybeExtB = yes(ExtB),
        module_name_to_file_name(Globals, $pred,
            ExtB, ModuleName, IntBFileName),
        NotWrittenPieces = [quote(IntAFileName), words("and"),
            quote(IntBFileName), words("not written."), nl],
        ToRemoveFileNames = [IntAFileName, IntBFileName, DateFileName]
    ),
    ModuleNameStr = sym_name_to_string(ModuleName),
    InvisPiece = invis_order_default_end(0, ModuleNameStr),
    NotWrittenSpec = simplest_no_context_spec($pred, severity_informational,
        phase_make_int, [InvisPiece | PrefixPieces] ++ NotWrittenPieces),
    Specs = [NotWrittenSpec | Specs0],
    % We remove the interface file(s) the errors prevented us from generating,
    % as well as the file indicating when they were last successfully written,
    % for the same reason: to prevent any previous versions of those files
    % being used in other compilations despite the fact that they are now
    % out-of-date. If we did not do this, compilations that read in the
    % now-obsolete interface files could generate error messages about
    % errors that do not now exist in the source files at all.
    list.map_foldl(io.file.remove_file,
        ToRemoveFileNames, _RemoveResults, !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_module_interface_files.
%---------------------------------------------------------------------------%
