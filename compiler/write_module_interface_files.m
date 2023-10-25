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
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module parse_tree.comp_unit_interface.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.module_baggage.
:- import_module parse_tree.read_modules.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % write_short_interface_file_int3(ProgressStream, Globals, AddToHptm,
    %   BurdenedModule, Succeeded, Specs, !HaveParseTreeMaps, !IO):
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
    % We add the contents of the .int3 file to !HaveParseTreeMaps if we could
    % construct it without any errors, *and* AddToHptm says we should.
    %
:- pred generate_and_write_interface_file_int3(io.text_output_stream::in,
    globals::in, maybe_add_to_hptm::in, burdened_module::in,
    maybe_succeeded::out, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

    % write_private_interface_file_int0(ProgressStream, Globals, AddToHptm,
    %   BurdenedModule, Succeeded, Specs, !HaveParseTreeMaps, !IO):
    %
    % Given a burdened_module, output the private (`.int0') interface file
    % for the module. (The private interface contains all the declarations
    % in the module, including those in the `implementation' section;
    % it is used when compiling submodules.)
    %
:- pred generate_and_write_interface_file_int0(io.text_output_stream::in,
    globals::in, maybe_add_to_hptm::in, burdened_module::in,
    maybe_succeeded::out, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

    % write_interface_file_int1_int2(ProgressStream, Globals, AddToHptm,
    %   BurdenedModule, Succeeded, Specs, !HaveParseTreeMaps, !IO):
    %
    % Given a burdened_module, output the long (`.int') and short (`.int2')
    % interface files for the module.
    %
:- pred generate_and_write_interface_file_int1_int2(io.text_output_stream::in,
    globals::in, maybe_add_to_hptm::in, burdened_module::in,
    maybe_succeeded::out, list(error_spec)::out,
    have_parse_tree_maps::in, have_parse_tree_maps::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.prog_item.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module bool.
:- import_module dir.
:- import_module getopt.
:- import_module io.file.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%
%
% Write out .int3 files.
%

generate_and_write_interface_file_int3(ProgressStream, Globals, AddToHptm,
        BurdenedModule, Succeeded, Specs, !HaveParseTreeMaps, !IO) :-
    generate_parse_tree_int3(Globals, AddToHptm,
        BurdenedModule, GenerateResult, !HaveParseTreeMaps, !IO),
    write_parse_tree_int3(ProgressStream, Globals, GenerateResult,
        Specs, Succeeded, !IO).

:- pred write_parse_tree_int3(io.text_output_stream::in, globals::in,
    generate_int3_result::in, list(error_spec)::out, maybe_succeeded::out,
    io::di, io::uo) is det.

write_parse_tree_int3(ProgressStream, Globals, GenerateResult,
        Specs, Succeeded, !IO) :-
    (
        GenerateResult = gpti3_ok(ParseTreeInt3, FileName, TmpFileName, Specs),
        ModuleName = ParseTreeInt3 ^ pti3_module_name,
        actually_write_interface_file3(ProgressStream, Globals, ParseTreeInt3,
            FileName, TmpFileName, no, OutputSucceeded, !IO),
        touch_module_ext_datestamp(Globals, ProgressStream, ModuleName,
            ext_cur_ngs(ext_cur_ngs_int_date_int3), TouchSucceeded, !IO),
        Succeeded = OutputSucceeded `and` TouchSucceeded
    ;
        GenerateResult = gpti3_error(ModuleName, PrefixPieces, GenerateSpecs),
        ExtInt3 = ext_cur_ngs(ext_cur_ngs_int_int3),
        ExtDate3 = ext_cur_ngs(ext_cur_ngs_int_date_int3),
        report_file_not_written(Globals, PrefixPieces, ModuleName,
            ExtInt3, no, ExtDate3, GenerateSpecs, Specs, !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
%
% Write out .int0 files.
%

generate_and_write_interface_file_int0(ProgressStream, Globals, AddToHptm,
        BurdenedModule, Succeeded, Specs, !HaveParseTreeMaps, !IO) :-
    generate_parse_tree_int0(ProgressStream, Globals, AddToHptm,
        BurdenedModule, GenerateResult, !HaveParseTreeMaps, !IO),
    write_parse_tree_int0(ProgressStream, Globals, GenerateResult,
        Specs, Succeeded, !IO).

:- pred write_parse_tree_int0(io.text_output_stream::in, globals::in,
    generate_int0_result::in, list(error_spec)::out, maybe_succeeded::out,
    io::di, io::uo) is det.

write_parse_tree_int0(ProgressStream, Globals, GenerateResult,
        Specs, Succeeded, !IO) :-
    (
        GenerateResult = gpti0_ok(ParseTreeInt0, MaybeTimestamp,
            FileName, TmpFileName, Specs),
        ModuleName = ParseTreeInt0 ^ pti0_module_name,
        actually_write_interface_file0(ProgressStream, Globals, ParseTreeInt0,
            FileName, TmpFileName, MaybeTimestamp, OutputSucceeded, !IO),
        touch_module_ext_datestamp(Globals, ProgressStream, ModuleName,
            ext_cur_ngs(ext_cur_ngs_int_date_int0), TouchSucceeded, !IO),
        Succeeded = OutputSucceeded `and` TouchSucceeded
    ;
        GenerateResult = gpti0_error(ModuleName, PrefixPieces, GenerateSpecs),
        ExtInt0 = ext_cur_ngs(ext_cur_ngs_int_int0),
        ExtDate0 = ext_cur_ngs(ext_cur_ngs_int_date_int0),
        report_file_not_written(Globals, PrefixPieces, ModuleName,
            ExtInt0, no, ExtDate0, GenerateSpecs, Specs, !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%
%
% Write out .int and .int2 files.
%

generate_and_write_interface_file_int1_int2(ProgressStream, Globals, AddToHptm,
        BurdenedModule, Succeeded, Specs, !HaveParseTreeMaps, !IO) :-
    generate_parse_tree_int12(ProgressStream, Globals, AddToHptm,
        BurdenedModule, GenerateResult, !HaveParseTreeMaps, !IO),
    write_parse_tree_int12(ProgressStream, Globals, GenerateResult,
        Specs, Succeeded, !IO).

:- pred write_parse_tree_int12(io.text_output_stream::in, globals::in,
    generate_int12_result::in, list(error_spec)::out, maybe_succeeded::out,
    io::di, io::uo) is det.

write_parse_tree_int12(ProgressStream, Globals, GenerateResult,
        Specs, Succeeded, !IO) :-
    (
        GenerateResult = gpti12_ok(ParseTreeInt1, ParseTreeInt2,
            MaybeTimestamp, FileName1, TmpFileName1, FileName2, TmpFileName2,
            Specs),
        ModuleName = ParseTreeInt1 ^ pti1_module_name,
        actually_write_interface_file1(ProgressStream, Globals,
            ParseTreeInt1, FileName1, TmpFileName1, MaybeTimestamp,
            OutputSucceeded1, !IO),
        actually_write_interface_file2(ProgressStream, Globals,
            ParseTreeInt2, FileName2, TmpFileName2, MaybeTimestamp,
            OutputSucceeded2, !IO),
        touch_module_ext_datestamp(Globals, ProgressStream, ModuleName,
            ext_cur_ngs(ext_cur_ngs_int_date_int12), TouchSucceeded, !IO),
        Succeeded = OutputSucceeded1 `and` OutputSucceeded2 `and`
            TouchSucceeded
    ;
        GenerateResult = gpti12_error(ModuleName, PrefixPieces, GenerateSpecs),
        ExtInt1 = ext_cur_ngs(ext_cur_ngs_int_int1),
        ExtInt2 = ext_cur_ngs(ext_cur_ngs_int_int2),
        ExtDate12 = ext_cur_ngs(ext_cur_ngs_int_date_int12),
        report_file_not_written(Globals, PrefixPieces, ModuleName,
            ExtInt1, yes(ExtInt2), ExtDate12, GenerateSpecs, Specs, !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

:- pred actually_write_interface_file0(io.text_output_stream::in, globals::in,
    parse_tree_int0::in, string::in, string::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file0(ProgressStream, Globals, ParseTreeInt0,
        FileName, TmpFileName, MaybeTimestamp, Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    % We handle any failure to read in the old interface version as
    % every item in the module source being brand new.
    maybe_read_old_int0_and_compare_for_smart_recomp(ProgressStream,
        NoLineNumGlobals, ParseTreeInt0, MaybeTimestamp,
        MaybeVersionNumbers, !IO),
    ParseTreeInt0V = ParseTreeInt0 ^ pti0_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int0(ProgressStream, NoLineNumGlobals,
        TmpFileName, ParseTreeInt0V, OutputSucceeded, !IO),
    copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
        ".int0", FileName, UpdateSucceeded, !IO),
    Succeeded = OutputSucceeded `and` UpdateSucceeded.

:- pred actually_write_interface_file1(io.text_output_stream::in, globals::in,
    parse_tree_int1::in, string::in, string::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file1(ProgressStream, Globals, ParseTreeInt1,
        FileName, TmpFileName, MaybeTimestamp, Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    % We handle any failure to read in the old interface version as
    % every item in the module source being brand new.
    maybe_read_old_int1_and_compare_for_smart_recomp(ProgressStream,
        NoLineNumGlobals, ParseTreeInt1, MaybeTimestamp,
        MaybeVersionNumbers, !IO),
    ParseTreeInt1V = ParseTreeInt1 ^ pti1_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int1(ProgressStream, NoLineNumGlobals,
        TmpFileName, ParseTreeInt1V, OutputSucceeded, !IO),
    copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
        ".int", FileName, UpdateSucceeded, !IO),
    Succeeded = OutputSucceeded `and` UpdateSucceeded.

:- pred actually_write_interface_file2(io.text_output_stream::in, globals::in,
    parse_tree_int2::in, string::in, string::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file2(ProgressStream, Globals, ParseTreeInt2,
        FileName, TmpFileName, MaybeTimestamp, Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    maybe_read_old_int2_and_compare_for_smart_recomp(ProgressStream,
        NoLineNumGlobals, ParseTreeInt2, MaybeTimestamp,
        MaybeVersionNumbers, !IO),
    ParseTreeInt2V = ParseTreeInt2 ^ pti2_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int2(ProgressStream, NoLineNumGlobals,
        TmpFileName, ParseTreeInt2V, OutputSucceeded, !IO),
    copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
        ".int2", FileName, UpdateSucceeded, !IO),
    Succeeded = OutputSucceeded `and` UpdateSucceeded.

:- pred actually_write_interface_file3(io.text_output_stream::in, globals::in,
    parse_tree_int3::in, string::in, string::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file3(ProgressStream, Globals, ParseTreeInt3,
        FileName, TmpFileName, _MaybeTimestamp, Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    output_parse_tree_int3(ProgressStream, NoLineNumGlobals,
        TmpFileName, ParseTreeInt3, OutputSucceeded, !IO),
    copy_dot_tmp_to_base_file_report_any_error(ProgressStream, Globals,
        ".int3", FileName, UpdateSucceeded, !IO),
    Succeeded = OutputSucceeded `and` UpdateSucceeded.

%---------------------------------------------------------------------------%

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
            HaveReadInt0 = have_module(_FN, OldParseTreeInt0, Source),
            have_parse_tree_source_get_maybe_timestamp_errors(Source,
                _, OldModuleErrors),
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
            HaveReadInt1 = have_module(_FN, OldParseTreeInt1, Source),
            have_parse_tree_source_get_maybe_timestamp_errors(Source,
                _, OldModuleErrors),
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
            HaveReadInt2 = have_module(_FN, OldParseTreeInt2, Source),
            have_parse_tree_source_get_maybe_timestamp_errors(Source,
                _, OldModuleErrors),
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
    StdIntAFileName = standardize_filename_if_asked(Globals, IntAFileName),
    (
        MaybeExtB = no,
        NotWrittenPieces = [quote(StdIntAFileName), words("not written."), nl],
        ToRemoveFileNames = [IntAFileName, DateFileName]
    ;
        MaybeExtB = yes(ExtB),
        module_name_to_file_name(Globals, $pred,
            ExtB, ModuleName, IntBFileName),
        StdIntBFileName = standardize_filename_if_asked(Globals, IntBFileName),
        NotWrittenPieces = [quote(StdIntAFileName), words("and"),
            quote(StdIntBFileName), words("not written."), nl],
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

:- func standardize_filename_if_asked(globals, string) = string.

standardize_filename_if_asked(Globals, FileName) = StdFileName :-
    globals.lookup_bool_option(Globals, std_int_file_not_written_msgs, Std),
    (
        Std = no,
        StdFileName = FileName
    ;
        Std = yes,
        ( if dir.basename(FileName, BaseName) then
            StdFileName = BaseName
        else
            StdFileName = FileName
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_module_interface_files.
%---------------------------------------------------------------------------%
