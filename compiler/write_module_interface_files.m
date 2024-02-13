%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2024 The Mercury team.
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

:- import_module libs.file_util.
:- import_module libs.options.
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.parse_error.
:- import_module parse_tree.parse_module.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_item.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module bool.
:- import_module dir.
:- import_module getopt.
:- import_module io.file.
:- import_module maybe.
:- import_module require.
:- import_module string.

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
        GenerateResult = gpti3_ok(ParseTreeInt3, FileName, Specs),
        ModuleName = ParseTreeInt3 ^ pti3_module_name,
        actually_write_interface_file3(ProgressStream, Globals, ParseTreeInt3,
            FileName, OutputSucceeded, !IO),
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
            FileName, Specs),
        ModuleName = ParseTreeInt0 ^ pti0_module_name,
        actually_write_interface_file0(ProgressStream, Globals, ParseTreeInt0,
            FileName, MaybeTimestamp, OutputSucceeded, !IO),
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
            MaybeSourceFileTimestamp, FileName1, FileName2, Specs),
        ModuleName = ParseTreeInt1 ^ pti1_module_name,
        actually_write_interface_file1(ProgressStream, Globals,
            ParseTreeInt1, FileName1, MaybeSourceFileTimestamp,
            OutputSucceeded1, !IO),
        actually_write_interface_file2(ProgressStream, Globals,
            ParseTreeInt2, FileName2, MaybeSourceFileTimestamp,
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
%
% The following three predicates operate on parse_tree_int0s, parse_tree_int1s
% and parse_tree_int2s. They do exactly the same thing with all three,
% so their codes are identical, except for the fact that
%
% - they operate on values of different types, and
% - they call predicates that perform the same operations on those different
%   types.
%
% The fourth operates on parse_tree_int3s, and is also *almost* identical
% in the above sense, except that it has one additional difference, which is
% that it does not deal with version numbers for smart recompilation, since
% .int3 files do not contain version numbers.
%
% The difference in types is why this common code cannot be factored out.
%

:- pred actually_write_interface_file0(io.text_output_stream::in, globals::in,
    parse_tree_int0::in, file_name::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file0(ProgressStream, Globals, GenParseTreeInt0,
        FileName, MaybeSourceFileTimestamp, Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    Info = init_merc_out_info(NoLineNumGlobals, unqualified_item_names,
        output_mercury),
    should_generate_item_version_numbers(NoLineNumGlobals,
        WantVersionNumbers, !IO),
    globals.lookup_bool_option(NoLineNumGlobals, verbose, Verbose),
    string.format("%% Reading old version of `%s'... ",
        [s(FileName)], ReadOldStartMsg),
    maybe_write_string(ProgressStream, Verbose, ReadOldStartMsg, !IO),
    io.read_named_file_as_string(FileName, ReadFileResult, !IO),
    (
        ReadFileResult = ok(OldFileStr),
        maybe_parse_old_int0_and_compare_for_smart_recomp(NoLineNumGlobals,
            WantVersionNumbers, FileName, yes(OldFileStr),
            MaybeSourceFileTimestamp, GenParseTreeInt0, NewParseTreeInt0),
        maybe_write_string(ProgressStream, Verbose, "done.\n", !IO),
        NewParseTreeStr = parse_tree_int0_to_string(Info, NewParseTreeInt0),
        ( if OldFileStr = NewParseTreeStr then
            string.format("%% `%s' has not changed.\n",
                [s(FileName)], NoChangeMsg),
            maybe_write_string(ProgressStream, Verbose, NoChangeMsg, !IO),
            Succeeded = succeeded
        else
            % This prints a progress message if --verbose is enabled.
            output_parse_tree_string(ProgressStream, NoLineNumGlobals,
                FileName, NewParseTreeStr, Succeeded, !IO)
        )
    ;
        ReadFileResult = error(_),
        maybe_write_string(ProgressStream, Verbose, "unsuccessful.\n", !IO),
        MaybeOldFileStr = maybe.no,
        maybe_parse_old_int0_and_compare_for_smart_recomp(NoLineNumGlobals,
            WantVersionNumbers, FileName, MaybeOldFileStr,
            MaybeSourceFileTimestamp, GenParseTreeInt0, NewParseTreeInt0),
        NewParseTreeStr = parse_tree_int0_to_string(Info, NewParseTreeInt0),
        % This prints a progress message if --verbose is enabled.
        output_parse_tree_string(ProgressStream, NoLineNumGlobals,
            FileName, NewParseTreeStr, Succeeded, !IO)
    ).

:- pred actually_write_interface_file1(io.text_output_stream::in, globals::in,
    parse_tree_int1::in, file_name::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file1(ProgressStream, Globals, GenParseTreeInt1,
        FileName, MaybeSourceFileTimestamp, Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    Info = init_merc_out_info(NoLineNumGlobals, unqualified_item_names,
        output_mercury),
    should_generate_item_version_numbers(NoLineNumGlobals,
        WantVersionNumbers, !IO),
    globals.lookup_bool_option(NoLineNumGlobals, verbose, Verbose),
    string.format("%% Reading old version of `%s'... ",
        [s(FileName)], ReadOldStartMsg),
    maybe_write_string(ProgressStream, Verbose, ReadOldStartMsg, !IO),
    io.read_named_file_as_string(FileName, ReadFileResult, !IO),
    (
        ReadFileResult = ok(OldFileStr),
        maybe_parse_old_int1_and_compare_for_smart_recomp(NoLineNumGlobals,
            WantVersionNumbers, FileName, yes(OldFileStr),
            MaybeSourceFileTimestamp, GenParseTreeInt1, NewParseTreeInt1),
        maybe_write_string(ProgressStream, Verbose, "done.\n", !IO),
        NewParseTreeStr = parse_tree_int1_to_string(Info, NewParseTreeInt1),
        ( if OldFileStr = NewParseTreeStr then
            string.format("%% `%s' has not changed.\n",
                [s(FileName)], NoChangeMsg),
            maybe_write_string(ProgressStream, Verbose, NoChangeMsg, !IO),
            Succeeded = succeeded
        else
            % This prints a progress message if --verbose is enabled.
            output_parse_tree_string(ProgressStream, NoLineNumGlobals,
                FileName, NewParseTreeStr, Succeeded, !IO)
        )
    ;
        ReadFileResult = error(_),
        maybe_write_string(ProgressStream, Verbose, "unsuccessful.\n", !IO),
        MaybeOldFileStr = maybe.no,
        maybe_parse_old_int1_and_compare_for_smart_recomp(NoLineNumGlobals,
            WantVersionNumbers, FileName, MaybeOldFileStr,
            MaybeSourceFileTimestamp, GenParseTreeInt1, NewParseTreeInt1),
        NewParseTreeStr = parse_tree_int1_to_string(Info, NewParseTreeInt1),
        % This prints a progress message if --verbose is enabled.
        output_parse_tree_string(ProgressStream, NoLineNumGlobals,
            FileName, NewParseTreeStr, Succeeded, !IO)
    ).

:- pred actually_write_interface_file2(io.text_output_stream::in, globals::in,
    parse_tree_int2::in, file_name::in, maybe(timestamp)::in,
    maybe_succeeded::out, io::di, io::uo) is det.

actually_write_interface_file2(ProgressStream, Globals, GenParseTreeInt2,
        FileName, MaybeSourceFileTimestamp, Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    Info = init_merc_out_info(NoLineNumGlobals, unqualified_item_names,
        output_mercury),
    should_generate_item_version_numbers(NoLineNumGlobals,
        WantVersionNumbers, !IO),
    globals.lookup_bool_option(NoLineNumGlobals, verbose, Verbose),
    string.format("%% Reading old version of `%s'... ",
        [s(FileName)], ReadOldStartMsg),
    maybe_write_string(ProgressStream, Verbose, ReadOldStartMsg, !IO),
    io.read_named_file_as_string(FileName, ReadFileResult, !IO),
    (
        ReadFileResult = ok(OldFileStr),
        maybe_parse_old_int2_and_compare_for_smart_recomp(NoLineNumGlobals,
            WantVersionNumbers, FileName, yes(OldFileStr),
            MaybeSourceFileTimestamp, GenParseTreeInt2, NewParseTreeInt2),
        maybe_write_string(ProgressStream, Verbose, "done.\n", !IO),
        NewParseTreeStr = parse_tree_int2_to_string(Info, NewParseTreeInt2),
        ( if OldFileStr = NewParseTreeStr then
            string.format("%% `%s' has not changed.\n",
                [s(FileName)], NoChangeMsg),
            maybe_write_string(ProgressStream, Verbose, NoChangeMsg, !IO),
            Succeeded = succeeded
        else
            % This prints a progress message if --verbose is enabled.
            output_parse_tree_string(ProgressStream, NoLineNumGlobals,
                FileName, NewParseTreeStr, Succeeded, !IO)
        )
    ;
        ReadFileResult = error(_),
        maybe_write_string(ProgressStream, Verbose, "unsuccessful.\n", !IO),
        MaybeOldFileStr = maybe.no,
        maybe_parse_old_int2_and_compare_for_smart_recomp(NoLineNumGlobals,
            WantVersionNumbers, FileName, MaybeOldFileStr,
            MaybeSourceFileTimestamp, GenParseTreeInt2, NewParseTreeInt2),
        NewParseTreeStr = parse_tree_int2_to_string(Info, NewParseTreeInt2),
        % This prints a progress message if --verbose is enabled.
        output_parse_tree_string(ProgressStream, NoLineNumGlobals,
            FileName, NewParseTreeStr, Succeeded, !IO)
    ).

:- pred actually_write_interface_file3(io.text_output_stream::in, globals::in,
    parse_tree_int3::in, file_name::in, maybe_succeeded::out,
    io::di, io::uo) is det.

actually_write_interface_file3(ProgressStream, Globals, NewParseTreeInt3,
        FileName, Succeeded, !IO) :-
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    Info = init_merc_out_info(NoLineNumGlobals, unqualified_item_names,
        output_mercury),
    globals.lookup_bool_option(NoLineNumGlobals, verbose, Verbose),
    string.format("%% Reading old version of `%s'... ",
        [s(FileName)], ReadOldStartMsg),
    maybe_write_string(ProgressStream, Verbose, ReadOldStartMsg, !IO),
    io.read_named_file_as_string(FileName, ReadFileResult, !IO),
    (
        ReadFileResult = ok(OldFileStr),
        maybe_write_string(ProgressStream, Verbose, "done.\n", !IO),
        NewParseTreeStr = parse_tree_int3_to_string(Info, NewParseTreeInt3),
        ( if OldFileStr = NewParseTreeStr then
            string.format("%% `%s' has not changed.\n",
                [s(FileName)], NoChangeMsg),
            maybe_write_string(ProgressStream, Verbose, NoChangeMsg, !IO),
            Succeeded = succeeded
        else
            % This prints a progress message if --verbose is enabled.
            output_parse_tree_string(ProgressStream, NoLineNumGlobals,
                FileName, NewParseTreeStr, Succeeded, !IO)
        )
    ;
        ReadFileResult = error(_),
        maybe_write_string(ProgressStream, Verbose, "unsuccessful.\n", !IO),
        NewParseTreeStr = parse_tree_int3_to_string(Info, NewParseTreeInt3),
        % This prints a progress message if --verbose is enabled.
        output_parse_tree_string(ProgressStream, NoLineNumGlobals,
            FileName, NewParseTreeStr, Succeeded, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred disable_all_line_numbers(globals::in, globals::out) is det.

disable_all_line_numbers(Globals, NoLineNumGlobals) :-
    globals.set_option(line_numbers, bool(no),
        Globals, NoLineNumGlobals0),
    globals.set_option(line_numbers_around_foreign_code, bool(no),
        NoLineNumGlobals0, NoLineNumGlobals).

%---------------------------------------------------------------------------%
%
% The following three predicates operate on parse_tree_int0s, parse_tree_int1s
% and parse_tree_int2s. They do exactly the same thing with all three,
% so their codes are identical, except for the fact that
%
% - they operate on values of different types, and
% - they call predicates that perform the same operations on those different
%   types.
%
% The difference in types is why this common code cannot be factored out.
%

:- pred maybe_parse_old_int0_and_compare_for_smart_recomp(globals::in,
    maybe_generate_version_numbers::in, file_name::in, maybe(string)::in,
    maybe(timestamp)::in, parse_tree_int0::in, parse_tree_int0::out) is det.

maybe_parse_old_int0_and_compare_for_smart_recomp(NoLineNumGlobals,
        WantVersionNumbers, FileName, MaybeOldFileStr,
        MaybeSourceFileTimestamp, GenParseTreeInt0, NewParseTreeInt0) :-
    (
        WantVersionNumbers = generate_version_numbers,
        ModuleName = GenParseTreeInt0 ^ pti0_module_name,
        (
            MaybeOldFileStr = no,
            MaybeOldParseTreeInt0 = maybe.no
        ;
            MaybeOldFileStr = yes(OldFileStr),
            % Parse the previous version of the file.
            string.count_code_units(OldFileStr, OldFileStrLen),
            parse_int0_file(NoLineNumGlobals, FileName,
                OldFileStr, OldFileStrLen, ModuleName, [],
                MaybeOldParseTreeInt0Prime, OldModuleErrors),
            ( if there_are_no_errors(OldModuleErrors) then
                MaybeOldParseTreeInt0 = MaybeOldParseTreeInt0Prime
            else
                MaybeOldParseTreeInt0 = no
            )
        ),
        % If we couldn't read in, or can't parse in the old .int file,
        % we will set all the timestamps to the modification time
        % of the source file. Insist on our caller giving us that time.
        insist_on_timestamp(MaybeSourceFileTimestamp, SourceFileTimestamp),
        recompilation.version.compute_version_numbers_int0(
            MaybeOldParseTreeInt0, SourceFileTimestamp, GenParseTreeInt0,
            VersionNumbers),
        MaybeVersionNumbers = version_numbers(VersionNumbers)
    ;
        WantVersionNumbers = do_not_generate_version_numbers,
        MaybeVersionNumbers = no_version_numbers
    ),
    NewParseTreeInt0 = GenParseTreeInt0 ^ pti0_maybe_version_numbers
        := MaybeVersionNumbers.

:- pred maybe_parse_old_int1_and_compare_for_smart_recomp(globals::in,
    maybe_generate_version_numbers::in, file_name::in, maybe(string)::in,
    maybe(timestamp)::in, parse_tree_int1::in, parse_tree_int1::out) is det.

maybe_parse_old_int1_and_compare_for_smart_recomp(NoLineNumGlobals,
        WantVersionNumbers, FileName, MaybeOldFileStr,
        MaybeSourceFileTimestamp, GenParseTreeInt1, NewParseTreeInt1) :-
    (
        WantVersionNumbers = generate_version_numbers,
        ModuleName = GenParseTreeInt1 ^ pti1_module_name,
        (
            MaybeOldFileStr = no,
            MaybeOldParseTreeInt1 = maybe.no
        ;
            MaybeOldFileStr = yes(OldFileStr),
            % Parse the previous version of the file.
            string.count_code_units(OldFileStr, OldFileStrLen),
            parse_int1_file(NoLineNumGlobals, FileName,
                OldFileStr, OldFileStrLen, ModuleName, [],
                MaybeOldParseTreeInt1Prime, OldModuleErrors),
            ( if there_are_no_errors(OldModuleErrors) then
                MaybeOldParseTreeInt1 = MaybeOldParseTreeInt1Prime
            else
                MaybeOldParseTreeInt1 = no
            )
        ),
        % If we couldn't read in, or can't parse in the old .int file,
        % we will set all the timestamps to the modification time
        % of the source file. Insist on our caller giving us that time.
        insist_on_timestamp(MaybeSourceFileTimestamp, SourceFileTimestamp),
        recompilation.version.compute_version_numbers_int1(
            MaybeOldParseTreeInt1, SourceFileTimestamp, GenParseTreeInt1,
            VersionNumbers),
        MaybeVersionNumbers = version_numbers(VersionNumbers)
    ;
        WantVersionNumbers = do_not_generate_version_numbers,
        MaybeVersionNumbers = no_version_numbers
    ),
    NewParseTreeInt1 = GenParseTreeInt1 ^ pti1_maybe_version_numbers
        := MaybeVersionNumbers.

:- pred maybe_parse_old_int2_and_compare_for_smart_recomp(globals::in,
    maybe_generate_version_numbers::in, file_name::in, maybe(string)::in,
    maybe(timestamp)::in, parse_tree_int2::in, parse_tree_int2::out) is det.

maybe_parse_old_int2_and_compare_for_smart_recomp(NoLineNumGlobals,
        WantVersionNumbers, FileName, MaybeOldFileStr,
        MaybeSourceFileTimestamp, GenParseTreeInt2, NewParseTreeInt2) :-
    (
        WantVersionNumbers = generate_version_numbers,
        ModuleName = GenParseTreeInt2 ^ pti2_module_name,
        (
            MaybeOldFileStr = no,
            MaybeOldParseTreeInt2 = maybe.no
        ;
            MaybeOldFileStr = yes(OldFileStr),
            % Parse the previous version of the file.
            string.count_code_units(OldFileStr, OldFileStrLen),
            parse_int2_file(NoLineNumGlobals, FileName,
                OldFileStr, OldFileStrLen, ModuleName, [],
                MaybeOldParseTreeInt2Prime, OldModuleErrors),
            ( if there_are_no_errors(OldModuleErrors) then
                MaybeOldParseTreeInt2 = MaybeOldParseTreeInt2Prime
            else
                MaybeOldParseTreeInt2 = no
            )
        ),
        % If we couldn't read in, or can't parse in the old .int2 file,
        % we will set all the timestamps to the modification time
        % of the source file. Insist on our caller giving us that time.
        insist_on_timestamp(MaybeSourceFileTimestamp, SourceFileTimestamp),
        recompilation.version.compute_version_numbers_int2(
            MaybeOldParseTreeInt2, SourceFileTimestamp, GenParseTreeInt2,
            VersionNumbers),
        MaybeVersionNumbers = version_numbers(VersionNumbers)
    ;
        WantVersionNumbers = do_not_generate_version_numbers,
        MaybeVersionNumbers = no_version_numbers
    ),
    NewParseTreeInt2 = GenParseTreeInt2 ^ pti2_maybe_version_numbers
        := MaybeVersionNumbers.

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
