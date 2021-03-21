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
:- import_module libs.timestamp.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.read_modules.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Output the unqualified short interface file to <module>.int3.
    %
:- pred write_short_interface_file_int3(globals::in, file_name::in,
    raw_compilation_unit::in, io::di, io::uo) is det.

    % write_private_interface_file_int0(Globals, SourceFileName,
    %   SourceFileModuleName, CompUnit, MaybeTimestamp,
    %   !HaveReadModuleMaps, !IO):
    %
    % Given a source file name, the timestamp of the source file, and the
    % representation of a module in that file, output the private (`.int0')
    % interface file for the module. (The private interface contains all the
    % declarations in the module, including those in the `implementation'
    % section; it is used when compiling submodules.)
    %
    % XXX The comment on the predicate definition used to read:
    % Read in the .int3 files that the current module depends on, and use
    % these to qualify all the declarations as much as possible. Then write
    % out the .int0 file.
    %
:- pred write_private_interface_file_int0(globals::in, file_name::in,
    module_name::in, maybe(timestamp)::in, raw_compilation_unit::in,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

    % write_interface_file_int1_int2(Globals, SourceFileName,
    %   SourceFileModuleName, CompUnit, MaybeTimestamp,
    %   !HaveReadModuleMaps, !IO):
    %
    % Given a source file name, the timestamp of the source file, and the
    % representation of a module in that file, output the long (`.int')
    % and short (`.int2') interface files for the module.
    %
    % XXX The comment on the predicate definition used to read:
    % Read in the .int3 files that the current module depends on, and use these
    % to qualify all items in the interface as much as possible. Then write out
    % the .int and .int2 files.
    %
:- pred write_interface_file_int1_int2(globals::in, file_name::in,
    module_name::in, maybe(timestamp)::in, raw_compilation_unit::in,
    have_read_module_maps::in, have_read_module_maps::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.comp_unit_interface.
:- import_module parse_tree.convert_parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.grab_modules.           % undesirable dependency
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.parse_tree_out.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module bool.
:- import_module list.
:- import_module getopt.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Write out .int3 files.
%

write_short_interface_file_int3(Globals, _SourceFileName, RawCompUnit, !IO) :-
    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.
    RawCompUnit = raw_compilation_unit(ModuleName, _, _),
    generate_short_interface_int3(Globals, RawCompUnit, ParseTreeInt3,
        [], Specs0),
    filter_interface_generation_specs(Globals, Specs0, Specs, !IO),
    EffectivelyErrors =
        contains_errors_or_warnings_treated_as_errors(Globals, Specs),
    (
        EffectivelyErrors = no,
        actually_write_interface_file3(Globals, ParseTreeInt3, "", no, !IO),
        touch_interface_datestamp(Globals, ModuleName,
            other_ext(".date3"), !IO)
    ;
        EffectivelyErrors = yes,
        report_file_not_written(Globals, Specs, no, ModuleName,
            other_ext(".int3"), no, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out .int0 files.
%

write_private_interface_file_int0(Globals, SourceFileName,
        SourceFileModuleName, MaybeTimestamp, RawCompUnit0,
        !HaveReadModuleMaps, !IO) :-
    RawCompUnit0 = raw_compilation_unit(ModuleName, _, _),
    grab_unqual_imported_modules_make_int(Globals, SourceFileName,
        SourceFileModuleName, RawCompUnit0, ModuleAndImports,
        !HaveReadModuleMaps, !IO),

    % Check whether we succeeded.
    module_and_imports_get_aug_comp_unit(ModuleAndImports, AugCompUnit1,
        GetSpecs, GetErrors),
    GetSpecsEffectivelyErrors =
        contains_errors_or_warnings_treated_as_errors(Globals, GetSpecs),
    ( if
        GetSpecsEffectivelyErrors = no,
        set.is_empty(GetErrors)
    then
        % Module-qualify all items.
        % XXX ITEM_LIST We don't need grab_unqual_imported_modules
        % to include in ModuleAndImports and thus in AugCompUnit1
        % any items that (a) generate_private_interface_int0 below
        % will throw away, and (b) which don't help the module qualification
        % of the items that it keeps.
        module_qualify_aug_comp_unit(Globals, AugCompUnit1, AugCompUnit,
            map.init, _EventSpecMap, "", _, _, _, _, _, [], QualSpecs),
        filter_interface_generation_specs(Globals,
            GetSpecs ++ QualSpecs, EffectiveGetQualSpecs, !IO),
        (
            EffectiveGetQualSpecs = [],
            % Construct the `.int0' file.
            generate_private_interface_int0(AugCompUnit, ParseTreeInt0,
                [], GenerateSpecs),
            filter_interface_generation_specs(Globals,
                EffectiveGetQualSpecs ++ GenerateSpecs, Specs, !IO),
            get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
            write_error_specs_ignore(ErrorStream, Globals, Specs, !IO),
            % Write out the `.int0' file.
            actually_write_interface_file0(Globals, ParseTreeInt0, "",
                MaybeTimestamp, !IO),
            touch_interface_datestamp(Globals, ModuleName,
                other_ext(".date0"), !IO)
        ;
            EffectiveGetQualSpecs = [_ | _],
            report_file_not_written(Globals, EffectiveGetQualSpecs, no,
                ModuleName, other_ext(".int0"), no, !IO)
        )
    else
        PrefixMsg = "Error reading interface files.\n",
        report_file_not_written(Globals, GetSpecs, yes(PrefixMsg),
            ModuleName, other_ext(".int0"), no, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out .int and .int2 files.
%

write_interface_file_int1_int2(Globals, SourceFileName, SourceFileModuleName,
        MaybeTimestamp, RawCompUnit0, !HaveReadModuleMaps, !IO) :-
    RawCompUnit0 = raw_compilation_unit(ModuleName, _, _),
    generate_pre_grab_pre_qual_interface_for_int1_int2(RawCompUnit0,
        IntRawCompUnit),

    % Get the .int3 files for imported modules.
    grab_unqual_imported_modules_make_int(Globals, SourceFileName,
        SourceFileModuleName, IntRawCompUnit, ModuleAndImports,
        !HaveReadModuleMaps, !IO),

    % Check whether we succeeded.
    module_and_imports_get_aug_comp_unit(ModuleAndImports, AugCompUnit1,
        GetSpecs, GetErrors),
    GetSpecsEffectivelyErrors =
        contains_errors_or_warnings_treated_as_errors(Globals, GetSpecs),
    ( if
        GetSpecsEffectivelyErrors = no,
        set.is_empty(GetErrors)
    then
        % Module-qualify all items.
        module_qualify_aug_comp_unit(Globals, AugCompUnit1, AugCompUnit,
            map.init, _, "", _, _, _, _, _, [], QualSpecs),
        filter_interface_generation_specs(Globals,
            GetSpecs ++ QualSpecs, EffectiveGetQualSpecs, !IO),
        (
            EffectiveGetQualSpecs = [],
            % Construct the `.int' and `.int2' files.
            generate_interfaces_int1_int2(Globals, AugCompUnit,
                ParseTreeInt1, ParseTreeInt2, [], GenerateSpecs),
            filter_interface_generation_specs(Globals,
                EffectiveGetQualSpecs ++ GenerateSpecs, Specs, !IO),
            get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
            write_error_specs_ignore(ErrorStream, Globals, Specs, !IO),
            % Write out the `.int' and `.int2' files.
            actually_write_interface_file1(Globals, ParseTreeInt1, "",
                MaybeTimestamp, !IO),
            actually_write_interface_file2(Globals, ParseTreeInt2, "",
                MaybeTimestamp, !IO),
            touch_interface_datestamp(Globals, ModuleName,
                other_ext(".date"), !IO)
        ;
            EffectiveGetQualSpecs = [_ | _],
            report_file_not_written(Globals, EffectiveGetQualSpecs, no,
                ModuleName, other_ext(".int"), yes(other_ext(".int2")), !IO)
        )
    else
        PrefixMsg = "Error reading .int3 files.\n",
        report_file_not_written(Globals, GetSpecs, yes(PrefixMsg),
            ModuleName, other_ext(".int"), yes(other_ext(".int2")), !IO)
    ).

%---------------------------------------------------------------------------%

:- pred actually_write_interface_file0(globals::in, parse_tree_int0::in,
    string::in, maybe(timestamp)::in, io::di, io::uo) is det.

actually_write_interface_file0(Globals, ParseTreeInt0, ExtraSuffix,
        MaybeTimestamp, !IO) :-
    ModuleName = ParseTreeInt0 ^ pti0_module_name,
    construct_int_file_name(Globals, ModuleName, ifk_int0, ExtraSuffix,
        OutputFileName, TmpOutputFileName, !IO),
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    maybe_read_old_int_and_compare_for_smart_recomp(NoLineNumGlobals,
        ParseTreeInt0, convert_parse_tree_int0_to_int, MaybeTimestamp,
        MaybeVersionNumbers, !IO),
    ParseTreeInt0V = ParseTreeInt0 ^ pti0_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int0(NoLineNumGlobals, TmpOutputFileName,
        ParseTreeInt0V, !IO),
    update_interface(Globals, ModuleName, OutputFileName, !IO).

:- pred actually_write_interface_file1(globals::in, parse_tree_int1::in,
    string::in, maybe(timestamp)::in, io::di, io::uo) is det.

actually_write_interface_file1(Globals, ParseTreeInt1, ExtraSuffix,
        MaybeTimestamp, !IO) :-
    ModuleName = ParseTreeInt1 ^ pti1_module_name,
    construct_int_file_name(Globals, ModuleName, ifk_int1, ExtraSuffix,
        OutputFileName, TmpOutputFileName, !IO),
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    maybe_read_old_int_and_compare_for_smart_recomp(NoLineNumGlobals,
        ParseTreeInt1, convert_parse_tree_int1_to_int, MaybeTimestamp,
        MaybeVersionNumbers, !IO),
    ParseTreeInt1V = ParseTreeInt1 ^ pti1_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int1(NoLineNumGlobals, TmpOutputFileName,
        ParseTreeInt1V, !IO),
    update_interface(Globals, ModuleName, OutputFileName, !IO).

:- pred actually_write_interface_file2(globals::in, parse_tree_int2::in,
    string::in, maybe(timestamp)::in, io::di, io::uo) is det.

actually_write_interface_file2(Globals, ParseTreeInt2, ExtraSuffix,
        MaybeTimestamp, !IO) :-
    ModuleName = ParseTreeInt2 ^ pti2_module_name,
    construct_int_file_name(Globals, ModuleName, ifk_int2, ExtraSuffix,
        OutputFileName, TmpOutputFileName, !IO),
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    maybe_read_old_int_and_compare_for_smart_recomp(NoLineNumGlobals,
        ParseTreeInt2, convert_parse_tree_int2_to_int, MaybeTimestamp,
        MaybeVersionNumbers, !IO),
    ParseTreeInt2V = ParseTreeInt2 ^ pti2_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int2(NoLineNumGlobals, TmpOutputFileName,
        ParseTreeInt2V, !IO),
    update_interface(Globals, ModuleName, OutputFileName, !IO).

:- pred actually_write_interface_file3(globals::in, parse_tree_int3::in,
    string::in, maybe(timestamp)::in, io::di, io::uo) is det.

actually_write_interface_file3(Globals, ParseTreeInt3, ExtraSuffix,
        _MaybeTimestamp, !IO) :-
    ModuleName = ParseTreeInt3 ^ pti3_module_name,
    construct_int_file_name(Globals, ModuleName, ifk_int3, ExtraSuffix,
        OutputFileName, TmpOutputFileName, !IO),
    disable_all_line_numbers(Globals, NoLineNumGlobals),
    output_parse_tree_int3(NoLineNumGlobals, TmpOutputFileName,
        ParseTreeInt3, !IO),
    update_interface(Globals, ModuleName, OutputFileName, !IO).

%---------------------------------------------------------------------------%

:- pred construct_int_file_name(globals::in,
    module_name::in, int_file_kind::in, string::in,
    string::out, string::out, io::di, io::uo) is det.

construct_int_file_name(Globals, ModuleName, IntFileKind, ExtraSuffix,
        OutputFileName, TmpOutputFileName, !IO) :-
    int_file_kind_to_extension(IntFileKind, _ExtStr, OtherExt),
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(OtherExt), ModuleName, OutputFileName0, !IO),
    OutputFileName = OutputFileName0 ++ ExtraSuffix,
    TmpOutputFileName = OutputFileName ++ ".tmp".

:- pred disable_all_line_numbers(globals::in, globals::out) is det.

disable_all_line_numbers(Globals, NoLineNumGlobals) :-
    globals.set_option(line_numbers, bool(no),
        Globals, NoLineNumGlobals0),
    globals.set_option(line_numbers_around_foreign_code, bool(no),
        NoLineNumGlobals0, NoLineNumGlobals).

%---------------------------------------------------------------------------%

:- pred maybe_read_old_int_and_compare_for_smart_recomp(globals::in,
    T::in, (func(T) = parse_tree_int)::in, maybe(timestamp)::in,
    maybe_version_numbers::out, io::di, io::uo) is det.

maybe_read_old_int_and_compare_for_smart_recomp(NoLineNumGlobals,
        ParseTreeIntN, ParseTreeConvert, MaybeTimestamp,
        MaybeVersionNumbers, !IO) :-
    should_generate_item_version_numbers(NoLineNumGlobals,
        WantVersionNumbers, !IO),
    (
        WantVersionNumbers = generate_version_numbers,
        ParseTreeInt = ParseTreeConvert(ParseTreeIntN),
        ModuleName = ParseTreeInt ^ pti_module_name,
        IntFileKind = ParseTreeInt ^ pti_int_file_kind,
        % XXX ITEM_LIST We do this for .int2 files as well as
        % .int and .int0 files. Should we?

        % Find the timestamp of the current module.
        insist_on_timestamp(MaybeTimestamp, Timestamp),
        % Read in the previous version of the file.
        read_module_int(NoLineNumGlobals,
            "Reading old interface for module", ignore_errors, do_search,
            ModuleName, IntFileKind, _OldIntFileName,
            always_read_module(dont_return_timestamp), _OldTimestamp,
            OldParseTreeInt, _OldSpecs, OldErrors, !IO),
        ( if set.is_empty(OldErrors) then
            MaybeOldParseTreeInt = yes(OldParseTreeInt)
        else
            % If we can't read in the old file, the timestamps will
            % all be set to the modification time of the source file.
            MaybeOldParseTreeInt = no
        ),
        recompilation.version.compute_version_numbers(Timestamp,
            ParseTreeInt, MaybeOldParseTreeInt, VersionNumbers),
        MaybeVersionNumbers = version_numbers(VersionNumbers)
    ;
        WantVersionNumbers = do_not_generate_version_numbers,
        MaybeVersionNumbers = no_version_numbers
    ).

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

:- pred report_file_not_written(globals::in, list(error_spec)::in,
    maybe(string)::in, module_name::in, other_ext::in, maybe(other_ext)::in,
    io::di, io::uo) is det.

report_file_not_written(Globals, Specs, MaybePrefixMsg,
        ModuleName, OtherExtA, MaybeOtherExtB, !IO) :-
    get_error_output_stream(Globals, ModuleName, ErrorStream, !IO),
    write_error_specs_ignore(ErrorStream, Globals, Specs, !IO),
    (
        MaybePrefixMsg = no
    ;
        MaybePrefixMsg = yes(PrefixMsg),
        io.write_string(ErrorStream, PrefixMsg, !IO)
    ),
    % We use write_error_spec to print the message the interface file or
    % files not being written in order to wrap the message if it is
    % longer than the line length.
    module_name_to_file_name(Globals, $pred, do_not_create_dirs,
        ext_other(OtherExtA), ModuleName, IntAFileName, !IO),
    (
        MaybeOtherExtB = no,
        NotWrittenPieces = [quote(IntAFileName), words("not written."), nl]
    ;
        MaybeOtherExtB = yes(OtherExtB),
        module_name_to_file_name(Globals, $pred, do_not_create_dirs,
            ext_other(OtherExtB), ModuleName, IntBFileName, !IO),
        NotWrittenPieces = [quote(IntAFileName), words("and"),
            quote(IntBFileName), words("not written."), nl]
    ),
    NotWrittenMsg = error_msg(no, treat_as_first, 0,
        [always(NotWrittenPieces)]),
    NotWrittenSpec = error_spec($pred, severity_informational,
        phase_read_files, [NotWrittenMsg]),
    write_error_spec_ignore(ErrorStream, Globals, NotWrittenSpec, !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_module_interface_files.
%---------------------------------------------------------------------------%
