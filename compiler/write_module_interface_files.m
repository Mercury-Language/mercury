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

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Output the unqualified short interface file to <module>.int3.
    %
:- pred write_short_interface_file_int3(globals::in, file_name::in,
    raw_compilation_unit::in, io::di, io::uo) is det.

    % write_private_interface_file_int0(Globals, SourceFileName,
    %   SourceFileModuleName, CompUnit, MaybeTimestamp, !IO):
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
    io::di, io::uo) is det.

    % write_interface_file_int1_int2(Globals, SourceFileName,
    %   SourceFileModuleName, CompUnit, MaybeTimestamp, !IO):
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
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.canonicalize_interface.
:- import_module parse_tree.comp_unit_interface.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.
:- import_module parse_tree.module_imports.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.modules.            % undesirable dependency
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.read_modules.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module bool.
:- import_module list.
:- import_module getopt_io.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%
%
% Write out .int3 files.
%

write_short_interface_file_int3(Globals, SourceFileName, RawCompUnit, !IO) :-
    % This qualifies everything as much as it can given the information
    % in the current module and writes out the .int3 file.

    RawCompUnit = raw_compilation_unit(ModuleName, _, _),
    generate_short_interface_int3(Globals, RawCompUnit, ParseTreeInt3, Specs0),
    module_qualify_parse_tree_int(Globals, ParseTreeInt3, QualParseTreeInt3,
        Specs0, Specs),
    write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
    % XXX Why do we do this even if there are some errors?
    actually_write_interface_file(Globals, SourceFileName, QualParseTreeInt3,
        no, !IO),
    touch_interface_datestamp(Globals, ModuleName, ".date3", !IO).

%---------------------------------------------------------------------------%
%
% Write out .int0 files.
%

write_private_interface_file_int0(Globals, SourceFileName,
        SourceFileModuleName, MaybeTimestamp, RawCompUnit0, !IO) :-
    RawCompUnit0 = raw_compilation_unit(ModuleName, _, _),
    grab_unqual_imported_modules(Globals, SourceFileName, SourceFileModuleName,
        RawCompUnit0, ModuleAndImports, !IO),

    % Check whether we succeeded.
    module_and_imports_get_aug_comp_unit(ModuleAndImports, AugCompUnit1,
        Specs0, Errors),
    ( if set.is_non_empty(Errors) then
        PrefixMsg = "Error reading interface files.\n",
        report_file_not_written(Globals, Specs0, yes(PrefixMsg),
            ModuleName, ".int0", no, !IO)
    else
        % Module-qualify all items.
        % XXX ITEM_LIST We don't need grab_unqual_imported_modules
        % to include in ModuleAndImports and thus in AugCompUnit1
        % any items that (a) generate_private_interface_int0 below
        % will throw away, and (b) which don't help the module qualification
        % of the items that it keeps.
        module_qualify_aug_comp_unit(Globals, AugCompUnit1, AugCompUnit,
            map.init, _EventSpecMap, "", _, _, _, _, _, Specs0, Specs),
        (
            Specs = [_ | _],
            report_file_not_written(Globals, Specs, no,
                ModuleName, ".int0", no, !IO)
        ;
            Specs = [],
            % Construct and write out the `.int0' file.
            generate_private_interface_int0(AugCompUnit, ParseTreeInt0),
            actually_write_interface_file(Globals, SourceFileName,
                ParseTreeInt0, MaybeTimestamp, !IO),
            touch_interface_datestamp(Globals, ModuleName, ".date0", !IO)
        )
    ).

%---------------------------------------------------------------------------%
%
% Write out .int and .int2 files.
%

write_interface_file_int1_int2(Globals, SourceFileName, SourceFileModuleName,
        MaybeTimestamp, RawCompUnit0, !IO) :-
    RawCompUnit0 = raw_compilation_unit(ModuleName, _, _),
    generate_pre_grab_pre_qual_interface_for_int1_int2(RawCompUnit0,
        IntRawCompUnit),

    % Get the .int3 files for imported modules.
    grab_unqual_imported_modules(Globals, SourceFileName,
        SourceFileModuleName, IntRawCompUnit, ModuleAndImports, !IO),

    % Check whether we succeeded.
    module_and_imports_get_aug_comp_unit(ModuleAndImports, AugCompUnit1,
        Specs0, Errors),
    ( if set.is_non_empty(Errors) then
        PrefixMsg = "Error reading short interface files.\n",
        report_file_not_written(Globals, Specs0, yes(PrefixMsg),
            ModuleName, ".int", yes(".int2"), !IO)
    else
        % Module-qualify all items.
        module_qualify_aug_comp_unit(Globals, AugCompUnit1, AugCompUnit,
            map.init, _, "", _, _, _, _, _, Specs0, Specs),

        % We want to finish writing the interface files (and keep
        % the exit status at zero) if we found some warnings.
        globals.set_option(halt_at_warn, bool(no),
            Globals, NoHaltAtWarnGlobals),
        write_error_specs(Specs, NoHaltAtWarnGlobals,
            0, _NumWarnings, 0, NumErrors, !IO),
        ( if NumErrors > 0 then
            report_file_not_written(Globals, [], no,
                ModuleName, ".int", yes(".int2"), !IO)
        else
            % Construct and write out the `.int' and `.int2' files.
            generate_interfaces_int1_int2(Globals, AugCompUnit,
                ParseTreeInt1, ParseTreeInt2, InterfaceSpecs),
            % XXX _NumErrors2
            write_error_specs(InterfaceSpecs, Globals,
                0, _NumWarnings2, 0, _NumErrors2, !IO),
            actually_write_interface_file(Globals, SourceFileName,
                ParseTreeInt1, MaybeTimestamp, !IO),
            actually_write_interface_file(Globals, SourceFileName,
                ParseTreeInt2, MaybeTimestamp, !IO),
            touch_interface_datestamp(Globals, ModuleName, ".date", !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred actually_write_interface_file(globals::in, file_name::in,
    parse_tree_int::in, maybe(timestamp)::in, io::di, io::uo) is det.

actually_write_interface_file(Globals, _SourceFileName, ParseTreeInt0,
        MaybeTimestamp, !IO) :-
    order_parse_tree_int_contents(ParseTreeInt0, ParseTreeInt1),

    % Create (e.g.) `foo.int.tmp'.
    ModuleName = ParseTreeInt1 ^ pti_module_name,
    IntFileKind = ParseTreeInt1 ^ pti_int_file_kind,
    Suffix = int_file_kind_to_extension(IntFileKind),
    TmpSuffix = Suffix ++ ".tmp",
    module_name_to_file_name(Globals, do_create_dirs, Suffix,
        ModuleName, OutputFileName, !IO),
    module_name_to_file_name(Globals, do_not_create_dirs, TmpSuffix,
        ModuleName, TmpOutputFileName, !IO),

    globals.set_option(line_numbers, bool(no),
        Globals, NoLineNumGlobals0),
    globals.set_option(line_numbers_around_foreign_code, bool(no),
        NoLineNumGlobals0, NoLineNumGlobals),
    globals.lookup_bool_option(NoLineNumGlobals, generate_item_version_numbers,
        GenerateVersionNumbers),
    io_get_disable_generate_item_version_numbers(DisableVersionNumbers, !IO),
    ( if
        GenerateVersionNumbers = yes,
        DisableVersionNumbers = no
        % XXX ITEM_LIST We do this for .int2 files as well as .int files.
        % Should we?
    then
        % Find the timestamp of the current module.
        (
            MaybeTimestamp = no,
            unexpected($pred,
                "with `--smart-recompilation', timestamp not read")
        ;
            MaybeTimestamp = yes(Timestamp)
        ),

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
            ParseTreeInt1, MaybeOldParseTreeInt, VersionNumbers),
        MaybeVersionNumbers = yes(VersionNumbers)
    else
        MaybeVersionNumbers = no
    ),
    ParseTreeInt = ParseTreeInt1 ^ pti_maybe_version_numbers
        := MaybeVersionNumbers,
    output_parse_tree_int(NoLineNumGlobals, TmpOutputFileName,
        ParseTreeInt, !IO),
    % Start using the original globals again.
    update_interface(Globals, OutputFileName, !IO).

%---------------------------------------------------------------------------%

:- pred report_file_not_written(globals::in, list(error_spec)::in,
    maybe(string)::in, module_name::in, string::in, maybe(string)::in,
    io::di, io::uo) is det.

report_file_not_written(Globals, Specs, MaybePrefixMsg,
        ModuleName, SuffixA, MaybeSuffixB, !IO) :-
    % XXX _NumErrors
    write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
    (
        MaybePrefixMsg = no
    ;
        MaybePrefixMsg = yes(PrefixMsg),
        io.write_string(PrefixMsg, !IO)
    ),
    % We use write_error_spec to print the message the interface file or
    % files not being written in order to wrap the message if it is
    % longer than the line length.
    module_name_to_file_name(Globals, do_not_create_dirs, SuffixA,
        ModuleName, IntAFileName, !IO),
    (
        MaybeSuffixB = no,
        NotWrittenPieces = [quote(IntAFileName), words("not written."), nl]
    ;
        MaybeSuffixB = yes(SuffixB),
        module_name_to_file_name(Globals, do_not_create_dirs, SuffixB,
            ModuleName, IntBFileName, !IO),
        NotWrittenPieces = [quote(IntAFileName), words("and"),
            quote(IntBFileName), words("not written."), nl]
    ),
    NotWrittenMsg = error_msg(no, treat_as_first, 0,
        [always(NotWrittenPieces)]),
    NotWrittenSpec = error_spec(severity_informational, phase_read_files,
        [NotWrittenMsg]),
    write_error_spec(NotWrittenSpec, Globals, 0, _, 0, _, !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_module_interface_files.
%---------------------------------------------------------------------------%
