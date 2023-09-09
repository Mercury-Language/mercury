%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2009, 2011 The University of Melbourne.
% Copyright (C) 2014-2015, 2019-2020 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: source_file_map.m.
% Author: stayl.
%
% Maintain a mapping from module name to source file name.
%
% The reason why this module is in the parse_tree package is that discovering
% what module is stored in a file requires reading the ":- module" declaration
% in that file.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.source_file_map.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % lookup_module_source_file(ModuleName, MaybeFileName, !IO):
    %
    % Return `yes(FileName)' if FileName is the source file for ModuleName,
    % either through the source file map, or by default. Return `no' if no
    % source file is available for ModuleName because the default file name
    % for ModuleName is mapped to another module.
    %
:- pred lookup_module_source_file(module_name::in, maybe(file_name)::out,
    io::di, io::uo) is det.

    % lookup_source_file_module(FileName, MaybeModuleName, !IO):
    %
    % Return `yes(ModuleName)' if FileName is the source file for ModuleName,
    % either through the source file map, or by default. Return `no' if no
    % module name is available for FileName because the default module name
    % for FileName is stored in another file.
    %
:- pred lookup_source_file_module(file_name::in, maybe(module_name)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Return `yes' if there is a valid Mercury.modules file.
    %
:- pred have_source_file_map(bool::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Return the default fully qualified source file name.
    %
:- func default_source_file_name(module_name) = file_name.

%-----------------------------------------------------------------------------%

    % write_source_file_map(Globals, ProgressStream, FileNames, !IO):
    %
    % Given a list of file names, produce the Mercury.modules file.
    %
:- pred write_source_file_map(io.text_output_stream::in,
    globals::in, list(string)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.
:- import_module parse_tree.parse_tree_out_sym_name.

:- import_module bimap.
:- import_module dir.
:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%
%
% ZZZ Reorder the contents of this module.
%
%-----------------------------------------------------------------------------%

lookup_module_source_file(ModuleName, MaybeFileName, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( if bimap.search(SourceFileMap, ModuleName, FileName) then
        MaybeFileName = yes(FileName)
    else
        DefaultFileName = default_source_file_name(ModuleName),
        ( if bimap.reverse_search(SourceFileMap, _, DefaultFileName) then
            MaybeFileName = no
        else
            MaybeFileName = yes(DefaultFileName)
        )
    ).

lookup_source_file_module(FileName, MaybeModuleName, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( if bimap.reverse_search(SourceFileMap, ModuleName, FileName) then
        MaybeModuleName = yes(ModuleName)
    else
        ( if default_module_name_for_file(FileName, DefaultModuleName) then
            ( if bimap.search(SourceFileMap, DefaultModuleName, _) then
                MaybeModuleName = no
            else
                MaybeModuleName = yes(DefaultModuleName)
            )
        else
            MaybeModuleName = no
        )
    ).

%-----------------------------------------------------------------------------%

have_source_file_map(HaveMap, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( if bimap.is_empty(SourceFileMap) then
        HaveMap = no
    else
        HaveMap = yes
    ).

default_source_file_name(ModuleName) = sym_name_to_string(ModuleName) ++ ".m".

    % If the file name ends in ".m", return the module name whose
    % default file name that would be.
    %
:- pred default_module_name_for_file(file_name::in, module_name::out)
    is semidet.

default_module_name_for_file(FileName, DefaultModuleName) :-
    string.remove_suffix(FileName, ".m", FileNameBeforeDotM),
    file_name_to_module_name(FileNameBeforeDotM, DefaultModuleName).

%-----------------------------------------------------------------------------%

    % Bidirectional map between module names and file names.
    %
:- type source_file_map == bimap(module_name, string).

:- mutable(maybe_source_file_map, maybe(source_file_map), no, ground,
    [untrailed, attach_to_io_state]).

%-----------------------------------------------------------------------------%

    % Read the Mercury.modules file (if it exists, and if we have not
    % read it before) to find and return the mapping from module names
    % to file names and vice versa.
    %
:- pred get_source_file_map(source_file_map::out, io::di, io::uo) is det.

get_source_file_map(SourceFileMap, !IO) :-
    get_maybe_source_file_map(MaybeSourceFileMap0, !IO),
    (
        MaybeSourceFileMap0 = yes(SourceFileMap0),
        SourceFileMap = SourceFileMap0
    ;
        MaybeSourceFileMap0 = no,
        ModulesFileName = modules_file_name,
        io.read_named_file_as_lines(ModulesFileName, ReadResult, !IO),
        (
            ReadResult = ok(FileLines),
            bimap.init(SourceFileMap0),
            parse_source_file_map(FileLines, ModulesFileName, 1, ErrorMsg,
                SourceFileMap0, SourceFileMap1),
            ( if ErrorMsg = "" then
                SourceFileMap = SourceFileMap1
            else
                % If the file does exist but is malformed, then
                % we *should* generate an error, but pretending that
                % the file exists and is empty preserves old behavior.
                bimap.init(SourceFileMap)
            )
        ;
            ReadResult = error(_),
            % If the file doesn't exist, then the mapping is empty.
            % XXX ReadResult can be error/1 even when the file *does* exist.
            % For example, the open could fail due to a permission problem.
            SourceFileMap = bimap.init
        ),
        set_maybe_source_file_map(yes(SourceFileMap), !IO)
    ).

:- pred parse_source_file_map(list(string)::in, string::in, int::in,
    string::out, source_file_map::in, source_file_map::out) is det.

parse_source_file_map(Lines, ModulesFileName, CurLineNumber, ErrorMsg,
        !SourceFileMap) :-
    (
        Lines = [HeadLine | TailLines],
        ( if string.sub_string_search(HeadLine, "\t", TabIndex) then
            string.length(HeadLine, LineLength),
            string.unsafe_between(HeadLine, 0, TabIndex, ModuleNameStr),
            string.unsafe_between(HeadLine, TabIndex + 1, LineLength,
                FileName),
            ModuleName = string_to_sym_name(ModuleNameStr),
            % XXX A module cannot be contained in two files, which means that
            % ModuleName should be a unique key in the forward map.
            % However, with nested modules, a single file may contain
            % more than one module, so FileName may *not* be a unique key
            % in the backward map.
            % XXX However, if Mercury.modules contains more than one line
            % with the same filename, then this code has a bug, because
            % the call sequence
            %
            %   bimap.set("module_a", "filename", !SourceFileMap)
            %   bimap.set("module_a.sub1", "filename", !SourceFileMap)
            %   bimap.set("module_a.sub2", "filename", !SourceFileMap)
            %
            % will leave only one key that maps to the value "filename",
            % which will be the last one added ("module_a.sub2" in this case).
            %
            % XXX We should call bimap.det_insert here to abort in such
            % situations, but I (zs) am not sure that output generated by
            % write_source_file_map is guaranteed to be a bijection.
            bimap.set(ModuleName, FileName, !SourceFileMap),
            parse_source_file_map(TailLines,
                ModulesFileName, CurLineNumber + 1, ErrorMsg, !SourceFileMap)
        else
            string.format("line %d of %s is missing a tab character",
                [i(CurLineNumber), s(ModulesFileName)], ErrorMsg)
        )
    ;
        Lines = [],
        ErrorMsg = ""
    ).

%-----------------------------------------------------------------------------%

write_source_file_map(ProgressStream, Globals, FileNames, !IO) :-
    % XXX We print error messages to stderr, because there is no appropriate
    % module name we can give to globals.get_error_output_stream.
    ModulesFileName = modules_file_name,
    io.open_output(ModulesFileName, FileResult, !IO),
    (
        FileResult = ok(FileStream),
        list.foldl2(
            write_source_file_map_2(ProgressStream, FileStream, Globals),
            FileNames, bimap.init, _, !IO),
        io.close_output(FileStream, !IO)
    ;
        FileResult = error(Error),
        io.stderr_stream(StdErr, !IO),
        io.format(StdErr,
            "mercury_compile: error opening `%s' for output: %s",
            [s(ModulesFileName), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred write_source_file_map_2(io.text_output_stream::in,
    io.text_output_stream::in, globals::in, file_name::in,
    bimap(module_name, file_name)::in, bimap(module_name, file_name)::out,
    io::di, io::uo) is det.

write_source_file_map_2(ProgressStream, MapStream, Globals,
        FileName, SeenModules0, SeenModules, !IO) :-
    find_module_name(ProgressStream, Globals, FileName, MaybeModuleName, !IO),
    (
        MaybeModuleName = yes(ModuleName),
        ( if
            bimap.search(SeenModules0, ModuleName, PrevFileName),
            PrevFileName \= FileName
        then
            io.format(ProgressStream,
                "mercury_compile: " ++
                "module `%s' defined in multiple files: %s, %s\n.",
                [s(sym_name_to_string(ModuleName)),
                s(PrevFileName), s(FileName)], !IO),
            io.set_exit_status(1, !IO),
            SeenModules = SeenModules0
        else
            bimap.set(ModuleName, FileName, SeenModules0, SeenModules)
        ),
        ( if string.remove_suffix(FileName, ".m", PartialFileName0) then
            PartialFileName = PartialFileName0
        else
            PartialFileName = FileName
        ),
        file_name_to_module_name(dir.det_basename(PartialFileName),
            DefaultModuleName),
        ( if
            % Only include a module in the mapping if the name doesn't match
            % the default.
            dir.dirname(PartialFileName) = dir.this_directory : string,
            ModuleName = DefaultModuleName
        then
            true
        else
            io.format(MapStream, "%s\t%s\n",
                [s(escaped_sym_name_to_string(ModuleName)), s(FileName)], !IO)
        )
    ;
        MaybeModuleName = no,
        SeenModules = SeenModules0
    ).

%-----------------------------------------------------------------------------%

:- func modules_file_name = string.

modules_file_name = "Mercury.modules".

%-----------------------------------------------------------------------------%
:- end_module parse_tree.source_file_map.
%-----------------------------------------------------------------------------%
