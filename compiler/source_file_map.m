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

    % lookup_module_source_file(ModuleName, MaybeFileName, !IO)
    % Return `yes(FileName)' if FileName is the source file for ModuleName,
    % either through the source file map, or by default. Return `no' if no
    % source file is available for ModuleName because the default file name
    % is mapped to another module.
    %
:- pred lookup_module_source_file(module_name::in, maybe(file_name)::out,
    io::di, io::uo) is det.

    % Return `yes' if there is a valid Mercury.modules file.
    %
:- pred have_source_file_map(bool::out, io::di, io::uo) is det.

    % Return the default fully qualified source file name.
    %
:- func default_source_file(module_name) = file_name.

    % Given a list of file names, produce the Mercury.modules file.
    %
:- pred write_source_file_map(globals::in, list(string)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.file_names.
:- import_module parse_tree.find_module.
:- import_module parse_tree.prog_out.

:- import_module bimap.
:- import_module char.
:- import_module dir.
:- import_module string.

%-----------------------------------------------------------------------------%

lookup_module_source_file(ModuleName, MaybeFileName, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( if bimap.search(SourceFileMap, ModuleName, FileName) then
        MaybeFileName = yes(FileName)
    else
        DefaultFileName = default_source_file(ModuleName),
        ( if bimap.reverse_search(SourceFileMap, _, DefaultFileName) then
            MaybeFileName = no
        else
            MaybeFileName = yes(DefaultFileName)
        )
    ).

have_source_file_map(HaveMap, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( if bimap.is_empty(SourceFileMap) then
        HaveMap = no
    else
        HaveMap = yes
    ).

default_source_file(ModuleName) = sym_name_to_string(ModuleName) ++ ".m".

    % Read the Mercury.modules file (if it exists) to find the mapping
    % from module name to file name.
    %
:- pred get_source_file_map(source_file_map::out, io::di, io::uo) is det.

get_source_file_map(SourceFileMap, !IO) :-
    globals.io_get_maybe_source_file_map(MaybeSourceFileMap0, !IO),
    (
        MaybeSourceFileMap0 = yes(SourceFileMap0),
        SourceFileMap = SourceFileMap0
    ;
        MaybeSourceFileMap0 = no,
        io.open_input(modules_file_name, OpenRes, !IO),
        (
            OpenRes = ok(Stream),
            io.set_input_stream(Stream, OldStream, !IO),
            read_source_file_map([], bimap.init, SourceFileMap, !IO),
            io.set_input_stream(OldStream, _, !IO),
            io.close_input(Stream, !IO)
        ;
            OpenRes = error(_),
            % If the file doesn't exist, then the mapping is empty.
            SourceFileMap = bimap.init
        ),
        globals.io_set_maybe_source_file_map(yes(SourceFileMap), !IO)
    ).

:- pred read_source_file_map(list(char)::in,
    source_file_map::in, source_file_map::out, io::di, io::uo) is det.

read_source_file_map(ModuleChars, !Map, !IO) :-
    read_until_char('\t', [], ModuleCharsResult, !IO),
    (
        ModuleCharsResult = ok(RevModuleChars),
        string.from_rev_char_list(RevModuleChars, ModuleStr),
        ModuleName = string_to_sym_name(ModuleStr),
        read_until_char('\n', [], FileNameCharsResult, !IO),
        (
            FileNameCharsResult = ok(FileNameChars),
            string.from_rev_char_list(FileNameChars, FileName),
            bimap.set(ModuleName, FileName, !Map),
            read_source_file_map(ModuleChars, !Map, !IO)
        ;
            FileNameCharsResult = eof,
            io.set_exit_status(1, !IO),
            io.write_string("mercury_compile: unexpected end " ++
                "of file in Mercury.modules file.\n", !IO)
        ;
            FileNameCharsResult = error(Error),
            io.set_exit_status(1, !IO),
            io.write_string("mercury_compile: error in " ++
                "Mercury.modules file: ", !IO),
            io.write_string(io.error_message(Error), !IO),
            io.nl(!IO)
        )
    ;
        ModuleCharsResult = eof
    ;
        ModuleCharsResult = error(Error),
        io.set_exit_status(1, !IO),
        io.write_string("mercury_compile: error in " ++
            "Mercury.modules file: ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ).

:- pred read_until_char(char::in, list(char)::in, io.result(list(char))::out,
    io::di, io::uo) is det.

read_until_char(EndChar, Chars0, Result, !IO) :-
    io.read_char(CharRes, !IO),
    (
        CharRes = ok(Char),
        ( if Char = EndChar then
            Result = ok(Chars0)
        else
            read_until_char(EndChar, [Char | Chars0], Result, !IO)
        )
    ;
        CharRes = eof,
        (
            Chars0 = [],
            Result = eof
        ;
            Chars0 = [_ | _],
            Result = ok(Chars0)
        )
    ;
        CharRes = error(Error),
        Result = error(Error)
    ).

write_source_file_map(Globals, FileNames, !IO) :-
    ModulesFileName = modules_file_name,
    io.open_output(ModulesFileName, OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        list.foldl2(write_source_file_map_2(Globals, Stream), FileNames,
            bimap.init, _, !IO),
        io.close_output(Stream, !IO)
    ;
        OpenRes = error(Error),
        io.set_exit_status(1, !IO),
        io.write_string("mercury_compile: error opening `", !IO),
        io.write_string(ModulesFileName, !IO),
        io.write_string("' for output: ", !IO),
        io.write_string(io.error_message(Error), !IO)
    ).

:- pred write_source_file_map_2(globals::in, io.output_stream::in,
    file_name::in,
    bimap(module_name, file_name)::in, bimap(module_name, file_name)::out,
    io::di, io::uo) is det.

write_source_file_map_2(Globals, MapStream, FileName,
        SeenModules0, SeenModules, !IO) :-
    find_module_name(Globals, FileName, MaybeModuleName, !IO),
    (
        MaybeModuleName = yes(ModuleName),
        ( if
            bimap.search(SeenModules0, ModuleName, PrevFileName),
            PrevFileName \= FileName
        then
            io.write_string("mercury_compile: module `", !IO),
            io.write_string(sym_name_to_string(ModuleName), !IO),
            io.write_string("' defined in multiple files: ", !IO),
            io.write_string(PrevFileName, !IO),
            io.write_string(", ", !IO),
            io.write_string(FileName, !IO),
            io.write_string(".\n", !IO),
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
            io.set_output_stream(MapStream, OldStream, !IO),
            prog_out.write_sym_name(ModuleName, !IO),
            io.write_string("\t", !IO),
            io.write_string(FileName, !IO),
            io.nl(!IO),
            io.set_output_stream(OldStream, _, !IO)
        )
    ;
        MaybeModuleName = no,
        SeenModules = SeenModules0
    ).

:- func modules_file_name = string.

modules_file_name = "Mercury.modules".

%-----------------------------------------------------------------------------%
:- end_module parse_tree.source_file_map.
%-----------------------------------------------------------------------------%
