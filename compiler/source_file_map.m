%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: source_file_map.m
% Author: stayl
%
% Maintain a mapping from module name to source file name.
%-----------------------------------------------------------------------------%

:- module parse_tree__source_file_map.

:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_io.

:- import_module bool.
:- import_module io.
:- import_module list.

    % lookup_module_source_file(ModuleName, FileName, !IO)
    %
:- pred lookup_module_source_file(module_name::in, file_name::out,
    io::di, io::uo) is det.

    % Return `yes' if there is a valid Mercury.modules file.
    %
:- pred have_source_file_map(bool::out, io::di, io::uo) is det.

    % Return the default fully-qualified source file name.
    %
:- func default_source_file(module_name) = file_name.

    % Given a list of file names, produce the Mercury.modules file.
    %
:- pred write_source_file_map(list(string)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module char.
:- import_module dir.
:- import_module map.
:- import_module std_util.
:- import_module string.

lookup_module_source_file(ModuleName, FileName, !IO) :-
    get_source_file_map(SourceFileMap, !IO),
    ( map__search(SourceFileMap, ModuleName, FileName0) ->
        FileName = FileName0
    ;
        FileName = default_source_file(ModuleName)
    ).

default_source_file(ModuleName) = BaseFileName ++ ".m" :-
    mdbcomp__prim_data__sym_name_to_string(ModuleName, ".", BaseFileName).

have_source_file_map(HaveMap, !IO) :-
    get_source_file_map(_, !IO),
    globals__io_get_globals(Globals, !IO),
    globals__get_source_file_map(Globals, MaybeSourceFileMap),
    ( MaybeSourceFileMap = yes(Map), \+ map__is_empty(Map) ->
        HaveMap = yes
    ;
        HaveMap = no
    ).

    % Read the Mercury.modules file (if it exists) to find the mapping
    % from module name to file name.
    %
:- pred get_source_file_map(source_file_map::out, io::di, io::uo) is det.

get_source_file_map(SourceFileMap, !IO) :-
    globals__io_get_globals(Globals0, !IO),
    globals__get_source_file_map(Globals0, MaybeSourceFileMap0),
    (
        MaybeSourceFileMap0 = yes(SourceFileMap0),
        SourceFileMap = SourceFileMap0
    ;
        MaybeSourceFileMap0 = no,
        io__open_input(modules_file_name, OpenRes, !IO),
        (
            OpenRes = ok(Stream),
            io__set_input_stream(Stream, OldStream, !IO),
            read_source_file_map([], map__init, SourceFileMap, !IO),
            io__set_input_stream(OldStream, _, !IO),
            io__close_input(Stream, !IO)
        ;
            OpenRes = error(_),
            % If the file doesn't exist, then the mapping is empty.
            SourceFileMap = map__init
        ),
        globals__io_get_globals(Globals1, !IO),
        globals__set_source_file_map(yes(SourceFileMap), Globals1, Globals2),
        unsafe_promise_unique(Globals2, Globals),
        globals__io_set_globals(Globals, !IO)
    ).

:- pred read_source_file_map(list(char)::in,
    source_file_map::in, source_file_map::out, io::di, io::uo) is det.

read_source_file_map(ModuleChars, !Map, !IO) :-
    read_until_char('\t', [], ModuleCharsResult, !IO),
    (
        ModuleCharsResult = ok(RevModuleChars),
        string__from_rev_char_list(RevModuleChars, ModuleStr),
        string_to_sym_name(ModuleStr, ".", ModuleName),
        read_until_char('\n', [], FileNameCharsResult, !IO),
        (
            FileNameCharsResult = ok(FileNameChars),
            string__from_rev_char_list(FileNameChars, FileName),
            map__set(!.Map, ModuleName, FileName, !:Map),
            read_source_file_map(ModuleChars, !Map, !IO)
        ;
            FileNameCharsResult = eof,
            io__set_exit_status(1, !IO),
            io__write_string("mercury_compile: unexpected end " ++
                "of file in Mercury.modules file.\n", !IO)
        ;
            FileNameCharsResult = error(Error),
            io__set_exit_status(1, !IO),
            io__write_string("mercury_compile: error in " ++
                "Mercury.modules file: ", !IO),
            io__write_string(io__error_message(Error), !IO),
            io__nl(!IO)
        )
    ;
        ModuleCharsResult = eof
    ;
        ModuleCharsResult = error(Error),
        io__set_exit_status(1, !IO),
        io__write_string("mercury_compile: error in " ++
            "Mercury.modules file: ", !IO),
        io__write_string(io__error_message(Error), !IO),
        io__nl(!IO)
    ).

:- pred read_until_char(char::in, list(char)::in, io__result(list(char))::out,
    io::di, io::uo) is det.

read_until_char(EndChar, Chars0, Result, !IO) :-
    io__read_char(CharRes, !IO),
    (
        CharRes = ok(Char),
        ( Char = EndChar ->
            Result = ok(Chars0)
        ;
            read_until_char(EndChar, [Char | Chars0], Result, !IO)
        )
    ;
        CharRes = eof,
        Result = ( Chars0 = [] -> eof ; ok(Chars0) )
    ;
        CharRes = error(Error),
        Result = error(Error)
    ).

write_source_file_map(FileNames, !IO) :-
    ModulesFileName = modules_file_name,
    io__open_output(ModulesFileName, OpenRes, !IO),
    (
        OpenRes = ok(Stream),
        list__foldl(write_source_file_map_2(Stream), FileNames, !IO),
        io__close_output(Stream, !IO)
    ;
        OpenRes = error(Error),
        io__set_exit_status(1, !IO),
        io__write_string("mercury_compile: error opening `", !IO),
        io__write_string(ModulesFileName, !IO),
        io__write_string("' for output: ", !IO),
        io__write_string(io__error_message(Error), !IO)
    ).

:- pred write_source_file_map_2(io__output_stream::in, file_name::in,
    io::di, io::uo) is det.

write_source_file_map_2(MapStream, FileName, !IO) :-
    find_module_name(FileName, MaybeModuleName, !IO),
    (
        MaybeModuleName = yes(ModuleName),
        ( string__remove_suffix(FileName, ".m", PartialFileName0) ->
            PartialFileName = PartialFileName0
        ;
            PartialFileName = FileName
        ),
        file_name_to_module_name(dir__basename_det(PartialFileName),
            DefaultModuleName),
        (
            % Only include a module in the mapping if the name doesn't match
            % the default.
            dir__dirname(PartialFileName) = dir__this_directory : string,
            ModuleName = DefaultModuleName
        ->
            true
        ;
            io__set_output_stream(MapStream, OldStream, !IO),
            prog_out__write_sym_name(ModuleName, !IO),
            io__write_string("\t", !IO),
            io__write_string(FileName, !IO),
            io__nl(!IO),
            io__set_output_stream(OldStream, _, !IO)
        )
    ;
        MaybeModuleName = no
    ).

:- func modules_file_name = string.

modules_file_name = "Mercury.modules".
