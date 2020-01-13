%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2014, 2016-2017, 2019-2020 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines predicates that find Mercury modules.
%
%---------------------------------------------------------------------------%

:- module parse_tree.find_module.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % search_for_module_source(Globals, Dirs, ModuleName, FoundSourceFile,
    %   !IO):
    %
    % Look for the source for ModuleName in Dirs. If found, return the
    % (relative or absolute) path name of the source file that contains
    % the module.
    %
:- pred search_for_module_source(globals::in, list(dir_name)::in,
    module_name::in, maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_module_source_and_stream(Globals, Dirs, ModuleName,
    %   FoundSourceFileNameAndStream, !IO):
    %
    % As search_for_module_source, but if we find the file, then return
    % not just its path name, but also an open stream reading from it.
    % Closing that stream is the caller's responsibility.
    %
:- pred search_for_module_source_and_stream(globals::in, list(dir_name)::in,
    module_name::in, maybe_error(path_name_and_stream)::out, io::di, io::uo)
    is det.

    % Read the first item from the given file to find the module name.
    %
:- pred find_module_name(globals::in, file_name::in, maybe(module_name)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.parse_module.        % for peek_at_file

:- import_module dir.
:- import_module string.

%---------------------------------------------------------------------------%

search_for_module_source(Globals, Dirs, ModuleName, MaybeFileName, !IO) :-
    search_for_module_source_and_stream(Globals, Dirs, ModuleName,
        MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream =
            ok(path_name_and_stream(SourceFileName, SourceStream)),
        io.close_input(SourceStream, !IO),
        MaybeFileName = ok(SourceFileName)
    ;
        MaybeFileNameAndStream = error(Msg),
        MaybeFileName = error(Msg)
    ).

search_for_module_source_and_stream(Globals, Dirs, ModuleName,
        MaybeFileNameAndStream, !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs, ".m",
        ModuleName, FileName0, !IO),
    search_for_file_and_stream(Dirs, FileName0, MaybeFileNameAndStream0, !IO),
    (
        MaybeFileNameAndStream0 = ok(_),
        MaybeFileNameAndStream = MaybeFileNameAndStream0
    ;
        MaybeFileNameAndStream0 = error(_),
        Error = find_source_error(ModuleName, Dirs, no),
        MaybeFileNameAndStream = error(Error)
    ).

%------------%

:- func find_source_error(module_name, list(dir_name), maybe(file_name))
    = string.

find_source_error(ModuleName, Dirs, MaybeBetterMatch) = Msg :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    Msg0 = "cannot find source for module `" ++ ModuleNameStr ++
        "' in directories " ++
        string.join_list(", ",
            map((func(Dir) = "`" ++ Dir ++ "'"), Dirs)),
    (
        MaybeBetterMatch = no,
        Msg = Msg0
    ;
        MaybeBetterMatch = yes(BetterMatchFile),
        Msg = Msg0 ++ ", but found " ++ BetterMatchFile ++
            " in interface search path"
    ).

%---------------------------------------------------------------------------%

find_module_name(Globals, FileName, MaybeModuleName, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(FileStream),
        ( if string.remove_suffix(FileName, ".m", PartialFileName0) then
            PartialFileName = PartialFileName0
        else
            PartialFileName = FileName
        ),
        ( if dir.basename(PartialFileName, BaseName0) then
            BaseName = BaseName0
        else
            BaseName = ""
        ),
        file_name_to_module_name(BaseName, DefaultModuleName),
        peek_at_file(FileStream, DefaultModuleName, [], FileName, ModuleName,
            Specs, !IO),
        io.close_input(FileStream, !IO),
        MaybeModuleName = yes(ModuleName),
        % XXX We don't check whether ModuleName was actually read
        % from the named file; it could just be DefaultModuleName.
        write_error_specs_ignore(Specs, Globals, !IO)
    ;
        OpenRes = error(Error),
        ErrorMsg = io.error_message(Error),
        io.progname_base("mercury_compile", Progname, !IO),
        Pieces = [fixed(Progname), suffix(":"), words("error opening"),
            quote(FileName), suffix(":"), words(ErrorMsg), suffix("."), nl],
        Spec = error_spec(severity_error, phase_read_files,
            [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
        write_error_spec_ignore(Spec, Globals, !IO),
        MaybeModuleName = no
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.find_module.
%---------------------------------------------------------------------------%
