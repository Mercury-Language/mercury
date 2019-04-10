%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2014 The Mercury team.
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

    % search_for_module_source(Globals, Dirs, InterfaceDirs,
    %   ModuleName, FoundSourceFile, !IO):
    %
    % Look for the source for ModuleName in Dirs. If found, return the
    % (relative or absolute) path name of the source file that contains
    % the module.
    %
    % This will also search for files matching partially qualified versions
    % of ModuleName, dropping qualifiers outermost to innermost, but only if
    % a more qualified `.m' or `.int' file doesn't exist in InterfaceDirs.
    % For example, module foo.bar.baz can be found in foo.bar.m, bar.baz.m
    % or bar.m.
    %
:- pred search_for_module_source(globals::in, list(dir_name)::in,
    list(dir_name)::in, module_name::in,
    maybe_error(file_name)::out, io::di, io::uo) is det.

    % search_for_module_source_and_stream(Globals, Dirs, InterfaceDirs,
    %   ModuleName, FoundSourceFileNameAndStream, !IO):
    %
    % As search_for_module_source, but if we find the file, then return
    % not just its path name, but also an open stream reading from it.
    % Closing that stream is the caller's responsibility.
    %
:- pred search_for_module_source_and_stream(globals::in, list(dir_name)::in,
    list(dir_name)::in, module_name::in,
    maybe_error(path_name_and_stream)::out, io::di, io::uo) is det.

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

search_for_module_source(Globals, Dirs, InterfaceDirs, ModuleName,
        MaybeFileName, !IO) :-
    search_for_module_source_and_stream(Globals, Dirs, InterfaceDirs,
        ModuleName, MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream =
            ok(path_name_and_stream(SourceFileName, SourceStream)),
        io.close_input(SourceStream, !IO),
        MaybeFileName = ok(SourceFileName)
    ;
        MaybeFileNameAndStream = error(Msg),
        MaybeFileName = error(Msg)
    ).

search_for_module_source_and_stream(Globals, Dirs, InterfaceDirs, ModuleName,
        MaybeFileNameAndStream, !IO) :-
    search_for_module_source_dropping_qualifiers(Globals,
        Dirs, ModuleName, MaybeFileNameAndStream0, !IO),
    (
        MaybeFileNameAndStream0 =
            ok(path_name_and_stream(SourceFileName, SourceStream)),
        ( if
            string.remove_suffix(dir.basename(SourceFileName),
                ".m", SourceFileBaseName),
            file_name_to_module_name(SourceFileBaseName, SourceFileModuleName),
            ModuleName \= SourceFileModuleName
        then
            % The module name doesn't match the file name. Return an error
            % if there is a more qualified matching `.m' or `.int' file in
            % the interface search path. This avoids having a file `read.m'
            % in the current directory prevent the compiler from finding
            % `bit_buffer.read.int' in the standard library.
            % Note that we never need to read from this more qualified file.

            search_for_module_source_dropping_qualifiers(Globals,
                InterfaceDirs, ModuleName, MaybeIFaceFileNameAndStream, !IO),
            (
                MaybeIFaceFileNameAndStream =
                    ok(path_name_and_stream(_IFaceFileName, IFaceStream)),
                io.close_input(IFaceStream, !IO)
            ;
                MaybeIFaceFileNameAndStream = error(_)
            ),
            ( if
                MaybeIFaceFileNameAndStream =
                    ok(path_name_and_stream(IFaceFileName, _IFaceStream)),
                IFaceFileName \= SourceFileName,
                string.remove_suffix(dir.basename(IFaceFileName), ".m",
                    IFaceFileBaseName),
                file_name_to_module_name(IFaceFileBaseName,
                    IFaceFileModuleName),
                partial_sym_name_matches_full(SourceFileModuleName,
                    IFaceFileModuleName)
            then
                io.close_input(SourceStream, !IO),
                Error =
                    find_source_error(ModuleName, Dirs, yes(IFaceFileName)),
                MaybeFileNameAndStream = error(Error)
            else
                module_name_to_file_name(Globals, do_not_create_dirs, ".int",
                    ModuleName, IntFile, !IO),
                search_for_file_returning_dir(InterfaceDirs, IntFile,
                    MaybeIntDir, !IO),
                ( if
                    MaybeIntDir = ok(IntDir),
                    IntDir \= dir.this_directory
                then
                    io.close_input(SourceStream, !IO),
                    Error = find_source_error(ModuleName, Dirs,
                        yes(IntDir/IntFile)),
                    MaybeFileNameAndStream = error(Error)
                else
                    MaybeFileNameAndStream = MaybeFileNameAndStream0
                )
            )
        else
            MaybeFileNameAndStream = MaybeFileNameAndStream0
        )
    ;
        MaybeFileNameAndStream0 = error(_),
        MaybeFileNameAndStream = MaybeFileNameAndStream0
    ).

%------------%

:- pred search_for_module_source_dropping_qualifiers(globals::in,
    list(dir_name)::in, module_name::in,
    maybe_error(path_name_and_stream)::out, io::di, io::uo) is det.

search_for_module_source_dropping_qualifiers(Globals, Dirs, ModuleName,
        MaybeErrorFileNameAndStream, !IO) :-
    search_for_module_source_dropping_qualifiers_loop(Globals, Dirs,
        ModuleName, MaybeFileNameAndStream, !IO),
    (
        MaybeFileNameAndStream = yes(FileNameAndStream),
        MaybeErrorFileNameAndStream = ok(FileNameAndStream)
    ;
        MaybeFileNameAndStream = no,
        Error = find_source_error(ModuleName, Dirs, no),
        MaybeErrorFileNameAndStream = error(Error)
    ).

:- pred search_for_module_source_dropping_qualifiers_loop(globals::in,
    list(dir_name)::in, module_name::in, maybe(path_name_and_stream)::out,
    io::di, io::uo) is det.

search_for_module_source_dropping_qualifiers_loop(Globals, Dirs,
        PartialModuleName0, MaybeFileNameAndStream, !IO) :-
    module_name_to_file_name(Globals, do_not_create_dirs, ".m",
        PartialModuleName0, FileName0, !IO),
    search_for_file_and_stream(Dirs, FileName0, MaybeFileNameAndStream0, !IO),
    (
        MaybeFileNameAndStream0 = ok(FileNameAndStream0),
        MaybeFileNameAndStream = yes(FileNameAndStream0)
    ;
        MaybeFileNameAndStream0 = error(_),
        ( if
            drop_outermost_qualifier(PartialModuleName0, PartialModuleName1)
        then
            search_for_module_source_dropping_qualifiers_loop(Globals, Dirs,
                PartialModuleName1, MaybeFileNameAndStream, !IO)
        else
            MaybeFileNameAndStream = no
        )
    ).

%------------%

:- pred drop_outermost_qualifier(module_name::in, module_name::out) is semidet.

drop_outermost_qualifier(SymName, DroppedQualifierSymName) :-
    SymName = qualified(ParentModuleName, ChildName), 
    drop_outermost_qualifier_loop(ParentModuleName, ChildName,
        DroppedQualifierSymName).

:- pred drop_outermost_qualifier_loop(module_name::in, string::in,
    module_name::out) is det.

drop_outermost_qualifier_loop(ParentModuleName, ChildName,
        DroppedQualifierSymName) :-
    (
        ParentModuleName = unqualified(_ParentName),
        DroppedQualifierSymName = unqualified(ChildName)
    ;
        ParentModuleName = qualified(GrandParentModuleName, ParentName),
        drop_outermost_qualifier_loop(GrandParentModuleName, ParentName,
            DroppedQualifierGrandParentModuleName),
        DroppedQualifierSymName =
            qualified(DroppedQualifierGrandParentModuleName, ChildName)
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
