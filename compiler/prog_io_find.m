%-----------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------e
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% This module defines predicates that find Mercury modules.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_io_find.
:- interface.

:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module mdbcomp.sym_name.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % search_for_module_source(Globals, Dirs, InterfaceDirs, ModuleName,
    %   FoundSourceFileName, !IO):
    %
    % Look for the source for ModuleName in Dirs. This will also search for
    % files matching partially qualified versions of ModuleName, but only if
    % a more qualified `.m' or `.int' file doesn't exist in InterfaceDirs.
    % in InterfaceDirs. For example, module foo.bar.baz can be found in
    % foo.bar.m, bar.baz.m or bar.m.
    %
:- pred search_for_module_source(globals::in, list(dir_name)::in,
    list(dir_name)::in, module_name::in, maybe_error(file_name)::out,
    io::di, io::uo) is det.

    % Read the first item from the given file to find the module name.
    %
:- pred find_module_name(globals::in, file_name::in, maybe(module_name)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_io.        % for peek_at_file

:- import_module dir.
:- import_module string.

%-----------------------------------------------------------------------------%

search_for_module_source(Globals, Dirs, InterfaceDirs, ModuleName,
        MaybeFileName, !IO) :-
    InitialPartialModuleName = ModuleName,
    search_for_module_source_qualifier_loop(Globals, Dirs, ModuleName,
        InitialPartialModuleName, MaybeFileName0, !IO),
    (
        MaybeFileName0 = ok(SourceFileName),
        (
            string.remove_suffix(dir.basename(SourceFileName),
                ".m", SourceFileBaseName),
            file_name_to_module_name(SourceFileBaseName, SourceFileModuleName),
            ModuleName \= SourceFileModuleName
        ->
            % The module name doesn't match the file name. Return an error
            % if there is a more qualified matching `.m' or `.int' file in
            % the interface search path. This avoids having a file `read.m'
            % in the current directory prevent the compiler from finding
            % `bit_buffer.read.int' in the standard library.

            io.input_stream(SourceStream, !IO),
            search_for_module_source_qualifier_loop(Globals, InterfaceDirs,
                ModuleName, ModuleName, MaybeFileName2, !IO),
            ( MaybeFileName2 = ok(_) ->
                io.seen(!IO)
            ;
                true
            ),
            (
                MaybeFileName2 = ok(SourceFileName2),
                SourceFileName2 \= SourceFileName,
                string.remove_suffix(dir.basename(SourceFileName2), ".m",
                    SourceFileBaseName2),
                file_name_to_module_name(SourceFileBaseName2,
                    SourceFileModuleName2),
                match_sym_name(SourceFileModuleName, SourceFileModuleName2)
            ->
                io.close_input(SourceStream, !IO),
                MaybeFileName = error(find_source_error(ModuleName,
                    Dirs, yes(SourceFileName2)))
            ;
                module_name_to_file_name(Globals, ModuleName, ".int",
                    do_not_create_dirs, IntFile, !IO),
                search_for_file_returning_dir(do_not_open_file, InterfaceDirs,
                    IntFile, MaybeIntDir, !IO),
                (
                    MaybeIntDir = ok(IntDir),
                    IntDir \= dir.this_directory
                ->
                    io.close_input(SourceStream, !IO),
                    MaybeFileName = error(find_source_error(ModuleName,
                        Dirs, yes(IntDir/IntFile)))
                ;
                    io.set_input_stream(SourceStream, _, !IO),
                    MaybeFileName = MaybeFileName0
                )
            )
        ;
            MaybeFileName = MaybeFileName0
        )
    ;
        MaybeFileName0 = error(_),
        MaybeFileName = MaybeFileName0
    ).

:- pred search_for_module_source_qualifier_loop(globals::in,
    list(dir_name)::in, module_name::in, module_name::in,
    maybe_error(file_name)::out, io::di, io::uo) is det.

search_for_module_source_qualifier_loop(Globals, Dirs, ModuleName,
        PartialModuleName, MaybeFileName, !IO) :-
    module_name_to_file_name(Globals, PartialModuleName, ".m",
        do_not_create_dirs, FileName, !IO),
    search_for_file(open_file, Dirs, FileName, MaybeFileName0, !IO),
    (
        MaybeFileName0 = ok(_),
        MaybeFileName = MaybeFileName0
    ;
        MaybeFileName0 = error(_),
        ( PartialModuleName1 = drop_one_qualifier(PartialModuleName) ->
            search_for_module_source_qualifier_loop(Globals, Dirs, ModuleName,
                PartialModuleName1, MaybeFileName, !IO)
        ;
            MaybeFileName = error(find_source_error(ModuleName, Dirs, no))
        )
    ).

%-----------------------------------------------------------------------------%

:- func drop_one_qualifier(module_name) = module_name is semidet.

drop_one_qualifier(qualified(ParentQual, ChildName)) =
    drop_one_qualifier_2(ParentQual, ChildName).

:- func drop_one_qualifier_2(module_name, string) = module_name.

drop_one_qualifier_2(ParentQual, ChildName) =  PartialQual :-
    (
        ParentQual = unqualified(_ParentName),
        PartialQual = unqualified(ChildName)
    ;
        ParentQual = qualified(GrandParentQual, ParentName),
        PartialGrandParentQual = drop_one_qualifier_2(GrandParentQual,
            ParentName),
        PartialQual = qualified(PartialGrandParentQual, ChildName)
    ).

%-----------------------------------------------------------------------------%

:- func find_source_error(module_name, list(dir_name), maybe(file_name))
    = string.

find_source_error(ModuleName, Dirs, MaybeBetterMatch) = Msg :-
    ModuleNameStr = sym_name_to_string(ModuleName),
    Msg0 = "cannot find source for module `" ++ ModuleNameStr ++
        "' in directories " ++ string.join_list(", ", Dirs),
    (
        MaybeBetterMatch = no,
        Msg = Msg0
    ;
        MaybeBetterMatch = yes(BetterMatchFile),
        Msg = Msg0 ++ ", but found " ++ BetterMatchFile ++
            " in interface search path"
    ).

%-----------------------------------------------------------------------------%

find_module_name(Globals, FileName, MaybeModuleName, !IO) :-
    io.open_input(FileName, OpenRes, !IO),
    (
        OpenRes = ok(InputStream),
        io.set_input_stream(InputStream, OldInputStream, !IO),
        ( string.remove_suffix(FileName, ".m", PartialFileName0) ->
            PartialFileName = PartialFileName0
        ;
            PartialFileName = FileName
        ),
        ( dir.basename(PartialFileName, BaseName0) ->
            BaseName = BaseName0
        ;
            BaseName = ""
        ),
        file_name_to_module_name(BaseName, DefaultModuleName),
        peek_at_file(DefaultModuleName, FileName, ModuleName, Specs, !IO),
        MaybeModuleName = yes(ModuleName),
        % XXX We don't check whether ModuleName was actually read
        % from the named file; it could just be DefaultModuleName.
        % XXX _NumErrors
        write_error_specs(Specs, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
        io.set_input_stream(OldInputStream, _, !IO),
        io.close_input(InputStream, !IO)
    ;
        OpenRes = error(Error),
        ErrorMsg = io.error_message(Error),
        io.progname_base("mercury_compile", Progname, !IO),
        Pieces = [fixed(Progname), suffix(":"), words("error opening"),
            quote(FileName), suffix(":"), words(ErrorMsg), suffix("."), nl],
        Spec = error_spec(severity_error, phase_read_files,
            [error_msg(no, treat_as_first, 0, [always(Pieces)])]),
        % XXX _NumErrors
        write_error_spec(Spec, Globals, 0, _NumWarnings, 0, _NumErrors, !IO),
        MaybeModuleName = no
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_io_find.
%-----------------------------------------------------------------------------%
