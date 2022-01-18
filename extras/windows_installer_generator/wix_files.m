%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% Copyright (C) 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: Ian MacLarty (maclarty@cs.mu.oz.au).
%
%---------------------------------------------------------------------------%
%
% This module is responsible for telling Wix what files and shortcuts to
% install.
% Each file corresponds to a `Component' element in the Wix source file.
% Each component must have a unique GUID.
% Components are grouped together under Features.  Wix allows Features to
% be installed independently, however the current implementation of this
% Wix source generator always installs all the Features.
%
%---------------------------------------------------------------------------%

:- module wix_files.

:- interface.

:- import_module io.
:- import_module list.
:- import_module term_to_xml.

:- import_module wix.
:- import_module wix_util.

:- type file(L)
    --->    file(
                filename    :: string,
                    % A list of shortcuts that should be
                    % created to the file.
                shortcuts   :: list(shortcut(L))
            )
    ;       directory(
                dir_name    :: string,
                dir_contents    :: list(file(L))
            ).

    % Files are converted to annotated files and then to XML.
    % annotated files contain more information, such as the Id and
    % GUID of each file.
    %
:- type annotated_file.

:- func annotated_file_to_xml(annotated_file) = xml.
:- mode annotated_file_to_xml(in) = (out(xml_doc)) is det.

    % Assign Ids and GUIDs to the files and their descendents.
    %
:- pred annotate_files(language::in, list(file(L))::in,
    id_supply::in, id_supply::out,
    io.input_stream::in, string::in, list(annotated_file)::out,
    io::di, io::uo) is det <= language_independent_tokens(L).

:- pred gen_files(string::in, shortcut_function(L)::in,
    list(file(L))::out, io::di, io::uo) is det.

    % Generate Feature elements containing the given components.
    % Multiple features will be generated if there are more than 800
    % components.  This is to get round a limitation of Windows 9X where
    % each feature may have no more that 817 components.
    %
:- func generate_feature_elements(list(annotated_file)) = list(xml).

    % Succeed if there are any shortcuts from the Programs menu to any
    % of the given files.
    %
:- pred is_shortcut_from_programs_menu(list(annotated_file)::in) is semidet.

:- pred gen_guid(io.input_stream::in, guid::out, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module string.

:- import_module wix_language.

%----------------------------------------------------------------------------%

:- type annotated_file
    --->    annotated_file(
                ann_comp_id     :: id,
                ann_comp_guid       :: guid,
                ann_file_id     :: id,
                ann_filename        :: string,
                ann_path        :: string,
                ann_shortcuts       :: list(annotated_shortcut)
            )
    ;       annotated_directory(
                ann_dir_id  :: id,
                ann_dir_name    :: string,
                ann_contents    :: list(annotated_file)
            ).

:- type annotated_shortcut
    --->    annotated_shortcut(
                ann_shortcut_id     :: id,
                ann_shortcut_where  :: shortcut_where,
                ann_shortcut_name   :: string
            ).

gen_files(Dir, ShortCuts, Files, !IO) :-
    dir.foldl2(add_file(ShortCuts), Dir, [], MaybeFiles, !IO),
    (
        MaybeFiles = ok(Files)
    ;
        MaybeFiles = error(_, Err),
        throw(io_error(Err))
    ).

:- pred add_file(shortcut_function(L)::in, string::in, string::in,
    io.file_type::in, bool::out,
    list(file(L))::in, list(file(L))::out,
    io::di, io::uo) is det.

add_file(ShortCutsMap, DirName, BaseName, FileType, yes, !Files, !IO) :-
    ( if FileType = directory then
        gen_files(DirName ++ dir_sep ++ BaseName, ShortCutsMap,
            FilesInDir, !IO),
        File = directory(BaseName, FilesInDir),
        !:Files = [File | !.Files]
    else
        ShortCutsMap(DirName, BaseName) = ShortCuts,
        !:Files = [file(BaseName, ShortCuts) | !.Files]
    ).

:- pred annotate_file(language::in, file(L)::in, id_supply::in, id_supply::out,
    io.input_stream::in, string::in, annotated_file::out,
    io::di, io::uo) is det <= language_independent_tokens(L).

annotate_file(Language, file(Name, ShortCuts), !IdSupply, GUIDFile, Path,
        AnnotatedFile, !IO) :-
    allocate_id(ComponentId, !IdSupply),
    allocate_id(FileId, !IdSupply),
    annotate_shortcuts(Language, ShortCuts, AnnShortCuts, !IdSupply),
    gen_guid(GUIDFile, GUID, !IO),
    AnnotatedFile = annotated_file(ComponentId, GUID,
        FileId, Name, Path, AnnShortCuts).
annotate_file(Language, directory(Name, Files), !IdStore, GUIDGenCmd, Path0,
        AnnotatedDir, !IO) :-
    allocate_id(DirId, !IdStore),
    Path = Path0 ++ dir_sep ++ Name,
    annotate_files(Language, Files, !IdStore, GUIDGenCmd, Path,
        AnnotatedFiles, !IO),
    AnnotatedDir = annotated_directory(DirId, Name, AnnotatedFiles).

annotate_files(_, [], !IdStore, _, _, [], !IO).
annotate_files(Language, [File | Files], !IdStore, GUIDStream, Path,
        [AnnFile | AnnFiles], !IO) :-
    annotate_file(Language, File, !IdStore, GUIDStream, Path, AnnFile,
        !IO),
    annotate_files(Language, Files, !IdStore, GUIDStream, Path, AnnFiles,
        !IO).

:- pred annotate_shortcuts(language::in, list(shortcut(L))::in,
    list(annotated_shortcut)::out, id_supply::in, id_supply::out) is det
    <= language_independent_tokens(L).

annotate_shortcuts(_, [], [], !IdSupply).
annotate_shortcuts(Language, [ShortCut | ShortCuts], AnnShortCuts, !IdSupply)
        :-
    ShortCut = shortcut(Where, Name),
    annotate_shortcuts(Language, ShortCuts, Rest, !IdSupply),
    det_translate(Name, Language, TranslatedName),
    allocate_id(ShortCutId, !IdSupply),
    AnnShortCuts = [annotated_shortcut(ShortCutId,
        Where, TranslatedName) | Rest].

:- instance xmlable(annotated_file) where [
    func(to_xml/1) is annotated_file_to_xml
].

annotated_file_to_xml(annotated_file(CompId, GUID, FileId, Name, Path,
        ShortCuts)) =
    elem("Component", [
            id_attr(CompId),
            guid_attr(GUID)
        ],
            [elem("File", [id_attr(FileId)] ++
                name_attrs(Name, FileId) ++
                [attr("src", Path ++ dir_sep ++ Name)]
                ++ [disk_id_attr],
                list.map(annotated_shortcut_to_xml,
                    list.sort_and_remove_dups(ShortCuts))
            )]
        ).
annotated_file_to_xml(annotated_directory(DirId, Name, Files)) =
    elem("Directory", [id_attr(DirId)] ++ name_attrs(Name, DirId),
        list.map(annotated_file_to_xml, Files)).

:- pred file_is_directory(annotated_file::in) is semidet.

file_is_directory(annotated_directory(_, _, _)).

:- func component_refs(annotated_file) = list(xml).

component_refs(annotated_file(CompId, _, _, _, _, _)) =
    [elem("ComponentRef", [id_attr(CompId)], [])].
component_refs(annotated_directory(_, _, Files)) =
    list.condense(list.map(component_refs, Files)).

:- func annotated_shortcut_to_xml(annotated_shortcut) = xml.

annotated_shortcut_to_xml(annotated_shortcut(Id, Where, Name)) =
    elem("Shortcut", [
        id_attr(Id),
        shortcut_where_attr(Where)] ++
        name_attrs(Name, Id), []).

generate_feature_elements(Contents) = XML :-
    % Win9X can't handle features with more than 817 components.
    generate_component_list(Contents, [], Components),
    list.chunk(Components, 800, ComponentChunks),
    XML = generate_feature_chunks(1, ComponentChunks).

:- pred generate_component_list(list(annotated_file)::in,
    list(annotated_file)::in, list(annotated_file)::out) is det.

generate_component_list([], !Components).
generate_component_list([File | Files], Components0, Components) :-
    (
        File = annotated_file(_, _, _, _, _, _),
        Components1 = [File]
    ;
        File = annotated_directory(_, _, FilesInDir),
        generate_component_list(FilesInDir, [], Components1)
    ),
    generate_component_list(Files, Components0 ++ Components1, Components).

:- func generate_feature_chunks(int, list(list(annotated_file))) = list(xml).

generate_feature_chunks(_, []) = [].
generate_feature_chunks(FeatureNum, [Chunk | Chunks]) = XML :-
    XML =
        [elem("Feature", [
            id_attr("Feature" ++ int_to_string(FeatureNum)),
            attr("Level", "1")
        ],
            ( if FeatureNum = 1 then
                [elem("ComponentRef", [id_attr(env_vars_component_id)], [])]
            else
                []
            ) ++
            list.condense(list.map(component_refs, Chunk))
        )] ++
        generate_feature_chunks(FeatureNum + 1, Chunks).

is_shortcut_from_programs_menu(
        [annotated_file(_, _, _, _, _, ShortCuts) | Files]) :-
    (
        is_shortcut_from_programs_menu_2(ShortCuts)
    ;
        is_shortcut_from_programs_menu(Files)
    ).
is_shortcut_from_programs_menu(
        [annotated_directory(_, _, MoreFiles) | Files]) :-
    (
        is_shortcut_from_programs_menu(MoreFiles)
    ;
        is_shortcut_from_programs_menu(Files)
    ).

:- pred is_shortcut_from_programs_menu_2(list(annotated_shortcut)::in)
    is semidet.

is_shortcut_from_programs_menu_2([annotated_shortcut(_, programs, _) | _]).
is_shortcut_from_programs_menu_2([_ | Rest]) :-
    is_shortcut_from_programs_menu_2(Rest).

gen_guid(GUIDFile, GUID, !IO) :-
    io.read_line_as_string(GUIDFile, Result, !IO),
    (
        Result = ok(GUID)
    ;
        Result = error(Err),
        throw(guid_gen_error(guid_io_error(Err)))
    ;
        Result = eof,
        throw(guid_gen_error(guid_eof))
    ).
