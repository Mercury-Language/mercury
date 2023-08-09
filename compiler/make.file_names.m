%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.file_names.m.
% Authors: stayl, wangp.
%
% Predicates dealing with file names, used to implement `mmc --make'.
%
%---------------------------------------------------------------------------%

:- module make.file_names.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module make.dependencies.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.file_names.

:- import_module io.

%---------------------------------------------------------------------------%
%
% File names.
%

    % get_file_name(Globals, From, Search, TargetFile, FileName, !IO):
    %
    % Compute a file name for the given target file.
    % `Search' should be `for_search' if the file could be part of an
    % installed library.
    %
:- pred get_file_name(globals::in, string::in, maybe_for_search::in,
    target_file::in, file_name::out,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- pred dependency_file_to_file_name(globals::in, dependency_file::in,
    string::out, io::di, io::uo) is det.

    % Return the file name for the given target_file. The I/O state pair
    % may be needed to find this file name.
    %
:- pred get_make_target_file_name(globals::in, string::in,
    target_file::in, string::out, io::di, io::uo) is det.

:- pred module_target_to_file_name(globals::in, string::in,
    module_target_type::in, module_name::in, file_name::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Extensions on file names.
%

:- pred extension_to_target_type(globals::in, string::in,
    module_target_type::out) is semidet.

:- pred target_extension_synonym(string::in, module_target_type::out)
    is semidet.

    % Find the extension for the timestamp file for the given target type,
    % if one exists.
    %
:- pred timestamp_extension(module_target_type::in, ext::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module make.module_dep_file.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.prog_foreign.

:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

get_file_name(Globals, From, Search, TargetFile, FileName, !Info, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    ( if TargetType = module_target_source then
        % In some cases the module name won't match the file name
        % (module mdb.parse might be in parse.m or mdb.m), so we need to
        % look up the file name here.
        get_module_dependencies(Globals, ModuleName, MaybeModuleDepInfo,
            !Info, !IO),
        (
            MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
            module_dep_info_get_source_file_name(ModuleDepInfo, FileName)
        ;
            MaybeModuleDepInfo = no_module_dep_info,
            % Something has gone wrong generating the dependencies,
            % so just take a punt (which probably won't work).
            module_name_to_source_file_name(ModuleName, FileName, !IO)
        )
    else
        target_type_to_target_extension(TargetType, TargetExt),
        (
            TargetExt = source,
            module_name_to_source_file_name(ModuleName, FileName, !IO)
        ;
            TargetExt = extension(Ext),
            (
                Search = not_for_search,
                module_name_to_file_name(Globals, From, Ext,
                    ModuleName, FileName)
            ;
                Search = for_search,
                module_name_to_search_file_name(Globals, From, Ext,
                    ModuleName, FileName)
            )
        ;
            ( TargetExt = foreign_obj(_, _)
            ; TargetExt = fact_table_obj(_, _)
            ),
            (
                Search = not_for_search,
                module_target_to_file_name(Globals, From, TargetType,
                    ModuleName, FileName, !IO)
            ;
                Search = for_search,
                module_target_to_search_file_name(Globals, From, TargetType,
                    ModuleName, FileName, !IO)
            )
        )
    ).

dependency_file_to_file_name(Globals, DepFile, FileName, !IO) :-
    (
        DepFile = dep_target(TargetFile),
        get_make_target_file_name(Globals, $pred, TargetFile, FileName, !IO)
    ;
        DepFile = dep_file(FileName)
    ).

get_make_target_file_name(Globals, From, TargetFile, FileName, !IO) :-
    TargetFile = target_file(ModuleName, TargetType),
    module_target_to_file_name(Globals, From, TargetType, ModuleName,
        FileName, !IO).

module_target_to_file_name(Globals, From, TargetType, ModuleName,
        FileName, !IO) :-
    target_type_to_target_extension(TargetType, TargetExt),
    (
        TargetExt = source,
        module_name_to_source_file_name(ModuleName, FileName, !IO)
    ;
        TargetExt = extension(Ext),
        module_name_to_file_name(Globals, From, Ext,
            ModuleName, FileName)
    ;
        TargetExt = foreign_obj(PIC, Lang),
        foreign_language_module_name(ModuleName, Lang, ForeignModuleName),
        module_target_to_file_name(Globals, From,
            module_target_object_code(PIC),
            ForeignModuleName, FileName, !IO)
    ;
        TargetExt = fact_table_obj(PIC, FactFile),
        maybe_pic_object_file_extension(PIC, ObjExt, _),
        fact_table_file_name_return_dirs(Globals, $pred,
            ext_target_obj(ObjExt), FactFile, _FactDirs, FileName)
    ).

:- pred module_target_to_search_file_name(globals::in, string::in,
    module_target_type::in,
    module_name::in, file_name::out, io::di, io::uo) is det.

module_target_to_search_file_name(Globals, From, TargetType, ModuleName,
        FileName, !IO) :-
    target_type_to_target_extension(TargetType, TargetExt),
    (
        TargetExt = source,
        % XXX This call ignores the implicit for_search setting.
        module_name_to_source_file_name(ModuleName, FileName, !IO)
    ;
        TargetExt = extension(Ext),
        module_name_to_search_file_name(Globals, From, Ext,
            ModuleName, FileName)
    ;
        TargetExt = foreign_obj(PIC, Lang),
        foreign_language_module_name(ModuleName, Lang, ForeignModuleName),
        module_target_to_search_file_name(Globals, From,
            module_target_object_code(PIC), ForeignModuleName, FileName, !IO)
    ;
        TargetExt = fact_table_obj(PIC, FactFile),
        maybe_pic_object_file_extension(PIC, ObjExt, _),
        % XXX This call ignores the implicit for_search setting.
        fact_table_file_name_return_dirs(Globals, $pred,
            ext_target_obj(ObjExt), FactFile, _FactDirs, FileName)
    ).

%---------------------------------------------------------------------------%

:- type target_extension
    --->    source
    ;       extension(ext)
    ;       foreign_obj(pic, foreign_language)
    ;       fact_table_obj(pic, string).

:- pred target_type_to_target_extension(module_target_type::in,
    target_extension::out) is det.

target_type_to_target_extension(Target, TargetExt) :-
    % target_type_to_extension and extension_to_target_type represent
    % the same relationship between targets and suffixes, but in different
    % directions. Their codes should be kept in sync.
    require_complete_switch [Target]
    (
        Target = module_target_source,
        TargetExt = source
    ;
        Target = module_target_errors,
        TargetExt = extension(ext_cur(ext_cur_user_err))
    ;
        Target = module_target_int0,
        TargetExt = extension(ext_int(ext_int_int0))
    ;
        Target = module_target_int1,
        TargetExt = extension(ext_int(ext_int_int1))
    ;
        Target = module_target_int2,
        TargetExt = extension(ext_int(ext_int_int2))
    ;
        Target = module_target_int3,
        TargetExt = extension(ext_int(ext_int_int3))
    ;
        Target = module_target_opt,
        TargetExt = extension(ext_opt(ext_opt_plain))
    ;
        Target = module_target_analysis_registry,
        TargetExt = extension(ext_analysis(ext_an_analysis))
    ;
        Target = module_target_track_flags,
        TargetExt = extension(ext_misc_gs(ext_misc_gs_track_flags))
    ;
        Target = module_target_c_header(header_mih),
        TargetExt = extension(ext_mih(ext_mih_mih))
    ;
        Target = module_target_c_header(header_mh),
        TargetExt = extension(ext_cur(ext_cur_mh))
    ;
        Target = module_target_c_code,
        TargetExt = extension(ext_target_c_cs(ext_target_c))
    ;
        Target = module_target_csharp_code,
        % XXX ".exe" if the module contains main.
        TargetExt = extension(ext_target_c_cs(ext_target_cs))
    ;
        Target = module_target_java_code,
        TargetExt = extension(ext_target_java(ext_target_java_java))
    ;
        Target = module_target_java_class_code,
        TargetExt = extension(ext_target_java(ext_target_java_class))
    ;
        Target = module_target_object_code(PIC),
        maybe_pic_object_file_extension(PIC, ObjExt, _),
        TargetExt = extension(ext_target_obj(ObjExt))
    ;
        Target = module_target_xml_doc,
        TargetExt = extension(ext_user_ngs(ext_user_ngs_xml))
    ;
        Target = module_target_foreign_object(PIC, Lang),
        TargetExt = foreign_obj(PIC, Lang)
    ;
        Target = module_target_fact_table_object(PIC, FactFile),
        TargetExt = fact_table_obj(PIC, FactFile)
    ).

extension_to_target_type(Globals, ExtStr, Target) :-
    % target_type_to_extension and extension_to_target_type represent
    % the same relationship between targets and suffixes, but in different
    % directions. Their codes should be kept in sync.
    ( if
        (
            ExtStr = ".m",
            TargetPrime = module_target_source
        ;
            ExtStr = ".err",
            TargetPrime = module_target_errors
        ;
            ExtStr = ".int0",
            TargetPrime = module_target_int0
        ;
            ExtStr = ".int",
            TargetPrime = module_target_int1
        ;
            ExtStr = ".int2",
            TargetPrime = module_target_int2
        ;
            ExtStr = ".int3",
            TargetPrime = module_target_int3
        ;
            ExtStr = ".opt",
            TargetPrime = module_target_opt
        ;
            ExtStr = ".mih",
            TargetPrime = module_target_c_header(header_mih)
        ;
            ExtStr = ".mh",
            TargetPrime = module_target_c_header(header_mh)
        ;
            ExtStr = ".c",
            TargetPrime = module_target_c_code
        ;
            ExtStr = ".cs",
            TargetPrime = module_target_csharp_code
        ;
            ExtStr = ".java",
            TargetPrime = module_target_java_code
        ;
            ExtStr = ".class",
            TargetPrime = module_target_java_class_code
        ;
            ExtStr = ".track_flags",
            TargetPrime = module_target_track_flags
        ;
            ExtStr = ".xml",
            TargetPrime = module_target_xml_doc
        ;
            ExtStr = ".analysis",
            TargetPrime = module_target_analysis_registry
        )
    then
        Target = TargetPrime
    else if
        is_maybe_pic_object_file_extension(Globals, ExtStr, PIC)
    then
        Target = module_target_object_code(PIC)
    else
        fail
    ).

target_extension_synonym(".csharp", module_target_csharp_code).
    % Currently the ".cs" extension is still treated as the build-all target
    % for C files, so we accept ".csharp" for C# files.

timestamp_extension(ModuleTargetType, Ext) :-
    % XXX EXT The absence of code handling .trans_opt_date files
    % would seem to me (zs) to be a bug.
    (
        ModuleTargetType = module_target_errors,
        % We need a timestamp file for `.err' files because errors are written
        % to the `.err' file even when writing interfaces. The timestamp
        % is only updated when compiling to target code.
        Ext = ext_misc_ngs(ext_misc_ngs_err_date)
    ;
        ModuleTargetType = module_target_int0,
        Ext = ext_int(ext_int_date_int0)
    ;
        ModuleTargetType = module_target_int1,
        Ext = ext_int(ext_int_date_int12)
    ;
        ModuleTargetType = module_target_int2,
        Ext = ext_int(ext_int_date_int12)
    ;
        ModuleTargetType = module_target_int3,
        Ext = ext_int(ext_int_date_int3)
    ;
        ModuleTargetType = module_target_opt,
        Ext = ext_opt_date(ext_opt_date_plain)
    ;
        ModuleTargetType = module_target_analysis_registry,
        % We need a timestamp file for `.analysis' files because they
        % can be modified in the process of analysing _another_ module.
        % The timestamp is only updated after actually analysing the module
        % that the `.analysis' file corresponds to.
        Ext = ext_analysis_ds(ext_an_ds_date)
    ;
        % Header files share a timestamp file with their corresponding
        % target code files.
        ( ModuleTargetType = module_target_c_code
        ; ModuleTargetType = module_target_c_header(_)
        ),
        Ext = ext_target_date(ext_target_date_c)
    ;
        ModuleTargetType = module_target_csharp_code,
        Ext = ext_target_date(ext_target_date_cs)
    ;
        ModuleTargetType = module_target_java_code,
        Ext = ext_target_date(ext_target_date_java)
    ).

%---------------------------------------------------------------------------%
:- end_module make.file_names.
%---------------------------------------------------------------------------%
