%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2002-2012 The University of Melbourne.
% Copyright (C) 2013-2017, 2019-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: make.clean.m.
%
% Build targets which clean up.
%
%---------------------------------------------------------------------------%

:- module make.clean.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module make.make_info.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module bool.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred maybe_make_grade_clean(io.text_output_stream::in, globals::in,
    bool::in, module_name::in, list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

:- pred make_main_module_realclean(io.text_output_stream::in, globals::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred make_module_clean(io.text_output_stream::in, globals::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

:- pred make_module_realclean(io.text_output_stream::in, globals::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred remove_init_files(io.text_output_stream::in, globals::in, option::in,
    module_name::in, make_info::in, make_info::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.compile_target_code.
:- import_module libs.file_util.
:- import_module make.get_module_dep_info.
:- import_module make.util.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_dep_info.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_foreign.

:- import_module set.
:- import_module string.

%---------------------------------------------------------------------------%

maybe_make_grade_clean(ProgressStream, Globals, Clean, ModuleName, AllModules,
        !Info, !IO) :-
    (
        Clean = yes,
        make_grade_clean(ProgressStream, Globals, ModuleName, AllModules,
            !Info, !IO)
    ;
        Clean = no
    ).

    % Clean up grade-dependent files.
    %
:- pred make_grade_clean(io.text_output_stream::in, globals::in,
    module_name::in, list(module_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

make_grade_clean(ProgressStream, Globals, ModuleName, AllModules,
        !Info, !IO) :-
    globals.get_grade_dir(Globals, Grade),
    % XXX MAKE_EXTRA_PERIOD
    string.format("Cleaning up grade-dependent files for `%s' in grade %s.",
        [s(escaped_sym_name_to_string(ModuleName)), s(Grade)], Part1),
    verbose_make_one_part_msg(Globals, Part1, CleaningMsg),
    maybe_write_msg(ProgressStream, CleaningMsg, !IO),

    make_main_module_realclean(ProgressStream, Globals,
        ModuleName, !Info, !IO),
    list.foldl2(make_module_clean(ProgressStream, Globals),
        AllModules, !Info, !IO).

make_main_module_realclean(ProgressStream, Globals, ModuleName, !Info, !IO) :-
    % XXX MAKE_EXTRA_PERIOD
    string.format("Removing executable and library files for `%s'.",
        [s(escaped_sym_name_to_string(ModuleName))], Part1),
    verbose_make_one_part_msg(Globals, Part1, CleaningMsg),
    maybe_write_msg(ProgressStream, CleaningMsg, !IO),

    LinkedTargetTypes = [
        executable,
        static_library,
        shared_library,
        csharp_executable,
        csharp_library,
        java_executable,
        java_archive
    ],
    list.map2_foldl(linked_target_file_name_full_curdir(Globals, ModuleName),
        LinkedTargetTypes, FileNames, CurDirFileNames, !IO),
    % Remove the symlinks created for `--use-grade-subdirs'.
    % XXX This symlink should not be necessary anymore for `mmc --make'.
    % XXX LEGACY
    module_name_to_file_name_full_curdir(Globals, $pred,
        ext_cur_gs(ext_cur_gs_lib_init), ModuleName,
        FullInitFileNameLegacy, FullInitFileNameProposed, CurDirInitFileName),
    FilesToRemove = FileNames ++ CurDirFileNames ++
        [FullInitFileNameLegacy, FullInitFileNameProposed, CurDirInitFileName],
    list.foldl2(remove_file_for_make(ProgressStream, Globals, very_verbose),
        FilesToRemove, !Info, !IO),
    remove_init_files(ProgressStream, Globals, very_verbose, ModuleName,
        !Info, !IO).

%---------------------------------------------------------------------------%

make_module_clean(ProgressStream, Globals, ModuleName, !Info, !IO) :-
    % XXX MAKE_EXTRA_PERIOD
    string.format("Cleaning up target files for module `%s'.",
        [s(escaped_sym_name_to_string(ModuleName))], Part1),
    verbose_make_one_part_msg(Globals, Part1, CleaningMsg),
    maybe_write_msg(ProgressStream, CleaningMsg, !IO),

    list.foldl2(
        remove_make_target_file_by_name(ProgressStream, Globals, $pred,
            very_verbose, ModuleName),
        [module_target_errors,
        module_target_c_code,
        module_target_c_header(header_mih),
        module_target_csharp_code,
        module_target_java_code,
        module_target_java_class_code], !Info, !IO),

    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs_gs(ext_cur_ngs_gs_misc_used), !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs(ext_cur_ngs_misc_call_graph_for_prof),
        !Info, !IO),

    get_maybe_module_dep_info(ProgressStream, Globals,
        ModuleName, MaybeModuleDepInfo, !Info, !IO),
    (
        MaybeModuleDepInfo = some_module_dep_info(ModuleDepInfo),
        module_dep_info_get_fact_tables(ModuleDepInfo, FactTableFilesSet),
        set.to_sorted_list(FactTableFilesSet, FactTableFiles)
    ;
        MaybeModuleDepInfo = no_module_dep_info,
        FactTableFiles = []
    ),

    list.foldl2(remove_fact_table_c_file(ProgressStream, Globals),
        FactTableFiles, !Info, !IO),

    foreign_language_module_name(ModuleName, lang_c, CCodeModule),
    remove_make_target_file_by_name(ProgressStream, Globals, $pred,
        very_verbose, CCodeModule, module_target_c_code, !Info, !IO),

    remove_object_and_assembler_files(ProgressStream, Globals,
        ModuleName, pic, FactTableFiles, !Info, !IO),
    remove_object_and_assembler_files(ProgressStream, Globals,
        ModuleName, non_pic, FactTableFiles, !Info, !IO).

%---------------------------------------------------------------------------%

make_module_realclean(ProgressStream, Globals, ModuleName, !Info, !IO) :-
    make_module_clean(ProgressStream, Globals, ModuleName, !Info, !IO),

    string.format("Cleaning up interface files for module `%s'",
        [s(escaped_sym_name_to_string(ModuleName))], Part1),
    verbose_make_one_part_msg(Globals, Part1, CleaningMsg),
    maybe_write_msg(ProgressStream, CleaningMsg, !IO),
    Targets = [module_target_int0, module_target_int1, module_target_int2,
        module_target_int3, module_target_opt,
        module_target_analysis_registry,
        module_target_c_header(header_mh),
        module_target_track_flags],
    list.foldl2(
        remove_make_target_file_by_name(ProgressStream, Globals, $pred,
            very_verbose, ModuleName),
        Targets, !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs(ext_cur_ngs_misc_module_dep),
        !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_imdg),
        !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, very_verbose,
        ModuleName, ext_cur_ngs_gs_max_ngs(ext_cur_ngs_gs_max_ngs_an_request),
        !Info, !IO).

%---------------------------------------------------------------------------%

:- pred remove_fact_table_c_file(io.text_output_stream::in, globals::in,
    string::in, make_info::in, make_info::out, io::di, io::uo) is det.

remove_fact_table_c_file(ProgressStream, Globals, FactTableFile, !Info, !IO) :-
    % XXX LEGACY
    fact_table_file_name_return_dirs(Globals, $pred,
        ext_cur_ngs_gs(ext_cur_ngs_gs_target_c),
        FactTableFile, _FactTableDirsLegacy, _FactTableDirsProposed,
        FactTableCFileLegacy, FactTableCFileProposed),
    remove_file_for_make(ProgressStream, Globals, very_verbose,
        FactTableCFileLegacy, !Info, !IO),
    remove_file_for_make(ProgressStream, Globals, very_verbose,
        FactTableCFileProposed, !Info, !IO).

:- pred remove_object_and_assembler_files(io.text_output_stream::in,
    globals::in, module_name::in, pic::in, list(file_name)::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_object_and_assembler_files(ProgressStream, Globals, ModuleName, PIC,
        FactTableFiles, !Info, !IO) :-
    remove_make_target_file_by_name(ProgressStream, Globals, $pred,
        very_verbose, ModuleName, module_target_object_code(PIC), !Info, !IO),
    list.foldl2(
        remove_fact_table_object_and_assembler_files(ProgressStream, Globals,
            ModuleName, PIC),
        FactTableFiles, !Info, !IO).

:- pred remove_fact_table_object_and_assembler_files(io.text_output_stream::in,
    globals::in, module_name::in, pic::in, file_name::in,
    make_info::in, make_info::out, io::di, io::uo) is det.

remove_fact_table_object_and_assembler_files(ProgressStream, Globals,
        ModuleName, PIC, FactTableFile, !Info, !IO) :-
    remove_make_target_file_by_name(ProgressStream, Globals, $pred,
        very_verbose, ModuleName,
        module_target_fact_table_object(PIC, FactTableFile), !Info, !IO).

remove_init_files(ProgressStream, Globals, Verbose, ModuleName, !Info, !IO) :-
    remove_module_file_for_make(ProgressStream, Globals, Verbose, ModuleName,
        ext_cur_ngs_gs(ext_cur_ngs_gs_init_c), !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, Verbose, ModuleName,
        ext_cur_ngs_gas(ext_cur_ngs_gas_init_obj_obj_opt), !Info, !IO),
    remove_module_file_for_make(ProgressStream, Globals, Verbose, ModuleName,
        ext_cur_ngs_gas(ext_cur_ngs_gas_init_obj_pic_obj_opt), !Info, !IO).

%---------------------------------------------------------------------------%
:- end_module make.clean.
%---------------------------------------------------------------------------%
