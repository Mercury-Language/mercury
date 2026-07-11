%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: link_target_util.m.
%
% Types and operations needed by both link_target_code.m
% and link_target_code_c.m.
%
%---------------------------------------------------------------------------%

:- module backend_libs.link_target_util.
:- interface.

:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module bool.
:- import_module io.

%---------------------------------------------------------------------------%

:- type linked_target_type
    --->    c_executable
    ;       c_static_library
    ;       c_shared_library
    ;       csharp_executable
    ;       csharp_library
    ;       java_executable
    ;       java_archive.

:- inst c_linked_target_type for linked_target_type/0
    --->    c_executable
    ;       c_static_library
    ;       c_shared_library.

:- inst c_exe_or_shared_lib for linked_target_type/0
    --->    c_executable
    ;       c_shared_library.

:- inst csharp_linked_target_type for linked_target_type/0
    --->    csharp_executable
    ;       csharp_library.

:- inst java_linked_target_type for linked_target_type/0
    --->    java_executable
    ;       java_archive.

%---------------------------------------------------------------------------%

    % linked_target_file_name_full_curdir(Globals, MainModuleName,
    %   LinkedTargetType, FullFileName, CurDirFileName, !IO):
    %
    % Return both the full filename (including any subdirs) and the
    % in-current-directory filename of the executable of the kind
    % specified by LinkedTargetType for MainModuleName.
    %
    % Create any subdirs that need to be created.
    %
:- pred linked_target_file_name_full_curdir(globals::in, module_name::in,
    linked_target_type::in, file_name::out, file_name::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Print the pre-link message if the verbosity level calls for one.
    %
:- pred pre_link_msg(io.text_output_stream::in, globals::in,
    io::di, io::uo) is det.

    % Print the post-link message if the verbosity level calls for one.
    %
:- pred post_link_msg(io.text_output_stream::in, globals::in,
    io::di, io::uo) is det.

    % post_link_maybe_make_symlink(ProgressStream, Globals, LinkedTargetType,
    %   ModuleName, FullOutputFileName, CurDirOutputFileName,
    %   LinkSucceeded, Succeeded, !IO):
    %
    % If LinkSucceeded = succeeded, then invoke the predicate just below
    % to do its job.
    %
:- pred post_link_maybe_make_symlink(io.text_output_stream::in, globals::in,
    linked_target_type::in, module_name::in, file_name::in, file_name::in,
    maybe_succeeded::in, maybe_succeeded::out, io::di, io::uo) is det.

    % post_link_maybe_make_symlink_or_copy(ProgressStream, Globals,
    %   LinkedTargetType, ModuleName, FullFileName, CurDirFileName,
    %   Succeeded, MadeSymlinkOrCopy, !IO):
    %
    % If FullFileName, which results from converting MainModuleName
    % and LinkedTargetType to a fully specified relative pathname, differs from
    % CurDirFileName, which is the same filename in the current directory,
    % then link or copy FileName into the current directory after having
    % successfully built it, if the target does not exist or is not up-to-date.
    %
    % If the target platform requires a launcher script that differs from
    % the current-directory form of the executable, then create that as well.
    %
:- pred post_link_maybe_make_symlink_or_copy(io.text_output_stream::in,
    globals::in, linked_target_type::in, module_name::in,
    file_name::in, file_name::in, maybe_succeeded::out, bool::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.module_cmds.

:- import_module io.file.
:- import_module maybe.

%---------------------------------------------------------------------------%

linked_target_file_name_full_curdir(Globals, MainModuleName, LinkedTargetType,
        FullFileName, CurDirFileName, !IO) :-
    % This code should be in file_names.m. The only reason why it is here
    % is that the linked_target_type type is defined here.
    (
        % Java archives and Java executables get the same filename.
        % XXX Then why make the distinction in linked_target_type?
        (
            LinkedTargetType = c_executable,
            Ext = ext_cur_gas(ext_cur_gas_exec_exec_opt)
        ;
            LinkedTargetType = csharp_executable,
            Ext = ext_cur_gas(ext_cur_gas_exec_exe)
        ;
            LinkedTargetType = csharp_library,
            Ext = ext_cur_gs(ext_cur_gs_lib_cil_dll)
        ;
            LinkedTargetType = java_archive,
            Ext = ext_cur_gs(ext_cur_gs_lib_jar)
        ;
            LinkedTargetType = java_executable,
            Ext = ext_cur_gs(ext_cur_gs_lib_jar)
        ),
        % XXX LEGACY
        module_name_to_file_name_full_curdir_create_dirs(Globals, $pred,
            Ext, MainModuleName, FullFileName, _FullFileNameProposed,
            CurDirFileName, !IO)
    ;
        (
            LinkedTargetType = c_static_library,
            Ext = ext_cur_gas(ext_cur_gas_lib_lib_opt)
        ;
            LinkedTargetType = c_shared_library,
            Ext = ext_cur_gas(ext_cur_gas_lib_sh_lib_opt)
        ),
        % XXX LEGACY
        module_name_to_lib_file_name_full_curdir_create_dirs(Globals, $pred,
            "lib", Ext, MainModuleName, FullFileName, _FullFileNameProposed,
            CurDirFileName, !IO)
    ).

%---------------------------------------------------------------------------%

pre_link_msg(ProgressStream, Globals, !IO) :-
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(ProgressStream, Verbose, "% Linking...\n", !IO).

post_link_msg(ProgressStream, Globals, !IO) :-
    globals.lookup_bool_option(Globals, statistics, Stats),
    maybe_report_stats(ProgressStream, Stats, !IO).

%---------------------%

post_link_maybe_make_symlink(ProgressStream, Globals,
        LinkedTargetType, ModuleName, FullOutputFileName, CurDirOutputFileName,
        LinkSucceeded, Succeeded, !IO) :-
    (
        LinkSucceeded = succeeded,
        post_link_maybe_make_symlink_or_copy(ProgressStream, Globals,
            LinkedTargetType, ModuleName,
            FullOutputFileName, CurDirOutputFileName,
            Succeeded, _MadeSymlinkOrCopy, !IO)
    ;
        LinkSucceeded = did_not_succeed,
        Succeeded = did_not_succeed
    ).

post_link_maybe_make_symlink_or_copy(ProgressStream, Globals,
        LinkedTargetType, ModuleName, FullFileName, CurDirFileName,
        Succeeded, MadeSymlinkOrCopy, !IO) :-
    ( if FullFileName = CurDirFileName then
        Succeeded = succeeded,
        MadeSymlinkOrCopy = no
    else
        do_full_curdir_timestamps_match(FullFileName, CurDirFileName,
            DoTimestampsMatch, !IO),
        (
            DoTimestampsMatch = yes,
            Succeeded0 = succeeded,
            MadeSymlinkOrCopy = no
        ;
            DoTimestampsMatch = no,
            % Remove the target of the symlink/copy in case it already exists.
            io.file.remove_file_recursively(CurDirFileName, _, !IO),

            make_symlink_or_copy_file(ProgressStream, Globals,
                FullFileName, CurDirFileName, Succeeded0, !IO),
            MadeSymlinkOrCopy = yes
        ),

        % For Java and C# grades, we also need to symlink or copy
        % the launcher scripts or batch files.
        ( if
            Succeeded0 = succeeded,
            (
                LinkedTargetType = csharp_executable,
                % NOTE: we don't generate a launcher script for C# executables
                % on Windows; it is not necessary, since they can be executed
                % directly.
                %
                % XXX What about OSs other than Linux and Windows?
                globals.get_target_env_type(Globals, TargetEnvType),
                TargetEnvType = env_type_posix
            ;
                LinkedTargetType = java_executable
            )
        then
            get_launcher_script_extension(Globals, ScriptExt),
            % XXX LEGACY
            module_name_to_file_name_full_curdir(Globals, $pred, ScriptExt,
                ModuleName, FullLauncherName, _FullLauncherNameProposed,
                CurDirLauncherName),

            do_full_curdir_timestamps_match(FullLauncherName,
                CurDirLauncherName, DoLauncherTimestampsMatch, !IO),
            (
                DoLauncherTimestampsMatch = yes,
                Succeeded = succeeded
            ;
                DoLauncherTimestampsMatch = no,
                % Remove the target of the symlink/copy in case
                % it already exists.
                io.file.remove_file_recursively(CurDirLauncherName, _, !IO),
                make_symlink_or_copy_file(ProgressStream, Globals,
                    FullLauncherName, CurDirLauncherName, Succeeded, !IO)
            )
        else
            Succeeded = Succeeded0
        )
    ).

%---------------------%

:- pred do_full_curdir_timestamps_match(string::in, string::in, bool::out,
    io::di, io::uo) is det.

do_full_curdir_timestamps_match(FullFileName, CurDirFileName,
        SameTimestamp, !IO) :-
    ( if FullFileName = CurDirFileName then
        io.file.file_modification_time(FullFileName, FullTimeResult, !IO),
        (
            FullTimeResult = ok(_),
            SameTimestamp = yes
        ;
            FullTimeResult = error(_),
            % There are no timestamps at all.
            SameTimestamp = no
        )
    else
        compare_file_timestamps(FullFileName, CurDirFileName, MaybeCmp, !IO),
        ( if MaybeCmp = yes(=) then
            SameTimestamp = yes
        else
            SameTimestamp = no
        )
    ).

:- pred get_launcher_script_extension(globals::in, ext::out) is det.

get_launcher_script_extension(Globals, Ext) :-
    globals.get_target_env_type(Globals, TargetEnvType),
    (
        % XXX We should actually generate a .ps1 file for PowerShell.
        ( TargetEnvType = env_type_win_cmd
        ; TargetEnvType = env_type_powershell
        ),
        Ext = ext_cur_gas(ext_cur_gas_exec_bat)
    ;
        ( TargetEnvType = env_type_posix
        ; TargetEnvType = env_type_cygwin
        ; TargetEnvType = env_type_msys
        ),
        Ext = ext_cur_gas(ext_cur_gas_exec_noext)
    ).

%---------------------------------------------------------------------------%
:- end_module backend_libs.link_target_util.
%---------------------------------------------------------------------------%
