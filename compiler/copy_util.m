%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2012 The University of Melbourne.
% Copyright (C) 2013-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: copy_util.m.
%
% This module provides predicates for copying files. The Mercury compiler
% copies files for the following reasons.
%
% 1. "mmc --make" will copy files into an installation directory when
%    it is given an install target. (See make.program_target.m.)
%
% 2  "mmc --make", when asked to build some kinds of target files,
%    such as .int* and .*opt files, will put the contents it constructs
%    for that file not into the file with its intended name, but into a file
%    with a .tmp suffix, and will copy that file to the originally intended
%    file name only if the new file differs from the old contents of the
%    target file. This allows us to avoid rebuild all the files that
%    depend on *that* file.
%
% 2  "mmc --make", after constructing some kinds of files (such as executables)
%    in subdirectories of the current directory, will want to make that file
%    available in the current directory as well. If the current platform
%    supports symbolic links, we would normally do this by creating such a
%    link, but if symbolic links are not available, or if the user asks us
%    not to use them, then we fall back to copying the file back to the
%    current directory.
%
% This module can use either external or internal code to copy a file,
% with the choice being dictated by the value of the --install-method option.
%
% 1. The external method invokes a shell command. The identity of the command
%    is specified using the --install-command option, which defaults to "cp".
%    (XXX In practice, the way this is currently implemented only works for the
%    cp command on Unix-like systems; the various file copying commands on
%    Windows -- e.g. copy, xcopy, robocopy -- do not work.)
%
% 2. The internal method uses code internal to the compiler's executable code.
%    We intend to support two different internal methods:
%
%    - We can call mercury_copy_file/5, a file copy predicate that uses
%      only the facilities of the Mercury standard library.
%
%    - We also support copying using mechanisms provided by the underlying
%      platform, which we expect may be faster than just using pure Mercury
%      code. We access those mechanisms via foreign_procs. This is currently
%      only done for C grades on Windows and for the Java grade.
%
% Regardless of the mechanism, we must ensure that copying preserves
% as much of the file's metadata as possible. Notably, this must include
% the execute permission bits on platforms that use them.
%
%-----------------------------------------------------------------------------%

:- module libs.copy_util.
:- interface.

:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.maybe_util.

:- import_module io.

%-----------------------------------------------------------------------------%

    % copy_file_to_file_name(Globals, ProgressStream, SourceFile,
    %   DestinationFile, Succeeded, !IO):
    %
    % Copy SourceFile to DestinationFile. Both SourceFile and DestinationFile
    % must be paths that name files, not directories.
    %
    % If DestinationFile already exists, it will be overwritten and replaced.
    %
    % XXX what if SourceFile = DestinationFile? I (juliensf), don't think
    % the Mercury compiler actually does a file copy operation like that.
    %
:- pred copy_file_to_file_name(globals::in, io.text_output_stream::in,
    file_name::in, file_name::in, maybe_succeeded::out, io::di, io::uo) is det.

    % copy_file_to_directory(Globals, ProgressStream, SourceFile,
    %   DestinationDir, Succeeded, !IO):
    %
    % Copy SourceFile into the directory DestinationDir. The name of the
    % new file in DestinationDir will be the basename of SourceFile.
    %
    % This predicate assumes that DestinationDir exists and is writable.
    %
    % If the destination file already exists, it will be overwritten and
    % replaced.
    %
:- pred copy_file_to_directory(globals::in, io.text_output_stream::in,
    file_name::in, dir_name::in, maybe_succeeded::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.system_cmds.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module io.error_util.
:- import_module list.
:- import_module maybe.
:- import_module require.   % Required by non-C grades.
:- import_module string.

%-----------------------------------------------------------------------------%

copy_file_to_file_name(Globals, ProgressStream, SourceFile, DestinationFile,
        Succeeded, !IO) :-
    globals.get_install_method(Globals, InstallMethod),
    (
        InstallMethod = install_method_external_cmd,
        Command = make_install_file_command(Globals, SourceFile,
            DestinationFile),
        invoke_system_command(Globals, ProgressStream, ProgressStream,
            cmd_verbose, Command, Succeeded, !IO)
    ;
        InstallMethod = install_method_internal_code,
        do_copy_file(SourceFile, DestinationFile, Res, !IO),
        (
            Res = ok,
            Succeeded = succeeded
        ;
            Res = error(Error),
            string.format("could not copy %s to %s: %s.",
                [s(SourceFile), s(DestinationFile),
                s(io.error_message(Error))], Msg),
            report_error(ProgressStream, Msg, !IO),
            Succeeded = did_not_succeed
        )
    ).

%-----------------------------------------------------------------------------%

copy_file_to_directory(Globals, ProgressStream, SourceFile, DestinationDir,
        Succeeded, !IO) :-
    globals.get_install_method(Globals, InstallMethod),
    (
        InstallMethod = install_method_external_cmd,
        Command = make_install_file_command(Globals, SourceFile,
            DestinationDir),
        invoke_system_command(Globals, ProgressStream, ProgressStream,
            cmd_verbose, Command, Succeeded, !IO)
    ;
        InstallMethod = install_method_internal_code,
        ( if dir.basename(SourceFile, BaseSourceFile) then
            DestinationFile = DestinationDir / BaseSourceFile,
            do_copy_file(SourceFile, DestinationFile, Res, !IO),
            (
                Res = ok,
                Succeeded = succeeded
            ;
                Res = error(Error),
                string.format("could not copy %s to %s: %.s",
                    [s(SourceFile), s(DestinationFile),
                    s(io.error_message(Error))], Msg),
                report_error(ProgressStream, Msg, !IO),
                Succeeded = did_not_succeed
            )
        else
            string.format("could not copy %s: it is a root directory.",
                [s(SourceFile)], Msg),
            report_error(ProgressStream, Msg, !IO),
            Succeeded = did_not_succeed
        )
    ).

%-----------------------------------------------------------------------------%

:- pred do_copy_file(file_name::in, file_name::in, io.res::out,
    io::di, io::uo) is det.

do_copy_file(SourceFile, DestinationFile, Res, !IO) :-
    CopyMethod = get_internal_copy_method,
    (
        CopyMethod = icm_mercury_impl,
        mercury_copy_file(SourceFile, DestinationFile, Res, !IO)
    ;
        CopyMethod = icm_windows_api,
        windows_copy_file(SourceFile, DestinationFile, Res, !IO)
    ;
        CopyMethod = icm_java_library,
        java_copy_file(SourceFile, DestinationFile, Res, !IO)
    ).

:- type internal_copy_method
    --->    icm_mercury_impl  % Mercury copy implementation.
    ;       icm_windows_api   % CopyFileW() function from the Windows API.
    ;       icm_java_library. % Files.copy() from the Java library.

:- pragma foreign_export_enum("C", internal_copy_method/0,
    [prefix("MC_"), uppercase]).

:- pragma foreign_export_enum("Java", internal_copy_method/0,
    [prefix("MC_"), uppercase]).

:- func get_internal_copy_method = internal_copy_method.

:- pragma foreign_proc("C",
    get_internal_copy_method = (Method::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
#if defined(MR_WIN32) && !defined(MR_CYGWIN)
    Method = MC_ICM_WINDOWS_API;
#else
    Method = MC_ICM_MERCURY_IMPL;
#endif
").

:- pragma foreign_proc("Java",
    get_internal_copy_method = (Method::out),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    Method = MC_ICM_JAVA_LIBRARY;
").

% For the C# backend.
get_internal_copy_method = icm_mercury_impl.

%-----------------------------------------------------------------------------%
%
% File copying using the Windows API.
%

:- pred windows_copy_file(file_name::in, file_name::in, io.res::out,
    io::di, io::uo) is det.

windows_copy_file(Source, Destination, Res, !IO) :-
    do_windows_copy_file(Source, Destination, SysErr, !IO),
    is_error_maybe_win32(SysErr, yes, "file copy failed: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IO_Error),
        Res = error(IO_Error)
    ;
        MaybeIOError = no,
        Res = ok
    ).

:- pragma foreign_decl("C", "
#if defined(MR_WIN32)
   #include ""mercury_string.h""   // For MR_utf8_to_wide.
   #include ""mercury_windows.h""  // For windows.h.
   #include <winbase.h>            // For CopyFileW().
#endif
").

:- pred do_windows_copy_file(file_name::in, file_name::in, system_error::out,
    io::di, io::uo) is det.
:- pragma no_determinism_warning(pred(do_windows_copy_file/5)).

:- pragma foreign_proc("C",
    do_windows_copy_file(Src::in, Dst::in, SysErr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#if defined(MR_WIN32)
     if (CopyFileW(MR_utf8_to_wide(Src), MR_utf8_to_wide(Dst), FALSE)) {
         SysErr = 0;
     } else {
         SysErr = GetLastError();
     }
#else
     MR_fatal_error(""do_windows_copy_file/6 not supported on this system"");
#endif
").

do_windows_copy_file(_, _, _, _, _) :-
    unexpected($pred,
        "do_windows_copy_file/5 not supported on non-C backends").

%-----------------------------------------------------------------------------%
%
% File copying using the Java library.
%

:- pred java_copy_file(file_name::in, file_name::in, io.res::out,
    io::di, io::uo) is det.

java_copy_file(Source, Destination, Res, !IO) :-
    do_java_copy_file(Source, Destination, SysErr, !IO),
    is_error(SysErr, "file copy failed: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IO_Error),
        Res = error(IO_Error)
    ;
        MaybeIOError = no,
        Res = ok
    ).

:- pred do_java_copy_file(file_name::in, file_name::in, system_error::out,
    io::di, io::uo) is det.
:- pragma no_determinism_warning(pred(do_java_copy_file/5)).

:- pragma foreign_proc("Java",
    do_java_copy_file(Src::in, Dst::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        java.nio.file.Files.copy(
            java.nio.file.Paths.get(Src),
            java.nio.file.Paths.get(Dst),
            java.nio.file.StandardCopyOption.REPLACE_EXISTING,
            java.nio.file.StandardCopyOption.COPY_ATTRIBUTES);
            Error = null;
    } catch (java.lang.Exception e) {
        Error = e;
    }
").

do_java_copy_file(_, _, _, _, _) :-
    unexpected($pred,
        "do_java_copy_file/5 not supported on non-Java backends").

%-----------------------------------------------------------------------------%
%
% Mercury implementation of file copying.
%

    % XXX TODO: copying the file byte-by-byte is inefficient.
    % If the OS or platform we are on provides a system call for copying files,
    % we should use that in preference to the code below.
    % When the standard library has a byte_array type, the code below should be
    % change the code below to read the file being copied into a byte_array and
    % then write out that array using a single system call.
    %
:- pred mercury_copy_file(file_name::in, file_name::in, io.res::out,
    io::di, io::uo) is det.

mercury_copy_file(Source, Destination, Res, !IO) :-
    io.open_binary_input(Source, SourceRes, !IO),
    get_file_permissions(Source, SourceFilePermissions, !IO),
    (
        SourceRes = ok(SourceStream),
        io.open_binary_output(Destination, DestRes, !IO),
        (
            DestRes = ok(DestStream),
            copy_bytes(SourceStream, DestStream, Res, !IO),
            io.close_binary_input(SourceStream, !IO),
            io.close_binary_output(DestStream, !IO),
            set_file_permissions(Destination, SourceFilePermissions, !IO)
        ;
            DestRes = error(Error),
            Res = error(Error)
        )
    ;
        SourceRes = error(Error),
        Res = error(Error)
    ).

:- pred copy_bytes(io.binary_input_stream::in, io.binary_output_stream::in,
    io.res::out, io::di, io::uo) is det.

copy_bytes(Source, Destination, Res, !IO) :-
    should_reduce_stack_usage(ShouldReduce),
    (
        ShouldReduce = no,
        copy_bytes_plain(Source, Destination, Res, !IO)
    ;
        ShouldReduce = yes,
        copy_bytes_chunk(Source, Destination, Res, !IO)
    ).

:- pred copy_bytes_plain(io.binary_input_stream::in,
    io.binary_output_stream::in, io.res::out, io::di, io::uo) is det.

copy_bytes_plain(Source, Destination, Res, !IO) :-
    io.read_binary_uint8_unboxed(Source, ByteResult, Byte, !IO),
    (
        ByteResult = ok,
        io.write_binary_uint8(Destination, Byte, !IO),
        copy_bytes_plain(Source, Destination, Res, !IO)
    ;
        ByteResult = eof,
        Res = ok
    ;
        ByteResult = error(Error),
        Res = error(Error)
    ).

:- type copy_chunk_inner_res0
    --->    ccir0_ok
    ;       ccir0_error(io.error)
    ;       ccir0_more.

:- pred copy_bytes_chunk(io.binary_input_stream::in,
    io.binary_output_stream::in, io.res::out, io::di, io::uo) is det.

copy_bytes_chunk(Source, Destination, Res, !IO) :-
    % ChunkSize gives the maximum number of recursive calls we want to allow in
    % the copy_bytes_inner predicate. Without such a limit, the depth of
    % recursion, which depends on the size of the file they read, will cause
    % exhaustion of the det stack in debug grades, since there is no tail
    % recursion in such grades.
    %
    % With this arrangement, the maximum number of stack frames needed to
    % process a file of size N is N/1000 + 1000, the former being the number of
    % frames of the copy_bytes_chunk predicate, the latter being the max
    % number of frames of the copy_bytes_inner predicate.
    %
    ChunkSize = 1000,
    copy_bytes_inner(ChunkSize, Source, Destination, InnerRes, !IO),
    (
        InnerRes = ccir0_ok,
        Res = ok
    ;
        InnerRes = ccir0_error(Error),
        Res = error(Error)
    ;
        InnerRes = ccir0_more,
        copy_bytes_chunk(Source, Destination, Res, !IO)
    ).

:- pred copy_bytes_inner(int::in, io.binary_input_stream::in,
    io.binary_output_stream::in, copy_chunk_inner_res0::out,
    io::di, io::uo) is det.

copy_bytes_inner(Left, Source, Destination, Res, !IO) :-
    ( if Left > 0 then
        io.read_binary_uint8_unboxed(Source, ByteResult, Byte, !IO),
        (
            ByteResult = ok,
            io.write_binary_uint8(Destination, Byte, !IO),
            copy_bytes_inner(Left - 1, Source, Destination, Res, !IO)
        ;
            ByteResult = eof,
            Res = ccir0_ok
        ;
            ByteResult = error(Error),
            Res = ccir0_error(Error)
        )
    else
        Res = ccir0_more
    ).

:- pred should_reduce_stack_usage(bool::out) is det.

% For non-C backends.
should_reduce_stack_usage(yes).

:- pragma foreign_proc("C",
    should_reduce_stack_usage(ShouldReduce::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness],
"
#ifdef  MR_EXEC_TRACE
    ShouldReduce = MR_YES;
#else
    ShouldReduce = MR_NO;
#endif
").

%-----------------------------------------------------------------------------%
%
% Getting and setting file permissions.
%

    % This type describes the result of retrieving file permissions.
    %
:- type file_permissions
    --->    unknown_file_permissions
            % The file permissions are unavailable, either due an error or not
            % being supported on this system.

    ;       have_file_permissions(uint64).
            % We have file permissions, which are returned in the argument.
            % (We return the file mode as a uint64 to avoid having to
            % conditionally define a foreign type to represent it.)

:- pred get_file_permissions(file_name::in, file_permissions::out,
    io::di, io::uo) is det.

get_file_permissions(FileName, FilePermissions, !IO) :-
    do_get_file_permissions(FileName, RawFilePermissions, IsOk, !IO),
    (
        IsOk = yes,
        FilePermissions = have_file_permissions(RawFilePermissions)
    ;
        IsOk = no,
        FilePermissions = unknown_file_permissions
    ).

:- pragma foreign_decl("C", "
#if defined(MR_HAVE_SYS_TYPES_H)
    #include <sys/types.h>
#endif
#if defined(MR_HAVE_SYS_STAT_H)
    #include <sys/stat.h>
#endif
#if defined(MR_HAVE_UNISTD_H)
    #include <unistd.h>
#endif
").

:- pred do_get_file_permissions(file_name::in, uint64::out, bool::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_get_file_permissions(FileName::in, RawFilePermissions::out, IsOk::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#if defined(MR_HAVE_STAT)

    int stat_result;
    #if defined(MR_WIN32)
        struct _stat statbuf;
        stat_result = _wstat(MR_utf8_to_wide(FileName), &statbuf);
    #else
        struct stat statbuf;
        stat_result = stat(FileName, &statbuf);
    #endif

    if (stat_result == 0) {
        IsOk = MR_YES;
        RawFilePermissions = statbuf.st_mode;
    } else {
        IsOk = MR_NO;
        RawFilePermissions = 0;
    }
#else
    IsOk = MR_NO;
    RawFilePermissions = 0;
#endif
").

% For the non-C backends.
do_get_file_permissions(_, 0u64, no, !IO).

:- pred set_file_permissions(file_name::in, file_permissions::in,
    io::di, io::uo) is det.

set_file_permissions(FileName, FilePermissions, !IO) :-
    (
        FilePermissions = unknown_file_permissions
    ;
        FilePermissions = have_file_permissions(RawFilePermissions),
        do_set_file_permissions(FileName, RawFilePermissions, !IO)
    ).

:- pred do_set_file_permissions(file_name::in, uint64::in, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    do_set_file_permissions(FileName::in, RawFilePermissions::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
#if defined(MR_HAVE_CHMOD)
    #if defined(MR_MSVC)
        // MSVC does not define mode_t.
        (void) _wchmod(MR_utf8_to_wide(FileName), (int) RawFilePermissions);
    #elif defined(MR_WIN32)
        (void) _wchmod(MR_utf8_to_wide(FileName), (mode_t) RawFilePermissions);
    #else
        (void) chmod(FileName, (mode_t) RawFilePermissions);
    #endif
#endif
").

% For the non-C backends.
do_set_file_permissions(_, _, !IO).

%-----------------------------------------------------------------------------%
:- end_module copy_util.
%-----------------------------------------------------------------------------%
