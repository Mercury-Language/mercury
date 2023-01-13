%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2013-2022 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: io.file.m.
%
% This module provides operations on files other than input/output.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module io.file.
:- interface.

:- import_module time.

%---------------------------------------------------------------------------%
%
% File handling predicates.
%

    % remove_file(FileName, Result, !IO) attempts to remove the file
    % FileName, binding Result to ok/0 if it succeeds, or error/1 if it
    % fails. If FileName names a file that is currently open, the behaviour
    % is implementation-dependent.
    %
    % If FileName names a directory, the behavior is currently
    % implementation-dependent. On most platforms, an empty directory will be
    % deleted.
    %
:- pred remove_file(string::in, io.res::out, io::di, io::uo) is det.

    % remove_file_recursively(FileName, Result, !IO) attempts to remove
    % the file FileName, binding Result to ok/0 if it succeeds, or error/1
    % if it fails. If FileName names a file that is currently open, the
    % behaviour is implementation-dependent.
    %
    % Unlike remove_file, this predicate will attempt to remove non-empty
    % directories (recursively). If it fails, some of the directory elements
    % may already have been removed.
    %
:- pred remove_file_recursively(string::in, io.res::out, io::di, io::uo)
    is det.

    % rename_file(OldFileName, NewFileName, Result, !IO).
    %
    % Attempts to rename the file or directory OldFileName as NewFileName,
    % binding Result to ok/0 if it succeeds, or error/1 if it fails.
    % If OldFileName names a file that is currently open, the behaviour is
    % implementation-dependent. If NewFileName names a file that already
    % exists the behaviour is also implementation-dependent; on some systems,
    % the file previously named NewFileName will be deleted and replaced
    % with the file previously named OldFileName.
    %
:- pred rename_file(string::in, string::in, io.res::out, io::di, io::uo)
    is det.

%---------------------%

    % Succeeds if this platform can read and create symbolic links.
    %
:- pred have_symlinks is semidet.

    % make_symlink(FileName, LinkFileName, Result, !IO).
    %
    % Attempts to make LinkFileName be a symbolic link to FileName.
    % If FileName is a relative path, it is interpreted relative
    % to the directory containing LinkFileName.
    %
:- pred make_symlink(string::in, string::in, io.res::out, io::di, io::uo)
    is det.

    % read_symlink(FileName, Result, !IO) returns `ok(LinkTarget)'
    % if FileName is a symbolic link pointing to LinkTarget, and
    % `error(Error)' otherwise. If LinkTarget is a relative path,
    % it should be interpreted relative the directory containing FileName,
    % not the current directory.
    %
:- pred read_symlink(string::in, io.res(string)::out, io::di, io::uo) is det.

%---------------------%

    % check_file_accessibility(FileName, AccessTypes, Result):
    %
    % Check whether the current process can perform the operations given
    % in AccessTypes on FileName.
    %
    % The C# implementation is limited:
    %
    % - The "execute" access check passes for a regular file if we can read
    %   from the file, and have unrestricted permissions to execute unmanaged
    %   code.
    %
    % - The "write" access check passes for a directory if the directory does
    %   not have the ReadOnly attribute, which does not necessarily mean we can
    %   write to it.
    %
    % - The "execute" access check is ignored for directories.
    %
:- pred check_file_accessibility(string::in, list(access_type)::in,
    io.res::out, io::di, io::uo) is det.

    % file_type(FollowSymLinks, FileName, TypeResult)
    % finds the type of the given file.
    %
:- pred file_type(bool::in, string::in, io.res(file_type)::out,
    io::di, io::uo) is det.

    % file_modification_time(FileName, TimeResult)
    % finds the last modification time of the given file.
    %
:- pred file_modification_time(string::in, io.res(time_t)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% Predicates for handling temporary files.
%

    % make_temp_file(Result, !IO) creates an empty file whose name is different
    % to the name of any existing file. If successful Result returns the name
    % of the file. It is the responsibility of the caller to delete the file
    % when it is no longer required.
    %
    % The file is placed in the directory returned by get_temp_directory/3.
    %
    % On the Java backend, this does not attempt to create the file
    % with restrictive permissions (600 on Unix-like systems) and therefore
    % should not be used when security is required.
    %
:- pred make_temp_file(io.res(string)::out, io::di, io::uo) is det.

    % make_temp_file(Dir, Prefix, Suffix, Result, !IO) creates an empty file
    % whose name is different to the name of any existing file. The file will
    % reside in the directory specified by Dir and will have a prefix using up
    % to the first 5 code units of Prefix. If successful, Result returns the
    % name of the file. It is the responsibility of the caller to delete the
    % file when it is no longer required.
    %
    % The reason for truncating Prefix is historical; in future the behaviour
    % may be changed. Note that the truncation is performed without regard for
    % code point boundaries. It is recommended to use only (printable) ASCII
    % characters in the prefix string.
    %
    % The C backend has the following limitations:
    %   - Suffix may be ignored.
    %
    % The C# backend has the following limitations:
    %   - Dir is ignored.
    %   - Prefix is ignored.
    %   - Suffix is ignored.
    %
    % On the Java backend, this does not attempt to create the file
    % with restrictive permissions (600 on Unix-like systems) and therefore
    % should not be used when security is required.
    %
:- pred make_temp_file(string::in, string::in, string::in, io.res(string)::out,
    io::di, io::uo) is det.

    % make_temp_directory(Result, !IO) creates an empty directory whose name
    % is different from the name of any existing directory.
    %
    % On the Java backend this is insecure as the file permissions are not set.
    %
:- pred make_temp_directory(io.res(string)::out, io::di, io::uo) is det.

    % make_temp_directory(ParentDirName, Prefix, Suffix, Result, !IO) creates
    % an empty directory whose name is different from the name of any existing
    % directory. The new directory will reside in the existing directory
    % specified by ParentDirName and will have a prefix using up to the
    % first 5 characters of Prefix and a Suffix. Result returns the name of the
    % new directory. It is the responsibility of the program to delete the
    % directory when it is no longer needed.
    %
    % The C backend has the following limitations:
    %   - Suffix is ignored.
    %
    % The C# backend has the following limitations:
    %   - Prefix is ignored.
    %   - Suffix is ignored.
    %
    % On the Java backend this is insecure as the file permissions are not set.
    %
:- pred make_temp_directory(string::in, string::in, string::in,
    io.res(string)::out, io::di, io::uo) is det.

    % Test if the make_temp_directory predicates are available.
    % This is false for C backends without support for mkdtemp(3).
    %
:- pred have_make_temp_directory is semidet.

    % get_temp_directory(DirName, !IO)
    %
    % DirName is the name of a directory where applications should put
    % temporary files.
    %
    % This is implementation-dependent. For current Mercury implementations,
    % it is determined as follows:
    % 1. For the non-Java back-ends:
    %    - On Microsoft Windows systems, the file will reside in
    %      the current directory if the TMP environment variable
    %      is not set, or in the directory specified by TMP if it is set.
    %    - On Unix systems, the file will reside in /tmp if the TMPDIR
    %      environment variable is not set, or in the directory specified
    %      by TMPDIR if it is set.
    % 2. For the Java back-end, the system-dependent default
    %    temporary-file directory will be used, specified by the Java
    %    system property java.io.tmpdir. On UNIX systems the default
    %    value of this property is typically "/tmp" or "/var/tmp";
    %    on Microsoft Windows systems it is typically "c:\\temp".
    %
:- pred get_temp_directory(string::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module io.environment.
:- import_module io.error_util.

%---------------------------------------------------------------------------%
%
% File handling predicates.
%

remove_file(FileName, Result, !IO) :-
    remove_file_2(FileName, Error, !IO),
    is_error(Error, "remove failed: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok
    ).

:- pred remove_file_2(string::in, system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    remove_file_2(FileName::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    int rc;
#ifdef MR_WIN32
    // XXX _wremove will not delete an empty directory; _wrmdir does that.
    rc = _wremove(ML_utf8_to_wide(FileName));
#else
    rc = remove(FileName);
#endif
    if (rc == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
").

:- pragma foreign_proc("C#",
    remove_file_2(FileName::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        // System.IO.File.Delete() does not throw an exception
        // if the file does not exist.
        if (System.IO.File.Exists(FileName)) {
            System.IO.File.Delete(FileName);
            Error = null;
        } else if (System.IO.Directory.Exists(FileName)) {
            System.IO.Directory.Delete(FileName);
            Error = null;
        } else {
            Error = new System.IO.FileNotFoundException();
        }
    }
    catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    remove_file_2(FileName::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    // Java 7 java.nio.file.Files.delete() provides more detailed information
    // about failure to delete.

    try {
        java.io.File file = new java.io.File(FileName);

        if (file.delete()) {
            Error = null;
        } else {
            Error = new java.io.IOException(""Error deleting file"");
        }
    } catch (java.lang.Exception e) {
        Error = e;
    }
").

%---------------------%

remove_file_recursively(FileName, Res, !IO) :-
    FollowSymLinks = no,
    io.file.file_type(FollowSymLinks, FileName, ResFileType, !IO),
    (
        ResFileType = ok(FileType),
        (
            FileType = directory,
            dir.foldl2(remove_directory_entry, FileName, ok, Res0, !IO),
            (
                Res0 = ok(MaybeError),
                (
                    MaybeError = ok,
                    io.file.remove_file(FileName, Res, !IO)
                ;
                    MaybeError = error(Error),
                    Res = error(Error)
                )
            ;
                Res0 = error(_, Error),
                Res = error(Error)
            )
        ;
            ( FileType = regular_file
            ; FileType = symbolic_link
            ; FileType = named_pipe
            ; FileType = socket
            ; FileType = character_device
            ; FileType = block_device
            ; FileType = message_queue
            ; FileType = semaphore
            ; FileType = shared_memory
            ; FileType = unknown
            ),
            io.file.remove_file(FileName, Res, !IO)
        )
    ;
        ResFileType = error(Error),
        Res = error(Error)
    ).

:- pred remove_directory_entry(string::in, string::in, file_type::in,
    bool::out, io.res::in, io.res::out, io::di, io::uo) is det.

remove_directory_entry(DirName, FileName, _FileType, Continue, _, Res, !IO) :-
    io.file.remove_file_recursively(DirName / FileName, Res0, !IO),
    (
        Res0 = ok,
        Res = ok,
        Continue = yes
    ;
        Res0 = error(_),
        Res = Res0,
        Continue = no
    ).

%---------------------%

rename_file(OldFileName, NewFileName, Result, !IO) :-
    rename_file_2(OldFileName, NewFileName, Error, !IO),
    is_error(Error, "rename failed: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok
    ).

:- pred rename_file_2(string::in, string::in, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    rename_file_2(OldFileName::in, NewFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
    int rc;
#ifdef MR_WIN32
    rc = _wrename(ML_utf8_to_wide(OldFileName),
        ML_utf8_to_wide(NewFileName));
#else
    rc = rename(OldFileName, NewFileName);
#endif
    if (rc == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
").

:- pragma foreign_proc("C#",
    rename_file_2(OldFileName::in, NewFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (System.IO.Directory.Exists(OldFileName)) {
            System.IO.Directory.Move(OldFileName, NewFileName);
        } else {
            // XXX This won't clobber NewFileName.
            // .NET Core 3.0 and later versions support a overload of the
            // Move() method with an overwrite parameter.
            System.IO.File.Move(OldFileName, NewFileName);
        }
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    rename_file_2(OldFileName::in, NewFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    // Java 7 java.nio.file.Files.move may provide more detailed information
    // about failure to rename.

    try {
        java.io.File old_file = new java.io.File(OldFileName);
        java.io.File new_file = new java.io.File(NewFileName);

        // This first test just improves the error message in a common case.
        if (!old_file.exists()) {
            // java.io.FileNotFoundException is documented as being thrown when
            // failing to open a file but I don't see any reason we cannot use
            // it in this case. (nio also defines a NoSuchFileException class.)
            Error = new java.io.FileNotFoundException(
                ""No such file or directory"");
        } else if (old_file.renameTo(new_file)) {
            Error = null;
        } else {
            Error = new java.io.IOException(""Error renaming file"");
        }
    } catch (java.lang.Exception e) {
        Error = e;
    }
").

%---------------------%

:- pragma foreign_proc("C",
    have_symlinks,
    [will_not_call_mercury, promise_pure, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#if defined(MR_HAVE_SYMLINK) && defined(MR_HAVE_READLINK)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

have_symlinks :-
    semidet_fail.

%---------------------%

make_symlink(FileName, LinkFileName, Result, !IO) :-
    ( if io.file.have_symlinks then
        make_symlink_2(FileName, LinkFileName, Error, !IO),
        is_error(Error, "io.make_symlink failed: ", MaybeIOError, !IO),
        (
            MaybeIOError = yes(IOError),
            Result = error(IOError)
        ;
            MaybeIOError = no,
            Result = ok
        )
    else
        Result = error(make_io_error(
            "io.make_symlink not supported on this platform"))
    ).

:- pred make_symlink_2(string::in, string::in, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    make_symlink_2(FileName::in, LinkFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_HAVE_SYMLINK
    if (symlink(FileName, LinkFileName) == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
#else
    Error = ENOSYS;
#endif
").

% XXX MISSING C# make_symlink_2

    % Since io.have_symlinks will fail for Java, this procedure
    % should never be called:
    % XXX Java 7 has createSymbolicLink, readSymbolicLink
:- pragma foreign_proc("Java",
    make_symlink_2(_FileName::in, _LinkFileName::in, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    Error = new java.lang.UnsupportedOperationException(
        ""io.make_symlink_2 not implemented"");
").

%---------------------%

read_symlink(FileName, Result, !IO) :-
    ( if io.file.have_symlinks then
        read_symlink_2(FileName, TargetFileName, Error, !IO),
        is_error(Error, "io.read_symlink failed: ", MaybeIOError, !IO),
        (
            MaybeIOError = yes(IOError),
            Result = error(IOError)
        ;
            MaybeIOError = no,
            Result = ok(TargetFileName)
        )
    else
        Result = error(make_io_error(
            "io.read_symlink not supported on this platform"))
    ).

:- pred read_symlink_2(string::in, string::out, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_symlink_2(FileName::in, TargetFileName::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_HAVE_READLINK
  #ifndef PATH_MAX
    #define PATH_MAX 256
  #endif
    int     num_chars;
    char    *buffer2 = NULL;
    int     buffer_size2 = PATH_MAX;
    char    buffer[PATH_MAX + 1];

    // readlink() does not null-terminate the buffer.
    num_chars = readlink(FileName, buffer, PATH_MAX);

    if (num_chars == PATH_MAX) {
        do {
            buffer_size2 *= 2;
            buffer2 = MR_RESIZE_ARRAY(buffer2, char, buffer_size2);
            num_chars = readlink(FileName, buffer2, buffer_size2);
        } while (num_chars == buffer_size2);

        // Invariant: num_chars < buffer_size2.

        if (num_chars == -1) {
            TargetFileName = MR_make_string_const("""");
            Error = errno;
        } else {
            buffer2[num_chars] = '\\0';
            MR_make_aligned_string_copy_msg(TargetFileName, buffer2,
                MR_ALLOC_ID);
            Error = 0;
        }
        MR_free(buffer2);
    } else if (num_chars == -1) {
        TargetFileName = MR_make_string_const("""");
        Error = errno;
    } else {
        buffer[num_chars] = '\\0';
        MR_make_aligned_string_copy_msg(TargetFileName, buffer, MR_ALLOC_ID);
        Error = 0;
    }
#else // !MR_HAVE_READLINK
    TargetFileName = MR_make_string_const("""");
    Error = ENOSYS;
#endif
").

% XXX MISSING C# read_symlink_2

    % Since io.have_symlinks will fail for Java, this procedure
    % should never be called:
    % XXX Java 7 has createSymbolicLink, readSymbolicLink
:- pragma foreign_proc("Java",
    read_symlink_2(_FileName::in, TargetFileName::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    TargetFileName = """";
    Error = new java.lang.UnsupportedOperationException(
        ""io.read_symlink_2 not implemented"");
").

%---------------------%

check_file_accessibility(FileName, AccessTypes, Result, !IO) :-
    CheckRead = pred_to_bool(contains(AccessTypes, read)),
    CheckWrite = pred_to_bool(contains(AccessTypes, write)),
    CheckExecute = pred_to_bool(contains(AccessTypes, execute)),
    check_file_accessibility_2(FileName, CheckRead, CheckWrite,
        CheckExecute, Error, !IO),
    is_error(Error, "file not accessible: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok
    ).

:- pred check_file_accessibility_2(string::in, bool::in, bool::in, bool::in,
    system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    check_file_accessibility_2(FileName::in, CheckRead::in,
        CheckWrite::in, CheckExecute::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#if defined(MR_HAVE_ACCESS)
  #ifdef F_OK
    const int MODE_EXISTS = F_OK;
  #else
    const int MODE_EXISTS = 0;
  #endif
  #ifdef X_OK
    const int MODE_EXECUTE = X_OK;
  #else
    const int MODE_EXECUTE = 1;
  #endif
  #ifdef W_OK
    const int MODE_WRITE = W_OK;
  #else
    const int MODE_WRITE = 2;
  #endif
  #ifdef R_OK
    const int MODE_READ = R_OK;
  #else
    const int MODE_READ = 4;
  #endif

    int mode = MODE_EXISTS;
    int access_result;

  #if !defined(MR_WIN32) || defined(MR_CYGWIN)
    // Earlier versions of MSVCRT ignored flags it does not support,
    // later versions return an error (e.g. on Vista).
    if (CheckExecute) {
        mode |= MODE_EXECUTE;
    }
  #endif
    if (CheckWrite) {
        mode |= MODE_WRITE;
    }
    if (CheckRead) {
        mode |= MODE_READ;
    }

  #ifdef MR_WIN32
    access_result = _waccess(ML_utf8_to_wide(FileName), mode);
  #else
    access_result = access(FileName, mode);
  #endif

    if (access_result == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
#else // !MR_HAVE_ACCESS
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("Java",
    check_file_accessibility_2(FileName::in, CheckRead::in, CheckWrite::in,
        CheckExecute::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        java.io.File file = new java.io.File(FileName);

        // This first test just improves the error message in a common case.
        if (!file.exists()) {
            // java.io.FileNotFoundException is documented as being thrown when
            // failing to open a file but I don't see any reason we cannot use
            // it in this case. (nio also defines a NoSuchFileException class.)
            Error = new java.io.FileNotFoundException(
                ""No such file or directory"");
        } else {
            boolean ok = true;

            if (CheckRead == bool.YES) {
                ok = file.canRead();
            }
            if (ok && CheckWrite == bool.YES) {
                ok = file.canWrite();
            }
            if (ok && CheckExecute == bool.YES) {
                ok = file.canExecute();
            }

            if (ok) {
                Error = null;
            } else {
                Error = new java.io.IOException(""Permission denied"");
            }
        }
    }
    catch (java.lang.Exception e) {
        Error = e;
    }
").

:- pragma foreign_proc("C#",
    check_file_accessibility_2(FileName::in, CheckRead::in, CheckWrite::in,
        CheckExecute::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        if (System.IO.Directory.Exists(FileName)) {
            ML_dotnet_check_dir_accessibility(FileName,
                CheckRead == mr_bool.YES,
                CheckWrite == mr_bool.YES,
                CheckExecute == mr_bool.YES);
        } else {
            ML_dotnet_check_nondir_accessibility(FileName,
                CheckRead == mr_bool.YES,
                CheckWrite == mr_bool.YES,
                CheckExecute == mr_bool.YES);
        }
        Error = null;
    } catch (System.Exception e) {
        Error = e;
    }
").

:- pragma foreign_code("C#", "
    static void
    ML_dotnet_check_dir_accessibility(String path,
        bool checkRead, bool checkWrite, bool checkExecute)
    {
        if (checkRead) {
            System.IO.Directory.EnumerateFileSystemEntries(path);
        }

        if (checkWrite) {
            // XXX This isn't quite right. Just because the directory isn't
            // read-only doesn't mean we have permission to write to it.
            // The only way to test whether a directory is writable is to write
            // a file to it, which is ugly and also changes the last modified
            // timestamp on the directory.
            System.IO.FileAttributes attrs =
                System.IO.File.GetAttributes(path);
            if ((attrs & System.IO.FileAttributes.ReadOnly) ==
                System.IO.FileAttributes.ReadOnly)
            {
                throw new System.IO.IOException(
                    ""Directory has ReadOnly attribute"");
            }
        }

        if (checkExecute) {
            // We do not know what to do here.
        }
    }

    static void
    ML_dotnet_check_nondir_accessibility(String path,
        bool checkRead, bool checkWrite, bool checkExecute)
    {
        // We need to be able to read a file to execute it.
        // This behaves differently from the other backends, though.
        if (checkExecute) {
            checkRead = true;
        }

        if (checkRead || checkWrite) {
            System.IO.FileAccess file_access;
            if (checkRead && checkWrite) {
                file_access = System.IO.FileAccess.ReadWrite;
            } else if (checkRead) {
                file_access = System.IO.FileAccess.Read;
            } else {
                file_access = System.IO.FileAccess.Write;
            }
            // Throws an exception if we do not have permission.
            System.IO.FileStream fs = System.IO.File.Open(path,
                System.IO.FileMode.Open, file_access);
            fs.Close();
        } else {
            if (!System.IO.File.Exists(path)) {
                throw new System.IO.FileNotFoundException();
            }
        }

        if (checkExecute) {
            // We need unrestricted permissions to execute unmanaged code.
            (new System.Security.Permissions.SecurityPermission(
                System.Security.Permissions.SecurityPermissionFlag.
                AllFlags)).Demand();
        }
    }
").

%---------------------%

:- pragma foreign_export_enum("C", file_type/0,
    [prefix("ML_FILE_TYPE_"), uppercase]).
:- pragma foreign_export_enum("C#", file_type/0,
    [prefix("ML_FILE_TYPE_"), uppercase]).
:- pragma foreign_export_enum("Java", file_type/0,
    [prefix("ML_FILE_TYPE_"), uppercase]).

file_type(FollowSymLinks, FileName, Result, !IO) :-
    (
        FollowSymLinks = yes,
        FollowSymLinksInt = 1
    ;
        FollowSymLinks = no,
        FollowSymLinksInt = 0
    ),
    file_type_2(FollowSymLinksInt, FileName, FileType, Error,  !IO),
    is_error(Error, "can't find file type: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(FileType)
    ).

:- pred file_type_2(int::in, string::in, file_type::out, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    file_type_2(FollowSymLinks::in, FileName::in, FileType::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_HAVE_STAT
  #ifdef MR_WIN32
    struct _stat s;
    int stat_result = _wstat(ML_utf8_to_wide(FileName), &s);
  #else
    struct stat s;
    int stat_result;

    if (FollowSymLinks == 1) {
        stat_result = stat(FileName, &s);
    } else {
        #ifdef MR_HAVE_LSTAT
            stat_result = lstat(FileName, &s);
        #else
            stat_result = stat(FileName, &s);
        #endif
    }
  #endif

    if (stat_result == 0) {
        // Do we still need the non-POSIX S_IFMT style?
        if
            #if defined(S_ISREG)
                (S_ISREG(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFREG)
                ((s.st_mode & S_IFMT) == S_IFREG)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_REGULAR_FILE;
        }
        else if
            #if defined(S_ISDIR)
                (S_ISDIR(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFDIR)
                ((s.st_mode & S_IFMT) == S_IFDIR)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_DIRECTORY;
        }
        else if
            #if defined(S_ISBLK)
                (S_ISBLK(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFBLK)
                ((s.st_mode & S_IFMT) == S_IFBLK)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_BLOCK_DEVICE;
        }
        else if
            #if defined(S_ISCHR)
                (S_ISCHR(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFCHR)
                ((s.st_mode & S_IFMT) == S_IFCHR)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_CHARACTER_DEVICE;
        }
        else if
            #if defined(S_ISFIFO)
                (S_ISFIFO(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFIFO)
                ((s.st_mode & S_IFMT) == S_IFIFO)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_NAMED_PIPE;
        }
        else if
            #if defined(S_ISLNK)
                (S_ISLNK(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFLNK)
                ((s.st_mode & S_IFMT) == S_IFLNK)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_SYMBOLIC_LINK;
        }
        else if
            #if defined(S_ISSOCK)
                (S_ISSOCK(s.st_mode))
            #elif defined(S_IFMT) && defined(S_IFSOCK)
                ((s.st_mode & S_IFMT) == S_IFSOCK)
            #else
                (0)
            #endif
        {
            FileType = ML_FILE_TYPE_SOCKET;
        } else {

        #ifdef S_TYPEISMQ
            if (S_TYPEISMQ(&s)) {
                FileType = ML_FILE_TYPE_MESSAGE_QUEUE;
            } else
        #endif

        #ifdef S_TYPEISSEM
            if (S_TYPEISSEM(&s)) {
                FileType = ML_FILE_TYPE_SEMAPHORE;
            } else
        #endif

        #ifdef S_TYPEISSHM
            if (S_TYPEISSHM(&s)) {
                FileType = ML_FILE_TYPE_SHARED_MEMORY;
            } else
        #endif

            {
                FileType = ML_FILE_TYPE_UNKNOWN;
            }
        }
        Error = 0;
    } else {
        FileType = ML_FILE_TYPE_UNKNOWN;
        Error = errno;
    }
#else
    FileType = ML_FILE_TYPE_UNKNOWN;
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("C#",
    file_type_2(_FollowSymLinks::in, FileName::in, FileType::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        System.IO.FileAttributes attrs =
            System.IO.File.GetAttributes(FileName);
        if ((attrs & System.IO.FileAttributes.Directory) ==
            System.IO.FileAttributes.Directory)
        {
            FileType = mercury.io__file.ML_FILE_TYPE_DIRECTORY;
        }
        else if ((attrs & System.IO.FileAttributes.Device) ==
            System.IO.FileAttributes.Device)
        {
            // XXX It may be a block device, but .NET doesn't
            // distinguish between character and block devices.
            FileType = mercury.io__file.ML_FILE_TYPE_CHARACTER_DEVICE;
        }
        else
        {
            FileType = mercury.io__file.ML_FILE_TYPE_REGULAR_FILE;
        }
        Error = null;
    } catch (System.Exception e) {
        FileType = ML_FILE_TYPE_UNKNOWN;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    file_type_2(_FollowSymLinks::in, FileName::in, FileType::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    // The Java implementation can distinguish between regular files and
    // directories, and for everything else it just returns unknown.

    // Java 7 java.nio.file.Files.readAttributes() can do better.

    FileType = jmercury.io__file.ML_FILE_TYPE_UNKNOWN;
    Error = null;

    try {
        java.io.File file = new java.io.File(FileName);
        if (file.isFile()) {
            FileType = jmercury.io__file.ML_FILE_TYPE_REGULAR_FILE;
        } else if (file.isDirectory()) {
            FileType = jmercury.io__file.ML_FILE_TYPE_DIRECTORY;
        } else if (file.exists()) {
            FileType = jmercury.io__file.ML_FILE_TYPE_UNKNOWN;
        } else {
            Error = new java.io.FileNotFoundException(
                ""File not found or I/O error"");
        }
    } catch (java.lang.Exception e) {
        Error = e;
    }
").

%---------------------%

file_modification_time(File, Result, !IO) :-
    file_modification_time_2(File, Time, Error, !IO),
    is_error(Error, "can't get file modification time: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(Time)
    ).

:- pred file_modification_time_2(string::in, time_t::out, system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    file_modification_time_2(FileName::in, Time::out, Error::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        does_not_affect_liveness, no_sharing],
"
#ifdef MR_HAVE_STAT
  #ifdef MR_WIN32
    struct _stat s;
    int stat_result = _wstat(ML_utf8_to_wide(FileName), &s);
  #else
    struct stat s;
    int stat_result = stat(FileName, &s);
  #endif

    if (stat_result == 0) {
        // XXX avoid ML_construct_time_t by returning time_t_rep?
        Time = ML_construct_time_t(s.st_mtime);
        Error = 0;
    } else {
        Error = errno;
        Time = 0;
    }
#else
    Error = ENOSYS;
    Time = 0;
#endif
").

:- pragma foreign_proc("C#",
    file_modification_time_2(FileName::in, Time::out, Error::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        // We test for existence first because if the file or directory does
        // not exist, GetLastWriteTime() returns '12:00 midnight, January 1,
        // 1601 A.D. UTC adjusted to local time'. What kind of interface is
        // that?
        if (System.IO.File.Exists(FileName) ||
            System.IO.Directory.Exists(FileName))
        {
            System.DateTime t = System.IO.File.GetLastWriteTime(FileName);
            Time = time.ML_construct_time_t(t);
            Error = null;
        } else {
            Error = new System.IO.FileNotFoundException();
            Time = null;
        }
    } catch (System.Exception e) {
        Error = e;
        Time = null;
    }
").

:- pragma foreign_proc("Java",
    file_modification_time_2(FileName::in, Time::out, Error::out,
        _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    // Java 7 java.nio.file.Files.readAttributes() presumably can
    // distinguish a modtime of 0 from file not found or I/O error.

    try {
        java.io.File file = new java.io.File(FileName);
        long modtime = file.lastModified();
        if (modtime == 0) {
            Error = new java.io.FileNotFoundException(
                ""File not found or I/O error"");
            Time = null;
        } else {
            Time = time.ML_construct_time_t(
                java.time.Instant.ofEpochMilli(modtime));
            Error = null;
        }
    } catch (java.lang.Exception e) {
        Error = e;
        Time = null;
    }
").

%---------------------------------------------------------------------------%
%
% Predicates for handling temporary files.
%

make_temp_file(Result, !IO) :-
    io.file.get_temp_directory(Dir, !IO),
    io.file.make_temp_file(Dir, "mtmp", "", Result, !IO).

make_temp_file(Dir, Prefix, Suffix, Result, !IO) :-
    do_make_temp(Dir, Prefix, Suffix, char_to_string(dir.directory_separator),
        Name, Error, !IO),
    is_error(Error, "error creating temporary file: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(Name)
    ).

%---------------------%

% XXX The code for io.make_temp assumes POSIX. It uses the functions open(),
% close(), and getpid() and the macros EEXIST, O_WRONLY, O_CREAT, and O_EXCL.
% We should be using conditional compilation here to avoid these POSIX
% dependencies.

:- pred do_make_temp(string::in, string::in, string::in, string::in,
    string::out, system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_make_temp(DirName::in, Prefix::in, Suffix::in, Sep::in, FileName::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure,
        not_thread_safe, % due to ML_io_tempnam_counter
        tabled_for_io, does_not_affect_liveness],
"
#ifdef MR_HAVE_MKSTEMP
    int err, fd;

    // We cannot append Suffix because the last six chars in the argument
    // to mkstemp() must be XXXXXX.
    FileName = MR_make_string(MR_ALLOC_ID, ""%s%s%.5sXXXXXX"",
        DirName, Sep, Prefix);
    fd = mkstemp(FileName);
    if (fd == -1) {
        Error = errno;
    } else {
        do {
            err = close(fd);
        } while (err == -1 && MR_is_eintr(errno));
        if (err == 0) {
            Error = 0;
        } else {
            Error = errno;
        }
    }
#else
    // Constructs a temporary name by concatenating DirName, `/', the first 5
    // chars of Prefix, six hex digits, and Suffix. The six digit hex number is
    // generated by starting with the pid of this process. Uses
    // `open(..., O_CREATE | O_EXCL, ...)' to create the file, checking that
    // there was no existing file with that name.

    int     err, fd, num_tries;
    int     flags;

    if (ML_io_tempnam_counter == 0) {
        ML_io_tempnam_counter = getpid();
    }
    num_tries = 0;
    do {
        FileName = MR_make_string(MR_ALLOC_ID, ""%s%s%.5s%06lX%s"",
            DirName, Sep, Prefix, ML_io_tempnam_counter & 0xffffffL, Suffix);
        flags = O_WRONLY | O_CREAT | O_EXCL;
        do {
            #ifdef MR_WIN32
                fd = _wopen(ML_utf8_to_wide(FileName), flags, 0600);
            #else
                fd = open(FileName, flags, 0600);
            #endif
        } while (fd == -1 && MR_is_eintr(errno));
        num_tries++;
        ML_io_tempnam_counter += (1 << num_tries);
    } while (fd == -1 && errno == EEXIST &&
        num_tries < ML_MAX_TEMPNAME_TRIES);
    if (fd == -1) {
        Error = errno;
    }  else {
        do {
            err = close(fd);
        } while (err == -1 && MR_is_eintr(errno));
        if (err == 0) {
            Error = 0;
        } else {
            Error = errno;
        }
    }
#endif
").

:- pragma foreign_proc("C#",
    do_make_temp(_DirName::in, _Prefix::in, _Suffix::in, _Sep::in,
        FileName::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        FileName = System.IO.Path.GetTempFileName();
        Error = null;
    } catch (System.Exception e) {
        FileName = """";
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_make_temp(DirName::in, Prefix::in, Suffix::in, _Sep::in, FileName::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    // Java 7 java.nio.file.Files.createTempFile() would allow us
    // to set the file mode.

    if (Prefix.length() > 5) {
        // The documentation for io.make_temp says that we should only use
        // the first five characters of Prefix.
        Prefix = Prefix.substring(0, 5);
    }

    try {
        File new_file = new File(DirName, makeTempName(Prefix, Suffix));
        if (new_file.createNewFile()) {
            FileName = new_file.getAbsolutePath();
            Error = null;
        } else {
            FileName = """";
            // Any other errors should be be reported by createNewFile() by
            // throwing an exception.
            Error = new java.io.IOException(""File already exists"");
        }
    } catch (java.lang.Exception e) {
        FileName = """";
        Error = e;
    }
").

%---------------------%

make_temp_directory(Result, !IO) :-
    io.file.get_temp_directory(ParentDirName, !IO),
    io.file.make_temp_directory(ParentDirName, "mtmp", "", Result, !IO).

make_temp_directory(ParentDirName, Prefix, Suffix, Result, !IO) :-
    do_make_temp_directory(ParentDirName, Prefix, Suffix,
        char_to_string(dir.directory_separator), DirName, Error, !IO),
    is_error(Error, "error creating temporary directory: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(DirName)
    ).

:- pred do_make_temp_directory(string::in, string::in, string::in, string::in,
    string::out, system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_make_temp_directory(ParentDirName::in, Prefix::in, Suffix::in, Sep::in,
        DirName::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        does_not_affect_liveness],
"
#ifdef MR_HAVE_MKDTEMP
    int err;

    // We cannot append Suffix because the last six chars in the argument
    // to mkdtemp() must be XXXXXX.

    DirName = MR_make_string(MR_ALLOC_ID, ""%s%s%.5sXXXXXX"",
        ParentDirName, Sep, Prefix);
    DirName = mkdtemp(DirName);
    if (DirName == NULL) {
        Error = errno;
    } else {
        Error = 0;
    }
#else
    Error = ENOSYS;
    DirName = MR_make_string_const("""");
#endif // HAVE_MKDTEMP
").

:- pragma foreign_proc("C#",
    do_make_temp_directory(ParentDirName::in, _Prefix::in, _Suffix::in,
        _Sep::in, DirName::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        DirName = Path.Combine(ParentDirName, Path.GetRandomFileName());

        switch (Environment.OSVersion.Platform) {
            case PlatformID.Win32NT:
                // obtain the owner of the temporary directory
                IdentityReference tempInfo =
                    new DirectoryInfo(ParentDirName)
                        .GetAccessControl(AccessControlSections.Owner)
                        .GetOwner(typeof(SecurityIdentifier));

                DirectorySecurity security = new DirectorySecurity();
                security.AddAccessRule(
                    new FileSystemAccessRule(tempInfo,
                        FileSystemRights.ListDirectory
                            | FileSystemRights.Read
                            | FileSystemRights.Modify,
                        InheritanceFlags.None,
                        PropagationFlags.None,
                        AccessControlType.Allow
                    )
                );
                Directory.CreateDirectory(DirName, security);
                Error = null;
                break;
#if __MonoCS__
            case PlatformID.Unix:
            case (PlatformID)6: // MacOSX:
                int rc = ML_sys_mkdir(DirName, 0x7 << 6);
                if (rc == 0) {
                    Error = null;
                } else {
                    // The actual error would need to be retrieved from errno.
                    Error = new System.IO.IOException(
                        ""Error creating directory"");
                }
                break;
#endif
            default:
                Error = new System.NotImplementedException(
                    ""Changing folder permissions is not supported for: "" +
                    Environment.OSVersion);
                break;
        }
    } catch (System.Exception e) {
        DirName = string.Empty;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    do_make_temp_directory(ParentDirName::in, Prefix::in, Suffix::in, _Sep::in,
        DirName::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    if (Prefix.length() > 5) {
        // The documentation for io.make_temp says that we should only use
        // the first five characters of Prefix.
        Prefix = Prefix.substring(0, 5);
    }

    try {
        File new_dir = new File(ParentDirName, makeTempName(Prefix, Suffix));
        if (new_dir.mkdir()) {
            DirName = new_dir.getAbsolutePath();
            Error = null;
        } else {
            DirName = """";
            Error = new java.io.IOException(""Error creating directory"");
        }
    } catch (java.lang.Exception e) {
        DirName = """";
        Error = e;
    }
").

%---------------------%

:- pragma foreign_proc("C",
    have_make_temp_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#ifdef MR_HAVE_MKDTEMP
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").

:- pragma foreign_proc("Java",
    have_make_temp_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("C#",
    have_make_temp_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

%---------------------%

:- pragma foreign_decl("C", "
#ifdef MR_HAVE_UNISTD_H
    #include <unistd.h>
#endif
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <fcntl.h>

    #define ML_MAX_TEMPNAME_TRIES   (6 * 4)

    extern long ML_io_tempnam_counter;
").

:- pragma foreign_code("C", "
    long    ML_io_tempnam_counter = 0;
").

:- pragma foreign_decl("C#", "
using System;      // For Environment, PlatformID.
using System.IO;   // For Directory, Path, DirectoryInfo.
using System.Runtime.InteropServices; // For DllImport.
using System.Security.AccessControl;  // For DirectorySecurity etc.
using System.Security.Principal;      // For IdentifyReference etc.
").

:- pragma foreign_code("C#", "
#if __MonoCS__
    // int chmod(const char *path, mode_t mode);
    [DllImport(""libc"", SetLastError=true, EntryPoint=""mkdir"",
        CallingConvention=CallingConvention.Cdecl)]
    static extern int ML_sys_mkdir (string path, uint mode);
#endif
").

:- pragma foreign_decl("Java", local, "
import java.io.File;
import java.io.IOException;
import java.util.Random;
").

:- pragma foreign_code("Java", "
    public static Random ML_rand = new Random();

    public static String makeTempName(String prefix, String suffix)
    {
        StringBuilder sb = new StringBuilder();

        sb.append(prefix);
        // Make an 8-digit mixed case alpha-numeric code.
        for (int i = 0; i < 8; i++) {
            char c;
            int c_num = ML_rand.nextInt(10+26+26);
            if (c_num < 10) {
                c_num = c_num + '0';
            } else if (c_num < 10+26) {
                c_num = c_num + 'A' - 10;
            } else{
                c_num = c_num + 'a' - 10 - 26;
            }
            c = (char)c_num;
            sb.append(c);
        }
        sb.append(suffix);

        return sb.toString();
    }
").

%---------------------%

get_temp_directory(Dir, !IO) :-
    % If using the Java or C# backend then use their API to get the location of
    % temporary files.
    system_temp_dir(Dir0, OK, !IO),
    ( if OK = 1 then
        Dir = Dir0
    else
        % Either this is not a Java or C# grade or the Java or C# backend
        % couldn't determine the temporary directory.
        %
        % We need to do an explicit check of TMPDIR because not all
        % systems check TMPDIR for us (eg Linux #$%*@&).
        Var = ( if dir.use_windows_paths then "TMP" else "TMPDIR" ),
        io.environment.get_environment_var(Var, Result, !IO),
        (
            Result = yes(Dir)
        ;
            Result = no,
            ( if dir.use_windows_paths then
                Dir = dir.this_directory
            else
                Dir = "/tmp"
            )
        )
    ).

:- pred system_temp_dir(string::out, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    system_temp_dir(Dir::out, OK::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        Dir = java.lang.System.getProperty(""java.io.tmpdir"");
        OK = (Dir != null) ? 1 : 0;
    } catch (Exception e) {
        Dir = null;
        OK = 0;
    }
").

:- pragma foreign_proc("C#",
    system_temp_dir(Dir::out, OK::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        Dir = System.IO.Path.GetTempPath();
        OK = (Dir != null) ? 1 : 0;
    } catch (System.Exception) {
        Dir = null;
        OK = 0;
    }
").

system_temp_dir("", 0, !IO).

%---------------------------------------------------------------------------%
:- end_module io.file.
%---------------------------------------------------------------------------%
