%-----------------------------------------------------------------------%
% vim: ft=mercury sts=4 sw=4 et tw=78
%-----------------------------------------------------------------------%
% Copyright (C) 2016, 2018 The Mercury Team
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------%
%
% This module contains alternatives to io.make_temp and
% io.make_temp_directory that properly set the permission bits during
% creation.  However this cannot be included in io.m as it requires Java
% 1.7.
%
%-----------------------------------------------------------------------%
:- module make_temp.

:- interface.

:- import_module io.
:- import_module string.

%-----------------------------------------------------------------------%

:- type make_temp_error
    --->    make_temp_error(string).

%-----------------------------------------------------------------------%

    % make_temp(Name, !IO) creates an empty file whose name is different
    % to the name of any existing file. Name is bound to the name of the file.
    % It is the responsibility of the caller to delete the file when it
    % is no longer required.
    %
    % The file is placed in the directory returned by get_temp_directory/3.
    %
:- pred make_temp(string::out, io::di, io::uo) is det.

    % make_temp(Dir, Prefix, Name, !IO) creates an empty file whose
    % name is different to the name of any existing file. The file will reside
    % in the directory specified by Dir and will have a prefix using up to
    % the first 5 characters of Prefix. Name is bound to the name of the
    % file.  It is the responsibility of the caller to delete the file when it
    % is no longer required.
    %
    % The C# backend has the following limitations:
    %   - Dir is ignored.
    %   - Prefix is ignored.
    %
:- pred make_temp(string::in, string::in, string::out, io::di, io::uo)
    is det.

    % make_temp_directory(DirName, !IO) creates an empty directory whose name
    % is different from the name of any existing directory.
    %
    % On the C# backend this is insecure as the file permissions are not set
    % and this call does not test for an existing directory.
    %
:- pred make_temp_directory(string::out, io::di, io::uo) is det.

    % make_temp_directory(Dir, Prefix, DirName, !IO) creates an empty directory
    % whose name is different from the name of any existing directory.  The new
    % directory will reside in the existing directory specified by `Dir' and
    % will have a prefix using up to the first 5 characters of `Prefix'.
    % DirName is bound to the name of the new directory. It is the
    % responsibility of the program to delete the directory when it is no
    % longer needed.
    %
    % The C# backend has the following limitations:
    %   - It does not attempt to create the file with restrictive permissions
    %     (600 on Unix-like systems) and therefore should not be used when
    %     security is required.
    %   - Prefix is ignored.
    %
:- pred make_temp_directory(string::in, string::in, string::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module exception.
:- import_module io.file.

%-----------------------------------------------------------------------%

:- pragma foreign_decl("Java", local,
"
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFilePermissions;
").

%-----------------------------------------------------------------------%

make_temp(Name, !IO) :-
    io.file.get_temp_directory(Dir, !IO),
    make_temp.make_temp(Dir, "mtmp", Name, !IO).

make_temp(Dir, Prefix, Name, !IO) :-
    do_make_temp(Dir, Prefix, char_to_string(dir.directory_separator),
        Name, Okay, Message, !IO),
    (
        Okay = yes
    ;
        Okay = no,
        throw(make_temp_error(Message))
    ).

make_temp_directory(DirName, !IO) :-
    io.file.get_temp_directory(Dir, !IO),
    make_temp.make_temp_directory(Dir, "mtmp", DirName, !IO).

make_temp_directory(Dir, Prefix, DirName, !IO) :-
    do_make_temp_directory(Dir, Prefix,
        char_to_string(dir.directory_separator), DirName, Okay, Message, !IO),
    (
        Okay = yes
    ;
        Okay = no,
        throw(make_temp_error(Message))
    ).

%-----------------------------------------------------------------------%

:- pred do_make_temp(string::in, string::in, string::in,
    string::out, bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    do_make_temp(Dir::in, Prefix::in, _Sep::in, FileName::out,
        Okay::out, ErrorMessage::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        Path dir_path, new_file;

        if (Prefix.length() > 5) {
            // The documentation for io.make_temp says that we should only use
            // the first five characters of Prefix.
            Prefix = Prefix.substring(0, 5);
        }
        dir_path = Paths.get(Dir);
        new_file = Files.createTempFile(dir_path, Prefix, null);
        FileName = new_file.toAbsolutePath().toString();
        Okay = bool.YES;
        ErrorMessage = """";
    } catch (java.lang.Exception e) {
        FileName = """";
        Okay = bool.NO;
        ErrorMessage = e.toString();
    }
").

:- pred do_make_temp_directory(string::in, string::in, string::in,
    string::out, bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    do_make_temp_directory(Dir::in, Prefix::in, _Sep::in, DirName::out,
        Okay::out, ErrorMessage::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        Path dir_path, new_file;

        if (Prefix.length() > 5) {
            // The documentation for io.make_temp says that we should only use
            // the first five characters of Prefix.
            Prefix = Prefix.substring(0, 5);
        }
        dir_path = Paths.get(Dir);
        new_file = Files.createTempDirectory(dir_path, Prefix);
        DirName = new_file.toAbsolutePath().toString();
        Okay = bool.YES;
        ErrorMessage = """";
    } catch (java.lang.Exception e) {
        DirName = """";
        Okay = bool.NO;
        ErrorMessage = e.toString();
    }
").

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
