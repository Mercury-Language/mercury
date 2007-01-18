%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995,1997,1999-2000,2002-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: dir.m.
% Main authors: fjh, stayl.
% Stability: high.
% 
% Filename and directory handling.
%
% Note that the predicates and functions in this module change directory
% separators in paths passed to them to the normal separator for the platform,
% if that doesn't change the meaning of the path name.
%
% Duplicate directory separators and trailing separators are also removed
% where that doesn't change the meaning of the path name.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module dir.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%
%
% Predicates to isolate system dependencies
%

    % Returns the default separator between components of a pathname --
    % '/' on Unix systems and '\\' on Microsoft Windows systems.
    %
:- func dir.directory_separator = character.
:- pred dir.directory_separator(character::out) is det.

    % Is the character a directory separator.
    % On Microsoft Windows systems this will succeed for '/'
    % as well as '\\'.
    %
:- pred dir.is_directory_separator(character).
:- mode dir.is_directory_separator(in) is semidet.
:- mode dir.is_directory_separator(out) is multi.

    % Returns ".".
    %
:- func dir.this_directory = string.
:- pred dir.this_directory(string::out) is det.

    % Returns "..".
    %
:- func dir.parent_directory = string.
:- pred dir.parent_directory(string::out) is det.

    % dir.split_name(PathName, DirName, BaseName).
    %
    % Split a filename into a directory part and a filename part.
    %
    % Fails for root directories or relative filenames not containing
    % directory information.
    %
    % Trailing slashes are removed from PathName before splitting,
    % if that doesn't change the meaning of PathName.
    %
    % Trailing slashes are removed from DirName after splitting,
    % if that doesn't change the meaning of DirName.
    %
    % On Windows, drive current directories are handled correctly,
    % for example `dir.split_name("C:foo", "C:", "foo")'.
    % (`X:' is the current directory on drive `X').
    % Note that Cygwin doesn't support drive current directories,
    % so `dir.split_name("C:foo, _, _)' will fail when running under Cygwin.
    %
:- pred dir.split_name(string::in, string::out, string::out) is semidet.

    % dir.basename(PathName) = BaseName.
    %
    % Returns the non-directory part of a filename.
    %
    % Fails when given a root directory, ".", ".." or a Windows path
    % such as "X:".
    %
    % Trailing slashes are removed from PathName before splitting,
    % if that doesn't change the meaning of PathName.
    %
:- func dir.basename(string) = string is semidet.
:- pred dir.basename(string::in, string::out) is semidet.

    % As above, but throws an exception instead of failing.
    %
:- func dir.basename_det(string) = string.

    % A synonym for the above.
    %
:- func dir.det_basename(string) = string.

    % dir.dirname(PathName) = DirName.
    %
    % Returns the directory part of a filename.
    %
    % Returns PathName if it specifies a root directory.
    %
    % Returns PathName for Windows paths such as "X:".
    %
    % Returns `dir.this_directory' when given a filename
    % without any directory information (e.g. "foo").
    %
    % Trailing slashes in PathName are removed first, if that doesn't change
    % the meaning of PathName.
    %
    % Trailing slashes are removed from DirName after splitting,
    % if that doesn't change the meaning of DirName.
    %
:- func dir.dirname(string) = string.
:- pred dir.dirname(string::in, string::out) is det.

    % dir.path_name_is_absolute(PathName)
    %
    % Is the path name syntactically an absolute path
    % (this doesn't check whether the path exists).
    %
    % An path is absolute iff it begins with a root directory
    % (see dir.path_name_is_root_directory).
    %
:- pred dir.path_name_is_absolute(string::in) is semidet.

    % dir.path_name_is_root_directory(PathName)
    %
    % On Unix, '/' is the only root directory.
    % On Windows, a root directory is one of the following:
    %   'X:\', which specifies the root directory of drive X,
    %       where X is any letter.
    %   '\', which specifies the root directory of the current drive.
    %   '\\server\share\', which specifies a UNC (Universal Naming
    %       Convention) root directory for a network drive.
    %
    % Note that 'X:' is not a Windows root directory -- it specifies the
    % current directory on drive X, where X is any letter.
    %
:- pred dir.path_name_is_root_directory(string::in) is semidet.

    % PathName = DirName / FileName
    %
    % Given a directory name and a filename, return the pathname of that
    % file in that directory.
    %
    % Duplicate directory separators will not be introduced if
    % DirName ends with a directory separator.
    %
    % On Windows, a call such as `"C:"/"foo"' will return "C:foo".
    %
    % Throws an exception if FileName is an absolute path name.
    % Throws an exception on Windows if FileName is a current
    % drive relative path such as "C:".
    %
:- func string / string = string.
:- func dir.make_path_name(string, string) = string.

%-----------------------------------------------------------------------------%

    % Make the given directory, and all parent directories.
    % This will also succeed if the directory already exists
    % and is readable and writable by the current user.
    %
:- pred dir.make_directory(string::in, io.res::out, io::di, io::uo) is det.

    % Make only the given directory.
    % Fails if the directory already exists, or the parent directory doesn't.
    %
:- pred dir.make_single_directory(string::in, io.res::out, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%

    % FoldlPred(DirName, BaseName, FileType, Continue, !Data, !IO).
    %
    % A predicate passed to dir.foldl2 to process each entry in a directory.
    % Processing will stop if Continue is bound to `no'.
    %
:- type dir.foldl_pred(T) ==
    pred(string, string, io.file_type, bool, T, T, io, io).
:- inst dir.foldl_pred == (pred(in, in, in, out, in, out, di, uo) is det).

    % dir.foldl2(P, DirName, InitialData, Result, !IO).
    %
    % Apply `P' to all files and directories in the given directory.
    % Directories are not processed recursively.
    % Processing will stop if the boolean (Continue) output of P is bound
    % to `no'.
    % The order in which the entries are processed is unspecified.
    %
:- pred dir.foldl2(dir.foldl_pred(T)::in(dir.foldl_pred), string::in,
    T::in, io.maybe_partial_res(T)::out, io::di, io::uo) is det.

    % dir.recursive_foldl2(P, DirName, FollowSymLinks,
    %   InitialData, Result, !IO).
    %
    % As above, but recursively process subdirectories.
    % Subdirectories are processed depth-first, processing the directory itself
    % before its contents. If `FollowSymLinks' is `yes', recursively process
    % the directories referenced by symbolic links.
    %
:- pred dir.recursive_foldl2(dir.foldl_pred(T)::in(dir.foldl_pred),
    string::in, bool::in, T::in, io.maybe_partial_res(T)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Implement brace expansion, as in sh: return the sequence of strings
    % generated from the given input string. Throw an exception if the
    % input string contains mismatched braces.
    %
    % The following is the documentation of brace expansion from the sh manual:
    %
    %   Brace expansion is a mechanism by which arbitrary strings may be
    %   generated. This mechanism is similar to pathname expansion, but the
    %   filenames generated need not exist. Patterns to be brace expanded
    %   take the form of an optional preamble, followed by a series of
    %   comma-separated strings between a pair of braces, followed by an
    %   optional postscript. The preamble is prefixed to each string contained
    %   within the braces, and the postscript is then appended to each
    %   resulting string, expanding left to right.
    %
    %   Brace expansions may be nested. The results of each expanded string
    %   are not sorted; left to right order is preserved. For example,
    %   a{d,c,b}e expands into `ade ace abe'.
    %
:- func expand_braces(string) = list(string).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

% Anything below here will not appear in the Mercury Library Reference Manual.

:- interface.

    % For use by io.m.
    %
:- pred dir.use_windows_paths is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module std_util.
:- import_module string.

%-----------------------------------------------------------------------------%

dir.directory_separator = (if have_win32 then ('\\') else ('/')).

:- pragma foreign_proc("C#",
    dir.directory_separator = (Sep::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Sep = System.IO.Path.DirectorySeparatorChar;
").

:- pragma foreign_proc("Java",
    dir.directory_separator = (Sep::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Sep = java.io.File.separatorChar;
").

:- func dir.alt_directory_separator = char.

dir.alt_directory_separator = (io.have_cygwin -> ('\\') ; ('/')).

:- pragma foreign_proc("C#",
    dir.alt_directory_separator = (Sep::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Sep = System.IO.Path.AltDirectorySeparatorChar;
").

dir.is_directory_separator(Char) :-
    (
        Char = dir.directory_separator,
        Char \= dir.alt_directory_separator
    ;
        Char = dir.alt_directory_separator
    ).

    % Single-moded version of is_directory_separator
    % for passing as a closure.
    %
:- pred dir.is_directory_separator_semidet(char::in) is semidet.

dir.is_directory_separator_semidet(Char) :-
    dir.is_directory_separator(Char).

use_windows_paths :- dir.directory_separator = ('\\').

:- pragma foreign_export("C", (dir.this_directory = out),
    "ML_dir_this_directory").
:- pragma foreign_export("IL", (dir.this_directory = out),
    "ML_dir_this_directory").

dir.this_directory = ".".

dir.parent_directory = "..".

%-----------------------------------------------------------------------------%

dir.det_basename(FileName) = dir.basename_det(FileName).

dir.basename_det(FileName) =
    ( BaseName = dir.basename(FileName) ->
        BaseName
    ;
        func_error("dir.basename_det: given directory is root directory")
    ).

dir.basename(FileName) = BaseName :-
    FileNameChars = canonicalize_path_chars(string.to_char_list(FileName)),
    \+ dir.is_root_directory(FileNameChars),
    \+ (
        % Current directory on the given drive.
        use_windows_paths,
        FileNameChars = [Drive, (':')],
        char.is_alpha(Drive)
    ),

    FileNameWithoutSlash = remove_trailing_dir_separator(FileNameChars),
    FileNameWithoutSlash \= string.to_char_list(dir.this_directory),
    FileNameWithoutSlash \= string.to_char_list(dir.parent_directory),
    ( dir.split_name_2(FileNameChars, _, BaseName0) ->
        BaseName = BaseName0
    ;
        BaseName = FileName
    ).

dir.dirname(FileName) = DirName :-
    FileNameChars = canonicalize_path_chars(string.to_char_list(FileName)),
    (
        dir.is_root_directory(FileNameChars)
    ->
        DirName = string.from_char_list(FileNameChars)
    ;
        % Current directory on the given drive.
        use_windows_paths,
        FileNameChars = [Drive, (':')],
        char.is_alpha(Drive)
    ->
        DirName = string.from_char_list(FileNameChars)
    ;
        dir.split_name_2(FileNameChars, DirName0, _)
    ->
        DirName = DirName0
    ;
        remove_trailing_dir_separator(FileNameChars) =
            string.to_char_list(dir.parent_directory)
    ->
        DirName = dir.parent_directory
    ;
        DirName = dir.this_directory
    ).

dir.split_name(FileName, DirName, BaseName) :-
    FileNameChars = canonicalize_path_chars(string.to_char_list(FileName)),
    \+ is_root_directory(FileNameChars),
    dir.split_name_2(FileNameChars, DirName, BaseName).

    % Check that the filename is not empty or dir.this_directory,
    % pass the directory off to any backend-specific implementations,
    % or if none exist, invoke split_name_3 to split the filename using
    % Mercury code.
    % This assumes that the caller has already checked whether the
    %
    % directory is a root directory.
:- pred dir.split_name_2(list(char)::in, string::out, string::out) is semidet.

dir.split_name_2(FileNameChars0, DirName, BaseName) :-
    FileNameChars0 = [_ | _],
    FileNameWithoutSlash = remove_trailing_dir_separator(FileNameChars0),
    FileNameWithoutSlash \= string.to_char_list(dir.this_directory),
    FileNameWithoutSlash \= string.to_char_list(dir.parent_directory),
    ( io.have_dotnet ->
        % System.IO.Path.GetFileName() returns the empty string
        % if the path ends in a separator).
        dir.split_name_dotnet(string.from_char_list(FileNameWithoutSlash),
            DirName, BaseName)
    ;
        dir.split_name_3(FileNameChars0, DirName, BaseName)
    ).

:- pred dir.split_name_3(list(char)::in, string::out, string::out) is semidet.

dir.split_name_3(FileNameChars, DirName, BaseName) :-
    % Remove any trailing separator.
    RevFileNameChars0 = reverse(FileNameChars),
    (
        RevFileNameChars0 = [LastChar | RevFileNameChars1],
        dir.is_directory_separator(LastChar)
    ->
        RevFileNameChars = RevFileNameChars1
    ;
        RevFileNameChars = RevFileNameChars0
    ),
    (
        list.takewhile(isnt(dir.is_directory_separator_semidet),
            RevFileNameChars, RevBaseName, RevDirName0),
        RevBaseName = [_ | _],
        RevDirName0 = [_ | _]
    ->
        %
        % Strip the trailing separator off the directory name
        % if doing so doesn't change the meaning.
        %
        (
            RevDirName0 = [Sep | RevDirName1],
            \+ (
                dir.is_directory_separator(Sep),
                (
                    ( use_windows_paths
                    ; io.have_cygwin
                    ),
                    RevDirName1 = [(':'), Drive],
                    char.is_alpha(Drive)
                ;
                    RevDirName1 = []
                )
            )
        ->
            RevDirName = RevDirName1
        ;
            RevDirName = RevDirName0
        ),

        BaseName = string.from_rev_char_list(RevBaseName),
        DirName = string.from_rev_char_list(RevDirName)
    ;
        % Check for relative paths of the form `C:foo'.
        use_windows_paths,
        FileNameChars = [Drive, (':') | BaseNameChars],
        char.is_alpha(Drive),
        BaseNameChars = [BaseNameFirst | _],
        \+ dir.is_directory_separator(BaseNameFirst)
    ->
        BaseName = string.from_char_list(BaseNameChars),
        DirName = string.from_char_list([Drive, (':')])
    ;
        fail
    ).

:- pred dir.split_name_dotnet(string::in, string::out, string::out)
    is semidet.

dir.split_name_dotnet(_, "", "") :- semidet_fail.

% The .NET CLI provides functions to split directory names in a
% system-dependent manner.
:- pragma foreign_proc("C#",
    dir.split_name_dotnet(FileName::in, DirName::out, BaseName::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        DirName = System.IO.Path.GetDirectoryName(FileName);
        if (DirName == null || DirName == System.String.Empty) {
            BaseName = null;
            SUCCESS_INDICATOR = false;
        } else {
            BaseName = System.IO.Path.GetFileName(FileName);
            SUCCESS_INDICATOR = (BaseName != null);
        }
    } catch (System.Exception e) {
        BaseName = null;
        DirName = null;
        SUCCESS_INDICATOR = false;
    }
").

    % Remove repeated path separators.
    %
:- func canonicalize_path_chars(list(char)) = list(char).

canonicalize_path_chars(FileName0) = FileName :-
    (
        % Windows allows path names of the form "\\server\share".
        % These path names are referred to as UNC path names.
        ( use_windows_paths ; io.have_cygwin ),
        FileName0 = [Char1 | FileName1],
        is_directory_separator(Char1)
    ->
        % On Cygwin "//" is different to "\\"
        % ("//" is the Cygwin root directory, "\\" is
        % the root directory of the current drive).
        CanonicalChar1 = ( io.have_cygwin -> Char1 ; directory_separator ),
        FileName2 = canonicalize_path_chars_2(FileName1, []),

        % "\\" isn't a UNC path name, so it is equivalent to "\".
        (
            FileName2 = [Char2],
            is_directory_separator(Char2)
        ->
            FileName = [CanonicalChar1]
        ;
            FileName = [CanonicalChar1 | FileName2]
        )
    ;
        FileName = canonicalize_path_chars_2(FileName0, [])
    ).

:- func canonicalize_path_chars_2(list(char), list(char)) = list(char).

canonicalize_path_chars_2([], RevFileName) = reverse(RevFileName).
canonicalize_path_chars_2([C0 | FileName0], RevFileName0) =
        canonicalize_path_chars_2(FileName0, RevFileName) :-
    % Convert all directory separators to the standard separator
    % for the platform, if that doesn't change the meaning.
    % On Cygwin, "\foo\bar" (relative to root of current drive)
    % is different to "/foo/bar" (relative to Cygwin root directory),
    % so we can't convert separators.
    (
        \+ io.have_cygwin,
        is_directory_separator(C0)
    ->
        C = directory_separator
    ;
        C = C0
    ),

    % Remove repeated directory separators.
    (
        dir.is_directory_separator(C),
        FileName0 = [C2 | _],
        dir.is_directory_separator(C2)
    ->
        RevFileName = RevFileName0
    ;
        RevFileName = [C | RevFileName0]
    ).

:- func remove_trailing_dir_separator(list(char)) = list(char).

remove_trailing_dir_separator(Chars) =
    (
        list.split_last(Chars, Chars1, Sep),
        dir.is_directory_separator(Sep)
    ->
        Chars1
    ;
        Chars
    ).

dir.path_name_is_root_directory(PathName) :-
    is_root_directory(canonicalize_path_chars(string.to_char_list(PathName))).

    % Assumes repeated directory separators have been removed.
:- pred is_root_directory(list(char)::in) is semidet.

is_root_directory(FileName) :-
    (
        have_dotnet
    ->
        is_dotnet_root_directory(string.from_char_list(FileName))
    ;
        ( use_windows_paths
        ; io.have_cygwin
        )
    ->
        strip_leading_win32_root_directory(FileName, [])
    ;
        FileName = [Char],
        dir.is_directory_separator(Char)
    ).

    % strip_leading_win32_root_directory(FileName, FileNameMinusRoot)
    %
    % XXX Handle Unicode file names.
    %
:- pred strip_leading_win32_root_directory(list(char)::in,
    list(char)::out) is semidet.

strip_leading_win32_root_directory(!FileName) :-
    ( strip_leading_win32_drive_root_directory(!FileName) ->
        true
    ; strip_leading_win32_unc_root_directory(!FileName) ->
        true
    ;
        strip_leading_win32_current_drive_root_directory(!FileName)
    ).

    % Check for `X:\'.
    % XXX On Cygwin `C:' is treated as being identical to `C:\'.
    % The comments in the Cygwin source imply that this behaviour may change,
    % and it's pretty awful anyway (`C:foo' isn't the same as `C:\foo'),
    % so we don't support it here.
    %
:- pred strip_leading_win32_drive_root_directory(list(char)::in,
    list(char)::out) is semidet.

strip_leading_win32_drive_root_directory([Letter, ':', Sep | !.FileName],
        !:FileName) :-
    char.is_alpha(Letter),
    dir.is_directory_separator(Sep).

    % Check for `\foo...'.
    %
:- pred strip_leading_win32_current_drive_root_directory(list(char)::in,
    list(char)::out) is semidet.

strip_leading_win32_current_drive_root_directory([Char1 | !.FileName],
        !:FileName) :-
    dir.is_directory_separator(Char1),
    (
        !.FileName = []
    ;
        !.FileName = [Char2 | !:FileName],
        \+ dir.is_directory_separator(Char2)
    ).

    % Check for `\\server\' or `\\server\share\'.
    %
:- pred strip_leading_win32_unc_root_directory(list(char)::in, list(char)::out)
    is semidet.

strip_leading_win32_unc_root_directory([Sep, Sep | !.FileName], !:FileName) :-
    dir.is_directory_separator(Sep),
    list.takewhile(isnt(dir.is_directory_separator_semidet), !.FileName,
        Server, !:FileName),
    Server = [_ | _],
    (
        !.FileName = []
    ;
        !.FileName = [Sep | !:FileName],
        (
            !.FileName = []
        ;
            !.FileName = [_|_],
            list.takewhile(isnt(dir.is_directory_separator_semidet),
                !.FileName, Share, !:FileName),
            Share = [_ | _],
            ( !.FileName = [Sep | !:FileName]
            ; !.FileName = []
            )
        )
    ).

:- pred is_dotnet_root_directory(string::in) is semidet.

is_dotnet_root_directory(FileName) :-
    dir.path_name_is_absolute(FileName),
    (
        is_dotnet_root_directory_2(FileName)
    ;
        % For reasons known only to Microsoft,
        % trailing slashes are significant.
        FileNameLen = length(FileName),
        ( FileNameLen > 0 ->
            is_directory_separator(string.unsafe_index(FileName,
                FileNameLen - 1)),
            is_dotnet_root_directory_2(string.left(FileName, FileNameLen - 1))
        ;
            fail
        )
    ).

:- pred is_dotnet_root_directory_2(string::in) is semidet.

is_dotnet_root_directory_2(_) :-
    error("dir.is_dotnet_root_directory called for non-.NET CLI backend").

:- pragma foreign_proc("C#",
    is_dotnet_root_directory_2(FileName::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    try {
        SUCCESS_INDICATOR =
            (System.IO.Path.GetDirectoryName(FileName) == null);
    } catch (System.Exception e) {
        SUCCESS_INDICATOR = false;
    }
}").

%-----------------------------------------------------------------------------%

dir.path_name_is_absolute(FileName) :-
    (
        have_dotnet
    ->
        dotnet_path_name_is_absolute(FileName)
    ;
        ( use_windows_paths
        ; io.have_cygwin
        )
    ->
        strip_leading_win32_root_directory(
            canonicalize_path_chars(string.to_char_list(FileName)), _)
    ;
        string.index(FileName, 0, FirstChar),
        dir.is_directory_separator(FirstChar)
    ).

:- pred dir.dotnet_path_name_is_absolute(string::in) is semidet.

dir.dotnet_path_name_is_absolute(FileName) :-
    dir.dotnet_path_name_is_absolute_2(FileName),

    % The .NET CLI function System.IO.Path.IsPathRooted succeeds for
    % paths such as `C:', which specifies a directory relative to the
    % current directory on drive C.
    \+ (
        use_windows_paths,
        FileNameLen = length(FileName),
        ( FileNameLen >= 2 ->
            char.is_alpha(string.unsafe_index(FileName, 0)),
            string.unsafe_index(FileName, 1) = (':'),
            ( FileNameLen > 2 ->
                \+ dir.is_directory_separator(
                    string.unsafe_index(FileName, 2))
            ;
                true
            )
        ;
            fail
        )
    ).

:- pred dir.dotnet_path_name_is_absolute_2(string::in) is semidet.

dir.dotnet_path_name_is_absolute_2(_) :-
    error("dir.dotnet_path_name_is_absolute_2 called " ++
        "for non-.NET CLI backend").

:- pragma foreign_proc("C#",
    dir.dotnet_path_name_is_absolute_2(FileName::in),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        SUCCESS_INDICATOR = System.IO.Path.IsPathRooted(FileName);
    } catch (System.Exception e) {
        SUCCESS_INDICATOR = false;
    }
").

%-----------------------------------------------------------------------------%

dir.make_path_name(DirName, FileName) = DirName/FileName.

:- pragma foreign_export("C", dir.make_path_name(in, in) = out,
    "ML_make_path_name").
:- pragma foreign_export("IL", dir.make_path_name(in, in) = out,
    "ML_make_path_name").

DirName0/FileName0 = PathName :-
    DirName = string.from_char_list(canonicalize_path_chars(
        string.to_char_list(DirName0))),
    FileName = string.from_char_list(canonicalize_path_chars(
        string.to_char_list(FileName0))),
    (
        dir.path_name_is_absolute(FileName)
    ->
        error("dir./: second argument is absolute")
    ;
        % Check that FileName is not a relative path
        % of the form "C:foo".
        use_windows_paths,
        Length = length(FileName),
        ( Length >= 2 ->
            char.is_alpha(string.unsafe_index(FileName, 0)),
            string.unsafe_index(FileName, 1) = (':'),
            ( Length > 2 ->
                \+ is_directory_separator(string.unsafe_index(FileName, 2))
            ;
                true
            )
        ;
            fail
        )
    ->
        error("dir./: second argument is a current drive relative path")
    ;
        DirNameLength = length(DirName),
        (
            % Check for construction of relative paths
            % of the form "C:foo".
            use_windows_paths,
            DirNameLength = 2,
            char.is_alpha(string.unsafe_index(DirName, 0)),
            string.unsafe_index(DirName, 1) = (':')
        ;
            % Don't introduce duplicate directory separators.
            % On Windows \\foo (a UNC server specification) is
            % not equivalent to \foo (the directory X:\foo, where
            % X is the current drive).
            ( DirNameLength \= 0 ->
                dir.is_directory_separator(string.unsafe_index(DirName,
                    DirNameLength - 1))
            ;

                fail
            )
        )
    ->
        PathName = DirName ++ FileName
    ;
        % Using string.append_list has a fixed overhead of six words, whereas
        % using two string.appends back to back would have a memory overhead
        % proportional to the size of the string copied twice. We prefer the
        % former because it is bounded.
        PathName = string.append_list([DirName,
            string.char_to_string(dir.directory_separator),
            FileName])
    ).

%-----------------------------------------------------------------------------%

dir.make_directory(PathName, Result, !IO) :-
    ( can_implement_make_directory ->
        DirName = dir.dirname(PathName),
        ( PathName = DirName ->
            % We've been asked to make a root directory -- the mkdir will fail.
            dir.make_single_directory_2(0, PathName, Result, !IO)
        ;
            ( DirName = dir.this_directory ->
                % Just go ahead and attempt to make the directory -- if the
                % current directory is not accessible, the mkdir will fail.
                dir.make_single_directory_2(0, PathName, Result, !IO)
            ;
                io.check_file_accessibility(DirName, [],
                    ParentAccessResult, !IO),
                (
                    ParentAccessResult = ok,
                    dir.make_single_directory_2(0, PathName, Result, !IO)
                ;
                    ParentAccessResult = error(_),
                    dir.make_directory(DirName, ParentResult, !IO),
                    (
                        ParentResult = ok,
                        dir.make_single_directory_2(0, PathName, Result, !IO)
                    ;
                        ParentResult = error(_),
                        Result = ParentResult
                    )
                )
            )
        )
    ;
        Result = error(make_io_error(
            "dir.make_directory not implemented on this platform"))
    ).

% The .NET CLI library function System.IO.Directory.CreateDirectory()
% creates the entire path in one call.
:- pragma foreign_proc("C#",
    dir.make_directory(DirName::in, Res::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    try {
        // CreateDirectory doesn't fail if a file with the same
        // name as the directory being created already exists.
        if (System.IO.File.Exists(DirName)) {
            mercury.dir.mercury_code.ML_make_mkdir_res_error(
                new System.Exception(""a file with that name already exists""),
                ref Res);
        } else if (System.IO.Directory.Exists(DirName)) {
            mercury.dir.mercury_code.ML_check_dir_accessibility(DirName,
                ref Res);
        } else {
            System.IO.Directory.CreateDirectory(DirName);
            Res = mercury.dir.mercury_code.ML_make_mkdir_res_ok();
        }
    } catch (System.Exception e) {
        mercury.dir.mercury_code.ML_make_mkdir_res_error(e, ref Res);
    }
}").

% Java has a similar library function java.io.File.mkdirs()
:- pragma foreign_proc("Java",
    dir.make_directory(DirName::in, Res::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    try {
        java.io.File dir = new java.io.File(DirName);
        if (dir.isFile()) {
            throw new java.lang.RuntimeException(
                ""a file with that name already exists"");
        }
        if (dir.isDirectory()) {
            Res = check_dir_accessibility_4_p_0(DirName);
        } else {
            if (!dir.mkdirs()) {
                throw new java.lang.RuntimeException(
                    ""make_directory failed"");
            }
            Res = make_mkdir_res_ok_0_f_0();
        }
    } catch (java.lang.Exception e) {
        Res = make_mkdir_res_error_4_p_0(e);
    }
").

:- pred can_implement_make_directory is semidet.

can_implement_make_directory :- semidet_fail.

:- pragma foreign_proc("C",
    can_implement_make_directory,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#if defined(MR_WIN32)
    SUCCESS_INDICATOR = MR_TRUE;
#elif defined(MR_HAVE_MKDIR)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").
:- pragma foreign_proc("C#",
    can_implement_make_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
"
).
:- pragma foreign_proc("Java",
    can_implement_make_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    succeeded = true;
"
).

dir.make_single_directory(DirName, Result, !IO) :-
    dir.make_single_directory_2(1, DirName, Result, !IO).

:- pred dir.make_single_directory_2(int::in, string::in, io.res::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.make_single_directory_2(ErrorIfExists::in, DirName::in,
        Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates, will_not_modify_trail, does_not_affect_liveness],
"{
#if defined(MR_WIN32)
    if (CreateDirectory(DirName, NULL)) {
        Result = ML_make_mkdir_res_ok();
    } else {
        int error;

        error = GetLastError();
        if (!ErrorIfExists && error == ERROR_ALREADY_EXISTS) {
            ML_make_mkdir_res_exists(error, DirName, &Result);
        } else {
            ML_make_mkdir_res_error(error, &Result);
        }
    }
#elif defined(MR_HAVE_MKDIR)
    if (mkdir(DirName, 0777) == 0) {
        Result = ML_make_mkdir_res_ok();
  #ifdef EEXIST
    } else if (!ErrorIfExists && errno == EEXIST) {
        ML_make_mkdir_res_exists(errno, DirName, &Result);
  #endif /* EEXIST */
    } else {
        ML_make_mkdir_res_error(errno, &Result);
    }
#else /* !MR_WIN32 && !MR_HAVE_MKDIR */
    MR_fatal_error(
        ""dir.make_single_directory_2 called but not supported"");
#endif
    IO = IO0;
}").
:- pragma foreign_proc("C#",
    dir.make_single_directory_2(ErrorIfExists::in, DirName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    try {
    // CreateDirectory doesn't fail if a file with the same
    // name as the directory being created already exists.
    if (System.IO.File.Exists(DirName)) {
        mercury.dir.mercury_code.ML_make_mkdir_res_error(
            new System.Exception(
                ""a file with that name already exists""),
                ref Result);
    } else {
        System.IO.DirectoryInfo info =
            new System.IO.DirectoryInfo(DirName);
        System.IO.DirectoryInfo parent_info = info.Parent;

        if (parent_info == null) {
            mercury.dir.mercury_code.ML_make_mkdir_res_error(
                new System.Exception(""can't create root directory""),
                ref Result);
        } else if (!info.Parent.Exists) {
            mercury.dir.mercury_code.ML_make_mkdir_res_error(
                new System.Exception(""parent directory does not exist""),
                ref Result);
        } else if (ErrorIfExists == 1 && info.Exists) {
            mercury.dir.mercury_code.ML_make_mkdir_res_error(
                new System.Exception(""directory already exists""),
                ref Result);
        } else {
            info.Create();
            Result = mercury.dir.mercury_code.ML_make_mkdir_res_ok();
        }
    }
    } catch (System.Exception e) {
    mercury.dir.mercury_code.ML_make_mkdir_res_error(e, ref Result);
    }
}").

:- pragma foreign_proc("Java",
    dir.make_single_directory_2(ErrorIfExists::in, DirName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    try {
        java.io.File newDir = new java.io.File(DirName);
        java.io.File parent = newDir.getParentFile();

        if (parent == null) {
            Result = make_mkdir_res_error_4_p_0(
                new java.io.IOException(""can't create root directory""));
        } else if (!parent.exists()) {
            Result = make_mkdir_res_error_4_p_0(
                new java.io.IOException(""parent directory does not exist""));
        } else if (ErrorIfExists == 1 && newDir.exists()) {
            Result = make_mkdir_res_error_4_p_0(
                new java.io.IOException(""directory already exists""));
        } else {
            if (!newDir.mkdir()) {
                throw new java.lang.RuntimeException(
                    ""make_single_directory failed"");
            }
            Result = make_mkdir_res_ok_0_f_0();
        }
    } catch (java.lang.Exception e) {
        Result = make_mkdir_res_error_4_p_0(e);
    }
").

:- func dir.make_mkdir_res_ok = io.res.
:- pragma foreign_export("C", (dir.make_mkdir_res_ok = out),
    "ML_make_mkdir_res_ok").
:- pragma foreign_export("IL", (dir.make_mkdir_res_ok = out),
    "ML_make_mkdir_res_ok").

dir.make_mkdir_res_ok = ok.

:- pred dir.make_mkdir_res_error(io.system_error::in, io.res::out,
    io::di, io::uo) is det.
:- pragma foreign_export("C", dir.make_mkdir_res_error(in, out, di, uo),
    "ML_make_mkdir_res_error").
:- pragma foreign_export("IL", dir.make_mkdir_res_error(in, out, di, uo),
    "ML_make_mkdir_res_error").

dir.make_mkdir_res_error(Error, error(make_io_error(Msg)), !IO) :-
    io.make_maybe_win32_err_msg(Error, "dir.make_directory failed: ",
        Msg, !IO).

:- pred dir.make_mkdir_res_exists(io.system_error::in,
    string::in, io.res::out, io::di, io::uo) is det.
:- pragma foreign_export("C", dir.make_mkdir_res_exists(in, in, out, di, uo),
    "ML_make_mkdir_res_exists").
:- pragma foreign_export("IL", dir.make_mkdir_res_exists(in, in, out, di, uo),
    "ML_make_mkdir_res_exists").

dir.make_mkdir_res_exists(Error, DirName, Res, !IO) :-
    io.file_type(yes, DirName, TypeResult, !IO),
    ( TypeResult = ok(directory) ->
        dir.check_dir_accessibility(DirName, Res, !IO)
    ;
        dir.make_mkdir_res_error(Error, Res, !IO)
    ).

:- pred dir.check_dir_accessibility(string::in, io.res::out, io::di, io::uo)
    is det.
:- pragma foreign_export("C", dir.check_dir_accessibility(in, out, di, uo),
    "ML_check_dir_accessibility").
:- pragma foreign_export("IL", dir.check_dir_accessibility(in, out, di, uo),
    "ML_check_dir_accessibility").

dir.check_dir_accessibility(DirName, Res, !IO) :-
    % Check whether we can read and write the directory.
    io.check_file_accessibility(DirName, [read, write, execute], Res, !IO).

%-----------------------------------------------------------------------------%

dir.foldl2(P, DirName, T, Res, !IO) :-
    dir.foldl2_process_dir(no, P, DirName, [], no, no, _, T, Res, !IO).

dir.recursive_foldl2(P, DirName, FollowLinks, T, Res, !IO) :-
    dir.foldl2_process_dir(no, P, DirName, [], yes, FollowLinks, _,
        T, Res, !IO).

:- pred dir.foldl2_process_dir(bool::in,
    dir.foldl_pred(T)::in(dir.foldl_pred), string::in,
    list(file_id)::in, bool::in, bool::in, bool::out, T::in,
    io.maybe_partial_res(T)::out, io::di, io::uo) is det.

:- pred dir.foldl2_process_dir2(dir.stream::in, bool::in,
    dir.foldl_pred(T)::in(dir.foldl_pred), string::in,
    list(file_id)::in, string::in, bool::in, bool::in, T::in,
    {io.maybe_partial_res(T), bool}::out, io::di, io::uo) is det.

dir.foldl2_process_dir2(Dir, SymLinkParent, P, DirName, ParentIds, FirstEntry,
        Recursive, FollowLinks, T0, {Res, Cont}, !IO) :-
    dir.foldl2_process_entries(Dir, SymLinkParent, P, DirName,
        ok(FirstEntry), ParentIds, Recursive, FollowLinks, Cont,
        T0, Res, !IO).

dir.foldl2_process_dir(SymLinkParent, P, DirName, ParentIds0, Recursive,
        FollowLinks, Continue, T0, Result, !IO) :-
    ( can_implement_dir_foldl ->
        (
            Recursive = yes,
            FollowLinks = yes
        ->
            check_for_symlink_loop(SymLinkParent, DirName,
                LoopRes, ParentIds0, ParentIds, !IO)
        ;
            ParentIds = ParentIds0,
            LoopRes = ok(no)
        ),
        (
            LoopRes = ok(no),
            dir.open(DirName, OpenResult, !IO),
            (
                OpenResult = ok({Dir, FirstEntry}),

                % We need to close the directory if an
                % exception is thrown to avoid resource leaks.
                Cleanup = dir.close(Dir),
                exception.finally(dir.foldl2_process_dir2(Dir, SymLinkParent,
                    P, DirName, ParentIds, FirstEntry, Recursive, FollowLinks,
                    T0), {DirRes, Continue}, Cleanup, CleanupRes, !IO),
                (
                    DirRes = ok(T),
                    (
                        CleanupRes = ok,
                        Result = DirRes
                    ;
                        CleanupRes = error(Error),
                        Result = error(T, Error)
                    )
                ;
                    DirRes = error(_, _),
                    Result = DirRes
                )
            ;
                OpenResult = eof,
                Continue = yes,
                Result = ok(T0)
            ;
                OpenResult = error(Error),
                Continue = no,
                Result = error(T0, Error)
            )
        ;
            LoopRes = ok(yes),

            Continue = yes,
            Result = ok(T0)
        ;
            LoopRes = error(Error),

            Continue = no,
            Result = error(T0, Error)
        )
    ;
        Continue = no,
        Result = error(T0, make_io_error("dir.foldl2 " ++
            "not implemented on this platform"))
    ).

:- pred dir.foldl2_process_entries(dir.stream::in, bool::in,
    dir.foldl_pred(T)::in(dir.foldl_pred), string::in,
    io.result(string)::in, list(file_id)::in, bool::in,
    bool::in, bool::out, T::in, io.maybe_partial_res(T)::out,
    io::di, io::uo) is det.

dir.foldl2_process_entries(_, _, _, _, error(Error), _, _, _, no,
        T0, error(T0, Error), !IO).
dir.foldl2_process_entries(_, _, _, _, eof, _, _, _, yes, T0, ok(T0), !IO).
dir.foldl2_process_entries(Dir, SymLinkParent, P, DirName, ok(FileName),
        ParentIds, Recursive, FollowLinks, Continue, T0, Res, !IO) :-
    PathName = DirName/FileName,
    io.file_type(no, PathName, FileTypeRes, !IO),
    (
        FileTypeRes = ok(Type),
        P(DirName, FileName, Type, Continue1, T0, T1, !IO),
        (
            Continue1 = yes,
            (
                Recursive = yes,
                Type = directory
            ->
                dir.foldl2_process_dir(SymLinkParent, P, PathName, ParentIds,
                    Recursive, FollowLinks, Continue2, T1, Res1, !IO)
            ;
                Recursive = yes,
                Type = symbolic_link,
                FollowLinks = yes
            ->
                io.file_type(yes, PathName, TargetTypeRes, !IO),
                (
                    TargetTypeRes = ok(TargetType),
                    ( TargetType = directory ->
                        dir.foldl2_process_dir(yes, P, PathName, ParentIds,
                            Recursive, FollowLinks, Continue2, T1, Res1, !IO)
                    ;
                        Continue2 = yes,
                        Res1 = ok(T1)
                    )
                ;
                    TargetTypeRes = error(TargetTypeError),
                    Continue2 = no,
                    Res1 = error(T1, TargetTypeError)
                )
            ;
                Continue2 = yes,
                Res1 = ok(T1)
            ),
            (
                Continue2 = yes,
                Res1 = ok(T)
            ->
                dir.read_entry(Dir, EntryResult, !IO),
                dir.foldl2_process_entries(Dir, SymLinkParent, P, DirName,
                    EntryResult, ParentIds, Recursive, FollowLinks, Continue,
                    T, Res, !IO)
            ;
                Continue = no,
                Res = Res1
            )
        ;
            Continue1 = no,
            Res = ok(T1),
            Continue = no
        )
    ;
        FileTypeRes = error(Error),
        Continue = no,
        Res = error(T0, Error)
    ).

    % Check whether we've seen this directory before in this branch of the
    % directory tree. This only works if the system can provide a unique
    % identifier for each file. Returns `ok(DetectedLoop : bool)' on success.
    %
:- pred check_for_symlink_loop(bool::in, string::in, io.res(bool)::out,
    list(file_id)::in, list(file_id)::out, io::di, io::uo) is det.

check_for_symlink_loop(SymLinkParent, DirName, LoopRes, !ParentIds, !IO) :-
    ( io.have_symlinks ->
        io.file_id(DirName, IdRes, !IO),
        (
            IdRes = ok(Id),
            (
                SymLinkParent = yes,
                list.member(Id, !.ParentIds)
            ->
                Loop = yes
            ;
                !:ParentIds = [Id | !.ParentIds],
                Loop = no
            ),
            LoopRes = ok(Loop)
        ;
            IdRes = error(Msg),
            LoopRes = error(Msg)
        )
    ;
        LoopRes = ok(no)
    ).

% MS-Windows doesn't provide the POSIX directory functions.
:- pragma foreign_decl("C", "

#include ""mercury_string.h""
#include ""mercury_types.h""

#if defined(MR_WIN32) && defined(MR_HAVE_WINDOWS_H)
  #include <windows.h>
#endif

#ifdef HAVE_UNISTD_H
  #include <unistd.h>
#endif

#ifdef MR_HAVE_SYS_TYPES_H
  #include <sys/types.h>
#endif

#ifdef MR_HAVE_DIRENT_H
  #include <dirent.h>
#endif

#if defined(MR_WIN32)
  typedef   HANDLE      ML_DIR_STREAM;
#elif defined(MR_HAVE_READDIR)
  typedef   DIR *       ML_DIR_STREAM;
#else
  typedef   MR_Integer  ML_DIR_STREAM;
#endif
").

    % A dir.stream should be treated like an io.input_stream,
    % except using dir.read_entry, rather than io.read_char.
    % dir.streams must be closed to avoid resource leaks.
:- type dir.stream ---> dir.stream.
:- pragma foreign_type("C", dir.stream, "ML_DIR_STREAM").
:- pragma foreign_type("il", dir.stream,
    "class [mscorlib]System.Collections.IEnumerator").
:- pragma foreign_type("Java", dir.stream, "java.util.Iterator").

:- pred can_implement_dir_foldl is semidet.

can_implement_dir_foldl :- semidet_fail.
:- pragma foreign_proc("C",
    can_implement_dir_foldl,
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        does_not_affect_liveness],
"
#if defined(MR_HAVE_OPENDIR) && defined(MR_HAVE_READDIR) && \\
        defined(MR_HAVE_CLOSEDIR)
    SUCCESS_INDICATOR = MR_TRUE;
#elif defined(MR_WIN32)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").
:- pragma foreign_proc("C#",
    can_implement_dir_foldl,
    [will_not_call_mercury, promise_pure, thread_safe],
    "SUCCESS_INDICATOR = true;"
).

    % Win32 doesn't allow us to open a directory without
    % returning the first item.
    %
:- pred dir.open(string::in, io.result({dir.stream, string})::out,
    io::di, io::uo) is det.

dir.open(DirName, Res, !IO) :-
    ( can_implement_dir_foldl ->
        dir.open_2(DirName, Res, !IO)
    ;
        Res = error(io.make_io_error("dir.foldl2 not implemented " ++
            "on this platform"))
    ).

:- pred dir.open_2(string::in, io.result({dir.stream, string})::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.open_2(DirName::in, Result::out, IO0::di, IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates, will_not_modify_trail, does_not_affect_liveness],
"{
#if defined(MR_WIN32)
    WIN32_FIND_DATA file_data;
    ML_DIR_STREAM Dir;
    LPTSTR FirstFileName;
    char *dir_pattern;
    MR_Integer is_readable;

    ML_check_dir_readable(DirName, &is_readable, &Result);
    if (is_readable) {
        dir_pattern = ML_make_path_name(DirName, MR_make_string_const(""*""));
        Dir = FindFirstFile(dir_pattern, &file_data);
        if (Dir == INVALID_HANDLE_VALUE) {
            int error = GetLastError();
            if (error == ERROR_NO_MORE_FILES) {
                Result = ML_make_dir_open_result_eof();
            } else {
                ML_make_dir_open_result_error(error, &Result);
            }
        } else {
            ML_make_win32_dir_open_result_ok(Dir,
                (MR_Word) file_data.cFileName, &Result);
        }
    }

#elif defined(MR_HAVE_OPENDIR) && defined(MR_HAVE_READDIR) && \\
        defined(MR_HAVE_CLOSEDIR)
    ML_DIR_STREAM Dir;

    Dir = opendir(DirName);
    if (Dir == NULL) {
        ML_make_dir_open_result_error(errno, &Result);
    } else {
        ML_dir_read_first_entry(Dir, &Result);
    }

#else /* !MR_WIN32 && !(MR_HAVE_OPENDIR etc.) */
    MR_fatal_error(""dir.open called but not supported"");
#endif
    IO = IO0;
}").

:- pragma foreign_proc("C#",
    dir.open_2(DirName::in, Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    try {
        System.Collections.IEnumerator Dir =
            System.IO.Directory.GetFileSystemEntries(DirName).GetEnumerator();
        mercury.dir.mercury_code.ML_dir_read_first_entry(Dir, ref Result);
    } catch (System.Exception e) {
        mercury.dir.mercury_code.ML_make_dir_open_result_error(e, ref Result);
    }
}").

:- pragma foreign_proc("Java",
    dir.open_2(DirName::in, Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    try {
        java.lang.String[] fileList = (new java.io.File(DirName)).list();
        java.util.List list = java.util.Arrays.asList(fileList);

        Result = read_first_entry_4_p_0(list.iterator());
    } catch (java.lang.Exception e) {
        Result = make_dir_open_result_error_4_p_0(e);
    }
").

:- pred dir.check_dir_readable(string::in, int::out,
    io.result({dir.stream, string})::out, io::di, io::uo) is det.
:- pragma foreign_export("C", dir.check_dir_readable(in, out, out, di, uo),
    "ML_check_dir_readable").
:- pragma foreign_export("IL", dir.check_dir_readable(in, out, out, di, uo),
    "ML_check_dir_readable").

dir.check_dir_readable(DirName, IsReadable, Result, !IO) :-
    io.file_type(yes, DirName, FileTypeRes, !IO),
    (
        FileTypeRes = ok(FileType),
        ( FileType = directory ->
            io.check_file_accessibility(DirName, [read, execute],
                AccessResult, !IO),
            (
                AccessResult = ok,
                IsReadable = 1,
                % This will not be used.
                Result = error(make_io_error("no error"))
            ;
                AccessResult = error(Msg),
                IsReadable = 0,
                Result = error(Msg)
            )
        ;
            IsReadable = 0,
            Result = error(make_io_error(
                "dir.foldl2: pathname is not a directory"))

        )
    ;
        FileTypeRes = error(Msg),
        IsReadable = 0,
        Result = error(Msg)
    ).

:- pred dir.read_first_entry(dir.stream::in,
    io.result({dir.stream, string})::out, io::di, io::uo) is det.
:- pragma foreign_export("C", dir.read_first_entry(in, out, di, uo),
    "ML_dir_read_first_entry").
:- pragma foreign_export("IL", dir.read_first_entry(in, out, di, uo),
    "ML_dir_read_first_entry").

dir.read_first_entry(Dir, Result, !IO) :-
    dir.read_entry(Dir, EntryResult, !IO),
    (
        EntryResult = ok(FirstEntry),
        Result = ok({Dir, FirstEntry})
    ;
        EntryResult = eof,
        Result = eof
    ;
        EntryResult = error(Msg),
        Result = error(Msg)
    ).

:- pred make_win32_dir_open_result_ok(dir.stream::in, c_pointer::in,
    io.result({dir.stream, string})::out, io::di, io::uo) is det.
:- pragma foreign_export("C", make_win32_dir_open_result_ok(in, in, out, di, uo),
    "ML_make_win32_dir_open_result_ok").
:- pragma foreign_export("IL", make_win32_dir_open_result_ok(in, in, out, di, uo),
    "ML_make_win32_dir_open_result_ok").

make_win32_dir_open_result_ok(Dir, FirstFilePtr, Result, !IO) :-
    FirstFile0 = copy_c_string(FirstFilePtr),
    (
        ( FirstFile0 = dir.this_directory
        ; FirstFile0 = dir.parent_directory
        )
    ->
        dir.read_entry(Dir, ReadResult, !IO),
        (
            ReadResult = ok(FirstFile),
            Result = ok({Dir, FirstFile})
        ;
            ReadResult = eof,
            dir.close(Dir, CloseRes, !IO),
            ( CloseRes = ok, Result = eof
            ; CloseRes = error(Error), Result = error(Error)
            )
        ;
            ReadResult = error(Error),
            dir.close(Dir, _, !IO),
            Result = error(Error)
        )
    ;
        Result = ok({Dir, FirstFile0})
    ).

    % This is needed because the heap pointer is not valid in the
    % `may_call_mercury' foreign proc for dir.open_2. Instead, we pass it
    % as a c_pointer to copy_c_string, which doesn't call Mercury, so the
    % heap pointer is valid. Passing it as a c_pointer avoids having the
    % accurate garbage collector attempt to copy a potentially unaligned
    % string.
    %
:- func copy_c_string(c_pointer) = string.

copy_c_string(_) = _ :-
    error("dir.copy_c_string should only be called " ++
        "by code generated by C backends").

:- pragma foreign_proc("C",
    copy_c_string(Ptr::in) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe,
        will_not_modify_trail, does_not_affect_liveness],
"
    MR_make_aligned_string_copy(Str, (char *) Ptr);
").

:- func make_dir_open_result_eof = io.result({dir.stream, string}).
:- pragma foreign_export("C", (make_dir_open_result_eof = out),
    "ML_make_dir_open_result_eof").
:- pragma foreign_export("IL", (make_dir_open_result_eof = out),
    "ML_make_dir_open_result_eof").

make_dir_open_result_eof = eof.

:- pred make_dir_open_result_error(io.system_error::in,
    io.result({dir.stream, string})::out, io::di, io::uo) is det.
:- pragma foreign_export("C", make_dir_open_result_error(in, out, di, uo),
    "ML_make_dir_open_result_error").
:- pragma foreign_export("IL", make_dir_open_result_error(in, out, di, uo),
    "ML_make_dir_open_result_error").

make_dir_open_result_error(Error, error(io.make_io_error(Msg)), !IO) :-
    io.make_err_msg(Error, "dir.foldl2: opening directory failed: ", Msg, !IO).

:- pred dir.close(dir.stream::in, io.res::out, io::di, io::uo) is det.

dir.close(Dir, Res, !IO) :-
    dir.close_2(Dir, Status, Error, !IO),
    ( Status = 0 ->
        io.make_maybe_win32_err_msg(Error,
            "dir.foldl2: closing directory failed: ", Msg, !IO),
        Res = error(io.make_io_error(Msg))
    ;
        Res = ok
    ).

:- pred dir.close_2(dir.stream::in, int::out, io.system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.close_2(Dir::in, Status::out, Error::out, IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness],
"{
    IO = IO0;
#if defined(MR_WIN32)
    Status = FindClose(Dir);
    Error = GetLastError();
#elif defined(MR_HAVE_CLOSEDIR)
    Status = (closedir(Dir) == 0);
    Error = errno;
#else
    MR_fatal_error(""dir.open called but not supported"");
#endif
}").

:- pragma foreign_proc("C#",
    dir.close_2(_Dir::in, Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    /* Nothing to do. */
    Error = null;
    Status = 1;
}").

:- pragma foreign_proc("Java",
    dir.close_2(_Dir::in, Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    /* Nothing to do. */
    Error = null;
    Status = 1;
}").

:- pred dir.read_entry(dir.stream::in, io.result(string)::out,
    io::di, io::uo) is det.

dir.read_entry(Dir, Res, !IO) :-
    dir.read_entry_2(Dir, Status, Error, FileName, !IO),
    (
        Status = 0
    ->
        io.make_maybe_win32_err_msg(Error,
            "dir.foldl2: reading directory entry failed: ", Msg, !IO),
        Res = error(io.make_io_error(Msg))
    ;
        Status = -1
    ->
        Res = eof
    ;
        ( FileName = dir.this_directory
        ; FileName = dir.parent_directory
        )
    ->
        dir.read_entry(Dir, Res, !IO)
    ;
        Res = ok(FileName)
    ).

    % dir.read_entry_2(Dir, Status, Error, FileName, !IO).
    % Status is -1 for EOF, 0 for error, 1 for success.
    %
:- pred dir.read_entry_2(dir.stream::in, int::out, io.system_error::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.read_entry_2(Dir::in, Status::out, Error::out, FileName::out,
        IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness],
"{
#if defined(MR_WIN32)
    WIN32_FIND_DATA file_data;

    IO = IO0;
    if (FindNextFile(Dir, &file_data)) {
        Status = 1;
        MR_make_aligned_string_copy(FileName, file_data.cFileName);
    } else {
        Error = GetLastError();
        Status = (Error == ERROR_NO_MORE_FILES ? -1 : 0);
        FileName = NULL;
    }

#elif defined(MR_HAVE_READDIR) && defined(MR_HAVE_CLOSEDIR)
    struct dirent *dir_entry;

    IO = IO0;
    errno = 0;
    dir_entry = readdir(Dir);
    if (dir_entry == NULL) {
        Error = errno;
        FileName = NULL;
        Status = (Error == 0 ? -1 : 0);
    } else {
        MR_make_aligned_string_copy(FileName, dir_entry->d_name);
        Error = 0;
        Status = 1;
    }

#else /* !MR_WIN32 && !(MR_HAVE_READDIR etc.) */
    MR_fatal_error(""dir.read_entry_2 called but not supported"");
#endif
}").

:- pragma foreign_proc("C#",
    dir.read_entry_2(Dir::in, Status::out, Error::out, FileName::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    try {
        if (Dir.MoveNext()) {
            // The .NET CLI returns path names qualified with
            // the directory name passed to dir.open.
            FileName = System.IO.Path.GetFileName((string) Dir.Current);
            Status = 1;
        } else {
            FileName = null;
            Status = -1;
        }
        Error = null;
    } catch (System.Exception e) {
        Error = e;
        FileName = null;
        Status = 0;
    }
}").

:- pragma foreign_proc("Java",
    dir.read_entry_2(Dir::in, Status::out, Error::out, FileName::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    if (Dir.hasNext()) {
        FileName = (java.lang.String) Dir.next();
        Status = 1;
    } else {
        FileName = null;
        Status = -1;
    }
    Error = null;
").

%-----------------------------------------------------------------------------%

expand_braces(ArgStr) = ExpandStrs :-
    ArgChar = string.to_char_list(ArgStr),
    ExpandChars = expand(ArgChar),
    ExpandStrs = list.map(string.from_char_list, ExpandChars).

:- func expand(list(char)) = list(list(char)).

expand(Chars) = expand_acc(Chars, [[]]).

:- func expand_acc(list(char), list(list(char))) = list(list(char)).

expand_acc([], Prefixes) = Prefixes.
expand_acc([Char | Chars], Prefixes0) = Strings :-
    ( Char = '{' ->
        find_matching_brace(Chars, Alternatives0, Left),
        AlternativeLists = list.map(expand, Alternatives0),
        Alternatives = list.condense(AlternativeLists),
        PrefixLists = list.map(add_alternatives(Alternatives), Prefixes0),
        Prefixes1 = list.condense(PrefixLists),
        expand_acc(Left, Prefixes1) = Strings
    ;
        Prefixes1 = list.map(add_char_at_end(Char), Prefixes0),
        Strings = expand_acc(Chars, Prefixes1)
    ).

:- func add_alternatives(list(list(char)), list(char)) = list(list(char)).

add_alternatives(Alternatives, Prefix) =
    list.map(list.append(Prefix), Alternatives).

:- func add_char_at_end(char, list(char)) = list(char).

add_char_at_end(Char, Prefix) = list.append(Prefix, [Char]).

:- pred find_matching_brace(list(char)::in, list(list(char))::out,
    list(char)::out) is det.

find_matching_brace(Chars, Alternatives, Left) :-
    find_matching_brace_or_comma(Chars, [], [], 0, Alternatives, Left).

:- pred find_matching_brace_or_comma(list(char)::in, list(list(char))::in,
    list(char)::in, int::in, list(list(char))::out, list(char)::out) is det.

find_matching_brace_or_comma([], _, _, _, _, _) :-
    throw("no matching brace").
find_matching_brace_or_comma([Char | Chars], Alternatives0, CurAlternative,
        BraceLevel, Alternatives, Left) :-
    ( Char = '}' ->
        ( BraceLevel = 0 ->
            list.append(Alternatives0, [CurAlternative], Alternatives),
            Left = Chars
        ;
            find_matching_brace_or_comma(Chars, Alternatives0,
                list.append(CurAlternative, [Char]),
                BraceLevel - 1, Alternatives, Left)
        )
    ; Char = '{' ->
        find_matching_brace_or_comma(Chars, Alternatives0,
            list.append(CurAlternative, [Char]),
            BraceLevel + 1, Alternatives, Left)
    ; Char = (',') ->
        ( BraceLevel = 0 ->
            list.append(Alternatives0, [CurAlternative], Alternatives1),
            find_matching_brace_or_comma(Chars, Alternatives1,
                [], BraceLevel, Alternatives, Left)
        ;
            find_matching_brace_or_comma(Chars, Alternatives0,
                list.append(CurAlternative, [Char]),
                BraceLevel, Alternatives, Left)
        )
    ;
        find_matching_brace_or_comma(Chars, Alternatives0,
            list.append(CurAlternative, [Char]),
            BraceLevel, Alternatives, Left)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%   Functional forms added.

dir.directory_separator(dir.directory_separator).

dir.this_directory(dir.this_directory).

dir.parent_directory(dir.parent_directory).

dir.basename(S, dir.basename(S)).

dir.dirname(S, dir.dirname(S)).
