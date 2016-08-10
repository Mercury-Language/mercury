%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995,1997,1999-2000,2002-2012 The University of Melbourne.
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
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module dir.
:- interface.

:- import_module bool.
:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%
%
% Predicates to isolate system dependencies
%

    % Returns the default separator between components of a pathname --
    % '/' on Unix systems and '\\' on Microsoft Windows systems.
    %
:- func directory_separator = character.
:- pred directory_separator(character::out) is det.

    % Is the character a directory separator.
    % On Microsoft Windows systems this will succeed for '/'
    % as well as '\\'.
    %
:- pred is_directory_separator(character).
:- mode is_directory_separator(in) is semidet.
:- mode is_directory_separator(out) is multi.

    % Returns ".".
    %
:- func this_directory = string.
:- pred this_directory(string::out) is det.

    % Returns "..".
    %
:- func parent_directory = string.
:- pred parent_directory(string::out) is det.

    % split_name(PathName, DirName, BaseName).
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
    % for example `split_name("C:foo", "C:", "foo")'.
    % (`X:' is the current directory on drive `X').
    % Note that Cygwin doesn't support drive current directories,
    % so `split_name("C:foo, _, _)' will fail when running under Cygwin.
    %
:- pred split_name(string::in, string::out, string::out) is semidet.

    % basename(PathName) = BaseName.
    %
    % Returns the non-directory part of a filename.
    %
    % Fails when given a root directory, ".", ".." or a Windows path
    % such as "X:".
    %
    % Trailing slashes are removed from PathName before splitting,
    % if that doesn't change the meaning of PathName.
    %
:- func basename(string) = string is semidet.
:- pred basename(string::in, string::out) is semidet.

    % As above, but throws an exception instead of failing.
    %
:- func det_basename(string) = string.

    % dirname(PathName) = DirName.
    %
    % Returns the directory part of a filename.
    %
    % Returns PathName if it specifies a root directory.
    %
    % Returns PathName for Windows paths such as "X:".
    %
    % Returns `this_directory' when given a filename
    % without any directory information (e.g. "foo").
    %
    % Trailing slashes in PathName are removed first, if that doesn't change
    % the meaning of PathName.
    %
    % Trailing slashes are removed from DirName after splitting,
    % if that doesn't change the meaning of DirName.
    %
:- func dirname(string) = string.
:- pred dirname(string::in, string::out) is det.

    % path_name_is_absolute(PathName)
    %
    % Is the path name syntactically an absolute path
    % (this doesn't check whether the path exists).
    %
    % An path is absolute iff it begins with a root directory
    % (see path_name_is_root_directory).
    %
:- pred path_name_is_absolute(string::in) is semidet.

    % path_name_is_root_directory(PathName)
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
:- pred path_name_is_root_directory(string::in) is semidet.

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
:- func make_path_name(string, string) = string.

    % relative_path_name_from_components(List) = PathName.
    %
    % Return the relative pathname from the components in the list.
    % The components of the list must not contain directory separators.
    %
:- func relative_path_name_from_components(list(string)) = string.

%---------------------------------------------------------------------------%

    % current_directory(Result)
    % Return the current working directory.
    %
:- pred current_directory(io.res(string)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Make the given directory, and all parent directories.
    % This will also succeed if the directory already exists
    % and is readable and writable by the current user.
    %
:- pred make_directory(string::in, io.res::out, io::di, io::uo) is det.

    % Make only the given directory.
    % Fails if the directory already exists, or the parent directory doesn't.
    %
:- pred make_single_directory(string::in, io.res::out, io::di, io::uo)
    is det.

%---------------------------------------------------------------------------%

    % FoldlPred(DirName, BaseName, FileType, Continue, !Data, !IO).
    %
    % A predicate passed to foldl2 to process each entry in a directory.
    % Processing will stop if Continue is bound to `no'.
    %
:- type foldl_pred(T) ==
    pred(string, string, io.file_type, bool, T, T, io, io).
:- inst foldl_pred == (pred(in, in, in, out, in, out, di, uo) is det).

    % foldl2(P, DirName, InitialData, Result, !IO).
    %
    % Apply `P' to all files and directories in the given directory.
    % Directories are not processed recursively.
    % Processing will stop if the boolean (Continue) output of P is bound
    % to `no'.
    % The order in which the entries are processed is unspecified.
    %
:- pred foldl2(foldl_pred(T)::in(foldl_pred), string::in,
    T::in, io.maybe_partial_res(T)::out, io::di, io::uo) is det.

    % recursive_foldl2(P, DirName, FollowSymLinks,
    %   InitialData, Result, !IO).
    %
    % As above, but recursively process subdirectories.
    % Subdirectories are processed depth-first, processing the directory itself
    % before its contents. If `FollowSymLinks' is `yes', recursively process
    % the directories referenced by symbolic links.
    %
:- pred recursive_foldl2(foldl_pred(T)::in(foldl_pred),
    string::in, bool::in, T::in, io.maybe_partial_res(T)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% Anything below here will not appear in the Mercury Library Reference Manual.

:- interface.

    % For use by io.m.
    %
:- pred dir.use_windows_paths is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module require.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

directory_separator = (if have_win32 then ('\\') else ('/')).

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

directory_separator(dir.directory_separator).

:- func dir.alt_directory_separator = char.

alt_directory_separator = (if io.have_cygwin then ('\\') else ('/')).

:- pragma foreign_proc("C#",
    dir.alt_directory_separator = (Sep::out),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Sep = System.IO.Path.AltDirectorySeparatorChar;
").

is_directory_separator(Char) :-
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

is_directory_separator_semidet(Char) :-
    dir.is_directory_separator(Char).

:- pred dir.ends_with_directory_separator(string::in, int::in, int::out)
    is semidet.

ends_with_directory_separator(String, End, PrevIndex) :-
    string.unsafe_prev_index(String, End, PrevIndex, Char),
    dir.is_directory_separator(Char).

use_windows_paths :- dir.directory_separator = ('\\').

:- pragma foreign_export("C", (dir.this_directory = out),
    "ML_dir_this_directory").
:- pragma foreign_export("C#", (dir.this_directory = out),
    "ML_dir_this_directory").

this_directory = ".".

this_directory(dir.this_directory).

parent_directory = "..".

parent_directory(dir.parent_directory).

%---------------------------------------------------------------------------%

det_basename(FileName) =
    ( if BaseName = dir.basename(FileName) then
        BaseName
    else
        unexpected($pred, "given directory is root directory")
    ).

basename(FileName) = BaseName :-
    FileNameChars = canonicalize_path_chars(string.to_char_list(FileName)),
    not dir.is_root_directory(FileNameChars),
    not (
        % Current directory on the given drive.
        use_windows_paths,
        FileNameChars = [Drive, (':')],
        char.is_alpha(Drive)
    ),

    FileNameWithoutSlash = remove_trailing_dir_separator(FileNameChars),
    FileNameWithoutSlash \= string.to_char_list(dir.this_directory),
    FileNameWithoutSlash \= string.to_char_list(dir.parent_directory),
    ( if dir.split_name_2(FileNameChars, _, BaseName0) then
        BaseName = BaseName0
    else
        BaseName = FileName
    ).

basename(S, dir.basename(S)).

%---------------------------------------------------------------------------%

dirname(FileName) = DirName :-
    FileNameChars = canonicalize_path_chars(string.to_char_list(FileName)),
    ( if
        dir.is_root_directory(FileNameChars)
    then
        DirName = string.from_char_list(FileNameChars)
    else if
        % Current directory on the given drive.
        use_windows_paths,
        FileNameChars = [Drive, (':')],
        char.is_alpha(Drive)
    then
        DirName = string.from_char_list(FileNameChars)
    else if
        dir.split_name_2(FileNameChars, DirName0, _)
    then
        DirName = DirName0
    else if
        remove_trailing_dir_separator(FileNameChars) =
            string.to_char_list(dir.parent_directory)
    then
        DirName = dir.parent_directory
    else
        DirName = dir.this_directory
    ).

dirname(S, dir.dirname(S)).

split_name(FileName, DirName, BaseName) :-
    FileNameChars = canonicalize_path_chars(string.to_char_list(FileName)),
    not is_root_directory(FileNameChars),
    dir.split_name_2(FileNameChars, DirName, BaseName).

    % Check that the filename is not empty or dir.this_directory,
    % pass the directory off to any backend-specific implementations,
    % or if none exist, invoke split_name_3 to split the filename using
    % Mercury code.
    % This assumes that the caller has already checked whether the
    %
    % directory is a root directory.
:- pred dir.split_name_2(list(char)::in, string::out, string::out) is semidet.

split_name_2(FileNameChars0, DirName, BaseName) :-
    FileNameChars0 = [_ | _],
    FileNameWithoutSlash = remove_trailing_dir_separator(FileNameChars0),
    FileNameWithoutSlash \= string.to_char_list(dir.this_directory),
    FileNameWithoutSlash \= string.to_char_list(dir.parent_directory),
    ( if io.have_dotnet then
        % System.IO.Path.GetFileName() returns the empty string
        % if the path ends in a separator).
        dir.split_name_dotnet(string.from_char_list(FileNameWithoutSlash),
            DirName, BaseName)
    else
        dir.split_name_3(FileNameChars0, DirName, BaseName)
    ).

:- pred dir.split_name_3(list(char)::in, string::out, string::out) is semidet.

split_name_3(FileNameChars, DirName, BaseName) :-
    % Remove any trailing separator.
    RevFileNameChars0 = reverse(FileNameChars),
    ( if
        RevFileNameChars0 = [LastChar | RevFileNameChars1],
        dir.is_directory_separator(LastChar)
    then
        RevFileNameChars = RevFileNameChars1
    else
        RevFileNameChars = RevFileNameChars0
    ),
    ( if
        list.take_while(isnt(dir.is_directory_separator_semidet),
            RevFileNameChars, RevBaseName, RevDirName0),
        RevBaseName = [_ | _],
        RevDirName0 = [_ | _]
    then
        % Strip the trailing separator off the directory name
        % if doing so doesn't change the meaning.
        ( if
            RevDirName0 = [Sep | RevDirName1],
            not (
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
        then
            RevDirName = RevDirName1
        else
            RevDirName = RevDirName0
        ),

        BaseName = string.from_rev_char_list(RevBaseName),
        DirName = string.from_rev_char_list(RevDirName)
    else if
        % Check for relative paths of the form `C:foo'.
        use_windows_paths,
        FileNameChars = [Drive, (':') | BaseNameChars],
        char.is_alpha(Drive),
        BaseNameChars = [BaseNameFirst | _],
        not dir.is_directory_separator(BaseNameFirst)
    then
        BaseName = string.from_char_list(BaseNameChars),
        DirName = string.from_char_list([Drive, (':')])
    else
        fail
    ).

:- pred dir.split_name_dotnet(string::in, string::out, string::out)
    is semidet.

split_name_dotnet(_, "", "") :-
    semidet_fail.

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
    } catch (System.Exception) {
        BaseName = null;
        DirName = null;
        SUCCESS_INDICATOR = false;
    }
").

    % Remove repeated path separators.
    %
:- func canonicalize_path_chars(list(char)) = list(char).

canonicalize_path_chars(FileName0) = FileName :-
    ( if
        % Windows allows path names of the form "\\server\share".
        % These path names are referred to as UNC path names.
        ( use_windows_paths ; io.have_cygwin ),
        FileName0 = [Char1 | FileName1],
        is_directory_separator(Char1)
    then
        % On Cygwin "//" is different to "\\"
        % ("//" is the Cygwin root directory, "\\" is
        % the root directory of the current drive).
        ( if io.have_cygwin then
            CanonicalChar1 = Char1
        else
            CanonicalChar1 = directory_separator
        ),
        FileName2 = canonicalize_path_chars_2(FileName1, []),

        % "\\" isn't a UNC path name, so it is equivalent to "\".
        ( if
            FileName2 = [Char2],
            is_directory_separator(Char2)
        then
            FileName = [CanonicalChar1]
        else
            FileName = [CanonicalChar1 | FileName2]
        )
    else
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
    ( if
        not io.have_cygwin,
        is_directory_separator(C0)
    then
        C = directory_separator
    else
        C = C0
    ),

    % Remove repeated directory separators.
    ( if
        dir.is_directory_separator(C),
        FileName0 = [C2 | _],
        dir.is_directory_separator(C2)
    then
        RevFileName = RevFileName0
    else
        RevFileName = [C | RevFileName0]
    ).

:- func remove_trailing_dir_separator(list(char)) = list(char).

remove_trailing_dir_separator(Chars) =
    ( if
        list.split_last(Chars, Chars1, Sep),
        dir.is_directory_separator(Sep)
    then
        Chars1
    else
        Chars
    ).

path_name_is_root_directory(PathName) :-
    is_root_directory(canonicalize_path_chars(string.to_char_list(PathName))).

    % Assumes repeated directory separators have been removed.
    %
:- pred is_root_directory(list(char)::in) is semidet.

is_root_directory(FileName) :-
    ( if
        have_dotnet
    then
        is_dotnet_root_directory(string.from_char_list(FileName))
    else if
        ( use_windows_paths
        ; io.have_cygwin
        )
    then
        strip_leading_win32_root_directory(FileName, [])
    else
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
    ( if strip_leading_win32_drive_root_directory(!FileName) then
        true
    else if strip_leading_win32_unc_root_directory(!FileName) then
        true
    else
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
        not dir.is_directory_separator(Char2)
    ).

    % Check for `\\server\' or `\\server\share\'.
    %
:- pred strip_leading_win32_unc_root_directory(list(char)::in, list(char)::out)
    is semidet.

strip_leading_win32_unc_root_directory([Sep, Sep | !.FileName], !:FileName) :-
    dir.is_directory_separator(Sep),
    list.take_while(isnt(dir.is_directory_separator_semidet), !.FileName,
        Server, !:FileName),
    Server = [_ | _],
    (
        !.FileName = []
    ;
        !.FileName = [Sep | !:FileName],
        (
            !.FileName = []
        ;
            !.FileName = [_ | _],
            list.take_while(isnt(dir.is_directory_separator_semidet),
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
        ( if FileNameLen > 0 then
            ends_with_directory_separator(FileName, FileNameLen, PrevIndex),
            string.unsafe_between(FileName, 0, PrevIndex, Prefix),
            is_dotnet_root_directory_2(Prefix)
        else
            fail
        )
    ).

:- pred is_dotnet_root_directory_2(string::in) is semidet.

is_dotnet_root_directory_2(_) :-
    unexpected($pred, "called for non-.NET CLI backend").

:- pragma foreign_proc("C#",
    is_dotnet_root_directory_2(FileName::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    try {
        SUCCESS_INDICATOR =
            (System.IO.Path.GetDirectoryName(FileName) == null);
    } catch (System.Exception) {
        SUCCESS_INDICATOR = false;
    }
}").

%---------------------------------------------------------------------------%

path_name_is_absolute(FileName) :-
    ( if
        have_dotnet
    then
        dotnet_path_name_is_absolute(FileName)
    else if
        ( use_windows_paths
        ; io.have_cygwin
        )
    then
        strip_leading_win32_root_directory(
            canonicalize_path_chars(string.to_char_list(FileName)), _)
    else
        string.index(FileName, 0, FirstChar),
        dir.is_directory_separator(FirstChar)
    ).

:- pred dir.dotnet_path_name_is_absolute(string::in) is semidet.

dotnet_path_name_is_absolute(FileName) :-
    dir.dotnet_path_name_is_absolute_2(FileName),

    % The .NET CLI function System.IO.Path.IsPathRooted succeeds for
    % paths such as `C:', which specifies a directory relative to the
    % current directory on drive C.
    not (
        use_windows_paths,
        FileNameLen = length(FileName),
        ( if FileNameLen >= 2 then
            char.is_alpha(string.unsafe_index(FileName, 0)),
            string.unsafe_index(FileName, 1) = (':'),
            ( if FileNameLen > 2 then
                not dir.is_directory_separator(
                    string.unsafe_index(FileName, 2))
            else
                true
            )
        else
            fail
        )
    ).

:- pred dir.dotnet_path_name_is_absolute_2(string::in) is semidet.

dotnet_path_name_is_absolute_2(_) :-
    unexpected($pred, "called on non-.NET CLI backend").

:- pragma foreign_proc("C#",
    dir.dotnet_path_name_is_absolute_2(FileName::in),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        SUCCESS_INDICATOR = System.IO.Path.IsPathRooted(FileName);
    } catch (System.Exception) {
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

make_path_name(DirName, FileName) = DirName/FileName.

:- pragma foreign_export("C", dir.make_path_name(in, in) = out,
    "ML_make_path_name").
:- pragma foreign_export("C#", dir.make_path_name(in, in) = out,
    "ML_make_path_name").

DirName0/FileName0 = PathName :-
    DirName = string.from_char_list(canonicalize_path_chars(
        string.to_char_list(DirName0))),
    FileName = string.from_char_list(canonicalize_path_chars(
        string.to_char_list(FileName0))),
    ( if
        dir.path_name_is_absolute(FileName)
    then
        unexpected($pred, "second argument is absolute")
    else if
        % Check that FileName is not a relative path
        % of the form "C:foo".
        use_windows_paths,
        Length = length(FileName),
        ( if Length >= 2 then
            char.is_alpha(string.unsafe_index(FileName, 0)),
            string.unsafe_index(FileName, 1) = (':'),
            ( if Length > 2 then
                not is_directory_separator(string.unsafe_index(FileName, 2))
            else
                true
            )
        else
            fail
        )
    then
        unexpected($pred, "second argument is a current drive relative path")
    else if
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
            ( if DirNameLength > 0 then
                ends_with_directory_separator(DirName, DirNameLength, _)
            else

                fail
            )
        )
    then
        PathName = DirName ++ FileName
    else
        % Using string.append_list has a fixed overhead of six words, whereas
        % using two string.appends back to back would have a memory overhead
        % proportional to the size of the string copied twice. We prefer the
        % former because it is bounded.
        PathName = string.append_list([DirName,
            string.char_to_string(dir.directory_separator),
            FileName])
    ).

relative_path_name_from_components(Components) = PathName :-
    Sep = string.from_char(dir.directory_separator),
    PathName = string.join_list(Sep, Components).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    dir.current_directory(Res::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        may_not_duplicate],
"
    /*
    ** Marked thread_safe because ML_make_io_res_1_error_string will acquire
    ** the global lock.
    */

#ifdef MR_WIN32
    wchar_t     *wbuf;
    MR_String   str;

    wbuf = _wgetcwd(NULL, 1);
    if (wbuf != NULL) {
        str = ML_wide_to_utf8(wbuf, MR_ALLOC_ID);
        Res = ML_make_io_res_1_ok_string(str);
        free(wbuf);
    } else {
        ML_make_io_res_1_error_string(errno,
            MR_make_string_const(""dir.current_directory failed: ""),
            &Res);
    }
#else
    size_t      size = 256;
    MR_Word     ptr;
    char        *buf;
    MR_String   str;

    while (1) {
        MR_offset_incr_hp_atomic_msg(ptr, 0,
            (size + sizeof(MR_Word) - 1) / sizeof(MR_Word),
            MR_ALLOC_ID, ""string.string/0"");
        buf = (char *) ptr;
        if (getcwd(buf, size)) {
            MR_make_aligned_string(str, buf);
            Res = ML_make_io_res_1_ok_string(str);
            break;
        }
        if (errno != ERANGE) {
            ML_make_io_res_1_error_string(errno,
                MR_make_string_const(""dir.current_directory failed: ""),
                &Res);
            break;
        }
        /* Buffer too small. Resize and try again. */
        size *= 1.5;
    }
#endif
").

:- pragma foreign_proc("C#",
    dir.current_directory(Res::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        may_not_duplicate],
"
    try {
        string dir = System.IO.Directory.GetCurrentDirectory();
        Res = io.ML_make_io_res_1_ok_string(dir);
    } catch (System.Exception e) {
        Res = io.ML_make_io_res_1_error_string(e,
            ""dir.current_directory failed: "");
    }
").

:- pragma foreign_proc("Java",
    dir.current_directory(Res::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        may_not_duplicate],
"
    java.io.File dir = new java.io.File(""."");
    try {
        Res = io.ML_make_io_res_1_ok_string(dir.getCanonicalPath());
    } catch (Exception e) {
        Res = io.ML_make_io_res_1_error_string(e,
            ""dir.current_directory failed: "");
    }
").

:- pragma foreign_proc("Erlang",
    dir.current_directory(Res::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    case file:get_cwd() of
        {ok, Cwd} ->
            Res = mercury__io:'ML_make_io_res_1_ok_string'(
                list_to_binary(Cwd));
        {error, Reason} ->
            Res = mercury__io:'ML_make_io_res_1_error_string'(Reason,
                ""dir.current_directory failed: "")
    end
").

%---------------------------------------------------------------------------%

make_directory(PathName, Result, !IO) :-
    ( if can_implement_make_directory then
        DirName = dir.dirname(PathName),
        ( if PathName = DirName then
            % We've been asked to make a root directory -- the mkdir will fail.
            dir.make_single_directory_2(0, PathName, Result, !IO)
        else if DirName = dir.this_directory then
            % Just go ahead and attempt to make the directory -- if the
            % current directory is not accessible, the mkdir will fail.
            dir.make_single_directory_2(0, PathName, Result, !IO)
        else
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
    else
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
            Res = dir.ML_make_mkdir_res_error(
                new System.Exception(""a file with that name already exists""));
        } else if (System.IO.Directory.Exists(DirName)) {
            Res = dir.ML_check_dir_accessibility(DirName);
        } else {
            System.IO.Directory.CreateDirectory(DirName);
            Res = dir.ML_make_mkdir_res_ok();
        }
    } catch (System.Exception e) {
        Res = dir.ML_make_mkdir_res_error(e);
    }
}").

% Java has a similar library function java.io.File.mkdirs()
:- pragma foreign_proc("Java",
    dir.make_directory(DirName::in, Res::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        may_not_duplicate],
"
    try {
        java.io.File dir = new java.io.File(DirName);
        if (dir.isFile()) {
            throw new java.lang.RuntimeException(
                ""a file with that name already exists"");
        }
        if (dir.isDirectory()) {
            Res = ML_check_dir_accessibility(DirName);
        } else {
            if (!dir.mkdirs()) {
                throw new java.lang.RuntimeException(
                    ""make_directory failed"");
            }
            Res = make_mkdir_res_ok_0_f_0();
        }
    } catch (java.lang.Exception e) {
        Res = ML_make_mkdir_res_error(e);
    }
").

:- pragma foreign_proc("Erlang",
    dir.make_directory(DirName::in, Res::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    DirNameStr = binary_to_list(DirName),
    % filelib:ensure_dir makes all the parent directories.
    case filelib:ensure_dir(DirNameStr) of
        ok ->
            ErrorIfExists = 0,
            Res = mercury__dir:'ML_make_single_directory_2'(ErrorIfExists,
                DirName);   % not DirNameStr
        {error, Reason} ->
            Res = mercury__dir:'ML_make_mkdir_res_error'(Reason)
    end
").

:- pred can_implement_make_directory is semidet.

can_implement_make_directory :-
    semidet_fail.

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
    SUCCESS_INDICATOR = true;
"
).
:- pragma foreign_proc("Erlang",
    can_implement_make_directory,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true
").

make_single_directory(DirName, Result, !IO) :-
    dir.make_single_directory_2(1, DirName, Result, !IO).

:- pragma foreign_export("Erlang",
    dir.make_single_directory_2(in, in, out, di, uo),
    "ML_make_single_directory_2").

:- pred dir.make_single_directory_2(int::in, string::in, io.res::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.make_single_directory_2(ErrorIfExists::in, DirName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates, will_not_modify_trail, does_not_affect_liveness,
        may_not_duplicate],
"
#if defined(MR_WIN32)
    if (CreateDirectoryW(ML_utf8_to_wide(DirName), NULL)) {
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
").
:- pragma foreign_proc("C#",
    dir.make_single_directory_2(ErrorIfExists::in, DirName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    try {
    // CreateDirectory doesn't fail if a file with the same
    // name as the directory being created already exists.
    if (System.IO.File.Exists(DirName)) {
        Result = dir.ML_make_mkdir_res_error(
            new System.Exception(
                ""a file with that name already exists""));
    } else {
        System.IO.DirectoryInfo info =
            new System.IO.DirectoryInfo(DirName);
        System.IO.DirectoryInfo parent_info = info.Parent;

        if (parent_info == null) {
            Result = dir.ML_make_mkdir_res_error(
                new System.Exception(""can't create root directory""));
        } else if (!info.Parent.Exists) {
            Result = dir.ML_make_mkdir_res_error(
                new System.Exception(""parent directory does not exist""));
        } else if (ErrorIfExists == 1 && info.Exists) {
            Result = dir.ML_make_mkdir_res_error(
                new System.Exception(""directory already exists""));
        } else {
            info.Create();
            Result = dir.ML_make_mkdir_res_ok();
        }
    }
    } catch (System.Exception e) {
        Result = dir.ML_make_mkdir_res_error(e);
    }
}").

:- pragma foreign_proc("Java",
    dir.make_single_directory_2(ErrorIfExists::in, DirName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates,
        may_not_duplicate],
"
    try {
        java.io.File newDir = new java.io.File(DirName);
        java.io.File parent = newDir.getParentFile();

        if (parent == null) {
            Result = ML_make_mkdir_res_error(
                new java.io.IOException(""can't create root directory""));
        } else if (!parent.exists()) {
            Result = ML_make_mkdir_res_error(
                new java.io.IOException(""parent directory does not exist""));
        } else if (ErrorIfExists == 1 && newDir.exists()) {
            Result = ML_make_mkdir_res_error(
                new java.io.IOException(""directory already exists""));
        } else {
            if (!newDir.mkdir()) {
                throw new java.lang.RuntimeException(
                    ""make_single_directory failed"");
            }
            Result = ML_make_mkdir_res_ok();
        }
    } catch (java.lang.Exception e) {
        Result = ML_make_mkdir_res_error(e);
    }
").

:- pragma foreign_proc("Erlang",
    dir.make_single_directory_2(ErrorIfExists::in, DirName::in,
        Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    DirNameStr = binary_to_list(DirName),
    case file:make_dir(DirNameStr) of
        ok ->
            Result = mercury__dir:'ML_make_mkdir_res_ok'();
        {error, eexist} when ErrorIfExists =:= 0 ->
            Result = mercury__dir:'ML_make_mkdir_res_exists'(eexist,
                DirName);   % not DirNameStr
        {error, Reason} ->
            Result = mercury__dir:'ML_make_mkdir_res_error'(Reason)
    end
").

:- func dir.make_mkdir_res_ok = io.res.
:- pragma foreign_export("C", (dir.make_mkdir_res_ok = out),
    "ML_make_mkdir_res_ok").
:- pragma foreign_export("C#", (dir.make_mkdir_res_ok = out),
    "ML_make_mkdir_res_ok").
:- pragma foreign_export("Java", (dir.make_mkdir_res_ok = out),
    "ML_make_mkdir_res_ok").
:- pragma foreign_export("Erlang", (dir.make_mkdir_res_ok = out),
    "ML_make_mkdir_res_ok").

make_mkdir_res_ok = ok.

:- pred dir.make_mkdir_res_error(io.system_error::in, io.res::out,
    io::di, io::uo) is det.
:- pragma foreign_export("C", dir.make_mkdir_res_error(in, out, di, uo),
    "ML_make_mkdir_res_error").
:- pragma foreign_export("C#", dir.make_mkdir_res_error(in, out, di, uo),
    "ML_make_mkdir_res_error").
:- pragma foreign_export("Java", dir.make_mkdir_res_error(in, out, di, uo),
    "ML_make_mkdir_res_error").
:- pragma foreign_export("Erlang", dir.make_mkdir_res_error(in, out, di, uo),
    "ML_make_mkdir_res_error").

make_mkdir_res_error(Error, error(make_io_error(Msg)), !IO) :-
    io.make_maybe_win32_err_msg(Error, "dir.make_directory failed: ",
        Msg, !IO).

:- pred dir.make_mkdir_res_exists(io.system_error::in,
    string::in, io.res::out, io::di, io::uo) is det.
:- pragma foreign_export("C",
    dir.make_mkdir_res_exists(in, in, out, di, uo),
    "ML_make_mkdir_res_exists").
:- pragma foreign_export("C#",
    dir.make_mkdir_res_exists(in, in, out, di, uo),
    "ML_make_mkdir_res_exists").
:- pragma foreign_export("Java",
    dir.make_mkdir_res_exists(in, in, out, di, uo),
    "ML_make_mkdir_res_exists").
:- pragma foreign_export("Erlang",
    dir.make_mkdir_res_exists(in, in, out, di, uo),
    "ML_make_mkdir_res_exists").

make_mkdir_res_exists(Error, DirName, Res, !IO) :-
    io.file_type(yes, DirName, TypeResult, !IO),
    ( if TypeResult = ok(directory) then
        dir.check_dir_accessibility(DirName, Res, !IO)
    else
        dir.make_mkdir_res_error(Error, Res, !IO)
    ).

:- pred dir.check_dir_accessibility(string::in, io.res::out, io::di, io::uo)
    is det.
:- pragma foreign_export("C", dir.check_dir_accessibility(in, out, di, uo),
    "ML_check_dir_accessibility").
:- pragma foreign_export("C#", dir.check_dir_accessibility(in, out, di, uo),
    "ML_check_dir_accessibility").
:- pragma foreign_export("Java", dir.check_dir_accessibility(in, out, di, uo),
    "ML_check_dir_accessibility").
:- pragma foreign_export("Erlang", dir.check_dir_accessibility(in, out, di, uo),
    "ML_check_dir_accessibility").

check_dir_accessibility(DirName, Res, !IO) :-
    % Check whether we can read and write the directory.
    io.check_file_accessibility(DirName, [read, write, execute], Res, !IO).

%---------------------------------------------------------------------------%

foldl2(P, DirName, T, Res, !IO) :-
    dir.foldl2_process_dir(no, P, fixup_dirname(DirName), [], no,
        no, _, T, Res, !IO).

recursive_foldl2(P, DirName, FollowLinks, T, Res, !IO) :-
    dir.foldl2_process_dir(no, P, fixup_dirname(DirName), [], yes,
        FollowLinks, _, T, Res, !IO).

    %
    % Under windows you cannot list the files of a directory if the directory
    % name contains a trailing slash, except when the trailing slash indicates
    % the root directory.
    %
    % This function removes the trailing slash, except when we are in the
    % root directory.
    %
:- func fixup_dirname(string) = string.

fixup_dirname(Dir0) = Dir :-
    DirChars = canonicalize_path_chars(string.to_char_list(Dir0)),
    ( if is_root_directory(DirChars) then
        Dir = Dir0
    else
        Dir = string.from_char_list(remove_trailing_dir_separator(DirChars))
    ).

:- pred dir.foldl2_process_dir(bool::in,
    dir.foldl_pred(T)::in(dir.foldl_pred), string::in,
    list(file_id)::in, bool::in, bool::in, bool::out, T::in,
    io.maybe_partial_res(T)::out, io::di, io::uo) is det.

:- pred dir.foldl2_process_dir2(dir.stream::in, dir.stream::out, bool::in,
    dir.foldl_pred(T)::in(dir.foldl_pred), string::in,
    list(file_id)::in, string::in, bool::in, bool::in, T::in,
    {io.maybe_partial_res(T), bool}::out, io::di, io::uo) is det.

foldl2_process_dir2(!Dir, SymLinkParent, P, DirName, ParentIds, FirstEntry,
        Recursive, FollowLinks, T0, {Res, Cont}, !IO) :-
    dir.foldl2_process_entries(!Dir, SymLinkParent, P, DirName,
        ok(FirstEntry), ParentIds, Recursive, FollowLinks, Cont,
        T0, Res, !IO).

foldl2_process_dir(SymLinkParent, P, DirName, ParentIds0, Recursive,
        FollowLinks, Continue, T0, Result, !IO) :-
    ( if can_implement_dir_foldl then
        ( if
            Recursive = yes,
            FollowLinks = yes
        then
            check_for_symlink_loop(SymLinkParent, DirName,
                LoopRes, ParentIds0, ParentIds, !IO)
        else
            ParentIds = ParentIds0,
            LoopRes = ok(no)
        ),
        (
            LoopRes = ok(no),
            dir.open(DirName, OpenResult, !IO),
            (
                OpenResult = ok({Dir0, FirstEntry}),

                % We need to close the directory if an
                % exception is thrown to avoid resource leaks.
                ProcessDir =
                    (pred({DirRes1, Continue1, Dir1}::out,
                            IO0::di, IO::uo) is det :-
                        dir.foldl2_process_dir2(Dir0, Dir1, SymLinkParent,
                            P, DirName, ParentIds, FirstEntry, Recursive,
                            FollowLinks, T0, {DirRes1, Continue1}, IO0, IO)
                    ),
                promise_equivalent_solutions [!:IO, ExcpResult] (
                    exception.try_io(ProcessDir, ExcpResult, !IO)
                ),
                (
                    ExcpResult = succeeded({DirRes, Continue, Dir}),
                    dir.close(Dir, CleanupRes, !IO),
                    (
                        DirRes = ok(T),
                        (
                            CleanupRes = ok,
                            Result = ok(T)
                        ;
                            CleanupRes = error(Error),
                            Result = error(T, Error)
                        )
                    ;
                        DirRes = error(_, _),
                        Result = DirRes
                    )
                ;
                    ExcpResult = exception(_),
                    % We are relying on the fact that in the C, C# and Java
                    % backends Dir0 = Dir, and in the Erlang backend dir.close
                    % does nothing.
                    dir.close(Dir0, _, !IO),
                    rethrow(ExcpResult)
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
    else
        Continue = no,
        Result = error(T0, make_io_error("dir.foldl2 " ++
            "not implemented on this platform"))
    ).

:- pred dir.foldl2_process_entries(dir.stream::in, dir.stream::out, bool::in,
    dir.foldl_pred(T)::in(dir.foldl_pred), string::in,
    io.result(string)::in, list(file_id)::in, bool::in,
    bool::in, bool::out, T::in, io.maybe_partial_res(T)::out,
    io::di, io::uo) is det.

foldl2_process_entries(!Dir, _, _, _, error(Error), _, _, _, no,
        T0, error(T0, Error), !IO).
foldl2_process_entries(!Dir, _, _, _, eof, _, _, _, yes, T0, ok(T0), !IO).
foldl2_process_entries(!Dir, SymLinkParent, P, DirName, ok(FileName),
        ParentIds, Recursive, FollowLinks, Continue, T0, Res, !IO) :-
    PathName = DirName/FileName,
    io.file_type(no, PathName, FileTypeRes, !IO),
    (
        FileTypeRes = ok(Type),
        P(DirName, FileName, Type, Continue1, T0, T1, !IO),
        (
            Continue1 = yes,
            ( if
                Recursive = yes,
                Type = directory
            then
                dir.foldl2_process_dir(SymLinkParent, P, PathName, ParentIds,
                    Recursive, FollowLinks, Continue2, T1, Res1, !IO)
            else if
                Recursive = yes,
                Type = symbolic_link,
                FollowLinks = yes
            then
                io.file_type(yes, PathName, TargetTypeRes, !IO),
                (
                    TargetTypeRes = ok(TargetType),
                    (
                        TargetType = directory,
                        dir.foldl2_process_dir(yes, P, PathName, ParentIds,
                            Recursive, FollowLinks, Continue2, T1, Res1, !IO)
                    ;

                        ( TargetType = regular_file
                        ; TargetType = symbolic_link
                        ; TargetType = named_pipe
                        ; TargetType = socket
                        ; TargetType = character_device
                        ; TargetType = block_device
                        ; TargetType = message_queue
                        ; TargetType = semaphore
                        ; TargetType = shared_memory
                        ; TargetType = unknown
                        ),
                        Continue2 = yes,
                        Res1 = ok(T1)
                    )
                ;
                    TargetTypeRes = error(TargetTypeError),
                    Continue2 = no,
                    Res1 = error(T1, TargetTypeError)
                )
            else
                Continue2 = yes,
                Res1 = ok(T1)
            ),
            ( if
                Continue2 = yes,
                Res1 = ok(T)
            then
                dir.read_entry(!.Dir, EntryResult0, !IO),
                (
                    EntryResult0 = ok({!:Dir, FileName1}),
                    EntryResult = ok(FileName1)
                ;
                    EntryResult0 = eof,
                    EntryResult = eof
                ;
                    EntryResult0 = error(Error),
                    EntryResult = error(Error)
                ),
                dir.foldl2_process_entries(!Dir, SymLinkParent, P, DirName,
                    EntryResult, ParentIds, Recursive, FollowLinks, Continue,
                    T, Res, !IO)
            else
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
    ( if io.have_symlinks then
        io.file_id(DirName, IdRes, !IO),
        (
            IdRes = ok(Id),
            ( if
                SymLinkParent = yes,
                list.member(Id, !.ParentIds)
            then
                Loop = yes
            else
                !:ParentIds = [Id | !.ParentIds],
                Loop = no
            ),
            LoopRes = ok(Loop)
        ;
            IdRes = error(Msg),
            LoopRes = error(Msg)
        )
    else
        LoopRes = ok(no)
    ).

% MS-Windows doesn't provide the POSIX directory functions.
:- pragma foreign_decl("C", "

#include ""mercury_string.h""
#include ""mercury_types.h""

#if defined(MR_WIN32) && defined(MR_HAVE_WINDOWS_H)
  #include ""mercury_windows.h""
  #include <direct.h>   /* for _wgetcwd */
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
:- type dir.stream
    --->    dir.stream.
:- pragma foreign_type("C", dir.stream, "ML_DIR_STREAM").
:- pragma foreign_type("C#", dir.stream, "System.Collections.IEnumerator").
:- pragma foreign_type("Java", dir.stream, "java.util.Iterator").
:- pragma foreign_type("Erlang", dir.stream, "").

:- pred can_implement_dir_foldl is semidet.

can_implement_dir_foldl :-
    semidet_fail.

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
"
    SUCCESS_INDICATOR = true;
").
:- pragma foreign_proc("Java",
    can_implement_dir_foldl,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").
:- pragma foreign_proc("Erlang",
    can_implement_dir_foldl,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true
").

    % Win32 doesn't allow us to open a directory without
    % returning the first item.
    %
:- pred dir.open(string::in, io.result({dir.stream, string})::out,
    io::di, io::uo) is det.

open(DirName, Res, !IO) :-
    ( if can_implement_dir_foldl then
        dir.open_2(DirName, Res, !IO)
    else
        Res = error(io.make_io_error("dir.foldl2 not implemented " ++
            "on this platform"))
    ).

:- pred dir.open_2(string::in, io.result({dir.stream, string})::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.open_2(DirName::in, Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe,
        terminates, will_not_modify_trail, does_not_affect_liveness],
"
#if defined(MR_WIN32)
    WIN32_FIND_DATAW    file_data;
    ML_DIR_STREAM       Dir;
    LPTSTR              FirstFileName;
    char                *dir_pattern;
    MR_Integer          is_readable;
    char                *filename;

    ML_check_dir_readable(DirName, &is_readable, &Result);
    if (is_readable) {
        dir_pattern = ML_make_path_name(DirName, MR_make_string_const(""*""));
        Dir = FindFirstFileW(ML_utf8_to_wide(dir_pattern), &file_data);
        if (Dir == INVALID_HANDLE_VALUE) {
            int error = GetLastError();
            if (error == ERROR_NO_MORE_FILES) {
                Result = ML_make_dir_open_result_eof();
            } else {
                ML_make_dir_open_result_error(error, &Result);
            }
        } else {
            filename = ML_wide_to_utf8(file_data.cFileName, MR_ALLOC_ID);
            ML_make_win32_dir_open_result_ok(Dir, filename, &Result);
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
").

:- pragma foreign_proc("C#",
    dir.open_2(DirName::in, Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"{
    try {
        System.Collections.IEnumerator Dir =
            System.IO.Directory.GetFileSystemEntries(DirName).GetEnumerator();
        Result = dir.ML_dir_read_first_entry(Dir);
    } catch (System.Exception e) {
        Result = dir.ML_make_dir_open_result_error(e);
    }
}").

:- pragma foreign_proc("Java",
    dir.open_2(DirName::in, Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    try {
        java.lang.String[] fileList = (new java.io.File(DirName)).list();
        java.util.List list = java.util.Arrays.asList(fileList);

        Result = ML_dir_read_first_entry(list.iterator());
    } catch (java.lang.Exception e) {
        Result = ML_make_dir_open_result_error(e);
    }
").

:- pragma foreign_proc("Erlang",
    dir.open_2(DirName::in, Result::out, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, tabled_for_io, thread_safe, terminates],
"
    DirNameStr = binary_to_list(DirName),
    case file:list_dir(DirNameStr) of
        {ok, FileNames0} ->
            FileNames = lists:sort(FileNames0),
            Result = mercury__dir:'ML_dir_read_first_entry'(FileNames);
        {error, Reason} ->
            Result = mercury__dir:'ML_make_dir_open_result_error'(Reason)
    end
").

:- pred dir.check_dir_readable(string::in, int::out,
    io.result({dir.stream, string})::out, io::di, io::uo) is det.
:- pragma foreign_export("C", dir.check_dir_readable(in, out, out, di, uo),
    "ML_check_dir_readable").

check_dir_readable(DirName, IsReadable, Result, !IO) :-
    io.file_type(yes, DirName, FileTypeRes, !IO),
    (
        FileTypeRes = ok(FileType),
        (
            FileType = directory,
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
:- pragma foreign_export("C#", dir.read_first_entry(in, out, di, uo),
    "ML_dir_read_first_entry").
:- pragma foreign_export("Java", dir.read_first_entry(in, out, di, uo),
    "ML_dir_read_first_entry").
:- pragma foreign_export("Erlang", dir.read_first_entry(in, out, di, uo),
    "ML_dir_read_first_entry").

read_first_entry(Dir, Result, !IO) :-
    dir.read_entry(Dir, Result, !IO),
    (
        Result = ok(_)
    ;
        ( Result = eof
        ; Result = error(_)
        ),
        % Close the directory stream immediately to avoid resource leaks.
        dir.close(Dir, _, !IO)
    ).

:- pred make_win32_dir_open_result_ok(dir.stream::in, string::in,
    io.result({dir.stream, string})::out, io::di, io::uo) is det.
:- pragma foreign_export("C",
    make_win32_dir_open_result_ok(in, in, out, di, uo),
    "ML_make_win32_dir_open_result_ok").

make_win32_dir_open_result_ok(Dir, FirstFile0, Result, !IO) :-
    ( if
        ( FirstFile0 = dir.this_directory
        ; FirstFile0 = dir.parent_directory
        )
    then
        dir.read_entry(Dir, ReadResult, !IO),
        (
            ReadResult = ok(_),
            Result = ReadResult
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
    else
        Result = ok({Dir, FirstFile0})
    ).

:- func make_dir_open_result_eof = io.result({dir.stream, string}).
:- pragma foreign_export("C", (make_dir_open_result_eof = out),
    "ML_make_dir_open_result_eof").
:- pragma foreign_export("C#", (make_dir_open_result_eof = out),
    "ML_make_dir_open_result_eof").
:- pragma foreign_export("Java", (make_dir_open_result_eof = out),
    "ML_make_dir_open_result_eof").

make_dir_open_result_eof = eof.

:- pred make_dir_open_result_error(io.system_error::in,
    io.result({dir.stream, string})::out, io::di, io::uo) is det.
:- pragma foreign_export("C", make_dir_open_result_error(in, out, di, uo),
    "ML_make_dir_open_result_error").
:- pragma foreign_export("C#", make_dir_open_result_error(in, out, di, uo),
    "ML_make_dir_open_result_error").
:- pragma foreign_export("Java", make_dir_open_result_error(in, out, di, uo),
    "ML_make_dir_open_result_error").
:- pragma foreign_export("Erlang", make_dir_open_result_error(in, out, di, uo),
    "ML_make_dir_open_result_error").

make_dir_open_result_error(Error, error(io.make_io_error(Msg)), !IO) :-
    io.make_err_msg(Error, "dir.foldl2: opening directory failed: ", Msg, !IO).

:- pred dir.close(dir.stream::in, io.res::out, io::di, io::uo) is det.

close(Dir, Res, !IO) :-
    dir.close_2(Dir, Status, Error, !IO),
    ( if Status = 0 then
        io.make_maybe_win32_err_msg(Error,
            "dir.foldl2: closing directory failed: ", Msg, !IO),
        Res = error(io.make_io_error(Msg))
    else
        Res = ok
    ).

:- pred dir.close_2(dir.stream::in, int::out, io.system_error::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.close_2(Dir::in, Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness],
"
#if defined(MR_WIN32)
    Status = FindClose(Dir);
    Error = GetLastError();
#elif defined(MR_HAVE_CLOSEDIR)
    Status = (closedir(Dir) == 0);
    Error = errno;
#else
    MR_fatal_error(""dir.open called but not supported"");
#endif
").

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

:- pragma foreign_proc("Erlang",
    dir.close_2(_Dir::in, Status::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    % Nothing to do.
    Error = null,
    Status = 1
").

:- pred dir.read_entry(dir.stream::in, io.result({dir.stream, string})::out,
    io::di, io::uo) is det.

read_entry(Dir0, Res, !IO) :-
    dir.read_entry_2(Dir0, Dir, Status, Error, FileName, !IO),
    ( if
        Status = 0
    then
        io.make_maybe_win32_err_msg(Error,
            "dir.foldl2: reading directory entry failed: ", Msg, !IO),
        Res = error(io.make_io_error(Msg))
    else if
        Status = -1
    then
        Res = eof
    else if
        ( FileName = dir.this_directory
        ; FileName = dir.parent_directory
        )
    then
        dir.read_entry(Dir0, Res, !IO)
    else
        Res = ok({Dir, FileName})
    ).

    % dir.read_entry_2(!Dir, Status, Error, FileName, !IO).
    % Status is -1 for EOF, 0 for error, 1 for success.
    %
:- pred dir.read_entry_2(dir.stream::in, dir.stream::out, int::out,
    io.system_error::out, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.read_entry_2(Dir0::in, Dir::out, Status::out, Error::out,
        FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness],
"
#if defined(MR_WIN32)
    WIN32_FIND_DATAW file_data;

    Dir = Dir0;
    if (FindNextFileW(Dir, &file_data)) {
        Status = 1;
        FileName = ML_wide_to_utf8(file_data.cFileName, MR_ALLOC_ID);
    } else {
        Error = GetLastError();
        Status = (Error == ERROR_NO_MORE_FILES ? -1 : 0);
        FileName = NULL;
    }

#elif defined(MR_HAVE_READDIR) && defined(MR_HAVE_CLOSEDIR)
    struct dirent *dir_entry;

    Dir = Dir0;
    errno = 0;
    dir_entry = readdir(Dir);
    if (dir_entry == NULL) {
        Error = errno;
        FileName = NULL;
        Status = (Error == 0 ? -1 : 0);
    } else {
        MR_make_aligned_string_copy_msg(FileName, dir_entry->d_name,
            MR_ALLOC_ID);
        Error = 0;
        Status = 1;
    }

#else /* !MR_WIN32 && !(MR_HAVE_READDIR etc.) */
    MR_fatal_error(""dir.read_entry_2 called but not supported"");
#endif
").

:- pragma foreign_proc("C#",
    dir.read_entry_2(Dir0::in, Dir::out, Status::out, Error::out,
        FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"{
    Dir = Dir0;
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
    dir.read_entry_2(Dir0::in, Dir::out, Status::out, Error::out,
        FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    Dir = Dir0;
    if (Dir.hasNext()) {
        FileName = (java.lang.String) Dir.next();
        Status = 1;
    } else {
        FileName = null;
        Status = -1;
    }
    Error = null;
").

:- pragma foreign_proc("Erlang",
    dir.read_entry_2(Dir0::in, Dir::out, Status::out, Error::out,
        FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    case Dir0 of
        [] ->
            FileName = null,
            Status = -1,
            Dir = [];
        [FileNameStr | Dir] ->
            FileName = list_to_binary(FileNameStr),
            Status = 1
    end,
    Error = null
").

%---------------------------------------------------------------------------%

expand_braces(ArgStr) = ExpandStrs :-
    ArgChar = string.to_char_list(ArgStr),
    ExpandChars = expand(ArgChar),
    ExpandStrs = list.map(string.from_char_list, ExpandChars).

:- func expand(list(char)) = list(list(char)).

expand(Chars) = expand_acc(Chars, [[]]).

:- func expand_acc(list(char), list(list(char))) = list(list(char)).

expand_acc([], Prefixes) = Prefixes.
expand_acc([Char | Chars], Prefixes0) = Strings :-
    ( if Char = '{' then
        find_matching_brace(Chars, Alternatives0, Left),
        AlternativeLists = list.map(expand, Alternatives0),
        Alternatives = list.condense(AlternativeLists),
        PrefixLists = list.map(add_alternatives(Alternatives), Prefixes0),
        Prefixes1 = list.condense(PrefixLists),
        expand_acc(Left, Prefixes1) = Strings
    else
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
    ( if Char = '}' then
        ( if BraceLevel = 0 then
            list.append(Alternatives0, [CurAlternative], Alternatives),
            Left = Chars
        else
            find_matching_brace_or_comma(Chars, Alternatives0,
                list.append(CurAlternative, [Char]),
                BraceLevel - 1, Alternatives, Left)
        )
    else if Char = '{' then
        find_matching_brace_or_comma(Chars, Alternatives0,
            list.append(CurAlternative, [Char]),
            BraceLevel + 1, Alternatives, Left)
    else if Char = (',') then
        ( if BraceLevel = 0 then
            list.append(Alternatives0, [CurAlternative], Alternatives1),
            find_matching_brace_or_comma(Chars, Alternatives1,
                [], BraceLevel, Alternatives, Left)
        else
            find_matching_brace_or_comma(Chars, Alternatives0,
                list.append(CurAlternative, [Char]),
                BraceLevel, Alternatives, Left)
        )
    else
        find_matching_brace_or_comma(Chars, Alternatives0,
            list.append(CurAlternative, [Char]),
            BraceLevel, Alternatives, Left)
    ).

%---------------------------------------------------------------------------%
:- end_module dir.
%---------------------------------------------------------------------------%
