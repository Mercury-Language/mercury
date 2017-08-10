%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995,1997,1999-2000,2002-2012 The University of Melbourne.
% Copyright (C) 2016 The Mercury team.
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

    % path_name_is_absolute(PathName)
    %
    % Is the path name syntactically an absolute path
    % (this doesn't check whether the path exists).
    %
    % An path is absolute iff it begins with a root directory
    % (see path_name_is_root_directory).
    %
:- pred path_name_is_absolute(string::in) is semidet.

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

    % recursive_foldl2(P, DirName, FollowSymLinks, InitialData, Result, !IO).
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
:- pred use_windows_paths is semidet.

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
:- pred is_directory_separator_semidet(char::in) is semidet.

is_directory_separator_semidet(Char) :-
    dir.is_directory_separator(Char).

:- pred ends_with_directory_separator(string::in, int::in, int::out)
    is semidet.

ends_with_directory_separator(String, End, PrevIndex) :-
    string.unsafe_prev_index(String, End, PrevIndex, Char),
    dir.is_directory_separator(Char).

this_directory = ".".

this_directory(dir.this_directory).

parent_directory = "..".

parent_directory(dir.parent_directory).

%---------------------------------------------------------------------------%

split_name(FileName, DirName, BaseName) :-
    FileNameChars = canonicalize_path_chars(string.to_char_list(FileName)),
    not is_root_directory(FileNameChars),
    dir.split_name_2(FileNameChars, DirName, BaseName).

    % Check that the filename is not empty or dir.this_directory,
    % pass the directory off to any backend-specific implementations,
    % or if none exist, invoke split_name_3 to split the filename using
    % Mercury code.
    % This assumes that the caller has already checked whether the
    % directory is a root directory.
    %
:- pred split_name_2(list(char)::in, string::out, string::out) is semidet.

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

:- pred split_name_3(list(char)::in, string::out, string::out) is semidet.

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

:- pred split_name_dotnet(string::in, string::out, string::out) is semidet.

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

%---------------------------------------------------------------------------%

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

det_basename(FileName) =
    ( if BaseName = dir.basename(FileName) then
        BaseName
    else
        unexpected($pred, "given directory is root directory")
    ).

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

:- pragma no_determinism_warning(is_dotnet_root_directory_2/1).
is_dotnet_root_directory_2(_) :-
    unexpected($pred, "called for non-.NET CLI backend").

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

:- pred dotnet_path_name_is_absolute(string::in) is semidet.

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

:- pred dotnet_path_name_is_absolute_2(string::in) is semidet.

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

:- pragma no_determinism_warning(dotnet_path_name_is_absolute_2/1).
dotnet_path_name_is_absolute_2(_) :-
    unexpected($pred, "called on non-.NET CLI backend").

%---------------------------------------------------------------------------%

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

make_path_name(DirName, FileName) = DirName/FileName.

relative_path_name_from_components(Components) = PathName :-
    Sep = string.from_char(dir.directory_separator),
    PathName = string.join_list(Sep, Components).

%---------------------------------------------------------------------------%

current_directory(Res, !IO) :-
    current_directory_2(CurDir, Error, !IO),
    ( if is_error(Error, "dir.current_directory failed: ", IOError) then
        Res = error(IOError)
    else
        Res = ok(CurDir)
    ).

:- pred current_directory_2(string::out, io.system_error::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    current_directory_2(CurDir::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
#ifdef MR_WIN32
    wchar_t     *wbuf;
    MR_String   str;

    wbuf = _wgetcwd(NULL, 1);
    if (wbuf != NULL) {
        CurDir = ML_wide_to_utf8(wbuf, MR_ALLOC_ID);
        Error = 0;
        free(wbuf);
    } else {
        CurDir = MR_make_string_const("""");
        Error = errno;
    }
#else
    size_t      size = 256;

    while (1) {
        /* `size' includes the NUL terminator. */
        MR_allocate_aligned_string_msg(CurDir, size - 1, MR_ALLOC_ID);
        if (getcwd(CurDir, size)) {
            Error = 0;
            break;
        }
        if (errno != ERANGE) {
            CurDir = MR_make_string_const("""");
            Error = errno;
            break;
        }
        /* Buffer too small. Resize and try again. */
        size *= 1.5;
    }
#endif
").

:- pragma foreign_proc("C#",
    current_directory_2(CurDir::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        CurDir = System.IO.Directory.GetCurrentDirectory();
        Error = null;
    } catch (System.Exception e) {
        CurDir = """";
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    current_directory_2(CurDir::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        java.io.File dir = new java.io.File(""."");
        CurDir = dir.getCanonicalPath();
        Error = null;
    } catch (java.lang.Exception e) {
        CurDir = """";
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    current_directory_2(CurDir::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    case file:get_cwd() of
        {ok, Cwd} ->
            CurDir = list_to_binary(Cwd),
            Error = ok;
        {error, Reason} ->
            CurDir = <<>>,
            Error = {error, Reason}
    end
").

%---------------------------------------------------------------------------%

make_directory(PathName, Result, !IO) :-
    ( if have_make_directory_including_parents then
        make_directory_including_parents(PathName, Result, !IO)
    else
        DirName = dir.dirname(PathName),
        ( if PathName = DirName then
            % We've been asked to make a root directory -- the mkdir will fail.
            make_directory_or_check_exists(PathName, Result, !IO)
        else if DirName = dir.this_directory then
            % Just go ahead and attempt to make the directory -- if the
            % current directory is not accessible, the mkdir will fail.
            make_directory_or_check_exists(PathName, Result, !IO)
        else
            io.check_file_accessibility(DirName, [], ParentAccessResult, !IO),
            (
                ParentAccessResult = ok,
                make_directory_or_check_exists(PathName, Result, !IO)
            ;
                ParentAccessResult = error(_),
                make_directory(DirName, ParentResult, !IO),
                (
                    ParentResult = ok,
                    make_directory_or_check_exists(PathName, Result, !IO)
                ;
                    ParentResult = error(_),
                    Result = ParentResult
                )
            )
        )
    ).

:- pred make_directory_or_check_exists(string::in, io.res::out, io::di, io::uo)
    is det.

make_directory_or_check_exists(DirName, Res, !IO) :-
    make_single_directory_2(DirName, Res0, MaybeWin32Error, !IO),
    (
        Res0 = ok,
        Res = ok
    ;
        Res0 = name_exists,
        io.file_type(yes, DirName, TypeRes, !IO),
        ( if TypeRes = ok(directory) then
            check_dir_accessibility(DirName, Res, !IO)
        else
            make_maybe_win32_err_msg(MaybeWin32Error,
                "cannot create directory: ", Message),
            Res = error(make_io_error(Message))
        )
    ;
        Res0 = dir_exists,
        check_dir_accessibility(DirName, Res, !IO)
    ;
        Res0 = error,
        make_maybe_win32_err_msg(MaybeWin32Error,
            "cannot create directory: ", Message),
        Res = error(make_io_error(Message))
    ).

:- pred check_dir_accessibility(string::in, io.res::out, io::di, io::uo)
    is det.

check_dir_accessibility(DirName, Res, !IO) :-
    % Check whether we can read and write the directory.
    io.check_file_accessibility(DirName, [read, write, execute], Res, !IO).

%---------------------------------------------------------------------------%

:- pred have_make_directory_including_parents is semidet.

have_make_directory_including_parents :-
    semidet_fail.

:- pragma foreign_proc("C#",
    have_make_directory_including_parents,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").
:- pragma foreign_proc("Java",
    have_make_directory_including_parents,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

:- pred make_directory_including_parents(string::in, io.res::out,
    io::di, io::uo) is det.

make_directory_including_parents(DirName, Res, !IO) :-
    make_directory_including_parents_2(DirName, Error, CheckAccess, !IO),
    ( if is_error(Error, "cannot make directory: ", IOError) then
        Res = error(IOError)
    else
        (
            CheckAccess = yes,
            check_dir_accessibility(DirName, Res, !IO)
        ;
            CheckAccess = no,
            Res = ok
        )
    ).

:- pred make_directory_including_parents_2(string::in, io.system_error::out,
    bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    make_directory_including_parents_2(_DirName::in, Error::out,
        CheckAccess::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Error = ENOSYS;
    CheckAccess = MR_NO;
").

:- pragma foreign_proc("C#",
    make_directory_including_parents_2(DirName::in, Error::out,
        CheckAccess::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        // System.IO.Directory.CreateDirectory() creates all directories and
        // subdirectories in the specified path unless they already exist.

        // CreateDirectory() doesn't fail if a file with the same name as the
        // directory being created already exists.
        if (System.IO.File.Exists(DirName)) {
            Error =
                new System.Exception(""a file with that name already exists"");
            CheckAccess = mr_bool.NO;
        } else if (System.IO.Directory.Exists(DirName)) {
            Error = null;
            CheckAccess = mr_bool.YES;
        } else {
            System.IO.Directory.CreateDirectory(DirName);
            Error = null;
            CheckAccess = mr_bool.NO;
        }
    } catch (System.Exception e) {
        Error = e;
        CheckAccess = mr_bool.NO;
    }
").

:- pragma foreign_proc("Java",
    make_directory_including_parents_2(DirName::in, Error::out,
        CheckAccess::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        java.io.File dir = new java.io.File(DirName);
        if (dir.isFile()) {
            Error = new java.lang.RuntimeException(
                ""a file with that name already exists"");
            CheckAccess = bool.NO;
        } else if (dir.isDirectory()) {
            Error = null;
            CheckAccess = bool.YES;
        } else if (dir.mkdirs()) {
            Error = null;
            CheckAccess = bool.NO;
        } else {
            Error = new java.lang.RuntimeException(""make_directory failed"");
            CheckAccess = bool.NO;
        }
    } catch (java.lang.Exception e) {
        Error = e;
        CheckAccess = bool.NO;
    }
").

%---------------------------------------------------------------------------%

make_single_directory(DirName, Result, !IO) :-
    make_single_directory_2(DirName, Status, MaybeWin32Error, !IO),
    (
        Status = ok,
        Result = ok
    ;
        ( Status = name_exists
        ; Status = dir_exists
        ; Status = error
        ),
        make_maybe_win32_err_msg(MaybeWin32Error, "cannot create directory: ",
            Message),
        Result = error(make_io_error(Message))
    ).

:- type make_single_directory_status
    --->    ok
    ;       name_exists     % may or may not be directory
    ;       dir_exists
    ;       error.

:- pragma foreign_export_enum("C", make_single_directory_status/0,
    [prefix("ML_MAKE_SINGLE_DIRECTORY_"), uppercase]).
:- pragma foreign_export_enum("C#", make_single_directory_status/0,
    [prefix("ML_MAKE_SINGLE_DIRECTORY_"), uppercase]).
:- pragma foreign_export_enum("Java", make_single_directory_status/0,
    [prefix("ML_MAKE_SINGLE_DIRECTORY_"), uppercase]).

:- pred make_single_directory_2(string::in, make_single_directory_status::out,
    io.system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    make_single_directory_2(DirName::in, Status::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness, may_not_duplicate],
"
#if defined(MR_WIN32)
    if (CreateDirectoryW(ML_utf8_to_wide(DirName), NULL)) {
        Status = ML_MAKE_SINGLE_DIRECTORY_OK;
        Error = 0;
    } else {
        Error = GetLastError();
        if (Error == ERROR_ALREADY_EXISTS) {
            Status = ML_MAKE_SINGLE_DIRECTORY_NAME_EXISTS;
        } else {
            Status = ML_MAKE_SINGLE_DIRECTORY_ERROR;
        }
    }
#elif defined(MR_HAVE_MKDIR)
    if (mkdir(DirName, 0777) == 0) {
        Status = ML_MAKE_SINGLE_DIRECTORY_OK;
        Error = 0;
    } else {
        Status = ML_MAKE_SINGLE_DIRECTORY_ERROR;
        Error = errno;
      #ifdef EEXIST
        if (Error == EEXIST) {
            Status = ML_MAKE_SINGLE_DIRECTORY_NAME_EXISTS;
        }
      #endif /* EEXIST */
    }
#else /* !MR_WIN32 && !MR_HAVE_MKDIR */
    Status = ML_MAKE_SINGLE_DIRECTORY_ERROR;
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("C#",
    make_single_directory_2(DirName::in, Status::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        // DirectoryInfo.Create doesn't fail if a file with the same name as
        // the directory being created already exists.
        if (System.IO.File.Exists(DirName)) {
            Status = dir.ML_MAKE_SINGLE_DIRECTORY_ERROR;
            Error = new System.Exception(
                ""a file with that name already exists"");
        } else {
            System.IO.DirectoryInfo info =
                new System.IO.DirectoryInfo(DirName);
            System.IO.DirectoryInfo parent_info = info.Parent;

            // Not sure why we need these first two tests.
            if (parent_info == null) {
                Status = dir.ML_MAKE_SINGLE_DIRECTORY_ERROR;
                Error = new System.Exception(""can't create root directory"");
            } else if (!info.Parent.Exists) {
                Status = dir.ML_MAKE_SINGLE_DIRECTORY_ERROR;
                Error =
                    new System.Exception(""parent directory does not exist"");
            } else if (info.Exists) {
                // DirectoryInfo.Create does nothing if the directory already
                // exists, so we check explicitly. There is a race here.
                Status = dir.ML_MAKE_SINGLE_DIRECTORY_DIR_EXISTS;
                Error = new System.Exception(""directory already exists"");
            } else {
                info.Create();
                Status = dir.ML_MAKE_SINGLE_DIRECTORY_OK;
                Error = null;
            }
        }
    } catch (System.Exception e) {
        Status = dir.ML_MAKE_SINGLE_DIRECTORY_ERROR;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    make_single_directory_2(DirName::in, Status::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    try {
        java.io.File newDir = new java.io.File(DirName);
        java.io.File parent = newDir.getParentFile();

        // Are these first two checks just to produce better error messages?
        if (parent == null) {
            Status = dir.ML_MAKE_SINGLE_DIRECTORY_ERROR;
            Error = new java.io.IOException(""can't create root directory"");
        } else if (!parent.exists()) {
            Status = dir.ML_MAKE_SINGLE_DIRECTORY_ERROR;
            Error =
                new java.io.IOException(""parent directory does not exist"");
        } else if (newDir.isDirectory()) {
            Status = dir.ML_MAKE_SINGLE_DIRECTORY_DIR_EXISTS;
            Error = new java.io.IOException(""directory already exists"");
        } else {
            if (newDir.mkdir()) {
                Status = dir.ML_MAKE_SINGLE_DIRECTORY_OK;
                Error = null;
            } else {
                Status = dir.ML_MAKE_SINGLE_DIRECTORY_ERROR;
                Error = new java.io.IOException(""mkdir failed"");
            }
        }
    } catch (java.lang.Exception e) {
        Status = dir.ML_MAKE_SINGLE_DIRECTORY_ERROR;
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    make_single_directory_2(DirName::in, Status::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        may_not_duplicate],
"
    DirNameStr = binary_to_list(DirName),
    case file:make_dir(DirNameStr) of
        ok ->
            Status = {ok},
            Error = ok;
        {error, eexist} ->
            Status = {name_exists},
            Error = {error, eexist};
        {error, Reason} ->
            Status = {error},
            Error = {error, Reason}
    end
").

%---------------------------------------------------------------------------%

foldl2(P, DirName, Data0, Res, !IO) :-
    dir.foldl2_process_dir(no, P, fixup_dirname(DirName), [], no,
        no, _, Res0, Data0, Data, !IO),
    (
        Res0 = ok,
        Res = ok(Data)
    ;
        Res0 = error(Error),
        Res = error(Data, Error)
    ).

recursive_foldl2(P, DirName, FollowLinks, Data0, Res, !IO) :-
    dir.foldl2_process_dir(no, P, fixup_dirname(DirName), [], yes,
        FollowLinks, _, Res0, Data0, Data, !IO),
    (
        Res0 = ok,
        Res = ok(Data)
    ;
        Res0 = error(Error),
        Res = error(Data, Error)
    ).

    % Under Windows, you cannot list the files of a directory if the directory
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

:- pred foldl2_process_dir(bool::in, dir.foldl_pred(T)::in(dir.foldl_pred),
    string::in, list(file_id)::in, bool::in, bool::in, bool::out, io.res::out,
    T::in, T::out, io::di, io::uo) is det.

foldl2_process_dir(SymLinkParent, P, DirName, ParentIds0, Recursive,
        FollowLinks, Continue, Result, !Data, !IO) :-
    ( if
        Recursive = yes,
        FollowLinks = yes
    then
        check_for_symlink_loop(SymLinkParent, DirName, LoopRes,
            ParentIds0, ParentIds, !IO)
    else
        ParentIds = ParentIds0,
        LoopRes = ok(no)
    ),
    (
        LoopRes = ok(no),
        dir.open(DirName, OpenResult, !IO),
        (
            OpenResult = ok(Dir),
            promise_equivalent_solutions [!:IO, TryResult] (
                try_io(
                    foldl2_process_dir_aux(Dir, SymLinkParent, P, DirName,
                        ParentIds, Recursive, FollowLinks, !.Data),
                    TryResult, !IO)
            ),
            dir.close(Dir, CloseRes, !IO),
            (
                TryResult = succeeded({Continue, Result1, !:Data}),
                (
                    Result1 = ok,
                    Result = CloseRes
                ;
                    Result1 = error(Error),
                    Result = error(Error)
                )
            ;
                TryResult = exception(_),
                rethrow(TryResult)
            )
        ;
            OpenResult = eof,
            Continue = yes,
            Result = ok
        ;
            OpenResult = error(Error),
            Continue = no,
            Result = error(Error)
        )
    ;
        LoopRes = ok(yes),
        Continue = yes,
        Result = ok
    ;
        LoopRes = error(Error),
        Continue = no,
        Result = error(Error)
    ).

:- pred foldl2_process_dir_aux(dir.stream::in, bool::in,
    dir.foldl_pred(T)::in(dir.foldl_pred), string::in, list(file_id)::in,
    bool::in, bool::in, T::in, {bool, io.res, T}::out, io::di, io::uo) is det.

foldl2_process_dir_aux(Dir, SymLinkParent, P, DirName, ParentIds,
        Recursive, FollowLinks, !.Data, {Continue, Res, !:Data}, !IO) :-
    foldl2_process_dir_entries(Dir, SymLinkParent, P, DirName, ParentIds,
        Recursive, FollowLinks, Continue, Res, !Data, !IO).

:- pred foldl2_process_dir_entries(dir.stream::in, bool::in,
    dir.foldl_pred(T)::in(dir.foldl_pred), string::in, list(file_id)::in,
    bool::in, bool::in, bool::out, io.res::out, T::in, T::out,
    io::di, io::uo) is det.

foldl2_process_dir_entries(Dir, SymLinkParent, P, DirName, ParentIds,
        Recursive, FollowLinks, Continue, Res, !Data, !IO) :-
    dir.read_entry(Dir, ReadRes, !IO),
    (
        ReadRes = ok(FileName),
        PathName = make_path_name(DirName, FileName),
        io.file_type(no, PathName, FileTypeRes, !IO),
        (
            FileTypeRes = ok(FileType),
            P(DirName, FileName, FileType, Continue0, !Data, !IO),
            (
                Continue0 = yes,
                ( if
                    Recursive = yes,
                    FileType = directory
                then
                    % XXX SymLinkParent?
                    foldl2_process_dir(SymLinkParent, P, PathName, ParentIds,
                        Recursive, FollowLinks, Continue1, Res1, !Data, !IO)
                else if
                    Recursive = yes,
                    FileType = symbolic_link,
                    FollowLinks = yes
                then
                    io.file_type(yes, PathName, TargetTypeRes, !IO),
                    (
                        TargetTypeRes = ok(TargetType),
                        (
                            TargetType = directory,
                            foldl2_process_dir(yes, P, PathName, ParentIds,
                                Recursive, FollowLinks, Continue1, Res1,
                                !Data, !IO)
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
                            Continue1 = yes,
                            Res1 = ok
                        )
                    ;
                        TargetTypeRes = error(TargetTypeError),
                        Continue1 = no,
                        Res1 = error(TargetTypeError)
                    )
                else
                    Continue1 = yes,
                    Res1 = ok
                ),
                ( if
                    Continue1 = yes,
                    Res1 = ok
                then
                    foldl2_process_dir_entries(Dir, SymLinkParent, P, DirName,
                        ParentIds, Recursive, FollowLinks, Continue, Res,
                        !Data, !IO)
                else
                    Continue = no,
                    Res = Res1
                )
            ;
                Continue0 = no,
                Continue = no,
                Res = ok
            )
        ;
            FileTypeRes = error(Error),
            Continue = no,
            Res = error(Error)
        )
    ;
        ReadRes = eof,
        Continue = yes,
        Res = ok
    ;
        ReadRes = error(Error),
        Continue = no,
        Res = error(Error)
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

:- pragma foreign_decl("C", local, "

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
    struct ML_DIR_STREAM {
        HANDLE      handle;         /* may be INVALID_HANDLE_VALUE */
        MR_String   pending_entry;  /* initially populated, then NULL */
    };
    typedef struct ML_DIR_STREAM *ML_DIR_STREAM;
#elif defined(MR_HAVE_READDIR)
    typedef DIR *ML_DIR_STREAM;
#else
    typedef MR_Integer ML_DIR_STREAM;
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

:- pred open(string::in, io.result(dir.stream)::out, io::di, io::uo) is det.

open(DirName, Res, !IO) :-
    ( if have_win32 then
        check_dir_readable(DirName, Res0, !IO),
        (
            Res0 = ok,
            DirPattern = make_path_name(DirName, "*"),
            dir.open_2(DirName, DirPattern, Res, !IO)
        ;
            Res0 = error(Error),
            Res = error(Error)
        )
    else
        DirPattern = "", % unused
        dir.open_2(DirName, DirPattern, Res, !IO)
    ).

:- pred open_2(string::in, string::in, io.result(dir.stream)::out,
    io::di, io::uo) is det.

open_2(DirName, DirPattern, Res, !IO) :-
    dir.open_3(DirName, DirPattern, Dir, MaybeWin32Error, !IO),
    ( if
        is_maybe_win32_error(MaybeWin32Error, "cannot open directory: ",
            IOError)
    then
        Res = error(IOError)
    else
        Res = ok(Dir)
    ).

:- pred open_3(string::in, string::in, dir.stream::out,
    io.system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.open_3(DirName::in, DirPattern::in, Dir::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness, may_not_duplicate],
"
#if defined(MR_WIN32)
    WIN32_FIND_DATAW    file_data;

    Dir = MR_GC_NEW_ATTRIB(struct ML_DIR_STREAM, MR_ALLOC_ID);

    Dir->handle = FindFirstFileW(ML_utf8_to_wide(DirPattern), &file_data);
    if (Dir->handle == INVALID_HANDLE_VALUE) {
        Error = GetLastError();
        if (Error == ERROR_NO_MORE_FILES) {
            Error = 0;
        }
        Dir->pending_entry = NULL;
    } else {
        Error = 0;
        Dir->pending_entry = ML_wide_to_utf8(file_data.cFileName, MR_ALLOC_ID);
    }

#elif defined(MR_HAVE_OPENDIR) && defined(MR_HAVE_READDIR) && \\
        defined(MR_HAVE_CLOSEDIR)

    Dir = opendir(DirName);
    if (Dir == NULL) {
        Error = errno;
    } else {
        Error = 0;
    }

#else /* !MR_WIN32 && !(MR_HAVE_OPENDIR etc.) */
    Dir = NULL;
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("C#",
    dir.open_3(DirName::in, _DirPattern::in, Dir::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_modify_trail, promise_pure, tabled_for_io, thread_safe],
"
    try {
        Dir =
            System.IO.Directory.GetFileSystemEntries(DirName).GetEnumerator();
        Error = null;
    } catch (System.Exception e) {
        Dir = null;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    dir.open_3(DirName::in, _DirPattern::in, Dir::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        java.io.File file = new java.io.File(DirName);
        if (file.isDirectory()) {
            String[] list = file.list();
            if (list != null) {
                Dir = java.util.Arrays.asList(list).iterator();
                Error = null;
            } else {
                Dir = null;
                // Probably permission problem.
                Error = new java.io.IOException(""Error getting file list"");
            }
        } else if (!file.exists()) {
            Dir = null;
            Error = new java.io.IOException(""No such file or directory"");
        } else {
            Dir = null;
            Error = new java.io.IOException(""Not a directory"");
        }
    } catch (java.lang.Exception e) {
        Dir = null;
        Error = e;
    }
").

:- pragma foreign_proc("Erlang",
    dir.open_3(DirName::in, _DirPattern::in, Dir::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    DirNameStr = binary_to_list(DirName),
    case file:list_dir(DirNameStr) of
        {ok, FileNames} ->
            Dir = ets:new(mutvar, [set, private]),
            ets:insert(Dir, {value, lists:sort(FileNames)}),
            Error = ok;
        {error, Reason} ->
            Dir = null,
            Error = {error, Reason}
    end
").

:- pred check_dir_readable(string::in, io.res::out, io::di, io::uo) is det.

check_dir_readable(DirName, Res, !IO) :-
    io.file_type(yes, DirName, FileTypeRes, !IO),
    (
        FileTypeRes = ok(FileType),
        (
            FileType = directory,
            io.check_file_accessibility(DirName, [read, execute], Res, !IO)
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
            Res = error(make_io_error(
                "dir.foldl2: pathname is not a directory"))
        )
    ;
        FileTypeRes = error(Error),
        Res = error(Error)
    ).

:- pred close(dir.stream::in, io.res::out, io::di, io::uo) is det.

close(Dir, Res, !IO) :-
    dir.close_2(Dir, MaybeWin32Error, !IO),
    ( if
        is_maybe_win32_error(MaybeWin32Error,
            "dir.foldl2: closing directory failed: ", IOError)
    then
        Res = error(IOError)
    else
        Res = ok
    ).

:- pred close_2(dir.stream::in, io.system_error::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    dir.close_2(Dir::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness, may_not_duplicate],
"
#if defined(MR_WIN32)
    if (Dir->handle == INVALID_HANDLE_VALUE) {
        Error = 0;
    } else if (FindClose(Dir->handle)) {
        Dir->handle = INVALID_HANDLE_VALUE;
        Error = 0;
    } else {
        Error = GetLastError();
    }
#elif defined(MR_HAVE_CLOSEDIR)
    if (closedir(Dir) == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
#else
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("C#",
    dir.close_2(_Dir::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    /* Nothing to do. */
    Error = null;
").

:- pragma foreign_proc("Java",
    dir.close_2(_Dir::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    /* Nothing to do. */
    Error = null;
").

:- pragma foreign_proc("Erlang",
    dir.close_2(Dir::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    ets:delete(Dir, value),
    Error = ok
").

:- pred read_entry(dir.stream::in, io.result(string)::out, io::di, io::uo)
    is det.

read_entry(Dir, Res, !IO) :-
    dir.read_entry_2(Dir, MaybeWin32Error, HaveFileName, FileName, !IO),
    ( if
        is_maybe_win32_error(MaybeWin32Error,
            "dir.foldl2: reading directory entry failed: ", IOError)
    then
        Res = error(IOError)
    else
        (
            HaveFileName = no,
            Res = eof
        ;
            HaveFileName = yes,
            ( if
                ( FileName = dir.this_directory
                ; FileName = dir.parent_directory
                )
            then
                dir.read_entry(Dir, Res, !IO)
            else
                Res = ok(FileName)
            )
        )
    ).

    % read_entry_2(Dir, MaybeWin32Error, HaveFileName, FileName, !IO):
    % If there is no error and HaveFileName = no, then we have reached the
    % end-of-stream.
    %
:- pred read_entry_2(dir.stream::in, io.system_error::out, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    dir.read_entry_2(Dir::in, Error::out, HaveFileName::out, FileName::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness, may_not_duplicate],
"
#if defined(MR_WIN32)
    WIN32_FIND_DATAW file_data;

    if (Dir->handle == INVALID_HANDLE_VALUE) {
        /* Directory was empty when opened. */
        Error = 0;
        HaveFileName = MR_NO;
        FileName = MR_make_string_const("""");
    } else if (Dir->pending_entry != NULL) {
        /* FindFirstFileW already returned the first entry. */
        Error = 0;
        HaveFileName = MR_YES;
        FileName = Dir->pending_entry;
        Dir->pending_entry = NULL;
    } else if (FindNextFileW(Dir->handle, &file_data)) {
        Error = 0;
        HaveFileName = MR_YES;
        FileName = ML_wide_to_utf8(file_data.cFileName, MR_ALLOC_ID);
    } else {
        Error = GetLastError();
        if (Error == ERROR_NO_MORE_FILES) {
            Error = 0;
        }
        HaveFileName = MR_NO;
        FileName = MR_make_string_const("""");
    }

#elif defined(MR_HAVE_READDIR) && defined(MR_HAVE_CLOSEDIR)
    struct dirent *dir_entry;

    errno = 0;          /* to detect end-of-stream */
    dir_entry = readdir(Dir);
    if (dir_entry == NULL) {
        Error = errno;  /* remains zero at end-of-stream */
        HaveFileName = MR_NO;
        FileName = MR_make_string_const("""");
    } else {
        Error = 0;
        HaveFileName = MR_YES;
        MR_make_aligned_string_copy_msg(FileName, dir_entry->d_name,
            MR_ALLOC_ID);
    }

#else /* !MR_WIN32 && !(MR_HAVE_READDIR etc.) */
    Error = ENOSYS;
    HaveFileName = MR_NO;
    FileName = MR_make_string_const("""");
#endif
").

:- pragma foreign_proc("C#",
    dir.read_entry_2(Dir::in, Error::out, HaveFileName::out, FileName::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (Dir.MoveNext()) {
            // The .NET CLI returns path names qualified with
            // the directory name passed to dir.open.
            HaveFileName = mr_bool.YES;
            FileName = System.IO.Path.GetFileName((string) Dir.Current);
        } else {
            HaveFileName = mr_bool.NO;
            FileName = """";
        }
        Error = null;
    } catch (System.Exception e) {
        Error = e;
        HaveFileName = mr_bool.NO;
        FileName = """";
    }
").

:- pragma foreign_proc("Java",
    dir.read_entry_2(Dir::in, Error::out, HaveFileName::out, FileName::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (Dir.hasNext()) {
            HaveFileName = bool.YES;
            FileName = (java.lang.String) Dir.next();
        } else {
            HaveFileName = bool.NO;
            FileName = """";
        }
        Error = null;
    } catch (java.lang.Exception e) {
        Error = e;
        HaveFileName = bool.NO;
        FileName = """";
    }
").

:- pragma foreign_proc("Erlang",
    dir.read_entry_2(Dir::in, Error::out, HaveFileName::out,
        FileName::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    [{value, FileNames0}] = ets:lookup(Dir, value),
    case FileNames0 of
        [] ->
            HaveFileName = {no},
            FileName = <<>>;
        [Head | Tail] ->
            HaveFileName = {yes},
            FileName = list_to_binary(Head),
            ets:insert(Dir, {value, Tail})
    end,
    Error = ok
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

use_windows_paths :-
    dir.directory_separator = ('\\').

%---------------------------------------------------------------------------%
:- end_module dir.
%---------------------------------------------------------------------------%
