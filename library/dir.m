%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995,1997,1999-2000,2002-2012 The University of Melbourne.
% Copyright (C) 2016-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
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
% if that does not change the meaning of the path name.
%
% Duplicate directory separators and trailing separators are also removed
% where that does not change the meaning of the path name.
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
% Predicates to isolate system dependencies.
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
    % if that does not change the meaning of PathName.
    %
    % Trailing slashes are removed from DirName after splitting,
    % if that does not change the meaning of DirName.
    %
    % On Windows, drive current directories are handled correctly,
    % for example `split_name("C:foo", "C:", "foo")'.
    % (`X:' is the current directory on drive `X').
    % Note that Cygwin does not support drive current directories,
    % so `split_name("C:foo", _, _)' will fail when running under Cygwin.
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
    % if that does not change the meaning of PathName.
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
    % Trailing slashes in PathName are removed first, if that does not change
    % the meaning of PathName.
    %
    % Trailing slashes are removed from DirName after splitting,
    % if that does not change the meaning of DirName.
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
    % (this does not check whether the path exists).
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
    % Fails if the directory already exists, or the parent directory
    % does not exist.
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

    % foldl2(Pred, DirName, InitialData, Result, !IO):
    %
    % Apply `Pred' to all files and directories in the given directory.
    % Directories are not processed recursively.
    % Processing will stop if the boolean (Continue) output of Pred is bound
    % to `no'.
    % The order in which the entries are processed is unspecified.
    %
:- pred foldl2(foldl_pred(T)::in(foldl_pred), string::in,
    T::in, io.maybe_partial_res(T)::out, io::di, io::uo) is det.

    % recursive_foldl2(Pred, DirName, FollowSymLinks, InitialData, Result,
    %   !IO):
    %
    % As above, but recursively process subdirectories.
    % Subdirectories are processed depth-first, processing the directory itself
    % before its contents. If `FollowSymLinks' is `yes', recursively process
    % the directories referenced by symbolic links.
    %
:- pred recursive_foldl2(foldl_pred(T)::in(foldl_pred),
    string::in, bool::in, T::in, io.maybe_partial_res(T)::out,
    io::di, io::uo) is det.

:- type fold_params
    --->    fold_params(
                fp_subdirs      :: maybe_subdirs,
                fp_on_error     :: on_error
            ).

:- type maybe_subdirs
    --->    do_not_enter_subdirs
    ;       enter_subdirs(maybe_follow_symlinks).

:- type maybe_follow_symlinks
    --->    do_not_follow_symlinks
    ;       follow_symlinks.

:- type on_error
    --->    on_error_stop
    ;       on_error_keep_going.

:- type file_error
    --->    file_error(string, file_operation, io.error).
            % file_error(PathName, Operation, Error) means that
            % when we tried to perform Operation on PathName, the result
            % was Error. PathName specifies the file name relative to
            % the directory name given to general_foldl2.

:- type file_operation
    --->    file_open
    ;       file_close
    ;       file_get_id
    ;       file_get_type
    ;       file_check_accessibility
    ;       file_read_dir_entry.

    % general_foldl2(Params, Pred, DirName, Data0, Data, Errors, !IO).
    %
    % A generalised version of the above, whose behavior is controlled
    % by setting up Params.
    %
    % Whether we recursively process subdirectories depends on whether
    % the fp_subdirs field of Params is do_not_enter_subdirs or enter_subdirs.
    % If it is do_not_enter_subdirs, then we do not process subdirectories
    % at all. If it is enter_subdirs, then we process subdirectories depth
    % first. The traversal is preorder, meaning that we call Pred on the
    % pathname of a subdirectory *before* we process the contents of that
    % subdirectory.
    %
    % Whether we recursively process subdirectories referenced by symlinks
    % depends on the first argument of enter_subdirs.
    %
    % When we encounter an error, such as a failure to open a directory
    % for reading, we record that error, but what happens after that
    % depends on the fp_on_error field of Params. If this field is
    % on_error_stop, then we stop the traversal, which means that
    % with on_error_stop, we will return at most one error.
    % If it is on_error_keep_going, we continue with the traversal after
    % errors, which means that with on_error_keep_going, we can return
    % more than one error.
    %
    % Regardless of the setting of fp_on_error, we stop the traversal
    % if Pred returns Continue = `no'.
    %
    % In all cases, the value of Data will reflect the results of all
    % the invocations of Pred during the traversal up to the time
    % the traversal was stopped either by an error, by Continue = `no',
    % or by running out of files to traverse.
    %
:- pred general_foldl2(fold_params::in, foldl_pred(T)::in(foldl_pred),
    string::in, T::in, T::out, list(file_error)::out,
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
:- import_module maybe.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module unit.

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
        % if doing so does not change the meaning.
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

        % "\\" is not a UNC path name, so it is equivalent to "\".
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
    % for the platform, if that does not change the meaning.
    % On Cygwin, "\foo\bar" (relative to root of current drive)
    % is different to "/foo/bar" (relative to Cygwin root directory),
    % so we cannot convert separators.
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
    % and it is pretty awful anyway (`C:foo' is not the same as `C:\foo'),
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
:- pragma no_determinism_warning(pred(is_dotnet_root_directory_2/1)).

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
:- pragma no_determinism_warning(pred(dotnet_path_name_is_absolute_2/1)).

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
            % Do not introduce duplicate directory separators.
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

current_directory(Result, !IO) :-
    current_directory_2(CurDir, Error, !IO),
    is_error(Error, "dir.current_directory failed: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        Result = ok(CurDir)
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
        // `size' includes the NUL terminator.
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
        // Buffer too small. Resize and try again.
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

%---------------------------------------------------------------------------%

make_directory(PathName, Result, !IO) :-
    ( if have_make_directory_including_parents then
        make_directory_including_parents(PathName, Result, !IO)
    else
        DirName = dir.dirname(PathName),
        ( if PathName = DirName then
            % We have been asked to make a root directory
            % -- the mkdir will fail.
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

:- pred make_directory_or_check_exists(string::in, io.res::out,
    io::di, io::uo) is det.

make_directory_or_check_exists(DirName, Result, !IO) :-
    make_single_directory_2(DirName, MakeDirStatus, MaybeWin32Error, !IO),
    (
        MakeDirStatus = ok,
        Result = ok
    ;
        MakeDirStatus = name_exists,
        io.file_type(yes, DirName, TypeResult, !IO),
        ( if TypeResult = ok(directory) then
            check_dir_accessibility(DirName, Result, !IO)
        else
            make_maybe_win32_err_msg(MaybeWin32Error,
                "cannot create directory: ", Message, !IO),
            Result = error(make_io_error(Message))
        )
    ;
        MakeDirStatus = dir_exists,
        check_dir_accessibility(DirName, Result, !IO)
    ;
        MakeDirStatus = error,
        make_maybe_win32_err_msg(MaybeWin32Error,
            "cannot create directory: ", Message, !IO),
        Result = error(make_io_error(Message))
    ).

:- pred check_dir_accessibility(string::in, io.res::out, io::di, io::uo)
    is det.

check_dir_accessibility(DirName, Result, !IO) :-
    % Check whether we can read and write the directory.
    io.check_file_accessibility(DirName, [read, write, execute], Result, !IO).

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

make_directory_including_parents(DirName, Result, !IO) :-
    make_directory_including_parents_2(DirName, SystemError, CheckAccess, !IO),
    is_error(SystemError, "cannot make directory: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        (
            CheckAccess = yes,
            check_dir_accessibility(DirName, Result, !IO)
        ;
            CheckAccess = no,
            Result = ok
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
            Message, !IO),
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
      #endif // EEXIST
    }
#else // !MR_WIN32 && !MR_HAVE_MKDIR
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

%---------------------------------------------------------------------------%

foldl2(Pred, DirName, Data0, Result, !IO) :-
    SubDirs = do_not_enter_subdirs,
    Params = fold_params(SubDirs, on_error_stop),
    dir.foldl2_process_dir(Params, Pred, fixup_dirname(DirName),
        parent_is_not_symlink, [], user_continue, _MaybeUserStop,
        [], RevErrors, Data0, Data, !IO),
    list.reverse(RevErrors, Errors),
    (
        Errors = [],
        Result = ok(Data)
    ;
        Errors = [HeadError | _],
        HeadError = file_error(_, _, Error),
        Result = error(Data, Error)
    ).

recursive_foldl2(Pred, DirName, FollowLinks0, Data0, Result, !IO) :-
    ( FollowLinks0 = no,  FollowLinks = do_not_follow_symlinks
    ; FollowLinks0 = yes, FollowLinks = follow_symlinks
    ),
    SubDirs = enter_subdirs(FollowLinks),
    Params = fold_params(SubDirs, on_error_stop),
    dir.foldl2_process_dir(Params, Pred, fixup_dirname(DirName),
        parent_is_not_symlink, [], user_continue, _MaybeUserStop,
        [], RevErrors, Data0, Data, !IO),
    list.reverse(RevErrors, Errors),
    (
        Errors = [],
        Result = ok(Data)
    ;
        Errors = [HeadError | _],
        HeadError = file_error(_, _, Error),
        Result = error(Data, Error)
    ).

general_foldl2(Params, Pred, DirName, Data0, Data, Errors, !IO) :-
    dir.foldl2_process_dir(Params, Pred, fixup_dirname(DirName),
        parent_is_not_symlink, [], user_continue, _MaybeUserStop,
        [], RevErrors, Data0, Data, !IO),
    list.reverse(RevErrors, Errors).

%---------------------%

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

%---------------------%

:- type is_parent_symlink
    --->    parent_is_not_symlink
    ;       parent_is_symlink.

:- type maybe_user_stop
    --->    user_continue
    ;       user_stop.

:- type maybe_file_error(T)
    --->    mfe_ok(T)
    ;       mfe_error(file_error).

:- type maybe_file_error == maybe_file_error(unit).

:- pred foldl2_process_dir(fold_params::in,
    dir.foldl_pred(T)::in(dir.foldl_pred),
    string::in, is_parent_symlink::in, list(file_id)::in,
    maybe_user_stop::in, maybe_user_stop::out,
    list(file_error)::in, list(file_error)::out, T::in, T::out,
    io::di, io::uo) is det.

foldl2_process_dir(Params, Pred, DirName, SymLinkParent, ParentIds0,
        !MaybeUserStop, !RevErrors, !Data, !IO) :-
    % XXX We remove this sanity check after some experience with this case,
    % say in July 2021.
    ( if
        (
            !.MaybeUserStop = user_stop
        ;
            !.RevErrors = [_ | _],
            Params ^ fp_on_error = on_error_stop
        )
    then
        unexpected($pred, "our caller did not check stop condition")
    else
        true
    ),
    ( if Params ^ fp_subdirs = enter_subdirs(follow_symlinks) then
        check_for_symlink_loop(DirName, SymLinkParent, ParentIds0,
            MaybeLoop, !IO)
    else
        MaybeLoop = is_not_loop(ParentIds0)
    ),
    (
        MaybeLoop = is_not_loop(ParentIds),
        dir.open(DirName, OpenResult, !IO),
        (
            OpenResult = mfe_ok(DirStream),
            % To avoid resource leaks, we need to close DirStream
            % even if an exception is thrown.
            % XXX It would be nice to know what code could throw exceptions.
            % The Mercury code in this module does not throw exceptions,
            % except for the *re*throw just below.
            promise_equivalent_solutions [!:IO, TryResult] (
                try_io(
                    foldl2_process_dir_entries_for_try(Params, Pred,
                        DirName, DirStream, SymLinkParent, ParentIds,
                        !.MaybeUserStop, !.RevErrors, !.Data),
                    TryResult, !IO)
            ),
            dir.close(DirName, DirStream, CloseResult, !IO),
            (
                TryResult = succeeded({!:MaybeUserStop, !:RevErrors, !:Data}),
                (
                    CloseResult = mfe_ok(unit)
                ;
                    CloseResult = mfe_error(Error),
                    !:RevErrors = [Error | !.RevErrors]
                )
            ;
                TryResult = exception(_),
                rethrow(TryResult)
            )
        ;
            OpenResult = mfe_error(Error),
            !:RevErrors = [Error | !.RevErrors]
        )
    ;
        MaybeLoop = is_loop
    ;
        MaybeLoop = is_error(Error),
        !:RevErrors = [Error | !.RevErrors]
    ).

:- pred foldl2_process_dir_entries_for_try(fold_params::in,
    dir.foldl_pred(T)::in(dir.foldl_pred),
    string::in, dir.stream::in, is_parent_symlink::in, list(file_id)::in,
    maybe_user_stop::in, list(file_error)::in, T::in,
    {maybe_user_stop, list(file_error), T}::out,
    io::di, io::uo) is det.

foldl2_process_dir_entries_for_try(Params, Pred, DirName, DirStream,
        SymLinkParent, ParentIds, !.MaybeUserStop, !.RevErrors, !.Data,
        {!:MaybeUserStop, !:RevErrors, !:Data}, !IO) :-
    foldl2_process_dir_entries(Params, Pred, DirName, DirStream, SymLinkParent,
        ParentIds, !MaybeUserStop, !RevErrors, !Data, !IO).

:- pred foldl2_process_dir_entries(fold_params::in,
    dir.foldl_pred(T)::in(dir.foldl_pred),
    string::in, dir.stream::in, is_parent_symlink::in, list(file_id)::in,
    maybe_user_stop::in, maybe_user_stop::out,
    list(file_error)::in, list(file_error)::out, T::in, T::out,
    io::di, io::uo) is det.

foldl2_process_dir_entries(Params, Pred, DirName, DirStream, SymLinkParent,
        ParentIds, !MaybeUserStop, !RevErrors, !Data, !IO) :-
    ( if
        (
            !.MaybeUserStop = user_stop
        ;
            !.RevErrors = [_ | _],
            Params ^ fp_on_error = on_error_stop
        )
    then
        true
    else
        dir.read_entry(DirStream, ReadResult, !IO),
        (
            ReadResult = ok(FileName),
            PathName = make_path_name(DirName, FileName),
            io.file_type(no, PathName, FileTypeResult, !IO),
            (
                FileTypeResult = ok(FileType),
                Pred(DirName, FileName, FileType, PredSaysContinue,
                    !Data, !IO),
                (
                    PredSaysContinue = yes,
                    ( if
                        FileType = directory,
                        Params ^ fp_subdirs = enter_subdirs(_)
                    then
                        % XXX SymLinkParent?
                        foldl2_process_dir(Params, Pred, PathName,
                            SymLinkParent, ParentIds,
                            !MaybeUserStop, !RevErrors, !Data, !IO)
                    else if
                        FileType = symbolic_link,
                        Params ^ fp_subdirs = enter_subdirs(follow_symlinks)
                    then
                        io.file_type(yes, PathName, TargetTypeResult, !IO),
                        (
                            TargetTypeResult = ok(TargetType),
                            (
                                TargetType = directory,
                                foldl2_process_dir(Params, Pred, PathName,
                                    parent_is_symlink, ParentIds,
                                    !MaybeUserStop, !RevErrors, !Data, !IO)
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
                                )
                            )
                        ;
                            TargetTypeResult = error(TargetTypeError),
                            Error = file_error(PathName, file_get_type,
                                TargetTypeError),
                            !:RevErrors = [Error | !.RevErrors]
                        )
                    else
                        true
                    ),
                    foldl2_process_dir_entries(Params, Pred,
                        DirName, DirStream, SymLinkParent, ParentIds,
                        !MaybeUserStop, !RevErrors, !Data, !IO)
                ;
                    PredSaysContinue = no,
                    % We do not call foldl2_process_dir_entries recursively.
                    !:MaybeUserStop = user_stop
                )
            ;
                FileTypeResult = error(IOError),
                Error = file_error(PathName, file_get_type, IOError),
                !:RevErrors = [Error | !.RevErrors]
            )
        ;
            ReadResult = eof
        ;
            ReadResult = error(IOError),
            Error = file_error(DirName, file_read_dir_entry, IOError),
            !:RevErrors = [Error | !.RevErrors]
        )
    ).

:- type maybe_loop
    --->    is_not_loop(list(file_id))
    ;       is_loop
    ;       is_error(file_error).

    % Check whether we have seen this directory before in this branch of the
    % directory tree. This only works if the system can provide a unique
    % identifier for each file. Returns `ok(DetectedLoop : bool)' on success.
    %
:- pred check_for_symlink_loop(string::in, is_parent_symlink::in,
    list(file_id)::in, maybe_loop::out, io::di, io::uo) is det.

check_for_symlink_loop(DirName, SymLinkParent, ParentIds0, MaybeLoop, !IO) :-
    ( if io.have_symlinks then
        io.file_id(DirName, IdResult, !IO),
        (
            IdResult = ok(Id),
            ( if
                SymLinkParent = parent_is_symlink,
                list.member(Id, ParentIds0)
            then
                MaybeLoop = is_loop
            else
                ParentIds = [Id | ParentIds0],
                MaybeLoop = is_not_loop(ParentIds)
            )
        ;
            IdResult = error(Error),
            MaybeLoop = is_error(file_error(DirName, file_get_id, Error))
        )
    else
        % There is no point in updating the list of parent ids,
        % since we will never need them.
        MaybeLoop = is_not_loop(ParentIds0)
    ).

:- pragma foreign_decl("C", local,
"
#include ""mercury_string.h""
#include ""mercury_types.h""

#if defined(MR_WIN32) && defined(MR_HAVE_WINDOWS_H)
  #include ""mercury_windows.h""
  #include <direct.h>   // for _wgetcwd
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
        HANDLE      handle;         // may be INVALID_HANDLE_VALUE
        MR_String   pending_entry;  // initially populated, then NULL
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

:- pred open(string::in, maybe_file_error(dir.stream)::out,
    io::di, io::uo) is det.

open(DirName, Result, !IO) :-
    ( if have_win32 then
        check_dir_readable(DirName, ReadabilityResult, !IO),
        (
            ReadabilityResult = mfe_ok(_),
            DirPattern = make_path_name(DirName, "*"),
            dir.open_2(DirName, DirPattern, Result, !IO)
        ;
            ReadabilityResult = mfe_error(Error),
            Result = mfe_error(Error)
        )
    else
        DirPattern = "", % unused
        dir.open_2(DirName, DirPattern, Result, !IO)
    ).

:- pred open_2(string::in, string::in, maybe_file_error(dir.stream)::out,
    io::di, io::uo) is det.

open_2(DirName, DirPattern, Result, !IO) :-
    open_3(DirName, DirPattern, DirStream, MaybeWin32Error, !IO),
    is_maybe_win32_error(MaybeWin32Error, "cannot open directory: ",
        MaybeIOError, !IO),
    (
        MaybeIOError = yes(Error),
        Result = mfe_error(file_error(DirName, file_open, Error))
    ;
        MaybeIOError = no,
        Result = mfe_ok(DirStream)
    ).

:- pred open_3(string::in, string::in, dir.stream::out,
    io.system_error::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    open_3(DirName::in, DirPattern::in, DirStream::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness, may_not_duplicate],
"
#if defined(MR_WIN32)
    WIN32_FIND_DATAW    file_data;

    DirStream = MR_GC_NEW_ATTRIB(struct ML_DIR_STREAM, MR_ALLOC_ID);

    DirStream->handle = FindFirstFileW(ML_utf8_to_wide(DirPattern), &file_data);
    if (DirStream->handle == INVALID_HANDLE_VALUE) {
        Error = GetLastError();
        if (Error == ERROR_NO_MORE_FILES) {
            Error = 0;
        }
        DirStream->pending_entry = NULL;
    } else {
        Error = 0;
        DirStream->pending_entry = ML_wide_to_utf8(file_data.cFileName, MR_ALLOC_ID);
    }

#elif defined(MR_HAVE_OPENDIR) && defined(MR_HAVE_READDIR) && \\
        defined(MR_HAVE_CLOSEDIR)

    DirStream = opendir(DirName);
    if (DirStream == NULL) {
        Error = errno;
    } else {
        Error = 0;
    }

#else // !MR_WIN32 && !(MR_HAVE_OPENDIR etc.)
    DirStream = NULL;
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("C#",
    open_3(DirName::in, _DirPattern::in, DirStream::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_modify_trail, promise_pure, tabled_for_io, thread_safe],
"
    try {
        DirStream =
            System.IO.Directory.GetFileSystemEntries(DirName).GetEnumerator();
        Error = null;
    } catch (System.Exception e) {
        DirStream = null;
        Error = e;
    }
").

:- pragma foreign_proc("Java",
    open_3(DirName::in, _DirPattern::in, DirStream::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        java.io.File file = new java.io.File(DirName);
        if (file.isDirectory()) {
            String[] list = file.list();
            if (list != null) {
                DirStream = java.util.Arrays.asList(list).iterator();
                Error = null;
            } else {
                DirStream = null;
                // Probably permission problem.
                Error = new java.io.IOException(""Error getting file list"");
            }
        } else if (!file.exists()) {
            DirStream = null;
            Error = new java.io.IOException(""No such file or directory"");
        } else {
            DirStream = null;
            Error = new java.io.IOException(""Not a directory"");
        }
    } catch (java.lang.Exception e) {
        DirStream = null;
        Error = e;
    }
").

:- pred check_dir_readable(string::in, maybe_file_error::out,
    io::di, io::uo) is det.

check_dir_readable(DirName, Result, !IO) :-
    io.file_type(yes, DirName, FileTypeResult, !IO),
    (
        FileTypeResult = ok(FileType),
        (
            FileType = directory,
            io.check_file_accessibility(DirName, [read, execute],
                CheckResult, !IO),
            (
                CheckResult = ok,
                Result = mfe_ok(unit)
            ;
                CheckResult = error(IOError),
                Error = file_error(DirName, file_check_accessibility, IOError),
                Result = mfe_error(Error)
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
            % XXX The top level caller may not be dir.foldl2.
            % XXX The message is too verbose for use in a full file_error.
            IOError = make_io_error("pathname is not a directory"),
            % XXX Should file_check_accessibility be something else?
            Error = file_error(DirName, file_check_accessibility, IOError),
            Result = mfe_error(Error)
        )
    ;
        FileTypeResult = error(IOError),
        Result = mfe_error(file_error(DirName, file_get_type, IOError))
    ).

:- pred close(string::in, dir.stream::in, maybe_file_error::out,
    io::di, io::uo) is det.

close(DirName, DirStream, Result, !IO) :-
    close_2(DirStream, MaybeWin32Error, !IO),
    % XXX The top level caller may not be dir.foldl2.
    % XXX The message is too verbose for use in a full file_error.
    is_maybe_win32_error(MaybeWin32Error,
        "closing directory failed: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = mfe_error(file_error(DirName, file_close, IOError))
    ;
        MaybeIOError = no,
        Result = mfe_ok(unit)
    ).

:- pred close_2(dir.stream::in, io.system_error::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    close_2(DirStream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness, may_not_duplicate],
"
#if defined(MR_WIN32)
    if (DirStream->handle == INVALID_HANDLE_VALUE) {
        Error = 0;
    } else if (FindClose(DirStream->handle)) {
        DirStream->handle = INVALID_HANDLE_VALUE;
        Error = 0;
    } else {
        Error = GetLastError();
    }
#elif defined(MR_HAVE_CLOSEDIR)
    if (closedir(DirStream) == 0) {
        Error = 0;
    } else {
        Error = errno;
    }
#else
    Error = ENOSYS;
#endif
").

:- pragma foreign_proc("C#",
    close_2(_DirStream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    // Nothing to do.
    Error = null;
").

:- pragma foreign_proc("Java",
    close_2(_DirStream::in, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    // Nothing to do.
    Error = null;
").

:- pred read_entry(dir.stream::in, io.result(string)::out, io::di, io::uo)
    is det.

read_entry(DirStream, Result, !IO) :-
    read_entry_2(DirStream, MaybeWin32Error, HaveFileName, FileName, !IO),
    % XXX The top level caller may not be dir.foldl2.
    % XXX The message is too verbose for use in a full file_error.
    is_maybe_win32_error(MaybeWin32Error,
        "reading directory entry failed: ", MaybeIOError, !IO),
    (
        MaybeIOError = yes(IOError),
        Result = error(IOError)
    ;
        MaybeIOError = no,
        (
            HaveFileName = no,
            Result = eof
        ;
            HaveFileName = yes,
            ( if
                ( FileName = dir.this_directory
                ; FileName = dir.parent_directory
                )
            then
                dir.read_entry(DirStream, Result, !IO)
            else
                Result = ok(FileName)
            )
        )
    ).

    % read_entry_2(DirStream, MaybeWin32Error, HaveFileName, FileName, !IO):
    % If there is no error and HaveFileName = no, then we have reached the
    % end-of-stream.
    %
:- pred read_entry_2(dir.stream::in, io.system_error::out, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    read_entry_2(DirStream::in, Error::out, HaveFileName::out, FileName::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe,
        will_not_modify_trail, does_not_affect_liveness, may_not_duplicate],
"
#if defined(MR_WIN32)
    WIN32_FIND_DATAW file_data;

    if (DirStream->handle == INVALID_HANDLE_VALUE) {
        // Directory was empty when opened.
        Error = 0;
        HaveFileName = MR_NO;
        FileName = MR_make_string_const("""");
    } else if (DirStream->pending_entry != NULL) {
        // FindFirstFileW already returned the first entry.
        Error = 0;
        HaveFileName = MR_YES;
        FileName = DirStream->pending_entry;
        DirStream->pending_entry = NULL;
    } else if (FindNextFileW(DirStream->handle, &file_data)) {
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

    errno = 0;          // to detect end-of-stream
    dir_entry = readdir(DirStream);
    if (dir_entry == NULL) {
        Error = errno;  // remains zero at end-of-stream
        HaveFileName = MR_NO;
        FileName = MR_make_string_const("""");
    } else {
        Error = 0;
        HaveFileName = MR_YES;
        MR_make_aligned_string_copy_msg(FileName, dir_entry->d_name,
            MR_ALLOC_ID);
    }

#else // !MR_WIN32 && !(MR_HAVE_READDIR etc.)
    Error = ENOSYS;
    HaveFileName = MR_NO;
    FileName = MR_make_string_const("""");
#endif
").

:- pragma foreign_proc("C#",
    read_entry_2(DirStream::in, Error::out, HaveFileName::out, FileName::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (DirStream.MoveNext()) {
            // The .NET CLI returns path names qualified with
            // the directory name passed to dir.open.
            HaveFileName = mr_bool.YES;
            FileName = System.IO.Path.GetFileName((string) DirStream.Current);
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
    read_entry_2(DirStream::in, Error::out, HaveFileName::out, FileName::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, thread_safe],
"
    try {
        if (DirStream.hasNext()) {
            HaveFileName = bool.YES;
            FileName = (java.lang.String) DirStream.next();
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
    error("dir.expand_braces: mismatched braces").
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
