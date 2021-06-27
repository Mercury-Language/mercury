%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The .exp file is for Windows.
% The .exp2 file is for Unix like systems.
% The .exp3 file is for Cygwin.
% The .exp4 file is for the Java grade (on Unix like systems).
%---------------------------------------------------------------------------%

:- module dir_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module exception.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module univ.

main(!IO) :-
    io.write_string("Directory separator is '", !IO),
    io.write_char(dir.directory_separator, !IO),
    io.write_string("'.\n", !IO),

    run_test("\\\\server\\share\\foo", !IO),
    run_test("\\\\server\\share", !IO),
    run_test("\\\\server\\share\\\\", !IO),
    run_test("C:\\foo", !IO),
    run_test("C:\\foo\\", !IO),
    run_test("C:\\", !IO),
    run_test("C:", !IO),
    run_test("\\", !IO),
    run_test("", !IO),
    run_test("foo\\\\bar\\", !IO),
    run_test("foo\\bar\\", !IO),
    run_test("foo", !IO),

    run_test("/foo", !IO),
    run_test("/foo//bar///", !IO),
    run_test("//foo//bar/", !IO),
    run_test("//foo//", !IO),
    run_test("/", !IO),
    run_test("//", !IO),
    run_test("foo/bar", !IO),

    test_make_path_name("C:", "foo", !IO),
    test_make_path_name("C:\\", "foo", !IO),
    test_make_path_name("C:", "C:", !IO),
    test_make_path_name("C:", "C:\\foo", !IO),
    test_make_path_name(".", "/foo", !IO),
    test_make_path_name(".", "\\foo", !IO),
    test_make_path_name("foo", "bar/baz", !IO),
    test_make_path_name("foo/", "bar/baz", !IO),

    io.write_string("checking whether `unwritable' is readable...", !IO),
    io.check_file_accessibility("unwritable", [read], ReadResult, !IO),
    io.write_line(ReadResult, !IO),

    io.check_file_accessibility("unwritable", [read, write], WriteResult, !IO),
    ( if WriteResult = ok then
        io.write_string("Error: unwritable file found to be writable\n", !IO)
    else
        io.write_string("unwritable file found to be unwritable\n", !IO)
    ),

    % Execute permissions are not handled correctly on all platforms so
    % just check that it doesn't crash.
    io.check_file_accessibility("unwritable", [execute], _ExecuteResult, !IO),

    dir.current_directory(CwdResult, !IO),
    (
        CwdResult = ok(Cwd),
        io.write_string("current_directory succeeded: ", !IO),
        io.write_string(dir.det_basename(Cwd), !IO),
        io.nl(!IO)
    ;
        CwdResult = error(Error),
        io.write_string("current_directory failed: ", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.nl(!IO)
    ),

    Dir1 = "test_dir"/"d1",
    test0("make_directory", dir.make_directory(Dir1), !IO),
    % Test making a directory that already exists.
    test0("make_directory", dir.make_directory(Dir1), !IO),

    Dir2 = "test_dir"/"d2",
    dir.make_single_directory(Dir2/"d2", Dir2Res, !IO),
    (
        Dir2Res = ok,
        io.write_string("Error: dir.make_single_directory succeeded " ++
            "but parent doesn't exist.\n", !IO)
    ;
        Dir2Res = error(_),
        io.write_string("dir.make_single_directory " ++
            "with non-existent parent failed as expected.\n", !IO)
    ),

    test0("make_single_directory", dir.make_single_directory(Dir2), !IO),
    test0("make_single_directory 2",
        dir.make_single_directory(Dir2/"d2"), !IO),

    test1("file_type", io.file_type(yes, Dir1), Type, !IO),
    io.write_string("type of ", !IO),
    io.write_string(Dir1, !IO),
    io.write_string(" is ", !IO),
    io.write_line(Type, !IO),

    test1("file_type 2", io.file_type(yes, "dir_test.m"), Type2, !IO),
    io.write_string("type of ", !IO),
    io.write_string("dir_test.m", !IO),
    io.write_string(" is ", !IO),
    io.write_line(Type2, !IO),

    % Create some dummy files
    touch_file(Dir1/"foo", !IO),
    touch_file(Dir1/"baz", !IO),
    touch_file("test_dir"/"quark", !IO),
    touch_file("test_dir"/"queeg", !IO),

    dir.make_directory(Dir1/"foo", MkdirRes, !IO),
    (
        MkdirRes = ok,
        io.write_string(
"Error: creating directory with same name as ordinary file succeeded.\n", !IO)
    ;
        MkdirRes = error(_),
        io.write_string("creating directory with same name " ++
            "as ordinary file failed (as expected).\n", !IO)
    ),

    ( if io.have_symlinks then
        test0("making symlink 1", io.make_symlink("baz", Dir1/"bar"), !IO),
        test0("making symlink 2", io.make_symlink("d1", "test_dir"/"d3"), !IO),

        % Make a loop.
        test0("making symlink 3",
            io.make_symlink(dir.parent_directory, Dir1/"parent"), !IO),

        test1("following symlink",
            io.read_symlink(Dir1/"bar"), LinkTarget, !IO),
        io.write_string(Dir1/"bar", !IO),
        io.write_string(" points to ", !IO),
        io.write_string(LinkTarget, !IO),
        io.nl(!IO),

        test1("file_type 3", io.file_type(no, Dir1/"bar"), Type3, !IO),
        io.write_string("type of ", !IO),
        io.write_string(Dir1/"bar", !IO),
        io.write_string(" is ", !IO),
        io.write_line(Type3, !IO)
    else
        io.write_string("symlinks not available on this platform\n", !IO)
    ),

    testp("dir.foldl2",
        dir.foldl2(list_files, "test_dir", []), TestDirFiles, !IO),
    io.write_string("Files in test_dir:\n", !IO),
    io.write_list(sort(TestDirFiles), ", ", io.write_string, !IO),
    io.nl(!IO),

    testp("dir.recursive_foldl2 (no symlinks)",
        dir.recursive_foldl2(list_files, "test_dir", no, []),
        NoFollowFiles, !IO),
    io.write_string(
        "Files in test_dir (recursive, not following symlinks):\n", !IO),
    io.write_list(sort(NoFollowFiles), ", ", io.write_string, !IO),
    io.nl(!IO),

    testp("dir.recursive_foldl2 (symlinks)",
        dir.recursive_foldl2(list_files, "test_dir", yes, []),
        FollowFiles, !IO),
    io.write_string(
        "Files in test_dir (recursive, following symlinks:\n", !IO),
    io.write_list(sort(FollowFiles), ", ", io.write_string, !IO),
    io.nl(!IO),

    dir.recursive_foldl2(list_files, "dir_test.m", yes, [], Res, !IO),
    (
        Res = ok(_),
        io.write_string("Error: dir.recursive_foldl2(list_files, " ++
            """dir_test.m"", ...) succeeded.\n", !IO)
    ;
        Res = error(_, _),
        io.write_string("dir.recursive_foldl2(list_files, " ++
            """dir_test.m"", ...) failed as expected.\n", !IO)
    ).

:- type test0 == pred(io.res, io, io).
:- inst test0 == (pred(out, di, uo) is det).

:- pred test0(string::in, test0::in(test0), io::di, io::uo) is det.

test0(Msg, P, !IO) :-
    P(Res, !IO),
    (
        Res = ok,
        io.write_string(Msg, !IO),
        io.write_string(" succeeded\n", !IO)
    ;
        Res = error(Error),
        error(Msg ++ " " ++ io.error_message(Error))
    ).

:- type test1(T) == pred(io.res(T), io, io).
:- inst test1 == (pred(out, di, uo) is det).

:- pred test1(string::in, test1(T)::in(test0), T::out, io::di, io::uo) is det.

test1(Msg, P, T, !IO) :-
    P(Res, !IO),
    (
        Res = ok(T),
        io.write_string(Msg, !IO),
        io.write_string(" succeeded\n", !IO)
    ;
        Res = error(Error),
        error(Msg ++ " " ++ io.error_message(Error))
    ).

:- type testp(T) == pred(io.maybe_partial_res(T), io, io).
:- inst testp == (pred(out, di, uo) is det).

:- pred testp(string::in, testp(T)::in(testp), T::out, io::di, io::uo) is det.

testp(Msg, P, T, !IO) :-
    P(Res, !IO),
    (
        Res = ok(T),
        io.write_string(Msg, !IO),
        io.write_string(" succeeded\n", !IO)
    ;
        Res = error(_, Error),
        error(Msg ++ " " ++ io.error_message(Error))
    ).

:- pred run_test(string::in, io::di, io::uo) is cc_multi.

run_test(PathName, !IO) :-
    test_split_name(PathName, !IO),
    test_dirname(PathName, !IO),
    test_basename(PathName, !IO),
    test_path_name_is_absolute(PathName, !IO),
    test_path_name_is_root_directory(PathName, !IO),
    io.nl(!IO).

:- pred test_split_name(string::in, io::di, io::uo) is cc_multi.

test_split_name(PathName, !IO) :-
    io.format("dir.split_name(""%s"", ",
        [s(PathName)], !IO),
    ( if dir.split_name(PathName, DirName, FileName) then
        io.format("""%s"", ""%s"").\n",
            [s(DirName), s(FileName)], !IO),
        test_make_path_name(DirName, FileName, !IO)
    else
        io.write_string("_, _) failed.\n", !IO)
    ).

:- pred test_dirname(string::in, io::di, io::uo) is det.

test_dirname(PathName, !IO) :-
    io.format("dir.dirname(""%s"") = ""%s"".\n",
        [s(PathName), s(dir.dirname(PathName))], !IO).

:- pred test_basename(string::in, io::di, io::uo) is det.

test_basename(PathName, !IO) :-
    io.write_string("dir.basename(""", !IO),
    io.write_string(PathName, !IO),
    io.write_string(""") = ", !IO),
    ( if BaseName = dir.basename(PathName) then
        io.write_string("""", !IO),
        io.write_string(BaseName, !IO),
        io.write_string(""".\n", !IO)
    else
        io.write_string("_ failed.\n", !IO)
    ).

:- pred test_path_name_is_absolute(string::in, io::di, io::uo) is det.

test_path_name_is_absolute(PathName, !IO) :-
    io.format("dir.path_name_is_absolute(""%s"")",
        [s(PathName)], !IO),
    ( if dir.path_name_is_absolute(PathName) then
        io.write_string(".\n", !IO)
    else
        io.write_string(" failed\n", !IO)
    ).

:- pred test_path_name_is_root_directory(string::in, io::di, io::uo) is det.

test_path_name_is_root_directory(PathName, !IO) :-
    io.format("dir.path_name_is_root_directory(""%s"")",
        [s(PathName)], !IO),
    ( if dir.path_name_is_root_directory(PathName) then
        io.write_string(".\n", !IO)
    else
        io.write_string(" failed\n", !IO)
    ).

:- pred test_make_path_name(string::in, string::in, io::di, io::uo)
    is cc_multi.

test_make_path_name(DirName, FileName, !IO) :-
    io.format("\"%s\"/\"%s\"", [s(DirName), s(FileName)], !IO),
    try((pred(R::out) is det :- R = DirName/FileName), Res),
    (
        Res = succeeded(Path),
        io.format(" = ""%s"".\n", [s(Path)], !IO)
    ;
        Res = exception(Excp),
        io.write_string(" threw exception: ", !IO),
        io.write_line(univ_value(Excp), !IO)
    ).

:- pred touch_file(string::in, io::di, io::uo) is det.

touch_file(FileName, !IO) :-
    test1("touching file", io.open_output(FileName), FileStream, !IO),
    io.close_output(FileStream, !IO).

:- pred list_files `with_type` dir.foldl_pred(list(string))
    `with_inst` dir.foldl_pred.

list_files(DirName, BaseName, _FileType, yes, Files,
        [DirName/BaseName | Files], !IO).
