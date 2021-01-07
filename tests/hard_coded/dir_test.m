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

main -->
    io.write_string("Directory separator is '"),
    io.write_char(dir.directory_separator),
    io.write_string("'.\n"),

    run_test("\\\\server\\share\\foo"),
    run_test("\\\\server\\share"),
    run_test("\\\\server\\share\\\\"),
    run_test("C:\\foo"),
    run_test("C:\\foo\\"),
    run_test("C:\\"),
    run_test("C:"),
    run_test("\\"),
    run_test(""),
    run_test("foo\\\\bar\\"),
    run_test("foo\\bar\\"),
    run_test("foo"),

    run_test("/foo"),
    run_test("/foo//bar///"),
    run_test("//foo//bar/"),
    run_test("//foo//"),
    run_test("/"),
    run_test("//"),
    run_test("foo/bar"),

    test_make_path_name("C:", "foo"),
    test_make_path_name("C:\\", "foo"),
    test_make_path_name("C:", "C:"),
    test_make_path_name("C:", "C:\\foo"),
    test_make_path_name(".", "/foo"),
    test_make_path_name(".", "\\foo"),
    test_make_path_name("foo", "bar/baz"),
    test_make_path_name("foo/", "bar/baz"),

    io.write_string("checking whether `unwritable' is readable..."),
    io.check_file_accessibility("unwritable", [read], ReadResult),
    io.write_line(ReadResult),

    io.check_file_accessibility("unwritable",
        [read, write], WriteResult),
    ( { WriteResult = ok } ->
        io.write_string(
        "Error: unwritable file found to be writable\n")
    ;
        io.write_string(
        "unwritable file found to be unwritable\n")
    ),

    % Execute permissions are not handled correctly on all platforms so
    % just check that it doesn't crash.
    io.check_file_accessibility("unwritable",
        [execute], _ExecuteResult),

    dir.current_directory(CwdResult),
    (
        { CwdResult = ok(Cwd) },
        io.write_string("current_directory succeeded: "),
        io.write_string(dir.det_basename(Cwd)),
        io.nl
    ;
        { CwdResult = error(Error) },
        io.write_string("current_directory failed: "),
        io.write_string(io.error_message(Error)),
        io.nl
    ),

    { Dir1 = "test_dir"/"d1" },
    test0("make_directory", dir.make_directory(Dir1)),
    % Test making a directory that already exists.
    test0("make_directory", dir.make_directory(Dir1)),

    { Dir2 = "test_dir"/"d2" },
    dir.make_single_directory(Dir2/"d2", Dir2Res),
    (
        { Dir2Res = ok },
        io.write_string(
"Error: dir.make_single_directory succeeded but parent doesn't exist.\n")
    ;
        { Dir2Res = error(_) },
        io.write_string(
"dir.make_single_directory with non-existent parent failed as expected.\n")
    ),

    test0("make_single_directory", dir.make_single_directory(Dir2)),
    test0("make_single_directory 2",
        dir.make_single_directory(Dir2/"d2")),

    test1("file_type", io.file_type(yes, Dir1), Type),
    io.write_string("type of "),
    io.write_string(Dir1),
    io.write_string(" is "),
    io.write_line(Type),

    test1("file_type 2", io.file_type(yes, "dir_test.m"), Type2),
    io.write_string("type of "),
    io.write_string("dir_test.m"),
    io.write_string(" is "),
    io.write_line(Type2),

    % Create some dummy files
    touch_file(Dir1/"foo"),
    touch_file(Dir1/"baz"),
    touch_file("test_dir"/"quark"),
    touch_file("test_dir"/"queeg"),

    dir.make_directory(Dir1/"foo", MkdirRes),
    (
        { MkdirRes = ok },
        io.write_string(
"Error: creating directory with same name as ordinary file succeeded.\n")
    ;
        { MkdirRes = error(_) },
        io.write_string(
"creating directory with same name as ordinary file failed (as expected).\n")
    ),

    ( { io.have_symlinks } ->
        test0("making symlink 1", io.make_symlink("baz", Dir1/"bar")),
        test0("making symlink 2", io.make_symlink("d1",
            "test_dir"/"d3")),

        % Make a loop.
        test0("making symlink 3",
            io.make_symlink(dir.parent_directory, Dir1/"parent")),

        test1("following symlink",
            io.read_symlink(Dir1/"bar"), LinkTarget),
        io.write_string(Dir1/"bar"),
        io.write_string(" points to "),
        io.write_string(LinkTarget),
        io.nl,

        test1("file_type 3", io.file_type(no, Dir1/"bar"), Type3),
        io.write_string("type of "),
        io.write_string(Dir1/"bar"),
        io.write_string(" is "),
        io.write_line(Type3)
    ;
        io.write_string("symlinks not available on this platform\n")
    ),

    testp("dir.foldl2",
        dir.foldl2(list_files, "test_dir", []), TestDirFiles),
    io.write_string("Files in test_dir:\n"),
    io.write_list(sort(TestDirFiles), ", ", io.write_string),
    io.nl,

    testp("dir.recursive_foldl2 (no symlinks)",
        dir.recursive_foldl2(list_files, "test_dir", no, []),
        NoFollowFiles),
    io.write_string(
        "Files in test_dir (recursive, not following symlinks):\n"),
    io.write_list(sort(NoFollowFiles), ", ", io.write_string),
    io.nl,

    testp("dir.recursive_foldl2 (symlinks)",
        dir.recursive_foldl2(list_files, "test_dir", yes, []),
        FollowFiles),
    io.write_string(
        "Files in test_dir (recursive, following symlinks:\n"),
    io.write_list(sort(FollowFiles), ", ", io.write_string),
    io.nl,

    dir.recursive_foldl2(list_files, "dir_test.m", yes, [], Res),
    (
        { Res = ok(_) },
        io.write_string(
"Error: dir.recursive_foldl2(list_files, ""dir_test.m"", ...) succeeded.\n")
    ;
        { Res = error(_, _) },
        io.write_string(
"dir.recursive_foldl2(list_files, ""dir_test.m"", ...) failed as expected.\n")
    ).

:- type test0 == pred(io.res, io, io).
:- inst test0 == (pred(out, di, uo) is det).

:- pred test0(string::in, test0::in(test0), io::di, io::uo) is det.

test0(Msg, P) -->
    P(Res),
    (
        { Res = ok },
        io.write_string(Msg),
        io.write_string(" succeeded\n")
    ;
        { Res = error(Error) },
        { error(Msg ++ " " ++ io.error_message(Error)) }
    ).

:- type test1(T) == pred(io.res(T), io, io).
:- inst test1 == (pred(out, di, uo) is det).

:- pred test1(string::in, test1(T)::in(test0), T::out, io::di, io::uo) is det.

test1(Msg, P, T) -->
    P(Res),
    (
        { Res = ok(T) },
        io.write_string(Msg),
        io.write_string(" succeeded\n")
    ;
        { Res = error(Error) },
        { error(Msg ++ " " ++ io.error_message(Error)) }
    ).

:- type testp(T) == pred(io.maybe_partial_res(T), io, io).
:- inst testp == (pred(out, di, uo) is det).

:- pred testp(string::in, testp(T)::in(testp), T::out, io::di, io::uo) is det.

testp(Msg, P, T) -->
    P(Res),
    (
        { Res = ok(T) },
        io.write_string(Msg),
        io.write_string(" succeeded\n")
    ;
        { Res = error(_, Error) },
        { error(Msg ++ " " ++ io.error_message(Error)) }
    ).

:- pred run_test(string::in, io::di, io::uo) is cc_multi.

run_test(PathName) -->
    test_split_name(PathName),
    test_dirname(PathName),
    test_basename(PathName),
    test_path_name_is_absolute(PathName),
    test_path_name_is_root_directory(PathName),
    io.nl.

:- pred test_split_name(string::in, io::di, io::uo) is cc_multi.

test_split_name(PathName) -->
    io.write_string("dir.split_name("""),
    io.write_string(PathName),
    io.write_string(""", "),
    ( { dir.split_name(PathName, DirName, FileName) } ->
        io.write_string(""""),
        io.write_string(DirName),
        io.write_string(""", """),
        io.write_string(FileName),
        io.write_string(""").\n"),
        test_make_path_name(DirName, FileName)
    ;
        io.write_string("_, _) failed.\n")
    ).

:- pred test_dirname(string::in, io::di, io::uo) is det.

test_dirname(PathName) -->
    io.write_string("dir.dirname("""),
    io.write_string(PathName),
    io.write_string(""") = """),
    io.write_string(dir.dirname(PathName)),
    io.write_string(""".\n").

:- pred test_basename(string::in, io::di, io::uo) is det.

test_basename(PathName) -->
    io.write_string("dir.basename("""),
    io.write_string(PathName),
    io.write_string(""") = "),
    ( { BaseName = dir.basename(PathName) } ->
        io.write_string(""""),
        io.write_string(BaseName),
        io.write_string(""".\n")
    ;
        io.write_string("_ failed.\n")
    ).

:- pred test_path_name_is_absolute(string::in,
        io::di, io::uo) is det.

test_path_name_is_absolute(PathName) -->
    io.write_string("dir.path_name_is_absolute("""),
    io.write_string(PathName),
    io.write_string(""")"),
    ( { dir.path_name_is_absolute(PathName) } ->
        io.write_string(".\n")
    ;
        io.write_string(" failed\n")
    ).

:- pred test_path_name_is_root_directory(string::in, io::di, io::uo) is det.

test_path_name_is_root_directory(PathName) -->
    io.write_string("dir.path_name_is_root_directory("""),
    io.write_string(PathName),
    io.write_string(""")"),
    ( { dir.path_name_is_root_directory(PathName) } ->
        io.write_string(".\n")
    ;
        io.write_string(" failed\n")
    ).

:- pred test_make_path_name(string::in, string::in, io::di, io::uo)
    is cc_multi.

test_make_path_name(DirName, FileName) -->
    io.write_string("\""),
    io.write_string(DirName),
    io.write_string("\"/\""),
    io.write_string(FileName),
    io.write_string("\""),
    { try((pred(R::out) is det :- R = DirName/FileName), Res) },
    (
        { Res = succeeded(Path) },
        io.write_string(" = """),
        io.write_string(Path),
        io.write_string(""".\n")
    ;
        { Res = failed },
        { error("dir./ failed") }
    ;
        { Res = exception(Excp) },
        io.write_string(" threw exception: "),
        io.write(univ_value(Excp)),
        io.nl
    ).

:- pred touch_file(string::in, io::di, io::uo) is det.

touch_file(FileName) -->
    test1("touching file", io.open_output(FileName), FileStream),
    io.close_output(FileStream).

:- pred list_files `with_type` dir.foldl_pred(list(string))
    `with_inst` dir.foldl_pred.

list_files(DirName, BaseName, _FileType, yes, Files,
        [DirName/BaseName | Files], !IO).
