:- module dir_test.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module bool, dir, exception, list, require, std_util, string.

main -->
	io__write_string("Directory separator is '"),
	io__write_char(dir__directory_separator),
	io__write_string("'.\n"),

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

	io__write_string("checking whether `unwritable' is readable..."),
	io__check_file_accessibility("unwritable", [read], ReadResult),
	io__write(ReadResult),
	io__nl,

	io__check_file_accessibility("unwritable",
		[read, write], WriteResult),
	( { WriteResult = ok } ->
		io__write_string(
		"Error: unwritable file found to be writable\n")
	;
		io__write_string(
		"unwritable file found to be unwritable\n")
	),

	{ Dir1 = "test_dir"/"d1" },
	test0("make_directory", dir__make_directory(Dir1)),
	% Test making a directory that already exists.
	test0("make_directory", dir__make_directory(Dir1)),

	{ Dir2 = "test_dir"/"d2" },
	dir__make_single_directory(Dir2/"d2", Dir2Res),
	(
		{ Dir2Res = ok },
		io__write_string(
"Error: dir.make_single_directory succeeded but parent doesn't exist.\n")
	;
		{ Dir2Res = error(_) },
		io__write_string(
"dir.make_single_directory with non-existent parent failed as expected.\n")
	),

	test0("make_single_directory", dir__make_single_directory(Dir2)),
	test0("make_single_directory 2",
		dir__make_single_directory(Dir2/"d2")),


	test1("file_type", io__file_type(yes, Dir1), Type),
	io__write_string("type of "),
	io__write_string(Dir1),
	io__write_string(" is "),
	io__write(Type),
	io__nl,

	test1("file_type 2", io__file_type(yes, "dir_test.m"), Type2),
	io__write_string("type of "),
	io__write_string("dir_test.m"),
	io__write_string(" is "),
	io__write(Type2),
	io__nl,

	% Create some dummy files
	touch_file(Dir1/"foo"),
	touch_file(Dir1/"baz"),
	touch_file("test_dir"/"quark"),
	touch_file("test_dir"/"queeg"),

	dir__make_directory(Dir1/"foo", MkdirRes),
	(
		{ MkdirRes = ok },
		io__write_string(
"Error: creating directory with same name as ordinary file succeeded.\n")
	;
		{ MkdirRes = error(_) },
		io__write_string(
"creating directory with same name as ordinary file failed (as expected).\n")
	),

	( { io__have_symlinks } ->
		test0("making symlink 1", io__make_symlink("baz", Dir1/"bar")),
		test0("making symlink 2", io__make_symlink("d1",
			"test_dir"/"d3")),

		% Make a loop.
		test0("making symlink 3",
			io__make_symlink(dir__parent_directory, Dir1/"parent")),

		test1("following symlink",
			io__read_symlink(Dir1/"bar"), LinkTarget),
		io__write_string(Dir1/"bar"),
		io__write_string(" points to "),
		io__write_string(LinkTarget),
		io__nl,

		test1("file_type 3", io__file_type(no, Dir1/"bar"), Type3),
		io__write_string("type of "),
		io__write_string(Dir1/"bar"),
		io__write_string(" is "),
		io__write(Type3),
		io__nl
	;
		io__write_string("symlinks not available on this platform\n")
	),

	testp("dir__foldl2",
		dir__foldl2(list_files, "test_dir", []), TestDirFiles),
	io__write_string("Files in test_dir:\n"),
	io__write_list(sort(TestDirFiles), ", ", io__write_string),
	io__nl,

	testp("dir__recursive_foldl2 (no symlinks)",
		dir__recursive_foldl2(list_files, "test_dir", no, []),
		NoFollowFiles),
	io__write_string(
		"Files in test_dir (recursive, not following symlinks):\n"),
	io__write_list(sort(NoFollowFiles), ", ", io__write_string),
	io__nl,

	testp("dir__recursive_foldl2 (symlinks)",
		dir__recursive_foldl2(list_files, "test_dir", yes, []),
		FollowFiles),
	io__write_string(
		"Files in test_dir (recursive, following symlinks:\n"),
	io__write_list(sort(FollowFiles), ", ", io__write_string),
	io__nl,
	
	dir__recursive_foldl2(list_files, "dir_test.m", yes, [], Res),
	(
		{ Res = ok(_) },
		io__write_string(
"Error: dir.recursive_foldl2(list_files, ""dir_test.m"", ...) succeeded.\n")
	;
		{ Res = error(_, _) },
		io__write_string(
"dir.recursive_foldl2(list_files, ""dir_test.m"", ...) failed as expected.\n")
	).

:- type test0 == pred(io__res, io__state, io__state).
:- inst test0 == (pred(out, di, uo) is det).

:- pred test0(string::in, test0::in(test0),
		io__state::di, io__state::uo) is det.

test0(Msg, P) -->
	P(Res),
	(
		{ Res = ok },
		io__write_string(Msg),
		io__write_string(" succeeded\n")
	;
		{ Res = error(Error) },
		{ error(Msg ++ " " ++ io__error_message(Error)) }
	).

:- type test1(T) == pred(io__res(T), io__state, io__state).
:- inst test1 == (pred(out, di, uo) is det).

:- pred test1(string::in, test1(T)::in(test0), T::out,
		io__state::di, io__state::uo) is det.

test1(Msg, P, T) -->
	P(Res),
	(
		{ Res = ok(T) },
		io__write_string(Msg),
		io__write_string(" succeeded\n")
	;
		{ Res = error(Error) },
		{ error(Msg ++ " " ++ io__error_message(Error)) }
	).

:- type testp(T) == pred(io__maybe_partial_res(T), io__state, io__state).
:- inst testp == (pred(out, di, uo) is det).

:- pred testp(string::in, testp(T)::in(testp), T::out,
		io__state::di, io__state::uo) is det.

testp(Msg, P, T) -->
	P(Res),
	(
		{ Res = ok(T) },
		io__write_string(Msg),
		io__write_string(" succeeded\n")
	;
		{ Res = error(_, Error) },
		{ error(Msg ++ " " ++ io__error_message(Error)) }
	).

:- pred run_test(string::in, io__state::di, io__state::uo) is cc_multi.

run_test(PathName) -->
	test_split_name(PathName),
	test_dirname(PathName),
	test_basename(PathName),
	test_path_name_is_absolute(PathName),
	test_path_name_is_root_directory(PathName),
	io__nl.

:- pred test_split_name(string::in, io__state::di, io__state::uo) is cc_multi.

test_split_name(PathName) -->
	io__write_string("dir__split_name("""),
	io__write_string(PathName),
	io__write_string(""", "),
	( { dir__split_name(PathName, DirName, FileName) } ->
		io__write_string(""""),
		io__write_string(DirName),
		io__write_string(""", """),
		io__write_string(FileName),
		io__write_string(""").\n"),
		test_make_path_name(DirName, FileName)
	;
		io__write_string("_, _) failed.\n")
	).

:- pred test_dirname(string::in, io__state::di, io__state::uo) is det.

test_dirname(PathName) -->
	io__write_string("dir__dirname("""),
	io__write_string(PathName),
	io__write_string(""") = """),
	io__write_string(dir__dirname(PathName)),
	io__write_string(""".\n").

:- pred test_basename(string::in, io__state::di, io__state::uo) is det.

test_basename(PathName) -->
	io__write_string("dir__basename("""),
	io__write_string(PathName),
	io__write_string(""") = "),
	( { BaseName = dir__basename(PathName) } ->
		io__write_string(""""),
		io__write_string(BaseName),
		io__write_string(""".\n")
	;
		io__write_string("_ failed.\n")
	).

:- pred test_path_name_is_absolute(string::in,
		io__state::di, io__state::uo) is det.

test_path_name_is_absolute(PathName) -->
	io__write_string("dir__path_name_is_absolute("""),
	io__write_string(PathName),
	io__write_string(""")"),
	( { dir__path_name_is_absolute(PathName) } ->
		io__write_string(".\n")		
	;
		io__write_string(" failed\n")
	).

:- pred test_path_name_is_root_directory(string::in,
		io__state::di, io__state::uo) is det.

test_path_name_is_root_directory(PathName) -->
	io__write_string("dir__path_name_is_root_directory("""),
	io__write_string(PathName),
	io__write_string(""")"),
	( { dir__path_name_is_root_directory(PathName) } ->
		io__write_string(".\n")		
	;
		io__write_string(" failed\n")
	).

:- pred test_make_path_name(string::in, string::in,
		io__state::di, io__state::uo) is cc_multi.

test_make_path_name(DirName, FileName) -->
	io__write_string("\""),
	io__write_string(DirName),
	io__write_string("\"/\""),
	io__write_string(FileName),
	io__write_string("\""),
	{ try((pred(R::out) is det :- R = DirName/FileName), Res) },
	(
		{ Res = succeeded(Path) },
		io__write_string(" = """),
		io__write_string(Path),
		io__write_string(""".\n")
	;
		{ Res = failed },
		{ error("dir./ failed") }
	;
		{ Res = exception(Excp) },
		io__write_string(" threw exception: "),
		io__write(univ_value(Excp)),
		io__nl
	).

:- pred touch_file(string::in, io__state::di, io__state::uo) is det.

touch_file(FileName) -->
	test1("touching file", io__open_output(FileName), FileStream),
	io__close_output(FileStream).

:- pred list_files `with_type` dir__foldl_pred(list(string))
			`with_inst` dir__foldl_pred.

list_files(DirName, BaseName, _FileType, yes,
		Files, [DirName/BaseName | Files], !IO).

