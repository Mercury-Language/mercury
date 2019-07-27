% vim: ts=4 sw=4 et ft=mercury

% Test that string.hash and MR_hash_string return the same value.
% Do the same for:
% * string.hash2 and MR_hash_string2,
% * string.hash3 and MR_hash_string3,
% * string.hash4 and MR_hash_string4,
% * string.hash5 and MR_hash_string5,
% * string.hash6 and MR_hash_string6.

:- module string_hash.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module random.
:- import_module require.
:- import_module string.

main(!IO) :-
    MaxLength = 1024,
    random.init(1, RS0),
    test(MaxLength, yes, Succeeded, RS0, _, !IO),
    (
        Succeeded = yes,
        io.write_string("all tests succeeded\n", !IO)
    ;
        Succeeded = no,
        io.write_string("some tests failed\n", !IO)
    ).

:- pred test(int::in, bool::in, bool::out,
    random.supply::mdi, random.supply::muo, io::di, io::uo) is det.

test(Length, !Succeeded, !RS, !IO) :-
    ( Length < 0 ->
        true
    ;
        make_char_list(Length, [], List, !RS),
        string.from_char_list(List, String),

        LibHash1 = string.hash(String),
        RuntimeHash1 = runtime_string_hash(String),
        test_hash("hash1", LibHash1, RuntimeHash1, String, !Succeeded, !IO),

        LibHash2 = string.hash2(String),
        RuntimeHash2 = runtime_string_hash2(String),
        test_hash("hash2", LibHash2, RuntimeHash2, String, !Succeeded, !IO),

        LibHash3 = string.hash3(String),
        RuntimeHash3 = runtime_string_hash3(String),
        test_hash("hash3", LibHash3, RuntimeHash3, String, !Succeeded, !IO),

        LibHash4 = string.hash4(String),
        RuntimeHash4 = runtime_string_hash4(String),
        test_hash("hash4", LibHash4, RuntimeHash4, String, !Succeeded, !IO),

        LibHash5 = string.hash5(String),
        RuntimeHash5 = runtime_string_hash5(String),
        test_hash("hash5", LibHash5, RuntimeHash5, String, !Succeeded, !IO),

        LibHash6 = string.hash6(String),
        RuntimeHash6 = runtime_string_hash6(String),
        test_hash("hash6", LibHash6, RuntimeHash6, String, !Succeeded, !IO),

        test(Length - 1, !Succeeded, !RS, !IO)
    ).

:- pred make_char_list(int::in, list(char)::in, list(char)::out,
    random.supply::mdi, random.supply::muo) is det.

make_char_list(Length, !List, !RS) :-
    ( Length = 0 ->
        true
    ;
        rand_char(Char, !RS),
        !:List = [Char | !.List],
        make_char_list(Length - 1, !List, !RS)
    ).

:- pred rand_char(char::out, random.supply::mdi, random.supply::muo) is det.

rand_char(Char, !RS) :-
    random.random(Rand, !RS),
    % U+0001..U+10ffff (avoid null character).
    Int = 1 + (Rand `mod` char.max_char_value),
    char.det_from_int(Int, Char).

:- pred test_hash(string::in, int::in, int::in, string::in,
    bool::in, bool::out, io::di, io::uo) is det.

test_hash(HashName, LibHash, RuntimeHash, String, !Succeeded, !IO) :-
    ( LibHash = RuntimeHash ->
        true
    ;
        !:Succeeded = no,
        io.write_string("failed " ++ HashName ++ ": runtime ", !IO),
        io.write_int(RuntimeHash, !IO),
        io.write_string(", library ", !IO),
        io.write_int(LibHash, !IO),
        io.write_string(": """, !IO),
        io.write_string(String, !IO),
        io.write_string("""\n", !IO)
    ).

:- pragma foreign_decl("C", "#include ""mercury_string.h""").

:- func runtime_string_hash(string) = int.

:- pragma foreign_proc("C",
    runtime_string_hash(StringArg::in) = (Hash::out),
    [promise_pure, will_not_call_mercury],
"
    Hash = MR_hash_string(StringArg);
").

:- func runtime_string_hash2(string) = int.

:- pragma foreign_proc("C",
    runtime_string_hash2(StringArg::in) = (Hash::out),
    [promise_pure, will_not_call_mercury],
"
    Hash = MR_hash_string2(StringArg);
").

:- func runtime_string_hash3(string) = int.

:- pragma foreign_proc("C",
    runtime_string_hash3(StringArg::in) = (Hash::out),
    [promise_pure, will_not_call_mercury],
"
    Hash = MR_hash_string3(StringArg);
").

:- func runtime_string_hash4(string) = int.

:- pragma foreign_proc("C",
    runtime_string_hash4(StringArg::in) = (Hash::out),
    [promise_pure, will_not_call_mercury],
"
    Hash = MR_hash_string4(StringArg);
").

:- func runtime_string_hash5(string) = int.

:- pragma foreign_proc("C",
    runtime_string_hash5(StringArg::in) = (Hash::out),
    [promise_pure, will_not_call_mercury],
"
    Hash = MR_hash_string5(StringArg);
").

:- func runtime_string_hash6(string) = int.

:- pragma foreign_proc("C",
    runtime_string_hash6(StringArg::in) = (Hash::out),
    [promise_pure, will_not_call_mercury],
"
    Hash = MR_hash_string6(StringArg);
").
