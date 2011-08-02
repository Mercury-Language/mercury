% vim: ts=4 sw=4 et ft=mercury

% Test that string.hash and MR_hash_string return the same value.
% Do the same for string.hash2 and MR_hash_string2, and for
% string.hash3 and MR_hash_string3.

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
        ( LibHash1 = RuntimeHash1 ->
            true
        ;
            !:Succeeded = no,
            io.write_string("failed hash1: runtime ", !IO),
            io.write_int(RuntimeHash1, !IO),
            io.write_string(", library ", !IO),
            io.write_int(LibHash1, !IO),
            io.write_string(": """, !IO),
            io.write_string(String, !IO),
            io.write_string("""\n", !IO)
        ),

        LibHash2 = string.hash2(String),
        RuntimeHash2 = runtime_string_hash2(String),
        ( LibHash2 = RuntimeHash2 ->
            true
        ;
            !:Succeeded = no,
            io.write_string("failed hash2: runtime ", !IO),
            io.write_int(RuntimeHash2, !IO),
            io.write_string(", library ", !IO),
            io.write_int(LibHash2, !IO),
            io.write_string(": """, !IO),
            io.write_string(String, !IO),
            io.write_string("""\n", !IO)
        ),

        LibHash3 = string.hash3(String),
        RuntimeHash3 = runtime_string_hash3(String),
        ( LibHash3 = RuntimeHash3 ->
            true
        ;
            !:Succeeded = no,
            io.write_string("failed hash3: runtime ", !IO),
            io.write_int(RuntimeHash3, !IO),
            io.write_string(", library ", !IO),
            io.write_int(LibHash3, !IO),
            io.write_string(": """, !IO),
            io.write_string(String, !IO),
            io.write_string("""\n", !IO)
        ),

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
