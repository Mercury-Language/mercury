% Test that string__hash and MR_hash_string return the same value.

:- module string_hash.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module bool, char, int, list, random, require, string.

main(!IO) :-
	MaxLength = 1024,
	random__init(1, RS0),
	test(MaxLength, yes, Succeeded, RS0, _, !IO),
	( Succeeded = yes ->
		io__write_string("all tests succeeded\n", !IO)
	;
		io__write_string("some tests failed\n", !IO)
	).

:- pred test(int::in, bool::in, bool::out,
	random__supply::mdi, random__supply::muo,
	io__state::di, io__state::uo) is det.

test(Length, !Succeeded, !RS, !IO) :-
	( Length = 0 ->
		true
	;
		make_char_list(Length, [], List, !RS),
		string__from_char_list(List, String),
		LibHash = string__hash(String),
		RuntimeHash = runtime_string_hash(String),
		( LibHash = RuntimeHash ->
			true
		;
			!:Succeeded = no,
			io__write_string("failed: runtime ", !IO),
			io__write_int(RuntimeHash, !IO),
			io__write_string(", library ", !IO),
			io__write_int(LibHash, !IO),
			io__write_string(": """, !IO),
			io__write_string(String, !IO),
			io__write_string("""\n", !IO)
		),
		test(Length - 1, !Succeeded, !RS, !IO)
	).

:- pred make_char_list(int::in, list(char)::in, list(char)::out,
	random__supply::mdi, random__supply::muo) is det.

make_char_list(Length, !List, !RS) :-
	( Length = 0 ->
		true
	;
		rand_char(Char, !RS),
		!:List = [Char | !.List],
		make_char_list(Length - 1, !List, !RS)
	).

:- pred rand_char(char::out, random__supply::mdi, random__supply::muo) is det.

rand_char(Char, !RS) :-
	random__random(Rand, !RS),
	% U+0001..U+10ffff (avoid null character).
	Int = 1 + (Rand `mod` char__max_char_value),
	char__det_from_int(Int, Char).

:- pragma foreign_decl("C", "#include ""mercury_string.h""").

:- func runtime_string_hash(string) = int.

:- pragma foreign_proc("C",
	runtime_string_hash(StringArg::in) = (Hash::out),
	[promise_pure, will_not_call_mercury],
"
	Hash = MR_hash_string(StringArg);
").

