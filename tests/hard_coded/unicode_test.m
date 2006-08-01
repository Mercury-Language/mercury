:- module unicode_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module list.
:- import_module string.

main(!IO) :-
	list.foldl(write_string_as_binary, utf8_strings, !IO),
	io.nl(!IO),
	Str = string.join_list("\n", utf8_strings),
	io.write_string(Str, !IO),
	io.nl(!IO).

:- func utf8_strings = list(string).

utf8_strings = [
	"\u0003",
	"\U00000003",
	"\u0394",  % delta
	"\u03A0",  % pi
	"\uFFFF",
	"\U0010ffff",
	"\U000ABCde",
	"r\u00E9sum\u00E9", % "resume" with accents
	"abc123",
	"\u005cu0041",
	"\x5c\u0041",
	"\x5c\\u0041",
	"\\u0041",
	"u0041"
].

:- pred write_string_as_binary(string::in, io::di, io::uo) is det.

write_string_as_binary(Str, !IO) :-
	Chars = string.to_char_list(Str),
	Ints = list.map(to_int, Chars),
	Bins = list.map(( func(X) = int_to_base_string(X, 2) ), Ints),
	PaddedBins = list.map(( func(S) = pad_left(S, '0', 8) ), Bins),
	Bin = string.join_list(" ", PaddedBins),
	io.write_string(Bin, !IO),
	io.nl(!IO).
