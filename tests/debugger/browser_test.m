:- module browser_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

:- type big
	--->	big(big, int, big)
	;	small
	;	seq(int, list(big), int).

main(!IO) :-
	% In case we have these files lying around.
	io__remove_file("browser_test.save.1", _, !IO),
	io__remove_file("browser_test.save.2", _, !IO),
	big_data(Data),
	io__print(Data, !IO),
	io__write_string(".\n", !IO),
	list_data(List),
	io__print(List, !IO),
	io__write_string(".\n", !IO),
	print_file("browser_test.save.1", !IO),
	print_file("browser_test.save.2", !IO),
	% Clean up after the test.
	io__remove_file("browser_test.save.1", _, !IO),
	io__remove_file("browser_test.save.2", _, !IO),
	a_func(Data) = Data2,
	write(Data2, !IO),
	nl(!IO).

:- pred big_data(big::out) is det.

big_data(Data) :-
	Data = big(
		big(
			big(
				small,
				1,
				small
			),
			2,
			small
		),
		3,
		big(
			big(
				small,
				4,
				big(
					small,
					5,
					small
				)
			),
			6,
			small
		)
	).

:- pred list_data(big::out) is det.

list_data(Data) :-
	Element1 = big(
			big(
				small,
				1,
				small
			),
			2,
			small
		),
	Element2 = small,
	Element3 = big(
			small,
			4,
			big(
				small,
				5,
				small
			)
		),
	Data = seq(1, [Element1, Element2, Element3], 5).

:- pred print_file(string::in, io::di, io::uo) is det.

print_file(FileName, !IO) :-
	io__open_input(FileName, OpenRes, !IO),
	(
		OpenRes = ok(Stream),
		io__read_file_as_string(Stream, ReadRes, !IO),
		(
			ReadRes = ok(Contents),
			io__write_string(FileName, !IO),
			io__write_string(":\n", !IO),
			io__write_string(Contents, !IO),
			io__write_string("\n", !IO)
		;
			ReadRes = error(_, _),
			io__write_string("read failed\n", !IO)
		)
	;
		OpenRes = error(_),
		io__write_string("open failed\n", !IO)
	).

:- func a_func(big) = big.

a_func(_) = Big :- big_data(Big).
