:- module func_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, std_util.

main --> 
	io__write_int(f(7 * 8) + g(9 * 10)),
	io__write_string("\n"),
	io__write_int(test),
	io__write_string("\n"),
	io__write_int(g(f(test))),
	io__write_string("\n"),
	io__write_int(test2),
	io__write_string("\n"),
	io__write_int(g(f(test2))),
	io__write_string("\n").

:- mode test2 == out.

:- func f(int) = int.
:- mode f(in) = test2 is det.

f(X) = Y :- Y = X + 1.

:- func g(int) = int.

g(X) = X + 2.

:- func test = int.
:- mode test = out is det.
:- mode test = in is semidet.

test = 123.

% test type inference

test2 = 456.
