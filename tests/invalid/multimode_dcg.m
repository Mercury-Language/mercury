:- module multimode_dcg.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	{ In = 42 },
	test0,
	test1(In),
	test1(_Out0),
	test2(In, In),
	test2(In, _Out1),
	test2(_Out2, In),
	test2(_Out3, _Out4).

:- pred test0(io__state, io__state).
:- mode test0(di, uo) is det.
test0 -->
	puts("test0").
	
:- pred test1(int, io__state, io__state).
:- mode test1(in, di, uo) is det.
:- mode test1(out, di, uo) is det.
test1(_::in) -->
	puts("test1(in)").
test1(0::out) -->
	puts("test1(out)").

:- pred test2(int, int, io__state, io__state).
:- mode test2(in, in, di, uo) is det.
:- mode test2(in, out, di, uo) is det.
:- mode test2(out, in, di, uo) is det.
:- mode test2(out, out, di, uo) is det.
test2(_::in, _::in) -->
	puts("test2(in, in)").
test2(_::in, 0::out) -->
	puts("test2(in, out)").
test2(0::out, _::in) -->
	puts("test2(out, in)").
test2(0::out, 0::out) -->
	puts("test2(out, out)").

:- pred puts(string::in, io__state::di, io__state::uo) is det.
puts(S) -->
	io__write_string(S), nl.
