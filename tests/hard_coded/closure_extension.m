% This is a test of the code that extends closures.

:- module closure_extension.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list.

main -->
	{ A_1 = append_4([1]) },
	{ call(A_1, [7], [8], [9], R_1_7_8_9) },
	io__write(R_1_7_8_9),
	io__write_string("\n"),

	{ A_1_2 = ho_call_append_3(A_1, [2]) },
	{ call(A_1_2, [8], [9], R_1_2_8_9) },
	io__write(R_1_2_8_9),
	io__write_string("\n"),

	{ A_1_2_3 = ho_call_append_2(A_1_2, [3]) },
	{ call(A_1_2_3, [9], R_1_2_3_9) },
	io__write(R_1_2_3_9),
	io__write_string("\n"),

	{ A_1_2_3_4 = ho_call_append_1(A_1_2_3, [4]) },
	{ call(A_1_2_3_4, R_1_2_3_4) },
	io__write(R_1_2_3_4),
	io__write_string("\n"),

	{ A_12 = append_4([1], [2]) },
	{ call(A_12, [8], [9], R_12_8_9) },
	io__write(R_12_8_9),
	io__write_string("\n"),

	{ A_12_3 = ho_call_append_2(A_12, [3]) },
	{ call(A_12_3, [9], R_12_3_9) },
	io__write(R_12_3_9),
	io__write_string("\n"),

	{ A_12_3_4 = ho_call_append_1(A_12_3, [4]) },
	{ call(A_12_3_4, R_12_3_4) },
	io__write(R_12_3_4),
	io__write_string("\n"),

	{ A_12_34 = ho_call_append_2(A_12, [3], [4]) },
	{ call(A_12_34, R_12_34) },
	io__write(R_12_34),
	io__write_string("\n"),

	{ A_1_234 = ho_call_append_3(A_1, [2], [3], [4]) },
	{ call(A_1_234, R_1_234) },
	io__write(R_1_234),
	io__write_string("\n"),

	{ A_123 = append_4([1], [2], [3]) },
	{ call(A_123, [9], R_123_9) },
	io__write(R_123_9),
	io__write_string("\n"),

	{ A_123_4 = ho_call_append_1(A_123, [4]) },
	{ call(A_123_4, R_123_4) },
	io__write(R_123_4),
	io__write_string("\n"),

	{ A_1234 = append_4([1], [2], [3], [4]) },
	{ call(A_1234, R_1234) },
	io__write(R_1234),
	io__write_string("\n"),

	{ ho_call_append_3(A_1, [7], [8], [9], H_1_7_8_9) },
	io__write(H_1_7_8_9),
	io__write_string("\n"),

	{ ho_call_append_2(A_1_2, [8], [9], H_1_2_8_9) },
	io__write(H_1_2_8_9),
	io__write_string("\n"),

	{ ho_call_append_1(A_1_2_3, [9], H_1_2_3_9) },
	io__write(H_1_2_3_9),
	io__write_string("\n"),

	{ ho_call_append_0(A_1_2_3_4, H_1_2_3_4) },
	io__write(H_1_2_3_4),
	io__write_string("\n"),

	{ ho_call_append_2(A_12, [8], [9], H_12_8_9) },
	io__write(H_12_8_9),
	io__write_string("\n"),

	{ ho_call_append_1(A_12_3, [9], H_12_3_9) },
	io__write(H_12_3_9),
	io__write_string("\n"),

	{ ho_call_append_0(A_12_3_4, H_12_3_4) },
	io__write(H_12_3_4),
	io__write_string("\n"),

	{ ho_call_append_0(A_12_34, H_12_34) },
	io__write(H_12_34),
	io__write_string("\n"),

	{ ho_call_append_0(A_1_234, H_1_234) },
	io__write(H_1_234),
	io__write_string("\n"),

	{ ho_call_append_1(A_123, [9], H_123_9) },
	io__write(H_123_9),
	io__write_string("\n"),

	{ ho_call_append_0(A_123_4, H_123_4) },
	io__write(H_123_4),
	io__write_string("\n"),

	{ ho_call_append_0(A_1234, H_1234) },
	io__write(H_1234),
	io__write_string("\n").

:- pred ho_call_append_0(pred(list(T)), list(T)).
:- mode ho_call_append_0(pred(out) is det, out) is det.

ho_call_append_0(P0, R) :-
	P0(R).

:- pred ho_call_append_1(pred(list(T), list(T)), list(T), list(T)).
:- mode ho_call_append_1(pred(in, out) is det, in, out) is det.

ho_call_append_1(P0, Z, R) :-
	P0(Z, R).

:- pred ho_call_append_2(pred(list(T), list(T), list(T)),
	list(T), list(T), list(T)).
:- mode ho_call_append_2(pred(in, in, out) is det, in, in, out) is det.

ho_call_append_2(P0, Y, Z, R) :-
	P0(Y, Z, R).

:- pred ho_call_append_3(pred(list(T), list(T), list(T), list(T)),
	list(T), list(T), list(T), list(T)).
:- mode ho_call_append_3(pred(in, in, in, out) is det, in, in, in, out) is det.

ho_call_append_3(P0, X, Y, Z, R) :-
	P0(X, Y, Z, R).

:- pred append_4(list(T)::in, list(T)::in, list(T)::in, list(T)::in,
	list(T)::out) is det.

append_4(A, B, C, D, ABCD) :-
	append(A, B, AB),
	append(AB, C, ABC),
	append(ABC, D, ABCD).
