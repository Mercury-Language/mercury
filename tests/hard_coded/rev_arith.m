% A test to see whether backwards arithmetic works.

% Note that currently only + and - can be run backwards;
% running * or // backwards would lead to multiple answers,
% and so we don't support that.

:- module rev_arith.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int, std_util.

main --> 
	{
	3 = 1 + A,
	3 = B + 2,
	C = 1 + 2,
%	10 = 2 * D,
%	10 = E * 5,
%	F = 2 * 5,
	20 = 30 - G,
	20 = H - 10,
	I = 30 - 10
%	15 = 90 // J,
%	15 = K // 6,
%	L = 90 // 6
	},
	io__write_int(A),
	io__write_string("\n"),
	io__write_int(B),
	io__write_string("\n"),
	io__write_int(C),
	io__write_string("\n"),
%	io__write_int(D),
%	io__write_string("\n"),
%	io__write_int(E),
%	io__write_string("\n"),
%	io__write_int(F),
%	io__write_string("\n"),
	io__write_int(G),
	io__write_string("\n"),
	io__write_int(H),
	io__write_string("\n"),
	io__write_int(I),
	io__write_string("\n").
%	io__write_int(J),
%	io__write_string("\n"),
%	io__write_int(K),
%	io__write_string("\n"),
%	io__write_int(L),
%	io__write_string("\n").
