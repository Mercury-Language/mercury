%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: e.m.
% Main author: bromage.

% This source file is hereby placed in the public domain.  -bromage.

% Calculate the base of natural logarithms using lazy evaluation.

% The algorithm is O(N^2) in the number of digits requested.

%-----------------------------------------------------------------------------%

:- module e.
:- interface.
:- import_module io.

:- pred main(io__state :: di, io__state :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, list, require, char, string.

	% Default number of digits displayed (if this is not specified
	% on the command line).
:- func default_digits = int.
default_digits = 1000.

	% Change this for a base other than 10.  Any integer between
	% 2 and 36 makes sense.
:- func base = int.
base = 10.

	% Number of columns on the terminal.
:- func columns = int.
columns = 78.

%-----------------------------------------------------------------------------%

	% This is a simple implementation of an infinite lazy stream.
:- type int_stream --->
		[int | int_stream]
	;	closure((func) = int_stream).

:- inst int_stream =
	bound(
			[ground | int_stream]
		;	closure((func) = is_out is det)
	).

:- mode is_in  :: in(int_stream).
:- mode is_out :: out(int_stream).

%-----------------------------------------------------------------------------%

	% An infinite stream of ones.
:- func ones = (int_stream :: is_out) is det.
ones = [1 | closure((func) = ones)].

	% All the digits of e in one stream.
:- func digits_of_e = (int_stream :: is_out) is det.
digits_of_e = next_digit(ones).

:- func next_digit(int_stream :: is_in) = (int_stream :: is_out) is det.
next_digit(Stream0) = [Digit | closure((func) = next_digit(Stream))] :-
	scale(2, Digit, Stream0, Stream).

:- pred scale(int, int, int_stream, int_stream).
:- mode scale(in, out, is_in, is_out) is det.

scale(C, Digit, closure(Func), Stream) :-
	scale(C, Digit, apply(Func), Stream).
scale(C, Digit, [D | Ds], Stream) :-
	K = base * D,
	KdC = K // C,
	( KdC = (K + base - 1) // C ->
		% We have the next digit.  Construct a closure to
		% generate the rest.

		Digit = KdC,
		Stream = closure((func) = [K rem C + NextDigit | Stream0] :-
				scale(C + 1, NextDigit, Ds, Stream0)
			)
	;
		% We have a carry to factor in, so calculate the next
		% digit eagerly then add it on.

		scale(C + 1, A1, Ds, B1),
		Digit = (K + A1) // C,
		Stream = [(K + A1) rem C | B1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

main -->
	io__command_line_arguments(Args),
	{
		Args = [Arg | _],
		string__to_int(Arg, Digits0)
	->
		Digits = Digits0
	;
		Digits = default_digits
	},
	{ string__int_to_base_string(2, base, BaseString) },
	io__write_strings([BaseString, "."]),
	{ string__length(BaseString, BaseStringLength) },
	main_2(Digits, columns - BaseStringLength - 1, digits_of_e),
	io__nl.

	% Print out digits until we don't have any more.
:- pred main_2(int, int, int_stream, io__state, io__state).
:- mode main_2(in, in, is_in, di, uo) is det.

main_2(Digits, Columns, closure(Func)) -->
	main_2(Digits, Columns, apply(Func)).
main_2(Digits, Columns, [I | Is]) -->
	( { Digits = 0 } ->
		[]
	; { Columns = 0 } ->
		io__nl,
		main_2(Digits, columns, [I | Is])
	;
		{ char__det_int_to_digit(I, Digit) },
		io__write_char(Digit),
		main_2(Digits - 1, Columns - 1, Is)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
