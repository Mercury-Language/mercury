/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
*/

% enum tags

:- module simple01.

:- interface.

:- import_module int, io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is det.

:- func temp(colour) = int.
:- mode temp(in) = out is det.

:- func temp2(int) = int.
:- mode temp2(in) = out is det.

:- pred ccode(int, int, int, int, int).
:- mode ccode(in, in, in, out, out) is det.

:- type colour
	--->	red
	;	green
	;	blue
	.

:- implementation.

:- import_module bool.
:- pred forcebool(bool::out) is det.

forcebool(X) :- X = yes.

:- pragma c_code(
	ccode(A::in, B::in, C::in, X::out, Y::out),
	[may_call_mercury],
	"X = A+B; Y = A+B+C;"
	).

temp(X) = Y :- 
	(	X = red,	ccode(1, 2, 3, Q, R), Y = Q+R
	;	X = green,	Y = temp2(3)
	;	X = blue,	Y = temp2(5)
	).

temp2(X) = X + 1.

main -->
	{
		R = temp(red),
		G = temp(green),
		B = temp(blue)
		
	},
	io__write_int(R),
	io__write_int(G),
	io__write_int(B),
	{ forcebool(X) },
	io__write(X).

