% This test checks that a higher order func type with inst ground is 
% able to be treated as a though it has the default function mode.

:- module ho_func_default_inst.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module map.

main -->
	{ Map = map },
	{ F1 = Map ^ det_elem(1) },
	{ F2 = Map ^ det_elem(2) },
	io__write_int(F1(1)),
	io__nl,
	write_func(F2).

:- type t == (func(int) = int).

:- inst one == bound(1).

:- func map = map(int, t).

map = ((map__init
		^ elem(1) := foo1)
		^ elem(2) := foo2)
		^ elem(3) := foo3.

:- pred write_func(t, io, io) is det.
:- mode write_func(func(in) = out is det, di, uo) is det.

write_func(F) -->
	io__write_int(F(1)),
	io__nl.

:- func foo1(int) = int.

foo1(_) = 1.
	
:- func foo2(int) = int.
:- mode foo2(in) = uo is det.

foo2(_) = 2.

:- func foo3(int) = int.
:- mode foo3(in) = out(one) is det.

foo3(_) = 1.
