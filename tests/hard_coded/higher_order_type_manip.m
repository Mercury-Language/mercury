%
% File: ho_type_manip.m
%
% Test case for higher order type manipulation.
% 
% Author: trd

:- module higher_order_type_manip.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util, list.

:- func tryme = int.

:- type container(T) --->		container(T).

main -->
	io__write_string(type_name(type_of(type_name))),
	io__write_string("\n"),
	io__write_string(type_name(type_of(type_ctor_and_args))),
	io__write_string("\n"),
	io__write_string(type_name(type_of(tryme))),
	io__write_string("\n"),
	io__write_string(type_name(type_of((func) = tryme))),
	io__write_string("\n"),
	io__write_string(type_name(type_of(container([1,2,3])))),
	io__write_string("\n"),
	io__write_string(type_name(type_of(container(main)))),
	io__write_string("\n"),
	{ Ctor = type_ctor(type_of(type_name)) },
	{ IntType = type_of(8) },
	{ NewType = det_make_type(Ctor, [IntType, IntType]) },
	io__write_string(type_name(NewType)),
	io__write_string("\n").

tryme = 4.
