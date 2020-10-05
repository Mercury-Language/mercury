%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% File: ho_type_manip.m
%
% Test case for higher order type manipulation.
%
% Author: trd
%

:- module higher_order_type_manip.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module type_desc.

:- func tryme = int.

:- type container(T)
    --->    container(T).

main(!IO) :-
    io.write_string(type_name(type_of(type_name)), !IO),
    io.write_string("\n", !IO),
    io.write_string(type_name(type_of(type_ctor_and_args)), !IO),
    io.write_string("\n", !IO),
    io.write_string(type_name(type_of(tryme)), !IO),
    io.write_string("\n", !IO),
    io.write_string(type_name(type_of((func) = tryme)), !IO),
    io.write_string("\n", !IO),
    io.write_string(type_name(type_of(container([1, 2, 3]))), !IO),
    io.write_string("\n", !IO),
    io.write_string(type_name(type_of(container(main))), !IO),
    io.write_string("\n", !IO),
    Ctor = type_ctor(type_of(type_name)),
    IntType = type_of(8),
    NewType = det_make_type(Ctor, [IntType, IntType]),
    io.write_string(type_name(NewType), !IO),
    io.write_string("\n", !IO).

tryme = 4.
