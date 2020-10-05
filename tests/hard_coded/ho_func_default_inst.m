%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test checks that a higher order func type with inst ground is
% able to be treated as a though it has the default function mode.

:- module ho_func_default_inst.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module map.

main(!IO) :-
    Map = map,
    F1 = Map ^ det_elem(1),
    F2 = Map ^ det_elem(2),
    io.write_int(F1(1), !IO),
    io.nl(!IO),
    write_func(F2, !IO).

:- type t == (func(int) = int).

:- inst one == bound(1).

:- func map = map(int, t).

map = ((map.init
        ^ elem(1) := foo1)
        ^ elem(2) := foo2)
        ^ elem(3) := foo3.

:- pred write_func(t::in(func(in) = out is det), io::di, io::uo) is det.

write_func(F, !IO) :-
    io.write_int(F(1), !IO),
    io.nl(!IO).

:- func foo1(int) = int.

foo1(_) = 1.

:- func foo2(int) = int.
:- mode foo2(in) = uo is det.

foo2(_) = 2.

:- func foo3(int) = int.
:- mode foo3(in) = out(one) is det.

foo3(_) = 1.
