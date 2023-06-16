%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test whether lco works when taking the address of a float, and when
% taking the address of more than one word of a memory cell.
%

:- module lco_pack_args_3.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type thing
    --->    thing(enum, enum, float, thing, enum, enum)
    ;       nil.

:- type enum
    --->    enum1
    ;       enum2
    ;       enum3
    ;       enum4
    ;       enum5.

main(!IO) :-
    gen([enum1, enum2, enum3], _, T),
    io.write_line(T, !IO).

:- pred gen(list(enum)::in, float::out, thing::out) is det.

gen([], 42.0, nil).
gen([E | Es], 42.5, T) :-
    gen(Es, FTail, Tail),
    T = thing(E, E, FTail, Tail, E, E).
