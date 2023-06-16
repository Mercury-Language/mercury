%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module lco_double.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pair.

:- type thing
    --->    thing(enum, enum, thing, float)
    ;       nil.

:- type enum
    --->    enum1
    ;       enum2
    ;       enum3.

:- pred gen(list(pair(enum, float))::in, thing::out) is det.

gen([], nil).
gen([E - F | Xs], T) :-
    gen(Xs, Tail),
    T = thing(E, E, Tail, F).

main(!IO) :-
    gen([enum1 - 1.2345, enum2 - 2.3456, enum3 - 3.4567], T),
    io.write_line(T, !IO).
