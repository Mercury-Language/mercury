%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Testing whether lco detects attempts to apply it to sub-word arguments
% (which do not have an address, which means that lco should not be
% applied to them).
%

:- module lco_pack_args_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.

:- type thing
    --->    thing(enum, enum, thing, enum, enum)    % 5 words -> 3 words
    ;       nil.

:- type enum
    --->    enum1
    ;       enum2
    ;       enum3
    ;       enum4
    ;       enum5.

:- pred gen(list(enum)::in, enum::out, thing::out) is det.

gen([], enum5, nil).
gen([E | Es], enum4, T) :-
    gen(Es, ETail, Tail),
    T = thing(E, ETail, Tail, E, E).

main(!IO) :-
    gen([enum1, enum2, enum3], _, T),
    io.write(T, !IO),
    io.nl(!IO).
