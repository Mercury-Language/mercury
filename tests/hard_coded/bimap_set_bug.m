%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module bimap_set_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bimap.
:- import_module pprint.

:- type element
    --->    hydrogen
    ;       helium
    ;       lithium
    ;       beryllium
    ;       boron
    ;       carbon
    ;       nitrogen
    ;       oxygen
    ;       fluorine
    ;       neon
    ;       sodium.

:- type symbol
    --->    h
    ;       he
    ;       li
    ;       be
    ;       b
    ;       c
    ;       n
    ;       o
    ;       f
    ;       ne
    ;       na.

main(!IO) :-
    some [!Bimap] (
        bimap.init(!:Bimap),
        bimap.set(hydrogen,   na,  !Bimap),
        bimap.set(helium,     he,  !Bimap),
        bimap.set(lithium,    li,  !Bimap),
        bimap.set(beryllium,  be,  !Bimap),
        bimap.set(hydrogen,   na,  !Bimap),
        bimap.set(hydrogen,   h,   !Bimap),
        bimap.set(sodium,     h,   !Bimap),
        bimap.set(sodium,     na,  !Bimap),
        bimap.set(hydrogen,   h,   !Bimap),
        Forward = to_doc(bimap.forward_map(!.Bimap)),
        Reverse = to_doc(bimap.reverse_map(!.Bimap)),
        io.write_string("Forward map is:\n", !IO),
        pprint.write(4, Forward, !IO),
        io.nl(!IO),
        io.write_string("Reverse map is:\n", !IO),
        pprint.write(4, Reverse, !IO),
        io.nl(!IO)
    ).
