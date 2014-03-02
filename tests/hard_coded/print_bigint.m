% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0

% Test io.print with arbitrary precision integers.

:- module print_bigint.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module integer.

main(!IO) :- 
    I = integer.det_from_string("128391829381928390189238901823128931283908192389182903819283901890251908239081290380182031"),
    NegI = -I,
    io.print_line(I, !IO),
    io.print_line(NegI : integer, !IO).
