%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that uint64 values too large to be int64 values can be stored
% in static structures.
%

:- module uint64_ground_term.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module uint64.

:- type mono
    --->    mono(uint64).

:- type list_of_mono
    --->    []
    ;       [mono | list_of_mono].

:- type poly(T)
    --->    poly(T).

main(!IO) :-
    One =   1229782938247303441u64, % 0x_1111_1111_1111_1111_u64 
    Nine =  9u64 * One,             % 0x_9999_9999_9999_9999_u64
    M1 = [
        mono(One), mono(One), mono(One), mono(One), mono(One)
    ] : list_of_mono,
    M9 = [
        mono(Nine), mono(Nine), mono(Nine), mono(Nine), mono(Nine)
    ] : list_of_mono,
    P1 =[
        poly(One), poly(One), poly(One), poly(One), poly(One)
    ],
    P9 =[
        poly(Nine), poly(Nine), poly(Nine), poly(Nine), poly(Nine)
    ],
    io.write(M1, !IO),
    io.nl(!IO),
    io.write(M9, !IO),
    io.nl(!IO),
    io.write(P1, !IO),
    io.nl(!IO),
    io.write(P9, !IO),
    io.nl(!IO).
