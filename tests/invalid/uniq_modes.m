%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module uniq_modes.
:- interface.
:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

main(In, _Out) :-
    io.write("looking for", In, Int1),
    io.nl(Int1, _Int2),
    fail.
main(In, Out) :-
    io.write("not found", In, Int),
    io.nl(Int, Out).
