%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests that we recognise that even though the append/3 and +/3
% are assocative, the call to drop/3 will drop different amounts from H
% according to whether we start counting from the start or end,
% so don't introduce accumulator.
%

:- module nonrec.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    io.write_string("p(in, out, out): ", !IO),
    ( if p([[4, 3, 2, 1], [3, 2, 1], [2, 1]], Length, ListA) then
        io.write(Length, !IO),
        io.write_string(" ", !IO),
        io.write_line(ListA, !IO)
    else
        io.write_string("failed\n", !IO)
    ),
    io.write_string("p(in, in, out): ", !IO),
    ( if p([[4, 3, 2, 1], [3, 2, 1], [2, 1]], 2, ListB) then
        io.write_line(ListB, !IO)
    else
        io.write_string("failed\n", !IO)
    ).

:- pred p(list(list(T)), int, list(list(T))).
:- mode p(in, out, out) is semidet.
:- mode p(in, in, out) is semidet.

p([], 0, []).
p([H | T], Length, DroppedList) :-
    p(T, Length0, DroppedList0),
    Length = Length0 + 1,
    list.drop(Length, H, NewHead), % Length or Length0, shouldn't matter.
    append([NewHead], DroppedList0, DroppedList).
