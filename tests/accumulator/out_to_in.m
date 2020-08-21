%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Tests two things
%   * Recognise that Length will hold the same value no
%     matter if we process left to right or right to left.
%   * Realise that OutInt will always hold 10 so change its
%     mode from out to in and set it to be 10
%
% Used to work, doesn't work with the unfold/fold transformation.
%

:- module out_to_in.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

main(!IO) :-
    io.write_string("q: ", !IO),
    ( if q([[4, 3, 2, 1], [3, 2, 1], [2, 1]], 2, List, Out) then
        io.write(List, !IO),
        io.write_string(" ", !IO),
        io.write_line(Out, !IO)
    else
        io.write_string("failed\n", !IO)
    ).

:- pred q(list(list(T))::in, int::in, list(list(T))::out, int::out) is semidet.

q([], _, [], 10).
q([H | T], Length, DroppedList, OutInt) :-
    Length0 = 1,
    q(T, Length0, DroppedList0, OutInt),
    _X = OutInt + Length,
    list.drop(Length, H, NewHead),
    append(DroppedList0, [NewHead], DroppedList).
