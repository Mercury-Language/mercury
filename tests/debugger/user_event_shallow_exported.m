%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module user_event_shallow_exported.

:- interface.

:- import_module io.
:- import_module list.

:- pred main(io::di, io::uo) is det.

:- pred test(list(int)::in, io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    Data = [1, 2, 3, 4, 5],
    test(Data, !IO).

test(Data, !IO) :-
    event data(Data),
    io.write_line(Data, !IO).
