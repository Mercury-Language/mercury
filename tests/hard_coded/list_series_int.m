%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module list_series_int.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    io.write_line(1 .. 10, !IO),
    io.write_line(-10 .. 0, !IO),
    io.write_line(2 * 3 .. 5 + 5 - 1, !IO),
    io.write_line(10..2, !IO),
    io.write_line(0..0, !IO),
    io.write_line((1..4) ++ (1..4), !IO).
