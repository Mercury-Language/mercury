%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test io.print with dates and durations.

:- module print_date.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module calendar.

main(!IO) :-
    Date1 = det_init_date(2014, december, 19, 16, 5, 0, 0),
    Date2 = det_init_date(2014, december, 25, 0, 0, 0, 0),
    io.print_line(Date1, !IO),
    Duration = duration(Date1, Date2),
    io.print_line(Duration, !IO).
