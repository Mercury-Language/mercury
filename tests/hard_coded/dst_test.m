%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% In Melbourne, daylight savings
%   Starts: 2 am EST (Eastern Standard Time) on 26 October 2003
%   Ends:   2 am EST (Eastern Standard Time) on 28 March   2004
% At start of daylight saving period, move clock forward one hour.
% At end of daylight saving period, move clock back one hour.

:- module dst_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe.
:- import_module time.

main(!IO) :-
    difftest(!IO),
    io.nl(!IO),
    local_vs_gm_test(!IO),
    io.nl(!IO).

:- pred difftest(io::di, io::uo) is det.

difftest(!IO) :-
    % Sunday 2003-10-26 01:30:00
    mktime(tm(103, 9, 26, 1, 30, 0, 298, 0, yes(standard_time)),
        BeforeStart, !IO),
    % Sunday 2003-10-26 03:30:00
    mktime(tm(103, 9, 26, 3, 30, 0, 298, 0, yes(daylight_time)),
        AfterStart, !IO),
    % difference should be 1 hour
    ( if difftime(AfterStart, BeforeStart) = 3600.0 then
        io.write_string("start DST succeeded\n", !IO)
    else
        io.write_string("start DST failed\n", !IO)
    ),

    % Sunday 2004-02-28 02:30:00 (occurs twice)
    mktime(tm(104, 2, 28, 2, 30, 0, 87, 0, yes(daylight_time)),
        BeforeEnd, !IO),
    mktime(tm(104, 2, 28, 2, 30, 0, 87, 0, yes(standard_time)),
        AfterEnd, !IO),
    % difference should be 1 hour
    ( if difftime(AfterEnd, BeforeEnd) = 3600.0 then
        io.write_string("end DST succeeded\n", !IO)
    else
        io.write_string("end DST failed\n", !IO)
    ).

:- pred local_vs_gm_test(io::di, io::uo) is det.

local_vs_gm_test(!IO) :-
    local_vs_gm(tm(103, 9, 26, 1, 59, 0, 298, 0, yes(standard_time)), !IO),
    local_vs_gm(tm(103, 9, 26, 3, 0, 0, 298, 0, yes(daylight_time)), !IO),
    local_vs_gm(tm(103, 9, 26, 3, 1, 0, 298, 0, yes(daylight_time)), !IO),
    io.nl(!IO),

    local_vs_gm(tm(104, 2, 28, 1, 59, 0, 87, 0, yes(daylight_time)), !IO),
    local_vs_gm(tm(104, 2, 28, 2, 0, 0, 87, 0, yes(daylight_time)), !IO),
    local_vs_gm(tm(104, 2, 28, 2, 1, 0, 87, 0, yes(daylight_time)), !IO),
    io.nl(!IO),

    local_vs_gm(tm(104, 2, 28, 2, 59, 0, 87, 0, yes(daylight_time)), !IO),
    local_vs_gm(tm(104, 2, 28, 2, 0, 0, 87, 0, yes(standard_time)), !IO),
    local_vs_gm(tm(104, 2, 28, 2, 1, 0, 87, 0, yes(standard_time)), !IO),
    io.nl(!IO),

    local_vs_gm(tm(104, 2, 28, 2, 59, 0, 87, 0, yes(standard_time)), !IO),
    local_vs_gm(tm(104, 2, 28, 3, 0, 0, 87, 0, yes(standard_time)), !IO),
    local_vs_gm(tm(104, 2, 28, 3, 1, 0, 87, 0, yes(standard_time)), !IO).

:- pred local_vs_gm(tm::in, io::di, io::uo) is det.

local_vs_gm(TM, !IO) :-
    mktime(TM, Time, !IO),
    io.write_string("Local:\t", !IO),
    localtime(Time, LocalTM, !IO),
    io.write_string(asctime(LocalTM), !IO),
    io.write_string("GMT:\t", !IO),
    io.write_string(asctime(gmtime(Time)), !IO).
