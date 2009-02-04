:- module calendar_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module calendar.

:- import_module list.
:- import_module string.

main(!IO) :-
    io.write_string("Partial order on durations:\n", !IO),
    test_dur_leq("P1M", "P30D", !IO),           % <>
    test_dur_leq("P1M", "P32D", !IO),           % =<
    test_dur_leq("P3M", "P92D", !IO),           % =<
    test_dur_leq("P1Y", "PT31535000S", !IO),    % >=
    test_dur_leq("P11M", "P1Y", !IO),           % =<
    test_dur_leq("P12M", "P1Y", !IO),           % ==
    test_dur_leq("P1D", "PT24H", !IO),          % ==
    test_dur_leq("PT1S", "PT1M1S", !IO),        % =<
    test_dur_leq("PT1S", "PT1.0001S", !IO),     % =<
    test_dur_leq("-PT1S", "-PT1.0001S", !IO),   % >=
    io.nl(!IO),
    io.write_string("Adding durations to date-times:\n", !IO),
    test_add_dur("1901-12-31 00:00:00", "P1D", !IO),
    test_add_dur("1901-12-31 00:00:00", "P1M", !IO),
    test_add_dur("1901-12-31 00:00:00", "P1Y", !IO),
    test_add_dur("1901-12-31 00:00:00", "PT1H", !IO),
    test_add_dur("1901-12-31 00:00:00", "PT1M", !IO),
    test_add_dur("1901-12-31 00:00:00", "PT1S", !IO),
    test_add_dur("2000-02-28 00:00:00", "P1D", !IO),
    test_add_dur("2001-01-30 00:00:00", "P1M", !IO),
    test_add_dur("2000-01-30 00:00:00", "P1M", !IO),
    test_add_dur("2007-01-01 00:00:00", "PT31536000S", !IO),
    test_add_dur("2008-01-01 00:00:00", "PT31536000S", !IO),
    test_add_dur("2008-03-31 00:00:00", "-P1M", !IO),
    test_add_dur("2007-03-31 00:00:00", "-P1M", !IO),
    test_add_dur("1000-01-01 00:00:00", "-PT1M", !IO),
    test_add_dur("2000-03-01 00:00:00", "-PT1H", !IO),
    test_add_dur("-0001-01-01 00:00:00", "-PT1S", !IO),
    test_add_dur("-0001-01-01 00:00:00.123", "-PT1.123S", !IO),
    test_add_dur("2009-02-28 23:59:59.99", "PT0.01S", !IO),
    test_add_dur("2009-02-28 23:59:59.99", "P1MT0.02S", !IO),
    io.nl(!IO),
    io.write_string("Computing durations:\n", !IO),
    test_diff("2008-01-01 00:00:00", "2200-04-04 04:04:04", !IO),
    test_diff("2008-01-31 00:00:00", "2008-02-29 10:00:00", !IO),
    test_diff("2000-01-31 00:00:00", "2001-01-29 00:00:00", !IO),
    test_diff("2000-02-29 00:00:00", "2001-01-31 00:00:00", !IO),
    test_diff("2000-02-29 22:58:58", "2001-01-31 23:59:59", !IO),
    test_diff("2001-02-28 00:00:00", "2001-03-29 00:00:00", !IO),
    test_diff("2001-02-27 00:00:00", "2001-03-29 00:00:00", !IO),
    test_diff("1975-06-05 12:00:00", "1977-10-09 12:00:00", !IO),
    test_diff("1977-10-09 12:00:00", "1980-01-05 11:11:11", !IO),
    test_diff("1977-10-09 12:00:00", "1980-03-01 12:00:00", !IO),
    test_diff("1977-10-09 12:00:00", "1980-03-01 13:01:01.000007", !IO),
    io.nl(!IO),
    io.write_string("Day of the week:\n", !IO),
    test_day_of_week("2008-01-15 23:59:00", !IO),
    test_day_of_week("2008-01-16 19:08:00", !IO),
    test_day_of_week("1360-04-14 00:00:00", !IO),
    test_day_of_week("1865-02-27 00:00:00", !IO),
    test_day_of_week("1886-02-08 00:00:00", !IO),
    test_day_of_week("1929-10-28 00:00:00", !IO),
    test_day_of_week("0000-12-31 00:00:00", !IO),
    test_day_of_week("0001-01-01 00:00:00", !IO),
    test_day_of_week("-0001-12-31 00:00:00", !IO),
    test_day_of_week("0000-01-01 00:00:00", !IO),
    io.nl(!IO),
    io.write_string("Parse test:\n", !IO),
    io.write_string(duration_to_string(
    	det_duration_from_string("P1Y18M100DT10H15M90.0003S")), !IO),
    io.nl(!IO).

:- pred test_dur_leq(string::in, string::in, io::di, io::uo) is det.

test_dur_leq(Str1, Str2, !IO) :-
    Dur1 = det_duration_from_string(Str1),
    Dur2 = det_duration_from_string(Str2),
    ( duration_leq(Dur1, Dur2), duration_leq(Dur2, Dur1) ->
        RelationStr = " == "
    ; duration_leq(Dur1, Dur2) ->
        RelationStr = " =< "
    ; duration_leq(Dur2, Dur1) ->
        RelationStr = " >= "
    ; 
        RelationStr = " <> "
    ),
    io.format("%s %s %s\n", [s(Str1), s(RelationStr), s(Str2)], !IO).

:- pred test_add_dur(string::in, string::in, io::di, io::uo) is det.

test_add_dur(Date0Str, DurStr, !IO) :-
    Date0 = det_date_from_string(Date0Str),
    Dur = det_duration_from_string(DurStr),
    add_duration(Dur, Date0, Date),
    DateStr = date_to_string(Date),
    io.format("%s + %s = %s\n", [s(Date0Str), s(DurStr), s(DateStr)], !IO).

:- pred test_diff(string::in, string::in, io::di, io::uo) is det.

test_diff(Date1, Date2, !IO) :-
    io.write_string("G: ", !IO),
    test_greedy_diff(Date1, Date2, !IO),
    io.write_string("G: ", !IO),
    test_greedy_diff(Date2, Date1, !IO),
    io.write_string("D: ", !IO),
    test_days_diff(Date1, Date2, !IO),
    io.write_string("D: ", !IO),
    test_days_diff(Date2, Date1, !IO),
    io.nl(!IO).

:- pred test_greedy_diff(string::in, string::in, io::di, io::uo) is det.

test_greedy_diff(Date1Str, Date2Str, !IO) :-
    Date1 = det_date_from_string(Date1Str),
    Date2 = det_date_from_string(Date2Str),
    duration(Date1, Date2) = Dur,
    DurStr = duration_to_string(Dur),
    io.format("%s -> %s = %s", [s(Date1Str), s(Date2Str), s(DurStr)], !IO),
    add_duration(Dur, Date1, Date3),
    ( Date2 = Date3 ->
        io.write_string(" checked ok\n", !IO)
    ;
        io.write_string(" error: " ++ date_to_string(Date3) ++
            "\n", !IO)
    ).

:- pred test_days_diff(string::in, string::in, io::di, io::uo) is det.

test_days_diff(Date1Str, Date2Str, !IO) :-
    Date1 = det_date_from_string(Date1Str),
    Date2 = det_date_from_string(Date2Str),
    Dur = day_duration(Date1, Date2),
    DurStr = duration_to_string(Dur),
    io.format("%s -> %s = %s", [s(Date1Str), s(Date2Str), s(DurStr)], !IO),
    add_duration(Dur, Date1, Date3),
    ( Date2 = Date3 ->
        io.write_string(" checked ok\n", !IO)
    ;
        io.write_string(" error: " ++ date_to_string(Date3) ++
            "\n", !IO)
    ).

:- pred test_day_of_week(string::in, io::di, io::uo) is det.

test_day_of_week(DateStr, !IO) :-
    io.write_string(DateStr ++ " : ", !IO),
    io.write(day_of_week(det_date_from_string(DateStr)), !IO),
    io.nl(!IO).
