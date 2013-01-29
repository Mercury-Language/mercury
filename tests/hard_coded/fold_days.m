:- module fold_days.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module calendar.
:- import_module int.
:- import_module string.

main(!IO) :-

    % Test 1: 1900 was not a leap year.
    Date1A = det_init_date(1900, february, 25, 0, 0, 0, 0),
    Date1B = det_init_date(1900, march, 1, 0, 0, 0, 0),
    io.write_string("Test 1:\n", !IO),
    foldl_days(write_date, Date1A, Date1B, !IO),
    io.nl(!IO),

    % Test 2: 2000 was a leap year.
    Date2A = det_init_date(2000, february, 25, 0, 0, 0, 0),
    Date2B = det_init_date(2000, march, 1, 0, 0, 0, 0),
    io.write_string("Test 2:\n", !IO),
    foldl_days(write_date, Date2A, Date2B, !IO),
    io.nl(!IO),

    % Test 3: 1977 was not a leap year.
    Date3A = det_init_date(1977, february, 25, 0, 0, 0, 0),
    Date3B = det_init_date(1977, march, 1, 0, 0, 0, 0),
    io.write_string("Test 3:\n", !IO),
    foldl_days(write_date, Date3A, Date3B, !IO),
    io.nl(!IO),
 
    % Test 4: calendar dates of start and end are the same,
    % time of start is less than that of the end.
    Date4A = det_init_date(1977, february, 17, 0, 0, 0, 0),
    Date4B = det_init_date(1977, february, 17, 1, 0, 0, 0),
    io.write_string("Test 4:\n", !IO),
    foldl_days(write_date, Date4A, Date4B, !IO),
    io.nl(!IO),
    
    % Test 5: calendar dates of start and end are the same,
    % time of start is equal to that of the end.
    Date5A = det_init_date(1977, february, 17, 0, 0, 0, 0),
    Date5B = det_init_date(1977, february, 17, 0, 0, 0, 0),
    io.write_string("Test 5:\n", !IO),
    foldl_days(write_date, Date5A, Date5B, !IO),
    io.nl(!IO),

    % Test 6: calendar dates of start and end are the same,
    % time of the start is greater than that of the end. 
    Date6A = det_init_date(1977, february, 17, 1, 0, 0, 0),
    Date6B = det_init_date(1977, february, 17, 0, 0, 0, 0),
    io.write_string("Test 6:\n", !IO),
    foldl_days(write_date, Date6A, Date6B, !IO),
    io.nl(!IO).
 
:- pred write_date(date::in, io::di, io::uo) is det.

write_date(Date, !IO) :-
    Str = date_to_string(Date),
    io.write_string(Str ++ "\n", !IO). 
