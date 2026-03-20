%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module calendar_basics.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module calendar.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    test_det_int_to_month("det_int_to_month", det_int_to_month, !IO),
    test_det_int_to_month("det_int0_to_month", det_int0_to_month, !IO),
    test_int_to_month("int_to_month",
        (pred(I::in, M::out) is semidet :- int_to_month(I, M)), !IO),
    test_int_to_month("int0_to_month",
        (pred(I::in, M::out) is semidet :- int0_to_month(I, M)), !IO),
    test_month_to_int("month_to_int", month_to_int, !IO),
    test_month_to_int("month_to_int0", month_to_int0, !IO),
    test_days_in_month(!IO),
    test_is_leap_year(!IO).

%---------------------------------------------------------------------------%

:- pred test_det_int_to_month(string::in,
    (func(int) = month)::in, io::di, io::uo) is cc_multi.

test_det_int_to_month(Desc, Func, !IO) :-
    io.format("=== Test %s/2 ===\n\n", [s(Desc)], !IO),
    list.foldl(do_test_det_int_to_month(Desc, Func), ints, !IO),
    io.nl(!IO).

:- pred do_test_det_int_to_month(string::in,
    (func(int) = month)::in, int::in, io::di, io::uo) is cc_multi.

do_test_det_int_to_month(Desc, Func, Int, !IO) :-
    io.format("%s(%d) ==> ", [s(Desc), i(Int)], !IO),
    ( try []
        Month = Func(Int)
    then
        io.format("%s\n", [s(string(Month))], !IO)
    catch_any _ ->
        io.write_string("EXCEPTION\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred test_int_to_month(string::in,
    pred(int, month)::in(pred(in, out) is semidet), io::di, io::uo) is det.

test_int_to_month(Desc, Pred, !IO) :-
    io.format("=== Test %s/2 ===\n\n", [s(Desc)], !IO),
    list.foldl(do_test_int_to_month(Desc, Pred), ints, !IO),
    io.nl(!IO).

:- pred do_test_int_to_month(string::in,
    pred(int, month)::in(pred(in, out) is semidet), int::in,
    io::di, io::uo) is det.

do_test_int_to_month(Desc, Pred, Int, !IO) :-
    io.format("%s(%d) ==> ", [s(Desc), i(Int)], !IO),
    ( if Pred(Int, Month) then
        io.format("%s\n", [s(string(Month))], !IO)
    else
        io.write_string("FAILED\n", !IO)
    ).

:- func ints = list(int).

ints = [
    -1,
    0,
    1,
    2,
    11,
    12,
    13
].

%---------------------------------------------------------------------------%

:- pred test_month_to_int(string::in, (func(month) = int)::in,
    io::di, io::uo) is det.

test_month_to_int(Desc, Func, !IO) :-
    io.format("=== Test %s/1 ===\n\n", [s(Desc)], !IO),
    list.foldl(do_test_month_to_int(Desc, Func), months, !IO),
    io.nl(!IO).

:- pred do_test_month_to_int(string::in, (func(month) = int)::in, month::in,
    io::di, io::uo) is det.

do_test_month_to_int(Desc, Func, Month, !IO) :-
    Int = Func(Month),
    io.format("%s(%s) = %d\n", [s(Desc), s(string(Month)), i(Int)], !IO).

%---------------------------------------------------------------------------%

:- pred test_days_in_month(io::di, io::uo) is det.

test_days_in_month(!IO) :-
    io.write_string("=== Test days_in_month/2 ===\n\n", !IO),
    list.foldl(do_test_days_in_month, [1977, 2000], !IO).

:- pred do_test_days_in_month(year::in, io::di, io::uo) is det.

do_test_days_in_month(Year, !IO) :-
    list.foldl(do_test_days_in_month_2(Year), months, !IO),
    io.nl(!IO).

:- pred do_test_days_in_month_2(year::in, month::in, io::di, io::uo) is det.

do_test_days_in_month_2(Year, Month, !IO) :-
    DaysInMonth = days_in_month(Year, Month),
    io.format("days_in_month(%d, %s) = %d\n",
        [i(Year), s(string(Month)), i(DaysInMonth)], !IO).

%---------------------------------------------------------------------------%

:- pred test_is_leap_year(io::di, io::uo) is det.

test_is_leap_year(!IO) :-
    io.write_string("=== Test is_leap_year/1 ===\n\n", !IO),
    list.foldl(do_test_is_leap_year, test_years, !IO),
    io.nl(!IO).

:- pred do_test_is_leap_year(year::in, io::di, io::uo) is det.

do_test_is_leap_year(Year, !IO) :-
    Desc = ( if is_leap_year(Year) then "leap" else "common" ),
    io.format("Year %d is a %s year.\n", [i(Year), s(Desc)], !IO).

:- func test_years = list(year).

test_years = [
    2000,   % Divisible by 400: leap year.
    1900,   % Divisible by 100, but not by 100: common year.
    2024,   % Divisible by 4, but not by 1000: leap year.
    2023,   % Not divisible by 4: common year.
       0,   % Divisible by 400: leap year.
      -1,   % Not divisible by 4: common year.
      -4,   % Divisible by 4: leap year.
    -100    % Divisible by 100, but not 400: common year.
].

%---------------------------------------------------------------------------%

:- func months = list(month).

months = [
    january,
    february,
    march,
    april,
    may,
    june,
    july,
    august,
    september,
    october,
    november,
    december
].

%---------------------------------------------------------------------------%
:- end_module calendar_basics.
%---------------------------------------------------------------------------%
