%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Originally written in 1999 by Tomas By <T.By@dcs.shef.ac.uk>
% "Feel free to use this code or parts of it any way you want."
%
% Some portions are Copyright (C) 1999-2007,2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: time.m.
% Main authors: Tomas By <T.By@dcs.shef.ac.uk>, fjh.
% Stability: medium.
% 
% Time functions.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module time.
:- interface.

:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % The `clock_t' type represents times measured in clock ticks.
    % NOTE: the unit used for a value of this type depends on whether it was
    % returned by `time.clock' or `time.times'.  See the comments on these
    % predicates below.
    %
:- type clock_t == int.

    % The `tms' type holds information about the amount of processor
    % time that a process and its child processes have consumed.
    %
:- type tms
    --->    tms(
                clock_t,    % tms_utime: user time
                clock_t,    % tms_stime: system time
                clock_t,    % tms_cutime: user time of children
                clock_t     % tms_cstime: system time of children
            ).

    % The `time_t' type is an abstract type that represents
    % calendar times.
    %
:- type time_t.

    % The `tm' type is a concrete type that represents calendar
    % times, broken down into their constituent components.
    % Comparison (via compare/3) of `tm' values whose `tm_dst'
    % components are identical is equivalent to comparison of
    % the times those `tm' values represent.
    %
:- type tm
    --->    tm(
                tm_year :: int,         % Year (number since 1900)
                tm_mon  :: int,         % Month (number since January, 0-11)
                tm_mday :: int,         % MonthDay (1-31)
                tm_hour :: int,         % Hours (after midnight, 0-23)
                tm_min  :: int,         % Minutes (0-59)
                tm_sec  :: int,         % Seconds (0-61)
                                        % (60 and 61 are for leap seconds)
                tm_yday :: int,         % YearDay (number since Jan 1st, 0-365)
                tm_wday :: int,         % WeekDay (number since Sunday, 0-6)
                tm_dst  :: maybe(dst)   % IsDST (is DST in effect?)
            ).

:- type dst
    --->    standard_time   % no, DST is not in effect
    ;       daylight_time.  % yes, DST is in effect

    % Some of the procedures in this module throw this type
    % as an exception if they can't obtain a result.
    %
:- type time_error
    --->    time_error(string). % Error message

%-----------------------------------------------------------------------------%

    % time.clock(Result, !IO):
    %
    % Returns the elapsed processor time (number of clock ticks). The base time
    % is arbitrary but doesn't change within a single process. If the time
    % cannot be obtained, this procedure will throw a time_error exception.
    % To obtain a time in seconds, divide Result by `time.clocks_per_sec'.
    %
    % On Java the elapsed time for the calling thread is returned.
    %
:- pred time.clock(clock_t::out, io::di, io::uo) is det.

    % time.clocks_per_sec:
    %
    % Returns the number of "clocks" per second as defined by CLOCKS_PER_SEC.
    % A `clock_t' value returned by `time.clock' can be divided by this value
    % to obtain a time in seconds. Note that the value of this function does
    % not necessarily reflect the actual clock precision; it just indicates the
    % scaling factor for the results of time.clock.
    %
:- func time.clocks_per_sec = int.

%-----------------------------------------------------------------------------%

    % time.time(Result, !IO):
    %
    % Returns the current (simple) calendar time. If the time cannot be
    % obtained, this procedure will throw a time_error exception.
    %
:- pred time.time(time_t::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % time.times(ProcessorTime, ElapsedRealTime, !IO):
    %
    % (POSIX)
    %
    % Returns the processor time information in the `tms' value, and the
    % elapsed real time relative to an arbitrary base in the `clock_t' value.
    % To obtain a time in seconds, divide the result by `time.clk_tck'.
    % If the time cannot be obtained, this procedure will throw a time_error
    % exception.
    %
    % On non-POSIX systems that do not support this functionality,
    % this procedure may simply always throw an exception.
    %
    % On Java the times for the calling thread are returned.
    % On Win32 and Java the child part of 'tms' is always zero.
    %
:- pred time.times(tms::out, clock_t::out, io::di, io::uo) is det.

    % time.clk_tck:
    %
    % Returns the number of "clock ticks" per second as defined by
    % sysconf(_SC_CLK_TCK). A `clock_t' value returned by `time.times'
    % can be divided by this value to obtain a time in seconds.
    %
    % On non-POSIX systems that do not support this functionality,
    % this procedure may simply always throw an exception.
    %
:- func time.clk_tck = int.

%-----------------------------------------------------------------------------%

    % time.difftime(Time1, Time0) = Diff:
    %
    % Computes the number of seconds elapsed between `Time1' and `Time0'.
    %
:- func time.difftime(time_t, time_t) = float.

    % time.localtime(Time) = TM:
    %
    % Converts the calendar time `Time' to a broken-down representation,
    % expressed relative to the user's specified time zone.
    %
:- func time.localtime(time_t) = tm.

    % time.gmtime(Time) = TM:
    %
    % Converts the calendar time `Time' to a broken-down representation,
    % expressed as UTC (Universal Coordinated Time).
    %
:- func time.gmtime(time_t) = tm.

    % time.mktime(TM) = Time:
    %
    % Converts the broken-down local time value to calendar time.
    % It also normalises the value by filling in day of week and day of year
    % based on the other components.
    %
:- func time.mktime(tm) = time_t.

%-----------------------------------------------------------------------------%

    % time.asctime(TM) = String:
    %
    % Converts the broken-down time value `TM' to a string in a standard
    % format.
    %
:- func time.asctime(tm) = string.

    % time.ctime(Time) = String:
    %
    % Converts the calendar time value `Time' to a string in a standard format
    % (i.e. same as "asctime (localtime (<time>))").
    %
:- func time.ctime(time_t) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

:- pragma foreign_decl("C",
"
    #include <time.h>
    #ifdef MR_HAVE_SYS_TYPES_H
        #include <sys/types.h>
    #endif
    #ifdef MR_HAVE_SYS_TIMES_H
        #include <sys/times.h>
    #endif
    #ifdef MR_HAVE_UNISTD_H
        #include <unistd.h>
    #endif

    #include ""mercury_timing.h"" /* for MR_CLOCK_TICKS_PER_SECOND */
").

% We use a no-tag wrapper type for time_t, rather than defining it as an
% equivalence type or just using a d.u./pragma foreign_type directly,
% to avoid the following problems:
%
%   - type errors in --high-level-code grades, due to the caller seeing
%     the abstract type, but the callee seeing the equivalence type
%     definition or the foreign_type definition.
%
%   - users can't define instance declarations for abstract equiv. types.
%
:- type time_t ---> time_t(time_t_rep).

:- type time_t_rep ---> time_t_rep(c_pointer).
:- pragma foreign_type("C", time_t_rep, "time_t")
    where comparison is compare_time_t_reps.

    % The System.DateTime will hold the value in UTC.
:- pragma foreign_type("IL", time_t_rep, "valuetype [mscorlib]System.DateTime")
    where comparison is compare_time_t_reps.

:- pragma foreign_type("C#", time_t_rep, "System.DateTime")
    where comparison is compare_time_t_reps.

:- pragma foreign_type("Java", time_t_rep, "java.util.Date")
    where comparison is compare_time_t_reps.

:- pragma foreign_type("Erlang", time_t_rep, "")
    where comparison is compare_time_t_reps.

:- pred compare_time_t_reps(comparison_result::uo,
    time_t_rep::in, time_t_rep::in) is det.

compare_time_t_reps(Result, X, Y) :-
    compare(Result, difftime(time_t(X), time_t(Y)), 0.0).

%-----------------------------------------------------------------------------%

time.clock(Result, !IO) :-
    time.c_clock(Ret, !IO),
    ( Ret = -1 ->
        throw(time_error("can't get clock value"))
    ;
        Result = Ret
    ).

:- pred time.c_clock(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    time.c_clock(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Ret = (MR_Integer) clock();
").
/* XXX need to add System.dll to the references list.
:- pragma foreign_proc("C#",
    time.c_clock(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    // XXX Ticks is long in .NET!
    Ret = (int) System.Diagnostics.Process.GetCurrentProcess
        .UserProcessorTime.Ticks;
}").
*/
:- pragma foreign_proc("Java",
    time.c_clock(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    java.lang.management.ThreadMXBean bean =
        java.lang.management.ManagementFactory.getThreadMXBean();
    long nsecs = bean.getCurrentThreadCpuTime();
    if (nsecs == -1) {
        Ret = -1;
    } else {
        /* This must match the definition of clocks_per_sec. */
        Ret = (int) (nsecs / 1000L);
    }
").

%-----------------------------------------------------------------------------%

%:- func time.clocks_per_sec = int.

:- pragma foreign_proc("C",
    time.clocks_per_sec = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ret = (MR_Integer) CLOCKS_PER_SEC;
").
:- pragma foreign_proc("C#",
    time.clocks_per_sec = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    // TicksPerSecond is guaranteed to be 10,000,000
    Ret = (int) System.TimeSpan.TicksPerSecond;
}").
:- pragma foreign_proc("Java",
    time.clocks_per_sec = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /* Emulate the POSIX value. */
    Ret = 1000000;
").

%-----------------------------------------------------------------------------%

time.times(Tms, Result, !IO) :-
    time.c_times(Ret, Ut, St, CUt, CSt, !IO),
    ( Ret = -1 ->
        throw(time_error("can't get times value"))
    ;
        Tms = tms(Ut, St, CUt, CSt),
        Result = Ret
    ).

:- pragma foreign_decl("C", local, "
#ifdef MR_WIN32
    #include ""mercury_windows.h""
    typedef union
    {
        FILETIME ft;
        __int64 i64;
    } timeKernel;
#endif
").
:- pred time.c_times(int::out, int::out, int::out, int::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    time.c_times(Ret::out, Ut::out, St::out, CUt::out, CSt::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"{
#ifdef MR_HAVE_POSIX_TIMES
    struct tms t;

    Ret = (MR_Integer) times(&t);

    Ut = (MR_Integer) t.tms_utime;
    St = (MR_Integer) t.tms_stime;
    CUt = (MR_Integer) t.tms_cutime;
    CSt = (MR_Integer) t.tms_cstime;
#else
  #ifdef MR_WIN32
    HANDLE hProcess;
    FILETIME ftCreation, ftExit, ftKernel, ftUser;
    timeKernel user, kernel;

    int factor;

    hProcess = GetCurrentProcess();
    GetProcessTimes(hProcess, &ftCreation, &ftExit, &ftKernel, &ftUser);

    factor = 10000000U / MR_CLOCK_TICKS_PER_SECOND;
    
    user.ft = ftUser;
    kernel.ft = ftKernel;

    Ut = (MR_Integer) (user.i64 / factor);
    St = (MR_Integer) (kernel.i64 / factor);

        /* XXX Not sure how to return children times */
    CUt = 0;
    CSt = 0;
  #else
    Ret = -1;
  #endif
#endif
}").

:- pragma foreign_proc("Java",
    time.c_times(Ret::out, Ut::out, St::out, CUt::out, CSt::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    /* We can only keep the lower 31 bits of the timestamp. */
    Ret = (int) (System.currentTimeMillis() & 0x7fffffff);

    try {
        java.lang.management.ThreadMXBean bean =
            java.lang.management.ManagementFactory.getThreadMXBean();
        long user_nsecs = bean.getCurrentThreadUserTime();
        long cpu_nsecs = bean.getCurrentThreadCpuTime();
        if (user_nsecs == -1 || cpu_nsecs == -1) {
            Ut = -1;
            St = -1;
        } else {
            /* These units must match the definition of clk_tck. */
            Ut = (int) (user_nsecs / 1000000L);
            St = (int) ((cpu_nsecs - user_nsecs) / 1000000L);
        }
    } catch (java.lang.UnsupportedOperationException e) {
        Ut = -1;
        St = -1;
    }

    CUt = 0;
    CSt = 0;
").

:- pragma foreign_proc("C#",
    c_times(Ret::out, Ut::out, St::out, CUt::out, CSt::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io, may_not_duplicate],
"
    Ret = (int) System.DateTime.UtcNow.Ticks;

    /* Should We keep only the lower 31 bits of the timestamp, like in java? */
    // Ret = Ret & 0x7fffffff;

    long user = System.Diagnostics.Process.GetCurrentProcess().UserProcessorTime.Ticks;
    long total = System.Diagnostics.Process.GetCurrentProcess().TotalProcessorTime.Ticks;

    Ut = (int) user;
    St = (int) (total - user);

    CUt = 0;
    CSt = 0;
").

%-----------------------------------------------------------------------------%

time.clk_tck = Ret :-
    Ret0 = time.c_clk_tck,
    ( Ret0 = -1 ->
        throw(time_error("can't get clk_tck value"))
    ;
        Ret = Ret0
    ).

:- func time.c_clk_tck = int.

:- pragma foreign_proc("C",
    time.c_clk_tck = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#if defined(MR_CLOCK_TICKS_PER_SECOND)
    Ret = MR_CLOCK_TICKS_PER_SECOND;
#else
    Ret = -1;
#endif
").
time.c_clk_tck = -1.   % default is to throw an exception.

:- pragma foreign_proc("C#",
    time.clk_tck = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    // TicksPerSecond is guaranteed to be 10,000,000
    Ret = (int) System.TimeSpan.TicksPerSecond;
}").
:- pragma foreign_proc("Java",
    time.c_clk_tck = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    /*
    ** We use System.currentTimeMillis() to return elapsed time so say that
    ** there are 1000 clock ticks per second.
    */
    Ret = 1000;
").

%-----------------------------------------------------------------------------%

time.time(Result, !IO) :-
    time.c_time(Ret, !IO),
    ( time.time_t_is_invalid(Ret) ->
        throw(time_error("can't get time value"))
    ;
        Result = time_t(Ret)
    ).

:- pred time.c_time(time_t_rep::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    time.c_time(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Ret = time(NULL);
").
:- pragma foreign_proc("C#",
    time.c_time(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"{
    Ret = System.DateTime.UtcNow;
}").
:- pragma foreign_proc("Java",
    time.c_time(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Ret = new java.util.Date();
").
:- pragma foreign_proc("Erlang",
    time.c_time(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    Ret = erlang:universaltime()
").

:- pred time.time_t_is_invalid(time_t_rep::in) is semidet.

:- pragma foreign_proc("C",
    time.time_t_is_invalid(Val::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Val == -1);
").
:- pragma foreign_proc("C#",
    time.time_t_is_invalid(_Val::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    SUCCESS_INDICATOR = false;
}").
:- pragma foreign_proc("Java",
    time.time_t_is_invalid(_Val::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Erlang",
    time.time_t_is_invalid(_Val::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false
").

%-----------------------------------------------------------------------------%

%:- func time.difftime(time_t, time_t) = float.

time.difftime(time_t(T1), time_t(T0)) = Diff :-
    time.c_difftime(T1, T0, Diff).

:- pred time.c_difftime(time_t_rep::in, time_t_rep::in, float::out) is det.

:- pragma foreign_proc("C",
    time.c_difftime(T1::in, T0::in, Diff::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Diff = (MR_Float) difftime(T1, T0);
").
:- pragma foreign_proc("C#",
    time.c_difftime(T1::in, T0::in, Diff::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    System.TimeSpan span;
    span = T1 - T0;
    Diff = span.TotalSeconds;
}").
:- pragma foreign_proc("Java",
    time.c_difftime(T1::in, T0::in, Diff::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Diff = (double) (T1.getTime() - T0.getTime()) / 1000;
").
:- pragma foreign_proc("Erlang",
    time.c_difftime(T1::in, T0::in, Diff::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S0 = calendar:datetime_to_gregorian_seconds(T0),
    S1 = calendar:datetime_to_gregorian_seconds(T1),
    Diff = float(S1 - S0)
").

%-----------------------------------------------------------------------------%

time.localtime(time_t(Time)) = TM :-
    time.c_localtime(Time, Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, N),
    TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, int_to_maybe_dst(N)).

:- pred time.c_localtime(time_t_rep::in, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out, int::out) is det.

:- pragma foreign_proc("C",
    time.c_localtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out),
    [will_not_call_mercury, promise_pure],
"
    struct tm   *p;
    time_t      t;

    t = Time;

    p = localtime(&t);

    /* XXX do we need to handle the case where p == NULL here? */

    Sec = (MR_Integer) p->tm_sec;
    Min = (MR_Integer) p->tm_min;
    Hrs = (MR_Integer) p->tm_hour;
    Mnt = (MR_Integer) p->tm_mon;
    Yr = (MR_Integer) p->tm_year;
    WD = (MR_Integer) p->tm_wday;
    MD = (MR_Integer) p->tm_mday;
    YD = (MR_Integer) p->tm_yday;
    N = (MR_Integer) p->tm_isdst;
").
:- pragma foreign_proc("C#",
    time.c_localtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out),
    [will_not_call_mercury, promise_pure],
"{
    System.DateTime t = Time.ToLocalTime();

    // we don't handle leap seconds
    Sec = t.Second;
    Min = t.Minute;
    Hrs = t.Hour;
    Mnt = t.Month - 1;
    Yr = t.Year - 1900;
    WD = (int) t.DayOfWeek;
    MD = t.Day;
    YD = t.DayOfYear - 1;
    // XXX On the day when you switch back to standard time from daylight
    // savings time, the time '2:30am' occurs twice, once during daylight
    // savings time (N = 1), and then again an hour later, during standard
    // time (N = 0).  The .NET API does not seem to provide any way to
    // get the right answer in both cases.
    if (System.TimeZone.CurrentTimeZone.IsDaylightSavingTime(t)) {
        N = 1;
    } else {
        N = 0;
    }
}").
:- pragma foreign_proc("Java",
    time.c_localtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    java.util.GregorianCalendar gc = new java.util.GregorianCalendar();

    gc.setTime(Time);
    Yr = gc.get(java.util.Calendar.YEAR) - 1900;
    Mnt = gc.get(java.util.Calendar.MONTH);
    MD = gc.get(java.util.Calendar.DAY_OF_MONTH);
    Hrs = gc.get(java.util.Calendar.HOUR_OF_DAY);
    Min = gc.get(java.util.Calendar.MINUTE);
    Sec = gc.get(java.util.Calendar.SECOND);
    YD = gc.get(java.util.Calendar.DAY_OF_YEAR) - 1;

    switch (gc.get(java.util.Calendar.DAY_OF_WEEK)) {
        case java.util.Calendar.SUNDAY:
            WD = 0;
            break;
        case java.util.Calendar.MONDAY:
            WD = 1;
            break;
        case java.util.Calendar.TUESDAY:
            WD = 2;
            break;
        case java.util.Calendar.WEDNESDAY:
            WD = 3;
            break;
        case java.util.Calendar.THURSDAY:
            WD = 4;
            break;
        case java.util.Calendar.FRIDAY:
            WD = 5;
            break;
        case java.util.Calendar.SATURDAY:
            WD = 6;
            break;
        default:
            throw new RuntimeException(
                ""invalid DAY_OF_WEEK in time.c_local_time"");
    }

    if (gc.getTimeZone().inDaylightTime(Time)) {
        N = 1;
    } else {
        N = 0;
    }
").

%:- func time.gmtime(time_t) = tm.

time.gmtime(time_t(Time)) = TM :-
    time.c_gmtime(Time, Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, N),
    TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, int_to_maybe_dst(N)).

:- pred time.c_gmtime(time_t_rep::in, int::out, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out) is det.

:- pragma foreign_proc("C",
    time.c_gmtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out),
    [will_not_call_mercury, promise_pure],
"{
    struct tm   *p;
    time_t      t;

    t = Time;

    p = gmtime(&t);

    /* XXX do we need to handle the case where p == NULL here? */

    Sec = (MR_Integer) p->tm_sec;
    Min = (MR_Integer) p->tm_min;
    Hrs = (MR_Integer) p->tm_hour;
    Mnt = (MR_Integer) p->tm_mon;
    Yr = (MR_Integer) p->tm_year;
    WD = (MR_Integer) p->tm_wday;
    MD = (MR_Integer) p->tm_mday;
    YD = (MR_Integer) p->tm_yday;
    N = (MR_Integer) p->tm_isdst;
}").
:- pragma foreign_proc("C#",
    time.c_gmtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out),
    [will_not_call_mercury, promise_pure],
"{
    System.DateTime t = Time;

    // we don't handle leap seconds
    Sec = t.Second;
    Min = t.Minute;
    Hrs = t.Hour;
    Mnt = t.Month - 1;
    Yr = t.Year - 1900;
    WD = (int) t.DayOfWeek;
    MD = t.Day;
    YD = t.DayOfYear - 1;
    // UTC time can never have daylight savings.
    N = 0;
}").
:- pragma foreign_proc("Java",
    time.c_gmtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    java.util.GregorianCalendar gc =
        new java.util.GregorianCalendar(
        java.util.SimpleTimeZone.getTimeZone(""GMT""));

    gc.setTime(Time);
    Yr = gc.get(java.util.Calendar.YEAR) - 1900;
    Mnt = gc.get(java.util.Calendar.MONTH);
    MD = gc.get(java.util.Calendar.DAY_OF_MONTH);
    Hrs = gc.get(java.util.Calendar.HOUR_OF_DAY);
    Min = gc.get(java.util.Calendar.MINUTE);
    Sec = gc.get(java.util.Calendar.SECOND);
    YD = gc.get(java.util.Calendar.DAY_OF_YEAR) - 1;

    switch (gc.get(java.util.Calendar.DAY_OF_WEEK)) {
        case java.util.Calendar.SUNDAY:
            WD = 0;
            break;
        case java.util.Calendar.MONDAY:
            WD = 1;
            break;
        case java.util.Calendar.TUESDAY:
            WD = 2;
            break;
        case java.util.Calendar.WEDNESDAY:
            WD = 3;
            break;
        case java.util.Calendar.THURSDAY:
            WD = 4;
            break;
        case java.util.Calendar.FRIDAY:
            WD = 5;
            break;
        case java.util.Calendar.SATURDAY:
            WD = 6;
            break;
        default:
            throw new RuntimeException(
                ""invalid DAY_OF_WEEK in time.c_gmtime"");
    }

    N = 0;
").
:- pragma foreign_proc("Erlang",
    time.c_gmtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out),
    [will_not_call_mercury, promise_pure],
"
    {{Yr0, Mnt, MD}, {Hrs, Min, Sec}} = Time,
    Yr = Yr0 - 1900,

    DayNumber = calendar:date_to_gregorian_days(Yr, Mnt, MD),
    Jan1_Number = calendar:date_to_gregorian_days(Yr, 1, 1),
    YD = DayNumber - Jan1_Number,

    % Sunday = 7 = 0
    WD = calendar:day_of_the_week(Yr, Mnt, MD) rem 7,

    N = 0
").

:- func int_to_maybe_dst(int) = maybe(dst).

int_to_maybe_dst(N) = DST :-
    ( N = 0 ->
        DST = yes(standard_time)
    ; N > 0 ->
        DST = yes(daylight_time)
    ; % N < 0
        DST = no
    ).

%-----------------------------------------------------------------------------%

%:- func time.mktime(tm) = time_t.

time.mktime(TM) = time_t(Time) :-
    TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, DST),
    time.c_mktime(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD,
        maybe_dst_to_int(DST), Time).

:- pred time.c_mktime(int::in, int::in, int::in, int::in, int::in, int::in,
    int::in, int::in, int::in, time_t_rep::out) is det.

:- pragma foreign_proc("C",
    time.c_mktime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
        YD::in, WD::in, N::in, Time::out),
    [will_not_call_mercury, promise_pure],
 "{
    struct tm t;

    t.tm_sec = (int) Sec;
    t.tm_min = (int) Min;
    t.tm_hour = (int) Hrs;
    t.tm_mon = (int) Mnt;
    t.tm_year = (int) Yr;
    t.tm_wday = (int) WD;
    t.tm_mday = (int) MD;
    t.tm_yday = (int) YD;
    t.tm_isdst = (int) N;

    Time = mktime(&t);
}").
:- pragma foreign_proc("C#",
    time.c_mktime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
        _YD::in, _WD::in, _N::in, Time::out),
    [will_not_call_mercury, promise_pure],
 "{
    // We don't use YD, WD and N.
    // XXX Ignoring N the daylight savings time indicator is bad
    // On the day when you switch back to standard time from daylight
    // savings time, the time '2:30am' occurs twice, once during daylight
    // savings time (N = 1), and then again an hour later, during standard
    // time (N = 0).  The .NET API does not seem to provide any way to
    // get the right answer in both cases.
    System.DateTime local_time =
        new System.DateTime(Yr + 1900, Mnt + 1, MD, Hrs, Min, Sec);
    Time = local_time.ToUniversalTime();
}").
:- pragma foreign_proc("Java",
    time.c_mktime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
        _YD::in, _WD::in, N::in, Time::out),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    java.util.GregorianCalendar gc = new java.util.GregorianCalendar(
        Yr + 1900, Mnt, MD, Hrs, Min, Sec);

    Time = gc.getTime();

    // Correct for DST:  This is only an issue when it is possible for the
    // same 'time' to occur twice due to daylight savings ending.
    // (In Melbourne, 2:00am-2:59am occur twice when leaving DST)

    // If the time we constructed is not in daylight savings time, but
    // it should be, we need to subtract the DSTSavings.
    if (N == 1 && gc.getTimeZone().inDaylightTime(Time) == false) {
        Time.setTime(Time.getTime() - getDSTSavings(gc.getTimeZone()));
        if (gc.getTimeZone().inDaylightTime(Time) == false) {
            throw new RuntimeException(
                ""time.mktime: failed to correct for DST"");
        }
    }

    // If the time we constructed is in daylight savings time, but
    // should not be, we need to add the DSTSavings.
    if (N == 0 && gc.getTimeZone().inDaylightTime(Time) == true) {
        Time.setTime(Time.getTime() + getDSTSavings(gc.getTimeZone()));
        if (gc.getTimeZone().inDaylightTime(Time) == true) {
            throw new RuntimeException(
                ""time.mktime: failed to correct for DST"");
        }
    }
").

:- pragma foreign_code("Java",
"
/*
** getDSTSavings():
**
** This method uses reflection to retrieve and call the getDSTSavings()
** method for a given TimeZone object.
**
** The reason we do this is that for Java versions < 1.4, the
** TimeZone.getDSTSavings() method did not exist, but the
** SimpleTimeZone.getDSTSavings() method did, and the concrete instance of
** TimeZone used by GregorianCalender (which is what we use in this
** module) is a SimpleTimeZone.
** However, we can't just cast the TimeZone instance to SimpleTimeZone,
** because for Java versions >= 1.4, GregorianCalender no longer uses a
** SimpleTimeZone.  So in this case, what we really want is
** TimeZone.getDSTSavings(), but we can't just put that or the code won't
** compile for Java versions < 1.4.
**
** Thus, the solution is to invoke the getDSTSavings() method using
** reflection, which will cover both cases.
*/
public static int
getDSTSavings(java.util.TimeZone tz) {
    try {
        // Simulate
        //  return tz.getDSTSavings()
        // using reflection.

        return ((java.lang.Integer) (tz.getClass().
            getMethod(""getDSTSavings"").
            invoke(tz))).intValue();
    }
    catch (java.lang.Exception e) {
        throw new java.lang.RuntimeException(
            ""time.c_mktime: Failed to locate "" +
            ""getDSTSavings() method."");
    }
}
").

:- func maybe_dst_to_int(maybe(dst)) = int.

maybe_dst_to_int(M) = N :-
    ( M = yes(DST), DST = daylight_time,
        N = 1
    ; M = yes(DST), DST = standard_time,
        N = 0
    ; M = no,
        N = -1
    ).

%-----------------------------------------------------------------------------%

%:- func time.asctime(tm) = string.

time.asctime(TM) = Str :-
    TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, _YD, WD, _DST),
    Str = string.format("%.3s %.3s%3d %.2d:%.2d:%.2d %d\n",
        [s(wday_name(WD)), s(mon_name(Mnt)), i(MD), i(Hrs),
            i(Min), i(Sec), i(1900 + Yr)]).

:- func wday_name(int) = string.

wday_name(N) = Name :-
    ( wday_name(N, Name0) ->
        Name = Name0
    ;
        error("time: wday_name")
    ).

:- pred wday_name(int::in, string::out) is semidet.

wday_name(0, "Sun").
wday_name(1, "Mon").
wday_name(2, "Tue").
wday_name(3, "Wed").
wday_name(4, "Thu").
wday_name(5, "Fri").
wday_name(6, "Sat").

:- func mon_name(int) = string.

mon_name(N) = Name :-
    ( mon_name(N, Name0) ->
        Name = Name0
    ;
        error("time: mon_name")
    ).

:- pred mon_name(int::in, string::out) is semidet.

mon_name(0, "Jan").
mon_name(1, "Feb").
mon_name(2, "Mar").
mon_name(3, "Apr").
mon_name(4, "May").
mon_name(5, "Jun").
mon_name(6, "Jul").
mon_name(7, "Aug").
mon_name(8, "Sep").
mon_name(9, "Oct").
mon_name(10, "Nov").
mon_name(11, "Dec").

%-----------------------------------------------------------------------------%

time.ctime(Time) = asctime(localtime(Time)).

%-----------------------------------------------------------------------------%

% XXX This needs to be in the interface because pragma export doesn't work yet
% on the .NET backend and io.m needs to access this.
:- interface.

:- type time_t_rep.

:- func construct_time_t(time_t_rep) = time_t.

:- implementation.

:- pragma foreign_export("C", construct_time_t(in) = out,
    "ML_construct_time_t").
:- pragma foreign_export("IL", construct_time_t(in) = out,
    "ML_construct_time_t").
:- pragma foreign_export("C#", construct_time_t(in) = out,
    "ML_construct_time_t").
:- pragma foreign_export("Java", construct_time_t(in) = out,
    "ML_construct_time_t").

construct_time_t(T) = time_t(T).

%-----------------------------------------------------------------------------%
