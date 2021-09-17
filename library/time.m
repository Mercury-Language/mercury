%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Originally written in 1999 by Tomas By <T.By@dcs.shef.ac.uk>
% "Feel free to use this code or parts of it any way you want."
%
% Some portions are Copyright (C) 1999-2007,2009-2012 The University of Melbourne.
% Copyright (C) 2014-2021 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: time.m.
% Main authors: Tomas By <T.By@dcs.shef.ac.uk>, fjh.
% Stability: medium.
%
% Time functions.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module time.
:- interface.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % The `clock_t' type represents times measured in clock ticks.
    % NOTE: the unit used for a value of this type depends on whether it was
    % returned by `clock' or `times'. See the comments on these
    % predicates below.
    %
:- type clock_t == int.

    % The `tms' type holds information about the amount of processor time
    % that a process and its child processes have consumed.
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

    % The `tm' type is a concrete type that represents calendar times,
    % broken down into their constituent components. Comparison (via compare/3)
    % of `tm' values is equivalent to comparison of the times those `tm'
    % values represent IF AND ONLY IF their `tm_dst' components are identical.
    %
    % Whether leap seconds are supported depends on the target language.
    % Currently, only C supports leap seconds, while Java and C# do not.
    % For target languages that do not support leap seconds:
    %
    % - predicates in this module that produce a `tm' value as an output
    %   will never set its `tm_sec' component to a value beyond 59;
    %
    % - predicates in this module that take a `tm_sec' value as an input
    %   will throw a time_error/1 exception if the value of the `tm_sec'
    %   component is beyond 59.
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
                tm_dst  :: maybe(dst)   % IsDST (is DST applicable, and if so,
                                        % is it in effect?)
            ).

:- type dst
    --->    standard_time   % no, DST is not in effect
    ;       daylight_time.  % yes, DST is in effect

    % Some of the procedures in this module throw this type
    % as an exception if they can't obtain a result.
    %
:- type time_error
    --->    time_error(string). % Error message

%---------------------------------------------------------------------------%

    % clock(Result, !IO):
    %
    % Returns the elapsed processor time (number of clock ticks). The base time
    % is arbitrary but doesn't change within a single process. If the time
    % cannot be obtained, this procedure will throw a time_error exception.
    % To obtain a time in seconds, divide Result by `clocks_per_sec'.
    %
    % On Java the elapsed time for the calling thread is returned.
    %
:- pred clock(clock_t::out, io::di, io::uo) is det.

    % clocks_per_sec:
    %
    % Returns the number of "clocks" per second as defined by CLOCKS_PER_SEC.
    % A `clock_t' value returned by `clock' can be divided by this value
    % to obtain a time in seconds. Note that the value of this function does
    % not necessarily reflect the actual clock precision; it just indicates the
    % scaling factor for the results of `clock'.
    %
:- func clocks_per_sec = int.

%---------------------------------------------------------------------------%

    % time(Result, !IO):
    %
    % Returns the current (simple) calendar time. If the time cannot be
    % obtained, this procedure will throw a time_error exception.
    %
:- pred time(time_t::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % times(ProcessorTime, ElapsedRealTime, !IO):
    %
    % (POSIX)
    %
    % Returns the processor time information in the `tms' value, and the
    % elapsed real time relative to an arbitrary base in the `clock_t' value.
    % To obtain a time in seconds, divide the result by `clk_tck'.
    % If the time cannot be obtained, this procedure will throw a time_error
    % exception.
    %
    % On non-POSIX systems that do not support this functionality,
    % this procedure may simply always throw an exception.
    %
    % On Java the times for the calling thread are returned.
    % On Win32 and Java the child part of 'tms' is always zero.
    %
:- pred times(tms::out, clock_t::out, io::di, io::uo) is det.

    % clk_tck:
    %
    % Returns the number of "clock ticks" per second as defined by
    % sysconf(_SC_CLK_TCK). A `clock_t' value returned by `times'
    % can be divided by this value to obtain a time in seconds.
    %
    % On non-POSIX systems that do not support this functionality,
    % this procedure may simply always throw an exception.
    %
:- func clk_tck = int.

%---------------------------------------------------------------------------%

    % difftime(Time1, Time0) = Diff:
    %
    % Computes the number of seconds elapsed between `Time1' and `Time0'.
    %
:- func difftime(time_t, time_t) = float.

    % localtime(Time, TM, !IO):
    %
    % Converts the (simple) calendar time `Time' to a broken-down
    % representation `TM', expressed relative to the current time zone.
    %
:- pred localtime(time_t::in, tm::out, io::di, io::uo) is det.

    % gmtime(Time) = TM:
    %
    % Converts the (simple) calendar time `Time' to a broken-down
    % representation `TM', expressed as UTC (Universal Coordinated Time).
    %
:- func gmtime(time_t) = tm.

    % mktime(TM, Time, !IO):
    %
    % Converts the broken-down time value `TM' to a (simple) calendar time
    % `Time'. That is, `TM' is relative to the current time zone.
    % The `tm_wday' and `tm_yday' fields of `TM' are ignored.
    %
:- pred mktime(tm::in, time_t::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % asctime(TM) = String:
    %
    % Converts the broken-down time value `TM' to a string in a standard
    % format.
    %
:- func asctime(tm) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
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

    #include ""mercury_timing.h""       // for MR_CLOCK_TICKS_PER_SECOND
    #include ""mercury_runtime_util.h"" // For MR_strerror.
    #include ""mercury_regs.h""         // For MR_{save,restore}_transient_hp
    #include ""mercury_string.h""       // For MR_make_aligned_string_copy etc.
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
:- type time_t
    --->    time_t(time_t_rep).

:- type time_t_rep
    --->    time_t_rep(c_pointer).

:- pragma foreign_type("C", time_t_rep, "time_t")
    where comparison is compare_time_t_reps.
:- pragma foreign_type("C#", time_t_rep, "System.DateTime")
    where comparison is compare_time_t_reps.
:- pragma foreign_type("Java", time_t_rep, "java.time.Instant")
    where comparison is compare_time_t_reps.

:- pred compare_time_t_reps(comparison_result::uo,
    time_t_rep::in, time_t_rep::in) is det.

compare_time_t_reps(Result, X, Y) :-
    compare(Result, difftime(time_t(X), time_t(Y)), 0.0).

%---------------------------------------------------------------------------%

clock(Result, !IO) :-
    target_clock(Ret, !IO),
    ( if Ret = -1 then
        throw(time_error("can't get clock value"))
    else
        Result = Ret
    ).

:- pred target_clock(int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    target_clock(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Ret = (MR_Integer) clock();
").
:- pragma foreign_proc("C#",
    target_clock(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    // XXX Ticks is long in .NET!
    Ret = (int) System.Diagnostics.Process.GetCurrentProcess().
        UserProcessorTime.Ticks;
").
:- pragma foreign_proc("Java",
    target_clock(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    java.lang.management.ThreadMXBean bean =
        java.lang.management.ManagementFactory.getThreadMXBean();
    long nsecs = bean.getCurrentThreadCpuTime();
    if (nsecs == -1) {
        Ret = -1;
    } else {
        // This must match the definition of clocks_per_sec.
        Ret = (int) (nsecs / 1000L);
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    clocks_per_sec = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ret = (MR_Integer) CLOCKS_PER_SEC;
").
:- pragma foreign_proc("C#",
    clocks_per_sec = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // TicksPerSecond is guaranteed to be 10,000,000
    Ret = (int) System.TimeSpan.TicksPerSecond;
").
:- pragma foreign_proc("Java",
    clocks_per_sec = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // Emulate the POSIX value.
    Ret = 1000000;
").

%---------------------------------------------------------------------------%

time(Result, !IO) :-
    target_time(Ret, !IO),
    ( if time_t_is_invalid(Ret) then
        throw(time_error("can't get time value"))
    else
        Result = time_t(Ret)
    ).

:- pred target_time(time_t_rep::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    target_time(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    Ret = time(NULL);
").
:- pragma foreign_proc("C#",
    target_time(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Ret = System.DateTime.UtcNow;
").
:- pragma foreign_proc("Java",
    target_time(Ret::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    Ret = java.time.Instant.now();
").

:- pred time_t_is_invalid(time_t_rep::in) is semidet.

:- pragma foreign_proc("C",
    time_t_is_invalid(Val::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = (Val == -1);
").
:- pragma foreign_proc("C#",
    time_t_is_invalid(_Val::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Java",
    time_t_is_invalid(_Val::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = false;
").

%---------------------------------------------------------------------------%

times(Tms, Result, !IO) :-
    target_times(Ret, Ut, St, CUt, CSt, !IO),
    ( if Ret = -1 then
        throw(time_error("can't get times value"))
    else
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

:- pred target_times(int::out, int::out, int::out, int::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    target_times(Ret::out, Ut::out, St::out, CUt::out, CSt::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MR_HAVE_POSIX_TIMES
    struct tms t;

    Ret = (MR_Integer) times(&t);

    Ut = (MR_Integer) t.tms_utime;
    St = (MR_Integer) t.tms_stime;
    CUt = (MR_Integer) t.tms_cutime;
    CSt = (MR_Integer) t.tms_cstime;
#else
  #if defined(MR_WIN32) && defined(MR_CLOCK_TICKS_PER_SECOND)
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

    // XXX Not sure how to return children times.
    CUt = 0;
    CSt = 0;
  #else
    Ret = -1;
  #endif
#endif
").

:- pragma foreign_proc("Java",
    target_times(Ret::out, Ut::out, St::out, CUt::out, CSt::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    // We can only keep the lower 31 bits of the timestamp.
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
            // These units must match the definition of clk_tck.
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
    target_times(Ret::out, Ut::out, St::out, CUt::out, CSt::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    Ret = (int) System.DateTime.UtcNow.Ticks;

    // Should we keep only the lower 31 bits of the timestamp, like in java?
    // Ret = Ret & 0x7fffffff;

    long user =
        System.Diagnostics.Process.GetCurrentProcess().UserProcessorTime.Ticks;
    long total =
        System.Diagnostics.Process.GetCurrentProcess().TotalProcessorTime.Ticks;

    Ut = (int) user;
    St = (int) (total - user);

    CUt = 0;
    CSt = 0;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C#",
    clk_tck = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // TicksPerSecond is guaranteed to be 10,000,000.
    Ret = (int) System.TimeSpan.TicksPerSecond;
").

clk_tck = Ret :-
    Ret0 = target_clk_tck,
    ( if Ret0 = -1 then
        throw(time_error("can't get clk_tck value"))
    else
        Ret = Ret0
    ).

:- func target_clk_tck = int.
:- pragma consider_used(func(target_clk_tck/0)).
:- pragma foreign_proc("C",
    target_clk_tck = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
#if defined(MR_CLOCK_TICKS_PER_SECOND)
    Ret = MR_CLOCK_TICKS_PER_SECOND;
#else
    Ret = -1;
#endif
").
:- pragma foreign_proc("Java",
    target_clk_tck = (Ret::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    // We use System.currentTimeMillis() to return elapsed time,
    // so say that there are 1000 clock ticks per second.
    Ret = 1000;
").

target_clk_tck = -1.   % default, to get clk_tck to throw an exception.

%---------------------------------------------------------------------------%

difftime(time_t(T1), time_t(T0)) = Diff :-
    target_difftime(T1, T0, Diff).

:- pred target_difftime(time_t_rep::in, time_t_rep::in, float::out) is det.

:- pragma foreign_proc("C",
    target_difftime(T1::in, T0::in, Diff::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Diff = (MR_Float) difftime(T1, T0);
").
:- pragma foreign_proc("C#",
    target_difftime(T1::in, T0::in, Diff::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    System.TimeSpan span;
    span = T1 - T0;
    Diff = span.TotalSeconds;
").
:- pragma foreign_proc("Java",
    target_difftime(T1::in, T0::in, Diff::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Diff = (double) (T1.toEpochMilli() - T0.toEpochMilli()) / 1000;
").

%---------------------------------------------------------------------------%

localtime(Time, TM, !IO) :-
    Time = time_t(RawTime),
    target_localtime(RawTime, IsOk, Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, N,
        ErrorMsg, !IO),
    (
        IsOk = yes,
        TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, int_to_maybe_dst(N))
    ;
        IsOk = no,
        throw(time_error("time.localtime: conversion failed: " ++ ErrorMsg))
    ).

:- pred target_localtime(time_t_rep::in, bool::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out, int::out, int::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    target_localtime(Time::in, IsOk::out, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io],
"
    struct tm   *p;
    time_t      t;

    t = Time;

    p = localtime(&t);
    if (p == NULL) {
        char errbuf[MR_STRERROR_BUF_SIZE];
        const char *errno_msg;
        IsOk = MR_NO;
        Sec = 0; // Dummy values.
        Min = 0;
        Hrs = 0;
        Mnt = 0;
        Yr = 0;
        WD = 0;
        MD = 0;
        YD = 0;
        N = 0;
        errno_msg = MR_strerror(errno, errbuf, sizeof(errbuf));
        MR_save_transient_hp();
        MR_make_aligned_string_copy(ErrorMsg, errno_msg);
        MR_restore_transient_hp();
    } else {
        IsOk = MR_YES;
        Sec = (MR_Integer) p->tm_sec;
        Min = (MR_Integer) p->tm_min;
        Hrs = (MR_Integer) p->tm_hour;
        Mnt = (MR_Integer) p->tm_mon;
        Yr = (MR_Integer) p->tm_year;
        WD = (MR_Integer) p->tm_wday;
        MD = (MR_Integer) p->tm_mday;
        YD = (MR_Integer) p->tm_yday;
        N = (MR_Integer) p->tm_isdst;
        ErrorMsg = MR_make_string_const(\"\");
    }
").
:- pragma foreign_proc("C#",
    target_localtime(Time::in, IsOk::out, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out, ErrorMsg::out,
         _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    // XXX t will be clamped to MinValue / MaxValue if the converted
    // time cannot be represented by a DateTime object.
    System.DateTime t = Time.ToLocalTime();

    // We don't handle leap seconds.
    // (XXX actually, C# does not handle leap seconds.)
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
    // time (N = 0). The .NET API does not seem to provide any way to
    // get the right answer in both cases.
    if (System.TimeZoneInfo.Local.IsDaylightSavingTime(t)) {
        N = 1;
    } else {
        N = 0;
    }
    IsOk = mr_bool.YES;
    ErrorMsg = \"\";
").
:- pragma foreign_proc("Java",
    target_localtime(Time::in, IsOk::out, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    try {
        java.time.ZoneId tz = java.time.ZoneId.systemDefault();
        java.time.ZonedDateTime zdt =
            java.time.ZonedDateTime.ofInstant(Time, tz);

        Yr = zdt.getYear() - 1900;
        Mnt = zdt.getMonthValue() - 1;
        MD = zdt.getDayOfMonth();
        Hrs = zdt.getHour();
        Min = zdt.getMinute();
        Sec = zdt.getSecond();
        YD = zdt.getDayOfYear() - 1;
        WD = zdt.getDayOfWeek().getValue() % 7;

        if (tz.getRules().isDaylightSavings(Time)) {
            N = 1;
        } else {
            N = 0;
        }
        IsOk = bool.YES;
        ErrorMsg = \"\";
    } catch (java.time.DateTimeException e) {
        Yr = 0;
        Mnt = 0;
        MD = 0;
        Hrs = 0;
        Min = 0;
        Sec = 0;
        YD = 0;
        WD = 0;
        N = 0;
        IsOk = bool.NO;
        if (e.getMessage() != null) {
            ErrorMsg = e.getMessage();
        } else {
            ErrorMsg = \"\";
        }
    }
").

%---------------------------------------------------------------------------%

gmtime(Time) = TM :-
    Time = time_t(RawTime),
    target_gmtime(RawTime, IsOk, Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, N,
        ErrorMsg),
    (
        IsOk = yes,
        TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, int_to_maybe_dst(N))
    ;
        IsOk = no,
        throw(time_error("time.gmtime: conversion failed: " ++ ErrorMsg))
    ).

:- pred target_gmtime(time_t_rep::in, bool::out, int::out, int::out, int::out,
    int::out, int::out, int::out, int::out, int::out, int::out,
    string::out) is det.

:- pragma foreign_proc("C",
    target_gmtime(Time::in, IsOk::out, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out, ErrorMsg::out),
    [will_not_call_mercury, promise_pure, not_thread_safe],
"
    struct tm   *p;
    time_t      t;

    t = Time;

    p = gmtime(&t);
    if (p == NULL) {
        char errbuf[MR_STRERROR_BUF_SIZE];
        const char *errno_msg;
        IsOk = MR_NO;
        Sec = 0;
        Min = 0;
        Hrs = 0;
        Mnt = 0;
        Yr = 0;
        WD = 0;
        MD = 0;
        YD = 0;
        N = 0;
        errno_msg = MR_strerror(errno, errbuf, sizeof(errbuf));
        MR_save_transient_hp();
        MR_make_aligned_string_copy(ErrorMsg, errno_msg);
        MR_restore_transient_hp();
    } else {
        IsOk = MR_YES;
        Sec = (MR_Integer) p->tm_sec;
        Min = (MR_Integer) p->tm_min;
        Hrs = (MR_Integer) p->tm_hour;
        Mnt = (MR_Integer) p->tm_mon;
        Yr = (MR_Integer) p->tm_year;
        WD = (MR_Integer) p->tm_wday;
        MD = (MR_Integer) p->tm_mday;
        YD = (MR_Integer) p->tm_yday;
        N = (MR_Integer) p->tm_isdst;
        ErrorMsg = MR_make_string_const(\"\");
    }
").
:- pragma foreign_proc("C#",
    target_gmtime(Time::in, IsOk::out, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out, ErrorMsg::out),
    [will_not_call_mercury, promise_pure],
"
    System.DateTime t = Time;

    // We don't handle leap seconds.
    // (XXX actually C# does not handle leap seconds.)
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
    IsOk = mr_bool.YES;
    ErrorMsg = \"\";
").
:- pragma foreign_proc("Java",
    target_gmtime(Time::in, IsOk::out, Yr::out, Mnt::out, MD::out, Hrs::out,
        Min::out, Sec::out, YD::out, WD::out, N::out, ErrorMsg::out),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    try {
        java.time.OffsetDateTime utcTime =
            java.time.OffsetDateTime.ofInstant(Time,
            java.time.ZoneOffset.UTC);

        Yr = utcTime.getYear() - 1900;
        Mnt = utcTime.getMonthValue() - 1;
        MD = utcTime.getDayOfMonth();
        Hrs = utcTime.getHour();
        Min = utcTime.getMinute();
        Sec = utcTime.getSecond();
        YD = utcTime.getDayOfYear() - 1;
        // Sunday == 7 == 0.
        WD = utcTime.getDayOfWeek().getValue() % 7;
        N = 0;
        IsOk = bool.YES;
        ErrorMsg = \"\";
    } catch (java.time.DateTimeException e) {
        Yr = 0;
        Mnt = 0;
        MD = 0;
        Hrs = 0;
        Min = 0;
        Sec = 0;
        YD = 0;
        WD = 0;
        N = 0;
        IsOk = bool.NO;
        if (e.getMessage() != null) {
            ErrorMsg = e.getMessage();
        } else {
            ErrorMsg = \"\";
        }
    }
").

:- func int_to_maybe_dst(int) = maybe(dst).

int_to_maybe_dst(N) = DST :-
    ( if N = 0 then
        DST = yes(standard_time)
    else if N > 0 then
        DST = yes(daylight_time)
    else % N < 0
        DST = no
    ).

%---------------------------------------------------------------------------%

mktime(TM, Time, !IO) :-
    TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, DST),
    % XXX we need to check the validity  of TM's fields here, since mktime()'s
    % checks are rubbish.
    target_mktime(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, maybe_dst_to_int(DST),
        IsOk, RawTime, ErrMsg, !IO),
    (
        IsOk = yes,
        Time = time_t(RawTime)
    ;
        IsOk = no,
        unexpected($pred, "cannot convert to calendar time: " ++ ErrMsg)
    ).

    % NOTE: mktime() modifies tzname, however we do not expose tzname
    % through a Mercury interface.
    %
:- pred target_mktime(int::in, int::in, int::in, int::in, int::in, int::in,
    int::in, int::in, int::in, bool::out, time_t_rep::out, string::out,
     io::di, io::uo) is det.

:- pragma foreign_proc("C",
    target_mktime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
        YD::in, WD::in, N::in, IsOk::out, Time::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, not_thread_safe, tabled_for_io],
"
    struct tm t;
    char errbuf[MR_STRERROR_BUF_SIZE];
    const char *errno_msg;

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
    if (Time == (time_t) -1) {
        IsOk = MR_NO;
        errno_msg = MR_strerror(errno, errbuf, sizeof(errbuf));
        MR_save_transient_hp();
        MR_make_aligned_string_copy(ErrorMsg, errno_msg);
        MR_restore_transient_hp();
    } else {
        IsOk = MR_YES;
        ErrorMsg = MR_make_string_const(\"\");
    }
").
:- pragma foreign_proc("C#",
    target_mktime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
        _YD::in, _WD::in, _N::in, IsOk::out, Time::out, ErrorMsg::out,
         _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    // We don't use YD, WD and N.
    // XXX Ignoring N, the daylight savings time indicator, is bad.
    // On the day when you switch back to standard time from daylight
    // savings time, the time '2:30am' occurs twice, once during daylight
    // savings time (N = 1), and then again an hour later, during standard
    // time (N = 0). The .NET API does not seem to provide any way
    // to get the right answer in both cases.
    try {
        System.DateTime local_time =
            new System.DateTime(Yr + 1900, Mnt + 1, MD, Hrs, Min, Sec);
        Time = local_time.ToUniversalTime();
        IsOk = mr_bool.YES;
        ErrorMsg = \"\";
    } catch (System.ArgumentOutOfRangeException e) {
        Time = System.DateTime.MinValue; // Dummy value.
        IsOk = mr_bool.NO;
        ErrorMsg = e.Message;
    }
").
:- pragma foreign_proc("Java",
    target_mktime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
        _YD::in, _WD::in, N::in, IsOk::out, Time::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
    try {
        java.time.ZoneId tz = java.time.ZoneId.systemDefault();
        java.time.LocalDateTime localDateTime =
            java.time.LocalDateTime.of(Yr + 1900, Mnt + 1, MD, Hrs, Min, Sec);
        java.time.ZonedDateTime zonedDateTime =
            java.time.ZonedDateTime.of(localDateTime, tz);
        java.time.Instant Time0 = zonedDateTime.toInstant();

        // Correct for DST:  This is only an issue when it is possible for the
        // same 'time' to occur twice due to daylight savings ending.
        // (In Melbourne, 2:00am-2:59am occur twice when leaving DST)

        java.time.zone.ZoneRules rules = tz.getRules();
        boolean isDST = rules.isDaylightSavings(Time0);

        if (N == 1 & !isDST) {
            // If the time we constructed is not in daylight savings time,
            // but it should be, we need to subtract the DSTSavings.
            java.time.Duration savings = rules.getDaylightSavings(Time0);
            Time = Time0.minus(savings);
            if (!rules.isDaylightSavings(Time)) {
                IsOk = bool.NO;
                ErrorMsg = \"failed to correct for DST\";
            } else {
                IsOk = bool.YES;
                ErrorMsg = \"\";
            }
        } else if (N == 0 && isDST) {
            // If the time we constructed is in daylight savings time,
            // but should not be, we need to add the DSTSavings.
            java.time.Duration savings = rules.getDaylightSavings(Time0);
            Time = Time0.plus(savings);
            if (rules.isDaylightSavings(Time)) {
                IsOk = bool.NO;
                ErrorMsg = \"failed to correct for DST\";
            } else {
                IsOk = bool.YES;
                ErrorMsg = \"\";
            }
        } else {
            IsOk = bool.YES;
            Time = Time0;
            ErrorMsg = \"\";
        }
    } catch (java.lang.Exception e) {
        IsOk = bool.NO;
        Time = java.time.Instant.MIN; // Dummy value.
        if (e.getMessage() != null) {
            ErrorMsg = e.getMessage();
        } else {
            ErrorMsg = \"\";
        }
    }
").

:- func maybe_dst_to_int(maybe(dst)) = int.

maybe_dst_to_int(M) = N :-
    (
        M = yes(DST), DST = daylight_time,
        N = 1
    ;
        M = yes(DST), DST = standard_time,
        N = 0
    ;
        M = no,
        N = -1
    ).

%---------------------------------------------------------------------------%

asctime(TM) = Str :-
    TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, _YD, WD, _DST),
    Str = string.format("%.3s %.3s%3d %.2d:%.2d:%.2d %d\n",
        [s(weekday_name(WD)), s(month_name(Mnt)), i(MD), i(Hrs),
        i(Min), i(Sec), i(1900 + Yr)]).

:- func weekday_name(int) = string.

weekday_name(N) = Name :-
    ( if weekday_name(N, Name0) then
        Name = Name0
    else
        error("time: weekday_name")
    ).

:- pred weekday_name(int::in, string::out) is semidet.

weekday_name(0, "Sun").
weekday_name(1, "Mon").
weekday_name(2, "Tue").
weekday_name(3, "Wed").
weekday_name(4, "Thu").
weekday_name(5, "Fri").
weekday_name(6, "Sat").

:- func month_name(int) = string.

month_name(N) = Name :-
    ( if month_name(N, Name0) then
        Name = Name0
    else
        error("time: month_name")
    ).

:- pred month_name(int::in, string::out) is semidet.

month_name(0, "Jan").
month_name(1, "Feb").
month_name(2, "Mar").
month_name(3, "Apr").
month_name(4, "May").
month_name(5, "Jun").
month_name(6, "Jul").
month_name(7, "Aug").
month_name(8, "Sep").
month_name(9, "Oct").
month_name(10, "Nov").
month_name(11, "Dec").

%---------------------------------------------------------------------------%

    % For io.m.
    %
:- func construct_time_t(time_t_rep) = time_t.

:- pragma foreign_export("C", construct_time_t(in) = out,
    "ML_construct_time_t").
:- pragma foreign_export("C#", construct_time_t(in) = out,
    "ML_construct_time_t").
:- pragma foreign_export("Java", construct_time_t(in) = out,
    "ML_construct_time_t").

construct_time_t(T) = time_t(T).

%---------------------------------------------------------------------------%
