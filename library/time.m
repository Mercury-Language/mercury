%-----------------------------------------------------------------------------%
% Originally written in 1999 by Tomas By <T.By@dcs.shef.ac.uk>
% "Feel free to use this code or parts of it any way you want."
%
% Some portions are Copyright (C) 1999-2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: time.m.
% Main authors: Tomas By <T.By@dcs.shef.ac.uk>, fjh
% Stability: medium
%
% Time functions.
%
%-----------------------------------------------------------------------------%

:- module time.

:- interface.

:- use_module io.
:- import_module std_util.

	% The `clock_t' type represents times measured in clock ticks.
	% NOTE: the unit used for a value of this type depends on whether it was
	% returned by `time__clock' or `time__times'.  See the comments on these
	% predicates below.
:- type clock_t == int.

	% The `tms' type holds information about the amount of processor
	% time that a process and its child processes have consumed.
:- type tms --->
        tms(clock_t,    % tms_utime: user time
            clock_t,    % tms_stime: system time
            clock_t,    % tms_cutime: user time of children
            clock_t).   % tms_cstime: system time of children

	% The `time_t' type is an abstract type that represents
	% calendar times.
:- type time_t.

	% The `tm' type is a concrete type that represents calendar
	% times, broken down into their constituent components.
	% Comparison (via compare/3) of `tm' values whose `tm_dst'
	% components are identical is equivalent to comparison of
	% the times those `tm' values represent.
:- type tm
	---> tm(
		tm_year :: int,		% Year (number since 1900)
		tm_mon :: int,		% Month (number since January, 0-11)
		tm_mday :: int,		% MonthDay (1-31)
		tm_hour :: int,		% Hours (after midnight, 0-23)
		tm_min :: int,		% Minutes (0-59)
		tm_sec :: int,		% Seconds (0-61)
					% (60 and 61 are for leap seconds)
		tm_yday :: int,		% YearDay (number since Jan 1st, 0-365)
		tm_wday :: int,		% WeekDay (number since Sunday, 0-6)
		tm_dst :: maybe(dst)	% IsDST (is DST in effect?)
	).

:- type dst
	--->	standard_time	% no, DST is not in effect
	;	daylight_time.	% yes, DST is in effect

	% Some of the procedures in this module throw this type
	% as an exception if they can't obtain a result.
:- type time_error --->
	time_error(string).	% Error message

%-----------------------------------------------------------------------------%

	% time__clock(Result, IO_state, IO_state):
	%	Returns the elapsed processor time (number of clock
	%	ticks). The base time is arbitrary but doesn't change
	%	within a single process.
	%	If the time cannot be obtained, this procedure
	%	will throw a time_error exception.
	%	To obtain a time in seconds, divide Result by
	%	`time__clocks_per_sec'.
	% 
:- pred time__clock(clock_t, io__state, io__state).
:- mode time__clock(out, di, uo) is det.

	% time__clocks_per_sec:
	%	Returns the number of "clocks" per second as defined by
	%	CLOCKS_PER_SEC.  A `clock_t' value returned by `time__clock' can
	%	be divided by this value to obtain a time in seconds.
	%
:- func time__clocks_per_sec = int.

%-----------------------------------------------------------------------------%

	% time__time(Result, IO_state, IO_state):
	%	Returns the current (simple) calendar time.
	%	If the time cannot be obtained, this procedure
	%	will throw a time_error exception.
	%
:- pred time__time(time_t, io__state, io__state).
:- mode time__time(out, di, uo) is det.

%-----------------------------------------------------------------------------%

	% time__times(ProcessorTime, ElapsedRealTime, IO_state, IO_state)
	%	(POSIX)
	%	Returns the processor time information in the `tms'
	%	value, and the elapsed real time relative to an
	%	arbitrary base in the `clock_t' value.
	%	To obtain a time in seconds, divide the result by
	%	`time__clk_tck'.
	%	If the time cannot be obtained, this procedure
	%	will throw a time_error exception.
	%
	%	On non-POSIX systems that do not support this functionality,
	%	this procedure may simply always throw an exception.
	%
:- pred time__times(tms, clock_t, io__state, io__state).
:- mode time__times(out, out, di, uo) is det.

	% time__clk_tck:
	%	Returns the number of "clock ticks" per second as defined by
	%	sysconf(_SC_CLK_TCK).  A `clock_t' value returned by
	%	`time__times' can be divided by this value to obtain a time in
	%	seconds.
	%
	%	On non-POSIX systems that do not support this functionality,
	%	this procedure may simply always throw an exception.
	%
:- func time__clk_tck = int.

%-----------------------------------------------------------------------------%

	% time__difftime(Time1, Time0) = Diff:
	%	Computes the number of seconds elapsed between
	%	`Time1' and `Time0'.
	%
:- func time__difftime(time_t, time_t) = float.

	% time__localtime(Time) = TM:
	%	Converts the calendar time `Time' to a broken-down
	%	representation, expressed relative to the user's
	%	specified time zone.
	%
:- func time__localtime(time_t) = tm.

	% time__gmtime(Time) = TM:
	%	Converts the calendar time `Time' to a broken-down
	%	representation, expressed as UTC (Universal Coordinated Time).
	%
:- func time__gmtime(time_t) = tm.

	% time__mktime(TM) = Time:
	%	Converts the broken-down local time value to calendar time.
	%	It also normalises the value by filling in day of
	%	week and day of year based on the other components.
	%
:- func time__mktime(tm) = time_t.

%-----------------------------------------------------------------------------%

	% time__asctime(TM) = String:
	% 	Converts the broken-down time value `TM' to a string
	% 	in a standard format.
	%
:- func time__asctime(tm) = string.

	% time__ctime(Time) = String:
	% 	Converts the calendar time value `Time' to a string
	% 	in a standard format.
	%	(ie same as "asctime (localtime (<time>))")
	%
:- func time__ctime(time_t) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, exception, list, require, string.

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

	#define MR_update_io(r_src, r_dest)	((r_dest) = (r_src))

	#include ""mercury_string.h"" /* for MR_make_aligned_string_copy() */
").

% We use a no-tag wrapper type for time_t, rather than defining it as an
% equivalence type or just using a d.u./pragma foreign_type directly,
% to avoid the following problems:
%
%	- type errors in --high-level-code grades, due to the caller seeing
%	  the abstract type, but the callee seeing the equivalence type
%	  definition or the foreign_type definition.
%
%	- users can't define instance declarations for abstract equiv. types.
%
:- type time_t ---> time_t(time_t_rep).

:- type time_t_rep ---> time_t_rep(c_pointer).
:- pragma foreign_type("C", time_t_rep, "time_t")
	where comparison is compare_time_t_reps.

	% The System.DateTime will hold the value in UTC.
:- pragma foreign_type(il, time_t_rep, "valuetype [mscorlib]System.DateTime")
	where comparison is compare_time_t_reps.

:- pragma foreign_type("Java", time_t_rep, "java.util.Date")
	where comparison is compare_time_t_reps.

:- pred compare_time_t_reps(comparison_result::uo,
		time_t_rep::in, time_t_rep::in) is det.
compare_time_t_reps(Result, X, Y) :-
	compare(Result, difftime(time_t(X), time_t(Y)), 0.0).

%-----------------------------------------------------------------------------%

%:- pred time__clock(clock_t, io__state, io__state).
%:- mode time__clock(out, di, uo) is det.

time__clock(Result, IO0, IO) :-
	time__c_clock(Ret, IO0, IO),
	( Ret = -1 ->
		throw(time_error("can't get clock value"))
	;
		Result = Ret
	).

:- pred time__c_clock(int, io__state, io__state).
:- mode time__c_clock(out, di, uo) is det.

:- pragma foreign_proc("C", time__c_clock(Ret::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	Ret = (MR_Integer) clock();
	MR_update_io(IO0, IO);
}").
/* XXX need to add System.dll to the references list.
:- pragma foreign_proc("C#", time__c_clock(Ret::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	// XXX Ticks is long in .NET!
	Ret = (int) System.Diagnostics.Process.GetCurrentProcess
		.UserProcessorTime.Ticks;
}").
*/
:- pragma foreign_proc("Java", time__c_clock(Ret::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	if (mercury.runtime.Native.isAvailable()) {
		Ret = mercury.runtime.Native.clock();
	} else {
		throw new java.lang.RuntimeException(
				""time__clock is not implemented "" +
				""in pure Java.  Native dynamic link "" +
				""library is required."");
	}
").

%-----------------------------------------------------------------------------%

%:- func time__clocks_per_sec = int.

:- pragma foreign_proc("C", time__clocks_per_sec = (Ret::out),
	[will_not_call_mercury, promise_pure],
"{
	Ret = (MR_Integer) CLOCKS_PER_SEC;
}").
:- pragma foreign_proc("C#", time__clocks_per_sec = (Ret::out),
	[will_not_call_mercury, promise_pure],
"{
	// TicksPerSecond is guaranteed to be 10,000,000
	Ret = (int) System.TimeSpan.TicksPerSecond;
}").
:- pragma foreign_proc("Java", time__clocks_per_sec = (Ret::out),
	[will_not_call_mercury, promise_pure],
"
	if (mercury.runtime.Native.isAvailable()) {
		Ret = mercury.runtime.Native.clocks_per_sec();
	} else {
		throw new java.lang.RuntimeException(
				""time__clocks_per_sec is not implemented "" +
				""in pure Java.  Native dynamic link "" +
				""library is required."");
	}
").

%-----------------------------------------------------------------------------%

%:- pred time__times(tms, clock_t, io__state, io__state).
%:- mode time__times(out, out, di, uo) is det.

time__times(Tms, Result, IO0, IO) :-
	time__c_times(Ret, Ut, St, CUt, CSt, IO0, IO),
	( Ret = -1 ->
		throw(time_error("can't get times value"))
	;
		Tms = tms(Ut, St, CUt, CSt),
		Result = Ret
	).

:- pred time__c_times(int, int, int, int, int, io__state, io__state).
:- mode time__c_times(out, out, out, out, out, di, uo) is det.

:- pragma foreign_proc("C",
	time__c_times(Ret::out, Ut::out, St::out, CUt::out,
                               CSt::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
#ifdef MR_HAVE_POSIX_TIMES
	struct tms t;

	Ret = (MR_Integer) times(&t);

	Ut = (MR_Integer) t.tms_utime;
	St = (MR_Integer) t.tms_stime;
	CUt = (MR_Integer) t.tms_cutime;
	CSt = (MR_Integer) t.tms_cstime;
#else
	Ret = -1;
#endif
	MR_update_io(IO0, IO);
}").

:- pragma foreign_proc("Java",
	time__c_times(Ret::out, Ut::out, St::out, CUt::out,
                               CSt::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	if (mercury.runtime.Native.isAvailable()) {
		int[] times = mercury.runtime.Native.times();
		if (times != null) {
			Ret	= times[0];
			Ut	= times[1];
			St	= times[2];
			CUt	= times[3];
			CSt	= times[4];
		} else {
			throw new java.lang.RuntimeException(
					""time_times failed to construct "" +
					""integer array"");
		}
	} else {
		throw new java.lang.RuntimeException(
				""time__times is not implemented "" +
				""in pure Java.  Native dynamic link "" +
				""library is required."");
	}
").

%-----------------------------------------------------------------------------%

time__clk_tck = Ret :-
	Ret0 = time__c_clk_tck,
	( Ret0 = -1 ->
		throw(time_error("can't get clk_tck value"))
	;
		Ret = Ret0
	).

:- func time__c_clk_tck = int.

:- pragma foreign_proc("C",
	time__c_clk_tck = (Ret::out),
	[will_not_call_mercury, promise_pure],
"{
#if defined(MR_HAVE_SYSCONF) && defined(_SC_CLK_TCK)
	Ret = (MR_Integer) sysconf(_SC_CLK_TCK);
#elif defined(CLK_TCK)
	/*
	** If sysconf is not available, try using the (obsolete) macro CLK_TCK.
	*/
	Ret = (MR_Integer) CLK_TCK;
#else
	Ret = -1;
#endif
}").
time__c_clk_tck = -1.	% default is to throw an exception.

:- pragma foreign_proc("C#", time__clk_tck = (Ret::out),
	[will_not_call_mercury, promise_pure],
"{
	// TicksPerSecond is guaranteed to be 10,000,000
	Ret = (int) System.TimeSpan.TicksPerSecond;
}").
% XXX Java implementation still to come, will require some native code.

%-----------------------------------------------------------------------------%

%:- pred time__time(time_t, io__state, io__state).
%:- mode time__time(out, di, uo) is det.

time__time(Result, IO0, IO) :-
	time__c_time(Ret, IO0, IO),
	( time__time_t_is_invalid(Ret) ->
		throw(time_error("can't get time value"))
	;
		Result = time_t(Ret)
	).

:- pred time__c_time(time_t_rep, io__state, io__state).
:- mode time__c_time(out, di, uo) is det.

:- pragma foreign_proc("C",
	time__c_time(Ret::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	Ret = time(NULL);
	MR_update_io(IO0, IO);
}").
:- pragma foreign_proc("C#",
	time__c_time(Ret::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	Ret = System.DateTime.UtcNow;
}").
:- pragma foreign_proc("Java",
	time__c_time(Ret::out, _IO0::di, _IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"
	Ret = new java.util.Date();
").

:- pred time__time_t_is_invalid(time_t_rep).
:- mode time__time_t_is_invalid(in) is semidet.

:- pragma foreign_proc("C",
	time__time_t_is_invalid(Val::in),
	[will_not_call_mercury, promise_pure],
"{
	SUCCESS_INDICATOR = (Val == -1);
}").
:- pragma foreign_proc("C#",
	time__time_t_is_invalid(_Val::in),
	[will_not_call_mercury, promise_pure],
"{
	SUCCESS_INDICATOR = false;
}").
:- pragma foreign_proc("Java",
	time__time_t_is_invalid(_Val::in),
	[will_not_call_mercury, promise_pure],
"
	succeeded = false;
").


%-----------------------------------------------------------------------------%

%:- func time__difftime(time_t, time_t) = float.

time__difftime(time_t(T1), time_t(T0)) = Diff :-
	time__c_difftime(T1, T0, Diff).

:- pred time__c_difftime(time_t_rep, time_t_rep, float).
:- mode time__c_difftime(in, in, out) is det.

:- pragma foreign_proc("C",
	time__c_difftime(T1::in, T0::in, Diff::out),
	[will_not_call_mercury, promise_pure],
"{
	Diff = (MR_Float) difftime(T1, T0);
}").
:- pragma foreign_proc("C#",
	time__c_difftime(T1::in, T0::in, Diff::out),
	[will_not_call_mercury, promise_pure],
"{
	System.TimeSpan span;
	span = T1 - T0;
	Diff = span.TotalSeconds;
}").
:- pragma foreign_proc("Java",
	time__c_difftime(T1::in, T0::in, Diff::out),
	[will_not_call_mercury, promise_pure],
"
	Diff = (double) (T1.getTime() - T0.getTime()) / 1000;
").

%-----------------------------------------------------------------------------%

%:- func time__localtime(time_t) = tm.

time__localtime(time_t(Time)) = TM :-
	time__c_localtime(Time, Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, N),
	TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, int_to_maybe_dst(N)).

:- pred time__c_localtime(time_t_rep, int, int, int, int, int, int,
		int, int, int).
:- mode time__c_localtime(in, out, out, out, out, out, out,
		out, out, out) is det.

:- pragma foreign_proc("C",
	time__c_localtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
		Min::out, Sec::out, YD::out, WD::out, N::out),
	[will_not_call_mercury, promise_pure],
"{
	struct tm* p;
	time_t t;

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
}").
:- pragma foreign_proc("C#",
	time__c_localtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
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
	time__c_localtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
		Min::out, Sec::out, YD::out, WD::out, N::out),
	[will_not_call_mercury, promise_pure],
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
				""invalid DAY_OF_WEEK in time__c_local_time"");
	}

	if (gc.getTimeZone().inDaylightTime(Time)) {
		N = 1;
	} else {
		N = 0;
	}
"). % time__c_local_time


%:- func time__gmtime(time_t) = tm.

time__gmtime(time_t(Time)) = TM :-
	time__c_gmtime(Time, Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, N),
	TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, int_to_maybe_dst(N)).

:- pred time__c_gmtime(time_t_rep, int, int, int, int, int,
			int, int, int, int).
:- mode time__c_gmtime(in, out, out, out, out, out,
			out, out, out, out) is det.

:- pragma foreign_proc("C",
	time__c_gmtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
		Min::out, Sec::out, YD::out, WD::out, N::out),
	[will_not_call_mercury, promise_pure],
"{
	struct tm* p;
	time_t t;

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
	time__c_gmtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
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
	time__c_gmtime(Time::in, Yr::out, Mnt::out, MD::out, Hrs::out,
		Min::out, Sec::out, YD::out, WD::out, N::out),
	[will_not_call_mercury, promise_pure],
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
				""invalid DAY_OF_WEEK in time__c_gmtime"");
	}

	N = 0;
"). % time__c_gmtime

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

%:- func time__mktime(tm) = time_t.

time__mktime(TM) = time_t(Time) :-
	TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, DST),
	time__c_mktime(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD,
		maybe_dst_to_int(DST), Time).

:- pred time__c_mktime(int, int, int, int, int, int, int, int, int, time_t_rep).
:- mode time__c_mktime(in, in, in, in, in, in, in, in, in, out) is det.

:- pragma foreign_proc("C",
	time__c_mktime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
		YD::in, WD::in, N::in, Time::out),
	[will_not_call_mercury, promise_pure],
 "{
	struct tm t;

	t.tm_sec = Sec;
	t.tm_min = Min;
	t.tm_hour = Hrs;
	t.tm_mon = Mnt;
	t.tm_year = Yr;
	t.tm_wday = WD;
	t.tm_mday = MD;
	t.tm_yday = YD;
	t.tm_isdst = N;

	Time = mktime(&t);
}").
:- pragma foreign_proc("C#",
	time__c_mktime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
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
	time__c_mktime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
		_YD::in, _WD::in, N::in, Time::out),
	[will_not_call_mercury, promise_pure],
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
				""time__mktime: failed to correct for DST"");
		}
	}

	// If the time we constructed is in daylight savings time, but
	// should not be, we need to add the DSTSavings.
	if (N == 0 && gc.getTimeZone().inDaylightTime(Time) == true) {
		Time.setTime(Time.getTime() + getDSTSavings(gc.getTimeZone()));
		if (gc.getTimeZone().inDaylightTime(Time) == true) {
			throw new RuntimeException(
				""time__mktime: failed to correct for DST"");
		}
	}
").

:- pragma foreign_code("Java",
"
/*
** getDSTSavings():
**	This method uses reflection to retrieve and call the getDSTSavings()
**	method for a given TimeZone object.
**
**	The reason we do this is that for Java versions < 1.4, the
**	TimeZone.getDSTSavings() method did not exist, but the
**	SimpleTimeZone.getDSTSavings() method did, and the concrete instance of
**	TimeZone used by GregorianCalender (which is what we use in this
**	module) is a SimpleTimeZone.
**	However, we can't just cast the TimeZone instance to SimpleTimeZone,
**	because for Java versions >= 1.4, GregorianCalender no longer uses a
**	SimpleTimeZone.  So in this case, what we really want is
**	TimeZone.getDSTSavings(), but we can't just put that or the code won't
**	compile for Java versions < 1.4.
**
**	Thus, the solution is to invoke the getDSTSavings() method using
**	reflection, which will cover both cases.
*/
public static int
getDSTSavings(java.util.TimeZone tz) {
	try {
		// Simulate
		//	return tz.getDSTSavings()
		// using reflection.

		return ((java.lang.Integer) (tz.getClass().
				getMethod(""getDSTSavings"", null).
				invoke(tz, null))).intValue();
	}
	catch (java.lang.Exception e) {
		throw new java.lang.RuntimeException(
				""time__c_mktime: Failed to locate "" +
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

%:- func time__asctime(tm) = string.

time__asctime(TM) = Str :-
	TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, _YD, WD, _DST),
	Str = string__format("%.3s %.3s%3d %.2d:%.2d:%.2d %d\n",
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

%:- func time__ctime(time_t) = string.

time__ctime(Time) = asctime(localtime(Time)).

%-----------------------------------------------------------------------------%

% XXX This needs to be in the interface because pragma export doesn't work yet
% on the .NET backend and io.m needs to access this.
:- interface.
:- type time_t_rep.
:- func construct_time_t(time_t_rep) = time_t.
:- implementation.
:- pragma export(construct_time_t(in) = out, "ML_construct_time_t").
construct_time_t(T) = time_t(T).

%-----------------------------------------------------------------------------%
:- end_module time.
%-----------------------------------------------------------------------------%
