%-----------------------------------------------------------------------------%
% Originally written in 1999 by Tomas By <T.By@dcs.shef.ac.uk>
% "Feel free to use this code or parts of it any way you want."
%
% Some portions are Copyright (C) 1999-2002 The University of Melbourne.
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
	% 
:- pred time__clock(clock_t, io__state, io__state).
:- mode time__clock(out, di, uo) is det.

	% time__clocks_per_sec:
	%	Returns the number of clock ticks per second.
	%
:- func time__clocks_per_sec = int.

	% time__time(Result, IO_state, IO_state):
	%	Returns the current (simple) calendar time.
	%	If the time cannot be obtained, this procedure
	%	will throw a time_error exception.
	%
:- pred time__time(time_t, io__state, io__state).
:- mode time__time(out, di, uo) is det.

	% time__times(ProcessorTime, ElapsedRealTime, IO_state, IO_state)
	%	(POSIX)
	%	Returns the processor time information in the `tms'
	%	value, and the elapsed real time relative to an
	%	arbitrary base in the `clock_t' value.
	%	If the time cannot be obtained, this procedure
	%	will throw a time_error exception.
	%
	%	On non-POSIX systems that do not support this functionality,
	%	this procedure may simply always throw an exception.
	%
:- pred time__times(tms, clock_t, io__state, io__state).
:- mode time__times(out, out, di, uo) is det.

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

:- import_module int, exception.

% XXX The assumption that a C `time_t' can fit into a Mercury `MR_Integer'
% is not very portable.
:- type time_t == int.

:- pragma c_header_code("
	#include <time.h>
	#ifdef MR_HAVE_SYS_TYPES_H
		#include <sys/types.h>
	#endif
	#ifdef MR_HAVE_SYS_TIMES_H
		#include <sys/times.h>
	#endif

	#define update_io(r_src, r_dest)	((r_dest) = (r_src))

	#include ""mercury_string.h"" /* for MR_make_aligned_string_copy() */
").

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
	update_io(IO0, IO);
}").
time__c_clock(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("time__c_clock") }.

%-----------------------------------------------------------------------------%

%:- func time__clocks_per_sec = int.

time__clocks_per_sec = Val :-
	time__c_clocks_per_sec(Val).

:- pred time__c_clocks_per_sec(int).
:- mode time__c_clocks_per_sec(out) is det.

:- pragma foreign_proc("C", time__c_clocks_per_sec(Ret::out),
	[will_not_call_mercury, promise_pure],
"{
	Ret = (MR_Integer) CLOCKS_PER_SEC;
}").
time__c_clocks_per_sec(_) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("time__c_clocks_per_sec").

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
	update_io(IO0, IO);
}").
time__c_times(_, _, _, _, _) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("time__c_times") }.


%-----------------------------------------------------------------------------%

%:- pred time__time(time_t, io__state, io__state).
%:- mode time__time(out, di, uo) is det.

time__time(Result, IO0, IO) :-
	time__c_time(Ret, IO0, IO),
	( Ret = -1 ->
		throw(time_error("can't get time value"))
	;
		Result = Ret
	).

:- pred time__c_time(int, io__state, io__state).
:- mode time__c_time(out, di, uo) is det.

:- pragma foreign_proc("C",
	time__c_time(Ret::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure, tabled_for_io],
"{
	Ret = (MR_Integer) time(NULL);
	update_io(IO0, IO);
}").
time__c_time(_) -->
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	{ private_builtin__sorry("time__c_time") }.

%-----------------------------------------------------------------------------%

%:- func time__difftime(time_t, time_t) = float.

time__difftime(T1, T0) = Diff :-
	time__c_difftime(T1, T0, Diff).

:- pred time__c_difftime(int, int, float).
:- mode time__c_difftime(in, in, out) is det.

:- pragma foreign_proc("C",
	time__c_difftime(T1::in, T0::in, Diff::out),
	[will_not_call_mercury, promise_pure],
"{
	Diff = (MR_Float) difftime((time_t) T1, (time_t) T0);
}").
time__c_difftime(_, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("time__c_difftime").

%-----------------------------------------------------------------------------%

%:- func time__localtime(time_t) = tm.

time__localtime(Time) = TM :-
	time__c_localtime(Time, Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, N),
	TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, int_to_maybe_dst(N)).

:- pred time__c_localtime(int, int, int, int, int, int, int, int, int, int).
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
time__c_localtime(_, _, _, _, _, _, _, _, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("time__c_localtime").


%:- func time__gmtime(time_t) = tm.

time__gmtime(Time) = TM :-
	time__c_gmtime(Time, Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, N),
	TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, int_to_maybe_dst(N)).

:- pred time__c_gmtime(int, int, int, int, int, int, int, int, int, int).
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
time__c_gmtime(_, _, _, _, _, _, _, _, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("time__c_gmtime").

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

time__mktime(TM) = Time :-
	TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, DST),
	time__c_mktime(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD,
		maybe_dst_to_int(DST), Time).

:- pred time__c_mktime(int, int, int, int, int, int, int, int, int, int).
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

	Time = (MR_Integer) mktime(&t);
}").
time__c_mktime(_, _, _, _, _, _, _, _, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("time__c_mktime").

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
	TM = tm(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD, DST),
	time__c_asctime(Yr, Mnt, MD, Hrs, Min, Sec, YD, WD,
		maybe_dst_to_int(DST), Str).

:- pred time__c_asctime(int, int, int, int, int, int, int, int, int, string).
:- mode time__c_asctime(in, in, in, in, in, in, in, in, in, out) is det.

:- pragma foreign_proc("C",
	time__c_asctime(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
		YD::in, WD::in, N::in, Str::out),
	[will_not_call_mercury, promise_pure],
"{
	struct tm t;
	char* s;

	t.tm_sec = Sec;
	t.tm_min = Min;
	t.tm_hour = Hrs;
	t.tm_mon = Mnt;
	t.tm_year = Yr;
	t.tm_wday = WD;
	t.tm_mday = MD;
	t.tm_yday = YD;
	t.tm_isdst = N;

	s = asctime(&t);

	MR_make_aligned_string_copy(Str, s);
}").
time__c_asctime(_, _, _, _, _, _, _, _, _, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("time__c_asctime").

%-----------------------------------------------------------------------------%

%:- func time__ctime(time_t) = string.

time__ctime(Time) = Str :-
	time__c_ctime(Time, Str).

:- pred time__c_ctime(int, string).
:- mode time__c_ctime(in, out) is det.

:- pragma foreign_proc("C",
	time__c_ctime(Time::in, Str::out),
	[will_not_call_mercury, promise_pure],
"{
	char *s;
	time_t t;

	t = Time;

	s = ctime(&t);

	MR_make_aligned_string_copy(Str, s);
}").
time__c_ctime(_, _) :-
	% This version is only used for back-ends for which there is no
	% matching foreign_proc version.
	private_builtin__sorry("time__c_ctime").

%-----------------------------------------------------------------------------%
:- end_module time.
%-----------------------------------------------------------------------------%
