%-----------------------------------------------------------------------------%
% Originally written in 1999 by Tomas By <T.By@dcs.shef.ac.uk>
% "Feel free to use this code or parts of it any way you want."
%
% Some portions are Copyright (C) 1999 The University of Melbourne.
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
:- type tm --->
        tm(int,          % Seconds (0-60)
           int,          % Minutes (0-59)
           int,          % Hours (after midnight, 0-23)
           int,          % WeekDay (number since Sunday, 0-6)
           int,          % YearDay (number since Jan 1st, 0-365)
           int,          % Month (number since January, 0-11)
           int,          % Year (number since 1900)
           maybe(dst)).  % IsDST (is DST in effect?)

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
	%	Converts the broken-down time value to calendar time.
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

% XXX The assumption that a C `time_t' can fit into a Mercury `Integer'
% is not very portable.
:- type time_t == int.

:- pragma c_header_code("
	#include <time.h>
	#ifdef HAVE_SYS_TYPES_H
		#include <sys/types.h>
	#endif
	#ifdef HAVE_SYS_TIMES_H
		#include <sys/times.h>
	#endif

	#define update_io(r_src, r_dest)	((r_dest) = (r_src))
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

:- pragma c_code(time__c_clock(Ret::out, IO0::di, IO::uo),
	[will_not_call_mercury],
"{
	Ret = (Integer) clock();
	update_io(IO0, IO);
}").

%-----------------------------------------------------------------------------%

%:- func time__clocks_per_sec = int.

time__clocks_per_sec = Val :-
	time__c_clocks_per_sec(Val).

:- pred time__c_clocks_per_sec(int).
:- mode time__c_clocks_per_sec(out) is det.

:- pragma c_code(time__c_clocks_per_sec(Ret::out),
	[will_not_call_mercury],
"{
	Ret = (Integer) CLOCKS_PER_SEC;
}").

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

:- pragma c_code(time__c_times(Ret::out, Ut::out, St::out, CUt::out,
                               CSt::out, IO0::di, IO::uo),
	[will_not_call_mercury],
"{
#ifdef MR_HAVE_POSIX_TIMES
	struct tms t;

	Ret = (Integer) times(&t);

	Ut = (Integer) t.tms_utime;
	St = (Integer) t.tms_stime;
	CUt = (Integer) t.tms_cutime;
	CSt = (Integer) t.tms_cstime;
#else
	Ret = -1
#endif
	update_io(IO0, IO);
}").

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

:- pragma c_code(time__c_time(Ret::out, IO0::di, IO::uo),
	[will_not_call_mercury],
"{
	Ret = (Integer) time(NULL);
	update_io(IO0, IO);
}").

%-----------------------------------------------------------------------------%

%:- func time__difftime(time_t, time_t) = float.

time__difftime(T1, T0) = Diff :-
	time__c_difftime(T1, T0, Diff).

:- pred time__c_difftime(int, int, float).
:- mode time__c_difftime(in, in, out) is det.

:- pragma c_code(time__c_difftime(T1::in, T0::in, Diff::out),
	[will_not_call_mercury],
"{
	Diff = (Float) difftime((time_t) T1, (time_t) T0);
}").

%-----------------------------------------------------------------------------%

%:- func time__localtime(time_t) = tm.

time__localtime(Time) = TM :-
	time__c_localtime(Time, Sec, Min, Hrs, WD, YD, Mnt, Yr, N),
	( N = 0 ->
		DST = yes(standard_time)
	; N > 0 ->
		DST = yes(daylight_time)
	; % N < 0
		DST = no
	),
	TM = tm(Sec, Min, Hrs, WD, YD, Mnt, Yr, DST).

:- pred time__c_localtime(int, int, int, int, int, int, int, int, int).
:- mode time__c_localtime(in, out, out, out, out, out, out, out, out) is det.

:- pragma c_code(time__c_localtime(Time::in, Sec::out, Min::out, Hrs::out,
                                   WD::out, YD::out, Mnt::out,
                                   Yr::out, N::out),
	[will_not_call_mercury],
"{
	struct tm* p;
	time_t t;

	t = Time;

	p = localtime(&t);

	/* XXX do we need to handle the case where p == NULL here? */

	Sec = (Integer) p->tm_sec;
	Min = (Integer) p->tm_min;
	Hrs = (Integer) p->tm_hour;
	Mnt = (Integer) p->tm_mon;
	Yr = (Integer) p->tm_year;
	WD = (Integer) p->tm_wday;
	YD = (Integer) p->tm_yday;
	N = (Integer) p->tm_isdst;
}").

%:- func time__gmtime(time_t) = tm.

time__gmtime(Time) = TM :-
	time__c_gmtime(Time, Sec, Min, Hrs, WD, YD, Mnt, Yr, N),
	( N = 0 ->
		DST = yes(standard_time)
	; N > 0 ->
		DST = yes(daylight_time)
	; % N < 0
		DST = no
	),
	TM = tm(Sec, Min, Hrs, WD, YD, Mnt, Yr, DST).

:- pred time__c_gmtime(int, int, int, int, int, int, int, int, int).
:- mode time__c_gmtime(in, out, out, out, out, out, out, out, out) is det.

:- pragma c_code(time__c_gmtime(Time::in, Sec::out, Min::out, Hrs::out,
                                   WD::out, YD::out, Mnt::out,
                                   Yr::out, N::out),
	[will_not_call_mercury],
"{
	struct tm* p;
	time_t t;

	t = Time;

	p = gmtime(&t);

	/* XXX do we need to handle the case where p == NULL here? */

	Sec = (Integer) p->tm_sec;
	Min = (Integer) p->tm_min;
	Hrs = (Integer) p->tm_hour;
	Mnt = (Integer) p->tm_mon;
	Yr = (Integer) p->tm_year;
	WD = (Integer) p->tm_wday;
	YD = (Integer) p->tm_yday;
	N = (Integer) p->tm_isdst;
}").

%-----------------------------------------------------------------------------%

%:- func time__mktime(tm) = time_t.

time__mktime(TM) = Time :-
	TM = tm(Sec, Min, Hrs, WD, YD, Mnt, Yr, M),
	( M = yes(DST), DST = daylight_time,
		N = 1
	; M = yes(DST), DST = standard_time,
		N = 0
	; M = no,
		N = -1
	),
	time__c_mktime(Sec, Min, Hrs, WD, YD, Mnt, Yr, N, Time).

:- pred time__c_mktime(int, int, int, int, int, int, int, int, int).
:- mode time__c_mktime(in, in, in, in, in, in, in, in, out) is det.

:- pragma c_code(time__c_mktime(Sec::in, Min::in, Hrs::in, WD::in,
                                YD::in, Mnt::in, Yr::in,
                                N::in, Time::out),
	[will_not_call_mercury],
 "{
	struct tm t;

	t.tm_sec = Sec;
	t.tm_min = Min;
	t.tm_hour = Hrs;
	t.tm_mon = Mnt;
	t.tm_year = Yr;
	t.tm_wday = WD;
	t.tm_yday = YD;
	t.tm_isdst = N;

	Time = (Integer) mktime(&t);
}").

%-----------------------------------------------------------------------------%

%:- func time__asctime(tm) = string.

time__asctime(TM) = Str :-
	TM = tm(Sec, Min, Hrs, WD, YD, Mnt, Yr, M),
	( M = yes(DST), DST = daylight_time,
		N = 1
	; M = yes(DST), DST = standard_time,
		N = 0
	; M = no,
		N = -1
	),
	time__c_asctime(Sec, Min, Hrs, WD, YD, Mnt, Yr, N, Str).

:- pred time__c_asctime(int, int, int, int, int, int, int, int, string).
:- mode time__c_asctime(in, in, in, in, in, in, in, in, out) is det.

:- pragma c_code(time__c_asctime(Sec::in, Min::in, Hrs::in, WD::in,
                                 YD::in, Mnt::in, Yr::in, N::in, Str::out),
	[will_not_call_mercury],
"{
	struct tm t;
	char* s;

	t.tm_sec = Sec;
	t.tm_min = Min;
	t.tm_hour = Hrs;
	t.tm_mon = Mnt;
	t.tm_year = Yr;
	t.tm_wday = WD;
	t.tm_yday = YD;
	t.tm_isdst = N;

	s = asctime(&t);

	MR_make_aligned_string_copy(Str, s);
}").

%-----------------------------------------------------------------------------%

%:- func time__ctime(time_t) = string.

time__ctime(Time) = Str :-
	time__c_ctime(Time, Str).

:- pred time__c_ctime(int, string).
:- mode time__c_ctime(in, out) is det.

:- pragma c_code(time__c_ctime(Time::in, Str::out),
	[will_not_call_mercury],
"{
	char *s;
	time_t t;

	t = Time;

	s = ctime(&t);

	MR_make_aligned_string_copy(Str, s);
}").

%-----------------------------------------------------------------------------%
:- end_module time.
%-----------------------------------------------------------------------------%
