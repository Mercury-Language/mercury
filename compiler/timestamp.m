%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2002 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% File: timestamp.m
% Main author: stayl
%
% Timestamp representation for smart recompilation.
%-----------------------------------------------------------------------------%
:- module timestamp.

:- interface.

:- import_module time.

	% A `timestamp' is similar to a `time_t' except that
	% timestamps are system independent. A timestamp string
	% (obtained using time__timestamp_to_string) written on
	% one system can be read on any other system.
	% Comparison of values of type `timestamp' (via compare/3)
	% is equivalent to comparison of the times represented.
:- type timestamp.

	% time__time_t_to_timestamp(Time) = Timestamp:
	%	Converts the calendar time value `Time' into a timestamp.
	%	Equivalent to `gm_time_to_timestamp(gmtime(Time))'.
:-func time_t_to_timestamp(time_t) = timestamp.

	% time__timestamp_to_string(Timestamp) = String:
	%	Converts `Timestamp' into a string with format
	%	"yyyy-mm-dd hh:mm:ss", expressed as UTC. 
:- func timestamp_to_string(timestamp) = string.

	% time__string_to_timestamp(String) = Timestamp:
	%	Converts a string formatted as "yyyy-mm-dd hh:mm:ss",
	%	into a timestamp. Fails if the string does not have the
	%	correct format.
:- func string_to_timestamp(string) = timestamp is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, std_util, string.

	% A timestamp is a string formatted as "yyyy-mm-dd hh:mm:ss"
	% representing a time expressed as UTC (Universal Coordinated Time).
:- type timestamp == string.

:- pragma c_header_code("#include <time.h>").

time_t_to_timestamp(Time) = gmtime_to_timestamp(time__gmtime(Time)).

:- func gmtime_to_timestamp(tm) = timestamp.

gmtime_to_timestamp(tm(Year, Month, MD, Hrs, Min, Sec, YD, WD, DST)) =
	gmtime_to_timestamp(Year, Month, MD, Hrs, Min, Sec,
		YD, WD, maybe_dst_to_int(DST)).

:- func gmtime_to_timestamp(int, int, int, int,
		int, int, int, int, int) = timestamp.

:- pragma foreign_decl("C", "
	#include ""mercury_string.h""
	#include <time.h>
").
:- pragma foreign_proc("C",
	gmtime_to_timestamp(Yr::in, Mnt::in, MD::in, Hrs::in, Min::in, Sec::in,
		YD::in, WD::in, N::in) = (Result::out),
	[will_not_call_mercury, promise_pure],
"{
	int size;
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

	size = sizeof ""yyyy-mm-dd hh:mm:ss"";
	MR_allocate_aligned_string_msg(Result, size - 1, MR_PROC_LABEL);

	strftime(Result, size, ""%Y-%m-%d %H:%M:%S"", &t);
}").

:- pragma foreign_proc("MC++",
	gmtime_to_timestamp(_Yr::in, _Mnt::in, _MD::in, _Hrs::in, _Min::in,
		_Sec::in, _YD::in, _WD::in, _N::in) = (_Result::out),
	[will_not_call_mercury, promise_pure],
"{
	mercury::runtime::Errors::SORRY(""foreign code for this function"");
}").

:- func maybe_dst_to_int(maybe(dst)) = int.

maybe_dst_to_int(M) = N :-
	( M = yes(DST), DST = daylight_time,
		N = 1
	; M = yes(DST), DST = standard_time,
		N = 0
	; M = no,
		N = -1
	).

timestamp_to_string(Timestamp) = Timestamp.

string_to_timestamp(Timestamp) = Timestamp :-
	string__length(Timestamp) `with_type` int =
		string__length("yyyy-mm-dd hh:mm:ss"),

	string__to_int(string__unsafe_substring(Timestamp, 0, 4), _),

	string__unsafe_index(Timestamp, 4, '-'),

	string__to_int(string__unsafe_substring(Timestamp, 5, 2), Month),
	Month >= 1,
	Month =< 12,

	string__unsafe_index(Timestamp, 7, '-'),

	string__to_int(string__unsafe_substring(Timestamp, 8, 2), Day),
	Day >= 1,
	Day =< 31,

	string__unsafe_index(Timestamp, 10, ' '),

	string__to_int(string__unsafe_substring(Timestamp, 11, 2), Hour),
	Hour >= 0,
	Hour =< 23,

	string__unsafe_index(Timestamp, 13, ':'),

	string__to_int(string__unsafe_substring(Timestamp, 14, 2), Minute),
	Minute >= 0,
	Minute =< 59,

	string__unsafe_index(Timestamp, 16, ':'),

	string__to_int(string__unsafe_substring(Timestamp, 17, 2), Second),
	Second >= 0,
	Second =< 61.	% Seconds 60 and 61 are for leap seconds.

