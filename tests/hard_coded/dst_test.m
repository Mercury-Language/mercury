%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% In Melbourne, daylight savings
%   Starts: 2 am EST (Eastern Standard Time) on 26 October 2003
%   Ends:   2 am EST (Eastern Standard Time) on 28 March   2004
% At start of daylight saving period, move clock forward one hour
% At end of daylight saving period, move clock back one hour
%

:- module dst_test.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module maybe, time.

main -->
	difftest,
	io__nl,
	local_vs_gm_test,
	io__nl.

:- pred difftest(io__state::di, io__state::uo) is det.

difftest -->
	% Sunday 2003-10-26 01:30:00
	{ BeforeStart = mktime(tm(103, 9, 26, 1, 30, 0, 298, 0,
				yes(standard_time))) },
	% Sunday 2003-10-26 03:30:00
	{ AfterStart = mktime(tm(103, 9, 26, 3, 30, 0, 298, 0,
				yes(daylight_time))) },
	% difference should be 1 hour
	( { difftime(AfterStart, BeforeStart) = 3600.0 } ->
		io__write_string("start DST succeeded\n")
	;
		io__write_string("start DST failed\n")
	),

	% Sunday 2004-02-28 02:30:00 (occurs twice)
	{ BeforeEnd = mktime(tm(104, 2, 28, 2, 30, 0, 87, 0,
				yes(daylight_time))) },
	{ AfterEnd = mktime(tm(104, 2, 28, 2, 30, 0, 87, 0,
				yes(standard_time))) },
	% difference should be 1 hour
	( { difftime(AfterEnd, BeforeEnd) = 3600.0 } ->
		io__write_string("end DST succeeded\n")
	;
		io__write_string("end DST failed\n")
	).

:- pred local_vs_gm_test(io__state::di, io__state::uo) is det.

local_vs_gm_test -->
	local_vs_gm_test(tm(103, 9, 26, 1, 59, 0, 298, 0, yes(standard_time))),
	local_vs_gm_test(tm(103, 9, 26, 3, 0, 0, 298, 0, yes(daylight_time))),
	local_vs_gm_test(tm(103, 9, 26, 3, 1, 0, 298, 0, yes(daylight_time))),
	io__nl,

	local_vs_gm_test(tm(104, 2, 28, 1, 59, 0, 87, 0, yes(daylight_time))),
	local_vs_gm_test(tm(104, 2, 28, 2, 0, 0, 87, 0, yes(daylight_time))),
	local_vs_gm_test(tm(104, 2, 28, 2, 1, 0, 87, 0, yes(daylight_time))),
	io__nl,

	local_vs_gm_test(tm(104, 2, 28, 2, 59, 0, 87, 0, yes(daylight_time))),
	local_vs_gm_test(tm(104, 2, 28, 2, 0, 0, 87, 0, yes(standard_time))),
	local_vs_gm_test(tm(104, 2, 28, 2, 1, 0, 87, 0, yes(standard_time))),
	io__nl,

	local_vs_gm_test(tm(104, 2, 28, 2, 59, 0, 87, 0, yes(standard_time))),
	local_vs_gm_test(tm(104, 2, 28, 3, 0, 0, 87, 0, yes(standard_time))),
	local_vs_gm_test(tm(104, 2, 28, 3, 1, 0, 87, 0, yes(standard_time))).

:- pred local_vs_gm_test(tm::in, io__state::di, io__state::uo) is det.

local_vs_gm_test(TM) -->
	{ Time = mktime(TM) },
	io__write_string("Local:\t"),
	io__write_string(asctime(localtime(Time))),
	io__write_string("GMT:\t"),
	io__write_string(asctime(gmtime(Time))).

