%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module lcss.
:- interface.
:- import_module io, file.

:- pred lcss__show_diff(file, file, io__state, io__state).
:- mode lcss__show_diff(in, in, di, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, require, std_util, int, list, char.

:- type lcss --->
		empty
	;	or(lcss, lcss)
	;	prefix(pair(int,int), lcss).

:- type memo == map(pair(int,int), pair(int,lcss)).

:- type single_diff --->
		add(int, pair(int,int))
	;	delete(pair(int,int), int)
	;	change(pair(int,int), pair(int,int)).

:- type diff == list(single_diff).

%-----------------------------------------------------------------------------%

:- pred lcss__create_memo(memo :: out) is det.
lcss__create_memo(Memo) :- 
	map__init(Memo).

:- pred lcss__lookup_memo(int, int, int, lcss, memo, memo).
:- mode lcss__lookup_memo(in, in, out, out, in, out) is semidet.
lcss__lookup_memo(X, Y, Length, Lcss, Memo, Memo) :-
	map__search(Memo, X-Y, Length-Lcss).

:- pred lcss__add_memo(int, int, int, lcss, memo, memo).
:- mode lcss__add_memo(in, in, in, in, in, out) is det.
lcss__add_memo(X, Y, Length, Lcss, MemoIn, MemoOut) :-
	map__det_insert(MemoIn, X-Y, Length-Lcss, MemoOut).

%-----------------------------------------------------------------------------%

lcss__show_diff(File1, File2) -->
%	io__write_string("Finding LCSS...\n"),
	{ lcss__find_lcss(File1, File2, Lcss) },
%	lcss__show_lcss(Lcss),
%	io__write_string("Converting to diff...\n"),
	{ lcss__to_diff(File1, File2, Lcss, Diff) },
%	io__write_string("Writing...\n"),
	lcss__display_diff(File1, File2, Diff).

%-----------------------------------------------------------------------------%

:- pred lcss__show_lcss(lcss :: in, io__state :: di, io__state :: uo) is det.
lcss__show_lcss(empty) -->
	io__write_string("<empty>").
lcss__show_lcss(prefix(X-Y,Lcss)) -->
	io__write_int(X),
	io__write_char('-'),
	io__write_int(Y),
	io__write_char(','),
	lcss__show_lcss(Lcss).
lcss__show_lcss(or(Lcss1, Lcss2)) -->
	io__write_string("or("),
	lcss__show_lcss(Lcss1),
	io__write_char(','),
	lcss__show_lcss(Lcss2),
	io__write_char(')').

%-----------------------------------------------------------------------------%

:- pred lcss__find_lcss(file :: in, file :: in, lcss :: out) is det.
lcss__find_lcss(File1, File2, Lcss) :-
	lcss__create_memo(Memo),
	lcss__find_lcss2(1, 1, File1, File2, _, Lcss, Memo, _).

:- pred lcss__find_lcss2(int, int, file, file, int, lcss, memo, memo).
:- mode lcss__find_lcss2(in, in, in, in, out, out, in, out) is det.
lcss__find_lcss2(X, Y, File1, File2, Length, Lcss) -->
	( lcss__lookup_memo(X, Y, Length0, Lcss0) ->
	    { Length = Length0, Lcss = Lcss0 }
	;
	    { X1 is X+1, Y1 is Y+1 },
	    ( { file__get_line(File1, X, Line1),
	        file__get_line(File2, Y, Line2) } ->
		( { Line1 = Line2 } ->
		    lcss__find_lcss2(X1, Y1, File1, File2, Length1, Lcss1),
		    { Length is Length1+1, Lcss = prefix(X-Y, Lcss1) }
		;
		    lcss__find_lcss2(X, Y1, File1, File2, Length1, Lcss1),
		    lcss__find_lcss2(X1, Y, File1, File2, Length2, Lcss2),
		    ( { Length1 = Length2 } ->
			{ Length = Length1 },
			( { Lcss1 = Lcss2 } ->
			    { Lcss = Lcss1 }
			;
%			    { Lcss = or(Lcss1, Lcss2) }
			    { Lcss = Lcss1 }
			)
		    ; { Length1 < Length2 } ->
			{ Length = Length2, Lcss = Lcss2 }
		    ;
			{ Length = Length1, Lcss = Lcss1 }
		    )
		)
	    ;
		{ Length = 0, Lcss = empty }
	    ),
	    lcss__add_memo(X, Y, Length, Lcss)
	).

%-----------------------------------------------------------------------------%

:- pred lcss__to_diff(file :: in, file :: in, lcss :: in, diff :: out) is det.
lcss__to_diff(File1, File2, Lcss, Diff) :-
	file__get_numlines(File1, Length1),
	file__get_numlines(File2, Length2),
	lcss__to_diff2(1, 1, Length1, Length2, Lcss, Diff).

:- pred lcss__to_diff2(int, int, int, int, lcss, diff).
:- mode lcss__to_diff2(in, in, in, in, in, out) is det.
lcss__to_diff2(X, Y, L1, L2, empty, Diff) :-
	XX1 is X-1, YY1 is Y-1,
	( X > L1 ->
	    ( Y > L2 ->
		Diff = []
	    ;
		Diff = [add(XX1,Y-L2)]
	    )
	;
	    ( Y > L2 ->
		Diff = [delete(X-L1,YY1)]
	    ;
		Diff = [change(X-L1,Y-L2)]
	    )
	).
lcss__to_diff2(X, Y, L1, L2, prefix(X2-Y2, Lcss), Diff) :-
	XX1 is X-1, XX2 is X2-1,
	YY1 is Y-1, YY2 is Y2-1,
	X1 is X2+1, Y1 is Y2+1,
	( X = X2 ->
	    ( Y = Y2 ->
		lcss__to_diff2(X1, Y1, L1, L2, Lcss, Diff)
	    ;
		lcss__to_diff2(X1, Y1, L1, L2, Lcss, Diff1),
		Diff = [ add(XX1,Y-YY2) | Diff1 ]
	    )
	;
	    ( Y = Y2 ->
		lcss__to_diff2(X1, Y1, L1, L2, Lcss, Diff1),
		Diff = [ delete(X-XX2,YY1) | Diff1 ]
	    ;
		lcss__to_diff2(X1, Y1, L1, L2, Lcss, Diff1),
		Diff = [ change(X-XX2,Y-YY2) | Diff1 ]
	    )
	).
lcss__to_diff2(X, Y, L1, L2, or(Lcss1, Lcss2), Diff) :-
	lcss__to_diff2(X, Y, L1, L2, Lcss1, Diff1),
	lcss__to_diff2(X, Y, L1, L2, Lcss2, Diff2),
	Diff = Diff1.

%-----------------------------------------------------------------------------%

:- pred lcss__display_diff(file, file, diff, io__state, io__state).
:- mode lcss__display_diff(in, in, in, di, uo) is det.
lcss__display_diff(_, _, []) --> { true }.
lcss__display_diff(File1, File2, [ SingDiff | Diff ]) -->
	( { SingDiff = add(X, Y1-Y2) },
	    lcss__write_command(X, X, 'a', Y1, Y2),
	    lcss__show_file(File2, '>', Y1, Y2)
	; { SingDiff = delete(X1-X2, Y) },
	    lcss__write_command(X1, X2, 'd', Y, Y),
	    lcss__show_file(File1, '<', X1, X2)
	; { SingDiff = change(X1-X2, Y1-Y2) },
	    lcss__write_command(X1, X2, 'c', Y1, Y2),
	    lcss__show_file(File1, '<', X1, X2),
	    io__write_string("---\n"),
	    lcss__show_file(File2, '>', Y1, Y2)
	),
	lcss__display_diff(File1, File2, Diff).

% lcss__display_diff(_, _, []) --> { true }.
% lcss__display_diff(File1, File2, [ add(X, Y1-Y2) | Diff ]) -->
% 	lcss__write_command(X, X, 'a', Y1, Y2),
% 	lcss__display_diff(File1, File2, Diff).
% lcss__display_diff(File1, File2, [ delete(X1-X2, Y) | Diff ]) -->
% 	lcss__write_command(X1, X2, 'd', Y, Y),
% 	lcss__display_diff(File1, File2, Diff).
% lcss__display_diff(File1, File2, [ change(X1-X2, Y1-Y2) | Diff ]) -->
% 	lcss__write_command(X1, X2, 'c', Y1, Y2),
% 	lcss__display_diff(File1, File2, Diff).

:- pred lcss__show_file(file, char, int, int, io__state, io__state).
:- mode lcss__show_file(in, in, in, in, di, uo) is det.
lcss__show_file(File, Prefix, Low, High) -->
	( { Low > High } ->
	    { true }
	;
	    ( { file__get_line(File, Low, Line) } ->
	    	{ Low1 is Low+1 },
	    	io__write_char(Prefix),
	    	io__write_char(' '),
		io__write_string(Line),
	    	lcss__show_file(File, Prefix, Low1, High)
	    ;
		{ true }
	    )
	).

:- pred lcss__write_command(int, int, char, int, int, io__state, io__state).
:- mode lcss__write_command(in, in, in, in, in, di, uo) is det.
lcss__write_command(X1, X2, C, Y1, Y2) -->
	( { X1 = X2 } ->
	    io__write_int(X1)
	;
	    io__write_int(X1),
	    io__write_char(','),
	    io__write_int(X2)
	),
	io__write_char(C),
	( { Y1 = Y2 } ->
	    io__write_int(Y1)
	;
	    io__write_int(Y1),
	    io__write_char(','),
	    io__write_int(Y2)
	),
	io__write_char('\n').

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
