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
:- import_module map, require, std_util, int, list, char, array.

:- type lcss == list(pair(int,int)).

:- type single_diff --->
		add(int, pair(int,int))
	;	delete(pair(int,int), int)
	;	change(pair(int,int), pair(int,int)).

:- type diff == list(single_diff).

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
lcss__show_lcss([]) -->
	io__write_string("[]").
lcss__show_lcss([ X-Y | Lcss ]) -->
	io__write_int(X),
	io__write_char('-'),
	io__write_int(Y),
	io__write_char(','),
	lcss__show_lcss(Lcss).

%-----------------------------------------------------------------------------%

:- pred lcss__find_lcss(file :: in, file :: in, lcss :: out) is det.
lcss__find_lcss(File1, File2, Lcss) :-
	file__get_numlines(File1, L1),
	file__get_numlines(File2, L2),
	( L1 >= L2 ->
	    F1 = File1, F2 = File2, N = L1
	;
	    F1 = File2, F2 = File1, N = L2
	),
	lcss__build_matchlist(F1, F2, MatchList),
	lcss__build_thresh(N, MatchList, Thresh, Link),
	lcss__build_lcss(N, Thresh, Link, Lcss).

%-----------------------------------------------------------------------------%

:- pred lcss__build_matchlist(file, file, list(list(int))).
:- mode lcss__build_matchlist(in, in, out) is det.
lcss__build_matchlist(File1, File2, MatchList) :-
	file__to_list(File2, File2list),
	lcss__build_match_map(1, File2list, Map),
	file__to_list(File1, File1list),
	lcss__match_map_to_matchlist(1, File1list, Map, MatchList0),
	array__to_list(MatchList0, MatchList).

:- pred lcss__build_match_map(int, list(string), map(string,list(int))).
:- mode lcss__build_match_map(in, in, out) is det.
lcss__build_match_map(_, [], Map) :-
	map__init(Map).
lcss__build_match_map(N, [S | Ss], MapOut) :-
	N1 is N+1,
	lcss__build_match_map(N1, Ss, MapIn),
	( map__search(MapIn, S, Ns0) ->
	    Ns1 = [ N | Ns0 ]
	;
	    Ns1 = [ N ]
	),
	map__set(MapIn, S, Ns1, MapOut).

:- pred lcss__match_map_to_matchlist(int, list(string), map(string,list(int)), 
		array(list(int))).
:- mode lcss__match_map_to_matchlist(in, in, in, out) is det.
lcss__match_map_to_matchlist(N, [], _, Array) :-
	array__init(1, N, [], Array).
lcss__match_map_to_matchlist(N, [S | Ss], Map, ArrayOut) :-
	N1 is N+1,
	lcss__match_map_to_matchlist(N1, Ss, Map, ArrayIn),
	( map__search(Map, S, Ns0) ->
	    Ns1 = Ns0
	;
	    Ns1 = []
	),
	array__set(ArrayIn, N, Ns1, ArrayOut).

%-----------------------------------------------------------------------------%

:- pred lcss__build_thresh(int, list(list(int)), array(int), 
		array(lcss)).
:- mode lcss__build_thresh(in, in, out, out) is det.
lcss__build_thresh(N, MatchList, Thresh, Link) :-
	N1 is N+1,
	array__init(0, N, N1, Thresh0),
	array__set(Thresh0, 0, 0, Thresh1),
	array__init(1, N, [], Link1),
	lcss__build_thresh2(1, N, MatchList, Thresh1, Link1, Thresh, Link).

:- pred lcss__build_thresh2(int, int, list(list(int)),
		array(int), array(lcss),
		array(int), array(lcss)).
:- mode lcss__build_thresh2(in, in, in, in, in, out, out) is det.
lcss__build_thresh2(I, N, MatchList, Thresh0, Link0, Thresh1, Link1) :-
	( I > N ->
	    Thresh0=Thresh1, Link0=Link1
	; MatchList = [ Matches | MatchRest ] ->
	    I1 is I+1,
	    lcss__build_thresh3(N, I, Matches, Thresh0, Link0, Thresh2, Link2),
	    lcss__build_thresh2(I1, N, MatchRest, Thresh2, Link2,
			Thresh1, Link1)
	;
	    error("Internal fault")
	).

:- pred lcss__build_thresh3(int, int, list(int),
		array(int), array(lcss),
	 	array(int), array(lcss)).
:- mode lcss__build_thresh3(in, in, in, in, in, out, out) is det.
lcss__build_thresh3(_, _, [], Thresh, Link, Thresh, Link).
lcss__build_thresh3(N, I, [ J | Js ], Thresh0, Link0, Thresh1, Link1) :-
	lcss__build_thresh4(0, N, J, K, Thresh0),
	array__lookup(Thresh0, K, ThreshK),
	( J < ThreshK ->
	    array__set(Thresh0, K, J, Thresh2),
	    K1 is K-1,
	    array__lookup(Link0, K1, LinkK1),
	    array__set(Link0, K, [ I-J | LinkK1 ], Link2)
	;
	    Link0 = Link2, Thresh0 = Thresh2
	),
	lcss__build_thresh3(N, I, Js, Thresh2, Link2, Thresh1, Link1).

:- pred lcss__build_thresh4(int, int, int, int, array(int)).
:- mode lcss__build_thresh4(in, in, in, out, in) is det.
lcss__build_thresh4(Lo, Hi, J, K, Thresh) :-
	Width is Hi-Lo,
	( Width < 1 ->
	    error("Internal fault")
	; Width = 1 ->
	    K = Hi
	;
	    Mid is (Lo + Hi)//2,
	    ( J < Mid ->
		lcss__build_thresh4(Mid, Hi, J, K, Thresh)
	    ;
		lcss__build_thresh4(Lo, Mid, J, K, Thresh)
	    )
	).

%-----------------------------------------------------------------------------%

:- pred lcss__build_lcss(int, array(int), array(lcss), lcss).
:- mode lcss__build_lcss(in, in, in, out) is det.
lcss__build_lcss(N, Thresh, Link, Lcss) :-
	N1 is N+1,
	lcss__build_lcss2(N, N1, Thresh, K),
	array__lookup(Link, K, Lcss).

:- pred lcss__build_lcss2(int, int, array(int), int).
:- mode lcss__build_lcss2(in, in, in, out) is det.
lcss__build_lcss2(N, Max, Thresh, K) :-
	( array__lookup(Thresh, N, Max) ->
	    N1 is N+1,
	    lcss__build_lcss2(N1, Max, Thresh, K)
	;
	    K = N
	).

%-----------------------------------------------------------------------------%

:- pred lcss__to_diff(file :: in, file :: in, lcss :: in, diff :: out) is det.
lcss__to_diff(File1, File2, Lcss, Diff) :-
	file__get_numlines(File1, Length1),
	file__get_numlines(File2, Length2),
	lcss__to_diff2(1, 1, Length1, Length2, Lcss, Diff).

:- pred lcss__to_diff2(int, int, int, int, lcss, diff).
:- mode lcss__to_diff2(in, in, in, in, in, out) is det.
lcss__to_diff2(X, Y, L1, L2, [], Diff) :-
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
lcss__to_diff2(X, Y, L1, L2, [X2-Y2 | Lcss], Diff) :-
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
