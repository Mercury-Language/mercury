%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

:- module lcss.
% Main author: bromage

:- interface.
:- import_module io, file.

%-----------------------------------------------------------------------------%

	% The type of a difference.
:- type diff.

	% lcss__find_diff takes two files and finds their
	% differences.
:- pred lcss__find_diff(file, file, diff).
:- mode lcss__find_diff(in, in, out) is det.

	% lcss__display_diff takes a diff and displays it
	% in the standard diff(1) format.
:- pred lcss__display_diff(file, file, diff, io__state, io__state).
:- mode lcss__display_diff(in, in, in, di, uo) is det.

	% lcss__display_diff takes a diff and displays it
	% in the RCS difference format.
:- pred lcss__display_diff_rcs(file, file, diff, io__state, io__state).
:- mode lcss__display_diff_rcs(in, in, in, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module map, require, std_util, int, list, char, array.

	% The longest common subsequence of two files can be
	% represented as an ordered list of "matches".  A
	% match is a pair of the form I-J where I is the
	% number of a line in file 1 and J is the number of
	% a line in file 2.
:- type lcss == list(pair(int,int)).

	% A single diff entry is an addition, a deletion or
	% a change.
:- type single_diff --->
		add(int, pair(int,int))
	;	delete(pair(int,int), int)
	;	change(pair(int,int), pair(int,int)).

	% The complete diff file is a list of single
	% differences.
:- type diff == list(single_diff).

%-----------------------------------------------------------------------------%

	% The process to "diff" two files is:
	%
	%	- Identify the longest common subsequence
	%	  in the files.
	%
	%	- Use this information to determine the
	%	  set of operations required to convert
	%	  one file to the other.
lcss__find_diff(File1, File2, Diff) :-
	lcss__find_lcss(File1, File2, Lcss),
	lcss__to_diff(File1, File2, Lcss, Diff).

%-----------------------------------------------------------------------------%

	% For debugging only.  Will be removed in the
	% final version.
:- pred lcss__show_lcss(lcss :: in, io__state :: di, io__state :: uo) is det.
lcss__show_lcss([]) -->
	io__write_string("[]").
lcss__show_lcss([X - Y | Lcss]) -->
	io__write_int(X),
	io__write_char('-'),
	io__write_int(Y),
	io__write_char(','),
	lcss__show_lcss(Lcss).

%-----------------------------------------------------------------------------%

	% Find the longest common subsequence.  The algorithm
	% used is very similar to that in:
	%
	%     Hunt & Szymanski, "A fast algorithm for computing
	%     longest common subsequences", CACM 20:5, pp 350--353,
	%     1977.
:- pred lcss__find_lcss(file :: in, file :: in, lcss :: out) is det.
lcss__find_lcss(File1, File2, Lcss) :-
	file__get_numlines(File1, L1),
	file__get_numlines(File2, L2),

	% Set N to the size of the larger of File1 or File2,
	% let F1 be the larger file and F2 be the smaller file.

	( L1 >= L2 ->
	    N = L1, F1 = File1, F2 = File2
	;
	    N = L2, F2 = File1, F1 = File2
	),

	% Calculate the LCSS

	lcss__build_matchlist(F1, F2, MatchList),
	lcss__build_thresh(N, MatchList, Thresh, Link),
	lcss__build_lcss(N, Thresh, Link, Lcss1),

	% If we mapped File1 |-> F2, File2 |-> F1 then we
	% need to swap all the entries in the Lcss.

	( L1 >= L2 ->
	    Lcss = Lcss1
	;
	    lcss__swap(Lcss1, Lcss)
	).

:- pred lcss__swap(lcss :: in, lcss :: out) is det.
lcss__swap([], []).
lcss__swap([I - J | RestIn], [J - I | RestOut]) :-
	lcss__swap(RestIn, RestOut).

%-----------------------------------------------------------------------------%

	% The matchlist is the set of all matchings (I,J)
	% such that F1[I] = F2[J].  It is stored as a list
	% of list of integers where the Ith element of
	% the list is the list of all J such that F1[I]=F2[J].
	% This list should be increasing order.
:- pred lcss__build_matchlist(file, file, list(list(int))).
:- mode lcss__build_matchlist(in, in, out) is det.
lcss__build_matchlist(File1, File2, MatchList) :-

	% First, invert File2.  The inverted file is a
	% mapping from strings to lists of integers where
	% a given string maps to the list of lines in File2
	% which match that string.

	file__to_list(File2, File2list),
	lcss__build_match_map(1, File2list, Map),

	% Now match each line in File1 with those in File2.

	file__to_list(File1, File1list),
	lcss__match_map_to_matchlist(File1list, Map, MatchList).

:- pred lcss__build_match_map(int, list(string), map(string,list(int))).
:- mode lcss__build_match_map(in, in, out) is det.
lcss__build_match_map(_, [], Map) :-
	map__init(Map).
lcss__build_match_map(N, [S | Ss], MapOut) :-
	N1 is N + 1,
	lcss__build_match_map(N1, Ss, MapIn),
	( map__search(MapIn, S, Ns0) ->
	    list__append(Ns0, [N], Ns1)
	;
	    Ns1 = [ N ]
	),
	map__set(MapIn, S, Ns1, MapOut).

:- pred lcss__match_map_to_matchlist(list(string), map(string,list(int)), 
		list(list(int))).
:- mode lcss__match_map_to_matchlist(in, in, out) is det.
lcss__match_map_to_matchlist([], _, []).
lcss__match_map_to_matchlist([S | Ss], Map, [M | Ms]) :-
	lcss__match_map_to_matchlist(Ss, Map, Ms),
	( map__search(Map, S, Ns0) ->
	    M = Ns0
	;
	    M = []
	).

%-----------------------------------------------------------------------------%

	% This is the heart of the lcss procedure.  The
	% algorithm maintains two arrays, Thresh and Link.
	% Thresh[I] is defined as the length of the longest
	% common subsequence found so far which terminates at 
	% position I in File1.  Link[I] is this actual
	% subsequence stored in reverse.
	%
	% The special value N+1 is used to denote that no
	% subsequence has been found that terminates at that
	% position.
:- pred lcss__build_thresh(int, list(list(int)), array(int), 
		array(lcss)).
:- mode lcss__build_thresh(in, in, out, out) is det.
lcss__build_thresh(N, MatchList, Thresh, Link) :-
	N1 is N + 1,
	array__init(0, N, N1, Thresh0),
	array__set(Thresh0, 0, 0, Thresh1),
	array__init(0, N, [], Link1),
	lcss__build_thresh2(1, N, MatchList, Thresh1, Link1, Thresh, Link).

:- pred lcss__build_thresh2(int, int, list(list(int)),
		array(int), array(lcss),
		array(int), array(lcss)).
:- mode lcss__build_thresh2(in, in, in, in, in, out, out) is det.
lcss__build_thresh2(I, N, MatchList, Thresh0, Link0, Thresh1, Link1) :-
	( I > N ->
	    % If we have stepped past the end of the file, we
	    % have finished.
	    Thresh0=Thresh1, Link0=Link1
	; MatchList = [Matches | MatchRest] ->
	    % Otherwise step through each match in this MatchList
	    % entry, and arrach 

	    I1 is I + 1,
	    lcss__build_thresh3(N, I, Matches, Thresh0, Link0, Thresh2, Link2),
	    lcss__build_thresh2(I1, N, MatchRest, Thresh2, Link2,
			Thresh1, Link1)
	;
	    error("lcss__build_thresh2")
	).

:- pred lcss__build_thresh3(int, int, list(int),
		array(int), array(lcss),
	 	array(int), array(lcss)).
:- mode lcss__build_thresh3(in, in, in, in, in, out, out) is det.
lcss__build_thresh3(_, _, [], Thresh, Link, Thresh, Link).
lcss__build_thresh3(N, I, [ J | Js ], Thresh0, Link0, Thresh1, Link1) :-
	% Find which Thresh entry we should attach this match to.
	lcss__build_thresh4(0, N, J, K, Thresh0),
	array__lookup(Thresh0, K, ThreshK),
	% Is the found Thresh entry shorter than that which would 
	% be obtained by using this?
	( J < ThreshK ->
	    % Yes, so make this match part of a new entry.
	    K1 is K - 1,
	    array__set(Thresh0, K, J, Thresh2),
	    array__lookup(Link0, K1, LinkK1),
	    array__set(Link0, K, [I - J | LinkK1], Link2)
	;
	    % Otherwise forget it.
	    Link0 = Link2, Thresh0 = Thresh2
	),
	lcss__build_thresh3(N, I, Js, Thresh2, Link2, Thresh1, Link1).

	% lcss__build_thresh4 performs a binary search
	% through Thresh to find the value of K such
	% that Thresh[K-1] < J =< Thresh[K].
:- pred lcss__build_thresh4(int, int, int, int, array(int)).
:- mode lcss__build_thresh4(in, in, in, out, in) is det.
lcss__build_thresh4(Lo, Hi, J, K, Thresh) :-
	Width is Hi - Lo,
	( Width < 1 ->
	    error("lcss__build_thresh4")
	; Width = 1 ->
	    K = Hi
	;
	    % Use the middle element of the range.
	    Mid is (Lo + Hi) // 2,
	    array__lookup(Thresh, Mid, ThreshMid),
	    ( ThreshMid < J ->
		lcss__build_thresh4(Mid, Hi, J, K, Thresh)
	    ;
		lcss__build_thresh4(Lo, Mid, J, K, Thresh)
	    )
	).

%-----------------------------------------------------------------------------%

	% Now that we have the array Thresh, it is a simple
	% exercise to recover the Lcss: Simply find the
	% largesst value of K such that Thresh[K] < N+1
	% and Link[K] should be the Lcss in reverse.
:- pred lcss__build_lcss(int, array(int), array(lcss), lcss).
:- mode lcss__build_lcss(in, in, in, out) is det.
lcss__build_lcss(N, Thresh, Link, Lcss) :-
	N1 is N + 1,
	lcss__build_lcss2(N, N1, Thresh, K),
	( array__semidet_lookup(Link, K, Lcss1) ->
	    list__reverse(Lcss1, Lcss)
	;
	    Lcss = []
	).

	% A simple linear search should be sufficient.  On
	% "normal" input, the number of changes is expected
	% to be small compared to the size of the file, so
	% a full-blown binary search is not necessary.
	% (Even if this were not the case, this predicate
	% is not the bottleneck at the moment.)
:- pred lcss__build_lcss2(int, int, array(int), int).
:- mode lcss__build_lcss2(in, in, in, out) is det.
lcss__build_lcss2(N, Max, Thresh, K) :-
	( array__lookup(Thresh, N, Max) ->
	    N1 is N - 1,
	    lcss__build_lcss2(N1, Max, Thresh, K)
	;
	    K = N
	).

%-----------------------------------------------------------------------------%

	% lcss__to_diff turns the longest common subsequence
	% of two files into a list of single_diffs.
:- pred lcss__to_diff(file :: in, file :: in, lcss :: in, diff :: out) is det.
lcss__to_diff(File1, File2, Lcss, Diff) :-
	file__get_numlines(File1, Length1),
	file__get_numlines(File2, Length2),
	lcss__to_diff2(1, 1, Length1, Length2, Lcss, Diff).

:- pred lcss__to_diff2(int, int, int, int, lcss, diff).
:- mode lcss__to_diff2(in, in, in, in, in, out) is det.

	% This case is taken if we have run out of matches.
	% If we have not reached the end of both files, then
	% some changes have to be added to reflect this.
lcss__to_diff2(X, Y, L1, L2, [], Diff) :-
	XLoc is X - 1, YLoc is Y - 1,
	( X > L1 ->
	    ( Y > L2 ->
		Diff = []
	    ;
		Diff = [add(XLoc,Y - L2)]
	    )
	;
	    ( Y > L2 ->
		Diff = [delete(X - L1,YLoc)]
	    ;
		Diff = [change(X - L1,Y - L2)]
	    )
	).

	% Otherwise we only have to consider the amount
	% of file between the current position and the
	% next match.
lcss__to_diff2(X, Y, L1, L2, [X2 - Y2 | Lcss], Diff) :-
	XLoc is X - 1,   YLoc is Y - 1,
	XEnd is X2 - 1,  YEnd is Y2 - 1,
	XNext is X2 + 1, YNext is Y2 + 1,
	( X = X2 ->
	    ( Y = Y2 ->
		lcss__to_diff2(XNext, YNext, L1, L2, Lcss, Diff)
	    ;
		lcss__to_diff2(XNext, YNext, L1, L2, Lcss, Diff1),
		Diff = [add(XLoc,Y - YEnd) | Diff1]
	    )
	;
	    ( Y = Y2 ->
		lcss__to_diff2(XNext, YNext, L1, L2, Lcss, Diff1),
		Diff = [delete(X - XEnd,YLoc) | Diff1]
	    ;
		lcss__to_diff2(XNext, YNext, L1, L2, Lcss, Diff1),
		Diff = [change(X - XEnd,Y - YEnd) | Diff1]
	    )
	).

%-----------------------------------------------------------------------------%

	% This is a quick 'n' dirty version until deep
	% indexing is implemented in the determinism
	% checker.
lcss__display_diff(_, _, []) --> { true }.
lcss__display_diff(File1, File2, [SingDiff | Diff]) -->
	( { SingDiff = add(X, Y1 - Y2) },
	    lcss__write_command(X, X, 'a', Y1, Y2),
	    lcss__show_file(File2, "> ", Y1, Y2)
	; { SingDiff = delete(X1 - X2, Y) },
	    lcss__write_command(X1, X2, 'd', Y, Y),
	    lcss__show_file(File1, "< ", X1, X2)
	; { SingDiff = change(X1 - X2, Y1 - Y2) },
	    lcss__write_command(X1, X2, 'c', Y1, Y2),
	    lcss__show_file(File1, "< ", X1, X2),
	    io__write_string("---\n"),
	    lcss__show_file(File2, "> ", Y1, Y2)
	),
	lcss__display_diff(File1, File2, Diff).

	% lcss__display_diff takes a list of diffs and
	% displays it in the standard diff(1) format.
% lcss__display_diff(_, _, []) --> { true }.
% lcss__display_diff(File1, File2, [ add(X, Y1 - Y2) | Diff ]) -->
% 	lcss__write_command(X, X, 'a', Y1, Y2),
%	lcss__show_file(File2, "> "), Y1, Y2),
% 	lcss__display_diff(File1, File2, Diff),
% lcss__display_diff(File1, File2, [ delete(X1 - X2, Y) | Diff ]) -->
% 	lcss__write_command(X1, X2, 'd', Y, Y),
%	lcss__show_file(File1, "< ", X1, X2),
% 	lcss__display_diff(File1, File2, Diff).
% lcss__display_diff(File1, File2, [ change(X1 - X2, Y1 - Y2) | Diff ]) -->
% 	lcss__write_command(X1, X2, 'c', Y1, Y2),
%	lcss__show_file(File1, "< ", X1, X2),
%	io__write_string("---\n"),
%	lcss__show_file(File2, "> ", Y1, Y2),
% 	lcss__display_diff(File1, File2, Diff).

	% lcss__write_command displays a diff(1) command.
	% Like ed(1), a pair of numbers which are identical
	% are abbreviated by a single number.
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

	% lcss__display_diff takes a list of diffs and
	% displays it in the RCS ,v format.
%lcss__display_diff_rcs(_File1, _File2, []) --> { true }.
%lcss__display_diff_rcs(File1, File2, [ add(X, Y1 - Y2) | Diff ]) -->
%	{ Y is Y2 - Y1 + 1 },
%	lcss__write_command_rcs('a', X, Y),
%	lcss__show_file(File1, "", Y1, Y2),
%	lcss__display_diff_rcs(File1, File2, Diff).
%lcss__display_diff_rcs(File1, File2, [ delete(X1 - X2, Y) | Diff ]) -->
%	{ X is X2 - X1 + 1 },
%	lcss__write_command_rcs('d', X, Y),
%	lcss__display_diff_rcs(File1, File2, Diff).
%lcss__display_diff_rcs(File1, File2, [ change(X1 - X2, Y1 - Y2) | Diff ]) -->
%	{ X is X2 - X1 + 1 },
%	{ Y is Y2 - Y1 + 1 },
%	lcss__write_command_rcs('d', X, Y),
%	lcss__write_command_rcs('a', X, Y),
%	lcss__show_file(File1, "", Y1, Y2),
%	lcss__display_diff_rcs(File1, File2, Diff).

lcss__display_diff_rcs(_File1, _File2, []) --> { true }.
lcss__display_diff_rcs(File1, File2, [Cmd | Diff]) -->
	( { Cmd = add(X, Y1 - Y2) },
	    { Y is Y2 - Y1 + 1 },
	    lcss__write_command_rcs('a', X, Y),
	    lcss__show_file(File2, "", Y1, Y2)
	; { Cmd = delete(X1 - X2, Y) },
	    { X is X2 - X1 + 1 },
	    lcss__write_command_rcs('d', X1, X)
	; { Cmd = change(X1 - X2, Y1 - Y2) },
	    { X is X2 - X1 + 1 },
	    { Y is Y2 - Y1 + 1 },
	    lcss__write_command_rcs('d', X1, X),
	    lcss__write_command_rcs('a', X1, Y),
	    lcss__show_file(File2, "", Y1, Y2)
	),
	lcss__display_diff_rcs(File1, File2, Diff).

	% lcss__write_command_rcs displays a diff command in
	% the RCS ,v format.
:- pred lcss__write_command_rcs(char, int, int, io__state, io__state).
:- mode lcss__write_command_rcs(in, in, in, di, uo) is det.
lcss__write_command_rcs(C, X, Y) -->
	io__write_char(C),
	io__write_int(X),
	io__write_char(' '),
	io__write_int(Y),
	io__write_char('\n').

	% lcss__show_file shows the portion of the file
	% from Low to High, with each line preceeded by
	% the Prefix characher and a space.  The diff(1)
	% format specifies that the lines effected in the
	% first file should be flagged by '<' and the
	% lines effected in the second file should be
	% flagged by '>'.
:- pred lcss__show_file(file, string, int, int, io__state, io__state).
:- mode lcss__show_file(in, in, in, in, di, uo) is det.
lcss__show_file(File, Prefix, Low, High) -->
	( { Low > High } ->
	    { true }
	;
	    ( { file__get_line(File, Low, Line) } ->
	    	{ Low1 is Low + 1 },
	    	io__write_strings([Prefix, Line]),
	    	lcss__show_file(File, Prefix, Low1, High)
	    ;
		{ true }
	    )
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
