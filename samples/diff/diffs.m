%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: bromage
% Simplified by Marnix Klooster <marnix@worldonline.nl>

% This module contains the predicates to convert an lcss to a diff,
% and to display diffs.

%-----------------------------------------------------------------------------%

:- module diffs.

:- interface.
:- import_module file, io, lcsstype.

%-----------------------------------------------------------------------------%

	% The type of a difference.
:- type diff.

	% Convert a lcss to a diff.  The last pair of the lcss is
	% expected to be L1-L2, where L1 and L2 are the lengths of the
	% lists that were compared.  (See lcss.m for details.)
:- pred diffs__to_diff(lcss :: in, diff :: out) is det.

	% diffs__display_diff takes a diff and displays it
	% in the standard diff(1) format.
:- pred diffs__display_diff(file, file, diff, io__state, io__state).
:- mode diffs__display_diff(in, in, in, di, uo) is det.

	% diffs__display_diff takes a diff and displays it
	% in the RCS difference format.
:- pred diffs__display_diff_rcs(file, file, diff, io__state, io__state).
:- mode diffs__display_diff_rcs(in, in, in, di, uo) is det.

	% diffs__display_diff takes a diff and displays it
	% in the CVS merge conflict format.
:- pred diffs__display_diff_cvs_merge(file, file, diff, io__state, io__state).
:- mode diffs__display_diff_cvs_merge(in, in, in, di, uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module require, std_util, int, list, char.

	% A segment is a pair of positions.  Numbering items from 0,
	% segment P-Q stands for items P up to, but not including, Q.
	% (Rationale: see the interpretation of type pos in
	% lcsstype.m.)
:- type segment == pair(pos,pos).

	% An edit operation is an addition, a deletion or
	% a change.
:- type edit --->
		add(pos,segment)
	;	delete(segment,pos)
	;	change(segment,segment).

	% The complete diff file is a list of edit
	% operations.
:- type diff == list(edit).

%-----------------------------------------------------------------------------%

	% diffs__to_diff turns the longest common subsequence of two
	% lists into a list of edits.  We assume that the last pair is
	% the match of the 'end-of-list markers', so we don't have to
	% pass the list lengths explicitly.

diffs__to_diff(Lcss, Diff) :-
	diffs__to_diff2(0, 0, Lcss, Diff).

:- pred diffs__to_diff2(int, int, lcss, diff).
:- mode diffs__to_diff2(in, in, in, out) is det.

diffs__to_diff2(_X, _Y, [], []).

diffs__to_diff2(X, Y, [X2 - Y2 | Lcss], Diff) :-
	 XNext is X2 + 1, YNext is Y2 + 1,
	 diffs__to_diff2(XNext, YNext, Lcss, Diff1),
	 ( X = X2 ->
		 ( Y = Y2 ->
	 	 	Diff = Diff1
		 ;
		 	Diff = [add(X,Y - Y2) | Diff1]
		 )
	 ;
		 ( Y = Y2 ->
		 	Diff = [delete(X - X2,Y) | Diff1]
		 ;
		 	Diff = [change(X - X2,Y - Y2) | Diff1]
		 )
	 ).

%-----------------------------------------------------------------------------%

diffs__display_diff(_, _, []) --> [].
diffs__display_diff(File1, File2, [SingDiff | Diff]) -->
	( { SingDiff = add(X, Y1 - Y2) },
		diffs__write_command(X - X, 'a', Y1 - Y2),
		diffs__show_file(File2, "> ", Y1 - Y2)
	; { SingDiff = delete(X1 - X2, Y) },
		diffs__write_command(X1 - X2, 'd', Y - Y),
		diffs__show_file(File1, "< ", X1 - X2)
	; { SingDiff = change(X1 - X2, Y1 - Y2) },
		diffs__write_command(X1 - X2, 'c', Y1 - Y2),
		diffs__show_file(File1, "< ", X1 - X2),
		io__write_string("---\n"),
		diffs__show_file(File2, "> ", Y1 - Y2)
	),
	diffs__display_diff(File1, File2, Diff).

	% diffs__write_command displays a diff(1) command.
	% Like ed(1), a pair of numbers which are identical
	% are abbreviated by a single number.
	% MK: Assumption X=<X2
:- pred diffs__write_command(segment, char, segment, io__state, io__state).
:- mode diffs__write_command(in, in, in, di, uo) is det.
diffs__write_command(X - X2, C, Y - Y2) -->
	{ X1 is X + 1 },
	( { X1 >= X2 } ->	% either empty or singleton segment
		io__write_int(X2)
	;
		io__write_int(X1),
		io__write_char(','),
		io__write_int(X2)
	),
	io__write_char(C),
	{ Y1 is Y + 1 },
	( { Y1 >= Y2 } ->	% either empty or singleton segment
		io__write_int(Y2)
	;
		io__write_int(Y1),
		io__write_char(','),
		io__write_int(Y2)
	),
	io__write_char('\n').

%-----------------------------------------------------------------------------%

diffs__display_diff_rcs(_File1, _File2, []) --> [].
diffs__display_diff_rcs(File1, File2, [Cmd | Diff]) -->
	( { Cmd = add(X, Y1 - Y2) },
		{ Y is Y2 - Y1 },
		diffs__write_command_rcs('a', X, Y),
		diffs__show_file(File2, "", Y1 - Y2)
	; { Cmd = delete(X1 - X2, _Y) },
		{ X is X2 - X1 },
		diffs__write_command_rcs('d', X1, X)
	; { Cmd = change(X1 - X2, Y1 - Y2) },
		{ X is X2 - X1 },
		{ Y is Y2 - Y1 },
		diffs__write_command_rcs('d', X1, X),
		diffs__write_command_rcs('a', X1, Y),
		diffs__show_file(File2, "", Y1 - Y2)
	),
	diffs__display_diff_rcs(File1, File2, Diff).

	% diffs__write_command_rcs displays a diff command in
	% the RCS ,v format.
:- pred diffs__write_command_rcs(char, int, int, io__state, io__state).
:- mode diffs__write_command_rcs(in, in, in, di, uo) is det.
diffs__write_command_rcs(C, X, Y) -->
	{ X1 is X + 1 },
	io__write_char(C),
	io__write_int(X1),
	io__write_char(' '),
	io__write_int(Y),
	io__write_char('\n').

%-----------------------------------------------------------------------------%

diffs__display_diff_cvs_merge(File1, File2, Diff) -->
	diffs__display_diff_cvs_merge_2(0, File1, File2, Diff).

:- pred diffs__display_diff_cvs_merge_2(int, file, file, diff,
		io__state, io__state).
:- mode diffs__display_diff_cvs_merge_2(in, in, in, in, di, uo) is det.

diffs__display_diff_cvs_merge_2(Prev, File1, _File2, []) -->
	{ file__get_numlines(File1, SegEnd) },
	diffs__show_file(File1, "", Prev - SegEnd).
diffs__display_diff_cvs_merge_2(Prev, File1, File2, [Cmd0 | Diff]) -->
	{ file__get_file_name(File1, Name1) },
	{ file__get_file_name(File2, Name2) },
	( { Cmd0 = add(X, Seg2) },
		diffs__show_file(File1, "", Prev - X),
		io__write_strings(["<<<<<<< ", Name1, "\n"]),
		io__write_string("=======\n"),
		diffs__show_file(File2, "", Seg2),
		io__write_strings([">>>>>>> ", Name2, "\n"]),
		{ Next = X }
	; { Cmd0 = delete(X1 - X2, _) },
		diffs__show_file(File1, "", Prev - X1),
		io__write_strings(["<<<<<<< ", Name1, "\n"]),
		diffs__show_file(File1, "", X1 - X2),
		io__write_string("=======\n"),
		io__write_strings([">>>>>>> ", Name2, "\n"]),
		{ Next = X2 }
	; { Cmd0 = change(X1 - X2, Y1 - Y2) },
		diffs__show_file(File1, "", Prev - X1),
		io__write_strings(["<<<<<<< ", Name1, "\n"]),
		diffs__show_file(File1, "", X1 - X2),
		io__write_string("=======\n"),
		diffs__show_file(File2, "", Y1 - Y2),
		io__write_strings([">>>>>>> ", Name2, "\n"]),
		{ Next = X2 }
	),
	diffs__display_diff_cvs_merge_2(Next, File1, File2, Diff).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% diffs__show_file shows the segment of the file
	% from Low to High, with each line preceeded by
	% the Prefix characher and a space.  The diff(1)
	% format specifies that the lines effected in the
	% first file should be flagged by '<' and the
	% lines effected in the second file should be
	% flagged by '>'.
:- pred diffs__show_file(file, string, segment, io__state, io__state).
:- mode diffs__show_file(in, in, in, di, uo) is det.
diffs__show_file(File, Prefix, Seg) -->
	{ Seg = Low - High },
	( { Low < High } ->
		( { file__get_line(File, Low, Line) } ->
			{ Low1 is Low + 1 },
			io__write_strings([Prefix, Line]),
			diffs__show_file(File, Prefix, Low1 - High)
		;
			{ error("diffs_show_file: file ended prematurely") }
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
