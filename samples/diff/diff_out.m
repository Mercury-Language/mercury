%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: bromage
% Based on diffs.m, written by bromage and simplified by
% Marnix Klooster <marnix@worldonline.nl>

% This module contains the predicates to display a diff in various
% output styles, based on the command-line options supplied.

%-----------------------------------------------------------------------------%

:- module diff_out.

:- interface.
:- import_module file, io, int, string, difftype.

%-----------------------------------------------------------------------------%

:- type diff_out__output_style --->
		normal
	;	help_only
	;	version_only
	;	context(int)
	;	unified(int)
	;	ed
	;	forward_ed
	;	rcs
	;	ifdef(string)
	;	brief
	;	side_by_side.

:- pred diff_out__default_output_style(diff_out__output_style :: out) is det.

	% display_diff takes a diff and displays it
	% in the user's specified output format.
:- pred display_diff(file, file, diff, io__state, io__state).
:- mode display_diff(in, in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module globals, options.
:- import_module require, std_util, int, list, char, string, bool.

diff_out__default_output_style(normal).

%-----------------------------------------------------------------------------%

	% diff_out__show_file shows the segment of the file
	% from Low to High, with each line preceeded by
	% the Prefix characher and a space.  The diff(1)
	% format specifies that the lines effected in the
	% first file should be flagged by '<' and the
	% lines effected in the second file should be
	% flagged by '>'.
	%
	% NOTE: GCC 2.7.2 under Digital Unix 3.2 doesn't compile
	%       this predicate correctly with optimisation turned on.
:- pred diff_out__show_file(file, string, segment, io__state, io__state).
:- mode diff_out__show_file(in, in, in, di, uo) is det.

diff_out__show_file(File, Prefix, Low - High) -->
	( { Low < High } ->
		( { file__get_line(File, Low, Line) } ->
			{ Low1 is Low + 1 },
			io__write_strings([Prefix, Line]),
			diff_out__show_file(File, Prefix, Low1 - High)
		;
			{ error("diff_out_show_file: file ended prematurely") }
		)
	;
		[]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% display_diff: Determine which output style to use, then call
	% the predicate to display that output.
	%
	% Some of these options (notably the ones which require no
	% output) should have been handled already by the time we
	% reach here.  In those cases, we just call error/1.
display_diff(File1, File2, Diff) -->
	globals__io_get_output_style(OutputStyle),
	( { OutputStyle = normal },
		display_diff_normal(File1, File2, Diff)
	; { OutputStyle = help_only },
		{ error("display_diff: help_only") }
	; { OutputStyle = version_only },
		{ error("display_diff: version_only") }
	; { OutputStyle = context(Context) },
		display_context_diff(Context, File1, File2, Diff)
	; { OutputStyle = unified(Context) },
		display_unified_diff(Context, File1, File2, Diff)
	; { OutputStyle = ed },
		display_diff_ed(File1, File2, Diff)
	; { OutputStyle = forward_ed },
		display_diff_forward_ed(File1, File2, Diff)
	; { OutputStyle = rcs },
		display_diff_rcs(File1, File2, Diff)
	; { OutputStyle = ifdef(Sym) },
		display_diff_ifdef(Sym, File1, File2, Diff)
	; { OutputStyle = brief },
		% XXX For this output style, we really don't need to
		%     perform a complete diff.  This should be handled
		%     higher up for efficiency.
		( { Diff \= [] } ->
			{ file__get_file_name(File1, FileName1) },
			{ file__get_file_name(File2, FileName2) },
			io__write_strings(["Files ", FileName1, " and ",
				FileName2, " differ\n"])
		;
			[]
		)
	; { OutputStyle = side_by_side },
		display_diff_side_by_side(File1, File2, Diff)
	).

%-----------------------------------------------------------------------------%

	% display_diff_normal takes a diff and displays it
	% in the standard diff(1) output format.
:- pred display_diff_normal(file, file, diff, io__state, io__state).
:- mode display_diff_normal(in, in, in, di, uo) is det.

display_diff_normal(_, _, []) --> [].
display_diff_normal(File1, File2, [SingDiff | Diff]) -->
	( { SingDiff = add(X, Y1 - Y2) },
		diff_out__write_command(X - X, 'a', Y1 - Y2),
		diff_out__show_file(File2, "> ", Y1 - Y2)
	; { SingDiff = delete(X1 - X2, Y) },
		diff_out__write_command(X1 - X2, 'd', Y - Y),
		diff_out__show_file(File1, "< ", X1 - X2)
	; { SingDiff = change(X1 - X2, Y1 - Y2) },
		diff_out__write_command(X1 - X2, 'c', Y1 - Y2),
		diff_out__show_file(File1, "< ", X1 - X2),
		io__write_string("---\n"),
		diff_out__show_file(File2, "> ", Y1 - Y2)
	),
	display_diff_normal(File1, File2, Diff).


	% diff_out__write_command displays a diff(1) command.
	% Like ed(1), a pair of numbers which are identical
	% are abbreviated by a single number.
	% MK: Assumption X=<X2
	% AJB: And, similarly, Y=<Y2.  This is actually an
	%      invariant of the segment type.  See difftype.m.
:- pred diff_out__write_command(segment, char, segment, io__state, io__state).
:- mode diff_out__write_command(in, in, in, di, uo) is det.

diff_out__write_command(X - X2, C, Y - Y2) -->
	{ X1 is X + 1 },	% Convert from pos to line number
	( { X1 >= X2 } ->
		% either empty or singleton segment
		io__write_int(X2)
	;
		io__write_int(X1),
		io__write_char(','),
		io__write_int(X2)
	),
	io__write_char(C),
	{ Y1 is Y + 1 },	% Convert from pos to line number
	( { Y1 >= Y2 } ->
		% either empty or singleton segment
		io__write_int(Y2)
	;
		io__write_int(Y1),
		io__write_char(','),
		io__write_int(Y2)
	),
	io__write_char('\n').

%-----------------------------------------------------------------------------%

	% display_diff_rcs takes a diff and displays it
	% in the RCS difference format.
:- pred display_diff_rcs(file, file, diff, io__state, io__state).
:- mode display_diff_rcs(in, in, in, di, uo) is det.

display_diff_rcs(_File1, _File2, []) --> [].
display_diff_rcs(File1, File2, [Cmd | Diff]) -->
	( { Cmd = add(X, Y1 - Y2) },
		{ Y is Y2 - Y1 },
		diff_out__write_command_rcs('a', X, Y),
		diff_out__show_file(File2, "", Y1 - Y2)
	; { Cmd = delete(X1 - X2, _Y) },
		{ X is X2 - X1 },
		diff_out__write_command_rcs('d', X1, X)
	; { Cmd = change(X1 - X2, Y1 - Y2) },
		{ X is X2 - X1 },
		{ Y is Y2 - Y1 },
		diff_out__write_command_rcs('d', X1, X),
		diff_out__write_command_rcs('a', X1, Y),
		diff_out__show_file(File2, "", Y1 - Y2)
	),
	display_diff_rcs(File1, File2, Diff).


	% diff_out__write_command_rcs displays a diff command in
	% the RCS ,v format.
:- pred diff_out__write_command_rcs(char, int, int, io__state, io__state).
:- mode diff_out__write_command_rcs(in, in, in, di, uo) is det.

diff_out__write_command_rcs(C, X, Y) -->
	{ X1 is X + 1 },		% Convert from pos to line number
	io__write_char(C),
	io__write_int(X1),
	io__write_char(' '),
	io__write_int(Y),
	io__write_char('\n').

%-----------------------------------------------------------------------------%

	% display_diff_ed takes a diff and displays it
	% in ed(1) format, but with all diffs backward.
:- pred display_diff_ed(file, file, diff, io__state, io__state).
:- mode display_diff_ed(in, in, in, di, uo) is det.

display_diff_ed(_File1, _File2, []) --> [].
display_diff_ed(File1, File2, [Cmd | Diff]) -->
	display_diff_ed(File1, File2, Diff),
	( { Cmd = add(X, Y1 - Y2) },
		diff_out__write_command_ed(X - X, 'a'),
		diff_out__show_file(File2, "", Y1 - Y2),
		io__write_string(".\n")
	; { Cmd = delete(X, _Y) },
		diff_out__write_command_ed(X, 'd')
	; { Cmd = change(X, Y) },
		diff_out__write_command_ed(X, 'c'),
		diff_out__show_file(File2, "", Y),
		io__write_string(".\n")
	).


	% diff_out__write_command_ed displays an ed(1) command.
:- pred diff_out__write_command_ed(segment, char, io__state, io__state).
:- mode diff_out__write_command_ed(in, in, di, uo) is det.

diff_out__write_command_ed(X - X2, C) -->
	{ X1 is X + 1 },		% Convert from pos to line number
	( { X1 >= X2 } ->
		% either empty or singleton segment
		io__write_int(X2)
	;
		io__write_int(X1),
		io__write_char(','),
		io__write_int(X2)
	),
	io__write_char(C),
	io__write_char('\n').

%-----------------------------------------------------------------------------%

	% display_diff_forward_ed takes a diff and displays it
	% in ed(1) format, but with all diff_out forward.  This
	% is actually useless for feeding to ed(1), but nicer
	% to read.
:- pred display_diff_forward_ed(file, file, diff, io__state, io__state).
:- mode display_diff_forward_ed(in, in, in, di, uo) is det.

display_diff_forward_ed(_File1, _File2, []) --> { true }.
display_diff_forward_ed(File1, File2, [Cmd | Diff]) -->
	( { Cmd = add(X, Y1 - Y2) },
		diff_out__write_command_forward_ed(X - X, 'a'),
		diff_out__show_file(File2, "", Y1 - Y2),
		io__write_string(".\n")
	; { Cmd = delete(X, _Y) },
		diff_out__write_command_forward_ed(X, 'd')
	; { Cmd = change(X, Y) },
		diff_out__write_command_forward_ed(X, 'c'),
		diff_out__show_file(File2, "", Y),
		io__write_string(".\n")
	),
	display_diff_forward_ed(File1, File2, Diff).

	% diff_out__write_command_ed displays a forward ed(1) command.
:- pred diff_out__write_command_forward_ed(segment, char, io__state, io__state).
:- mode diff_out__write_command_forward_ed(in, in, di, uo) is det.
diff_out__write_command_forward_ed(X - X2, C) -->
	io__write_char(C),
	{ X1 is X + 1 },		% Convert from pos to line number
	( { X1 >= X2 } ->
		% either empty or singleton segment
		io__write_int(X2)
	;
		io__write_int(X1),
		io__write_char(' '),
		io__write_int(X2)
	),
	io__write_char('\n').

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% display_diff_ifdef writes out the files in a unified diff,
	% using #ifdefs around each edit.
	%
	% TO DO: GNU diff makes this output style much more
	%        configurable.  We should too.
:- pred display_diff_ifdef(string, file, file, diff, io__state, io__state).
:- mode display_diff_ifdef(in, in, in, in, di, uo) is det.

display_diff_ifdef(Sym, File1, File2, Diff) -->
	display_diff_ifdef_2(0, Sym, File1, File2, Diff).

	% Argument 1 (prev) is the last pos displayed before
	% the current edit (or end of edits, in the base case).
	% This is important for when we have to display the
	% "non-diffed" text between edits.
:- pred display_diff_ifdef_2(int, string, file, file, diff,
		io__state, io__state).
:- mode display_diff_ifdef_2(in, in, in, in, in, di, uo) is det.

display_diff_ifdef_2(Prev, _Sym, File1, _File2, []) -->
	{ file__get_numlines(File1, SegEnd) },
	diff_out__show_file(File1, "", Prev - SegEnd).
display_diff_ifdef_2(Prev, Sym, File1, File2, [Edit | Diff]) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	diff_out__show_file(File1, "", Prev - StartOfEdit),
	( { Edit = add(X, Seg2) },
		io__write_strings(["#ifdef ", Sym, "\n"]),
		diff_out__show_file(File2, "", Seg2),
		io__write_strings(["#endif /* ", Sym, " */\n"]),
		{ Next = X }
	; { Edit = delete(X1 - X2, _) },
		io__write_strings(["#ifndef ", Sym, "\n"]),
		diff_out__show_file(File1, "", X1 - X2),
		io__write_strings(["#endif /* not ", Sym, " */\n"]),
		{ Next = X2 }
	; { Edit = change(X1 - X2, Y1 - Y2) },
		io__write_strings(["#ifndef ", Sym, "\n"]),
		diff_out__show_file(File1, "", X1 - X2),
		io__write_strings(["#else /* ", Sym, " */\n"]),
		diff_out__show_file(File2, "", Y1 - Y2),
		io__write_strings(["#endif /* ", Sym, " */\n"]),
		{ Next = X2 }
	),
	display_diff_ifdef_2(Next, Sym, File1, File2, Diff).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Types for context/unified diffs.

	% A context diff is a bit more complicated than a "standard"
	% diff, because it requires the display of some parts of the
	% files which are not actually part of the diff, but not all
	% of it.
	%
	% Because context and unified diffs both require the same
	% kind of information, we factor out the code to turn a
	% normal diff into a context diff.

:- type context_edit
	--->	context_edit(segment, segment, diff).

:- type context_diff == list(context_edit).

%-----------------------------------------------------------------------------%

:- pred diff_to_context_diff(int :: in, int :: in, int :: in, diff :: in,
		context_diff :: out) is det.

diff_to_context_diff(_Xsize, _Ysize, _Context, [], []).
diff_to_context_diff(Xsize, Ysize, Context, [Edit | Diff], CDiff) :-
	diff_to_context_diff(Xsize, Ysize, Context, Diff, CDiff0),
	first_mentioned_positions(Edit, Xfirst0, Yfirst0),
	last_mentioned_positions(Edit, Xlast0, Ylast0),
	adjust_context(Context, Xsize, Xfirst0, Xlast0, Xfirst, Xlast),
	adjust_context(Context, Ysize, Yfirst0, Ylast0, Yfirst, Ylast),
	( CDiff0 = [],
		CDiff = [context_edit(Xfirst - Xlast, Yfirst - Ylast, [Edit])]
	; CDiff0 = [context_edit(XsegLo - XsegHi, YsegLo - YsegHi, DDiff) |
				CDiff1],
		% Should we merge this edit into the next one?
		(
			( XsegLo =< Xlast
			; YsegLo =< Ylast
			)
		->
			CDiff = [context_edit(Xfirst - XsegHi, Yfirst - YsegHi,
					[Edit | DDiff]) | CDiff1]
		;
			CDiff = [context_edit(Xfirst - Xlast, Yfirst - Ylast,
					[Edit]) | CDiff0]
		)
	).

:- pred first_mentioned_positions(edit :: in, pos :: out, pos :: out) is det.

first_mentioned_positions(add(X, Y - _), X, Y).
first_mentioned_positions(delete(X - _, Y), X, Y).
first_mentioned_positions(change(X - _, Y - _), X, Y).

:- pred last_mentioned_positions(edit :: in, pos :: out, pos :: out) is det.

last_mentioned_positions(add(X, _ - Y), X, Y).
last_mentioned_positions(delete(_ - X, Y), X, Y).
last_mentioned_positions(change(_ - X, _ - Y), X, Y).

	% Adjust a range to incorporate a given number of lines
	% of context.  Ensure that the new range stays within the
	% size of the file being considered.
:- pred adjust_context(int :: in, int :: in, int :: in, int :: in,
		int :: out, int :: out) is det.
adjust_context(Context, Size, First0, Last0, First, Last) :-
	First1 is First0 - Context,
	Last1 is Last0 + Context,
	( First1 < 0 ->
		First = 0
	;
		First = First1
	),
	( Last1 > Size ->
		Last = Size
	;
		Last = Last1
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Display a diff in unified format.
:- pred display_unified_diff(int, file, file, diff, io__state, io__state).
:- mode display_unified_diff(in, in, in, in, di, uo) is det.

display_unified_diff(Context, File1, File2, Diff) -->
	{ file__get_numlines(File1, Size1) },
	{ file__get_numlines(File2, Size2) },
	{ diff_to_context_diff(Size1, Size2, Context, Diff, CDiff) },
	{ file__get_file_name(File1, Name1) },
	{ file__get_file_name(File2, Name2) },
		% XXX Should also print out file dates.  But how??
	io__write_strings(["--- ", Name1, "\n"]),
	io__write_strings(["+++ ", Name2, "\n"]),
	display_unified_diff_2(File1, File2, CDiff).

:- pred display_unified_diff_2(file, file, context_diff, io__state, io__state).
:- mode display_unified_diff_2(in, in, in, di, uo) is det.

display_unified_diff_2(_File1, _File2, []) --> [].
display_unified_diff_2(File1, File2, [Edit | CDiff]) -->
	{ Edit = context_edit(Xlow - Xhigh, Ylow - Yhigh, Diff) },
	{ Xlow1 is Xlow + 1 },
	{ Ylow1 is Ylow + 1 },
	{ Xsize is Xhigh - Xlow },
	{ Ysize is Yhigh - Ylow },
	io__format("@@ -%d,%d +%d,%d @@\n",
		[i(Xlow1), i(Xsize), i(Ylow1), i(Ysize)]),
	display_unified_diff_3(Xlow, Xhigh, File1, File2, Diff),
	display_unified_diff_2(File1, File2, CDiff).

:- pred display_unified_diff_3(int, int, file, file, diff,
		io__state, io__state).
:- mode display_unified_diff_3(in, in, in, in, in, di, uo) is det.

display_unified_diff_3(Prev, Size1, File1, _File2, []) -->
	diff_out__show_file(File1, " ", Prev - Size1).
display_unified_diff_3(Prev, Size1, File1, File2, [Edit | Diff]) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	diff_out__show_file(File1, " ", Prev - StartOfEdit),
	( { Edit = add(X, Seg2) },
		diff_out__show_file(File2, "+", Seg2),
		{ Next = X }
	; { Edit = delete(X1 - X2, _) },
		diff_out__show_file(File1, "-", X1 - X2),
		{ Next = X1 }
	; { Edit = change(X1 - X2, Y1 - Y2) },
		diff_out__show_file(File1, "-", X1 - X2),
		diff_out__show_file(File2, "+", Y1 - Y2),
		{ Next = X1 }
	),
	display_unified_diff_3(Next, Size1, File1, File2, Diff).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Display a diff in context format.
:- pred display_context_diff(int, file, file, diff, io__state, io__state).
:- mode display_context_diff(in, in, in, in, di, uo) is det.

display_context_diff(Context, File1, File2, Diff) -->
	{ file__get_numlines(File1, Size1) },
	{ file__get_numlines(File2, Size2) },
	{ diff_to_context_diff(Size1, Size2, Context, Diff, CDiff) },
	{ file__get_file_name(File1, Name1) },
	{ file__get_file_name(File2, Name2) },
		% XXX Should also print out file dates.  But how??
	io__write_strings(["*** ", Name1, "\n"]),
	io__write_strings(["--- ", Name2, "\n"]),
	display_context_diff_2(File1, File2, CDiff).

:- pred display_context_diff_2(file, file, context_diff, io__state, io__state).
:- mode display_context_diff_2(in, in, in, di, uo) is det.

display_context_diff_2(_File1, _File2, []) --> [].
display_context_diff_2(File1, File2, [Edit | CDiff]) -->
	{ Edit = context_edit(Xlow - Xhigh, Ylow - Yhigh, Diff) },
	{ Xlow1 is Xlow + 1 },
	{ Ylow1 is Ylow + 1 },
	io__write_string("***************\n"),
	io__format("*** %d,%d ****\n", [i(Xlow1), i(Xhigh)]),

		% Don't display the "context from" lines if there's
		% nothing deleted or changed.
	( { list__member(AddEdit, Diff) => AddEdit = add(_, _) } ->
		[]
	;
		display_context_diff_left(Xlow, Xhigh, File1, Diff)
	),
	io__format("--- %d,%d ----\n", [i(Ylow1), i(Yhigh)]),

		% Don't display the "context to" lines if there's
		% nothing added or changed.
	( { list__member(DelEdit, Diff) => DelEdit = delete(_, _) } ->
		[]
	;
		display_context_diff_right(Ylow, Yhigh, File2, Diff)
	),
	display_context_diff_2(File1, File2, CDiff).

:- pred display_context_diff_left(int, int, file, diff, io__state, io__state).
:- mode display_context_diff_left(in, in, in, in, di, uo) is det.

display_context_diff_left(Prev, Size1, File1, []) -->
	diff_out__show_file(File1, "  ", Prev - Size1).
display_context_diff_left(Prev, Size1, File1, [Edit | Diff]) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	diff_out__show_file(File1, "  ", Prev - StartOfEdit),
	( { Edit = add(X, _) },
		{ Next = X }
	; { Edit = delete(X1 - X2, _) },
		diff_out__show_file(File1, "- ", X1 - X2),
		{ Next = X2 }
	; { Edit = change(X1 - X2, _) },
		diff_out__show_file(File1, "! ", X1 - X2),
		{ Next = X2 }
	),
	display_context_diff_left(Next, Size1, File1, Diff).

:- pred display_context_diff_right(int, int, file, diff, io__state, io__state).
:- mode display_context_diff_right(in, in, in, in, di, uo) is det.

display_context_diff_right(Prev, Size2, File2, []) -->
	diff_out__show_file(File2, "  ", Prev - Size2).
display_context_diff_right(Prev, Size2, File2, [Edit | Diff]) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	diff_out__show_file(File2, "  ", Prev - StartOfEdit),
	( { Edit = add(_, Y1 - Y2) },
		diff_out__show_file(File2, "+ ", Y1 - Y2),
		{ Next = Y2 }
	; { Edit = delete(_, Y) },
		{ Next = Y }
	; { Edit = change(_, Y1 - Y2) },
		diff_out__show_file(File2, "! ", Y1 - Y2),
		{ Next = Y2 }
	),
	display_context_diff_right(Next, Size2, File2, Diff).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Side-by-side diffs are incredibly complex, as you'll see if
	% you inspect the code below.
	%
	% TO DO: GNU diff has --sdiff-merge-assist, but I can find no
	%        documentation on what this actually does, and haven't
	%        had the time to investigate.  For the moment, we accept
	%        the option and note here whether or not it's turned on,
	%        but do nothing with it.

	% Parameters to pass around.
:- type side_by_side_info
	--->	side_by_side_info(
			int,		% Half width
			int,		% Column 2 offset
			bool,		% Left column only
			bool,		% Suppress common lines
			bool		% Help sdiff
		).

:- pred display_diff_side_by_side(file, file, diff, io__state, io__state).
:- mode display_diff_side_by_side(in, in, in, di, uo) is det.

display_diff_side_by_side(File1, File2, Diff) -->
	globals__io_lookup_int_option(width, Width0),

	% Calculate the half-width and offset stuff.

		% XXX If we're expanding tabs, we should
		%     factor this in.
	{ Off is (Width0 + 4) // 8 * 4 },
	{ Max is Off - 3 },
	{ HalfWidth0 is Width0 - Off + 1 },
	{ HalfWidth0 =< 0 ->
		HalfWidth = 0
	; HalfWidth0 > Max ->
		HalfWidth = Max
	;
		HalfWidth = HalfWidth0
	},
	{ HalfWidth > 0 ->
		Col2Off = Off
	;
		Col2Off = Width0
	},
	globals__io_lookup_bool_option(left_column, LeftCol),
	globals__io_lookup_bool_option(suppress_common_lines, Suppress),
	globals__io_lookup_bool_option(sdiff_merge_assist, Sdiff),
	{ SBS = side_by_side_info(HalfWidth, Col2Off, LeftCol,
		Suppress, Sdiff) },
	display_diff_side_by_side_2(0, SBS, File1, File2, Diff).

:- pred display_diff_side_by_side_2(int, side_by_side_info, file, file, diff,
		io__state, io__state).
:- mode display_diff_side_by_side_2(in, in, in, in, in, di, uo) is det.

display_diff_side_by_side_2(Prev, SBS, File1, _File2, []) -->
	{ SBS = side_by_side_info(_, _, _, Suppress, _) },
	( { Suppress = no } ->
		{ file__get_numlines(File1, SegEnd) },
		show_sbs_same_lines(File1, SBS, Prev - SegEnd)
	;
		[]
	).
display_diff_side_by_side_2(Prev, SBS, File1, File2, [Edit | Diff]) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	{ SBS = side_by_side_info(_, _, _, Suppress, _) },
	( { Suppress = no } ->
		show_sbs_same_lines(File1, SBS, Prev - StartOfEdit)
	;
		[]
	),
	( { Edit = add(X, Seg2) },
		show_sbs_added_lines(File2, SBS, Seg2),
		{ Next = X }
	; { Edit = delete(X1 - X2, _) },
		show_sbs_deleted_lines(File1, SBS, X1 - X2),
		{ Next = X2 }
	; { Edit = change(X1 - X2, Y1 - Y2) },
		% The side-by-side change diff format is sort of weird.
		% We have to compute the minimum of the two change sizes,
		% and display "changed" lines for the minimum of these
		% sizes.  Then we display "added" or "deleted" lines for
		% whatever is left over.
		{
			SizeX is X2 - X1,
			SizeY is Y2 - Y1,
			int__min(SizeX, SizeY, Size)

		},
		show_sbs_changed_lines(File1, File2, SBS, X1, Y1, Size),
		{
			NewX1 is X1 + Size,
			NewY1 is Y1 + Size
		},
		show_sbs_deleted_lines(File1, SBS, NewX1 - X2),
		show_sbs_added_lines(File2, SBS, NewY1 - Y2),
		{ Next = X2 }
	),
	display_diff_side_by_side_2(Next, SBS, File1, File2, Diff).

:- pred show_sbs_changed_lines(file, file, side_by_side_info, int, int, int,
		io__state, io__state).
:- mode show_sbs_changed_lines(in, in, in, in, in, in, di, uo) is det.

show_sbs_changed_lines(File1, File2, SBS, X1, Y1, Size) -->
	( { Size > 0 } ->
		(
			{ file__get_line(File1, X1, Line1),
			  file__get_line(File2, Y1, Line2)
			}
		->
			{ SBS = side_by_side_info(Width, _, _, _, _) },
			{ string__to_char_list(Line1, Chars1) },
			print_half_line(Chars1, SBS, 0, 0, Width, OutPos),
			tab_to_column(OutPos, Width),
			io__write_string("|"),
			{ Width1 is Width + 1 },
			{ Width2 is Width + 2 },
			tab_to_column(Width1, Width2),
			{ string__to_char_list(Line2, Chars2) },
			print_half_line(Chars2, SBS, 0, 0, Width, _),
			io__write_string("\n"),
			{ X2 is X1 + 1, Y2 is Y1 + 1, Size1 is Size - 1 },
			show_sbs_changed_lines(File1, File2, SBS, X2, Y2, Size1)
		;
			{ error("show_sbs_changed_lines: file ended prematurely") }
		)
	;
		[]
	).

:- pred show_sbs_same_lines(file, side_by_side_info, segment,
		io__state, io__state).
:- mode show_sbs_same_lines(in, in, in, di, uo) is det.

show_sbs_same_lines(File, SBS, Low - High) -->
	( { Low < High } ->
		( { file__get_line(File, Low, Line) } ->
			{ SBS = side_by_side_info(Width, _, LeftCol, _, _) },
			{ Low1 is Low + 1 },
			{ string__to_char_list(Line, Chars) },
			print_half_line(Chars, SBS, 0, 0, Width, OutPos),

			% If the user specified --left, don't
			% display the right column here.
			( { LeftCol = yes } ->
				tab_to_column(OutPos, Width),
				io__write_string("(")
			;
				{ Width2 is Width + 2 },
				tab_to_column(OutPos, Width2),
				print_half_line(Chars, SBS, 0, 0, Width, _)
			),
			io__write_string("\n"),
			show_sbs_same_lines(File, SBS, Low1 - High)
		;
			{ error("show_sbs_same_lines: file ended prematurely") }
		)
	;
		[]
	).

:- pred show_sbs_added_lines(file, side_by_side_info, segment,
		io__state, io__state).
:- mode show_sbs_added_lines(in, in, in, di, uo) is det.

show_sbs_added_lines(File, SBS, Low - High) -->
	( { Low < High } ->
		( { file__get_line(File, Low, Line) } ->
			{ SBS = side_by_side_info(Width, _, _, _, _) },
			{ Low1 is Low + 1 },
			{ string__to_char_list(Line, Chars) },
			tab_to_column(0, Width),
			io__write_string("> "),
			print_half_line(Chars, SBS, 0, 0, Width, _),
			io__write_string("\n"),
			show_sbs_added_lines(File, SBS, Low1 - High)
		;
			{ error("show_sbs_added_lines: file ended prematurely") }
		)
	;
		[]
	).

:- pred show_sbs_deleted_lines(file, side_by_side_info, segment,
		io__state, io__state).
:- mode show_sbs_deleted_lines(in, in, in, di, uo) is det.

show_sbs_deleted_lines(File, SBS, Low - High) -->
	( { Low < High } ->
		( { file__get_line(File, Low, Line) } ->
			{ SBS = side_by_side_info(Width, _, _, _, _) },
			{ Low1 is Low + 1 },
			{ string__to_char_list(Line, Chars) },
			print_half_line(Chars, SBS, 0, 0, Width, OutPos),
			tab_to_column(OutPos, Width),
			io__write_string("<\n"),
			show_sbs_deleted_lines(File, SBS, Low1 - High)
		;
			{ error("show_sbs_deleted_lines: file ended prematurely") }
		)
	;
		[]
	).

:- pred tab_width(int :: out) is det.
tab_width(8).

	% Put a number of spaces on the output stream.  Update
	% the output column as we go.
:- pred put_spaces(int, int, int, io__state, io__state).
:- mode put_spaces(in, in, out, di, uo) is det.

put_spaces(Spaces, OutPos0, OutPos) -->
	( { Spaces =< 0 } ->
		{ OutPos = OutPos0 }
	;
		io__write_char(' '),
		{ Spaces1 is Spaces - 1 },
		{ OutPos1 is OutPos0 + 1 },
		put_spaces(Spaces1, OutPos1, OutPos)
	).

	% Given a "from" column and a "to" column, put sufficient
	% spaces on the output stream to reach that column.  Use
	% tabs if we can.
:- pred tab_to_column(int, int, io__state, io__state).
:- mode tab_to_column(in, in, di, uo) is det.

tab_to_column(From, To) -->
	{ tab_width(Tab) },
	{ AfterTab is From + Tab - (From mod Tab) },
	( { AfterTab > To } ->
		( { From < To } ->
			io__write_char(' '),
			{ From1 is From + 1 },
			tab_to_column(From1, To)
		;
			[]
		)
	;
		io__write_char('\t'),
		tab_to_column(AfterTab, To)
	).

	% Display half a line in a side-by-side diff, stopping when
	% we reach a certain column.
	%
	% This is actually a very simple thing to do, except for one
	% complication, which is the displaying of tab characters.
	%
	% The important variables are:
	% 
	%	InPos: The current column in the input line.
	%	OutPos: The current column in the output line.
	%	OutBound: The column that we must stop at.
	%
:- pred print_half_line(list(char) :: in, side_by_side_info :: in,
		int :: in, int :: in, int :: in, int :: out,
		io__state :: di, io__state :: uo) is det.

print_half_line([], _SBS, _InPos, OutPos, _OutBound, OutPos) --> [].
print_half_line([C | Cs], SBS, InPos0, OutPos0, OutBound, OutPos) -->
	( { C = '\t' } ->
		{ tab_width(Tab) },

			% Calculate how many spaces this tab is worth.
		{ Spaces is Tab - InPos0 mod Tab },
		( { InPos0 = OutPos0 } ->
			globals__io_lookup_bool_option(expand_tabs, ExpandTabs),
			( { ExpandTabs = yes } ->
				% If we're expanding tabs, we just pretend that
				% we had Spaces spaces and write them.
				{ TabStop0 is OutPos0 + Spaces },
				{ TabStop0 > OutBound ->
					TabStop = OutBound
				;
					TabStop = TabStop0
				},
				{ WriteSpaces is TabStop - OutPos0 },
				put_spaces(WriteSpaces, OutPos0, OutPos1)
			;
				% If we're not exanding tabs, just print it and
				% hope everything lines up okay.
				io__write_char('\t'),
				{ OutPos1 is OutPos0 + Spaces }
			)
		;
			{ OutPos1 = OutPos0 }
		),
		{ InPos is InPos0 + Spaces }
	; { C = '\r' ; C = '\b' ; C = '\n' } ->
		% XXX What to do?  For the moment, we'll just ignore it.
		{ InPos = InPos0, OutPos1 = OutPos0 }
	/***********
		% XXX Binary files aren't really supported.
	; { \+ char__is_print(C) } ->
		{ InPos = InPos0, OutPos1 = OutPos0 }
		( { InPos < OutBound } ->
			io__write_char(C)
		;
			[]
		)
	***********/
	;
		% The default case.  Print and be done with it.
		{ InPos is InPos0+1 },
		( { InPos < OutBound } ->
			{ OutPos1 = InPos },
			io__write_char(C)
		;
			{ OutPos1 = OutPos0 }
		)
	),
	print_half_line(Cs, SBS, InPos, OutPos1, OutBound, OutPos).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
