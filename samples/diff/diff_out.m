%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% Main author: bromage
% Based on diffs.m, written by bromage and simplified by
% Marnix Klooster <marnix@worldonline.nl>
% 
% This module contains the predicates to display a diff in various
% output styles, based on the command-line options supplied.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module diff_out.
:- interface.

:- import_module difftype.
:- import_module file.

:- import_module int.
:- import_module io.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type diff_out__output_style
	--->	normal
	;	help_only
	;	version_only
	;	context(int)
	;	unified(int)
	;	ed
	;	forward_ed
	;	rcs
	;	ifdef(string)
	;	brief
	;	side_by_side
	;	cvs_merge_conflict.

	% The default output style.
:- pred diff_out__default_output_style(diff_out__output_style).
:- mode diff_out__default_output_style(out) is det.

	% Succeeds if, for this output style, an absence of differences
	% means that no output should be generated.
:- pred diff_out__no_diff_implies_no_output(diff_out__output_style).
:- mode diff_out__no_diff_implies_no_output(in) is semidet.

	% Succeeds if the user only wants to know about the presence
	% of any differences, not what they actually are.
:- pred diff_out__full_diff_not_required(diff_out__output_style).
:- mode diff_out__full_diff_not_required(in) is semidet.

	% Succeeds if the output style is "robust", that is, the
	% absence of a newline at the end of the file actually
	% matters.
:- pred diff_out__robust(diff_out__output_style).
:- mode diff_out__robust(in) is semidet.

	% display_diff takes a diff and displays it
	% in the user's specified output format.
:- pred display_diff(file, file, diff, io__state, io__state).
:- mode display_diff(in, in, in, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals.
:- import_module options.

:- import_module bool.
:- import_module char.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

diff_out__default_output_style(normal).

%-----------------------------------------------------------------------------%

diff_out__no_diff_implies_no_output(normal).
diff_out__no_diff_implies_no_output(context(_)).
diff_out__no_diff_implies_no_output(unified(_)).
diff_out__no_diff_implies_no_output(ed).
diff_out__no_diff_implies_no_output(forward_ed).
diff_out__no_diff_implies_no_output(rcs).
diff_out__no_diff_implies_no_output(brief).

%-----------------------------------------------------------------------------%

diff_out__full_diff_not_required(brief).

%-----------------------------------------------------------------------------%

diff_out__robust(normal).
diff_out__robust(context(_)).
diff_out__robust(unified(_)).
diff_out__robust(rcs).
diff_out__robust(ifdef(_)).
diff_out__robust(side_by_side).
diff_out__robust(cvs_merge_conflict).

%-----------------------------------------------------------------------------%

	% diff_out__show_file shows the segment of the file
	% from Low to High, with each line preceeded by
	% the Prefix characher and a space.  The diff(1)
	% format specifies that the lines effected in the
	% first file should be flagged by '<' and the
	% lines effected in the second file should be
	% flagged by '>'.
	%
:- pred diff_out__show_file(file, string, pos, pos, io__state, io__state).
:- mode diff_out__show_file(in, in, in, in, di, uo) is det.

diff_out__show_file(File, Prefix, Low, High) -->
	globals__io_lookup_bool_option(expand_tabs, ExpandTabs),
	diff_out__show_file_2(ExpandTabs, File, Prefix, Low, High).

	% NOTE: GCC 2.7.2 under Digital Unix 3.2 doesn't compile
	%       this predicate correctly with optimisation turned on.
:- pred diff_out__show_file_2(bool, file, string, pos, pos,
		io__state, io__state).
:- mode diff_out__show_file_2(in, in, in, in, in, di, uo) is det.

diff_out__show_file_2(ExpandTabs, File, Prefix, Low, High) -->
	( { Low < High } ->
		( { file__get_line(File, Low, Line) } ->
			io__write_string(Prefix),
			( { ExpandTabs = yes },
				{ string__to_char_list(Line, LineList) },
				diff_out__expand_tabs(LineList, 0)
			; { ExpandTabs = no },
				io__write_string(Line)
			),
			diff_out__show_file_2(ExpandTabs, File, Prefix,
					Low + 1, High)
		;
			{ error("diff_out_show_file: file ended prematurely") }
		)
	;
		[]
	).

:- pred diff_out__expand_tabs(list(char), int, io__state, io__state).
:- mode diff_out__expand_tabs(in, in, di, uo) is det.

diff_out__expand_tabs([], _) --> [].
diff_out__expand_tabs([C | Cs], Pos) -->
	( { C = '\t' } ->
		{ Spaces = tab_width - (Pos rem tab_width) },
		put_spaces(Spaces, Pos, NewPos),
		diff_out__expand_tabs(Cs, NewPos)
	;
		io__write_char(C),
		diff_out__expand_tabs(Cs, Pos + 1)
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
	(
		{ Diff = [],
		  diff_out__no_diff_implies_no_output(OutputStyle)
		}
	->
		[]
	;
		display_diff_2(OutputStyle, File1, File2, Diff)
	).


:- pred display_diff_2(diff_out__output_style, file, file, diff,
			io__state, io__state).
:- mode display_diff_2(in, in, in, in, di, uo) is det.

display_diff_2(normal, File1, File2, Diff) -->
	display_diff_normal(File1, File2, Diff).

display_diff_2(help_only, _File1, _File2, _Diff) -->
	{ error("display_diff: help_only") }.

display_diff_2(version_only, _File1, _File2, _Diff) -->
	{ error("display_diff: version_only") }.

display_diff_2(context(Context), File1, File2, Diff) -->
	display_context_diff(Context, File1, File2, Diff).

display_diff_2(unified(Context), File1, File2, Diff) -->
	display_unified_diff(Context, File1, File2, Diff).

display_diff_2(ed, File1, File2, Diff) -->
	display_diff_ed(File1, File2, Diff).

display_diff_2(forward_ed, File1, File2, Diff) -->
	display_diff_forward_ed(File1, File2, Diff).

display_diff_2(rcs, File1, File2, Diff) -->
	display_diff_rcs(File1, File2, Diff).

display_diff_2(ifdef(Sym), File1, File2, Diff) -->
	display_diff_ifdef(Sym, File1, File2, Diff).

display_diff_2(brief, File1, File2, _Diff) -->
	% XXX For this output style, we really don't need to
	%     perform a complete diff.  This should be handled
	%     higher up for efficiency.
	{ file__get_file_name(File1, FileName1) },
	{ file__get_file_name(File2, FileName2) },
	io__write_strings(["Files ", FileName1, " and ",
		FileName2, " differ\n"]).

display_diff_2(side_by_side, File1, File2, Diff) -->
	display_diff_side_by_side(File1, File2, Diff).

display_diff_2(cvs_merge_conflict, File1, File2, Diff) -->
	display_diff_cvs_merge_conflict(File1, File2, Diff).

%-----------------------------------------------------------------------------%

	% display_diff_normal takes a diff and displays it
	% in the standard diff(1) output format.
:- pred display_diff_normal(file, file, diff, io__state, io__state).
:- mode display_diff_normal(in, in, in, di, uo) is det.

display_diff_normal(File1, File2, Diff) -->
	globals__io_lookup_bool_option(initial_tab, InitialTab),
	{ InitialTab = no,
		FromStr = "< ",
		ToStr = "> "
	; InitialTab = yes,
		FromStr = "<\t",
		ToStr = ">\t"
	},
	display_diff_normal_2(File1, File2, Diff, FromStr, ToStr).

	% display_diff_normal takes a diff and displays it
	% in the standard diff(1) output format.
:- pred display_diff_normal_2(file, file, diff, string, string,
			io__state, io__state).
:- mode display_diff_normal_2(in, in, in, in, in, di, uo) is det.

display_diff_normal_2(_, _, [], _, _) --> [].
display_diff_normal_2(File1, File2, [SingDiff | Diff], FromStr, ToStr) -->
	( { SingDiff = add(X, Y1 - Y2) },
		diff_out__write_command(X - X, 'a', Y1 - Y2),
		diff_out__show_file(File2, ToStr, Y1, Y2)
	; { SingDiff = delete(X1 - X2, Y) },
		diff_out__write_command(X1 - X2, 'd', Y - Y),
		diff_out__show_file(File1, FromStr, X1, X2)
	; { SingDiff = change(X1 - X2, Y1 - Y2) },
		diff_out__write_command(X1 - X2, 'c', Y1 - Y2),
		diff_out__show_file(File1, FromStr, X1, X2),
		io__write_string("---\n"),
		diff_out__show_file(File2, ToStr, Y1, Y2)
	),
	display_diff_normal_2(File1, File2, Diff, FromStr, ToStr).


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
		diff_out__write_command_rcs('a', X, Y2-Y1),
		diff_out__show_file(File2, "", Y1, Y2)
	; { Cmd = delete(X1 - X2, _Y) },
		diff_out__write_command_rcs('d', X1, X2-X1)
	; { Cmd = change(X1 - X2, Y1 - Y2) },
		diff_out__write_command_rcs('d', X1, X2-X1),
		diff_out__write_command_rcs('a', X1, Y2-Y1),
		diff_out__show_file(File2, "", Y1, Y2)
	),
	display_diff_rcs(File1, File2, Diff).


	% diff_out__write_command_rcs displays a diff command in
	% the RCS ,v format.
:- pred diff_out__write_command_rcs(char, int, int, io__state, io__state).
:- mode diff_out__write_command_rcs(in, in, in, di, uo) is det.

diff_out__write_command_rcs(C, X, Y) -->
	io__write_char(C),
	io__write_int(X + 1),	% Convert from pos to line number
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
		diff_out__show_file(File2, "", Y1, Y2),
		io__write_string(".\n")
	; { Cmd = delete(X, _Y) },
		diff_out__write_command_ed(X, 'd')
	; { Cmd = change(X, Y1 - Y2) },
		diff_out__write_command_ed(X, 'c'),
		diff_out__show_file(File2, "", Y1, Y2),
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
		diff_out__show_file(File2, "", Y1, Y2),
		io__write_string(".\n")
	; { Cmd = delete(X, _Y) },
		diff_out__write_command_forward_ed(X, 'd')
	; { Cmd = change(X, Y1 - Y2) },
		diff_out__write_command_forward_ed(X, 'c'),
		diff_out__show_file(File2, "", Y1, Y2),
		io__write_string(".\n")
	),
	display_diff_forward_ed(File1, File2, Diff).

	% diff_out__write_command_forward_ed displays a forward ed(1)
	% command.  The difference between this and write_command_ed is
	% that the command char comes first here.  Who comes up with
	% these dumb formats anyway?
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
	diff_out__show_file(File1, "", Prev, SegEnd).
display_diff_ifdef_2(Prev, Sym, File1, File2, [Edit | Diff]) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	diff_out__show_file(File1, "", Prev, StartOfEdit),
	( { Edit = add(X, Y1 - Y2) },
		io__write_strings(["#ifdef ", Sym, "\n"]),
		diff_out__show_file(File2, "", Y1, Y2),
		io__write_strings(["#endif /* ", Sym, " */\n"]),
		{ Next = X }
	; { Edit = delete(X1 - X2, _) },
		io__write_strings(["#ifndef ", Sym, "\n"]),
		diff_out__show_file(File1, "", X1, X2),
		io__write_strings(["#endif /* not ", Sym, " */\n"]),
		{ Next = X2 }
	; { Edit = change(X1 - X2, Y1 - Y2) },
		io__write_strings(["#ifndef ", Sym, "\n"]),
		diff_out__show_file(File1, "", X1, X2),
		io__write_strings(["#else /* ", Sym, " */\n"]),
		diff_out__show_file(File2, "", Y1, Y2),
		io__write_strings(["#endif /* ", Sym, " */\n"]),
		{ Next = X2 }
	),
	display_diff_ifdef_2(Next, Sym, File1, File2, Diff).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% display_diff_cvs_merge_conflict writes out the files in a
	% unified diff, using CVS merge conflict marks around each edit.
	%
:- pred display_diff_cvs_merge_conflict(file, file, diff, io__state, io__state).
:- mode display_diff_cvs_merge_conflict(in, in, in, di, uo) is det.

display_diff_cvs_merge_conflict(File1, File2, Diff) -->
	display_diff_cvs_merge_conflict_2(0, File1, File2, Diff).

	% Argument 1 (prev) is the last pos displayed before
	% the current edit (or end of edits, in the base case).
	% This is important for when we have to display the
	% "non-diffed" text between edits.
:- pred display_diff_cvs_merge_conflict_2(int, file, file, diff,
		io__state, io__state).
:- mode display_diff_cvs_merge_conflict_2(in, in, in, in, di, uo) is det.

display_diff_cvs_merge_conflict_2(Prev, File1, _File2, []) -->
	{ file__get_numlines(File1, SegEnd) },
	diff_out__show_file(File1, "", Prev, SegEnd).
display_diff_cvs_merge_conflict_2(Prev, File1, File2, [Edit | Diff]) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	diff_out__show_file(File1, "", Prev, StartOfEdit),
	{ file__get_file_name(File1, FileName1) },
	{ file__get_file_name(File2, FileName2) },
	( { Edit = add(X, Y1 - Y2) },
		io__write_strings(["<<<<<<< ", FileName1, "\n"]),
		diff_out__show_file(File2, "", Y1, Y2),
		io__write_string("=======\n"),
		io__write_strings([">>>>>>> ", FileName2, "\n"]),
		{ Next = X }
	; { Edit = delete(X1 - X2, _) },
		io__write_strings(["<<<<<<< ", FileName1, "\n"]),
		io__write_string("=======\n"),
		diff_out__show_file(File1, "", X1, X2),
		io__write_strings([">>>>>>> ", FileName2, "\n"]),
		{ Next = X2 }
	; { Edit = change(X1 - X2, Y1 - Y2) },
		io__write_strings(["<<<<<<< ", FileName1, "\n"]),
		diff_out__show_file(File1, "", X1, X2),
		io__write_string("=======\n"),
		diff_out__show_file(File2, "", Y1, Y2),
		io__write_strings([">>>>>>> ", FileName2, "\n"]),
		{ Next = X2 }
	),
	display_diff_cvs_merge_conflict_2(Next, File1, File2, Diff).

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

		% Work out how far the context of this edit reaches.
	first_mentioned_positions(Edit, Xfirst0, Yfirst0),
	int__max(Xfirst0 - Context, 0, Xfirst),
	int__max(Yfirst0 - Context, 0, Yfirst),
	last_mentioned_positions(Edit, Xlast0, Ylast0),
	int__min(Xlast0 + Context, Xsize, Xlast),
	int__min(Ylast0 + Context, Ysize, Ylast),

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
		% XXX Should also print out file dates.  But how?
	io__write_strings(["--- ", Name1, "\n"]),
	io__write_strings(["+++ ", Name2, "\n"]),
	globals__io_lookup_bool_option(initial_tab, InitialTab),
	{ InitialTab = no,
		NoneStr = " ",
		AddStr = "+",
		DelStr = "-"
	; InitialTab = yes,
		NoneStr = "\t",
		AddStr = "+\t",
		DelStr = "-\t"
	},
	display_unified_diff_2(File1, File2, CDiff, NoneStr, AddStr, DelStr).

:- pred display_unified_diff_2(file, file, context_diff, string, string, string,
				io__state, io__state).
:- mode display_unified_diff_2(in, in, in, in, in, in, di, uo) is det.

display_unified_diff_2(_File1, _File2, [], _, _, _) --> [].
display_unified_diff_2(File1, File2, [Edit | CDiff],
			NoneStr, AddStr, DelStr) -->
	{ Edit = context_edit(Xlow - Xhigh, Ylow - Yhigh, Diff) },
	io__format("@@ -%d,%d +%d,%d @@\n",
		[i(Xlow + 1), i(Xhigh - Xlow), i(Ylow + 1), i(Yhigh - Ylow)]),
	display_unified_diff_3(Xlow, Xhigh, File1, File2, Diff,
			NoneStr, AddStr, DelStr),
	display_unified_diff_2(File1, File2, CDiff, NoneStr, AddStr, DelStr).

:- pred display_unified_diff_3(int, int, file, file, diff,
				string, string, string, io__state, io__state).
:- mode display_unified_diff_3(in, in, in, in, in, in, in, in, di, uo) is det.

display_unified_diff_3(Prev, Size1, File1, _File2, [], NoneStr, _, _) -->
	diff_out__show_file(File1, NoneStr, Prev, Size1).
display_unified_diff_3(Prev, Size1, File1, File2, [Edit | Diff],
			NoneStr, AddStr, DelStr) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	diff_out__show_file(File1, NoneStr, Prev, StartOfEdit),
	( { Edit = add(X, Y1 - Y2) },
		diff_out__show_file(File2, AddStr, Y1, Y2),
		{ Next = X }
	; { Edit = delete(X1 - X2, _) },
		diff_out__show_file(File1, DelStr, X1, X2),
		{ Next = X1 }
	; { Edit = change(X1 - X2, Y1 - Y2) },
		diff_out__show_file(File1, DelStr, X1, X2),
		diff_out__show_file(File2, AddStr, Y1, Y2),
		{ Next = X1 }
	),
	display_unified_diff_3(Next, Size1, File1, File2, Diff,
				NoneStr, AddStr, DelStr).

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

	globals__io_lookup_bool_option(initial_tab, InitialTab),
	{ InitialTab = no,
		NoneStr = "  ",
		AddStr = "+ ",
		DelStr = "- ",
		ChgStr = "! "
	; InitialTab = yes,
		NoneStr = "\t",
		AddStr = "+\t",
		DelStr = "-\t",
		ChgStr = "!\t"
	},
	display_context_diff_2(File1, File2, CDiff,
			NoneStr, AddStr, DelStr, ChgStr).

:- pred display_context_diff_2(file, file, context_diff,
		string, string, string, string, io__state, io__state).
:- mode display_context_diff_2(in, in, in, in, in, in, in, di, uo) is det.

display_context_diff_2(_File1, _File2, [], _, _, _, _) --> [].
display_context_diff_2(File1, File2, [Edit | CDiff],
		NoneStr, AddStr, DelStr, ChgStr) -->
	{ Edit = context_edit(Xlow - Xhigh, Ylow - Yhigh, Diff) },
	io__write_string("***************\n"),
	io__format("*** %d,%d ****\n", [i(Xlow + 1), i(Xhigh)]),

		% Don't display the "context from" lines if there's
		% nothing deleted or changed.
	( { all [AEdit] list__member(AEdit, Diff) => AEdit = add(_, _) } ->
		[]
	;
		display_context_diff_left(Xlow, Xhigh, File1, Diff,
				NoneStr, DelStr, ChgStr)
	),
	io__format("--- %d,%d ----\n", [i(Ylow + 1), i(Yhigh)]),

		% Don't display the "context to" lines if there's
		% nothing added or changed.
	( { all [DEdit] list__member(DEdit, Diff) => DEdit = delete(_, _) } ->
		[]
	;
		display_context_diff_right(Ylow, Yhigh, File2, Diff,
				NoneStr, AddStr, ChgStr)
	),
	display_context_diff_2(File1, File2, CDiff,
			NoneStr, AddStr, DelStr, ChgStr).

:- pred display_context_diff_left(int, int, file, diff, string, string, string,
			io__state, io__state).
:- mode display_context_diff_left(in, in, in, in, in, in, in, di, uo) is det.

display_context_diff_left(Prev, Size1, File1, [], NoneStr, _, _) -->
	diff_out__show_file(File1, NoneStr, Prev, Size1).
display_context_diff_left(Prev, Size1, File1, [Edit | Diff],
			NoneStr, DelStr, ChgStr) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	diff_out__show_file(File1, NoneStr, Prev, StartOfEdit),
	( { Edit = add(X, _) },
		{ Next = X }
	; { Edit = delete(X1 - X2, _) },
		diff_out__show_file(File1, DelStr, X1, X2),
		{ Next = X2 }
	; { Edit = change(X1 - X2, _) },
		diff_out__show_file(File1, ChgStr, X1, X2),
		{ Next = X2 }
	),
	display_context_diff_left(Next, Size1, File1, Diff,
			NoneStr, DelStr, ChgStr).

:- pred display_context_diff_right(int, int, file, diff,
			string, string, string, io__state, io__state).
:- mode display_context_diff_right(in, in, in, in, in, in, in, di, uo) is det.

display_context_diff_right(Prev, Size2, File2, [], NoneStr, _, _) -->
	diff_out__show_file(File2, NoneStr, Prev, Size2).
display_context_diff_right(Prev, Size2, File2, [Edit | Diff],
			NoneStr, AddStr, ChgStr) -->
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
	diff_out__show_file(File2, NoneStr, Prev, StartOfEdit),
	( { Edit = add(_, Y1 - Y2) },
		diff_out__show_file(File2, AddStr, Y1, Y2),
		{ Next = Y2 }
	; { Edit = delete(_, Y) },
		{ Next = Y }
	; { Edit = change(_, Y1 - Y2) },
		diff_out__show_file(File2, ChgStr, Y1, Y2),
		{ Next = Y2 }
	),
	display_context_diff_right(Next, Size2, File2, Diff,
				NoneStr, AddStr, ChgStr).

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
	{ SBS = side_by_side_info(_, _, _, Suppress, _) },
	{ first_mentioned_positions(Edit, StartOfEdit, _) },
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
		{ int__min(X2 - X1, Y2 - Y1, Size) },
		show_sbs_changed_lines(File1, File2, SBS, X1, Y1, Size),
		show_sbs_deleted_lines(File1, SBS, (X1 + Size) - X2),
		show_sbs_added_lines(File2, SBS, (Y1 + Size) - Y2),
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
			tab_to_column(Width + 1, Width + 2),
			{ string__to_char_list(Line2, Chars2) },
			print_half_line(Chars2, SBS, 0, 0, Width, _),
			io__write_string("\n"),
			show_sbs_changed_lines(File1, File2, SBS,
					X1 + 1, Y1 + 1, Size - 1)
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
			{ string__to_char_list(Line, Chars) },
			print_half_line(Chars, SBS, 0, 0, Width, OutPos),

			% If the user specified --left, don't
			% display the right column here.
			( { LeftCol = yes } ->
				tab_to_column(OutPos, Width),
				io__write_string("(")
			;
				tab_to_column(OutPos, Width + 2),
				print_half_line(Chars, SBS, 0, 0, Width, _)
			),
			io__write_string("\n"),
			show_sbs_same_lines(File, SBS, (Low + 1) - High)
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
			{ string__to_char_list(Line, Chars) },
			tab_to_column(0, Width),
			io__write_string("> "),
			print_half_line(Chars, SBS, 0, 0, Width, _),
			io__write_string("\n"),
			show_sbs_added_lines(File, SBS, (Low + 1) - High)
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
			{ string__to_char_list(Line, Chars) },
			print_half_line(Chars, SBS, 0, 0, Width, OutPos),
			tab_to_column(OutPos, Width),
			io__write_string("<\n"),
			show_sbs_deleted_lines(File, SBS, (Low + 1) - High)
		;
			{ error("show_sbs_deleted_lines: file ended prematurely") }
		)
	;
		[]
	).

:- func tab_width = int.
tab_width = 8.

	% Put a number of spaces on the output stream.  Update
	% the output column as we go.
:- pred put_spaces(int, int, int, io__state, io__state).
:- mode put_spaces(in, in, out, di, uo) is det.

put_spaces(Spaces, OutPos0, OutPos) -->
	( { Spaces =< 0 } ->
		{ OutPos = OutPos0 }
	;
		io__write_char(' '),
		put_spaces(Spaces - 1, OutPos0 + 1, OutPos)
	).

	% Given a "from" column and a "to" column, put sufficient
	% spaces on the output stream to reach that column.  Use
	% tabs if we can.
:- pred tab_to_column(int, int, io__state, io__state).
:- mode tab_to_column(in, in, di, uo) is det.

tab_to_column(From, To) -->
	{ AfterTab is From + tab_width - (From rem tab_width) },
	( { AfterTab > To } ->
		( { From < To } ->
			io__write_char(' '),
			tab_to_column(From + 1, To)
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
			% Calculate how many spaces this tab is worth.
		{ Spaces is tab_width - InPos0 rem tab_width },
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
				put_spaces(TabStop - OutPos0, OutPos0, OutPos1)
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
		{ InPos is InPos0 + 1 },
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
