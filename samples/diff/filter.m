%-----------------------------------------------------------------------------%
% Copyright (C) 1998, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: filter.m.
% Main author: bromage.
% 
% This module contains code to filter the diff before output, based on
% the command-line options presented.
% 
% At the moment, only one option is handled: --ignore-blank-lines.
% This causes edits to be dropped if they contain changes which only
% add, delete or change blank lines.
% 
% TO DO: What exactly is a blank line, and does its definition change
%        if --ignore-space-change or --ignore-all-space have been
%        specified?  At the moment, we define a blank line to be a line
%        containing zero or more whitespace characters.  Check if this is
%        correct or not.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module filter.
:- interface.

:- import_module difftype, file, io.

:- pred filter_diff(diff::in, file::in, file::in, diff::out, io::di, io::uo)
	is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module globals, options.
:- import_module bool, list, int, pair, string, char.

filter_diff(Diff0, File1, File2, Diff) -->
	globals__io_lookup_bool_option(ignore_blank_lines, FilterBlank),

	{ FilterBlank = no ->
		% If we didn't request a filter, skip this pass.

		Diff = Diff0
	;
		filter__blank_lines(Diff0, File1, File2, Diff)
	}.

:- pred filter__blank_lines(diff :: in, file :: in, file :: in, diff :: out)
		is det.

filter__blank_lines([], _, _, []).
filter__blank_lines([Edit | Diff0], File1, File2, Diff) :-
	filter__blank_lines(Diff0, File1, File2, Diff1),
	( Edit = add(_, Y1 - Y2),
		( range_has_only_blank_lines(Y1, Y2, File2) ->
			Diff = Diff1
		;
			Diff = [Edit | Diff1]
		)
	; Edit = delete(X1 - X2, _),
		( range_has_only_blank_lines(X1, X2, File1) ->
			Diff = Diff1
		;
			Diff = [Edit | Diff1]
		)
	; Edit = change(X1 - X2, Y1 - Y2),
		(
			range_has_only_blank_lines(X1, X2, File1),
			range_has_only_blank_lines(Y1, Y2, File2)
		->
			Diff = Diff1
		;
			Diff = [Edit | Diff1]
		)
	).

%-----------------------------------------------------------------------------%

:- pred range_has_only_blank_lines(int, int, file).
:- mode range_has_only_blank_lines(in, in, in) is semidet.

range_has_only_blank_lines(First, Last, File) :-
	(
		First < Last
	=>
		(
			file__get_line(File, First, Line),
			string__to_char_list(Line, Chars),
			all [C] (
				list__member(C, Chars)
			=>
				char__is_whitespace(C)
			),
			range_has_only_blank_lines(First + 1, Last, File)
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
