%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% error_util.m
% Main author: zs.
% 
% This module contains code that can be helpful in the formatting of
% error messages.
%
%-----------------------------------------------------------------------------%

:- module error_util.

:- interface.

:- import_module io, list, term, string, int.

	% Given a context, a starting indentation level and a list of words,
	% print an error message that looks like this:
	%
	% module.m:10: first line of error message blah blah blah
	% module.m:10:   second line of error message blah blah blah
	% module.m:10:   third line of error message blah blah blah
	%
	% The words will be packed into lines as tightly as possible,
	% with spaces between each pair of words, subject to the constraints
	% that every line starts with a context, followed by Indent+1 spaces
	% on the first line and Indent+3 spaces on later lines, and that every
	% line contains at most 79 characters (unless a long single word
	% forces the line over this limit).

:- pred write_error_pieces(term__context::in, int::in, list(string)::in,
	io__state::di, io__state::uo) is det.

:- implementation.

:- import_module prog_out.

write_error_pieces(Context, Indent, Words) -->
	{
			% The fixed characters at the start of the line are:
			% filename
			% :
			% line number (min 3 chars)
			% :
			% space
			% indent
		Context = term__context(FileName, LineNumber),
		string__length(FileName, FileNameLength),
		string__int_to_string(LineNumber, LineNumberStr),
		string__length(LineNumberStr, LineNumberStrLength0),
		( LineNumberStrLength0 < 3 ->
			LineNumberStrLength = 3
		;
			LineNumberStrLength = LineNumberStrLength0
		),
		Remain is 79 - (FileNameLength + 1 +
			LineNumberStrLength + 2 + Indent),
		group_words(Words, Remain, Lines)
	},
	write_lines(Lines, Context, Indent).

:- pred write_lines(list(list(string))::in, term__context::in, int::in,
	io__state::di, io__state::uo) is det.

write_lines([], _, _) --> [].
write_lines([Line | Lines], Context, Indent) -->
	prog_out__write_context(Context),
	{ string__pad_left("", ' ', Indent, IndentStr) },
	io__write_string(IndentStr),
	write_line(Line),
	{ Indent2 is Indent + 2 },
	write_nonfirst_lines(Lines, Context, Indent2).

:- pred write_nonfirst_lines(list(list(string))::in, term__context::in, int::in,
	io__state::di, io__state::uo) is det.

write_nonfirst_lines([], _, _) --> [].
write_nonfirst_lines([Line | Lines], Context, Indent) -->
	prog_out__write_context(Context),
	{ string__pad_left("", ' ', Indent, IndentStr) },
	io__write_string(IndentStr),
	write_line(Line),
	write_nonfirst_lines(Lines, Context, Indent).

:- pred write_line(list(string)::in, io__state::di, io__state::uo) is det.

write_line([]) --> [].
write_line([Word | Words]) -->
	io__write_string(Word),
	write_line_rest(Words),
	io__write_char('\n').

:- pred write_line_rest(list(string)::in, io__state::di, io__state::uo) is det.

write_line_rest([]) --> [].
write_line_rest([Word | Words]) -->
	io__write_char(' '),
	io__write_string(Word),
	write_line_rest(Words).

%----------------------------------------------------------------------------%

	% Groups the given words into lines. The first line can have up to Max
	% characters on it; the later lines (if any) up to Max-2 characters.
	% The given list of words must be nonempty, since we always return
	% at least one line.

:- pred group_words(list(string)::in, int::in, list(list(string))::out) is det.

group_words(Words, Max, Lines) :-
	(
		Words = [],
		Lines = []
	;
		Words = [FirstWord | LaterWords],
		get_line_of_words(FirstWord, LaterWords, Max, Line, RestWords),
		Max2 is Max - 2,
		group_nonfirst_line_words(RestWords, Max2, RestLines),
		Lines = [Line | RestLines]
	).

:- pred group_nonfirst_line_words(list(string)::in, int::in,
	list(list(string))::out) is det.

group_nonfirst_line_words(Words, Max, Lines) :-
	(
		Words = [],
		Lines = []
	;
		Words = [FirstWord | LaterWords],
		get_line_of_words(FirstWord, LaterWords, Max, Line, RestWords),
		group_nonfirst_line_words(RestWords, Max, RestLines),
		Lines = [Line | RestLines]
	).

:- pred get_line_of_words(string::in, list(string)::in, int::in,
	list(string)::out, list(string)::out) is det.

get_line_of_words(FirstWord, LaterWords, MaxLen, Line, RestWords) :-
	string__length(FirstWord, FirstWordLen),
	get_later_words(LaterWords, FirstWordLen, MaxLen, [FirstWord],
		Line, RestWords).

:- pred get_later_words(list(string)::in, int::in, int::in,
	list(string)::in, list(string)::out, list(string)::out) is det.

get_later_words([], _, _, Line, Line, []).
get_later_words([Word | Words], OldLen, MaxLen, Line0, Line, RestWords) :-
	string__length(Word, WordLen),
	NewLen is OldLen + 1 + WordLen,
	( NewLen =< MaxLen ->
		append(Line0, [Word], Line1),
		get_later_words(Words, NewLen, MaxLen,
			Line1, Line, RestWords)
	;
		Line = Line0,
		RestWords = [Word | Words]
	).
