%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1999 The University of Melbourne.
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

:- import_module hlds_module, hlds_pred, prog_data.
:- import_module assoc_list, io, list, std_util.

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
	%
	% The caller supplies the list of words to be printed in the form
	% of a list of error message components. Each component may specify
	% a string to printed exactly as it is, or it may specify a string
	% containing a list of words, which may be broken at white space.

:- type format_component
	--->	fixed(string)	% This string should appear in the output
				% in one piece, as it is.

	;	words(string)	% This string contains words separated by
				% white space. The words should appear in
				% the output in the given order, but the
				% white space may be rearranged and line
				% breaks may be inserted.

	;	nl		% Insert a line break if there has been text
				% output since the last line break.
	.
		

:- pred error_util__list_to_pieces(list(string)::in,
		list(format_component)::out) is det.

:- pred write_error_pieces(prog_context::in, int::in,
	list(format_component)::in, io__state::di, io__state::uo) is det.


	% Predicates to convert a predicate names to strings.

:- pred error_util__describe_one_pred_name(module_info::in, pred_id::in,
	string::out) is det.

:- pred error_util__describe_one_proc_name(module_info::in, pred_proc_id::in,
	string::out) is det.

:- pred error_util__describe_several_proc_names(module_info::in,
	list(pred_proc_id)::in, list(format_component)::out) is det.

:- pred error_util__describe_one_call_site(module_info::in,
	pair(pred_proc_id, prog_context)::in, string::out) is det.

:- pred error_util__describe_several_call_sites(module_info::in,
	assoc_list(pred_proc_id, prog_context)::in,
	list(format_component)::out) is det.

:- implementation.

:- import_module prog_out.
:- import_module bool, io, list, term, char, string, int.

error_util__list_to_pieces([], []).
error_util__list_to_pieces([Elem], [words(Elem)]).
error_util__list_to_pieces([Elem1, Elem2],
		[fixed(Elem1), words("and"), fixed(Elem2)]).
error_util__list_to_pieces([Elem1, Elem2, Elem3 | Elems], Pieces) :-
	string__append(Elem1, ",", Piece1),
	error_util__list_to_pieces([Elem2, Elem3 | Elems], Pieces1),
	Pieces = [fixed(Piece1) | Pieces1].

write_error_pieces(Context, Indent, Components) -->
	{
			% The fixed characters at the start of the line are:
			% filename
			% :
			% line number (min 3 chars)
			% :
			% space
			% indent
		term__context_file(Context, FileName),
		term__context_line(Context, LineNumber),
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
		convert_components_to_word_list(Components, [], [], Words),
		group_words(yes, Words, Remain, Lines)
	},
	write_lines(Lines, Context, Indent).

:- pred write_lines(list(list(string))::in, prog_context::in, int::in,
	io__state::di, io__state::uo) is det.

write_lines([], _, _) --> [].
write_lines([Line | Lines], Context, Indent) -->
	prog_out__write_context(Context),
	{ string__pad_left("", ' ', Indent, IndentStr) },
	io__write_string(IndentStr),
	write_line(Line),
	{ Indent2 is Indent + 2 },
	write_nonfirst_lines(Lines, Context, Indent2).

:- pred write_nonfirst_lines(list(list(string))::in, prog_context::in, int::in,
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

:- pred convert_components_to_word_list(list(format_component)::in,
		list(string)::in, list(list(string))::in,
		list(list(string))::out) is det.

convert_components_to_word_list([], Words0, Paras0, Paras) :-
	list__reverse(Words0, Words),
	list__reverse([Words | Paras0], Paras).
convert_components_to_word_list([Component | Components], Words0,
		Paras0, Paras) :-
	(
		Component = fixed(Word),
		Words1 = [Word | Words0],
		Paras1 = Paras0
	;
		Component = words(WordsStr),
		break_into_words(WordsStr, Words0, Words1),
		Paras1 = Paras0
	;
		Component = nl,
		list__reverse(Words0, Words),
		Paras1 = [Words | Paras0],
		Words1 = []
	),
	convert_components_to_word_list(Components, Words1, Paras1, Paras).

:- pred break_into_words(string::in, list(string)::in,
		list(string)::out) is det.

break_into_words(String, Words0, Words) :-
	break_into_words_from(String, 0, Words0, Words).

:- pred break_into_words_from(string::in, int::in,
		list(string)::in, list(string)::out) is det.

break_into_words_from(String, Cur, Words0, Words) :-
	( find_word_start(String, Cur, Start) ->
		find_word_end(String, Start, End),
		Length is End - Start + 1,
		string__substring(String, Start, Length, Word),
		Next is End + 1,
		break_into_words_from(String, Next, [Word | Words0], Words)
	;
		Words = Words0
	).

:- pred find_word_start(string::in, int::in, int::out) is semidet.

find_word_start(String, Cur, WordStart) :-
	string__index(String, Cur, Char),
	( char__is_whitespace(Char) ->
		Next is Cur + 1,
		find_word_start(String, Next, WordStart)
	;
		WordStart = Cur
	).

:- pred find_word_end(string::in, int::in, int::out) is det.

find_word_end(String, Cur, WordEnd) :-
	Next is Cur + 1,
	( string__index(String, Next, Char) ->
		( char__is_whitespace(Char) ->
			WordEnd = Cur
		;
			find_word_end(String, Next, WordEnd)
		)
	;
		WordEnd = Cur
	).

%----------------------------------------------------------------------------%

	% Groups the given words into lines. The first line can have up to Max
	% characters on it; the later lines (if any) up to Max-2 characters.
	% The given list of words must be nonempty, since we always return
	% at least one line.

:- pred group_words(bool::in, list(list(string))::in, int::in,
		list(list(string))::out) is det.

group_words(IsFirst, Paras, Max, Lines) :-
	(
		Paras = [],
		Lines = []
	;
		Paras = [FirstPara | LaterParas],
		(
			FirstPara = [],
			group_words(IsFirst, LaterParas, Max, Lines)
		;
			FirstPara = [FirstWord | LaterWords],
			get_line_of_words(FirstWord, LaterWords,
				Max, Line, RestWords),
			( IsFirst = yes ->
				Max2 is Max - 2
			;
				Max2 = Max
			),
			group_nonfirst_line_words(RestWords, Max2, RestLines1),
			Lines1 = [Line | RestLines1],
			group_words(no, LaterParas, Max2, RestLines),
			list__append(Lines1, RestLines, Lines)
		)
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
		list__append(Line0, [Word], Line1),
		get_later_words(Words, NewLen, MaxLen,
			Line1, Line, RestWords)
	;
		Line = Line0,
		RestWords = [Word | Words]
	).
		
%-----------------------------------------------------------------------------%

	% The code of this predicate duplicates the functionality of
	% hlds_out__write_pred_id. Changes here should be made there as well.

error_util__describe_one_pred_name(Module, PredId, Piece) :-
	module_info_pred_info(Module, PredId, PredInfo),
	pred_info_module(PredInfo, ModuleName),
	prog_out__sym_name_to_string(ModuleName, ModuleNameString),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, Arity),
	pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
	(
		PredOrFunc = predicate,
		PredOrFuncPart = "predicate",
		OrigArity = Arity
	;
		PredOrFunc = function,
		PredOrFuncPart = "function",
		OrigArity is Arity - 1
	),
	string__int_to_string(OrigArity, ArityPart),
	string__append_list([
		PredOrFuncPart,
		" `",
		ModuleNameString,
		":",
		PredName,
		"/",
		ArityPart,
		"'"], Piece).

error_util__describe_one_proc_name(Module, proc(PredId, ProcId), Piece) :-
	error_util__describe_one_pred_name(Module, PredId, PredPiece),
	proc_id_to_int(ProcId, ProcIdInt),
	string__int_to_string(ProcIdInt, ProcIdPart),
	string__append_list([
		PredPiece,
		" mode ",
		ProcIdPart
		], Piece).

error_util__describe_several_proc_names(Module, PPIds, Pieces) :-
	list__map(error_util__describe_one_proc_name(Module), PPIds, Pieces0),
	error_util__list_to_pieces(Pieces0, Pieces).

error_util__describe_one_call_site(Module, PPId - Context, Piece) :-
	error_util__describe_one_proc_name(Module, PPId, ProcName),
	term__context_file(Context, FileName),
	term__context_line(Context, LineNumber),
	string__int_to_string(LineNumber, LineNumberPart),
	string__append_list([
		ProcName,
		" at ",
		FileName,
		":",
		LineNumberPart
		], Piece).

error_util__describe_several_call_sites(Module, Sites, Pieces) :-
	list__map(error_util__describe_one_call_site(Module), Sites, Pieces0),
	error_util__list_to_pieces(Pieces0, Pieces).

