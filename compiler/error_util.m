%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2005 The University of Melbourne.
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
%
%-----------------------------------------------------------------------------%

:- module parse_tree__error_util.

:- interface.

:- import_module mdbcomp__prim_data.
:- import_module parse_tree__prog_data.

:- import_module bool, char, io, list, std_util.

:- type format_component
	--->	fixed(string)	% This string should appear in the output
				% in one piece, as it is.

	;	suffix(string)	% This string should appear in the output
				% in one pieces, as it is, appended directly
				% after the previous format_component, without
				% any intervening space.

	;	words(string)	% This string contains words separated by
				% white space. The words should appear in
				% the output in the given order, but the
				% white space may be rearranged and line
				% breaks may be inserted.

	;	sym_name(sym_name)
				% The output should contain the string form of
				% the sym_name, surrounded by `' quotes.
	
	;	sym_name_and_arity(sym_name_and_arity)
				% The output should contain the string form of
				% the sym_name, followed by '/' and the arity,
				% all surrounded by `' quotes.

	;	nl.		% Insert a line break if there has been text
				% output since the last line break.

	% Convert a list of strings into a list of format_components
	% separated by commas, with the last two elements separated by `and'.
:- func list_to_pieces(list(string)) = list(format_component).

	% Convert a list of lists of format_components into a list of
	% format_components separated by commas, with the last two
	% elements separated by `and'.
:- func component_lists_to_pieces(list(list(format_component))) =
	list(format_component).

	% Display the given error message, without a context and with standard
	% indentation.
:- pred write_error_pieces_plain(list(format_component)::in,
	io::di, io::uo) is det.

	% write_error_plain_with_progname(ProgName, Msg):
	% Display Msg as the error string, with ProgName as a context
	% and with standard indentation.
:- pred write_error_plain_with_progname(string::in, string::in,
	io::di, io::uo) is det.

	% write_error_pieces(Context, Indent, Components).
	% Display `Components' as the error message, with
	% `Context' as a context and indent by `Indent'.
	%
:- pred write_error_pieces(prog_context::in, int::in,
	list(format_component)::in, io::di, io::uo) is det.

	% Display the given error message, but indent the first line.
	% This is useful when adding extra lines to an already
	% displayed message.
:- pred write_error_pieces_not_first_line(prog_context::in, int::in,
	list(format_component)::in, io::di, io::uo) is det.

	% Display the given error message. The bool is true iff
	% this is the first line.
:- pred write_error_pieces_maybe_first_line(bool::in, prog_context::in,
	int::in, list(format_component)::in, io::di, io::uo) is det.

:- pred write_error_pieces_maybe_with_context(maybe(prog_context)::in, int::in,
	list(format_component)::in, io::di, io::uo) is det.

:- func error_pieces_to_string(list(format_component)) = string.

:- func describe_sym_name(sym_name) = string.

:- func describe_sym_name_and_arity(sym_name_and_arity) = string.

:- func pred_or_func_to_string(pred_or_func) = string.

	% Append a punctuation character to a message, avoiding unwanted
	% line splitting between the message and the punctuation.
:- func append_punctuation(list(format_component), char) =
	list(format_component).

	% report_error_num_args(MaybePredOrFunc, Arity, CorrectArities).
	%
	% Write
	% "wrong number of arguments (<Arity>; should be <CorrectArities>)",
	% adjusting `Arity' and `CorrectArities' if `MaybePredOrFunc' is
	% `yes(function)'.
:- pred report_error_num_args(maybe(pred_or_func)::in, int::in, list(int)::in,
	io::di, io::uo) is det.

	% sorry(ModuleName, Message)
	% Call error/1 with a "Sorry, not implemented" message.
	%
	% Use this for features that should be implemented (or at
	% least could be implemented).
	%
:- pred sorry(string::in, string::in) is erroneous.

	% unexpected(ModuleName, Message)
	% Call error/1 with an "Unexpected" message.
	%
	% Use this to handle cases which are not expected to arise (i.e.
	% bugs).
	%
:- pred unexpected(string::in, string::in) is erroneous.

	% Record the fact that a warning has been issued; set the exit status
	% to error if the --halt-at-warn option is set.
:- pred record_warning(io::di, io::uo) is det.

	% Report a warning, and set the exit status to error if the
	% --halt-at-warn option is set.
:- pred report_warning(string::in, io::di, io::uo) is det.

	% Report a warning to the specified stream, and set the exit status
	% to error if the --halt-at-warn option is set.
:- pred report_warning(io__output_stream::in, string::in, io::di, io::uo)
	is det.

	% Report a warning, and set the exit status to error if the
	% --halt-at-warn option is set.
:- pred report_warning(prog_context::in, int::in, list(format_component)::in,
	io::di, io::uo) is det.

:- implementation.

:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.
:- import_module libs__globals.
:- import_module libs__options.

:- import_module io, list, term, char, string, int, require.

list_to_pieces([]) = [].
list_to_pieces([Elem]) = [words(Elem)].
list_to_pieces([Elem1, Elem2]) = [fixed(Elem1), words("and"), fixed(Elem2)].
list_to_pieces([Elem1, Elem2, Elem3 | Elems]) =
	[fixed(Elem1 ++ ",") | list_to_pieces([Elem2, Elem3 | Elems])].

component_lists_to_pieces([]) = [].
component_lists_to_pieces([Components]) = Components.
component_lists_to_pieces([Components1, Components2]) =
		list__condense([Components1, [words("and")], Components2]).
component_lists_to_pieces(
		[Components1, Components2, Components3 | Components]) =
	list__append(append_punctuation(Components1, ','),
		component_lists_to_pieces(
			[Components2, Components3 | Components])).

write_error_pieces_plain(Components, !IO) :-
	write_error_pieces_maybe_with_context(yes, no, 0, Components, !IO).

write_error_plain_with_progname(ProgName, Msg, !IO) :-
	write_error_pieces_plain([fixed(ProgName ++ ":"), words(Msg)], !IO).

write_error_pieces(Context, Indent, Components, !IO) :-
	write_error_pieces_maybe_with_context(yes, yes(Context),
		Indent, Components, !IO).

write_error_pieces_not_first_line(Context, Indent, Components, !IO) :-
	write_error_pieces_maybe_with_context(no, yes(Context),
		Indent, Components, !IO).

write_error_pieces_maybe_first_line(IsFirst, Context, Indent, Components,
		!IO) :-
	(
		IsFirst = yes,
		write_error_pieces(Context, Indent, Components, !IO)
	;
		IsFirst = no,
		write_error_pieces_not_first_line(Context, Indent, Components,
			!IO)
	).

write_error_pieces_maybe_with_context(MaybeContext, Indent, Components, !IO) :-
	write_error_pieces_maybe_with_context(yes, MaybeContext,
		Indent, Components, !IO).

:- pred write_error_pieces_maybe_with_context(bool::in,
	maybe(prog_context)::in, int::in, list(format_component)::in,
	io::di, io::uo) is det.

write_error_pieces_maybe_with_context(IsFirst, MaybeContext,
		Indent, Components, !IO) :-
	(
			% The fixed characters at the start of the line are:
			% filename
			% :
			% line number (min 3 chars)
			% :
			% space
			% indent
		(
			MaybeContext = yes(Context),
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
			ContextLength = FileNameLength + 1 +
					LineNumberStrLength + 2
		;
			MaybeContext = no,
			ContextLength = 0
		),
		NotFirstIndent = (IsFirst = yes -> 0 ; 2),
		Remain = 79 - (ContextLength + Indent + NotFirstIndent),
		convert_components_to_word_list(Components, [], [], Words),
		group_words(IsFirst, Words, Remain, Lines)
	),
	( IsFirst = yes ->
		write_lines(Lines, MaybeContext, Indent, !IO)
	;
		write_nonfirst_lines(Lines, MaybeContext, Indent + 2, !IO)
	).

:- pred write_lines(list(list(string))::in, maybe(prog_context)::in, int::in,
	io::di, io::uo) is det.

write_lines([], _, _, !IO).
write_lines([Line | Lines], MaybeContext, Indent, !IO) :-
	(
		MaybeContext = yes(Context),
		prog_out__write_context(Context, !IO)
	;
		MaybeContext = no
	),
	string__pad_left("", ' ', Indent, IndentStr),
	io__write_string(IndentStr, !IO),
	write_line(Line, !IO),
	Indent2 = Indent + 2,
	write_nonfirst_lines(Lines, MaybeContext, Indent2, !IO).

:- pred write_nonfirst_lines(list(list(string))::in, maybe(prog_context)::in,
	int::in, io::di, io::uo) is det.

write_nonfirst_lines([], _, _, !IO).
write_nonfirst_lines([Line | Lines], MaybeContext, Indent, !IO) :-
	(
		MaybeContext = yes(Context),
		prog_out__write_context(Context, !IO)
	;
		MaybeContext = no
	),
	string__pad_left("", ' ', Indent, IndentStr),
	io__write_string(IndentStr, !IO),
	write_line(Line, !IO),
	write_nonfirst_lines(Lines, MaybeContext, Indent, !IO).

:- pred write_line(list(string)::in, io::di, io::uo) is det.

write_line([], !IO).
write_line([Word | Words], !IO) :-
	io__write_string(Word, !IO),
	write_line_rest(Words, !IO),
	io__write_char('\n', !IO).

:- pred write_line_rest(list(string)::in, io::di, io::uo) is det.

write_line_rest([], !IO).
write_line_rest([Word | Words], !IO) :-
	io__write_char(' ', !IO),
	io__write_string(Word, !IO),
	write_line_rest(Words, !IO).

error_pieces_to_string([]) = "".
error_pieces_to_string([Component | Components]) = Str :-
	TailStr = error_pieces_to_string(Components),
	(
		Component = fixed(Word),
		( TailStr = "" ->
			Str = Word
		;
			Str = Word ++ " " ++ TailStr
		)
	;
		Component = suffix(Word),
		Str = Word ++ TailStr
	;
		Component = words(Words),
		( TailStr = "" ->
			Str = Words
		;
			Str = Words ++ " " ++ TailStr
		)
	;
		Component = sym_name(SymName),
		Word = sym_name_to_word(SymName),
		( TailStr = "" ->
			Str = Word
		;
			Str = Word ++ " " ++ TailStr
		)
	;
		Component = sym_name_and_arity(SymNameAndArity),
		Word = sym_name_and_arity_to_word(SymNameAndArity),
		( TailStr = "" ->
			Str = Word
		;
			Str = Word ++ " " ++ TailStr
		)
	;
		Component = nl,
		Str = "\n" ++ TailStr
	).

%----------------------------------------------------------------------------%

:- type word
	--->	word(string)
	;	suffix_word(string).

:- pred convert_components_to_word_list(list(format_component)::in,
	list(word)::in, list(list(string))::in, list(list(string))::out)
	is det.

convert_components_to_word_list([], RevWords0, Paras0, Paras) :-
	Strings = rev_words_to_strings(RevWords0),
	list__reverse([Strings | Paras0], Paras).
convert_components_to_word_list([Component | Components], RevWords0,
		Paras0, Paras) :-
	(
		Component = fixed(Word),
		RevWords1 = [word(Word) | RevWords0],
		Paras1 = Paras0
	;
		Component = suffix(Word),
		RevWords1 = [suffix_word(Word) | RevWords0],
		Paras1 = Paras0
	;
		Component = words(WordsStr),
		break_into_words(WordsStr, RevWords0, RevWords1),
		Paras1 = Paras0
	;
		Component = sym_name(SymName),
		RevWords1 = [word(sym_name_to_word(SymName)) | RevWords0],
		Paras1 = Paras0
	;
		Component = sym_name_and_arity(SymNameAndArity),
		Word = sym_name_and_arity_to_word(SymNameAndArity),
		RevWords1 = [word(Word) | RevWords0],
		Paras1 = Paras0
	;
		Component = nl,
		Strings = rev_words_to_strings(RevWords0),
		Paras1 = [Strings | Paras0],
		RevWords1 = []
	),
	convert_components_to_word_list(Components, RevWords1, Paras1, Paras).

:- func rev_words_to_strings(list(word)) = list(string).

rev_words_to_strings(RevWords) =
	list__reverse(rev_words_to_rev_strings(RevWords)).

:- func rev_words_to_rev_strings(list(word)) = list(string).

rev_words_to_rev_strings([]) = [].
rev_words_to_rev_strings([Word | Words]) = Strings :-
	(
		Word = word(String),
		Strings = [String | rev_words_to_rev_strings(Words)]
	;
		Word = suffix_word(Suffix),
		(
			Words = [],
			Strings = [Suffix]
		;
			Words = [word(String) | Tail],
			Strings = [String ++ Suffix |
				rev_words_to_rev_strings(Tail)]
		;
			Words = [suffix_word(MoreSuffix) | Tail],
			Strings = rev_words_to_rev_strings(
				[suffix_word(MoreSuffix ++ Suffix) | Tail])
		)
	).

:- func sym_name_to_word(sym_name) = string.

sym_name_to_word(SymName) = "`" ++ SymStr ++ "'" :-
	sym_name_to_string(SymName, SymStr).

:- func sym_name_and_arity_to_word(sym_name_and_arity) = string.

sym_name_and_arity_to_word(SymNameAndArity) = "`" ++ SymStr ++ "'" :-
	sym_name_and_arity_to_string(SymNameAndArity, SymStr).

:- pred break_into_words(string::in, list(word)::in, list(word)::out) is det.

break_into_words(String, Words0, Words) :-
	break_into_words_from(String, 0, Words0, Words).

:- pred break_into_words_from(string::in, int::in, list(word)::in,
	list(word)::out) is det.

break_into_words_from(String, Cur, Words0, Words) :-
	( find_word_start(String, Cur, Start) ->
		find_word_end(String, Start, End),
		Length = End - Start + 1,
		string__substring(String, Start, Length, WordStr),
		Next = End + 1,
		break_into_words_from(String, Next,
			[word(WordStr) | Words0], Words)
	;
		Words = Words0
	).

:- pred find_word_start(string::in, int::in, int::out) is semidet.

find_word_start(String, Cur, WordStart) :-
	string__index(String, Cur, Char),
	( char__is_whitespace(Char) ->
		Next = Cur + 1,
		find_word_start(String, Next, WordStart)
	;
		WordStart = Cur
	).

:- pred find_word_end(string::in, int::in, int::out) is det.

find_word_end(String, Cur, WordEnd) :-
	Next = Cur + 1,
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
				Max2 = Max - 2
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
	NewLen = OldLen + 1 + WordLen,
	( NewLen =< MaxLen ->
		list__append(Line0, [Word], Line1),
		get_later_words(Words, NewLen, MaxLen,
			Line1, Line, RestWords)
	;
		Line = Line0,
		RestWords = [Word | Words]
	).

%-----------------------------------------------------------------------------%

describe_sym_name_and_arity(SymName / Arity) =
		string__append_list(["`", SymNameString,
			"/", string__int_to_string(Arity), "'"]) :-
	sym_name_to_string(SymName, SymNameString).

describe_sym_name(SymName) =
		string__append_list(["`", SymNameString, "'"]) :-
	sym_name_to_string(SymName, SymNameString).

pred_or_func_to_string(predicate) = "predicate".
pred_or_func_to_string(function) = "function".

append_punctuation([], _) = _ :-
	error("append_punctuation: " ++
		"appending punctuation after nothing").
append_punctuation([Piece0], Punc) = [Piece] :-
	% Avoid unwanted line splitting between the message
	% and the punctuation.
	(
		Piece0 = words(String),
		Piece = words(string__append(String, char_to_string(Punc)))
	;
		Piece0 = fixed(String),
		Piece = fixed(string__append(String, char_to_string(Punc)))
	;
		Piece0 = suffix(Suffix),
		Piece = suffix(string__append(Suffix, char_to_string(Punc)))
	;
		Piece0 = sym_name(SymName),
		String = sym_name_to_word(SymName),
		Piece = fixed(string__append(String, char_to_string(Punc)))
	;
		Piece0 = sym_name_and_arity(SymNameAndArity),
		String = sym_name_and_arity_to_word(SymNameAndArity),
		Piece = fixed(string__append(String, char_to_string(Punc)))
	;
		Piece0 = nl,
		error("append_punctutation: " ++
			"appending punctuation after newline")
	).
append_punctuation([Piece1, Piece2 | Pieces], Punc) =
	[Piece1 | append_punctuation([Piece2 | Pieces], Punc)].

%-----------------------------------------------------------------------------%

report_error_num_args(MaybePredOrFunc, Arity0, Arities0, !IO) :-
	% Adjust arities for functions.
	( MaybePredOrFunc = yes(function) ->
		adjust_func_arity(function, Arity, Arity0),
		list__map(
			(pred(OtherArity0::in, OtherArity::out) is det :-
				adjust_func_arity(function,
					OtherArity, OtherArity0)
			),
			Arities0, Arities)
	;
		Arity = Arity0,
		Arities = Arities0
	),
	io__write_string("wrong number of arguments (", !IO),
	io__write_int(Arity, !IO),
	io__write_string("; should be ", !IO),
	report_error_right_num_args(Arities, !IO),
	io__write_string(")", !IO).

:- pred report_error_right_num_args(list(int)::in, io::di, io::uo) is det.

report_error_right_num_args([], !IO).
report_error_right_num_args([Arity | Arities], !IO) :-
	io__write_int(Arity, !IO),
	( Arities = [] ->
		true
	; Arities = [_] ->
		io__write_string(" or ", !IO)
	;
		io__write_string(", ", !IO)
	),
	report_error_right_num_args(Arities, !IO).

	% Call error/1 with a "Sorry, not implemented" message.
	%
sorry(Module, What) :-
	string__format("%s: Sorry, not implemented: %s",
		[s(Module), s(What)], ErrorMessage),
	error(ErrorMessage).

unexpected(Module, What) :-
	string__format("%s: Unexpected: %s",
		[s(Module), s(What)], ErrorMessage),
	error(ErrorMessage).

record_warning(!IO) :-
	globals__io_lookup_bool_option(halt_at_warn, HaltAtWarn, !IO),
	( HaltAtWarn = yes ->
		io__set_exit_status(1, !IO)
	;
		true
	).

report_warning(Message, !IO) :-
	record_warning(!IO),
	io__write_string(Message, !IO).

report_warning(Stream, Message, !IO) :-
	record_warning(!IO),
	io__write_string(Stream, Message, !IO).

report_warning(Context, Indent, Components, !IO) :-
	record_warning(!IO),
	write_error_pieces(Context, Indent, Components, !IO).
