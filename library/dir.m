%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997, 1999-2000, 2002-2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: dir.m.
% Main author: fjh.

% Filename and directory handling.
% Stability: high.

%-----------------------------------------------------------------------------%

:- module dir.
:- interface.

:- import_module list.

	% predicates to isolate system dependencies 

:- func dir__directory_separator = character.
:- pred dir__directory_separator(character).
:- mode dir__directory_separator(out) is det.
:- mode dir__directory_separator(in) is semidet.
	% Returns '/'.

:- func dir__this_directory = string.
:- pred dir__this_directory(string).
:- mode dir__this_directory(out) is det.	
:- mode dir__this_directory(in) is semidet.	 % Implied
	% Returns ".".

	% predicates for splitting filenames into a directory part and
	% a filename part.

:- pred dir__split_name(string::in, string::out, string::out) is det.
:- pred dir__basename(string::in, string::out) is det.
:- func dir__basename(string) = string.
:- pred dir__dirname(string::in, string::out) is det.
:- func dir__dirname(string) = string.

	% Given a directory name and a filename, return the pathname of that
	% file in that directory.
:- func dir__make_path_name(string, string) = string.
:- func string / string = string.

       % Implement brace expansion, as in sh: return the sequence of strings
       % generated from the given input string. Throw an exception if the
       % input string contains mismatched braces.
       %
       % The following is the documentation of brace expansion from the sh
       % manual:
       %
       %	Brace expansion is a mechanism by which arbitrary strings may
       %	be generated. This mechanism is similar to pathname expansion,
       %	but the filenames generated need not exist. Patterns to be
       %	brace expanded take the form of an optional preamble, followed
       %	by a series of comma-separated strings between a pair of
       %	braces, followed by an optional postscript. The preamble is
       %	prefixed to each string contained within the braces, and the
       %	postscript is then appended to each resulting string, expanding
       %	left to right.
       %
       %	Brace expansions may be nested. The results of each expanded
       %	string are not sorted; left to right order is preserved.
       %	For example, a{d,c,b}e expands into `ade ace abe'.
:- func expand_braces(string) = list(string).

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, char, list, require, exception, string.

dir__directory_separator('/').

dir__this_directory(".").

dir__split_name(FileName, DirName, BaseName) :-
	string__length(FileName, Length),
	dir__split_name_2(FileName, Length, DirName, BaseName).

:- pred dir__split_name_2(string::in, int::in, string::out, string::out)
	is det.

dir__split_name_2(FileName, N, DirName, BaseName) :-
	N1 = N - 1,
	(
		N1 < 0
	->
		dir__this_directory(DirName),
		BaseName = FileName
	;
		string__index_det(FileName, N1, Separator),
		dir__directory_separator(Separator)
	->
		string__split(FileName, N1, DirName, Rest),
		( string__first_char(Rest, _Sep, BaseName0) ->
			BaseName = BaseName0
		;
			error("dir__split_name_2")
		)
	;
		dir__split_name_2(FileName, N1, DirName, BaseName)
	).

dir__basename(FileName, BaseName) :-
	dir__split_name(FileName, _, BaseName).

dir__dirname(FileName, DirName) :-
	dir__split_name(FileName, DirName, _).

dir__make_path_name(DirName, FileName) = PathName :-
		% Using string__append_list has a fixed overhead of six
		% words, whereas using two string__appends back to back
		% would have a memory overhead proportional to the size
		% of the string copied twice. We prefer the former because
		% it is bounded.
	string__append_list([DirName,
		string__char_to_string(dir__directory_separator),
		FileName], PathName).

DirName / FileName = dir__make_path_name(DirName, FileName).

expand_braces(ArgStr) = ExpandStrs :-
	ArgChar = string__to_char_list(ArgStr),
	ExpandChars = expand(ArgChar),
	ExpandStrs = list__map(string__from_char_list, ExpandChars).

:- func expand(list(char)) = list(list(char)).

expand(Chars) = expand_acc(Chars, [[]]).

:- func expand_acc(list(char), list(list(char))) = list(list(char)).

expand_acc([], Prefixes) = Prefixes.
expand_acc([Char | Chars], Prefixes0) = Strings :-
	( Char = '{' ->
		find_matching_brace(Chars, Alternatives0, Left),
		AlternativeLists = list__map(expand, Alternatives0),
		Alternatives = list__condense(AlternativeLists),
		PrefixLists = list__map(add_alternatives(Alternatives),
			Prefixes0),
		Prefixes1 = list__condense(PrefixLists),
		expand_acc(Left, Prefixes1) = Strings
	;
		Prefixes1 = list__map(add_char_at_end(Char), Prefixes0),
		Strings = expand_acc(Chars, Prefixes1)
	).

:- func add_alternatives(list(list(char)), list(char)) = list(list(char)).

add_alternatives(Alternatives, Prefix) =
	list__map(list__append(Prefix), Alternatives).

:- func add_char_at_end(char, list(char)) = list(char).

add_char_at_end(Char, Prefix) = list__append(Prefix, [Char]).

:- pred find_matching_brace(list(char)::in, list(list(char))::out,
	list(char)::out) is det.

find_matching_brace(Chars, Alternatives, Left) :-
	find_matching_brace_or_comma(Chars, [], [], 0, Alternatives, Left).

:- pred find_matching_brace_or_comma(list(char)::in, list(list(char))::in,
	list(char)::in, int::in, list(list(char))::out, list(char)::out)
	is det.

find_matching_brace_or_comma([], _, _, _, _, _) :-
	throw("no matching brace").
find_matching_brace_or_comma([Char | Chars], Alternatives0, CurAlternative,
		BraceLevel, Alternatives, Left) :-
	( Char = '}' ->
		( BraceLevel = 0 ->
			list__append(Alternatives0, [CurAlternative],
				Alternatives),
			Left = Chars
		;
			find_matching_brace_or_comma(Chars, Alternatives0,
				list__append(CurAlternative, [Char]),
				BraceLevel - 1, Alternatives, Left)
		)
	; Char = '{' ->
		find_matching_brace_or_comma(Chars, Alternatives0,
			list__append(CurAlternative, [Char]),
			BraceLevel + 1, Alternatives, Left)
	; Char = (',') ->
		( BraceLevel = 0 ->
			list__append(Alternatives0, [CurAlternative],
				Alternatives1),
			find_matching_brace_or_comma(Chars, Alternatives1,
				[], BraceLevel, Alternatives, Left)
		;
			find_matching_brace_or_comma(Chars, Alternatives0,
				list__append(CurAlternative, [Char]),
				BraceLevel, Alternatives, Left)
		)
	;
		find_matching_brace_or_comma(Chars, Alternatives0,
			list__append(CurAlternative, [Char]),
			BraceLevel, Alternatives, Left)
	).
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 27/04/99
%       Functional forms added.

dir__directory_separator = C :-
	dir__directory_separator(C).

dir__this_directory = S :-
	dir__this_directory(S).

dir__basename(S1) = S2 :-
	dir__basename(S1, S2).

dir__dirname(S1) = S2 :-
	dir__dirname(S1, S2).
