%---------------------------------------------------------------------------%
% Copyright (C) 1994-1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: string.nu.nl.
% Main author: fjh.

%-----------------------------------------------------------------------------%

% In NU-Prolog, strings are represented as list of ASCII codes.

% To do this correctly, we really ought to check that the list of
% ints are all valid character codes (i.e. <= 255), and if not,
% call error/1.  But string__to_int_list is private to string.m
% anyway, so for efficiency we don't worry about that run-time type check.

string__to_int_list(S, S).

%-----------------------------------------------------------------------------%

string__to_float(String, Float) :-
	tokenize(String, Num, number, ""),
	Float is float(Num).

string__float_to_string(Float, String) :-
	sformat("~g", [Float], String).

string__float_to_f_string(Float, String) :-
	sformat("~f", [Float], String).

%-----------------------------------------------------------------------------%

string__index(String, Int, Char) :-
	list__index0(String, Int, Code),
	char__to_int(Char, Code).

string__unsafe_index(String, Int, Char) :-
	string__index(String, Int, Char).

%-----------------------------------------------------------------------------%

string__append(A, B, C) :-
	list__append(A, B, C).

%-----------------------------------------------------------------------------%

string__length(String, Length) :-
	list__length(String, Length).

%-----------------------------------------------------------------------------%

string__substring(String, Start, Count, Substring) :-
	string__split(String, Start, _Left, Right),
	string__left(Right, Count, Substring).

string__unsafe_substring(String, Start, Count, Substring) :-
	string__substring(String, Start, Count, Substring).

%-----------------------------------------------------------------------------%

string__split(String, Count, LeftString, RightString) :-
	(
		Count =< 0
	->
		LeftString = "",
		RightString = String
	;
		string__to_int_list(String, CodesList),
		list__split_list(Count, CodesList, LeftCodes, RightCodes)
	->
		string__to_int_list(LeftString, LeftCodes),
		string__to_int_list(RightString, RightCodes)
	;
		LeftString = String,
		RightString = ""
	).

%-----------------------------------------------------------------------------%

string__first_char(String0, Char, String) :-
	string__to_int_list(String0, List0),
	List0 = [CharCode | List],
	string__to_int_list(String, List),
	char__to_int(Char, CharCode).

%-----------------------------------------------------------------------------%

string__contains_char(String, Char) :-
	string__to_int_list(String, List),
	char__to_int(Char, CharCode),
	list__member(CharCode, List).

%-----------------------------------------------------------------------------%
