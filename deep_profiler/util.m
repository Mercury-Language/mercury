%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module defines utility predicates for both the CGI program and
% for the server.

:- module util.

:- interface.

:- import_module char, list.

% split(Str, Char, Pieces): splits Str into pieces at every occurrence of Char,
% and returns the pieces in order. No piece will contain Char.

:- pred split(string::in, char::in, list(string)::out) is det.

:- implementation.

:- import_module string, require.

split(Str0, SChar, Strs) :-
	string__to_char_list(Str0, Chars0),
	split(Chars0, SChar, [], [], Strs0),
	list__reverse(Strs0, Strs).

:- pred split(list(char)::in, char::in, list(char)::in,
	list(string)::in, list(string)::out) is det.

split([], _SChar, Chars0, Strs0, Strs) :-
	(
		Chars0 = [],
		Strs = Strs0
	;
		Chars0 = [_ | _],
		list__reverse(Chars0, Chars),
		string__from_char_list(Chars, Str),
		Strs = [Str | Strs0]
	).
split([C | Cs], SChar, Chars0, Strs0, Strs) :-
	( C = SChar ->
		(
			Chars0 = [],
			Strs1 = Strs0
		;
			Chars0 = [_ | _],
			list__reverse(Chars0, Chars),
			string__from_char_list(Chars, Str),
			Strs1 = [Str | Strs0]
		),
		split(Cs, SChar, [], Strs1, Strs)
	;
		split(Cs, SChar, [C | Chars0], Strs0, Strs)
	).
