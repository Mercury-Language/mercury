%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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

	% predicates to isolate system dependencies 

:- pred dir__directory_separator(character).
:- mode dir__directory_separator(out) is det.
:- mode dir__directory_separator(in) is semidet.
	% Returns '/'.

:- pred dir__this_directory(string).
:- mode dir__this_directory(out) is det.	
:- mode dir__this_directory(in) is semidet.	 % Implied
	% Returns ".".

	% predicates for splitting filenames into a directory part and
	% a filename part.

:- pred dir__split_name(string::in, string::out, string::out) is det.
:- pred dir__basename(string::in, string::out) is det.
:- pred dir__dirname(string::in, string::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, require, string.

dir__directory_separator('/').

dir__this_directory(".").

dir__split_name(FileName, DirName, BaseName) :-
	string__length(FileName, Length),
	dir__split_name_2(FileName, Length, DirName, BaseName).

:- pred dir__split_name_2(string::in, int::in, string::out, string::out) is det.

dir__split_name_2(FileName, N, DirName, BaseName) :-
	N1 is N - 1,
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

%-----------------------------------------------------------------------------%
