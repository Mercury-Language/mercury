%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997, 1999-2000, 2002 The University of Melbourne.
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

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, list, require, string.

dir__directory_separator('/').

dir__this_directory(".").

dir__split_name(FileName, DirName, BaseName) :-
	string__length(FileName, Length),
	dir__split_name_2(FileName, Length, DirName, BaseName).

:- pred dir__split_name_2(string::in, int::in, string::out, string::out)
	is det.

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
