%-----------------------------------------------------------------------------%

% File: dir.nl.
% Main author: fjh.

% Filename and directory handling.

%-----------------------------------------------------------------------------%

:- module dir.
:- interface.
:- import_module string.

	% predicates to isolate system dependencies 

:- pred dir__directory_seperator(character::out) is det. % Returns '/'.
:- pred dir__this_directory(string::out) is det.	 % Returns ".".

	% predicates for splitting filenames into a directory
	% part and a filename part.

:- pred dir__split_name(string::in, string::out, string::out) is det.
:- pred dir__basename(string::in, string::out) is det.
:- pred dir__dirname(string::in, string::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.
:- import_module int, require.

dir__directory_seperator('/').

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
		string__index_det(FileName, N1, Seperator),
		dir__directory_seperator(Seperator)
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
