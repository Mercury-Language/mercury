%-----------------------------------------------------------------------------%
% Copyright (C) 1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Main author: bromage

% This module contains code to match common lines before diffing, based on
% the command-line options presented.  The important command-line options
% are --ignore-case, --ignore-all-space and --ignore-space-change.

% The output of build_matches is two arrays of integers, where any two
% lines are assigned the same integer iff they are identical (modulo case,
% space and/or space change depending on the command line options).  An
% added benefit of doing this here is that the diff algorithm (myers.m)
% only has to compare integers instead of strings.

% TO DO: We should collapse sequences of lines which only appear in one
%        file and pretend the whole sequence is just one line.  (GNU
%        diff does the same thing a slightly different way, but this
%        approach seems a bit more Mercury-esque.)  Since Myers'
%	 algorithm runs in O(ND) time, and performing this pre-filtering
%	 here would reduce the value of D (by quite a lot in real-world
%	 cases), things should speed up.

%-----------------------------------------------------------------------------%

:- module match.

:- interface.
:- import_module file, io, array.

:- pred build_matches(file :: in, file :: in,
		array(int) :: out, array(int) :: out,
		io__state :: di, io__state :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module globals, options.
:- import_module bool, list, int, std_util, string, char, map, require.

:- type match_options
	--->	match_options(
			bool,		% No options set
			bool,		% --ignore-case
			bool,		% --ignore-all-space
			bool		% --ignore-space-change
		).

build_matches(File1, File2, FileX, FileY) -->
	globals__io_lookup_bool_option(ignore_case, IgnCase),
	globals__io_lookup_bool_option(ignore_all_space, IgnAllSpc),
	globals__io_lookup_bool_option(ignore_space_change, IgnSpcChg),
	{
		bool__or_list([IgnCase, IgnAllSpc, IgnSpcChg], AnyOpts),
		bool__not(AnyOpts, NoOpts),
		Opts = match_options(NoOpts, IgnCase, IgnAllSpc, IgnSpcChg),
		map__init(MatchMap0),
		file__get_numlines(File1, SizeX),
		array__init(SizeX, -1, FileX0),
		build_matches_for_file(Opts, File1, SizeX - 1, MatchMap0,
			MatchMap1, 0, ID1, FileX0, FileX),
		file__get_numlines(File2, SizeY),
		array__init(SizeY, -1, FileY0),
		build_matches_for_file(Opts, File2, SizeY - 1, MatchMap1, _,
			ID1, _, FileY0, FileY)
	}.

:- pred build_matches_for_file(match_options, file, int,
	map(string, int), map(string, int), int, int, array(int), array(int)).
:- mode build_matches_for_file(in, in, in, in, out, in, out,
	array_di, array_uo) is det.

build_matches_for_file(Opts, OrigFile, I, MatchMap0, MatchMap, ID0, ID,
		File0, File) :-
	( I < 0 ->
		MatchMap = MatchMap0,
		ID = ID0,
		File = File0
	;
		( file__get_line(OrigFile, I, Line0) ->
			Line1 = Line0
		;
			error("build_matches_for_file")
		),
		Opts = match_options(NoOpts, IgnCase, IgnAllSpc, IgnSpcChg),
		( NoOpts = yes ->
			Line = Line1
		;
			string__to_char_list(Line1, Chars0),
			normalise_line(no, IgnCase, IgnAllSpc, IgnSpcChg,
				Chars0, Chars1),
			string__from_char_list(Chars1, Line)
		),
		( map__search(MatchMap0, Line, MaybeID) ->
			array__set(File0, I, MaybeID, File1),
			MatchMap1 = MatchMap0,
			ID1 = ID0
		;
			array__set(File0, I, ID0, File1),
			map__det_insert(MatchMap0, Line, ID0, MatchMap1),
			ID1 is ID0 + 1
		),
		build_matches_for_file(Opts, OrigFile, I - 1, MatchMap1,
			MatchMap, ID1, ID, File1, File)
	).

:- pred normalise_line(bool, bool, bool, bool, list(char), list(char)).
:- mode normalise_line(in, in, in, in, in, out) is det.

normalise_line(_, _, _, _, [], []).
normalise_line(LastSpace, IgnCase, IgnAllSpc, IgnSpcChg, [C0 | Cs0], Cs) :-
	( IgnCase = yes ->
		char__to_lower(C0, C)
	;
		C = C0
	),
	(
		char__is_whitespace(C),
		(
			IgnAllSpc = yes
		->
			normalise_line(LastSpace, IgnCase, IgnAllSpc, IgnSpcChg,
					Cs0, CsX)
		;
			IgnSpcChg = yes
		->
			( LastSpace = yes ->
				normalise_line(yes, IgnCase, IgnAllSpc,
						IgnSpcChg, Cs0, CsX)
			;
				normalise_line(yes, IgnCase, IgnAllSpc,
						IgnSpcChg, Cs0, Cs1),
				CsX = [' ' | Cs1]
				
			)
		;
			fail
		)
	->
		Cs = CsX
	;
		normalise_line(no, IgnCase, IgnAllSpc, IgnSpcChg,
				Cs0, Cs1),
		Cs = [C | Cs1]
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
