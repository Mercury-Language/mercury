%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1998, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: bromage
%
% This module contains code to match common lines before diffing, based on
% the command-line options presented. The important command-line options
% are --ignore-case, --ignore-all-space and --ignore-space-change.
%
% The output of build_matches is two arrays of integers, where any two
% lines are assigned the same integer iff they are identical (modulo case,
% space and/or space change depending on the command line options).
% An added benefit of doing this here is that the diff algorithm (myers.m)
% only has to compare integers instead of strings.
%
% TO DO: We should collapse sequences of lines which only appear in one file
% and pretend the whole sequence is just one line. (GNU diff does the same
% thing a slightly different way, but this approach seems a bit more
% Mercury-esque.)  Since Myers' algorithm runs in O(ND) time, and performing
% this pre-filtering here would reduce the value of D (by quite a lot
% in real-world cases), things should speed up.
%
%-----------------------------------------------------------------------------%

:- module match.
:- interface.

:- import_module file.

:- import_module array.
:- import_module io.

%-----------------------------------------------------------------------------%

:- pred build_matches(file::in, file::in, array(int)::out, array(int)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module globals.
:- import_module options.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

:- type match_options
    --->    match_options(
                bool,       % No options set
                bool,       % --ignore-case
                bool,       % --ignore-all-space
                bool        % --ignore-space-change
            ).

build_matches(File1, File2, FileX, FileY, !IO) :-
    globals.io_lookup_bool_option(ignore_case, IgnCase, !IO),
    globals.io_lookup_bool_option(ignore_all_space, IgnAllSpc, !IO),
    globals.io_lookup_bool_option(ignore_space_change, IgnSpcChg, !IO),
    (
        bool.or_list([IgnCase, IgnAllSpc, IgnSpcChg], AnyOpts),
        bool.not(AnyOpts, NoOpts),
        Opts = match_options(NoOpts, IgnCase, IgnAllSpc, IgnSpcChg),
        map.init(MatchMap0),
        file.get_numlines(File1, SizeX),
        array.init(SizeX, -1, FileX0),
        build_matches_for_file(Opts, File1, SizeX - 1, MatchMap0,
            MatchMap1, 0, ID1, FileX0, FileX),
        file.get_numlines(File2, SizeY),
        array.init(SizeY, -1, FileY0),
        build_matches_for_file(Opts, File2, SizeY - 1, MatchMap1, _,
            ID1, _, FileY0, FileY)
    ).

:- pred build_matches_for_file(match_options::in, file::in, int::in,
    map(string, int)::in, map(string, int)::out,
    int::in, int::out,
    array(int)::array_di, array(int)::array_uo) is det.

build_matches_for_file(Opts, OrigFile, I, !MatchMap, !ID, !File) :-
    ( if I < 0 then
        true
    else
        ( if file.get_line(OrigFile, I, Line0) then
            Line1 = Line0
        else
            error("build_matches_for_file")
        ),
        Opts = match_options(NoOpts, IgnCase, IgnAllSpc, IgnSpcChg),
        (
            NoOpts = yes,
            Line = Line1
        ;
            NoOpts = no,
            string.to_char_list(Line1, Chars0),
            normalise_line(no, IgnCase, IgnAllSpc, IgnSpcChg, Chars0, Chars1),
            string.from_char_list(Chars1, Line)
        ),
        ( if map.search(!.MatchMap, Line, MaybeID) then
            array.set(I, MaybeID, !File)
        else
            array.set(I, !.ID, !File),
            map.det_insert(Line, !.ID, !MatchMap),
            !:ID = !.ID + 1
        ),
        build_matches_for_file(Opts, OrigFile, I - 1, !MatchMap, !ID, !File)
    ).

:- pred normalise_line(bool::in, bool::in, bool::in, bool::in,
    list(char)::in, list(char)::out) is det.

normalise_line(_, _, _, _, [], []).
normalise_line(LastSpace, IgnCase, IgnAllSpc, IgnSpcChg, [C0 | Cs0], Cs) :-
    (
        IgnCase = yes,
        char.to_lower(C0, C)
    ;
        IgnCase = no,
        C = C0
    ),
    ( if
        char.is_whitespace(C),
        ( if
            IgnAllSpc = yes
        then
            normalise_line(LastSpace, IgnCase, IgnAllSpc, IgnSpcChg, Cs0, CsX)
        else if
            IgnSpcChg = yes
        then
            (
                LastSpace = yes,
                normalise_line(yes, IgnCase, IgnAllSpc, IgnSpcChg, Cs0, CsX)
            ;
                LastSpace = no,
                normalise_line(yes, IgnCase, IgnAllSpc, IgnSpcChg, Cs0, Cs1),
                CsX = [' ' | Cs1]
            )
        else
            fail
        )
    then
        Cs = CsX
    else
        normalise_line(no, IgnCase, IgnAllSpc, IgnSpcChg, Cs0, Cs1),
        Cs = [C | Cs1]
    ).

%-----------------------------------------------------------------------------%
:- end_module match.
%-----------------------------------------------------------------------------%
