%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1998, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: bromage
%
% This module contains code to filter the diff before output, based on
% the command-line options presented.
%
% At the moment, only one option is handled: --ignore-blank-lines.
% This causes edits to be dropped if they contain changes which only
% add, delete or change blank lines.
%
% TO DO: What exactly is a blank line, and does its definition change
% if --ignore-space-change or --ignore-all-space have been specified?
% At the moment, we define a blank line to be a line containing zero or more
% whitespace characters. Check if this is correct or not.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module filter.
:- interface.

:- import_module difftype.
:- import_module file.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred filter_diff(file::in, file::in, diff::in, diff::out,
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
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

filter_diff(File1, File2, !Diff, !IO) :-
    globals.io_lookup_bool_option(ignore_blank_lines, FilterBlank, !IO),
    (
        % If we didn't request a filter, skip this pass.
        FilterBlank = no
    ;
        FilterBlank = yes,
        filter.blank_lines(!.Diff, File1, File2, !:Diff)
    ).

:- pred filter.blank_lines(diff::in, file::in, file::in, diff::out) is det.

filter.blank_lines([], _, _, []).
filter.blank_lines([Edit | Diff0], File1, File2, Diff) :-
    filter.blank_lines(Diff0, File1, File2, Diff1),
    (
        Edit = add(_, Y1 - Y2),
        ( if range_has_only_blank_lines(Y1, Y2, File2) then
            Diff = Diff1
        else
            Diff = [Edit | Diff1]
        )
    ;
        Edit = delete(X1 - X2, _),
        ( if range_has_only_blank_lines(X1, X2, File1) then
            Diff = Diff1
        else
            Diff = [Edit | Diff1]
        )
    ;
        Edit = change(X1 - X2, Y1 - Y2),
        ( if
            range_has_only_blank_lines(X1, X2, File1),
            range_has_only_blank_lines(Y1, Y2, File2)
        then
            Diff = Diff1
        else
            Diff = [Edit | Diff1]
        )
    ).

%-----------------------------------------------------------------------------%

:- pred range_has_only_blank_lines(int::in, int::in, file::in) is semidet.

range_has_only_blank_lines(First, Last, File) :-
    (
        First < Last
    =>
        (
            file.get_line(File, First, Line),
            string.to_char_list(Line, Chars),
            all [C] (
                list.member(C, Chars)
            =>
                char.is_whitespace(C)
            ),
            range_has_only_blank_lines(First + 1, Last, File)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module filter.
%-----------------------------------------------------------------------------%
