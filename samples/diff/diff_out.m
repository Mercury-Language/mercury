%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1998, 2006, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: bromage
% Based on diffs.m, written by bromage and simplified by
% Marnix Klooster <marnix@worldonline.nl>
%
% This module contains the predicates to display a diff in various
% output styles, based on the command-line options supplied.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module diff_out.
:- interface.

:- import_module difftype.
:- import_module file.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type output_style
    --->    normal
    ;       help_only
    ;       version_only
    ;       context(int)
    ;       unified(int)
    ;       ed
    ;       forward_ed
    ;       rcs
    ;       ifdef(string)
    ;       brief
    ;       side_by_side
    ;       cvs_merge_conflict.

    % The default output style.
    %
:- pred default_output_style(output_style::out) is det.

    % Succeeds if, for this output style, an absence of differences
    % means that no output should be generated.
    %
:- pred no_diff_implies_no_output(output_style::in) is semidet.

    % Succeeds if the user only wants to know about the presence
    % of any differences, not what they actually are.
    %
:- pred full_diff_not_required(output_style::in) is semidet.

    % Succeeds if the output style is "robust", that is, the absence of a
    % newline at the end of the file actually matters.
    %
:- pred robust(output_style::in) is semidet.

    % display_diff takes a diff and displays it in the user's specified output
    % format.
    %
:- pred display_diff(file::in, file::in, diff::in, io::di, io::uo) is det.

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
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%

default_output_style(normal).

%-----------------------------------------------------------------------------%

no_diff_implies_no_output(normal).
no_diff_implies_no_output(context(_)).
no_diff_implies_no_output(unified(_)).
no_diff_implies_no_output(ed).
no_diff_implies_no_output(forward_ed).
no_diff_implies_no_output(rcs).
no_diff_implies_no_output(brief).

%-----------------------------------------------------------------------------%

full_diff_not_required(brief).

%-----------------------------------------------------------------------------%

robust(normal).
robust(context(_)).
robust(unified(_)).
robust(rcs).
robust(ifdef(_)).
robust(side_by_side).
robust(cvs_merge_conflict).

%-----------------------------------------------------------------------------%

    % show_file shows the segment of the file from Low to High, with
    % each line preceded by the Prefix character and a space. The diff(1)
    % format specifies that the lines effected in the first file should be
    % flagged by '<' and the lines effected in the second file should be
    % flagged by '>'.
    %
:- pred show_file(file::in, string::in, pos::in, pos::in,
    io::di, io::uo) is det.

show_file(File, Prefix, Low, High, !IO) :-
    globals.io_lookup_bool_option(expand_tabs, ExpandTabs, !IO),
    show_file_2(ExpandTabs, File, Prefix, Low, High, !IO).

:- pred show_file_2(bool::in, file::in, string::in, pos::in, pos::in,
    io::di, io::uo) is det.

show_file_2(ExpandTabs, File, Prefix, Low, High, !IO) :-
    ( if Low < High then
        ( if file.get_line(File, Low, Line) then
            io.write_string(Prefix, !IO),
            (
                ExpandTabs = yes,
                string.to_char_list(Line, LineList),
                expand_tabs(LineList, 0, !IO)
            ;
                ExpandTabs = no,
                io.write_string(Line, !IO)
            ),
            show_file_2(ExpandTabs, File, Prefix, Low + 1, High, !IO)
        else
            error("diff_out_show_file: file ended prematurely")
        )
    else
        true
    ).

:- pred expand_tabs(list(char)::in, int::in, io::di, io::uo) is det.

expand_tabs([], _, !IO).
expand_tabs([C | Cs], Pos, !IO) :-
    ( if C = '\t' then
        Spaces = tab_width - (Pos rem tab_width),
        put_spaces(Spaces, Pos, NewPos, !IO),
        expand_tabs(Cs, NewPos, !IO)
    else
        io.write_char(C, !IO),
        expand_tabs(Cs, Pos + 1, !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % display_diff: Determine which output style to use, then call the
    % predicate to display that output.
    %
    % Some of these options (notably the ones which require no output) should
    % have been handled already by the time we reach here. In those cases, we
    % just call error/1.
    %
display_diff(File1, File2, Diff, !IO) :-
    globals.io_get_output_style(OutputStyle, !IO),
    ( if
        Diff = [],
        no_diff_implies_no_output(OutputStyle)
    then
        true
    else
        display_diff_2(OutputStyle, File1, File2, Diff, !IO)
    ).

:- pred display_diff_2(output_style::in, file::in, file::in, diff::in,
    io::di, io::uo) is det.

display_diff_2(normal, File1, File2, Diff, !IO) :-
    display_diff_normal(File1, File2, Diff, !IO).
display_diff_2(help_only, _File1, _File2, _Diff, !IO) :-
    error("display_diff: help_only").
display_diff_2(version_only, _File1, _File2, _Diff, !IO) :-
    error("display_diff: version_only").
display_diff_2(context(Context), File1, File2, Diff, !IO) :-
    display_context_diff(Context, File1, File2, Diff, !IO).
display_diff_2(unified(Context), File1, File2, Diff, !IO) :-
    display_unified_diff(Context, File1, File2, Diff, !IO).
display_diff_2(ed, File1, File2, Diff, !IO) :-
    display_diff_ed(File1, File2, Diff, !IO).
display_diff_2(forward_ed, File1, File2, Diff, !IO) :-
    display_diff_forward_ed(File1, File2, Diff, !IO).
display_diff_2(rcs, File1, File2, Diff, !IO) :-
    display_diff_rcs(File1, File2, Diff, !IO).
display_diff_2(ifdef(Sym), File1, File2, Diff, !IO) :-
    display_diff_ifdef(Sym, File1, File2, Diff, !IO).
display_diff_2(brief, File1, File2, _Diff, !IO) :-
    % XXX For this output style, we really don't need to perform
    % a complete diff. This should be handled higher up for efficiency.
    file.get_file_name(File1, FileName1),
    file.get_file_name(File2, FileName2),
    io.write_strings(["Files ", FileName1, " and ",
        FileName2, " differ\n"], !IO).
display_diff_2(side_by_side, File1, File2, Diff, !IO) :-
    display_diff_side_by_side(File1, File2, Diff, !IO).
display_diff_2(cvs_merge_conflict, File1, File2, Diff, !IO) :-
    display_diff_cvs_merge_conflict(File1, File2, Diff, !IO).

%-----------------------------------------------------------------------------%

    % display_diff_normal takes a diff and displays it
    % in the standard diff(1) output format.
    %
:- pred display_diff_normal(file::in, file::in, diff::in,
    io::di, io::uo) is det.

display_diff_normal(File1, File2, Diff, !IO) :-
    globals.io_lookup_bool_option(initial_tab, InitialTab, !IO),
    (
        InitialTab = no,
        FromStr = "< ",
        ToStr = "> "
    ;
        InitialTab = yes,
        FromStr = "<\t",
        ToStr = ">\t"
    ),
    display_diff_normal_2(File1, File2, Diff, FromStr, ToStr, !IO).

    % display_diff_normal takes a diff and displays it
    % in the standard diff(1) output format.
    %
:- pred display_diff_normal_2(file::in, file::in, diff::in, string::in,
    string::in, io::di, io::uo) is det.

display_diff_normal_2(_, _, [], _, _, !IO).
display_diff_normal_2(File1, File2, [SingDiff | Diff], FromStr, ToStr, !IO) :-
    (
        SingDiff = add(X, Y1 - Y2),
        diff_out.write_command(X - X, 'a', Y1 - Y2, !IO),
        diff_out.show_file(File2, ToStr, Y1, Y2, !IO)
    ;
        SingDiff = delete(X1 - X2, Y),
        diff_out.write_command(X1 - X2, 'd', Y - Y, !IO),
        diff_out.show_file(File1, FromStr, X1, X2, !IO)
    ;
        SingDiff = change(X1 - X2, Y1 - Y2),
        diff_out.write_command(X1 - X2, 'c', Y1 - Y2, !IO),
        diff_out.show_file(File1, FromStr, X1, X2, !IO),
        io.write_string("---\n", !IO),
        diff_out.show_file(File2, ToStr, Y1, Y2, !IO)
    ),
    display_diff_normal_2(File1, File2, Diff, FromStr, ToStr, !IO).

    % write_command displays a diff(1) command.
    % Like ed(1), a pair of numbers which are identical are abbreviated by a
    % single number.
    % MK: Assumption X=<X2
    % AJB: And, similarly, Y=<Y2. This is actually an invariant of the
    % segment type. See difftype.m.
    %
:- pred write_command(segment::in, char::in, segment::in,
    io::di, io::uo) is det.

write_command(X - X2, C, Y - Y2, !IO) :-
    X1 = X + 1,    % Convert from pos to line number.
    ( if X1 >= X2 then
        % Either empty or singleton segment.
        io.write_int(X2, !IO)
    else
        io.write_int(X1, !IO),
        io.write_char(',', !IO),
        io.write_int(X2, !IO)
    ),
    io.write_char(C, !IO),
    Y1 = Y + 1,    % Convert from pos to line number.
    ( if Y1 >= Y2 then
        % Either empty or singleton segment.
        io.write_int(Y2, !IO)
    else
        io.write_int(Y1, !IO),
        io.write_char(',', !IO),
        io.write_int(Y2, !IO)
    ),
    io.write_char('\n', !IO).

%-----------------------------------------------------------------------------%

    % display_diff_rcs takes a diff and displays it in the RCS difference
    % format.
    %
:- pred display_diff_rcs(file::in, file::in, diff::in, io::di, io::uo) is det.

display_diff_rcs(_File1, _File2, [], !IO).
display_diff_rcs(File1, File2, [Cmd | Diff], !IO) :-
    (
        Cmd = add(X, Y1 - Y2),
        write_command_rcs('a', X, Y2 - Y1, !IO),
        show_file(File2, "", Y1, Y2, !IO)
    ;
        Cmd = delete(X1 - X2, _Y),
        write_command_rcs('d', X1, X2 - X1, !IO)
    ;
        Cmd = change(X1 - X2, Y1 - Y2),
        write_command_rcs('d', X1, X2 - X1, !IO),
        write_command_rcs('a', X1, Y2 - Y1, !IO),
        show_file(File2, "", Y1, Y2, !IO)
    ),
    display_diff_rcs(File1, File2, Diff, !IO).

    % diff_out.write_command_rcs displays a diff command in the RCS ,v format.
    %
:- pred write_command_rcs(char::in, int::in, int::in, io::di, io::uo) is det.

write_command_rcs(C, X, Y, !IO) :-
    io.write_char(C, !IO),
    io.write_int(X + 1, !IO),    % Convert from pos to line number.
    io.write_char(' ', !IO),
    io.write_int(Y, !IO),
    io.write_char('\n', !IO).

%-----------------------------------------------------------------------------%

    % display_diff_ed takes a diff and displays it in ed(1) format, but with
    % all diffs backward.
    %
:- pred display_diff_ed(file::in, file::in, diff::in, io::di, io::uo) is det.

display_diff_ed(_File1, _File2, [], !IO).
display_diff_ed(File1, File2, [Cmd | Diff], !IO) :-
    display_diff_ed(File1, File2, Diff, !IO),
    (
        Cmd = add(X, Y1 - Y2),
        write_command_ed(X - X, 'a', !IO),
        show_file(File2, "", Y1, Y2, !IO),
        io.write_string(".\n", !IO)
    ;
        Cmd = delete(X, _Y),
        write_command_ed(X, 'd', !IO)
    ;
        Cmd = change(X, Y1 - Y2),
        write_command_ed(X, 'c', !IO),
        show_file(File2, "", Y1, Y2, !IO),
        io.write_string(".\n", !IO)
    ).

    % write_command_ed displays an ed(1) command.
    %
:- pred write_command_ed(segment::in, char::in, io::di, io::uo) is det.

write_command_ed(X - X2, C, !IO) :-
    X1 = X + 1,        % Convert from pos to line number
    ( if X1 >= X2 then
        % Either empty or singleton segment.
        io.write_int(X2, !IO)
    else
        io.write_int(X1, !IO),
        io.write_char(',', !IO),
        io.write_int(X2, !IO)
    ),
    io.write_char(C, !IO),
    io.write_char('\n', !IO).

%-----------------------------------------------------------------------------%

    % display_diff_forward_ed takes a diff and displays it in ed(1) format, but
    % with all diff_out forward. This is actually useless for feeding to ed(1),
    % but nicer to read.
    %
:- pred display_diff_forward_ed(file::in, file::in, diff::in,
    io::di, io::uo) is det.

display_diff_forward_ed(_File1, _File2, [], !IO).
display_diff_forward_ed(File1, File2, [Cmd | Diff], !IO) :-
    (
        Cmd = add(X, Y1 - Y2),
        write_command_forward_ed(X - X, 'a', !IO),
        show_file(File2, "", Y1, Y2, !IO),
        io.write_string(".\n", !IO)
    ;
        Cmd = delete(X, _Y),
        write_command_forward_ed(X, 'd', !IO)
    ;
        Cmd = change(X, Y1 - Y2),
        write_command_forward_ed(X, 'c', !IO),
        show_file(File2, "", Y1, Y2, !IO),
        io.write_string(".\n", !IO)
    ),
    display_diff_forward_ed(File1, File2, Diff, !IO).

    % write_command_forward_ed displays a forward ed(1) command.
    % The difference between this and write_command_ed is that the command char
    % comes first here. Who comes up with these dumb formats anyway?
    %
:- pred write_command_forward_ed(segment::in, char::in, io::di, io::uo) is det.

write_command_forward_ed(X - X2, C, !IO) :-
    io.write_char(C, !IO),
    X1 = X + 1,        % Convert from pos to line number
    ( if X1 >= X2 then
        % Either empty or singleton segment.
        io.write_int(X2, !IO)
    else
        io.write_int(X1, !IO),
        io.write_char(' ', !IO),
        io.write_int(X2, !IO)
    ),
    io.write_char('\n', !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % display_diff_ifdef writes out the files in a unified diff,
    % using #ifdefs around each edit.
    %
    % TO DO: GNU diff makes this output style much more configurable.
    % We should too.
    %
:- pred display_diff_ifdef(string::in, file::in, file::in, diff::in,
    io::di, io::uo) is det.

display_diff_ifdef(Sym, File1, File2, Diff, !IO) :-
    display_diff_ifdef_2(0, Sym, File1, File2, Diff, !IO).

    % Argument 1 (prev) is the last pos displayed before the current edit (or
    % end of edits, in the base case).
    % This is important for when we have to display the "non-diffed" text
    % between edits.
    %
:- pred display_diff_ifdef_2(int::in, string::in, file::in, file::in,
    diff::in, io::di, io::uo) is det.

display_diff_ifdef_2(Prev, _Sym, File1, _File2, [], !IO) :-
    file.get_numlines(File1, SegEnd),
    show_file(File1, "", Prev, SegEnd, !IO).
display_diff_ifdef_2(Prev, Sym, File1, File2, [Edit | Diff], !IO) :-
    first_mentioned_positions(Edit, StartOfEdit, _),
    show_file(File1, "", Prev, StartOfEdit, !IO),
    (
        Edit = add(X, Y1 - Y2),
        io.write_strings(["#ifdef ", Sym, "\n"], !IO),
        show_file(File2, "", Y1, Y2, !IO),
        io.write_strings(["#endif /* ", Sym, " */\n"], !IO),
        Next = X
    ;
        Edit = delete(X1 - X2, _),
        io.write_strings(["#ifndef ", Sym, "\n"], !IO),
        show_file(File1, "", X1, X2, !IO),
        io.write_strings(["#endif /* not ", Sym, " */\n"], !IO),
        Next = X2
    ;
        Edit = change(X1 - X2, Y1 - Y2),
        io.write_strings(["#ifndef ", Sym, "\n"], !IO),
        show_file(File1, "", X1, X2, !IO),
        io.write_strings(["#else /* ", Sym, " */\n"], !IO),
        show_file(File2, "", Y1, Y2, !IO),
        io.write_strings(["#endif /* ", Sym, " */\n"], !IO),
        Next = X2
    ),
    display_diff_ifdef_2(Next, Sym, File1, File2, Diff, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % display_diff_cvs_merge_conflict writes out the files in a
    % unified diff, using CVS merge conflict marks around each edit.
    %
:- pred display_diff_cvs_merge_conflict(file::in, file::in, diff::in,
    io::di, io::uo) is det.

display_diff_cvs_merge_conflict(File1, File2, Diff, !IO) :-
    display_diff_cvs_merge_conflict_2(0, File1, File2, Diff, !IO).

    % Argument 1 (prev) is the last pos displayed before the current edit (or
    % end of edits, in the base case).
    % This is important for when we have to display the "non-diffed" text
    % between edits.
    %
:- pred display_diff_cvs_merge_conflict_2(int::in, file::in, file::in,
    diff::in, io::di, io::uo) is det.

display_diff_cvs_merge_conflict_2(Prev, File1, _File2, [], !IO) :-
    file.get_numlines(File1, SegEnd),
    show_file(File1, "", Prev, SegEnd, !IO).
display_diff_cvs_merge_conflict_2(Prev, File1, File2, [Edit | Diff], !IO) :-
    first_mentioned_positions(Edit, StartOfEdit, _),
    show_file(File1, "", Prev, StartOfEdit, !IO),
    file.get_file_name(File1, FileName1),
    file.get_file_name(File2, FileName2),
    (
        Edit = add(X, Y1 - Y2),
        io.write_strings(["<<<<<<< ", FileName1, "\n"], !IO),
        show_file(File2, "", Y1, Y2, !IO),
        io.write_string("=======\n", !IO),
        io.write_strings([">>>>>>> ", FileName2, "\n"], !IO),
        Next = X
    ;
        Edit = delete(X1 - X2, _),
        io.write_strings(["<<<<<<< ", FileName1, "\n"], !IO),
        io.write_string("=======\n", !IO),
        show_file(File1, "", X1, X2, !IO),
        io.write_strings([">>>>>>> ", FileName2, "\n"], !IO),
        Next = X2
    ;
        Edit = change(X1 - X2, Y1 - Y2),
        io.write_strings(["<<<<<<< ", FileName1, "\n"], !IO),
        show_file(File1, "", X1, X2, !IO),
        io.write_string("=======\n", !IO),
        show_file(File2, "", Y1, Y2, !IO),
        io.write_strings([">>>>>>> ", FileName2, "\n"], !IO),
        Next = X2
    ),
    display_diff_cvs_merge_conflict_2(Next, File1, File2, Diff, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Types for context/unified diffs.

    % A context diff is a bit more complicated than a "standard"
    % diff, because it requires the display of some parts of the
    % files which are not actually part of the diff, but not all
    % of it.
    %
    % Because context and unified diffs both require the same
    % kind of information, we factor out the code to turn a
    % normal diff into a context diff.
    %
:- type context_edit
    --->    context_edit(segment, segment, diff).

:- type context_diff == list(context_edit).

%-----------------------------------------------------------------------------%

:- pred diff_to_context_diff(int::in, int::in, int::in, diff::in,
    context_diff::out) is det.

diff_to_context_diff(_Xsize, _Ysize, _Context, [], []).
diff_to_context_diff(Xsize, Ysize, Context, [Edit | Diff], CDiff) :-
    diff_to_context_diff(Xsize, Ysize, Context, Diff, CDiff0),

    % Work out how far the context of this edit reaches.
    first_mentioned_positions(Edit, Xfirst0, Yfirst0),
    int.max(Xfirst0 - Context, 0, Xfirst),
    int.max(Yfirst0 - Context, 0, Yfirst),
    last_mentioned_positions(Edit, Xlast0, Ylast0),
    int.min(Xlast0 + Context, Xsize, Xlast),
    int.min(Ylast0 + Context, Ysize, Ylast),

    (
        CDiff0 = [],
        CDiff = [context_edit(Xfirst - Xlast, Yfirst - Ylast, [Edit])]
    ;
        CDiff0 =
            [context_edit(XsegLo - XsegHi, YsegLo - YsegHi, DDiff) | CDiff1],
        % Should we merge this edit into the next one?
        ( if
            ( XsegLo =< Xlast
            ; YsegLo =< Ylast
            )
        then
            CDiff = [context_edit(Xfirst - XsegHi, Yfirst - YsegHi,
                [Edit | DDiff]) | CDiff1]
        else
            CDiff = [context_edit(Xfirst - Xlast, Yfirst - Ylast,
                [Edit]) | CDiff0]
        )
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Display a diff in unified format.
    %
:- pred display_unified_diff(int::in, file::in, file::in, diff::in,
    io::di, io::uo) is det.

display_unified_diff(Context, File1, File2, Diff, !IO) :-
    file.get_numlines(File1, Size1),
    file.get_numlines(File2, Size2),
    diff_to_context_diff(Size1, Size2, Context, Diff, CDiff),
    file.get_file_name(File1, Name1),
    file.get_file_name(File2, Name2),
    % XXX Should also print out file dates. But how?
    io.write_strings(["--- ", Name1, "\n"], !IO),
    io.write_strings(["+++ ", Name2, "\n"], !IO),
    globals.io_lookup_bool_option(initial_tab, InitialTab, !IO),
    (
        InitialTab = no,
        NoneStr = " ",
        AddStr = "+",
        DelStr = "-"
    ;
        InitialTab = yes,
        NoneStr = "\t",
        AddStr = "+\t",
        DelStr = "-\t"
    ),
    display_unified_diff_2(File1, File2, CDiff, NoneStr, AddStr, DelStr, !IO).

:- pred display_unified_diff_2(file::in, file::in, context_diff::in,
    string::in, string::in, string::in, io::di, io::uo) is det.

display_unified_diff_2(_File1, _File2, [], _, _, _, !IO).
display_unified_diff_2(File1, File2, [Edit | CDiff], NoneStr, AddStr, DelStr,
        !IO) :-
    Edit = context_edit(Xlow - Xhigh, Ylow - Yhigh, Diff),
    io.format("@@ -%d,%d +%d,%d @@\n",
        [i(Xlow + 1), i(Xhigh - Xlow), i(Ylow + 1), i(Yhigh - Ylow)], !IO),
    display_unified_diff_3(Xlow, Xhigh, File1, File2, Diff, NoneStr, AddStr,
        DelStr, !IO),
    display_unified_diff_2(File1, File2, CDiff, NoneStr, AddStr, DelStr, !IO).

:- pred display_unified_diff_3(int::in, int::in, file::in, file::in, diff::in,
    string::in, string::in, string::in, io::di, io::uo) is det.

display_unified_diff_3(Prev, Size1, File1, _File2, [], NoneStr, _, _, !IO) :-
    show_file(File1, NoneStr, Prev, Size1, !IO).
display_unified_diff_3(Prev, Size1, File1, File2, [Edit | Diff],
        NoneStr, AddStr, DelStr, !IO) :-
    first_mentioned_positions(Edit, StartOfEdit, _),
    show_file(File1, NoneStr, Prev, StartOfEdit, !IO),
    (
        Edit = add(X, Y1 - Y2),
        show_file(File2, AddStr, Y1, Y2, !IO),
        Next = X
    ;
        Edit = delete(X1 - X2, _),
        show_file(File1, DelStr, X1, X2, !IO),
        Next = X1
    ;
        Edit = change(X1 - X2, Y1 - Y2),
        show_file(File1, DelStr, X1, X2, !IO),
        show_file(File2, AddStr, Y1, Y2, !IO),
        Next = X1
    ),
    display_unified_diff_3(Next, Size1, File1, File2, Diff,
        NoneStr, AddStr, DelStr, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Display a diff in context format.
    %
:- pred display_context_diff(int::in, file::in, file::in, diff::in,
    io::di, io::uo) is det.

display_context_diff(Context, File1, File2, Diff, !IO) :-
    file.get_numlines(File1, Size1),
    file.get_numlines(File2, Size2),
    diff_to_context_diff(Size1, Size2, Context, Diff, CDiff),
    file.get_file_name(File1, Name1),
    file.get_file_name(File2, Name2),
    % XXX Should also print out file dates. But how??
    io.write_strings(["*** ", Name1, "\n"], !IO),
    io.write_strings(["--- ", Name2, "\n"], !IO),

    globals.io_lookup_bool_option(initial_tab, InitialTab, !IO),
    (
        InitialTab = no,
        NoneStr = "  ",
        AddStr = "+ ",
        DelStr = "- ",
        ChgStr = "! "
    ;
        InitialTab = yes,
        NoneStr = "\t",
        AddStr = "+\t",
        DelStr = "-\t",
        ChgStr = "!\t"
    ),
    display_context_diff_2(File1, File2, CDiff, NoneStr, AddStr, DelStr,
        ChgStr, !IO).

:- pred display_context_diff_2(file::in, file::in, context_diff::in,
    string::in, string::in, string::in, string::in, io::di, io::uo) is det.

display_context_diff_2(_File1, _File2, [], _, _, _, _, !IO).
display_context_diff_2(File1, File2, [Edit | CDiff],
        NoneStr, AddStr, DelStr, ChgStr, !IO) :-
    Edit = context_edit(Xlow - Xhigh, Ylow - Yhigh, Diff),
    io.write_string("***************\n", !IO),
    io.format("*** %d,%d ****\n", [i(Xlow + 1), i(Xhigh)], !IO),

    % Don't display the "context from" lines if there's nothing deleted or
    % changed.
    %
    ( if
        all [AEdit] list.member(AEdit, Diff) => AEdit = add(_, _)
    then
        true
    else
        display_context_diff_left(Xlow, Xhigh, File1, Diff, NoneStr,
            DelStr, ChgStr, !IO)
    ),
    io.format("--- %d,%d ----\n", [i(Ylow + 1), i(Yhigh)], !IO),

    % Don't display the "context to" lines if there's nothing added or changed.
    %
    ( if
        all [DEdit] list.member(DEdit, Diff) => DEdit = delete(_, _)
    then
        true
    else
        display_context_diff_right(Ylow, Yhigh, File2, Diff, NoneStr,
            AddStr, ChgStr, !IO)
    ),
    display_context_diff_2(File1, File2, CDiff, NoneStr, AddStr, DelStr,
        ChgStr, !IO).

:- pred display_context_diff_left(int::in, int::in, file::in, diff::in,
    string::in, string::in, string::in, io::di, io::uo) is det.

display_context_diff_left(Prev, Size1, File1, [], NoneStr, _, _, !IO) :-
    show_file(File1, NoneStr, Prev, Size1, !IO).
display_context_diff_left(Prev, Size1, File1, [Edit | Diff], NoneStr,
        DelStr, ChgStr, !IO) :-
    first_mentioned_positions(Edit, StartOfEdit, _),
    show_file(File1, NoneStr, Prev, StartOfEdit, !IO),
    (
        Edit = add(X, _),
        Next = X
    ;
        Edit = delete(X1 - X2, _),
        show_file(File1, DelStr, X1, X2, !IO),
        Next = X2
    ;
        Edit = change(X1 - X2, _),
        show_file(File1, ChgStr, X1, X2, !IO),
        Next = X2
    ),
    display_context_diff_left(Next, Size1, File1, Diff, NoneStr,
        DelStr, ChgStr, !IO).

:- pred display_context_diff_right(int::in, int::in, file::in, diff::in,
    string::in, string::in, string::in, io::di, io::uo) is det.

display_context_diff_right(Prev, Size2, File2, [], NoneStr, _, _, !IO) :-
    diff_out.show_file(File2, NoneStr, Prev, Size2, !IO).
display_context_diff_right(Prev, Size2, File2, [Edit | Diff], NoneStr,
        AddStr, ChgStr, !IO) :-
    first_mentioned_positions(Edit, StartOfEdit, _),
    show_file(File2, NoneStr, Prev, StartOfEdit, !IO),
    (
        Edit = add(_, Y1 - Y2),
        show_file(File2, AddStr, Y1, Y2, !IO),
        Next = Y2
    ;
        Edit = delete(_, Y),
        Next = Y
    ;
        Edit = change(_, Y1 - Y2),
        show_file(File2, ChgStr, Y1, Y2, !IO),
        Next = Y2
    ),
    display_context_diff_right(Next, Size2, File2, Diff, NoneStr, AddStr,
        ChgStr, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Side-by-side diffs are incredibly complex, as you'll see if
    % you inspect the code below.
    %
    % TO DO: GNU diff has --sdiff-merge-assist, but I can find no documentation
    % on what this actually does, and haven't had the time to investigate.
    % For the moment, we accept the option and note here whether or not
    % it is turned on, but do nothing with it.

    % Parameters to pass around.
    %
:- type side_by_side_info
    --->    side_by_side_info(
                int,        % Half width
                int,        % Column 2 offset
                bool,       % Left column only
                bool,       % Suppress common lines
                bool        % Help sdiff
            ).

:- pred display_diff_side_by_side(file::in, file::in, diff::in, io::di, io::uo)
    is det.

display_diff_side_by_side(File1, File2, Diff, !IO) :-
    globals.io_lookup_int_option(width, Width0, !IO),

    % Calculate the half-width and offset stuff.

    % XXX If we are expanding tabs, we should factor this in.
    Off = (Width0 + 4) // 8 * 4,
    Max =  Off - 3,
    HalfWidth0 = Width0 - Off + 1,
    ( if HalfWidth0 =< 0 then
        HalfWidth = 0
    else if HalfWidth0 > Max then
        HalfWidth = Max
    else
        HalfWidth = HalfWidth0
    ),
    ( if HalfWidth > 0 then
        Col2Off = Off
    else
        Col2Off = Width0
    ),
    globals.io_lookup_bool_option(left_column, LeftCol, !IO),
    globals.io_lookup_bool_option(suppress_common_lines, Suppress, !IO),
    globals.io_lookup_bool_option(sdiff_merge_assist, Sdiff, !IO),
    SBS = side_by_side_info(HalfWidth, Col2Off, LeftCol, Suppress, Sdiff),
    display_diff_side_by_side_2(0, SBS, File1, File2, Diff, !IO).

:- pred display_diff_side_by_side_2(int::in, side_by_side_info::in,
    file::in, file::in, diff::in, io::di, io::uo) is det.

display_diff_side_by_side_2(Prev, SBS, File1, _File2, [], !IO) :-
    SBS = side_by_side_info(_, _, _, Suppress, _),
    (
        Suppress = no,
        file.get_numlines(File1, SegEnd),
        show_sbs_same_lines(File1, SBS, Prev - SegEnd, !IO)
    ;
        Suppress = yes
    ).
display_diff_side_by_side_2(Prev, SBS, File1, File2, [Edit | Diff], !IO) :-
    SBS = side_by_side_info(_, _, _, Suppress, _),
    first_mentioned_positions(Edit, StartOfEdit, _),
    (
        Suppress = no,
        show_sbs_same_lines(File1, SBS, Prev - StartOfEdit, !IO)
    ;
        Suppress = yes
    ),
    (
        Edit = add(X, Seg2),
        show_sbs_added_lines(File2, SBS, Seg2, !IO),
        Next = X
    ;
        Edit = delete(X1 - X2, _),
        show_sbs_deleted_lines(File1, SBS, X1 - X2, !IO),
        Next = X2
    ;
        Edit = change(X1 - X2, Y1 - Y2),
        % The side-by-side change diff format is sort of weird.
        % We have to compute the minimum of the two change sizes, and display
        % "changed" lines for the minimum of these sizes. Then we display
        % "added" or "deleted" lines for whatever is left over.
        int.min(X2 - X1, Y2 - Y1, Size),
        show_sbs_changed_lines(File1, File2, SBS, X1, Y1, Size, !IO),
        show_sbs_deleted_lines(File1, SBS, (X1 + Size) - X2, !IO),
        show_sbs_added_lines(File2, SBS, (Y1 + Size) - Y2, !IO),
        Next = X2
    ),
    display_diff_side_by_side_2(Next, SBS, File1, File2, Diff, !IO).

:- pred show_sbs_changed_lines(file::in, file::in, side_by_side_info::in,
    int::in, int::in, int::in, io::di, io::uo) is det.

show_sbs_changed_lines(File1, File2, SBS, X1, Y1, Size, !IO) :-
    ( if Size > 0 then
        ( if
            file.get_line(File1, X1, Line1),
            file.get_line(File2, Y1, Line2)
        then
            SBS = side_by_side_info(Width, _, _, _, _),
            string.to_char_list(Line1, Chars1),
            print_half_line(Chars1, SBS, 0, 0, Width, OutPos, !IO),
            tab_to_column(OutPos, Width, !IO),
            io.write_string("|", !IO),
            tab_to_column(Width + 1, Width + 2, !IO),
            string.to_char_list(Line2, Chars2),
            print_half_line(Chars2, SBS, 0, 0, Width, _, !IO),
            io.write_string("\n", !IO),
            show_sbs_changed_lines(File1, File2, SBS, X1 + 1, Y1 + 1,
                Size - 1, !IO)
        else
            error("show_sbs_changed_lines: file ended prematurely")
        )
    else
        true
    ).

:- pred show_sbs_same_lines(file::in, side_by_side_info::in, segment::in,
    io::di, io::uo) is det.

show_sbs_same_lines(File, SBS, Low - High, !IO) :-
    ( if Low < High then
        ( if file.get_line(File, Low, Line) then
            SBS = side_by_side_info(Width, _, LeftCol, _, _),
            string.to_char_list(Line, Chars),
            print_half_line(Chars, SBS, 0, 0, Width, OutPos, !IO),

            % If the user specified --left, don't display the right column
            % here.
            (
                LeftCol = yes,
                tab_to_column(OutPos, Width, !IO),
                io.write_string("(", !IO)
            ;
                LeftCol = no,
                tab_to_column(OutPos, Width + 2, !IO),
                print_half_line(Chars, SBS, 0, 0, Width, _, !IO)
            ),
            io.write_string("\n", !IO),
            show_sbs_same_lines(File, SBS, (Low + 1) - High, !IO)
        else
            error("show_sbs_same_lines: file ended prematurely")
        )
    else
        true
    ).

:- pred show_sbs_added_lines(file::in, side_by_side_info::in,
    segment::in, io::di, io::uo) is det.

show_sbs_added_lines(File, SBS, Low - High, !IO) :-
    ( if Low < High then
        ( if file.get_line(File, Low, Line) then
            SBS = side_by_side_info(Width, _, _, _, _),
            string.to_char_list(Line, Chars),
            tab_to_column(0, Width, !IO),
            io.write_string("> ", !IO),
            print_half_line(Chars, SBS, 0, 0, Width, _, !IO),
            io.write_string("\n", !IO),
            show_sbs_added_lines(File, SBS, (Low + 1) - High, !IO)
        else
            error("show_sbs_added_lines: file ended prematurely")
        )
    else
        true
    ).

:- pred show_sbs_deleted_lines(file::in, side_by_side_info::in,
    segment::in, io::di, io::uo) is det.

show_sbs_deleted_lines(File, SBS, Low - High, !IO) :-
    ( if Low < High then
        ( if file.get_line(File, Low, Line) then
            SBS = side_by_side_info(Width, _, _, _, _),
            string.to_char_list(Line, Chars),
            print_half_line(Chars, SBS, 0, 0, Width, OutPos, !IO),
            tab_to_column(OutPos, Width, !IO),
            io.write_string("<\n", !IO),
            show_sbs_deleted_lines(File, SBS, (Low + 1) - High, !IO)
        else
            error("show_sbs_deleted_lines: file ended prematurely")
        )
    else
        true
    ).

:- func tab_width = int.

tab_width = 8.

    % Put a number of spaces on the output stream.
    % Update % the output column as we go.
    %
:- pred put_spaces(int::in, int::in, int::out, io::di, io::uo) is det.

put_spaces(Spaces, !OutPos, !IO) :-
    ( if Spaces =< 0 then
        true
    else
        io.write_char(' ', !IO),
        !:OutPos = !.OutPos + 1,
        put_spaces(Spaces - 1, !OutPos, !IO)
    ).

    % Given a "from" column and a "to" column, put sufficient spaces
    % on the output stream to reach that column. Use tabs if we can.
    %
:- pred tab_to_column(int::in, int::in, io::di, io::uo) is det.

tab_to_column(From, To, !IO) :-
    AfterTab = From + tab_width - (From rem tab_width),
    ( if AfterTab > To then
        ( if From < To then
            io.write_char(' ', !IO),
            tab_to_column(From + 1, To, !IO)
        else
            true
        )
    else
        io.write_char('\t', !IO),
        tab_to_column(AfterTab, To, !IO)
    ).

    % Display half a line in a side-by-side diff, stopping when
    % we reach a certain column.
    %
    % This is actually a very simple thing to do, except for one
    % complication, which is the displaying of tab characters.
    %
    % The important variables are:
    %
    %   InPos: The current column in the input line.
    %   OutPos: The current column in the output line.
    %   OutBound: The column that we must stop at.
    %
:- pred print_half_line(list(char)::in, side_by_side_info::in,
    int::in, int::in, int::in, int::out,
    io::di, io::uo) is det.

print_half_line([], _SBS, _InPos, OutPos, _OutBound, OutPos, !IO).
print_half_line([C | Cs], SBS, InPos0, OutPos0, OutBound, OutPos, !IO) :-
    ( if
        C = '\t'
    then
            % Calculate how many spaces this tab is worth.
        Spaces = tab_width - InPos0 rem tab_width,
        ( if InPos0 = OutPos0 then
            globals.io_lookup_bool_option(expand_tabs, ExpandTabs, !IO),
            (
                ExpandTabs = yes,
                % If we are expanding tabs, we just pretend that
                % we had Spaces spaces and write them.
                TabStop0 = OutPos0 + Spaces,
                ( if TabStop0 > OutBound then
                    TabStop = OutBound
                else
                    TabStop = TabStop0
                ),
                put_spaces(TabStop - OutPos0, OutPos0, OutPos1, !IO)
            ;
                % If we are not expanding tabs, just print it and
                % hope everything lines up okay.
                ExpandTabs = no,
                io.write_char('\t', !IO),
                OutPos1 = OutPos0 + Spaces
            )
        else
            OutPos1 = OutPos0
        ),
        InPos = InPos0 + Spaces
    else if
        ( C = '\r' ; C = '\b' ; C = '\n' )
    then
        % XXX What to do?  For the moment, we'll just ignore it.
        InPos = InPos0, OutPos1 = OutPos0
    /***********
        % XXX Binary files aren't really supported.
    else if
        not char.is_print(C)
    then
        InPos = InPos0, OutPos1 = OutPos0
        ( if InPos < OutBound then
            io.write_char(C, !IO)
        else
            true
        )
    ***********/
    else
        % The default case. Print and be done with it.
        InPos = InPos0 + 1,
        ( if InPos < OutBound then
            OutPos1 = InPos,
            io.write_char(C, !IO)
        else
            OutPos1 = OutPos0
        )
    ),
    print_half_line(Cs, SBS, InPos, OutPos1, OutBound, OutPos, !IO).

%-----------------------------------------------------------------------------%
:- end_module diff_out.
%-----------------------------------------------------------------------------%
