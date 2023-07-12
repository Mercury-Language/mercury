%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: write_error_spec.m.
% Main author: zs.
%
% This module contains code to format error_specs, which are specifications
% of diagnostics, for output. The output we generate has the following form:
%
% module.m:10: first line of error message blah blah blah
% module.m:10:   second line of error message blah blah blah
% module.m:10:   third line of error message blah blah blah
%
% The words of the diagnostic will be packed into lines as tightly as possible,
% with spaces between each pair of words, subject to the constraints
% that every line starts with a context, followed by Indent+1 spaces
% on the first line and Indent+3 spaces on later lines, and that every
% line contains at most <n> characters (unless a long single word
% forces the line over this limit) where --max-error-line-width <n>.
% The error_spec may modify this structure, e.g. by inserting line breaks,
% inserting blank lines, and by incresing/decreasing the indent level.
%
%---------------------------------------------------------------------------%

:- module parse_tree.write_error_spec.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % write_error_spec(Globals, Spec, !IO):
    % write_error_spec(Stream, Globals, Spec, !IO):
    % write_error_specs(Globals, Specs, !IO):
    % write_error_specs(Stream, Globals, Specs, !IO):
    %
    % Write out the error message(s) specified by Spec or Specs, minus the
    % parts whose conditions are false.
    %
    % Set the exit status to 1 if we found any errors, or if we found any
    % warnings and --halt-at-warn is set. If some error specs have verbose
    % components but they aren't being printed out, set the flag for reminding
    % the user about --verbose-errors.
    %
    % Look up option values in the supplied Globals.
    %
    % If an error spec contains only conditional messages and those conditions
    % are all false, then nothing will be printed out and the exit status
    % will not be changed. This will happen even if the severity means
    % that something should have been printed out.
    %
:- pred write_error_spec(globals::in,
    error_spec::in, io::di, io::uo) is det.
:- pragma obsolete(pred(write_error_spec/4), [write_error_spec/5]).
:- pred write_error_spec(io.text_output_stream::in, globals::in,
    error_spec::in, io::di, io::uo) is det.
:- pred write_error_specs(globals::in,
    list(error_spec)::in, io::di, io::uo) is det.
:- pragma obsolete(pred(write_error_specs/4), [write_error_specs/5]).
:- pred write_error_specs(io.text_output_stream::in, globals::in,
    list(error_spec)::in, io::di, io::uo) is det.

:- pred write_error_specs_opt_table(io.text_output_stream::in,
    option_table::in, list(error_spec)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

% XXX The predicates in this section should not be called in new code.
% New code should create error specifications, and then call write_error_spec
% to print them.

    % Display the given error message, without a context and with standard
    % indentation.
    %
:- pred write_error_pieces_plain(globals::in, list(format_piece)::in,
    io::di, io::uo) is det.
:- pragma obsolete(pred(write_error_pieces_plain/4),
    [write_error_pieces_plain/5]).
:- pred write_error_pieces_plain(io.text_output_stream::in, globals::in,
    list(format_piece)::in, io::di, io::uo) is det.

    % write_error_pieces(Globals, Context, Indent, Pieces):
    %
    % Display `Pieces' as the error message, with `Context' as a context
    % and indent by `Indent'.
    %
:- pred write_error_pieces(globals::in, prog_context::in, int::in,
    list(format_piece)::in, io::di, io::uo) is det.
:- pred write_error_pieces(io.text_output_stream::in, globals::in,
    prog_context::in, int::in,
    list(format_piece)::in, io::di, io::uo) is det.

:- pred write_error_pieces_maybe_with_context(globals::in,
    maybe(prog_context)::in, int::in, list(format_piece)::in,
    io::di, io::uo) is det.
:- pragma obsolete(pred(write_error_pieces_maybe_with_context/6),
    [write_error_pieces_maybe_with_context/7]).
:- pred write_error_pieces_maybe_with_context(io.text_output_stream::in,
    globals::in, maybe(prog_context)::in, int::in, list(format_piece)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Does (almost) the same job as write_error_pieces, but returns
    % the resulting string instead of printing it out.
    %
:- func error_pieces_to_string(list(format_piece)) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred pre_hlds_maybe_write_out_errors(io.text_output_stream::in,
    bool::in, globals::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Report a warning, and set the exit status to error if the
    % --halt-at-warn option is set.
    %
:- pred report_warning(globals::in,
    prog_context::in, int::in, list(format_piece)::in, io::di, io::uo) is det.
:- pragma obsolete(pred(report_warning/6), [report_warning/7]).
:- pred report_warning(io.text_output_stream::in, globals::in,
    prog_context::in, int::in, list(format_piece)::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % If we withheld some error information from the user (at the users'
    % own request, of course), then print a reminder of that fact,
    % to avoid violating the law of least astonishment.
    %
:- pred maybe_print_delayed_error_messages(io.text_output_stream::in,
    globals::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.indent.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_sort.
:- import_module parse_tree.error_util.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module char.
:- import_module cord.
:- import_module getopt.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%
%
% We keep a record of the set of already-printed verbose_once components
% only during the invocation of a single call to write_error_specs, or
% its singular version write_error_spec.
%
% We could possibly keep this set in a mutable, but there is no need for that.
% All error messages are generated in only one place, which means that
% they are generated in one pass. For pretty much all our passes,
% all the error messages generated by the pass are printed by a single
% call to write_error_specs. This means that while in theory, it is possible
% for verbose_once message to be printed by each of several invocations
% of write_error_specs, in practice it won't happen.

write_error_spec(Globals, Spec, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_spec(Stream, Globals, Spec, !IO).

write_error_spec(Stream, Globals, Spec, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_spec(Stream, OptionTable, LimitErrorContextsMap,
        Spec, set.init, _, !IO).

%---------------------%

write_error_specs(Globals, Specs0, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_specs(Stream, Globals, Specs0, !IO).

write_error_specs(Stream, Globals, Specs0, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    sort_error_specs(Globals, Specs0, Specs),
    list.foldl2(
        do_write_error_spec(Stream, OptionTable, LimitErrorContextsMap),
        Specs, set.init, _, !IO).

%---------------------%

write_error_specs_opt_table(Stream, OptionTable, Specs0, !IO) :-
    sort_error_specs_opt_table(OptionTable, Specs0, Specs),
    getopt.lookup_accumulating_option(OptionTable, limit_error_contexts,
        LimitErrorContexts),
    % There is nothing we can usefully do about _BadOptions.
    convert_limit_error_contexts(LimitErrorContexts, _BadOptions,
        LimitErrorContextsMap),
    list.foldl2(
        do_write_error_spec(Stream, OptionTable, LimitErrorContextsMap),
        Specs, set.init, _, !IO).

%---------------------------------------------------------------------------%

:- pred do_write_error_spec(io.text_output_stream::in, option_table::in,
    limit_error_contexts_map::in, error_spec::in,
    already_printed_verbose::in, already_printed_verbose::out,
    io::di, io::uo) is det.

do_write_error_spec(Stream, OptionTable, LimitErrorContextsMap, Spec,
        !AlreadyPrintedVerbose, !IO) :-
    (
        Spec = error_spec(Id, Severity, _Phase, Msgs1),
        MaybeActual = actual_error_severity_opt_table(OptionTable, Severity)
    ;
        Spec = simplest_spec(Id, Severity, _Phase, Context, Pieces),
        MaybeActual = actual_error_severity_opt_table(OptionTable, Severity),
        Msgs1 = [simplest_msg(Context, Pieces)]
    ;
        Spec = simplest_no_context_spec(Id, Severity, _Phase, Pieces),
        MaybeActual = actual_error_severity_opt_table(OptionTable, Severity),
        Msgs1 = [simplest_no_context_msg(Pieces)]
    ;
        Spec = conditional_spec(Id, Option, MatchValue,
            Severity, _Phase, Msgs0),
        getopt.lookup_bool_option(OptionTable, Option, Value),
        ( if Value = MatchValue then
            MaybeActual =
                actual_error_severity_opt_table(OptionTable, Severity),
            Msgs1 = Msgs0
        else
            MaybeActual = no,
            Msgs1 = []
        )
    ),
    getopt.lookup_bool_option(OptionTable, print_error_spec_id, PrintId),
    (
        PrintId = no,
        Msgs = Msgs1
    ;
        PrintId = yes,
        (
            Msgs1 = [],
            % Don't add a pred id message to an empty list of messages,
            % since there is nothing to identify.
            Msgs = Msgs1
        ;
            Msgs1 = [HeadMsg | _],
            (
                ( HeadMsg = simplest_msg(HeadContext, _Pieces)
                ; HeadMsg = simple_msg(HeadContext, _)
                ),
                MaybeHeadContext = yes(HeadContext)
            ;
                HeadMsg = simplest_no_context_msg(_),
                MaybeHeadContext = no
            ;
                HeadMsg = error_msg(MaybeHeadContext, _, _, _)
            ),
            IdMsg = error_msg(MaybeHeadContext, treat_based_on_posn, 0,
                [always([words("error_spec id:"), fixed(Id), nl])]),
            Msgs = Msgs1 ++ [IdMsg]
        )
    ),
    do_write_error_msgs(Stream, OptionTable, LimitErrorContextsMap, Msgs,
        treat_as_first, have_not_printed_anything, PrintedSome,
        !AlreadyPrintedVerbose, !IO),
    (
        PrintedSome = have_not_printed_anything
        % XXX The following assertion is commented out because the compiler
        % can generate error specs that consist only of conditional error
        % messages whose conditions can all be false (in which case nothing
        % will be printed). Such specs will cause the assertion to fail if
        % they have a severity that means something *should* have been
        % printed out. Error specs like this are generated by --debug-modes.
        % expect(unify(MaybeActual, no), $pred, "MaybeActual isn't no")
    ;
        PrintedSome = printed_something,
        (
            MaybeActual = yes(Actual),
            (
                Actual = actual_severity_error,
                io.set_exit_status(1, !IO)
            ;
                Actual = actual_severity_warning,
                record_warning_opt_table(OptionTable, !IO)
            ;
                Actual = actual_severity_informational
            )
        ;
            MaybeActual = no,
            unexpected($pred, "printed_something but MaybeActual = no")
        )
    ).

%---------------------------------------------------------------------------%

:- type maybe_treat_as_first
    --->    treat_as_first
    ;       do_not_treat_as_first.

:- type maybe_printed_something
    --->    printed_something
    ;       have_not_printed_anything.

:- type maybe_lower_next_initial
    --->    lower_next_initial
    ;       do_not_lower_next_initial.

:- type already_printed_verbose == set(list(format_piece)).

:- pred do_write_error_msgs(io.text_output_stream::in,
    option_table::in, limit_error_contexts_map::in,
    list(error_msg)::in, maybe_treat_as_first::in,
    maybe_printed_something::in, maybe_printed_something::out,
    already_printed_verbose::in, already_printed_verbose::out,
    io::di, io::uo) is det.

do_write_error_msgs(_Stream, _, _, [], _, !PrintedSome,
        !AlreadyPrintedVerbose, !IO).
do_write_error_msgs(Stream, OptionTable, LimitErrorContextsMap, [Msg | Msgs],
        !.First, !PrintedSome, !AlreadyPrintedVerbose, !IO) :-
    (
        Msg = simplest_msg(SimpleContext, Pieces),
        Components = [always(Pieces)],
        MaybeContext = yes(SimpleContext),
        TreatAsFirst = treat_based_on_posn,
        ExtraIndentLevel = 0
    ;
        Msg = simplest_no_context_msg(Pieces),
        Components = [always(Pieces)],
        MaybeContext = no,
        TreatAsFirst = treat_based_on_posn,
        ExtraIndentLevel = 0
    ;
        Msg = simple_msg(SimpleContext, Components),
        MaybeContext = yes(SimpleContext),
        TreatAsFirst = treat_based_on_posn,
        ExtraIndentLevel = 0
    ;
        Msg = error_msg(MaybeContext, TreatAsFirst, ExtraIndentLevel,
            Components)
    ),
    (
        TreatAsFirst = always_treat_as_first,
        !:First = treat_as_first
    ;
        TreatAsFirst = treat_based_on_posn
        % Leave !:First as it is, even if it is treat_as_first.
    ),
    Indent = ExtraIndentLevel * indent2_increment,
    write_msg_components(Stream, OptionTable, LimitErrorContextsMap,
        MaybeContext, Components, Indent, !First, !PrintedSome,
        !AlreadyPrintedVerbose, !IO),
    do_write_error_msgs(Stream, OptionTable, LimitErrorContextsMap,
        Msgs, !.First, !PrintedSome, !AlreadyPrintedVerbose, !IO).

%---------------------------------------------------------------------------%

:- pred write_msg_components(io.text_output_stream::in, option_table::in,
    limit_error_contexts_map::in, maybe(prog_context)::in,
    list(error_msg_component)::in, int::in,
    maybe_treat_as_first::in, maybe_treat_as_first::out,
    maybe_printed_something::in, maybe_printed_something::out,
    already_printed_verbose::in, already_printed_verbose::out,
    io::di, io::uo) is det.

write_msg_components(_Stream, _, _, _, [], _, !First, !PrintedSome,
        !AlreadyPrintedVerbose, !IO).
write_msg_components(Stream, OptionTable, LimitErrorContextsMap, MaybeContext,
        [Component | Components], Indent, !First, !PrintedSome,
        !AlreadyPrintedVerbose, !IO) :-
    (
        Component = always(Pieces),
        do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap,
            MaybeContext, !.First, Indent, Pieces, !IO),
        !:First = do_not_treat_as_first,
        !:PrintedSome = printed_something
    ;
        Component = option_is_set(Option, MatchValue, EmbeddedComponents),
        getopt.lookup_bool_option(OptionTable, Option, OptionValue),
        ( if OptionValue = MatchValue then
            write_msg_components(Stream, OptionTable, LimitErrorContextsMap,
                MaybeContext, EmbeddedComponents, Indent, !First, !PrintedSome,
                !AlreadyPrintedVerbose, !IO)
        else
            true
        )
    ;
        Component = verbose_only(AlwaysOrOnce, Pieces),
        getopt.lookup_bool_option(OptionTable, verbose_errors, VerboseErrors),
        (
            VerboseErrors = yes,
            (
                AlwaysOrOnce = verbose_always,
                do_write_error_pieces(Stream, OptionTable,
                    LimitErrorContextsMap, MaybeContext,
                    !.First, Indent, Pieces, !IO),
                !:First = do_not_treat_as_first,
                !:PrintedSome = printed_something
            ;
                AlwaysOrOnce = verbose_once,
                ( if set.contains(!.AlreadyPrintedVerbose, Pieces) then
                    true
                else
                    do_write_error_pieces(Stream, OptionTable,
                        LimitErrorContextsMap, MaybeContext,
                        !.First, Indent, Pieces, !IO),
                    !:First = do_not_treat_as_first,
                    !:PrintedSome = printed_something,
                    set.insert(Pieces, !AlreadyPrintedVerbose)
                )
            )
        ;
            VerboseErrors = no,
            set_extra_error_info(some_extra_error_info, !IO)
        )
    ;
        Component = verbose_and_nonverbose(VerbosePieces, NonVerbosePieces),
        getopt.lookup_bool_option(OptionTable, verbose_errors, VerboseErrors),
        (
            VerboseErrors = yes,
            do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap,
                MaybeContext, !.First, Indent, VerbosePieces, !IO)
        ;
            VerboseErrors = no,
            do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap,
                MaybeContext, !.First, Indent, NonVerbosePieces, !IO),
            set_extra_error_info(some_extra_error_info, !IO)
        ),
        !:First = do_not_treat_as_first,
        !:PrintedSome = printed_something
    ;
        Component = print_anything(Anything),
        print_anything(Anything, !IO),
        !:First = do_not_treat_as_first,
        !:PrintedSome = printed_something
    ),
    write_msg_components(Stream, OptionTable, LimitErrorContextsMap,
        MaybeContext, Components, Indent, !First, !PrintedSome,
        !AlreadyPrintedVerbose, !IO).

%---------------------------------------------------------------------------%

write_error_pieces_plain(Globals, Pieces, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_pieces_plain(Stream, Globals, Pieces, !IO).

write_error_pieces_plain(Stream, Globals, Pieces, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap,
        no, treat_as_first, 0, Pieces, !IO).

%---------------------------------------------------------------------------%

write_error_pieces(Globals, Context, Indent, Pieces, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_pieces(Stream, Globals, Context, Indent, Pieces, !IO).

write_error_pieces(Stream, Globals, Context, Indent, Pieces, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap,
        yes(Context), treat_as_first, Indent, Pieces, !IO).

%---------------------%

write_error_pieces_maybe_with_context(Globals, MaybeContext,
        Indent, Pieces, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_pieces_maybe_with_context(Stream, Globals, MaybeContext,
        Indent, Pieces, !IO).

write_error_pieces_maybe_with_context(Stream, Globals, MaybeContext, Indent,
        Pieces, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap,
        MaybeContext, treat_as_first, Indent, Pieces, !IO).

%---------------------------------------------------------------------------%

:- pred do_write_error_pieces(io.text_output_stream::in, option_table::in,
    limit_error_contexts_map::in, maybe(prog_context)::in,
    maybe_treat_as_first::in, int::in, list(format_piece)::in,
    io::di, io::uo) is det.

do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap, MaybeContext,
        TreatAsFirst, FixedIndent, Pieces, !IO) :-
    getopt.lookup_maybe_int_option(OptionTable, max_error_line_width,
        MaybeMaxWidth),
    (
        MaybeContext = yes(Context),
        FileName = term_context.context_file(Context),
        LineNumber = term_context.context_line(Context),
        ( if
            (
                map.search(LimitErrorContextsMap, FileName, LineNumberRanges),
                line_number_is_in_a_range(LineNumberRanges, LineNumber) = no
            ;
                % The entry for the empty filename applies to all files.
                map.search(LimitErrorContextsMap, "", LineNumberRanges),
                line_number_is_in_a_range(LineNumberRanges, LineNumber) = no
            )
        then
            set_some_errors_were_context_limited(
                some_errors_were_context_limited, !IO),
            MaybeContextStr = no
        else
            context_to_string(Context, ContextStr0),
            MaybeContextStr = yes(ContextStr0)
        )
    ;
        MaybeContext = no,
        MaybeContextStr = yes("")
    ),
    (
        MaybeContextStr = no
        % Suppress the printing of the error pieces.
    ;
        MaybeContextStr = yes(ContextStr),
        (
            Pieces = []
            % There are no error pieces to print. Don't print the context
            % at the start of a line followed by nothing.
            %
            % This can happen if e.g. the original error_msg_component was
            % verbose_and_nonverbose(SomePieces, []), and this compiler
            % invocation is not printing verbose errors.
        ;
            Pieces = [_ | _],
            convert_pieces_to_paragraphs(Pieces, Paragraphs),
            string.pad_left("", ' ', FixedIndent, FixedIndentStr),
            PrefixStr = ContextStr ++ FixedIndentStr,
            PrefixLen = string.count_code_points(PrefixStr),
            (
                MaybeMaxWidth = yes(MaxWidth),
                AvailLen = MaxWidth - PrefixLen,
                MaybeAvailLen = yes(AvailLen)
            ;
                MaybeMaxWidth = no,
                MaybeAvailLen = no
            ),
            FirstIndent = (if TreatAsFirst = treat_as_first then 0 else 1),
            divide_paragraphs_into_lines(MaybeAvailLen, TreatAsFirst,
                FirstIndent, Paragraphs, Lines0),
            try_to_join_lp_to_rp_lines(Lines0, Lines),
            trace [compile_time(flag("debug_try_join_lp_to_rp")), io(!TIO)] (
                io.stderr_stream(StdErr, !TIO),
                io.write_string(StdErr, "START\n", !TIO),
                list.foldl(io.write_line(StdErr), Paragraphs, !TIO),
                list.foldl(io.write_line(StdErr), Lines0, !TIO),
                io.write_string(StdErr, "JOINED\n", !TIO),
                list.foldl(io.write_line(StdErr), Lines, !TIO),
                io.write_string(StdErr, "END\n", !TIO)
            ),
            write_msg_lines(Stream, PrefixStr, Lines, !IO)
        )
    ).

:- func line_number_is_in_a_range(list(line_number_range), int) = bool.

line_number_is_in_a_range([], _) = no.
line_number_is_in_a_range([Range | Ranges], LineNumber) = IsInARange :-
    Range = line_number_range(MaybeMin, MaybeMax),
    ( if
        (
            MaybeMin = no
        ;
            MaybeMin = yes(Min),
            Min =< LineNumber
        ),
        (
            MaybeMax = no
        ;
            MaybeMax = yes(Max),
            LineNumber =< Max
        )
    then
        IsInARange = yes
    else
        IsInARange = line_number_is_in_a_range(Ranges, LineNumber)
    ).

%---------------------%

:- pred write_msg_lines(io.text_output_stream::in, string::in,
    list(error_line)::in, io::di, io::uo) is det.

write_msg_lines(_Stream, _, [], !IO).
write_msg_lines(Stream, PrefixStr, [Line | Lines], !IO) :-
    write_msg_line(Stream, PrefixStr, Line, !IO),
    write_msg_lines(Stream, PrefixStr, Lines, !IO).

:- pred write_msg_line(io.text_output_stream::in, string::in, error_line::in,
    io::di, io::uo) is det.

write_msg_line(Stream, PrefixStr, Line, !IO) :-
    Line = error_line(_MaybeAvail, LineIndent, LineWordsStr, _LineWordsLen,
        _LineParen),
    ( if LineWordsStr = "" then
        % Don't bother to print out indents that are followed by nothing.
        io.format(Stream, "%s\n", [s(PrefixStr)], !IO)
    else
        IndentStr = indent2_string(LineIndent),
        % If ContextStr is non-empty, it will end with a space,
        % which guarantees that PrefixStr, which is ContextStr possibly with
        % some indentation added, will be separated from LineWords.
        io.format(Stream, "%s%s%s\n",
            [s(PrefixStr), s(IndentStr), s(LineWordsStr)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% Convert components to paragraphs.
%

:- type paragraph
    --->    paragraph(
                % The list of words to print in the paragraph.
                % It should not be empty.
                list(string),

                % The number of blank lines to print after the paragraph.
                int,

                % The indent delta to apply for the next paragraph.
                int,

                % See the documentation of the line_paren field
                % in the error_line type. It has the same meaning here,
                % except it applies only to the last line of the paragraph.
                paren_status
            ).

:- type paren_status
    --->    paren_none
    ;       paren_lp_end    % This paragraph/line ends with a left paren.
    ;       paren_end_rp.   % Next paragraph/line starts with a right paren.

:- pred convert_pieces_to_paragraphs(list(format_piece)::in,
    list(paragraph)::out) is det.

convert_pieces_to_paragraphs(Pieces, Paras) :-
    convert_pieces_to_paragraphs_acc(first_in_msg, Pieces,
        [], cord.empty, ParasCord),
    Paras = cord.list(ParasCord).

:- type word
    --->    plain_word(string)
    ;       prefix_word(string)
    ;       suffix_word(string)
    ;       lower_next_word.

:- pred convert_pieces_to_paragraphs_acc(maybe_first_in_msg::in,
    list(format_piece)::in, list(word)::in,
    cord(paragraph)::in, cord(paragraph)::out) is det.

convert_pieces_to_paragraphs_acc(_, [], RevWords0, !Paras) :-
    Strings = rev_words_to_strings(RevWords0),
    add_paragraph(paragraph(Strings, 0, 0, paren_none), !Paras).
convert_pieces_to_paragraphs_acc(FirstInMsg, [Piece | Pieces],
        RevWords0, !Paras) :-
    (
        Piece = words(WordsStr),
        break_into_words(WordsStr, RevWords0, RevWords1)
    ;
        Piece = words_quote(WordsStr),
        break_into_words(add_quotes(WordsStr), RevWords0, RevWords1)
    ;
        Piece = fixed(Word),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Piece = quote(Word),
        RevWords1 = [plain_word(add_quotes(Word)) | RevWords0]
    ;
        Piece = int_fixed(Int),
        RevWords1 = [plain_word(int_to_string(Int)) | RevWords0]
    ;
        Piece = int_name(Int),
        RevWords1 = [plain_word(int_name_str(Int)) | RevWords0]
    ;
        Piece = nth_fixed(Int),
        RevWords1 = [plain_word(nth_fixed_str(Int)) | RevWords0]
    ;
        Piece = lower_case_next_if_not_first,
        (
            FirstInMsg = first_in_msg,
            RevWords1 = RevWords0
        ;
            FirstInMsg = not_first_in_msg,
            RevWords1 = [lower_next_word | RevWords0]
        )
    ;
        Piece = prefix(Word),
        RevWords1 = [prefix_word(Word) | RevWords0]
    ;
        Piece = suffix(Word),
        RevWords1 = [suffix_word(Word) | RevWords0]
    ;
        (
            Piece = qual_sym_name(SymName)
        ;
            Piece = unqual_sym_name(SymName0),
            SymName = unqualified(unqualify_name(SymName0))
        ),
        Word = sym_name_to_word(SymName),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Piece = name_arity(NameAndArity),
        Word = name_arity_to_word(NameAndArity),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        (
            Piece = qual_sym_name_arity(SymNameAndArity)
        ;
            Piece = unqual_sym_name_arity(SymNameAndArity0),
            SymNameAndArity0 = sym_name_arity(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0)),
            SymNameAndArity = sym_name_arity(SymName, Arity)
        ),
        Word = sym_name_arity_to_word(SymNameAndArity),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        (
            Piece = qual_pf_sym_name_pred_form_arity(PFSymNameArity)
        ;
            Piece = unqual_pf_sym_name_pred_form_arity(PFSymNameArity0),
            PFSymNameArity0 = pf_sym_name_arity(PF, SymName0, PredFormArity),
            SymName = unqualified(unqualify_name(SymName0)),
            PFSymNameArity = pf_sym_name_arity(PF, SymName, PredFormArity)
        ),
        WordsStr = pf_sym_name_pred_form_arity_to_string(PFSymNameArity),
        break_into_words(WordsStr, RevWords0, RevWords1)
    ;
        (
            Piece = qual_pf_sym_name_user_arity(PFSymNameArity)
        ;
            Piece = unqual_pf_sym_name_user_arity(PFSymNameArity0),
            PFSymNameArity0 = pred_pf_name_arity(PF, SymName0, UserArity),
            SymName = unqualified(unqualify_name(SymName0)),
            PFSymNameArity = pred_pf_name_arity(PF, SymName, UserArity)
        ),
        WordsStr = pf_sym_name_user_arity_to_string(PFSymNameArity),
        break_into_words(WordsStr, RevWords0, RevWords1)
    ;
        (
            Piece = qual_cons_id_and_maybe_arity(ConsId0),
            strip_builtin_qualifier_from_cons_id(ConsId0, ConsId)
        ;
            Piece = unqual_cons_id_and_maybe_arity(ConsId0),
            strip_module_qualifier_from_cons_id(ConsId0, ConsId)
        ),
        Word = maybe_quoted_cons_id_and_arity_to_string(ConsId),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        (
            Piece = qual_type_ctor(TypeCtor),
            TypeCtor = type_ctor(SymName, Arity)
        ;
            Piece = unqual_type_ctor(TypeCtor),
            TypeCtor = type_ctor(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0))
        ;
            Piece = qual_inst_ctor(InstCtor),
            InstCtor = inst_ctor(SymName, Arity)
        ;
            Piece = unqual_inst_ctor(InstCtor),
            InstCtor = inst_ctor(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0))
        ;
            Piece = qual_mode_ctor(ModeCtor),
            ModeCtor = mode_ctor(SymName, Arity)
        ;
            Piece = unqual_mode_ctor(ModeCtor),
            ModeCtor = mode_ctor(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0))
        ;
            Piece = qual_class_id(ClassId),
            ClassId = class_id(SymName, Arity)
        ;
            Piece = unqual_class_id(ClassId),
            ClassId = class_id(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0))
        ),
        SymNameAndArity = sym_name_arity(SymName, Arity),
        Word = sym_name_arity_to_word(SymNameAndArity),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Piece = qual_top_ctor_of_type(Type),
        type_to_ctor_det(Type, TypeCtor),
        TypeCtor = type_ctor(TypeCtorName, TypeCtorArity),
        SymNameArity = sym_name_arity(TypeCtorName, TypeCtorArity),
        Word = sym_name_arity_to_word(SymNameArity),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Piece = p_or_f(PredOrFunc),
        Word = pred_or_func_to_string(PredOrFunc),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Piece = purity_desc(Purity),
        Word = purity_to_string(Purity),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Piece = a_purity_desc(Purity),
        Word = a_purity_to_string(Purity),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Piece = decl(DeclName),
        Word = add_quotes(":- " ++ DeclName),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Piece = pragma_decl(PragmaName),
        Word = add_quotes(":- pragma " ++ PragmaName),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Piece = nl,
        Strings = rev_words_to_strings(RevWords0),
        add_paragraph(paragraph(Strings, 0, 0, paren_none), !Paras),
        RevWords1 = []
    ;
        Piece = nl_indent_delta(IndentDelta),
        Strings = rev_words_to_strings(RevWords0),
        add_paragraph(paragraph(Strings, 0, IndentDelta, paren_none), !Paras),
        RevWords1 = []
    ;
        Piece = blank_line,
        Strings = rev_words_to_strings(RevWords0),
        add_paragraph(paragraph(Strings, 1, 0, paren_none), !Paras),
        RevWords1 = []
    ;
        Piece = left_paren_maybe_nl_inc(LP, LPWordKind),
        (
            LPWordKind = lp_plain,
            LPWord = plain_word(LP)
        ;
            LPWordKind = lp_suffix,
            LPWord = suffix_word(LP)
        ),
        Strings = rev_words_to_strings([LPWord | RevWords0]),
        add_paragraph(paragraph(Strings, 0, 1, paren_lp_end), !Paras),
        RevWords1 = []
    ;
        Piece = maybe_nl_dec_right_paren(RP, RPWordKind),
        Strings = rev_words_to_strings(RevWords0),
        add_paragraph(paragraph(Strings, 0, -1, paren_end_rp), !Paras),
        (
            RPWordKind = rp_plain,
            RPWord = plain_word(RP)
        ;
            RPWordKind = rp_prefix,
            RPWord = prefix_word(RP)
        ),
        RevWords1 = [RPWord]
    ;
        ( Piece = invis_order_default_start(_)
        ; Piece = invis_order_default_end(_)
        ; Piece = treat_next_as_first
        ),
        RevWords1 = RevWords0
    ),
    first_in_msg_after_piece(Piece, FirstInMsg, TailFirstInMsg),
    convert_pieces_to_paragraphs_acc(TailFirstInMsg, Pieces,
        RevWords1, !Paras).

:- pred add_paragraph(paragraph::in, cord(paragraph)::in, cord(paragraph)::out)
    is det.

add_paragraph(Para, !Paras) :-
    Para = paragraph(Strings, NumBlankLines, IndentDelta, ParaParen),
    % Do not add no-op paragraphs to the cord.
    ( if
        Strings = [],
        NumBlankLines = 0,
        IndentDelta = 0,
        ParaParen = paren_none
    then
        true
    else
        !:Paras = snoc(!.Paras, Para)
    ).

:- type plain_or_prefix
    --->    plain(string)
    ;       prefix(string)
    ;       lower_next.

:- func rev_words_to_strings(list(word)) = list(string).

rev_words_to_strings(RevWords) = Strings :-
    PorPs = list.reverse(rev_words_to_rev_plain_or_prefix(RevWords)),
    Strings = join_prefixes(PorPs).

:- func rev_words_to_rev_plain_or_prefix(list(word)) = list(plain_or_prefix).

rev_words_to_rev_plain_or_prefix([]) = [].
rev_words_to_rev_plain_or_prefix([Word | Words]) = PorPs :-
    (
        Word = plain_word(String),
        PorPs = [plain(String) | rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = lower_next_word,
        PorPs = [lower_next | rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = prefix_word(Prefix),
        PorPs = [prefix(Prefix) | rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = suffix_word(Suffix),
        (
            Words = [],
            PorPs = [plain(Suffix)]
        ;
            Words = [plain_word(String) | Tail],
            PorPs = [plain(String ++ Suffix)
                | rev_words_to_rev_plain_or_prefix(Tail)]
        ;
            Words = [lower_next_word | Tail],
            % Convert the lower_next_word/suffix combination into just the
            % suffix after lowercasing the suffix (which will probably have
            % no effect, since the initial character of a suffix is usually
            % not a letter).
            NewWords = [suffix_word(uncapitalize_first(Suffix)) | Tail],
            PorPs = rev_words_to_rev_plain_or_prefix(NewWords)
        ;
            Words = [prefix_word(Prefix) | Tail],
            % Convert the prefix/suffix combination into a plain word.
            % We could convert it into a prefix, but since prefix/suffix
            % combinations shouldn't come up at all, what we do here probably
            % doesn't matter.
            PorPs = [plain(Prefix ++ Suffix)
                | rev_words_to_rev_plain_or_prefix(Tail)]
        ;
            Words = [suffix_word(MoreSuffix) | Tail],
            PorPs = rev_words_to_rev_plain_or_prefix(
                [suffix_word(MoreSuffix ++ Suffix) | Tail])
        )
    ).

:- func join_prefixes(list(plain_or_prefix)) = list(string).

join_prefixes([]) = [].
join_prefixes([Head | Tail]) = Strings :-
    TailStrings = join_prefixes(Tail),
    (
        Head = plain(String),
        Strings = [String | TailStrings]
    ;
        Head = prefix(Prefix),
        (
            TailStrings = [First | Later],
            Strings = [Prefix ++ First | Later]
        ;
            TailStrings = [],
            Strings = [Prefix | TailStrings]
        )
    ;
        Head = lower_next,
        (
            TailStrings = [],
            Strings = TailStrings
        ;
            TailStrings = [FirstTailString | LaterTailStrings],
            Strings = [uncapitalize_first(FirstTailString) | LaterTailStrings]
        )
    ).

:- pred break_into_words(string::in, list(word)::in, list(word)::out) is det.

break_into_words(String, Words0, Words) :-
    break_into_words_from(String, 0, Words0, Words).

:- pred break_into_words_from(string::in, int::in, list(word)::in,
    list(word)::out) is det.

break_into_words_from(String, Cur, Words0, Words) :-
    ( if find_word_start(String, Cur, Start) then
        find_word_end(String, Start, End),
        string.between(String, Start, End, WordStr),
        Words1 = [plain_word(WordStr) | Words0],
        break_into_words_from(String, End, Words1, Words)
    else
        Words = Words0
    ).

:- pred find_word_start(string::in, int::in, int::out) is semidet.

find_word_start(String, Cur, WordStart) :-
    string.unsafe_index_next(String, Cur, Next, Char),
    ( if char.is_whitespace(Char) then
        find_word_start(String, Next, WordStart)
    else
        WordStart = Cur
    ).

:- pred find_word_end(string::in, int::in, int::out) is det.

find_word_end(String, Cur, WordEnd) :-
    ( if string.unsafe_index_next(String, Cur, Next, Char) then
        ( if char.is_whitespace(Char) then
            WordEnd = Cur
        else
            find_word_end(String, Next, WordEnd)
        )
    else
        WordEnd = Cur
    ).

%---------------------------------------------------------------------------%
%
% Divide paragraphs into lines.
%

:- type error_line
    --->    error_line(
                % In the usual case, this will be yes(AvailLen) where
                % AvailLen is the Total space available on the line
                % after the context and the fixed indent.
                %
                % The absence of an integer here means that there is
                % no limit on the lengths of lines.
                maybe_avail_len     :: maybe(int),

                % Indent level of the line; multiply by indent2_increment
                % to get the number of spaces this turns into.
                line_indent_level   :: int,

                % The words on the line as a single string, with one space
                % between each pair of words.
                line_words_str      :: string,

                % The length of the line_words_str field.
                %
                % This field is meaningful only if maybe_avail_len is yes(...).
                line_words_len      :: int,

                % If this field is paren_none, this a normal line.
                % If this field is paren_lp_end, this line ends a left
                % parenthesis.
                % If this field is paren_end_rp, the *next* line *starts*
                % with a right parenthesis.
                %
                % We use these fields to try to put everything
                %
                % - in a paren_lp_end line,
                % - in zero or more paren_none lines,
                % - in a paren_end_rp line, and
                % - the very next line (which starts with the rp)
                %
                % into a single line, if there is room. (Note that the code
                % that creates a paren_end_rp line will also ensure that
                % there *will be* a next line.)
                %
                % It is ok for some of the lines to be squashed together
                % to result from earlier squash operations, on inner
                % parentheses.
                line_paren          :: paren_status
            ).

    % Groups the words in the given paragraphs into lines. The first line
    % can have up to Max characters on it; the later lines (if any) up
    % to Max-2 characters.
    %
    % If MaybeAvailLen is `no', handle it as if AvailLen were infinity,
    % which means putting everything in each paragraph on one line.
    %
    % The given list of paragraphs should be nonempty, since we always return
    % at least one line.
    %
:- pred divide_paragraphs_into_lines(maybe(int)::in, maybe_treat_as_first::in,
    int::in, list(paragraph)::in, list(error_line)::out) is det.

divide_paragraphs_into_lines(MaybeAvailLen, TreatAsFirst, CurIndent, Paras,
        Lines) :-
    (
        Paras = [],
        Lines = []
    ;
        Paras = [FirstPara | LaterParas],
        FirstPara = paragraph(FirstParaWords, NumBlankLines, FirstIndentDelta,
            ParaParen),
        (
            TreatAsFirst = treat_as_first,
            RestIndent = CurIndent + 1
        ;
            TreatAsFirst = do_not_treat_as_first,
            RestIndent = CurIndent
        ),
        NextIndent = RestIndent + FirstIndentDelta,

        BlankLine = error_line(MaybeAvailLen, CurIndent, "", 0, paren_none),
        list.duplicate(NumBlankLines, BlankLine, FirstParaBlankLines),
        (
            FirstParaWords = [],
            NextTreatAsFirst = TreatAsFirst,
            FirstParaLines = []
        ;
            FirstParaWords = [FirstWord | LaterWords],
            NextTreatAsFirst = do_not_treat_as_first,
            (
                MaybeAvailLen = yes(AvailLen),
                get_line_of_words(AvailLen, FirstWord, LaterWords, CurIndent,
                    LineWordsLen, LineWords, RestWords),
                LineWordsStr = line_words_to_str(LineWords), 
                (
                    RestWords = [],
                    CurLine = error_line(MaybeAvailLen, CurIndent,
                        LineWordsStr, LineWordsLen, ParaParen),
                    FirstParaLines = [CurLine]
                ;
                    RestWords = [FirstRestWord | LaterRestWords],
                    CurLine = error_line(MaybeAvailLen, CurIndent,
                        LineWordsStr, LineWordsLen, paren_none),
                    group_nonfirst_line_words(AvailLen,
                        FirstRestWord, LaterRestWords, RestIndent, ParaParen,
                        FirstParaRestLines),
                    FirstParaLines = [CurLine | FirstParaRestLines]
                )
            ;
                MaybeAvailLen = no,
                FirstParaLines = [error_line(MaybeAvailLen, CurIndent,
                    line_words_to_str(FirstParaWords), -1, ParaParen)]
            )
        ),
        divide_paragraphs_into_lines(MaybeAvailLen, NextTreatAsFirst,
            NextIndent, LaterParas, LaterParaLines),
        Lines = FirstParaLines ++ FirstParaBlankLines ++ LaterParaLines
    ).

:- pred group_nonfirst_line_words(int::in, string::in, list(string)::in,
    int::in, paren_status::in, list(error_line)::out) is det.

group_nonfirst_line_words(AvailLen, FirstWord, LaterWords,
        Indent, LastParen, Lines) :-
    get_line_of_words(AvailLen, FirstWord, LaterWords, Indent,
        LineWordsLen, LineWords, RestWords),
    LineWordsStr = line_words_to_str(LineWords),
    (
        RestWords = [],
        Line = error_line(yes(AvailLen), Indent, LineWordsStr, LineWordsLen,
            LastParen),
        Lines = [Line]
    ;
        RestWords = [FirstRestWord | LaterRestWords],
        Line = error_line(yes(AvailLen), Indent, LineWordsStr, LineWordsLen,
            paren_none),
        group_nonfirst_line_words(AvailLen, FirstRestWord, LaterRestWords,
            Indent, LastParen, RestLines),
        Lines = [Line | RestLines]
    ).

:- pred get_line_of_words(int::in, string::in, list(string)::in,
    int::in, int::out, list(string)::out, list(string)::out) is det.

get_line_of_words(AvailLen, FirstWord, LaterWords, Indent, LineWordsLen,
        LineWords, RestWords) :-
    string.count_code_points(FirstWord, FirstWordLen),
    AvailLeft = AvailLen - Indent * indent2_increment,
    get_later_words(AvailLeft, LaterWords, FirstWordLen, LineWordsLen,
        cord.singleton(FirstWord), LineWordsCord, RestWords),
    LineWords = cord.list(LineWordsCord).

:- pred get_later_words(int::in, list(string)::in, int::in, int::out,
    cord(string)::in, cord(string)::out, list(string)::out) is det.

get_later_words(_, [], CurLen, FinalLen, LineWords, LineWords, []) :-
    FinalLen = CurLen.
get_later_words(Avail, [Word | Words], CurLen, FinalLen,
        LineWords0, LineWords, RestWords) :-
    string.count_code_points(Word, WordLen),
    NextLen = CurLen + 1 + WordLen,
    ( if NextLen =< Avail then
        cord.snoc(Word, LineWords0, LineWords1),
        get_later_words(Avail, Words, NextLen, FinalLen,
            LineWords1, LineWords, RestWords)
    else
        FinalLen = CurLen,
        LineWords = LineWords0,
        RestWords = [Word | Words]
    ).

:- func line_words_to_str(list(string)) = string.

line_words_to_str(LineWords) = string.join_list(" ", LineWords).

%---------------------------------------------------------------------------%

    % Look for sequences of
    %
    % - a paren_lp_end line,
    % - zero or more paren_none lines,
    % - a paren_end_rp line, and
    % - the very next line (which starts with the rp),
    %
    % and join them up into a single line, if there is room.
    %
    % This predicate is the top level loop. All calls to it, including
    % recursive calls, are outside of all parentheses.
    %
:- pred try_to_join_lp_to_rp_lines(list(error_line)::in,
    list(error_line)::out) is det.

try_to_join_lp_to_rp_lines([], []).
try_to_join_lp_to_rp_lines([HeadLine0 | TailLines0], Lines) :-
    HeadLine0 = error_line(_MaybeAvailLen, _HeadIndent, _HeadLineWords,
        _HeadLineWordsLen, HeadParen),
    (
        ( HeadParen = paren_none
        ; HeadParen = paren_end_rp  % This is an unbalanced right paren.
        ),
        try_to_join_lp_to_rp_lines(TailLines0, TailLines),
        Lines = [HeadLine0 | TailLines]
    ;
        HeadParen = paren_lp_end,
        % We got the first line in the pattern we are looking for.
        % look for the rest, and act on it, if possible.
        ( if
            find_matching_rp_and_maybe_join(HeadLine0, TailLines0,
                ReplacementLines, LeftOverLines0),
            ReplacementLines = [FirstReplacementLine | _],
            FirstReplacementLine \= HeadLine0
        then
            % If we could optimize the pattern starting at HeadLine0,
            % then there is a small chance that the replacement *also* ends
            % with a paren_lp_end line. If it does, then we want to optimize
            % the pattern starting at *that* line as well.
            Lines1 = ReplacementLines ++ LeftOverLines0,
            try_to_join_lp_to_rp_lines(Lines1, Lines)
        else
            % If we could not optimize the pattern starting at HeadLine0,
            % don't process that line again, since that would lead to
            % an infinite loop.
            try_to_join_lp_to_rp_lines(TailLines0, TailLines),
            Lines = [HeadLine0 | TailLines]
        )
    ).

    % Given a line with paren_lp_end, look for the rest of the pattern
    % documented in the comment on try_to_join_lp_to_rp_lines, and
    % join the lines involved, if this is possible.
    %
:- pred find_matching_rp_and_maybe_join(error_line::in,
    list(error_line)::in, list(error_line)::out, list(error_line)::out)
    is semidet.

find_matching_rp_and_maybe_join(LPLine, TailLines0, ReplacementLines,
        LeftOverLines) :-
    LPLine = error_line(MaybeAvailLen, LPIndent, LPLineWordsStr,
        LPLineWordsLen, LPParen),
    expect(unify(LPParen, paren_lp_end), $pred, "LPParen != paren_lp_end"),
    ( if
        find_matching_rp(TailLines0, cord.init, MidLinesCord, 0, MidLinesLen,
            RPLine, LeftOverLinesPrime)
    then
        RPLine = error_line(_, _RPIndent, RPLineWordsStr, RPLineWordsLen,
            RPParen),
        MidLines = cord.list(MidLinesCord),
        list.length(MidLines, NumMidLines),
        MidLineSpaces = (if NumMidLines = 0 then 0 else NumMidLines - 1),
        TotalLpRpLen =
            LPLineWordsLen + MidLinesLen + MidLineSpaces + RPLineWordsLen,
        ChunkLines = [LPLine | MidLines] ++ [RPLine],
        ( if
            (
                MaybeAvailLen = no
            ;
                MaybeAvailLen = yes(AvailLen),
                LPIndent * indent2_increment + TotalLpRpLen =< AvailLen
            )
        then
            % We insert spaces
            % - between the middle lines, but
            % - not after the ( in LPLine,
            % - nor before the ) in RPLine.
            MidLineStrs = list.map((func(L) = L ^ line_words_str), MidLines),
            MidSpaceLinesStr = string.join_list(" ", MidLineStrs),
            ReplacementLineStr =
                LPLineWordsStr ++ MidSpaceLinesStr ++ RPLineWordsStr,
            string.count_code_points(ReplacementLineStr, ReplacementLineLen),
            expect(unify(TotalLpRpLen, ReplacementLineLen), $pred,
                "TotalLpRpLen != ReplacementLineLen"),
            ReplacementLine = error_line(MaybeAvailLen, LPIndent,
                ReplacementLineStr, TotalLpRpLen, RPParen),
            ReplacementLines = [ReplacementLine]
        else
            ReplacementLines = ChunkLines
        ),
        LeftOverLines = LeftOverLinesPrime
    else
        % We can't find the rest of the pattern so we can't optimize anything.
        fail
    ).

    % find_matching_rp(Lines0, !MidLinesCord, !MidLinesLen, RPLine,
    %   LeftOverLines):
    %
    % Look for the part of the pattern after the initial paren_lp_end line,
    % which consists of
    %
    % - zero or more paren_none lines,
    % - a paren_end_rp line (return both of these in !:MidLinesCord), and
    % - the very next line, which starts with the rp (return this in RPLine).
    %
    % LeftOverLines will be the lines following these.
    %
    % Also, return in !:MidLinesLen by the total length of the lines
    % in !:MidLinesCord.
    % 
:- pred find_matching_rp(list(error_line)::in,
    cord(error_line)::in, cord(error_line)::out,
    int::in, int::out, error_line::out, list(error_line)::out) is semidet.

find_matching_rp([], !MidLinesCord, !MidLinesLen, _, _) :-
    fail.
find_matching_rp([HeadLine0 | TailLines0], !MidLinesCord, !MidLinesLen,
        RPLine, LeftOverLines) :-
    HeadLine0 = error_line(_HeadMaybeAvailLen, _HeadIndent, _HeadLineWordsStr,
        HeadLineWordsLen, HeadParen),
    (
        HeadParen = paren_none,
        cord.snoc(HeadLine0, !MidLinesCord),
        !:MidLinesLen = !.MidLinesLen + HeadLineWordsLen,
        find_matching_rp(TailLines0, !MidLinesCord, !MidLinesLen, RPLine,
            LeftOverLines)
    ;
        HeadParen = paren_end_rp,
        % The right parenthesis is at the start of the *next* line.
        (
            TailLines0 = [],
            % There is no right paren; the original left paren is unbalanced.
            fail
        ;
            TailLines0 = [RPLine | TailTailLines0],
            cord.snoc(HeadLine0, !MidLinesCord),
            !:MidLinesLen = !.MidLinesLen + HeadLineWordsLen,
            LeftOverLines = TailTailLines0
        )
    ;
        HeadParen = paren_lp_end,
        find_matching_rp_and_maybe_join(HeadLine0, TailLines0,
            ReplacementLines, AfterRpLines),
        (
            ReplacementLines = [],
            % Getting here means that the text of _HeadLineWordsStr
            % has disappeared, which is a bug.
            unexpected($pred, "ReplacementLines = []")
        ;
            ReplacementLines = [HeadReplacementLine | TailReplacementLines],
            (
                TailReplacementLines = [],
                % We replaced the inner pattern with a single line.
                % If we replaced a line with itself, making the recursive call
                % in the else arm would lead to an infinite loop.
                ( if HeadReplacementLine = HeadLine0 then
                    fail
                else
                    % Try to fit this single line into the larger pattern.
                    find_matching_rp([HeadReplacementLine | AfterRpLines],
                        !MidLinesCord, !MidLinesLen, RPLine, LeftOverLines)
                )
            ;
                TailReplacementLines = [_ | _],
                % We couldn't optimize the pattern starting at HeadLine0,
                % which is nested inside the larger pattern our ancestor
                % find_matching_rp_and_maybe_join call was trying to
                % optimize. But if even just the lines in this inner pattern
                % are too long to fit on our caller's available space,
                % then the large pattern that includes those lines *and *more*
                % will also be too large to fit into that same space.
                %
                % NOTE: This assumes that the inner pattern is at least as
                % indented as our caller's paren_lp_end line. As far as I (zs)
                % know, this is true for all our error messages.
                fail
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

error_pieces_to_string(Pieces) =
    error_pieces_to_string_loop(first_in_msg, Pieces).

:- func error_pieces_to_string_loop(maybe_first_in_msg, list(format_piece))
    = string.

error_pieces_to_string_loop(_, []) = "".
error_pieces_to_string_loop(FirstInMsg, [Piece | Pieces]) = Str :-
    first_in_msg_after_piece(Piece, FirstInMsg, TailFirstInMsg),
    TailStr = error_pieces_to_string_loop(TailFirstInMsg, Pieces),
    (
        Piece = words(Words),
        Str = join_string_and_tail(Words, Pieces, TailStr)
    ;
        Piece = words_quote(Words),
        Str = join_string_and_tail(add_quotes(Words), Pieces, TailStr)
    ;
        Piece = fixed(Word),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        Piece = quote(Word),
        Str = join_string_and_tail(add_quotes(Word), Pieces, TailStr)
    ;
        Piece = int_fixed(Int),
        Str = join_string_and_tail(int_to_string(Int), Pieces, TailStr)
    ;
        Piece = int_name(Int),
        Str = join_string_and_tail(int_name_str(Int), Pieces, TailStr)
    ;
        Piece = nth_fixed(Int),
        Str = join_string_and_tail(nth_fixed_str(Int), Pieces, TailStr)
    ;
        Piece = lower_case_next_if_not_first,
        (
            FirstInMsg = first_in_msg,
            Str = TailStr
        ;
            FirstInMsg = not_first_in_msg,
            Str = uncapitalize_first(TailStr)
        )
    ;
        Piece = treat_next_as_first,
        Str = TailStr
    ;
        Piece = prefix(Prefix),
        Str = Prefix ++ TailStr
    ;
        Piece = suffix(Suffix),
        Str = join_string_and_tail(Suffix, Pieces, TailStr)
    ;
        (
            Piece = qual_sym_name(SymName)
        ;
            Piece = unqual_sym_name(SymName0),
            SymName = unqualified(unqualify_name(SymName0))
        ),
        Word = sym_name_to_word(SymName),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        Piece = name_arity(NameAndArity),
        Word = name_arity_to_word(NameAndArity),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        (
            Piece = qual_sym_name_arity(SymNameAndArity)
        ;
            Piece = unqual_sym_name_arity(SymNameAndArity0),
            SymNameAndArity0 = sym_name_arity(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0)),
            SymNameAndArity = sym_name_arity(SymName, Arity)
        ),
        Word = sym_name_arity_to_word(SymNameAndArity),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        (
            Piece = qual_pf_sym_name_pred_form_arity(PFSymNameArity)
        ;
            Piece = unqual_pf_sym_name_pred_form_arity(PFSymNameArity0),
            PFSymNameArity0 = pf_sym_name_arity(PF, SymName0, PredFormArity),
            SymName = unqualified(unqualify_name(SymName0)),
            PFSymNameArity = pf_sym_name_arity(PF, SymName, PredFormArity)
        ),
        Word = pf_sym_name_pred_form_arity_to_string(PFSymNameArity),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        (
            Piece = qual_pf_sym_name_user_arity(PFSymNameArity)
        ;
            Piece = unqual_pf_sym_name_user_arity(PFSymNameArity0),
            PFSymNameArity0 = pred_pf_name_arity(PF, SymName0, UserArity),
            SymName = unqualified(unqualify_name(SymName0)),
            PFSymNameArity = pred_pf_name_arity(PF, SymName, UserArity)
        ),
        Word = pf_sym_name_user_arity_to_string(PFSymNameArity),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        (
            Piece = qual_cons_id_and_maybe_arity(ConsId0),
            strip_builtin_qualifier_from_cons_id(ConsId0, ConsId)
        ;
            Piece = unqual_cons_id_and_maybe_arity(ConsId0),
            strip_module_qualifier_from_cons_id(ConsId0, ConsId)
        ),
        Word = maybe_quoted_cons_id_and_arity_to_string(ConsId),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        (
            Piece = qual_type_ctor(TypeCtor),
            TypeCtor = type_ctor(SymName, Arity)
        ;
            Piece = unqual_type_ctor(TypeCtor),
            TypeCtor = type_ctor(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0))
        ;
            Piece = qual_inst_ctor(InstCtor),
            InstCtor = inst_ctor(SymName, Arity)
        ;
            Piece = unqual_inst_ctor(InstCtor),
            InstCtor = inst_ctor(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0))
        ;
            Piece = qual_mode_ctor(ModeCtor),
            ModeCtor = mode_ctor(SymName, Arity)
        ;
            Piece = unqual_mode_ctor(ModeCtor),
            ModeCtor = mode_ctor(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0))
        ;
            Piece = qual_class_id(ClassId),
            ClassId = class_id(SymName, Arity)
        ;
            Piece = unqual_class_id(ClassId),
            ClassId = class_id(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0))
        ),
        SymNameAndArity = sym_name_arity(SymName, Arity),
        Word = sym_name_arity_to_word(SymNameAndArity),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        Piece = qual_top_ctor_of_type(Type),
        type_to_ctor_det(Type, TypeCtor),
        TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
        SymNameArity = sym_name_arity(TypeCtorSymName, TypeCtorArity),
        Word = sym_name_arity_to_word(SymNameArity),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        Piece = p_or_f(PredOrFunc),
        Word = pred_or_func_to_string(PredOrFunc),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        Piece = purity_desc(Purity),
        Word = purity_to_string(Purity),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        Piece = a_purity_desc(Purity),
        Word = a_purity_to_string(Purity),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        Piece = decl(Decl),
        Word = add_quotes(":- " ++ Decl),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        Piece = pragma_decl(PragmaName),
        Word = add_quotes(":- pragma " ++ PragmaName),
        Str = join_string_and_tail(Word, Pieces, TailStr)
    ;
        Piece = nl,
        Str = "\n" ++ TailStr
    ;
        Piece = nl_indent_delta(_),
        % There is nothing we can do about the indent delta.
        Str = "\n" ++ TailStr
    ;
        Piece = blank_line,
        Str = "\n\n" ++ TailStr
    ;
        Piece = left_paren_maybe_nl_inc(LP, _LPWordKind),
        % There is nothing we can do about the indent delta.
        % _LPWordKind is handled by the call to join_string_and_tail
        % when we process the *previous* piece.
        Str = join_string_and_tail(LP ++ "\n", Pieces, TailStr)
    ;
        Piece = maybe_nl_dec_right_paren(RP, RPWordKind),
        % There is nothing we can do about the indent delta.
        (
            RPWordKind = rp_plain,
            Str = join_string_and_tail("\n" ++ RP, Pieces, TailStr)
        ;
            RPWordKind = rp_prefix,
            Str = "\n" ++ RP ++ TailStr
        )
    ;
        ( Piece = invis_order_default_start(_)
        ; Piece = invis_order_default_end(_)
        ),
        Str = TailStr
    ).

:- func join_string_and_tail(string, list(format_piece), string) = string.

join_string_and_tail(Word, Pieces, TailStr) = Str :-
    ( if TailStr = "" then
        Str = Word
    else
        ( if
            Pieces = [NextPiece | _],
            ( NextPiece = suffix(_)
            ; NextPiece = left_paren_maybe_nl_inc(_, lp_suffix)
            )
        then
            Str = Word ++ TailStr
        else
            Str = Word ++ " " ++ TailStr
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Utility predicates used by both do_write_error_pieces and
% error_pieces_to_string.
%

:- type maybe_first_in_msg
    --->    first_in_msg
    ;       not_first_in_msg.

:- pred first_in_msg_after_piece(format_piece::in,
    maybe_first_in_msg::in, maybe_first_in_msg::out) is det.

first_in_msg_after_piece(Piece, FirstInMsg, TailFirstInMsg) :-
    (
        ( Piece = treat_next_as_first
        ; Piece = blank_line
        ),
        TailFirstInMsg = first_in_msg
    ;
        ( Piece = lower_case_next_if_not_first
        ; Piece = nl
        ; Piece = nl_indent_delta(_)
        ; Piece = invis_order_default_start(_)
        ; Piece = invis_order_default_end(_)
        ),
        TailFirstInMsg = FirstInMsg
    ;
        ( Piece = words(_)
        ; Piece = words_quote(_)
        ; Piece = fixed(_)
        ; Piece = quote(_)
        ; Piece = int_fixed(_)
        ; Piece = int_name(_)
        ; Piece = nth_fixed(_)
        ; Piece = prefix(_)
        ; Piece = suffix(_)
        ; Piece = qual_sym_name(_)
        ; Piece = unqual_sym_name(_)
        ; Piece = name_arity(_)
        ; Piece = qual_sym_name_arity(_)
        ; Piece = unqual_sym_name_arity(_)
        ; Piece = qual_pf_sym_name_pred_form_arity(_)
        ; Piece = unqual_pf_sym_name_pred_form_arity(_)
        ; Piece = qual_pf_sym_name_user_arity(_)
        ; Piece = unqual_pf_sym_name_user_arity(_)
        ; Piece = qual_cons_id_and_maybe_arity(_)
        ; Piece = unqual_cons_id_and_maybe_arity(_)
        ; Piece = qual_type_ctor(_)
        ; Piece = unqual_type_ctor(_)
        ; Piece = qual_inst_ctor(_)
        ; Piece = unqual_inst_ctor(_)
        ; Piece = qual_mode_ctor(_)
        ; Piece = unqual_mode_ctor(_)
        ; Piece = qual_class_id(_)
        ; Piece = unqual_class_id(_)
        ; Piece = qual_top_ctor_of_type(_)
        ; Piece = p_or_f(_)
        ; Piece = purity_desc(_)
        ; Piece = a_purity_desc(_)
        ; Piece = decl(_)
        ; Piece = pragma_decl(_)
        ; Piece = left_paren_maybe_nl_inc(_, _)
        ; Piece = maybe_nl_dec_right_paren(_, _)
        ),
        TailFirstInMsg = not_first_in_msg
    ).

:- func sym_name_to_word(sym_name) = string.

sym_name_to_word(SymName) =
    add_quotes(sym_name_to_string(SymName)).

:- func name_arity_to_word(name_arity) = string.

name_arity_to_word(name_arity(Name, Arity)) =
    add_quotes(Name) ++ "/" ++ int_to_string(Arity).

:- func sym_name_arity_to_word(sym_name_arity) = string.

sym_name_arity_to_word(sym_name_arity(SymName, Arity)) =
    add_quotes(sym_name_to_string(SymName)) ++ "/" ++ int_to_string(Arity).

:- func int_name_str(int) = string.

int_name_str(N) = Str :-
    ( if
        ( N = 0,  StrPrime = "zero"
        ; N = 1,  StrPrime = "one"
        ; N = 2,  StrPrime = "two"
        ; N = 3,  StrPrime = "three"
        ; N = 4,  StrPrime = "four"
        ; N = 5,  StrPrime = "five"
        ; N = 6,  StrPrime = "six"
        ; N = 7,  StrPrime = "seven"
        ; N = 8,  StrPrime = "eight"
        ; N = 9,  StrPrime = "nine"
        ; N = 10, StrPrime = "ten"
        )
    then
        Str = StrPrime
    else
        Str = int_to_string(N)
    ).

:- func nth_fixed_str(int) = string.

nth_fixed_str(N) = Str :-
    ( if
        ( N = 1,  StrPrime = "first"
        ; N = 2,  StrPrime = "second"
        ; N = 3,  StrPrime = "third"
        ; N = 4,  StrPrime = "fourth"
        ; N = 5,  StrPrime = "fifth"
        ; N = 6,  StrPrime = "sixth"
        ; N = 7,  StrPrime = "seventh"
        ; N = 8,  StrPrime = "eighth"
        ; N = 9,  StrPrime = "ninth"
        ; N = 10, StrPrime = "tenth"
        )
    then
        Str = StrPrime
    else
        % We want to print 12th and 13th, not 12nd and 13rd,
        % but 42nd and 43rd instead of 42th and 43th.
        NStr = int_to_string(N),
        LastDigit = N mod 10,
        ( if N > 20, LastDigit = 2 then
            Str = NStr ++ "nd"
        else if N > 20, LastDigit = 3 then
            Str = NStr ++ "rd"
        else
            Str = NStr ++ "th"
        )
    ).

:- func purity_to_string(purity) = string.

purity_to_string(purity_pure) = "pure".
purity_to_string(purity_semipure) = "semipure".
purity_to_string(purity_impure) = "impure".

:- func a_purity_to_string(purity) = string.

a_purity_to_string(purity_pure) = "a pure".
a_purity_to_string(purity_semipure) = "a semipure".
a_purity_to_string(purity_impure) = "an impure".

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

pre_hlds_maybe_write_out_errors(Stream, Verbose, Globals, !Specs, !IO) :-
    % maybe_write_out_errors in hlds_error_util.m is a HLDS version
    % of this predicate. The documentation is in that file.
    (
        Verbose = no
    ;
        Verbose = yes,
        write_error_specs(Stream, Globals, !.Specs, !IO),
        !:Specs = []
    ).

%---------------------------------------------------------------------------%

report_warning(Globals, Context, Indent, Pieces, !IO) :-
    io.output_stream(Stream, !IO),
    report_warning(Stream, Globals, Context, Indent, Pieces, !IO).

report_warning(Stream, Globals, Context, Indent, Pieces, !IO) :-
    record_warning(Globals, !IO),
    write_error_pieces(Stream, Globals, Context, Indent, Pieces, !IO).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type maybe_extra_error_info
    --->    no_extra_error_info
    ;       some_extra_error_info.

:- type context_limited_errors
    --->    no_errors_were_context_limited
    ;       some_errors_were_context_limited.

    % Is there extra information about errors available that could be printed
    % out if `-E' were enabled?
    %
:- mutable(extra_error_info,
    maybe_extra_error_info, no_extra_error_info, ground,
    [untrailed, attach_to_io_state]).

    % Is there extra information about errors available that could be printed
    % if the values of --limit-error-contexts options allowed it?
    %
:- mutable(some_errors_were_context_limited,
    context_limited_errors, no_errors_were_context_limited, ground,
    [untrailed, attach_to_io_state]).

maybe_print_delayed_error_messages(ErrorStream, Globals, !IO) :-
    % Pick up the values of these flags, and then reset them
    % for the next module.
    get_some_errors_were_context_limited(Limited, !IO),
    set_some_errors_were_context_limited(no_errors_were_context_limited, !IO),
    get_extra_error_info(ExtraErrorInfo, !IO),
    set_extra_error_info(no_extra_error_info, !IO),

    % If we suppressed the printing of some errors, then tell the user
    % about this fact, because the absence of any errors being printed
    % during a failing compilation would otherwise be baffling.
    (
        Limited = no_errors_were_context_limited
    ;
        Limited = some_errors_were_context_limited,
        io.write_string(ErrorStream, "Some error messages were suppressed " ++
            "by `--limit-error-contexts' options.\n", !IO),
        io.write_string(ErrorStream, "You can see the suppressed messages " ++
            "if you recompile without these options.\n", !IO)
    ),

    % If we found some errors with verbose-only components, but the user
    % did not enable the `-E' (`--verbose-errors') option, tell them about it.
    (
        ExtraErrorInfo = no_extra_error_info
    ;
        ExtraErrorInfo = some_extra_error_info,
        globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
        (
            VerboseErrors = no,
            io.write_string(ErrorStream,
                "For more information, recompile with `-E'.\n", !IO)
        ;
            VerboseErrors = yes
            % We have already printed the verbose parts of error messages.
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.write_error_spec.
%---------------------------------------------------------------------------%
