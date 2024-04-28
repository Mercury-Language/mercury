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
% inserting blank lines, and by increasing/decreasing the indent level.
%
%---------------------------------------------------------------------------%

:- module parse_tree.write_error_spec.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.indent.
:- import_module libs.options.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % write_error_spec(Stream, Globals, Spec, !IO):
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
:- pred write_error_spec(io.text_output_stream::in, globals::in,
    error_spec::in, io::di, io::uo) is det.
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
:- pred write_error_pieces_plain(io.text_output_stream::in, globals::in,
    list(format_piece)::in, io::di, io::uo) is det.
:- pragma obsolete(pred(write_error_pieces_plain/5)).

    % write_error_pieces(Globals, Context, Indent, Pieces):
    %
    % Display `Pieces' as the error message, with `Context' as a context
    % and indent by `Indent'.
    %
:- pred write_error_pieces(io.text_output_stream::in, globals::in,
    prog_context::in, indent::in, list(format_piece)::in,
    io::di, io::uo) is det.
:- pragma obsolete(pred(write_error_pieces/7)).

:- pred write_error_pieces_maybe_with_context(io.text_output_stream::in,
    globals::in, maybe(prog_context)::in, indent::in, list(format_piece)::in,
    io::di, io::uo) is det.
:- pragma obsolete(pred(write_error_pieces_maybe_with_context/7)).

%---------------------------------------------------------------------------%

:- type error_line.

    % Convert the given list of pieces to an abstract representation
    % of the lines of output it corresponds to. The caller can then
    %
    % - test whether the text in these lines can fit in a given number
    %   of characters, using do_lines_fit_in_n_code_points, and
    % - convert the lines either
    %   - to a string containing a single line of text
    %     using error_lines_to_one_line_string, or
    %   - to a string containing one line of text for each abstract line
    %     using error_lines_to_multi_line_string.
    %

:- func error_pieces_to_std_lines(list(format_piece)) = list(error_line).

    % Succeed if and only if the contents of the given lines, if appended
    % with a space between each pair of successive lines, would fit
    % in the given number of characters.
    %
:- pred do_lines_fit_in_n_code_points(int::in, list(error_line)::in)
    is semidet.

    % Convert the given abstract representation of lines to either
    % a string containing a single line of text, or a string containing
    % one line of text per abstract line.
    %
:- func error_lines_to_one_line_string(list(error_line)) = string.
:- func error_lines_to_multi_line_string(string, list(error_line)) = string.

%---------------------%

    % These two functions do (almost) the same job as write_error_pieces,
    % but returns the resulting string instead of printing it out.
    %
    % error_pieces_to_one_line_string returns the string on one single line,
    % without a final newline. This is good for inputs that are known to be
    % short by construction, as well as in cases where we use its output
    % if it is short enough, but switch to doing something else if it is not.
    %
    % error_pieces_to_multi_line_string preserves both the line structure
    % and the indentation structure of the output that write_error_pieces
    % would generate. The first argument is a prefix that the caller can
    % specify for every one of the lines in the output. The intended use case
    % is the printing of e.g. insts in goals' instmap_deltas in HLDS dumps.
    %
    % NOTE If your intention is to generate a single line of output
    % if the input pieces are short enough, and several lines of output if
    % they are not, then you are better of calling error_pieces_to_std_lines,
    % then do_lines_fit_in_n_code_points, and then (depending on the success
    % or failure of that test) either error_lines_to_one_line_string or
    % error_lines_to_multi_line_string.
    %
:- func error_pieces_to_one_line_string(list(format_piece)) = string.
:- func error_pieces_to_multi_line_string(string, list(format_piece)) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred pre_hlds_maybe_write_out_errors(io.text_output_stream::in,
    bool::in, globals::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

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
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_sort.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
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
:- import_module stack.
:- import_module string.
:- import_module term_context.
:- import_module uint.
:- import_module uint8.

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

write_error_spec(Stream, Globals, Spec, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_spec(Stream, OptionTable, LimitErrorContextsMap,
        Spec, set.init, _, !IO).

%---------------------%

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
        Spec = spec(Id, Severity, _Phase, Context, Pieces),
        MaybeActual = actual_error_severity_opt_table(OptionTable, Severity),
        Msgs1 = [msg(Context, Pieces)]
    ;
        Spec = no_ctxt_spec(Id, Severity, _Phase, Pieces),
        MaybeActual = actual_error_severity_opt_table(OptionTable, Severity),
        Msgs1 = [no_ctxt_msg(Pieces)]
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
                ( HeadMsg = msg(HeadContext, _Pieces)
                ; HeadMsg = simple_msg(HeadContext, _)
                ),
                MaybeHeadContext = yes(HeadContext)
            ;
                HeadMsg = no_ctxt_msg(_),
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
        Msg = msg(SimpleContext, Pieces),
        Components = [always(Pieces)],
        MaybeContext = yes(SimpleContext),
        TreatAsFirst = treat_based_on_posn,
        ExtraIndentLevel = 0u
    ;
        Msg = no_ctxt_msg(Pieces),
        Components = [always(Pieces)],
        MaybeContext = no,
        TreatAsFirst = treat_based_on_posn,
        ExtraIndentLevel = 0u
    ;
        Msg = simple_msg(SimpleContext, Components),
        MaybeContext = yes(SimpleContext),
        TreatAsFirst = treat_based_on_posn,
        ExtraIndentLevel = 0u
    ;
        Msg = error_msg(MaybeContext, TreatAsFirst, ExtraIndentLevelInt,
            Components),
        ExtraIndentLevel = uint.cast_from_int(ExtraIndentLevelInt)
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
    list(error_msg_component)::in, indent::in,
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
        print_anything(Stream, Anything, !IO),
        !:First = do_not_treat_as_first,
        !:PrintedSome = printed_something
    ),
    write_msg_components(Stream, OptionTable, LimitErrorContextsMap,
        MaybeContext, Components, Indent, !First, !PrintedSome,
        !AlreadyPrintedVerbose, !IO).

%---------------------------------------------------------------------------%

write_error_pieces_plain(Stream, Globals, Pieces, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap,
        no, treat_as_first, 0u, Pieces, !IO).

%---------------------------------------------------------------------------%

write_error_pieces(Stream, Globals, Context, Indent, Pieces, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap,
        yes(Context), treat_as_first, Indent, Pieces, !IO).

%---------------------%

write_error_pieces_maybe_with_context(Stream, Globals, MaybeContext, Indent,
        Pieces, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_pieces(Stream, OptionTable, LimitErrorContextsMap,
        MaybeContext, treat_as_first, Indent, Pieces, !IO).

%---------------------------------------------------------------------------%

:- pred do_write_error_pieces(io.text_output_stream::in, option_table::in,
    limit_error_contexts_map::in, maybe(prog_context)::in,
    maybe_treat_as_first::in, indent::in, list(format_piece)::in,
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
            MaybePrintWithContextStr = no
        else
            ContextStr0 = context_to_string(Context),
            MaybePrintWithContextStr = yes(ContextStr0)
        )
    ;
        MaybeContext = no,
        MaybePrintWithContextStr = yes("")
    ),
    (
        MaybePrintWithContextStr = no
        % Suppress the printing of the error pieces.
    ;
        MaybePrintWithContextStr = yes(ContextStr),
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
            ColorDb = init_color_db(OptionTable),
            convert_pieces_to_lines(ColorDb, MaybeMaxWidth, ContextStr,
                TreatAsFirst, FixedIndent, Pieces, PrefixStr, Lines),
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

% ZZZ move to end, next to convert_line_words_to_string
:- pred write_msg_lines(io.text_output_stream::in, string::in,
    list(error_line)::in, io::di, io::uo) is det.

write_msg_lines(_Stream, _, [], !IO).
write_msg_lines(Stream, PrefixStr, [Line | Lines], !IO) :-
    write_msg_line(Stream, PrefixStr, Line, !IO),
    write_msg_lines(Stream, PrefixStr, Lines, !IO).

:- pred write_msg_line(io.text_output_stream::in, string::in, error_line::in,
    io::di, io::uo) is det.

write_msg_line(Stream, PrefixStr, Line, !IO) :-
    LineWordsStr = convert_line_words_to_string(Line),
    ( if LineWordsStr = "" then
        % Don't bother to print out indents that are followed by nothing.
        io.format(Stream, "%s\n", [s(PrefixStr)], !IO)
    else
        LineIndent = Line ^ line_indent_level,
        IndentStr = indent2_string(LineIndent),
        % If ContextStr is non-empty, it will end with a space,
        % which guarantees that PrefixStr, which is ContextStr possibly with
        % some indentation added, will be separated from LineWords.
        io.format(Stream, "%s%s%s\n",
            [s(PrefixStr), s(IndentStr), s(LineWordsStr)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% Oversee the whole process of converting
%
% - format pieces to paragraphs
% - paragraphs to lines
% - optimize the lines.
%

:- pred convert_pieces_to_lines(color_db::in, maybe(int)::in, string::in,
    maybe_treat_as_first::in, indent::in, list(format_piece)::in,
    string::out, list(error_line)::out) is det.

convert_pieces_to_lines(ColorDb, MaybeMaxWidth, ContextStr, TreatAsFirst,
        FixedIndent, Pieces, PrefixStr, Lines) :-
    convert_pieces_to_paragraphs(ColorDb, Pieces, Paragraphs),
    string.pad_left("", ' ', uint.cast_to_int(FixedIndent), FixedIndentStr),
    PrefixStr = ContextStr ++ FixedIndentStr,
    PrefixLen = string.count_code_points(PrefixStr),
    (
        MaybeMaxWidth = yes(MaxWidth),
        AvailLen = MaxWidth - PrefixLen
    ;
        MaybeMaxWidth = no,
        int.max_int(AvailLen)
    ),
    FirstIndent = (if TreatAsFirst = treat_as_first then 0u else 1u),
    stack.init(ColorStack0),
    divide_paragraphs_into_lines(AvailLen, TreatAsFirst, FirstIndent,
        Paragraphs, Lines0, ColorStack0),
    try_to_join_lp_to_rp_lines(Lines0, Lines),
    trace [compile_time(flag("debug_convert_pieces_to_lines")),
        runtime(env("DEBUG_CONVERT_PIECES_TO_LINES")),
        io(!TIO)]
    (
        io.stderr_stream(StdErr, !TIO),
        io.nl(StdErr, !TIO),
        io.write_string(StdErr, "PARAGRAPHS\n", !TIO),
        list.foldl(io.write_line(StdErr), Paragraphs, !TIO),
        io.nl(StdErr, !TIO),
        io.write_string(StdErr, "LINES\n", !TIO),
        list.foldl(io.write_line(StdErr), Lines0, !TIO),
        io.nl(StdErr, !TIO),
        io.write_string(StdErr, "JOINED\n", !TIO),
        list.foldl(io.write_line(StdErr), Lines, !TIO),
        io.nl(StdErr, !TIO),
        io.write_string(StdErr, "END\n", !TIO),
        io.nl(StdErr, !TIO)
    ).

%---------------------------------------------------------------------------%
%
% Convert components to paragraphs.
%

:- type paragraph
    --->    paragraph(
                % The list of words to print in the paragraph, possibly
                % together with color changes.
                %
                % The list should contain at least one string.
                list(sc_unit),

                % The number of blank lines to print after the paragraph.
                % (This is actually a uint, but its only use is to call
                % a standard library predicate that expects an int.)
                int,

                % The indent delta to apply for the next paragraph.
                int,

                % See the documentation of the line_paren field
                % in the error_line type. It has the same meaning here,
                % except it applies only to the last line of the paragraph.
                paren_status
            ).

:- type sc_unit
    --->    sc_str(string)
    ;       sc_color_start(color_spec)
    ;       sc_color_end.

:- type paren_status
    --->    paren_none
    ;       paren_lp_end    % This paragraph/line ends with a left paren.
    ;       paren_end_rp.   % Next paragraph/line starts with a right paren.

:- pred convert_pieces_to_paragraphs(color_db::in, list(format_piece)::in,
    list(paragraph)::out) is det.

convert_pieces_to_paragraphs(ColorDb, Pieces, Paras) :-
    convert_pieces_to_paragraphs_acc(ColorDb, first_in_msg, Pieces,
        [], cord.empty, ParasCord),
    Paras = cord.list(ParasCord).

:- type word
    --->    plain_word(string)
    ;       prefix_word(string)
    ;       suffix_word(string)
    ;       lower_next_word
    ;       color_start_word(color_spec)
    ;       color_end_word.

:- pred convert_pieces_to_paragraphs_acc(color_db::in, maybe_first_in_msg::in,
    list(format_piece)::in, list(word)::in,
    cord(paragraph)::in, cord(paragraph)::out) is det.

convert_pieces_to_paragraphs_acc(_, _, [], RevWords0, !Paras) :-
    SCUnits = rev_words_to_sc_units(RevWords0),
    add_paragraph(paragraph(SCUnits, 0, 0, paren_none), !Paras).
convert_pieces_to_paragraphs_acc(ColorDb, FirstInMsg, [Piece0 | Pieces0],
        RevWords0, !Paras) :-
    ( if
        ( Piece0 = nl
        ; Piece0 = nl_indent_delta(_)
        ),
        Pieces0 = [HeadPiece0 | TailPieces0],
        HeadPiece0 = not_for_general_use_end_color
    then
        % The pattern [nl, end_color, ...] happens when one of the
        % color_pieces functions in error_spec.m is given a piece list 
        % that includes the end of a line. This leads to unnecessary
        % color changes:
        %
        % - we will automatically add a color reset at the end of the line,
        % - we will automatically switch to the current color at the start
        %   of the next line, and then
        % - when we process the end_color, we will either switch to the
        %   new top color, or reset colors if there is none.
        %
        % I (zs) find that these unnecessary color changes make any
        % .err_exp files containing them harder to check for correctness,
        % since they violate the law of least astonishment.
        %
        % If the two format_pieces were switched, we would get only one
        % color change if the color stack is empty after the color_end.
        % As of this writing, this will always be the case, since we don't
        % (yet) have any code in the compiler that uses nested color scopes.
        % However, even once we start having nested scopes, I expect them
        % to remain a small minority.
        %
        % We could make a rule that color scopes should not include final
        % newlines. However, while complying with such a rule would not be
        % that hard, would also be not that pleasant. It is better to apply
        % the switch here, where the issue can be solved in just one central
        % place.
        %
        % Note that if a newline piece is followed by more than one
        % end_color piece, then this invocation of this predicate will move
        % the newline after the first end_color, the next recursive invocation
        % will move it after the next end_color, and so on.
        Piece = HeadPiece0,
        Pieces = [Piece0 | TailPieces0]
    else
        % The usual path.
        Piece = Piece0,
        Pieces = Pieces0
    ),
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
        SCUnits = rev_words_to_sc_units(RevWords0),
        add_paragraph(paragraph(SCUnits, 0, 0, paren_none), !Paras),
        RevWords1 = []
    ;
        Piece = nl_indent_delta(IndentDelta),
        SCUnits = rev_words_to_sc_units(RevWords0),
        add_paragraph(paragraph(SCUnits, 0, IndentDelta, paren_none), !Paras),
        RevWords1 = []
    ;
        Piece = blank_line,
        SCUnits = rev_words_to_sc_units(RevWords0),
        add_paragraph(paragraph(SCUnits, 1, 0, paren_none), !Paras),
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
        SCUnits = rev_words_to_sc_units([LPWord | RevWords0]),
        add_paragraph(paragraph(SCUnits, 0, 1, paren_lp_end), !Paras),
        RevWords1 = []
    ;
        Piece = maybe_nl_dec_right_paren(RP, RPWordKind),
        SCUnits = rev_words_to_sc_units(RevWords0),
        add_paragraph(paragraph(SCUnits, 0, -1, paren_end_rp), !Paras),
        (
            RPWordKind = rp_plain,
            RPWord = plain_word(RP)
        ;
            RPWordKind = rp_prefix,
            RPWord = prefix_word(RP)
        ),
        RevWords1 = [RPWord]
    ;
        Piece = not_for_general_use_start_color(ColorName),
        (
            ColorDb = no_color_db,
            RevWords1 = RevWords0
        ;
            ColorDb = color_db(ColorNameMap),
            (
                ColorName = color_correct,
                Color = ColorNameMap ^ cnm_correct
            ;
                ColorName = color_incorrect,
                Color = ColorNameMap ^ cnm_incorrect
            ;
                ColorName = color_cause,
                Color = ColorNameMap ^ cnm_cause
            ),
            RevWords1 = [color_start_word(Color) | RevWords0]
        )
    ;
        Piece = not_for_general_use_end_color,
        (
            ColorDb = no_color_db,
            RevWords1 = RevWords0
        ;
            ColorDb = color_db(_ColorNameMap),
            RevWords1 = [color_end_word | RevWords0]
        )
    ;
        ( Piece = invis_order_default_start(_, _)
        ; Piece = invis_order_default_end(_, _)
        ; Piece = treat_next_as_first
        ),
        RevWords1 = RevWords0
    ),
    update_first_in_msg_after_piece(Piece, FirstInMsg, TailFirstInMsg),
    convert_pieces_to_paragraphs_acc(ColorDb, TailFirstInMsg, Pieces,
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
    ;       lower_next
    ;       color_start(color_spec)
    ;       color_end.

:- func rev_words_to_sc_units(list(word)) = list(sc_unit).

rev_words_to_sc_units(RevWords) = SCUnits :-
    PorPs = list.reverse(rev_words_to_rev_plain_or_prefix(RevWords)),
    SCUnits = join_prefixes(PorPs).

:- func rev_words_to_rev_plain_or_prefix(list(word)) = list(plain_or_prefix).

rev_words_to_rev_plain_or_prefix([]) = [].
rev_words_to_rev_plain_or_prefix([Word | Words]) = RevPorPs :-
    (
        Word = plain_word(String),
        RevPorPs = [plain(String) | rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = color_start_word(Color),
        RevPorPs = [color_start(Color) |
            rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = color_end_word,
        RevPorPs = [color_end | rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = lower_next_word,
        RevPorPs = [lower_next | rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = prefix_word(Prefix),
        RevPorPs = [prefix(Prefix) | rev_words_to_rev_plain_or_prefix(Words)]
    ;
        Word = suffix_word(Suffix),
        (
            Words = [],
            RevPorPs = [plain(Suffix)]
        ;
            Words = [color_start_word(Color) | Tail],
            RevPorPs = [plain(Suffix), color_start(Color)
                | rev_words_to_rev_plain_or_prefix(Tail)]
        ;
            Words = [color_end_word | Tail],
            RevPorPs = [plain(Suffix), color_end
                | rev_words_to_rev_plain_or_prefix(Tail)]
        ;
            Words = [plain_word(String) | Tail],
            RevPorPs = [plain(String ++ Suffix)
                | rev_words_to_rev_plain_or_prefix(Tail)]
        ;
            Words = [lower_next_word | Tail],
            % Convert the lower_next_word/suffix combination into just the
            % suffix after lowercasing the suffix (which will probably have
            % no effect, since the initial character of a suffix is usually
            % not a letter).
            NewWords = [suffix_word(uncapitalize_first(Suffix)) | Tail],
            RevPorPs = rev_words_to_rev_plain_or_prefix(NewWords)
        ;
            Words = [prefix_word(Prefix) | Tail],
            % Convert the prefix/suffix combination into a plain word.
            % We could convert it into a prefix, but since prefix/suffix
            % combinations shouldn't come up at all, what we do here probably
            % doesn't matter.
            RevPorPs = [plain(Prefix ++ Suffix)
                | rev_words_to_rev_plain_or_prefix(Tail)]
        ;
            Words = [suffix_word(MoreSuffix) | Tail],
            RevPorPs = rev_words_to_rev_plain_or_prefix(
                [suffix_word(MoreSuffix ++ Suffix) | Tail])
        )
    ).

:- func join_prefixes(list(plain_or_prefix)) = list(sc_unit).

join_prefixes([]) = [].
join_prefixes([Head | Tail]) = SCUnits :-
    TailSCUnits = join_prefixes(Tail),
    (
        Head = plain(String),
        SCUnits = [sc_str(String) | TailSCUnits]
    ;
        Head = prefix(Prefix),
        (
            TailSCUnits = [HeadTailSCUnit | TailTailSCUnits],
            (
                HeadTailSCUnit = sc_str(HeadTailStr),
                SCUnits = [sc_str(Prefix ++ HeadTailStr) | TailTailSCUnits]
            ;
                ( HeadTailSCUnit = sc_color_start(_)
                ; HeadTailSCUnit = sc_color_end
                ),
                SCUnits = [sc_str(Prefix), HeadTailSCUnit | TailTailSCUnits]
            )
        ;
            TailSCUnits = [],
            SCUnits = [sc_str(Prefix) | TailSCUnits]
        )
    ;
        Head = lower_next,
        (
            TailSCUnits = [],
            SCUnits = TailSCUnits
        ;
            TailSCUnits = [FirstTailSCUnits | LaterTailSCUnits],
            SCUnits = [lower_next_sc_unit(FirstTailSCUnits) | LaterTailSCUnits]
        )
    ;
        Head = color_start(Color),
        SCUnits = [sc_color_start(Color) | TailSCUnits]
    ;
        Head = color_end,
        SCUnits = [sc_color_end | TailSCUnits]
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

:- func lower_next_sc_unit(sc_unit) = sc_unit.

lower_next_sc_unit(SCUnit0) = SCUnit :-
    (
        SCUnit0 = sc_str(String0),
        SCUnit = sc_str(uncapitalize_first(String0))
    ;
        ( SCUnit0 = sc_color_start(_)
        ; SCUnit0 = sc_color_end
        ),
        SCUnit = SCUnit0
    ).

%---------------------------------------------------------------------------%
%
% Divide paragraphs into lines.
%

:- type color_stack == stack(color_spec).

:- type line_end_reset
    --->    line_end_reset_nothing
    ;       line_end_reset_color.

:- type error_line
    --->    error_line(
                % This will be AvailLen, the total number of code points
                % available on the line after the context and the fixed indent.
                %
                % In the absence of a limit on the lengths of lines,
                % this will be int.max_int.
                maybe_avail_len     :: int,

                % Indent level of the line; multiply by indent2_increment
                % to get the number of spaces this turns into.
                line_indent_level   :: indent,

                % The words on the line as a single string, with one space
                % between each pair of words.
                line_words_str      :: string,

                % The length of the line_words_str field.
                line_words_len      :: int,

                % Have we made any color changes on the line? If yes,
                % then we want to reset the colors at the end of the line,
                % because we don't want them to affect how we print either
                % - the context of the next line, if there is one, or
                % - whatever follows after, if there is no next line.
                % (But see the line_end_colors field below.)
                line_end_reset       :: line_end_reset,

                % The color stack at the start of the line.
                %
                % We need to know whether we were in the middle of the scope
                % of a color at the end of the previous line. If there was
                % none, we need not do anything at the start of this one.
                % If there was one, we need switch to it before we print
                % this line. We therefore technically need a maybe(color)
                % here, but we can get that same info from a color_stack,
                % and this way requires no data format conversion.
                line_start_colors   :: color_stack,

                % The color stack at the end of the line.
                %
                % If the line either contains color changes, or started out
                % with a nonempty color stack, then we set line_end_reset
                % to line_end_reset_color. However, if the line ends with
                % an empty color stack, then the color reset has happened
                % *before* we got to the end of the line, and any reset
                % we could add at the end of the line would be redundant.
                line_end_colors   :: color_stack,

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
    % The given list of paragraphs should be nonempty, since we always return
    % at least one line.
    %
:- pred divide_paragraphs_into_lines(int::in, maybe_treat_as_first::in,
    indent::in, list(paragraph)::in, list(error_line)::out,
    color_stack::in) is det.

divide_paragraphs_into_lines(AvailLen, TreatAsFirst, CurIndent, Paras,
        Lines, ColorStack0) :-
    (
        Paras = [],
        Lines = []
    ;
        Paras = [FirstPara | LaterParas],
        FirstPara = paragraph(FirstParaWords, NumBlankLines, FirstIndentDelta,
            ParaParen),
        (
            TreatAsFirst = treat_as_first,
            RestIndent = CurIndent + 1u
        ;
            TreatAsFirst = do_not_treat_as_first,
            RestIndent = CurIndent
        ),
        NextIndentInt = uint.cast_to_int(RestIndent) + FirstIndentDelta,
        ( if uint.from_int(NextIndentInt, NextIndentPrime) then
            NextIndent = NextIndentPrime,
            FirstParaWarningLines = []
        else
            % This indicates a bug in the code constructing the error_spec
            % that we are trying to output here, with the bug being a
            % nl_indent_delta with a negative delta that exceeds the current
            % indent level.
            %
            % We *could* abort here to warn about the problem, but that
            % would be drastic. Adding this warning to the output should grab
            % just as much attention, but it also allows the problem to be
            % addressed at a time chosen by the programmer.
            WarningLine = error_line(AvailLen, CurIndent,
                "WARNING: NEGATIVE INDENT", 0,
                line_end_reset_nothing, ColorStack0, ColorStack0, paren_none),
            FirstParaWarningLines = [WarningLine],
            NextIndent = 0u
        ),

        ( if NumBlankLines > 0 then
            BlankLine = error_line(AvailLen, CurIndent, "", 0,
                line_end_reset_nothing, ColorStack0, ColorStack0, paren_none),
            list.duplicate(NumBlankLines, BlankLine, FirstParaBlankLines)
        else
            FirstParaBlankLines = []
        ),
        (
            FirstParaWords = [],
            NextTreatAsFirst = TreatAsFirst,
            FirstParaLines = [],
            ColorStackNext = ColorStack0
        ;
            FirstParaWords = [FirstWord | LaterWords],
            NextTreatAsFirst = do_not_treat_as_first,
            get_line_of_words(AvailLen, FirstWord, LaterWords,
                CurIndent, LineWordsLen, LineWords, RestWords,
                ColorStack0, ColorStack1, LineEndReset),
            LineWordsStr = string.append_list(LineWords),
            (
                RestWords = [],
                CurLine = error_line(AvailLen, CurIndent, LineWordsStr,
                    LineWordsLen, LineEndReset, ColorStack0, ColorStack1,
                    ParaParen),
                FirstParaLines = [CurLine],
                ColorStackNext = ColorStack1
            ;
                RestWords = [FirstRestWord | LaterRestWords],
                CurLine = error_line(AvailLen, CurIndent, LineWordsStr,
                    LineWordsLen, LineEndReset, ColorStack0, ColorStack1,
                    paren_none),
                group_nonfirst_line_words(AvailLen,
                    FirstRestWord, LaterRestWords, RestIndent, ParaParen,
                    FirstParaRestLines, ColorStack1, ColorStackNext),
                FirstParaLines = [CurLine | FirstParaRestLines]
            )
        ),
        divide_paragraphs_into_lines(AvailLen, NextTreatAsFirst,
            NextIndent, LaterParas, LaterParaLines, ColorStackNext),
        Lines = FirstParaWarningLines ++ FirstParaLines ++
            FirstParaBlankLines ++ LaterParaLines
    ).

:- pred group_nonfirst_line_words(int::in, sc_unit::in, list(sc_unit)::in,
    indent::in, paren_status::in, list(error_line)::out,
    color_stack::in, color_stack::out) is det.

group_nonfirst_line_words(AvailLen, FirstWord, LaterWords,
        Indent, LastParen, Lines, ColorStack0, ColorStack) :-
    get_line_of_words(AvailLen, FirstWord, LaterWords, Indent,
        LineWordsLen, LineWords, RestWords, ColorStack0, ColorStack1,
        EndLineReset),
    LineWordsStr = string.append_list(LineWords),
    (
        RestWords = [],
        Line = error_line(AvailLen, Indent, LineWordsStr, LineWordsLen,
            EndLineReset, ColorStack0, ColorStack1, LastParen),
        Lines = [Line],
        ColorStack = ColorStack1
    ;
        RestWords = [FirstRestWord | LaterRestWords],
        Line = error_line(AvailLen, Indent, LineWordsStr, LineWordsLen,
            EndLineReset, ColorStack0, ColorStack1, paren_none),
        group_nonfirst_line_words(AvailLen, FirstRestWord, LaterRestWords,
            Indent, LastParen, RestLines, ColorStack1, ColorStack),
        Lines = [Line | RestLines]
    ).

% The rules for color starts and ends:
%
% When we see one, we always look for and process all following consecutive
% color starts and ends, and handle their aggregrate effect. These "spans"
% of color changes are always both preceded and followed by sc_str units;
% if they were preceded or followed by color change units, those units
% would have been included in the span.
% 
% If code always creates color changes using the color_pieces_as_X
% functions in error_spec.m, then a color change span whose first unit
% is sc_color_start will end up with a stack that is at least as high
% as it started with, while a span whose first unit is sc_color_end
% will end up with a stack stack that is at most as high as it started with.
% Therefore the total effect of each span is given by its initial unit.
%
% We want to limit color changes to apply to just the pieces they are
% supposed to cover, not to any spaces around them. We therefore added
% the NextSpace parameter to get_later_words, which should contain
% either zero spaces or one space, to allow the addition of spaces
% to be delayed. We exploit this capability using the following rules
% for how spaces should be handled around color changes:
%
% at left margin (get_line_of_words)
% str:          add the string, NextSpace := 1
% color_start: add new color, NextSpace := 0
% color_end:   add new color, NextSpace := 0
%
% not at left margin (get_later_words)
% str:          add NextSpace spaces, add the string, NextSpace := 1
% color_start: check NextSpace=1, add 1 space, add top color, NextSpace := 0
% color_end:   check NextSpace=1, add top color, NextSpace := 1
%
% Note how with color_start, get_later_words add the space *before*
% the color change, while with color_end, it adds the space *after*.
%
% (The checks for NextSpace=1 in get_later_words when the first unit
% is a color change must succeed, because the first unit is a color change
% *only* if was not included in a previously-started span of color changes,
% which means that the previous unit (which was handled by get_later_words)
% was sc_str.)

:- pred get_line_of_words(int::in, sc_unit::in, list(sc_unit)::in,
    indent::in, int::out, list(string)::out, list(sc_unit)::out,
    color_stack::in, color_stack::out, line_end_reset::out) is det.

get_line_of_words(AvailLen, FirstSCUnit, LaterSCUnits0, Indent, LineWordsLen,
        LineStrs, RestSCUnits, !ColorStack, LineEndReset) :-
    AvailLeft = AvailLen - uint.cast_to_int(Indent * indent2_increment),
    (
        FirstSCUnit = sc_str(FirstStr),
        ( if stack.is_empty(!.ColorStack) then
            LineEndReset1 = line_end_reset_nothing
        else
            % If the initial stack contains a color, then we need to reset
            % colors at the end of the line even if the line itself
            % contains no color changes.
            LineEndReset1 = line_end_reset_color
        ),
        NextSpace = " ",
        string.count_code_points(FirstStr, LenSoFar),
        LaterSCUnits0 = LaterSCUnits
    ;
        ( FirstSCUnit = sc_color_start(_Color)
        ; FirstSCUnit = sc_color_end
        ),
        LineEndReset1 = line_end_reset_color,
        NextSpace = "",
        merge_adjacent_color_changes(FirstSCUnit, LaterSCUnits0, LaterSCUnits,
            !ColorStack),
        FirstStr = top_color_to_string(!.ColorStack),
        LenSoFar = 0
    ),
    LineStrsCord0 = cord.singleton(FirstStr),
    get_later_words(AvailLeft, NextSpace, LaterSCUnits, LenSoFar, LineWordsLen,
        LineStrsCord0, LineStrsCord, RestSCUnits, !ColorStack,
        LineEndReset1, LineEndReset),
    LineStrs = cord.list(LineStrsCord).

:- pred get_later_words(int::in, string::in, list(sc_unit)::in,
    int::in, int::out, cord(string)::in, cord(string)::out, list(sc_unit)::out,
    color_stack::in, color_stack::out,
    line_end_reset::in, line_end_reset::out) is det.

get_later_words(_, _, [], CurLen, FinalLen,
        LineStrs, LineStrs, [], !ColorStack, LineEndReset, LineEndReset) :-
    FinalLen = CurLen.
get_later_words(Avail, NextSpace0, [SCUnit | SCUnits0], CurLen, FinalLen,
        LineStrs0, LineStrs, RestSCUnits, !ColorStack,
        LineEndReset0, LineEndReset) :-
    (
        SCUnit = sc_str(Str0),
        LineEndReset1 = LineEndReset0,
        SCUnits = SCUnits0,
        FirstStr = NextSpace0 ++ Str0,
        string.count_code_points(FirstStr, FirstStrLen),
        NextLen = CurLen + FirstStrLen,
        NextSpace = " "
    ;
        SCUnit = sc_color_start(_),
        LineEndReset1 = line_end_reset_color,
        expect(unify(NextSpace0, " "), $pred,
            "NextSpace0 != 1 for color start"),
        merge_adjacent_color_changes(SCUnit, SCUnits0, SCUnits, !ColorStack),
        % We put the space between the previous actual word and the next one
        % *before* the color change.
        FirstStr = NextSpace0 ++ top_color_to_string(!.ColorStack),
        NextLen0 = CurLen + 1, % The 1 is for NextSpace0.
        NextSpace = "",
        % Check whether the next actual word fits on the line.
        PeekWordLen = peek_and_find_len_of_next_word(SCUnits),
        ( if NextLen0 + PeekWordLen =< Avail then
            % It does, so let the recursive call proceed.
            NextLen = NextLen0
        else
            % It does not, so force the check against Avail to fail *now*,
            % *not* in the recursive call, so that we change the color
            % just before the next word.
            NextLen = Avail + 1
        )
    ;
        SCUnit = sc_color_end,
        LineEndReset1 = line_end_reset_color,
        expect(unify(NextSpace0, " "), $pred, "NextSpace0 != 1 for color end"),
        merge_adjacent_color_changes(SCUnit, SCUnits0, SCUnits, !ColorStack),
        % We put the space between the previous actual word and the next one
        % *after* the color change. (Actually, the recursive call takes
        % care of that, *if* there are any words on the rest of the line.)
        FirstStr = top_color_to_string(!.ColorStack),
        NextLen = CurLen,
        NextSpace = " "
    ),
    ( if
        % Include FirstStr on the current line if either ...
        (
            NextLen =< Avail
            % ... there is room for it, or ...
        ;
            CurLen = 0
            % ... if FirstStr is too long for a line overall.
            % Before we added support for color, this latter condition
            % could happen only in get_line_of_words, but now, that
            % predicate can put a color change on the line (which takes up
            % zero columns) and call *us* with a too-long-to-fit sc_str unit.
        )
    then
        cord.snoc(FirstStr, LineStrs0, LineStrs1),
        get_later_words(Avail, NextSpace, SCUnits, NextLen, FinalLen,
            LineStrs1, LineStrs, RestSCUnits, !ColorStack,
            LineEndReset1, LineEndReset)
    else
        FinalLen = CurLen,
        LineStrs = LineStrs0,
        RestSCUnits = [SCUnit | SCUnits],
        LineEndReset = LineEndReset0
    ).

:- inst color_unit for sc_unit/0
    --->    sc_color_start(ground)
    ;       sc_color_end.

:- pred merge_adjacent_color_changes(sc_unit::in(color_unit),
    list(sc_unit)::in, list(sc_unit)::out,
    color_stack::in, color_stack::out) is det.

merge_adjacent_color_changes(HeadSCUnit, TailSCUnits0, SCUnits,
        !ColorStack) :-
    (
        HeadSCUnit = sc_color_start(Color),
        stack.push(Color, !ColorStack)
    ;
        HeadSCUnit = sc_color_end,
        ( if stack.pop(_OldTopColor, !ColorStack) then
            % Keep the stack with the top color popped off.
            true
        else
            % Keep the original stack.
            true
        )
    ),
    (
        TailSCUnits0 = [],
        SCUnits = []
    ;
        TailSCUnits0 = [HeadTailSCUnit0 | TailTailSCUnits0],
        (
            HeadTailSCUnit0 = sc_str(_),
            SCUnits = TailSCUnits0
        ;
            ( HeadTailSCUnit0 = sc_color_start(_)
            ; HeadTailSCUnit0 = sc_color_end
            ),
            merge_adjacent_color_changes(HeadTailSCUnit0, TailTailSCUnits0,
                SCUnits, !ColorStack)
        )
    ).

:- func peek_and_find_len_of_next_word(list(sc_unit)) = int.

peek_and_find_len_of_next_word([]) = 0.
peek_and_find_len_of_next_word([SCUnit | _]) = Len :-
    (
        SCUnit = sc_str(Str),
        string.count_code_points(Str, Len)
    ;
        ( SCUnit = sc_color_start(_)
        ; SCUnit = sc_color_end
        ),
        unexpected($pred, "next sc_unit after color change is not sc_str")
    ).

:- func top_color_of_stack(color_stack) = color.

top_color_of_stack(ColorStack) = TopColor :-
    ( if stack.top(ColorStack, TopColorSpec) then
        TopColor = color_set(TopColorSpec)
    else
        TopColor= color_reset
    ).

:- func top_color_to_string(color_stack) = string.

top_color_to_string(ColorStack) = Str :-
    Str = color_to_string(top_color_of_stack(ColorStack)).

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
    HeadLine0 = error_line(_AvailLen, _HeadIndent, _HeadLineWords,
        _HeadLineWordsLen, _LineEndReset, _HeadStartCS, _HeadEndCS, HeadParen),
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
    LPLine = error_line(AvailLen, LPIndent, LPLineWordsStr,
        LPLineWordsLen, LPLineEndReset, LPStartCS, LPEndCS, LPParen),
    expect(unify(LPParen, paren_lp_end), $pred, "LPParen != paren_lp_end"),
    ( if
        find_matching_rp(TailLines0, cord.init, MidLinesCord, 0, MidLinesLen,
            RPLine, LeftOverLinesPrime)
    then
        RPLine = error_line(_, _RPIndent, RPLineWordsStr, RPLineWordsLen,
            RPLineEndReset, RPStartCS, RPEndCS, RPParen),
        expect(unify(LPEndCS, RPStartCS), $pred, "LPEndCS != RPStartCS"),
        MidLines = cord.list(MidLinesCord),
        list.length(MidLines, NumMidLines),
        MidLineSpaces = (if NumMidLines = 0 then 0 else NumMidLines - 1),
        TotalLpRpLen =
            LPLineWordsLen + MidLinesLen + MidLineSpaces + RPLineWordsLen,
        ChunkLines = [LPLine | MidLines] ++ [RPLine],
        ( if
            uint.cast_to_int(LPIndent * indent2_increment) + TotalLpRpLen
                =< AvailLen
        then
            % We insert spaces
            % - between the middle lines, but
            % - not after the ( in LPLine,
            % - nor before the ) in RPLine.
            MidLineStrs = list.map((func(L) = L ^ line_words_str), MidLines),
            MidSpaceLinesStr = string.join_list(" ", MidLineStrs),
            ReplacementLineStr =
                LPLineWordsStr ++ MidSpaceLinesStr ++ RPLineWordsStr,
            ( if
                LPLineEndReset = line_end_reset_nothing,
                RPLineEndReset = line_end_reset_nothing
            then
                LPRPLineEndReset = line_end_reset_nothing
            else
                LPRPLineEndReset = line_end_reset_color
            ),
            ReplacementLine = error_line(AvailLen, LPIndent,
                ReplacementLineStr, TotalLpRpLen, LPRPLineEndReset,
                LPStartCS, RPEndCS, RPParen),
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
    HeadLine0 = error_line(_HeadAvailLen, _HeadIndent, _HeadLineWordsStr,
        HeadLineWordsLen, _HeadLineEndReset, _HeadStartCS, _HeadEndCS,
        HeadParen),
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
%
% Color management.
%

:- type color
    --->    color_set(color_spec)
    ;       color_reset.

:- type color_db
    --->    no_color_db
            % Never use any color, and ignore all color changes.
    ;       color_db(color_name_map).
            % Do use color.

:- type color_name_map
    --->    color_name_map(
                cnm_correct     :: color_spec,
                cnm_incorrect   :: color_spec,
                cnm_cause       :: color_spec
            ).

:- func init_color_db(option_table) = color_db.

init_color_db(OptionTable) = ColorDb :-
    getopt.lookup_bool_option(OptionTable, use_color_diagnostics, UseColors),
    (
        UseColors = no,
        ColorDb = no_color_db
    ;
        UseColors = yes,
        MaybeColorSpecs = convert_color_spec_options(OptionTable),
        (
            MaybeColorSpecs = error1(_Specs),
            % This should not happen, because handle_options.m should
            % report _Specs, and not let execution proceed any further.
            % But just in case ...
            ColorDb = no_color_db
        ;
            MaybeColorSpecs = ok1(ColorSpecs),
            ColorSpecs = color_specs(MaybeCorrect, MaybeIncorrect, MaybeCause),
            (
                MaybeCorrect = yes(Correct)
            ;
                MaybeCorrect = no,
                Correct = color_8bit(40u8)          % This is green.
            ),
            (
                MaybeIncorrect = yes(Incorrect)
            ;
                MaybeIncorrect = no,
                Incorrect = color_8bit(203u8)       % This is red.
            ),
            (
                MaybeCause = yes(Cause)
            ;
                MaybeCause = no,
                Cause = color_8bit(226u8)           % This is yellow.
            ),
            ColorNameMap = color_name_map(Correct, Incorrect, Cause),
            ColorDb = color_db(ColorNameMap)
        )
    ).

    % The terminal control codes we use here are described by
    % https://en.wikipedia.org/wiki/ANSI_escape_code#Colors.
    %
:- func color_to_string(color) = string.

color_to_string(SetOrReset) = Str :-
    (
        SetOrReset = color_set(Color),
        Color = color_8bit(ColorNum),
        string.format("\033\[38;5;%dm", [i(uint8.cast_to_int(ColorNum))], Str)
    ;
        SetOrReset = color_reset,
        Str =  "\033\[39;49m"
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

error_pieces_to_std_lines(Pieces) = Lines :-
    convert_pieces_to_lines(no_color_db, yes(80), "",
        treat_as_first, 0u, Pieces, _, Lines).

do_lines_fit_in_n_code_points(_Max, []).
do_lines_fit_in_n_code_points(Max, [Line1 | Lines2plus]) :-
    LineLen1 = error_line_len(Line1),
    MaxLeft = Max - LineLen1,
    MaxLeft >= 0,
    (
        Lines2plus = []
    ;
        Lines2plus = [Line2 | Lines3plus],
        do_spaces_lines_fit_in_n_code_points(MaxLeft, Line2, Lines3plus)
    ).

:- pred do_spaces_lines_fit_in_n_code_points(int::in,
    error_line::in, list(error_line)::in) is semidet.

do_spaces_lines_fit_in_n_code_points(Max, Line1, Lines2plus) :-
    LineLen1 = error_line_len(Line1),
    % The -1 accounts for the space before Line1.
    MaxLeft = Max - 1 - LineLen1,
    MaxLeft >= 0,
    (
        Lines2plus = []
    ;
        Lines2plus = [Line2 | Lines3plus],
        do_spaces_lines_fit_in_n_code_points(MaxLeft, Line2, Lines3plus)
    ).

:- func error_line_len(error_line) = int.

error_line_len(Line) = LineLen :-
    Line = error_line(_AvailLen, _LineIndent, _LineWordsStr, LineWordsLen,
        _LineEndReset, _StartCS, _EndCS, _LineParen),
    LineLen = LineWordsLen.

%---------------------%

error_lines_to_one_line_string(Lines) = Str :-
    LineStrs = list.map(convert_line_words_to_string, Lines),
    Str = string.join_list(" ", LineStrs).

error_lines_to_multi_line_string(Prefix, Lines) = Str :-
    LineStrs = list.map(convert_line_and_nl_to_string(Prefix), Lines),
    string.append_list(LineStrs, Str).

%---------------------%

error_pieces_to_one_line_string(Pieces) = Str :-
    Lines = error_pieces_to_std_lines(Pieces),
    Str = error_lines_to_one_line_string(Lines).

error_pieces_to_multi_line_string(Prefix, Pieces) = Str :-
    Lines = error_pieces_to_std_lines(Pieces),
    Str = error_lines_to_multi_line_string(Prefix, Lines).

%---------------------%

:- func convert_line_words_to_string(error_line) = string.

convert_line_words_to_string(Line) = Str :-
    Line = error_line(_AvailLen, _LineIndent, LineWordsStr, _LineWordsLen,
        LineEndReset, StartColorStack, EndColorStack, _LineParen),
    ( if LineWordsStr = "" then
        Str = ""
    else
        ( if stack.top(StartColorStack, StartColor) then
            StartColorStr = color_to_string(color_set(StartColor))
        else
            StartColorStr = ""
        ),
        (
            LineEndReset = line_end_reset_nothing,
            EndColorResetStr = ""
        ;
            LineEndReset = line_end_reset_color,
            ( if stack.top(EndColorStack, _) then
                EndColorResetStr = color_to_string(color_reset)
            else
                % Any color we had on the line was reset when we popped
                % the last color off the stack.
                EndColorResetStr = ""
            )
        ),
        Str = StartColorStr ++ LineWordsStr ++ EndColorResetStr
    ).

:- func convert_line_and_nl_to_string(string, error_line) = string.

convert_line_and_nl_to_string(Prefix, Line) = Str :-
    Line = error_line(_AvailLen, LineIndent, RawLineWordsStr, _LineWordsLen,
        _LineEndReset, _StartColorStack, _EndColorStack, _LineParen),
    ( if RawLineWordsStr = "" then
        % Don't include the indent.
        Str = Prefix ++ "\n"
    else
        IndentStr = indent2_string(LineIndent),
        LineWordsStr = convert_line_words_to_string(Line),
        Str = Prefix ++ IndentStr ++ LineWordsStr ++ "\n"
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates.
%

:- type maybe_first_in_msg
    --->    first_in_msg
    ;       not_first_in_msg.

:- pred update_first_in_msg_after_piece(format_piece::in,
    maybe_first_in_msg::in, maybe_first_in_msg::out) is det.

update_first_in_msg_after_piece(Piece, FirstInMsg, TailFirstInMsg) :-
    (
        ( Piece = treat_next_as_first
        ; Piece = blank_line
        ),
        TailFirstInMsg = first_in_msg
    ;
        ( Piece = lower_case_next_if_not_first
        ; Piece = nl
        ; Piece = nl_indent_delta(_)
        ; Piece = not_for_general_use_start_color(_)
        ; Piece = not_for_general_use_end_color
        ; Piece = invis_order_default_start(_, _)
        ; Piece = invis_order_default_end(_, _)
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
    [thread_local, untrailed, attach_to_io_state]).

    % Is there extra information about errors available that could be printed
    % if the values of --limit-error-contexts options allowed it?
    %
:- mutable(some_errors_were_context_limited,
    context_limited_errors, no_errors_were_context_limited, ground,
    [thread_local, untrailed, attach_to_io_state]).

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
