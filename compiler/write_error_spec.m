%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2022-2025 The Mercury team.
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
    % but return the resulting string instead of printing it out.
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

    % maybe_write_out_errors(Stream, Verbose, Globals, !Specs, !IO):
    %
    % Every possible path of execution in mercury_compile.m should call
    % write_error_specs on the accumulated but not-yet-printed error_specs
    % exactly once, just after the compiler has finished doing all the things
    % that can generate error reports. This predicate is intended to manage
    % the printing of error_specs before that point.
    %
    % If Verbose = no, then that call to write_error_specs should write out
    % all at once all the error specifications accumulated until then.
    % Being written out all at once, write_error_specs can sort them
    % by context.
    %
    % If Verbose = yes, then keeping all the error messages until the end would
    % be confusing, since we would be reporting that e.g. the program had type
    % errors *before* printing the type error messages. In that case,
    % we want to print (using maybe_write_out_errors) all the accumulated
    % errors before each message to the user.
    %
    % This applies to *all* messages.
    %
    % - The calls to maybe_write_out_errors before a message that announces
    %   the completion (and success or failure) of a phase should obviously
    %   report the errors (if any) discovered by the phase.
    %
    % - The calls to maybe_write_out_errors before a message that announces
    %   the phase the compiler is about to enter serve to write out any
    %   messages from previous phases that have not yet been written out.
    %
    %   We could require each phase to write out the errors it discovers
    %   when it finishes (if Verbose = yes, that is), but that would eliminate
    %   any opportunity to group and sort together the error messages
    %   of two or more adjacent phases that are *not* separated by a message
    %   to the user even with Verbose = yes. Since the cost of calling
    %   maybe_write_out_errors when there is nothing to print is so low
    %   (a few dozen instructions), we can easily afford to incur it
    %   unnecessarily once per compiler phase.
    %
:- pred maybe_write_out_errors(io.text_output_stream::in, bool::in,
    globals::in, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred record_bad_color_scheme(error_spec::in, io::di, io::uo) is det.

    % If we withheld some error information from the user (at the users'
    % own request, of course), then print a reminder of that fact,
    % to avoid violating the law of least astonishment.
    %
:- pred maybe_print_delayed_error_messages(io.text_output_stream::in,
    globals::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.color_schemes.
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
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module stack.
:- import_module string.
:- import_module term_context.
:- import_module uint.

%---------------------------------------------------------------------------%
%
% We keep a record of the set of already-printed verbose_once components
% only during the invocation of a single call to write_error_specs, or
% its singular version write_error_spec.
%
% We could possibly keep this set in a mutable, but there is no need for that.
% Each specific error message is generated in only one place, which means that
% all its occrrences are generated in one pass. For pretty much all our passes,
% all the error messages generated by the pass are printed by a single
% call to write_error_specs. This means that while in theory, it is possible
% for verbose_once message to be printed by each of several invocations
% of write_error_specs, in practice it won't happen.

write_error_spec(Stream, Globals, Spec, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    ColorDb = init_color_db(OptionTable),
    getopt.lookup_maybe_int_option(OptionTable, max_error_line_width,
        MaybeMaxWidth),
    do_write_error_spec(Stream, OptionTable, LimitErrorContextsMap,
        ColorDb, MaybeMaxWidth, Spec, set.init, _, !IO).

%---------------------%

write_error_specs(Stream, Globals, Specs0, !IO) :-
    globals.get_options(Globals, OptionTable),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_specs(Stream, OptionTable, LimitErrorContextsMap,
        Specs0, !IO).

%---------------------%

write_error_specs_opt_table(Stream, OptionTable, Specs0, !IO) :-
    getopt.lookup_accumulating_option(OptionTable, limit_error_contexts,
        LimitErrorContexts),
    % There is nothing we can usefully do about _BadOptions.
    convert_limit_error_contexts(LimitErrorContexts, _BadOptions,
        LimitErrorContextsMap),
    do_write_error_specs(Stream, OptionTable, LimitErrorContextsMap,
        Specs0, !IO).

:- pred do_write_error_specs(io.text_output_stream::in, option_table::in,
    limit_error_contexts_map::in, list(error_spec)::in, io::di, io::uo) is det.

do_write_error_specs(Stream, OptionTable, LimitErrorContextsMap,
        Specs0, !IO) :-
    sort_error_specs_opt_table(OptionTable, Specs0, Specs),
    ColorDb = init_color_db(OptionTable),
    getopt.lookup_maybe_int_option(OptionTable, max_error_line_width,
        MaybeMaxWidth),
    list.foldl2(
        do_write_error_spec(Stream, OptionTable, LimitErrorContextsMap,
            ColorDb, MaybeMaxWidth),
        Specs, set.init, _, !IO).

%---------------------------------------------------------------------------%

:- pred do_write_error_spec(io.text_output_stream::in, option_table::in,
    limit_error_contexts_map::in, color_db::in, maybe(int)::in, error_spec::in,
    already_printed_verbose::in, already_printed_verbose::out,
    io::di, io::uo) is det.

do_write_error_spec(Stream, OptionTable, LimitErrorContextsMap, ColorDb,
        MaybeMaxWidth, Spec, !AlreadyPrintedVerbose, !IO) :-
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
    maybe_add_error_spec_id(OptionTable, Id, Msgs1, Msgs),
    collect_msgs(OptionTable, LimitErrorContextsMap, Msgs, treat_as_first,
        cord.init, AllMsgsCord, do_not_print_spec, MaybePrintSpec,
        !.AlreadyPrintedVerbose, UpdatedAlreadyPrintedVerbose, !IO),
    AllMsgs = cord.list(AllMsgsCord),
    (
        MaybePrintSpec = do_not_print_spec
        % There may be (and almost certainly will be) some messages in AllMsgs,
        % but none of them are to be printed. This is
        %
        % - why we don't actually print any messages, and
        % - why we leave !.AlreadyPrintedVerbose as it was before the call
        %   to collect_msgs.
        %
        % XXX The following assertion is commented out because the compiler
        % can generate error specs that consist only of conditional error
        % messages whose conditions can all be false (in which case nothing
        % will be printed). Such specs will cause the assertion to fail if
        % they have a severity that means something *should* have been
        % printed out. Error specs like this are generated by --debug-modes.
        % expect(unify(MaybeActual, no), $pred, "MaybeActual isn't no")
    ;
        MaybePrintSpec = do_print_spec,
        % If *some* messages in Spec are to be printed, then we print *all*
        % the messages in Spec, even the ones whose contexts do not fall
        % into a to-be-printed range.
        list.foldl(write_msg_pieces(Stream, ColorDb, MaybeMaxWidth),
            AllMsgs, !IO),
        !:AlreadyPrintedVerbose = UpdatedAlreadyPrintedVerbose,
        set_wrote_something(yes, !IO),
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

:- type maybe_print_spec
    --->    do_not_print_spec
    ;       do_print_spec.

:- type already_printed_verbose == set(list(format_piece)).

:- pred collect_msgs(option_table::in, limit_error_contexts_map::in,
    list(error_msg)::in, maybe_treat_as_first::in,
    cord(msg_pieces)::in, cord(msg_pieces)::out,
    maybe_print_spec::in, maybe_print_spec::out,
    already_printed_verbose::in, already_printed_verbose::out,
    io::di, io::uo) is det.

collect_msgs(_, _, [], _,
        !MsgsCord, !MaybePrintSpec, !AlreadyPrintedVerbose, !IO).
collect_msgs(OptionTable, LimitErrorContextsMap, [Msg | Msgs], !.First,
        !MsgsCord, !MaybePrintSpec, !AlreadyPrintedVerbose, !IO) :-
    (
        Msg = msg(SimpleContext, Pieces0),
        Components = [always(Pieces0)],
        MaybeContext = yes(SimpleContext),
        TreatAsFirst = treat_based_on_posn,
        Indent = 0u
    ;
        Msg = no_ctxt_msg(Pieces0),
        Components = [always(Pieces0)],
        MaybeContext = no,
        TreatAsFirst = treat_based_on_posn,
        Indent = 0u
    ;
        Msg = simple_msg(SimpleContext, Components),
        MaybeContext = yes(SimpleContext),
        TreatAsFirst = treat_based_on_posn,
        Indent = 0u
    ;
        Msg = error_msg(MaybeContext, TreatAsFirst, ExtraIndent, Components),
        Indent = ExtraIndent * indent2_increment
    ;
        Msg = blank_msg(MaybeContext),
        Components = [always([blank_line])],
        TreatAsFirst = treat_based_on_posn,
        Indent = 0u
    ),
    (
        TreatAsFirst = always_treat_as_first,
        !:First = treat_as_first
    ;
        TreatAsFirst = treat_based_on_posn
        % Leave !:First as it is, even if it is treat_as_first.
    ),
    collect_msg_components(OptionTable, Components,
        cord.init, PiecesCord, !AlreadyPrintedVerbose, !IO),
    Pieces = cord.list(PiecesCord),
    should_this_msg_be_printed(LimitErrorContextsMap, !.First, Indent,
        MaybeContext, Pieces, PrintOrNot, !IO),
    (
        PrintOrNot = do_not_print(MaybeMsgPieces),
        (
            MaybeMsgPieces = no
        ;
            MaybeMsgPieces = yes(MsgPieces),
            cord.snoc(MsgPieces, !MsgsCord)
        )
    ;
        PrintOrNot = do_print(MsgPieces),
        cord.snoc(MsgPieces, !MsgsCord),
        !:MaybePrintSpec = do_print_spec
    ),
    !:First = do_not_treat_as_first,
    collect_msgs(OptionTable, LimitErrorContextsMap, Msgs, !.First,
        !MsgsCord, !MaybePrintSpec, !AlreadyPrintedVerbose, !IO).

%---------------------------------------------------------------------------%

    % Collect all the format_pieces to print out in an error_msg
    % (which contains a list of error_msg_components in the general case),
    % but do not print them yet. We take a pair of I/O states as arguments
    % *only* so that we can record in an I/O-linked mutable the fact that
    % the verbose components of some messages are not marked to be printed.
    %
:- pred collect_msg_components(option_table::in, list(error_msg_component)::in,
    cord(format_piece)::in, cord(format_piece)::out,
    already_printed_verbose::in, already_printed_verbose::out,
    io::di, io::uo) is det.

collect_msg_components(_, [], !PiecesCord, !AlreadyPrintedVerbose, !IO).
collect_msg_components(OptionTable, [Component | Components],
        !PiecesCord, !AlreadyPrintedVerbose, !IO) :-
    (
        Component = always(Pieces),
        !:PiecesCord = !.PiecesCord ++ cord.from_list(Pieces)
    ;
        Component = option_is_set(Option, MatchValue, EmbeddedComponents),
        getopt.lookup_bool_option(OptionTable, Option, OptionValue),
        ( if OptionValue = MatchValue then
            collect_msg_components(OptionTable, EmbeddedComponents,
                !PiecesCord, !AlreadyPrintedVerbose, !IO)
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
                !:PiecesCord = !.PiecesCord ++ cord.from_list(Pieces)
            ;
                AlwaysOrOnce = verbose_once,
                ( if set.contains(!.AlreadyPrintedVerbose, Pieces) then
                    true
                else
                    !:PiecesCord = !.PiecesCord ++ cord.from_list(Pieces),
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
            !:PiecesCord = !.PiecesCord ++ cord.from_list(VerbosePieces)
        ;
            VerboseErrors = no,
            !:PiecesCord = !.PiecesCord ++ cord.from_list(NonVerbosePieces),
            set_extra_error_info(some_extra_error_info, !IO)
        )
    ),
    collect_msg_components(OptionTable, Components,
        !PiecesCord, !AlreadyPrintedVerbose, !IO).

%---------------------------------------------------------------------------%

write_error_pieces_plain(Stream, Globals, Pieces, !IO) :-
    write_error_pieces_maybe_with_context(Stream, Globals, no,
        0u, Pieces, !IO).

%---------------------%

write_error_pieces(Stream, Globals, Context, Indent, Pieces, !IO) :-
    write_error_pieces_maybe_with_context(Stream, Globals, yes(Context),
        Indent, Pieces, !IO).

%---------------------%

write_error_pieces_maybe_with_context(Stream, Globals, MaybeContext, Indent,
        Pieces, !IO) :-
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    should_this_msg_be_printed(LimitErrorContextsMap, treat_as_first, Indent,
        MaybeContext, Pieces, PrintOrNot, !IO),
    (
        PrintOrNot = do_not_print(_)
    ;
        PrintOrNot = do_print(MsgPieces),
        globals.get_options(Globals, OptionTable),
        ColorDb = init_color_db(OptionTable),
        getopt.lookup_maybe_int_option(OptionTable, max_error_line_width,
            MaybeMaxWidth),
        write_msg_pieces(Stream, ColorDb, MaybeMaxWidth, MsgPieces, !IO)
    ).

%---------------------------------------------------------------------------%

:- type print_or_not
    --->    do_not_print(maybe(msg_pieces))
            % We can return a msg_pieces even if its context says it should
            % not be printed because
            %
            % - it may be part of an error_spec that has components
            %   that *should* be printed, and
            %
            % - in such cases, we want to print the *whole* error message,
            %   including even the components whose contexts fall outside the
            %   should-be-printed range.
            %
            % The obvious exception from this second point is that we do not
            % want to print components that contain nothing to print.
            % The reason why this matters is that some later decisions
            % depend on whether we have printed *something*, and our test
            % for *that* is whether we printed at least one msg_pieces or not.
    ;       do_print(msg_pieces).

:- pred should_this_msg_be_printed(limit_error_contexts_map::in,
    maybe_treat_as_first::in, uint::in, maybe(prog_context)::in,
    list(format_piece)::in, print_or_not::out, io::di, io::uo) is det.

should_this_msg_be_printed(LimitErrorContextsMap, TreatAsFirst, Indent,
        MaybeContext, Pieces, PrintOrNot, !IO) :-
    (
        Pieces = [],
        % Regardless of the context, there is nothing to print.
        PrintOrNot = do_not_print(no)
    ;
        Pieces = [HeadPiece | TailPieces],
        % We could add [nl] after TailPieces to ensure that pieces from
        % different error_msgs end up separated by a newline.
        % However, we have eventually ended up being pretty consistent
        % about adding a nl, with or without an indent delta, to the end
        % of all error_msgs anyway, so this is not needed.
        OoMPieces = one_or_more(HeadPiece, TailPieces),
        MsgPieces =
            msg_pieces(MaybeContext, TreatAsFirst, Indent, OoMPieces),
        (
            MaybeContext = no,
            PrintOrNot = do_print(MsgPieces)
        ;
            MaybeContext = yes(Context),
            Context = context(FileName, LineNumber),
            ( if
                (
                    map.search(LimitErrorContextsMap, FileName,
                        LineNumberRanges),
                    line_number_is_in_a_range(LineNumberRanges, LineNumber)
                        = no
                ;
                    % The entry for the empty filename applies to all files.
                    map.search(LimitErrorContextsMap, "", LineNumberRanges),
                    line_number_is_in_a_range(LineNumberRanges, LineNumber)
                        = no
                )
            then
                set_some_errors_were_context_limited(
                    some_errors_were_context_limited, !IO),
                PrintOrNot = do_not_print(yes(MsgPieces))
            else
                PrintOrNot = do_print(MsgPieces)
            )
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

%---------------------------------------------------------------------------%

:- type msg_pieces
    --->    msg_pieces(
                mp_context              :: maybe(prog_context),
                mp_first                :: maybe_treat_as_first,
                mp_indent               :: uint,
                % We use one_or_more instead of list because we do not want to
                % print a context at the start of a line followed by nothing.
                mp_pieces               :: one_or_more(format_piece)
            ).

:- pred write_msg_pieces(io.text_output_stream::in, color_db::in,
    maybe(int)::in, msg_pieces::in, io::di, io::uo) is det.

write_msg_pieces(Stream, ColorDb, MaybeMaxWidth, MsgPieces, !IO) :-
    MsgPieces = msg_pieces(MaybeContext, TreatAsFirst, Indent, OoMPieces),
    (
        MaybeContext = yes(Context),
        % ContextStr will include a space after the context itself.
        ContextStr = context_to_string(Context)
    ;
        MaybeContext = no,
        ContextStr = ""
    ),
    Pieces = one_or_more_to_list(OoMPieces),
    convert_pieces_to_lines(ColorDb, MaybeMaxWidth, ContextStr,
        TreatAsFirst, Indent, Pieces, PrefixStr, Lines),
    write_msg_lines(Stream, PrefixStr, Lines, !IO).

%---------------------------------------------------------------------------%
%
% Oversee the whole process of converting
%
% - format pieces to paragraphs
% - paragraphs to lines
% - lines to joined_up lines.
%

:- pred convert_pieces_to_lines(color_db::in, maybe(int)::in, string::in,
    maybe_treat_as_first::in, indent::in, list(format_piece)::in,
    string::out, list(error_line)::out) is det.

convert_pieces_to_lines(ColorDb, MaybeMaxWidth, ContextStr, TreatAsFirst,
        FixedIndent, Pieces, PrefixStr, Lines) :-
    convert_pieces_to_words(ColorDb, Pieces, Words),
    convert_words_to_paragraphs(ColorDb, Words, Paragraphs),
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
        io.write_string(StdErr, "PIECES\n", !TIO),
        list.foldl(io.write_line(StdErr), Pieces, !TIO),
        io.nl(StdErr, !TIO),
        io.write_string(StdErr, "WORDS\n", !TIO),
        list.foldl(io.write_line(StdErr), Words, !TIO),
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
        io.nl(StdErr, !TIO),
        io.flush_output(StdErr, !TIO)
    ).

%---------------------------------------------------------------------------%
%
% Convert pieces to words.
%

:- type word
    --->    word_text(text_word)
    ;       word_color(color_change)
    ;       word_nl(newline_word).

:- type text_word
    --->    plain(string)
    ;       prefix(string)
    ;       suffix(string).

:- type color_change
    --->    color_start(color_spec)
    ;       color_end.

:- type newline_word
    --->    nl
    ;       nl_delta(int)
    ;       blank_line
    ;       lp_maybe_nl_inc(string, lp_piece_kind)
    ;       maybe_nl_dec_rp(string, rp_piece_kind).

:- pred convert_pieces_to_words(color_db::in, list(format_piece)::in,
    list(word)::out) is det.

convert_pieces_to_words(ColorDb, Pieces, Words) :-
    WordsCord0 = cord.init,
    convert_pieces_to_words_acc(ColorDb, first_in_msg, do_not_change_case_next,
        Pieces, WordsCord0, WordsCord),
    Words = cord.list(WordsCord).

:- type maybe_change_case_next
    --->    do_not_change_case_next
    ;       do_lower_case_next
    ;       do_upper_case_next.

:- pred convert_pieces_to_words_acc(color_db::in, maybe_first_in_msg::in,
    maybe_change_case_next::in, list(format_piece)::in,
    cord(word)::in, cord(word)::out) is det.

convert_pieces_to_words_acc(_, _, _, [], !WordsCord).
convert_pieces_to_words_acc(ColorDb, FirstInMsg, !.CaseChange,
        [Piece | Pieces], !WordsCord) :-
    (
        ( Piece = invis_order_default_start(_, _)
        ; Piece = invis_order_default_end(_, _)
        ; Piece = treat_next_as_first
        )
    ;
        Piece = words(WordsStr),
        trace [compile_time(flag("check_words_pieces"))] (
            string.to_char_list(WordsStr, WordsChars),
            ( if
                ( WordsChars = [' ' | _]
                ; list.last(WordsChars, ' ')
                )
            then
                string.format("words with end space <%s>",
                    [s(WordsStr)], Msg),
                unexpected($pred, Msg)
            else
                true
            )
        ),
        break_into_words(WordsStr, !CaseChange, !WordsCord)
    ;
        Piece = words_quote(WordsStr),
        trace [compile_time(flag("check_words_pieces"))] (
            string.to_char_list(WordsStr, WordsChars),
            ( if
                ( WordsChars = [' ' | _]
                ; list.last(WordsChars, ' ')
                )
            then
                string.format("words_quote with end space <%s>",
                    [s(WordsStr)], Msg),
                unexpected($pred, Msg)
            else
                true
            )
        ),
        break_into_words(add_quotes(WordsStr), !CaseChange, !WordsCord)
    ;
        (
            Piece = fixed(Word),
            PlainWord = Word
        ;
            Piece = quote(Word),
            PlainWord = add_quotes(Word)
        ;
            Piece = int_fixed(Int),
            PlainWord = int_to_string(Int)
        ;
            Piece = int_name(Int),
            PlainWord = int_name_str(Int)
        ;
            Piece = nth_fixed(Int),
            PlainWord = nth_fixed_str(Int)
        ;
            Piece = p_or_f(PredOrFunc),
            PlainWord = pred_or_func_to_full_str(PredOrFunc)
        ;
            Piece = purity_desc(Purity),
            PlainWord = purity_to_string(Purity)
        ;
            Piece = a_purity_desc(Purity),
            PlainWord = a_purity_to_string(Purity)
        ;
            Piece = purity_desc_article(Purity),
            PlainWord = purity_article_to_string(Purity)
        ;
            Piece = decl(DeclName),
            PlainWord = add_quotes(":- " ++ DeclName)
        ;
            Piece = pragma_decl(PragmaName),
            PlainWord = add_quotes(":- pragma " ++ PragmaName)
        ),
        add_word_to_cord(word_text(plain(PlainWord)), !CaseChange, !WordsCord)
    ;
        (
            Piece = qual_top_ctor_of_type(Type),
            type_to_ctor_det(Type, TypeCtor),
            TypeCtor = type_ctor(SymName, Arity)
        ;
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
        add_word_to_cord(word_text(plain(Word)), !CaseChange, !WordsCord)
    ;
        (
            Piece = qual_sym_name(SymName)
        ;
            Piece = unqual_sym_name(SymName0),
            SymName = unqualified(unqualify_name(SymName0))
        ),
        Word = sym_name_to_word(SymName),
        add_word_to_cord(word_text(plain(Word)), !CaseChange, !WordsCord)
    ;
        Piece = name_arity(NameAndArity),
        Word = name_arity_to_word(NameAndArity),
        add_word_to_cord(word_text(plain(Word)), !CaseChange, !WordsCord)
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
        add_word_to_cord(word_text(plain(Word)), !CaseChange, !WordsCord)
    ;
        (
            Piece = qual_cons_id_and_maybe_arity(ConsId0),
            strip_builtin_qualifier_from_cons_id(ConsId0, ConsId)
        ;
            Piece = unqual_cons_id_and_maybe_arity(ConsId0),
            strip_module_qualifier_from_cons_id(ConsId0, ConsId)
        ),
        Word = maybe_quoted_cons_id_and_arity_to_string(ConsId),
        add_word_to_cord(word_text(plain(Word)), !CaseChange, !WordsCord)
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
        break_into_words(WordsStr, !CaseChange, !WordsCord)
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
        break_into_words(WordsStr, !CaseChange, !WordsCord)
    ;
        Piece = prefix(Word),
        add_word_to_cord(word_text(prefix(Word)), !CaseChange, !WordsCord)
    ;
        Piece = suffix(Word),
        add_word_to_cord(word_text(suffix(Word)), !CaseChange, !WordsCord)
    ;
        Piece = lower_case_next_if_not_first,
        (
            FirstInMsg = first_in_msg
        ;
            FirstInMsg = not_first_in_msg,
            !:CaseChange = do_lower_case_next
        )
    ;
        Piece = upper_case_next,
        !:CaseChange = do_upper_case_next
    ;
        (
            Piece = nl,
            Newline = nl
        ;
            Piece = nl_indent_delta(IndentDelta),
            Newline = nl_delta(IndentDelta)
        ;
            Piece = blank_line,
            Newline = blank_line
        ;
            Piece = left_paren_maybe_nl_inc(LP, LPWordKind),
            Newline = lp_maybe_nl_inc(LP, LPWordKind)
        ;
            Piece = maybe_nl_dec_right_paren(RP, RPWordKind),
            Newline = maybe_nl_dec_rp(RP, RPWordKind)
        ),
        add_word_to_cord(word_nl(Newline), !CaseChange, !WordsCord)
    ;
        Piece = not_for_general_use_start_color(ColorName),
        lookup_color_in_db(ColorDb, ColorName, MaybeColorSpec),
        (
            MaybeColorSpec = no
        ;
            MaybeColorSpec = yes(ColorSpec),
            add_word_to_cord(word_color(color_start(ColorSpec)),
                !CaseChange, !WordsCord)
        )
    ;
        Piece = not_for_general_use_end_color(ColorName),
        lookup_color_in_db(ColorDb, ColorName, MaybeColorSpec),
        (
            MaybeColorSpec = no
        ;
            MaybeColorSpec = yes(_ColorSpec),
            add_word_to_cord(word_color(color_end), !CaseChange, !WordsCord)
        )
    ),
    update_first_in_msg_after_piece(Piece, FirstInMsg, TailFirstInMsg),
    convert_pieces_to_words_acc(ColorDb, TailFirstInMsg, !.CaseChange, Pieces,
        !WordsCord).

:- pred lookup_color_in_db(color_db::in, color_name::in,
    maybe(color_spec)::out) is det.

lookup_color_in_db(ColorDb, ColorName, MaybeColorSpec) :-
    (
        ColorDb = no_color_db,
        MaybeColorSpec = no
    ;
        ColorDb = color_db(ColorSpecs),
        (
            ColorName = color_subject,
            MaybeColorSpec = ColorSpecs ^ color_spec_subject
        ;
            ColorName = color_correct,
            MaybeColorSpec = ColorSpecs ^ color_spec_correct
        ;
            ColorName = color_incorrect,
            MaybeColorSpec = ColorSpecs ^ color_spec_incorrect
        ;
            ColorName = color_inconsistent,
            MaybeColorSpec = ColorSpecs ^ color_spec_inconsistent
        ;
            ColorName = color_hint,
            MaybeColorSpec = ColorSpecs ^ color_spec_hint
        )
    ).

:- pred add_word_to_cord(word::in,
    maybe_change_case_next::in, maybe_change_case_next::out,
    cord(word)::in, cord(word)::out) is det.

add_word_to_cord(Word, !CaseChange, !WordsCord) :-
    (
        !.CaseChange = do_not_change_case_next,
        % Leave !CaseChange as it is.
        cord.snoc(Word, !WordsCord)
    ;
        ( !.CaseChange = do_lower_case_next
        ; !.CaseChange = do_upper_case_next
        ),
        (
            Word = word_text(Text),
            (
                !.CaseChange = do_lower_case_next,
                (
                    Text = plain(Str),
                    ChangedText = plain(uncapitalize_first(Str))
                ;
                    Text = prefix(Str),
                    ChangedText = prefix(uncapitalize_first(Str))
                ;
                    Text = suffix(Str),
                    ChangedText = suffix(uncapitalize_first(Str))
                )
            ;
                !.CaseChange = do_upper_case_next,
                (
                    Text = plain(Str),
                    ChangedText = plain(capitalize_first(Str))
                ;
                    Text = prefix(Str),
                    ChangedText = prefix(capitalize_first(Str))
                ;
                    Text = suffix(Str),
                    ChangedText = suffix(capitalize_first(Str))
                )
            ),
            ChangedWord = word_text(ChangedText),
            % We have changed the case of the next word; do not change the word
            % *after* the next unless asked to so by another piece.
            !:CaseChange = do_not_change_case_next
        ;
            ( Word = word_color(_)
            ; Word = word_nl(_)
            ),
            ChangedWord = Word
            % We have not yet lowered the next word, so keep !CaseChange
            % as it is.
        ),
        cord.snoc(ChangedWord, !WordsCord)
    ).

%---------------------------------------------------------------------------%
%
% Convert words to paragraphs.
%

:- type paragraph
    --->    paragraph(
                % The list of words to print in the paragraph, together
                % with any color changes, with spaces between them
                % explicitly listed at all the appropriate points,
                %
                % The list should contain at least one ssc_string element.
                % However, it should not contain two *consecutive* ssc_str
                % elements; instead, it should contain one ssc_str containing
                % their concatenation.
                list(ssc_unit),

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

:- type ssc_unit
    --->    ssc_space
    ;       ssc_str(string)
    ;       ssc_color(color_change).

:- type paren_status
    --->    paren_none
    ;       paren_lp_end    % This paragraph/line ends with a left paren.
    ;       paren_end_rp.   % Next paragraph/line starts with a right paren.

:- pred convert_words_to_paragraphs(color_db::in, list(word)::in,
    list(paragraph)::out) is det.

convert_words_to_paragraphs(ColorDb, Words, Paras) :-
    WordsCord0 = cord.init,
    ParasCord0 = cord.init,
    InWork0 = empty_slate,
    convert_words_to_paragraphs_acc(ColorDb, Words, WordsCord0, InWork0,
        ParasCord0, ParasCord),
    Paras = cord.list(ParasCord).

:- type in_work
    --->    empty_slate
    ;       in_work(
                iw_text_word        :: string,
                iw_text_word_prefix :: is_in_work_text_prefix
            ).

:- type is_in_work_text_prefix
    --->    in_work_text_is_plain
    ;       in_work_text_is_prefix
    ;       in_work_text_is_pure_suffix.

:- pred convert_words_to_paragraphs_acc(color_db::in, list(word)::in,
    cord(ssc_unit)::in, in_work::in,
    cord(paragraph)::in, cord(paragraph)::out) is det.

convert_words_to_paragraphs_acc(_, [], !.Done, !.InWork, !Paras) :-
    end_work_on_one_paragraphs_ssc_units(SSCUnits, !.Done, _, !.InWork, _),
    add_paragraph(paragraph(SSCUnits, 0, 0, paren_none), !Paras).
convert_words_to_paragraphs_acc(ColorDb, [Word | Words],
        !.Done, !.InWork, !Paras) :-
    (
        Word = word_text(TextWord),
        record_text_word(TextWord, !Done, !InWork)
    ;
        Word = word_color(ColorChange),
        % Note how with color_start, we want to put any space *before*
        % the color change, while with color_end, we want to put any space
        % *after* the color change. The objective is to enable color
        % only for the shortest span of characters to accomplish the
        % intended coloring effect. This may not matter much when the
        % output we generate gets viewed by a user, but it matters when
        % we as developers work with .err_exp files in the test directories
        % that test diagnostic outputs.
        %
        % It is simply easier to visually check whether the output of a diff
        % between an .err file containing new or updated diagnostic output
        % and the existing .err_exp file contains just the expected changes
        % if neither file contains color change escape sequences that are
        % effectively no-ops. (The addition or deletion of such a no-op
        % sequence would show up in the diff as a red herring.)
        (
            ColorChange = color_start(_),
            Done0 = !.Done,
            InWork0 = !.InWork,
            (
                InWork0 = empty_slate,
                ( if cord.is_non_empty(Done0) then
                    cord.snoc(ssc_space, !Done)
                else
                    true
                )
            ;
                InWork0 = in_work(_, InWorkTextKind),
                mark_in_work_as_done(!Done, !InWork),
                (
                    ( InWorkTextKind = in_work_text_is_plain
                    ; InWorkTextKind = in_work_text_is_pure_suffix
                    ),
                    cord.snoc(ssc_space, !Done)
                ;
                    InWorkTextKind = in_work_text_is_prefix
                )
            ),
            cord.snoc(ssc_color(ColorChange), !Done)
        ;
            ColorChange = color_end,
            mark_in_work_as_done(!Done, !InWork),
            cord.snoc(ssc_color(ColorChange), !Done)
        )
    ;
        Word = word_nl(NewlineWord),
        (
            (
                NewlineWord = nl,
                Blank = 0,
                Delta = 0
            ;
                NewlineWord = nl_delta(Delta),
                Blank = 0
            ;
                NewlineWord = blank_line,
                Blank = 1,
                Delta = 0
            ),
            end_work_on_one_paragraphs_ssc_units(SSCUnits, !Done, !InWork),
            Para = paragraph(SSCUnits, Blank, Delta, paren_none),
            add_paragraph(Para, !Paras)
        ;
            NewlineWord = lp_maybe_nl_inc(LP, LPWordKind),
            (
                LPWordKind = lp_plain,
                LPTextWord = plain(LP)
            ;
                LPWordKind = lp_suffix,
                LPTextWord = suffix(LP)
            ),
            record_text_word(LPTextWord, !Done, !InWork),
            end_work_on_one_paragraphs_ssc_units(SSCUnits, !Done, !InWork),
            add_paragraph(paragraph(SSCUnits, 0, 1, paren_lp_end), !Paras)
        ;
            NewlineWord = maybe_nl_dec_rp(RP, RPWordKind),
            end_work_on_one_paragraphs_ssc_units(SSCUnits, !Done, !InWork),
            add_paragraph(paragraph(SSCUnits, 0, -1, paren_end_rp), !Paras),
            (
                RPWordKind = rp_plain,
                RPTextWord = plain(RP)
            ;
                RPWordKind = rp_prefix,
                RPTextWord = prefix(RP)
            ),
            record_text_word(RPTextWord, !Done, !InWork)
        )
    ),
    convert_words_to_paragraphs_acc(ColorDb, Words, !.Done, !.InWork, !Paras).

:- pred record_text_word(text_word::in,
    cord(ssc_unit)::in, cord(ssc_unit)::out, in_work::in, in_work::out) is det.

record_text_word(TextWord, Done0, Done, InWork0, InWork) :-
    (
        InWork0 = empty_slate,
        Done = Done0,
        (
            TextWord = plain(Text),
            InWork = in_work(Text, in_work_text_is_plain)
        ;
            TextWord = suffix(Text),
            % There is nothing to add the suffix to.
            InWork = in_work(Text, in_work_text_is_pure_suffix)
        ;
            TextWord = prefix(Text),
            InWork = in_work(Text, in_work_text_is_prefix)
        )
    ;
        InWork0 = in_work(InWorkText0, InWorkTextKind0),
        (
            InWorkTextKind0 = in_work_text_is_plain,
            (
                TextWord = plain(Text),
                % We can't add Text to InWorkText0, so move InWorkText0
                % to Done, and make Text the new in-work text.
                add_space_if_needed(Done0, Done1),
                cord.snoc(ssc_str(InWorkText0), Done1, Done),
                InWork = in_work(Text, in_work_text_is_plain)
            ;
                TextWord = prefix(Text),
                % Do the same as for plain, but record the new in-work text
                % as prefix.
                add_space_if_needed(Done0, Done1),
                cord.snoc(ssc_str(InWorkText0), Done1, Done),
                InWork = in_work(Text, in_work_text_is_prefix)
            ;
                TextWord = suffix(Suffix),
                Done = Done0,
                InWorkText = InWorkText0 ++ Suffix,
                InWork = in_work(InWorkText, in_work_text_is_plain)
            )
        ;
            InWorkTextKind0 = in_work_text_is_pure_suffix,
            (
                TextWord = plain(Text),
                % We can't add Text to InWorkText0, so move InWorkText0
                % to Done, and make Text the new in-work text.
                cord.snoc(ssc_str(InWorkText0), Done0, Done),
                InWork = in_work(Text, in_work_text_is_plain)
            ;
                TextWord = prefix(Text),
                % Do the same as for plain, but record the new in-work text
                % as prefix.
                cord.snoc(ssc_str(InWorkText0), Done0, Done),
                InWork = in_work(Text, in_work_text_is_prefix)
            ;
                TextWord = suffix(Suffix),
                Done = Done0,
                InWorkText = InWorkText0 ++ Suffix,
                InWork = in_work(InWorkText, in_work_text_is_pure_suffix)
            )
        ;
            InWorkTextKind0 = in_work_text_is_prefix,
            Done = Done0,
            (
                TextWord = plain(Text),
                InWorkText = InWorkText0 ++ Text,
                InWork = in_work(InWorkText, in_work_text_is_plain)
            ;
                TextWord = prefix(AdditionalPrefix),
                InWorkText = InWorkText0 ++ AdditionalPrefix,
                InWork = in_work(InWorkText, in_work_text_is_prefix)
            ;
                TextWord = suffix(Text),
                InWorkText = InWorkText0 ++ Text,
                % prefix + suffix is not PURE suffix
                InWork = in_work(InWorkText, in_work_text_is_plain)
            )
        )
    ).

:- pred mark_in_work_as_done(cord(ssc_unit)::in, cord(ssc_unit)::out,
    in_work::in, in_work::out) is det.

mark_in_work_as_done(Done0, Done, InWork0, InWork) :-
    (
        InWork0 = empty_slate,
        Done = Done0
    ;
        InWork0 = in_work(InWorkText0, InWorkTextKind0),
        (
            ( InWorkTextKind0 = in_work_text_is_plain
            ; InWorkTextKind0 = in_work_text_is_prefix
            ),
            add_space_if_needed(Done0, Done1)
        ;
            InWorkTextKind0 = in_work_text_is_pure_suffix,
            % If this suffix were preceded by other non-suffix text,
            % it would have been glued to its end, and the InWorkTextKind0
            % would have been changed to in_work_text_is_plain. So the
            % ssc_unit preceding it would be a color change. We want
            % the suffix to be printed immediately after thet color change.
            Done1 = Done0
        ),
        cord.snoc(ssc_str(InWorkText0), Done1, Done)
    ),
    InWork = empty_slate.

:- pred add_space_if_needed(cord(ssc_unit)::in, cord(ssc_unit)::out) is det.

add_space_if_needed(Done0, Done) :-
    ( if cord.get_last(Done0, Last) then
        (
            Last = ssc_str(_),
            cord.snoc(ssc_space, Done0, Done)
        ;
            Last = ssc_space,
            Done = Done0
        ;
            Last = ssc_color(ColorChange),
            (
                ColorChange = color_start(_),
                Done = Done0
            ;
                ColorChange = color_end,
                cord.snoc(ssc_space, Done0, Done)
            )
        )
    else
        % There is no last ssc_unit, so visual separation is not needed.
        Done = Done0
    ).

    % Return the ssc_units we have gathered so far for inclusion in a
    % new paragraph, and set up  Done and InWork for starting work
    % on the next paragraph.
    %
:- pred end_work_on_one_paragraphs_ssc_units(list(ssc_unit)::out,
    cord(ssc_unit)::in, cord(ssc_unit)::out, in_work::in, in_work::out) is det.

end_work_on_one_paragraphs_ssc_units(SSCUnits, Done0, Done, InWork0, InWork) :-
    mark_in_work_as_done(Done0, Done1, InWork0, InWork),
    SSCUnits = cord.list(Done1),
    Done = cord.init.

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

:- pred break_into_words(string::in,
    maybe_change_case_next::in, maybe_change_case_next::out,
    cord(word)::in, cord(word)::out) is det.

break_into_words(String, !CaseChange, !WordsCord) :-
    break_into_words_from(String, 0, !CaseChange, !WordsCord).

:- pred break_into_words_from(string::in, int::in,
    maybe_change_case_next::in, maybe_change_case_next::out,
    cord(word)::in, cord(word)::out) is det.

break_into_words_from(String, Cur, !CaseChange, !WordsCord) :-
    ( if find_word_start(String, Cur, Start) then
        find_word_end(String, Start, End),
        string.between(String, Start, End, WordStr),
        add_word_to_cord(word_text(plain(WordStr)), !CaseChange, !WordsCord),
        break_into_words_from(String, End, !CaseChange, !WordsCord)
    else
        true
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

:- pred group_nonfirst_line_words(int::in, ssc_unit::in, list(ssc_unit)::in,
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

:- pred get_line_of_words(int::in, ssc_unit::in, list(ssc_unit)::in,
    indent::in, int::out, list(string)::out, list(ssc_unit)::out,
    color_stack::in, color_stack::out, line_end_reset::out) is det.

get_line_of_words(AvailLen, FirstSSCUnit, LaterSSCUnits0, Indent, LineWordsLen,
        LineStrs, RestSSCUnits, !ColorStack, LineEndReset) :-
    AvailLeft = AvailLen - uint.cast_to_int(Indent * indent2_increment),
    (
        FirstSSCUnit = ssc_space,
        LaterSSCUnits1 = LaterSSCUnits0,
        LenSoFar = 0,
        LineStrsCord0 = cord.init,
        LineEndReset1 = line_end_reset_nothing
        % This shouldn't happen, but we don't put spaces at the start of
        % a line, so ignore it if it does happen.
    ;
        FirstSSCUnit = ssc_str(FirstStr),
        LineStrsCord0 = cord.singleton(FirstStr),
        ( if stack.is_empty(!.ColorStack) then
            LineEndReset1 = line_end_reset_nothing
        else
            % If the initial stack contains a color, then we need to reset
            % colors at the end of the line even if the line itself
            % contains no color changes.
            LineEndReset1 = line_end_reset_color
        ),
        string.count_code_points(FirstStr, LenSoFar),
        LaterSSCUnits1 = LaterSSCUnits0
    ;
        FirstSSCUnit = ssc_color(_),
        LenSoFar = 0,
        LineEndReset1 = line_end_reset_color,
        merge_adjacent_color_changes(FirstSSCUnit,
            LaterSSCUnits0, LaterSSCUnits1, !ColorStack),
        FirstStr = top_color_to_string(!.ColorStack),
        LineStrsCord0 = cord.singleton(FirstStr)
    ),
    get_later_words(AvailLeft, LaterSSCUnits1, LenSoFar,
        LineWordsLen, LineStrsCord0, LineStrsCord, RestSSCUnits,
        !ColorStack, LineEndReset1, LineEndReset),
    LineStrs = cord.list(LineStrsCord).

:- pred get_later_words(int::in, list(ssc_unit)::in,
    int::in, int::out, cord(string)::in, cord(string)::out,
    list(ssc_unit)::out, color_stack::in, color_stack::out,
    line_end_reset::in, line_end_reset::out) is det.

get_later_words(_, [], CurLen, FinalLen,
        LineStrs, LineStrs, [], !ColorStack, LineEndReset, LineEndReset) :-
    FinalLen = CurLen.
get_later_words(Avail, [SSCUnit | SSCUnits0], CurLen, FinalLen,
        LineStrs0, LineStrs, RestSSCUnits, !ColorStack,
        LineEndReset0, LineEndReset) :-
    ColorStack0 = !.ColorStack,
    (
        SSCUnit = ssc_space,
        PeekWordLen = peek_and_find_len_of_next_word(SSCUnits0),
        % Check whether the next actual word fits on the line.
        ( if CurLen + 1 + PeekWordLen =< Avail then
            % It does, so let the recursive call proceed.
            cord.snoc(" ", LineStrs0, LineStrs1),
            get_later_words(Avail, SSCUnits0, CurLen + 1, FinalLen,
                LineStrs1, LineStrs, RestSSCUnits, !ColorStack,
                LineEndReset0, LineEndReset)
        else
            % It does not, so force the check against Avail to fail *now*,
            % *not* in the recursive call, so that we change the color
            % just before the next word.
            SSCUnits = SSCUnits0,
            merge_adjacent_color_ends(SSCUnits, !.ColorStack, MaybeEndResult),
            (
                MaybeEndResult = no,
                FinalLen = CurLen,
                LineStrs = LineStrs0,
                % Throw away the space, since there is no need for it at the
                % start of the next line (if there is one).
                RestSSCUnits = SSCUnits0,
                % There are no changes to !ColorStack.
                LineEndReset = LineEndReset0
            ;
                MaybeEndResult = yes({RestSSCUnits, !:ColorStack}),
                % Throw away the space, since there is no need for it at the
                % start of the next line (if there is one), *and* all the
                % initial color_ends in SSCUnits0, since we include their
                % cumulative effect here.
                ColorStr = top_color_to_string(!.ColorStack),
                cord.snoc(ColorStr, LineStrs0, LineStrs),
                % The color reset takes zero columns.
                FinalLen = CurLen,
                LineEndReset = line_end_reset_color
            )
        )
    ;
        SSCUnit = ssc_str(Str),
        LineEndReset1 = LineEndReset0,
        string.count_code_points(Str, StrLen),
        NextLen = CurLen + StrLen,
        % Add Str to the current line only if there is space on there
        % for any suffix string that wasn't merged into Str due to
        % some color changes between them.
        AnySuffixLen = peek_and_find_len_of_next_suffix(SSCUnits0),
        CheckLen = NextLen + AnySuffixLen,
        ( if append_to_current_line(Avail, CurLen, CheckLen) then
            cord.snoc(Str, LineStrs0, LineStrs1),
            get_later_words(Avail, SSCUnits0, NextLen, FinalLen,
                LineStrs1, LineStrs, RestSSCUnits, !ColorStack,
                LineEndReset1, LineEndReset)
        else
            FinalLen = CurLen,
            LineStrs = LineStrs0,
            RestSSCUnits = [SSCUnit | SSCUnits0],
            % There were no changes to !ColorStack.
            LineEndReset = LineEndReset0
        )
    ;
        SSCUnit = ssc_color(ColorChange),
        (
            ColorChange = color_start(_),
            merge_adjacent_color_changes(SSCUnit, SSCUnits0, SSCUnits1,
                !ColorStack),
            ColorStr = top_color_to_string(!.ColorStack),
            PeekWordLen = peek_and_find_len_of_next_word(SSCUnits1),
            % Check whether the next actual word fits on the line.
            ( if CurLen + PeekWordLen =< Avail then
                % It does, so let the recursive call proceed.
                cord.snoc(ColorStr, LineStrs0, LineStrs1),
                LineEndReset1 = line_end_reset_color,
                get_later_words(Avail, SSCUnits1, CurLen, FinalLen,
                    LineStrs1, LineStrs, RestSSCUnits, !ColorStack,
                    LineEndReset1, LineEndReset)
            else
                % It does not, so force the check against Avail to fail *now*,
                % *not* in the recursive call, so that we change the color
                % just before the next word.
                FinalLen = CurLen,
                LineStrs = LineStrs0,
                RestSSCUnits = [SSCUnit | SSCUnits0],
                !:ColorStack = ColorStack0,
                LineEndReset = LineEndReset0
            )
        ;
            ColorChange = color_end,
            merge_adjacent_color_ends([SSCUnit | SSCUnits0], !.ColorStack,
                MaybeEndResult),
            (
                MaybeEndResult = no,
                unexpected($pred, "MaybeEndResult = no")
            ;
                MaybeEndResult = yes({SSCUnits1, !:ColorStack}),
                ColorStr = top_color_to_string(!.ColorStack),
                cord.snoc(ColorStr, LineStrs0, LineStrs1),
                LineEndReset1 = line_end_reset_color,

                PeekWordLen = peek_and_find_len_of_next_word(SSCUnits1),
                ( if CurLen + PeekWordLen =< Avail then
                    get_later_words(Avail, SSCUnits1, CurLen, FinalLen,
                        LineStrs1, LineStrs, RestSSCUnits, !ColorStack,
                        LineEndReset1, LineEndReset)
                else
                    FinalLen = CurLen,  % The color reset uses zero columns.
                    RestSSCUnits = SSCUnits1,
                    LineStrs = LineStrs1,
                    LineEndReset = LineEndReset1
                )
            )
        )
    ).

:- pred append_to_current_line(int::in, int::in, int::in) is semidet.

append_to_current_line(Avail, CurLen, NextLen) :-
    % Include the new string on the current line if either ...
    (
        NextLen =< Avail
        % ... there is room for it, or ...
    ;
        CurLen = 0
        % ... if the new string is too long for a line overall
        % (since its length must be NextLen - CurLen, and CurLen = 0).
        %
        % Before we added support for color, this latter condition
        % could happen only in get_line_of_words, but now, that
        % predicate can put a color change on the line (which takes up
        % zero columns) and call *us* with a too-long-to-fit sc_str unit.
    ).

    % If the given list of SSCUnits starts with a color_end, then
    % process all the color_ends adjacent to it, and return the remaining
    % scc_inits and the updated color stack. Otherwise, fail.
    %
:- pred merge_adjacent_color_ends(list(ssc_unit)::in, color_stack::in,
    maybe({list(ssc_unit), color_stack})::out) is det.

merge_adjacent_color_ends(SSCUnits, ColorStack0, MaybeResult) :-
    (
        SSCUnits = [],
        MaybeResult = no
    ;
        SSCUnits = [HeadSSCUnit | TailSSCUnits],
        (
            ( HeadSSCUnit = ssc_space
            ; HeadSSCUnit = ssc_str(_)
            ),
            MaybeResult = no
        ;
            HeadSSCUnit = ssc_color(ColorChange),
            (
                ColorChange = color_start(_),
                MaybeResult = no
            ;
                ColorChange = color_end,
                pop_stack_ignore_empty(ColorStack0, ColorStack1),
                merge_adjacent_color_ends(TailSSCUnits, ColorStack1,
                    TailResult),
                (
                    TailResult = yes(_),
                    MaybeResult = TailResult
                ;
                    TailResult = no,
                    MaybeResult = yes({TailSSCUnits, ColorStack1})
                )
            )
        )
    ).

:- inst color_unit for ssc_unit/0
    --->    ssc_color(ground).

:- pred merge_adjacent_color_changes(ssc_unit::in(color_unit),
    list(ssc_unit)::in, list(ssc_unit)::out,
    color_stack::in, color_stack::out) is det.

merge_adjacent_color_changes(HeadSSCUnit, TailSSCUnits0, SSCUnits,
        !ColorStack) :-
    HeadSSCUnit = ssc_color(ColorChange),
    (
        ColorChange = color_start(Color),
        stack.push(Color, !ColorStack)
    ;
        ColorChange = color_end,
        pop_stack_ignore_empty(!ColorStack)
    ),
    (
        TailSSCUnits0 = [],
        SSCUnits = []
    ;
        TailSSCUnits0 = [HeadTailSSCUnit0 | TailTailSSCUnits0],
        (
            ( HeadTailSSCUnit0 = ssc_space
            ; HeadTailSSCUnit0 = ssc_str(_)
            ),
            SSCUnits = TailSSCUnits0
        ;
            HeadTailSSCUnit0 = ssc_color(_),
            merge_adjacent_color_changes(HeadTailSSCUnit0, TailTailSSCUnits0,
                SSCUnits, !ColorStack)
        )
    ).

:- func peek_and_find_len_of_next_word(list(ssc_unit)) = int.

peek_and_find_len_of_next_word([]) = 0.
peek_and_find_len_of_next_word([SSCUnit | SSCUnits]) = Len :-
    (
        SSCUnit = ssc_str(Str),
        string.count_code_points(Str, Len)
    ;
        SSCUnit = ssc_space,
        TailLen = peek_and_find_len_of_next_word(SSCUnits),
        % The space itself consumes one column.
        Len = 1 + TailLen
    ;
        SSCUnit = ssc_color(_),
        % The color change itself consumes zero columns.
        Len = peek_and_find_len_of_next_word(SSCUnits)
    ).

:- func peek_and_find_len_of_next_suffix(list(ssc_unit)) = int.

peek_and_find_len_of_next_suffix([]) = 0.
peek_and_find_len_of_next_suffix([SSCUnit | SSCUnits]) = Len :-
    (
        SSCUnit = ssc_str(Str),
        string.count_code_points(Str, Len)
    ;
        SSCUnit = ssc_space,
        Len = 0
    ;
        SSCUnit = ssc_color(_),
        % The color change itself consumes zero columns.
        Len = peek_and_find_len_of_next_suffix(SSCUnits)
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
    ;       color_db(color_specs).
            % Do use color, using the given color specifications.

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
            ColorDb = color_db(ColorSpecs)
        )
    ).

    % The terminal control codes we use here are described by
    % https://en.wikipedia.org/wiki/ANSI_escape_code#Colors.
    %
:- func color_to_string(color) = string.

color_to_string(SetOrReset) = Str :-
    (
        SetOrReset = color_set(Color),
        (
            Color = color_8bit(ColorNum),
            string.format("\e[38;5;%um", [u8(ColorNum)], Str)
        ;
            Color = color_24bit(R, G, B),
            string.format("\e[38;2;%u;%u;%um", [u8(R), u8(G), u8(B)], Str)
        )
    ;
        SetOrReset = color_reset,
        Str = "\e[39;49m"
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
        ; Piece = upper_case_next
        ; Piece = nl
        ; Piece = nl_indent_delta(_)
        ; Piece = not_for_general_use_start_color(_)
        ; Piece = not_for_general_use_end_color(_)
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
        ; Piece = purity_desc_article(_)
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

:- func purity_to_string(purity) = string.

purity_to_string(purity_pure) = "pure".
purity_to_string(purity_semipure) = "semipure".
purity_to_string(purity_impure) = "impure".

:- func a_purity_to_string(purity) = string.

a_purity_to_string(purity_pure) = "a pure".
a_purity_to_string(purity_semipure) = "a semipure".
a_purity_to_string(purity_impure) = "an impure".

:- func purity_article_to_string(purity) = string.

purity_article_to_string(purity_pure) = "a".        % pure".
purity_article_to_string(purity_semipure) = "a".    % semipure".
purity_article_to_string(purity_impure) = "an".     % impure".

:- pred pop_stack_ignore_empty(stack(T)::in, stack(T)::out) is det.

pop_stack_ignore_empty(Stack0, Stack) :-
    ( if stack.pop(_OldTopItem, Stack0, StackPrime) then
        Stack = StackPrime
    else
        Stack = Stack0
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

maybe_write_out_errors(Stream, Verbose, Globals, !Specs, !IO) :-
    (
        Verbose = no
    ;
        Verbose = yes,
        write_error_specs(Stream, Globals, !.Specs, !IO),
        !:Specs = []
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Have we written out one or more error_specs?
    % We need this into to print out diagnostics about bad color schemes
    % only if a non-bad color scheme could have had an effect.
    %
    % Note that calls to the various versions of write_error_pieces,
    % which do not pass a full error_spec to this module, do not count here.
    % This is because they have been obsolete since *before* we supported
    % colors, and therefore don't contain anything that a color scheme
    % could affect.
    %
:- mutable(wrote_something, bool, no, ground,
    [thread_local, untrailed, attach_to_io_state]).

%---------------------%

    % A list of the informational messages generated by the predicate
    % record_color_scheme_in_options in globals.m when given a color scheme
    % that it cannot parse properly.
    %
:- mutable(bad_color_schemes, list(error_spec), [], ground,
    [thread_local, untrailed, attach_to_io_state]).

record_bad_color_scheme(Spec, !IO) :-
    get_bad_color_schemes(BadColorSchemes0, !IO),
    BadColorSchemes = [Spec | BadColorSchemes0],
    set_bad_color_schemes(BadColorSchemes, !IO).

%---------------------%

:- type maybe_extra_error_info
    --->    no_extra_error_info
    ;       some_extra_error_info.

    % Is there extra information about errors available that could be printed
    % out if `-E' were enabled?
    %
:- mutable(extra_error_info,
    maybe_extra_error_info, no_extra_error_info, ground,
    [thread_local, untrailed, attach_to_io_state]).

%---------------------%

:- type context_limited_errors
    --->    no_errors_were_context_limited
    ;       some_errors_were_context_limited.

    % Is there extra information about errors available that could be printed
    % if the values of --limit-error-contexts options allowed it?
    %
:- mutable(some_errors_were_context_limited,
    context_limited_errors, no_errors_were_context_limited, ground,
    [thread_local, untrailed, attach_to_io_state]).

%---------------------------------------------------------------------------%

maybe_print_delayed_error_messages(ErrorStream, Globals, !IO) :-
    % Pick up the values of these flags, and then reset them
    % for the next module.
    get_wrote_something(WroteSomething, !IO),
    set_wrote_something(no, !IO),
    get_bad_color_schemes(BadColorSchemeSpecs0, !IO),
    set_bad_color_schemes([], !IO),
    get_some_errors_were_context_limited(Limited, !IO),
    set_some_errors_were_context_limited(no_errors_were_context_limited, !IO),
    get_extra_error_info(ExtraErrorInfo, !IO),
    set_extra_error_info(no_extra_error_info, !IO),

    (
        WroteSomething = no
    ;
        WroteSomething = yes,
        list.sort_and_remove_dups(BadColorSchemeSpecs0, BadColorSchemeSpecs),
        write_error_specs(ErrorStream, Globals, BadColorSchemeSpecs, !IO)
    ),

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
