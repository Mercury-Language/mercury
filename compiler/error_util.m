%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: error_util.m.
% Main author: zs.
%
% This module contains code that can be helpful in the formatting of
% error messages.
%
% Given a context, a starting indentation level and a list of words,
% print an error message that looks like this:
%
% module.m:10: first line of error message blah blah blah
% module.m:10:   second line of error message blah blah blah
% module.m:10:   third line of error message blah blah blah
%
% The words will be packed into lines as tightly as possible,
% with spaces between each pair of words, subject to the constraints
% that every line starts with a context, followed by Indent+1 spaces
% on the first line and Indent+3 spaces on later lines, and that every
% line contains at most 79 characters (unless a long single word
% forces the line over this limit).
%
% The caller supplies the list of words to be printed in the form
% of a list of error message components. Each component may specify
% a string to printed exactly as it is, or it may specify a string
% containing a list of words, which may be broken at white space.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.error_util.
:- interface.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

% Every distinct problem should generate a single error specification. This
% specification should state the severity of the problem (so that we can update
% the exit status of the compiler accordingly), which phase of the compiler
% found the problem (since later phases may wish to suppress some problem
% reports if some specific earlier phases found problems, e.g. when a missing
% clause could be caused by a syntax error), and a specification of what to
% print.
%
% In most cases, the "what to print" will be a single message for a single
% context. However, we may want to print messages for several contexts.
% For example, when reporting a duplicate declaration, we want to report
% this fact in the duplicate declaration's context, while printing another
% message giving the original declaration's context.

:- type error_spec
    --->    error_spec(
                error_severity          :: error_severity,
                error_phase             :: error_phase,
                error_msgs              :: list(error_msg)
            ).

:- type error_severity
    --->    severity_error
            % Always set the exit status to indicate an error.

    ;       severity_warning
            % Only set the exit status to indicate an error if --halt-at-warn
            % is enabled.

    ;       severity_informational
            % Don't set the exit status to indicate an error.

    ;       severity_conditional(
            % If the given boolean option has the given value, then the actual
            % severity is given by the third argument; if it has the other
            % value, then the actual severity is given by the fourth argument.
            % If the fourth argument is `no', then the error_spec shouldn't
            % actually print anything if cond_option doesn't have the value
            % in cond_option_value.

                cond_option             :: option,
                cond_option_value       :: bool,
                cond_if_match           :: error_severity,
                cond_if_no_match        :: maybe(error_severity)
            ).

:- type actual_severity
    --->    actual_severity_error
    ;       actual_severity_warning
    ;       actual_severity_informational.

:- type mode_report_control
    --->    report_in_any_mode
    ;       report_only_if_in_all_modes.

:- type error_phase
    --->    phase_read_files
    ;       phase_term_to_parse_tree
    ;       phase_parse_tree_to_hlds
    ;       phase_expand_types
    ;       phase_type_check
    ;       phase_mode_check
    ;       phase_purity_check
    ;       phase_detism_check
    ;       phase_simplify(mode_report_control)
    ;       phase_dead_code
    ;       phase_termination_analysis
    ;       phase_accumulator_intro
    ;       phase_interface_gen
    ;       phase_code_gen.

% An error message may have several components that may be printed under
% different circumstances. Some components are always printed; some are
% printed only if specific options have specific values. When an error
% specification is printed, we concatenate the list of all the
% format_components that should be printed. If this yields the empty list,
% we print nothing. Otherwise, we give them all to write_error_pieces to print
% out.
%
% When we print an error message in a list of error messages, we normally
% treat the first line of the first message differently than the rest:
% we separate it from the context by one space, whereas following lines
% are separate by three spaces. You can request that the first line of
% a message be treated as it were the first by setting the error_treat_as_first
% field to "yes". You can also request that the pieces in a message be given
% extra indentation by setting the error_extra_indent field to a nonzero value.
%
% The term simple_msg(Context, Components) is a shorthand for (and equivalent
% in every respect to) the term error_msg(yes(Context), no, 0, Components).

:- type error_msg
    --->    simple_msg(
                simple_context          :: prog_context,
                simple_components       :: list(error_msg_component)
            )
    ;       error_msg(
                error_context           :: maybe(prog_context),
                error_treat_as_first    :: bool,
                error_extra_indent      :: int,
                error_components        :: list(error_msg_component)
            ).

:- type error_msg_component
    --->    always(format_components)
            % Print these components under all circumstances.

    ;       option_is_set(option, bool, list(error_msg_component))
            % Print the embedded components only if the specified boolean
            % option has the specified value.

    ;       verbose_only(format_components)
            % Print these components only if --verbose-errors is specified.
            % If it is not specified, set the flag that triggers the printing
            % of the message reminding the user about --verbose-errors.

    ;       verbose_and_nonverbose(format_components, format_components)
            % If --verbose-errors is specified, print the first set of
            % components. If it is not specified, print the second set,
            % and set the flag that triggers the printing of the message
            % reminding the user about --verbose-errors.

    ;       print_anything(pred(io, io)).
            % This alternative allows the caller to specify an arbitrary thing
            % to be printed at any point in the sequence. Since things printed
            % this way aren't formatted as error messages should be (context
            % at start etc), this capability is intended only for messages
            % that help debug the compiler itself.

%-----------------------------------------------------------------------------%

    % Return the worst of two actual severities.
    %
:- func worst_severity(actual_severity, actual_severity)
    = actual_severity.

    % Compute the actual severity of a message with the given severity
    % (if it actually prints anything).
    %
:- func actual_error_severity(globals, error_severity)
    = maybe(actual_severity).

    % Compute the worst actual severity (if any) occurring a list ofmessages.
    %
:- func worst_severity_in_specs(globals, list(error_spec))
    = maybe(actual_severity).

    % Return `yes' if the given list contains error_specs whose actual severity
    % is actual_severity_error.
    %
:- func contains_errors(globals, list(error_spec)) = bool.

    % Return `yes' if the given list contains error_specs whose actual severity
    % is actual_severity_error or actual_severity_warning.
    %
:- func contains_errors_and_or_warnings(globals, list(error_spec)) = bool.

%-----------------------------------------------------------------------------%

:- pred sort_error_specs(list(error_spec)::in, list(error_spec)::out) is det.

:- pred sort_error_msgs(list(error_msg)::in, list(error_msg)::out) is det.

%-----------------------------------------------------------------------------%

    % write_error_spec(Spec, Globals, !NumWarnings, !NumErrors, !IO):
    % write_error_specs(Specs, Globals, !NumWarnings, !NumErrors, !IO):
    %
    % Write out the error message(s) specified by Spec or Specs, minus the
    % parts whose conditions are false. Increment !NumWarnings by the number
    % of printed warnings and !NumErrors by the number of printed errors.
    % Set the exit status to 1 if we found any errors, or if we found any
    % warnings and --halt-at-warn is set. If some error specs have verbose
    % components but they aren't being printed out, set the flag for reminding
    % the user about --verbose-errors.
    %
    % Look up option values in the supplied Globals.
    %
:- pred write_error_spec(error_spec::in, globals::in, int::in, int::out,
    int::in, int::out, io::di, io::uo) is det.
:- pred write_error_specs(list(error_spec)::in, globals::in, int::in, int::out,
    int::in, int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type format_component
    --->    fixed(string)   % This string should appear in the output
                            % in one piece, as it is.
    
    ;       quote(string)   % Surround the string with `' quotes, then treat
                            % as fixed.

    ;       int_fixed(int)  % Convert the integer to a string, then treat
                            % as fixed.

    ;       prefix(string)  % This string should appear in the output
                            % in one piece, as it is, inserted directly
                            % before the next format_component, without
                            % any intervening space.

    ;       suffix(string)  % This string should appear in the output
                            % in one piece, as it is, appended directly
                            % after the previous format_component, without
                            % any intervening space.
            
    ;       words(string)   % This string contains words separated by
                            % white space. The words should appear in
                            % the output in the given order, but the
                            % white space may be rearranged and line
                            % breaks may be inserted.

    ;       sym_name(sym_name)
                            % The output should contain the string form of
                            % the sym_name, surrounded by `' quotes.

    ;       sym_name_and_arity(sym_name_and_arity)
                            % The output should contain the string form of
                            % the sym_name, followed by '/' and the arity,
                            % all surrounded by `' quotes.

    ;       top_ctor_of_type(mer_type)
                            % The top level type constructor of the given type,
                            % which must have one (i.e. must not be a
                            % variable).

    ;       p_or_f(pred_or_func)
                            % Output the string "predicate" or "function"
                            % as appropriate.

    ;       simple_call(simple_call_id)
                            % Output the identity of the given call.

    ;       nl              % Insert a line break if there has been text
                            % output since the last line break.

    ;       nl_indent_delta(int)
                            % Act as nl, but also add the given integer
                            % (which should be a small positive or negative
                            % integer) to the current indent level.
    ;       blank_line.
                            % Create a blank line.

:- type format_components == list(format_component).

    % Wrap words() around a string.
    %
:- func string_to_words_piece(string) = format_component.

    % Convert a list of strings into a list of format_components
    % separated by commas, with the last two elements separated by `and'.
    %
:- func list_to_pieces(list(string)) = list(format_component).

    % Convert a list of lists of format_components into a list of
    % format_components separated by commas, with the last two elements
    % separated by `and'.
    %
:- func component_lists_to_pieces(list(list(format_component))) =
    list(format_component).

    % Convert a list of format_components into a list of format_components
    % separated by commas, with the last two elements separated by `and'.
    %
:- func component_list_to_pieces(list(format_component)) =
    list(format_component).

    % component_list_to_line_pieces(Lines, Final):
    %
    % Convert Lines, a list of lines (each given by a list of format_components
    % *without* a final nl) into a condensed list of format_components
    % in which adjacent lines are separated by commas and newlines.
    % What goes between the last line and the newline ending is not
    % a comma but the value of Final.
    %
:- func component_list_to_line_pieces(list(list(format_component)),
    list(format_component)) = list(format_component).

    % choose_number(List, Singular, Plural) = Form
    %
    % Choose between a singular version and a plural version of something,
    % based on the length of a list.  Chooses the plural if the list is empty.
    %
:- func choose_number(list(T), U, U) = U.

    % is_or_are(List) returns "is" if the list is singleton, an exception
    % if the list is empty, otherwise it returns "are"
:- func is_or_are(list(T)) = string.

%-----------------------------------------------------------------------------%

% XXX The predicates below should not be called in new code. New code should
% create error specifications, and then call write_error_spec to print them.

    % Display the given error message, without a context and with standard
    % indentation.
    %
:- pred write_error_pieces_plain(list(format_component)::in,
    io::di, io::uo) is det.

    % write_error_plain_with_progname(ProgName, Msg):
    %
    % Display Msg as the error string, with ProgName as a context
    % and with standard indentation.
    %
:- pred write_error_plain_with_progname(string::in, string::in,
    io::di, io::uo) is det.

    % write_error_pieces(Context, Indent, Components):
    %
    % Display `Components' as the error message, with `Context' as a context
    % and indent by `Indent'.
    %
:- pred write_error_pieces(prog_context::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.

    % Display the given error message, but indent the first line.
    % This is useful when adding extra lines to an already displayed message.
    %
:- pred write_error_pieces_not_first_line(prog_context::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.

    % Display the given error message. The bool is true iff
    % this is the first line.
    %
:- pred write_error_pieces_maybe_first_line(bool::in, prog_context::in,
    int::in, list(format_component)::in, io::di, io::uo) is det.

:- pred write_error_pieces_maybe_with_context(maybe(prog_context)::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.

:- func error_pieces_to_string(list(format_component)) = string.

%-----------------------------------------------------------------------------%

:- func describe_sym_name(sym_name) = string.

:- func describe_sym_name_and_arity(sym_name_and_arity) = string.

:- func pred_or_func_to_string(pred_or_func) = string.

    % Put `' quotes around the given string.
    %
:- func add_quotes(string) = string.

    % Ensure that the first character of the input string is not a lower case
    % letter.
    %
:- func capitalize(string) = string.

    % Report a warning, and set the exit status to error if the
    % --halt-at-warn option is set.
    %
:- pred report_warning(prog_context::in, int::in, list(format_component)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Report why the file is not able to be opened,
    % and set the exit status to be 1.
    %
:- pred unable_to_open_file(string::in, io.error::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module libs.compiler_util.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

worst_severity(actual_severity_error, actual_severity_error) =
    actual_severity_error.
worst_severity(actual_severity_error, actual_severity_warning) =
    actual_severity_error.
worst_severity(actual_severity_error, actual_severity_informational) =
    actual_severity_error.
worst_severity(actual_severity_warning, actual_severity_error) =
    actual_severity_error.
worst_severity(actual_severity_warning, actual_severity_warning) =
    actual_severity_warning.
worst_severity(actual_severity_warning, actual_severity_informational) =
    actual_severity_warning.
worst_severity(actual_severity_informational, actual_severity_error) =
    actual_severity_error.
worst_severity(actual_severity_informational, actual_severity_warning) =
    actual_severity_warning.
worst_severity(actual_severity_informational, actual_severity_informational) =
    actual_severity_informational.

actual_error_severity(Globals, Severity) = MaybeActual :-
    (
        Severity = severity_error,
        MaybeActual = yes(actual_severity_error)
    ;
        Severity = severity_warning,
        MaybeActual = yes(actual_severity_warning)
    ;
        Severity = severity_informational,
        MaybeActual = yes(actual_severity_informational)
    ;
        Severity = severity_conditional(Option, MatchValue,
            Match, MaybeNoMatch),
        globals.lookup_bool_option(Globals, Option, Value),
        ( Value = MatchValue ->
            MaybeActual = actual_error_severity(Globals, Match)
        ;
            (
                MaybeNoMatch = no,
                MaybeActual = no
            ;
                MaybeNoMatch = yes(NoMatch),
                MaybeActual = actual_error_severity(Globals, NoMatch)
            )
        )
    ).

worst_severity_in_specs(Globals, Specs) = MaybeWorst :-
    worst_severity_in_specs_2(Globals, Specs, no, MaybeWorst).

:- pred worst_severity_in_specs_2(globals::in, list(error_spec)::in,
    maybe(actual_severity)::in, maybe(actual_severity)::out) is det.

worst_severity_in_specs_2(_Globals, [], !MaybeWorst).
worst_severity_in_specs_2(Globals, [Spec | Specs], !MaybeWorst) :-
    Spec = error_spec(Severity, _, _),
    MaybeThis = actual_error_severity(Globals, Severity),
    (
        !.MaybeWorst = no,
        !:MaybeWorst = MaybeThis
    ;
        !.MaybeWorst = yes(_Worst),
        MaybeThis = no
    ;
        !.MaybeWorst = yes(Worst),
        MaybeThis = yes(This),
        !:MaybeWorst = yes(worst_severity(Worst, This))
    ),
    worst_severity_in_specs_2(Globals, Specs, !MaybeWorst).

contains_errors(Globals, Specs) = Errors :-
    MaybeWorstActual = worst_severity_in_specs(Globals, Specs),
    (
        MaybeWorstActual = no,
        Errors = no
    ;
        MaybeWorstActual = yes(WorstActual),
        (
            WorstActual = actual_severity_error,
            Errors = yes
        ;
            ( WorstActual = actual_severity_warning
            ; WorstActual = actual_severity_informational
            ),
            Errors = no
        )
    ).

contains_errors_and_or_warnings(Globals, Specs) = ErrorsOrWarnings :-
    MaybeWorstActual = worst_severity_in_specs(Globals, Specs),
    (
        MaybeWorstActual = no,
        ErrorsOrWarnings = no
    ;
        MaybeWorstActual = yes(WorstActual),
        (
            ( WorstActual = actual_severity_error
            ; WorstActual = actual_severity_warning
            ),
            ErrorsOrWarnings = yes
        ;
            WorstActual = actual_severity_informational,
            ErrorsOrWarnings = no
        )
    ).

%-----------------------------------------------------------------------------%

sort_error_specs(Specs0, Specs) :-
    list.sort_and_remove_dups(compare_error_specs, Specs0, Specs).

sort_error_msgs(Msgs0, Msgs) :-
    list.sort_and_remove_dups(compare_error_msgs, Msgs0, Msgs).

:- pred compare_error_specs(error_spec::in, error_spec::in,
    comparison_result::out) is det.

compare_error_specs(SpecA, SpecB, Result) :-
    SpecA = error_spec(_, _, MsgsA),
    SpecB = error_spec(_, _, MsgsB),
    ContextsA = project_msgs_contexts(MsgsA),
    ContextsB = project_msgs_contexts(MsgsB),
    compare(ContextResult, ContextsA, ContextsB),
    ( ContextResult = (=) ->
        compare(Result, SpecA, SpecB)
    ;
        Result = ContextResult
    ).

:- func project_msgs_contexts(list(error_msg)) = list(prog_context).

project_msgs_contexts([]) = [].
project_msgs_contexts([Msg | Msgs]) = Contexts :-
    TailContexts = project_msgs_contexts(Msgs),
    MaybeContext = project_msg_context(Msg),
    (
        MaybeContext = yes(Context),
        Contexts = [Context | TailContexts]
    ;
        MaybeContext = no,
        Contexts = TailContexts
    ).

:- pred compare_error_msgs(error_msg::in, error_msg::in,
    comparison_result::out) is det.

compare_error_msgs(MsgA, MsgB, Result) :-
    MaybeContextA = project_msg_context(MsgA),
    MaybeContextB = project_msg_context(MsgB),
    compare(ContextResult, MaybeContextA, MaybeContextB),
    ( ContextResult = (=) ->
        compare(Result, MsgA, MsgB)
    ;
        Result = ContextResult
    ).

:- func project_msg_context(error_msg) = maybe(prog_context).

project_msg_context(Msg) = MaybeContext :-
    (
        Msg = simple_msg(Context, _),
        MaybeContext = yes(Context)
    ;
        Msg = error_msg(yes(Context), _, _, _),
        MaybeContext = yes(Context)
    ;
        Msg = error_msg(no, _, _, __),
        MaybeContext = no
    ).

%-----------------------------------------------------------------------------%

write_error_spec(Spec, Globals, !NumWarnings, !NumErrors, !IO) :-
    write_error_specs([Spec], Globals, !NumWarnings, !NumErrors, !IO).

write_error_specs(Specs0, Globals, !NumWarnings, !NumErrors, !IO) :-
    sort_error_specs(Specs0, Specs),
    io.get_exit_status(OrigExitStatus, !IO),
    list.foldl3(do_write_error_spec(Globals, OrigExitStatus), Specs,
        !NumWarnings, !NumErrors, !IO).

:- pred do_write_error_spec(globals::in, int::in, error_spec::in,
    int::in, int::out, int::in, int::out, io::di, io::uo) is det.

do_write_error_spec(Globals, OrigExitStatus, Spec, !NumWarnings, !NumErrors,
        !IO) :-
    Spec = error_spec(Severity, _, Msgs),
    do_write_error_msgs(Msgs, Globals, OrigExitStatus, yes, no, PrintedSome,
        !IO),
    MaybeActual = actual_error_severity(Globals, Severity),
    (
        PrintedSome = no,
        expect(unify(MaybeActual, no), this_file,
            "do_write_error_spec: MaybeActual isn't no")
    ;
        PrintedSome = yes,
        (
            MaybeActual = yes(Actual),
            (
                Actual = actual_severity_error,
                !:NumErrors = !.NumErrors + 1,
                io.set_exit_status(1, !IO)
            ;
                Actual = actual_severity_warning,
                !:NumWarnings = !.NumWarnings + 1,
                record_warning(!IO)
            ;
                Actual = actual_severity_informational
            )
        ;
            MaybeActual = no,
            unexpected(this_file, "do_write_error_spec: MaybeActual is no")
        )
    ).

:- pred do_write_error_msgs(list(error_msg)::in, globals::in, int::in,
    bool::in, bool::in, bool::out, io::di, io::uo) is det.

do_write_error_msgs([], _Globals, _OrigExitStatus, _First, !PrintedSome, !IO).
do_write_error_msgs([Msg | Msgs], Globals, OrigExitStatus, !.First,
        !PrintedSome, !IO) :-
    (
        Msg = simple_msg(SimpleContext, Components),
        MaybeContext = yes(SimpleContext),
        TreatAsFirst = no,
        ExtraIndentLevel = 0
    ;
        Msg = error_msg(MaybeContext, TreatAsFirst, ExtraIndentLevel,
            Components)
    ),
    (
        TreatAsFirst = yes,
        !:First = yes
    ;
        TreatAsFirst = no
    ),
    Indent = ExtraIndentLevel * indent_increment,
    write_msg_components(Components, MaybeContext, Indent, Globals,
        OrigExitStatus, !First, !PrintedSome, !IO),
    do_write_error_msgs(Msgs, Globals, OrigExitStatus, !.First, !PrintedSome,
        !IO).

:- pred write_msg_components(list(error_msg_component)::in,
    maybe(prog_context)::in, int::in, globals::in, int::in,
    bool::in, bool::out, bool::in, bool::out, io::di, io::uo) is det.

write_msg_components([], _, _, _, _, !First, !PrintedSome, !IO).
write_msg_components([Component | Components], MaybeContext, Indent, Globals,
        OrigExitStatus, !First, !PrintedSome, !IO) :-
    (
        Component = always(ComponentPieces),
        do_write_error_pieces(!.First, MaybeContext, Indent,
            ComponentPieces, !IO),
        !:First = no,
        !:PrintedSome = yes
    ;
        Component = option_is_set(Option, RequiredValue, EmbeddedComponents),
        globals.lookup_bool_option(Globals, Option, OptionValue),
        ( OptionValue = RequiredValue ->
            write_msg_components(EmbeddedComponents, MaybeContext, Indent,
                Globals, OrigExitStatus, !First, !PrintedSome, !IO)
        ;
            true
        )
    ;
        Component = verbose_only(ComponentPieces),
        globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
        (
            VerboseErrors = yes,
            do_write_error_pieces(!.First, MaybeContext, Indent,
                ComponentPieces, !IO),
            !:First = no,
            !:PrintedSome = yes
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO)
        )
    ;
        Component = verbose_and_nonverbose(VerbosePieces, NonVerbosePieces),
        globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
        (
            VerboseErrors = yes,
            do_write_error_pieces(!.First, MaybeContext, Indent,
                VerbosePieces, !IO)
        ;
            VerboseErrors = no,
            do_write_error_pieces(!.First, MaybeContext, Indent,
                NonVerbosePieces, !IO),
            globals.io_set_extra_error_info(yes, !IO)
        ),
        !:First = no,
        !:PrintedSome = yes
    ;
        Component = print_anything(Anything),
        unsafe_cast_to_io_pred(Anything, Pred),
        Pred(!IO),
        !:First = no,
        !:PrintedSome = yes
    ),
    write_msg_components(Components, MaybeContext, Indent, Globals,
        OrigExitStatus, !First, !PrintedSome, !IO).

:- pred unsafe_cast_to_io_pred(pred(io, io)::in,
    pred(io, io)::out(pred(di, uo) is det)) is det.

:- pragma foreign_proc("C",
    unsafe_cast_to_io_pred(Anything::in, Pred::out(pred(di, uo) is det)),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Pred = Anything;
").

unsafe_cast_to_io_pred(_, _) :-
    unexpected(this_file, "unsafe_cast_to_io_pred").

%-----------------------------------------------------------------------------%

string_to_words_piece(Str) = words(Str).

list_to_pieces([]) = [].
list_to_pieces([Elem]) = [words(Elem)].
list_to_pieces([Elem1, Elem2]) = [fixed(Elem1), words("and"), fixed(Elem2)].
list_to_pieces([Elem1, Elem2, Elem3 | Elems]) =
    [fixed(Elem1 ++ ",") | list_to_pieces([Elem2, Elem3 | Elems])].

component_lists_to_pieces([]) = [].
component_lists_to_pieces([Comps]) = Comps.
component_lists_to_pieces([Comps1, Comps2]) =
    Comps1 ++ [words("and")] ++ Comps2.
component_lists_to_pieces([Comps1, Comps2, Comps3 | Comps]) =
    Comps1 ++ [suffix(",")]
    ++ component_lists_to_pieces([Comps2, Comps3 | Comps]).

component_list_to_pieces([]) = [].
component_list_to_pieces([Comp]) = [Comp].
component_list_to_pieces([Comp1, Comp2]) = [Comp1, words("and"), Comp2].
component_list_to_pieces([Comp1, Comp2, Comp3 | Comps]) =
    [Comp1, suffix(",")]
    ++ component_list_to_pieces([Comp2, Comp3 | Comps]).

component_list_to_line_pieces([], _) = [].
component_list_to_line_pieces([Comps], Final) = Comps ++ Final ++ [nl].
component_list_to_line_pieces([Comps1, Comps2 | CompLists], Final) =
    Comps1 ++ [suffix(","), nl]
    ++ component_list_to_line_pieces([Comps2 | CompLists], Final).

choose_number([], _Singular, Plural) = Plural.
choose_number([_], Singular, _Plural) = Singular.
choose_number([_, _ | _], _Singular, Plural) = Plural.


is_or_are([]) = "" :-
    unexpected(this_file, "error_util.is_or_are").
is_or_are([_]) = "is".
is_or_are([_, _ | _]) = "are".

write_error_pieces_plain(Components, !IO) :-
    do_write_error_pieces(yes, no, 0, Components, !IO).

write_error_plain_with_progname(ProgName, Msg, !IO) :-
    write_error_pieces_plain([fixed(ProgName ++ ":"), words(Msg)], !IO).

write_error_pieces(Context, Indent, Components, !IO) :-
    do_write_error_pieces(yes, yes(Context), Indent, Components, !IO).

write_error_pieces_not_first_line(Context, Indent, Components, !IO) :-
    do_write_error_pieces(no, yes(Context), Indent, Components, !IO).

write_error_pieces_maybe_first_line(IsFirst, Context, Indent, Components,
        !IO) :-
    do_write_error_pieces(IsFirst, yes(Context), Indent, Components, !IO).

write_error_pieces_maybe_with_context(MaybeContext, Indent, Components, !IO) :-
    do_write_error_pieces(yes, MaybeContext, Indent, Components, !IO).

:- pred do_write_error_pieces(bool::in, maybe(prog_context)::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.

do_write_error_pieces(IsFirst, MaybeContext, FixedIndent, Components, !IO) :-
    (
            % The fixed characters at the start of the line are:
            % filename
            % :
            % line number (min 3 chars)
            % :
            % space
            % indent
        (
            MaybeContext = yes(Context),
            term.context_file(Context, FileName),
            term.context_line(Context, LineNumber),
            string.length(FileName, FileNameLength),
            string.int_to_string(LineNumber, LineNumberStr),
            string.length(LineNumberStr, LineNumberStrLength0),
            ( LineNumberStrLength0 < 3 ->
                LineNumberStrLength = 3
            ;
                LineNumberStrLength = LineNumberStrLength0
            ),
            ContextLength = FileNameLength + 1 + LineNumberStrLength + 2
        ;
            MaybeContext = no,
            ContextLength = 0
        ),
        convert_components_to_paragraphs(Components, Paragraphs),
        FirstIndent = (IsFirst = yes -> 0 ; 1),
        Remain = 79 - (ContextLength + FixedIndent),
        group_words(IsFirst, FirstIndent, Paragraphs, Remain, Lines)
    ),
    write_lines(Lines, MaybeContext, FixedIndent, !IO).

:- func indent_increment = int.

indent_increment = 2.

:- pred write_lines(list(line)::in, maybe(prog_context)::in, int::in,
    io::di, io::uo) is det.

write_lines([], _, _, !IO).
write_lines([Line | Lines], MaybeContext, FixedIndent, !IO) :-
    (
        MaybeContext = yes(Context),
        prog_out.write_context(Context, !IO)
    ;
        MaybeContext = no
    ),
    Line = line(LineIndent, LineWords),
    Indent = FixedIndent + LineIndent * indent_increment,
    string.pad_left("", ' ', Indent, IndentStr),
    io.write_string(IndentStr, !IO),
    write_line(LineWords, !IO),
    write_lines(Lines, MaybeContext, FixedIndent, !IO).

:- pred write_line(list(string)::in, io::di, io::uo) is det.

write_line([], !IO) :-
    io.write_char('\n', !IO).
write_line([Word | Words], !IO) :-
    io.write_string(Word, !IO),
    write_line_rest(Words, !IO),
    io.write_char('\n', !IO).

:- pred write_line_rest(list(string)::in, io::di, io::uo) is det.

write_line_rest([], !IO).
write_line_rest([Word | Words], !IO) :-
    io.write_char(' ', !IO),
    io.write_string(Word, !IO),
    write_line_rest(Words, !IO).

error_pieces_to_string([]) = "".
error_pieces_to_string([Component | Components]) = Str :-
    TailStr = error_pieces_to_string(Components),
    (
        Component = fixed(Word),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = quote(Word),
        Str = join_string_and_tail(add_quotes(Word), Components, TailStr)
    ;
        Component = int_fixed(Int),
        Str = join_string_and_tail(int_to_string(Int), Components, TailStr)
    ;
        Component = prefix(Word),
        Str = Word ++ TailStr
    ;
        Component = suffix(Word),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = words(Words),
        Str = join_string_and_tail(Words, Components, TailStr)
    ;
        Component = sym_name(SymName),
        Word = sym_name_to_word(SymName),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = sym_name_and_arity(SymNameAndArity),
        Word = sym_name_and_arity_to_word(SymNameAndArity),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = p_or_f(PredOrFunc),
        Word = pred_or_func_to_string(PredOrFunc),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = simple_call(SimpleCallId),
        Word = simple_call_id_to_string(SimpleCallId),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = top_ctor_of_type(Type),
        ( type_to_ctor_and_args(Type, TypeCtor, _) ->
            TypeCtor = type_ctor(TypeCtorName, TypeCtorArity),
            SymName = TypeCtorName / TypeCtorArity,
            Word = sym_name_and_arity_to_word(SymName),
            Str = join_string_and_tail(Word, Components, TailStr)
        ;
            error("error_pieces_to_string: type is variable")
        )
    ;
        Component = nl,
        Str = "\n" ++ TailStr
    ;
        Component = nl_indent_delta(_),
        % There is nothing we can do about the indent delta.
        Str = "\n" ++ TailStr
    ;
        Component = blank_line,
        Str = "\n\n" ++ TailStr
    ).

:- func join_string_and_tail(string, list(format_component), string) = string.

join_string_and_tail(Word, Components, TailStr) = Str :-
    ( TailStr = "" ->
        Str = Word
    ; Components = [suffix(_) | _] ->
        Str = Word ++ TailStr
    ;
        Str = Word ++ " " ++ TailStr
    ).

%----------------------------------------------------------------------------%

:- type paragraph
    --->    paragraph(
                list(string),   % The list of words to print in the paragraph.
                                % It should not be empty.
                int,            % The number of blank lines to print after
                                % the paragraph.
                int             % The indent delta to apply for the next
                                % paragraph.
            ).

:- pred convert_components_to_paragraphs(list(format_component)::in,
    list(paragraph)::out) is det.

convert_components_to_paragraphs(Components, Paras) :-
    convert_components_to_paragraphs_acc(Components, [], [], Paras).

:- type word
    --->    plain_word(string)
    ;       prefix_word(string)
    ;       suffix_word(string).

:- pred convert_components_to_paragraphs_acc(list(format_component)::in,
    list(word)::in, list(paragraph)::in, list(paragraph)::out) is det.

convert_components_to_paragraphs_acc([], RevWords0, !Paras) :-
    Strings = rev_words_to_strings(RevWords0),
    list.reverse([paragraph(Strings, 0, 0) | !.Paras], !:Paras).
convert_components_to_paragraphs_acc([Component | Components], RevWords0,
        !Paras) :-
    (
        Component = fixed(Word),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Component = quote(Word),
        RevWords1 = [plain_word(add_quotes(Word)) | RevWords0]
    ;
        Component = int_fixed(Int),
        RevWords1 = [plain_word(int_to_string(Int)) | RevWords0]
    ;
        Component = prefix(Word),
        RevWords1 = [prefix_word(Word) | RevWords0]
    ;
        Component = suffix(Word),
        RevWords1 = [suffix_word(Word) | RevWords0]
    ;
        Component = words(WordsStr),
        break_into_words(WordsStr, RevWords0, RevWords1)
    ;
        Component = sym_name(SymName),
        RevWords1 = [plain_word(sym_name_to_word(SymName)) | RevWords0]
    ;
        Component = sym_name_and_arity(SymNameAndArity),
        Word = sym_name_and_arity_to_word(SymNameAndArity),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Component = top_ctor_of_type(Type),
        ( type_to_ctor_and_args(Type, TypeCtor, _) ->
            TypeCtor = type_ctor(TypeCtorName, TypeCtorArity),
            SymName = TypeCtorName / TypeCtorArity,
            RevWords1 = [plain_word(sym_name_and_arity_to_word(SymName))
                | RevWords0]
        ;
            error("convert_components_to_paragraphs_acc: type is variable")
        )
    ;
        Component = p_or_f(PredOrFunc),
        Word = pred_or_func_to_string(PredOrFunc),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Component = simple_call(SimpleCallId),
        WordsStr = simple_call_id_to_string(SimpleCallId),
        break_into_words(WordsStr, RevWords0, RevWords1)
    ;
        Component = nl,
        Strings = rev_words_to_strings(RevWords0),
        list.cons(paragraph(Strings, 0, 0), !Paras),
        RevWords1 = []
    ;
        Component = nl_indent_delta(IndentDelta),
        Strings = rev_words_to_strings(RevWords0),
        list.cons(paragraph(Strings, 0, IndentDelta), !Paras),
        RevWords1 = []
    ;
        Component = blank_line,
        Strings = rev_words_to_strings(RevWords0),
        list.cons(paragraph(Strings, 1, 0), !Paras),
        RevWords1 = []
    ),
    convert_components_to_paragraphs_acc(Components, RevWords1, !Paras).

:- type plain_or_prefix
    --->    plain(string)
    ;       prefix(string).

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
    ).

:- func sym_name_to_word(sym_name) = string.

sym_name_to_word(SymName) =
    "`" ++ sym_name_to_string(SymName) ++ "'".

:- func sym_name_and_arity_to_word(sym_name_and_arity) = string.

sym_name_and_arity_to_word(SymName / Arity) =
    "`" ++ sym_name_to_string(SymName) ++ "'" ++ "/" ++ int_to_string(Arity).

:- pred break_into_words(string::in, list(word)::in, list(word)::out) is det.

break_into_words(String, Words0, Words) :-
    break_into_words_from(String, 0, Words0, Words).

:- pred break_into_words_from(string::in, int::in, list(word)::in,
    list(word)::out) is det.

break_into_words_from(String, Cur, Words0, Words) :-
    ( find_word_start(String, Cur, Start) ->
        find_word_end(String, Start, End),
        Length = End - Start + 1,
        string.substring(String, Start, Length, WordStr),
        Next = End + 1,
        break_into_words_from(String, Next, [plain_word(WordStr) | Words0],
            Words)
    ;
        Words = Words0
    ).

:- pred find_word_start(string::in, int::in, int::out) is semidet.

find_word_start(String, Cur, WordStart) :-
    string.index(String, Cur, Char),
    ( char.is_whitespace(Char) ->
        Next = Cur + 1,
        find_word_start(String, Next, WordStart)
    ;
        WordStart = Cur
    ).

:- pred find_word_end(string::in, int::in, int::out) is det.

find_word_end(String, Cur, WordEnd) :-
    Next = Cur + 1,
    ( string.index(String, Next, Char) ->
        ( char.is_whitespace(Char) ->
            WordEnd = Cur
        ;
            find_word_end(String, Next, WordEnd)
        )
    ;
        WordEnd = Cur
    ).

%----------------------------------------------------------------------------%

:- type line
    --->    line(
                int,            % Indent level; multiply by indent_increment
                                % to get number of spaces of indentation.
                list(string)    % The words on the line.
            ).

    % Groups the given words into lines. The first line can have up to Max
    % characters on it; the later lines (if any) up to Max-2 characters.
    % The given list of paragraphs must be nonempty, since we always return
    % at least one line.
    %
:- pred group_words(bool::in, int::in, list(paragraph)::in, int::in,
    list(line)::out) is det.

group_words(IsFirst, CurIndent, Paras, Max, Lines) :-
    (
        Paras = [],
        Lines = []
    ;
        Paras = [FirstPara | LaterParas],
        FirstPara = paragraph(FirstParaWords, NumBlankLines, FirstIndentDelta),
        (
            IsFirst = yes,
            RestIndent = CurIndent + 1
        ;
            IsFirst = no,
            RestIndent = CurIndent
        ),
        NextIndent = RestIndent + FirstIndentDelta,

        BlankLine = line(CurIndent, []),
        list.duplicate(NumBlankLines, BlankLine, BlankLines),
        (
            FirstParaWords = [],
            group_words(IsFirst, NextIndent, LaterParas, Max, RestLines),
            Lines = BlankLines ++ RestLines
        ;
            FirstParaWords = [FirstWord | LaterWords],
            get_line_of_words(FirstWord, LaterWords, CurIndent, Max,
                LineWords, RestWords),
            CurLine = line(CurIndent, LineWords),

            group_nonfirst_line_words(RestWords, RestIndent, Max,
                ParaRestLines),
            ParaLines = [CurLine | ParaRestLines],

            group_words(no, NextIndent, LaterParas, Max, RestLines),
            Lines = ParaLines ++ BlankLines ++ RestLines
        )
    ).

:- pred group_nonfirst_line_words(list(string)::in, int::in, int::in,
    list(line)::out) is det.

group_nonfirst_line_words(Words, Indent, Max, Lines) :-
    (
        Words = [],
        Lines = []
    ;
        Words = [FirstWord | LaterWords],
        get_line_of_words(FirstWord, LaterWords, Indent, Max,
            LineWords, RestWords),
        Line = line(Indent, LineWords),
        group_nonfirst_line_words(RestWords, Indent, Max, RestLines),
        Lines = [Line | RestLines]
    ).

:- pred get_line_of_words(string::in, list(string)::in, int::in, int::in,
    list(string)::out, list(string)::out) is det.

get_line_of_words(FirstWord, LaterWords, Indent, Max, Line, RestWords) :-
    string.length(FirstWord, FirstWordLen),
    Avail = Max - Indent * indent_increment,
    get_later_words(LaterWords, FirstWordLen, Avail, [FirstWord],
        Line, RestWords).

:- pred get_later_words(list(string)::in, int::in, int::in,
    list(string)::in, list(string)::out, list(string)::out) is det.

get_later_words([], _, _, Line, Line, []).
get_later_words([Word | Words], OldLen, Avail, Line0, Line, RestWords) :-
    string.length(Word, WordLen),
    NewLen = OldLen + 1 + WordLen,
    ( NewLen =< Avail ->
        list.append(Line0, [Word], Line1),
        get_later_words(Words, NewLen, Avail, Line1, Line, RestWords)
    ;
        Line = Line0,
        RestWords = [Word | Words]
    ).

%-----------------------------------------------------------------------------%

describe_sym_name_and_arity(SymName / Arity) =
    string.append_list(["`", sym_name_to_string(SymName), "/",
        string.int_to_string(Arity), "'"]).

describe_sym_name(SymName) =
    string.append_list(["`", sym_name_to_string(SymName), "'"]).

pred_or_func_to_string(pf_predicate) = "predicate".
pred_or_func_to_string(pf_function) = "function".

add_quotes(Str) = "`" ++ Str ++ "'".

capitalize(Str0) = Str :-
    Chars0 = string.to_char_list(Str0),
    (
        Chars0 = [Char0 | TailChars],
        char.is_lower(Char0),
        Char = char.to_upper(Char0)
    ->
        Chars = [Char | TailChars],
        Str = string.from_char_list(Chars)
    ;
        Str = Str0
    ).

%-----------------------------------------------------------------------------%

report_warning(Context, Indent, Components, !IO) :-
    record_warning(!IO),
    write_error_pieces(Context, Indent, Components, !IO).

%-----------------------------------------------------------------------------%

unable_to_open_file(FileName, IOErr, !IO) :-
    io.stderr_stream(StdErr, !IO),
    io.write_string(StdErr, "Unable to open file: '", !IO),
    io.write_string(StdErr, FileName, !IO),
    io.write_string(StdErr, "' because\n", !IO),
    io.write_string(StdErr, io.error_message(IOErr), !IO),
    io.nl(StdErr, !IO),

    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "error_util.m".

%-----------------------------------------------------------------------------%
:- end_module error_util.
%-----------------------------------------------------------------------------%
