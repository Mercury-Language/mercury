%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
% line contains at most <n> characters (unless a long single word
% forces the line over this limit) where --max-error-line-width <n>.
%
% The caller supplies the list of words to be printed in the form
% of a list of error message components. Each component may specify
% a string to printed exactly as it is, or it may specify a string
% containing a list of words, which may be broken at white space.
%
%---------------------------------------------------------------------------%

:- module parse_tree.error_util.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

% Every distinct problem should generate a single error specification.
% This specification should state
%
% - the severity of the problem (so that we can update the exit status
%   of the compiler accordingly),
% - which phase of the compiler found the problem (since later phases
%   may wish to suppress some problem reports if some specific earlier phases
%   found problems, e.g. when a missing clause could be caused
%   by a syntax error), and
% - a specification of what to print.
%
% In most cases, the "what to print" will be a single message for a single
% context. However, we may want to print messages for several contexts.
% For example, when reporting a duplicate declaration, we want to report
% this fact in the duplicate declaration's context, while printing another
% message giving the original declaration's context.
%
% simplest_spec(Id, Severity, Phase, Context, Pieces) is a shorthand for
% (and equivalent in every respect to) error_spec(Id, Severity, Phase,
% [simple_msg(Context, always(Pieces)])]).
%
% conditional_spec(Id, Option, MatchValue, Severity, Phase, Msgs) is intended
% to represent the error specification given by its last three fields
% *iff* Option has the value MatchValue. If Option is *not* MatchValue,
% it asks for nothing to be printed, and for the exit status to be left alone.
%
% The id field, which is present in all three alternatives, is totally
% ignored when printing error_specs. Its job is something completely different:
% helping developers track down where in the source code each error_spec
% was constructed. Without the id fields, if developers wants to know this,
% e.g. because they do not want the message printed, or because there is
% a problem with its wording, they have to grep for some words in the message.
% However, grepping for a single word will usually get many false hits,
% while grepping for two or more consecutive words in the message may miss
% the code generating the message, because in that code, some of those
% consecutive words are on different lines. On the other hand, if every
% place that constructs an error_spec, of any of these three varieties,
% fills in the id field with $pred, then finding the right place is easy:
% just specify the developer-only option --print-error-spec-id, and
% the identity of the predicate or function that generated each error_spec
% will be output just after the messages in that error_spec. Even if the
% predicate or function that this identifies has several pieces of code
% that construct specs, the scope in which you have to search for it
% will be easily manageable.

:- type error_spec
    --->    error_spec(
                error_id                :: string,
                error_severity          :: error_severity,
                error_phase             :: error_phase,
                error_msgs              :: list(error_msg)
            )
    ;       simplest_spec(
                simp_id                 :: string,
                simp_spec_severity      :: error_severity,
                simp_spec_phase         :: error_phase,
                simp_spec_context       :: prog_context,
                simp_spec_pieces        :: list(format_component)
            )
    ;       conditional_spec(
                cond_id                 :: string,
                cond_spec_option        :: option,
                cond_spec_value         :: bool,

                cond_spec_severity      :: error_severity,
                cond_spec_phase         :: error_phase,
                cond_spec_msgs          :: list(error_msg)
            ).

:- pred extract_spec_msgs(globals::in, error_spec::in,
    list(error_msg)::out) is det.

%---------------------------------------------------------------------------%

% Many operations in the compiler may either succeed or fail.
% When they succeed, they return some result(s); when they don't,
% they return one or more errors.

:- type maybe_error_specs(T)
    --->    ok_no_spec(T)
    ;       error_specs(error_spec, list(error_spec)).

%---------------------------------------------------------------------------%

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
    --->    phase_options
    ;       phase_read_files
    ;       phase_module_name
    ;       phase_term_to_parse_tree
    ;       phase_parse_tree_to_hlds
    ;       phase_expand_types
    ;       phase_type_check
    ;       phase_inst_check
    ;       phase_polymorphism
    ;       phase_mode_check(mode_report_control)
    ;       phase_purity_check
    ;       phase_detism_check
    ;       phase_oisu_check
    ;       phase_simplify(mode_report_control)
    ;       phase_style
    ;       phase_dead_code
    ;       phase_termination_analysis
    ;       phase_accumulator_intro
    ;       phase_auto_parallelism
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
% are separated by three spaces. You can request that the first line of
% a message be treated as it were the first by setting the error_treat_as_first
% field to "treat_as_first". You can also request that the pieces in a message
% be given extra indentation by setting the error_extra_indent field
% to a nonzero value.
%
% The term simple_msg(Context, Components) is a shorthand for (and equivalent
% in every respect to) the term error_msg(yes(Context), do_not_treat_as_first,
% 0, Components).
%
% The term simplest_msg(Context, Pieces) is a shorthand for (and equivalent
% in every respect to) the term simple_msg(Context, [always(Pieces)]).

:- type maybe_treat_as_first
    --->    treat_as_first
    ;       do_not_treat_as_first.

:- type error_msg
    --->    simplest_msg(
                simplest_context        :: prog_context,
                simple_pieces           :: list(format_component)
            )
    ;       simple_msg(
                simple_context          :: prog_context,
                simple_components       :: list(error_msg_component)
            )
    ;       error_msg(
                error_context           :: maybe(prog_context),
                error_treat_as_first    :: maybe_treat_as_first,
                error_extra_indent      :: int,
                error_components        :: list(error_msg_component)
            ).

:- type verbose_always_or_once
    --->    verbose_always
    ;       verbose_once.
            % Message components marked as verbose_once should be printed
            % just once.

:- type error_msg_component
    --->    always(list(format_component))
            % Print these components under all circumstances.

    ;       option_is_set(option, bool, list(error_msg_component))
            % Print the embedded components only if the specified boolean
            % option has the specified value.

    ;       verbose_only(verbose_always_or_once, list(format_component))
            % Print these components only if --verbose-errors is specified.
            % If it is not specified, set the flag that triggers the printing
            % of the message reminding the user about --verbose-errors.
            % In addition, if the first field is verbose_once, then disable
            % all but the first printing of the message even if
            % --verbose-errors is specified.

    ;       verbose_and_nonverbose(list(format_component),
                list(format_component))
            % If --verbose-errors is specified, print the first set of
            % components. If it is not specified, print the second set,
            % and set the flag that triggers the printing of the message
            % reminding the user about --verbose-errors. The verbose part
            % is implicitly verbose_always.

    ;       some [T] ( print_anything(T) => print_anything(T) ).
            % This alternative allows the caller to specify an arbitrary thing
            % to be printed at any point in the sequence. Since things printed
            % this way aren't formatted as error messages should be (context
            % at start etc), this capability is intended only for messages
            % that help debug the compiler itself.

:- typeclass print_anything(T) where [
    pred print_anything(T::in, io::di, io::uo) is det
].

%---------------------------------------------------------------------------%

    % Return the worst of two actual severities.
    %
:- func worst_severity(actual_severity, actual_severity)
    = actual_severity.

    % Compute the actual severity of a message with the given severity
    % (if it actually prints anything).
    %
:- func actual_error_severity(globals, error_severity)
    = maybe(actual_severity).

    % Compute the actual severity of an error_spec
    % (if it actually prints anything).
    %
:- func actual_spec_severity(globals, error_spec) = maybe(actual_severity).

    % Compute the worst actual severity (if any) occurring in a list of
    % error_specs.
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

    % If --halt-at-warn is not set, then return `yes' if the given list
    % contains error_specs whose actual severity is actual_severity_error.
    %
    % If --halt-at-warn is set, then return `yes' if the given list
    % contains error_specs whose actual severity is either
    % actual_severity_error or actual_severity_warning.
    %
:- func contains_errors_or_warnings_treated_as_errors(globals,
    list(error_spec)) = bool.

%---------------------------------------------------------------------------%

:- pred sort_error_msgs(list(error_msg)::in, list(error_msg)::out) is det.

%---------------------------------------------------------------------------%

    % Delete all the given error_specs, which are supposed to have been
    % gathered during the process that generates the contents of an interface
    % file, if two conditions are both satisfied:
    %
    % - print_errors_warnings_when_generating_interface is NOT set, and
    % - the exit status has NOT been set to indicate an error.
    %
:- pred filter_interface_generation_specs(globals::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%
% The error_spec_accumulator type can be used to accumulate errors for
% multiple modes of a predicate. accumulate_error_specs_for_proc will
% eliminate warnings that should only be reported if they occur in every mode,
% but don't occur in every mode.

:- type error_spec_accumulator.

:- func init_error_spec_accumulator = error_spec_accumulator.

:- pred accumulate_error_specs_for_proc(list(error_spec)::in,
    error_spec_accumulator::in, error_spec_accumulator::out) is det.

:- func error_spec_accumulator_to_list(error_spec_accumulator) =
    list(error_spec).

%---------------------------------------------------------------------------%

:- pred pre_hlds_maybe_write_out_errors(bool::in, globals::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.
:- pred pre_hlds_maybe_write_out_errors(io.text_output_stream::in,
    bool::in, globals::in,
    list(error_spec)::in, list(error_spec)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % write_error_spec_ignore(Spec, Globals, !IO):
    % write_error_spec_ignore(Stream, Spec, Globals, !IO):
    % write_error_specs_ignore(Specs, Globals, !IO):
    % write_error_specs_ignore(Stream, Specs, Globals, !IO):
    % write_error_spec(Spec, Globals, !NumWarnings, !NumErrors, !IO):
    % write_error_spec(Stream, Spec, Globals, !NumWarnings, !NumErrors, !IO):
    % write_error_specs(Specs, Globals, !NumWarnings, !NumErrors, !IO):
    % write_error_specs(Stream, Specs, Globals, !NumWarnings, !NumErrors, !IO):
    %
    % Write out the error message(s) specified by Spec or Specs, minus the
    % parts whose conditions are false. In the non-ignore versions,
    % increment !NumWarnings by the number of printed warnings and
    % !NumErrors by the number of printed errors.
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
    % Calls to the ignore versions could be considered to implicitly have
    % an XXX next to them, for not incrementing the num_errors field
    % in the module_info if the (ignored) number of errors printed
    % is nonzero. However, we have got along fine for a long time ignoring
    % the number of warnings and errors at most calls to write out specs,
    % since we do set the exit status to a nonzero value when we print an
    % error, and the main use of the num_errors field is to do likewise.
    %
:- pred write_error_spec_ignore(error_spec::in,
    globals::in, io::di, io::uo) is det.
:- pred write_error_spec_ignore(io.text_output_stream::in, error_spec::in,
    globals::in, io::di, io::uo) is det.
:- pred write_error_specs_ignore(list(error_spec)::in,
    globals::in, io::di, io::uo) is det.
:- pred write_error_specs_ignore(io.text_output_stream::in,
    list(error_spec)::in, globals::in, io::di, io::uo) is det.

:- pred write_error_spec(error_spec::in,
    globals::in, int::in, int::out, int::in, int::out, io::di, io::uo) is det.
:- pred write_error_spec(io.text_output_stream::in, error_spec::in,
    globals::in, int::in, int::out, int::in, int::out, io::di, io::uo) is det.
:- pred write_error_specs(list(error_spec)::in,
    globals::in, int::in, int::out, int::in, int::out, io::di, io::uo) is det.
:- pred write_error_specs(io.text_output_stream::in, list(error_spec)::in,
    globals::in, int::in, int::out, int::in, int::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- type format_component
    --->    invis_order_default_start(int)
            % Prints nothing. If the compiler generates two different specs
            % for the same context that we intend to appear in a specific
            % order, even though it may not be the order that sorting those
            % specs would normally give, we can add one of these to the
            % start of each error_spec, with the order of the numbers
            % inside these invis orders controlling the final order
            % of the error_specs.
            %
            % This component sorts before other components that do not
            % specify such an ordinal number. The invis_order_default_end
            % component sorts after them. By choosing to use one or the other,
            % users of this type can control sorting with respect to
            % error messages generated in places in the code they do not
            % control.

    ;       fixed(string)
            % This string should appear in the output in one piece, as it is.

    ;       quote(string)
            % Surround the string with `' quotes, then treat as fixed.

    ;       int_fixed(int)
            % Convert the integer to a string, then treat as fixed.

    ;       nth_fixed(int)
            % Convert the integer to a string, such as "first", "second",
            % "third", ... 49th, and then treat as fixed.

    ;       lower_case_next_if_not_first
            % If this is the first component, ignore it. If this is not
            % the first component, lower case the initial letter of the
            % next component. There is no effect if the next component
            % does not exist or does not start with an upper case letter.

    ;       prefix(string)
            % This string should appear in the output in one piece, as it is,
            % inserted directly before the next format_component, without
            % any intervening space.

    ;       suffix(string)
            % This string should appear in the output in one piece, as it is,
            % appended directly after the previous format_component, without
            % any intervening space.

    ;       words(string)
            % This string contains words separated by white space. The words
            % should appear in the output in the given order, but the white
            % space may be rearranged and line breaks may be inserted.

    ;       words_quote(string)
            % Surround the string with `' quotes, then treat as words.

    ;       qual_sym_name(sym_name)
    ;       unqual_sym_name(sym_name)
            % The output should contain the string form of the sym_name,
            % surrounded by `' quotes.

    ;       qual_sym_name_arity(sym_name_arity)
    ;       unqual_sym_name_arity(sym_name_arity)
            % The output should contain the string form of the sym_name,
            % surrounded by `' quotes, followed by '/' and the arity.

    ;       qual_pf_sym_name_orig_arity(pf_sym_name_arity)
    ;       unqual_pf_sym_name_orig_arity(pf_sym_name_arity)
            % The output should contain the string form of the sym_name,
            % surrounded by `' quotes, followed by '/' and the arity, but
            % - precede them with either "predicate" or "function", and
            % - for functions, use their *original* arity, which does not
            %   count the function result.

    ;       qual_type_ctor(type_ctor)
    ;       unqual_type_ctor(type_ctor)
            % The output should contain the string form of the type_ctor,
            % surrounded by `' quotes, followed by '/' and the arity.

    ;       qual_cons_id_and_maybe_arity(cons_id)
    ;       unqual_cons_id_and_maybe_arity(cons_id)
            % If the cons_id is a cons_id for a builtin type, strip the
            % builtin qualifier (or all qualifier) from it, and output
            % the result. If the cons_id is for a du type, output its name
            % in quotes, followed by '/' and its arity.

    ;       qual_top_ctor_of_type(mer_type)
            % The top level type constructor of the given type,
            % which must have one (i.e. must not be a variable).

    ;       p_or_f(pred_or_func)
            % Output the string "predicate" or "function" as appropriate.

    ;       decl(string)
            % Prefix the string with ":- ", surround with single quotes
            % and then treat as fixed.

    ;       pragma_decl(string)
            % As above but prefix the string with ":- pragma ".

    ;       nl
            % Insert a line break if there has been text output since
            % the last line break.

    ;       nl_indent_delta(int)
            % Act as nl, but also add the given integer (which should be a
            % small positive or negative integer) to the current indent level.

    ;       blank_line
            % Create a blank line.

    ;       invis_order_default_end(int).
            % See the documentation of invis_order_default_start above.

    % Wrap words() around a string.
    %
:- func string_to_words_piece(string) = format_component.

    % Convert a list of strings into a list of format_components
    % separated by commas, with the last two elements separated by `and'.
    %
:- func list_to_pieces(list(string)) = list(format_component).

    % Convert a list of strings into a list of format_components
    % separated by commas. Even the last pair of strings will be
    % separated by commas.
    %
:- func strict_list_to_pieces(list(string)) = list(format_component).

    % As list_to_pieces, but surround each string by `' quotes.
    %
:- func list_to_quoted_pieces(list(string)) = list(format_component).

    % As above, but with the last two elements separated by `or'.
    %
:- func list_to_quoted_pieces_or(list(string)) = list(format_component).

    % Convert a list of lists of format_components into a list of
    % format_components separated by commas, with the last two elements
    % separated by the first argument as a word.
    %
:- func component_lists_to_pieces(string, list(list(format_component))) =
    list(format_component).

    % Convert a list of lists of format_components into a list of
    % format_components separated by commas. Even the last pair of lists
    % will be separated by commas.
    %
:- func strict_component_lists_to_pieces(list(list(format_component))) =
    list(format_component).

    % Convert a list of format_components into a list of format_components
    % separated by commas, with the last two elements separated
    % by the first argument as a word.
    %
:- func component_list_to_pieces(string, list(format_component)) =
    list(format_component).

    % Convert a list of format_components into a list of format_components
    % separated by commas. Even the last pair of list elements will be
    % separated by commas.
    %
:- func strict_component_list_to_pieces(list(format_component)) =
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
    % based on the length of a list. Chooses the plural if the list is empty.
    %
:- func choose_number(list(T), U, U) = U.

    % is_or_are(List) throws an exception if the list is empty, returns "is"
    % if the list is a singleton, and otherwise returns "are".
    %
:- func is_or_are(list(T)) = string.

%---------------------------------------------------------------------------%

% XXX The predicates below should not be called in new code. New code should
% create error specifications, and then call write_error_spec to print them.

    % Display the given error message, without a context and with standard
    % indentation.
    %
:- pred write_error_pieces_plain(globals::in, list(format_component)::in,
    io::di, io::uo) is det.
:- pred write_error_pieces_plain(io.text_output_stream::in, globals::in,
    list(format_component)::in, io::di, io::uo) is det.

    % write_error_plain_with_progname(ProgName, Msg):
    %
    % Display Msg as the error string, with ProgName as a context
    % and with standard indentation.
    %
:- pred write_error_plain_with_progname(string::in, string::in,
    io::di, io::uo) is det.
:- pred write_error_plain_with_progname(io.text_output_stream::in,
    string::in, string::in, io::di, io::uo) is det.

    % write_error_pieces(Globals, Context, Indent, Components):
    %
    % Display `Components' as the error message, with `Context' as a context
    % and indent by `Indent'.
    %
:- pred write_error_pieces(globals::in, prog_context::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.
:- pred write_error_pieces(io.text_output_stream::in, globals::in,
    prog_context::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.

:- pred write_error_pieces_maybe_with_context(globals::in,
    maybe(prog_context)::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.
:- pred write_error_pieces_maybe_with_context(io.text_output_stream::in,
    globals::in, maybe(prog_context)::in, int::in,
    list(format_component)::in, io::di, io::uo) is det.

:- func error_pieces_to_string(list(format_component)) = string.

%---------------------------------------------------------------------------%

:- func describe_sym_name(sym_name) = string.

:- func describe_sym_name_arity(sym_name_arity) = string.

    % Put `' quotes around the given string.
    %
:- func add_quotes(string) = string.

    % Report a warning, and set the exit status to error if the
    % --halt-at-warn option is set.
    %
:- pred report_warning(globals::in,
    prog_context::in, int::in, list(format_component)::in,
    io::di, io::uo) is det.
:- pred report_warning(io.text_output_stream::in, globals::in,
    prog_context::in, int::in, list(format_component)::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Report why the file is not able to be opened to stderr_stream,
    % and set the exit status to 1.
    %
:- pred unable_to_open_file(string::in, io.error::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module libs.compiler_util.

:- import_module char.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

extract_spec_msgs(Globals, Spec, Msgs) :-
    (
        Spec = error_spec(_Id, _Severity, _Phase, Msgs)
    ;
        Spec = simplest_spec(_Id, _Severity, _Phase, Context, Pieces),
        Msgs = [simplest_msg(Context, Pieces)]
    ;
        Spec = conditional_spec(_Id, Option, MatchValue, _Severity, _Phase,
            Msgs0),
        globals.lookup_bool_option(Globals, Option, Value),
        ( if Value = MatchValue then
            Msgs = Msgs0
        else
            Msgs = []
        )
    ).

%---------------------------------------------------------------------------%

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
        ( if Value = MatchValue then
            MaybeActual = actual_error_severity(Globals, Match)
        else
            (
                MaybeNoMatch = no,
                MaybeActual = no
            ;
                MaybeNoMatch = yes(NoMatch),
                MaybeActual = actual_error_severity(Globals, NoMatch)
            )
        )
    ).

actual_spec_severity(Globals, Spec) = MaybeSeverity :-
    (
        ( Spec = error_spec(_, Severity, _, _)
        ; Spec = simplest_spec(_, Severity, _, _, _)
        ),
        MaybeSeverity = actual_error_severity(Globals, Severity)
    ;
        Spec = conditional_spec(_, Option, MatchValue, Severity, _, _),
        globals.lookup_bool_option(Globals, Option, OptionValue),
        ( if OptionValue = MatchValue then
            MaybeSeverity = actual_error_severity(Globals, Severity)
        else
            MaybeSeverity = no
        )
    ).

worst_severity_in_specs(Globals, Specs) = MaybeWorst :-
    worst_severity_in_specs_2(Globals, Specs, no, MaybeWorst).

:- pred worst_severity_in_specs_2(globals::in, list(error_spec)::in,
    maybe(actual_severity)::in, maybe(actual_severity)::out) is det.

worst_severity_in_specs_2(_Globals, [], !MaybeWorst).
worst_severity_in_specs_2(Globals, [Spec | Specs], !MaybeWorst) :-
    MaybeThis = actual_spec_severity(Globals, Spec),
    (
        !.MaybeWorst = no,
        !:MaybeWorst = MaybeThis
    ;
        !.MaybeWorst = yes(Worst),
        (
            MaybeThis = no
        ;
            MaybeThis = yes(This),
            !:MaybeWorst = yes(worst_severity(Worst, This))
        )
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

contains_errors_or_warnings_treated_as_errors(Globals, Specs) = Halt :-
    MaybeWorstActual = worst_severity_in_specs(Globals, Specs),
    (
        MaybeWorstActual = no,
        Halt = no
    ;
        MaybeWorstActual = yes(WorstActual),
        (
            WorstActual = actual_severity_error,
            Halt = yes
        ;
            WorstActual = actual_severity_warning,
            globals.lookup_bool_option(Globals, halt_at_warn, HaltAtWarn),
            (
                HaltAtWarn = yes,
                Halt = yes
            ;
                HaltAtWarn = no,
                Halt = no
            )
        ;
            WorstActual = actual_severity_informational,
            Halt = no
        )
    ).

%---------------------------------------------------------------------------%

:- pred sort_error_specs(globals::in,
    list(error_spec)::in, list(error_spec)::out) is det.

sort_error_specs(Globals, !Specs) :-
    % The purpose of remove_conditionals_in_spec is to remove differences
    % between error_specs that exist only in the structure of the error_specs
    % themselves, as opposed to the text that we output for them.
    %
    % For example, the parser can generate two error specs for a bad module
    % name that differ in two things.
    %
    % - The first difference is that one has "severity_error", while the other
    %   has "severity_conditional(warn_wrong_module_name, yes, severity_error,
    %   no)". However, since warn_wrong_module_name is yes by default,
    %   this difference has no effect.
    %
    % - The second difference is that some error_msg_components consist of
    %   "always(...)" in one, and "option_is_set(warn_wrong_module_name, yes,
    %   always(...))" in the other. But if warn_wrong_module_name is yes,
    %   this difference has no effect either.
    %
    % (The parser should no longer generate duplicate error messages
    % for bad module names, but we still keep this workaround in place,
    % since the cost of doing so is trivial.)
    %
    list.filter_map(remove_conditionals_in_spec(Globals), !Specs),
    list.sort_and_remove_dups(compare_error_specs(Globals), !Specs).

:- pred remove_conditionals_in_spec(globals::in,
    error_spec::in, error_spec::out) is semidet.

remove_conditionals_in_spec(Globals, Spec0, Spec) :-
    require_det (
        (
            Spec0 = error_spec(Id, Severity0, Phase, Msgs0),
            MaybeActualSeverity = actual_error_severity(Globals, Severity0),
            list.filter_map(remove_conditionals_in_msg(Globals), Msgs0, Msgs)
        ;
            Spec0 = simplest_spec(Id, Severity0, Phase, Context0, Pieces0),
            MaybeActualSeverity = actual_error_severity(Globals, Severity0),
            Msgs = [simplest_msg(Context0, Pieces0)]
        ;
            Spec0 = conditional_spec(Id, Option, MatchValue,
                Severity0, Phase, Msgs0),
            globals.lookup_bool_option(Globals, Option, OptionValue),
            ( if OptionValue = MatchValue then
                MaybeActualSeverity =
                    actual_error_severity(Globals, Severity0),
                Msgs = Msgs0
            else
                MaybeActualSeverity = no,
                Msgs = []
            )
        )
    ),
    ( if
        MaybeActualSeverity = yes(ActualSeverity),
        Msgs = [_ | _]
    then
        require_det (
            (
                ActualSeverity = actual_severity_error,
                Severity = severity_error
            ;
                ActualSeverity = actual_severity_warning,
                Severity = severity_warning
            ;
                ActualSeverity = actual_severity_informational,
                Severity = severity_informational
            ),
            Spec = error_spec(Id, Severity, Phase, Msgs)
        )
    else
        % Spec0 would result in nothing being printed.
        fail
    ).

:- pred remove_conditionals_in_msg(globals::in,
    error_msg::in, error_msg::out) is semidet.

remove_conditionals_in_msg(Globals, Msg0, Msg) :-
    require_det (
        (
            Msg0 = simplest_msg(Context, Pieces0),
            Components0 = [always(Pieces0)],
            MaybeContext = yes(Context),
            TreatAsFirst = do_not_treat_as_first,
            ExtraIndent = 0
        ;
            Msg0 = simple_msg(Context, Components0),
            MaybeContext = yes(Context),
            TreatAsFirst = do_not_treat_as_first,
            ExtraIndent = 0
        ;
            Msg0 = error_msg(MaybeContext, TreatAsFirst, ExtraIndent,
                Components0)
        ),
        list.foldl(remove_conditionals_in_msg_component(Globals),
            Components0, cord.init, ComponentCord),
        Components = cord.list(ComponentCord),
        Msg = error_msg(MaybeContext, TreatAsFirst, ExtraIndent, Components)
    ),
    % Don't include the Msg if Components is empty.
    Components = [_ | _].

:- pred remove_conditionals_in_msg_component(globals::in,
    error_msg_component::in,
    cord(error_msg_component)::in, cord(error_msg_component)::out) is det.

remove_conditionals_in_msg_component(Globals, Component, !ComponentCord) :-
    (
        Component = option_is_set(Option, MatchValue, EmbeddedComponents),
        % We could recurse down into EmbeddedComponents, but we currently
        % have any places in the compiler that can generate two error messages
        % that differ only in nested option settings, so there would be
        % no point.
        globals.lookup_bool_option(Globals, Option, OptionValue),
        ( if OptionValue = MatchValue then
            !:ComponentCord =
                !.ComponentCord ++ cord.from_list(EmbeddedComponents)
        else
            true
        )
    ;
        % We don't want to eliminate the verbose only part of a message
        % even if verbose_errors isn't set. We want to keep them around
        % until we print the component, so that we can record the presence
        % of such verbose components, and generate a reminder of their
        % existence at the end of the compilation.
        %
        % Besides, the compiler can't (yet) generate two error_msg_components
        % that differ only in the presence of a verbose error.
        ( Component = always(_)
        ; Component = verbose_only(_, _)
        ; Component = verbose_and_nonverbose(_, _)
        ; Component = print_anything(_)
        ),
        !:ComponentCord = cord.snoc(!.ComponentCord, Component)
    ).

%---------------------------------------------------------------------------%

:- pred compare_error_specs(globals::in, error_spec::in, error_spec::in,
    comparison_result::out) is det.

compare_error_specs(Globals, SpecA, SpecB, Result) :-
    extract_spec_msgs(Globals, SpecA, MsgsA),
    extract_spec_msgs(Globals, SpecB, MsgsB),
    compare_error_msg_lists(MsgsA, MsgsB, MsgsResult),
    (
        MsgsResult = (=),
        compare(Result, SpecA, SpecB)
    ;
        ( MsgsResult = (>)
        ; MsgsResult = (<)
        ),
        Result = MsgsResult
    ).

:- pred compare_error_msg_lists(list(error_msg)::in, list(error_msg)::in,
    comparison_result::out) is det.

compare_error_msg_lists(MsgsA, MsgsB, Result) :-
    (
        MsgsA = [],
        MsgsB = [],
        Result = (=)
    ;
        MsgsA = [],
        MsgsB = [_ | _],
        Result = (<)
    ;
        MsgsA = [_ | _],
        MsgsB = [],
        Result = (>)
    ;
        MsgsA = [HeadMsgA | TailMsgsA],
        MsgsB = [HeadMsgB | TailMsgsB],
        compare_error_msgs(HeadMsgA, HeadMsgB, HeadResult),
        (
            HeadResult = (=),
            compare_error_msg_lists(TailMsgsA, TailMsgsB, Result)
        ;
            ( HeadResult = (>)
            ; HeadResult = (<)
            ),
            Result = HeadResult
        )
    ).

sort_error_msgs(Msgs0, Msgs) :-
    list.sort_and_remove_dups(compare_error_msgs, Msgs0, Msgs).

:- pred compare_error_msgs(error_msg::in, error_msg::in,
    comparison_result::out) is det.

compare_error_msgs(MsgA, MsgB, Result) :-
    MaybeContextA = project_msg_context(MsgA),
    MaybeContextB = project_msg_context(MsgB),
    compare(ContextResult, MaybeContextA, MaybeContextB),
    (
        ContextResult = (=),
        ComponentsA = project_msg_components(MsgA),
        ComponentsB = project_msg_components(MsgB),
        compare(ComponentsResult, ComponentsA, ComponentsB),
        (
            ComponentsResult = (=),
            compare(Result, MsgA, MsgB)
        ;
            ( ComponentsResult = (>)
            ; ComponentsResult = (<)
            ),
            Result = ComponentsResult
        )
    ;
        ( ContextResult = (>)
        ; ContextResult = (<)
        ),
        Result = ContextResult
    ).

:- func project_msg_context(error_msg) = maybe(prog_context).

project_msg_context(Msg) = MaybeContext :-
    (
        Msg = simplest_msg(Context, _),
        MaybeContext = yes(Context)
    ;
        Msg = simple_msg(Context, _),
        MaybeContext = yes(Context)
    ;
        Msg = error_msg(yes(Context), _, _, _),
        MaybeContext = yes(Context)
    ;
        Msg = error_msg(no, _, _, __),
        MaybeContext = no
    ).

:- func project_msg_components(error_msg) = list(error_msg_component).

project_msg_components(Msg) = Components :-
    (
        Msg = simplest_msg(_, Pieces),
        Components = [always(Pieces)]
    ;
        Msg = simple_msg(_, Components)
    ;
        Msg = error_msg(_, _, _, Components)
    ).

%---------------------------------------------------------------------------%

filter_interface_generation_specs(Globals, Specs, SpecsToPrint, !IO) :-
    globals.lookup_bool_option(Globals,
        print_errors_warnings_when_generating_interface, PrintErrors),
    (
        PrintErrors = yes,
        SpecsToPrint = Specs
    ;
        PrintErrors = no,
        io.get_exit_status(Status, !IO),
        ( if Status = 0 then
            SpecsToPrint = []
        else
            SpecsToPrint = Specs
        )
    ).

%---------------------------------------------------------------------------%

:- type error_spec_accumulator == maybe(pair(set(error_spec))).

init_error_spec_accumulator = no.

accumulate_error_specs_for_proc(ProcSpecs, !MaybeSpecs) :-
    list.filter(
        ( pred(Spec::in) is semidet :-
            Phase = project_spec_phase(Spec),
            ModeReportControl = get_maybe_mode_report_control(Phase),
            ModeReportControl = yes(report_only_if_in_all_modes)
        ), ProcSpecs, ProcAllModeSpecs, ProcAnyModeSpecs),
    ProcAnyModeSpecSet = set.list_to_set(ProcAnyModeSpecs),
    ProcAllModeSpecSet = set.list_to_set(ProcAllModeSpecs),
    (
        !.MaybeSpecs = yes(AnyModeSpecSet0 - AllModeSpecSet0),
        set.union(AnyModeSpecSet0, ProcAnyModeSpecSet, AnyModeSpecSet),
        set.intersect(AllModeSpecSet0, ProcAllModeSpecSet, AllModeSpecSet),
        !:MaybeSpecs = yes(AnyModeSpecSet - AllModeSpecSet)
    ;
        !.MaybeSpecs = no,
        !:MaybeSpecs = yes(ProcAnyModeSpecSet - ProcAllModeSpecSet)
    ).

:- func project_spec_phase(error_spec) = error_phase.

project_spec_phase(Spec) = Phase :-
    (
        Spec = error_spec(_, _, Phase, _)
    ;
        Spec = simplest_spec(_, _, Phase, _, _)
    ;
        Spec = conditional_spec(_, _, _, _, Phase, _)
    ).

error_spec_accumulator_to_list(no) = [].
error_spec_accumulator_to_list(yes(AnyModeSpecSet - AllModeSpecSet)) =
    set.to_sorted_list(set.union(AnyModeSpecSet, AllModeSpecSet)).

:- func get_maybe_mode_report_control(error_phase) =
    maybe(mode_report_control).

get_maybe_mode_report_control(phase_options) = no.
get_maybe_mode_report_control(phase_read_files) = no.
get_maybe_mode_report_control(phase_module_name) = no.
get_maybe_mode_report_control(phase_term_to_parse_tree) = no.
get_maybe_mode_report_control(phase_parse_tree_to_hlds) = no.
get_maybe_mode_report_control(phase_expand_types) = no.
get_maybe_mode_report_control(phase_type_check) = no.
get_maybe_mode_report_control(phase_inst_check) = no.
get_maybe_mode_report_control(phase_polymorphism) = no.
get_maybe_mode_report_control(phase_mode_check(Control)) = yes(Control).
get_maybe_mode_report_control(phase_purity_check) = no.
get_maybe_mode_report_control(phase_detism_check) = no.
get_maybe_mode_report_control(phase_oisu_check) = no.
get_maybe_mode_report_control(phase_simplify(Control)) = yes(Control).
get_maybe_mode_report_control(phase_style) = no.
get_maybe_mode_report_control(phase_dead_code) = no.
get_maybe_mode_report_control(phase_termination_analysis) = no.
get_maybe_mode_report_control(phase_accumulator_intro) = no.
get_maybe_mode_report_control(phase_auto_parallelism) = no.
get_maybe_mode_report_control(phase_interface_gen) = no.
get_maybe_mode_report_control(phase_code_gen) = no.

%---------------------------------------------------------------------------%

pre_hlds_maybe_write_out_errors(Verbose, Globals, !Specs, !IO) :-
    io.output_stream(Stream, !IO),
    pre_hlds_maybe_write_out_errors(Stream, Verbose, Globals, !Specs, !IO).

pre_hlds_maybe_write_out_errors(Stream, Verbose, Globals, !Specs, !IO) :-
    % maybe_write_out_errors in hlds_error_util.m is a HLDS version
    % of this predicate. The documentation is in that file.
    (
        Verbose = no
    ;
        Verbose = yes,
        write_error_specs_ignore(Stream, !.Specs, Globals, !IO),
        !:Specs = []
    ).

%---------------------------------------------------------------------------%

write_error_spec_ignore(Spec, Globals, !IO) :-
    write_error_spec(Spec, Globals, 0, _, 0, _, !IO).

write_error_spec_ignore(Stream, Spec, Globals, !IO) :-
    write_error_spec(Stream, Spec, Globals, 0, _, 0, _, !IO).

write_error_specs_ignore(Specs, Globals, !IO) :-
    write_error_specs(Specs, Globals, 0, _, 0, _, !IO).

write_error_specs_ignore(Stream, Specs, Globals, !IO) :-
    write_error_specs(Stream, Specs, Globals, 0, _, 0, _, !IO).

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

write_error_spec(Spec, Globals, !NumWarnings, !NumErrors, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_spec(Stream, Spec, Globals, !NumWarnings, !NumErrors, !IO).

write_error_spec(Stream, Spec, Globals, !NumWarnings, !NumErrors, !IO) :-
    do_write_error_spec(Stream, Globals, Spec, !NumWarnings, !NumErrors,
        set.init, _, !IO).

%---------------------%

write_error_specs(Specs0, Globals, !NumWarnings, !NumErrors, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_specs(Stream, Specs0, Globals, !NumWarnings, !NumErrors, !IO).

write_error_specs(Stream, Specs0, Globals, !NumWarnings, !NumErrors, !IO) :-
    sort_error_specs(Globals, Specs0, Specs),
    list.foldl4(do_write_error_spec(Stream, Globals), Specs,
        !NumWarnings, !NumErrors, set.init, _, !IO).

%---------------------%

:- pred do_write_error_spec(io.text_output_stream::in, globals::in,
    error_spec::in, int::in, int::out, int::in, int::out,
    already_printed_verbose::in, already_printed_verbose::out,
    io::di, io::uo) is det.

do_write_error_spec(Stream, Globals, Spec, !NumWarnings, !NumErrors,
        !AlreadyPrintedVerbose, !IO) :-
    (
        Spec = error_spec(Id, Severity, _Phase, Msgs1),
        MaybeActual = actual_error_severity(Globals, Severity)
    ;
        Spec = simplest_spec(Id, Severity, _Phase, Context, Pieces),
        MaybeActual = actual_error_severity(Globals, Severity),
        Msgs1 = [simplest_msg(Context, Pieces)]
    ;
        Spec = conditional_spec(Id, Option, MatchValue,
            Severity, _Phase, Msgs0),
        globals.lookup_bool_option(Globals, Option, Value),
        ( if Value = MatchValue then
            MaybeActual = actual_error_severity(Globals, Severity),
            Msgs1 = Msgs0
        else
            MaybeActual = no,
            Msgs1 = []
        )
    ),
    globals.lookup_bool_option(Globals, print_error_spec_id, PrintId),
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
                HeadMsg = error_msg(MaybeHeadContext, _, _, _)
            ),
            IdMsg = error_msg(MaybeHeadContext, do_not_treat_as_first, 0,
                [always([words("error_spec id:"), fixed(Id), nl])]),
            Msgs = Msgs1 ++ [IdMsg]
        )
    ),
    do_write_error_msgs(Stream, Msgs, Globals, treat_as_first,
        have_not_printed_anything, PrintedSome, !AlreadyPrintedVerbose, !IO),
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
                !:NumErrors = !.NumErrors + 1,
                io.set_exit_status(1, !IO)
            ;
                Actual = actual_severity_warning,
                !:NumWarnings = !.NumWarnings + 1,
                record_warning(Globals, !IO)
            ;
                Actual = actual_severity_informational
            )
        ;
            MaybeActual = no,
            unexpected($pred, "printed_something but MaybeActual = no")
        )
    ).

:- type maybe_printed_something
    --->    printed_something
    ;       have_not_printed_anything.

:- type maybe_lower_next_initial
    --->    lower_next_initial
    ;       do_not_lower_next_initial.

:- type already_printed_verbose == set(list(format_component)).

:- pred do_write_error_msgs(io.text_output_stream::in,
    list(error_msg)::in, globals::in, maybe_treat_as_first::in,
    maybe_printed_something::in, maybe_printed_something::out,
    already_printed_verbose::in, already_printed_verbose::out,
    io::di, io::uo) is det.

do_write_error_msgs(_Stream, [], _Globals, _First, !PrintedSome,
        !AlreadyPrintedVerbose, !IO).
do_write_error_msgs(Stream, [Msg | Msgs], Globals, !.First, !PrintedSome,
        !AlreadyPrintedVerbose, !IO) :-
    (
        Msg = simplest_msg(SimpleContext, Pieces),
        Components = [always(Pieces)],
        MaybeContext = yes(SimpleContext),
        TreatAsFirst = do_not_treat_as_first,
        ExtraIndentLevel = 0
    ;
        Msg = simple_msg(SimpleContext, Components),
        MaybeContext = yes(SimpleContext),
        TreatAsFirst = do_not_treat_as_first,
        ExtraIndentLevel = 0
    ;
        Msg = error_msg(MaybeContext, TreatAsFirst, ExtraIndentLevel,
            Components)
    ),
    (
        TreatAsFirst = treat_as_first,
        !:First = treat_as_first
    ;
        TreatAsFirst = do_not_treat_as_first
        % Leave !:First as it is, even if it is treat_as_first.
    ),
    Indent = ExtraIndentLevel * indent_increment,
    write_msg_components(Stream, Components, MaybeContext, Indent, Globals,
        !First, !PrintedSome, !AlreadyPrintedVerbose, !IO),
    do_write_error_msgs(Stream, Msgs, Globals, !.First, !PrintedSome,
        !AlreadyPrintedVerbose, !IO).

:- pred write_msg_components(io.text_output_stream::in,
    list(error_msg_component)::in, maybe(prog_context)::in,
    int::in, globals::in, maybe_treat_as_first::in, maybe_treat_as_first::out,
    maybe_printed_something::in, maybe_printed_something::out,
    already_printed_verbose::in, already_printed_verbose::out,
    io::di, io::uo) is det.

write_msg_components(_Stream, [], _, _, _, !First, !PrintedSome,
        !AlreadyPrintedVerbose, !IO).
write_msg_components(Stream, [Component | Components], MaybeContext, Indent,
        Globals, !First, !PrintedSome, !AlreadyPrintedVerbose, !IO) :-
    (
        Component = always(ComponentPieces),
        do_write_error_pieces(Stream, !.First, MaybeContext, Indent, Globals,
            ComponentPieces, !IO),
        !:First = do_not_treat_as_first,
        !:PrintedSome = printed_something
    ;
        Component = option_is_set(Option, MatchValue, EmbeddedComponents),
        globals.lookup_bool_option(Globals, Option, OptionValue),
        ( if OptionValue = MatchValue then
            write_msg_components(Stream, EmbeddedComponents, MaybeContext,
                Indent, Globals, !First, !PrintedSome,
                !AlreadyPrintedVerbose, !IO)
        else
            true
        )
    ;
        Component = verbose_only(AlwaysOrOnce, ComponentPieces),
        globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
        (
            VerboseErrors = yes,
            (
                AlwaysOrOnce = verbose_always,
                do_write_error_pieces(Stream, !.First, MaybeContext,
                    Indent, Globals, ComponentPieces, !IO),
                !:First = do_not_treat_as_first,
                !:PrintedSome = printed_something
            ;
                AlwaysOrOnce = verbose_once,
                ( if
                    set.contains(!.AlreadyPrintedVerbose, ComponentPieces)
                then
                    true
                else
                    do_write_error_pieces(Stream, !.First, MaybeContext,
                        Indent, Globals, ComponentPieces, !IO),
                    !:First = do_not_treat_as_first,
                    !:PrintedSome = printed_something,
                    set.insert(ComponentPieces, !AlreadyPrintedVerbose)
                )
            )
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO)
        )
    ;
        Component = verbose_and_nonverbose(VerbosePieces, NonVerbosePieces),
        globals.lookup_bool_option(Globals, verbose_errors, VerboseErrors),
        (
            VerboseErrors = yes,
            do_write_error_pieces(Stream, !.First, MaybeContext,
                Indent, Globals, VerbosePieces, !IO)
        ;
            VerboseErrors = no,
            do_write_error_pieces(Stream, !.First, MaybeContext,
                Indent, Globals, NonVerbosePieces, !IO),
            globals.io_set_extra_error_info(yes, !IO)
        ),
        !:First = do_not_treat_as_first,
        !:PrintedSome = printed_something
    ;
        Component = print_anything(Anything),
        print_anything(Anything, !IO),
        !:First = do_not_treat_as_first,
        !:PrintedSome = printed_something
    ),
    write_msg_components(Stream, Components, MaybeContext, Indent, Globals,
        !First, !PrintedSome, !AlreadyPrintedVerbose, !IO).

%---------------------------------------------------------------------------%

:- type maybe_first_in_msg
    --->    first_in_msg
    ;       not_first_in_msg.

string_to_words_piece(Str) = words(Str).

list_to_pieces([]) = [].
list_to_pieces([Elem]) = [words(Elem)].
list_to_pieces([Elem1, Elem2]) = [fixed(Elem1), words("and"), fixed(Elem2)].
list_to_pieces([Elem1, Elem2, Elem3 | Elems]) =
    [fixed(Elem1 ++ ",") | list_to_pieces([Elem2, Elem3 | Elems])].

strict_list_to_pieces([]) = [].
strict_list_to_pieces([Elem]) = [words(Elem)].
strict_list_to_pieces([Elem1, Elem2 | Elems]) =
    [fixed(Elem1 ++ ",") | strict_list_to_pieces([Elem2 | Elems])].

list_to_quoted_pieces([]) = [].
list_to_quoted_pieces([Elem]) = [quote(Elem)].
list_to_quoted_pieces([Elem1, Elem2]) =
    [quote(Elem1), words("and"), quote(Elem2)].
list_to_quoted_pieces([Elem1, Elem2, Elem3 | Elems]) =
    [quote(Elem1), suffix(",") |
        list_to_quoted_pieces([Elem2, Elem3 | Elems])].

list_to_quoted_pieces_or([]) = [].
list_to_quoted_pieces_or([Elem]) = [quote(Elem)].
list_to_quoted_pieces_or([Elem1, Elem2]) =
    [quote(Elem1), words("or"), quote(Elem2)].
list_to_quoted_pieces_or([Elem1, Elem2, Elem3 | Elems]) =
    [quote(Elem1), suffix(",") |
        list_to_quoted_pieces_or([Elem2, Elem3 | Elems])].

component_lists_to_pieces(_, []) = [].
component_lists_to_pieces(_, [Comps]) = Comps.
component_lists_to_pieces(LastSep, [Comps1, Comps2]) =
    Comps1 ++ [words(LastSep)] ++ Comps2.
component_lists_to_pieces(LastSep, [Comps1, Comps2, Comps3 | Comps]) =
    Comps1 ++ [suffix(",")]
    ++ component_lists_to_pieces(LastSep, [Comps2, Comps3 | Comps]).

strict_component_lists_to_pieces([]) = [].
strict_component_lists_to_pieces([Comps]) = Comps.
strict_component_lists_to_pieces([Comps1, Comps2 | Comps]) =
    Comps1 ++ [suffix(",")]
    ++ strict_component_lists_to_pieces([Comps2 | Comps]).

component_list_to_pieces(_, []) = [].
component_list_to_pieces(_, [Comp]) = [Comp].
component_list_to_pieces(LastSep, [Comp1, Comp2]) =
    [Comp1, words(LastSep), Comp2].
component_list_to_pieces(LastSep, [Comp1, Comp2, Comp3 | Comps]) =
    [Comp1, suffix(",")]
    ++ component_list_to_pieces(LastSep, [Comp2, Comp3 | Comps]).

strict_component_list_to_pieces([]) = [].
strict_component_list_to_pieces([Comp]) = [Comp].
strict_component_list_to_pieces([Comp1, Comp2 | Comps]) =
    [Comp1, suffix(",")]
    ++ strict_component_list_to_pieces([Comp2 | Comps]).

component_list_to_line_pieces([], _) = [].
component_list_to_line_pieces([Comps], Final) = Comps ++ Final ++ [nl].
component_list_to_line_pieces([Comps1, Comps2 | CompLists], Final) =
    Comps1 ++ [suffix(","), nl]
    ++ component_list_to_line_pieces([Comps2 | CompLists], Final).

choose_number([], _Singular, Plural) = Plural.
choose_number([_], Singular, _Plural) = Singular.
choose_number([_, _ | _], _Singular, Plural) = Plural.

is_or_are([]) = "" :-
    unexpected($pred, "[]").
is_or_are([_]) = "is".
is_or_are([_, _ | _]) = "are".

%---------------------------------------------------------------------------%

write_error_pieces_plain(Globals, Components, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_pieces_plain(Stream, Globals, Components, !IO).

write_error_pieces_plain(Stream, Globals, Components, !IO) :-
    do_write_error_pieces(Stream, treat_as_first, no, 0,
        Globals, Components, !IO).

%---------------------%

write_error_plain_with_progname(ProgName, Msg, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_plain_with_progname(Stream, ProgName, Msg, !IO).

write_error_plain_with_progname(Stream, ProgName, Msg, !IO) :-
    MaxWidth = 79,
    LinesInMsg = string.split_at_char('\n', Msg),
    convert_lines_in_msg_to_pieces(LinesInMsg, LinesInMsgPieces),
    Components = [fixed(ProgName ++ ":") | LinesInMsgPieces],
    do_write_error_pieces_params(Stream, treat_as_first, no, 0,
        yes(MaxWidth), map.init, Components, !IO).

:- pred convert_lines_in_msg_to_pieces(list(string)::in,
    list(format_component)::out) is det.

convert_lines_in_msg_to_pieces([], []).
convert_lines_in_msg_to_pieces([Line | Lines], Pieces) :-
    convert_lines_in_msg_to_pieces(Lines, TailPieces),
    Pieces = [words(Line), nl | TailPieces].

%---------------------------------------------------------------------------%

write_error_pieces(Globals, Context, Indent, Components, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_pieces(Stream, Globals, Context, Indent, Components, !IO).

write_error_pieces(Stream, Globals, Context, Indent, Components, !IO) :-
    do_write_error_pieces(Stream, treat_as_first, yes(Context), Indent,
        Globals, Components, !IO).

%---------------------%

write_error_pieces_maybe_with_context(Globals, MaybeContext,
        Indent, Components, !IO) :-
    io.output_stream(Stream, !IO),
    write_error_pieces_maybe_with_context(Stream, Globals, MaybeContext,
        Indent, Components, !IO).

write_error_pieces_maybe_with_context(Stream, Globals, MaybeContext, Indent,
        Components, !IO) :-
    do_write_error_pieces(Stream, treat_as_first, MaybeContext, Indent,
        Globals, Components, !IO).

%---------------------------------------------------------------------------%

:- pred do_write_error_pieces(io.text_output_stream::in,
    maybe_treat_as_first::in, maybe(prog_context)::in, int::in, globals::in,
    list(format_component)::in, io::di, io::uo) is det.

do_write_error_pieces(Stream, TreatAsFirst, MaybeContext, FixedIndent,
        Globals, Components, !IO) :-
    globals.lookup_maybe_int_option(Globals, max_error_line_width,
        MaybeMaxWidth),
    globals.get_limit_error_contexts_map(Globals, LimitErrorContextsMap),
    do_write_error_pieces_params(Stream, TreatAsFirst, MaybeContext,
        FixedIndent, MaybeMaxWidth, LimitErrorContextsMap, Components, !IO).

:- pred do_write_error_pieces_params(io.text_output_stream::in,
    maybe_treat_as_first::in, maybe(prog_context)::in,
    int::in, maybe(int)::in, limit_error_contexts_map::in,
    list(format_component)::in, io::di, io::uo) is det.

do_write_error_pieces_params(Stream, TreatAsFirst, MaybeContext, FixedIndent,
        MaybeMaxWidth, LimitErrorContextsMap, Components, !IO) :-
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
            io_set_some_errors_were_context_limited(yes, !IO),
            MaybeContextLength = no
        else
            string.count_codepoints(FileName, FileNameLength),
            string.int_to_string(LineNumber, LineNumberStr),
            string.count_codepoints(LineNumberStr, LineNumberStrLength0),
            ( if LineNumberStrLength0 < 3 then
                LineNumberStrLength = 3
            else
                LineNumberStrLength = LineNumberStrLength0
            ),
            MaybeContextLength =
                yes(FileNameLength + 1 + LineNumberStrLength + 2)
        )
    ;
        MaybeContext = no,
        MaybeContextLength = yes(0)
    ),
    (
        MaybeContextLength = no
        % Suppress the printing of the error pieces.
    ;
        MaybeContextLength = yes(ContextLength),
        (
            Components = []
            % There are no error pieces to print. Don't print the context
            % at the start of a line followed by nothing.
            %
            % This can happen if e.g. the original error_msg_component was
            % verbose_and_nonverbose(SomePieces, []), and this compiler
            % invocation is not printing verbose errors.
        ;
            Components = [_ | _],
            convert_components_to_paragraphs(Components, Paragraphs),
            FirstIndent = (if TreatAsFirst = treat_as_first then 0 else 1),
            (
                MaybeMaxWidth = yes(MaxWidth),
                Remain = MaxWidth - (ContextLength + FixedIndent),
                MaybeRemain = yes(Remain)
            ;
                MaybeMaxWidth = no,
                MaybeRemain = no
            ),
            divide_paragraphs_into_lines(TreatAsFirst, FirstIndent, Paragraphs,
                MaybeRemain, Lines),
            write_msg_lines(Stream, Lines, MaybeContext, FixedIndent, !IO)
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

:- func indent_increment = int.

indent_increment = 2.

:- pred write_msg_lines(io.text_output_stream::in, list(error_line)::in,
    maybe(prog_context)::in, int::in, io::di, io::uo) is det.

write_msg_lines(_Stream, [], _, _, !IO).
write_msg_lines(Stream, [Line | Lines], MaybeContext, FixedIndent, !IO) :-
    (
        MaybeContext = yes(Context),
        prog_out.write_context(Stream, Context, !IO)
    ;
        MaybeContext = no
    ),
    Line = error_line(LineIndent, LineWords),
    Indent = FixedIndent + LineIndent * indent_increment,
    string.pad_left("", ' ', Indent, IndentStr),
    io.write_string(Stream, IndentStr, !IO),
    error_util.write_msg_line(Stream, LineWords, !IO),
    write_msg_lines(Stream, Lines, MaybeContext, FixedIndent, !IO).

:- pred write_msg_line(io.text_output_stream::in, list(string)::in,
    io::di, io::uo) is det.

write_msg_line(Stream, [], !IO) :-
    io.write_char(Stream, '\n', !IO).
write_msg_line(Stream, [Word | Words], !IO) :-
    io.write_string(Stream, Word, !IO),
    write_msg_line_rest(Stream, Words, !IO),
    io.write_char(Stream, '\n', !IO).

:- pred write_msg_line_rest(io.text_output_stream::in, list(string)::in,
    io::di, io::uo) is det.

write_msg_line_rest(_Stream, [], !IO).
write_msg_line_rest(Stream, [Word | Words], !IO) :-
    io.write_char(Stream, ' ', !IO),
    io.write_string(Stream, Word, !IO),
    write_msg_line_rest(Stream, Words, !IO).

error_pieces_to_string(Components) =
    error_pieces_to_string_2(first_in_msg, Components).

:- func error_pieces_to_string_2(maybe_first_in_msg, list(format_component))
    = string.

error_pieces_to_string_2(_, []) = "".
error_pieces_to_string_2(FirstInMsg, [Component | Components]) = Str :-
    TailStr = error_pieces_to_string_2(not_first_in_msg, Components),
    (
        Component = words(Words),
        Str = join_string_and_tail(Words, Components, TailStr)
    ;
        Component = words_quote(Words),
        Str = join_string_and_tail(add_quotes(Words), Components, TailStr)
    ;
        Component = fixed(Word),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = quote(Word),
        Str = join_string_and_tail(add_quotes(Word), Components, TailStr)
    ;
        Component = int_fixed(Int),
        Str = join_string_and_tail(int_to_string(Int), Components, TailStr)
    ;
        Component = nth_fixed(Int),
        Str = join_string_and_tail(nth_fixed_str(Int), Components, TailStr)
    ;
        Component = lower_case_next_if_not_first,
        (
            FirstInMsg = first_in_msg,
            Str = TailStr
        ;
            FirstInMsg = not_first_in_msg,
            Str = uncapitalize_first(TailStr)
        )
    ;
        Component = prefix(Prefix),
        Str = Prefix ++ TailStr
    ;
        Component = suffix(Suffix),
        Str = join_string_and_tail(Suffix, Components, TailStr)
    ;
        (
            Component = qual_sym_name(SymName)
        ;
            Component = unqual_sym_name(SymName0),
            SymName = unqualified(unqualify_name(SymName0))
        ),
        Word = sym_name_to_word(SymName),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        (
            Component = qual_sym_name_arity(SymNameAndArity)
        ;
            Component = unqual_sym_name_arity(SymNameAndArity0),
            SymNameAndArity0 = sym_name_arity(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0)),
            SymNameAndArity = sym_name_arity(SymName, Arity)
        ),
        Word = sym_name_arity_to_word(SymNameAndArity),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        (
            Component = qual_pf_sym_name_orig_arity(PFSymNameArity)
        ;
            Component = unqual_pf_sym_name_orig_arity(PFSymNameArity0),
            PFSymNameArity0 = pf_sym_name_arity(PF, SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0)),
            PFSymNameArity = pf_sym_name_arity(PF, SymName, Arity)
        ),
        Word = pf_sym_name_orig_arity_to_string(PFSymNameArity),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        (
            Component = qual_cons_id_and_maybe_arity(ConsId0),
            strip_builtin_qualifier_from_cons_id(ConsId0, ConsId)
        ;
            Component = unqual_cons_id_and_maybe_arity(ConsId0),
            strip_module_qualifier_from_cons_id(ConsId0, ConsId)
        ),
        Word = maybe_quoted_cons_id_and_arity_to_string(ConsId),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        (
            Component = qual_type_ctor(TypeCtor),
            TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity)
        ;
            Component = unqual_type_ctor(TypeCtor0),
            TypeCtor0 = type_ctor(TypeCtorSymName0, TypeCtorArity),
            TypeCtorSymName = unqualified(unqualify_name(TypeCtorSymName0))
        ),
        SymNameAndArity = sym_name_arity(TypeCtorSymName, TypeCtorArity),
        Word = sym_name_arity_to_word(SymNameAndArity),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = qual_top_ctor_of_type(Type),
        type_to_ctor_det(Type, TypeCtor),
        TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity),
        SymNameArity = sym_name_arity(TypeCtorSymName, TypeCtorArity),
        Word = sym_name_arity_to_word(SymNameArity),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = p_or_f(PredOrFunc),
        Word = pred_or_func_to_string(PredOrFunc),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = decl(Decl),
        Word = add_quotes(":- " ++ Decl),
        Str = join_string_and_tail(Word, Components, TailStr)
    ;
        Component = pragma_decl(PragmaName),
        Word = add_quotes(":- pragma " ++ PragmaName),
        Str = join_string_and_tail(Word, Components, TailStr)
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
    ;
        ( Component = invis_order_default_start(_)
        ; Component = invis_order_default_end(_)
        ),
        Str = TailStr
    ).

:- func nth_fixed_str(int) = string.

nth_fixed_str(N) = Str :-
    ( if N = 1 then
        Str = "first"
    else if N = 2 then
        Str = "second"
    else if N = 3 then
        Str = "third"
    else if N = 4 then
        Str = "fourth"
    else if N = 5 then
        Str = "fifth"
    else if N = 6 then
        Str = "sixth"
    else if N = 7 then
        Str = "seventh"
    else if N = 8 then
        Str = "eighth"
    else if N = 9 then
        Str = "ninth"
    else if N = 10 then
        Str = "tenth"
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

:- func join_string_and_tail(string, list(format_component), string) = string.

join_string_and_tail(Word, Components, TailStr) = Str :-
    ( if TailStr = "" then
        Str = Word
    else if Components = [suffix(_) | _] then
        Str = Word ++ TailStr
    else
        Str = Word ++ " " ++ TailStr
    ).

%---------------------------------------------------------------------------%

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
    convert_components_to_paragraphs_acc(first_in_msg, Components,
        [], cord.empty, ParasCord),
    Paras = cord.list(ParasCord).

:- type word
    --->    plain_word(string)
    ;       prefix_word(string)
    ;       suffix_word(string)
    ;       lower_next_word.

:- pred convert_components_to_paragraphs_acc(maybe_first_in_msg::in,
    list(format_component)::in, list(word)::in,
    cord(paragraph)::in, cord(paragraph)::out) is det.

convert_components_to_paragraphs_acc(_, [], RevWords0, !Paras) :-
    Strings = rev_words_to_strings(RevWords0),
    !:Paras = snoc(!.Paras, paragraph(Strings, 0, 0)).
convert_components_to_paragraphs_acc(FirstInMsg, [Component | Components],
        RevWords0, !Paras) :-
    (
        Component = words(WordsStr),
        break_into_words(WordsStr, RevWords0, RevWords1)
    ;
        Component = words_quote(WordsStr),
        break_into_words(add_quotes(WordsStr), RevWords0, RevWords1)
    ;
        Component = fixed(Word),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Component = quote(Word),
        RevWords1 = [plain_word(add_quotes(Word)) | RevWords0]
    ;
        Component = int_fixed(Int),
        RevWords1 = [plain_word(int_to_string(Int)) | RevWords0]
    ;
        Component = nth_fixed(Int),
        RevWords1 = [plain_word(nth_fixed_str(Int)) | RevWords0]
    ;
        Component = lower_case_next_if_not_first,
        (
            FirstInMsg = first_in_msg,
            RevWords1 = RevWords0
        ;
            FirstInMsg = not_first_in_msg,
            RevWords1 = [lower_next_word | RevWords0]
        )
    ;
        Component = prefix(Word),
        RevWords1 = [prefix_word(Word) | RevWords0]
    ;
        Component = suffix(Word),
        RevWords1 = [suffix_word(Word) | RevWords0]
    ;
        (
            Component = qual_sym_name(SymName)
        ;
            Component = unqual_sym_name(SymName0),
            SymName = unqualified(unqualify_name(SymName0))
        ),
        RevWords1 = [plain_word(sym_name_to_word(SymName)) | RevWords0]
    ;
        (
            Component = qual_sym_name_arity(SymNameAndArity)
        ;
            Component = unqual_sym_name_arity(SymNameAndArity0),
            SymNameAndArity0 = sym_name_arity(SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0)),
            SymNameAndArity = sym_name_arity(SymName, Arity)
        ),
        Word = sym_name_arity_to_word(SymNameAndArity),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        (
            Component = qual_pf_sym_name_orig_arity(PFSymNameArity)
        ;
            Component = unqual_pf_sym_name_orig_arity(PFSymNameArity0),
            PFSymNameArity0 = pf_sym_name_arity(PF, SymName0, Arity),
            SymName = unqualified(unqualify_name(SymName0)),
            PFSymNameArity = pf_sym_name_arity(PF, SymName, Arity)
        ),
        WordsStr = pf_sym_name_orig_arity_to_string(PFSymNameArity),
        break_into_words(WordsStr, RevWords0, RevWords1)
    ;
        (
            Component = qual_cons_id_and_maybe_arity(ConsId0),
            strip_builtin_qualifier_from_cons_id(ConsId0, ConsId)
        ;
            Component = unqual_cons_id_and_maybe_arity(ConsId0),
            strip_module_qualifier_from_cons_id(ConsId0, ConsId)
        ),
        Word = maybe_quoted_cons_id_and_arity_to_string(ConsId),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        (
            Component = qual_type_ctor(TypeCtor),
            TypeCtor = type_ctor(TypeCtorSymName, TypeCtorArity)
        ;
            Component = unqual_type_ctor(TypeCtor0),
            TypeCtor0 = type_ctor(TypeCtorSymName0, TypeCtorArity),
            TypeCtorSymName = unqualified(unqualify_name(TypeCtorSymName0))
        ),
        SymNameAndArity = sym_name_arity(TypeCtorSymName, TypeCtorArity),
        Word = sym_name_arity_to_word(SymNameAndArity),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Component = qual_top_ctor_of_type(Type),
        type_to_ctor_det(Type, TypeCtor),
        TypeCtor = type_ctor(TypeCtorName, TypeCtorArity),
        SymNameArity = sym_name_arity(TypeCtorName, TypeCtorArity),
        NewWord = plain_word(sym_name_arity_to_word(SymNameArity)),
        RevWords1 = [NewWord | RevWords0]
    ;
        Component = p_or_f(PredOrFunc),
        Word = pred_or_func_to_string(PredOrFunc),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Component = decl(DeclName),
        Word = add_quotes(":- " ++ DeclName),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Component = pragma_decl(PragmaName),
        Word = add_quotes(":- pragma " ++ PragmaName),
        RevWords1 = [plain_word(Word) | RevWords0]
    ;
        Component = nl,
        Strings = rev_words_to_strings(RevWords0),
        !:Paras = snoc(!.Paras, paragraph(Strings, 0, 0)),
        RevWords1 = []
    ;
        Component = nl_indent_delta(IndentDelta),
        Strings = rev_words_to_strings(RevWords0),
        !:Paras = snoc(!.Paras, paragraph(Strings, 0, IndentDelta)),
        RevWords1 = []
    ;
        Component = blank_line,
        Strings = rev_words_to_strings(RevWords0),
        !:Paras = snoc(!.Paras, paragraph(Strings, 1, 0)),
        RevWords1 = []
    ;
        ( Component = invis_order_default_start(_)
        ; Component = invis_order_default_end(_)
        ),
        RevWords1 = RevWords0
    ),
    convert_components_to_paragraphs_acc(not_first_in_msg, Components,
        RevWords1, !Paras).

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

:- func sym_name_to_word(sym_name) = string.

sym_name_to_word(SymName) =
    add_quotes(sym_name_to_string(SymName)).

:- func sym_name_arity_to_word(sym_name_arity) = string.

sym_name_arity_to_word(sym_name_arity(SymName, Arity)) =
    add_quotes(sym_name_to_string(SymName)) ++ "/" ++ int_to_string(Arity).

:- pred break_into_words(string::in, list(word)::in, list(word)::out) is det.

break_into_words(String, Words0, Words) :-
    break_into_words_from(String, 0, Words0, Words).

:- pred break_into_words_from(string::in, int::in, list(word)::in,
    list(word)::out) is det.

break_into_words_from(String, Cur, Words0, Words) :-
    ( if find_word_start(String, Cur, Start) then
        find_word_end(String, Start, End),
        string.between(String, Start, End, WordStr),
        break_into_words_from(String, End, [plain_word(WordStr) | Words0],
            Words)
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

:- type error_line
    --->    error_line(
                int,            % Indent level; multiply by indent_increment
                                % to get number of spaces of indentation.
                list(string)    % The words on the line.
            ).

    % Groups the words in the given paragraphs into lines. The first line
    % can have up to Max characters on it; the later lines (if any) up
    % to Max-2 characters.
    %
    % If MaybeMax is `no', handle it as if Max were infinity (i.e. put
    % everything in each paragraph on one line).
    %
    % The given list of paragraphs should be nonempty, since we always return
    % at least one line.
    %
:- pred divide_paragraphs_into_lines(maybe_treat_as_first::in, int::in,
    list(paragraph)::in, maybe(int)::in, list(error_line)::out) is det.

divide_paragraphs_into_lines(TreatAsFirst, CurIndent, Paras, MaybeMax,
        Lines) :-
    (
        Paras = [],
        Lines = []
    ;
        Paras = [FirstPara | LaterParas],
        FirstPara = paragraph(FirstParaWords, NumBlankLines, FirstIndentDelta),
        (
            TreatAsFirst = treat_as_first,
            RestIndent = CurIndent + 1
        ;
            TreatAsFirst = do_not_treat_as_first,
            RestIndent = CurIndent
        ),
        NextIndent = RestIndent + FirstIndentDelta,

        BlankLine = error_line(CurIndent, []),
        list.duplicate(NumBlankLines, BlankLine, FirstParaBlankLines),
        (
            FirstParaWords = [],
            NextTreatAsFirst = TreatAsFirst,
            FirstParaLines = []
        ;
            FirstParaWords = [FirstWord | LaterWords],
            NextTreatAsFirst = do_not_treat_as_first,
            (
                MaybeMax = yes(Max),
                get_line_of_words(FirstWord, LaterWords, CurIndent, Max,
                    LineWords, RestWords),
                CurLine = error_line(CurIndent, LineWords),

                group_nonfirst_line_words(RestWords, RestIndent, Max,
                    FirstParaRestLines),
                FirstParaLines = [CurLine | FirstParaRestLines]
            ;
                MaybeMax = no,
                FirstParaLines = [error_line(CurIndent, FirstParaWords)]
            )
        ),
        divide_paragraphs_into_lines(NextTreatAsFirst, NextIndent, LaterParas,
            MaybeMax, LaterParaLines),
        Lines = FirstParaLines ++ FirstParaBlankLines ++ LaterParaLines
    ).

:- pred group_nonfirst_line_words(list(string)::in, int::in, int::in,
    list(error_line)::out) is det.

group_nonfirst_line_words(Words, Indent, Max, Lines) :-
    (
        Words = [],
        Lines = []
    ;
        Words = [FirstWord | LaterWords],
        get_line_of_words(FirstWord, LaterWords, Indent, Max,
            LineWords, RestWords),
        Line = error_line(Indent, LineWords),
        group_nonfirst_line_words(RestWords, Indent, Max, RestLines),
        Lines = [Line | RestLines]
    ).

:- pred get_line_of_words(string::in, list(string)::in, int::in, int::in,
    list(string)::out, list(string)::out) is det.

get_line_of_words(FirstWord, LaterWords, Indent, Max, Line, RestWords) :-
    string.count_codepoints(FirstWord, FirstWordLen),
    Avail = Max - Indent * indent_increment,
    get_later_words(LaterWords, FirstWordLen, Avail, [FirstWord],
        Line, RestWords).

:- pred get_later_words(list(string)::in, int::in, int::in,
    list(string)::in, list(string)::out, list(string)::out) is det.

get_later_words([], _, _, Line, Line, []).
get_later_words([Word | Words], OldLen, Avail, Line0, Line, RestWords) :-
    string.count_codepoints(Word, WordLen),
    NewLen = OldLen + 1 + WordLen,
    ( if NewLen =< Avail then
        list.append(Line0, [Word], Line1),
        get_later_words(Words, NewLen, Avail, Line1, Line, RestWords)
    else
        Line = Line0,
        RestWords = [Word | Words]
    ).

%---------------------------------------------------------------------------%

describe_sym_name(SymName) =
    string.append_list(["`", sym_name_to_string(SymName), "'"]).

describe_sym_name_arity(sym_name_arity(SymName, Arity)) =
    string.append_list(["`", sym_name_to_string(SymName), "/",
        string.int_to_string(Arity), "'"]).

add_quotes(Str) = "`" ++ Str ++ "'".

%---------------------------------------------------------------------------%

report_warning(Globals, Context, Indent, Components, !IO) :-
    io.output_stream(Stream, !IO),
    report_warning(Stream, Globals, Context, Indent, Components, !IO).

report_warning(Stream, Globals, Context, Indent, Components, !IO) :-
    record_warning(Globals, !IO),
    write_error_pieces(Stream, Globals, Context, Indent, Components, !IO).

%---------------------------------------------------------------------------%

unable_to_open_file(FileName, IOErr, !IO) :-
    io.stderr_stream(StdErr, !IO),
    io.write_string(StdErr, "Unable to open file: '", !IO),
    io.write_string(StdErr, FileName, !IO),
    io.write_string(StdErr, "' because\n", !IO),
    io.write_string(StdErr, io.error_message(IOErr), !IO),
    io.nl(StdErr, !IO),

    io.set_exit_status(1, !IO).

%---------------------------------------------------------------------------%
:- end_module parse_tree.error_util.
%---------------------------------------------------------------------------%
