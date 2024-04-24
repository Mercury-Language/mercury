%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2022-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: error_spec.m.
% Main author: zs.
%
% This module defines the error_spec structure for representing
% diagnostic messages, and utility predicates and functions that can help
% create error_specs.
%
%---------------------------------------------------------------------------%

:- module parse_tree.error_spec.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module edit_seq.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

% Every distinct problem should generate a single error specification,
% whose general form is error_spec(Id, Severity, Phase, Msgs).
% The second, third and fourth fields of this term state respectively
%
% - the severity of the problem (so that we can update the exit status
%   of the compiler accordingly);
% - which phase of the compiler found the problem (since later phases
%   may wish to suppress some problem reports if some specific earlier phases
%   found problems, e.g. when a missing clause could be caused
%   by a syntax error); and
% - a specification of what to print.
%
% In most cases, the "what to print" will be a single message for a single
% context. However, we may want to print messages for several contexts.
% For example, when reporting a duplicate declaration, we want to report
% this fact in the duplicate declaration's context, while printing another
% message giving the original declaration's context.
%
% spec(Id, Severity, Phase, Context, Pieces) is a shorthand for
% (and equivalent in every respect to) error_spec(Id, Severity, Phase,
% [simple_msg(Context, always(Pieces)])]).
%
% no_ctxt_spec(Id, Severity, Phase, Pieces) is shorthand for
% (and equivalent in every respect to) error_spec(Id, Severity, Phase,
% error_msg(maybe.no, treat_based_on_posn, 0, [always(Pieces)])).
%
% conditional_spec(Id, Option, MatchValue, Severity, Phase, Msgs) is intended
% to represent the error specification given by its last three fields
% *iff* Option has the value MatchValue. If Option is *not* MatchValue,
% it asks for nothing to be printed, and for the exit status to be left alone.
%
% The Id field, which is present in all these alternatives, is totally
% ignored when printing error_specs. Its job is something completely different:
% helping developers track down where in the source code each error_spec
% was constructed. Without the id fields, if developers wants to know this,
% e.g. because they do not want the message printed, or because there is
% a problem with its wording, they have to grep for some words in the message.
% However, grepping for a single word will usually get many false hits,
% while grepping for two or more consecutive words in the message may miss
% the code generating the message, because in that code, some of those
% consecutive words may be on different lines. On the other hand, if every
% place that constructs an error_spec, of any of these varieties,
% fills in the id field with $pred, then finding the right place is easy:
% just specify the developer-only option --print-error-spec-id, and
% the identity of the predicate or function that generated each error_spec
% will be output just after the messages in that error_spec. Even if the
% predicate or function that this identifies has several pieces of code
% that construct error_specs, the scope in which you have to search for
% the one you are looking for will be easily manageable.

:- type error_spec
    --->    spec(
                % ZZZ field name prefixes
                simp_id                 :: string,
                simp_spec_severity      :: error_severity,
                simp_spec_phase         :: error_phase,
                simp_spec_context       :: prog_context,
                simp_spec_pieces        :: list(format_piece)
            )
    ;       no_ctxt_spec(
                simpnc_id               :: string,
                simpnc_spec_severity    :: error_severity,
                simpnc_spec_phase       :: error_phase,
                simpnc_spec_pieces      :: list(format_piece)
            )
    ;       error_spec(
                error_id                :: string,
                error_severity          :: error_severity,
                error_phase             :: error_phase,
                error_msgs              :: list(error_msg)
            )
    ;       conditional_spec(
                cond_id                 :: string,
                cond_spec_option        :: option,
                cond_spec_value         :: bool,

                cond_spec_severity      :: error_severity,
                cond_spec_phase         :: error_phase,
                cond_spec_msgs          :: list(error_msg)
            ).

    % An error_spec that is *intended* to contain a warning,
    % XXX We can now enforce that intention using subtypes.
    %
:- type warning_spec == error_spec.

    % Many operations in the compiler may either succeed or fail.
    % When they succeed, they return some result(s); when they don't,
    % they return one or more errors.
    %
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

%---------------------------------------------------------------------------%

:- type error_phase
    --->    phase_options
    ;       phase_check_libs
    ;       phase_make_target
    ;       phase_make_int
    ;       phase_find_files(string)
            % The name of the file we tried to find, but failed.
            % The intention is to make it possible to use this info
            % to replace several messages that all report failures
            % to find files with a single, shorter message to that effect.
    ;       phase_read_files
    ;       phase_module_name
    ;       phase_t2pt              % short for "term to parse tree"
    % The "tim" in the next few phase names is short for "type inst mode".
    % Some errors in check_type_inst_mode_defns.m report an invalid type, ...
    ;       phase_tim_check_invalid_type
    % some report an invalid inst or mode, ...
    ;       phase_tim_check_invalid_inst_mode
    % and some do neither.
    ;       phase_tim_check
    ;       phase_type_repn
    ;       phase_pt2h              % short for "parse tree to HLDS"
    ;       phase_expand_types
    ;       phase_type_check
    ;       phase_inst_check
    ;       phase_polymorphism
    ;       phase_mode_check(mode_report_control)
    ;       phase_purity_check
    ;       phase_detism_check
    ;       phase_fact_table_check
    ;       phase_oisu_check
    ;       phase_simplify(mode_report_control)
    ;       phase_direct_arg_in_out
    ;       phase_style
    ;       phase_dead_code
    ;       phase_termination_analysis
    ;       phase_accumulator_intro
    ;       phase_auto_parallelism
    ;       phase_interface_gen
    ;       phase_code_gen.

:- type mode_report_control
    --->    report_in_any_mode
    ;       report_only_if_in_all_modes.

%---------------------------------------------------------------------------%

% An error message may have several components that may be printed under
% different circumstances. Some components are always printed; some are
% printed only if specific options have specific values. When an error
% specification is printed, we concatenate the list of all the
% format_pieces that should be printed. If this yields the empty list,
% we print nothing. Otherwise, we give them all to write_error_pieces to print
% out.
%
% When we print an error message in a list of error messages, we normally
% treat the first line of the first message differently than the rest:
% we separate it from the context by one space, whereas following lines
% are separated by three spaces. You can request that the first line of
% a message be treated as it were the first, even if it is not, by setting
% the error_treat_as_first field to "always_treat_as_first". You can also
% request that the pieces in a message be given extra indentation by setting
% the error_extra_indent field to a strictly positive value.
%
% The term simple_msg(Context, Components) is a shorthand for (and equivalent
% in every respect to) the term error_msg(yes(Context), treat_based_on_posn,
% 0, Components).
%
% The term msg(Context, Pieces) is a shorthand for (and equivalent
% in every respect to) the term simple_msg(Context, [always(Pieces)]).

:- type maybe_always_treat_as_first
    --->    always_treat_as_first
    ;       treat_based_on_posn.

:- type error_msg
    --->    msg(
                simplest_context        :: prog_context,
                simplest_pieces         :: list(format_piece)
            )
    ;       no_ctxt_msg(
                simplestnc_pieces       :: list(format_piece)
            )
    ;       simple_msg(
                simple_context          :: prog_context,
                simple_components       :: list(error_msg_component)
            )
    ;       error_msg(
                error_context           :: maybe(prog_context),
                error_treat_as_first    :: maybe_always_treat_as_first,
                error_extra_indent      :: int,
                error_components        :: list(error_msg_component)
            ).

:- type verbose_always_or_once
    --->    verbose_always
    ;       verbose_once.
            % Message components marked as verbose_once should be printed
            % just once.

:- type error_msg_component
    --->    always(list(format_piece))
            % Print these components under all circumstances.

    ;       option_is_set(option, bool, list(error_msg_component))
            % Print the embedded components only if the specified boolean
            % option has the specified value.

    ;       verbose_only(verbose_always_or_once, list(format_piece))
            % Print these components only if --verbose-errors is specified.
            % If it is not specified, set the flag that triggers the printing
            % of the message reminding the user about --verbose-errors.
            % In addition, if the first field is verbose_once, then disable
            % all but the first printing of the message even if
            % --verbose-errors is specified.

    ;       verbose_and_nonverbose(list(format_piece), list(format_piece))
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
    pred print_anything(io.text_output_stream::in, T::in,
        io::di, io::uo) is det
].

%---------------------------------------------------------------------------%

:- type format_piece
    --->    invis_order_default_start(int, string)
            % Prints nothing. If the compiler generates two different specs
            % for the same context that we intend to appear in a specific
            % order, even though it may not be the order that sorting those
            % specs would normally give, we can add one of these to the
            % start of each error_spec, with the order of the numbers
            % and/or strings inside these invis orders controlling
            % the final order of the error_specs.
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
    ;       int_name(int)
            % Convert the integer to a string, then treat as fixed.
            % int_fixed always generates numerals, such as 1, 2, 3 etc,
            % while int_name generates one, two, three etc up to ten,
            % then switches back to numerals starting with 11.

    ;       nth_fixed(int)
            % Convert the integer to a string, such as "first", "second",
            % "third", tenth, 11th and so on, and then treat as fixed.

    ;       lower_case_next_if_not_first
            % If this is the first component, ignore it. If this is not
            % the first component, lower case the initial letter of the
            % next component. There is no effect if the next component
            % does not exist or does not start with an upper case letter.

    ;       treat_next_as_first
            % For the purpose of the test done by lower_case_next_if_not_first,
            % treat the next component as the first, even if it isn't.

    ;       prefix(string)
            % This string should appear in the output in one piece, as it is,
            % inserted directly before the next format_piece, without
            % any intervening space.

    ;       suffix(string)
            % This string should appear in the output in one piece, as it is,
            % appended directly after the previous format_piece, without
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

    ;       name_arity(name_arity)
            % The output should contain the name, surrounded by `' quotes,
            % followed by '/' and the arity.

    ;       qual_sym_name_arity(sym_name_arity)
    ;       unqual_sym_name_arity(sym_name_arity)
            % The output should contain the string form of the sym_name,
            % surrounded by `' quotes, followed by '/' and the arity.

    ;       qual_pf_sym_name_pred_form_arity(pf_sym_name_arity)
    ;       unqual_pf_sym_name_pred_form_arity(pf_sym_name_arity)
    ;       qual_pf_sym_name_user_arity(pred_pf_name_arity)
    ;       unqual_pf_sym_name_user_arity(pred_pf_name_arity)
            % The output should contain the string form of the sym_name,
            % surrounded by `' quotes, followed by '/' and the arity, but
            % - precede them with either "predicate" or "function", and
            % - for functions, use their *user-visible* arity, which does not
            %   count the function result.
            %
            % With the forms taking a pf_sym_name_arity argument, the
            % pf_sym_name_arity contains a pred_form_arity that we convert
            % to the user visible arity for printing. With the forms taking
            % a pred_pf_name_arity argument, the pf_sym_name_arity contains
            % a user_arity that we print unchanged.

    ;       qual_type_ctor(type_ctor)
    ;       unqual_type_ctor(type_ctor)
            % The output should contain the string form of the type_ctor,
            % surrounded by `' quotes, followed by '/' and the arity.

    ;       qual_inst_ctor(inst_ctor)
    ;       unqual_inst_ctor(inst_ctor)
            % The output should contain the string form of the inst_ctor,
            % surrounded by `' quotes, followed by '/' and the arity.

    ;       qual_mode_ctor(mode_ctor)
    ;       unqual_mode_ctor(mode_ctor)
            % The output should contain the string form of the mode_ctor,
            % surrounded by `' quotes, followed by '/' and the arity.

    ;       qual_class_id(class_id)
    ;       unqual_class_id(class_id)
            % The output should contain the string form of the class_id,
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

    ;       purity_desc(purity)
            % Output the string "pure", "semipure" or "impure" as appropriate.

    ;       a_purity_desc(purity)
            % Output the string "a pure", "a semipure" or "an impure"
            % as appropriate.

    ;       decl(string)
            % Prefix the string with ":- ", surround it with single quotes,
            % and then treat as fixed.

    ;       pragma_decl(string)
            % As above, but prefix the string with ":- pragma ".

    ;       left_paren_maybe_nl_inc(string, lp_piece_kind)
    ;       maybe_nl_dec_right_paren(string, rp_piece_kind)
            % These two pieces are intended to help implement messages
            % that should be formatted to look like either
            %
            %   aaa(bbb, ccc)
            %
            % if there is space on the current line, or to look like
            %
            %   aaa(
            %       bbb,
            %       ccc
            %   )
            %
            % if there isn't.
            %
            % The piece sequence that would yield the above would be
            %
            %   fixed("aaa")
            %   left_paren_maybe_nl_inc("(", lp_suffix)
            %   fixed("bbb"),
            %   suffix(","),
            %   nl,
            %   fixed("ccc"),
            %   maybe_nl_dec_right_paren(")", rp_plain)
            %
            % The left_paren_maybe_nl_inc adds the given string to the
            % text, followed by an nl_indent(1). The maybe_nl_dec_right_paren
            % adds an nl_indent(-1) to the text, followed by the given string.
            % The left parenthesis may be added to the previous piece
            % as a suffix, or not; the right parenthesis may be added
            % to the next piece as a prefix, or not. The strings are usually
            % "(" and ")", but they could also be "{" and "}", or "[" and "]",
            % or anything else.
            %
            % The "maybe" is there in the names of these pieces because
            % these pieces expressly tell the code of write_error_pieces.m
            % to delete both these indent-incrementing/decrementing newlines,
            % and all other newlines between the left_paren_maybe_nl_inc
            % and its matching maybe_nl_dec_right_paren, provided the text
            % between them fits in the space available on the line.
            % Note that the size of the space depends on both the length
            % of the context printed at the start of the line, and on the
            % indent printed after the context, which may or may not be
            % available to the code constructing these pieces.
            %
            % These pieces should always be used in left/right pairs,
            % and should always be properly nested.

    ;       nl
            % Insert a line break if there has been text output since
            % the last line break.

    ;       nl_indent_delta(int)
            % Act as nl, but also add the given integer (which should be a
            % small positive or negative integer) to the current indent level.

    ;       blank_line
            % Create a blank line.

    ;       not_for_general_use_start_colour(colour_name)
    ;       not_for_general_use_end_colour

    ;       invis_order_default_end(int, string).
            % See the documentation of invis_order_default_start above.

:- type lp_piece_kind
    --->    lp_plain
            % The left parenthesis should be added to the previous pieces
            % as if it were in a fixed(...) piece.
    ;       lp_suffix.
            % The left parenthesis should be added to the previous pieces
            % as if it were in a suffix(...) piece.

:- type rp_piece_kind
    --->    rp_plain
            % The right parenthesis should be added to the following pieces
            % as if it were in a fixed(...) piece.
    ;       rp_prefix.
            % The right parenthesis should be added to the following pieces
            % as if it were in a prefix(...) piece.

    % Exported for use by write_error_spec.m. This definition should be
    % ignored by every part of the compiler other than error_spec.m
    % and write_error_spec.m.
:- type colour_name
    --->    colour_incorrect
    ;       colour_correct.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Wrap words() around a string.
    %
:- func string_to_words_piece(string) = format_piece.

    % Convert a list of strings into a list of format_pieces
    % separated by commas, with the last two elements separated by `and'.
    %
:- func list_to_pieces(list(string)) = list(format_piece).

    % Convert a list of strings into a list of format_pieces
    % separated by commas. Even the last pair of strings will be
    % separated by commas.
    %
:- func strict_list_to_pieces(list(string)) = list(format_piece).

    % As list_to_pieces, but surround each string by `' quotes.
    %
:- func list_to_quoted_pieces(list(string)) = list(format_piece).

    % As above, but with the last two elements separated by `or'.
    %
:- func list_to_quoted_pieces_or(list(string)) = list(format_piece).

    % Convert a list of lists of format_pieces into a list of
    % format_pieces separated by commas, with the last two elements
    % separated by the first argument as a word.
    %
:- func component_lists_to_pieces(string, list(list(format_piece))) =
    list(format_piece).

    % Convert a list of lists of format_pieces into a list of
    % format_pieces separated by commas. Even the last pair of lists
    % will be separated by commas.
    %
:- func strict_component_lists_to_pieces(list(list(format_piece))) =
    list(format_piece).

    % Convert a list of format_pieces into a list of format_pieces
    % separated by commas, with the last two elements separated
    % by the first argument as a word.
    %
:- func component_list_to_pieces(string, list(format_piece)) =
    list(format_piece).

    % Convert a list of format_pieces into a list of format_pieces
    % separated by commas. Even the last pair of list elements will be
    % separated by commas.
    %
:- func strict_component_list_to_pieces(list(format_piece)) =
    list(format_piece).

    % component_list_to_line_pieces(Lines, Final):
    %
    % Convert Lines, a list of lines (each given by a list of format_pieces
    % *without* a final nl) into a condensed list of format_pieces
    % in which adjacent lines are separated by commas and newlines.
    % What goes after the end of the last line is not a comma, but
    % the value of Final.
    %
:- func component_list_to_line_pieces(list(list(format_piece)),
    list(format_piece)) = list(format_piece).

    % indented_list(Lines):
    %
    % Format Lines, a list of lines each given by a single format_piece,
    % by putting newlines between them, and by adding nl_indent_deltas
    % before and after the list to first increase and then decrease
    % the indent level.
    %
:- func indented_list(list(format_piece)) = list(format_piece).

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

:- func colour_pieces_as_correct(list(format_piece)) = list(format_piece).
:- func colour_pieces_as_incorrect(list(format_piece)) = list(format_piece).

%---------------------------------------------------------------------------%

    % Convert the output of find_change_hunks from library/edit_seq.m
    % to a diff we can include in error messages.
    %
:- pred change_hunk_to_pieces(change_hunk(string)::in,
    list(format_piece)::out) is det.

%---------------------------------------------------------------------------%

:- func describe_sym_name(sym_name) = string.

:- func describe_sym_name_arity(sym_name_arity) = string.

    % Put `' quotes around the given string.
    %
:- func add_quotes(string) = string.

%---------------------------------------------------------------------------%

:- pred maybe_construct_did_you_mean_pieces(string::in, list(string)::in,
    list(format_piece)::out) is det.

:- pred maybe_construct_prefixed_did_you_mean_pieces(string::in, string::in,
    list(string)::in, list(format_piece)::out) is det.

%---------------------------------------------------------------------------%

:- pred extract_spec_phase(error_spec::in, error_phase::out) is det.

:- pred extract_spec_msgs(globals::in, error_spec::in,
    list(error_msg)::out) is det.
:- pred extract_spec_msgs_opt_table(option_table::in, error_spec::in,
    list(error_msg)::out) is det.

:- pred accumulate_contexts(error_spec::in,
    set(prog_context)::in, set(prog_context)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module edit_distance.
:- import_module getopt.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term_context.
:- import_module uint.

%---------------------------------------------------------------------------%

string_to_words_piece(Str) = words(Str).

list_to_pieces([]) = [].
list_to_pieces([Elem]) = [fixed(Elem)].
list_to_pieces([Elem1, Elem2]) = [fixed(Elem1), words("and"), fixed(Elem2)].
list_to_pieces([Elem1, Elem2, Elem3 | Elems]) =
    [fixed(Elem1 ++ ",") | list_to_pieces([Elem2, Elem3 | Elems])].

strict_list_to_pieces([]) = [].
strict_list_to_pieces([Elem]) = [fixed(Elem)].
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
component_list_to_line_pieces([Comps], Final) = Comps ++ Final.
component_list_to_line_pieces([Comps1, Comps2 | CompLists], Final) =
    Comps1 ++ [suffix(","), nl]
    ++ component_list_to_line_pieces([Comps2 | CompLists], Final).

indented_list(Comps) =
    [nl_indent_delta(1)] ++ indented_list_loop(Comps) ++ [nl_indent_delta(-1)].

:- func indented_list_loop(list(format_piece)) = list(format_piece).

indented_list_loop([]) = [].
indented_list_loop([Comp]) = [Comp].
indented_list_loop([Comp1, Comp2 | CompLists]) =
    [Comp1, nl] ++ indented_list_loop([Comp2 | CompLists]).

choose_number([], _Singular, Plural) = Plural.
choose_number([_], Singular, _Plural) = Singular.
choose_number([_, _ | _], _Singular, Plural) = Plural.

is_or_are([]) = "" :-
    unexpected($pred, "[]").
is_or_are([_]) = "is".
is_or_are([_, _ | _]) = "are".

%---------------------------------------------------------------------------%

colour_pieces_as_correct(Pieces) =
    [not_for_general_use_start_colour(colour_correct)] ++
    Pieces ++
    [not_for_general_use_end_colour].

colour_pieces_as_incorrect(Pieces) =
    [not_for_general_use_start_colour(colour_incorrect)] ++
    Pieces ++
    [not_for_general_use_end_colour].

%---------------------------------------------------------------------------%

change_hunk_to_pieces(ChangeHunk, ChangeHunkPieces) :-
    ChangeHunk = change_hunk(StartA, LenA, StartB, LenB, Diffs),
    string.format("@@ -%d,%d +%d,%d @@",
        [i(StartA), i(LenA), i(StartB), i(LenB)], HeaderStr),
    HeaderPieces = [fixed(HeaderStr), nl],
    list.map(diff_seq_line_to_pieces, Diffs, DiffPieceLists),
    list.condense([HeaderPieces | DiffPieceLists], ChangeHunkPieces).

:- pred diff_seq_line_to_pieces(diff(string)::in, list(format_piece)::out)
    is det.

diff_seq_line_to_pieces(Diff, Pieces) :-
    (
        Diff = unchanged(Str),
        LinePieces = [fixed(" " ++ Str)]
    ;
        Diff = deleted(Str),
        LinePieces = colour_pieces_as_correct([fixed("-" ++ Str)])
    ;
        Diff = inserted(Str),
        LinePieces = colour_pieces_as_incorrect([fixed("+" ++ Str)])
    ),
    Pieces = LinePieces ++ [nl].

%---------------------------------------------------------------------------%

describe_sym_name(SymName) =
    string.format("`%s'", [s(sym_name_to_string(SymName))]).

describe_sym_name_arity(sym_name_arity(SymName, Arity)) =
    string.format("`%s'/%d", [s(sym_name_to_string(SymName)), i(Arity)]).

add_quotes(Str) = "`" ++ Str ++ "'".

%---------------------------------------------------------------------------%

maybe_construct_did_you_mean_pieces(BaseName, CandidateNames,
        DidYouMeanPieces) :-
    do_maybe_construct_did_you_mean_pieces(BaseName, CandidateNames,
        std_util.id, DidYouMeanPieces).

maybe_construct_prefixed_did_you_mean_pieces(Prefix, BaseName, CandidateNames,
        DidYouMeanPieces) :-
    do_maybe_construct_did_you_mean_pieces(BaseName, CandidateNames,
        add_prefix(Prefix), DidYouMeanPieces).

:- pred do_maybe_construct_did_you_mean_pieces(string::in, list(string)::in,
    (func(string) = string)::in, list(format_piece)::out) is det.

do_maybe_construct_did_you_mean_pieces(BaseName, CandidateNames,
        TransformFunc, DidYouMeanPieces) :-
    % Note: name_is_close_enough below depends on all costs here
    % except for case changes being 2u.
    Params = edit_params(2u, 2u, case_sensitive_replacement_cost, 2u),
    string.count_code_points(BaseName, BaseNameLen),
    BaseNameLenU = uint.cast_from_int(BaseNameLen),
    % The algorithm we use here to set MaxCost has two purposes.
    %
    % One is to speed up the process of finding close enough candidates,
    % by allowing candidates with too-large edit distances to be rejected
    % without having to finish the computation of those edit distances.
    %
    % The other is to require edits to replace at most half of any base name
    % that exceeds one character. Note that the heuristic for "close enough"
    % that name_is_close_enough uses, which is originally from gcc, does *not*
    % impose that requirement.
    ( if BaseNameLenU < 2u then
        % If BaseName consists of a single character, allow a suggestion
        % to replace that character.
        MaxCost = 2u
    else
        % If BaseName consists of two or more characters, allow suggestions
        % to replace at most half of those characters (rounded up).
        % 2u is the replacement cost.
        MaxCost = ((BaseNameLenU + 1u) / 2u) * 2u
    ),
    ( if
        CandidateNames = [_ | _],
        find_best_close_enough_strings(Params, BaseName, CandidateNames,
            MaxCost, BestCost, HeadBestName, TailBestNames),
        BestNames = [HeadBestName | TailBestNames],
        % Don't offer a string as a replacement for itself.
        BestCost > 0u,
        % Don't offer a string as a replacement if it is too far
        % from the original, either.
        list.filter(name_is_close_enough(BestCost, BaseName, BaseNameLenU),
            BestNames, CloseEnoughBestNames),
        CloseEnoughBestNames = [_ | _]
    then
        % For hand-written code, having more than ten names
        % equally close to BaseName should be vanishingly rare,
        % so the limit we impose here should not matter.
        % But programs that automatically generate Mercury code
        % may use naming schemes that make such occurrences
        % much more likely, and for these, avoiding the generation
        % of far-too-long error messages may be important.
        list.split_upto(10, CloseEnoughBestNames,
            SuggestedNames0, NonSuggestedNames),
        (
            NonSuggestedNames = [],
            SuggestedNames = list.map(TransformFunc, SuggestedNames0)
        ;
            NonSuggestedNames = [_ | _],
            % This should cause the message we create below
            % to end with "or `...'?".
            SuggestedNames =
                list.map(TransformFunc, SuggestedNames0) ++ ["..."]
        ),
        SuggestionPieces = list_to_quoted_pieces_or(SuggestedNames),
        DidYouMeanPieces =
            [words("(Did you mean")] ++ SuggestionPieces ++
            [suffix("?)"), nl]
    else
        DidYouMeanPieces = []
    ).

:- pred name_is_close_enough(uint::in, string::in, uint::in, string::in)
    is semidet.

name_is_close_enough(Cost, Query, QueryLenU, Name) :-
    % This heuristic for when a name is "close enough"
    % is from spellcheck.cc in the gcc source code.
    require_det (
        string.count_code_points(Name, NameLen),
        NameLenU = uint.cast_from_int(NameLen),
        MinLenU = uint.min(QueryLenU, NameLenU),
        MaxLenU = uint.max(QueryLenU, NameLenU),
        % Accept a candidate as "close enough" if the number of changes
        % is at most one third of the length of the longer of the two strings.
        %
        % If the lengths are close, then ...
        ( if MaxLenU - MinLenU =< 1u then
            % ... round down, but allow at least one change.
            MaxAcceptableLen = uint.max(MaxLenU / 3u, 1u)
        else
            % Otherwise, round up, thus giving a little extra leeway
            % to some cases involving insertions/deletions.
            MaxAcceptableLen = (MaxLenU + 2u) / 3u
        ),
        % Note that in situations where the Name is much longer than BaseName,
        % and therefore MaxLenU is much greater than MinLenU, the above formula
        % generates values of MaxAcceptableLen that exceed BaseLenU, which thus
        % allows suggestions that replace *every* item in Query. This may be
        % a usability bug in the gcc heuristic.

        % The 2u represents the cost of edits other than case transformations.
        MaxAcceptableCost = 2u * MaxAcceptableLen,
        trace [compile_time(flag("debug_close_enough")), io(!IO)] (
            io.output_stream(Stream, !IO),
            io.format(Stream, "%s vs %s: cost %u, max acceptable cost %u\n",
                [s(Query), s(Name), u(Cost), u(MaxAcceptableCost)], !IO),
            io.format(Stream, "%srecommended\n",
                [s(if Cost =< MaxAcceptableCost then "" else "not ")], !IO)
        )
    ),
    Cost =< MaxAcceptableCost.

:- func case_sensitive_replacement_cost(char, char) = uint.

case_sensitive_replacement_cost(CharA, CharB) = ReplacementCost :-
    char.to_lower(CharA, LowerCharA),
    char.to_lower(CharB, LowerCharB),
    ( if LowerCharA = LowerCharB then
        % CharA and CharB differ only in case.
        ReplacementCost = 1u
    else
        ReplacementCost = 2u
    ).

%---------------------------------------------------------------------------%

extract_spec_phase(Spec, Phase) :-
    (
        Spec = error_spec(_, _, Phase, _)
    ;
        Spec = spec(_, _, Phase, _, _)
    ;
        Spec = no_ctxt_spec(_, _, Phase, _)
    ;
        Spec = conditional_spec(_, _, _, _, Phase, _)
    ).

extract_spec_msgs(Globals, Spec, Msgs) :-
    (
        Spec = error_spec(_Id, _Severity, _Phase, Msgs)
    ;
        Spec = spec(_Id, _Severity, _Phase, Context, Pieces),
        Msgs = [msg(Context, Pieces)]
    ;
        Spec = no_ctxt_spec(_Id, _Severity, _Phase, Pieces),
        Msgs = [no_ctxt_msg(Pieces)]
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

extract_spec_msgs_opt_table(OptionTable, Spec, Msgs) :-
    (
        Spec = error_spec(_Id, _Severity, _Phase, Msgs)
    ;
        Spec = spec(_Id, _Severity, _Phase, Context, Pieces),
        Msgs = [msg(Context, Pieces)]
    ;
        Spec = no_ctxt_spec(_Id, _Severity, _Phase, Pieces),
        Msgs = [no_ctxt_msg(Pieces)]
    ;
        Spec = conditional_spec(_Id, Option, MatchValue, _Severity, _Phase,
            Msgs0),
        getopt.lookup_bool_option(OptionTable, Option, Value),
        ( if Value = MatchValue then
            Msgs = Msgs0
        else
            Msgs = []
        )
    ).

accumulate_contexts(Spec, !Contexts) :-
    (
        ( Spec = error_spec(_, _, _, Msgs)
        ; Spec = conditional_spec(_, _, _, _, _, Msgs)
        ),
        list.foldl(accumulate_contexts_in_msg, Msgs, !Contexts)
    ;
        Spec = spec(_, _, _, Context, _),
        set.insert(Context, !Contexts)
    ;
        Spec = no_ctxt_spec(_, _, _, _)
    ).

:- pred accumulate_contexts_in_msg(error_msg::in,
    set(prog_context)::in, set(prog_context)::out) is det.

accumulate_contexts_in_msg(Msg, !Contexts) :-
    (
        Msg = no_ctxt_msg(_)
    ;
        ( Msg = msg(Context, _)
        ; Msg = simple_msg(Context, _)
        ),
        set.insert(Context, !Contexts)
    ;
        Msg = error_msg(MaybeContext, _, _, _),
        (
            MaybeContext = no
        ;
            MaybeContext = yes(Context),
            set.insert(Context, !Contexts)
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.error_spec.
%---------------------------------------------------------------------------%
