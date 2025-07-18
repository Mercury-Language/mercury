%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2022-2025 The Mercury team.
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
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module edit_seq.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module term.
:- import_module varset.

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
                s_id                    :: string,
                s_spec_severity         :: error_severity,
                s_spec_phase            :: error_phase,
                s_spec_context          :: prog_context,
                s_spec_pieces           :: list(format_piece)
            )
    ;       no_ctxt_spec(
                ncs_id                  :: string,
                ncs_spec_severity       :: error_severity,
                ncs_spec_phase          :: error_phase,
                ncs_spec_pieces         :: list(format_piece)
            )
    ;       error_spec(
                es_id                   :: string,
                es_severity             :: error_severity,
                es_phase                :: error_phase,
                es_msgs                 :: list(error_msg)
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
% we print nothing. Otherwise, we print them all out.
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
                error_extra_indent      :: uint,
                error_components        :: list(error_msg_component)
            )
    ;       blank_msg(
                blank_context           :: maybe(prog_context)
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

    ;       verbose_and_nonverbose(list(format_piece), list(format_piece)).
            % If --verbose-errors is specified, print the first set of
            % components. If it is not specified, print the second set,
            % and set the flag that triggers the printing of the message
            % reminding the user about --verbose-errors. The verbose part
            % is implicitly verbose_always.

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
    ;       uint_fixed(uint)
    ;       uint_name(uint)
            % Convert the integer to a string, then treat as fixed.
            % int_fixed always generates numerals, such as 1, 2, 3 etc,
            % while int_name generates one, two, three etc up to ten,
            % then switches back to numerals starting with 11.

    ;       nth_fixed(int)
    ;       unth_fixed(uint)
            % Convert the integer to a string, such as "first", "second",
            % "third", tenth, 11th and so on, and then treat as fixed.

    ;       lower_case_next_if_not_first
            % If this is the first component, ignore it. If this is not
            % the first component, lower case the initial letter of the
            % next component. There is no effect if the next component
            % does not exist or does not start with an upper case letter.

    ;       upper_case_next

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

    ;       purity_desc_article(purity)
            % Output the article that forms the difference between
            % the output of a_purity_desc(Purity) and purity_desc(Purity).

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

    ;       not_for_general_use_start_color(color_name)
    ;       not_for_general_use_end_color(color_name)
            % Start or end the scope of a color.

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

:- type color_name
    --->    color_subject
    ;       color_correct
    ;       color_incorrect
    ;       color_inconsistent
    ;       color_hint.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Wrap words() around a string.
    %
:- func string_to_words_piece(string) = format_piece.

:- func var_to_quote_piece(varset(T), var(T)) = format_piece.
:- func var_in_table_to_quote_piece(var_table, prog_var) = format_piece.

%---------------------------------------------------------------------------%

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

    % Return the name of the given integer as zero, one, two, three etc,
    % up to ten. For integers other than 0..10, return the value as digits.
    %
:- func int_name_str(int) = string.
:- func uint_name_str(uint) = string.

    % The ordinal version of int_name_str, returning first, second etc
    % for 1..10.
    %
:- func nth_fixed_str(int) = string.
:- func unth_fixed_str(uint) = string.

%---------------------------------------------------------------------------%

:- func describe_sym_name(sym_name) = string.

:- func describe_sym_name_arity(sym_name_arity) = string.

    % Put `' quotes around the given string.
    %
:- func add_quotes(string) = string.

:- func add_suffix_if_nonempty(list(format_piece), list(format_piece)) =
    list(format_piece).

:- pred append_prefix_and_maybe_verbose(maybe(color_name)::in,
    list(format_piece)::in, list(format_piece)::in,
    list(format_piece)::in, list(format_piece)::in,
    error_msg_component::out) is det.

%---------------------------------------------------------------------------%

:- pred extract_msg_maybe_context(error_msg::in, maybe(prog_context)::out)
    is det.

:- pred extract_spec_phase(error_spec::in, error_phase::out) is det.

:- pred accumulate_contexts(error_spec::in,
    set(prog_context)::in, set(prog_context)::out) is det.

%---------------------------------------------------------------------------%

:- pred extract_spec_msgs_and_id(globals::in, error_spec::in,
    list(error_msg)::out, string::out) is det.
:- pred extract_spec_msgs_and_id_opt_table(option_table::in, error_spec::in,
    list(error_msg)::out, string::out) is det.

:- pred extract_spec_msgs_and_maybe_add_id(globals::in, error_spec::in,
    list(error_msg)::out) is det.

:- pred maybe_add_error_spec_id(option_table::in, string::in,
    list(error_msg)::in, list(error_msg)::out) is det.

%---------------------------------------------------------------------------%

:- pred construct_diff_for_string_seqs(edit_params::in,
    list(string)::in, list(string)::in, list(format_piece)::out) is det.

%---------------------------------------------------------------------------%

:- pred maybe_construct_did_you_mean_pieces(string::in, list(string)::in,
    list(format_piece)::out) is det.

:- pred maybe_construct_prefixed_did_you_mean_pieces(string::in, string::in,
    list(string)::in, list(format_piece)::out) is det.

%---------------------------------------------------------------------------%

:- func color_as_subject(list(format_piece)) = list(format_piece).
:- func color_as_correct(list(format_piece)) = list(format_piece).
:- func color_as_incorrect(list(format_piece)) = list(format_piece).
:- func color_as_inconsistent(list(format_piece)) = list(format_piece).
:- func color_as_hint(list(format_piece)) = list(format_piece).

:- func color_pieces(color_name, list(format_piece)) = list(format_piece).
:- func maybe_color_pieces(maybe(color_name), list(format_piece))
    = list(format_piece).

%---------------------------------------------------------------------------%
%
% The functions in this section have the task of formatting lists of
% various kinds of items for use in diagnostic messages.
% These functions form an (incomplete) 4 by 2 by 2 by 2 matrix.
%
% The first dimension specifies what the individual items are, and how
% they should be treated.
%
% - If the function name starts with "fixed", this means that each item
%   is a string, and it should be formatted as a fixed string.
%
% - If the function name starts with "quote", this means that each item
%   is a string, and it should be formatted as a quoted string.
%
% - If the function name starts with "piece", this means that each item
%   is a piece, and it should be used as is.
%
% - If the function name starts with "pieces", this means that each item
%   is a list of pieces, which should be used as is.
%
% This dimension controls the type of the last argument of the function.
%
% The second dimension is whether the items should be formatted in color.
%
% - If the function name includes "color", then the answer is yes.
% - Otherwise, the answer is no.
%
% This dimension controls the presence of two arguments. Functions that
% return colored pieces takes as one argument the name of that color,
% and they also take a LastSuffix parameter, which is a list of pieces
% that the function adds to the last item. The caller can use this argument
% e.g. to put a period as a suffix at the end of a sentence, if the given list
% is at the end of that sentence; or to put a comma or semicolon at the
% end of clause within a sentence, if the list is at the end of that clause.
% Of course, LastSuffix may also be the empty list of pieces. In any case,
% LastSuffix will be colored the same as the items. The functions that
% do not do coloring do not take a LastSuffix argument, because their caller
% can add any suffixes they like to the return value of the function.
%
% The third dimension controls whether the last gap between two items
% should be treated any differently than all the previous gaps between items.
%
% - If the function name includes either "strict" or "to_line_pieces", then
%   we treat all gaps between items the same, by adding a comma as a suffix
%   to the item before the gap.
%
% - Otherwise, we treat the last gap differently, by putting a separator word,
%   usually either "and" or "or", in that gap.
%
% This dimension controls whether the function has a string argument
% specifying the separator to put into the last gap.
%
% The fourth dimension controls whether each item in the list should be
% printed separately on its own line.
%
% - If the function name ends with "to_line_pieces", then the answer is yes.
% - If the function name ends with just "to_pieces", then the answer is no.
%
% This dimension has no effect on the argument list, except through its
% coupling to the third dimension. The 4 by 2 by 2 by 2 matrix is incomplete
% because we do not support non-strict list format when putting each item
% on its own line. (It would not be hard to offer such support; there is
% just no particular need for that capability.)
%

    % x_list_to_pieces(LastSepWord, Items) = Pieces.
    %
:- func fixed_list_to_pieces(string,
    list(string)) = list(format_piece).
:- func quote_list_to_pieces(string,
    list(string)) = list(format_piece).
:- func piece_list_to_pieces(string,
    list(format_piece)) = list(format_piece).
:- func pieces_list_to_pieces(string,
    list(list(format_piece))) = list(format_piece).

    % x_list_to_color_pieces(Color, LastSepWord, LastSuffix, Items) = Pieces.
    %
:- func fixed_list_to_color_pieces(color_name, string,
    list(format_piece), list(string)) = list(format_piece).
:- func quote_list_to_color_pieces(color_name, string,
    list(format_piece), list(string)) = list(format_piece).
:- func piece_list_to_color_pieces(color_name, string,
    list(format_piece), list(format_piece)) = list(format_piece).
:- func pieces_list_to_color_pieces(color_name, string,
    list(format_piece), list(list(format_piece))) = list(format_piece).

    % x_strict_list_to_pieces(Items) = Pieces.
    %
:- func fixed_strict_list_to_pieces(
    list(string)) = list(format_piece).
:- func quote_strict_list_to_pieces(
    list(string)) = list(format_piece).
:- func piece_strict_list_to_pieces(
    list(format_piece)) = list(format_piece).
:- func pieces_strict_list_to_pieces(
    list(list(format_piece))) = list(format_piece).

    % x_strict_list_to_color_pieces(Color, LastSepWord, LastSuffix, Items)
    %   = Pieces.
    %
:- func fixed_strict_list_to_color_pieces(color_name,
    list(format_piece), list(string)) = list(format_piece).
:- func quote_strict_list_to_color_pieces(color_name,
    list(format_piece), list(string)) = list(format_piece).
:- func piece_strict_list_to_color_pieces(color_name,
    list(format_piece), list(format_piece)) = list(format_piece).
:- func pieces_strict_list_to_color_pieces(color_name,
    list(format_piece), list(list(format_piece))) = list(format_piece).

    % x_list_to_line_pieces(Items) = Pieces.
    %
:- func fixed_list_to_line_pieces(
    list(string)) = list(format_piece).
:- func quote_list_to_line_pieces(
    list(string)) = list(format_piece).
:- func piece_list_to_line_pieces(
    list(format_piece)) = list(format_piece).
:- func pieces_list_to_line_pieces(
    list(list(format_piece))) = list(format_piece).

    % x_list_to_color_line_pieces(Color, LastSuffix, Items) = Pieces.
    %
:- func fixed_list_to_color_line_pieces(color_name,
    list(format_piece), list(string)) = list(format_piece).
:- func quote_list_to_color_line_pieces(color_name,
    list(format_piece), list(string)) = list(format_piece).
:- func piece_list_to_color_line_pieces(color_name,
    list(format_piece), list(format_piece)) = list(format_piece).
:- func pieces_list_to_color_line_pieces(color_name,
    list(format_piece), list(list(format_piece))) = list(format_piece).

%---------------------------------------------------------------------------%

    % This is a specialized version of pieces_list_to_color_line_pieces
    % that allows the caller to specify
    %
    % - two lists of pieces to put on each line,
    % - a color for each of those two lists of pieces.
    %
:- func pieces_list_to_split_color_line_pieces(
    maybe(color_name), maybe(color_name), list(format_piece),
    list({list(format_piece), list(format_piece)})) = list(format_piece).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_out_term.

:- import_module char.
:- import_module edit_distance.
:- import_module getopt.
:- import_module int.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term_context.
:- import_module uint.

%---------------------------------------------------------------------------%

string_to_words_piece(Str) = words(Str).

var_to_quote_piece(VarSet, Var) =
    quote(mercury_var_to_name_only_vs(VarSet, Var)).

var_in_table_to_quote_piece(VarTable, Var) =
    quote(var_table_entry_name(VarTable, Var)).

%---------------------------------------------------------------------------%

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

uint_name_str(N) = Str :-
    ( if
        ( N = 0u,  StrPrime = "zero"
        ; N = 1u,  StrPrime = "one"
        ; N = 2u,  StrPrime = "two"
        ; N = 3u,  StrPrime = "three"
        ; N = 4u,  StrPrime = "four"
        ; N = 5u,  StrPrime = "five"
        ; N = 6u,  StrPrime = "six"
        ; N = 7u,  StrPrime = "seven"
        ; N = 8u,  StrPrime = "eight"
        ; N = 9u,  StrPrime = "nine"
        ; N = 10u, StrPrime = "ten"
        )
    then
        Str = StrPrime
    else
        Str = uint_to_string(N)
    ).

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

unth_fixed_str(N) = Str :-
    ( if
        ( N = 1u,  StrPrime = "first"
        ; N = 2u,  StrPrime = "second"
        ; N = 3u,  StrPrime = "third"
        ; N = 4u,  StrPrime = "fourth"
        ; N = 5u,  StrPrime = "fifth"
        ; N = 6u,  StrPrime = "sixth"
        ; N = 7u,  StrPrime = "seventh"
        ; N = 8u,  StrPrime = "eighth"
        ; N = 9u,  StrPrime = "ninth"
        ; N = 10u, StrPrime = "tenth"
        )
    then
        Str = StrPrime
    else
        % We want to print 12th and 13th, not 12nd and 13rd,
        % but 42nd and 43rd instead of 42th and 43th.
        NStr = uint_to_string(N),
        LastDigit = N mod 10u,
        ( if N > 20u, LastDigit = 2u then
            Str = NStr ++ "nd"
        else if N > 20u, LastDigit = 3u then
            Str = NStr ++ "rd"
        else
            Str = NStr ++ "th"
        )
    ).

%---------------------------------------------------------------------------%

describe_sym_name(SymName) =
    string.format("`%s'", [s(sym_name_to_string(SymName))]).

describe_sym_name_arity(sym_name_arity(SymName, Arity)) =
    string.format("`%s'/%d", [s(sym_name_to_string(SymName)), i(Arity)]).

add_quotes(Str) = "`" ++ Str ++ "'".

add_suffix_if_nonempty(BasePieces, SuffixPieces) = Pieces :-
    (
        BasePieces = [],
        Pieces = []
    ;
        BasePieces = [_ | _],
        Pieces = BasePieces ++ SuffixPieces
    ).

append_prefix_and_maybe_verbose(MaybeColor,
        NeutralPrefixPieces, ColorPrefixPieces0, MainPieces, VerbosePieces,
        Component) :-
    PrefixPieces = NeutralPrefixPieces ++
        maybe_color_pieces(MaybeColor, ColorPrefixPieces0),
    (
        VerbosePieces = [],
        Component = always(PrefixPieces ++ MainPieces)
    ;
        VerbosePieces = [_ | _],
        Component = verbose_and_nonverbose(
            PrefixPieces ++ VerbosePieces,
            PrefixPieces ++ MainPieces)
    ).

%---------------------------------------------------------------------------%

extract_msg_maybe_context(Msg, MaybeContext) :-
    (
        Msg = no_ctxt_msg(_),
        MaybeContext = no
    ;
        ( Msg = msg(Context, _)
        ; Msg = simple_msg(Context, _)
        ),
        MaybeContext = yes(Context)
    ;
        ( Msg = error_msg(MaybeContext, _, _, _)
        ; Msg = blank_msg(MaybeContext)
        )
    ).

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
    extract_msg_maybe_context(Msg, MaybeContext),
    (
        MaybeContext = no
    ;
        MaybeContext = yes(Context),
        set.insert(Context, !Contexts)
    ).

%---------------------------------------------------------------------------%

extract_spec_msgs_and_id(Globals, Spec, Msgs, Id) :-
    (
        Spec = error_spec(Id, _Severity, _Phase, Msgs)
    ;
        Spec = spec(Id, _Severity, _Phase, Context, Pieces),
        Msgs = [msg(Context, Pieces)]
    ;
        Spec = no_ctxt_spec(Id, _Severity, _Phase, Pieces),
        Msgs = [no_ctxt_msg(Pieces)]
    ;
        Spec = conditional_spec(Id, Option, MatchValue, _Severity, _Phase,
            Msgs0),
        globals.lookup_bool_option(Globals, Option, Value),
        ( if Value = MatchValue then
            Msgs = Msgs0
        else
            Msgs = []
        )
    ).

extract_spec_msgs_and_id_opt_table(OptionTable, Spec, Msgs, Id) :-
    (
        Spec = error_spec(Id, _Severity, _Phase, Msgs)
    ;
        Spec = spec(Id, _Severity, _Phase, Context, Pieces),
        Msgs = [msg(Context, Pieces)]
    ;
        Spec = no_ctxt_spec(Id, _Severity, _Phase, Pieces),
        Msgs = [no_ctxt_msg(Pieces)]
    ;
        Spec = conditional_spec(Id, Option, MatchValue, _Severity, _Phase,
            Msgs0),
        getopt.lookup_bool_option(OptionTable, Option, Value),
        ( if Value = MatchValue then
            Msgs = Msgs0
        else
            Msgs = []
        )
    ).

%---------------------------------------------------------------------------%

extract_spec_msgs_and_maybe_add_id(Globals, Spec, Msgs) :-
    extract_spec_msgs_and_id(Globals, Spec, Msgs0, Id),
    globals.get_options(Globals, OptionTable),
    maybe_add_error_spec_id(OptionTable, Id, Msgs0, Msgs).

maybe_add_error_spec_id(OptionTable, Id, Msgs0, Msgs) :-
    getopt.lookup_bool_option(OptionTable, print_error_spec_id, PrintId),
    (
        PrintId = no,
        Msgs = Msgs0
    ;
        PrintId = yes,
        (
            Msgs0 = [],
            % Don't add a pred id message to an empty list of messages,
            % since there is nothing to identify.
            Msgs = Msgs0
        ;
            Msgs0 = [HeadMsg | _],
            extract_msg_maybe_context(HeadMsg, MaybeHeadContext),
            IdMsg = error_msg(MaybeHeadContext, treat_based_on_posn, 0u,
                [always([words("error_spec id:"), fixed(Id), nl])]),
            Msgs = Msgs0 ++ [IdMsg]
        )
    ).

%---------------------------------------------------------------------------%

construct_diff_for_string_seqs(Params, StrsA, StrsB, Pieces) :-
    find_shortest_edit_seq(Params, StrsA, StrsB, EditSeq),
    find_diff_seq(StrsA, EditSeq, DiffSeq),
    find_change_hunks(3, DiffSeq, ChangeHunks),
    list.map(change_hunk_to_pieces, ChangeHunks, PieceLists),
    list.condense(PieceLists, Pieces).

    % Convert the output of find_change_hunks from library/edit_seq.m
    % to a diff we can include in error messages.
    %
:- pred change_hunk_to_pieces(change_hunk(string)::in,
    list(format_piece)::out) is det.

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
        LinePieces = color_as_correct([fixed("-" ++ Str)])
    ;
        Diff = inserted(Str),
        LinePieces = color_as_incorrect([fixed("+" ++ Str)])
    ),
    Pieces = LinePieces ++ [nl].

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
        BaseNameChars = string.to_char_list(BaseName),
        list.map(char.to_lower, BaseNameChars, BaseNameLowerChars),
        is_any_suggestion_same_but_for_case(BaseNameLowerChars,
            CloseEnoughBestNames, SameButForCase),
        % For hand-written code, having more than ten names equally close
        % to BaseName should be vanishingly rare, so the limit we impose here
        % should not matter. But programs that generate Mercury code
        % automatically may use naming schemes that make such occurrences
        % much more likely, and for these, avoiding the generation of
        % far-too-long error messages may be important.
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
        SuggestedNamePieces = quote_list_to_pieces("or", SuggestedNames),
        (
            SameButForCase = all_have_non_case_difference,
            DidYouMeanPieces0 =
                [words("(Did you mean")] ++ SuggestedNamePieces ++
                [suffix("?)"), nl]
        ;
            SameButForCase = some_have_only_case_difference,
            DidYouMeanPieces0 =
                [words("(Did you mean")] ++ SuggestedNamePieces ++
                [suffix("?"),
                % Note that "lower-vs-upper" is probably just clutter
                % for experts, but may be essential for others, especially
                % non-native English speakers.
                words("Note the lower-vs-upper case difference.)"), nl]
        ),
        DidYouMeanPieces = color_as_hint(DidYouMeanPieces0)
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

:- type same_but_for_case
    --->    all_have_non_case_difference
    ;       some_have_only_case_difference.

:- pred is_any_suggestion_same_but_for_case(list(char)::in, list(string)::in,
    same_but_for_case::out) is det.

is_any_suggestion_same_but_for_case(_, [], all_have_non_case_difference).
is_any_suggestion_same_but_for_case(BaseNameLowerChars, [Name | Names],
        SameButForCase) :-
    NameChars = string.to_char_list(Name),
    list.map(char.to_lower, NameChars, NameLowerChars),
    ( if BaseNameLowerChars = NameLowerChars then
        SameButForCase = some_have_only_case_difference
    else
        is_any_suggestion_same_but_for_case(BaseNameLowerChars, Names,
            SameButForCase)
    ).

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

color_as_subject(Pieces) =
    color_pieces(color_subject, Pieces).

color_as_correct(Pieces) =
    color_pieces(color_correct, Pieces).

color_as_incorrect(Pieces) =
    color_pieces(color_incorrect, Pieces).

color_as_inconsistent(Pieces) =
    color_pieces(color_inconsistent, Pieces).

color_as_hint(Pieces) =
    color_pieces(color_hint, Pieces).

color_pieces(Color, Pieces0) = Pieces :-
    list.take_while(is_nl_piece, Pieces0, HeadNlPieces, Pieces1),
    list.reverse(Pieces1, RevPieces1),
    list.take_while(is_nl_piece, RevPieces1, RevTailNlPieces1, RevMainPieces1),
    list.reverse(RevTailNlPieces1, TailNlPieces),
    list.reverse(RevMainPieces1, MainPieces),
    (
        MainPieces = [_ | _],
        Pieces =
            HeadNlPieces ++
            [not_for_general_use_start_color(Color)] ++
            MainPieces ++
            [not_for_general_use_end_color(Color)] ++
            TailNlPieces
    ;
        MainPieces = [],
        % There are no pieces to apply color to.
        Pieces = HeadNlPieces ++ TailNlPieces
    ).

:- pred is_nl_piece(format_piece::in) is semidet.

is_nl_piece(Piece) :-
    ( Piece = nl
    ; Piece = nl_indent_delta(_)
    ).

maybe_color_pieces(MaybeColor, Pieces) = MaybeColorPieces :-
    (
        MaybeColor = no,
        MaybeColorPieces = Pieces
    ;
        MaybeColor = yes(Color),
        MaybeColorPieces = color_pieces(Color, Pieces)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

fixed_list_to_pieces(LastSep, Strings) = Pieces :-
    ItemToPieces = (func(S) = [fixed(S)]),
    transform_list_to_pieces(ItemToPieces, LastSep, Strings, Pieces).

quote_list_to_pieces(LastSep, Strings) = Pieces :-
    ItemToPieces = (func(S) = [quote(S)]),
    transform_list_to_pieces(ItemToPieces, LastSep, Strings, Pieces).

piece_list_to_pieces(LastSep, PieceList) = Pieces :-
    ItemToPieces = (func(P) = [P]),
    transform_list_to_pieces(ItemToPieces, LastSep, PieceList, Pieces).

pieces_list_to_pieces(LastSep, PiecesList) = Pieces :-
    ItemToPieces = (func(Ps) = Ps),
    transform_list_to_pieces(ItemToPieces, LastSep, PiecesList, Pieces).

%---------------------%

:- pred transform_list_to_pieces((func(T) = list(format_piece))::in,
    string::in, list(T)::in, list(format_piece)::out) is det.

transform_list_to_pieces(ItemToPieces, LastSep, Items, Pieces) :-
    MaybeColor = no,
    NonLastSepPieces = [suffix(",")],
    LastSepPieces = [words(LastSep)],
    LastSuffix = [],
    general_list_to_pieces(ItemToPieces, MaybeColor,
        NonLastSepPieces, LastSepPieces, LastSuffix, Items, Pieces).

%---------------------------------------------------------------------------%

fixed_list_to_color_pieces(Color, LastSep, LastSuffix, Strings) = Pieces :-
    ItemToPieces = (func(S) = [fixed(S)]),
    transform_list_to_color_pieces(ItemToPieces, Color, LastSep, LastSuffix,
        Strings, Pieces).

quote_list_to_color_pieces(Color, LastSep, LastSuffix, Strings) = Pieces :-
    ItemToPieces = (func(S) = [quote(S)]),
    transform_list_to_color_pieces(ItemToPieces, Color, LastSep, LastSuffix,
        Strings, Pieces).

piece_list_to_color_pieces(Color, LastSep, LastSuffix, PieceList) = Pieces :-
    ItemToPieces = (func(P) = [P]),
    transform_list_to_color_pieces(ItemToPieces, Color, LastSep, LastSuffix,
        PieceList, Pieces).

pieces_list_to_color_pieces(Color, LastSep, LastSuffix, PiecesList) = Pieces :-
    ItemToPieces = (func(Ps) = Ps),
    transform_list_to_color_pieces(ItemToPieces, Color, LastSep, LastSuffix,
        PiecesList, Pieces).

%---------------------%

:- pred transform_list_to_color_pieces((func(T) = list(format_piece))::in,
    color_name::in, string::in, list(format_piece)::in, list(T)::in,
    list(format_piece)::out) is det.

transform_list_to_color_pieces(ItemToPieces, Color, LastSep, LastSuffix,
        Items, Pieces) :-
    NonLastSepPieces = [suffix(",")],
    LastSepPieces = [words(LastSep)],
    general_list_to_pieces(ItemToPieces, yes(Color),
        NonLastSepPieces, LastSepPieces, LastSuffix, Items, Pieces).

%---------------------------------------------------------------------------%

fixed_strict_list_to_pieces(Strings) = Pieces :-
    ItemToPieces = (func(S) = [fixed(S)]),
    transform_strict_list_to_pieces(ItemToPieces, Strings, Pieces).

quote_strict_list_to_pieces(Strings) = Pieces :-
    ItemToPieces = (func(S) = [quote(S)]),
    transform_strict_list_to_pieces(ItemToPieces, Strings, Pieces).

piece_strict_list_to_pieces(PieceList) = Pieces :-
    ItemToPieces = (func(P) = [P]),
    transform_strict_list_to_pieces(ItemToPieces, PieceList, Pieces).

pieces_strict_list_to_pieces(PiecesList) = Pieces :-
    ItemToPieces = (func(Ps) = Ps),
    transform_strict_list_to_pieces(ItemToPieces, PiecesList, Pieces).

%---------------------%

:- pred transform_strict_list_to_pieces((func(T) = list(format_piece))::in,
    list(T)::in, list(format_piece)::out) is det.

transform_strict_list_to_pieces(ItemToPieces, Items, Pieces) :-
    MaybeColor = no,
    SepPieces = [suffix(",")],
    LastSuffix = [],
    strict_general_list_to_pieces(ItemToPieces, MaybeColor,
        SepPieces, LastSuffix, Items, Pieces).

%---------------------------------------------------------------------------%

fixed_strict_list_to_color_pieces(Color, LastSuffix, Strings) = Pieces :-
    ItemToPieces = (func(S) = [fixed(S)]),
    transform_strict_list_to_color_pieces(ItemToPieces, Color, LastSuffix,
        Strings, Pieces).

quote_strict_list_to_color_pieces(Color, LastSuffix, Strings) = Pieces :-
    ItemToPieces = (func(S) = [quote(S)]),
    transform_strict_list_to_color_pieces(ItemToPieces, Color, LastSuffix,
        Strings, Pieces).

piece_strict_list_to_color_pieces(Color, LastSuffix, PieceList) = Pieces :-
    ItemToPieces = (func(P) = [P]),
    transform_strict_list_to_color_pieces(ItemToPieces, Color, LastSuffix,
        PieceList, Pieces).

pieces_strict_list_to_color_pieces(Color, LastSuffix, PiecesList) = Pieces :-
    ItemToPieces = (func(Ps) = Ps),
    transform_strict_list_to_color_pieces(ItemToPieces, Color, LastSuffix,
        PiecesList, Pieces).

%---------------------%

:- pred transform_strict_list_to_color_pieces(
    (func(T) = list(format_piece))::in,
    color_name::in, list(format_piece)::in, list(T)::in,
    list(format_piece)::out) is det.

transform_strict_list_to_color_pieces(ItemToPieces, Color, LastSuffix,
        Items, Pieces) :-
    SepPieces = [suffix(",")],
    strict_general_list_to_pieces(ItemToPieces, yes(Color),
        SepPieces, LastSuffix, Items, Pieces).

%---------------------------------------------------------------------------%

fixed_list_to_line_pieces(Strings) = Pieces :-
    ItemToPieces = (func(S) = [fixed(S)]),
    transform_list_to_line_pieces(ItemToPieces, Strings, Pieces).

quote_list_to_line_pieces(Strings) = Pieces :-
    ItemToPieces = (func(S) = [quote(S)]),
    transform_list_to_line_pieces(ItemToPieces, Strings, Pieces).

piece_list_to_line_pieces(PieceList) = Pieces :-
    ItemToPieces = (func(P) = [P]),
    transform_list_to_line_pieces(ItemToPieces, PieceList, Pieces).

pieces_list_to_line_pieces(PiecesList) = Pieces :-
    ItemToPieces = (func(Ps) = Ps),
    transform_list_to_line_pieces(ItemToPieces, PiecesList, Pieces).

%---------------------%

:- pred transform_list_to_line_pieces((func(T) = list(format_piece))::in,
    list(T)::in, list(format_piece)::out) is det.

transform_list_to_line_pieces(ItemToPieces, Items, Pieces) :-
    MaybeColor = no,
    LastSuffix = [],
    strict_general_list_to_line_pieces(ItemToPieces, MaybeColor, LastSuffix,
        Items, Pieces).

%---------------------------------------------------------------------------%

fixed_list_to_color_line_pieces(Color, LastSuffix, Strings) = Pieces :-
    ItemToPieces = (func(S) = [fixed(S)]),
    transform_list_to_color_line_pieces(ItemToPieces, Color, LastSuffix,
        Strings, Pieces).

quote_list_to_color_line_pieces(Color, LastSuffix, Strings) = Pieces :-
    ItemToPieces = (func(S) = [quote(S)]),
    transform_list_to_color_line_pieces(ItemToPieces, Color, LastSuffix,
        Strings, Pieces).

piece_list_to_color_line_pieces(Color, LastSuffix, PieceList) = Pieces :-
    ItemToPieces = (func(P) = [P]),
    transform_list_to_color_line_pieces(ItemToPieces, Color, LastSuffix,
        PieceList, Pieces).

pieces_list_to_color_line_pieces(Color, LastSuffix, PiecesList) = Pieces :-
    ItemToPieces = (func(Ps) = Ps),
    transform_list_to_color_line_pieces(ItemToPieces, Color, LastSuffix,
        PiecesList, Pieces).

%---------------------%

:- pred transform_list_to_color_line_pieces((func(T) = list(format_piece))::in,
    color_name::in, list(format_piece)::in, list(T)::in,
    list(format_piece)::out) is det.

transform_list_to_color_line_pieces(ItemToPieces, Color, LastSuffix,
        Items, Pieces) :-
    strict_general_list_to_line_pieces(ItemToPieces, yes(Color),
        LastSuffix, Items, Pieces).

%---------------------------------------------------------------------------%

pieces_list_to_split_color_line_pieces(MaybeColorA, MaybeColorB,
        LastSuffixB, Pairs) = Pieces :-
    ItemToPieces = (func({A, B}) = {A, B}),
    strict_general_list_to_split_line_pieces(ItemToPieces,
        MaybeColorA, MaybeColorB, LastSuffixB, Pairs, Pieces).

%---------------------------------------------------------------------------%
%
% General predicates for converting any list of items to the specification
% of a nicely formatted and possibly colored output. Used to implement
% the functions above.
%

:- pred general_list_to_pieces((func(T) = list(format_piece))::in,
    maybe(color_name)::in, list(format_piece)::in, list(format_piece)::in,
    list(format_piece)::in, list(T)::in, list(format_piece)::out) is det.

general_list_to_pieces(ItemToPieces, MaybeColor, NonLastSep, LastSep,
        LastSuffix, Items, Pieces) :-
    (
        Items = [],
        Pieces = []
    ;
        Items = [Item1],
        Pieces1 = ItemToPieces(Item1),
        Pieces = maybe_color_pieces(MaybeColor, Pieces1 ++ LastSuffix)
    ;
        Items = [Item1, Item2],
        Pieces1 = ItemToPieces(Item1),
        Pieces2 = ItemToPieces(Item2),
        Pieces = maybe_color_pieces(MaybeColor, Pieces1) ++ LastSep ++
            maybe_color_pieces(MaybeColor, Pieces2 ++ LastSuffix)
    ;
        Items = [Item1, Item2, Item3 | Items4plus],
        Pieces1 = ItemToPieces(Item1),
        general_list_to_pieces(ItemToPieces, MaybeColor, NonLastSep, LastSep,
            LastSuffix, [Item2, Item3 | Items4plus], TailPieces),
        Pieces = maybe_color_pieces(MaybeColor, Pieces1 ++ NonLastSep) ++
            TailPieces
    ).

:- pred strict_general_list_to_pieces((func(T) = list(format_piece))::in,
    maybe(color_name)::in, list(format_piece)::in,
    list(format_piece)::in, list(T)::in, list(format_piece)::out) is det.

strict_general_list_to_pieces(ItemToPieces, MaybeColor, Sep, LastSuffix,
        Items, Pieces) :-
    (
        Items = [],
        Pieces = []
    ;
        Items = [Item1],
        Pieces1 = ItemToPieces(Item1),
        Pieces = maybe_color_pieces(MaybeColor, Pieces1 ++ LastSuffix)
    ;
        Items = [Item1, Item2 | Items3plus],
        Pieces1 = ItemToPieces(Item1),
        strict_general_list_to_pieces(ItemToPieces, MaybeColor, Sep,
            LastSuffix, [Item2 | Items3plus], TailPieces),
        Pieces = maybe_color_pieces(MaybeColor, Pieces1 ++ Sep) ++ TailPieces
    ).

:- pred strict_general_list_to_line_pieces((func(T) = list(format_piece))::in,
    maybe(color_name)::in, list(format_piece)::in,
    list(T)::in, list(format_piece)::out) is det.

strict_general_list_to_line_pieces(ItemToPieces, MaybeColor, LastSuffix,
        Items, Pieces) :-
    (
        Items = [],
        Pieces = []
    ;
        Items = [Item1],
        Pieces1 = ItemToPieces(Item1),
        Pieces = maybe_color_pieces(MaybeColor, Pieces1 ++ LastSuffix)
    ;
        Items = [Item1, Item2 | Items3plus],
        Pieces1 = ItemToPieces(Item1),
        HeadPieces = maybe_color_pieces(MaybeColor, Pieces1 ++ [suffix(",")]),
        strict_general_list_to_line_pieces(ItemToPieces, MaybeColor,
            LastSuffix, [Item2 | Items3plus], TailPieces),
        Pieces = HeadPieces ++ [nl] ++ TailPieces
    ).

:- pred strict_general_list_to_split_line_pieces(
    (func(T) = {list(format_piece), list(format_piece)})::in,
    maybe(color_name)::in, maybe(color_name)::in, list(format_piece)::in,
    list(T)::in, list(format_piece)::out) is det.

strict_general_list_to_split_line_pieces(ItemToPieces,
        MaybeColorA, MaybeColorB, LastSuffixB, Items, Pieces) :-
    (
        Items = [],
        Pieces = []
    ;
        Items = [Item1],
        {Pieces1A, Pieces1B} = ItemToPieces(Item1),
        HeadPiecesA = maybe_color_pieces(MaybeColorA, Pieces1A),
        HeadPiecesB = maybe_color_pieces(MaybeColorB, Pieces1B ++
            LastSuffixB),
        Pieces = HeadPiecesA ++ HeadPiecesB
    ;
        Items = [Item1, Item2 | Items3plus],
        {Pieces1A, Pieces1B} = ItemToPieces(Item1),
        CommaB = [suffix(",")],
        HeadPiecesA = maybe_color_pieces(MaybeColorA, Pieces1A),
        HeadPiecesB = maybe_color_pieces(MaybeColorB, Pieces1B ++ CommaB),
        strict_general_list_to_split_line_pieces(ItemToPieces,
            MaybeColorA, MaybeColorB, LastSuffixB, [Item2 | Items3plus],
            TailPieces),
        Pieces = HeadPiecesA ++ HeadPiecesB ++ [nl] ++ TailPieces
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.error_spec.
%---------------------------------------------------------------------------%
