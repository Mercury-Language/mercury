%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_tree_out_misc.m.
% Main author: fjh.
%
% This module converts some of the simplest parts of the parse tree structure
% back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out_misc.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.indent.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.parse_tree_output.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module string.
:- import_module string.builder.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Write out constants, or convert them to strings.
%

:- func mercury_det_to_string(determinism) = string.

:- func promise_to_string(promise_type) = string.
:- mode promise_to_string(in) = out is det.
:- mode promise_to_string(out) = in is semidet.
:- mode promise_to_string(out) = out is multi.

    % Return "predicate" or "function" depending on the given value.
    %
:- func pred_or_func_to_full_str(pred_or_func) = string.

    % Return "pred" or "func" depending on the given value.
    %
:- func pred_or_func_to_str(pred_or_func) = string.

    % Get a purity name as a string.
    %
:- pred purity_name(purity, string).
:- mode purity_name(in, out) is det.
:- mode purity_name(out, in) is semidet.

    % Print out a purity prefix.
    % This works under the assumptions that all purity names but `pure'
    % are operators, and that we never need `pure' indicators/declarations.
    %
:- func purity_prefix_to_string(purity) = string.

    % Convert an eval_method of a pragma to a string giving the name
    % of the pragma.
    %
:- func tabled_eval_method_to_pragma_name(tabled_eval_method) = string.

    % Convert an eval_method to a string description.
    %
:- func eval_method_to_string(eval_method) = string.
:- func tabled_eval_method_to_string(tabled_eval_method) = string.

:- func maybe_arg_tabling_method_to_string(maybe(arg_tabling_method)) = string.

:- func arg_tabling_method_to_string(arg_tabling_method) = string.

:- func determinism_to_string(determinism) = string.

:- func can_fail_to_string(can_fail) = string.

:- func goal_warning_to_string(goal_warning) = string.

%---------------------------------------------------------------------------%

    % Output an existential quantifier.
    %
:- func mercury_quantifier_to_string(tvarset, var_name_print, existq_tvars)
    = string.
:- pred mercury_output_quantifier(tvarset::in, var_name_print::in,
    existq_tvars::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_quantifier(tvarset::in, var_name_print::in,
    existq_tvars::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % Similar to mercury_output_vars/3, but prefixes each variable
    % with `!' to indicate that it is a state variable.
    %
:- pred mercury_output_state_vars(varset(T)::in, var_name_print::in,
    list(var(T))::in, io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_state_vars(varset(T)::in, var_name_print::in,
    list(var(T))::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- func mercury_foreign_language_to_string(foreign_language) = string.
:- pred mercury_output_foreign_language_string(foreign_language::in,
    io.text_output_stream::in, io::di, io::uo) is det.
:- pred mercury_format_foreign_language_string(foreign_language::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Write out context.
%

    % Write to a string the information in term context (at the moment,
    % just the line number) in a form suitable for the beginning of an
    % error message.
    %
:- pred context_to_string(prog_context::in, string::out) is det.

    % Write out the information in term context (at the moment, just the
    % line number) in a form suitable for the beginning of an error message.
    %
:- pred write_context(io.text_output_stream::in, prog_context::in,
    io::di, io::uo) is det.
:- pred format_context(S::in, prog_context::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Write out indentation.
%

:- pred mercury_format_tabs(indent::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- pred mercury_output_newline(indent::in, io.text_output_stream::in,
    io::di, io::uo) is det.
:- pred mercury_format_newline(indent::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pragma type_spec_constrained_preds([pt_output(Stream, State)],
    apply_to_superclasses,
    [subst([Stream => io.text_output_stream, State = io.state]),
    subst([Stream => string.builder.handle, State = string.builder.state])]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_tree_out_term.

:- import_module require.
:- import_module term_context.
:- import_module uint.

%---------------------------------------------------------------------------%

mercury_det_to_string(detism_det) = "det".
mercury_det_to_string(detism_semi) = "semidet".
mercury_det_to_string(detism_non) = "nondet".
mercury_det_to_string(detism_multi) = "multi".
mercury_det_to_string(detism_cc_multi) = "cc_multi".
mercury_det_to_string(detism_cc_non) = "cc_nondet".
mercury_det_to_string(detism_failure) = "failure".
mercury_det_to_string(detism_erroneous) = "erroneous".

promise_to_string(promise_type_true) = "promise".
promise_to_string(promise_type_exclusive) = "promise_exclusive".
promise_to_string(promise_type_exhaustive) = "promise_exhaustive".
promise_to_string(promise_type_exclusive_exhaustive) =
    "promise_exclusive_exhaustive".

pred_or_func_to_full_str(pf_predicate) = "predicate".
pred_or_func_to_full_str(pf_function) = "function".

pred_or_func_to_str(pf_predicate) = "pred".
pred_or_func_to_str(pf_function) = "func".

purity_name(purity_pure, "pure").
purity_name(purity_semipure, "semipure").
purity_name(purity_impure, "impure").

purity_prefix_to_string(Purity) = Str :-
    (
        Purity = purity_pure,
        Str = ""
    ;
        ( Purity = purity_impure
        ; Purity = purity_semipure
        ),
        purity_name(Purity, PurityName),
        Str = PurityName ++ " "
    ).

tabled_eval_method_to_pragma_name(tabled_loop_check) = "loop_check".
tabled_eval_method_to_pragma_name(tabled_memo(_)) =  "memo".
tabled_eval_method_to_pragma_name(tabled_minimal(MinimalMethod)) = Str :-
    (
        MinimalMethod = own_stacks_consumer,
        % The fact that this is not the name of the corresponding pragma
        % won't matter until this becomes the default way of doing minimal
        % model tabling, at which time we will return "minimal_model" here
        % and "minimal_model_stack_copy" in the other arm of the switch.
        Str = "minimal_model_own_stacks"
    ;
        MinimalMethod = own_stacks_generator,
        Str = "minimal_model_own_stacks_generator"
    ;
        MinimalMethod = stack_copy,
        Str = "minimal_model"
    ).
tabled_eval_method_to_pragma_name(tabled_io(_, _)) = _ :-
    unexpected($pred, "io").

eval_method_to_string(eval_normal) = "normal".
eval_method_to_string(eval_tabled(TabledMethod)) =
    tabled_eval_method_to_string(TabledMethod).

tabled_eval_method_to_string(TabledMethod) = Str :-
    (
        TabledMethod = tabled_loop_check,
        Str = "loop_check"
    ;
        TabledMethod = tabled_memo(_),
        Str = "memo"
    ;
        TabledMethod = tabled_minimal(MinimalMethod),
        (
            MinimalMethod = own_stacks_consumer,
            Str = "minimal_model_own_stacks_consumer"
        ;
            MinimalMethod = own_stacks_generator,
            Str = "minimal_model_own_stacks_generator"
        ;
            MinimalMethod = stack_copy,
            Str = "minimal_model_stack_copy"
        )
    ;
        TabledMethod = tabled_io(EntryKind, IsUnitize),
        (
            EntryKind = entry_stores_outputs,
            EntryKindStr = "entry_stores_outputs, "
        ;
            EntryKind = entry_stores_procid_outputs,
            EntryKindStr = "entry_stores_procid_outputs, "
        ;
            EntryKind = entry_stores_procid_inputs_outputs,
            EntryKindStr = "entry_stores_procid_inputs_outputs, "
        ),
        (
            IsUnitize = table_io_unitize,
            UnitizeStr = "unitize"
        ;
            IsUnitize = table_io_alone,
            UnitizeStr = "alone"
        ),
        Str = "table_io(" ++ EntryKindStr ++ UnitizeStr ++ ")"
    ).

maybe_arg_tabling_method_to_string(yes(ArgTablingMethod)) =
    arg_tabling_method_to_string(ArgTablingMethod).
maybe_arg_tabling_method_to_string(no) = "output".

arg_tabling_method_to_string(arg_value) = "value".
arg_tabling_method_to_string(arg_addr) = "addr".
arg_tabling_method_to_string(arg_promise_implied) = "promise_implied".

determinism_to_string(detism_det) = "det".
determinism_to_string(detism_semi) = "semidet".
determinism_to_string(detism_non) = "nondet".
determinism_to_string(detism_multi) = "multi".
determinism_to_string(detism_cc_non) = "cc_nondet".
determinism_to_string(detism_cc_multi) = "cc_multi".
determinism_to_string(detism_erroneous) = "erroneous".
determinism_to_string(detism_failure) = "failure".

can_fail_to_string(can_fail) = "can_fail".
can_fail_to_string(cannot_fail) = "cannot_fail".

goal_warning_to_string(Warning) = Str :-
    (
        Warning = goal_warning_non_tail_recursive_calls,
        Str = "non_tail_recursive_calls"
    ;
        Warning = goal_warning_occurs_check,
        Str = "suspected_occurs_check_failure"
    ;
        Warning = goal_warning_singleton_vars,
        Str = "singleton_vars"
    ;
        Warning = goal_warning_suspicious_recursion,
        Str = "suspicious_recursion"
    ;
        Warning = goal_warning_no_solution_disjunct,
        Str = "no_solution_disjunct"
    ;
        Warning = goal_warning_unknown_format_calls,
        Str = "unknown_format_calls"
    ).

%---------------------------------------------------------------------------%

mercury_quantifier_to_string(TypeVarSet, VarNamePrint, ExistQVars) = Str :-
    State0 = string.builder.init,
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars,
        string.builder.handle, State0, State),
    Str = string.builder.to_string(State).

mercury_output_quantifier(TypeVarSet, VarNamePrint, ExistQVars, Stream, !IO) :-
    mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars,
        Stream, !IO).

mercury_format_quantifier(TypeVarSet, VarNamePrint, ExistQVars, S, !U) :-
    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        add_string("some [", S, !U),
        mercury_format_vars_vs(TypeVarSet, VarNamePrint, ExistQVars, S, !U),
        add_string("] ", S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_output_state_vars(VarSet, VarNamePrint, StateVars, Stream, !IO) :-
    mercury_format_state_vars(VarSet, VarNamePrint, StateVars, Stream, !IO).

mercury_format_state_vars(VarSet, VarNamePrint, StateVars, S, !U) :-
    list.gap_foldl(mercury_format_state_var(VarSet, VarNamePrint, S),
        add_string(", ", S), StateVars, !U).

:- pred mercury_format_state_var(varset(T)::in, var_name_print::in, S::in,
    var(T)::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_state_var(VarSet, VarNamePrint, S, Var, !U) :-
    add_string("!", S, !U),
    mercury_format_var_vs(VarSet, VarNamePrint, Var, S, !U).

%---------------------------------------------------------------------------%

mercury_foreign_language_to_string(Lang) = Str :-
    Str = "\"" ++ foreign_language_string(Lang) ++ "\"".

mercury_output_foreign_language_string(Lang, Stream, !IO) :-
    mercury_format_foreign_language_string(Lang, Stream, !IO).

mercury_format_foreign_language_string(Lang, S, !U) :-
    add_string(mercury_foreign_language_to_string(Lang), S, !U).

%---------------------------------------------------------------------------%

context_to_string(Context, ContextStr) :-
    FileName = term_context.context_file(Context),
    LineNumber = term_context.context_line(Context),
    ( if FileName = "" then
        ContextStr = ""
    else
        string.format("%s:%03d: ", [s(FileName), i(LineNumber)], ContextStr)
    ).

write_context(Stream, Context, !IO) :-
    context_to_string(Context, ContextMessage),
    io.write_string(Stream, ContextMessage, !IO).

format_context(S, Context, !U) :-
    context_to_string(Context, ContextMessage),
    add_string(ContextMessage, S, !U).

%---------------------------------------------------------------------------%

mercury_format_tabs(Indent, S, !U) :-
    ( if Indent > 0u then
        add_string("\t", S, !U),
        mercury_format_tabs(Indent - 1u, S, !U)
    else
        true
    ).

%---------------------------------------------------------------------------%

mercury_output_newline(Indent, Stream, !IO) :-
    mercury_format_newline(Indent, Stream, !IO).

mercury_format_newline(Indent, S, !U) :-
    add_char('\n', S, !U),
    mercury_format_tabs(Indent, S, !U).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out_misc.
%---------------------------------------------------------------------------%
