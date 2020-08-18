%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module defines the types that represent information related to pragmas
% in the parse tree.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_data_pragma.
:- interface.

:- import_module libs.
:- import_module libs.compiler_util.
:- import_module libs.rat.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module unit.

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%
%
% Stuff for tabling pragmas.
%

:- interface.

    % The evaluation method that should be used for a procedure.
    %
:- type eval_method
    --->    eval_normal                 % normal mercury evaluation
    ;       eval_loop_check             % loop check only
    ;       eval_memo(                  % memoing + loop check
                % Preserve the value of this attribure until the invocation
                % of the relevant code in table_gen.m.
                table_attr_backend_warning
            )
    ;       eval_table_io(              % memoing I/O actions for debugging
                table_io_entry_kind,
                table_io_is_unitize
            )
    ;       eval_minimal(eval_minimal_method).
                                        % minimal model evaluation

:- type eval_minimal_method
    --->    stack_copy
            % Each minimal model procedure saves and restores stack segments
            % as necessary. See the paper "Tabling in Mercury" by Zoltan
            % Somogyi and Konstantinos Sagonas.

    ;       own_stacks_consumer
    ;       own_stacks_generator.
            % Each minimal model procedure is split into two: the consumer
            % and the generator. Each generator runs in its own context,
            % and thus has its own stacks.

:- type table_attributes
    --->    table_attributes(
                table_attr_strictness       :: call_table_strictness,
                table_attr_size_limit       :: maybe(int),
                table_attr_statistics       :: table_attr_statistics,
                table_attr_allow_reset      :: table_attr_allow_reset,
                table_attr_backend_warning  :: table_attr_backend_warning
            ).

:- func default_memo_table_attributes = table_attributes.

:- type table_attr_statistics
    --->    table_gather_statistics
    ;       table_dont_gather_statistics.

:- type table_attr_allow_reset
    --->    table_allow_reset
    ;       table_dont_allow_reset.

    % If the current backend cannot implement the requested form of tabling,
    % and is therefore forced to ignore it, should the compiler generate
    % a warning?
:- type table_attr_backend_warning
    --->    table_attr_ignore_with_warning      % Yes, generate a warning.
    ;       table_attr_ignore_without_warning.  % Do not generate a warning.

:- type call_table_strictness
    --->    cts_all_strict
    ;       cts_all_fast_loose
    ;       cts_specified(
                list(maybe(arg_tabling_method)),
                % This list contains one element for each user-visible
                % argument of the predicate. Elements that correspond
                % to output arguments should be "no". Elements that
                % correspond to input arguments should be "yes",
                % specifying how to look up that argument in the call table.

                hidden_arg_tabling_method
                % This specifies the tabling method for hidden arguments
                % introduced by the compiler.
            ).

:- type arg_tabling_method
    --->    arg_value
    ;       arg_addr
    ;       arg_promise_implied.

:- type hidden_arg_tabling_method
    --->    table_hidden_arg_value
    ;       table_hidden_arg_addr.

:- type table_io_entry_kind
    --->    entry_stores_outputs
            % Each entry in the I/O table stores only the outputs of the
            % action. The I/O action will be idempotent across retries
            % in mdb, but attempts to print out the action will cause
            % a core dump. This option is intended only for implementors
            % measuring the overheads of the two alternatives just below.

    ;       entry_stores_procid_outputs
            % Each entry in the I/O table starts with a pointer to the
            % MR_TableIoEntry structure of the procedure that performed
            % the action, and also contains the outputs of the action.
            % This makes the I/O action idempotent across retries and
            % allows the *name* of the I/O predicate to be printed
            % by mdb's "print action N" command, but not the values
            % of the arguments. Not even the output arguments can be printed,
            % since doing so requires knowing their types, and in general
            % that requires access to input type_info arguments.

    ;       entry_stores_procid_inputs_outputs.
            % Each entry in the I/O table starts with a pointer to the
            % MR_TableIoEntry structure of the procedure that performed
            % the action, and also contains both the inputs and outputs
            % of the action.
            %
            % This makes the I/O action idempotent across retries and
            % allows both the name and all the arguments of the I/O predicate
            % to be printed by mdb's "print action N" command. It also
            % allows the declarative debugger to consider the action to
            % be part of the effect of a call to its ancestors.

:- type table_io_is_unitize
    --->    table_io_unitize    % The procedure is tabled for I/O
                                % together with its Mercury descendants.

    ;       table_io_alone.     % The procedure is tabled for I/O by itself;
                                % it can have no Mercury descendants.

:- func eval_method_to_table_type(eval_method) = string.

:- implementation.

default_memo_table_attributes =
    table_attributes(cts_all_strict, no, table_dont_gather_statistics,
        table_dont_allow_reset, table_attr_ignore_with_warning).

eval_method_to_table_type(EvalMethod) = TableTypeStr :-
    (
        EvalMethod = eval_normal,
        unexpected($pred, "eval_normal")
    ;
        EvalMethod = eval_table_io(_, _),
        unexpected($pred, "eval_table_io")
    ;
        EvalMethod = eval_loop_check,
        TableTypeStr = "MR_TABLE_TYPE_LOOPCHECK"
    ;
        EvalMethod = eval_memo(_),
        TableTypeStr = "MR_TABLE_TYPE_MEMO"
    ;
        EvalMethod = eval_minimal(stack_copy),
        TableTypeStr = "MR_TABLE_TYPE_MINIMAL_MODEL_STACK_COPY"
    ;
        EvalMethod = eval_minimal(own_stacks_consumer),
        unexpected($pred, "own_stacks_consumer")
    ;
        EvalMethod = eval_minimal(own_stacks_generator),
        TableTypeStr = "MR_TABLE_TYPE_MINIMAL_MODEL_OWN_STACKS"
    ).

%---------------------------------------------------------------------------%
%
% Stuff for the `termination_info' pragma.
% See term_util.m.
%

:- interface.

:- type generic_arg_size_info(ErrorInfo)
    --->    finite(int, list(bool))
            % The termination constant is a finite integer. The list of bool
            % has a 1:1 correspondence with the input arguments of the
            % procedure. It stores whether the argument contributes to the
            % size of the output arguments.

    ;       infinite(ErrorInfo).
            % There is no finite integer for which the above equation is true.

:- type generic_termination_info(TermInfo, ErrorInfo)
    --->    cannot_loop(TermInfo)
            % This procedure definitely terminates for all possible inputs.
    ;       can_loop(ErrorInfo).
            % This procedure might not terminate.

:- type pragma_arg_size_info    == generic_arg_size_info(unit).
:- type pragma_termination_info == generic_termination_info(unit, unit).

%---------------------------------------------------------------------------%
%
% Stuff for the `termination2_info' pragma.
%

:- interface.

    % This is the form in which termination information from other
    % modules (imported via `.opt' or `.trans_opt' files) comes.
    % We convert this to an intermediate form and let the termination
    % analyser convert it to the correct form.
    %
    % NOTE: the reason that we cannot convert it to the correct form
    % is that we don't have complete information about how many typeinfo
    % related arguments there are until after the polymorphism pass.
    %
:- type arg_size_constr
    --->    le(list(arg_size_term), rat)
    ;       eq(list(arg_size_term), rat).

:- type arg_size_term
    --->    arg_size_term(
                as_term_var   :: int,
                as_term_coeff :: rat
            ).

:- type pragma_constr_arg_size_info == list(arg_size_constr).

%---------------------------------------------------------------------------%
%
% Stuff for the `structure_sharing_info' pragma.
%

:- interface.

    % Whenever structure sharing analysis is unable to determine a good
    % approximation of the set of structure sharing pairs that might exist
    % during the execution of a program, it must use "top" as the only safe
    % approximation.
    %
    % We divide the reasons for approximating by `top' into two cases:
    %
    % - the procedure calls some imported procedure for which we don't have an
    %   answer (yet). The result might be improved if we did have that
    %   information.
    %
    % - the procedure calls some imported procedure for which we managed to
    %   look up the answer, and that answer was `top'.
    %
    % - the procedure contains a call to foreign or generic code.
    %   Reanalysis will not improve the result.
    %
:- type top_feedback
    --->    top_failed_lookup(shrouded_pred_proc_id)
    ;       top_from_lookup(shrouded_pred_proc_id)
    ;       top_cannot_improve(string).

    % Elements of the structure sharing domain lattice are either bottom
    % (no structure sharing), top (any kind of structure sharing), or
    % a list of structure sharing pairs.
    %
    % This is the public representation of the type "sharing_as".
    %
:- type structure_sharing_domain
    --->    structure_sharing_bottom
    ;       structure_sharing_real(structure_sharing)
    ;       structure_sharing_top(set(top_feedback)).

    % Public representation of structure sharing.
    %
:- type structure_sharing == list(structure_sharing_pair).

    % A structure sharing pair represents the information that two
    % data structures might be represented by the same memoryspace, hence
    % its representation as a pair of datastructs.
    %
:- type structure_sharing_pair == pair(datastruct).

    % A datastruct is a concept that designates a particular subterm of the
    % term to which a particular variable may be bound. The selector is
    % normalized.
    %
:- type datastruct
    --->    selected_cel(
                sc_var      :: prog_var,
                sc_selector :: selector
            ).

    % A selector describes a path in a type-tree.
    %
:- type selector == list(unit_selector).

    % Unit-selectors are either term selectors or type selectors.
    % - A term selector selects a subterm f/n of a term, where f is a functor
    %   (identified by the cons_id), and n an integer.
    % - A type selector designates any subterm that has that specific type.
    %
:- type unit_selector
    --->    termsel(cons_id, int)       % term selector
    ;       typesel(mer_type).          % type selector

    % Type to represent the sharing information that is manually added
    % to procedures implemented as foreign_procs.
    %
:- type user_annotated_sharing
    --->    no_user_annotated_sharing
    ;       user_sharing(
                sharing     ::  structure_sharing_domain,
                maybe_types ::  maybe(user_sharing_type_information)
            ).

    % The user may have declared the sharing in terms of type variables. In
    % that case, we record the types, and the type variable set.
    %
:- type user_sharing_type_information
    --->    user_type_info(
                types       ::  list(mer_type),
                typevarset  ::  tvarset
            ).

%---------------------------------------------------------------------------%
%
% Stuff for the `structure_reuse_info' pragma.
%

:- interface.

:- type dead_var == prog_var.
:- type dead_vars == list(dead_var).
:- type dead_datastruct == datastruct.
:- type dead_datastructs == set(dead_datastruct).
:- type live_var == prog_var.
:- type live_vars == list(prog_var).
:- type set_of_live_var == set_of_progvar.
:- type live_datastruct == datastruct.
:- type live_datastructs == list(live_datastruct).

    % This is the public representation of the type "reuse_as".
    %
:- type structure_reuse_domain
    --->    has_no_reuse
    ;       has_only_unconditional_reuse
    ;       has_conditional_reuse(structure_reuse_conditions).

:- type structure_reuse_conditions == list(structure_reuse_condition).

    % A structure reuse condition specifies all the information needed to
    % verify whether some memory cells can safely be considered as dead at
    % some program point, depending on the calling context.
    % This information consists of three parts:
    %   - a list of dead datastructures specifying which memory cells
    %   might become dead, hence reusable;
    %   - a list of live datastructures that specifies which memory cells
    %   are always live at the place where the above dead datastructures might
    %   become dead;
    %   - a description of the structure sharing existing at the place
    %   where these datastructures might become dead.
    %
:- type structure_reuse_condition
    --->    structure_reuse_condition(
                dead_nodes          :: dead_datastructs,
                local_use_nodes     :: live_datastructs,
                local_sharing       :: structure_sharing_domain
            ).

%---------------------------------------------------------------------------%
%
% Stuff for the `unused_args' pragma.
%

:- interface.

    % This `mode_num' type is only used for mode numbers written out in
    % automatically-generated `pragma unused_args' pragmas in `.opt' files.
    % The mode_num gets converted to an HLDS proc_id by make_hlds.m.
    % We don't want to use the `proc_id' type here since the parse tree
    % (prog_data.m and prog_item.m) should not depend on the HLDS.
    %
:- type mode_num == int.

%---------------------------------------------------------------------------%
%
% Stuff for the `exceptions' pragma.
%

:- interface.

:- type exception_status
    --->    will_not_throw
            % This procedure will not throw an exception.

    ;       may_throw(exception_type)
            % This procedure may throw an exception. The exception is
            % classified by the `exception_type' type.

    ;       throw_conditional.
            % Whether the procedure will not throw an exception depends upon
            % the value of one or more polymorphic arguments. XXX This needs
            % to be extended for ho preds. (See exception_analysis.m for
            % more details).

:- type exception_type
    --->    user_exception
            % The exception that might be thrown is of a result of some code
            % calling exception.throw/1.

    ;       type_exception.
            % The exception is a result of a compiler introduced
            % unification/comparison maybe throwing an exception
            % (in the case of user-defined equality or comparison) or
            % propagating an exception from them.

%---------------------------------------------------------------------------%
%
% Stuff for the `type_spec' pragma.
%

:- interface.

    % The type substitution for a `pragma type_spec' declaration.
    % Elsewhere in the compiler we generally use the `tsubst' type
    % which is a map rather than an assoc_list.
    %
:- type type_subst == assoc_list(tvar, mer_type).

%---------------------------------------------------------------------------%
%
% Stuff for the `require_feature_set' pragma.
%

:- interface.

:- type required_feature
    --->    reqf_concurrency
    ;       reqf_single_prec_float
    ;       reqf_double_prec_float
    ;       reqf_memo
    ;       reqf_parallel_conj
    ;       reqf_trailing
    ;       reqf_strict_sequential
    ;       reqf_conservative_gc.

%---------------------------------------------------------------------------%
%
% Require tail recursion pragma.
%

:- interface.

:- type require_tail_recursion
    --->    suppress_tailrec_warnings(
                rtrs_context            :: prog_context
            )
    ;       enable_tailrec_warnings(
                rtre_warn_or_error      :: warning_or_error,
                rtre_recursion_type     :: require_tail_recursion_type,
                rtre_context            :: prog_context
            ).

:- type require_tail_recursion_type
    --->    only_self_recursion_must_be_tail
    ;       both_self_and_mutual_recursion_must_be_tail.

:- pred require_tailrec_type_string(require_tail_recursion_type, string).
:- mode require_tailrec_type_string(in, out) is det.
:- mode require_tailrec_type_string(out, in) is semidet.

:- implementation.

require_tailrec_type_string(only_self_recursion_must_be_tail,
    "self_recursion_only").
require_tailrec_type_string(both_self_and_mutual_recursion_must_be_tail,
    "self_or_mutual_recursion").

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_data_pragma.
%---------------------------------------------------------------------------%
