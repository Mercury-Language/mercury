%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_data.m.
% Main author: fjh.
%
% This module, together with prog_item, defines a data structure for
% representing Mercury programs.
%
% This data structure specifies basically the same information as is contained
% in the source code, but in a parse tree rather than a flat file.  This
% module defines the parts of the parse tree that are needed by the various
% compiler backends; parts of the parse tree that are not needed by the
% backends are contained in prog_item.m.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_data.
:- interface.

:- import_module libs.globals.
:- import_module libs.rat.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_item.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module term.
:- import_module unit.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Indicates the type of information the compiler should get from the
    % promise declaration's clause.
    %
:- type promise_type
            % promise ex declarations
    --->    promise_type_exclusive
            % Each disjunct is mutually exclusive.

    ;       promise_type_exhaustive
            % Disjunction cannot fail.

    ;       promise_type_exclusive_exhaustive
            % Both of the above assertions

    ;       promise_type_true.
            % Promise goal is true.

:- type type_and_mode
    --->    type_only(mer_type)
    ;       type_and_mode(mer_type, mer_mode).

    % Purity indicates whether a goal can have side effects or can depend on
    % global state. See purity.m and the "Purity" section of the Mercury
    % language reference manual.
:- type purity
    --->    purity_pure
    ;       purity_semipure
    ;       purity_impure.

    % Compare two purities.
    %
:- pred less_pure(purity::in, purity::in) is semidet.

    % Sort of a "maximum" for impurity.
    %
:- func worst_purity(purity, purity) = purity.

    % Sort of a "minimum" for impurity.
    %
:- func best_purity(purity, purity) = purity.

    % The `determinism' type specifies how many solutions a given procedure
    % may have.
    %
:- type determinism
    --->    detism_det
    ;       detism_semi
    ;       detism_multi
    ;       detism_non
    ;       detism_cc_multi
    ;       detism_cc_non
    ;       detism_erroneous
    ;       detism_failure.

:- type can_fail
    --->    can_fail
    ;       cannot_fail.

:- type soln_count
    --->    at_most_zero
    ;       at_most_one
    ;       at_most_many_cc
            % "_cc" means "committed-choice": there is more than one logical
            % solution, but the pred or goal is being used in a context where
            % we are only looking for the first solution.
    ;       at_most_many.

:- pred determinism_components(determinism, can_fail, soln_count).
:- mode determinism_components(in, out, out) is det.
:- mode determinism_components(out, in, in) is det.

    % The following predicates implement the tables for computing the
    % determinism of compound goals from the determinism of their components.

:- pred det_conjunction_detism(determinism::in, determinism::in,
    determinism::out) is det.

:- pred det_par_conjunction_detism(determinism::in, determinism::in,
    determinism::out) is det.

:- pred det_switch_detism(determinism::in, determinism::in, determinism::out)
    is det.

:- pred det_negation_det(determinism::in, maybe(determinism)::out) is det.

:- pred det_conjunction_maxsoln(soln_count::in, soln_count::in,
    soln_count::out) is det.

:- pred det_conjunction_canfail(can_fail::in, can_fail::in, can_fail::out)
    is det.

:- pred det_disjunction_maxsoln(soln_count::in, soln_count::in,
    soln_count::out) is det.

:- pred det_disjunction_canfail(can_fail::in, can_fail::in, can_fail::out)
    is det.

:- pred det_switch_maxsoln(soln_count::in, soln_count::in, soln_count::out)
    is det.

:- pred det_switch_canfail(can_fail::in, can_fail::in, can_fail::out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff for the foreign language interface pragmas
%

    % Is the foreign code declarations local to this module or
    % exported?
    %
:- type foreign_decl_is_local
    --->    foreign_decl_is_local
    ;       foreign_decl_is_exported.

    % A foreign_language_type represents a type that is defined in a
    % foreign language and accessed in Mercury (most likely through
    % pragma foreign_type).
    % Currently we only support foreign_language_types for IL.
    %
    % It is important to distinguish between IL value types and reference
    % types, the compiler may need to generate different code for each of
    % these cases.
    %
:- type foreign_language_type
    --->    il(il_foreign_type)
    ;       c(c_foreign_type)
    ;       java(java_foreign_type).

:- type il_foreign_type
    --->    il_type(
                ref_or_val, % An indicator of whether the type is a
                            % reference of value type.
                string,     % The location of the .NET name (the assembly)
                sym_name    % The .NET type name
            ).

:- type c_foreign_type
    --->    c_type(
                string      % The C type name
            ).

:- type java_foreign_type
    --->    java_type(
                string      % The Java type name
            ).

:- type ref_or_val
    --->    reference
    ;       value.

%-----------------------------------------------------------------------------%
%
% Stuff for tabling pragmas
%

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

    % The evaluation method that should be used for a procedure.
    %
:- type eval_method
    --->    eval_normal                 % normal mercury evaluation
    ;       eval_loop_check             % loop check only
    ;       eval_memo                   % memoing + loop check
    ;       eval_table_io(              % memoing I/O actions for debugging
                table_io_is_decl,
                table_io_is_unitize
            )
    ;       eval_minimal(eval_minimal_method).
                                        % minimal model evaluation

:- type table_attributes
    --->    table_attributes(
                table_attr_strictness   :: call_table_strictness,
                table_attr_size_limit   :: maybe(int),
                table_attr_statistics   :: table_attr_statistics,
                table_attr_allow_reset  :: table_attr_allow_reset
            ).

:- func default_memo_table_attributes = table_attributes.

:- type table_attr_statistics
    --->    table_gather_statistics
    ;       table_dont_gather_statistics.

:- type table_attr_allow_reset
    --->    table_allow_reset
    ;       table_dont_allow_reset.

:- type call_table_strictness
    --->    all_strict
    ;       all_fast_loose
    ;       specified(
                list(maybe(arg_tabling_method))
                % This list contains one element for each user-visible
                % argument of the predicate. Elements that correspond
                % to output arguments should be "no". Elements that
                % correspond to input arguments should be "yes",
                % specifying how to look up that argument in the call table.
            ).

:- type arg_tabling_method
    --->    arg_value
    ;       arg_addr
    ;       arg_promise_implied.

:- type table_io_is_decl
    --->    table_io_decl       % The procedure is tabled for
                                % declarative debugging.
    ;       table_io_proc.      % The procedure is tabled only for
                                % procedural debugging.

:- type table_io_is_unitize
    --->    table_io_unitize    % The procedure is tabled for I/O
                                % together with its Mercury descendants.

    ;       table_io_alone.     % The procedure is tabled for I/O by itself;
                                % it can have no Mercury descendants.

:- func eval_method_to_table_type(eval_method) = string.

%-----------------------------------------------------------------------------%
%
% Stuff for the `termination_info' pragma.
% See term_util.m.
%

:- type generic_arg_size_info(ErrorInfo)
    --->    finite(int, list(bool))
            % The termination constant is a finite integer.  The list of bool
            % has a 1:1 correspondence with the input arguments of the
            % procedure.  It stores whether the argument contributes to the
            % size of the output arguments.

    ;       infinite(ErrorInfo).
            % There is no finite integer for which the above equation is true.

:- type generic_termination_info(TermInfo, ErrorInfo)
    --->    cannot_loop(TermInfo)   % This procedure definitely terminates
                                    % for all possible inputs.
    ;       can_loop(ErrorInfo).
                                    % This procedure might not terminate.

:- type pragma_arg_size_info    == generic_arg_size_info(unit).
:- type pragma_termination_info == generic_termination_info(unit, unit).

%-----------------------------------------------------------------------------%
%
% Stuff for the `termination2_info' pragma
%

    % This is the form in which termination information from other
    % modules (imported via `.opt' or `.trans_opt' files) comes.
    % We convert this to an intermediate form and let the termination
    % analyser convert it to the correct form.
    %
    % NOTE: the reason that we cannot convert it to the correct form
    % is that we don't have complete information about how many typeinfo
    % related arguments there are until after the polymoprhism pass.
    %
:- type arg_size_constr
    --->    le(list(arg_size_term), rat)
    ;       eq(list(arg_size_term), rat).

:- type arg_size_term == pair(int, rat).

:- type pragma_constr_arg_size_info == list(arg_size_constr).

%-----------------------------------------------------------------------------%
%
% Stuff for the `structure_sharing_info' pragma.
%
    % Whenever structure sharing analysis is unable to determine a good
    % approximation of the set of structure sharing pairs that might exist
    % during the execution of a program, it must use "top" as the only safe
    % approximation. In order to collect some useful basic feedback information
    % as to `why' a top was generated, we use:
    %
:- type top_feedback == string.

    % Elements of the structure sharing domain lattice are either bottom
    % (no structure sharing), top (any kind of structure sharing), or
    % a list of structure sharing pairs.
    %
    % This is the public representation of the type "sharing_as".
    %
:- type structure_sharing_domain
    --->    structure_sharing_bottom
    ;       structure_sharing_real(structure_sharing)
    ;       structure_sharing_top(list(top_feedback)).

    % Public representation of structure sharing.
    %
:- type structure_sharing == list(structure_sharing_pair).

    % A structure sharing pair represents the information that two
    % data structures might be represented by the same memoryspace, hence
    % its representation as a pair of datastructs.
    %
:- type structure_sharing_pair == pair(datastruct).

    % A datastructure is a concept that designates a particular subterm of the
    % term to which a particular variable may be bound.
    %
:- type datastruct
    --->    selected_cel(
                sc_var      :: prog_var,
                sc_selector :: selector
            ).

    % A selector describes a path in a type-tree.
    %
:- type selector == list(unit_selector).

    % Unit-selectors are either term selectors or type selectors.  A term
    % selector selects a subterm f/n of a term, where f is a functor
    % (identified by the cons_id), and n an integer.  A type selector
    % designates any subterm that has that specific type.
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

%-----------------------------------------------------------------------------%
%
% Stuff for the `structure_reuse_info' pragma.
%

:- type dead_var == prog_var.
:- type dead_vars == list(dead_var).
:- type dead_datastruct == datastruct.
:- type dead_datastructs == list(dead_datastruct).
:- type live_var == prog_var.
:- type live_vars == list(live_var).
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
    %   might become dead, hence reuseable;
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

%-----------------------------------------------------------------------------%
%
% Stuff for the `unused_args' pragma
%

    % This `mode_num' type is only used for mode numbers written out in
    % automatically-generated `pragma unused_args' pragmas in `.opt' files.
    % The mode_num gets converted to an HLDS proc_id by make_hlds.m.
    % We don't want to use the `proc_id' type here since the parse tree
    % (prog_data.m and prog_item.m) should not depend on the HLDS.
    %
:- type mode_num == int.

%-----------------------------------------------------------------------------%
%
% Stuff for the `exceptions' pragma
%

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

%-----------------------------------------------------------------------------%
%
% Stuff for the trailing analysis
%

:- type trailing_status
    --->    trail_may_modify
    ;       trail_will_not_modify
    ;       trail_conditional.

%-----------------------------------------------------------------------------%
%
% Stuff for minimal model tabling analysis
%

:- type mm_tabling_status
    --->    mm_tabled_may_call
    ;       mm_tabled_will_not_call
    ;       mm_tabled_conditional.

%-----------------------------------------------------------------------------%
%
% Stuff for the `type_spec' pragma
%

    % The type substitution for a `pragma type_spec' declaration.
    % Elsewhere in the compiler we generally use the `tsubst' type
    % which is a map rather than an assoc_list.
    %
:- type type_subst == assoc_list(tvar, mer_type).

%-----------------------------------------------------------------------------%
%
% Stuff for `foreign_code' pragma
%

    % This type holds information about the implementation details
    % of procedures defined via `pragma foreign_code'.
    %
    % All the strings in this type may be accompanied by the context of their
    % appearance in the source code. These contexts are used to tell the
    % foreign language compiler where the included code comes from, to allow it
    % to generate error messages that refer to the original appearance of the
    % code in the Mercury program. The context is missing if the foreign code
    % was constructed by the compiler.
    %
    % NOTE: nondet pragma foreign definitions might not be possible in all
    % foreign languages.
    %
:- type pragma_foreign_code_impl
    --->    fc_impl_ordinary(
                % This is a foreign language definition of a model_det or
                % model_semi procedure. (We also allow model_non, until
                % everyone has had time to adapt to the new way of handling
                % model_non pragmas.)

                string,             % The code of the procedure.
                maybe(prog_context)
            )

    ;       fc_impl_model_non(
                % This is a foreign language definition of a model_non
                % procedure.

                string,
                maybe(prog_context),
                    % The info saved for the time when backtracking reenters
                    % this procedure is stored in a data structure.  This arg
                    % contains the field declarations.

                string,
                maybe(prog_context),
                    % Gives the code to be executed when the procedure is
                    % called for the first time. This code may access the
                    % input variables.

                string,
                maybe(prog_context),
                    % Gives the code to be executed when control backtracks
                    % into the procedure.  This code may not access the input
                    % variables.

                foreign_proc_shared_code_treatment,
                    % How should the shared code be treated during code
                    % generation.

                string,
                maybe(prog_context)
                    % Shared code that is executed after both the previous
                    % code fragments.  May not access the input variables.
            )

    ;       fc_impl_import(
                string,     % Pragma imported C func name
                string,     % Code to handle return value
                string,     % Comma separated variables which the import
                            % function is called with.
                maybe(prog_context)
            ).

    % The use of this type is explained in the comment at the top of
    % pragma_c_gen.m.
    %
:- type foreign_proc_shared_code_treatment
    --->    shared_code_duplicate
    ;       shared_code_share
    ;       shared_code_automatic.

:- type foreign_import_module_info_list  == list(foreign_import_module_info).
                    % in reverse order

:- type foreign_import_module_info
    --->    foreign_import_module_info(
                foreign_language,
                module_name,
                prog_context
            ).

%-----------------------------------------------------------------------------%
%
% Type classes
%

    % A class constraint represents a constraint that a given list of types
    % is a member of the specified type class. It is an invariant of this data
    % structure that the types in a class constraint do not contain any
    % information in their prog_context fields. This invariant is needed
    % to ensure that we can do unifications, map.lookups, etc., and get the
    % expected semantics. (This invariant now applies to all types, but is
    % especially important here.)
    %
:- type prog_constraint
    --->    constraint(
                class_name,
                list(mer_type)
            ).

:- type prog_constraints
    --->    constraints(
                univ_constraints    :: list(prog_constraint),
                                    % universally quantified
                                    % constraints
                exist_constraints   :: list(prog_constraint)
                                    % existentially quantified
                                    % constraints
            ).

    % A functional dependency on the variables in the head of a class
    % declaration. This asserts that, given the complete set of instances
    % of this class, the binding of the range variables can be uniquely
    % determined from the binding of the domain variables.
    %
:- type prog_fundep
    --->    fundep(
                domain          :: list(tvar),
                range           :: list(tvar)
            ).

:- type class_name == sym_name.
:- type class_id
    --->    class_id(class_name, arity).

:- type class_interface
    --->    class_interface_abstract
    ;       class_interface_concrete(class_methods).

:- type instance_method
    --->    instance_method(
                instance_method_p_or_f          :: pred_or_func,
                instance_method_name            :: sym_name,
                instance_method_proc_def        :: instance_proc_def,
                instance_method_arity           :: arity,

                % The context of the instance declaration.
                instance_method_decl_context    :: prog_context
            ).

:- type instance_proc_def
    --->    instance_proc_def_name(
                % defined using the `pred(...) is <Name>' syntax
                sym_name
            )

    ;       instance_proc_def_clauses(
                % defined using clauses
                list(item)          % the items must be either
                                    % pred_clause or func_clause items
            ).

:- type instance_body
    --->    instance_body_abstract
    ;       instance_body_concrete(instance_methods).

:- type instance_methods == list(instance_method).

%-----------------------------------------------------------------------------%
%
% Some more stuff for the foreign language interface
%

    % An abstract type for representing a set of
    % `pragma_foreign_proc_attribute's.
    %
:- type pragma_foreign_proc_attributes.

:- func default_attributes(foreign_language) = pragma_foreign_proc_attributes.
:- func get_may_call_mercury(pragma_foreign_proc_attributes) =
    proc_may_call_mercury.
:- func get_thread_safe(pragma_foreign_proc_attributes) = proc_thread_safe.
:- func get_purity(pragma_foreign_proc_attributes) = purity.
:- func get_terminates(pragma_foreign_proc_attributes) = proc_terminates.
:- func get_user_annotated_sharing(pragma_foreign_proc_attributes) =
    user_annotated_sharing.
:- func get_foreign_language(pragma_foreign_proc_attributes) =
    foreign_language.
:- func get_tabled_for_io(pragma_foreign_proc_attributes) =
    proc_tabled_for_io.
:- func get_legacy_purity_behaviour(pragma_foreign_proc_attributes) = bool.
:- func get_may_throw_exception(pragma_foreign_proc_attributes) =
    proc_may_throw_exception.
:- func get_ordinary_despite_detism(pragma_foreign_proc_attributes) = bool.
:- func get_may_modify_trail(pragma_foreign_proc_attributes) =
    proc_may_modify_trail.
:- func get_may_call_mm_tabled(pragma_foreign_proc_attributes) =
    may_call_mm_tabled.
:- func get_box_policy(pragma_foreign_proc_attributes) = box_policy.
:- func get_affects_liveness(pragma_foreign_proc_attributes) =
    proc_affects_liveness.
:- func get_allocates_memory(pragma_foreign_proc_attributes) =
    proc_allocates_memory.
:- func get_registers_roots(pragma_foreign_proc_attributes) =
    proc_registers_roots.
:- func get_may_duplicate(pragma_foreign_proc_attributes) =
    maybe(proc_may_duplicate).
:- func get_extra_attributes(pragma_foreign_proc_attributes)
    = pragma_foreign_proc_extra_attributes.

:- pred set_may_call_mercury(proc_may_call_mercury::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_thread_safe(proc_thread_safe::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_foreign_language(foreign_language::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_tabled_for_io(proc_tabled_for_io::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_purity(purity::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_terminates(proc_terminates::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_user_annotated_sharing(user_annotated_sharing::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_may_throw_exception(proc_may_throw_exception::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_legacy_purity_behaviour(bool::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_ordinary_despite_detism(bool::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_may_modify_trail(proc_may_modify_trail::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_may_call_mm_tabled(may_call_mm_tabled::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_box_policy(box_policy::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_affects_liveness(proc_affects_liveness::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_allocates_memory(proc_allocates_memory::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_registers_roots(proc_registers_roots::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred set_may_duplicate(maybe(proc_may_duplicate)::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

:- pred add_extra_attribute(pragma_foreign_proc_extra_attribute::in,
    pragma_foreign_proc_attributes::in,
    pragma_foreign_proc_attributes::out) is det.

    % For foreign_procs, there are two different calling conventions,
    % one for foreign code that may recursively call Mercury code, and another
    % more efficient one for the case when we know that the foreign code will
    % not recursively invoke Mercury code.
:- type proc_may_call_mercury
    --->    proc_may_call_mercury
    ;       proc_will_not_call_mercury.

    % If thread_safe execution is enabled, then we need to put a mutex
    % around the foreign code for each foreign_proc, unless it is declared
    % to be thread_safe.  If a piece of foreign code is declared to be
    % maybe_thread_safe whether we put the mutex around the foreign code
    % depends upon the `--maybe-thread-safe' compiler flag.
    %
:- type proc_thread_safe
    --->    proc_not_thread_safe
    ;       proc_thread_safe
    ;       proc_maybe_thread_safe.

:- type proc_tabled_for_io
    --->    proc_not_tabled_for_io
    ;       proc_tabled_for_io
    ;       proc_tabled_for_io_unitize
    ;       proc_tabled_for_descendant_io.

:- type proc_may_modify_trail
    --->    proc_may_modify_trail
    ;       proc_will_not_modify_trail.

:- type may_call_mm_tabled
    --->    may_call_mm_tabled
            % The foreign code may make callbacks to minimal model tabled
            % procedures.

    ;       will_not_call_mm_tabled
            % The foreign code may make callbacks to Mercury, but they will
            % not be to minimal model tabled code.

    ;       default_calls_mm_tabled.
            % If either of the above are not specified:
            %   - for `will_not_call_mercury' set `will_not_call_mm_tabled'
            %   - for `may_call_mercury' set `may_call_mm_tabled'

:- type pragma_var
    --->    pragma_var(prog_var, string, mer_mode, box_policy).
            % variable, name, mode
            % We explicitly store the name because we need the real
            % name in code_gen.

:- type box_policy
    --->    native_if_possible
    ;       always_boxed.

:- type proc_affects_liveness
    --->    proc_affects_liveness
    ;       proc_does_not_affect_liveness
    ;       proc_default_affects_liveness.

:- type proc_allocates_memory
    --->    proc_does_not_allocate_memory
    ;       proc_allocates_bounded_memory
    ;       proc_allocates_unbounded_memory
    ;       proc_default_allocates_memory.

:- type proc_registers_roots
    --->    proc_registers_roots
    ;       proc_does_not_register_roots
    ;       proc_does_not_have_roots
    ;       proc_default_registers_roots.

:- type proc_may_duplicate
    --->    proc_may_duplicate
    ;       proc_may_not_duplicate.

    % This type specifies the termination property of a procedure
    % defined using pragma c_code or pragma foreign_proc.
    %
:- type proc_terminates
    --->    proc_terminates
            % The foreign code will terminate for all input assuming
            % that any input streams are finite.

    ;       proc_does_not_terminate
            % The foreign code will not necessarily terminate for some
            % (possibly all) input.

    ;       depends_on_mercury_calls.
            % The termination of the foreign code depends on whether the code
            % makes calls back to Mercury (See termination.m for details).

:- type proc_may_throw_exception
    --->    proc_will_not_throw_exception
            % The foreign code will not result in an exception being thrown.

    ;       default_exception_behaviour.
            % If the foreign_proc is erroneous then mark it as throwing an
            % exception.  Otherwise mark it as throwing an exception if it
            % makes calls back to Mercury and not throwing an exception
            % otherwise.

:- type pragma_foreign_proc_extra_attribute
    --->    max_stack_size(int)
    ;       refers_to_llds_stack
    ;       backend(backend).

:- type pragma_foreign_proc_extra_attributes ==
    list(pragma_foreign_proc_extra_attribute).

    % Convert the foreign code attributes to their source code representations
    % suitable for placing in the attributes list of the pragma (not all
    % attributes have one). In particular, the foreign language attribute needs
    % to be handled separately as it belongs at the start of the pragma.
    %
:- func attributes_to_strings(pragma_foreign_proc_attributes) = list(string).

%-----------------------------------------------------------------------------%
%
% Goals
%

% NOTE: the representation of goals in the parse tree is defined in
%       prog_item.m.

:- type implicit_purity_promise
    --->    make_implicit_promises
    ;       dont_make_implicit_promises.

:- type trace_expr(Base)
    --->    trace_base(Base)
    ;       trace_not(trace_expr(Base))
    ;       trace_op(trace_op, trace_expr(Base), trace_expr(Base)).

:- type trace_op
    --->    trace_or
    ;       trace_and.

:- type trace_compiletime
    --->    trace_flag(string)
    ;       trace_grade(trace_grade)
    ;       trace_trace_level(trace_trace_level).

:- type trace_grade
    --->    trace_grade_debug.

:- type trace_trace_level
    --->    trace_level_shallow
    ;       trace_level_deep.

:- type trace_runtime
    --->    trace_envvar(string).

:- type trace_mutable_var
    --->    trace_mutable_var(
                trace_mutable_name      :: string,
                trace_state_var         :: prog_var
            ).

    % These type equivalences are for the type of program variables
    % and associated structures.
    %
:- type prog_var_type
    --->    prog_var_type.
:- type prog_var    ==  var(prog_var_type).
:- type prog_varset ==  varset(prog_var_type).
:- type prog_substitution ==    substitution(prog_var_type).
:- type prog_term   ==  term(prog_var_type).
:- type prog_vars   ==  list(prog_var).

    % A prog_context is just a term.context.
    %
:- type prog_context    ==  term.context.

%-----------------------------------------------------------------------------%
%
% Cons ids
%

    % The representation of cons_ids below is a compromise. The cons_id
    % type must be defined here, in a submodule of parse_tree.m, because
    % it is a component of insts. However, after the program has been read
    % in, the cons_ids cons, int_const, string_const and float_const,
    % which can appear in user programs, may also be augmented by the other
    % cons_ids, which can only be generated by the compiler.
    %
    % The problem is that some of these compiler generated cons_ids
    % refer to procedures, and the natural method of identifying
    % procedures requires the types pred_id and proc_id, defined
    % in hlds_pred.m, which we don't want to import here.
    %
    % We could try to avoid this problem using two different types
    % for cons_ids, one defined here for use in the parse tree and one
    % defined in hlds_data.m for use in the HLDS. We could distinguish
    % the two by having the HLDS cons_id have a definition such as
    % hlds_cons_id ---> parse_cons_id(parse_cons_id) ; ...
    % or, alternatively, by making cons_id parametric in the type of
    % constants, and substitute different constant types (since all the
    % cons_ids that refer to HLDS concepts are constants).
    %
    % Using two different types requires a translation from one to the
    % other. While the runtime cost would be acceptable, the cost in code
    % complexity isn't, since the translation isn't confined to
    % make_hlds.m. (I found this out the hard way.) This is especially so
    % if we want to use in each case only the tightest possible type.
    % For example, while construct goals can involve all cons_ids,
    % deconstruct goals and switches can currently involve only the
    % cons_ids that can appear in parse trees.
    %
    % The solution we have chosen is to exploit the fact that pred_ids
    % and proc_ids are integers. Those types are private to hlds_pred.m,
    % but hlds_pred.m also contains functions for translating them to and
    % from the shrouded versions defined below. The next three types are
    % designed to be used in only two ways: for translation to their HLDS
    % equivalents by the unshroud functions in hlds_pred.m, and for
    % printing for diagnostics.
    %
:- type shrouded_pred_id    ---> shrouded_pred_id(int).
:- type shrouded_proc_id    ---> shrouded_proc_id(int).
:- type shrouded_pred_proc_id   ---> shrouded_pred_proc_id(int, int).

:- type cons_id
    --->    cons(sym_name, arity)   % name, arity
            % Tuples have cons_id `cons(unqualified("{}"), Arity)'.

    ;       int_const(int)
    ;       string_const(string)
    ;       float_const(float)

    ;       pred_const(shrouded_pred_proc_id, lambda_eval_method)
            % Note that a pred_const represents a closure,
            % not just a code address.

    ;       type_ctor_info_const(
                module_name,
                string,         % Name of the type constructor.
                int             % Its arity.
            )
    ;       base_typeclass_info_const(
                module_name,    % Module name of instance declaration
                                % (not filled in so that link errors result
                                % from overlapping instances).
                class_id,       % Class name and arity.
                int,            % Class instance.
                string          % Encodes the type names and arities of the
                                % arguments of the instance declaration.
            )

    ;       type_info_cell_constructor(type_ctor)
    ;       typeclass_info_cell_constructor

    ;       tabling_info_const(shrouded_pred_proc_id)
            % The address of the static structure that holds information
            % about the table that implements memoization, loop checking
            % or the minimal model semantics for the given procedure.

    ;       deep_profiling_proc_layout(shrouded_pred_proc_id)
            % The Proc_Layout structure of a procedure. Its proc_static field
            % is used by deep profiling, as documented in the deep profiling
            % paper.

    ;       table_io_decl(shrouded_pred_proc_id).
            % The address of a structure that describes the layout of the
            % answer block used by I/O tabling for declarative debugging.

    % Describe how a lambda expression is to be evaluated.
    %
    % `normal' is the top-down Mercury execution algorithm.
    %
:- type lambda_eval_method
    --->    lambda_normal.

%-----------------------------------------------------------------------------%
%
% Types
%

% This is how types are represented.

% One day we might allow types to take
% value parameters as well as type parameters.

% type_defn/3 is defined in prog_item.m as a constructor for item/0

:- type type_defn
    --->    parse_tree_du_type(
                du_ctors            :: list(constructor),
                du_user_uc          :: maybe(unify_compare)
            )
    ;       parse_tree_eqv_type(
                eqv_type            :: mer_type
            )
    ;       parse_tree_abstract_type(
                abstract_is_solver  :: is_solver_type
            )
    ;       parse_tree_solver_type(
                solver_details      :: solver_type_details,
                solver_user_uc      :: maybe(unify_compare)
            )
    ;       parse_tree_foreign_type(
                foreign_lang_type   :: foreign_language_type,
                foreign_user_uc     :: maybe(unify_compare),
                foreign_assertions  :: list(foreign_type_assertion)
            ).

    % The `is_solver_type' type specifies whether a type is a "solver" type,
    % for which `any' insts are interpreted as "don't know", or a non-solver
    % type for which `any' is the same as `bound(...)'.
    %
:- type is_solver_type
    --->    non_solver_type
            % The inst `any' is always `bound' for this type.

    ;       solver_type.
            % The inst `any' is not always `bound' for this type
            % (i.e. the type was declared with
            % `:- solver type ...').

:- type foreign_type_assertion
    --->    foreign_type_can_pass_as_mercury_type
    ;       foreign_type_stable.

:- type constructor
    --->    ctor(
                cons_exist          :: existq_tvars,
                cons_constraints    :: list(prog_constraint),
                                    % existential constraints
                cons_name           :: sym_name,
                cons_args           :: list(constructor_arg),
                cons_context        :: prog_context
            ).

:- type constructor_arg
    --->    ctor_arg(
                arg_field_name      :: maybe(ctor_field_name),
                arg_type            :: mer_type,
                arg_context         :: prog_context
            ).

:- type ctor_field_name == sym_name.

    % unify_compare gives the user-defined unification and/or comparison
    % predicates for a noncanonical type, if they are known.  The value
    % `abstract_noncanonical_type' represents a type whose definition uses
    % the syntax `where type_is_abstract_noncanonical' and has been read
    % from a .int2 file.  This means we know that the type has a
    % noncanonical representation, but we don't know what the
    % unification/comparison predicates are.
    %
:- type unify_compare
    --->    unify_compare(
                unify               :: maybe(equality_pred),
                compare             :: maybe(comparison_pred)
            )
    ;       abstract_noncanonical_type(is_solver_type).

    % The `where' attributes of a solver type definition must begin
    % with
    %   representation is <<representation type>>,
    %   initialisation is <<init pred name>>,
    %   ground         is <<ground inst>>,
    %   any            is <<any inst>>,
    %   constraint_store is <<mutable(...) or [mutable(...), ...]>>
    %
:- type solver_type_details
    --->    solver_type_details(
                representation_type :: mer_type,
                init_pred           :: init_pred,
                ground_inst         :: mer_inst,
                any_inst            :: mer_inst,
                mutable_items       :: list(item)
            ).

    % An init_pred specifies the name of an impure user-defined predicate
    % used to initialise solver type values (the compiler will insert
    % calls to this predicate to convert free solver type variables to
    % inst any variables where necessary.)
    %
:- type init_pred   ==  sym_name.

    % An equality_pred specifies the name of a user-defined predicate
    % used for equality on a type.  See the chapter on them in the
    % Mercury Language Reference Manual.
    %
:- type equality_pred   ==  sym_name.

    % The name of a user-defined comparison predicate.
    %
:- type comparison_pred ==  sym_name.

    % Parameters of type definitions.
    %
:- type type_param  ==  tvar.

    % Use type_util.type_to_ctor_and_args to convert a type to a qualified
    % type_ctor and a list of arguments.  Use type_util.construct_type to
    % construct a type from a type_ctor and a list of arguments.
    %
:- type mer_type
    --->    type_variable(tvar, kind)
            % A type variable.

    ;       defined_type(sym_name, list(mer_type), kind)
            % A type using a user defined type constructor.

    ;       builtin_type(builtin_type)
            % These are all known to have kind `star'.

    % The above three functors should be kept as the first three, since
    % they will be the most commonly used and therefore we want them to
    % get the primary tags on a 32-bit machine.

    ;       higher_order_type(
                % A type for higher-order values. If the second argument
                % is yes(T) then the values are functions returning T,
                % otherwise they are predicates. The kind is always `star'.

                list(mer_type),
                maybe(mer_type),
                purity,
                lambda_eval_method
            )

    ;       tuple_type(list(mer_type), kind)
            % Tuple types.

    ;       apply_n_type(tvar, list(mer_type), kind)
            % An apply/N expression.  `apply_n(V, [T1, ...], K)'
            % would be the representation of type `V(T1, ...)'
            % with kind K.  The list must be non-empty.

    ;       kinded_type(mer_type, kind).
            % A type expression with an explicit kind annotation.
            % (These are not yet used.)

:- type vartypes == map(prog_var, mer_type).

:- type builtin_type
    --->    builtin_type_int
    ;       builtin_type_float
    ;       builtin_type_string
    ;       builtin_type_character.

:- type type_term   ==  term(tvar_type).

:- type tvar_type
    --->    type_var.

:- type tvar        ==  var(tvar_type).
                    % used for type variables
:- type tvarset     ==  varset(tvar_type).
                    % used for sets of type variables
:- type tsubst      ==  map(tvar, mer_type). % used for type substitutions
:- type tvar_renaming   ==  map(tvar, tvar). % type renaming

:- type type_ctor
    --->    type_ctor(sym_name, arity).

:- type tvar_name_map   ==  map(string, tvar).

    % existq_tvars is used to record the set of type variables which are
    % existentially quantified
    %
:- type existq_tvars    ==  list(tvar).

    % Types may have arbitrary assertions associated with them
    % (e.g. you can define a type which represents sorted lists).
    % Similarly, pred declarations can have assertions attached.
    % The compiler will ignore these assertions - they are intended
    % to be used by other tools, such as the debugger.
    %
:- type condition
    --->    cond_true
    ;       cond_where(term).

    % Similar to varset.merge_subst but produces a tvar_renaming
    % instead of a substitution, which is more suitable for types.
    %
:- pred tvarset_merge_renaming(tvarset::in, tvarset::in, tvarset::out,
    tvar_renaming::out) is det.

    % As above, but behaves like varset.merge_subst_without_names.
    %
:- pred tvarset_merge_renaming_without_names(tvarset::in, tvarset::in,
    tvarset::out, tvar_renaming::out) is det.

%-----------------------------------------------------------------------------%
%
% Kinds
%

    % Note that we don't support any kind other than `star' at the
    % moment.  The other kinds are intended for the implementation
    % of constructor classes.
    %
:- type kind
    --->    kind_star
            % An ordinary type.

    ;       kind_arrow(kind, kind)
            % A type with kind `A' applied to a type with kind `arrow(A, B)'
            % will have kind `B'.

    ;       kind_variable(kvar).
            % A kind variable. These can be used during kind inference;
            % after kind inference, all remaining kind variables will be
            % bound to `star'.

:- type kvar_type
    --->    kind_var.
:- type kvar        ==  var(kvar_type).

    % The kinds of type variables. For efficiency, we only have entries
    % for type variables that have a kind other than `star'. Any type variable
    % not appearing in this map, which will usually be the majority of type
    % variables, can be assumed to have kind `star'.
    %
:- type tvar_kind_map   ==  map(tvar, kind).

:- pred get_tvar_kind(tvar_kind_map::in, tvar::in, kind::out) is det.

    % Return the kind of a type.
    %
:- func get_type_kind(mer_type) = kind.

%-----------------------------------------------------------------------------%
%
% Insts and modes
%

    % This is how instantiatednesses and modes are represented.
    %
:- type mer_inst
    --->        any(uniqueness)
    ;           free
    ;           free(mer_type)

    ;           bound(uniqueness, list(bound_inst))
                % The list(bound_inst) must be sorted.

    ;           ground(uniqueness, ground_inst_info)
                % The ground_inst_info holds extra information
                % about the ground inst.

    ;           not_reached
    ;           inst_var(inst_var)

    ;           constrained_inst_vars(set(inst_var), mer_inst)
                % Constrained_inst_vars is a set of inst variables that are
                % constrained to have the same uniqueness as and to match_final
                % the specified inst.

    ;           defined_inst(inst_name)
                % A defined_inst is possibly recursive inst whose value is
                % stored in the inst_table. This is used both for user-defined
                % insts and for compiler-generated insts.

    ;           abstract_inst(sym_name, list(mer_inst)).
                % An abstract inst is a defined inst which
                % has been declared but not actually been
                % defined (yet).

:- type uniqueness
    --->        shared              % There might be other references.
    ;           unique              % There is only one reference.
    ;           mostly_unique       % There is only one reference,
                                    % but there might be more on backtracking.
    ;           clobbered           % This was the only reference, but
                                    % the data has already been reused.
    ;           mostly_clobbered.   % This was the only reference, but
                                    % the data has already been reused;
                                    % however, there may be more references
                                    % on backtracking, so we will need to
                                    % restore the old value on backtracking.

    % The ground_inst_info type gives extra information about ground insts.
    %
:- type ground_inst_info
    --->    higher_order(pred_inst_info)
            % The ground inst is higher-order.
    ;       none.
            % No extra information is available.

    % higher-order predicate terms are given the inst
    %   `ground(shared, higher_order(PredInstInfo))'
    % where the PredInstInfo contains the extra modes and the determinism
    % for the predicate.  Note that the higher-order predicate term
    % itself must be ground.
    %
:- type pred_inst_info
    --->    pred_inst_info(
                pred_or_func,       % Is this a higher-order func mode or a
                                    % higher-order pred mode?

                list(mer_mode),     % The modes of the additional (i.e.
                                    % not-yet-supplied) arguments of the pred;
                                    % for a function, this includes the mode
                                    % of the return value as the last element
                                    % of the list.

                determinism         % The determinism of the predicate or
                                    % function.
            ).

:- type inst_id
    --->    inst_id(sym_name, arity).

:- type bound_inst
    --->    bound_functor(cons_id, list(mer_inst)).

:- type inst_var_type
    --->    inst_var_type.

:- type inst_var    ==  var(inst_var_type).
:- type inst_term   ==  term(inst_var_type).
:- type inst_varset ==  varset(inst_var_type).

:- type inst_var_sub    ==  map(inst_var, mer_inst).

% inst_defn/5 is defined in prog_item.m.

:- type inst_defn
    --->    eqv_inst(mer_inst)
    ;       abstract_inst.

    % An `inst_name' is used as a key for the inst_table.
    % It is either a user-defined inst `user_inst(Name, Args)',
    % or some sort of compiler-generated inst, whose name
    % is a representation of its meaning.
    %
    % For example, `merge_inst(InstA, InstB)' is the name used for the
    % inst that results from merging InstA and InstB using `merge_inst'.
    % Similarly `unify_inst(IsLive, InstA, InstB, IsReal)' is
    % the name for the inst that results from a call to
    % `abstractly_unify_inst(IsLive, InstA, InstB, IsReal)'.
    % And `ground_inst' and `any_inst' are insts that result
    % from unifying an inst with `ground' or `any', respectively.
    % `typed_inst' is an inst with added type information.
    % `typed_ground(Uniq, Type)' a equivalent to
    % `typed_inst(ground(Uniq, no), Type)'.
    % Note that `typed_ground' is a special case of `typed_inst',
    % and `ground_inst' and `any_inst' are special cases of `unify_inst'.
    % The reason for having the special cases is efficiency.
    %
:- type inst_name
    --->    user_inst(sym_name, list(mer_inst))
    ;       merge_inst(mer_inst, mer_inst)
    ;       unify_inst(is_live, mer_inst, mer_inst, unify_is_real)
    ;       ground_inst(inst_name, is_live, uniqueness, unify_is_real)
    ;       any_inst(inst_name, is_live, uniqueness, unify_is_real)
    ;       shared_inst(inst_name)
    ;       mostly_uniq_inst(inst_name)
    ;       typed_ground(uniqueness, mer_type)
    ;       typed_inst(mer_type, inst_name).

    % NOTE: `is_live' records liveness in the sense used by
    % mode analysis.  This is not the same thing as the notion of liveness
    % used by code generation.  See compiler/notes/glossary.html.
    %
:- type is_live
    --->    is_live
    ;       is_dead.

    % Unifications of insts fall into two categories, "real" and "fake".
    % The "real" inst unifications correspond to real unifications,
    % and are not allowed to unify with `clobbered' insts (unless
    % the unification would be `det').
    % Any inst unification which is associated with some code that
    % will actually examine the contents of the variables in question
    % must be "real".  Inst unifications that are not associated with
    % some real code that examines the variables' values are "fake".
    % "Fake" inst unifications are used for procedure calls in implied
    % modes, where the final inst of the var must be computed by
    % unifying its initial inst with the procedure's final inst,
    % so that if you pass a ground var to a procedure whose mode
    % is `free -> list_skeleton', the result is ground, not list_skeleton.
    % But these fake unifications must be allowed to unify with `clobbered'
    % insts. Hence we pass down a flag to `abstractly_unify_inst' which
    % specifies whether or not to allow unifications with clobbered values.
    %
:- type unify_is_real
    --->    real_unify
    ;       fake_unify.

:- type mode_id
    --->    mode_id(sym_name, arity).

:- type mode_defn
    --->    eqv_mode(mer_mode).

:- type mer_mode
    --->    (mer_inst -> mer_inst)
    ;       user_defined_mode(sym_name, list(mer_inst)).

%-----------------------------------------------------------------------------%
%
% Module system
%

:- type backend
    --->    high_level_backend
    ;       low_level_backend.

:- type section
    --->    section_implementation
    ;       section_interface.

    % An import_locn is used to describe the place where an item was
    % imported from.
:- type import_locn
    --->    import_locn_implementation
            % The item is from a module imported in the implementation.

    ;       import_locn_interface
            % The item is from a module imported in the interface.

    ;       import_locn_ancestor
            % The item is from a module imported by an ancestor.

    ;       import_locn_ancestor_private_interface.
            % The item is from the private interface of an ancestor module.

:- type sym_list
    --->    list_sym(list(sym_specifier))
    ;       list_pred(list(pred_specifier))
    ;       list_func(list(func_specifier))
    ;       list_cons(list(cons_specifier))
    ;       list_op(list(op_specifier))
    ;       list_adt(list(adt_specifier))
    ;       list_type(list(type_specifier))
    ;       list_module(list(module_specifier)).

:- type sym_specifier
    --->    spec_sym(sym_name_specifier)
    ;       spec_typed_sym(typed_cons_specifier)
    ;       spec_pred(pred_specifier)
    ;       spec_func(func_specifier)
    ;       spec_cons(cons_specifier)
    ;       spec_op(op_specifier)
    ;       spec_adt(adt_specifier)
    ;       spec_type(type_specifier)
    ;       spec_module(module_specifier).

:- type pred_specifier
    --->    predspec_sym(sym_name_specifier)
    ;       predspec_name_args(sym_name, list(mer_type)).

:- type func_specifier  ==  cons_specifier.
:- type cons_specifier
    --->    consspec_sym(sym_name_specifier)
    ;       consspec_typed(typed_cons_specifier).

:- type typed_cons_specifier
    --->    name_args(sym_name, list(mer_type))
    ;       name_res(sym_name_specifier, mer_type)
    ;       name_args_res(sym_name, list(mer_type), mer_type).

:- type adt_specifier   ==  sym_name_specifier.
:- type type_specifier  ==  sym_name_specifier.

:- type op_specifier
    --->    opspec_sym(sym_name_specifier)
    ;       opspec_fixity(sym_name_specifier, fixity).
            % operator fixity specifiers not yet implemented

:- type fixity
    --->    infix
    ;       prefix
    ;       postfix
    ;       binary_prefix
    ;       binary_postfix.

:- type sym_name_specifier
    --->    name(sym_name)
    ;       name_arity(sym_name, arity).

:- type sym_name_and_arity
    --->    sym_name / arity.

:- type simple_call_id
    --->    simple_call_id(pred_or_func, sym_name, arity).

:- type module_specifier == sym_name.
:- type arity       ==  int.

    % Describes whether an item can be used without an explicit module
    % qualifier.
    %
:- type need_qualifier
    --->    must_be_qualified
    ;       may_be_unqualified.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type item_visibility
    --->    visibility_public
    ;       visibility_private.

:- type used_modules
    --->    used_modules(
                    % The modules used in the interface and implementation.
                int_used_modules    :: set(module_name),
                impl_used_modules   :: set(module_name)
            ).

    %
    % Initialize the used_modules structure.
    %
:- func used_modules_init = used_modules.

    %
    % Given a sym_name call add_all_modules on the module part of the name.
    %
:- pred add_sym_name_module(item_visibility::in, sym_name::in,
    used_modules::in, used_modules::out) is det.

    %
    % Given a module name add the module and all of its parent modules
    % to the used_modules.
    %
:- pred add_all_modules(item_visibility::in, sym_name::in,
    used_modules::in, used_modules::out) is det.

%-----------------------------------------------------------------------------%
%
% Event specifications
%

:- type event_attribute
    --->    event_attribute(
                attr_num                    :: int,
                attr_name                   :: string,
                attr_type                   :: mer_type,
                attr_mode                   :: mer_mode,
                attr_maybe_synth_call       :: maybe(event_attr_synth_call)
            ).

:- type event_attr_synth_call
    --->    event_attr_synth_call(
                synth_func_attr_name_num    :: pair(string, int),
                synth_arg_attr_name_nums    :: assoc_list(string, int),
                synth_eval_order            :: list(int)
            ).

:- type event_spec
    --->    event_spec(
                event_spec_num              :: int,
                event_spec_name             :: string,
                event_spec_linenum          :: int,
                event_spec_attrs            :: list(event_attribute),
                event_spec_synth_order      :: list(int)
            ).

    % This type maps the name of an event to the event's specification.
:- type event_spec_map == map(string, event_spec).

:- type event_set
    --->    event_set(
                event_set_name              :: string,
                event_set_spec_map          :: event_spec_map
            ).

:- type event_set_data
    --->    event_set_data(
                event_set_data_name         :: string,
                event_set_data_description  :: string,
                event_set_data_specs        :: list(event_spec),
                event_set_data_max_num_attr :: int
            ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.

:- import_module string.

eval_method_to_table_type(EvalMethod) = TableTypeStr :-
    (
        EvalMethod = eval_normal,
        unexpected(this_file, "eval_method_to_table_type: eval_normal")
    ;
        EvalMethod = eval_table_io(_, _),
        unexpected(this_file, "eval_method_to_table_type: eval_table_io")
    ;
        EvalMethod = eval_loop_check,
        TableTypeStr = "MR_TABLE_TYPE_LOOPCHECK"
    ;
        EvalMethod = eval_memo,
        TableTypeStr = "MR_TABLE_TYPE_MEMO"
    ;
        EvalMethod = eval_minimal(stack_copy),
        TableTypeStr = "MR_TABLE_TYPE_MINIMAL_MODEL_STACK_COPY"
    ;
        EvalMethod = eval_minimal(own_stacks_consumer),
        unexpected(this_file, "eval_method_to_table_type: own_stacks_consumer")
    ;
        EvalMethod = eval_minimal(own_stacks_generator),
        TableTypeStr = "MR_TABLE_TYPE_MINIMAL_MODEL_OWN_STACKS"
    ).

default_memo_table_attributes =
    table_attributes(all_strict, no, table_dont_gather_statistics,
        table_dont_allow_reset).

%-----------------------------------------------------------------------------%
%
% Some more stuff for the foreign language interface
%

:- type pragma_foreign_proc_attributes
    --->    attributes(
                attr_foreign_language           :: foreign_language,
                attr_may_call_mercury           :: proc_may_call_mercury,
                attr_thread_safe                :: proc_thread_safe,
                attr_tabled_for_io              :: proc_tabled_for_io,
                attr_purity                     :: purity,
                attr_terminates                 :: proc_terminates,
                attr_user_annotated_sharing     :: user_annotated_sharing,
                attr_may_throw_exception        :: proc_may_throw_exception,

                % There is some special case behaviour for pragma c_code
                % and pragma import purity if legacy_purity_behaviour is `yes'.
                attr_legacy_purity_behaviour    :: bool,
                attr_ordinary_despite_detism    :: bool,
                attr_may_modify_trail           :: proc_may_modify_trail,
                attr_may_call_mm_tabled         :: may_call_mm_tabled,
                attr_box_policy                 :: box_policy,
                attr_affects_liveness           :: proc_affects_liveness,
                attr_allocates_memory           :: proc_allocates_memory,
                attr_registers_roots            :: proc_registers_roots,
                attr_may_duplicate              :: maybe(proc_may_duplicate),
                attr_extra_attributes ::
                    list(pragma_foreign_proc_extra_attribute)
            ).

default_attributes(Language) =
    attributes(Language, proc_may_call_mercury, proc_not_thread_safe,
        proc_not_tabled_for_io, purity_impure, depends_on_mercury_calls,
        no_user_annotated_sharing, default_exception_behaviour,
        no, no, proc_may_modify_trail, default_calls_mm_tabled,
        native_if_possible, proc_default_affects_liveness,
        proc_default_allocates_memory, proc_default_registers_roots,
        no, []).

get_may_call_mercury(Attrs) = Attrs ^ attr_may_call_mercury.
get_thread_safe(Attrs) = Attrs ^ attr_thread_safe.
get_foreign_language(Attrs) = Attrs ^ attr_foreign_language.
get_tabled_for_io(Attrs) = Attrs ^ attr_tabled_for_io.
get_purity(Attrs) = Attrs ^ attr_purity.
get_terminates(Attrs) = Attrs ^ attr_terminates.
get_user_annotated_sharing(Attrs) = Attrs ^ attr_user_annotated_sharing.
get_may_throw_exception(Attrs) = Attrs ^ attr_may_throw_exception.
get_legacy_purity_behaviour(Attrs) = Attrs ^ attr_legacy_purity_behaviour.
get_ordinary_despite_detism(Attrs) = Attrs ^ attr_ordinary_despite_detism.
get_may_modify_trail(Attrs) = Attrs ^ attr_may_modify_trail.
get_may_call_mm_tabled(Attrs) = Attrs ^ attr_may_call_mm_tabled.
get_box_policy(Attrs) = Attrs ^ attr_box_policy.
get_affects_liveness(Attrs) = Attrs ^ attr_affects_liveness.
get_allocates_memory(Attrs) = Attrs ^ attr_allocates_memory.
get_registers_roots(Attrs) = Attrs ^ attr_registers_roots.
get_may_duplicate(Attrs) = Attrs ^ attr_may_duplicate.
get_extra_attributes(Attrs) = Attrs ^ attr_extra_attributes.

set_may_call_mercury(MayCallMercury, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_may_call_mercury := MayCallMercury.
set_thread_safe(ThreadSafe, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_thread_safe := ThreadSafe.
set_foreign_language(ForeignLanguage, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_foreign_language := ForeignLanguage.
set_tabled_for_io(TabledForIo, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_tabled_for_io := TabledForIo.
set_purity(Purity, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_purity := Purity.
set_terminates(Terminates, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_terminates := Terminates.
set_user_annotated_sharing(UserSharing, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_user_annotated_sharing := UserSharing.
set_may_throw_exception(MayThrowException, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_may_throw_exception := MayThrowException.
set_legacy_purity_behaviour(Legacy, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_legacy_purity_behaviour := Legacy.
set_ordinary_despite_detism(OrdinaryDespiteDetism, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_ordinary_despite_detism := OrdinaryDespiteDetism.
set_may_modify_trail(MayModifyTrail, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_may_modify_trail := MayModifyTrail.
set_may_call_mm_tabled(MayCallMM_Tabled, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_may_call_mm_tabled := MayCallMM_Tabled.
set_box_policy(BoxPolicyStr, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_box_policy := BoxPolicyStr.
set_affects_liveness(AffectsLiveness, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_affects_liveness := AffectsLiveness.
set_allocates_memory(AllocatesMemory, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_allocates_memory := AllocatesMemory.
set_registers_roots(RegistersRoots, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_registers_roots := RegistersRoots.
set_may_duplicate(MayDuplicate, Attrs0, Attrs) :-
    Attrs = Attrs0 ^ attr_may_duplicate := MayDuplicate.

attributes_to_strings(Attrs) = StringList :-
    % We ignore Lang because it isn't an attribute that you can put
    % in the attribute list -- the foreign language specifier string
    % is at the start of the pragma.
    Attrs = attributes(_Lang, MayCallMercury, ThreadSafe, TabledForIO,
        Purity, Terminates, _UserSharing, Exceptions, _LegacyBehaviour,
        OrdinaryDespiteDetism, MayModifyTrail, MayCallMM_Tabled,
        BoxPolicy, AffectsLiveness, AllocatesMemory, RegistersRoots,
        MaybeMayDuplicate, ExtraAttributes),
    (
        MayCallMercury = proc_may_call_mercury,
        MayCallMercuryStr = "may_call_mercury"
    ;
        MayCallMercury = proc_will_not_call_mercury,
        MayCallMercuryStr = "will_not_call_mercury"
    ),
    (
        ThreadSafe = proc_not_thread_safe,
        ThreadSafeStr = "not_thread_safe"
    ;
        ThreadSafe = proc_thread_safe,
        ThreadSafeStr = "thread_safe"
    ;
        ThreadSafe = proc_maybe_thread_safe,
        ThreadSafeStr = "maybe_thread_safe"
    ),
    (
        TabledForIO = proc_tabled_for_io,
        TabledForIOStr = "tabled_for_io"
    ;
        TabledForIO = proc_tabled_for_io_unitize,
        TabledForIOStr = "tabled_for_io_unitize"
    ;
        TabledForIO = proc_tabled_for_descendant_io,
        TabledForIOStr = "tabled_for_descendant_io"
    ;
        TabledForIO = proc_not_tabled_for_io,
        TabledForIOStr = "not_tabled_for_io"
    ),
    (
        Purity = purity_pure,
        PurityStrList = ["promise_pure"]
    ;
        Purity = purity_semipure,
        PurityStrList = ["promise_semipure"]
    ;
        Purity = purity_impure,
        PurityStrList = []
    ),
    (
        Terminates = proc_terminates,
        TerminatesStrList = ["terminates"]
    ;
        Terminates = proc_does_not_terminate,
        TerminatesStrList = ["does_not_terminate"]
    ;
        Terminates = depends_on_mercury_calls,
        TerminatesStrList = []
    ),
    (
        Exceptions = proc_will_not_throw_exception,
        ExceptionsStrList = ["will_not_throw_exception"]
    ;
        Exceptions = default_exception_behaviour,
        ExceptionsStrList = []
    ),
    (
        OrdinaryDespiteDetism = yes,
        OrdinaryDespiteDetismStrList = ["ordinary_despite_detism"]
    ;
        OrdinaryDespiteDetism = no,
        OrdinaryDespiteDetismStrList = []
    ),
    (
        MayModifyTrail = proc_may_modify_trail,
        MayModifyTrailStrList = ["may_modify_trail"]
    ;
        MayModifyTrail = proc_will_not_modify_trail,
        MayModifyTrailStrList = ["will_not_modify_trail"]
    ),
    (
        MayCallMM_Tabled = may_call_mm_tabled,
        MayCallMM_TabledStrList = ["may_call_mm_tabled"]
    ;
        MayCallMM_Tabled = will_not_call_mm_tabled,
        MayCallMM_TabledStrList =["will_not_call_mm_tabled"]
    ;
        MayCallMM_Tabled = default_calls_mm_tabled,
        MayCallMM_TabledStrList = []
    ),
    (
        BoxPolicy = native_if_possible,
        BoxPolicyStrList = []
    ;
        BoxPolicy = always_boxed,
        BoxPolicyStrList = ["always_boxed"]
    ),
    (
        AffectsLiveness = proc_affects_liveness,
        AffectsLivenessStrList = ["affects_liveness"]
    ;
        AffectsLiveness = proc_does_not_affect_liveness,
        AffectsLivenessStrList = ["doesnt_affect_liveness"]
    ;
        AffectsLiveness = proc_default_affects_liveness,
        AffectsLivenessStrList = []
    ),
    (
        AllocatesMemory = proc_does_not_allocate_memory,
        AllocatesMemoryStrList =["doesnt_allocate_memory"]
    ;
        AllocatesMemory = proc_allocates_bounded_memory,
        AllocatesMemoryStrList = ["allocates_bounded_memory"]
    ;
        AllocatesMemory = proc_allocates_unbounded_memory,
        AllocatesMemoryStrList = ["allocates_unbounded_memory"]
    ;
        AllocatesMemory = proc_default_allocates_memory,
        AllocatesMemoryStrList = []
    ),
    (
        RegistersRoots = proc_registers_roots,
        RegistersRootsStrList = ["registers_roots"]
    ;
        RegistersRoots = proc_does_not_register_roots,
        RegistersRootsStrList =["doesnt_register_roots"]
    ;
        RegistersRoots = proc_does_not_have_roots,
        RegistersRootsStrList = ["doesnt_have_roots"]
    ;
        RegistersRoots = proc_default_registers_roots,
        RegistersRootsStrList = []
    ),
    (
        MaybeMayDuplicate = yes(MayDuplicate),
        (
            MayDuplicate = proc_may_duplicate,
            MayDuplicateStrList = ["may_duplicate"]
        ;
            MayDuplicate = proc_may_not_duplicate,
            MayDuplicateStrList = ["may_not_duplicate"]
        )
    ;
        MaybeMayDuplicate = no,
        MayDuplicateStrList = []
    ),
    StringList = [MayCallMercuryStr, ThreadSafeStr, TabledForIOStr |
        PurityStrList] ++ TerminatesStrList ++ ExceptionsStrList ++
        OrdinaryDespiteDetismStrList ++ MayModifyTrailStrList ++
        MayCallMM_TabledStrList ++ BoxPolicyStrList ++
        AffectsLivenessStrList ++ AllocatesMemoryStrList ++
        RegistersRootsStrList ++ MayDuplicateStrList ++
        list.map(extra_attribute_to_string, ExtraAttributes).

add_extra_attribute(NewAttribute, Attributes0,
    Attributes0 ^ attr_extra_attributes :=
        [NewAttribute | Attributes0 ^ attr_extra_attributes]).

:- func extra_attribute_to_string(pragma_foreign_proc_extra_attribute)
    = string.

extra_attribute_to_string(refers_to_llds_stack) = "refers_to_llds_stack".
extra_attribute_to_string(backend(low_level_backend)) = "low_level_backend".
extra_attribute_to_string(backend(high_level_backend)) = "high_level_backend".
extra_attribute_to_string(max_stack_size(Size)) =
    "max_stack_size(" ++ string.int_to_string(Size) ++ ")".

%-----------------------------------------------------------------------------%
%
% Purity
%

less_pure(P1, P2) :-
    \+ ( worst_purity(P1, P2) = P2).

    % worst_purity/3 could be written more compactly, but this definition
    % guarantees us a determinism error if we add to type `purity'.  We also
    % define less_pure/2 in terms of worst_purity/3 rather than the other way
    % around for the same reason.
    %
worst_purity(purity_pure, purity_pure) = purity_pure.
worst_purity(purity_pure, purity_semipure) = purity_semipure.
worst_purity(purity_pure, purity_impure) = purity_impure.
worst_purity(purity_semipure, purity_pure) = purity_semipure.
worst_purity(purity_semipure, purity_semipure) = purity_semipure.
worst_purity(purity_semipure, purity_impure) = purity_impure.
worst_purity(purity_impure, purity_pure) = purity_impure.
worst_purity(purity_impure, purity_semipure) = purity_impure.
worst_purity(purity_impure, purity_impure) = purity_impure.

    % best_purity/3 is written as a switch for the same reason as
    % worst_purity/3.
    %
best_purity(purity_pure, purity_pure) = purity_pure.
best_purity(purity_pure, purity_semipure) = purity_pure.
best_purity(purity_pure, purity_impure) = purity_pure.
best_purity(purity_semipure, purity_pure) = purity_pure.
best_purity(purity_semipure, purity_semipure) = purity_semipure.
best_purity(purity_semipure, purity_impure) = purity_semipure.
best_purity(purity_impure, purity_pure) = purity_pure.
best_purity(purity_impure, purity_semipure) = purity_semipure.
best_purity(purity_impure, purity_impure) = purity_impure.

%-----------------------------------------------------------------------------%
%
% Determinism
%

determinism_components(detism_det,       cannot_fail, at_most_one).
determinism_components(detism_semi,      can_fail,    at_most_one).
determinism_components(detism_multi,     cannot_fail, at_most_many).
determinism_components(detism_non,       can_fail,    at_most_many).
determinism_components(detism_cc_multi,  cannot_fail, at_most_many_cc).
determinism_components(detism_cc_non,    can_fail,    at_most_many_cc).
determinism_components(detism_erroneous, cannot_fail, at_most_zero).
determinism_components(detism_failure,   can_fail,    at_most_zero).

det_conjunction_detism(DetismA, DetismB, Detism) :-
    % When figuring out the determinism of a conjunction, if the second goal
    % is unreachable, then then the determinism of the conjunction is just
    % the determinism of the first goal.

    determinism_components(DetismA, CanFailA, MaxSolnA),
    ( MaxSolnA = at_most_zero ->
        Detism = DetismA
    ;
        determinism_components(DetismB, CanFailB, MaxSolnB),
        det_conjunction_canfail(CanFailA, CanFailB, CanFail),
        det_conjunction_maxsoln(MaxSolnA, MaxSolnB, MaxSoln),
        determinism_components(Detism, CanFail, MaxSoln)
    ).

det_par_conjunction_detism(DetismA, DetismB, Detism) :-
    % Figuring out the determinism of a parallel conjunction is much easier
    % than for a sequential conjunction, since you simply ignore the case
    % where the second goal is unreachable. Just do a normal solution count.

    determinism_components(DetismA, CanFailA, MaxSolnA),
    determinism_components(DetismB, CanFailB, MaxSolnB),
    det_conjunction_canfail(CanFailA, CanFailB, CanFail),
    det_conjunction_maxsoln(MaxSolnA, MaxSolnB, MaxSoln),
    determinism_components(Detism, CanFail, MaxSoln).

det_switch_detism(DetismA, DetismB, Detism) :-
    determinism_components(DetismA, CanFailA, MaxSolnA),
    determinism_components(DetismB, CanFailB, MaxSolnB),
    det_switch_canfail(CanFailA, CanFailB, CanFail),
    det_switch_maxsoln(MaxSolnA, MaxSolnB, MaxSoln),
    determinism_components(Detism, CanFail, MaxSoln).

%-----------------------------------------------------------------------------%
%
% The predicates in this section do abstract interpretation to count
% the number of solutions and the possible number of failures.
%
% If the num_solns is at_most_many_cc, this means that the goal might have
% many logical solutions if there were no pruning, but that the goal occurs
% in a single-solution context, so only the first solution will be
% returned.
%
% The reason why we don't throw an exception in det_switch_maxsoln and
% det_disjunction_maxsoln is given in the documentation of the test case
% invalid/magicbox.m.

det_conjunction_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_one,     at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_many_cc, at_most_zero).
det_conjunction_maxsoln(at_most_zero,    at_most_many,    at_most_zero).

det_conjunction_maxsoln(at_most_one,     at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_one,     at_most_one,     at_most_one).
det_conjunction_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_conjunction_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_conjunction_maxsoln(at_most_many_cc, at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_conjunction_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_conjunction_maxsoln(at_most_many_cc, at_most_many,    _) :-
    % If the first conjunct could be cc pruned, the second conj ought to have
    % been cc pruned too.
    unexpected(this_file, "det_conjunction_maxsoln: many_cc , many").

det_conjunction_maxsoln(at_most_many,    at_most_zero,    at_most_zero).
det_conjunction_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_conjunction_maxsoln(at_most_many,    at_most_many_cc, at_most_many).
det_conjunction_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_conjunction_canfail(can_fail,    can_fail,    can_fail).
det_conjunction_canfail(can_fail,    cannot_fail, can_fail).
det_conjunction_canfail(cannot_fail, can_fail,    can_fail).
det_conjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

det_disjunction_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_disjunction_maxsoln(at_most_zero,    at_most_one,     at_most_one).
det_disjunction_maxsoln(at_most_zero,    at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_zero,    at_most_many,    at_most_many).

det_disjunction_maxsoln(at_most_one,     at_most_zero,    at_most_one).
det_disjunction_maxsoln(at_most_one,     at_most_one,     at_most_many).
det_disjunction_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_disjunction_maxsoln(at_most_many_cc, at_most_zero,    at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_many_cc, at_most_many,    at_most_many_cc).

det_disjunction_maxsoln(at_most_many,    at_most_zero,    at_most_many).
det_disjunction_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_disjunction_maxsoln(at_most_many,    at_most_many_cc, at_most_many_cc).
det_disjunction_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_disjunction_canfail(can_fail,    can_fail,    can_fail).
det_disjunction_canfail(can_fail,    cannot_fail, cannot_fail).
det_disjunction_canfail(cannot_fail, can_fail,    cannot_fail).
det_disjunction_canfail(cannot_fail, cannot_fail, cannot_fail).

det_switch_maxsoln(at_most_zero,    at_most_zero,    at_most_zero).
det_switch_maxsoln(at_most_zero,    at_most_one,     at_most_one).
det_switch_maxsoln(at_most_zero,    at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_zero,    at_most_many,    at_most_many).

det_switch_maxsoln(at_most_one,     at_most_zero,    at_most_one).
det_switch_maxsoln(at_most_one,     at_most_one,     at_most_one).
det_switch_maxsoln(at_most_one,     at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_one,     at_most_many,    at_most_many).

det_switch_maxsoln(at_most_many_cc, at_most_zero,    at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_one,     at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_many_cc, at_most_many,    at_most_many_cc).

det_switch_maxsoln(at_most_many,    at_most_zero,    at_most_many).
det_switch_maxsoln(at_most_many,    at_most_one,     at_most_many).
det_switch_maxsoln(at_most_many,    at_most_many_cc, at_most_many_cc).
det_switch_maxsoln(at_most_many,    at_most_many,    at_most_many).

det_switch_canfail(can_fail,    can_fail,    can_fail).
det_switch_canfail(can_fail,    cannot_fail, can_fail).
det_switch_canfail(cannot_fail, can_fail,    can_fail).
det_switch_canfail(cannot_fail, cannot_fail, cannot_fail).

det_negation_det(detism_det,       yes(detism_failure)).
det_negation_det(detism_semi,      yes(detism_semi)).
det_negation_det(detism_multi,     no).
det_negation_det(detism_non,       no).
det_negation_det(detism_cc_multi,  no).
det_negation_det(detism_cc_non,    no).
det_negation_det(detism_erroneous, yes(detism_erroneous)).
det_negation_det(detism_failure,   yes(detism_det)).

%-----------------------------------------------------------------------------%

tvarset_merge_renaming(TVarSetA, TVarSetB, TVarSet, Renaming) :-
    varset.merge_subst(TVarSetA, TVarSetB, TVarSet, Subst),
    map.map_values(convert_subst_term_to_tvar, Subst, Renaming).

tvarset_merge_renaming_without_names(TVarSetA, TVarSetB, TVarSet, Renaming) :-
    varset.merge_subst_without_names(TVarSetA, TVarSetB, TVarSet, Subst),
    map.map_values(convert_subst_term_to_tvar, Subst, Renaming).

:- pred convert_subst_term_to_tvar(tvar::in, term(tvar_type)::in, tvar::out)
    is det.

convert_subst_term_to_tvar(_, variable(TVar, _), TVar).
convert_subst_term_to_tvar(_, functor(_, _, _), _) :-
    unexpected(this_file, "non-variable found in renaming").

%-----------------------------------------------------------------------------%

get_tvar_kind(Map, TVar, Kind) :-
    ( map.search(Map, TVar, Kind0) ->
        Kind = Kind0
    ;
        Kind = kind_star
    ).

get_type_kind(type_variable(_, Kind)) = Kind.
get_type_kind(defined_type(_, _, Kind)) = Kind.
get_type_kind(builtin_type(_)) = kind_star.
get_type_kind(higher_order_type(_, _, _, _)) = kind_star.
get_type_kind(tuple_type(_, Kind)) = Kind.
get_type_kind(apply_n_type(_, _, Kind)) = Kind.
get_type_kind(kinded_type(_, Kind)) = Kind.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

used_modules_init = used_modules(set.init, set.init).

%-----------------------------------------------------------------------------%

add_sym_name_module(_Status, unqualified(_), !UsedModules).
add_sym_name_module(Visibility, qualified(ModuleName, _), !UsedModules) :-
    add_all_modules(Visibility, ModuleName, !UsedModules).

add_all_modules(Visibility, ModuleName @ unqualified(_), !UsedModules) :-
    add_module(Visibility, ModuleName, !UsedModules).
add_all_modules(Visibility, ModuleName @ qualified(Parent, _), !UsedModules) :-
    add_module(Visibility, ModuleName, !UsedModules),
    add_all_modules(Visibility, Parent, !UsedModules).

:- pred add_module(item_visibility::in, module_name::in,
    used_modules::in, used_modules::out) is det.

add_module(visibility_public, Module, !UsedModules) :-
    !:UsedModules = !.UsedModules ^ int_used_modules :=
            set.insert(!.UsedModules ^ int_used_modules, Module).
add_module(visibility_private, Module, !UsedModules) :-
    !:UsedModules = !.UsedModules ^ impl_used_modules :=
            set.insert(!.UsedModules ^ impl_used_modules, Module).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_data.m".

%-----------------------------------------------------------------------------%
:- end_module prog_data.
%-----------------------------------------------------------------------------%
