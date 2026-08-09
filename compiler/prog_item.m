%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_item.m.
% Original author: fjh.
% Main author of the current version: zs.
%
% The Mercury implementation uses several different kinds of files.
% Besides source files, it uses four kinds of interface files and
% two kinds of optimization files. The parse trees of these files
% contain a structured representation of the information in these files.
% The prog_parse_tree.m module defines the top levels of these parse trees,
% the parts that differ between the different kinds of files. This module
% defines the middle levels of the parse trees. These represent entities
% such as type definitions, predicate declarations and clauses, which are
% needed during the construction of the initial HLDS, but not later.
% This is due to the HLDS containing so much more information about
% those entities. The lowest levels of the parse tree, which are needed
% in the HLDS representation as well, are defined in prog_data*.m.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_item.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item_inst_mode.
:- import_module parse_tree.prog_item_pragma.
:- import_module parse_tree.prog_item_pred_proc_id.
:- import_module parse_tree.prog_item_type.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%
%
% The main parts of parse trees are items. There are many kinds of items,
% and most of those kinds have their own item-kind-specific type that stores
% all the information the parse tree has about an item of that kind.
%
% The sequence number fields in the item-kind-specific types are intended to
% allow the recreation of the original item sequence after we have processed
% it into more complex data structures. Negative sequence numbers represent
% items that were not in the original read-in sequence, but which were added
% by the compiler. It is possible for two items to have the same sequence
% number if one original term (e.g. one that imports two or more modules)
% is split apart (e.g. into several items that each import only one module).
%
% When we create interface files, we print out selected items in the module.
% If the sequence of items printed changes, all the other modules depending
% on that interface file will be recompiled.
%
% A nontrivial fraction of changes to a module affect only the *order*
% of the items included in the interface, not their *content*. To minimize
% the amount of recompilation we have to do, we sort (most of the kinds of)
% items in the interface file, so that a change in the item order in the
% source file does not change the order of the items in the interface file.
% To make this sorting effective, we put the fields we prefer to use as
% the sort keys at the start of the item-kind-specific types. These are
% usually those that define the name of the entity, and if it makes sense
% to have more than item with that name, the main fields that distinguish
% items of the same name from each other.
%
%---------------------------------------------------------------------------%

:- type item
    --->    item_clause(item_clause_info)
    ;       item_type_defn(item_type_defn_info)
    ;       item_inst_defn(item_inst_defn_info)
    ;       item_mode_defn(item_mode_defn_info)
    ;       item_pred_decl(item_pred_decl_info)
    ;       item_mode_decl(item_mode_decl_info)
    ;       item_foreign_proc(item_foreign_proc_info)
    ;       item_foreign_enum(item_foreign_enum_info)
    ;       item_foreign_export_enum(item_foreign_export_enum_info)
    ;       item_decl_pragma(item_decl_pragma_info)
    ;       item_decl_marker(item_decl_marker_info)
    ;       item_impl_pragma(item_impl_pragma_info)
    ;       item_impl_marker(item_impl_marker_info)
    ;       item_generated_pragma(item_generated_pragma_info)
    ;       item_promise(item_promise_info)
    ;       item_typeclass(item_typeclass_info)
    ;       item_instance(item_instance_info)
    ;       item_initialise(item_initialise_info)
    ;       item_finalise(item_finalise_info)
    ;       item_mutable(item_mutable_info)
    ;       item_type_repn(item_type_repn_info).

%---------------------------------------------------------------------------%

:- type item_clause_info
    --->    item_clause_info(
                cl_pred_or_func                 :: pred_or_func,
                cl_predname                     :: sym_name,
                cl_head_args                    :: list(prog_term),
                cl_varset                       :: prog_varset,
                cl_body                         :: parse_result1(goal),
                cl_context                      :: prog_context,
                cl_seq_num                      :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_pred_decl_info
    --->    item_pred_decl_info(
                % `:- pred ...' or `:- func ...':
                % a predicate or function declaration.
                % This specifies the type of the predicate or function,
                % and it may optionally also specify the mode and determinism.
                pf_name                         :: sym_name,
                pf_p_or_f                       :: pred_or_func,
                pf_arg_decls                    :: types_and_maybe_modes,
                % The next two fields hold the `with_type` and `with_inst`
                % annotations. This syntactic sugar is expanded out by
                % equiv_type.m, which will then set these fields to `no'.
                pf_maybe_with_type              :: maybe(mer_type),
                pf_maybe_with_inst              :: maybe(mer_inst),
                pf_maybe_detism                 :: maybe(determinism),
                pf_maybe_attrs                  :: item_maybe_attrs,
                pf_tvarset                      :: tvarset,
                pf_instvarset                   :: inst_varset,
                pf_existqvars                   :: existq_tvars,
                pf_purity                       :: purity,
                pf_constraints                  :: univ_exist_constraints,
                pf_context                      :: prog_context,
                pf_seq_num                      :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_mode_decl_info
    --->    item_mode_decl_info(
                % `:- mode ...':
                % a mode declaration for a predicate or function.
                pfm_name                        :: sym_name,
                pfm_p_or_f                      :: maybe(pred_or_func),
                pfm_arg_modes                   :: list(mer_mode),
                % The next field holds the `with_inst` annotation. This
                % syntactic sugar is expanded by equiv_type.m, which will
                % then set the field to `no'.
                pfm_maybe_with_inst             :: maybe(mer_inst),
                pfm_maybe_detism                :: maybe(determinism),
                pfm_instvarset                  :: inst_varset,
                pfm_context                     :: prog_context,
                pfm_seq_num                     :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_foreign_proc_info
    --->    item_foreign_proc_info(
                % Set of foreign proc attributes, such as:
                %   what language this code is in
                %   whether or not the code may call Mercury,
                %   whether or not the code is thread-safe
                % PredName, Predicate or Function, Vars/Mode,
                % VarNames, Foreign Code Implementation Info
                proc_attrs                      :: foreign_proc_attributes,
                proc_name                       :: sym_name,
                proc_p_or_f                     :: pred_or_func,
                proc_vars                       :: list(pragma_var),
                proc_varset                     :: prog_varset,
                proc_instvarset                 :: inst_varset,
                proc_impl                       :: pragma_foreign_proc_impl,
                proc_context                    :: prog_context,
                proc_seq_num                    :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_foreign_export_enum_info
    --->    item_foreign_export_enum_info(
                fee_language                    :: foreign_language,
                fee_type_ctor                   :: type_ctor,
                fee_attributes                  :: export_enum_attributes,
                fee_overrides                   :: assoc_list(sym_name,
                                                    string),
                fee_context                     :: prog_context,
                fee_seq_num                     :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_promise_info
    --->    item_promise_info(
                prom_type                       :: promise_type,
                prom_clause                     :: goal,
                prom_varset                     :: prog_varset,
                prom_univ_quant_vars            :: list(prog_var),
                prom_context                    :: prog_context,
                prom_seq_num                    :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_typeclass_info
    --->    item_typeclass_info(
                tc_class_name                   :: class_name,
                tc_class_params                 :: list(tvar),
                % The argument list of every superclass constraint
                % must be either a type variable, or a ground type.
                % This is enforced by parse_superclass_constraints
                % in parse_class.m.
                % XXX We should consider changing the type of this field
                % from list(prog_constraint) to list(var_or_ground_constraint).
                tc_superclasses                 :: list(prog_constraint),
                tc_fundeps                      :: list(prog_fundep),
                tc_class_methods                :: class_interface,
                tc_varset                       :: tvarset,
                tc_context                      :: prog_context,
                tc_seq_num                      :: item_seq_num
            ).

:- type item_abstract_typeclass_info =< item_typeclass_info
    --->    item_typeclass_info(
                tc_class_name                   :: class_name,
                tc_class_params                 :: list(tvar),
                tc_superclasses                 :: list(prog_constraint),
                tc_fundeps                      :: list(prog_fundep),
                tc_class_methods                :: abstract_class_interface,
                tc_varset                       :: tvarset,
                tc_context                      :: prog_context,
                tc_seq_num                      :: item_seq_num
            ).

:- type item_abstract_int3_typeclass_info =< item_typeclass_info
    --->    item_typeclass_info(
                tc_class_name                   :: class_name,
                tc_class_params                 :: list(tvar),
                % XXX Both of the following should be empty_lists,
                % if the definition of that subtype in library/list.m
                % worked.
                tc_superclasses                 :: list(prog_constraint),
                tc_fundeps                      :: list(prog_fundep),
                tc_class_methods                :: abstract_class_interface,
                tc_varset                       :: tvarset,
                tc_context                      :: prog_context,
                tc_seq_num                      :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_instance_info
    --->    item_instance_info(
                % The original types field preserves the types in the instance
                % declaration as written by the programmer. The types field
                % is subject to the expansion of equivalence types.
                ci_class_name                   :: class_name,
                ci_types                        :: list(mer_type),
                ci_original_types               :: list(mer_type),
                ci_deriving_class               :: list(prog_constraint),
                ci_method_instances             :: instance_body,
                ci_varset                       :: tvarset,
                ci_module_containing_instance   :: module_name,
                ci_context                      :: prog_context,
                ci_seq_num                      :: item_seq_num
            ).

:- type item_abstract_instance_info =< item_instance_info
    --->    item_instance_info(
                % The original types field preserves the types in the instance
                % declaration as written by the programmer. The types field
                % is subject to the expansion of equivalence types.
                ci_class_name                   :: class_name,
                ci_types                        :: list(mer_type),
                ci_original_types               :: list(mer_type),
                ci_deriving_class               :: list(prog_constraint),
                ci_method_instances             :: abstract_instance_body,
                ci_varset                       :: tvarset,
                ci_module_containing_instance   :: module_name,
                ci_context                      :: prog_context,
                ci_seq_num                      :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_initialise_info
    --->    item_initialise_info(
                % :- initialise pred_name.
                init_name                       :: sym_name,
                init_arity                      :: user_arity,
                init_maybe_attrs                :: item_maybe_attrs,
                init_context                    :: prog_context,
                init_seq_num                    :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_finalise_info
    --->    item_finalise_info(
                % :- finalise pred_name.
                final_name                      :: sym_name,
                final_arity                     :: user_arity,
                final_maybe_attrs               :: item_maybe_attrs,
                final_context                   :: prog_context,
                final_seq_num                   :: item_seq_num
            ).

%---------------------------------------------------------------------------%

:- type item_mutable_info
    --->    item_mutable_info(
                % :- mutable(var_name, type, inst, value, attrs).
                mut_name                        :: string,
                % The mut_type and mut_inst fields are subject to expansion
                % in equiv_type.m; the mut_orig_type and mut_orig_inst fields
                % are not. The latter are used to improve error reporting.
                mut_orig_type                   :: mer_type,
                mut_type                        :: mer_type,
                mut_orig_inst                   :: mer_inst,
                mut_inst                        :: mer_inst,
                mut_init_value                  :: prog_term,
                mut_init_value_varset           :: prog_varset,
                mut_attrs                       :: mutable_var_attributes,
                mut_context                     :: prog_context,
                mut_seq_num                     :: item_seq_num
            ).

%---------------------------------------------------------------------------%
%
% Declarations of relationships between modules.
%

:- type item_include
    --->    item_include(
                % The representation of an `:- include_module' declaration
                % is a list of one or more item_includes, each of which
                % declares the named module to be a submodule of the
                % current module,
                %
                % If this item_include occurs in module x.y, then
                % the module_name here is guaranteed to have the form x.y.z.
                % In other words, the included module is guaranteed to be
                % an immediate descendant of the including module.
                % Any attempt to include a non-descendant module or a
                % non-immediate descendant module will be caught and
                % diagnosed by the parser.

                incl_module                     :: module_name,

                % The context and item sequence number of the declaration.
                incl_context                    :: prog_context,
                incl_seq_num                    :: item_seq_num
            ).

:- type import_or_use
    --->    import_decl
    ;       use_decl.

    % The representation of an `:- import_module' or an `:- use_module'
    % declaration is a list of one or more item_avails, each of which
    % makes available to the current module the entities in the interface
    % of the module named in the declaration.
    %
    % With avail_use, references to these entities must be module qualified;
    % with avail_import, they don't have to be.

:- type item_avail
    --->    avail_import(avail_import_info)
    ;       avail_use(avail_use_info).

    % The structures of avail_import_info and avail_use_info are the same,
    % with the first argument being the name of the module that is the subject
    % of the import_module or use_module declaration, and the second and third
    % being the context and item sequence number of the declaration.
    %
    % The two types are separate to allow parse_tree_opts to contain only
    % values of a type that makes it clear that they contain information
    % ONLY about use_module declarations, not import_module declarations.
:- type avail_import_info
    --->    avail_import_info(
                aii_module_name     :: module_name,
                aii_context         :: prog_context,
                aii_seq_num         :: item_seq_num
            ).
:- type avail_use_info
    --->    avail_use_info(
                aui_module_name     :: module_name,
                aui_context         :: prog_context,
                aui_seq_num         :: item_seq_num
            ).

:- type item_fim
    --->    item_fim(
                % A `:- pragma foreign_import_module(Lang, ModuleName)'
                % declaration, which tells the compiler to include the
                % header file we automatically generate for Module
                % in the target language Lang when we compile this module
                % to that language, and, if this occurs in the interface,
                % when we compile the modules importing this one
                % to that same target language.
                %
                % Equivalent to
                % `:- pragma foreign_decl(Lang, "#include <module>.h")',
                % except that the name of the header file is not hard-coded,
                % and mmake can use the dependency information.
                %
                % Throughout most parts of the compiler, we use "FIM"
                % as shorthand for foreign_import_module.

                fim_lang                        :: foreign_language,
                fim_module_name                 :: module_name,
                fim_context                     :: prog_context,
                fim_seq_num                     :: item_seq_num
            ).

%---------------------------------------------------------------------------%
%
% Type classes.
%

    % The class_decl type represents any declaration that occurs
    % in the body of a type class definition.
    %
    % Such declarations may either declare class methods, or they may declare
    % the modes of class methods.
    %
:- type class_decl
    --->    class_decl_pred_or_func(class_pred_or_func_info)
    ;       class_decl_mode(class_mode_info).

:- type class_pred_or_func_info
    --->    class_pred_or_func_info(
                % This is a `pred ...' or `func ...' declaration in a
                % type class body, which declares a predicate or function
                % method. Such declarations specify the types of the
                % arguments, and may optionally also specify argument modes
                % and the determinism.

                % The name of the predicate or function.
                sym_name,
                pred_or_func,

                % The arguments' types, and maybe modes.
                types_and_maybe_modes,

                % Any `with_type` and/or `with_inst` annotation.
                maybe(mer_type),
                maybe(mer_inst),

                % The determinism declaration, if any.
                maybe(determinism),

                % The varsets of the type and inst variables.
                tvarset,
                inst_varset,

                % The existentially quantified type variables, if any.
                existq_tvars,

                % Any purity annotation.
                purity,

                % The typeclass constraints on the declaration.
                univ_exist_constraints,

                prog_context
            ).

:- type class_mode_info
    --->    class_mode_info(
                % This is a `mode ...' declaration in a type class body.
                % Such a declaration declares a mode for one of the methods
                % of the type class.

                % The name of the predicate or function.
                sym_name,

                % Whether the method is a predicate or a function.
                % For declarations using `with_inst`, we don't know
                % which it is until we have expanded the inst.
                maybe(pred_or_func),

                % The arguments' modes.
                list(mer_mode),

                % Any `with_inst` annotation.
                maybe(mer_inst),

                % Any determinism declaration.
                maybe(determinism),

                % The varset of the inst variables.
                inst_varset,

                prog_context
            ).

%---------------------------------------------------------------------------%
%
% Mutable variables.
%

    % Indicates if updates to the mutable are trailed or untrailed.
    %
:- type mutable_trailed
    --->    mutable_untrailed
    ;       mutable_trailed.

    % Indicates if a mutable is attached to the I/O state or not.
    %
:- type mutable_attach_to_io_state
    --->    mutable_do_not_attach_to_io_state
    ;       mutable_attach_to_io_state.

    % Indicates if a mutable is constant or not.
    %
:- type mutable_constant
    --->    mutable_not_constant
    ;       mutable_constant.

    % Indicates if a mutable is thread-local or not.
    %
:- type mutable_thread_local
    --->    mutable_not_thread_local
    ;       mutable_thread_local.

    % Attributes for mutable variables.
    %
:- type mutable_var_attributes
    --->    mutable_var_attributes(
                mutable_foreign_names       :: map(foreign_language, string),
                mutable_constant            :: mutable_maybe_constant
            ).

:- type mutable_maybe_constant
    --->    mutable_is_constant
            % implies mutable_do_not_attach_to_io_state
            % implies mutable_untrailed
            % implies mutable_not_thread_local
    ;       mutable_is_not_constant(
                mutable_attach_to_io_state,
                mutable_maybe_thread_local
            ).

:- type mutable_maybe_thread_local
    --->    mutable_is_not_thread_local(
                mutable_trailed
            )
    ;       mutable_is_thread_local.
            % implies mutable_untrailed

:- func mutable_var_thread_local(mutable_maybe_constant)
    = mutable_thread_local.
:- func mutable_thread_local_trailed(mutable_maybe_thread_local)
    = mutable_trailed.

%---------------------------------------------------------------------------%
%
% Goals.
%

    % Here is how goals are represented in the parse tree.
    % The three most frequent kinds of goals are first, to give them
    % their own primary tags on 32 bit machines, and
    % the seven most frequent kinds of goals are first, to give them
    % their own primary tags on 64 bit machines.
    %
    % During a bootcheck in august 2015, the frequencies of occurrence
    % of the various goal kinds were these:
    %
    % goal_unify               1360701
    % goal_conj                1316066 when we had a conj_expr for each ","
    % goal_call                1263403
    %
    % goal_true                 135352
    % goal_if_then_else         128052
    % goal_disj                 116547 when we had a disj_expr for each ";"
    % goal_not                    7080
    %
    % goal_fail                   5219
    % goal_pro_purity             1492
    % goal_trace                  1356
    % goal_pro_eqv_solns           913
    % goal_some_state_vars         620 now goal_quant/some/state
    % goal_some                    192 now goal_quant/some/ordinary
    % goal_req_compl_switch        172
    % goal_par_conj                132 when we had a par_conj_expr for each "&"
    % goal_implies                 129
    % goal_all                      78 now goal_quant/all/ordinary
    % goal_req_detism               49
    % goal_try                      35
    % goal_equivalent               18
    % goal_event                    17
    % goal_req_arm_detism           14
    % goal_pro_arbitrary            12
    % goal_pro_eqv_soln_sets         8
    % goal_atomic                    2
    % goal_all_state_vars            0 now goal_quant/all/state

:- type quant_type
    --->    quant_some
    ;       quant_all.

:- type quant_vars_kind
    --->    quant_ordinary_vars
    ;       quant_state_vars.

:- type plain_or_dot_var
    --->    podv_plain(prog_var)
            % V: a plain variable.
    ;       podv_dot(prog_var).
            % !.SV: the current state of this state variable.

:- type goal
    % The most frequent kinds of goals.
    --->    unify_expr(prog_context, prog_term, prog_term, purity)
    ;       call_expr(prog_context, sym_name, list(prog_term), purity)

    ;       conj_expr(prog_context, goal, list(goal))
            % nonempty plain conjunction
            % NOTE: We could replace this with
            %   conj_expr(prog_context, goal, goal, list(goal))
            % to encode the invariant that
            % - a conjunction has at least one conjunction operator, and
            % - that operator has two argument goals.
            % However, no part of the current compiler can exploit
            % this extra information.
            % NOTE: On the other hand, we could also replace this with
            %   conj_expr(prog_context, list(goal))
            % letting a conj_expr with an empty list of goals take over
            % the role of true_expr. However, that would make the parse tree
            % representation of plain conjunctions differ from the
            % representation of parallel conjunctions. And the most
            % frequent goal that does not now have its own primary tag
            % on 64 bit machines, fail_expr, is infrequent enough that
            % giving it its own primary tag would not materially improve
            % performance, and even if it were frequent enough, it could be
            % folded into disj_exprs in a similar way.

    ;       true_expr(prog_context)
            % empty conjunction

    ;       if_then_else_expr(
                prog_context,
                list(prog_var), % SomeVars
                list(prog_var), % StateVars
                goal,           % Cond
                goal,           % Then
                goal            % Else
            )
    ;       disj_expr(prog_context, goal, goal, list(goal))
            % nonempty disjunction; will contain at least two goals.

    ;       not_expr(prog_context, goal)

    % The other kinds of goals.

    ;       fail_expr(prog_context)
            % empty disjunction

    ;       par_conj_expr(prog_context, goal, list(goal))
            % nonempty parallel conjunction

    ;       quant_expr(
                % Existential or universal quantification?
                quant_type,

                % Are the variables ordinary variables or state variables?
                quant_vars_kind,

                prog_context,
                list(prog_var),
                goal
            )

    ;       promise_purity_expr(prog_context, purity, goal)
    ;       promise_equivalent_solutions_expr(
                prog_context,
                list(prog_var),  % OrdinaryVars
                list(prog_var),  % StateVars (!V)
                list(prog_var),  % DotStateVars (!.V)
                list(prog_var),  % ColonStateVars (!:V)
                goal
            )
    ;       promise_equivalent_solution_sets_expr(
                prog_context,
                list(prog_var),  % OrdinaryVars
                list(prog_var),  % StateVars (!V)
                list(prog_var),  % DotStateVars (!.V)
                list(prog_var),  % ColonStateVars (!:V)
                goal
            )
    ;       promise_equivalent_solution_arbitrary_expr(
                prog_context,
                list(prog_var),  % OrdinaryVars
                list(prog_var),  % StateVars (!V)
                list(prog_var),  % DotStateVars (!.V)
                list(prog_var),  % ColonStateVars (!:V)
                goal
            )
    ;       require_detism_expr(
                prog_context,
                determinism,
                goal
            )
    ;       require_complete_switch_expr(
                prog_context,
                plain_or_dot_var,
                goal
            )
    ;       require_switch_arms_detism_expr(
                prog_context,
                plain_or_dot_var,
                determinism,
                goal
            )
    ;       disable_warnings_expr(
                % Disable the given one or more warnings
                % in the goal inside the scope.
                prog_context,
                goal_warning,
                list(goal_warning),
                goal
            )
    ;       trace_expr(
                texpr_context       :: prog_context,
                texpr_compiletime   :: maybe(trace_expr(trace_compiletime)),
                texpr_runtime       :: maybe(trace_expr(trace_runtime)),
                texpr_maybe_io      :: maybe(prog_var),
                texpr_mutable_vars  :: list(trace_mutable_var),
                texpr_goal          :: goal
            )
    ;       atomic_expr(
                % Subgoals of the atomic goal are parsed into the following
                % datatype. During the creation of the parse tree, all
                % subterms of the "orelse" operator are flattened and placed
                % into a list. If this is the case, the first "orelse"
                % alternative is stored in "main_goal" whilst the other
                % alternatives are stored in "orelse_alternatives". If there
                % are no "or_else" operators within the atomic subgoal,
                % the subgoal is stored in "main_goal" whilst the
                % "orelse_alternatives" list remains empty.

                aexpr_context           :: prog_context,
                aexpr_outer             :: atomic_component_state,
                aexpr_inner             :: atomic_component_state,
                aexpr_output_vars       :: maybe(list(prog_var)),
                aexpr_main_goal         :: goal,
                aexpr_orelse_goals      :: list(goal)
            )
    ;       try_expr(
                tryexpr_context         :: prog_context,
                tryexpr_maybe_io        :: maybe(prog_var),
                tryexpr_goal            :: goal,
                tryexpr_then            :: goal,
                tryexpr_maybe_else      :: maybe(goal),
                tryexpr_catches         :: list(catch_expr),
                tryexpr_maybe_catch_any :: maybe(catch_any_expr)
            )

    ;       implies_expr(prog_context, goal, goal)
            % implies_expr(_, A, B) represents either A => B or B <= A.

    ;       equivalent_expr(prog_context, goal, goal)
            % equivalent_expr(_, A, B) represents A <=> B.

    ;       event_expr(prog_context, string, list(prog_term)).

:- type catch_expr
    --->    catch_expr(
                catch_pattern   :: prog_term,
                catch_goal      :: goal
            ).

:- type catch_any_expr
    --->    catch_any_expr(
                catch_any_var   :: prog_var,
                catch_any_goal  :: goal
            ).

%---------------------------------------------------------------------------%

:- func get_item_context(item) = prog_context.
:- func get_goal_context(goal) = prog_context.

%---------------------------------------------------------------------------%

    % A predicate or function declaration may give either
    % (a) only the types of the arguments, or
    % (b) both their types and modes.
    % However, if there are no arguments, then we need info from the rest
    % of the predicate declaration to decide whether to treat that declaration
    % as a predmode declaration or not.
:- type types_and_maybe_modes
    --->    no_types_arity_zero
    ;       types_only(list(mer_type))
    ;       types_and_modes(list(type_and_mode)).

    % get_declared_types_and_maybe_modes(TypesAndMaybeModes, WithInst,
    %   MaybeDetism, Types, MaybeModes):
    %
    % A pred declaration may contains just types, as in
    %   :- pred list.append(list(T), list(T), list(T)).
    % or it may contain both types and modes, as in
    %   :- pred list.append(list(T)::in, list(T)::in, list(T)::output).
    %
    % Due to that combination, the latter is a predmode declaration,
    % while the former is just a non-predmode pred declaration.
    %
    % In several places in the compiler, we want to replace any predmode
    % declarations with a pair of a non-predmode pred declaration and
    % a mode declaration. However, the absence of mode annotations
    % on arguments does NOT imply that a pred declaration does not need
    % a mode declaration created from it. If a pred declaration has
    % no visible arguments, then the statements "none of the visible arguments
    % have mode annotations" and "all the visible arguments have mode
    % annotations" are both true. In such cases, we use with pf_maybe_with_inst
    % and pf_maybe_detism fields of the item_pred_decl_info to decide matters.
    %
    % If an arity-zero pred declaration has a with_inst annotation, then it
    % should have a mode declaration generated for it (with the mode info
    % in that annotation joining the type info in a matching with_type
    % annotation). This can happen only before the execution of equiv_type.m,
    % which extends the argument list with the info in with_type and with_inst
    % annotations.
    %
    % If an arity-zero pred declaration without a with_inst annotation
    % has a specified determinism, then it is truly a arity-zero predicate
    % and thus has no argument modes to declare, but it nevertheless *should*
    % have a mode declaration generated for it, because we attach determinism
    % declarations to mode declarations.
    %
    % We should therefore return "no" as MaybeModes for arity-zero predicates
    % only if they have neither a with_inst annotation nor a declared
    % determinism. If they have either, we should return "yes([])".
    %
    % Despite the above, we return "no" in the absence of a with_inst
    % annotation even in the presence of a declared determinism. The reason
    % for this is that, while returning "yes([])" in that case leads to a
    % mostly-successful bootcheck, it does cause one test case to fail.
    %
    % This is the recompilation/unchange_with_type_nr test case. The cause
    % of the failure is the splitting up of this function declaration:
    %
    %   :- func with_type_6 `with_type` map_func(T, T) is det <= string(T).
    %
    % This function declaration has visible arity zero, no with_inst
    % annotation, but does declare a determinism. If we let the last point
    % cause is to return "yes([])" here, then our caller will output
    % the available mode/determinism info in a separate mode declaration.
    % Given that the function return value's type is not directly visible
    % (it will be known only after the with_type annotation has been
    % processed), the form in which we output this mode declaration will be
    %
    % :- mode with_type_6 is det.
    %
    % The problem is that this declaration is indistinguishable from the
    % mode declaration of a zero-arity *predicate* named with_type_6,
    % and indeed, that is what the parser believes it to be.
    % The test case fails because the compiler reports that it sees
    % a mode declaration for a predicate named with_type_6 which has no
    % pred declaration. This error prevents the compiler from proceeding
    % to the recompile/don't recompile decision that the test case is
    % all about.
    %
    % Until we define syntax rules that allow the mode declarations
    % of arity-zero predicates and functions (with the return value missing)
    % to be differentiated from each other, we want to keep ignoring
    % MaybeDetism, at least for functions. (We could pay attention
    % to MaybeDetism for predicates if we wanted to; getting our callers
    % to pass us a PredOrFunc value would be easy.)
    %
:- pred get_declared_types_and_maybe_modes(types_and_maybe_modes::in,
    maybe(mer_inst)::in, maybe(determinism)::in,
    list(mer_type)::out, maybe(list(mer_mode))::out) is det.

:- pred split_types_and_modes(list(type_and_mode)::in,
    list(mer_type)::out, list(mer_mode)::out) is det.

:- func types_and_maybe_modes_arity(types_and_maybe_modes) = pred_form_arity.

%---------------------------------------------------------------------------%

:- type contains_foreign_code
    --->    foreign_code_langs_known(set(foreign_language))
    ;       foreign_code_langs_unknown.

:- type contains_foreign_export
    --->    contains_foreign_export
    ;       contains_no_foreign_export.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_spec.

:- import_module one_or_more.
:- import_module pair.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Mutable variables.
%

mutable_var_thread_local(Const) = Local :-
    ( if
        Const = mutable_is_not_constant(_AttachToIO, IsLocal),
        % Const = mutable_is_constant would imply mutable_not_thread_local
        IsLocal = mutable_is_thread_local
    then
        Local = mutable_thread_local
    else
        Local = mutable_not_thread_local
    ).

mutable_thread_local_trailed(Local) = Trail :-
    (
        Local = mutable_is_not_thread_local(Trail)
    ;
        Local = mutable_is_thread_local,
        Trail = mutable_untrailed
    ).

%---------------------------------------------------------------------------%

get_item_context(Item) = Context :-
    (
        Item = item_clause(ItemClause),
        Context = ItemClause ^ cl_context
    ;
        Item = item_type_defn(ItemTypeDefn),
        Context = ItemTypeDefn ^ td_context
    ;
        Item = item_inst_defn(ItemInstDefn),
        Context = ItemInstDefn ^ id_context
    ;
        Item = item_mode_defn(ItemModeDefn),
        Context = ItemModeDefn ^ md_context
    ;
        Item = item_pred_decl(ItemPredDecl),
        Context = ItemPredDecl ^ pf_context
    ;
        Item = item_mode_decl(ItemModeDecl),
        Context = ItemModeDecl ^ pfm_context
    ;
        Item = item_foreign_proc(ItemForeignProc),
        Context = ItemForeignProc ^ proc_context
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        Context = ItemForeignEnum ^ fe_context
    ;
        Item = item_foreign_export_enum(ItemForeignExportEnum),
        Context = ItemForeignExportEnum ^ fee_context
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        Context = get_decl_pragma_context(ItemDeclPragma)
    ;
        Item = item_decl_marker(ItemDeclMarker),
        Context = ItemDeclMarker ^ dm_context
    ;
        Item = item_impl_pragma(ItemImplPragma),
        Context = get_impl_pragma_context(ItemImplPragma)
    ;
        Item = item_impl_marker(ItemImplMarker),
        Context = ItemImplMarker ^ im_context
    ;
        Item = item_generated_pragma(ItemGenPragma),
        Context = get_gen_pragma_context(ItemGenPragma)
    ;
        Item = item_promise(ItemPromise),
        Context = ItemPromise ^ prom_context
    ;
        Item = item_typeclass(ItemTypeClass),
        Context = ItemTypeClass ^ tc_context
    ;
        Item = item_instance(ItemInstance),
        Context = ItemInstance ^ ci_context
    ;
        Item = item_initialise(ItemInitialise),
        Context = ItemInitialise ^ init_context
    ;
        Item = item_finalise(ItemFinalise),
        Context = ItemFinalise ^ final_context
    ;
        Item = item_mutable(ItemMutable),
        Context = ItemMutable ^ mut_context
    ;
        Item = item_type_repn(ItemTypeRepn),
        Context = ItemTypeRepn ^ tr_context
    ).

get_goal_context(Goal) = Context :-
    ( Goal = conj_expr(Context, _, _)
    ; Goal = par_conj_expr(Context, _, _)
    ; Goal = true_expr(Context)
    ; Goal = disj_expr(Context, _, _, _)
    ; Goal = fail_expr(Context)
    ; Goal = quant_expr(_, _, Context, _, _)
    ; Goal = promise_purity_expr(Context, _, _)
    ; Goal = promise_equivalent_solutions_expr(Context, _, _, _, _, _)
    ; Goal = promise_equivalent_solution_sets_expr(Context, _, _, _, _, _)
    ; Goal = promise_equivalent_solution_arbitrary_expr(Context, _, _, _, _, _)
    ; Goal = require_detism_expr(Context, _, _)
    ; Goal = require_complete_switch_expr(Context, _, _)
    ; Goal = require_switch_arms_detism_expr(Context, _, _, _)
    ; Goal = disable_warnings_expr(Context, _, _, _)
    ; Goal = trace_expr(Context, _, _, _, _, _)
    ; Goal = atomic_expr(Context, _, _, _, _, _)
    ; Goal = try_expr(Context, _, _, _, _, _, _)
    ; Goal = implies_expr(Context, _, _)
    ; Goal = equivalent_expr(Context, _, _)
    ; Goal = not_expr(Context, _)
    ; Goal = if_then_else_expr(Context, _, _, _, _, _)
    ; Goal = event_expr(Context, _, _)
    ; Goal = call_expr(Context, _, _, _)
    ; Goal = unify_expr(Context, _, _, _)
    ).

%---------------------------------------------------------------------------%

get_declared_types_and_maybe_modes(TypesAndMaybeModes, WithInst, _MaybeDetism,
        Types, MaybeModes) :-
    (
        TypesAndMaybeModes = no_types_arity_zero,
        Types = [],
        ( if
            WithInst = no
            % This test is commented out, for the reason explained
            % in the comment on the declaration of this predicate.
            % MaybeDetism = no
        then
            MaybeModes = no
        else
            MaybeModes = yes([])
        )
    ;
        TypesAndMaybeModes = types_only(Types),
        MaybeModes = no
    ;
        TypesAndMaybeModes = types_and_modes(TypesAndModes),
        split_types_and_modes(TypesAndModes, Types, Modes),
        MaybeModes = yes(Modes)
    ).

split_types_and_modes([], [], []).
split_types_and_modes([TM | TMs], [T | Ts], [M | Ms]) :-
    TM = type_and_mode(T, M),
    split_types_and_modes(TMs, Ts, Ms).

types_and_maybe_modes_arity(TypesAndMaybeModes) = PredFormArity :-
    (
        TypesAndMaybeModes = no_types_arity_zero,
        PredFormArity = pred_form_arity(0)
    ;
        TypesAndMaybeModes = types_only(Types),
        PredFormArity = arg_list_arity(Types)
    ;
        TypesAndMaybeModes = types_and_modes(TypesAndModes),
        PredFormArity = arg_list_arity(TypesAndModes)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_item.
%---------------------------------------------------------------------------%
