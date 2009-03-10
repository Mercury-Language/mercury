%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_item.m.
% Main author: fjh.
%
% This module, together with prog_data, defines a data structure for
% representing Mercury programs.
%
% This data structure specifies basically the same information as is
% contained in the source code, but in a parse tree rather than a flat
% file.  This module defines the parts of the parse tree that are *not*
% needed by the various compiler backends; parts of the parse tree that
% are needed by the backends are contained in prog_data.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_item.
:- interface.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module recompilation.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%
%
% This is how programs (and parse errors) are represented.
%
% The sequence number fields in the item-kind-specific types are intended to
% allow the recreation of the original item sequence after we have processed
% it into more complex data structures. Negative sequence numbers represent
% items that were not in the original read-in sequence, but which were added
% by the compiler. It is possible for two items to have the same sequence
% number if one original item (e.g. one that imports two or more modules)
% is later split apart (e.g. into several items that each import only one
% module).
%

:- type compilation_unit
    --->    unit_module(
                module_name,
                list(item)
            ).

    % Did an item originate in user code or was it added by the compiler as
    % part of a source-to-source transformation, e.g.  the initialise
    % declarations.
    %
:- type item_origin
    --->    user
    ;       compiler(item_compiler_origin).

    % For items introduced by the compiler, why were they introduced?
    %
:- type item_compiler_origin
    --->    initialise_decl
            % The item was introduced by the transformation for `:- initialise'
            % decls. This should only apply to export pragms.

    ;       finalise_decl
            % This item was introduced by the transformation for `:- finalise'
            % decls. This should only apply to export pragmas.

    ;       mutable_decl
            % The item was introduced by the transformation for `:- mutable'
            % decls. This should only apply to `:- initialise' decls and
            % export pragmas.

    ;       solver_type
            % Solver types cause the compiler to create foreign procs for the
            % init and representation functions.

    ;       pragma_memo_attribute
            % This item was introduced for an attribute given in a pragma memo.

    ;       foreign_imports.
            % The compiler sometimes needs to insert additional foreign_import
            % pragmas. XXX Why?

:- type item
    --->    item_module_defn(item_module_defn_info)
    ;       item_clause(item_clause_info)
    ;       item_type_defn(item_type_defn_info)
    ;       item_inst_defn(item_inst_defn_info)
    ;       item_mode_defn(item_mode_defn_info)
    ;       item_pred_decl(item_pred_decl_info)
    ;       item_mode_decl(item_mode_decl_info)
    ;       item_pragma(item_pragma_info)
    ;       item_promise(item_promise_info)
    ;       item_typeclass(item_typeclass_info)
    ;       item_instance(item_instance_info)
    ;       item_initialise(item_initialise_info)
    ;       item_finalise(item_finalise_info)
    ;       item_mutable(item_mutable_info)
    ;       item_nothing(item_nothing_info).

:- type item_module_defn_info
    --->    item_module_defn_info(
                module_defn_module_defn         :: module_defn,
                module_defn_context             :: prog_context,
                module_defn_seq_num             :: int
            ).

:- type item_clause_info
    --->    item_clause_info(
                cl_origin                       :: item_origin,
                cl_varset                       :: prog_varset,
                cl_pred_or_func                 :: pred_or_func,
                cl_predname                     :: sym_name,
                cl_head_args                    :: list(prog_term),
                cl_body                         :: goal,
                cl_context                      :: prog_context,
                cl_seq_num                      :: int
            ).

:- type item_type_defn_info
    --->    item_type_defn_info(
                % `:- type ...':
                % a definition of a type, or a declaration of an abstract type.

                td_tvarset                      :: tvarset,
                td_ctor_name                    :: sym_name,
                td_ctor_args                    :: list(type_param),
                td_ctor_defn                    :: type_defn,
                td_cond                         :: condition,
                td_context                      :: prog_context,
                td_seq_num                      :: int
            ).

:- type item_inst_defn_info
    --->    item_inst_defn_info(
                % `:- inst ... = ...':
                % a definition of an inst.
                id_varset                       :: inst_varset,
                id_inst_name                    :: sym_name,
                id_inst_args                    :: list(inst_var),
                id_inst_defn                    :: inst_defn,
                id_cond                         :: condition,
                id_context                      :: prog_context,
                id_seq_num                      :: int
            ).

:- type item_mode_defn_info
    --->    item_mode_defn_info(
                % `:- mode ... = ...':
                % a definition of a mode.
                md_varset                       :: inst_varset,
                md_mode_name                    :: sym_name,
                md_mode_args                    :: list(inst_var),
                md_mode_defn                    :: mode_defn,
                md_cond                         :: condition,
                md_context                      :: prog_context,
                md_seq_num                      :: int
            ).

:- type item_pred_decl_info
    --->    item_pred_decl_info(
                % `:- pred ...' or `:- func ...':
                % a predicate or function declaration.
                % This specifies the type of the predicate or function,
                % and it may optionally also specify the mode and determinism.
                pf_origin                       :: item_origin,
                pf_tvarset                      :: tvarset,
                pf_instvarset                   :: inst_varset,
                pf_existqvars                   :: existq_tvars,
                pf_which                        :: pred_or_func,
                pf_name                         :: sym_name,
                pf_arg_decls                    :: list(type_and_mode),
                % The next two fields hold the `with_type` and `with_inst`
                % annotations. This syntactic sugar is expanded out by
                % equiv_type.m, which will then set these fields to `no'.
                pf_maybe_with_type              :: maybe(mer_type),
                pf_maybe_with_inst              :: maybe(mer_inst),
                pf_maybe_detism                 :: maybe(determinism),
                pf_cond                         :: condition,
                pf_purity                       :: purity,
                pf_class_context                :: prog_constraints,
                pf_context                      :: prog_context,
                pf_seq_num                      :: int
            ).

:- type item_mode_decl_info
    --->    item_mode_decl_info(
                % `:- mode ...':
                % a mode declaration for a predicate or function.
                pfm_instvarset                  :: inst_varset,
                pfm_which                       :: maybe(pred_or_func),
                pfm_name                        :: sym_name,
                pfm_arg_modes                   :: list(mer_mode),
                % The next field holds the `with_inst` annotation. This
                % syntactic sugar is expanded by equiv_type.m, which will
                % then set the field to `no'.
                pfm_maybe_with_inst             :: maybe(mer_inst),
                pfm_maybe_detism                :: maybe(determinism),
                pfm_cond                        :: condition,
                pfm_context                     :: prog_context,
                pfm_seq_num                     :: int
            ).

:- type item_pragma_info
    --->    item_pragma_info(
                pragma_origin                   :: item_origin,
                pragma_type                     :: pragma_type,
                pragma_context                  :: prog_context,
                pragma_seq_num                  :: int
            ).

:- type item_promise_info
    --->    item_promise_info(
                prom_type                       :: promise_type,
                prom_clause                     :: goal,
                prom_varset                     :: prog_varset,
                prom_univ_quant_vars            :: prog_vars,
                prom_context                    :: prog_context,
                prom_seq_num                    :: int
            ).

:- type item_typeclass_info
    --->    item_typeclass_info(
                tc_constraints                  :: list(prog_constraint),
                tc_fundeps                      :: list(prog_fundep),
                tc_class_name                   :: class_name,
                tc_class_params                 :: list(tvar),
                tc_class_methods                :: class_interface,
                tc_varset                       :: tvarset,
                tc_context                      :: prog_context,
                tc_seq_num                      :: int
            ).

:- type item_instance_info
    --->    item_instance_info(
                ci_deriving_class               :: list(prog_constraint),
                ci_class_name                   :: class_name,
                ci_types                        :: list(mer_type),
                ci_method_instances             :: instance_body,
                ci_varset                       :: tvarset,
                ci_module_containing_instance   :: module_name,
                ci_context                      :: prog_context,
                ci_seq_num                      :: int
            ).

:- type item_initialise_info
    --->    item_initialise_info(
                % :- initialise pred_name.
                init_origin                     :: item_origin,
                init_name                       :: sym_name,
                init_arity                      :: arity,
                init_context                    :: prog_context,
                init_seq_num                    :: int
            ).

:- type item_finalise_info
    --->    item_finalise_info(
                % :- finalise pred_name.
                final_origin                    :: item_origin,
                final_name                      :: sym_name,
                final_arity                     :: arity,
                final_context                   :: prog_context,
                final_seq_num                   :: int
            ).

:- type item_mutable_info
    --->    item_mutable_info(
                % :- mutable(var_name, type, inst, value, attrs).
                mut_name                        :: string,
                mut_type                        :: mer_type,
                mut_init_value                  :: prog_term,
                mut_inst                        :: mer_inst,
                mut_attrs                       :: mutable_var_attributes,
                mut_varset                      :: prog_varset,
                mut_context                     :: prog_context,
                mut_seq_num                     :: int
            ).

:- type item_nothing_info
    --->    item_nothing_info(
                % Used for items that should be ignored (for purposes of
                % backwards compatibility etc).
                % XXX Instead of maybe(item_warning), this should be
                % maybe(error_spec).
                nothing_maybe_warning           :: maybe(item_warning),
                nothing_context                 :: prog_context,
                nothing_seq_num                 :: int
            ).

:- func get_item_context(item) = prog_context.

:- type item_warning
    --->    item_warning(
                maybe(option),  % Option controlling whether the
                                % warning should be reported.
                string,         % The warning.
                term            % The term to which it relates.
            ).

%-----------------------------------------------------------------------------%
%
% Type classes
%

    % The name class_method is a slight misnomer; this type actually represents
    % any declaration that occurs in the body of a type class definition.
    % Such declarations may either declare class methods, or they may declare
    % modes of class methods.
    %
:- type class_method
    --->    method_pred_or_func(
                % pred_or_func(...) here represents a `pred ...' or `func ...'
                % declaration in a type class body, which declares
                % a predicate or function method.  Such declarations
                % specify the type of the predicate or function,
                % and may optionally also specify the mode and determinism.

                tvarset,            % type variables
                inst_varset,        % inst variables
                existq_tvars,       % existentially quantified
                                    % type variables
                pred_or_func,
                sym_name,           % name of the pred or func
                list(type_and_mode),% the arguments' types and modes
                maybe(mer_type),    % any `with_type` annotation
                maybe(mer_inst),    % any `with_inst` annotation
                maybe(determinism), % any determinism declaration
                condition,          % any attached declaration
                purity,             % any purity annotation
                prog_constraints,   % the typeclass constraints on
                                    % the declaration
                prog_context        % the declaration's context
            )

    ;       method_pred_or_func_mode(
                % pred_or_func_mode(...) here represents a `mode ...'
                % declaration in a type class body.  Such a declaration
                % declares a mode for one of the type class methods.

                inst_varset,        % inst variables
                maybe(pred_or_func),% whether the method is a pred
                                    % or a func; for declarations
                                    % using `with_inst`, we don't
                                    % know which until we've
                                    % expanded the inst.
                sym_name,           % the method name
                list(mer_mode),     % the arguments' modes
                maybe(mer_inst),    % any `with_inst` annotation
                maybe(determinism), % any determinism declaration
                condition,          % any attached condition
                prog_context        % the declaration's context
            ).

:- type class_methods == list(class_method).

%-----------------------------------------------------------------------------%
%
% Mutable variables
%

    % Indicates if updates to the mutable are trailed or untrailed.
    %
:- type mutable_trailed
    --->    mutable_trailed
    ;       mutable_untrailed.

    % Indicates if a mutable is thread-local or not.
    %
:- type mutable_thread_local
    --->    mutable_thread_local
    ;       mutable_not_thread_local.

    % Has the user specified a name for us to use on the target code side
    % of the FLI?
    %
:- type foreign_name
    --->    foreign_name(
                foreign_name_lang :: foreign_language,
                foreign_name_name :: string
            ).

    % An abstract type for representing a set of mutable variable
    % attributes.
    %
:- type mutable_var_attributes.

    % Return the default attributes for a mutable variable.
    %
:- func default_mutable_attributes = mutable_var_attributes.

    % Access functions for the `mutable_var_attributes' structure.
    %
:- func mutable_var_trailed(mutable_var_attributes) = mutable_trailed.
:- func mutable_var_maybe_foreign_names(mutable_var_attributes)
    = maybe(list(foreign_name)).
:- func mutable_var_constant(mutable_var_attributes) = bool.
:- func mutable_var_attach_to_io_state(mutable_var_attributes) = bool.
:- func mutable_var_thread_local(mutable_var_attributes)
    = mutable_thread_local.

:- pred set_mutable_var_trailed(mutable_trailed::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_add_foreign_name(foreign_name::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_attach_to_io_state(bool::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_constant(bool::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_thread_local(mutable_thread_local::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

%-----------------------------------------------------------------------------%
%
% Pragmas
%

:- type pragma_type
    %
    % Foreign language interfacing pragmas
    %
            % A foreign language declaration, such as C header code.
    --->    pragma_foreign_decl(
                decl_lang               :: foreign_language,
                decl_is_local           :: foreign_decl_is_local,
                decl_decl               :: string
            )

    ;       pragma_foreign_code(
                code_lang               :: foreign_language,
                code_code               :: string
            )

    ;       pragma_foreign_proc(
                proc_attrs              :: pragma_foreign_proc_attributes,
                proc_name               :: sym_name,
                proc_p_or_f             :: pred_or_func,
                proc_vars               :: list(pragma_var),
                proc_varset             :: prog_varset,
                proc_instvarset         :: inst_varset,
                proc_impl               :: pragma_foreign_code_impl
                % Set of foreign proc attributes, eg.:
                %   what language this code is in
                %   whether or not the code may call Mercury,
                %   whether or not the code is thread-safe
                % PredName, Predicate or Function, Vars/Mode,
                % VarNames, Foreign Code Implementation Info
            )

    ;       pragma_foreign_import_module(
                imp_lang                :: foreign_language,
                imp_module              :: module_name
                % Equivalent to
                % `:- pragma foreign_decl(Lang, "#include <module>.h").'
                % except that the name of the header file is not
                % hard-coded, and mmake can use the dependency information.
            )

    ;       pragma_foreign_export(
                exp_language            :: foreign_language,
                exp_predname            :: sym_name,
                exp_p_or_f              :: pred_or_func,
                exp_modes               :: list(mer_mode),
                exp_foreign_name        :: string
                % Predname, Predicate/function, Modes, foreign function name.
            )

    ;       pragma_import(
                import_pred_name        :: sym_name,
                import_p_or_f           :: pred_or_func,
                import_modes            :: list(mer_mode),
                import_attrs            :: pragma_foreign_proc_attributes,
                import_foreign_name     :: string
                % Predname, Predicate/function, Modes,
                % Set of foreign proc attributes, eg.:
                %    whether or not the foreign code may call Mercury,
                %    whether or not the foreign code is thread-safe
                % foreign function name.
            )
    ;
            pragma_foreign_export_enum(
                export_enum_language   :: foreign_language,
                export_enum_type_name  :: sym_name,
                export_enum_type_arity :: arity,
                export_enum_attributes :: export_enum_attributes,
                export_enum_overrides  :: assoc_list(sym_name, string)
            )
    ;
            pragma_foreign_enum(
                foreign_enum_language   :: foreign_language,
                foreign_enum_type_name  :: sym_name,
                foreign_enum_type_arity :: arity,
                foreign_enum_values     :: assoc_list(sym_name, string)
            )
    %
    % Optimization pragmas
    %
    ;       pragma_type_spec(
                tspec_pred_name         :: sym_name,
                tspec_new_name          :: sym_name,
                tspec_arity             :: arity,
                tspec_p_or_f            :: maybe(pred_or_func),
                tspec_modes             :: maybe(list(mer_mode)),
                tspec_tsubst            :: type_subst,
                tspec_tvarset           :: tvarset,
                tspec_items             :: set(item_id)
                % PredName, SpecializedPredName, Arity, PredOrFunc,
                % Modes if a specific procedure was specified, type
                % substitution (using the variable names from the pred
                % declaration), TVarSet, Equivalence types used
            )

    ;       pragma_inline(
                inline_name             :: sym_name,
                inline_arity            :: arity
                % Predname, Arity
            )

    ;       pragma_no_inline(
                noinline_name           :: sym_name,
                noinline_arity          :: arity
                % Predname, Arity
            )

    ;       pragma_unused_args(
                unused_p_or_f           :: pred_or_func,
                unused_name             :: sym_name,
                unused_arity            :: arity,
                unused_mode             :: mode_num,
                unused_args             :: list(int)
                % PredName, Arity, Mode number, Removed arguments.
                % Used for intermodule unused argument removal, should only
                % appear in .opt files.
            )

    ;       pragma_exceptions(
                exceptions_p_or_f       :: pred_or_func,
                exceptions_name         :: sym_name,
                exceptions_arity        :: arity,
                exceptions_mode         :: mode_num,
                exceptions_status       :: exception_status
                % PredName, Arity, Mode number, Exception status.
                % Should only appear in `.opt' or `.trans_opt' files.
            )

    ;       pragma_trailing_info(
                trailing_info_p_or_f    :: pred_or_func,
                trailing_info_name      :: sym_name,
                trailing_info_arity     :: arity,
                trailing_info_mode      :: mode_num,
                trailing_info_status    :: trailing_status
            )
                % PredName, Arity, Mode number, Trailing status.
                % Should on appear in `.opt' or `.trans_opt' files.

    ;       pragma_mm_tabling_info(
                mm_tabling_info_p_or_f  :: pred_or_func,
                mm_tabling_info_name    :: sym_name,
                mm_tabling_info_arity   :: arity,
                mm_tabling_info_mode    :: mode_num,
                mm_tabling_info_status  :: mm_tabling_status
            )
                % PredName, Arity, Mode number, MM Tabling status.
                % Should on appear in `.opt' or `.trans_opt' files.

    %
    % Diagnostics pragmas (pragmas related to compiler warnings/errors)
    %

    ;       pragma_obsolete(
                obsolete_name           :: sym_name,
                obsolete_arity          :: arity
                % Predname, Arity
            )

    ;       pragma_source_file(
                pragma_source_file      :: string
                % Source file name.
            )

    %
    % Evaluation method pragmas
    %

    ;       pragma_tabled(
                tabled_method           :: eval_method,
                tabled_name             :: sym_name,
                tabled_arity            :: int,
                tabled_p_or_f           :: maybe(pred_or_func),
                tabled_mode             :: maybe(list(mer_mode)),
                tabled_attributes       :: maybe(table_attributes)
                % Tabling type, Predname, Arity, PredOrFunc?, Mode?
            )

    ;       pragma_fact_table(
                fact_table_name         :: sym_name,
                fact_table_arity        :: arity,
                fact_table_file         :: string
                % Predname, Arity, Fact file name.
            )

    ;       pragma_reserve_tag(
                restag_type             :: sym_name,
                restag_arity            :: arity
                % Typename, Arity
            )

    %
    % Purity pragmas
    %

    ;       pragma_promise_equivalent_clauses(
                eqv_clauses_name        :: sym_name,
                eqv_clauses_arity       :: arity
                % Predname, Arity
            )

    ;       pragma_promise_pure(
                pure_name               :: sym_name,
                pure_arity              :: arity
                % Predname, Arity
            )

    ;       pragma_promise_semipure(
                semipure_name           :: sym_name,
                semipure_arity          :: arity
                % Predname, Arity
            )

    %
    % Termination analysis pragmas
    %

    ;       pragma_termination_info(
                terminfo_p_or_f         :: pred_or_func,
                terminfo_name           :: sym_name,
                terminfo_mode           :: list(mer_mode),
                terminfo_args           :: maybe(pragma_arg_size_info),
                terminfo_term           :: maybe(pragma_termination_info)
                % The list(mer_mode) is the declared argmodes of the
                % procedure, unless there are no declared argmodes, in which
                % case the inferred argmodes are used.  This pragma is used to
                % define information about a predicates termination
                % properties.  It is most useful where the compiler has
                % insufficient information to be able to analyse the
                % predicate.  This includes c_code, and imported predicates.
                % termination_info pragmas are used in opt and trans_opt
                % files.
            )

    ;       pragma_termination2_info(
                terminfo2_p_or_f        :: pred_or_func,
                terminfo2_name          :: sym_name,
                terminfo2_mode          :: list(mer_mode),
                terminfo2_args          :: maybe(pragma_constr_arg_size_info),
                terminfo2_args2         :: maybe(pragma_constr_arg_size_info),
                terminfo2_term          :: maybe(pragma_termination_info)
            )

    ;       pragma_terminates(
                term_name               :: sym_name,
                term_arity              :: arity
                % Predname, Arity
            )

    ;       pragma_does_not_terminate(
                noterm_name             :: sym_name,
                noterm_arity            :: arity
                % Predname, Arity
            )

    ;       pragma_check_termination(
                checkterm_name          :: sym_name,
                checkterm_arity         :: arity
                % Predname, Arity
            )

    ;       pragma_mode_check_clauses(
                mode_check_clause_name  :: sym_name,
                mode_check_clause_arity :: arity
            )

    %
    % CTGC pragmas: structure sharing / structure reuse analysis.
    %

    ;       pragma_structure_sharing(
                sharing_p_or_f          :: pred_or_func,
                sharing_name            :: sym_name,
                sharing_mode            :: list(mer_mode),
                sharing_headvars        :: prog_vars,
                sharing_headvartypes    :: list(mer_type),
                sharing_description     :: maybe(structure_sharing_domain)
            )
            % After structure sharing analysis, the compiler generates
            % structure sharing pragmas to be stored in and read from
            % optimization interface files.
            %
            % The list of modes consists of the declared argmodes (or inferred
            % argmodes if there are no declared ones).

    ;       pragma_structure_reuse(
                reuse_p_or_f          :: pred_or_func,
                reuse_name            :: sym_name,
                reuse_mode            :: list(mer_mode),
                reuse_headvars        :: prog_vars,
                reuse_headvartypes    :: list(mer_type),
                reuse_description     :: maybe(structure_reuse_domain)
            )
            % After reuse analysis, the compiler generates structure reuse
            % pragmas to be stored in and read from optimization interface
            % files.
            %
            % The list of modes consists of the declared argmodes (or inferred
            % argmodes if there are no declared ones).
            % The last sym_name (reuse_optimised_name) stores the name of the
            % optimised version of the exported predicate.

    ;       pragma_require_feature_set(
                rfs_feature_set :: set(required_feature)
            ).

:- inst pragma_type_spec == bound(pragma_type_spec(ground, ground, ground,
    ground, ground, ground, ground, ground)).

%-----------------------------------------------------------------------------%
%
% Goals
%

    % Here's how clauses and goals are represented.
    % a => b --> implies(a, b)
    % a <= b --> implies(b, a) [just flips the goals around!]
    % a <=> b --> equivalent(a, b)
    %
:- type goal == pair(goal_expr, prog_context).

:- type goals == list(goal).

:- type goal_expr
    % conjunctions
    --->    conj_expr(goal, goal)
                            % (non-empty) conjunction
    ;       true_expr       % empty conjunction
    ;       par_conj_expr(goal, goal)
                            % parallel conjunction

    % disjunctions
    ;       disj_expr(goal, goal)
                            % (non-empty) disjunction
    ;       fail_expr       % empty disjunction

    % quantifiers
    ;       some_expr(prog_vars, goal)
                            % existential quantification
    ;       all_expr(prog_vars, goal)
                            % universal quantification
    ;       some_state_vars_expr(prog_vars, goal)
    ;       all_state_vars_expr(prog_vars, goal)
                            % state variables extracted from
                            % some/2 and all/2 quantifiers.

    % other scopes
    ;       promise_purity_expr(purity, goal)
    ;       promise_equivalent_solutions_expr(
                prog_vars,  % OrdinaryVars
                prog_vars,  % DotStateVars
                prog_vars,  % ColonStateVars
                goal
            )
    ;       promise_equivalent_solution_sets_expr(
                prog_vars,  % OrdinaryVars
                prog_vars,  % DotStateVars
                prog_vars,  % ColonStateVars
                goal
            )
    ;       promise_equivalent_solution_arbitrary_expr(
                prog_vars,  % OrdinaryVars
                prog_vars,  % DotStateVars
                prog_vars,  % ColonStateVars
                goal
            )
    ;       trace_expr(
                texpr_compiletime   :: maybe(trace_expr(trace_compiletime)),
                texpr_runtime       :: maybe(trace_expr(trace_runtime)),
                texpr_maybe_io      :: maybe(prog_var),
                texpr_mutable_vars  :: list(trace_mutable_var),
                texpr_goal          :: goal
            )
    ;       atomic_expr(
                % Subgoals of the atomic goal are parsed into the following
                % datatype.  During the creation of the parse tree, all
                % subterms of the "orelse" operator are flattened and placed
                % into a list.  If this is the case, the first "orelse"
                % alternative is stored in "main_goal" whilst the other
                % alternatives are stored in "orelse_alternatives". If there
                % are no "or_else" operators within the atomic subgoal,
                % the subgoal is stored in "main_goal" whilst the
                % "orelse_alternatives" list remains empty.

                aexpr_outer         :: atomic_component_state,
                aexpr_inner         :: atomic_component_state,
                aexpr_output_vars   :: maybe(list(prog_var)),
                aexpr_main_goal     :: goal,    
                aexpr_orelse_goals  :: goals
            )
    ;       try_expr(
                tryexpr_maybe_io        :: maybe(prog_var),
                tryexpr_goal            :: goal,
                tryexpr_then            :: goal,
                tryexpr_maybe_else      :: maybe(goal),
                tryexpr_catches         :: list(catch_expr),
                tryexpr_maybe_catch_any :: maybe(catch_any_expr)
            )

    % implications
    ;       implies_expr(goal, goal)
                            % A => B
    ;       equivalent_expr(goal, goal)
                            % A <=> B

    % negation and if-then-else
    ;       not_expr(goal)
    ;       if_then_else_expr(prog_vars, prog_vars, goal, goal, goal)
                            % if_then_else(SomeVars, StateVars, If, Then, Else)

    % atomic goals
    ;       event_expr(string, list(prog_term))
    ;       call_expr(sym_name, list(prog_term), purity)
    ;       unify_expr(prog_term, prog_term, purity).

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

%-----------------------------------------------------------------------------%
%
% Module system
%

    % This is how module-system declarations (such as imports and exports)
    % are represented.
    %
:- type module_defn
    --->    md_module(module_name)
    ;       md_end_module(module_name)

    ;       md_interface
    ;       md_implementation

    ;       md_private_interface
            % This is used internally by the compiler, to identify items
            % which originally came from an implementation section for a
            % module that contains sub-modules; such items need to be exported
            % to the sub-modules.

    ;       md_imported(import_locn)
            % This is used internally by the compiler, to identify declarations
            % which originally came from some other module imported with a
            % `:- import_module' declaration, and which section the module
            % was imported.

    ;       md_used(import_locn)
            % This is used internally by the compiler, to identify declarations
            % which originally came from some other module and for which all
            % uses must be module qualified. This applies to items from modules
            % imported using `:- use_module', and items from `.opt' and `.int2'
            % files. It also records from which section the module was
            % imported.

    ;       md_abstract_imported
            % This is used internally by the compiler, to identify items which
            % originally came from the implementation section of an interface
            % file; usually type declarations (especially equivalence types)
            % which should be used in code generation but not in type checking.

    ;       md_opt_imported
            % This is used internally by the compiler, to identify items which
            % originally came from a .opt file.

    ;       md_transitively_imported
            % This is used internally by the compiler, to identify items which
            % originally came from a `.opt' or `.int2' file. These should not
            % be allowed to match items in the current module. Note that unlike
            % `:- interface', `:- implementation' and the other
            % pseudo-declarations `:- imported(interface)', etc., a
            % `:- transitively_imported' declaration applies to all of the
            % following items in the list, not just up to the next
            % pseudo-declaration.

    ;       md_external(maybe(backend), sym_name_specifier)

    ;       md_export(list(module_specifier))
    ;       md_import(list(module_specifier))
    ;       md_use(list(module_specifier))

    ;       md_include_module(list(module_name))

    ;       md_version_numbers(module_name, recompilation.version_numbers).
            % This is used to represent the version numbers of items in an
            % interface file for use in smart recompilation.

%-----------------------------------------------------------------------------%

:- type contains_foreign_code
    --->    contains_foreign_code(set(foreign_language))
    ;       contains_no_foreign_code
    ;       contains_foreign_code_unknown.

:- type contains_foreign_export
    --->    contains_foreign_export
    ;       contains_no_foreign_export.

:- pred get_item_list_foreign_code(globals::in, list(item)::in,
    set(foreign_language)::out, foreign_import_module_info_list::out,
    contains_foreign_export::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_foreign.

:- import_module map.

%-----------------------------------------------------------------------------%

get_item_context(Item) = Context :-
    (
        Item = item_module_defn(ItemModuleDefn),
        Context = ItemModuleDefn ^ module_defn_context
    ;
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
        Item = item_pragma(ItemPragma),
        Context = ItemPragma ^ pragma_context
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
        Item = item_nothing(ItemNothing),
        Context = ItemNothing ^ nothing_context
    ).

%-----------------------------------------------------------------------------%
%
% Mutable variables
%

    % Attributes for mutable variables.
    %
:- type mutable_var_attributes
    --->    mutable_var_attributes(
                mutable_trailed             :: mutable_trailed,
                mutable_foreign_names       :: maybe(list(foreign_name)),
                mutable_attach_to_io_state  :: bool,
                mutable_constant            :: bool,
                mutable_thread_local        :: mutable_thread_local
            ).

default_mutable_attributes =
    mutable_var_attributes(mutable_trailed, no, no, no,
        mutable_not_thread_local).

mutable_var_trailed(MVarAttrs) = MVarAttrs ^ mutable_trailed.
mutable_var_maybe_foreign_names(MVarAttrs) = MVarAttrs ^ mutable_foreign_names.
mutable_var_attach_to_io_state(MVarAttrs) =
    MVarAttrs ^ mutable_attach_to_io_state.
mutable_var_constant(MVarAttrs) = MVarAttrs ^ mutable_constant.
mutable_var_thread_local(MVarAttrs) = MVarAttrs ^ mutable_thread_local.

set_mutable_var_trailed(Trailed, !Attributes) :-
    !:Attributes = !.Attributes ^ mutable_trailed := Trailed.
set_mutable_add_foreign_name(ForeignName, !Attributes) :-
    MaybeForeignNames0 = !.Attributes ^ mutable_foreign_names,
    (
        MaybeForeignNames0 = no,
        MaybeForeignNames  = yes([ForeignName])
    ;
        MaybeForeignNames0 = yes(ForeignNames0),
        ForeignNames = [ ForeignName | ForeignNames0],
        MaybeForeignNames   = yes(ForeignNames)
    ),
    !:Attributes = !.Attributes ^ mutable_foreign_names := MaybeForeignNames.
set_mutable_var_attach_to_io_state(AttachToIOState, !Attributes) :-
    !:Attributes = !.Attributes ^ mutable_attach_to_io_state
        := AttachToIOState.
set_mutable_var_constant(Constant, !Attributes) :-
    !:Attributes = !.Attributes ^ mutable_constant := Constant.
set_mutable_var_thread_local(ThreadLocal, !Attributes) :-
    !:Attributes = !.Attributes ^ mutable_thread_local := ThreadLocal.

%-----------------------------------------------------------------------------%

:- type module_foreign_info
    --->    module_foreign_info(
                used_foreign_languages      :: set(foreign_language),
                foreign_proc_languages      :: map(sym_name, foreign_language),
                all_foreign_import_modules  :: foreign_import_module_info_list,
                module_has_foreign_export   :: contains_foreign_export
            ).

get_item_list_foreign_code(Globals, Items, LangSet, ForeignImports,
        ContainsPragmaExport) :-
    Info0 = module_foreign_info(set.init, map.init, [],
        contains_no_foreign_export),
    list.foldl(get_item_foreign_code(Globals), Items, Info0, Info),
    Info = module_foreign_info(LangSet0, LangMap, ForeignImports,
        ContainsPragmaExport),
    ForeignProcLangs = map.values(LangMap),
    LangSet = set.insert_list(LangSet0, ForeignProcLangs).

:- pred get_item_foreign_code(globals::in, item::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_item_foreign_code(Globals, Item, !Info) :-
    ( Item = item_pragma(ItemPragma) ->
        ItemPragma = item_pragma_info(_, Pragma, Context, _),
        do_get_item_foreign_code(Globals, Pragma, Context, !Info)
    ; Item = item_mutable(_) ->
        % Mutables introduce foreign_procs, but mutable declarations
        % won't have been expanded by the time we get here so we need
        % to handle them separately.
        % XXX mutables are currently only implemented for the C backends
        % but we should handle the Java/IL backends here as well.
        % (See do_get_item_foreign_code for details/5).
        !:Info = !.Info ^ used_foreign_languages :=
            set.insert(!.Info ^ used_foreign_languages, lang_c)
    ; ( Item = item_initialise(_) ; Item = item_finalise(_) ) ->
        % Intialise/finalise declarations introduce export pragmas, but
        % again they won't have been expanded by the time we get here.
        % XXX we don't currently support these on non-C backends.
        !:Info = !.Info ^ used_foreign_languages :=
            set.insert(!.Info ^ used_foreign_languages, lang_c),
        !:Info = !.Info ^ module_has_foreign_export := contains_foreign_export
    ;
        true
    ).

:- pred do_get_item_foreign_code(globals::in, pragma_type::in,
    prog_context::in, module_foreign_info::in, module_foreign_info::out)
    is det.

do_get_item_foreign_code(Globals, Pragma, Context, Info0, Info) :-
    globals.get_backend_foreign_languages(Globals, BackendLangs),
    globals.get_target(Globals, Target),

    % The code here should match the way that mlds_to_gcc.m decides whether
    % or not to call mlds_to_c.m.  XXX Note that we do NOT count foreign_decls
    % here. We only link in a foreign object file if mlds_to_gcc called
    % mlds_to_c.m to generate it, which it will only do if there is some
    % foreign_code, not just foreign_decls. Counting foreign_decls here
    % causes problems with intermodule optimization.
    (
        Pragma = pragma_foreign_code(Lang, _),
        list.member(Lang, BackendLangs)
    ->
        Info = Info0 ^ used_foreign_languages :=
            set.insert(Info0 ^ used_foreign_languages, Lang)
    ;
        Pragma = pragma_foreign_proc(Attrs, Name, _, _, _, _, _)
    ->
        NewLang = get_foreign_language(Attrs),
        ( OldLang = Info0 ^ foreign_proc_languages ^ elem(Name) ->
            % is it better than an existing one?
            (
                yes = prefer_foreign_language(Globals, Target,
                    OldLang, NewLang)
            ->
                Info = Info0 ^ foreign_proc_languages ^ elem(Name) := NewLang
            ;
                Info = Info0
            )
        ;
            % is it one of the languages we support?
            ( list.member(NewLang, BackendLangs) ->
                Info = Info0 ^ foreign_proc_languages ^ elem(Name) := NewLang
            ;
                Info = Info0
            )
        )
    ;
        % XXX `pragma export' should not be treated as foreign, but currently
        % mlds_to_gcc.m doesn't handle that declaration, and instead just
        % punts it on to mlds_to_c.m, thus generating C code for it,
        % rather than assembler code. So we need to treat `pragma export'
        % like the other pragmas for foreign code.
        Pragma = pragma_foreign_export(Lang, _, _, _, _),
        list.member(Lang, BackendLangs)
    ->
        Info1 = Info0 ^ used_foreign_languages :=
            set.insert(Info0 ^ used_foreign_languages, Lang),
        Info = Info1 ^ module_has_foreign_export :=
            contains_foreign_export
    ;
        Pragma = pragma_foreign_import_module(Lang, Import),
        list.member(Lang, BackendLangs)
    ->
        Info = Info0 ^ all_foreign_import_modules :=
            [foreign_import_module_info(Lang, Import, Context) |
                Info0 ^ all_foreign_import_modules]
    ;
        % We generate some C code for fact tables, so we need to treat modules
        % containing fact tables as if they contain foreign code.
        ( Target = target_asm
        ; Target = target_c
        ),
        Pragma = pragma_fact_table(_, _, _)
    ->
        Info = Info0 ^ used_foreign_languages :=
            set.insert(Info0 ^ used_foreign_languages, lang_c)
    ;
        Info = Info0
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
