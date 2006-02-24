%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: prog_item.m.
% Main author: fjh.

% This module, together with prog_data, defines a data structure for
% representing Mercury programs.  

% This data structure specifies basically the same information as is
% contained in the source code, but in a parse tree rather than a flat
% file.  This module defines the parts of the parse tree that are *not*
% needed by the various compiler backends; parts of the parse tree that
% are needed by the backends are contained in prog_data.m.

%-----------------------------------------------------------------------------%

:- module parse_tree.prog_item.
:- interface.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module recompilation.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module set.
:- import_module std_util.
:- import_module term.

%-----------------------------------------------------------------------------%
%
% This is how programs (and parse errors) are represented
%

    % An error/warning message, and the term to which it relates.
    %
:- type message_list == list(pair(string, term)).

:- type compilation_unit
    --->    module(
                module_name,
                item_list
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
    
    ;       foreign_imports.
            % The compiler sometimes needs to insert additional foreign_import
            % pragmas. XXX Why?

:- type item_list == list(item_and_context).

:- type item_and_context == pair(item, prog_context).

:- type item
    --->    clause(
                cl_origin                       :: item_origin,
                cl_varset                       :: prog_varset,
                cl_pred_or_func                 :: pred_or_func,
                cl_predname                     :: sym_name,
                cl_head_args                    :: list(prog_term),
                cl_body                         :: goal
            )

            % `:- type ...':
            % a definition of a type, or a declaration of an abstract type.
    ;       type_defn(
                td_tvarset                      :: tvarset,
                td_ctor_name                    :: sym_name,
                td_ctor_args                    :: list(type_param),
                td_ctor_defn                    :: type_defn,
                td_cond                         :: condition
            )

            % `:- inst ... = ...':
            % a definition of an inst.
    ;       inst_defn(
                id_varset                       :: inst_varset,
                id_inst_name                    :: sym_name,
                id_inst_args                    :: list(inst_var),
                id_inst_defn                    :: inst_defn,
                id_cond                         :: condition
            )

            % `:- mode ... = ...':
            % a definition of a mode.
    ;       mode_defn(
                md_varset                       :: inst_varset,
                md_mode_name                    :: sym_name,
                md_mode_args                    :: list(inst_var),
                md_mode_defn                    :: mode_defn,
                md_cond                         :: condition
            )

    ;       module_defn(
                module_defn_varset              :: prog_varset,
                module_defn_module_defn         :: module_defn
            )

            % `:- pred ...' or `:- func ...':
            % a predicate or function declaration.
            % This specifies the type of the predicate or function,
            % and it may optionally also specify the mode and determinism.
    ;       pred_or_func(
                pf_tvarset                      :: tvarset,
                pf_instvarset                   :: inst_varset,
                pf_existqvars                   :: existq_tvars,
                pf_which                        :: pred_or_func,
                pf_name                         :: sym_name,
                pf_arg_decls                    :: list(type_and_mode),
                pf_maybe_with_type              :: maybe(mer_type),
                pf_maybe_with_inst              :: maybe(mer_inst),
                pf_maybe_detism                 :: maybe(determinism),
                pf_cond                         :: condition,
                pf_purity                       :: purity,
                pf_class_context                :: prog_constraints
            )
            %   The WithType and WithInst fields hold the `with_type`
            %   and `with_inst` annotations, which are syntactic
            %   sugar that is expanded by equiv_type.m
            %   equiv_type.m will set these fields to `no'.

            % `:- mode ...':
            % a mode declaration for a predicate or function.
    ;       pred_or_func_mode(
                pfm_instvarset                  :: inst_varset,
                pfm_which                       :: maybe(pred_or_func),
                pfm_name                        :: sym_name,
                pfm_arg_modes                   :: list(mer_mode),
                pfm_maybe_with_inst             :: maybe(mer_inst),
                pfm_maybe_detism                :: maybe(determinism),
                pfm_cond                        :: condition
            )
            %   The WithInst field holds the `with_inst` annotation,
            %   which is syntactic sugar that is expanded by
            %   equiv_type.m. equiv_type.m will set the field to `no'.

    ;       pragma(
                pragma_origin                   :: item_origin,
                pragma_type                     :: pragma_type
            )

    ;       promise(
                prom_type                       :: promise_type,
                prom_clause                     :: goal,
                prom_varset                     :: prog_varset,
                prom_univ_quant_vars            :: prog_vars
            )

    ;       typeclass(
                tc_constraints                  :: list(prog_constraint),
                tc_fundeps                      :: list(prog_fundep),
                tc_class_name                   :: class_name,
                tc_class_params                 :: list(tvar),
                tc_class_methods                :: class_interface,
                tc_varset                       :: tvarset
            )

    ;       instance(
                ci_deriving_class               :: list(prog_constraint),
                ci_class_name                   :: class_name,
                ci_types                        :: list(mer_type),
                ci_method_instances             :: instance_body,
                ci_varset                       :: tvarset,
                ci_module_containing_instance   :: module_name
            )

            % :- initialise pred_name.
    ;       initialise(
                item_origin,
                sym_name,
                arity
            )
    
            % :- finalise pred_name.
    ;       finalise(
                item_origin,
                sym_name,
                arity
            )
    
            % :- mutable(var_name, type, inst, value, attrs).
    ;       mutable(
                mut_name                        :: string,
                mut_type                        :: mer_type,
                mut_init_value                  :: prog_term,
                mut_inst                        :: mer_inst,
                mut_attrs                       :: mutable_var_attributes 
            )

            % Used for items that should be ignored (for the
            % purposes of backwards compatibility etc).
    ;       nothing(
                nothing_maybe_warning           :: maybe(item_warning)
            ).

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
    --->    pred_or_func(
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

    ;       pred_or_func_mode(
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
:- type trailed
    --->    trailed
    ;       untrailed.

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
:- func mutable_var_thread_safe(mutable_var_attributes) = thread_safe.
:- func mutable_var_trailed(mutable_var_attributes) = trailed.
:- func mutable_var_maybe_foreign_names(mutable_var_attributes)
	= maybe(list(foreign_name)).
:- func mutable_var_attach_to_io_state(mutable_var_attributes) = bool.

:- pred set_mutable_var_thread_safe(thread_safe::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_trailed(trailed::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_add_foreign_name(foreign_name::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_attach_to_io_state(bool::in,
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
    --->    foreign_decl(
                decl_lang               :: foreign_language,
                decl_is_local           :: foreign_decl_is_local,
                decl_decl               :: string
            )

    ;       foreign_code(
                code_lang               :: foreign_language,
                code_code               :: string
            )

    ;       foreign_proc(
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

    ;       foreign_import_module(
                imp_lang                :: foreign_language,
                imp_module              :: module_name
                % Equivalent to
                % `:- pragma foreign_decl(Lang, "#include <module>.h").'
                % except that the name of the header file is not
                % hard-coded, and mmake can use the dependency information.
            )

    ;       export(
                exp_predname            :: sym_name,
                exp_p_or_f              :: pred_or_func,
                exp_modes               :: list(mer_mode),
                exp_foreign_name        :: string
                % Predname, Predicate/function, Modes, foreign function name.
            )

    ;       import(
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
    %
    % Optimization pragmas
    %
    ;       type_spec(
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

    ;       inline(
                inline_name             :: sym_name,
                inline_arity            :: arity
                % Predname, Arity
            )

    ;       no_inline(
                noinline_name           :: sym_name,
                noinline_arity          :: arity
                % Predname, Arity
            )

    ;       unused_args(
                unused_p_or_f           :: pred_or_func,
                unused_name             :: sym_name,
                unused_arity            :: arity,
                unused_mode             :: mode_num,
                unused_args             :: list(int)
                % PredName, Arity, Mode number, Removed arguments.
                % Used for intermodule unused argument removal, should only
                % appear in .opt files.
            )

    ;       exceptions(
                exceptions_p_or_f       :: pred_or_func,
                exceptions_name         :: sym_name,
                exceptions_arity        :: arity,
                exceptions_mode         :: mode_num,
                exceptions_status       :: exception_status
                % PredName, Arity, Mode number, Exception status.
                % Should only appear in `.opt' or `.trans_opt' files.
            )

    ;       trailing_info(
                trailing_info_p_or_f    :: pred_or_func,
                trailing_info_name      :: sym_name,
                trailing_info_arity     :: arity,
                trailing_info_mode      :: mode_num,
                trailing_info_status    :: trailing_status
            )
                % PredName, Arity, Mode number, Trailing status.
                % Should on appear in `.opt' or `.trans_opt' files.
    
    %
    % Diagnostics pragmas (pragmas related to compiler warnings/errors)
    %

    ;       obsolete(
                obsolete_name           :: sym_name,
                obsolete_arity          :: arity
                % Predname, Arity
            )

    ;       source_file(
                source_file             :: string
                % Source file name.
            )

    %
    % Evaluation method pragmas
    %

    ;       tabled(
                tabled_method           :: eval_method,
                tabled_name             :: sym_name,
                tabled_arity            :: int,
                tabled_p_or_f           :: maybe(pred_or_func),
                tabled_mode             :: maybe(list(mer_mode))
                % Tabling type, Predname, Arity, PredOrFunc?, Mode?
            )

    ;       fact_table(
                fact_table_name         :: sym_name,
                fact_table_arity        :: arity,
                fact_table_file         :: string
                % Predname, Arity, Fact file name.
            )

    ;       reserve_tag(
                restag_type             :: sym_name,
                restag_arity            :: arity
                % Typename, Arity
            )

    %
    % Purity pragmas
    %

    ;       promise_pure(
                pure_name               :: sym_name,
                pure_arity              :: arity
                % Predname, Arity
            )

    ;       promise_semipure(
                semipure_name           :: sym_name,
                semipure_arity          :: arity
                % Predname, Arity
            )

    %
    % Termination analysis pragmas
    %

    ;       termination_info(
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

    ;       termination2_info(
                terminfo2_p_or_f        :: pred_or_func, 
                terminfo2_name          :: sym_name, 
                terminfo2_mode          :: list(mer_mode),
                terminfo2_args          :: maybe(pragma_constr_arg_size_info),
                terminfo2_args2         :: maybe(pragma_constr_arg_size_info),
                terminfo2_term          :: maybe(pragma_termination_info)
            )

    ;       terminates(
                term_name               :: sym_name,
                term_arity              :: arity
                % Predname, Arity
            )

    ;       does_not_terminate(
                noterm_name             :: sym_name,
                noterm_arity            :: arity
                % Predname, Arity
            )

    ;       check_termination(
                checkterm_name          :: sym_name,
                checkterm_arity         :: arity
                % Predname, Arity
            )

    ;       mode_check_clauses(
                mode_check_clause_name  :: sym_name,
                mode_check_clause_arity :: arity
            )

    %
    % CTGC pragma: structure sharing / structure reuse analysis.
    %
    ;
            structure_sharing(
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

    .

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
    --->    (goal , goal)   % (non-empty) conjunction
    ;       true            % empty conjunction
    ;       {goal & goal}   % parallel conjunction
                            % (The curly braces just quote the '&'/2.)

    % disjunctions
    ;       {goal ; goal}   % (non-empty) disjunction
                            % (The curly braces just quote the ';'/2.)
    ;       fail            % empty disjunction

    % quantifiers
    ;       { some(prog_vars, goal) }
                            % existential quantification
                            % (The curly braces just quote the 'some'/2.)
    ;       all(prog_vars, goal)
                            % % universal quantification
    ;       some_state_vars(prog_vars, goal)
    ;       all_state_vars(prog_vars, goal)
                            % state variables extracted from
                            % some/2 and all/2 quantifiers.

    % other scopes
    ;       promise_purity(implicit_purity_promise, purity, goal)
    ;       promise_equivalent_solutions(prog_vars, prog_vars, prog_vars, goal)
                            % (OrdinaryVars, DotStateVars, ColonStateVars,
                            % % Goal)

    % implications
    ;       implies(goal, goal)
                            % A => B
    ;       equivalent(goal, goal)
                            % A <=> B

    % negation and if-then-else
    ;       not(goal)
    ;       if_then(prog_vars, prog_vars, goal, goal)
                            % if_then(SomeVars, StateVars, If, Then)
    ;       if_then_else(prog_vars, prog_vars, goal, goal, goal)
                            % if_then_else(SomeVars, StateVars, If, Then, Else)

    % atomic goals
    ;       call(sym_name, list(prog_term), purity)
    ;       unify(prog_term, prog_term, purity).

%-----------------------------------------------------------------------------%
%
% Module system
%
    
    % This is how module-system declarations (such as imports
    % and exports) are represented.
    %
:- type module_defn
    --->    module(module_name)
    ;       end_module(module_name)

    ;       interface
    ;       implementation

    ;       private_interface
            % This is used internally by the compiler, to identify items
            % which originally came from an implementation section for a
            % module that contains sub-modules; such items need to be exported
            % to the sub-modules.

    ;       imported(import_locn)
            % This is used internally by the compiler, to identify declarations
            % which originally came from some other module imported with a
            % `:- import_module' declaration, and which section the module
            % was imported.

    ;       used(import_locn)
            % This is used internally by the compiler, to identify declarations
            % which originally came from some other module and for which all
            % uses must be module qualified. This applies to items from modules
            % imported using `:- use_module', and items from `.opt' and `.int2'
            % files. It also records from which section the module was
            % imported.

    ;       abstract_imported
            % This is used internally by the compiler, to identify items which
            % originally came from the implementation section of an interface
            % file; usually type declarations (especially equivalence types)
            % which should be used in code generation but not in type checking.

    ;       opt_imported
            % This is used internally by the compiler, to identify items which
            % originally came from a .opt file.

    ;       transitively_imported
            % This is used internally by the compiler, to identify items which
            % originally came from a `.opt' or `.int2' file. These should not
            % be allowed to match items in the current module. Note that unlike
            % `:- interface', `:- implementation' and the other
            % pseudo-declarations `:- imported(interface)', etc., a
            % `:- transitively_imported' declaration applies to all of the
            % following items in the list, not just up to the next
            % pseudo-declaration.

    ;       external(maybe(backend), sym_name_specifier)

    ;       export(sym_list)
    ;       import(sym_list)
    ;       use(sym_list)

    ;       include_module(list(module_name))

    ;       version_numbers(module_name, recompilation__version_numbers).
            % This is used to represent the version numbers of items in an
            % interface file for use in smart recompilation.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%
% 
% Mutable variables
%

    % Attributes for mutable variables.
    %
:- type mutable_var_attributes
    --->    mutable_var_attributes(
                mutable_trailed            :: trailed,
                mutable_thread_safe        :: thread_safe,
                mutable_foreign_names      :: maybe(list(foreign_name)),
                mutable_attach_to_io_state :: bool
            ).

default_mutable_attributes =
	mutable_var_attributes(trailed, not_thread_safe, no, no).

mutable_var_thread_safe(MVarAttrs) = MVarAttrs ^ mutable_thread_safe.
mutable_var_trailed(MVarAttrs) = MVarAttrs ^ mutable_trailed.
mutable_var_maybe_foreign_names(MVarAttrs) = MVarAttrs ^ mutable_foreign_names.
mutable_var_attach_to_io_state(MVarAttrs) =
    MVarAttrs ^ mutable_attach_to_io_state.

set_mutable_var_thread_safe(ThreadSafe, !Attributes) :-
    !:Attributes = !.Attributes ^ mutable_thread_safe := ThreadSafe.
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
	!:Attributes =
        !.Attributes ^ mutable_attach_to_io_state := AttachToIOState.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
