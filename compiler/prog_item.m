%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
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
% contained in the source code, but in a parse tree rather than a flat file.
% This module defines the parts of the parse tree that are *not* needed
% by the various compiler backends; parts of the parse tree that
% are needed by the backends are contained in prog_data.m.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_item.
:- interface.

:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module recompilation.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.status.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module set.
:- import_module term.

%-----------------------------------------------------------------------------%
%
% The different kinds of files the Mercury compiler deals with:
%
% - source files,
% - automatically generated interface files, and
% - automatically generated optimization files.
%

:- type file_kind
    --->    fk_src
    ;       fk_int(int_file_kind)
    ;       fk_opt(opt_file_kind).

:- type src_file_kind
    --->    sfk_src.

:- type int_file_kind
    --->    ifk_int0
    ;       ifk_int3
    ;       ifk_int2
    ;       ifk_int.

:- type opt_file_kind
    --->    ofk_opt
    ;       ofk_trans_opt.

:- func file_kind_to_extension(file_kind) = string.
:- func int_file_kind_to_extension(int_file_kind) = string.
:- func opt_file_kind_to_extension(opt_file_kind) = string.

:- pred extension_to_file_kind(string::in, file_kind::out) is semidet.

%-----------------------------------------------------------------------------%
%
% The parse_tree_{src,int,opt} types define the ASTs we use for source files,
% interface files and optimization files respectively.
%
% Nested submodules may appear in source files, but not in interface files
% or optimization files.
%
% We use cords of items instead of lists of items where we may need to add
% items to an already-existing partial parse tree.
%
% The contexts of module and section declarations below may be
% term.context_init if the actual context isn't known, but if the recorded
% context is not term.context_init, then it is valid.

:- type parse_tree_src
    --->    parse_tree_src(
                module_name,
                prog_context,               % the context of the `:- module'
                cord(module_component)
            ).

:- type module_component
    --->    mc_section(
                module_section,
                prog_context,               % the context of the `:- interface'
                                            % or `:- implementation'
                cord(item)
            )
    ;       mc_nested_submodule(
                module_section,             % what section is the submodule in
                prog_context,               % the section's context
                parse_tree_src
            ).

:- type parse_tree_int
    --->    parse_tree_int(
                module_name,
                int_file_kind,
                prog_context,               % the context of the `:- module'
                list(item),                 % items in the interface section
                list(item)                  % items in the impl section
            ).

:- type parse_tree_opt
    --->    parse_tree_opt(
                module_name,
                opt_file_kind,
                prog_context,               % the context of the `:- module'
                list(item)
            ).

%-----------------------------------------------------------------------------%
%
% A raw compilation unit is one module to be compiled. A parse_tree_src
% that contains N nested submodules corresponds to 1 + N raw_compilation_units,
% one for the top level module and one for each (possibly deeply) nested
% submodule.
%
% A raw compilation unit consists of some raw item blocks, with each raw
% item block containing the items in an interface or implementation section
% of its module.
%
% Before we convert a raw compilation unit into the HLDS, we augment it
% with the contents of the interface files of the modules it imports
% (directly or indirectly), and if requested, with the contents of the
% optimization files of those modules as well. The augmented compilation unit
% will consist of the item blocks of the original raw compilation unit,
% followed by item blocks read from these other files. Each of those item
% blocks will have a aug_section_kind that indicates where it came from.
%
% As with the parse tree types above, the contexts in these types
% may be term.context_init if the actual context isn't known, but if the
% recorded context is not term.context_init, then it is valid.

:- type raw_compilation_unit == compilation_unit(module_section).
:- type aug_compilation_unit == compilation_unit(aug_module_section).

:- type raw_item_block == item_block(module_section).
:- type aug_item_block == item_block(aug_module_section).

:- type compilation_unit(MS)
    --->    compilation_unit(
                module_name,
                prog_context, % The context of the `:- module' declaration.
                list(item_block(MS))
            ).

:- type item_block(MS)
    --->    item_block(
                MS,
                prog_context,   % The context of the section marker.
                list(item)
            ).

%-----------------------------------------------------------------------------%

:- func compilation_unit_project_name(compilation_unit(MS)) = module_name.

:- pred cast_module_components_to_raw_item_blocks(list(module_component)::in,
    list(raw_item_block)::out) is det.

:- pred augment_block(raw_item_block::in, aug_item_block::out) is det.

:- pred separate_int_impl_items(list(raw_item_block)::in,
    list(item)::in, list(item)::out, list(item)::in, list(item)::out) is det.

:- pred int_impl_items_to_raw_item_blocks(prog_context::in,
    list(item)::in, list(item)::in, list(raw_item_block)::out) is det.

:- pred int_impl_items_to_specified_item_blocks(prog_context::in,
    MS::in, list(item)::in, MS::in, list(item)::in, list(item_block(MS))::out)
    is det.

%-----------------------------------------------------------------------------%
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

    % Did an item originate in user code or was it added by the compiler as
    % part of a source-to-source transformation, e.g. the initialise
    % declarations? If the latter, specify the information that the
    % make_hlds pass may need to answer questions about the item.
    %
:- type item_maybe_attrs
    --->    item_origin_user
    ;       item_origin_compiler(item_compiler_attributes).

:- type item_compiler_attributes
    --->    item_compiler_attributes(
                maybe_allow_export,
                maybe_is_mutable
            ).

:- type maybe_allow_export
    --->    do_not_allow_export
    ;       do_allow_export.

:- type maybe_is_mutable
    --->    is_not_mutable
    ;       is_mutable.

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
                cl_predname                     :: sym_name,
                cl_pred_or_func                 :: pred_or_func,
                cl_head_args                    :: list(prog_term),
                cl_maybe_attrs                  :: item_maybe_attrs,
                cl_varset                       :: prog_varset,
                cl_body                         :: goal,
                cl_context                      :: prog_context,
                cl_seq_num                      :: int
            ).

:- type item_type_defn_info
    --->    item_type_defn_info(
                % `:- type ...':
                % a definition of a type, or a declaration of an abstract type.
                td_ctor_name                    :: sym_name,
                td_ctor_args                    :: list(type_param),
                td_ctor_defn                    :: type_defn,
                td_tvarset                      :: tvarset,
                td_context                      :: prog_context,
                td_seq_num                      :: int
            ).

:- type item_inst_defn_info
    --->    item_inst_defn_info(
                % `:- inst ... = ...':
                % a definition of an inst.
                id_inst_name                    :: sym_name,
                id_inst_args                    :: list(inst_var),
                id_inst_defn                    :: inst_defn,
                id_varset                       :: inst_varset,
                id_context                      :: prog_context,
                id_seq_num                      :: int
            ).

:- type item_mode_defn_info
    --->    item_mode_defn_info(
                % `:- mode ... = ...':
                % a definition of a mode.
                md_mode_name                    :: sym_name,
                md_mode_args                    :: list(inst_var),
                md_mode_defn                    :: mode_defn,
                md_varset                       :: inst_varset,
                md_context                      :: prog_context,
                md_seq_num                      :: int
            ).

:- type item_pred_decl_info
    --->    item_pred_decl_info(
                % `:- pred ...' or `:- func ...':
                % a predicate or function declaration.
                % This specifies the type of the predicate or function,
                % and it may optionally also specify the mode and determinism.
                pf_name                         :: sym_name,
                pf_p_or_f                       :: pred_or_func,
                pf_arg_decls                    :: list(type_and_mode),
                % The next two fields hold the `with_type` and `with_inst`
                % annotations. This syntactic sugar is expanded out by
                % equiv_type.m, which will then set these fields to `no'.
                pf_maybe_with_type              :: maybe(mer_type),
                pf_maybe_with_inst              :: maybe(mer_inst),
                pf_maybe_detism                 :: maybe(determinism),
                pf_maybe_attrs                  :: item_maybe_attrs,
                % ZZZ pf_pred_origin            :: pred_origin_subset
                pf_tvarset                      :: tvarset,
                pf_instvarset                   :: inst_varset,
                pf_existqvars                   :: existq_tvars,
                pf_purity                       :: purity,
                pf_constraints                  :: prog_constraints,
                pf_context                      :: prog_context,
                pf_seq_num                      :: int
            ).

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
                pfm_seq_num                     :: int
            ).

:- type item_pragma_info
    --->    item_pragma_info(
                pragma_type                     :: pragma_type,
                pragma_maybe_attrs              :: item_maybe_attrs,
                pragma_context                  :: prog_context,
                pragma_seq_num                  :: int
            ).

:- type item_promise_info
    --->    item_promise_info(
                prom_type                       :: promise_type,
                prom_clause                     :: goal,
                prom_varset                     :: prog_varset,
                prom_univ_quant_vars            :: list(prog_var),
                prom_context                    :: prog_context,
                prom_seq_num                    :: int
            ).

:- type item_typeclass_info
    --->    item_typeclass_info(
                tc_class_name                   :: class_name,
                tc_class_params                 :: list(tvar),
                tc_constraints                  :: list(prog_constraint),
                tc_fundeps                      :: list(prog_fundep),
                tc_class_methods                :: class_interface,
                tc_varset                       :: tvarset,
                tc_context                      :: prog_context,
                tc_seq_num                      :: int
            ).

:- type item_instance_info
    --->    item_instance_info(
                % The original types field preserves the types in the instance
                % declaration as written by the programmer. The types field
                % is subject to the expansion of equivalent types.
                ci_class_name                   :: class_name,
                ci_types                        :: list(mer_type),
                ci_original_types               :: list(mer_type),
                ci_deriving_class               :: list(prog_constraint),
                ci_method_instances             :: instance_body,
                ci_varset                       :: tvarset,
                ci_module_containing_instance   :: module_name,
                ci_context                      :: prog_context,
                ci_seq_num                      :: int
            ).

:- type item_initialise_info
    --->    item_initialise_info(
                % :- initialise pred_name.
                init_name                       :: sym_name,
                init_arity                      :: arity,
                init_maybe_attrs                :: item_maybe_attrs,
                init_context                    :: prog_context,
                init_seq_num                    :: int
            ).

:- type item_finalise_info
    --->    item_finalise_info(
                % :- finalise pred_name.
                final_name                      :: sym_name,
                final_arity                     :: arity,
                final_maybe_attrs               :: item_maybe_attrs,
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
% Type classes.
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
                % a predicate or function method. Such declarations
                % specify the type of the predicate or function,
                % and may optionally also specify the mode and determinism.

                sym_name,           % name of the pred or func
                pred_or_func,
                list(type_and_mode),% the arguments' types and modes
                maybe(mer_type),    % any `with_type` annotation
                maybe(mer_inst),    % any `with_inst` annotation
                maybe(determinism), % any determinism declaration
                tvarset,            % type variables
                inst_varset,        % inst variables
                existq_tvars,       % existentially quantified
                                    % type variables
                purity,             % any purity annotation
                prog_constraints,   % the typeclass constraints on
                                    % the declaration
                prog_context        % the declaration's context
            )

    ;       method_pred_or_func_mode(
                % pred_or_func_mode(...) here represents a `mode ...'
                % declaration in a type class body. Such a declaration
                % declares a mode for one of the type class methods.

                sym_name,           % the method name
                maybe(pred_or_func),% whether the method is a pred
                                    % or a func; for declarations
                                    % using `with_inst`, we don't
                                    % know which until we've
                                    % expanded the inst.
                list(mer_mode),     % the arguments' modes
                maybe(mer_inst),    % any `with_inst` annotation
                maybe(determinism), % any determinism declaration
                inst_varset,        % inst variables
                prog_context        % the declaration's context
            ).

:- type class_methods == list(class_method).

%-----------------------------------------------------------------------------%
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
    --->    mutable_dont_attach_to_io_state
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
:- func mutable_var_constant(mutable_var_attributes) = mutable_constant.
:- func mutable_var_attach_to_io_state(mutable_var_attributes)
    = mutable_attach_to_io_state.
:- func mutable_var_thread_local(mutable_var_attributes)
    = mutable_thread_local.

:- pred set_mutable_var_trailed(mutable_trailed::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_add_foreign_name(foreign_name::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_attach_to_io_state(mutable_attach_to_io_state::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_constant(mutable_constant::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

:- pred set_mutable_var_thread_local(mutable_thread_local::in,
    mutable_var_attributes::in, mutable_var_attributes::out) is det.

%-----------------------------------------------------------------------------%
%
% Pragmas.
%

:- type pragma_type
    --->    pragma_foreign_decl(pragma_info_foreign_decl)
    ;       pragma_foreign_code(pragma_info_foreign_code)
    ;       pragma_foreign_proc(pragma_info_foreign_proc)
    ;       pragma_foreign_import_module(pragma_info_foreign_import_module)
    ;       pragma_foreign_proc_export(pragma_info_foreign_proc_export)
    ;       pragma_foreign_export_enum(pragma_info_foreign_export_enum)
    ;       pragma_foreign_enum(pragma_info_foreign_enum)
    ;       pragma_external_proc(pragma_info_external_proc)
    ;       pragma_type_spec(pragma_info_type_spec)
    ;       pragma_inline(pred_name_arity)
    ;       pragma_no_inline(pred_name_arity)
    ;       pragma_unused_args(pragma_info_unused_args)
    ;       pragma_exceptions(pragma_info_exceptions)
    ;       pragma_trailing_info(pragma_info_trailing_info)
    ;       pragma_mm_tabling_info(pragma_info_mm_tabling_info)
    ;       pragma_obsolete(pred_name_arity)
    ;       pragma_no_detism_warning(pred_name_arity)
    ;       pragma_tabled(pragma_info_tabled)
    ;       pragma_fact_table(pragma_info_fact_table)
    ;       pragma_reserve_tag(type_ctor)
    ;       pragma_oisu(pragma_info_oisu)
    ;       pragma_promise_eqv_clauses(pred_name_arity)
    ;       pragma_promise_pure(pred_name_arity)
    ;       pragma_promise_semipure(pred_name_arity)
    ;       pragma_termination_info(pragma_info_termination_info)
    ;       pragma_termination2_info(pragma_info_termination2_info)
    ;       pragma_terminates(pred_name_arity)
    ;       pragma_does_not_terminate(pred_name_arity)
    ;       pragma_check_termination(pred_name_arity)
    ;       pragma_mode_check_clauses(pred_name_arity)
    ;       pragma_structure_sharing(pragma_info_structure_sharing)
    ;       pragma_structure_reuse(pragma_info_structure_reuse)
    ;       pragma_require_feature_set(pragma_info_require_feature_set).

    % Check whether a particular `pragma' declaration is allowed
    % in the interface section of a module.
    %
:- func pragma_allowed_in_interface(pragma_type) = bool.

:- func pragma_context_pieces(pragma_type) = list(format_component).

    % Foreign language interfacing pragmas.

:- type pragma_info_foreign_decl
    --->    pragma_info_foreign_decl(
                % A foreign language declaration, such as C header code.
                decl_lang               :: foreign_language,
                decl_is_local           :: foreign_decl_is_local,
                decl_decl               :: foreign_literal_or_include
            ).

:- type pragma_info_foreign_code
    --->    pragma_info_foreign_code(
                code_lang               :: foreign_language,
                code_code               :: foreign_literal_or_include
            ).

:- type pragma_info_foreign_proc
    --->    pragma_info_foreign_proc(
                % Set of foreign proc attributes, such as:
                %   what language this code is in
                %   whether or not the code may call Mercury,
                %   whether or not the code is thread-safe
                % PredName, Predicate or Function, Vars/Mode,
                % VarNames, Foreign Code Implementation Info
                proc_attrs              :: pragma_foreign_proc_attributes,
                proc_name               :: sym_name,
                proc_p_or_f             :: pred_or_func,
                proc_vars               :: list(pragma_var),
                proc_varset             :: prog_varset,
                proc_instvarset         :: inst_varset,
                proc_impl               :: pragma_foreign_proc_impl
            ).

:- type pragma_info_foreign_import_module
    --->    pragma_info_foreign_import_module(
                % Equivalent to
                % `:- pragma foreign_decl(Lang, "#include <module>.h").'
                % except that the name of the header file is not hard-coded,
                % and mmake can use the dependency information.
                imp_lang                :: foreign_language,
                imp_module              :: module_name
            ).

:- type pragma_info_foreign_proc_export
    --->    pragma_info_foreign_proc_export(
                % Predname, Predicate/function, Modes, foreign function name.
                exp_language            :: foreign_language,
                exp_pred_id             :: pred_name_modes_pf,
                exp_foreign_name        :: string
            ).

:- type pragma_info_foreign_export_enum
    --->    pragma_info_foreign_export_enum(
                export_enum_language   :: foreign_language,
                export_enum_type_ctor  :: type_ctor,
                export_enum_attributes :: export_enum_attributes,
                export_enum_overrides  :: assoc_list(sym_name, string)
            ).

:- type pragma_info_foreign_enum
    --->    pragma_info_foreign_enum(
                foreign_enum_language   :: foreign_language,
                foreign_enum_type_ctor  :: type_ctor,
                foreign_enum_values     :: assoc_list(sym_name, string)
            ).

:- type pragma_info_external_proc
    --->    pragma_info_external_proc(
                % The specified procedure(s) is/are implemented outside
                % of Mercury code, for the named backend if there is one,
                % or if there isn't a named backend, then for all backends.
                external_pred_name      :: sym_name,
                external_pred_arity     :: arity,
                external_p_or_f         :: maybe(pred_or_func),
                external_maybe_backend  :: maybe(backend)
            ).

    % Optimization pragmas.

:- type pragma_info_type_spec
    --->    pragma_info_type_spec(
                % PredName, SpecializedPredName, Arity, PredOrFunc,
                % Modes if a specific procedure was specified, type
                % substitution (using the variable names from the pred
                % declaration), TVarSet, Equivalence types used
                tspec_pred_name         :: sym_name,
                tspec_new_name          :: sym_name,
                tspec_arity             :: arity,
                tspec_p_or_f            :: maybe(pred_or_func),
                tspec_modes             :: maybe(list(mer_mode)),
                tspec_tsubst            :: type_subst,
                tspec_tvarset           :: tvarset,
                tspec_items             :: set(item_id)
            ).

:- type pragma_info_unused_args
    --->    pragma_info_unused_args(
                % PredName, Arity, Mode number, Removed arguments.
                % Used for intermodule unused argument removal. Should only
                % appear in .opt files.
                unused_proc_id          :: pred_name_arity_pf_mn,
                unused_args             :: list(int)
            ).

:- type pragma_info_exceptions
    --->    pragma_info_exceptions(
                % PredName, Arity, Mode number, Exception status.
                % Should only appear in `.opt' or `.trans_opt' files.
                exceptions_proc_id      :: pred_name_arity_pf_mn,
                exceptions_status       :: exception_status
            ).

:- type pragma_info_trailing_info
    --->    pragma_info_trailing_info(
                % PredName, Arity, Mode number, Trailing status.
                % Should on appear in `.opt' or `.trans_opt' files.
                trailing_info_proc_id   :: pred_name_arity_pf_mn,
                trailing_info_status    :: trailing_status
            ).

:- type pragma_info_mm_tabling_info
    --->    pragma_info_mm_tabling_info(
                % PredName, Arity, Mode number, MM Tabling status.
                % Should on appear in `.opt' or `.trans_opt' files.
                mm_tabling_info_proc_id :: pred_name_arity_pf_mn,
                mm_tabling_info_status  :: mm_tabling_status
            ).

    % Evaluation method pragmas.

:- type pragma_info_tabled
    --->    pragma_info_tabled(
                % Tabling type, Predname, Arity, PredOrFunc?, Mode?
                tabled_method           :: eval_method,
                tabled_name             :: pred_name_arity_mpf,
                tabled_mode             :: maybe(list(mer_mode)),
                tabled_attributes       :: maybe(table_attributes)
            ).

:- type pragma_info_fact_table
    --->    pragma_info_fact_table(
                % Predname and Arity, Fact file name.
                fact_table_pred         :: pred_name_arity,
                fact_table_filename     :: string
            ).

:- type pragma_info_oisu
    --->    pragma_info_oisu(
                oisu_type_ctor          :: type_ctor,
                oisu_creator_preds      :: list(pred_name_arity),
                oisu_transformer_preds  :: list(pred_name_arity),
                oisu_destroyer_preds    :: list(pred_name_arity)
            ).

    % Termination analysis pragmas.

:- type pragma_info_termination_info
    --->    pragma_info_termination_info(
                % The list(mer_mode) is the declared argmodes of the
                % procedure, unless there are no declared argmodes, in which
                % case the inferred argmodes are used. This pragma is used to
                % define information about a predicates termination
                % properties. It is most useful where the compiler has
                % insufficient information to be able to analyse the
                % predicate. This includes c_code, and imported predicates.
                % termination_info pragmas are used in opt and trans_opt
                % files.
                terminfo_pred_id        :: pred_name_modes_pf,
                terminfo_args           :: maybe(pragma_arg_size_info),
                terminfo_term           :: maybe(pragma_termination_info)
            ).

:- type pragma_info_termination2_info
    --->    pragma_info_termination2_info(
                terminfo2_pred_id       :: pred_name_modes_pf,
                terminfo2_args          :: maybe(pragma_constr_arg_size_info),
                terminfo2_args2         :: maybe(pragma_constr_arg_size_info),
                terminfo2_term          :: maybe(pragma_termination_info)
            ).

    % CTGC pragmas: structure sharing / structure reuse analysis.

:- type pragma_info_structure_sharing
    --->    pragma_info_structure_sharing(
                % After structure sharing analysis, the compiler generates
                % structure sharing pragmas to be stored in and read from
                % optimization interface files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                sharing_pred_id         :: pred_name_modes_pf,
                sharing_headvars        :: list(prog_var),
                sharing_headvartypes    :: list(mer_type),
                sharing_description     :: maybe(structure_sharing_domain)
            ).

:- type pragma_info_structure_reuse
    --->    pragma_info_structure_reuse(
                % After reuse analysis, the compiler generates structure reuse
                % pragmas to be stored in and read from optimization interface
                % files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                % The last sym_name (reuse_optimised_name) stores the name
                % of the optimised version of the exported predicate.
                reuse_pred_id           :: pred_name_modes_pf,
                reuse_headvars          :: list(prog_var),
                reuse_headvartypes      :: list(mer_type),
                reuse_description       :: maybe(structure_reuse_domain)
            ).

    % Misc pragmas.

:- type pragma_info_require_feature_set
    --->    pragma_info_require_feature_set(
                rfs_feature_set         :: set(required_feature)
            ).

    % These types identify procedures in pragmas.

:- type pred_name_arity
    --->    pred_name_arity(
                pna_pred_name           :: sym_name,
                pna_arity               :: arity
            ).

:- type pred_name_arity_pf
    --->    pred_name_arity_pf(
                pnap_pred_name          :: sym_name,
                pnap_arity              :: arity,
                pnap_pf                 :: pred_or_func
            ).

:- type pred_name_arity_pf_mn
    --->    pred_name_arity_pf_mn(
                pnapm_pred_name         :: sym_name,
                pnapm_arity             :: arity,
                pnapm_pf                :: pred_or_func,
                pnapm_mode_num          :: mode_num
            ).

:- type pred_name_modes_pf
    --->    pred_name_modes_pf(
                pnmp_pred_name          :: sym_name,
                pnmp_arity              :: list(mer_mode),
                pnmp_pf                 :: pred_or_func
            ).

:- type pred_name_arity_mpf
    --->    pred_name_arity_mpf(
                pnam_pred_name          :: sym_name,
                pnam_arity              :: arity,
                pnam_maybe_pf           :: maybe(pred_or_func)
            ).

%-----------------------------------------------------------------------------%
%
% Goals.
%

    % Here is how clauses and goals are represented.
    % a => b --> implies(a, b)
    % a <= b --> implies(b, a) [just flips the goals around!]
    % a <=> b --> equivalent(a, b)
    %
:- type goal
    % conjunctions
    --->    conj_expr(prog_context, goal, goal)
                            % (non-empty) conjunction
    ;       par_conj_expr(prog_context, goal, goal)
                            % parallel conjunction
    ;       true_expr(prog_context)
                            % empty conjunction

    % disjunctions
    ;       disj_expr(prog_context, goal, goal)
                            % (non-empty) disjunction
    ;       fail_expr(prog_context)
                            % empty disjunction

    % quantifiers; the list of prog_vars should have no duplicates
    ;       some_expr(prog_context, list(prog_var), goal)
                            % existential quantification
    ;       all_expr(prog_context, list(prog_var), goal)
                            % universal quantification
    ;       some_state_vars_expr(prog_context, list(prog_var), goal)
    ;       all_state_vars_expr(prog_context, list(prog_var), goal)
                            % state variables extracted from
                            % some/2 and all/2 quantifiers.

    % other scopes
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
                prog_var,
                goal
            )
    ;       require_switch_arms_detism_expr(
                prog_context,
                prog_var,
                determinism,
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

    % implications
    ;       implies_expr(prog_context, goal, goal)
                            % A => B
    ;       equivalent_expr(prog_context, goal, goal)
                            % A <=> B

    % negation and if-then-else
    ;       not_expr(prog_context, goal)
    ;       if_then_else_expr(
                prog_context,
                list(prog_var), % SomeVars
                list(prog_var), % StateVars
                goal,           % Cond
                goal,           % Then
                goal            % Else
            )

    % atomic goals
    ;       event_expr(prog_context, string, list(prog_term))
    ;       call_expr(prog_context, sym_name, list(prog_term), purity)
    ;       unify_expr(prog_context, prog_term, prog_term, purity).

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

:- func goal_get_context(goal) = prog_context.

%-----------------------------------------------------------------------------%
%
% Module system.
%

    % This type used to record most module-system declarations, such as
    % section markers and imports. It is now a grab-bag of leftovers,
    % but that should be fixed soon. XXX ITEM_LIST
    %
:- type module_defn
    --->    md_include_module(module_name)
            % The named module is a submodule of the current module.
            % XXX ITEM_LIST This should be a separate kind of item.

    ;       md_version_numbers(module_name, recompilation.version_numbers)
            % This is used to represent the version numbers of items in an
            % interface file for use in smart recompilation.
            % XXX ITEM_LIST This should not be an item at all. It should be
            % parsed as a marker in prog_io.m, and recorded directly as a
            % new field in the parse_tree_int (and NOT in parse_tree_opt
            % or parse_tree_src).

    ;       md_import(module_name)
    ;       md_use(module_name).
            % Import the entities listed in the interface of the named module
            % into the current module. With md_use, references to these
            % entities must be module qualified; with md_import, they don't
            % have to be.
            % XXX ITEM_LIST Make this a single function symbol, with two
            % arguments: a module name and an import_or_use flag.
            % XXX ITEM_LIST Once the other function symbols of this type have
            % been removed, the type could be renamed, together with the item
            % kind that contains it.

%-----------------------------------------------------------------------------%

:- type contains_foreign_code
    --->    contains_foreign_code(set(foreign_language))
    ;       contains_no_foreign_code
    ;       contains_foreign_code_unknown.

:- type contains_foreign_export
    --->    contains_foreign_export
    ;       contains_no_foreign_export.

:- pred get_foreign_code_indicators_from_item_blocks(globals::in,
    list(item_block(MS))::in,
    set(foreign_language)::out, foreign_import_module_infos::out,
    foreign_include_file_infos::out, contains_foreign_export::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_foreign.

:- import_module cord.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%

file_kind_to_extension(fk_src) = ".m".
file_kind_to_extension(fk_int(IntFileKind)) =
    int_file_kind_to_extension(IntFileKind).
file_kind_to_extension(fk_opt(OptFileKind)) =
    opt_file_kind_to_extension(OptFileKind).

int_file_kind_to_extension(ifk_int0) = ".int0".
int_file_kind_to_extension(ifk_int2) = ".int2".
int_file_kind_to_extension(ifk_int3) = ".int3".
int_file_kind_to_extension(ifk_int) = ".int".

opt_file_kind_to_extension(ofk_opt) = ".opt".
opt_file_kind_to_extension(ofk_trans_opt) = ".trans_opt".

extension_to_file_kind(Extension, FileKind) :-
    (
        Extension = ".m",
        FileKind = fk_src
    ;
        Extension = ".int0",
        FileKind = fk_int(ifk_int0)
    ;
        Extension = ".int3",
        FileKind = fk_int(ifk_int3)
    ;
        Extension = ".int2",
        FileKind = fk_int(ifk_int2)
    ;
        Extension = ".int",
        FileKind = fk_int(ifk_int)
    ;
        Extension = ".opt",
        FileKind = fk_opt(ofk_opt)
    ;
        Extension = ".trans_opt",
        FileKind = fk_opt(ofk_trans_opt)
    ).

%-----------------------------------------------------------------------------%

compilation_unit_project_name(compilation_unit(ModuleName, _, _)) = ModuleName.

cast_module_components_to_raw_item_blocks([], []).
cast_module_components_to_raw_item_blocks([Component | Components],
        [RawItemBlock | RawItemBlocks]) :-
    (
        Component = mc_section(Section, Context, ItemCord),
        Items = cord.list(ItemCord),
        RawItemBlock = item_block(Section, Context, Items)
    ;
        Component = mc_nested_submodule(_, _, _),
        unexpected($module, $pred, "unexpected nested submodule")
    ),
    cast_module_components_to_raw_item_blocks(Components, RawItemBlocks).

augment_block(ItemBlock, AugItemBlock) :-
    ItemBlock = item_block(Section, Context, Items),
    (
        Section = ms_interface,
        AugSection = ams_interface
    ;
        Section = ms_implementation,
        AugSection = ams_implementation
    ),
    AugItemBlock = item_block(AugSection, Context, Items).

separate_int_impl_items([], !IntItems, !ImplItems).
separate_int_impl_items([ItemBlock | ItemBlocks], !IntItems, !ImplItems) :-
    separate_int_impl_items(ItemBlocks, !IntItems, !ImplItems),
    ItemBlock = item_block(Section, _Context, Items),
    (
        Section = ms_interface,
        !:IntItems = Items ++ !.IntItems
    ;
        Section = ms_implementation,
        !:ImplItems = Items ++ !.ImplItems
    ).

int_impl_items_to_raw_item_blocks(Context, IntItems, ImplItems,
        RawItemBlocks) :-
    (
        ImplItems = [],
        RawItemBlocks0 = []
    ;
        ImplItems = [_ | _],
        RawImplBlock = item_block(ms_implementation, Context, ImplItems),
        RawItemBlocks0 = [RawImplBlock]
    ),
    (
        IntItems = [],
        RawItemBlocks = RawItemBlocks0
    ;
        IntItems = [_ | _],
        RawIntBlock = item_block(ms_interface, Context, IntItems),
        RawItemBlocks = [RawIntBlock | RawItemBlocks0]
    ).

int_impl_items_to_specified_item_blocks(Context, IntSection, IntItems,
        ImplSection, ImplItems, ItemBlocks) :-
    (
        ImplItems = [],
        ItemBlocks0 = []
    ;
        ImplItems = [_ | _],
        ImplBlock = item_block(ImplSection, Context, ImplItems),
        ItemBlocks0 = [ImplBlock]
    ),
    (
        IntItems = [],
        ItemBlocks = ItemBlocks0
    ;
        IntItems = [_ | _],
        IntBlock = item_block(IntSection, Context, IntItems),
        ItemBlocks = [IntBlock | ItemBlocks0]
    ).

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
% Mutable variables.
%

    % Attributes for mutable variables.
    %
:- type mutable_var_attributes
    --->    mutable_var_attributes(
                mutable_trailed             :: mutable_trailed,
                mutable_foreign_names       :: maybe(list(foreign_name)),
                mutable_attach_to_io_state  :: mutable_attach_to_io_state,
                mutable_constant            :: mutable_constant,
                mutable_thread_local        :: mutable_thread_local
            ).

default_mutable_attributes =
    mutable_var_attributes(mutable_trailed, no,
        mutable_dont_attach_to_io_state, mutable_not_constant,
        mutable_not_thread_local).

mutable_var_trailed(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_trailed.
mutable_var_maybe_foreign_names(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_foreign_names.
mutable_var_attach_to_io_state(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_attach_to_io_state.
mutable_var_constant(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_constant.
mutable_var_thread_local(MVarAttrs) = X :-
    X = MVarAttrs ^ mutable_thread_local.

set_mutable_var_trailed(Trailed, !Attributes) :-
    !Attributes ^ mutable_trailed := Trailed.
set_mutable_add_foreign_name(ForeignName, !Attributes) :-
    MaybeForeignNames0 = !.Attributes ^ mutable_foreign_names,
    (
        MaybeForeignNames0 = no,
        MaybeForeignNames  = yes([ForeignName])
    ;
        MaybeForeignNames0 = yes(ForeignNames0),
        ForeignNames = [ForeignName | ForeignNames0],
        MaybeForeignNames   = yes(ForeignNames)
    ),
    !Attributes ^ mutable_foreign_names := MaybeForeignNames.
set_mutable_var_attach_to_io_state(AttachToIOState, !Attributes) :-
    !Attributes ^ mutable_attach_to_io_state := AttachToIOState.
set_mutable_var_constant(Constant, !Attributes) :-
    !Attributes ^ mutable_constant := Constant.
set_mutable_var_thread_local(ThreadLocal, !Attributes) :-
    !Attributes ^ mutable_thread_local := ThreadLocal.

%-----------------------------------------------------------------------------%

pragma_allowed_in_interface(Pragma) = Allowed :-
    % XXX This comment is out of date.
    % pragma `obsolete', `terminates', `does_not_terminate'
    % `termination_info', `check_termination', `reserve_tag' and
    % `foreign_enum' pragma declarations are supposed to go in the
    % interface, but all other pragma declarations are implementation details
    % only, and should go in the implementation.

    (
        ( Pragma = pragma_foreign_code(_)
        ; Pragma = pragma_foreign_decl(_)
        ; Pragma = pragma_foreign_proc_export(_)
        ; Pragma = pragma_foreign_export_enum(_)
        ; Pragma = pragma_foreign_proc(_)
        ; Pragma = pragma_external_proc(_)
        ; Pragma = pragma_inline(_)
        ; Pragma = pragma_no_inline(_)
        ; Pragma = pragma_no_detism_warning(_)
        ; Pragma = pragma_fact_table(_)
        ; Pragma = pragma_tabled(_)
        ; Pragma = pragma_promise_pure(_)
        ; Pragma = pragma_promise_semipure(_)
        ; Pragma = pragma_promise_eqv_clauses(_)
        ; Pragma = pragma_unused_args(_)
        ; Pragma = pragma_exceptions(_)
        ; Pragma = pragma_trailing_info(_)
        ; Pragma = pragma_mm_tabling_info(_)
        ; Pragma = pragma_require_feature_set(_)
        ),
        Allowed = no
    ;
        % Note that `reserve_tag' and `direct_arg' must be in the interface
        % iff the corresponding type definition is in the interface. This is
        % checked in make_hlds.
        ( Pragma = pragma_foreign_enum(_)
        ; Pragma = pragma_foreign_import_module(_)
        ; Pragma = pragma_obsolete(_)
        ; Pragma = pragma_reserve_tag(_)
        ; Pragma = pragma_type_spec(_)
        ; Pragma = pragma_termination_info(_)
        ; Pragma = pragma_termination2_info(_)
        ; Pragma = pragma_terminates(_)
        ; Pragma = pragma_does_not_terminate(_)
        ; Pragma = pragma_check_termination(_)
        ; Pragma = pragma_structure_sharing(_)
        ; Pragma = pragma_structure_reuse(_)
        ; Pragma = pragma_mode_check_clauses(_)
        ; Pragma = pragma_oisu(_)
        ),
        Allowed = yes
    ).

pragma_context_pieces(Pragma) = ContextPieces :-
    (
        Pragma = pragma_foreign_code(_),
        ContextPieces = [pragma_decl("foreign_code"), words("declaration")]
    ;
        Pragma = pragma_foreign_decl(_),
        ContextPieces = [pragma_decl("foreign_decl"), words("declaration")]
    ;
        Pragma = pragma_foreign_proc_export(_),
        ContextPieces = [pragma_decl("foreign_export"), words("declaration")]
    ;
        Pragma = pragma_foreign_export_enum(_),
        ContextPieces = [pragma_decl("foreign_export_enum"),
            words("declaration")]
    ;
        Pragma = pragma_foreign_proc(_),
        ContextPieces = [pragma_decl("foreign_proc"), words("declaration")]
    ;
        Pragma = pragma_external_proc(External),
        External = pragma_info_external_proc(_, _, MaybePorF, _),
        (
            MaybePorF = no,
            ContextPieces = [decl("external"), words("declaration")]
        ;
            MaybePorF = yes(pf_predicate),
            ContextPieces = [pragma_decl("external_pred"),
                words("declaration")]
        ;
            MaybePorF = yes(pf_function),
            ContextPieces = [pragma_decl("external_func"),
                words("declaration")]
        )
    ;
        Pragma = pragma_inline(_),
        ContextPieces = [pragma_decl("inline"), words("declaration")]
    ;
        Pragma = pragma_no_inline(_),
        ContextPieces = [pragma_decl("no_inline"), words("declaration")]
    ;
        Pragma = pragma_no_detism_warning(_),
        ContextPieces = [pragma_decl("no_determinism_warning"),
            words("declaration")]
    ;
        Pragma = pragma_fact_table(_),
        ContextPieces = [pragma_decl("fact_table"), words("declaration")]
    ;
        Pragma = pragma_tabled(Tabled),
        Tabled = pragma_info_tabled(EvalMethod, _, _, _),
        (
            EvalMethod = eval_memo,
            ContextPieces = [pragma_decl("memo"), words("declaration")]
        ;
            EvalMethod = eval_loop_check,
            ContextPieces = [pragma_decl("loop_check"), words("declaration")]
        ;
            EvalMethod = eval_minimal(_),
            ContextPieces = [pragma_decl("minimal_model"),
                words("declaration")]
        ;
            EvalMethod = eval_table_io(_, _),
            unexpected($module, $pred, "eval_table_io")
        ;
            EvalMethod = eval_normal,
            unexpected($module, $pred, "eval_normal")
        )
    ;
        Pragma = pragma_promise_pure(_),
        ContextPieces = [pragma_decl("promise_pure"), words("declaration")]
    ;
        Pragma = pragma_promise_semipure(_),
        ContextPieces = [pragma_decl("promise_semipure"), words("declaration")]
    ;
        Pragma = pragma_promise_eqv_clauses(_),
        ContextPieces = [pragma_decl("promise_equivalent_clauses"),
            words("declaration")]
    ;
        Pragma = pragma_unused_args(_),
        ContextPieces = [pragma_decl("unused_args"), words("declaration")]
    ;
        Pragma = pragma_exceptions(_),
        ContextPieces = [pragma_decl("exceptions"), words("declaration")]
    ;
        Pragma = pragma_trailing_info(_),
        ContextPieces = [pragma_decl("trailing_info"), words("declaration")]
    ;
        Pragma = pragma_mm_tabling_info(_),
        ContextPieces = [pragma_decl("tabling_info"), words("declaration")]
    ;
        Pragma = pragma_require_feature_set(_),
        ContextPieces = [pragma_decl("require_feature_set"),
            words("declaration")]
    ;
        Pragma = pragma_foreign_enum(_),
        ContextPieces = [pragma_decl("foreign_enum"), words("declaration")]
    ;
        Pragma = pragma_foreign_import_module(_),
        ContextPieces = [pragma_decl("foreign_import_module"),
            words("declaration")]
    ;
        Pragma = pragma_obsolete(_),
        ContextPieces = [pragma_decl("obsolete"), words("declaration")]
    ;
        Pragma = pragma_reserve_tag(_),
        ContextPieces = [pragma_decl("reserve_tag"), words("declaration")]
    ;
        Pragma = pragma_type_spec(_),
        ContextPieces = [pragma_decl("type_spec"), words("declaration")]
    ;
        Pragma = pragma_termination_info(_),
        ContextPieces = [pragma_decl("termination_info"),
            words("declaration")]
    ;
        Pragma = pragma_termination2_info(_),
        ContextPieces = [pragma_decl("termination2_info"),
            words("declaration")]
    ;
        Pragma = pragma_terminates(_),
        ContextPieces = [pragma_decl("terminates"), words("declaration")]
    ;
        Pragma = pragma_does_not_terminate(_),
        ContextPieces = [pragma_decl("does_not_terminate"),
            words("declaration")]
    ;
        Pragma = pragma_check_termination(_),
        ContextPieces = [pragma_decl("check_termination"),
            words("declaration")]
    ;
        Pragma = pragma_structure_sharing(_),
        ContextPieces = [pragma_decl("structure_sharing"),
            words("declaration")]
    ;
        Pragma = pragma_structure_reuse(_),
        ContextPieces = [pragma_decl("structure_reuse"), words("declaration")]
    ;
        Pragma = pragma_mode_check_clauses(_),
        ContextPieces = [pragma_decl("mode_check_clauses"),
            words("declaration")]
    ;
        Pragma = pragma_oisu(_),
        ContextPieces = [pragma_decl("oisu"), words("declaration")]
    ).

%-----------------------------------------------------------------------------%

goal_get_context(Goal) = Context :-
    ( Goal = conj_expr(Context, _, _)
    ; Goal = par_conj_expr(Context, _, _)
    ; Goal = true_expr(Context)
    ; Goal = disj_expr(Context, _, _)
    ; Goal = fail_expr(Context)
    ; Goal = some_expr(Context, _, _)
    ; Goal = all_expr(Context, _, _)
    ; Goal = some_state_vars_expr(Context, _, _)
    ; Goal = all_state_vars_expr(Context, _, _)
    ; Goal = promise_purity_expr(Context, _, _)
    ; Goal = promise_equivalent_solutions_expr(Context, _, _, _, _, _)
    ; Goal = promise_equivalent_solution_sets_expr(Context, _, _, _, _, _)
    ; Goal = promise_equivalent_solution_arbitrary_expr(Context, _, _, _, _, _)
    ; Goal = require_detism_expr(Context, _, _)
    ; Goal = require_complete_switch_expr(Context, _, _)
    ; Goal = require_switch_arms_detism_expr(Context, _, _, _)
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

%-----------------------------------------------------------------------------%

:- type module_foreign_info
    --->    module_foreign_info(
                used_foreign_languages      :: set(foreign_language),
                foreign_proc_languages      :: map(sym_name, foreign_language),
                all_foreign_import_modules  :: foreign_import_module_infos,
                all_foreign_include_files   :: foreign_include_file_infos,
                module_has_foreign_export   :: contains_foreign_export
            ).

get_foreign_code_indicators_from_item_blocks(Globals, ItemBlocks,
        LangSet, ForeignImports, ForeignIncludeFiles, ContainsForeignExport) :-
    Info0 = module_foreign_info(set.init, map.init, cord.init, cord.init,
        contains_no_foreign_export),
    list.foldl(get_foreign_code_indicators_from_item_block(Globals),
        ItemBlocks, Info0, Info),
    Info = module_foreign_info(LangSet0, LangMap, ForeignImports,
        ForeignIncludeFiles, ContainsForeignExport),
    ForeignProcLangs = map.values(LangMap),
    LangSet = set.insert_list(LangSet0, ForeignProcLangs).

:- pred get_foreign_code_indicators_from_item_block(globals::in,
    item_block(MS)::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_foreign_code_indicators_from_item_block(Globals, ItemBlock, !Info) :-
    ItemBlock = item_block(_, _, Items),
    list.foldl(get_foreign_code_indicators_from_item(Globals), Items, !Info).

:- pred get_foreign_code_indicators_from_item(globals::in, item::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_foreign_code_indicators_from_item(Globals, Item, !Info) :-
    (
        Item = item_pragma(ItemPragma),
        ItemPragma = item_pragma_info(Pragma, _, Context, _),
        get_pragma_foreign_code(Globals, Pragma, Context, !Info)
    ;
        Item = item_mutable(_),
        % Mutables introduce foreign_procs, but mutable declarations
        % won't have been expanded by the time we get here, so we need
        % to handle them separately.
        % XXX mutables are currently only implemented for the C backends
        % but we should handle the Java/IL backends here as well.
        % (See do_get_item_foreign_code for details/5).
        UsedForeignLanguages0 = !.Info ^ used_foreign_languages,
        set.insert(lang_c, UsedForeignLanguages0, UsedForeignLanguages),
        !Info ^ used_foreign_languages := UsedForeignLanguages
    ;
        ( Item = item_initialise(_)
        ; Item = item_finalise(_)
        ),
        % Intialise/finalise declarations introduce export pragmas, but
        % again they won't have been expanded by the time we get here.
        % XXX we don't currently support these on non-C backends.
        UsedForeignLanguages0 = !.Info ^ used_foreign_languages,
        set.insert(lang_c, UsedForeignLanguages0, UsedForeignLanguages),
        !Info ^ used_foreign_languages := UsedForeignLanguages,
        !Info ^ module_has_foreign_export := contains_foreign_export
    ;
        ( Item = item_module_defn(_)
        ; Item = item_clause(_)
        ; Item = item_type_defn(_)
        ; Item = item_inst_defn(_)
        ; Item = item_mode_defn(_)
        ; Item = item_pred_decl(_)
        ; Item = item_mode_decl(_)
        ; Item = item_promise(_)
        ; Item = item_typeclass(_)
        ; Item = item_instance(_)
        ; Item = item_nothing(_)
        )
    ).

:- pred get_pragma_foreign_code(globals::in, pragma_type::in, prog_context::in,
    module_foreign_info::in, module_foreign_info::out) is det.

get_pragma_foreign_code(Globals, Pragma, Context, !Info) :-
    globals.get_backend_foreign_languages(Globals, BackendLangs),
    globals.get_target(Globals, Target),

    % The code here should match the way that mlds_to_gcc.m decides whether
    % or not to call mlds_to_c.m.
    (
        Pragma = pragma_foreign_code(FCInfo),
        FCInfo = pragma_info_foreign_code(Lang, LiteralOrInclude),
        ( list.member(Lang, BackendLangs) ->
            Langs0 = !.Info ^ used_foreign_languages,
            set.insert(Lang, Langs0, Langs),
            !Info ^ used_foreign_languages := Langs
        ;
            true
        ),
        do_get_item_foreign_include_file(Lang, LiteralOrInclude, !Info)
    ;
        Pragma = pragma_foreign_proc(FPInfo),
        FPInfo = pragma_info_foreign_proc(Attrs, Name, _, _, _, _, _),
        NewLang = get_foreign_language(Attrs),
        FPLangs0 = !.Info ^ foreign_proc_languages,
        ( map.search(FPLangs0, Name, OldLang) ->
            % is it better than an existing one?
            PreferNew = prefer_foreign_language(Globals, Target,
                OldLang, NewLang),
            (
                PreferNew = yes,
                map.det_update(Name, NewLang, FPLangs0, FPLangs),
                !Info ^ foreign_proc_languages := FPLangs
            ;
                PreferNew = no
            )
        ;
            % is it one of the languages we support?
            ( list.member(NewLang, BackendLangs) ->
                map.det_insert(Name, NewLang, FPLangs0, FPLangs),
                !Info ^ foreign_proc_languages := FPLangs
            ;
                true
            )
        )
    ;
        % XXX `pragma export' should not be treated as foreign, but currently
        % mlds_to_gcc.m doesn't handle that declaration, and instead just
        % punts it on to mlds_to_c.m, thus generating C code for it,
        % rather than assembler code. So we need to treat `pragma export'
        % like the other pragmas for foreign code.
        Pragma = pragma_foreign_proc_export(FPEInfo),
        FPEInfo = pragma_info_foreign_proc_export(Lang, _, _),
        ( list.member(Lang, BackendLangs) ->
            !Info ^ used_foreign_languages :=
                set.insert(!.Info ^ used_foreign_languages, Lang),
            !Info ^ module_has_foreign_export :=
                contains_foreign_export
        ;
            true
        )
    ;
        Pragma = pragma_foreign_import_module(FIMInfo),
        FIMInfo = pragma_info_foreign_import_module(Lang, Import),
        ( list.member(Lang, BackendLangs) ->
            ForeignImportModule =
                foreign_import_module_info(Lang, Import, Context),
            ForeignImportModulesCord0 = !.Info ^ all_foreign_import_modules,
            ForeignImportModulesCord =
                cord.snoc(ForeignImportModulesCord0, ForeignImportModule),
            !Info ^ all_foreign_import_modules := ForeignImportModulesCord
        ;
            true
        )
    ;
        Pragma = pragma_fact_table(_),
        (
            % We generate some C code for fact tables, so we need to treat
            % modules containing fact tables as if they contain foreign code.
            Target = target_c
        ->
            !Info ^ used_foreign_languages :=
                set.insert(!.Info ^ used_foreign_languages, lang_c)
        ;
            true
        )
    ;
        % We do NOT count foreign_decls here. We only link in a foreign object
        % file if mlds_to_gcc called mlds_to_c.m to generate it, which it
        % will only do if there is some foreign_code, not just foreign_decls.
        % Counting foreign_decls here causes problems with intermodule
        % optimization.
        Pragma = pragma_foreign_decl(FDInfo),
        FDInfo = pragma_info_foreign_decl(Lang, _IsLocal, LiteralOrInclude),
        do_get_item_foreign_include_file(Lang, LiteralOrInclude, !Info)
    ;
        ( Pragma = pragma_foreign_enum(_)
        ; Pragma = pragma_foreign_export_enum(_)
        )
        % XXX Should we count these?
    ;
        ( Pragma = pragma_check_termination(_)
        ; Pragma = pragma_does_not_terminate(_)
        ; Pragma = pragma_exceptions(_)
        ; Pragma = pragma_external_proc(_)
        ; Pragma = pragma_inline(_)
        ; Pragma = pragma_mm_tabling_info(_)
        ; Pragma = pragma_mode_check_clauses(_)
        ; Pragma = pragma_no_detism_warning(_)
        ; Pragma = pragma_no_inline(_)
        ; Pragma = pragma_obsolete(_)
        ; Pragma = pragma_promise_eqv_clauses(_)
        ; Pragma = pragma_promise_pure(_)
        ; Pragma = pragma_promise_semipure(_)
        ; Pragma = pragma_require_feature_set(_)
        ; Pragma = pragma_reserve_tag(_)
        ; Pragma = pragma_structure_reuse(_)
        ; Pragma = pragma_structure_sharing(_)
        ; Pragma = pragma_oisu(_)
        ; Pragma = pragma_tabled(_)
        ; Pragma = pragma_terminates(_)
        ; Pragma = pragma_termination2_info(_)
        ; Pragma = pragma_termination_info(_)
        ; Pragma = pragma_trailing_info(_)
        ; Pragma = pragma_type_spec(_)
        ; Pragma = pragma_unused_args(_)
        )
        % Do nothing.
    ).

:- pred do_get_item_foreign_include_file(foreign_language::in,
    foreign_literal_or_include::in, module_foreign_info::in,
    module_foreign_info::out) is det.

do_get_item_foreign_include_file(Lang, LiteralOrInclude, !Info) :-
    (
        LiteralOrInclude = literal(_)
    ;
        LiteralOrInclude = include_file(FileName),
        IncludeFile = foreign_include_file_info(Lang, FileName),
        IncludeFilesCord0 = !.Info ^ all_foreign_include_files,
        IncludeFilesCord = cord.snoc(IncludeFilesCord0, IncludeFile),
        !Info ^ all_foreign_include_files := IncludeFilesCord
    ).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_item.
%-----------------------------------------------------------------------------%
