%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
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
:- import_module parse_tree.error_spec.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module recompilation.
:- import_module recompilation.item_types.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module set.

%---------------------------------------------------------------------------%
%
% The intended semantics of a type_ctor_defn_map is a map of
% all the type constructors defined in a given SECTION of a given
% interface file to all its definitions in that section.
%
% There are four intended uses of a type_ctor_defn_map. The most
% important is the fourth one.
%
% One is to eliminate unnecessary items from interface files.
% For example, library/set.m contains two definitions of the set
% type constructor: an abstract definition in the publicly documented
% interface section, and an actual du definition in another interface
% section that we do not include in the automatically generated
% documentation but we *do* export to other modules. In situations
% like this, the abstract definition is redundant. Never including it
% in an interface file lets that interface file to remain unchanged
% in the event that the user deletes the abstract definition from
% the source file as well.
%
% The second use is to canonicalize the parts of interface files
% containing type definitions.
%
% The third use is to help deal with sets of definitions that
% don't make sense. There are many rules that a set of definitions
% for a given type constructor must meet (such as "there may be at most one
% definition for a type constructor that is a du, equivalence or solver
% definition), and bugs may manifest themselves as violations of these rules.
%
% We have a choice in when these violations are detected.
%
% - If we allow the inclusion of inconsistent sets of type definitions
%   in interface files, then we must detect and handle these
%   inconsistencies every time a compiler invocation reads that interface
%   file. These invocations won't generate error messages for these
%   inconsistencies since the type constructor won't be local, but
%   they may generate messages for other "errors" that look like errors
%   only because the compiler's resolution of the inconsistency (i.e.
%   its choice of which type definitions to keep and which to throw out)
%   differs from the programmer's choice.
%
% - If we do NOT allow the inclusion of inconsistent sets of type
%   definitions in interface files, then we must report any violations
%   at interface file construction time, and make them cause that
%   construction to fail. Printing such error messages to stdout
%   instead of the module's .err file is less than ideal, but
%   this early detection can avoid avalanches of misleading diagnostics
%   of the kind mentioned in the previous point. It can also save
%   recompilations. If a module's source file contains inconsistent
%   definitions for a type constructor, then the programmer will
%   have to delete the unintended ones. Once this is done, the
%   interface file will have to be rebuilt. If we allow inconsistent
%   definitions in the interface file, its new contents will differ
%   from its old contents, which means that all the compilations
%   of *other* modules that read the old contents will have been wasted.
%   If we cause the construction of the interface file to fail instead,
%   those compilations won't have taken place.
%
% We implement the first choice by checking whether each entry in
% a type_ctor_defn_map makes sense, and generating error messages
% when they don't. This is done by code in check_type_inst_mode_defns.m.
%
% The fourth and most motivating use is that having all the definitions
% of a type_ctor, *and* all the foreign_enum pragmas that apply to that
% type_ctor, all together at once will make the code that decides
% the proper representation of that type significantly simpler.
%
% Everything above except the fourth use also applies to the inst_
% and mode_ctor_defn_maps, though for those, the consistency rules are
% much simpler: that each inst and mode constructor must have at most one
% non-abstract definition.
%

:- type type_ctor_defn_map == map(type_ctor, type_ctor_all_defns).

:- type type_ctor_all_defns
    --->    type_ctor_all_defns(
                % Abstract and nonabstract solver type definitions.
                tcad_abstract_solver    :: list(item_type_defn_info_abstract),
                tcad_solver             :: list(item_type_defn_info_solver),

                % Abstract and nonabstract nonsolver type definitions.
                tcad_abstract_std       :: list(item_type_defn_info_abstract),
                tcad_eqv                :: list(item_type_defn_info_eqv),
                tcad_du                 :: list(item_type_defn_info_du),
                tcad_sub                :: list(item_type_defn_info_sub),
                tcad_foreign            :: c_j_cs_defns
            ).

:- type type_ctor_maybe_defn
    --->    type_ctor_maybe_defn(
                % Abstract and nonabstract solver type definitions.
                tcmd_abstract_solver    :: maybe(item_type_defn_info_abstract),
                tcmd_solver             :: maybe(item_type_defn_info_solver),

                % Abstract and nonabstract nonsolver type definitions.
                tcmd_abstract_std       :: maybe(item_type_defn_info_abstract),
                tcmd_eqv                :: maybe(item_type_defn_info_eqv),
                tcmd_du                 :: maybe(item_type_defn_info_du),
                tcmd_sub                :: maybe(item_type_defn_info_sub),
                tcmd_foreign            :: c_j_cs_maybe_defn
            ).

    % We support foreign type definitions in all three of our target languages,
    % C, Java and C#. Likewise, we allow foreign enum declarations
    % in these three languages.
    %
    % There are several kinds of info that we may want to store for every
    % one of these foreign languages. This can be done in instances
    % of this type, whose fields always contain the info for C, Java and C#
    % (in that order).
:- type c_java_csharp(T)
    --->    c_java_csharp(T, T, T).

:- type c_j_cs_defns ==
    c_java_csharp(list(item_type_defn_info_foreign)).
:- type c_j_cs_maybe_defn ==
    c_java_csharp(maybe(item_type_defn_info_foreign)).
:- type c_j_cs_enums ==
    c_java_csharp(list(item_foreign_enum_info)).
:- type c_j_cs_maybe_enum ==
    c_java_csharp(maybe(item_foreign_enum_info)).
:- type c_j_cs_repn ==
    c_java_csharp(maybe(foreign_type_repn)).
:- type c_j_cs_enum_repn ==
    c_java_csharp(maybe(enum_foreign_repn)).

:- type inst_ctor_defn_map == map(inst_ctor, inst_ctor_all_defns).
:- type inst_ctor_all_defns
    --->    inst_ctor_all_defns(
                icad_abstract           :: list(item_inst_defn_info_abstract),
                icad_eqv                :: list(item_inst_defn_info_eqv)
            ).

:- type mode_ctor_defn_map == map(mode_ctor, mode_ctor_all_defns).
:- type mode_ctor_all_defns
    --->    mode_ctor_all_defns(
                mcad_abstract           :: list(item_mode_defn_info_abstract),
                mcad_eqv                :: list(item_mode_defn_info_eqv)
            ).

:- type type_ctor_foreign_enum_map == map(type_ctor, c_j_cs_enums).

:- type type_ctor_repn_map == map(type_ctor, item_type_repn_info).

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

    % Did an item originate in user code or was it added by the compiler
    % as part of a source-to-source transformation, e.g. the initialise
    % declarations? If the latter, specify the information that the
    % make_hlds pass may need to answer questions about the item.
    %
:- type item_maybe_attrs
    --->    item_origin_user
    ;       item_origin_compiler(item_compiler_attributes).

:- type item_compiler_attributes
    --->    item_compiler_attributes(
                compiler_origin
            ).

:- type compiler_origin
    --->    compiler_origin_initialise
    ;       compiler_origin_finalise
    ;       compiler_origin_class_method(
                cm_class_id                     :: class_id,
                cm_method                       :: pred_pf_name_arity
            )
    ;       compiler_origin_solver_repn(
                cosr_type_ctor                  :: type_ctor,
                cosr_aux_pred_kind              :: solver_type_pred_kind
            )
    ;       compiler_origin_mutable(
                com_module_name                 :: module_name,
                com_mutable_name                :: string,
                com_aux_pred_kind               :: mutable_pred_kind
            )
    ;       compiler_origin_tabling(
                cot_pred_spec                   :: pred_pf_name_arity,
                cot_aux_pred_kind               :: tabling_aux_pred_kind
            ).

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

:- type item_clause_info
    --->    item_clause_info(
                cl_pred_or_func                 :: pred_or_func,
                cl_predname                     :: sym_name,
                cl_head_args                    :: list(prog_term),
                cl_varset                       :: prog_varset,
                cl_body                         :: maybe2(goal,
                                                    list(warning_spec)),
                cl_context                      :: prog_context,
                cl_seq_num                      :: item_seq_num
            ).

:- type item_type_defn_info == item_type_defn_info_general(type_defn).

:- type item_type_defn_info_abstract
    == item_type_defn_info_general(type_details_abstract).
:- type item_type_defn_info_solver
    == item_type_defn_info_general(type_details_solver).
:- type item_type_defn_info_eqv
    == item_type_defn_info_general(type_details_eqv).
:- type item_type_defn_info_du
    == item_type_defn_info_general(type_details_du).
:- type item_type_defn_info_sub
    == item_type_defn_info_general(type_details_sub).
:- type item_type_defn_info_foreign
    == item_type_defn_info_general(type_details_foreign_generic).

:- type item_type_defn_info_general(T)
    --->    item_type_defn_info(
                % `:- type ...':
                % a definition of a type, or a declaration of an abstract type.
                td_ctor_name                    :: sym_name,
                td_ctor_args                    :: list(type_param),
                td_ctor_defn                    :: T,
                td_tvarset                      :: tvarset,
                td_context                      :: prog_context,
                td_seq_num                      :: item_seq_num
            ).

:- type item_inst_defn_info
    == item_inst_defn_info_general(maybe_abstract_inst_defn).

:- type item_inst_defn_info_abstract
    == item_inst_defn_info_general(no_inst_defn).
:- type item_inst_defn_info_eqv
    == item_inst_defn_info_general(inst_defn).

:- type item_inst_defn_info_general(T)
    --->    item_inst_defn_info(
                % `:- inst ... = ...':
                % a definition of an inst.
                id_inst_name                    :: sym_name,
                id_inst_args                    :: list(inst_var),
                id_maybe_for_type               :: maybe(type_ctor),
                id_inst_defn                    :: T,
                id_varset                       :: inst_varset,
                id_context                      :: prog_context,
                id_seq_num                      :: item_seq_num
            ).

:- type no_inst_defn
    --->    no_inst_defn.

:- type maybe_abstract_inst_defn
    --->    abstract_inst_defn
    ;       nonabstract_inst_defn(inst_defn).

:- type item_mode_defn_info
    == item_mode_defn_info_general(maybe_abstract_mode_defn).

:- type item_mode_defn_info_abstract
    == item_mode_defn_info_general(no_mode_defn).
:- type item_mode_defn_info_eqv
    == item_mode_defn_info_general(mode_defn).

:- type item_mode_defn_info_general(T)
    --->    item_mode_defn_info(
                % `:- mode ... = ...':
                % a definition of a mode.
                md_mode_name                    :: sym_name,
                md_mode_args                    :: list(inst_var),
                md_mode_defn                    :: T,
                md_varset                       :: inst_varset,
                md_context                      :: prog_context,
                md_seq_num                      :: item_seq_num
            ).

:- type no_mode_defn
    --->    no_mode_defn.

:- type maybe_abstract_mode_defn
    --->    abstract_mode_defn
    ;       nonabstract_mode_defn(mode_defn).

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

:- type item_foreign_enum_info
    --->    item_foreign_enum_info(
                fe_language                     :: foreign_language,
                fe_type_ctor                    :: type_ctor,
                fe_values                       :: one_or_more(
                                                    pair(sym_name, string)),
                fe_context                      :: prog_context,
                fe_seq_num                      :: item_seq_num
            ).

:- type foreign_enum_spec
    --->    foreign_enum_spec(
                foreign_language,
                type_ctor,
                one_or_more(pair(sym_name, string))
            ).

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

:- type item_promise_info
    --->    item_promise_info(
                prom_type                       :: promise_type,
                prom_clause                     :: goal,
                prom_varset                     :: prog_varset,
                prom_univ_quant_vars            :: list(prog_var),
                prom_context                    :: prog_context,
                prom_seq_num                    :: item_seq_num
            ).

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

:- type item_initialise_info
    --->    item_initialise_info(
                % :- initialise pred_name.
                init_name                       :: sym_name,
                init_arity                      :: user_arity,
                init_maybe_attrs                :: item_maybe_attrs,
                init_context                    :: prog_context,
                init_seq_num                    :: item_seq_num
            ).

:- type item_finalise_info
    --->    item_finalise_info(
                % :- finalise pred_name.
                final_name                      :: sym_name,
                final_arity                     :: user_arity,
                final_maybe_attrs               :: item_maybe_attrs,
                final_context                   :: prog_context,
                final_seq_num                   :: item_seq_num
            ).

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

:- type item_type_repn_info_eqv
    == item_type_repn_info_general(mer_type).
:- type item_type_repn_info_subtype
    == item_type_repn_info_general(type_ctor).
:- type item_type_repn_info
    == item_type_repn_info_general(type_ctor_repn_info).

:- type item_type_repn_info_general(T)
    --->    item_type_repn_info(
                % `:- type_representation ...':
                % An item added by the compiler to a .int3 file
                % to tell readers of that file the information they need
                % to correctly reconstruct the representation of the given
                % type constructor, even when that information is supposed
                % to be invisible to them semantically.
                % There should be at most one such item for any type_ctor
                % in the .int3 file of its defining module.
                % The sym_name should be fully qualified.
                tr_ctor                         :: sym_name,
                tr_ctor_arg_tvars               :: list(tvar),
                tr_ctor_repn_info               :: T,
                tr_tvarset                      :: tvarset,
                tr_context                      :: prog_context,
                tr_seq_num                      :: item_seq_num
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
% The representation of a checked-to-be-consistent set of type and
% foreign enum definitions for every type constructor defined in a module.
%

:- type type_ctor_checked_map == map(type_ctor, type_ctor_checked_defn).

    % A type is either a solver type, or not.
:- type type_ctor_checked_defn
    --->    checked_defn_solver(solver_type_defn, src_defns_solver)
    ;       checked_defn_std(std_type_defn, src_defns_std).

%---------------------%

    % Replace this one general type with one type for each function symbol
    % in solver_type_defn.
:- type src_defns_solver
    --->    src_defns_solver(
                % The item_type_defn_info (if any) in the interface section.
                maybe(item_type_defn_info),

                % The item_type_defn_info (if any) in the impl section.
                maybe(item_type_defn_info)
            ).

    % Replace this one general type with one type for each function symbol
    % in std_type_defn.
:- type src_defns_std
    --->    src_defns_std(
                % The item_type_defn_infos in the interface section.
                list(item_type_defn_info),

                % The item_type_defn_infos and item_foreign_enum_infos
                % in the implementation section.
                list(item_type_defn_info),
                list(item_foreign_enum_info)
            ).

%---------------------%

:- type solver_type_defn
    --->    solver_type_abstract(
                abstract_solver_type_status,

                % The abstract definition. It may be in either section;
                % the status specifies the section.
                item_type_defn_info_abstract
            )
    ;       solver_type_full(
                % The abstract definition in the interface section,
                % if one exists.
                maybe(item_type_defn_info_abstract),

                % The full solver type definition, which must be in the
                % implementation section.
                item_type_defn_info_solver
            ).

:- type abstract_solver_type_status
    --->    abstract_solver_type_exported
            % The type name is exported. The abstract definition
            % is in the interface section.
    ;       abstract_solver_type_private.
            % The type name is not exported. The abstract definition
            % is in the implementation section.

%---------------------%

:- type std_type_defn
    --->    std_mer_type_eqv(
                std_eqv_type_status,

                % The equivalence type definition.
                item_type_defn_info_eqv
            )
    ;       std_mer_type_subtype(
                std_subtype_status,

                % The subtype definition.
                item_type_defn_info_sub
            )
    ;       std_mer_type_du_all_plain_constants(
                std_du_type_status,

                % The discriminated union type definition which represents
                % either a direct dummy type or an enum.
                item_type_defn_info_du,

                % The first functor name in the type, and any later functor
                % names. If there are no later functor names, then the type
                % is a direct dummy type, and must satisfy the requirements
                % of non_sub_du_type_is_dummy; if there are, then the type
                % is an enum type, and must satisfy the requirements of
                % non_sub_du_type_is_enum. (Function symbols that do not meet
                % the relevant requirements may be constants, but we
                % don't consider them *plain* constants.)
                string,
                list(string),

                % For each of our target foreign languages, this field
                % specifies whether we have either a foreign language
                % definition for this type, or a foreign enum definition.
                %
                % While the Mercury representation uses small integers
                % allocated consecutively from 0 to represent function symbols,
                % this is not true even for foreign enum definitions,
                % much less foreign type definitions.
                c_j_cs_maybe_defn_or_enum
            )
    ;       std_mer_type_du_not_all_plain_constants(
                std_du_type_status,

                % The discriminated union type definition which represents
                % a type *other* than a direct dummy type or an enum.
                item_type_defn_info_du,

                % For each of our target foreign languages, this field
                % specifies whether we have a foreign language type definition
                % for this type.
                c_j_cs_maybe_defn
            )
    ;       std_mer_type_abstract(
                std_abs_type_status,

                % The abstract declaration of the type (not a subtype).
                item_type_defn_info_abstract,

                % For each of our target foreign languages, this field
                % specifies whether we have a foreign language type definition
                % for this type.
                c_j_cs_maybe_defn
            ).

:- type maybe_only_constants
    --->    not_only_plain_constants
    ;       only_plain_constants(
                % The names of the constants, in the order of declaration.
                opc_head_name       :: string,
                opc_tail_names      :: list(string)
            ).

:- type std_eqv_type_status
    --->    std_eqv_type_mer_exported
            % The Mercury definition (i.e. the equivalence) is exported.
    ;       std_eqv_type_abstract_exported
            % Only the type name is exported. The Mercury definition
            % is private.
    ;       std_eqv_type_all_private.
            % Everything about the type is private.

:- type std_du_type_status
    --->    std_du_type_mer_ft_exported
            % Both the Mercury and any foreign type definitions are exported.
            % Any foreign enum definitions are private, as they have to be.
            % This status is not applicable to equivalence types or subtypes,
            % since they may not have foreign type definitions.
    ;       std_du_type_mer_exported
            % The Mercury definition is exported. Any foreign type definitions
            % and/or foreign enum definitions are private.
    ;       std_du_type_abstract_exported
            % Only the type name is exported. The Mercury definition and
            % any foreign type definitions and/or foreign enum definitions
            % are private.
    ;       std_du_type_all_private.
            % Everything about the type is private.

    % A version of std_du_type_status for subtypes, which may not have
    % any foreign type definitions, and for which therefore the question of
    % whether any foreign type definitions are exported is moot.
:- type std_subtype_status
    --->    std_sub_type_mer_exported
    ;       std_sub_type_abstract_exported
    ;       std_sub_type_all_private.

:- type std_abs_type_status
    --->    std_abs_type_ft_exported
            % The type has foreign type definitions that are exported.
            % Any foreign enum definitions are private, as they have to be.
    ;       std_abs_type_abstract_exported
            % Only the type name is exported. Any foreign type definitions
            % and/or foreign enum definitions are private.
    ;       std_abs_type_all_private.
            % Everything about the type is private.

%---------------------%

:- type c_j_cs_maybe_defn_or_enum ==
    c_java_csharp(maybe(foreign_type_or_enum)).

:- type foreign_type_or_enum
    --->    foreign_type_or_enum_type(item_type_defn_info_foreign)
    ;       foreign_type_or_enum_enum(checked_foreign_enum).

    % Part of checking a foreign enum definition is checking whether
    % the correspondence it describes between the Mercury functors
    % of the type on the one hand and their foreign language counterparts
    % on the other hand is a bijection. If it is, then the second argument
    % of the checked_foreign_enum we construct gives the foreign language
    % counterpart of each Mercury function symbol in the type in the order
    % in which the Mercury function symbols are defined.
    %
    % For example, given
    %
    %   :- type t ---> m1 ; m2 ; m3.
    %
    % and a foreign enum definition that gives the correspondence correctly
    % but in a different order, such as
    %
    %   :- pragma foreign_enum("C", t/0, [m2 - "f2", m3 - "f3", m1 - "f1"]).
    %
    % the second argument will contain the (nonempty) list "f1", "f2", "f3".
    %
    % On the other hand, if the mapping in the foreign enum definition is
    % *not* a bijection, then we will not generate a checked_foreign_enum
    % structure for it.
    %
:- type checked_foreign_enum
    --->    checked_foreign_enum(item_foreign_enum_info, one_or_more(string)).

%---------------------------------------------------------------------------%
%
% The representation of a checked-to-be-consistent set of inst definitions
% for every inst constructor defined in a module.
%

:- type inst_ctor_checked_map == map(inst_ctor, inst_ctor_checked_defn).

:- type inst_ctor_checked_defn
    --->    checked_defn_inst(std_inst_defn, src_defns_inst).

:- type std_inst_defn
    --->    std_inst_defn(std_inst_status, item_inst_defn_info).

:- type std_inst_status
    --->    std_inst_exported
            % The inst definition is exported.
    ;       std_inst_abstract_exported
            % Only the inst name is exported. Its definition is private.
    ;       std_inst_all_private.
            % Everything about the inst is private.

:- type src_defns_inst
    --->    src_defns_inst(
                % The inst definition (if any) in the interface.
                maybe(item_inst_defn_info),

                % The inst definition (if any) in the implementation.
                maybe(item_inst_defn_info)
            ).

%---------------------------------------------------------------------------%
%
% The representation of a checked-to-be-consistent set of mode definitions
% for every mode constructor defined in a module.
%

:- type mode_ctor_checked_map == map(mode_ctor, mode_ctor_checked_defn).

:- type mode_ctor_checked_defn
    --->    checked_defn_mode(std_mode_defn, src_defns_mode).

:- type std_mode_defn
    --->    std_mode_defn(std_mode_status, item_mode_defn_info).

:- type std_mode_status
    --->    std_mode_exported
            % The mode definition is exported.
    ;       std_mode_abstract_exported
            % Only the mode name is exported. Its definition is private.
    ;       std_mode_all_private.
            % Everything about the mode is private.

:- type src_defns_mode
    --->    src_defns_mode(
                % The mode definition (if any) in the interface.
                maybe(item_mode_defn_info),

                % The mode definition (if any) in the implementation.
                maybe(item_mode_defn_info)
            ).

%---------------------------------------------------------------------------%
%
% Information about the representations of types defined in other modules.
%

    % This type and type_ctor_checked_defn are closely related.
    % The principal differences are the following.
    %
    % - type_ctor_checked_defn deals with solver types. Since solver types
    %   have no representation information themselves (they are represented
    %   by values of another type), this type does not deal with them.
    %
    % - One of the purposes of type_ctor_checked_defn is to decide
    %   what items to include in interface files, for use by code using
    %   the compiler's ancient approach to deciding type representation,
    %   where each compiler invocation that generated code decided for itself
    %   how every type it had access to was represented, including the types
    %   imported from other modules. This means that it needs to contain
    %   either whole items (of particular kinds), or information from which
    %   whole items can be reconstructed.
    %
    % - The above consideration also requires a type_ctor_checked_defn
    %   to specify the status of the type. On the other hand, values of
    %   this type have no use for status information. Status information
    %   is used only for checking whether an access to a type should be
    %   allowed or not; the only use of values of this type is to help
    %   compute type representations.
    %
    % - Only this type needs to contain representation information.
    %   A value of the type_ctor_checked_defn type needs to contain *part*
    %   of the information from which this representation information is
    %   computed for its type, but not *all* of it; some of that information
    %   comes from information about the representation of *other* types.
    %
    % One sort-of difference is while both contain information that has been
    % checked by a compiler invocation, values of this type that have been
    % read in from an interface file, while checked by another compiler
    % invocation before being written out, may be corrupted in the filesystem.
    % However, while this danger is always present, we need not take any
    % special steps to guard against it, precisely because no perfect defense
    % is possible.
    %
    % XXX TYPE_REPN Consider whether we can split this type into two,
    % one for the tcrepns that can occur in .int3 files, and one for the
    % tcrepns that can occur in .int/.int2 files.
    %
:- type type_ctor_repn_info
    --->    tcrepn_is_word_aligned_ptr
    ;       tcrepn_is_eqv_to(mer_type)
    ;       tcrepn_is_subtype_of(type_ctor)
    ;       tcrepn_du(du_repn)
    ;       tcrepn_foreign(c_j_cs_repn).

    % A type that has a discriminated union definition in Mercury
    % may also have a definition in each of our foreign languages,
    % If it is an direct_dummy or enum type, that definition may be
    % either a foreign type definition or a foreign enum definition;
    % otherwise, it can only be a foreign type definition.
:- type du_repn
    --->    dur_direct_dummy(direct_dummy_repn)
    ;       dur_enum(enum_repn)
    ;       dur_notag(notag_repn)
    ;       dur_gen_only_functor(gen_du_only_functor_repn)
    ;       dur_gen_more_functors(gen_du_more_functors_repn).

    % When targeting C, many argument packing decisions depend on
    % three properties of the target platform, i.e. on the combination
    % of the target hardware and the target grade:
    %
    % - whether the target is 64 or 32 bit;
    % - whether the grade is an spf (single-precision float) grade; and
    % - whether the grade allows the direct arg optimization.
    %
    % These have eight combinations, but the spf grade component has
    % no effect on argument packing on 64 bit targets (a float is one word
    % either way), so only six are meaningful.
    %
    % If the decision represented by the T parameter happens to be the same
    % on all six platforms, that decision can be represented by c_repns_same.
    %
    % If they are different on 64 vs 32 bit platforms, but are consistent
    % for each word size, then they can be represented by c_repns_64_32.
    %
    % If neither is the case, we can record all six decisions using
    % c_repns_all.
    %
    % XXX We should look for other partitions of the set of six platforms
    % which often have identical decision results; one could be da vs noda.
    %
    % The name of this type is c_repns because argument packing applies
    % only to the low level data representation, which is applicable only
    % when targeting C.
:- type c_repns(T)
    --->    c_repns_same(
                c_repn_same             :: T
            )
    ;       c_repns_64_32(
                c_repn_all_64           :: T,
                c_repn_all_32           :: T
            )
    ;       c_repns_all(
                c_repn_64_nospf_noda    :: T,
                c_repn_64_nospf_da      :: T,
                % c_repn_64_spf_noda    :: T,   % not needed; see above
                % c_repn_64_spf_da      :: T,   % not needed; see above
                c_repn_32_nospf_noda    :: T,
                c_repn_32_nospf_da      :: T,
                c_repn_32_spf_noda      :: T,
                c_repn_32_spf_da        :: T
            ).

%---------------------%

:- type direct_dummy_repn
    --->    direct_dummy_repn(
                % The type is a direct dummy type that satisfies the
                % requirements of du_type_is_dummy.

                % The name of the one functor in the type, which must be
                % arity 0. Its representation will be dummy_tag.
                dummy_functor_name      :: string,

                % Any foreign type or foreign enum definitions for the type.
                dummy_foreign           :: c_j_cs_enum_repn
            ).

%---------------------%

:- type enum_repn
    --->    enum_repn(
                % The type is an enum type that satisfies the requirements
                % of non_sub_du_type_is_enum.

                % The list of the functor names (all arity 0). We store
                % the first two separately to enforce the structural invariant
                % that an enum must have at least two functors.
                %
                % The representation of functor #N in Mercury will be
                % int_tag(int_tag_int(N)), with counting starting at 0.
                %
                % We do not care about the 32 vs 64 bit distinction here,
                % because the definition of an enum type with more than 2^32
                % function symbols will cause a compiler to run out of memory
                % for a *very* long time to come.
                enum_functor1           :: string,
                enum_functor2           :: string,
                enum_functors3plus      :: list(string),

                % Any foreign type or foreign enum definitions for the type.
                enum_foreign            :: c_j_cs_enum_repn
            ).

%---------------------%

:- type notag_repn
    --->    notag_repn(
                % The name of the one functor in the type, which must be
                % arity 1. Its representation will be no_tag.
                % The representation of the argument be *recorded*
                % as a full word at offset 0, but this should never be
                % looked up, since the argument will actually be stored
                % wherever the whole term is stored.
                notag_functor_name      :: string,

                % The type of the one functor's one argument.
                % We record this because without this information,
                % we cannot recognize that a notag type whose argument size
                % is less than one word can itself be stored in less than
                % one word.
                notag_functor_arg_type  :: mer_type,

                % The foreign language definitions for this type, if any.
                notag_foreign           :: c_j_cs_repn
            ).

%---------------------%

:- type gen_du_only_functor_repn
    --->    gen_du_only_functor_repn(
                % The name of the data constructor. The arity is given by
                % the length of list of argument types. The lists of argument
                % representations in all of the nonconstant_repns inside
                % the c_repns must also ave this length.
                only_functor            :: string,

                % The types of the constructor's arguments, after
                % the expansion of both equivalence types and notag types.
                only_deref_arg_types    :: list(mer_type),

                % The representation of this functor for each possible
                % target platform with the low level data representation.
                % The nonconstant_repn cannot be ncr_direct_arg.
                % XXX TYPE_REPN could we encode that invariant in the type?
                only_arg_repns          :: c_repns(only_nonconstant_repn),

                % The foreign language definitions for this type, if any.
                only_foreign            :: c_j_cs_repn
            ).

:- type gen_du_more_functors_repn
    --->    gen_du_more_functors_repn(
                % The first, second and any later functors in the type,
                % in declaration order, i.e. ordered on the functors'
                % original ordinal numbers.
                more_functor1           :: gen_du_functor_repn,
                more_functor2           :: gen_du_functor_repn,
                more_functors3plus      :: list(gen_du_functor_repn),

                % The foreign language definitions for this type, if any.
                more_foreign            :: c_j_cs_repn
            ).

%---------------------%

:- type gen_du_functor_repn
    --->    gen_du_constant_functor_repn(
                % The name of the data constructor. The arity is zero.
                gducf_functor           :: string,

                % The representation of this functor for each possible
                % target platform with the low level data representation.
                gducf_functor_repn      :: c_repns(constant_repn)
            )
    ;       gen_du_nonconstant_functor_repn(
                % The name of the data constructor. The arity is given by
                % the length of list of argument types. The lists of argument
                % representations in all of the nonconstant_repns inside
                % the c_repns must also ave this length.
                gduncf_functor          :: string,

                % The types of the constructor's arguments, after
                % the expansion of both equivalence types and notag types.
                %
                % Logically, the type of each argument belongs with
                % the representation of that argument, but we have to store
                % up to six versions of the representation, and we don't want
                % a duplicate copy of the type next to each version.
                gduncf_deref_arg_types  :: list(mer_type),

                % The representation of this functor for each possible
                % target platform with the low level data representation.
                gduncf_functor_repn     :: c_repns(more_nonconstant_repn)
            ).

:- type constant_repn
    --->    constant_repn(
                % The ptag is 0. The next two fields specify the value
                % and the size of the local secondary tag.
                cr_sectag               :: uint,
                cr_sectag_size          :: lsectag_word_or_size
            ).

:- type only_nonconstant_repn
    --->    oncr_local_cell(only_nonconstant_local_cell_repn)
    ;       oncr_remote_cell(only_nonconstant_remote_cell_repn).

:- type more_nonconstant_repn
    --->    mncr_local_cell(more_nonconstant_local_cell_repn)
    ;       mncr_remote_cell(more_nonconstant_remote_cell_repn)
    ;       mncr_direct_arg(ptag).

:- type only_nonconstant_local_cell_repn
    --->    only_nonconstant_local_cell_repn(
                % The ptag and local sectag are both implicitly 0u.
                onclcr_arg_repns        :: one_or_more(local_arg_repn)
            ).

:- type more_nonconstant_local_cell_repn
    --->    more_nonconstant_local_cell_repn(
                % The ptag is implicitly 0u.
                mnclcr_sectag           :: cell_local_sectag,
                mnclcr_arg_repns        :: one_or_more(local_arg_repn)
            ).

:- type only_nonconstant_remote_cell_repn
    --->    only_nonconstant_remote_cell_repn(
                % The ptag is both implicitly 0u, and there is
                % no remote sectag.
                ncrcr_arg_repns         :: one_or_more(remote_arg_repn)
            ).

:- type more_nonconstant_remote_cell_repn
    --->    more_nonconstant_remote_cell_repn(
                ncrcr_ptag              :: ptag,
                ncrcr_sectag            :: cell_remote_sectag,
                ncrcr_arg_repns         :: one_or_more(remote_arg_repn)
            ).

:- type cell_local_sectag
    --->    cell_local_sectag(
                clss_sectag             :: uint,
                clss_sectag_size        :: uint8
            ).

:- type cell_remote_sectag
    --->    cell_remote_no_sectag
    ;       cell_remote_sectag(
                crss_sectag             :: uint,
                crss_sectag_size        :: rsectag_word_or_size
            ).

:- type lsectag_word_or_size
    --->    lsectag_rest_of_word(uint8)
    ;       lsectag_part_of_word(uint8).

:- type rsectag_word_or_size
    --->    rsectag_full_word
    ;       rsectag_part_of_word(uint8).

:- type local_arg_repn
    --->    local_partial(
                lp_shift                :: uint,
                lp_fill                 :: fill_kind_size
            )
    ;       local_none.

:- type remote_arg_repn
    --->    remote_full(
                rf_arg_only_offset      :: arg_only_offset,
                rf_cell_offset          :: cell_offset
            )
    ;       remote_double(
                rd_arg_only_offset      :: arg_only_offset,
                rd_cell_offset          :: cell_offset,
                rd_kind                 :: double_word_kind
            )
    ;       remote_partial_first(
                rpf_arg_only_offset     :: arg_only_offset,
                rpf_cell_offset         :: cell_offset,
                rpf_shift               :: uint8,
                rpf_fill                :: fill_kind_size
            )
    ;       remote_partial_shifted(
                rps_arg_only_offset     :: arg_only_offset,
                rps_cell_offset         :: cell_offset,
                rps_shift               :: uint8,
                rps_fill                :: fill_kind_size
            )
    ;       remote_none_shifted(
                rns_arg_only_offset     :: arg_only_offset,
                rns_cell_offset         :: cell_offset
            )
    ;       remote_none_nowhere.

:- type fill_kind_size
    --->    fk_enum(uint)   % XXX TYPE_REPN should be uint8
    ;       fk_int8
    ;       fk_int16
    ;       fk_int32
    ;       fk_uint8
    ;       fk_uint16
    ;       fk_uint32
    ;       fk_char21.

    % XXX TYPE_REPN should return uint8
:- func fill_kind_size_num_bits(fill_kind_size) = uint.

%---------------------%

:- type foreign_type_lang_repn
    --->    foreign_type_lang_repn(
                ftlr_lang               :: foreign_language,
                ftlr_foreign_type       :: foreign_type_repn
            ).

:- type foreign_type_repn
    --->    foreign_type_repn(
                % The name of the foreign type that represents values
                % of this Mercury type.
                ftr_foreign_type        :: string,

                % The assertions about this foreign type.
                ftr_assertions          :: foreign_type_assertions
            ).

:- type enum_foreign_repn
    --->    enum_foreign_type(foreign_type_repn)
    ;       enum_foreign_enum(one_or_more(string)).

%---------------------------------------------------------------------------%
%
% Pragmas.
%

:- type item_decl_pragma_info
    --->    decl_pragma_obsolete_pred(decl_pragma_obsolete_pred_info)
    ;       decl_pragma_obsolete_proc(decl_pragma_obsolete_proc_info)
    ;       decl_pragma_format_call(decl_pragma_format_call_info)
    ;       decl_pragma_type_spec_constr(decl_pragma_type_spec_constr_info)
    ;       decl_pragma_type_spec(decl_pragma_type_spec_info)
    ;       decl_pragma_oisu(decl_pragma_oisu_info)
    ;       decl_pragma_termination(decl_pragma_termination_info)
    ;       decl_pragma_termination2(decl_pragma_termination2_info)
    ;       decl_pragma_struct_sharing(decl_pragma_struct_sharing_info)
    ;       decl_pragma_struct_reuse(decl_pragma_struct_reuse_info).

:- type item_impl_pragma_info
    --->    impl_pragma_foreign_decl(impl_pragma_foreign_decl_info)
    ;       impl_pragma_foreign_code(impl_pragma_foreign_code_info)
    ;       impl_pragma_fproc_export(impl_pragma_fproc_export_info)
    ;       impl_pragma_external_proc(impl_pragma_external_proc_info)
    ;       impl_pragma_fact_table(impl_pragma_fact_table_info)
    ;       impl_pragma_tabled(impl_pragma_tabled_info)
    ;       impl_pragma_req_tail_rec(impl_pragma_req_tail_rec_info)
    ;       impl_pragma_req_feature_set(impl_pragma_req_feature_set_info).

:- type item_generated_pragma_info
    --->    gen_pragma_unused_args(gen_pragma_unused_args_info)
    ;       gen_pragma_exceptions(gen_pragma_exceptions_info)
    ;       gen_pragma_trailing(gen_pragma_trailing_info)
    ;       gen_pragma_mm_tabling(gen_pragma_mm_tabling_info).

%---------------------------------------------------------------------------%
%
% Decl pragmas.
%

:- type decl_pragma_obsolete_pred_info
    --->    decl_pragma_obsolete_pred_info(
                obspred_obsolete_pred   :: pred_pfu_name_arity,
                obspred_in_favour_of    :: list(sym_name_arity),
                obspred_context         :: prog_context,
                obspred_seq_num         :: item_seq_num
            ).

:- type decl_pragma_obsolete_proc_info
    --->    decl_pragma_obsolete_proc_info(
                obsproc_obsolete_proc   :: proc_pf_name_modes,
                obsproc_in_favour_of    :: list(sym_name_arity),
                obsproc_context         :: prog_context,
                obsproc_seq_num         :: item_seq_num
            ).

:- type decl_pragma_format_call_info
    --->    decl_pragma_format_call_info(
                format_pred             :: pred_pf_name_arity,
                format_values           :: one_or_more(format_string_values),
                format_context          :: prog_context,
                format_seq_num          :: item_seq_num
            ).

:- type decl_pragma_type_spec_constr_info
    --->    decl_pragma_type_spec_constr_info(
                % The name of the module from whose (source or interface) file
                % we read the type_spec_constrained_preds pragma. This will
                % always name the module that contains the pragma, because
                % we never put a type_spec_constrained_preds pragma into
                % any interface file other than an interface file of the
                % module containing the pragma.
                tsc_module_name         :: module_name,

                % The list of constraints in the first argument of the pragma.
                % The pragma asks for the type specialization of any predicates
                % whose class context includes any nonempty subset of these
                % constraints, and possibly (see the next field) their
                % superclasses, as instances.
                tsc_constraints     :: one_or_more(var_or_ground_constraint),

                % The second argument of the pragma, which specifies whether
                % the constraints in the first argument also implicitly specify
                % their superclasses, *their* superclasses, and so on.
                % If e.g. tc1(A, B, C) has tc2(A, B) as one of its
                % superclasses, then a setting of apply_to_supers in this field
                % means that the pragma asks us to specialize not only
                % predicates whose class context includes tc1(A, char, B)
                % (if that is has as its instance of one of the constraints),
                % but also e.g. tc2(A, char).
                tsc_apply_to_supers     :: maybe_apply_to_supers,

                % The third argument of the pragma, which specifies the list
                % of type substitutions for which the pragma asks us to create
                % type-specialized versions of each predicate that matches
                % the requirements described by the first and second args.
                %
                % Each type var on the left-hand-side of a substitution
                % must occur in tsc_constraints, while all type vars that
                % occur in a type on the right-hand-side of a substitution
                % must be anonymous. These requirements are enforced by the
                % code that parses these pragmas.
                tsc_tsubst              :: one_or_more(type_subst),

                % The varset of the term containing the pragma, coerced
                % to being a tvarset (since all variables in the pragma
                % are type variables).
                %
                % All variables in this tvarset have to have explicit names.
                % If the original pragma contains anonymous variables, the
                % code constructing this decl_pragma_type_spec will give
                % those variable names. See the comment on the tspec_tvarset
                % field below for the reason behind this requirement.
                tsc_tvarset             :: tvarset,

                % The equivalence types used.
                tsc_items               :: set(recomp_item_id),

                tsc_context             :: prog_context,
                tsc_seq_num             :: item_seq_num
            ).

:- type decl_pragma_type_spec_info
    --->    decl_pragma_type_spec_info(
                tspec_pfumm             :: pred_func_or_unknown_maybe_modes,

                % The existing predicate name.
                tspec_pred_name         :: sym_name,

                % The name of the module from whose (source or interface) file
                % we read the type_spec pragma. This will always name
                % the module that contains the pragma, because we never put
                % a type_spec pragma into any interface file other than
                % an interface file of the module containing the pragma.
                tspec_module_name       :: module_name,

                % The type substitution (using the variable names
                % from the pred declaration).
                tspec_tsubst            :: type_subst,

                % The varset of the term containing the pragma, coerced
                % to being a tvarset (since no part of the pragma except
                % the type substitution may contain variables).
                %
                % All variables in this tvarset have to have explicit names.
                % If the original pragma contains anonymous variables, the
                % code constructing this decl_pragma_type_spec will give
                % those variable names.
                %
                % The reason for this requirement is that the process
                % of writing out an anonymous variable and reading it back in
                % will produce a non-anonymous variable. Since the names
                % (if any) of the variables in tspec_tsubst are an input
                % to the code that constructs the name of the type-specialized
                % predicate, we would get a discrepancy between the predicate
                % name constructed by compiler invocations that know the
                % variable as unnamed (this will be the invocation that
                % compiles the module containing the type_spec pragma,
                % which constructs the code of the type specialized predicate),
                % and compiler invocations that know that variable as named
                % (this will be all the invocations that read the original
                % module's .int file, which will be constructing many of
                % the *calls* to the type specialized predicate). The result
                % will be calls to the type specialized predicate that refer
                % to it by the wrong name, leading to link errors.
                %
                % By giving all anonymous variables in the type_spec pragma
                % in the original source file as soon as we have parsed it,
                % and then always using the resulting names, we avoid this
                % problem.
                tspec_tvarset           :: tvarset,

                % The equivalence types used.
                tspec_items             :: set(recomp_item_id),

                tspec_context           :: prog_context,
                tspec_seq_num           :: item_seq_num
            ).

:- type var_or_ground_constraint
    --->    var_or_ground_constraint(
                class_name,
                list(var_or_ground_type),
                prog_context
            ).

:- type var_or_ground_type
    --->    type_var_name(tvar, string)
    ;       ground_type(ground_type).

:- type maybe_apply_to_supers
    --->    do_not_apply_to_supers
    ;       apply_to_supers.

:- type decl_pragma_oisu_info
    --->    decl_pragma_oisu_info(
                oisu_type_ctor          :: type_ctor,
                oisu_creator_preds      :: list(pred_pf_name_arity),
                oisu_transformer_preds  :: list(pred_pf_name_arity),
                oisu_destroyer_preds    :: list(pred_pf_name_arity),
                oisu_context            :: prog_context,
                oisu_seq_num            :: item_seq_num
            ).

% The termination/termination2 pragmas record information
% about a predicate's or function's termination properties for our
% two different termination analyzers. Even though they are usually
% compiler generated, they are decl pragmas, not gen pragmas, because
% we allow users to include them in Mercury source programs, to tell
% the analyzers some things that they cannot figure out for themselves,
% such as the termination properties of foreign language code in
% foreign_procs.

:- type decl_pragma_termination_info
    --->    decl_pragma_termination_info(
                % The modes represent the declared argmodes of the procedure,
                % unless there are no declared argmodes, in which case
                % we use the inferred argmodes.
                terminfo_pred_id        :: proc_pf_name_modes,
                terminfo_args           :: maybe(pragma_arg_size_info),
                terminfo_term           :: maybe(pragma_termination_info),
                terminfo_context        :: prog_context,
                terminfo_seq_num        :: item_seq_num
            ).

:- type decl_pragma_termination2_info
    --->    decl_pragma_termination2_info(
                terminfo2_pred_id       :: proc_pf_name_modes,
                terminfo2_args          :: maybe(pragma_constr_arg_size_info),
                terminfo2_args2         :: maybe(pragma_constr_arg_size_info),
                terminfo2_term          :: maybe(pragma_termination_info),
                terminfo2_context       :: prog_context,
                terminfo2_seq_num       :: item_seq_num
            ).

% The sharing/reuse pragmas record information about a predicate's or
% function's properties that are relevant for compile-time garbage
% collection (ctgx). Even though they are usually compiler generated,
% they are decl pragmas, not gen pragmas, because we allow users
% to include them in Mercury source programs, to tell the compiler some things
% that it cannot figure out for itself, such as the ctgc properties
% of foreign language code in foreign_procs.

:- type decl_pragma_struct_sharing_info
    --->    decl_pragma_struct_sharing_info(
                % After structure sharing analysis, the compiler generates
                % structure sharing pragmas to be stored in and read from
                % optimization interface files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                sharing_pred_id         :: proc_pf_name_modes,
                sharing_headvars        :: list(prog_var),
                sharing_headvar_types   :: list(mer_type),

                % The prog_varset and tvarset are meaningful only when
                % writing out this pragma; add_pragma.m ignores both varsets.
                sharing_varset          :: prog_varset,
                sharing_tvarset         :: tvarset,

                % As of 2019 10 29, and probably long before then,
                % the compiler *always* fills this slot with `yes(...)'.
                % A `no' would mean that the relevant information is not
                % available, but in that case, we simply do not write out
                % this pragma.
                sharing_description     :: maybe(structure_sharing_domain),

                sharing_context         :: prog_context,
                sharing_seq_num         :: item_seq_num
            ).

:- type decl_pragma_struct_reuse_info
    --->    decl_pragma_struct_reuse_info(
                % After reuse analysis, the compiler generates structure reuse
                % pragmas to be stored in and read from optimization interface
                % files.
                %
                % The list of modes consists of the declared argmodes
                % (or inferred argmodes if there are no declared ones).
                %
                % The last sym_name (reuse_optimised_name) stores the name
                % of the optimised version of the exported predicate.
                % XXX As of 2019 10 29, the word "reuse_optimised_name"
                % appears nowhere in the compiler apart from this comment.
                reuse_pred_id           :: proc_pf_name_modes,
                reuse_headvars          :: list(prog_var),
                reuse_headvar_types     :: list(mer_type),

                % The prog_varset and tvarset are meaningful only when
                % writing out this pragma; add_pragma.m ignores both varsets.
                reuse_varset            :: prog_varset,
                reuse_tvarset           :: tvarset,

                % As of 2019 10 29, and probably long before then,
                % the compiler *always* fills this slot with `yes(...)'.
                % A `no' would mean that the relevant information is not
                % available, but in that case, we simply do not write out
                % this pragma.
                reuse_description       :: maybe(structure_reuse_domain),

                reuse_context           :: prog_context,
                reuse_seq_num           :: item_seq_num
            ).

:- type item_decl_marker_info
    --->    item_decl_marker_info(
                dm_marker_kind          :: decl_pragma_marker_kind,
                dm_pred_spec            :: pred_pfu_name_arity,
                dm_context              :: prog_context,
                dm_seq_num              :: item_seq_num
            ).

:- type item_decl_marker_info_opt =< item_decl_marker_info
    --->    item_decl_marker_info(
                dm_marker_kind          :: decl_pragma_marker_kind_opt,
                dm_pred_spec            :: pred_pfu_name_arity_pf,
                dm_context              :: prog_context,
                dm_seq_num              :: item_seq_num
            ).

    % XXX The "terminates" and "does_not_terminate" markers are assertions
    % about the behavior of a given predicate that the compiler may be able
    % to exploit when compiling other modules. The "check_termination" marker
    % is not like that: it is a directive that is useful only while
    % the compiler is working on the module in which it occurs. We should
    % therefore consider making this an *impl* marker, which would entail
    % allowing the "check_termination" pragma to occur only in implementation
    % sections, even when the predicate/function they name is exported.
:- type decl_pragma_marker_kind
    --->    dpmk_terminates
    ;       dpmk_does_not_terminate
    ;       dpmk_check_termination.

:- type decl_pragma_marker_kind_opt =< decl_pragma_marker_kind
    --->    dpmk_terminates
    ;       dpmk_does_not_terminate.

%---------------------------------------------------------------------------%
%
% Impl pragmas.
%

:- type impl_pragma_foreign_decl_info
    --->    impl_pragma_foreign_decl_info(
                % A foreign language declaration, such as C header code.
                decl_lang               :: foreign_language,
                decl_is_local           :: foreign_decl_is_local,
                decl_decl               :: foreign_literal_or_include,
                decl_context            :: prog_context,
                decl_seq_num            :: item_seq_num
            ).

:- type impl_pragma_foreign_code_info
    --->    impl_pragma_foreign_code_info(
                code_lang               :: foreign_language,
                code_code               :: foreign_literal_or_include,
                code_context            :: prog_context,
                code_seq_num            :: item_seq_num
            ).

:- type impl_pragma_fproc_export_info
    --->    impl_pragma_fproc_export_info(
                exp_maybe_attrs         :: item_maybe_attrs,

                exp_language            :: foreign_language,
                % Predname, Predicate/function, Modes, foreign function name.
                exp_pred_id             :: proc_pf_name_modes,
                exp_foreign_name        :: string,

                % Specified the names of any variables in the modes above.
                % Used for generating error messages about foreign_export
                % pragmas for undeclared modes.
                exp_varset              :: prog_varset,

                exp_context             :: prog_context,
                exp_seq_num             :: item_seq_num
            ).

:- type impl_pragma_external_proc_info
    --->    impl_pragma_external_proc_info(
                % The specified procedure(s) is/are implemented outside
                % of Mercury code, for the named backend if there is one,
                % or if there isn't a named backend, then for all backends.
                external_name           :: pred_pf_name_arity,
                external_maybe_backend  :: maybe(backend),
                external_context        :: prog_context,
                external_seq_num        :: item_seq_num
            ).

:- type impl_pragma_fact_table_info
    --->    impl_pragma_fact_table_info(
                % Predname and Arity, Fact file name.
                fact_table_pred         :: pred_pfu_name_arity,
                fact_table_filename     :: string,
                fact_table_context      :: prog_context,
                fact_table_seq_num      :: item_seq_num
            ).

:- type impl_pragma_tabled_info
    --->    impl_pragma_tabled_info(
                % Tabling type, Predname, Arity, PredOrFunc?, Mode?
                tabled_method           :: tabled_eval_method,
                tabled_name             :: pred_or_proc_pfumm_name,
                tabled_attributes       :: maybe(table_attributes),
                tabled_context          :: prog_context,
                tabled_seq_num          :: item_seq_num
            ).

:- type impl_pragma_req_tail_rec_info
    --->    impl_pragma_req_tail_rec_info(
                rtr_proc_id             :: pred_or_proc_pfumm_name,
                rtr_require_tailrec     :: require_tail_recursion,
                % This parameter only makes sense when options contains
                % either rtro_mutual_rec_only or rtro_all_recursion.
                % TODO, currently unused, may be used later to implement one
                % of Zoltan's suggestions here:
                % http://www.mercurylang.org/list-archives/developers/
                %   2015-November/016482.html
                % rtr_maybe_scc           :: maybe(list(
                %                             pred_or_proc_pfumm_name))
                rtr_context             :: prog_context,
                rtr_seq_num             :: item_seq_num
            ).

:- type impl_pragma_req_feature_set_info
    --->    impl_pragma_req_feature_set_info(
                rfs_feature_set         :: set(required_feature),
                rfs_context             :: prog_context,
                rfs_seq_num             :: item_seq_num
            ).

:- type item_impl_marker_info
    --->    item_impl_marker_info(
                im_marker_kind          :: impl_pragma_marker_kind,
                im_pred_spec            :: pred_pfu_name_arity,
                im_context              :: prog_context,
                im_seq_num              :: item_seq_num
            ).

:- type item_impl_marker_info_opt =< item_impl_marker_info
    --->    item_impl_marker_info(
                im_marker_kind          :: impl_pragma_marker_kind_opt,
                im_pred_spec            :: pred_pfu_name_arity_pf,
                im_context              :: prog_context,
                im_seq_num              :: item_seq_num
            ).

:- type impl_pragma_marker_kind
    --->    ipmk_inline
    ;       ipmk_no_inline
    ;       ipmk_consider_used
    ;       ipmk_mode_check_clauses
    ;       ipmk_no_detism_warning
    ;       ipmk_promise_pure
    ;       ipmk_promise_semipure
    ;       ipmk_promise_eqv_clauses
    ;       ipmk_req_sw_arms_type_order.

    % These are the kinds of impl markers that we put into .opt files.
:- type impl_pragma_marker_kind_opt =< impl_pragma_marker_kind
    --->    ipmk_inline
    ;       ipmk_no_inline
    ;       ipmk_mode_check_clauses
    ;       ipmk_promise_pure
    ;       ipmk_promise_semipure
    ;       ipmk_promise_eqv_clauses.

%---------------------------------------------------------------------------%
%
% Generated pragmas.
%

:- type gen_pragma_unused_args_info
    --->    gen_pragma_unused_args_info(
                % This pragma Should only appear in .opt files.
                unused_proc_id          :: proc_pf_name_arity_mn,

                % The argument positions of the unused arguments.
                % Used for intermodule unused argument removal.
                unused_args             :: list(int),

                unused_context          :: prog_context,
                unused_seq_num          :: item_seq_num
            ).

:- type gen_pragma_exceptions_info
    --->    gen_pragma_exceptions_info(
                % This pragma should only appear in `.opt' and
                % `.trans_opt' files.
                exceptions_proc_id      :: proc_pf_name_arity_mn,
                exceptions_status       :: exception_status,

                exceptions_context      :: prog_context,
                exceptions_seq_num      :: item_seq_num
            ).

:- type gen_pragma_trailing_info
    --->    gen_pragma_trailing_info(
                % This pragma should only appear in `.trans_opt' files.
                trailing_proc_id        :: proc_pf_name_arity_mn,
                trailing_status         :: trailing_status,

                trailing_context        :: prog_context,
                trailing_seq_num        :: item_seq_num
            ).

:- type gen_pragma_mm_tabling_info
    --->    gen_pragma_mm_tabling_info(
                % This pragma should only appear in `.opt' and
                % `.trans_opt' files.
                mm_tabling_proc_id      :: proc_pf_name_arity_mn,
                mm_tabling_status       :: mm_tabling_status,

                mm_tabling_context      :: prog_context,
                mm_tabling_seq_num      :: item_seq_num
            ).

%---------------------------------------------------------------------------%

    % These types identify predicates, functions and/or procedures in pragmas.

:- type pred_pfu_name_arity
    --->    pred_pfu_name_arity(
                ppfuna_pfu              :: pred_func_or_unknown,
                ppfuna_pred_name        :: sym_name,
                ppfuna_arity            :: user_arity
            ).

:- type pred_pfu_name_arity_pf =< pred_pfu_name_arity
    --->    pred_pfu_name_arity(
                ppfuna_pfu              :: pred_func_or_unknown_pf,
                ppfuna_pred_name        :: sym_name,
                ppfuna_arity            :: user_arity
            ).

:- type proc_pf_name_arity_mn
    --->    proc_pf_name_arity_mn(
                ppfnamn_pf              :: pred_or_func,
                ppfnamn_pred_name       :: sym_name,
                ppfnamn_arity           :: user_arity,
                ppfnamn_mode_num        :: mode_num
            ).

:- type proc_pf_name_modes
    --->    proc_pf_name_modes(
                ppfnm_pf                :: pred_or_func,
                ppfnm_pred_name         :: sym_name,
                ppfnm_arity             :: list(mer_mode)
            ).

:- type pred_or_proc_pfumm_name
    --->    pred_or_proc_pfumm_name(
                ppfummn_pfumm           :: pred_func_or_unknown_maybe_modes,
                ppfummn_pred_name       :: sym_name
            ).

:- type pred_func_or_unknown
    --->    pfu_predicate
    ;       pfu_function
    ;       pfu_unknown.

:- type pred_func_or_unknown_pf =< pred_func_or_unknown
    --->    pfu_predicate
    ;       pfu_function.

:- type pred_func_or_unknown_maybe_modes
    --->    pfumm_predicate(modes_or_arity)
    ;       pfumm_function(modes_or_arity)
    ;       pfumm_unknown(user_arity).

:- type modes_or_arity
    --->    moa_modes(list(mer_mode))
    ;       moa_arity(user_arity).

:- func pfu_to_maybe_pred_or_func(pred_func_or_unknown) = maybe(pred_or_func).
:- func maybe_pred_or_func_to_pfu(maybe(pred_or_func)) = pred_func_or_unknown.

:- pred pfumm_to_maybe_pf_arity_maybe_modes(
    pred_func_or_unknown_maybe_modes::in, maybe(pred_or_func)::out,
    user_arity::out, maybe(list(mer_mode))::out) is det.

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
:- func get_decl_pragma_context(item_decl_pragma_info) = prog_context.
:- func get_impl_pragma_context(item_impl_pragma_info) = prog_context.
:- func get_gen_pragma_context(item_generated_pragma_info) = prog_context.
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

:- import_module parse_tree.prog_util.

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

fill_kind_size_num_bits(FillKindSize) = NumBits :-
    (
        FillKindSize = fk_enum(NumBits)
    ;
        ( FillKindSize = fk_int8
        ; FillKindSize = fk_uint8
        ),
        NumBits = 8u
    ;
        ( FillKindSize = fk_int16
        ; FillKindSize = fk_uint16
        ),
        NumBits = 16u
    ;
        ( FillKindSize = fk_int32
        ; FillKindSize = fk_uint32
        ),
        NumBits = 32u
    ;
        FillKindSize = fk_char21,
        NumBits = 21u
    ).

%---------------------------------------------------------------------------%

pfu_to_maybe_pred_or_func(pfu_predicate) = yes(pf_predicate).
pfu_to_maybe_pred_or_func(pfu_function) = yes(pf_function).
pfu_to_maybe_pred_or_func(pfu_unknown) = no.

maybe_pred_or_func_to_pfu(yes(pf_predicate)) = pfu_predicate.
maybe_pred_or_func_to_pfu(yes(pf_function)) = pfu_function.
maybe_pred_or_func_to_pfu(no) = pfu_unknown.

pfumm_to_maybe_pf_arity_maybe_modes(PFUMM, MaybePredOrFunc, UserArity,
        MaybeModes) :-
    (
        (
            PFUMM = pfumm_predicate(ModesOrArity),
            PredOrFunc = pf_predicate
        ;
            PFUMM = pfumm_function(ModesOrArity),
            PredOrFunc = pf_function
        ),
        MaybePredOrFunc = yes(PredOrFunc),
        (
            ModesOrArity = moa_modes(Modes),
            list.length(Modes, NumModes),
            PredFormArity = pred_form_arity(NumModes),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            MaybeModes = yes(Modes)
        ;
            ModesOrArity = moa_arity(UserArity),
            MaybeModes = no
        )
    ;
        PFUMM = pfumm_unknown(UserArity),
        MaybePredOrFunc = no,
        MaybeModes = no
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

get_decl_pragma_context(DeclPragma) = Context :-
    (
        DeclPragma = decl_pragma_obsolete_pred(ObsPred),
        Context = ObsPred ^ obspred_context
    ;
        DeclPragma = decl_pragma_obsolete_proc(ObsProc),
        Context = ObsProc ^ obsproc_context
    ;
        DeclPragma = decl_pragma_format_call(FormatCall),
        Context = FormatCall ^ format_context
    ;
        DeclPragma = decl_pragma_type_spec_constr(TypeSpecConstr),
        Context = TypeSpecConstr ^ tsc_context
    ;
        DeclPragma = decl_pragma_type_spec(TypeSpec),
        Context = TypeSpec ^ tspec_context
    ;
        DeclPragma = decl_pragma_oisu(OISU),
        Context = OISU ^ oisu_context
    ;
        DeclPragma = decl_pragma_termination(Term),
        Context = Term ^ terminfo_context
    ;
        DeclPragma = decl_pragma_termination2(Term2),
        Context = Term2 ^ terminfo2_context
    ;
        DeclPragma = decl_pragma_struct_sharing(Sharing),
        Context = Sharing ^ sharing_context
    ;
        DeclPragma = decl_pragma_struct_reuse(Reuse),
        Context = Reuse ^ reuse_context
    ).

get_impl_pragma_context(ImplPragma) = Context :-
    (
        ImplPragma = impl_pragma_foreign_decl(ForeignDecl),
        Context = ForeignDecl ^ decl_context
    ;
        ImplPragma = impl_pragma_foreign_code(ForeignCode),
        Context = ForeignCode ^ code_context
    ;
        ImplPragma = impl_pragma_fproc_export(Export),
        Context = Export ^ exp_context
    ;
        ImplPragma = impl_pragma_external_proc(ExternalProc),
        Context = ExternalProc ^ external_context
    ;
        ImplPragma = impl_pragma_fact_table(FactTable),
        Context = FactTable ^ fact_table_context
    ;
        ImplPragma = impl_pragma_tabled(Tabled),
        Context = Tabled ^ tabled_context
    ;
        ImplPragma = impl_pragma_req_tail_rec(TailRec),
        Context = TailRec ^ rtr_context
    ;
        ImplPragma = impl_pragma_req_feature_set(FeatureSet),
        Context = FeatureSet ^ rfs_context
    ).

get_gen_pragma_context(GenPragma) = Context :-
    (
        GenPragma = gen_pragma_unused_args(UnusedArgs),
        Context = UnusedArgs ^ unused_context
    ;
        GenPragma = gen_pragma_exceptions(Excps),
        Context = Excps ^ exceptions_context
    ;
        GenPragma = gen_pragma_trailing(Trailing),
        Context = Trailing ^ trailing_context
    ;
        GenPragma = gen_pragma_mm_tabling(MMTabling),
        Context = MMTabling ^ mm_tabling_context
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
