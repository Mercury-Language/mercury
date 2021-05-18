%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: make_hlds.m.
% Main author: fjh.
%
% This module converts from the parse tree structure which is read in by
% parse_module.m, into the high level data structure defined in hlds.m.
%
% In the parse tree, the program is represented as a structure containing
% several lists of items; we insert each item into the appropriate table,
% and report any duplicate definition errors. We also transform clause bodies
% from (A,B,C) into conj([A,B,C]) form, convert all unifications into
% superhomogenous form, and introduce implicit quantification.
%
% WISHLIST - we should handle explicit module quantification.
%
%-----------------------------------------------------------------------------%

:- module hlds.make_hlds.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.error_util.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_used_modules.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

:- type make_hlds_qual_info.

:- type found_invalid_type
    --->    did_not_find_invalid_type
    ;       found_invalid_type.

:- type found_invalid_inst_or_mode
    --->    did_not_find_invalid_inst_or_mode
    ;       found_invalid_inst_or_mode.

:- type ims_list(T) == list(ims_sub_list(T)).
:- type ims_sub_list(T)
    --->    ims_sub_list(item_mercury_status, list(T)).

:- type sec_list(T) == list(sec_sub_list(T)).
:- type sec_sub_list(T)
    --->    sec_sub_list(sec_info, list(T)).
:- type sec_info
    --->    sec_info(
                % The status of the items defined in the section.
                item_mercury_status,

                % Whether you need to fully module qualify the items
                % that are defined in this section, directly or indirectly.
                %
                % There is one item that defines something directly
                % to which this is relevant: predicate items.
                % For them, this controls whether a reference to the predicate
                % (or function) has to be fully qualified or not.
                %
                % For typeclass items, this controls whether references
                % to the method predicates and/or functions has to be
                % fully qualified or not.
                %
                % For mutable items, this controls whether references
                % to the access predicates have to be fully qualified or not.
                %
                % For type defn items for solver types, this controls whether
                % references to the solver type's auxiliary predicates
                % (for representation changes) have to be fully qualified
                % or not.
                %
                % For type defn items for du types, this controls whether
                % references to the type's function symbols have to be
                % fully qualified or not.
                %
                % As it happens, ALL these references can occur only in
                % the implementation section of a Mercury module.
                % This means that in cases where an imported module's
                % qualification needs are different in the interface and in the
                % implementation (due to it being imported via `use_module'
                % in the interface and `import_module' in the implementation),
                % this field should be set to the value appropriate for the
                % implementation section.
                need_qualifier
            ).

%-----------------------------------------------------------------------------%

    % parse_tree_to_hlds(AugCompUnit, Globals, DumpBaseFileName, MQInfo,
    %   TypeEqvMap, UsedModules, QualInfo, InvalidTypes, InvalidModes,
    %   HLDS, Specs):
    %
    % Given MQInfo (returned by module_qual.m) and TypeEqvMap and UsedModules
    % (both returned by equiv_type.m), converts AugCompUnit to HLDS.
    % It returns messages for any errors it finds in Specs.
    % Returns InvalidTypes = yes if it finds any undefined types.
    % Returns InvalidModes = yes if it finds any undefined or cyclic
    % insts or modes.
    % QualInfo is an abstract type that check_typeclass.m will later pass
    % to produce_instance_method_clauses (see below).
    %
:- pred parse_tree_to_hlds(aug_compilation_unit::in, globals::in, string::in,
    mq_info::in, type_eqv_map::in, used_modules::in, make_hlds_qual_info::out,
    found_invalid_type::out, found_invalid_inst_or_mode::out,
    module_info::out, list(error_spec)::out) is det.

    % Given the definition for a predicate or function from a
    % type class instance declaration, produce the clauses_info
    % for that definition.
    %
:- pred produce_instance_method_clauses(instance_proc_def::in,
    pred_or_func::in, arity::in, list(mer_type)::in,
    pred_markers::in, term.context::in, instance_status::in, clauses_info::out,
    tvarset::in, tvarset::out, module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

    % Move the recompilation_info from the qual_info to the module_info
    % after make_hlds is finished with it and the qual_info is dead.
    %
:- pred set_module_recomp_info(make_hlds_qual_info::in,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module add_class.
:- include_module add_clause.
:- include_module add_foreign_proc.
:- include_module add_mode.
:- include_module add_mutable_aux_preds.
:- include_module add_pragma.
:- include_module add_solver.
:- include_module add_type.
:- include_module field_access.
:- include_module goal_expr_to_goal.
:- include_module make_hlds_passes.
:- include_module make_hlds_warn.
:- include_module qual_info.
:- include_module state_var.
:- include_module superhomogeneous.

:- import_module hlds.make_hlds.add_class.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.qual_info.

%-----------------------------------------------------------------------------%

:- type make_hlds_qual_info == hlds.make_hlds.qual_info.qual_info.

%-----------------------------------------------------------------------------%

parse_tree_to_hlds(AugCompilationUnit, Globals, DumpBaseFileName,
        MQInfo0, TypeEqvMap, UsedModules, QualInfo,
        FoundInvalidType, FoundInvalidMode, ModuleInfo, Specs) :-
    do_parse_tree_to_hlds(AugCompilationUnit, Globals, DumpBaseFileName,
        MQInfo0, TypeEqvMap, UsedModules, QualInfo,
        FoundInvalidType, FoundInvalidMode, ModuleInfo, Specs).

produce_instance_method_clauses(InstanceProcDefn,
        PredOrFunc, PredArity, ArgTypes, Markers, Context, Status,
        ClausesInfo, !TVarSet, !ModuleInfo, !QualInfo, !Specs) :-
    do_produce_instance_method_clauses(InstanceProcDefn, PredOrFunc,
        PredArity, ArgTypes, Markers, Context, Status, ClausesInfo, !TVarSet,
        !ModuleInfo, !QualInfo, !Specs).

set_module_recomp_info(QualInfo, !ModuleInfo) :-
    set_module_recompilation_info(QualInfo, !ModuleInfo).

%-----------------------------------------------------------------------------%
:- end_module hlds.make_hlds.
%-----------------------------------------------------------------------------%
