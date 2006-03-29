%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1993-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: make_hlds.m.
% Main author: fjh.

% This module converts from the parse tree structure which is read in by
% prog_io.m, into the simplified high level data structure defined in
% hlds.m.  In the parse tree, the program is represented as a list of
% items; we insert each item into the appropriate symbol table, and report
% any duplicate definition errors.  We also transform clause bodies from
% (A,B,C) into conj([A,B,C]) form, convert all unifications into
% super-homogenous form, and introduce implicit quantification.
%
% XXX we should record each error using module_info_incr_errors.

% WISHLIST - we should handle explicit module quantification

:- module hlds__make_hlds.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.equiv_type.
:- import_module parse_tree.module_qual.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module term.

:- type make_hlds_qual_info.

    % parse_tree_to_hlds(ParseTree, MQInfo, EqvMap, HLDS, QualInfo,
    %   InvalidTypes, InvalidModes):
    %
    % Given MQInfo (returned by module_qual.m) and EqvMap (returned by
    % equiv_type.m), converts ParseTree to HLDS. Any errors found are
    % recorded in the HLDS num_errors field.
    % Returns InvalidTypes = yes if undefined types found.
    % Returns InvalidModes = yes if undefined or cyclic insts or modes
    % found. QualInfo is an abstract type that is then passed back to
    % produce_instance_method_clauses (see below).
    %
:- pred parse_tree_to_hlds(compilation_unit::in, mq_info::in, eqv_map::in,
    module_info::out, make_hlds_qual_info::out, bool::out, bool::out,
    io::di, io::uo) is det.

:- pred add_new_proc(inst_varset::in, arity::in, list(mer_mode)::in,
    maybe(list(mer_mode))::in, maybe(list(is_live))::in,
    maybe(determinism)::in, prog_context::in, is_address_taken::in,
    pred_info::in, pred_info::out, proc_id::out) is det.

    % add_special_pred_for_real(SpecialPredId, TVarSet, Type, TypeCtor,
    %   TypeBody, TypeContext, TypeStatus, !ModuleInfo).
    %
    % Add declarations and clauses for a special predicate.
    % This is used by unify_proc.m to add a unification predicate
    % for an imported type for which special predicates are being
    % generated only when a unification procedure is requested
    % during mode analysis.
    %
:- pred add_special_pred_for_real(special_pred_id::in, tvarset::in,
    mer_type::in, type_ctor::in, hlds_type_body::in, prog_context::in,
    import_status::in, module_info::in, module_info::out) is det.

    % add_special_pred_decl_for_real(SpecialPredId, TVarSet,
    %   Type, TypeCtor, TypeContext, TypeStatus, !ModuleInfo).
    %
    % Add declarations for a special predicate.
    % This is used by higher_order.m when specializing an in-in
    % unification for an imported type for which unification procedures
    % are generated lazily.
    %
:- pred add_special_pred_decl_for_real(special_pred_id::in,
    tvarset::in, mer_type::in, type_ctor::in, prog_context::in,
    import_status::in, module_info::in, module_info::out) is det.

    % Given the definition for a predicate or function from a
    % type class instance declaration, produce the clauses_info
    % for that definition.
    %
:- pred produce_instance_method_clauses(instance_proc_def::in,
    pred_or_func::in, arity::in, list(mer_type)::in, pred_markers::in,
    term__context::in, import_status::in, clauses_info::out,
    module_info::in, module_info::out,
    make_hlds_qual_info::in, make_hlds_qual_info::out, io::di, io::uo) is det.

    % Move the recompilation_info from the qual_info to the module_info
    % after make_hlds is finished with it and the qual_info is dead.
    %
:- pred set_module_recomp_info(make_hlds_qual_info::in,
    module_info::in, module_info::out) is det.

:- implementation.

:- include_module add_class.
:- include_module add_clause.
:- include_module add_mode.
:- include_module add_pragma.
:- include_module add_pred.
:- include_module add_solver.
:- include_module add_special_pred.
:- include_module add_type.
:- include_module field_access.
:- include_module make_hlds_error.
:- include_module make_hlds_passes.
:- include_module make_hlds_warn.
:- include_module qual_info.
:- include_module state_var.
:- include_module superhomogeneous.

:- import_module hlds.make_hlds.add_class.
:- import_module hlds.make_hlds.add_pred.
:- import_module hlds.make_hlds.add_special_pred.
:- import_module hlds.make_hlds.make_hlds_passes.
:- import_module hlds.make_hlds.qual_info.

:- type make_hlds_qual_info == hlds__make_hlds__qual_info__qual_info.

parse_tree_to_hlds(Module, MQInfo0, EqvMap, ModuleInfo,
        QualInfo, InvalidTypes, InvalidModes, !IO) :-
    do_parse_tree_to_hlds(Module, MQInfo0, EqvMap, ModuleInfo,
        QualInfo, InvalidTypes, InvalidModes, !IO).

add_new_proc(InstVarSet, Arity, ArgModes, MaybeDeclaredArgModes,
        MaybeArgLives, MaybeDet, Context, IsAddressTaken, PredInfo0, PredInfo,
        ModeId) :-
    do_add_new_proc(InstVarSet, Arity, ArgModes, MaybeDeclaredArgModes,
        MaybeArgLives, MaybeDet, Context, IsAddressTaken, PredInfo0, PredInfo,
        ModeId).

add_special_pred_for_real(SpecialPredId, TVarSet,
        Type0, TypeCtor, TypeBody, Context, Status0, !ModuleInfo) :-
    do_add_special_pred_for_real(SpecialPredId, TVarSet,
        Type0, TypeCtor, TypeBody, Context, Status0, !ModuleInfo).

add_special_pred_decl_for_real(SpecialPredId, TVarSet,
        Type, TypeCtor, Context, Status0, !ModuleInfo) :-
    do_add_special_pred_decl_for_real(SpecialPredId, TVarSet,
        Type, TypeCtor, Context, Status0, !ModuleInfo).

produce_instance_method_clauses(InstanceProcDefn,
        PredOrFunc, PredArity, ArgTypes, Markers, Context, Status,
        ClausesInfo, !ModuleInfo, !QualInfo, !IO) :-
    do_produce_instance_method_clauses(InstanceProcDefn, PredOrFunc,
        PredArity, ArgTypes, Markers, Context, Status, ClausesInfo,
        !ModuleInfo, !QualInfo, !IO).

set_module_recomp_info(QualInfo, !ModuleInfo) :-
    set_module_recompilation_info(QualInfo, !ModuleInfo).
