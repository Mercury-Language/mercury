%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_pred.m.
% Main authors: fjh, conway.
%
% This module defines the pred_info type and its most direct operations.
% We use a pred_info to represent each predicate and function in the HLDS.
% We store the information specific to each *mode* of a predicate or function
% in proc_infos, which are defined in hlds_proc.m.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_pred.
:- interface.

:- import_module hlds.hlds_class.
:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_markers.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_proc.
:- import_module hlds.hlds_promise.
:- import_module hlds.hlds_rtti.
:- import_module hlds.inst_graph.
:- import_module hlds.instmap.
:- import_module hlds.pred_info_types.
:- import_module hlds.pred_name.
:- import_module hlds.pred_proc_id.
:- import_module hlds.pred_table.
:- import_module hlds.proc_info_types.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

:- type pred_info.

   % Various predicates for accessing the information stored in the
    % pred_id and pred_info data structures.
    %
:- type external_type_params == list(tvar).

:- type proc_table == map(proc_id, proc_info).

%---------------------------------------------------------------------------%
%
% Creating pred_infos.
%

    % pred_info_init(PredOrFunc, PredModuleName, PredName, Arity, Context,
    %   Origin, Status, CurUserDecl, GoalType, Markers,
    %   ArgTypes, TypeVarSet, ExistQVars, ClassContext, ClassProofs,
    %   ClassConstraintMap, ClausesInfo, VarNameRemap, PredInfo):
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults.
    %
:- pred pred_info_init(pred_or_func::in, module_name::in, string::in,
    pred_form_arity::in, prog_context::in, pred_origin::in,
    pred_status::in, maybe(cur_user_decl_info)::in, goal_type::in,
    pred_markers::in, list(mer_type)::in, tvarset::in, existq_tvars::in,
    univ_exist_constraints::in, constraint_proof_map::in, constraint_map::in,
    clauses_info::in, map(prog_var, string)::in, pred_info::out) is det.

    % pred_info_create(ModuleInfo, PredOrFunc, ModuleName, PredName,
    %   Context, Origin, Status, Markers, ArgTypes, TypeVarSet, ExistQVars,
    %   ClassContext, Assertions, VarNameRemap, ProcInfo, ProcId, PredInfo)
    %
    % Return a pred_info whose fields are filled in from the information
    % (direct and indirect) in the arguments, and from defaults. The given
    % proc_info becomes the only procedure of the predicate (currently)
    % and its proc_id is returned as the second last argument.
    %
:- pred pred_info_create(pred_or_func::in, module_name::in, string::in,
    prog_context::in, pred_origin::in, pred_status::in, pred_markers::in,
    list(mer_type)::in, tvarset::in, existq_tvars::in,
    univ_exist_constraints::in, set(assert_id)::in,
    map(prog_var, string)::in, goal_type::in, proc_info::in,
    proc_id::out, pred_info::out) is det.

    % define_new_pred(SymName, Origin, TVarSet, InstVarSet, VarTable,
    %   RttiVarMaps, ClassContext, InstMap0, VarNameRemap,
    %   Markers, IsAddressTaken, HasParallelConj, PredProcId,
    %   ArgVars, ExtraTiTcis, Goal0, CallGoal, !ModuleInfo):
    %
    % Create a new predicate for the given goal, returning a goal to
    % call the created predicate. ExtraArgs is the list of extra
    % type_infos and typeclass_infos required by typeinfo liveness
    % which were added to the front of the argument list.
    %
:- pred define_new_pred(sym_name::in, pred_origin::in,
    tvarset::in, inst_varset::in, var_table::in,
    rtti_varmaps::in, univ_exist_constraints::in, instmap::in,
    map(prog_var, string)::in, pred_markers::in,
    is_address_taken::in, has_parallel_conj::in, pred_proc_id::out,
    list(prog_var)::in, list(prog_var)::out, hlds_goal::in, hlds_goal::out,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%
% Cloning pred_infos.
%
%
% pred_prepare_to_clone returns all the fields of an existing pred_info,
% while pred_create constructs a new pred_info putting the supplied values
% to each field.
%
% These predicates exist because we want keep the definition of the pred_info
% type private (to make future changes easier), but we also want to make it
% possible to create slightly modified copies of existing predicates
% with the least amount of programming work. We also want to require
% (a) programmers writing such cloning code to consider what effect
% the modification may have on *all* fields of the pred_info, and
% (b) programmers who add new fields to the pred_info to update
% all the places in the compiler that do such cloning.
%

:- pred pred_prepare_to_clone(pred_info::in,
    module_name::out, pred_or_func::out, string::out, pred_form_arity::out,
    pred_origin::out, pred_status::out, pred_markers::out, list(mer_type)::out,
    tvarset::out, tvarset::out, existq_tvars::out, int::out,
    univ_exist_constraints::out, clauses_info::out,
    proc_table::out, prog_context::out,
    maybe(cur_user_decl_info)::out, goal_type::out, tvar_kind_map::out,
    tsubst::out, external_type_params::out, constraint_proof_map::out,
    constraint_map::out, list(prog_constraint)::out, inst_graph_info::out,
    list(arg_modes_map)::out, map(prog_var, string)::out, set(assert_id)::out,
    maybe(list(sym_name_arity))::out, maybe(format_call_info)::out,
    list(mer_type)::out) is det.

:- pred pred_create(module_name::in,
    pred_or_func::in, string::in, pred_form_arity::in, pred_origin::in,
    pred_status::in, pred_markers::in, list(mer_type)::in, tvarset::in,
    tvarset::in, existq_tvars::in, int::in, univ_exist_constraints::in,
    clauses_info::in, proc_table::in, prog_context::in,
    maybe(cur_user_decl_info)::in, goal_type::in, tvar_kind_map::in,
    tsubst::in, external_type_params::in, constraint_proof_map::in,
    constraint_map::in, list(prog_constraint)::in, inst_graph_info::in,
    list(arg_modes_map)::in, map(prog_var, string)::in, set(assert_id)::in,
    maybe(list(sym_name_arity))::in, maybe(format_call_info)::in,
    list(mer_type)::in, pred_info::out) is det.

%---------------------------------------------------------------------------%
%
% Updates of pred_infos.
%

:- pred add_new_proc(module_info::in, prog_context::in, item_seq_num::in,
    inst_varset::in, list(mer_mode)::in,
    maybe(list(mer_mode))::in, maybe(list(is_live))::in,
    detism_decl::in, maybe(determinism)::in,
    is_address_taken::in, has_parallel_conj::in,
    pred_info::in, pred_info::out, proc_id::out) is det.

:- pred pred_info_update_goal_type(np_goal_type::in,
    pred_info::in, pred_info::out) is det.

    % Set the pred_status of the predicate to `imported'.
    % This is used for `:- pragma external_{pred/func}(foo/2).'.
    %
:- pred pred_info_mark_as_external(pred_info::in, pred_info::out) is det.

%---------------------------------------------------------------------------%
%
% Predicate identification and its components.
%

:- func pred_info_module(pred_info) = module_name.
:- func pred_info_name(pred_info) = string.

    % N-ary functions are converted into N+1-ary predicates.
    % (Clauses are converted in make_hlds, but calls to functions
    % cannot be converted until after type-checking, once we have
    % resolved overloading. So we do that during mode analysis.)
    % The `is_pred_or_func' field of the pred_info records whether
    % a pred_info is really for a predicate or whether it is for
    % what was originally a function.
    %
:- func pred_info_is_pred_or_func(pred_info) = pred_or_func.

    % Pred_info_orig_arity returns the arity of the predicate
    % *not* counting inserted type_info arguments for polymorphic preds.
    %
:- func pred_info_pred_form_arity(pred_info) = pred_form_arity.
:- func pred_info_user_arity(pred_info) = user_arity.

:- pred pred_info_get_sym_name(pred_info::in, sym_name::out) is det.

:- pred pred_info_get_pf_sym_name_pred_form_arity(pred_info::in,
    pf_sym_name_pred_form_arity::out) is det.
:- pred pred_info_get_pf_sym_name_user_arity(pred_info::in,
    pf_sym_name_user_arity::out) is det.

%---------------------------------------------------------------------------%
%
% Procedure management.
%

    % Return a list of the proc_ids for all the modes of this predicate,
    %
:- func pred_info_all_proc_ids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes of this predicate
    % that are exported.
    %
:- func pred_info_all_exported_proc_ids(pred_info) = list(proc_id).

    % Return a list of the proc_ids for all the modes of this predicate
    % for which this module will generate code. This includes both
    %
    % - procedures defined in this module, and
    % - procedures opt-imported into this module.
    %
:- func pred_info_will_codegen_proc_ids(pred_info) = list(proc_id).

:- pred next_proc_id(proc_table::in, proc_id::out) is det.

:- pred pred_info_proc_info(pred_info::in, proc_id::in, proc_info::out) is det.

:- pred pred_info_set_proc_info(proc_id::in, proc_info::in,
    pred_info::in, pred_info::out) is det.

    % Remove a procedure from the pred_info.
    %
:- pred pred_info_remove_proc_id(proc_id::in, pred_info::in, pred_info::out)
    is det.

%---------------------------------------------------------------------------%
%
% Argument list management.
%

:- pred pred_info_get_arg_types(pred_info::in, tvarset::out, existq_tvars::out,
    list(mer_type)::out) is det.

:- pred pred_info_set_arg_types(tvarset::in, existq_tvars::in,
    list(mer_type)::in, pred_info::in, pred_info::out) is det.

:- pred pred_info_get_univ_quant_tvars(pred_info::in, list(tvar)::out)
    is det.

%---------------------------------------------------------------------------%
%
% Status tests.
%

:- pred pred_info_is_imported(pred_info::in) is semidet.

:- pred pred_info_is_imported_not_external(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_imported(pred_info::in) is semidet.

    % pred_info_is_exported does *not* include predicates which are
    % exported_to_submodules or pseudo_exported.
    %
:- pred pred_info_is_exported(pred_info::in) is semidet.

:- pred pred_info_is_opt_exported(pred_info::in) is semidet.

:- pred pred_info_is_exported_to_submodules(pred_info::in) is semidet.

:- pred pred_info_is_pseudo_exported(pred_info::in) is semidet.

    % procedure_is_exported includes all modes of exported or
    % exported_to_submodules predicates, plus the in-in mode
    % for pseudo_exported unification predicates.
    %
:- pred procedure_is_exported(module_info::in, pred_info::in, proc_id::in)
    is semidet.

%---------------------------------------------------------------------------%
%
% Non-status tests.
%

    % Do we have a clause goal type?
    % (this means either "clauses" or "clauses_and_pragmas")
    %
:- pred pred_info_defn_has_clause(pred_info::in) is semidet.

    % Do we have a pragma goal type?
    % (this means either "pragmas" or "clauses_and_pragmas")
    %
:- pred pred_info_defn_has_foreign_proc(pred_info::in) is semidet.

:- pred pred_info_infer_modes(pred_info::in) is semidet.

:- pred pred_info_get_purity(pred_info::in, purity::out) is det.

    % If the predicate has a purity promise, return it wrapped inside a `yes'.
    % Otherwise, return `no'.
    %
:- pred pred_info_get_promised_purity(pred_info::in, maybe(purity)::out)
    is det.

    % Succeeds if there was a `:- pragma inline(...)' declaration
    % for this predicate. Note that the compiler may decide
    % to inline a predicate even if there was no pragma inline(...)
    % declaration for that predicate.
    %
:- pred pred_info_requested_inlining(pred_info::in) is semidet.

    % Succeeds if there was a `:- pragma no_inline(...)' declaration
    % for this predicate.
    %
:- pred pred_info_requested_no_inlining(pred_info::in) is semidet.

    % Are calls from a predicate with the pred_markers always fully
    % qualified? Basically, this function tests for the presence or absence
    % of marker_calls_are_fully_qualified.
    %
:- func calls_are_fully_qualified(pred_markers) = is_fully_qualified.

%---------------------------------------------------------------------------%

    % Return true if the interface of the given procedure must include
    % typeinfos for all the type variables in the types of the arguments.
    %
:- pred proc_interface_should_use_typeinfo_liveness(pred_info::in, proc_id::in,
    globals::in, bool::out) is det.

    % Return true if the body of a procedure from the given predicate
    % must keep a typeinfo variable alive during the lifetime of all
    % variables whose type includes the corresponding type variable.
    % Note that body typeinfo liveness implies interface typeinfo liveness,
    % but not vice versa.
    %
:- pred body_should_use_typeinfo_liveness(pred_info::in, globals::in,
    bool::out) is det.

%---------------------------------------------------------------------------%
%
% Getter and setter predicates.
%

:- pred pred_info_get_module_name(pred_info::in, module_name::out) is det.
:- pred pred_info_get_is_pred_or_func(pred_info::in, pred_or_func::out) is det.
:- pred pred_info_get_name(pred_info::in, string::out) is det.
:- pred pred_info_get_orig_arity(pred_info::in, pred_form_arity::out) is det.
:- pred pred_info_get_origin(pred_info::in, pred_origin::out) is det.
:- pred pred_info_get_status(pred_info::in, pred_status::out) is det.
:- pred pred_info_get_markers(pred_info::in, pred_markers::out) is det.
:- pred pred_info_get_arg_types(pred_info::in, list(mer_type)::out) is det.
:- pred pred_info_get_typevarset(pred_info::in, tvarset::out) is det.
:- pred pred_info_get_exist_quant_tvars(pred_info::in,
    existq_tvars::out) is det.
:- pred pred_info_get_class_context(pred_info::in,
    univ_exist_constraints::out) is det.
:- pred pred_info_get_clauses_info(pred_info::in, clauses_info::out) is det.
:- pred pred_info_get_proc_table(pred_info::in, proc_table::out) is det.

:- pred pred_info_get_context(pred_info::in, prog_context::out) is det.
:- pred pred_info_get_cur_user_decl_info(pred_info::in,
    maybe(cur_user_decl_info)::out) is det.
:- pred pred_info_get_goal_type(pred_info::in, goal_type::out) is det.
:- pred pred_info_get_tvar_kind_map(pred_info::in, tvar_kind_map::out) is det.
:- pred pred_info_get_existq_tvar_binding(pred_info::in, tsubst::out) is det.
:- pred pred_info_get_polymorphism_added_args(pred_info::in,
    int::out) is det.
:- pred pred_info_get_external_type_params(pred_info::in,
    external_type_params::out) is det.
:- pred pred_info_get_constraint_proof_map(pred_info::in,
    constraint_proof_map::out) is det.
:- pred pred_info_get_constraint_map(pred_info::in,
    constraint_map::out) is det.
:- pred pred_info_get_unproven_body_constraints(pred_info::in,
    list(prog_constraint)::out) is det.
:- pred pred_info_get_inst_graph_info(pred_info::in,
    inst_graph_info::out) is det.
:- pred pred_info_get_arg_modes_maps(pred_info::in,
    list(arg_modes_map)::out) is det.
:- pred pred_info_get_var_name_remap(pred_info::in,
    map(prog_var, string)::out) is det.
:- pred pred_info_get_assertions(pred_info::in, set(assert_id)::out) is det.
:- pred pred_info_get_obsolete_in_favour_of(pred_info::in,
    maybe(list(sym_name_arity))::out) is det.
:- pred pred_info_get_format_call_info(pred_info::in,
    maybe(format_call_info)::out) is det.
:- pred pred_info_get_instance_method_arg_types(pred_info::in,
    list(mer_type)::out) is det.

    % Setting any part of the sym_name of a pred_info after its creation
    % won't remove its name from the indexes under its old name or insert it
    % into the indexes under its new name. If is therefore safe to do this
    % only after *all* the passes that look up predicates by name.
    %
:- pred pred_info_set_module_name(module_name::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_is_pred_or_func(pred_or_func::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_name(string::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_orig_arity(pred_form_arity::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_origin(pred_origin::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_status(pred_status::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_goal_type(goal_type::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_markers(pred_markers::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_typevarset(tvarset::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_class_context(univ_exist_constraints::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_clauses_info(clauses_info::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_proc_table(proc_table::in,
    pred_info::in, pred_info::out) is det.

:- pred pred_info_set_tvar_kind_map(tvar_kind_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_existq_tvar_binding(tsubst::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_polymorphism_added_args(int::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_external_type_params(external_type_params::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_constraint_proof_map(constraint_proof_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_constraint_map(constraint_map::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_unproven_body_constraints(list(prog_constraint)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_inst_graph_info(inst_graph_info::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_arg_modes_maps(list(arg_modes_map)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_var_name_remap(map(prog_var, string)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_assertions(set(assert_id)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_obsolete_in_favour_of(
    maybe(list(sym_name_arity))::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_format_call_info(maybe(format_call_info)::in,
    pred_info::in, pred_info::out) is det.
:- pred pred_info_set_instance_method_arg_types(list(mer_type)::in,
    pred_info::in, pred_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.goal_vars.
:- import_module hlds.hlds_args.
:- import_module hlds.hlds_data.
:- import_module libs.options.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_data_rare.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.vartypes.
:- import_module transform_hlds.

:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module unit.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Creating pred_infos.
%

pred_info_init(PredOrFunc, PredModuleName, PredName, PredFormArity, Context,
        Origin, Status, CurUserDecl, GoalType, Markers,
        ArgTypes, TypeVarSet, ExistQVars, ClassContext, ClassProofs,
        ClassConstraintMap, ClausesInfo, VarNameRemap, PredInfo) :-
    % argument Context
    % argument GoalType
    map.init(Kinds),
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(ExistQVarBindings),
    PolymorphismAddedArgs = 0,
    type_vars_in_types(ArgTypes, TVars),
    list.delete_elems(TVars, ExistQVars, HeadTypeParams),
    % argument ClassProofs
    % argument ClassConstraintMap
    UnprovenBodyConstraints = [],
    InstGraphInfo = inst_graph_info_init,
    ArgModesMaps = [],
    % argument VarNameRemap
    set.init(Assertions),
    ObsoleteInFavourOf = maybe.no,
    FormatCall = maybe.no,
    InstanceMethodArgTypes = [],
    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, PolymorphismAddedArgs, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, ObsoleteInFavourOf, FormatCall,
        InstanceMethodArgTypes),

    % argument PredModuleName
    % argument PredName
    % NOTE We cannot assert anything about the relationship
    % between PredFormArity and the number of arguments in ArgTypes, because
    %
    % - ArgTypes may be have more arguments than PredFormArity,
    %   due to the type_info/typeclass_info arguments added by the
    %   polymorphism pass, and
    %
    % - ArgTypes have have fewer arguments than PredFormArity,
    %   because some arguments may have been removed by the unused_args pass.
    %
    % XXX ARGVEC Eventually, when we start using arg vectors, the arguments
    % added by the polymorphism pass would be counted separately.
    %
    % XXX ARITY The unused_args pass *should* decrement PredFormArity
    % by the number of arguments it eliminates, but at the moment, it does not.
    %
    % argument PredOrFunc
    % argument Origin
    % argument Status
    % argument Markers
    % argument ArgTypes
    % argument TypeVarSet
    % argument ExistQVars
    % argument ClassContext
    % argument ClausesInfo
    map.init(ProcTable),
    PredInfo = pred_info(PredModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, TypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo).

pred_info_create(PredOrFunc, PredModuleName, PredName,
        Context, Origin, Status, Markers, ArgTypes, TypeVarSet,
        ExistQVars, ClassContext, Assertions, VarNameRemap, GoalType,
        ProcInfo, ProcId, PredInfo) :-
    % argument Context
    CurUserDecl = maybe.no,
    % argument GoalType
    map.init(Kinds),
    % XXX kind inference:
    % we assume all tvars have kind `star'.
    map.init(ExistQVarBindings),
    PolymorphismAddedArgs = 0,
    type_vars_in_types(ArgTypes, TVars),
    list.delete_elems(TVars, ExistQVars, HeadTypeParams),
    map.init(ClassProofs),
    map.init(ClassConstraintMap),
    UnprovenBodyConstraints = [],
    InstGraphInfo = inst_graph_info_init,
    ArgModesMaps = [],
    % argument VarNameRemap
    % argument Assertions
    ObsoleteInFavourOf = maybe.no,
    FormatCall = maybe.no,
    InstanceMethodArgTypes = [],

    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, PolymorphismAddedArgs, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, ObsoleteInFavourOf, FormatCall,
        InstanceMethodArgTypes),

    % The VarSet and ExplicitVarTypes fields are not needed after typechecking.
    varset.init(VarSet),
    init_vartypes(ExplicitVarTypes),
    proc_info_get_var_table(ProcInfo, VarTable),
    map.init(TVarNameMap),
    proc_info_get_headvars(ProcInfo, HeadVars),
    HeadVarVec = proc_arg_vector_init(PredOrFunc, HeadVars),
    % The empty list of clauses is a little white lie.
    ClausesRep = init_clauses_rep,
    ItemNumbers = init_clause_item_numbers_user,
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    ClausesInfo = clauses_info(VarSet, ExplicitVarTypes, VarTable, RttiVarMaps,
        TVarNameMap, HeadVarVec, ClausesRep, ItemNumbers,
        no_foreign_lang_clauses, no_clause_syntax_errors),

    % argument PredModuleName
    % argument PredName
    list.length(ArgTypes, NumArgs),
    PredFormArity = pred_form_arity(NumArgs),
    % argument PredOrFunc
    % argument Origin
    % argument Status
    % argument Markers
    % argument ArgTypes
    % argument TypeVarSet
    % argument ExistQVars
    % argument ClassContext
    map.init(ProcTable0),
    next_proc_id(ProcTable0, ProcId),
    map.det_insert(ProcId, ProcInfo, ProcTable0, ProcTable),
    PredInfo = pred_info(PredModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, TypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo).

define_new_pred(PredSymName, Origin, TVarSet, InstVarSet,
        VarTable0, RttiVarMaps, ClassContext, InstMap0, VarNameRemap,
        Markers, IsAddressTaken, HasParallelConj, PredProcId,
        ArgVars0, ExtraTiTcis, Goal0, CallGoal, !ModuleInfo) :-
    Goal0 = hlds_goal(_GoalExpr, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap),

    % XXX The set of existentially quantified type variables
    % here might not be correct.
    ExistQVars = [],

    % If interface typeinfo liveness is set, all type_infos for the
    % arguments need to be passed in, not just the ones that are used.
    % Similarly if the address of a procedure of this predicate is taken,
    % so that we can copy the closure.
    module_info_get_globals(!.ModuleInfo, Globals),
    PredStatus = pred_status(status_local),
    non_special_interface_should_use_typeinfo_liveness(PredStatus,
        IsAddressTaken, Globals, TypeInfoLiveness),
    (
        TypeInfoLiveness = yes,
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        goal_util.extra_nonlocal_typeinfos_typeclass_infos(RttiVarMaps,
            VarTable0, ExistQVars, NonLocals, ExtraTiTcis0),
        set_of_var.delete_list(ArgVars0, ExtraTiTcis0, ExtraTiTcis1),
        set_of_var.to_sorted_list(ExtraTiTcis1, ExtraTiTcis),
        ArgVars = ExtraTiTcis ++ ArgVars0
    ;
        TypeInfoLiveness = no,
        ArgVars = ArgVars0,
        ExtraTiTcis = []
    ),

    Context = goal_info_get_context(GoalInfo),
    ItemNumber = item_no_seq_num,
    Detism = goal_info_get_determinism(GoalInfo),
    compute_arg_types_modes(VarTable0, InstMap0, InstMap,
        ArgVars, ArgTypes, ArgModes),

    (
        PredSymName = qualified(PredModuleName, PredName)
    ;
        PredSymName = unqualified(PredName),
        module_info_get_name(!.ModuleInfo, ModuleName),
        PredModuleName = ModuleName
    ),

    % Remove unneeded variables from the var_table.
    vars_in_goal(Goal0, GoalVars0),
    set_of_var.insert_list(ArgVars, GoalVars0, GoalVars),
    GoalVarsSet = set_of_var.bitset_to_set(GoalVars),
    var_table_select(GoalVarsSet, VarTable0, VarTable),

    % Approximate the termination information for the new procedure.
    ( if goal_cannot_loop_term_info(!.ModuleInfo, Goal0) then
        TermInfo = yes(cannot_loop(unit))
    else
        TermInfo = no
    ),

    MaybeDeclaredDetism = no,
    proc_info_create_with_declared_detism(Context, ItemNumber,
        VarTable, ArgVars, InstVarSet, ArgModes,
        detism_decl_none, MaybeDeclaredDetism, Detism, Goal0,
        RttiVarMaps, IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo0),
    proc_info_set_maybe_termination_info(TermInfo, ProcInfo0, ProcInfo),

    set.init(Assertions),
    GoalType = goal_not_for_promise(np_goal_type_none),
    pred_info_create(pf_predicate, PredModuleName, PredName,
        Context, Origin, PredStatus, Markers, ArgTypes, TVarSet, ExistQVars,
        ClassContext, Assertions, VarNameRemap, GoalType, ProcInfo,
        ProcId, PredInfo),

    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(PredInfo, PredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo),

    CallGoalExpr =
        plain_call(PredId, ProcId, ArgVars, not_builtin, no, PredSymName),
    CallGoal = hlds_goal(CallGoalExpr, GoalInfo),
    PredProcId = proc(PredId, ProcId).

:- pred compute_arg_types_modes(var_table::in, instmap::in, instmap::in,
    list(prog_var)::in, list(mer_type)::out, list(mer_mode)::out) is det.

compute_arg_types_modes(_, _, _, [], [], []).
compute_arg_types_modes(VarTable, InstMapInit, InstMapFinal,
        [Var | Vars], [Type | Types], [Mode | Modes]) :-
    lookup_var_type(VarTable, Var, Type),
    instmap_lookup_var(InstMapInit, Var, InstInit),
    instmap_lookup_var(InstMapFinal, Var, InstFinal),
    Mode = from_to_mode(InstInit, InstFinal),
    compute_arg_types_modes(VarTable, InstMapInit, InstMapFinal,
        Vars, Types, Modes).

%---------------------------------------------------------------------------%
%
% Cloning pred_infos.
%

pred_prepare_to_clone(PredInfo, ModuleName, PredOrFunc, PredName,
        PredFormArity, Origin, Status, Markers, ArgTypes,
        DeclTypeVarSet, TypeVarSet, ExistQVars, PolymorphismAddedArgs,
        ClassContext, ClausesInfo, ProcTable, Context,
        CurUserDecl, GoalType, Kinds, ExistQVarBindings, HeadTypeParams,
        ClassProofs, ClassConstraintMap, UnprovenBodyConstraints,
        InstGraphInfo, ArgModesMaps, VarNameRemap, Assertions,
        ObsoleteInFavourOf, FormatCall, InstanceMethodArgTypes) :-
    PredInfo = pred_info(ModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, DeclTypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo),
    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, PolymorphismAddedArgs, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, ObsoleteInFavourOf, FormatCall,
        InstanceMethodArgTypes).

pred_create(ModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, DeclTypeVarSet, TypeVarSet,
        ExistQVars, PolymorphismAddedArgs,
        ClassContext, ClausesInfo, ProcTable, Context,
        CurUserDecl, GoalType, Kinds, ExistQVarBindings, HeadTypeParams,
        ClassProofs, ClassConstraintMap, UnprovenBodyConstraints,
        InstGraphInfo, ArgModesMaps, VarNameRemap, Assertions,
        ObsoleteInFavourOf, FormatCall, InstanceMethodArgTypes, PredInfo) :-
    PredSubInfo = pred_sub_info(Context, CurUserDecl, GoalType,
        Kinds, ExistQVarBindings, PolymorphismAddedArgs, HeadTypeParams,
        ClassProofs, ClassConstraintMap,
        UnprovenBodyConstraints, InstGraphInfo, ArgModesMaps,
        VarNameRemap, Assertions, ObsoleteInFavourOf, FormatCall,
        InstanceMethodArgTypes),
    PredInfo = pred_info(ModuleName, PredOrFunc, PredName, PredFormArity,
        Origin, Status, Markers, ArgTypes, DeclTypeVarSet, TypeVarSet,
        ExistQVars, ClassContext, ClausesInfo, ProcTable, PredSubInfo).

%---------------------------------------------------------------------------%
%
% Updates of pred_infos.
%

add_new_proc(ModuleInfo, Context, SeqNum, InstVarSet, ArgModes,
        MaybeDeclaredArgModes, MaybeArgLives, DetismDecl, MaybeDetism,
        IsAddressTaken, HasParallelConj, !PredInfo, ProcId) :-
    pred_info_get_arg_types(!.PredInfo, ArgTypes),
    pred_info_get_var_name_remap(!.PredInfo, VarNameRemap),
    proc_info_init(ModuleInfo, Context, SeqNum, ArgTypes,
        InstVarSet, MaybeDeclaredArgModes, ArgModes, MaybeArgLives,
        DetismDecl, MaybeDetism, IsAddressTaken, HasParallelConj,
        VarNameRemap, ProcInfo),
    pred_info_get_proc_table(!.PredInfo, ProcTable0),
    next_proc_id(ProcTable0, ProcId),
    map.det_insert(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, !PredInfo).

pred_info_update_goal_type(NPGoalType1, !PredInfo) :-
    pred_info_get_goal_type(!.PredInfo, GoalType0),
    (
        GoalType0 = goal_not_for_promise(NPGoalType0),
        (
            NPGoalType0 = np_goal_type_none,
            NPGoalType = NPGoalType1
        ;
            NPGoalType0 = np_goal_type_clause,
            ( if goal_type_has_foreign_proc(NPGoalType1) then
                NPGoalType = np_goal_type_clause_and_foreign
            else
                NPGoalType = np_goal_type_clause
            )
        ;
            NPGoalType0 = np_goal_type_foreign,
            ( if goal_type_has_clause(NPGoalType1) then
                NPGoalType = np_goal_type_clause_and_foreign
            else
                NPGoalType = np_goal_type_foreign
            )
        ;
            NPGoalType0 = np_goal_type_clause_and_foreign,
            NPGoalType = NPGoalType0
        ),
        GoalType = goal_not_for_promise(NPGoalType),
        ( if GoalType = GoalType0 then
            % Avoid unnecessary memory allocation.
            true
        else
            pred_info_set_goal_type(GoalType, !PredInfo)
        )
    ;
        GoalType0 = goal_for_promise(_),
        unexpected($pred, "promise")
    ).

pred_info_mark_as_external(!PredInfo) :-
    pred_info_get_status(!.PredInfo, PredStatus0),
    PredStatus0 = pred_status(OldImportStatus0),
    PredStatus = pred_status(status_external(OldImportStatus0)),
    pred_info_set_status(PredStatus, !PredInfo).

%---------------------------------------------------------------------------%
%
% Predicate identification and its components.
%

pred_info_module(PI) = X :-
    pred_info_get_module_name(PI, X).

pred_info_name(PI) = X :-
    pred_info_get_name(PI, X).

pred_info_is_pred_or_func(PI) = X :-
    pred_info_get_is_pred_or_func(PI, X).

pred_info_pred_form_arity(PI) = PredFormArity :-
    pred_info_get_orig_arity(PI, PredFormArity).

pred_info_user_arity(PI) = UserArity :-
    pred_info_get_is_pred_or_func(PI, PredOrFunc),
    pred_info_get_orig_arity(PI, PredFormArity),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity).

pred_info_get_sym_name(PredInfo, SymName) :-
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    SymName = qualified(Module, Name).

pred_info_get_pf_sym_name_pred_form_arity(PredInfo, PFSymNameArity) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_sym_name(PredInfo, SymName),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    PFSymNameArity =
        pf_sym_name_pred_form_arity(PredOrFunc, SymName, PredFormArity).

pred_info_get_pf_sym_name_user_arity(PredInfo, PFSymNameArity) :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    pred_info_get_sym_name(PredInfo, SymName),
    UserArity = pred_info_user_arity(PredInfo),
    PFSymNameArity = pf_sym_name_user_arity(PredOrFunc, SymName, UserArity).

%---------------------------------------------------------------------------%
%
% Procedure management.
%

pred_info_all_proc_ids(PredInfo) = ProcIds :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.keys(ProcTable, ProcIds).

pred_info_all_exported_proc_ids(PredInfo) = ProcIds :-
    pred_info_get_status(PredInfo, pred_status(OldImportStatus)),
    (
        ( OldImportStatus = status_exported
        ; OldImportStatus = status_opt_exported
        ; OldImportStatus = status_exported_to_submodules
        ),
        ProcIds = pred_info_all_proc_ids(PredInfo)
    ;
        OldImportStatus = status_pseudo_exported,
        in_in_unification_proc_id(InInUnifyProcId),
        ProcIds = [InInUnifyProcId]
    ;
        ( OldImportStatus = status_imported(_)
        ; OldImportStatus = status_opt_imported
        ; OldImportStatus = status_abstract_imported
        ; OldImportStatus = status_pseudo_imported
        ; OldImportStatus = status_abstract_exported
        ; OldImportStatus = status_local
        ; OldImportStatus = status_external(_)
        ),
        ProcIds = []
    ).

pred_info_will_codegen_proc_ids(PredInfo) = ProcIds :-
    pred_info_get_status(PredInfo, pred_status(OldImportStatus)),
    (
        ( OldImportStatus = status_imported(_)
        ; OldImportStatus = status_external(_)
        ),
        ProcIds = []
    ;
        OldImportStatus = status_pseudo_imported,
        ProcIds0 = pred_info_all_proc_ids(PredInfo),
        in_in_unification_proc_id(InInUnifyProcId),
        % For pseudo_imported preds, proc 0 is imported, but
        % the code generated for all other procs will be included
        % in the code generated for the current module.
        list.delete_all(ProcIds0, InInUnifyProcId, ProcIds)
    ;
        ( OldImportStatus = status_opt_imported
        ; OldImportStatus = status_exported
        ; OldImportStatus = status_opt_exported
        ; OldImportStatus = status_abstract_exported
        ; OldImportStatus = status_pseudo_exported
        ; OldImportStatus = status_exported_to_submodules
        ; OldImportStatus = status_local
        ),
        ProcIds = pred_info_all_proc_ids(PredInfo)
    ;
        OldImportStatus = status_abstract_imported,
        % This status is not applicable to predicates.
        unexpected($pred, "status_abstract_imported")
    ).

next_proc_id(ProcTable, ProcId) :-
    % We could store the next available ModeId rather than recomputing
    % it on demand, but it is probably more efficient this way.
    map.to_assoc_list(ProcTable, ProcIdsInfos),
    list.length(ProcIdsInfos, Num),
    proc_id_to_int(ProcId, Num).

pred_info_proc_info(PredInfo, ProcId, ProcInfo) :-
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo).

pred_info_set_proc_info(ProcId, ProcInfo, !PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, ProcTable0),
    map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, !PredInfo).

pred_info_remove_proc_id(ProcId, !PredInfo) :-
    pred_info_get_proc_table(!.PredInfo, Procs0),
    map.delete(ProcId, Procs0, Procs),
    pred_info_set_proc_table(Procs, !PredInfo).

%---------------------------------------------------------------------------%
%
% Argument list management.
%

pred_info_get_arg_types(PredInfo, X, Y, Z) :-
    X = PredInfo ^ pi_decl_typevarset,
    Y = PredInfo ^ pi_exist_quant_tvars,
    Z = PredInfo ^ pi_arg_types.

pred_info_set_arg_types(X, Y, Z, !PredInfo) :-
    !PredInfo ^ pi_decl_typevarset := X,
    !PredInfo ^ pi_exist_quant_tvars := Y,
    !PredInfo ^ pi_arg_types := Z.

pred_info_get_univ_quant_tvars(PredInfo, UnivQVars) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    type_vars_in_types(ArgTypes, ArgTypeVars0),
    list.sort_and_remove_dups(ArgTypeVars0, ArgTypeVars),
    pred_info_get_exist_quant_tvars(PredInfo, ExistQVars),
    list.delete_elems(ArgTypeVars, ExistQVars, UnivQVars).

%---------------------------------------------------------------------------%
%
% Status tests.
%

pred_info_is_imported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    ( PredStatus = pred_status(status_imported(_))
    ; PredStatus = pred_status(status_external(_))
    ).

pred_info_is_imported_not_external(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_imported(_)).

pred_info_is_pseudo_imported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_pseudo_imported).

pred_info_is_exported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_exported).

pred_info_is_opt_exported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_opt_exported).

pred_info_is_exported_to_submodules(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_exported_to_submodules).

pred_info_is_pseudo_exported(PredInfo) :-
    pred_info_get_status(PredInfo, PredStatus),
    PredStatus = pred_status(status_pseudo_exported).

procedure_is_exported(ModuleInfo, PredInfo, ProcId) :-
    % XXX STATUS
    (
        pred_info_is_exported(PredInfo)
    ;
        pred_info_is_opt_exported(PredInfo)
    ;
        pred_info_is_exported_to_submodules(PredInfo)
    ;
        pred_info_is_pseudo_exported(PredInfo),
        in_in_unification_proc_id(ProcId)
    ;
        pred_info_get_status(PredInfo, PredStatus),
        PredStatus = pred_status(status_external(ExternalImportStatus)),
        pred_status_is_exported(pred_status(ExternalImportStatus)) = yes
    ;
        pred_info_get_origin(PredInfo, Origin),
        Origin = origin_compiler(made_for_uci(SpecialPredId, TypeCtor)),
        module_info_get_type_table(ModuleInfo, TypeTable),
        % If the search fails, then TypeCtor must be a builtin type
        % constructor, such as the tuple constructor.
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_in_exported_eqv(TypeDefn, yes),
        require_complete_switch [SpecialPredId]
        (
            SpecialPredId = spec_pred_unify,
            % The other proc_ids are module-specific.
            in_in_unification_proc_id(ProcId)
        ;
            SpecialPredId = spec_pred_compare
            % The declared modes are all global, and we don't
            % generate any modes for compare preds dynamically.
        ;
            SpecialPredId = spec_pred_index,
            % The index predicate is never called from anywhere
            % except the compare predicate.
            fail
        )
    ).

%---------------------------------------------------------------------------%
%
% Non-status tests.
%

pred_info_defn_has_clause(PredInfo) :-
    pred_info_get_goal_type(PredInfo, GoalType),
    GoalType = goal_not_for_promise(NPGoalType),
    goal_type_has_clause(NPGoalType).

:- pred goal_type_has_clause(np_goal_type::in) is semidet.

goal_type_has_clause(np_goal_type_clause).
goal_type_has_clause(np_goal_type_clause_and_foreign).

pred_info_defn_has_foreign_proc(PredInfo) :-
    pred_info_get_goal_type(PredInfo, GoalType),
    GoalType = goal_not_for_promise(NPGoalType),
    goal_type_has_foreign_proc(NPGoalType).

:- pred goal_type_has_foreign_proc(np_goal_type::in) is semidet.

goal_type_has_foreign_proc(np_goal_type_foreign).
goal_type_has_foreign_proc(np_goal_type_clause_and_foreign).

pred_info_infer_modes(PredInfo) :-
    pred_info_get_markers(PredInfo, Markers),
    marker_is_present(Markers, marker_infer_modes).

pred_info_get_purity(PredInfo0, Purity) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( if marker_is_present(Markers, marker_is_impure) then
        Purity = purity_impure
    else if marker_is_present(Markers, marker_is_semipure) then
        Purity = purity_semipure
    else
        Purity = purity_pure
    ).

pred_info_get_promised_purity(PredInfo0, MaybePromisedPurity) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( if marker_is_present(Markers, marker_promised_pure) then
        MaybePromisedPurity = yes(purity_pure)
    else if marker_is_present(Markers, marker_promised_semipure) then
        MaybePromisedPurity = yes(purity_semipure)
    else
        MaybePromisedPurity = no
    ).

pred_info_requested_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( marker_is_present(Markers, marker_user_marked_inline)
    ; marker_is_present(Markers, marker_heuristic_inline)
    ).

pred_info_requested_no_inlining(PredInfo0) :-
    pred_info_get_markers(PredInfo0, Markers),
    ( marker_is_present(Markers, marker_user_marked_no_inline)
    ; marker_is_present(Markers, marker_mmc_marked_no_inline)
    ).

calls_are_fully_qualified(Markers) =
    ( if marker_is_present(Markers, marker_calls_are_fully_qualified) then
        is_fully_qualified
    else
        may_be_partially_qualified
    ).

%---------------------------------------------------------------------------%

proc_interface_should_use_typeinfo_liveness(PredInfo, ProcId, Globals,
        InterfaceTypeInfoLiveness) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    pred_info_get_orig_arity(PredInfo, pred_form_arity(PredFormArityInt)),
    ( if no_type_info_builtin(PredModule, PredName, PredFormArityInt) then
        InterfaceTypeInfoLiveness = no
    else
        pred_info_get_status(PredInfo, Status),
        pred_info_get_proc_table(PredInfo, ProcTable),
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_is_address_taken(ProcInfo, IsAddressTaken),
        non_special_interface_should_use_typeinfo_liveness(Status,
            IsAddressTaken, Globals, InterfaceTypeInfoLiveness)
    ).

    % Return true if the interface of a procedure in a non-special predicate
    % with the given characteristics (import/export/local status,
    % address taken status) must include typeinfos for all the type variables
    % in the types of the arguments.
    %
    % Note that only a few predicates in the builtin modules are special
    % in this sense, and that compiler-generated predicates are never special.
    %
:- pred non_special_interface_should_use_typeinfo_liveness(pred_status::in,
    is_address_taken::in, globals::in, bool::out) is det.

non_special_interface_should_use_typeinfo_liveness(PredStatus, IsAddressTaken,
        Globals, InterfaceTypeInfoLiveness) :-
    ( if
        (
            IsAddressTaken = address_is_taken
        ;
            % If the predicate is exported, its address may have
            % been taken elsewhere. If it is imported, then it
            % follows that it must be exported somewhere.
            PredStatus \= pred_status(status_local)
        ;
            % If term size profiling (of either form) is enabled,
            % then we may need to access the typeinfo of any
            % variable bound to a heap cell argument. The only way
            % to ensure that this is possible is to preserve the
            % ability to access the typeinfo of any variable.
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_words, yes)
        ;
            globals.lookup_bool_option(Globals,
                record_term_sizes_as_cells, yes)
        ;
            non_special_body_should_use_typeinfo_liveness(Globals, yes)
        )
    then
        InterfaceTypeInfoLiveness = yes
    else
        InterfaceTypeInfoLiveness = no
    ).

body_should_use_typeinfo_liveness(PredInfo, Globals, BodyTypeInfoLiveness) :-
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    pred_info_get_orig_arity(PredInfo, pred_form_arity(PredFormArityInt)),
    ( if no_type_info_builtin(PredModule, PredName, PredFormArityInt) then
        BodyTypeInfoLiveness = no
    else
        non_special_body_should_use_typeinfo_liveness(Globals,
            BodyTypeInfoLiveness)
    ).

    % Return true if the body of a procedure in a non-special predicate
    % must keep a typeinfo variable alive during the lifetime of all variables
    % whose type includes the corresponding type variable.
    %
:- pred non_special_body_should_use_typeinfo_liveness(globals::in,
    bool::out) is det.

non_special_body_should_use_typeinfo_liveness(Globals, BodyTypeInfoLiveness) :-
    globals.lookup_bool_option(Globals, body_typeinfo_liveness,
        BodyTypeInfoLiveness).

%---------------------------------------------------------------------------%
%
% The definition of the pred_info type, which contains the information
% specific to a predicate, as opposed to a procedure.
% (Functions count as predicates.)
%
% The pred_info and pred_sub_info types constitute a single logical
% data structure split into two parts for efficiency purposes.
%
% The pred_info type contains the most frequently accessed and/or updated
% pieces of information about the predicate. Everything else is in the
% pred_sub_info type. This arrangement minimizes the amount of memory that
% needs to be allocated, and filled in, when a field is updated.
%

:- type pred_info
    --->    pred_info(
                % The Boehm collector allocates blocks whose sizes are
                % multiples of 2. Ideally, we would want the number of fields
                % of pred_info to be a multiple of 2 as well, but as of
                % 2017 march 15, this seems to be the optimal arrangement (zs).

                % Module in which pred occurs.
/*  1 */        pi_module_name          :: module_name,

                % Is this "predicate" really a predicate or a function?
/*  2 */        pi_is_pred_or_func      :: pred_or_func,

                % Predicate name.
/*  3 */        pi_name                 :: string,

                % The original arity of the pred, i.e. its arity *not* counting
                % any type_info and/or typeclass_info arguments inserted
                % automatically by the compiler.
                %
                % For functions, the original arity *includes* the return
                % value, so that e.g. the original arity of int.+ would be 3.
/*  4 */        pi_orig_arity           :: pred_form_arity,

                % Where did the predicate come from?
/*  5 */        pi_pred_origin          :: pred_origin,

/*  6 */        pi_status               :: pred_status,

                % Various boolean flags.
/*  7 */        pi_markers              :: pred_markers,

                % Argument types.
                % Note that it is an invariant that any type_info- and/or
                % typeclass_info-related variables in the arguments of a
                % predicate must precede any polymorphically-typed arguments
                % whose type depends on the values of those type_info- and/or
                % typeclass_info-related variables; accurate GC for the MLDS
                % back-end relies on this.
/*  8 */        pi_arg_types            :: list(mer_type),

                % Names of type vars in the predicate's type declaration.
/*  9 */        pi_decl_typevarset      :: tvarset,

                % Names of type vars in the predicate's type declaration
                % or in the variable type assignments.
/* 10 */        pi_typevarset           :: tvarset,

                % The set of existentially quantified type variables in the
                % predicate's type declaration.
/* 11 */        pi_exist_quant_tvars    :: existq_tvars,

                % The class constraints on the type variables in the
                % predicate's type declaration.
                %
                % For predicates that represent a method of a typeclass,
                % the first universal constraint will be the constraint
                % for that typeclass. This is ensured by code in
                % module_add_class_method, which is executed when
                % the class method's pred declaration is added to the HLDS.
/* 12 */        pi_class_context        :: univ_exist_constraints,

/* 13 */        pi_clauses_info         :: clauses_info,

/* 14 */        pi_proc_table           :: proc_table,

/* 15 */        pi_pred_sub_info        :: pred_sub_info
            ).

:- type pred_sub_info
    --->    pred_sub_info(
                % The location (line #) of the :- pred decl.
                psi_context                     :: prog_context,

                % If the predicate is defined (a) in the current module, and
                % (b) explicitly by the user, as opposed to by the compiler,
                % then this records what section the predicate declaration
                % is in, and whether it is a predmode declaration.
                %
                % Note that "defined explicitly by the user" does not guarantee
                % that the cur_user_decl_info will contain a valid
                % item_seq_num, because for class methods, it won't.
                % (This is because predicate declarations in typeclass items
                % do not have their own separate item_seq_num.)
                psi_cur_user_decl               :: maybe(cur_user_decl_info),

                % Whether the goals seen so far, if any, for this predicate
                % are clauses or foreign_code(...) pragmas.
                psi_goal_type                   :: goal_type,

                % Kinds of the type vars.
                psi_tvar_kind_map               :: tvar_kind_map,

                % The statically known bindings of existentially quantified
                % type variables inside this predicate. This field is set
                % at the end of the polymorphism stage.
                psi_existq_tvar_binding         :: tsubst,

                % The number of type_info and/or typeclass_info arguments
                % added by the polymorphism pass. This field is set
                % at the end of that pass.
                %
                % XXX ARGVEC: When we use argvecs to record the predicate's
                % argument vector, we should be able to delete this field.
                psi_polymorphism_added_args     :: int,

                % The set of type variables which the body of the predicate
                % can't bind, and whose type_infos are produced elsewhere.
                % This includes universally quantified head types (the
                % type_infos are passed in) plus existentially quantified types
                % in preds called from the body (the type_infos are returned
                % from the called predicates). Computed during type checking.
                psi_external_type_params        :: external_type_params,

                % Explanations of how redundant constraints were eliminated.
                % These are needed by polymorphism.m to work out where to get
                % the typeclass_infos from. Computed during type checking.
                psi_constraint_proof_map        :: constraint_proof_map,

                % Maps constraint identifiers to the actual constraints.
                % Computed during type checking.
                psi_constraint_map              :: constraint_map,

                % Unproven class constraints on type variables in the
                % predicate's body, if any (if this remains non-empty after
                % type checking has finished, post_typecheck.m will report a
                % type error).
                psi_unproven_body_constraints   :: list(prog_constraint),

                % The predicate's inst graph, for constraint based
                % mode analysis.
                psi_inst_graph_info             :: inst_graph_info,

                % Mode information extracted from constraint based
                % mode analysis.
                psi_arg_modes_maps              :: list(arg_modes_map),

                % Renames of some head variables computed by headvar_names.m,
                % for use by the debugger.
                psi_var_name_remap              :: map(prog_var, string),

                % List of assertions which mention this predicate.
                psi_assertions                  :: set(assert_id),

                % If this predicate is marked as obsolete, this will be a
                % "yes(_)" wrapped around a list of the predicate names that
                % the compiler should suggest as possible replacements.
                % (Note that the list of possible replacements may be empty.)
                % In the usual case where this predicate is NOT marked
                % as obsolete, this will be "no".
                psi_obsolete_in_favour_of       :: maybe(list(sym_name_arity)),

                % If this field contains yes(FormatCall), then this predicate
                % has a format_call pragma, and FormatCall contains both the
                % <format string, values list> argument number pairs
                % specified in that pragma, and the context of that pragma.
                % If this field contains no, then the predicate does not have
                % a format_call pragma.
                %
                % When the HLDS is first created, the argument numbers
                % in the format_string_values structures in the list
                % refer to the position of the arguments in the visible
                % argument list. (The numbering starts at 1.) When polymorphism
                % adds compiler-generated arguments to the start of the
                % argument list, it increments all the argument numbers
                % in this field to compensate.
                %
                % Some optimizations may also add or delete arguments,
                % but they don't have to update this field, because
                %
                % - this field is used only by format_call.m, during the
                %   simplification pass done at the end of the front end,
                %
                % - optimizations that can change argument lists are
                %   all run *after* the front end, and therefore after
                %   all code that cares about the value of this field.
                psi_format_call_info            :: maybe(format_call_info),

                % If this predicate is a class method implementation, this
                % list records the argument types before substituting the type
                % variables for the instance.
                % XXX does that make sense?
                psi_instance_method_arg_types   :: list(mer_type)
            ).

% Access stats for the pred_info structure, derived on 2014 dec 13:
%
%  i        read        same        diff   same%
%  1 124,287,827     348,040  31,892,426   1.08%    procedures
%  2  72,037,616      96,336   1,082,541   8.17%    status
%  3  43,651,895           0           0            module_name
%  4  32,003,757           0         795   0.00%    name
%  5  25,261,836           0           0            orig_arity
%  6  24,876,447     905,599   1,098,352  45.19%    markers
%  7  22,294,552      19,444  12,496,762   0.16%    clauses_info
%  8  20,415,273           0           0            arg_types
%  9  15,356,498         727          68  91.45%    is_pred_or_func
% 10  12,075,408     382,680      89,618  81.03%    origin
% 11  11,783,136   9,736,724     752,983  92.82%    typevarset
% 12  11,128,685   4,600,914   1,642,568  73.69%    three fields:
%                                                       decl_typevarset,
%                                                       exist_quant_vars and
%                                                       arg_types
% 13   7,871,038           0   2,700,797   0.00%    class_context
% 14   6,629,313     100,630   1,054,197   8.71%    goal_type
% 15   5,892,199       6,544       6,726  49.31%    var_name_remap
% 16   3,820,195      85,054           0 100.00%    tvar_kind_map
% 17   2,752,537     404,771      23,921  94.42%    constraint_map
% 18   2,591,016     425,209       3,483  99.19%    constraint_proof_map
% 19   1,667,832           0           0            context
% 20   1,374,911           0           0            exist_quant_vars
% 21     476,703     276,426     152,903  64.39%    external_type_params
% 22     285,538     428,650           4 100.00%    unproven_body_constraints
% 23      22,563           0          80   0.00%    existq_tvar_binding
% 24       3,834           0       3,797   0.00%    assertions
% 25          10           0           0            attributes
% 26           0           0      19,439   0.00%    instance_method_arg_types
% 27           0           0           0            arg_modes_maps
% 28           0           0           0            inst_graph_info

%---------------------------------------------------------------------------%

pred_info_get_module_name(PI, X) :-
    X = PI ^ pi_module_name.
pred_info_get_is_pred_or_func(PI, X) :-
    X = PI ^ pi_is_pred_or_func.
pred_info_get_name(PI, X) :-
    X = PI ^ pi_name.
pred_info_get_orig_arity(PI, X) :-
    X = PI ^ pi_orig_arity.
pred_info_get_origin(PI, X) :-
    X = PI ^ pi_pred_origin.
pred_info_get_status(PI, X) :-
    X = PI ^ pi_status.
pred_info_get_markers(PI, X) :-
    X = PI ^ pi_markers.
pred_info_get_arg_types(PI, X) :-
    X = PI ^ pi_arg_types.
pred_info_get_typevarset(PI, X) :-
    X = PI ^ pi_typevarset.
pred_info_get_exist_quant_tvars(PI, X) :-
    X = PI ^ pi_exist_quant_tvars.
pred_info_get_class_context(PI, X) :-
    X = PI ^ pi_class_context.
pred_info_get_clauses_info(PI, X) :-
    X = PI ^ pi_clauses_info.
pred_info_get_proc_table(PI, X) :-
    X = PI ^ pi_proc_table.

pred_info_get_context(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_context.
pred_info_get_cur_user_decl_info(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_cur_user_decl.
pred_info_get_goal_type(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_goal_type.
pred_info_get_tvar_kind_map(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_tvar_kind_map.
pred_info_get_existq_tvar_binding(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_existq_tvar_binding.
pred_info_get_polymorphism_added_args(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_polymorphism_added_args.
pred_info_get_external_type_params(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_external_type_params.
pred_info_get_constraint_proof_map(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_constraint_proof_map.
pred_info_get_constraint_map(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_constraint_map.
pred_info_get_unproven_body_constraints(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_unproven_body_constraints.
pred_info_get_inst_graph_info(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_inst_graph_info.
pred_info_get_arg_modes_maps(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_arg_modes_maps.
pred_info_get_var_name_remap(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_var_name_remap.
pred_info_get_assertions(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_assertions.
pred_info_get_obsolete_in_favour_of(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_obsolete_in_favour_of.
pred_info_get_format_call_info(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_format_call_info.
pred_info_get_instance_method_arg_types(PI, X) :-
    X = PI ^ pi_pred_sub_info ^ psi_instance_method_arg_types.

pred_info_set_module_name(X, !PI) :-
    !PI ^ pi_module_name := X.
pred_info_set_is_pred_or_func(X, !PI) :-
    ( if X = !.PI ^ pi_is_pred_or_func then
        true
    else
        !PI ^ pi_is_pred_or_func := X
    ).
pred_info_set_name(X, !PI) :-
    !PI ^ pi_name := X.
pred_info_set_orig_arity(X, !PI) :-
    !PI ^ pi_orig_arity := X.
pred_info_set_origin(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ pi_pred_origin) then
        true
    else
        !PI ^ pi_pred_origin := X
    ).
pred_info_set_status(X, !PI) :-
    !PI ^ pi_status := X.
pred_info_set_goal_type(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_goal_type := X.
pred_info_set_markers(X, !PI) :-
    !PI ^ pi_markers := X.
pred_info_set_typevarset(X, !PI) :-
    ( if private_builtin.pointer_equal(X, !.PI ^ pi_typevarset) then
        true
    else
        !PI ^ pi_typevarset := X
    ).
pred_info_set_class_context(X, !PI) :-
    !PI ^ pi_class_context := X.
pred_info_set_clauses_info(X, !PI) :-
    !PI ^ pi_clauses_info := X.
pred_info_set_proc_table(X, !PI) :-
    !PI ^ pi_proc_table := X.

pred_info_set_tvar_kind_map(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_tvar_kind_map)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_tvar_kind_map:= X
    ).
pred_info_set_existq_tvar_binding(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_existq_tvar_binding := X.
pred_info_set_polymorphism_added_args(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_polymorphism_added_args := X.
pred_info_set_external_type_params(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_external_type_params)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_external_type_params := X
    ).
pred_info_set_constraint_proof_map(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_constraint_proof_map)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_constraint_proof_map := X
    ).
pred_info_set_constraint_map(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_constraint_map)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_constraint_map := X
    ).
pred_info_set_unproven_body_constraints(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_unproven_body_constraints)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_unproven_body_constraints := X
    ).
pred_info_set_inst_graph_info(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_inst_graph_info := X.
pred_info_set_arg_modes_maps(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_arg_modes_maps := X.
pred_info_set_var_name_remap(X, !PI) :-
    ( if
        private_builtin.pointer_equal(X,
            !.PI ^ pi_pred_sub_info ^ psi_var_name_remap)
    then
        true
    else
        !PI ^ pi_pred_sub_info ^ psi_var_name_remap := X
    ).
pred_info_set_assertions(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_assertions := X.
pred_info_set_obsolete_in_favour_of(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_obsolete_in_favour_of := X.
pred_info_set_format_call_info(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_format_call_info := X.
pred_info_set_instance_method_arg_types(X, !PI) :-
    !PI ^ pi_pred_sub_info ^ psi_instance_method_arg_types := X.

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_pred.
%---------------------------------------------------------------------------%
