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
% This module defines the part of the HLDS that deals with procedures.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_proc.
:- interface.

:- import_module check_hlds.
:- import_module check_hlds.mode_constraint_robdd.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_rtti.
:- import_module hlds.pred_proc_id.
:- import_module hlds.proc_info_types.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module termination.
:- import_module termination.term_constr.
:- import_module termination.term_constr.term_constr_main_types.
:- import_module termination.term_osi.
:- import_module termination.term_osi.term_osi_util.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

:- type proc_info.

%---------------------------------------------------------------------------%
%
% Creating proc_infos.
%

:- pred proc_info_create(prog_context::in, item_seq_num::in,
    var_table::in, list(prog_var)::in,
    inst_varset::in, list(mer_mode)::in,
    detism_decl::in, determinism::in, hlds_goal::in,
    rtti_varmaps::in, is_address_taken::in, has_parallel_conj::in,
    map(prog_var, string)::in, proc_info::out) is det.

    % Exported to hlds_pred.m only.
    %
:- pred proc_info_create_with_declared_detism(prog_context::in,
    item_seq_num::in, var_table::in, list(prog_var)::in,
    inst_varset::in, list(mer_mode)::in,
    detism_decl::in, maybe(determinism)::in, determinism::in, hlds_goal::in,
    rtti_varmaps::in, is_address_taken::in, has_parallel_conj::in,
    map(prog_var, string)::in, proc_info::out) is det.

    % Exported to hlds_pred.m only.
    %
:- pred proc_info_init(module_info::in, prog_context::in, item_seq_num::in,
    list(mer_type)::in, inst_varset::in, maybe(list(mer_mode))::in,
    list(mer_mode)::in, maybe(list(is_live))::in,
    detism_decl::in, maybe(determinism)::in, is_address_taken::in,
    has_parallel_conj::in, map(prog_var, string)::in, proc_info::out) is det.

%---------------------------------------------------------------------------%
%
% Cloning proc_infos.
%

% proc_prepare_to_clone returns all the fields of an existing proc_info,
% while proc_create constructs a new proc_info putting the supplied values
% to each field.
%
% These predicates exist because we want keep the definition of the proc_info
% type private (to make future changes easier), but we also want to make it
% possible to create slightly modified copies of existing procedures
% with the least amount of programming work. We also want to require
% (a) programmers writing such cloning code to consider what effect
% the modification may have on *all* fields of the proc_info, and
% (b) programmers who add new fields to the proc_info to update
% all the places in the compiler that do such cloning.

:- pred proc_prepare_to_clone(proc_info::in, list(prog_var)::out,
    hlds_goal::out, var_table::out, rtti_varmaps::out,
    inst_varset::out, maybe(list(mer_mode))::out, list(mer_mode)::out,
    maybe(list(is_live))::out, maybe(determinism)::out, determinism::out,
    eval_method::out, prog_context::out, item_seq_num::out,
    can_process::out, maybe(mode_constraint)::out, detism_decl::out,
    list(prog_context)::out, maybe(untuple_proc_info)::out,
    maybe_input_spec_proc::out, map(prog_var, string)::out,
    list(warn_spec)::out, set(pred_proc_id)::out,
    is_address_taken::out, proc_foreign_exports::out, has_parallel_conj::out,
    has_user_event::out, has_tail_rec_call::out, list(oisu_pred_kind_for)::out,
    maybe(require_tail_recursion)::out, set_of_progvar::out,
    maybe(list(arg_info))::out, maybe(special_proc_return)::out,
    codegen_liveness::out, stack_slots::out, needs_maxfr_slot::out,
    maybe(prog_var)::out, maybe(proc_table_io_info)::out,
    maybe(table_attributes)::out, maybe(list(sym_name_arity))::out,
    maybe(deep_profile_proc_info)::out, maybe(arg_size_info)::out,
    maybe(termination_info)::out, termination2_info::out,
    maybe(proc_exception_info)::out, maybe(proc_trailing_info)::out,
    maybe(proc_mm_tabling_info)::out, sharing_reuse_info::out) is det.

:- pred proc_create(list(prog_var)::in,
    hlds_goal::in, var_table::in, rtti_varmaps::in,
    inst_varset::in, maybe(list(mer_mode))::in, list(mer_mode)::in,
    maybe(list(is_live))::in, maybe(determinism)::in, determinism::in,
    eval_method::in, prog_context::in, item_seq_num::in, can_process::in,
    maybe(mode_constraint)::in, detism_decl::in, list(prog_context)::in,
    maybe(untuple_proc_info)::in, maybe_input_spec_proc::in,
    map(prog_var, string)::in, list(warn_spec)::in, set(pred_proc_id)::in,
    is_address_taken::in, proc_foreign_exports::in, has_parallel_conj::in,
    has_user_event::in, has_tail_rec_call::in, list(oisu_pred_kind_for)::in,
    maybe(require_tail_recursion)::in, set_of_progvar::in,
    maybe(list(arg_info))::in, maybe(special_proc_return)::in,
    codegen_liveness::in, stack_slots::in, needs_maxfr_slot::in,
    maybe(prog_var)::in, maybe(proc_table_io_info)::in,
    maybe(table_attributes)::in, maybe(list(sym_name_arity))::in,
    maybe(deep_profile_proc_info)::in, maybe(arg_size_info)::in,
    maybe(termination_info)::in, termination2_info::in,
    maybe(proc_exception_info)::in, maybe(proc_trailing_info)::in,
    maybe(proc_mm_tabling_info)::in, sharing_reuse_info::in,
    proc_info::out) is det.

%---------------------------------------------------------------------------%
%
% Nontrivial getters and setters.
%

:- pred proc_info_set_body(var_table::in,
    list(prog_var)::in, hlds_goal::in, rtti_varmaps::in,
    proc_info::in, proc_info::out) is det.

%---------------------------------------------------------------------------%
%
% Getters and setters.
%

:- pred proc_info_get_headvars(proc_info::in, list(prog_var)::out) is det.
:- pred proc_info_get_goal(proc_info::in, hlds_goal::out) is det.
:- pred proc_info_get_var_table(proc_info::in, var_table::out) is det.
:- pred proc_info_get_rtti_varmaps(proc_info::in, rtti_varmaps::out) is det.
:- pred proc_info_get_inst_varset(proc_info::in, inst_varset::out) is det.
:- pred proc_info_get_maybe_declared_argmodes(proc_info::in,
    maybe(list(mer_mode))::out) is det.
:- pred proc_info_get_argmodes(proc_info::in, list(mer_mode)::out) is det.
:- pred proc_info_get_maybe_arglives(proc_info::in,
    maybe(list(is_live))::out) is det.
:- pred proc_info_get_declared_determinism(proc_info::in,
    maybe(determinism)::out) is det.
:- pred proc_info_get_inferred_determinism(proc_info::in,
    determinism::out) is det.
:- pred proc_info_get_eval_method(proc_info::in, eval_method::out) is det.

:- pred proc_info_get_context(proc_info::in, prog_context::out) is det.
:- pred proc_info_get_item_number(proc_info::in, item_seq_num::out) is det.
:- pred proc_info_get_maybe_head_modes_constr(proc_info::in,
    maybe(mode_constraint)::out) is det.
:- pred proc_info_get_cse_nopull_contexts(proc_info::in,
    list(prog_context)::out) is det.
:- pred proc_info_get_var_name_remap(proc_info::in,
    map(prog_var, string)::out) is det.
:- pred proc_info_get_statevar_warnings(proc_info::in,
    list(warn_spec)::out) is det.
:- pred proc_info_get_deleted_call_callees(proc_info::in,
    set(pred_proc_id)::out) is det.
:- pred proc_info_get_can_process(proc_info::in, can_process::out) is det.
:- pred proc_info_get_detism_decl(proc_info::in, detism_decl::out) is det.
:- pred proc_info_get_is_address_taken(proc_info::in,
    is_address_taken::out) is det.
:- pred proc_info_get_has_any_foreign_exports(proc_info::in,
    proc_foreign_exports::out) is det.
:- pred proc_info_get_has_parallel_conj(proc_info::in,
    has_parallel_conj::out) is det.
:- pred proc_info_get_has_user_event(proc_info::in,
    has_user_event::out) is det.
:- pred proc_info_get_needs_maxfr_slot(proc_info::in,
    needs_maxfr_slot::out) is det.
:- pred proc_info_get_has_tail_rec_call(proc_info::in,
    has_tail_rec_call::out) is det.
:- pred proc_info_get_oisu_kind_fors(proc_info::in,
    list(oisu_pred_kind_for)::out) is det.
:- pred proc_info_get_maybe_require_tailrec_info(proc_info::in,
    maybe(require_tail_recursion)::out) is det.
:- pred proc_info_get_obsolete_in_favour_of(proc_info::in,
    maybe(list(sym_name_arity))::out) is det.
:- pred proc_info_get_reg_r_headvars(proc_info::in,
    set_of_progvar::out) is det.
:- pred proc_info_get_maybe_arg_info(proc_info::in,
    maybe(list(arg_info))::out) is det.
:- pred proc_info_get_maybe_special_return(proc_info::in,
    maybe(special_proc_return)::out) is det.
:- pred proc_info_get_initial_liveness(proc_info::in,
    codegen_liveness::out) is det.
:- pred proc_info_get_stack_slots(proc_info::in, stack_slots::out) is det.
:- pred proc_info_get_call_table_tip(proc_info::in,
    maybe(prog_var)::out) is det.
:- pred proc_info_get_maybe_proc_table_io_info(proc_info::in,
    maybe(proc_table_io_info)::out) is det.
:- pred proc_info_get_table_attributes(proc_info::in,
    maybe(table_attributes)::out) is det.
:- pred proc_info_get_maybe_deep_profile_info(proc_info::in,
    maybe(deep_profile_proc_info)::out) is det.
:- pred proc_info_get_maybe_untuple_info(proc_info::in,
    maybe(untuple_proc_info)::out) is det.
:- pred proc_info_get_maybe_input_spec(proc_info::in,
    maybe_input_spec_proc::out) is det.
:- pred proc_info_get_maybe_arg_size_info(proc_info::in,
    maybe(arg_size_info)::out) is det.
:- pred proc_info_get_maybe_termination_info(proc_info::in,
    maybe(termination_info)::out) is det.
:- pred proc_info_get_termination2_info(proc_info::in,
    termination2_info::out) is det.
:- pred proc_info_get_exception_info(proc_info::in,
    maybe(proc_exception_info)::out) is det.
:- pred proc_info_get_trailing_info(proc_info::in,
    maybe(proc_trailing_info)::out) is det.
:- pred proc_info_get_mm_tabling_info(proc_info::in,
    maybe(proc_mm_tabling_info)::out) is det.
:- pred proc_info_get_sharing_reuse_info(proc_info::in,
    sharing_reuse_info::out) is det.

:- pred proc_info_set_headvars(list(prog_var)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_goal(hlds_goal::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_var_table(var_table::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_rtti_varmaps(rtti_varmaps::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inst_varset(inst_varset::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_declared_argmodes(maybe(list(mer_mode))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_argmodes(list(mer_mode)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_arglives(maybe(list(is_live))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_inferred_determinism(determinism::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_eval_method(eval_method::in,
    proc_info::in, proc_info::out) is det.

:- pred proc_info_set_head_modes_constraint(mode_constraint::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_cse_nopull_contexts(list(prog_context)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_var_name_remap(map(prog_var, string)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_statevar_warnings(list(warn_spec)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_deleted_call_callees(set(pred_proc_id)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_can_process(can_process::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_detism_decl(detism_decl::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_address_taken(is_address_taken::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_has_any_foreign_exports(proc_foreign_exports::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_has_parallel_conj(has_parallel_conj::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_has_user_event(has_user_event::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_needs_maxfr_slot(needs_maxfr_slot::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_has_tail_rec_call(has_tail_rec_call::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_oisu_kind_fors(list(oisu_pred_kind_for)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_require_tailrec_info(require_tail_recursion::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_obsolete_in_favour_of(maybe(list(sym_name_arity))::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_reg_r_headvars(set_of_progvar::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_arg_info(list(arg_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_special_return(maybe(special_proc_return)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_initial_liveness(codegen_liveness::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_stack_slots(stack_slots::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_call_table_tip(maybe(prog_var)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_proc_table_io_info(maybe(proc_table_io_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_table_attributes(maybe(table_attributes)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_deep_profile_info(
    maybe(deep_profile_proc_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_untuple_info(maybe(untuple_proc_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_input_spec(maybe_input_spec_proc::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_arg_size_info(maybe(arg_size_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_maybe_termination_info(maybe(termination_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_termination2_info(termination2_info::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_exception_info(maybe(proc_exception_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_trailing_info(maybe(proc_trailing_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_mm_tabling_info(maybe(proc_mm_tabling_info)::in,
    proc_info::in, proc_info::out) is det.
:- pred proc_info_set_sharing_reuse_info(sharing_reuse_info::in,
    proc_info::in, proc_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.type_util.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module string.
:- import_module term.
:- import_module unit.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% Creating proc_infos.
%

proc_info_create(Context, ItemNumber, VarTable, HeadVars,
        InstVarSet, HeadModes, DetismDecl, Detism, Goal, RttiVarMaps,
        IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo) :-
    proc_info_create_with_declared_detism(Context, ItemNumber,
        VarTable, HeadVars, InstVarSet, HeadModes,
        DetismDecl, yes(Detism), Detism, Goal, RttiVarMaps, IsAddressTaken,
        HasParallelConj, VarNameRemap, ProcInfo).

proc_info_create_with_declared_detism(MainContext, ItemNumber,
        VarTable, HeadVars, InstVarSet, Modes,
        DetismDecl, MaybeDeclaredDetism, Detism, Goal, RttiVarMaps,
        IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo) :-
    % See the comment at the top of  proc_info_init; it applies here as well.

    % Please use a variable for every field of the proc_info and proc_sub_info,
    % and please keep the definitions of those variables in the same order
    % as the fields themselves.

    % argument MainContext
    % argument ItemNumber
    CanProcess = can_process_now,
    % argument DetismDecl
    CseNopullContexts = [],
    % argument VarNameRemap
    StateVarWarnings = [],
    set.init(DeletedCallees),
    % argument IsAddressTaken
    HasForeignProcExports = no_foreign_exports,
    % argument HasParallelConj
    HasUserEvent = has_no_user_event,
    HasTailCallEvent = has_tail_rec_call(has_no_self_tail_rec_call,
        has_no_mutual_tail_rec_call),
    OisuKinds = [],
    MaybeRequireTailRecursion = no,
    MaybeObsoleteInFavourOf = no `with_type` maybe(list(sym_name_arity)),
    set_of_var.init(RegR_HeadVars),
    MaybeArgPassInfo = no `with_type` maybe(list(arg_info)),
    MaybeSpecialReturn = no `with_type` maybe(special_proc_return),
    set_of_var.init(InitialLiveness),
    map.init(StackSlots),
    NeedsMaxfrSlot = does_not_need_maxfr_slot,
    MaybeCallTableTip = no `with_type` maybe(prog_var),
    MaybeTableIOInfo = no `with_type` maybe(proc_table_io_info),
    MaybeTableAttrs = no `with_type` maybe(table_attributes),
    MaybeDeepProfProcInfo = no `with_type` maybe(deep_profile_proc_info),
    MaybeUntupleInfo = no `with_type` maybe(untuple_proc_info),
    MaybeInputSpecProc = not_involved_in_input_spec,
    MaybeArgSizes = no `with_type` maybe(arg_size_info),
    MaybeTermInfo = no `with_type` maybe(termination_info),
    Term2Info = term_constr_main_types.term2_info_init,
    MaybeExceptionInfo = no `with_type` maybe(proc_exception_info),
    MaybeTrailingInfo = no `with_type` maybe(proc_trailing_info),
    MaybeMMTablingInfo = no `with_type` maybe(proc_mm_tabling_info),
    SharingReuseInfo = sharing_reuse_info_init,

    ProcSubInfo = proc_sub_info(
        MainContext,
        ItemNumber,
        MaybeHeadModesConstr,
        CseNopullContexts,
        VarNameRemap,
        StateVarWarnings,
        DeletedCallees,
        CanProcess,
        DetismDecl,
        IsAddressTaken,
        HasForeignProcExports,
        HasParallelConj,
        HasUserEvent,
        NeedsMaxfrSlot,
        HasTailCallEvent,
        OisuKinds,
        MaybeRequireTailRecursion,
        MaybeObsoleteInFavourOf,
        RegR_HeadVars,
        MaybeArgPassInfo,
        MaybeSpecialReturn,
        InitialLiveness,
        StackSlots,
        MaybeCallTableTip,
        MaybeTableIOInfo,
        MaybeTableAttrs,
        MaybeDeepProfProcInfo,
        MaybeUntupleInfo,
        MaybeInputSpecProc,
        MaybeArgSizes,
        MaybeTermInfo,
        Term2Info,
        MaybeExceptionInfo,
        MaybeTrailingInfo,
        MaybeMMTablingInfo,
        SharingReuseInfo),

    % argument HeadVars
    % argument Goal
    % argument VarSet
    % argument VarTypes
    % argument RttiVarMaps
    % argument InstVarSet
    DeclaredModes = no,
    % argument Modes
    MaybeHeadModesConstr = no `with_type` maybe(mode_constraint),
    MaybeArgLives = no,
    % argument MaybeDeclaredDetism
    % argument Detism
    EvalMethod = eval_normal,

    ProcInfo = proc_info(
        HeadVars,
        Goal,
        VarTable,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeArgLives,
        MaybeDeclaredDetism,
        Detism,
        EvalMethod,
        ProcSubInfo).

proc_info_init(ModuleInfo, MainContext, ItemNumber, Types, InstVarSet,
        DeclaredModes, Modes, MaybeArgLives, DetismDecl, MaybeDeclaredDetism,
        IsAddressTaken, HasParallelConj, VarNameRemap, ProcInfo) :-
    % When this predicate is invoked during the construction of the HLDS,
    % some parts of the procedure aren't known yet. In that case, we can
    % simply initialize them to any old garbage which we will later throw away.
    %
    % However, when this predicate is invoked by HLDS transformation passes
    % after the front-end has finished, this strategy won't work. We need
    % to fill in every field with meaningful, correct information, unless
    % we know for sure that before the next pass that needs the correct value
    % in a field, we will invoke another pass that fills in the correct value
    % in that field.
    %
    % XXX I (zs) am far from sure that all the field initializations below,
    % in this predicate and in proc_info_create_with_declared_detism,
    % fulfill this condition.

    % Please use a variable for every field of the proc_info and proc_sub_info,
    % and please keep the definitions of those variables in the same order
    % as the fields themselves.

    % argument MainContext
    % argument ItemNumber
    CanProcess = can_process_now,
    % argument DetismDecl
    CseNopullContexts = [],
    MaybeUntupleInfo = no `with_type` maybe(untuple_proc_info),
    MaybeInputSpecProc = not_involved_in_input_spec,
    % argument VarNameRemap
    StateVarWarnings = [],
    set.init(DeletedCallees),
    % argument IsAddressTaken
    HasForeignProcExports = no_foreign_exports,
    % argument HasParallelConj
    HasUserEvent = has_no_user_event,
    HasTailCallEvent = has_tail_rec_call(has_no_self_tail_rec_call,
        has_no_mutual_tail_rec_call),
    OisuKinds = [],
    MaybeRequireTailRecursion = no,
    set_of_var.init(RegR_HeadVars),
    MaybeArgPassInfo = no `with_type` maybe(list(arg_info)),
    MaybeSpecialReturn = no `with_type` maybe(special_proc_return),
    set_of_var.init(InitialLiveness),
    map.init(StackSlots),
    NeedsMaxfrSlot = does_not_need_maxfr_slot,
    MaybeCallTableTip = no `with_type` maybe(prog_var),
    MaybeTableIOInfo = no `with_type` maybe(proc_table_io_info),
    MaybeTableAttrs = no `with_type` maybe(table_attributes),
    MaybeObsoleteInFavourOf = no `with_type` maybe(list(sym_name_arity)),
    MaybeDeepProfProcInfo = no `with_type` maybe(deep_profile_proc_info),
    MaybeArgSizes = no `with_type` maybe(arg_size_info),
    MaybeTermInfo = no `with_type` maybe(termination_info),
    Term2Info = term_constr_main_types.term2_info_init,
    MaybeExceptionInfo = no `with_type` maybe(proc_exception_info),
    MaybeTrailingInfo = no `with_type` maybe(proc_trailing_info),
    MaybeMMTablingInfo = no `with_type` maybe(proc_mm_tabling_info),
    SharingReuseInfo = sharing_reuse_info_init,

    ProcSubInfo = proc_sub_info(
        MainContext,
        ItemNumber,
        MaybeHeadModesConstr,
        CseNopullContexts,
        VarNameRemap,
        StateVarWarnings,
        DeletedCallees,
        CanProcess,
        DetismDecl,
        IsAddressTaken,
        HasForeignProcExports,
        HasParallelConj,
        HasUserEvent,
        NeedsMaxfrSlot,
        HasTailCallEvent,
        OisuKinds,
        MaybeRequireTailRecursion,
        MaybeObsoleteInFavourOf,
        RegR_HeadVars,
        MaybeArgPassInfo,
        MaybeSpecialReturn,
        InitialLiveness,
        StackSlots,
        MaybeCallTableTip,
        MaybeTableIOInfo,
        MaybeTableAttrs,
        MaybeDeepProfProcInfo,
        MaybeUntupleInfo,
        MaybeInputSpecProc,
        MaybeArgSizes,
        MaybeTermInfo,
        Term2Info,
        MaybeExceptionInfo,
        MaybeTrailingInfo,
        MaybeMMTablingInfo,
        SharingReuseInfo),

    init_var_table(VarTable0),
    make_fresh_prefix_named_vars_from_types(ModuleInfo, "HeadVar__", 1,
        Types, HeadVars, VarTable0, VarTable),
    goal_info_init(GoalInfo),
    BodyGoal = hlds_goal(conj(plain_conj, []), GoalInfo),
    rtti_varmaps_init(RttiVarMaps),
    % argument InstVarSet
    % argument DeclaredModes
    % argument Modes
    MaybeHeadModesConstr = no `with_type` maybe(mode_constraint),
    % argument MaybeArgLives
    % argument MaybeDeclaredDetism
    % Inferred determinism gets initialized to `erroneous'.
    % This is what `det_analysis.m' wants. det_analysis.m
    % will later provide the correct inferred determinism for it.
    InferredDetism = detism_erroneous,
    EvalMethod = eval_normal,

    ProcInfo = proc_info(
        HeadVars,
        BodyGoal,
        VarTable,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeArgLives,
        MaybeDeclaredDetism,
        InferredDetism,
        EvalMethod,
        ProcSubInfo).

:- pred make_fresh_prefix_named_vars_from_types(module_info::in,
    string::in, int::in, list(mer_type)::in, list(prog_var)::out,
    var_table::in, var_table::out) is det.

make_fresh_prefix_named_vars_from_types(_, _, _, [], [], !Info).
make_fresh_prefix_named_vars_from_types(ModuleInfo, BaseName, Num,
        [Type | Types], [Var | Vars], !VarTable) :-
    make_fresh_prefix_named_var_from_type(ModuleInfo, BaseName, Num,
        Type, Var, !VarTable),
    make_fresh_prefix_named_vars_from_types(ModuleInfo, BaseName, Num + 1,
        Types, Vars, !VarTable).

:- pred make_fresh_prefix_named_var_from_type(module_info::in,
    string::in, int::in, mer_type::in, prog_var::out,
    var_table::in, var_table::out) is det.

make_fresh_prefix_named_var_from_type(ModuleInfo, BaseName, Num, Type, Var,
        !VarTable) :-
    string.format("%s%d", [s(BaseName), i(Num)], Name),
    IsDummy = is_type_a_dummy(ModuleInfo, Type),
    Entry = vte(Name, Type, IsDummy),
    add_var_entry(Entry, Var, !VarTable).

%---------------------------------------------------------------------------%
%
% Cloning proc_infos.
%

proc_prepare_to_clone(ProcInfo, HeadVars, Goal, VarTable, RttiVarMaps,
        InstVarSet, DeclaredModes, Modes, MaybeArgLives,
        MaybeDeclaredDetism, Detism, EvalMethod,
        MainContext, ItemNumber, CanProcess, MaybeHeadModesConstr, DetismDecl,
        CseNopullContexts, MaybeUntupleInfo, MaybeInputSpecProc, VarNameRemap,
        StateVarWarnings, DeletedCallees,
        IsAddressTaken, HasForeignProcExports, HasParallelConj, HasUserEvent,
        HasTailCallEvent, OisuKinds, MaybeRequireTailRecursion,
        RegR_HeadVars, MaybeArgPassInfo, MaybeSpecialReturn, InitialLiveness,
        StackSlots, NeedsMaxfrSlot, MaybeCallTableTip, MaybeTableIOInfo,
        MaybeTableAttrs, MaybeObsoleteInFavourOf, MaybeDeepProfProcInfo,
        MaybeArgSizes, MaybeTermInfo, Term2Info, MaybeExceptionInfo,
        MaybeTrailingInfo, MaybeMMTablingInfo, SharingReuseInfo) :-
    ProcInfo = proc_info(
        HeadVars,
        Goal,
        VarTable,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeArgLives,
        MaybeDeclaredDetism,
        Detism,
        EvalMethod,
        ProcSubInfo),
    ProcSubInfo = proc_sub_info(
        MainContext,
        ItemNumber,
        MaybeHeadModesConstr,
        CseNopullContexts,
        VarNameRemap,
        StateVarWarnings,
        DeletedCallees,
        CanProcess,
        DetismDecl,
        IsAddressTaken,
        HasForeignProcExports,
        HasParallelConj,
        HasUserEvent,
        NeedsMaxfrSlot,
        HasTailCallEvent,
        OisuKinds,
        MaybeRequireTailRecursion,
        MaybeObsoleteInFavourOf,
        RegR_HeadVars,
        MaybeArgPassInfo,
        MaybeSpecialReturn,
        InitialLiveness,
        StackSlots,
        MaybeCallTableTip,
        MaybeTableIOInfo,
        MaybeTableAttrs,
        MaybeDeepProfProcInfo,
        MaybeUntupleInfo,
        MaybeInputSpecProc,
        MaybeArgSizes,
        MaybeTermInfo,
        Term2Info,
        MaybeExceptionInfo,
        MaybeTrailingInfo,
        MaybeMMTablingInfo,
        SharingReuseInfo).

proc_create(HeadVars, Goal, VarTable, RttiVarMaps,
        InstVarSet, DeclaredModes, Modes, MaybeArgLives,
        MaybeDeclaredDetism, Detism, EvalMethod,
        MainContext, ItemNumber, CanProcess, MaybeHeadModesConstr, DetismDecl,
        CseNopullContexts, MaybeUntupleInfo, MaybeInputSpecProc, VarNameRemap,
        StateVarWarnings, DeletedCallees,
        IsAddressTaken, HasForeignProcExports, HasParallelConj, HasUserEvent,
        HasTailCallEvent, OisuKinds, MaybeRequireTailRecursion,
        RegR_HeadVars, MaybeArgPassInfo, MaybeSpecialReturn, InitialLiveness,
        StackSlots, NeedsMaxfrSlot, MaybeCallTableTip, MaybeTableIOInfo,
        MaybeTableAttrs, MaybeObsoleteInFavourOf, MaybeDeepProfProcInfo,
        MaybeArgSizes, MaybeTermInfo, Term2Info, MaybeExceptionInfo,
        MaybeTrailingInfo, MaybeMMTablingInfo, SharingReuseInfo, ProcInfo) :-
    ProcSubInfo = proc_sub_info(
        MainContext,
        ItemNumber,
        MaybeHeadModesConstr,
        CseNopullContexts,
        VarNameRemap,
        StateVarWarnings,
        DeletedCallees,
        CanProcess,
        DetismDecl,
        IsAddressTaken,
        HasForeignProcExports,
        HasParallelConj,
        HasUserEvent,
        NeedsMaxfrSlot,
        HasTailCallEvent,
        OisuKinds,
        MaybeRequireTailRecursion,
        MaybeObsoleteInFavourOf,
        RegR_HeadVars,
        MaybeArgPassInfo,
        MaybeSpecialReturn,
        InitialLiveness,
        StackSlots,
        MaybeCallTableTip,
        MaybeTableIOInfo,
        MaybeTableAttrs,
        MaybeDeepProfProcInfo,
        MaybeUntupleInfo,
        MaybeInputSpecProc,
        MaybeArgSizes,
        MaybeTermInfo,
        Term2Info,
        MaybeExceptionInfo,
        MaybeTrailingInfo,
        MaybeMMTablingInfo,
        SharingReuseInfo),
    ProcInfo = proc_info(
        HeadVars,
        Goal,
        VarTable,
        RttiVarMaps,
        InstVarSet,
        DeclaredModes,
        Modes,
        MaybeArgLives,
        MaybeDeclaredDetism,
        Detism,
        EvalMethod,
        ProcSubInfo).

%---------------------------------------------------------------------------%
%
% Nontrivial getters and setters.
%

proc_info_set_body(VarTable, HeadVars, Goal, RttiVarMaps, !ProcInfo) :-
    !ProcInfo ^ proc_var_table := VarTable,
    !ProcInfo ^ proc_head_vars := HeadVars,
    !ProcInfo ^ proc_body := Goal,
    !ProcInfo ^ proc_rtti_varmaps := RttiVarMaps.

%---------------------------------------------------------------------------%
%
% The information specific to a procedure, as opposed to a predicate.
%
% The proc_info and proc_sub_info types constitute a single logical
% data structure split into two parts for efficiency purposes.
%
% The proc_info type contains the most frequently accessed and/or updated
% pieces of information about the procedure. Everything else is in the
% proc_sub_info type. This arrangement minimizes the amount of memory that
% needs to be allocated, and filled in, when a field is updated.
%

:- type proc_info
    --->    proc_info(
                % The Boehm collector allocates blocks whose sizes are
                % multiples (and usually powers) of 2. Ideally, we would want
                % the number of fields of proc_info to match one of the Boehm
                % block sizes, but as of 2017 march 15, this seemed to be the
                % optimal arrangement (zs).

/*  1 */        proc_head_vars                  :: list(prog_var),
/*  2 */        proc_body                       :: hlds_goal,

/*  3 */        proc_var_table                  :: var_table,

                % Information about type_infos and typeclass_infos.
/*  4 */        proc_rtti_varmaps               :: rtti_varmaps,

/*  5 */        proc_inst_varset                :: inst_varset,

                % The declared modes of arguments.
/*  6 */        proc_maybe_decl_head_modes      :: maybe(list(mer_mode)),

/*  7 */        proc_actual_head_modes          :: list(mer_mode),

                % Liveness (in the mode analysis sense) of the arguments
                % in the caller; says whether each argument may be used
                % after the call.
/*  8 */        proc_headvar_caller_liveness    :: maybe(list(is_live)),

                % The _declared_ determinism of the procedure, or `no'
                % if there was no detism declaration.
/*  9 */        proc_declared_detism            :: maybe(determinism),
/* 10 */        proc_inferred_detism            :: determinism,

                % How should the proc be evaluated.
/* 11 */        proc_eval_method                :: eval_method,

/* 12 */        proc_sub_info                   :: proc_sub_info
            ).

:- type proc_sub_info
    --->    proc_sub_info(
                % The context of the `:- mode' decl, or the context of the
                % first clause if there was no mode declaration.
                psi_proc_context                :: prog_context,

                % The item number of the mode declaration, if there was one.
                psi_item_number                 :: item_seq_num,

                % XXX The mode of the procedure in the ROBDD based
                % constraint system. Whether it represents the declared
                % or the actual mode is unclear, but since that constraint
                % system is obsolete, this does not much matter :-(
                psi_maybe_head_modes_constr     :: maybe(mode_constraint),

                % A list of all the contexts at which cse_detection.m
                % declined to pull out a common deconstruction out of
                % a branched control structure due to concerns about
                % uniqueness in the inst of the affected variable.
                % Determinism analysis wants this information so that
                % it knows whether to mention this fact to the user
                % as a possible cause of a determinism error.
                % See Mantis bug #496.
                psi_cse_nopull_contexts         :: list(prog_context),

                % Remaps the compiler-created variables named HeadVar__N
                % to the user-given variable names that actually occupied the
                % corresponding argument slots in the procedure's clauses.
                % Has an entry for a head variable only if *all* the clauses
                % consistently give that argument that name, if they give it
                % any name at all.
                % This renaming is applied only after semantic analysis,
                % although it is recorded earlier. The reason for this is
                % to make any error messages about the goals that unify the
                % original headvar (e.g. "X") with the introduced headvar
                % (e.g. "HeadVar__1") give the goal as HeadVar__1 = X,
                % not as X = X, since the latter would be very confusing.
                psi_proc_var_name_remap         :: map(prog_var, string),

                % Any warnings generated by the state variable transformation
                % that we should print only if we find a mode error that could
                % be caused by the problem being warned about.
                psi_statevar_warnings           :: list(warn_spec),

                % The set of procedures that the body of this procedure
                % *used* to call, but doesn't anymore. This can happen
                % For several reason. These reasons include the call being
                % - inside a trace goal scope whose compile-time condition
                %   turned out to be false,
                % - in the then part of an if-then-else whose condition
                %   never succeeds,
                % - in the else part of an if-then-else whose condition
                %   never fails.
                % We record the callees of the deleted calls so that
                % dead procedure analysis does not generate warnings
                % for these procedures, or the other procedures reachable
                % from them.
                psi_deleted_call_callees        :: set(pred_proc_id),

                %-----------------------------------------------------------%
                % Flags that record simple properties of the procedure.
                %-----------------------------------------------------------%

                % Set to cannot_process if we must not process this procedure
                % just yet. This is used to delay mode checking etc. for
                % complicated modes of unification predicates until the end
                % of the unique_modes pass.
                psi_can_process                 :: can_process,

                % Was the determinism declaration explicit, or was it implicit,
                % as for functions?
                psi_detism_decl                 :: detism_decl,

                % Is the address of this procedure taken? If yes, we will
                % need to use typeinfo liveness for them, so that deep_copy
                % and accurate gc have the RTTI they need for copying closures.
                %
                % Note that any non-local procedure must be considered
                % as having its address taken, since it is possible that
                % some other module may do so.
                psi_is_address_taken            :: is_address_taken,

                % Is the procedure mentioned in any foreign_export pragma,
                % regardless of what the current supported foreign languages
                % are?
                psi_has_any_foreign_exports     :: proc_foreign_exports,

                % Does this procedure contain parallel conjunction?
                % If yes, it should be run through the dependent parallel
                % conjunction transformation.
                %
                % This slot is set by the simplification pass.
                % Note that after some optimization passes, this flag
                % may be a conservative approximation.
                psi_proc_has_parallel_conj      :: has_parallel_conj,

                % Does this procedure contain a user event?
                %
                % This slot is set by the simplification pass.
                psi_proc_has_user_event         :: has_user_event,

                % True iff tracing is enabled, this is a procedure that lives
                % on the det stack, and the code of this procedure may create
                % a frame on the det stack. (Only in these circumstances do we
                % need to reserve a stack slot to hold the value of maxfr
                % at the call, for use in implementing retry.) This slot
                % is used only with the LLDS backend XXX. Its value is set
                % during the live_vars pass; it is invalid before then.
                psi_needs_maxfr_slot            :: needs_maxfr_slot,

                psi_proc_has_tail_rec_call      :: has_tail_rec_call,

                %-----------------------------------------------------------%
                % Information about pragmas.
                %-----------------------------------------------------------%

                % Is the procedure mentioned in any order-independent-state-
                % update pragmas? If yes, list the role of this procedure
                % for the each of the types in those pragmas.
                psi_oisu_kind_fors              :: list(oisu_pred_kind_for),

                % Has the user requested (via a require_tail_recursion
                % pragma) that we suppress or enable warnings about tail
                % recursion for this procedure?
                psi_maybe_require_tailrec   :: maybe(require_tail_recursion),

                % If this procedure is marked as obsolete, this will be a
                % "yes(_)" wrapped around a list of the predicate names that
                % the compiler should suggest as possible replacements.
                % (Note that the list of possible replacements may be empty.)
                % In the usual case where this predicate is NOT marked
                % as obsolete, this will be "no".
                psi_proc_obsolete_in_favour_of :: maybe(list(sym_name_arity)),

                %-----------------------------------------------------------%
                % Information needed by the LLDS code generator.
                %-----------------------------------------------------------%

                % The head variables which must be forced to use regular
                % registers by the calling convention, despite having type
                % float. This is only meaningful with float registers.
                psi_reg_r_headvars              :: set_of_progvar,

                % The calling convention of each argument: information computed
                % by arg_info.m (based on the modes etc.) and used by code
                % generation to determine how each argument should be passed.
                psi_maybe_arg_info              :: maybe(list(arg_info)),

                psi_maybe_special_return        :: maybe(special_proc_return),

                % The initial liveness, for code generation.
                psi_initial_liveness            :: codegen_liveness,

                % Allocation of variables to stack slots.
                psi_stack_slots                 :: stack_slots,

                %-----------------------------------------------------------%
                % Information needed for tabling.
                %-----------------------------------------------------------%

                % If the procedure's evaluation method is memo, loopcheck or
                % minimal, this slot identifies the variable that holds the tip
                % of the call table. Otherwise, this field will be set to `no'.
                %
                % Tabled procedures record, in the data structure identified
                % by this variable, that the call is active. When performing
                % a retry across such a procedure, we must reset the state
                % of the call; if we don't, the retried call will find the
                % active call and report an infinite loop error.
                %
                % Such resetting of course requires the debugger to know
                % whether the procedure has reached the call table tip yet.
                % Therefore when binding this variable, the code generator
                % of the relevant backend must record this fact in a place
                % accessible to the debugger, if debugging is enabled.
                psi_call_table_tip              :: maybe(prog_var),

                % If set, it means that procedure has been subject to the I/O
                % tabling transformation. The argument will contain all the
                % information we need to display I/O actions involving
                % this procedure.
                %
                % (If the procedure has been subject to other kinds of tabling
                % transformations, the corresponding information will be
                % recorded in a map in the module_info.)
                % XXX For now, the compiler fully supports only procedures
                % whose arguments are all either ints, floats or strings.
                % However, this is still sufficient for debugging most problems
                % in the tabling system.
                psi_maybe_table_io_info         :: maybe(proc_table_io_info),

                psi_table_attributes            :: maybe(table_attributes),

                %-----------------------------------------------------------%
                % Information needed for deep profiling.
                %-----------------------------------------------------------%

                psi_maybe_deep_prof_info      :: maybe(deep_profile_proc_info),

                %-----------------------------------------------------------%
                % Information from code optimizations.
                %-----------------------------------------------------------%

                % If set, it means this procedure was created from another
                % procedure by the untupling transformation. This slot records
                % which of the procedure's arguments were derived from which
                % arguments in the original procedure.
                %
                % This is effectively a record of the *procedure*'s origin.
                % (The pred_origin field records the *predicate*'s origin.)
                psi_maybe_untuple_info          :: maybe(untuple_proc_info),

                psi_maybe_input_spec            :: maybe_input_spec_proc,

                %-----------------------------------------------------------%
                % The results of program analyses.
                %-----------------------------------------------------------%

                % Information about the relative sizes of the input and output
                % args of the procedure. Set by termination analysis.
                psi_maybe_arg_size_infos        :: maybe(arg_size_info),

                % The termination properties of the procedure.
                % Set by termination analysis.
                psi_maybe_termination           :: maybe(termination_info),

                % Termination properties and argument size constraints for
                % the procedure. Set by termination2 analysis.
                psi_termination2                :: termination2_info,

                % The results of the analyses in exception_analysis.m,
                % trailing_analysis.m and tabling_analysis, if available.
                psi_exception_info              :: maybe(proc_exception_info),
                psi_trailing_info               :: maybe(proc_trailing_info),
                psi_mm_tabling_info             :: maybe(proc_mm_tabling_info),

                % Structure sharing and reuse information as obtained by
                % structure sharing/reuse analysis (CTGC).
                psi_sharing_reuse_info          :: sharing_reuse_info
        ).

proc_info_get_headvars(PI, X) :-
    X = PI ^ proc_head_vars.
proc_info_get_goal(PI, X) :-
    X = PI ^ proc_body.
proc_info_get_var_table(PI, X) :-
    X = PI ^ proc_var_table.
proc_info_get_rtti_varmaps(PI, X) :-
    X = PI ^ proc_rtti_varmaps.
proc_info_get_inst_varset(PI, X) :-
    X = PI ^ proc_inst_varset.
proc_info_get_maybe_declared_argmodes(PI, X) :-
    X = PI ^ proc_maybe_decl_head_modes.
proc_info_get_argmodes(PI, X) :-
    X = PI ^ proc_actual_head_modes.
proc_info_get_maybe_arglives(PI, X) :-
    X = PI ^ proc_headvar_caller_liveness.
proc_info_get_declared_determinism(PI, X) :-
    X = PI ^ proc_declared_detism.
proc_info_get_inferred_determinism(PI, X) :-
    X = PI ^ proc_inferred_detism.
proc_info_get_eval_method(PI, X) :-
    X = PI ^ proc_eval_method.

proc_info_get_context(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_context.
proc_info_get_item_number(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_item_number.
proc_info_get_maybe_head_modes_constr(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_head_modes_constr.
proc_info_get_cse_nopull_contexts(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_cse_nopull_contexts.
proc_info_get_var_name_remap(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_var_name_remap.
proc_info_get_statevar_warnings(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_statevar_warnings.
proc_info_get_deleted_call_callees(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_deleted_call_callees.
proc_info_get_can_process(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_can_process.
proc_info_get_detism_decl(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_detism_decl.
proc_info_get_is_address_taken(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_is_address_taken.
proc_info_get_has_any_foreign_exports(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_has_any_foreign_exports.
proc_info_get_has_parallel_conj(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_has_parallel_conj.
proc_info_get_has_user_event(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_has_user_event.
proc_info_get_needs_maxfr_slot(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_needs_maxfr_slot.
proc_info_get_has_tail_rec_call(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_has_tail_rec_call.
proc_info_get_oisu_kind_fors(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_oisu_kind_fors.
proc_info_get_maybe_require_tailrec_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_require_tailrec.
proc_info_get_obsolete_in_favour_of(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_proc_obsolete_in_favour_of.
proc_info_get_reg_r_headvars(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_reg_r_headvars.
proc_info_get_maybe_arg_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_arg_info.
proc_info_get_maybe_special_return(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_special_return.
proc_info_get_initial_liveness(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_initial_liveness.
proc_info_get_stack_slots(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_stack_slots.
proc_info_get_call_table_tip(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_call_table_tip.
proc_info_get_maybe_proc_table_io_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_table_io_info.
proc_info_get_table_attributes(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_table_attributes.
proc_info_get_maybe_deep_profile_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_deep_prof_info.
proc_info_get_maybe_untuple_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_untuple_info.
proc_info_get_maybe_input_spec(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_input_spec.
proc_info_get_maybe_arg_size_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_arg_size_infos.
proc_info_get_maybe_termination_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_maybe_termination.
proc_info_get_termination2_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_termination2.
proc_info_get_exception_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_exception_info.
proc_info_get_trailing_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_trailing_info.
proc_info_get_mm_tabling_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_mm_tabling_info.
proc_info_get_sharing_reuse_info(PI, X) :-
    X = PI ^ proc_sub_info ^ psi_sharing_reuse_info.

proc_info_set_headvars(X, !PI) :-
    !PI ^ proc_head_vars := X.
proc_info_set_goal(X, !PI) :-
    !PI ^ proc_body := X.
proc_info_set_var_table(X, !PI) :-
    !PI ^ proc_var_table := X.
proc_info_set_rtti_varmaps(X, !PI) :-
    !PI ^ proc_rtti_varmaps := X.
proc_info_set_inst_varset(X, !PI) :-
    !PI ^ proc_inst_varset := X.
proc_info_set_maybe_declared_argmodes(X, !PI) :-
    !PI ^ proc_maybe_decl_head_modes := X.
proc_info_set_argmodes(X, !PI) :-
    !PI ^ proc_actual_head_modes := X.
proc_info_set_maybe_arglives(X, !PI) :-
    !PI ^ proc_headvar_caller_liveness := X.
proc_info_set_inferred_determinism(X, !PI) :-
    !PI ^ proc_inferred_detism := X.
proc_info_set_eval_method(X, !PI) :-
    !PI ^ proc_eval_method := X.

proc_info_set_head_modes_constraint(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_head_modes_constr := yes(X).
proc_info_set_cse_nopull_contexts(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_cse_nopull_contexts := X.
proc_info_set_var_name_remap(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_var_name_remap := X.
proc_info_set_statevar_warnings(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_statevar_warnings := X.
proc_info_set_deleted_call_callees(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_deleted_call_callees := X.
proc_info_set_can_process(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_can_process := X.
proc_info_set_detism_decl(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_detism_decl := X.
proc_info_set_address_taken(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_is_address_taken := X.
proc_info_set_has_any_foreign_exports(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_has_any_foreign_exports := X.
proc_info_set_has_parallel_conj(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_has_parallel_conj := X.
proc_info_set_has_user_event(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_has_user_event := X.
proc_info_set_needs_maxfr_slot(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_needs_maxfr_slot := X.
proc_info_set_has_tail_rec_call(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_has_tail_rec_call := X.
proc_info_set_oisu_kind_fors(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_oisu_kind_fors := X.
proc_info_set_require_tailrec_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_require_tailrec := yes(X).
proc_info_set_obsolete_in_favour_of(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_proc_obsolete_in_favour_of := X.
proc_info_set_reg_r_headvars(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_reg_r_headvars := X.
proc_info_set_arg_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_arg_info := yes(X).
proc_info_set_maybe_special_return(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_special_return := X.
proc_info_set_initial_liveness(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_initial_liveness := X.
proc_info_set_stack_slots(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_stack_slots := X.
proc_info_set_call_table_tip(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_call_table_tip := X.
proc_info_set_maybe_proc_table_io_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_table_io_info := X.
proc_info_set_table_attributes(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_table_attributes := X.
proc_info_set_maybe_deep_profile_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_deep_prof_info := X.
proc_info_set_maybe_untuple_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_untuple_info := X.
proc_info_set_maybe_input_spec(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_input_spec := X.
proc_info_set_maybe_arg_size_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_arg_size_infos := X.
proc_info_set_maybe_termination_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_maybe_termination := X.
proc_info_set_termination2_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_termination2 := X.
proc_info_set_exception_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_exception_info := X.
proc_info_set_trailing_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_trailing_info := X.
proc_info_set_mm_tabling_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_mm_tabling_info := X.
proc_info_set_sharing_reuse_info(X, !PI) :-
    !PI ^ proc_sub_info ^ psi_sharing_reuse_info := X.

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_proc.
%---------------------------------------------------------------------------%
