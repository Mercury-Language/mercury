%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: code_info.m.
% Main authors: conway, zs.
%
% This file defines the code_info type and various operations on it.
% The code_info structure is the persistent part of the 'state'
% of the code generator. The other part of the code generator state,
% the location-dependent part, is in code_loc_dep.m.
%
% This file is organized into three submodules:
%
%   - the code_info structure and its access predicates
%   - simple wrappers around access predicates
%   - managing stack slots
%
%---------------------------------------------------------------------------%

:- module ll_backend.code_info.
:- interface.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.global_data.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module ll_backend.trace_gen.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module counter.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module set_tree234.
:- import_module term.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.proc_label.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module ll_backend.code_util.
:- import_module parse_tree.prog_type.

:- import_module cord.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for the code_info type and its access predicates.
    %
    % This submodule has the following components:
    %
    %   declarations for exported access predicates
    %   declarations for non-exported access predicates
    %   the definition of the type and the init predicate
    %   the definition of the get access predicates
    %   the definition of the set access predicates
    %
    % Please keep the order of mention of the various fields
    % consistent in each of these five components.

:- interface.

:- type code_info.

    % Create a new code_info structure. Also return info about the non-fixed
    % stack slots used for tracing purposes.
    %
:- pred code_info_init(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, bool::in, static_cell_info::in,
    const_struct_map::in, maybe(containing_goal_map)::in,
    list(string)::in, int::in, trace_slot_info::out, code_info::out) is det.

:- pred get_module_info(code_info::in, module_info::out) is det.
:- pred get_globals(code_info::in, globals::out) is det.
:- pred get_exprn_opts(code_info::in, exprn_opts::out) is det.
:- pred get_pred_id(code_info::in, pred_id::out) is det.
:- pred get_proc_id(code_info::in, proc_id::out) is det.
:- pred get_pred_info(code_info::in, pred_info::out) is det.
:- pred get_proc_info(code_info::in, proc_info::out) is det.
:- pred get_proc_label(code_info::in, proc_label::out) is det.
:- pred get_varset(code_info::in, prog_varset::out) is det.
:- pred get_vartypes(code_info::in, vartypes::out) is det.
:- pred get_var_slot_count(code_info::in, int::out) is det.
:- pred get_maybe_trace_info(code_info::in, maybe(trace_info)::out) is det.
:- pred get_opt_no_return_calls(code_info::in, bool::out) is det.
:- pred get_emit_trail_ops(code_info::in, add_trail_ops::out) is det.
:- pred get_opt_trail_ops(code_info::in, bool::out) is det.
:- pred get_emit_region_ops(code_info::in, add_region_ops::out) is det.
:- pred get_opt_region_ops(code_info::in, bool::out) is det.
:- pred get_auto_comments(code_info::in, bool::out) is det.
:- pred get_lcmc_null(code_info::in, bool::out) is det.
:- pred get_maybe_containing_goal_map(code_info::in,
    maybe(containing_goal_map)::out) is det.
:- pred get_const_struct_map(code_info::in, const_struct_map::out) is det.

:- pred get_label_counter(code_info::in, counter::out) is det.
:- pred get_succip_used(code_info::in, bool::out) is det.
:- pred get_layout_info(code_info::in, proc_label_layout_info::out) is det.
:- pred get_proc_trace_events(code_info::in, bool::out) is det.
:- pred get_max_regs_in_use_at_trace(code_info::in, int::out, int::out) is det.
:- pred get_created_temp_frame(code_info::in, bool::out) is det.
:- pred get_max_temp_slot_count(code_info::in, int::out) is det.
:- pred get_temp_content_map(code_info::in, map(lval, slot_contents)::out)
    is det.
:- pred get_persistent_temps(code_info::in, set(lval)::out) is det.
% get_closure_seq_counter is not exported.
:- pred get_closure_layouts(code_info::in, list(closure_proc_id_data)::out)
    is det.
:- pred get_static_cell_info(code_info::in, static_cell_info::out) is det.
:- pred get_alloc_sites(code_info::in, set_tree234(alloc_site_info)::out)
    is det.
:- pred get_used_env_vars(code_info::in, set(string)::out) is det.
:- pred get_out_of_line_code(code_info::in, llds_code::out) is det.

% set_maybe_trace_info is not exported.

% set_label_counter is not exported.
% set_succip_used is not exported.
% set_layout_info is not exported.
:- pred set_proc_trace_events(bool::in,
    code_info::in, code_info::out) is det.
:- pred set_max_regs_in_use_at_trace(int::in, int::in,
    code_info::in, code_info::out) is det.
:- pred set_max_temp_slot_count(int::in,
    code_info::in, code_info::out) is det.
:- pred set_temp_content_map(map(lval, slot_contents)::in,
    code_info::in, code_info::out) is det.
:- pred set_persistent_temps(set(lval)::in,
    code_info::in, code_info::out) is det.
% set_closure_seq_counter is not exported.
% set_closure_layouts is not exported.
:- pred set_created_temp_frame(bool::in,
    code_info::in, code_info::out) is det.
:- pred set_static_cell_info(static_cell_info::in,
    code_info::in, code_info::out) is det.
:- pred set_alloc_sites(set_tree234(alloc_site_info)::in,
    code_info::in, code_info::out) is det.
:- pred set_used_env_vars(set(string)::in,
    code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred get_closure_seq_counter(code_info::in, counter::out) is det.

:- pred set_maybe_trace_info(maybe(trace_info)::in,
    code_info::in, code_info::out) is det.

:- pred set_label_counter(counter::in,
    code_info::in, code_info::out) is det.
:- pred set_succip_used(bool::in,
    code_info::in, code_info::out) is det.
:- pred set_layout_info(proc_label_layout_info::in,
    code_info::in, code_info::out) is det.
:- pred set_closure_layouts(list(closure_proc_id_data)::in,
    code_info::in, code_info::out) is det.
:- pred set_closure_seq_counter(counter::in,
    code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

    % The code_info structure has two groups of fields.
    %
    % Some fields are static; they are set when the code_info structure
    % is initialized, and never changed afterwards.
    %
    % The other fields record persistent information that does not depend
    % on a code location. Updates to these fields must remain effective
    % even when the code generator resets its location-dependent state.

:- type code_info
    --->    code_info(
                code_info_static        :: code_info_static,
                code_info_persistent    :: code_info_persistent
            ).

:- type code_info_static
    --->    code_info_static(
                % The module_info structure - you just never know
                % when you might need it.
                cis_module_info         :: module_info,

                % For the code generation options.
                cis_globals             :: globals,
                cis_exprn_opts          :: exprn_opts,

                % The id of the current predicate.
                cis_pred_id             :: pred_id,

                % The id of the current procedure.
                cis_proc_id             :: proc_id,

                % The pred_info for the predicate containing this procedure.
                cis_pred_info           :: pred_info,

                % The proc_info for this procedure.
                cis_proc_info           :: proc_info,

                % The proc_label for this procedure.
                cis_proc_label          :: proc_label,

                % The variables in this procedure.
                cis_varset              :: prog_varset,
                cis_vartypes            :: vartypes,

                % The number of stack slots allocated. for storing variables.
                % (Some extra stack slots are used for saving and restoring
                % registers.)
                cis_var_slot_count      :: int,

                % Information about which stack slots the call sequence number
                % and depth are stored in, provided tracing is switched on.
                cis_maybe_trace_info    :: maybe(trace_info),

                % Should we optimize calls that cannot return?
                cis_opt_no_return_calls :: bool,

                % Should we emit trail operations?
                cis_emit_trail_ops      :: add_trail_ops,

                % Should we try to avoid generating trail operations?
                cis_opt_trail_ops       :: bool,

                % Should we emit region operations?
                cis_emit_region_ops     :: add_region_ops,

                % Should we try to avoid generating region operations?
                cis_opt_region_ops      :: bool,

                % The setting of --auto-comments.
                cis_auto_comments       :: bool,

                % The setting of --optimize-constructor-last-call-null.
                cis_lcmc_null           :: bool,

                cis_containing_goal_map :: maybe(containing_goal_map),

                % Maps the number of an entry in the module's const_struct_db
                % to its rval.
                cis_const_struct_map    :: const_struct_map
            ).

:- type code_info_persistent
    --->    code_info_persistent(
                % Counter for the local labels used by this procedure.
                cip_label_num_src           :: counter,

                % Do we need to store succip?
                cip_store_succip            :: bool,

                % Information on which values are live and where at which
                % labels, for tracing and/or accurate gc.
                cip_label_layout_info       :: proc_label_layout_info,

                % Did the procedure have any trace events?
                cip_proc_trace_events       :: bool,

                % At each call to MR_trace, we compute the highest rN and fN
                % registers that contain useful values. These slot contain the
                % maximum of these highest values. Therefore at all calls to
                % MR_trace in the procedure, we need only save the registers
                % whose numbers are equal to or smaller than this field.
                % This slot contains -1 if tracing is not enabled.
                cip_max_reg_r_used          :: int,
                cip_max_reg_f_used          :: int,

                % The maximum number of extra temporary stackslots that
                % have been used during the procedure.
                cip_stackslot_max           :: int,

                % The temporary locations that have ever been used on the
                % stack, and what they contain. Once we have used a stack slot
                % to store e.g. a ticket, we never reuse that slot to hold
                % something else, e.g. a saved hp. This policy prevents us
                % from making such conflicting choices in parallel branches,
                % which would make it impossible to describe to gc what the
                % slot contains after the end of the branched control
                % structure.
                cip_temp_content_map        :: map(lval, slot_contents),

                % Stack slot locations that should not be released even when
                % the code generator resets its location-dependent state.
                cip_persistent_temps        :: set(lval),

                cip_closure_layout_seq      :: counter,

                % Closure layout structures generated by this procedure.
                cip_closure_layouts         :: list(closure_proc_id_data),

                % True iff the procedure has created one or more temporary
                % nondet frames.
                cip_created_temp_frame      :: bool,

                cip_static_cell_info        :: static_cell_info,

                cip_alloc_sites             :: set_tree234(alloc_site_info),

                cip_used_env_vars           :: set(string),

                % A counter and table for allocating and maintaining slots
                % where string IDs will be placed at runtime for threadscope
                % profiling. The actual string IDs are allocated at runtime
                % and their IDs are placed in an array slot which can be
                % referred to statically.
                cip_ts_string_table_size    :: int,
                cip_ts_rev_string_table     :: list(string),

                % Code that is part of this procedure, but that can be placed
                % after the procedure without a cache penalty. For example,
                % code that is spawned off by loop control is placed here.
                cip_out_of_line_code        :: llds_code
            ).

%---------------------------------------------------------------------------%

code_info_init(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo,
        SaveSuccip, StaticCellInfo, ConstStructMap, MaybeContainingGoalMap,
        TSRevStringTable, TSStringTableSize, TraceSlotInfo, CodeInfo) :-
    % argument ModuleInfo
    module_info_get_globals(ModuleInfo, Globals),
    ExprnOpts = init_exprn_opts(Globals),
    % argument PredId
    % argument ProcId
    % argument PredInfo
    % argument ProcInfo
    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_stack_slots(ProcInfo, StackSlots),
    max_var_slot(StackSlots, VarSlotMax),
    trace_reserved_slots(ModuleInfo, PredInfo, ProcInfo, Globals,
        FixedSlots, _),
    int.max(VarSlotMax, FixedSlots, SlotMax),
    MaybeTraceInfo = no,
    globals.lookup_bool_option(Globals, opt_no_return_calls,
        OptNoReturnCalls),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    globals.lookup_bool_option(Globals, disable_trail_ops, DisableTrailOps),
    ( if
        UseTrail = yes,
        DisableTrailOps = no
    then
        EmitTrailOps = add_trail_ops
    else
        EmitTrailOps = do_not_add_trail_ops
    ),
    globals.lookup_bool_option(Globals, optimize_trail_usage, OptTrailOps),
    globals.lookup_bool_option(Globals, region_analysis, UseRegions),
    (
        UseRegions = yes,
        EmitRegionOps = add_region_ops
    ;
        UseRegions = no,
        EmitRegionOps = do_not_add_region_ops
    ),
    globals.lookup_bool_option(Globals, optimize_region_ops, OptRegionOps),
    globals.lookup_bool_option(Globals, auto_comments, AutoComments),
    globals.lookup_bool_option(Globals, optimize_constructor_last_call_null,
        LCMCNull),
    % argument MaybeContainingGoalMap
    % argument ConstStructMap

    CodeInfoStatic0 = code_info_static(
        ModuleInfo,
        Globals,
        ExprnOpts,
        PredId,
        ProcId,
        PredInfo,
        ProcInfo,
        ProcLabel,
        VarSet,
        VarTypes,
        SlotMax,
        MaybeTraceInfo,
        OptNoReturnCalls,
        EmitTrailOps,
        OptTrailOps,
        EmitRegionOps,
        OptRegionOps,
        AutoComments,
        LCMCNull,
        MaybeContainingGoalMap,
        ConstStructMap
    ),

    LabelNumCounter0 = counter.init(1),
    % argument SaveSuccip
    globals.get_trace_level(Globals, TraceLevel),
    map.init(LayoutMap),
    ProcTraceEvents = no,
    MaxRegRUsed = -1,
    MaxRegFUsed = -1,
    MaxTempSlotCount = 0,
    map.init(TempContentMap),
    set.init(PersistentTemps),
    ClosureLayoutSeqNumCounter0 = counter.init(1),
    ClosureLayouts = [],
    CreatedTempFrame = no,
    % argument StaticCellInfo
    AllocSitesMap0 = set_tree234.init,
    set.init(UsedEnvVars),
    OutOfLineCode = cord.init,

    CodeInfoPersistent0 = code_info_persistent(
        LabelNumCounter0,
        SaveSuccip,
        LayoutMap,
        ProcTraceEvents,
        MaxRegRUsed,
        MaxRegFUsed,
        MaxTempSlotCount,
        TempContentMap,
        PersistentTemps,
        ClosureLayoutSeqNumCounter0,
        ClosureLayouts,
        CreatedTempFrame,
        StaticCellInfo,
        AllocSitesMap0,
        UsedEnvVars,
        TSStringTableSize,
        TSRevStringTable,
        OutOfLineCode
    ),
    CodeInfo0 = code_info(CodeInfoStatic0, CodeInfoPersistent0),
    init_maybe_trace_info(TraceLevel, Globals, ModuleInfo,
        PredInfo, ProcInfo, TraceSlotInfo, CodeInfo0, CodeInfo).

:- func init_exprn_opts(globals) = exprn_opts.

init_exprn_opts(Globals) = ExprnOpts :-
    globals.lookup_bool_option(Globals, gcc_non_local_gotos, OptNLG),
    (
        OptNLG = yes,
        NLG = have_non_local_gotos
    ;
        OptNLG = no,
        NLG = do_not_have_non_local_gotos
    ),
    globals.lookup_bool_option(Globals, asm_labels, OptASM),
    (
        OptASM = yes,
        ASM = have_asm_labels
    ;
        OptASM = no,
        ASM = do_not_have_asm_labels
    ),
    globals.lookup_bool_option(Globals, static_ground_cells, OptSGCell),
    (
        OptSGCell = yes,
        SGCell = have_static_ground_cells
    ;
        OptSGCell = no,
        SGCell = do_not_have_static_ground_cells
    ),
    globals.lookup_bool_option(Globals, unboxed_float, OptUBF),
    (
        OptUBF = yes,
        UBF = have_unboxed_floats
    ;
        OptUBF = no,
        UBF = do_not_have_unboxed_floats
    ),
    globals.lookup_bool_option(Globals, use_float_registers, OptFloatRegs),
    (
        OptFloatRegs = yes,
        UseFloatRegs = use_float_registers
    ;
        OptFloatRegs = no,
        UseFloatRegs = do_not_use_float_registers
    ),
    double_width_floats_on_det_stack(Globals, FloatDwords),
    (
        FloatDwords = yes,
        DetStackFloatWidth = double_width
    ;
        FloatDwords = no,
        DetStackFloatWidth = single_width
    ),
    globals.lookup_bool_option(Globals, unboxed_int64s, OptUBI64s),
    (
        OptUBI64s = yes,
        UBI64s = have_unboxed_int64s
    ;
        OptUBI64s = no,
        UBI64s = do_not_have_unboxed_int64s
    ),
    globals.lookup_bool_option(Globals, static_ground_floats, OptSGFloat),
    (
        OptSGFloat = yes,
        SGFloat = have_static_ground_floats
    ;
        OptSGFloat = no,
        SGFloat = do_not_have_static_ground_floats
    ),
    globals.lookup_bool_option(Globals, static_ground_int64s, OptSGInt64s),
    (
        OptSGInt64s = yes,
        SGInt64s = have_static_ground_int64s
    ;
        OptSGInt64s = no,
        SGInt64s = do_not_have_static_ground_int64s
    ),
    globals.lookup_bool_option(Globals, static_code_addresses,
        OptStaticCodeAddr),
    (
        OptStaticCodeAddr = yes,
        StaticCodeAddrs = have_static_code_addresses
    ;
        OptStaticCodeAddr = no,
        StaticCodeAddrs = do_not_have_static_code_addresses
    ),
    ExprnOpts = exprn_opts(NLG, ASM, UBF, UseFloatRegs, DetStackFloatWidth,
        UBI64s, SGCell, SGFloat, SGInt64s, StaticCodeAddrs).

:- pred max_var_slot(stack_slots::in, int::out) is det.

max_var_slot(StackSlots, SlotCount) :-
    map.values(StackSlots, StackSlotList),
    max_var_slot_2(StackSlotList, 0, SlotCount).

:- pred max_var_slot_2(list(stack_slot)::in, int::in, int::out) is det.

max_var_slot_2([], !Max).
max_var_slot_2([Slot | Slots], !Max) :-
    (
        Slot = det_slot(N, Width)
    ;
        Slot = parent_det_slot(N, Width)
    ;
        Slot = nondet_slot(N),
        Width = single_width
    ),
    (
        Width = single_width,
        int.max(N, !Max)
    ;
        Width = double_width,
        int.max(N + 1, !Max)
    ),
    max_var_slot_2(Slots, !Max).

:- pred init_maybe_trace_info(trace_level::in, globals::in,
    module_info::in, pred_info::in, proc_info::in, trace_slot_info::out,
    code_info::in, code_info::out) is det.

init_maybe_trace_info(TraceLevel, Globals, ModuleInfo, PredInfo,
        ProcInfo, TraceSlotInfo, !CI) :-
    TraceLevelIsNone =
        eff_trace_level_is_none(ModuleInfo, PredInfo, ProcInfo, TraceLevel),
    (
        TraceLevelIsNone = no,
        proc_info_get_has_tail_rec_call(ProcInfo, HasTailRecCall),
        HasTailRecCall =
            has_tail_rec_call(HasSelfTailRecCall, _HasMutualTailRecCall),
        (
            HasSelfTailRecCall = has_self_tail_rec_call,
            get_next_label(TailRecLabel, !CI),
            MaybeTailRecLabel = yes(TailRecLabel)
        ;
            HasSelfTailRecCall = has_no_self_tail_rec_call,
            MaybeTailRecLabel = no
        ),
        trace_setup(ModuleInfo, PredInfo, ProcInfo, Globals, MaybeTailRecLabel,
            TraceSlotInfo, TraceInfo, !CI),
        set_maybe_trace_info(yes(TraceInfo), !CI)
    ;
        TraceLevelIsNone = yes,
        TraceSlotInfo = trace_slot_info(no, no, no, no, no, no)
    ).

%---------------------------------------------------------------------------%

get_module_info(CI, X) :-
    X = CI ^ code_info_static ^ cis_module_info.
get_globals(CI, X) :-
    X = CI ^ code_info_static ^ cis_globals.
get_exprn_opts(CI, X) :-
    X = CI ^ code_info_static ^ cis_exprn_opts.
get_pred_id(CI, X) :-
    X = CI ^ code_info_static ^ cis_pred_id.
get_proc_id(CI, X) :-
    X = CI ^ code_info_static ^ cis_proc_id.
get_pred_info(CI, X) :-
    X = CI ^ code_info_static ^ cis_pred_info.
get_proc_info(CI, X) :-
    X = CI ^ code_info_static ^ cis_proc_info.
get_proc_label(CI, X) :-
    X = CI ^ code_info_static ^ cis_proc_label.
get_varset(CI, X) :-
    X = CI ^ code_info_static ^ cis_varset.
get_vartypes(CI, X) :-
    X = CI ^ code_info_static ^ cis_vartypes.
get_var_slot_count(CI, X) :-
    X = CI ^ code_info_static ^ cis_var_slot_count.
get_maybe_trace_info(CI, X) :-
    X = CI ^ code_info_static ^ cis_maybe_trace_info.
get_opt_no_return_calls(CI, X) :-
    X = CI ^ code_info_static ^ cis_opt_no_return_calls.
get_emit_trail_ops(CI, X) :-
    X = CI ^ code_info_static ^ cis_emit_trail_ops.
get_opt_trail_ops(CI, X) :-
    X = CI ^ code_info_static ^ cis_opt_trail_ops.
get_emit_region_ops(CI, X) :-
    X = CI ^ code_info_static ^ cis_emit_region_ops.
get_opt_region_ops(CI, X) :-
    X = CI ^ code_info_static ^ cis_opt_region_ops.
get_auto_comments(CI, X) :-
    X = CI ^ code_info_static ^ cis_auto_comments.
get_lcmc_null(CI, X) :-
    X = CI ^ code_info_static ^ cis_lcmc_null.
get_maybe_containing_goal_map(CI, X) :-
    X = CI ^ code_info_static ^ cis_containing_goal_map.
get_const_struct_map(CI, X) :-
    X = CI ^ code_info_static ^ cis_const_struct_map.

get_label_counter(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_label_num_src.
get_succip_used(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_store_succip.
get_layout_info(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_label_layout_info.
get_proc_trace_events(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_proc_trace_events.
get_max_regs_in_use_at_trace(CI, MaxRegR, MaxRegF) :-
    MaxRegR = CI ^ code_info_persistent ^ cip_max_reg_r_used,
    MaxRegF = CI ^ code_info_persistent ^ cip_max_reg_f_used.
get_created_temp_frame(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_created_temp_frame.
get_max_temp_slot_count(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_stackslot_max.
get_temp_content_map(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_temp_content_map.
get_persistent_temps(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_persistent_temps.
get_closure_seq_counter(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_closure_layout_seq.
get_closure_layouts(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_closure_layouts.
get_static_cell_info(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_static_cell_info.
get_alloc_sites(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_alloc_sites.
get_used_env_vars(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_used_env_vars.
get_out_of_line_code(CI, X) :-
    X = CI ^ code_info_persistent ^ cip_out_of_line_code.

set_maybe_trace_info(X, !CI) :-
    !CI ^ code_info_static ^ cis_maybe_trace_info := X.

set_label_counter(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_label_num_src := X.
set_succip_used(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_store_succip := X.
set_layout_info(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_label_layout_info := X.
set_proc_trace_events(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_proc_trace_events := X.
set_max_regs_in_use_at_trace(MR, MF, !CI) :-
    !CI ^ code_info_persistent ^ cip_max_reg_r_used := MR,
    !CI ^ code_info_persistent ^ cip_max_reg_f_used := MF.
set_max_temp_slot_count(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_stackslot_max := X.
set_temp_content_map(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_temp_content_map := X.
set_persistent_temps(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_persistent_temps := X.
set_closure_seq_counter(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_closure_layout_seq := X.
set_closure_layouts(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_closure_layouts := X.
set_created_temp_frame(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_created_temp_frame := X.
set_static_cell_info(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_static_cell_info := X.
set_alloc_sites(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_alloc_sites := X.
set_used_env_vars(X, !CI) :-
    !CI ^ code_info_persistent ^ cip_used_env_vars := X.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for simple wrappers around access predicates.

:- interface.

    % Get the hlds mapping from variables to stack slots.
    %
:- pred get_stack_slots(code_info::in, stack_slots::out) is det.

    % Find out whether the body of the current procedure should use
    % typeinfo liveness.
    %
:- func body_typeinfo_liveness(code_info) = bool.

    % Find out the type of the given variable.
    %
:- func variable_type(code_info, prog_var) = mer_type.

:- func variable_is_of_dummy_type(code_info, prog_var) = is_dummy_type.

    % Compute the principal type constructor of the given type, and return
    % the definition of this type constructor, if it has one (some type
    % constructors are built in, and some are hidden behind abstraction
    % barriers).
    %
:- pred search_type_defn(code_info::in, mer_type::in, hlds_type_defn::out) is
    semidet.

    % Compute the principal type constructor of the given type, and return
    % the definition of this type constructor. Abort if it doesn't have a
    % definition (e.g. because it is a builtin).
    %
:- func lookup_type_defn(code_info, mer_type) = hlds_type_defn.

:- func lookup_cheaper_tag_test(code_info, mer_type) = maybe_cheaper_tag_test.

:- func filter_region_vars(code_info, set_of_progvar) = set_of_progvar.

    % Get the code model of the current procedure.
    %
:- func get_proc_model(code_info) = code_model.

    % Get the list of the head variables of the current procedure.
    %
:- func get_headvars(code_info) = list(prog_var).

    % Get the call argument information for the current procedure
    %
:- func get_arginfo(code_info) = list(arg_info).

    % Get the call argument info for a given mode of a given predicate
    %
:- func get_pred_proc_arginfo(code_info, pred_id, proc_id) = list(arg_info).

:- func variable_name(code_info, prog_var) = string.

    % Create a code address which holds the address of the specified procedure.
    % The fourth argument should be `no' if the caller wants the
    % returned address to be valid from everywhere in the program.
    % If being valid from within the current procedure is enough,
    % this argument should be `yes' wrapped around the value of the
    % --procs-per-c-function option and the current procedure id.
    % Using an address that is only valid from within the current
    % procedure may make jumps more efficient.
    %
    % If the procs_per_c_function option tells us to put more than one
    % procedure into each C function, but not all procedures in the module
    % are in one function, then we would like to be able to use the
    % fast form of reference to a procedure for references not only from
    % within the same procedure but also from other procedures within
    % the same C function. However, at the time of code generation,
    % we do not yet know which procedures will be put into the same
    % C functions, and so we cannot do this.
    %
:- func make_proc_entry_label(code_info, module_info, pred_id, proc_id, bool)
    = code_addr.

    % Generate the next local label in sequence.
    %
:- pred get_next_label(label::out, code_info::in, code_info::out)
    is det.

    % Note that the succip slot is used, and thus cannot be optimized away.
    %
:- pred succip_is_used(code_info::in, code_info::out) is det.

:- pred add_trace_layout_for_label(label::in, term.context::in, trace_port::in,
    bool::in, forward_goal_path::in, maybe(user_event_info)::in,
    layout_label_info::in, code_info::in, code_info::out) is det.

:- pred get_next_closure_seq_no(int::out,
    code_info::in, code_info::out) is det.

:- pred add_resume_layout_for_label(label::in,
    layout_label_info::in, code_info::in, code_info::out) is det.

:- pred add_closure_layout(closure_proc_id_data::in,
    code_info::in, code_info::out) is det.

:- pred add_threadscope_string(string::in, int::out,
    code_info::in, code_info::out) is det.

:- pred get_threadscope_rev_string_table(code_info::in,
    list(string)::out, int::out) is det.

:- pred add_scalar_static_cell(list(typed_rval)::in,
    data_id::out, code_info::in, code_info::out) is det.

:- pred add_scalar_static_cell_natural_types(list(rval)::in,
    data_id::out, code_info::in, code_info::out) is det.

:- pred add_vector_static_cell(list(llds_type)::in, list(list(rval))::in,
    data_id::out, code_info::in, code_info::out) is det.

:- pred add_alloc_site_info(prog_context::in, string::in, int::in,
    alloc_site_id::out, code_info::in, code_info::out) is det.

    % Should we add trail ops to the code we generate for the goal with the
    % given goal_info. This will be 'no' unless we are in a trailing grade.
    %
:- func should_add_trail_ops(code_info, hlds_goal_info) = add_trail_ops.

    % Should we add region ops to the code we generate for the goal with the
    % given goal_info. This will be 'no' unless we are in a rbmm grade.
    %
:- func should_add_region_ops(code_info, hlds_goal_info) = add_region_ops.

:- pred get_containing_goal_map(code_info::in, containing_goal_map::out)
    is det.

:- pred add_out_of_line_code(llds_code::in, code_info::in, code_info::out)
    is det.

%---------------------------------------------------------------------------%

:- implementation.

get_stack_slots(CI, StackSlots) :-
    get_proc_info(CI, ProcInfo),
    proc_info_get_stack_slots(ProcInfo, StackSlots).

body_typeinfo_liveness(CI) = TypeInfoLiveness :-
    get_module_info(CI, ModuleInfo),
    get_pred_id(CI, PredId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    get_globals(CI, Globals),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness).

variable_type(CI, Var) = Type :-
    get_vartypes(CI, VarTypes),
    lookup_var_type(VarTypes, Var, Type).

variable_is_of_dummy_type(CI, Var) = IsDummy :-
    VarType = variable_type(CI, Var),
    get_module_info(CI, ModuleInfo),
    IsDummy = check_dummy_type(ModuleInfo, VarType).

search_type_defn(CI, Type, TypeDefn) :-
    get_module_info(CI, ModuleInfo),
    type_to_ctor_det(Type, TypeCtor),
    module_info_get_type_table(ModuleInfo, TypeTable),
    search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn).

lookup_type_defn(CI, Type) = TypeDefn :-
    ( if search_type_defn(CI, Type, TypeDefnPrime) then
        TypeDefn = TypeDefnPrime
    else
        unexpected($module, $pred, "type ctor has no definition")
    ).

lookup_cheaper_tag_test(CI, Type) = CheaperTagTest :-
    ( if
        search_type_defn(CI, Type, TypeDefn),
        get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_du_type(_, _, CheaperTagTestPrime, _, _, _, _, _, _)
    then
        CheaperTagTest = CheaperTagTestPrime
    else
        CheaperTagTest = no_cheaper_tag_test
    ).

filter_region_vars(CI, ForwardLiveVarsBeforeGoal) = RegionVars :-
    get_vartypes(CI, VarTypes),
    RegionVars = set_of_var.filter(is_region_var(VarTypes),
        ForwardLiveVarsBeforeGoal).

%---------------------------------------------------------------------------%

get_proc_model(CI) = CodeModel :-
    get_proc_info(CI, ProcInfo),
    CodeModel = proc_info_interface_code_model(ProcInfo).

get_headvars(CI) = HeadVars :-
    get_module_info(CI, ModuleInfo),
    get_pred_id(CI, PredId),
    get_proc_id(CI, ProcId),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars).

get_arginfo(CI) = ArgInfo :-
    get_pred_id(CI, PredId),
    get_proc_id(CI, ProcId),
    ArgInfo = get_pred_proc_arginfo(CI, PredId, ProcId).

get_pred_proc_arginfo(CI, PredId, ProcId) = ArgInfo :-
    get_module_info(CI, ModuleInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_arg_info(ProcInfo, ArgInfo).

variable_name(CI, Var) = Name :-
    get_varset(CI, Varset),
    varset.lookup_name(Varset, Var, Name).

%---------------------------------------------------------------------------%

make_proc_entry_label(CI, ModuleInfo, PredId, ProcId, Immed0) = CodeAddr :-
    (
        Immed0 = no,
        Immed = no
    ;
        Immed0 = yes,
        get_globals(CI, Globals),
        globals.lookup_int_option(Globals, procs_per_c_function, ProcsPerFunc),
        get_pred_id(CI, CurPredId),
        get_proc_id(CI, CurProcId),
        Immed = yes(ProcsPerFunc - proc(CurPredId, CurProcId))
    ),
    CodeAddr = make_entry_label(ModuleInfo, PredId, ProcId, Immed).

get_next_label(Label, !CI) :-
    get_proc_label(!.CI, ProcLabel),
    get_label_counter(!.CI, C0),
    counter.allocate(N, C0, C),
    set_label_counter(C, !CI),
    Label = internal_label(N, ProcLabel).

succip_is_used(!CI) :-
    set_succip_used(yes, !CI).

add_trace_layout_for_label(Label, Context, Port, IsHidden, GoalPath,
        MaybeSolverEventInfo, Layout, !CI) :-
    get_layout_info(!.CI, Internals0),
    Exec = yes(trace_port_layout_info(Context, Port, IsHidden, GoalPath,
        MaybeSolverEventInfo, Layout)),
    (
        Label = internal_label(LabelNum, _)
    ;
        Label = entry_label(_, _),
        unexpected($module, $pred, "entry")
    ),
    ( if map.search(Internals0, LabelNum, Internal0) then
        Internal0 = internal_layout_info(Exec0, Resume, Return),
        (
            Exec0 = no
        ;
            Exec0 = yes(_),
            unexpected($module, $pred, "already known label")
        ),
        Internal = internal_layout_info(Exec, Resume, Return),
        map.det_update(LabelNum, Internal, Internals0, Internals)
    else
        Internal = internal_layout_info(Exec, no, no),
        map.det_insert(LabelNum, Internal, Internals0, Internals)
    ),
    set_layout_info(Internals, !CI).

add_resume_layout_for_label(Label, LayoutInfo, !CI) :-
    get_layout_info(!.CI, Internals0),
    Resume = yes(LayoutInfo),
    (
        Label = internal_label(LabelNum, _)
    ;
        Label = entry_label(_, _),
        unexpected($module, $pred, "entry")
    ),
    ( if map.search(Internals0, LabelNum, Internal0) then
        Internal0 = internal_layout_info(Exec, Resume0, Return),
        (
            Resume0 = no
        ;
            Resume0 = yes(_),
            unexpected($module, $pred, "already known label")
        ),
        Internal = internal_layout_info(Exec, Resume, Return),
        map.det_update(LabelNum, Internal, Internals0, Internals)
    else
        Internal = internal_layout_info(no, Resume, no),
        map.det_insert(LabelNum, Internal, Internals0, Internals)
    ),
    set_layout_info(Internals, !CI).

get_next_closure_seq_no(SeqNo, !CI) :-
    get_closure_seq_counter(!.CI, C0),
    counter.allocate(SeqNo, C0, C),
    set_closure_seq_counter(C, !CI).

add_closure_layout(ClosureLayout, !CI) :-
    get_closure_layouts(!.CI, ClosureLayouts),
    set_closure_layouts([ClosureLayout | ClosureLayouts], !CI).

add_threadscope_string(String, SlotNum, !CI) :-
    Persistent0 = !.CI ^ code_info_persistent,
    Size0 = Persistent0 ^ cip_ts_string_table_size,
    RevTable0 = Persistent0 ^ cip_ts_rev_string_table,
    SlotNum = Size0,
    Size = Size0 + 1,
    RevTable = [String | RevTable0],
    Persistent1 = Persistent0 ^ cip_ts_string_table_size := Size,
    Persistent = Persistent1 ^ cip_ts_rev_string_table := RevTable,
    !CI ^ code_info_persistent := Persistent.

get_threadscope_rev_string_table(CI, RevTable, TableSize) :-
    RevTable = CI ^ code_info_persistent ^ cip_ts_rev_string_table,
    TableSize = CI ^ code_info_persistent ^ cip_ts_string_table_size.

add_scalar_static_cell(RvalsTypes, DataAddr, !CI) :-
    get_static_cell_info(!.CI, StaticCellInfo0),
    global_data.add_scalar_static_cell(RvalsTypes, DataAddr,
        StaticCellInfo0, StaticCellInfo),
    set_static_cell_info(StaticCellInfo, !CI).

add_scalar_static_cell_natural_types(Rvals, DataAddr, !CI) :-
    get_static_cell_info(!.CI, StaticCellInfo0),
    global_data.add_scalar_static_cell_natural_types(Rvals, DataAddr,
        StaticCellInfo0, StaticCellInfo),
    set_static_cell_info(StaticCellInfo, !CI).

add_vector_static_cell(Types, Vector, DataAddr, !CI) :-
    get_static_cell_info(!.CI, StaticCellInfo0),
    global_data.add_vector_static_cell(Types, Vector, DataAddr,
        StaticCellInfo0, StaticCellInfo),
    set_static_cell_info(StaticCellInfo, !CI).

add_alloc_site_info(Context, Type, Size, AllocId, !CI) :-
    get_proc_label(!.CI, ProcLabel),
    AllocSite = alloc_site_info(ProcLabel, Context, Type, Size),
    AllocId = alloc_site_id(AllocSite),
    get_alloc_sites(!.CI, AllocSites0),
    set_tree234.insert(AllocSite, AllocSites0, AllocSites),
    set_alloc_sites(AllocSites, !CI).

%---------------------------------------------------------------------------%

should_add_trail_ops(CodeInfo, _GoalInfo) = AddTrailOps :-
    % XXX We will eventually need to make use of GoalInfo here.
    get_emit_trail_ops(CodeInfo, AddTrailOps).

should_add_region_ops(CodeInfo, _GoalInfo) = AddRegionOps :-
    % XXX We will eventually need to make use of GoalInfo here.
    get_emit_region_ops(CodeInfo, AddRegionOps).

%---------------------------------------------------------------------------%

get_containing_goal_map(CI, ContainingGoalMap) :-
    get_maybe_containing_goal_map(CI, MaybeContainingGoalMap),
    (
        MaybeContainingGoalMap = yes(ContainingGoalMap)
    ;
        MaybeContainingGoalMap = no,
        unexpected($module, $pred, "no map")
    ).

add_out_of_line_code(NewCode, !CI) :-
    Code0 = !.CI ^ code_info_persistent ^ cip_out_of_line_code,
    Code = Code0 ++ NewCode,
    !CI ^ code_info_persistent ^ cip_out_of_line_code := Code.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for managing stack slots.

:- interface.

    % Return the lval of the stack slot in which the given variable is stored.
    % Aborts if the variable does not have a stack slot an assigned to it.
    %
:- pred get_variable_slot(code_info::in, prog_var::in, lval::out) is det.

    % Returns the total stackslot count, but not including space for
    % succip, and without padding for alignment. This total can change in the
    % future if this call is followed by further allocations of temp slots.
    %
:- pred get_total_stackslot_count(code_info::in, int::out) is det.

    % If necessary, round up a det stack frame allocation so that the stack
    % pointer remains on an even word boundary.
    %
:- func round_det_stack_frame_size(code_info, int) = int.

%---------------------------------------------------------------------------%

:- implementation.

get_variable_slot(CI, Var, Slot) :-
    get_stack_slots(CI, StackSlots),
    ( if map.search(StackSlots, Var, SlotLocn) then
        Slot = stack_slot_to_lval(SlotLocn)
    else
        Name = variable_name(CI, Var),
        term.var_to_int(Var, Num),
        string.int_to_string(Num, NumStr),
        Str = "variable `" ++ Name ++ "' " ++ "(" ++ NumStr ++ ") not found",
        unexpected($module, $pred, Str)
    ).

get_total_stackslot_count(CI, NumSlots) :-
    get_var_slot_count(CI, SlotsForVars),
    get_max_temp_slot_count(CI, SlotsForTemps),
    NumSlots = SlotsForVars + SlotsForTemps.

round_det_stack_frame_size(CI, NumSlots) = NumSlotsRoundup :-
    ( if
        odd(NumSlots),
        get_exprn_opts(CI, ExprnOpts),
        get_det_stack_float_width(ExprnOpts) = double_width
    then
        NumSlotsRoundup = NumSlots + 1
    else
        NumSlotsRoundup = NumSlots
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.code_info.
%---------------------------------------------------------------------------%
