%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: stack_layout.m.
% Main authors: trd, zs.
%
% This module generates label, procedure, module and closure layout structures
% for code in the current module for the LLDS backend. Layout structures are
% used by the parts of the runtime system that need to look at the stacks
% (and sometimes the registers) and make sense of their contents. The parts
% of the runtime system that need to do this include exception handling,
% the debugger, and (eventually) the accurate garbage collector.
%
% The tables we generate are mostly of (Mercury) types defined in layout.m,
% which are turned into C code (global variable declarations and
% initializations) by layout_out.m.
%
% The C types of the structures we generate are defined and documented in
% runtime/mercury_stack_layout.h.
%
% TODO: Handle the parent_sp register and parent stack variables.
%
%---------------------------------------------------------------------------%

:- module ll_backend.stack_layout.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.global_data.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module ll_backend.prog_rep.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Process all the continuation information stored in the HLDS,
    % converting it into LLDS data structures.
    %
:- pred generate_llds_layout_data(module_info::in,
    global_data::in, global_data::out,
    list(rval)::out, list(int)::out, list(int)::out, list(int)::out,
    list(maybe(int))::out, list(user_event_data)::out,
    list(label_layout_no_vars)::out,
    list(label_layout_short_vars)::out, list(label_layout_long_vars)::out,
    map(label, layout_slot_name)::out, map(label, data_id)::out,
    list(call_site_static_data)::out, list(coverage_point_info)::out,
    list(proc_layout_proc_static)::out,
    list(int)::out, list(int)::out, list(int)::out,
    list(table_io_entry_data)::out, map(pred_proc_id, layout_slot_name)::out,
    list(layout_slot_name)::out, list(proc_layout_exec_trace)::out,
    list(proc_layout_data)::out, list(module_layout_data)::out) is det.

:- pred construct_closure_layout(proc_label::in, int::in,
    closure_layout_info::in, proc_label::in, module_name::in,
    string::in, int::in, pred_origin::in, string::in,
    static_cell_info::in, static_cell_info::out,
    list(typed_rval)::out, closure_proc_id_data::out) is det.

:- pred convert_table_arg_info(table_arg_infos::in, int::out,
    rval::out, rval::out, static_cell_info::in, static_cell_info::out) is det.

    % Construct a representation of a variable location as a 32-bit
    % integer.
    %
:- pred represent_locn_as_int(layout_locn::in, int::out) is det.

    % Construct a representation of the interface determinism of a procedure.
    %
:- pred represent_determinism_rval(determinism::in, rval::out) is det.

%---------------------------------------------------------------------------%

:- pred compute_var_number_map(list(prog_var)::in, prog_varset::in,
    assoc_list(int, internal_layout_info)::in, hlds_goal::in,
    var_num_map::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_rtti.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.ll_pseudo_type_info.
:- import_module ll_backend.prog_rep_tables.
:- import_module ll_backend.trace_gen.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.rtti_access.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

generate_llds_layout_data(ModuleInfo, !GlobalData,
        PseudoTypeInfoRvals, HLDSVarNums, ShortLocns, LongLocns,
        UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        InternalLabelToLayoutMap, ProcLabelToLayoutMap,
        CallSiteStatics, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes,
        TableIoEntries, TableIoEntryMap, ProcEventLayouts, ExecTraces,
        ProcLayouts, ModuleLayouts) :-

    Params = init_stack_layout_params(ModuleInfo),
    map.init(LabelTables0),
    StringTable0 = init_string_table_info,
    TypeTable0 = init_type_table_info,
    global_data_get_static_cell_info(!.GlobalData, StaticCellInfo0),
    LabelLayoutInfo0 = init_label_layouts_info,
    ProcLayoutInfo0 = init_proc_layouts_info,

    global_data_get_all_proc_layouts(!.GlobalData, ProcLayoutList),
    list.foldl6(construct_proc_and_label_layouts_for_proc(Params),
        ProcLayoutList,
        LabelTables0, LabelTables,
        StringTable0, StringTable,
        TypeTable0, TypeTable,
        StaticCellInfo0, StaticCellInfo1,
        LabelLayoutInfo0, LabelLayoutInfo,
        ProcLayoutInfo0, ProcLayoutInfo),

    LabelsCounter = LabelLayoutInfo ^ lli_label_counter,
    counter.allocate(NumLabels, LabelsCounter, _),

    RevCallSiteStatics =
        ProcLayoutInfo ^ pli_proc_statics ^ psi_rev_call_sites,
    RevCoveragePoints =
        ProcLayoutInfo ^ pli_proc_statics ^ psi_rev_coverage_points,
    RevProcStatics =
        ProcLayoutInfo ^ pli_proc_statics ^ psi_rev_proc_statics,
    list.reverse(RevCallSiteStatics, CallSiteStatics),
    list.reverse(RevCoveragePoints, CoveragePoints),
    list.reverse(RevProcStatics, ProcStatics),

    RevProcHeadVarNums =
        ProcLayoutInfo ^ pli_exec_traces ^ eti_rev_proc_head_var_nums,
    RevProcVarNames =
        ProcLayoutInfo ^ pli_exec_traces ^ eti_rev_proc_var_names,
    RevProcEventLayouts =
        ProcLayoutInfo ^ pli_exec_traces ^ eti_rev_proc_event_layouts,
    RevTableIoEntries =
        ProcLayoutInfo ^ pli_exec_traces ^ eti_rev_table_io_entry_datas,
    RevExecTraces =
        ProcLayoutInfo ^ pli_exec_traces ^ eti_rev_exec_traces,
    list.reverse(RevProcHeadVarNums, ProcHeadVarNums),
    list.reverse(RevProcVarNames, ProcVarNames),
    list.reverse(RevProcEventLayouts, ProcEventLayouts),
    list.reverse(RevTableIoEntries, TableIoEntries),
    list.reverse(RevExecTraces, ExecTraces),

    TableIoEntryMap =
        ProcLayoutInfo ^ pli_exec_traces ^ eti_table_io_entry_map,

    RevProcBodyBytecodes = ProcLayoutInfo ^ pli_rev_proc_bytes,
    RevProcLayouts = ProcLayoutInfo ^ pli_rev_proc_layouts,
    RevProcLayoutNames = ProcLayoutInfo ^ pli_rev_proc_layout_names,
    list.reverse(RevProcBodyBytecodes, ProcBodyBytecodes),
    list.reverse(RevProcLayouts, ProcLayouts),
    list.reverse(RevProcLayoutNames, ProcLayoutNames),

    RevPseudoTypeInfoRvals = LabelLayoutInfo ^ lli_rev_ptis,
    RevLongLocns = LabelLayoutInfo ^ lli_rev_long_locns,
    RevShortLocns = LabelLayoutInfo ^ lli_rev_short_locns,
    RevHLDSVarNums = LabelLayoutInfo ^ lli_rev_hlds_var_nums,
    list.reverse(RevPseudoTypeInfoRvals, PseudoTypeInfoRvals),
    list.reverse(RevLongLocns, LongLocns),
    list.reverse(RevShortLocns, ShortLocns),
    list.reverse(RevHLDSVarNums, HLDSVarNums),

    UserEventVarNumsCord = LabelLayoutInfo ^ lli_user_event_var_nums,
    UserEventsCord = LabelLayoutInfo ^ lli_user_events,
    UserEventVarNums = cord.list(UserEventVarNumsCord),
    UserEvents = cord.list(UserEventsCord),

    RevNoVarLabelLayouts = LabelLayoutInfo ^ lli_rev_no_var_label_layouts,
    RevSVarLabelLayouts = LabelLayoutInfo ^ lli_rev_svar_label_layouts,
    RevLVarLabelLayouts = LabelLayoutInfo ^ lli_rev_lvar_label_layouts,
    list.reverse(RevNoVarLabelLayouts, NoVarLabelLayouts),
    list.reverse(RevSVarLabelLayouts, SVarLabelLayouts),
    list.reverse(RevLVarLabelLayouts, LVarLabelLayouts),

    InternalLabelToLayoutMap = LabelLayoutInfo ^ lli_i_label_to_layout_map,
    ProcLabelToLayoutMap = ProcLayoutInfo ^ pli_p_label_to_layout_map,

    DeepProfiling = Params ^ slp_deep_profiling,
    (
        DeepProfiling = yes,
        module_info_get_oisu_map(ModuleInfo, OISUMap),
        map.to_assoc_list(OISUMap, OISUPairs),
        encode_oisu_type_procs(ModuleInfo, OISUPairs,
            NumOISUTypes, OISUBytesCord),
        OISUBytes0 = cord.list(OISUBytesCord),
        (
            OISUBytes0 = [],
            OISUBytes = []
        ;
            OISUBytes0 = [_ | _],
            encode_int32_det(list.length(OISUBytes0) + 4, OISULimitBytes),
            OISUBytes = OISULimitBytes ++ OISUBytes0
        ),

        get_type_table_contents(TypeTable, NumTypes, TypeBytes0),
        (
            TypeBytes0 = [],
            TypeBytes = []
        ;
            TypeBytes0 = [_ | _],
            encode_int32_det(list.length(TypeBytes0) + 4, TypeTableSizeBytes),
            TypeBytes = TypeTableSizeBytes ++ TypeBytes0
        ),
        DeepProfInfo = module_layout_deep_prof(NumOISUTypes, OISUBytes,
            NumTypes, TypeBytes),
        MaybeDeepProfInfo = yes(DeepProfInfo)
    ;
        DeepProfiling = no,
        MaybeDeepProfInfo = no
    ),

    TraceLayout = Params ^ slp_trace_stack_layout,
    (
        TraceLayout = yes,
        RttiLineNumbers = Params ^ slp_rtti_line_numbers,
        (
            RttiLineNumbers = yes,
            EffLabelTables = LabelTables
        ;
            RttiLineNumbers = no,
            map.init(EffLabelTables)
        ),
        format_label_tables(EffLabelTables, SourceFileLayouts),
        TraceSuppress = Params ^ slp_trace_suppress,
        SuppressedEvents = encode_suppressed_events(TraceSuppress),
        (
            UserEvents = [],
            HasUserEvent = no
        ;
            UserEvents = [_ | _],
            HasUserEvent = yes
        ),
        (
            HasUserEvent = no,
            MaybeEventSet = no,
            StaticCellInfo = StaticCellInfo1
        ;
            HasUserEvent = yes,
            module_info_get_event_set(ModuleInfo, EventSet),
            EventSetData = derive_event_set_data(EventSet),
            list.foldl2(build_event_arg_type_info_map,
                EventSetData ^ event_set_data_specs,
                map.init, EventArgTypeInfoMap,
                StaticCellInfo1, StaticCellInfo),
            EventSetLayoutData = event_set_layout_data(EventSetData,
                EventArgTypeInfoMap),
            MaybeEventSet = yes(EventSetLayoutData)
        ),

        TraceLevel = Params ^ slp_trace_level,
        DebugInfo = module_layout_debug(ProcLayoutNames, SourceFileLayouts,
            TraceLevel, SuppressedEvents, NumLabels, MaybeEventSet),
        MaybeDebugInfo = yes(DebugInfo)
    ;
        TraceLayout = no,
        StaticCellInfo = StaticCellInfo1,
        MaybeDebugInfo = no
    ),
    ( if
        MaybeDeepProfInfo = no,
        MaybeDebugInfo = no
    then
        ModuleLayouts = []
    else
        module_info_get_name(ModuleInfo, ModuleName),
        get_string_table_contents(StringTable, StringList, StringTableSize),
        StringTableContents = string_with_0s(StringList),
        ModuleLayout = module_layout_data(ModuleName,
            StringTableSize, StringTableContents,
            MaybeDeepProfInfo, MaybeDebugInfo),
        ModuleLayouts = [ModuleLayout]
    ),
    global_data_set_static_cell_info(StaticCellInfo, !GlobalData).

:- pred valid_proc_layout(proc_layout_info::in) is semidet.

valid_proc_layout(ProcLayoutInfo) :-
    EntryLabel = ProcLayoutInfo ^ pli_entry_label,
    ProcLabel = get_proc_label(EntryLabel),
    (
        ProcLabel = ordinary_proc_label(_, _, DeclModule, Name, Arity, _),
        not no_type_info_builtin(DeclModule, Name, Arity)
    ;
        ProcLabel = special_proc_label(_, _, _, _, _, _)
    ).

:- pred build_event_arg_type_info_map(event_spec::in,
    map(int, rval)::in, map(int, rval)::out,
    static_cell_info::in, static_cell_info::out) is det.

build_event_arg_type_info_map(EventSpec, !EventArgTypeInfoMap,
        !StaticCellInfo) :-
    EventNumber = EventSpec ^ event_spec_num,
    Attrs = EventSpec ^ event_spec_attrs,
    list.map_foldl(build_event_arg_type_info, Attrs, RvalsAndTypes,
        !StaticCellInfo),
    add_scalar_static_cell(RvalsAndTypes, TypesDataAddr, !StaticCellInfo),
    Rval = const(llconst_data_addr(TypesDataAddr, no)),
    map.det_insert(EventNumber, Rval, !EventArgTypeInfoMap).

:- pred build_event_arg_type_info(event_attribute::in, typed_rval::out,
    static_cell_info::in, static_cell_info::out) is det.

build_event_arg_type_info(Attr, TypeRvalAndType, !StaticCellInfo) :-
    Type = Attr ^ attr_type,
    ExistQTvars = [],
    NumUnivQTvars = -1,
    ll_pseudo_type_info.construct_typed_llds_pseudo_type_info(Type,
        NumUnivQTvars, ExistQTvars, !StaticCellInfo, TypeRval, TypeRvalType),
    TypeRvalAndType = typed_rval(TypeRval, TypeRvalType).

%---------------------------------------------------------------------------%

:- pred format_label_tables(map(string, file_label_table)::in,
    list(file_layout_data)::out) is det.

format_label_tables(LabelTableMap, SourceFileLayouts) :-
    map.to_assoc_list(LabelTableMap, LabelTableList),
    list.map(format_label_table, LabelTableList, SourceFileLayouts).

:- pred format_label_table(pair(string, file_label_table)::in,
    file_layout_data::out) is det.

format_label_table(FileName - LineNoMap, FileLayoutData) :-
    % This step should produce a list ordered on line numbers.
    map.to_assoc_list(LineNoMap, LineNoList),
    % And this step should preserve that order.
    flatten_label_table(LineNoList, [], FlatLineNoList),
    Filter = (pred(LineNoInfo::in, FilteredLineNoInfo::out) is det :-
        LineNoInfo = LineNo - (Label - _IsReturn),
        FilteredLineNoInfo = LineNo - Label
    ),
    list.map(Filter, FlatLineNoList, FilteredList),
    FileLayoutData = file_layout_data(FileName, FilteredList).

:- pred flatten_label_table(assoc_list(int, list(line_no_info))::in,
    assoc_list(int, line_no_info)::in,
    assoc_list(int, line_no_info)::out) is det.

flatten_label_table([], RevList, List) :-
    list.reverse(RevList, List).
flatten_label_table([LineNo - LinesInfos | Lines], RevList0, List) :-
    list.foldl(add_line_no(LineNo), LinesInfos, RevList0, RevList1),
    flatten_label_table(Lines, RevList1, List).

:- pred add_line_no(int::in, line_no_info::in,
    assoc_list(int, line_no_info)::in,
    assoc_list(int, line_no_info)::out) is det.

add_line_no(LineNo, LineInfo, RevList0, RevList) :-
    RevList = [LineNo - LineInfo | RevList0].

%---------------------------------------------------------------------------%

    % The per-sourcefile label table maps line numbers to the list of
    % labels that correspond to that line. Each label is accompanied
    % by a flag that says whether the label is the return site of a call
    % or not, and if it is, whether the called procedure is known.
    %
:- type is_label_return
    --->    known_callee(label)
    ;       unknown_callee
    ;       not_a_return.

:- type line_no_info == pair(layout_slot_name, is_label_return).

:- type file_label_table == map(int, list(line_no_info)).

:- type label_tables == map(string, file_label_table).

    % Construct the layouts that concern a single procedure: the procedure
    % layout and the layouts of the labels inside that procedure. Also update
    % the module-wide label table with the labels defined in this procedure.
    %
:- pred construct_proc_and_label_layouts_for_proc(stack_layout_params::in,
    proc_layout_info::in,
    label_tables::in, label_tables::out,
    string_table_info::in, string_table_info::out,
    type_table_info::in, type_table_info::out,
    static_cell_info::in, static_cell_info::out,
    label_layouts_info::in, label_layouts_info::out,
    proc_layouts_info::in, proc_layouts_info::out) is det.

construct_proc_and_label_layouts_for_proc(Params, PLI, !LabelTables,
        !StringTable, !TypeTable, !StaticCellInfo,
        !LabelLayoutInfo, !ProcLayoutInfo) :-
    PLI = proc_layout_info(RttiProcLabel, EntryLabel,
        _Detism, _StackSlots, _SuccipLoc, _EvalMethod, _EffTraceLevel,
        _MaybeCallLabel, _MaxTraceRegR, _MaxTraceRegF, HeadVars, _ArgModes,
        Goal, _NeedGoalRep, _InstMap,
        _TraceSlotInfo, ForceProcIdLayout, VarSet, _VarTypes,
        InternalMap, MaybeTableIoEntry, _NeedsAllNames, _OISUKindFors,
        _MaybeDeepProfInfo),
    map.to_assoc_list(InternalMap, Internals),
    compute_var_number_map(HeadVars, VarSet, Internals, Goal, VarNumMap),

    ProcLabel = get_proc_label(EntryLabel),
    bool.or(Params ^ slp_procid_stack_layout, ForceProcIdLayout, ProcIdLayout),
    ( if
        ( ProcIdLayout = yes
        ; MaybeTableIoEntry = yes(_)
        )
    then
        UserOrUci = proc_label_user_or_uci(ProcLabel),
        Kind = proc_layout_proc_id(UserOrUci)
    else
        Kind = proc_layout_traversal
    ),
    ProcLayoutName = proc_layout(RttiProcLabel, Kind),
    ( if
        ( Params ^ slp_agc_stack_layout = yes
        ; Params ^ slp_trace_stack_layout = yes
        ),
        valid_proc_layout(PLI)
    then
        list.map_foldl3(
            construct_internal_layout(Params, ProcLabel, ProcLayoutName,
                VarNumMap),
            Internals, InternalLabelInfos,
            !StringTable, !StaticCellInfo, !LabelLayoutInfo)
    else
        InternalLabelInfos = []
    ),
    list.foldl(update_label_table, InternalLabelInfos, !LabelTables),
    construct_proc_layout(Params, PLI, ProcLayoutName, Kind,
        InternalLabelInfos, VarNumMap, !.LabelLayoutInfo,
        !StringTable, !TypeTable, !StaticCellInfo, !ProcLayoutInfo).

%---------------------------------------------------------------------------%

:- type internal_label_info
    --->    internal_label_info(
                containing_proc         :: proc_label,
                label_num_in_proc       :: int,
                maybe_has_var_info      :: label_vars,
                slot_in_array           :: int,
                internal_layout_info    :: internal_layout_info
            ).

    % Add the given label layout to the module-wide label tables.
    %
:- pred update_label_table(internal_label_info::in,
    map(string, file_label_table)::in, map(string, file_label_table)::out)
    is det.

update_label_table(InternalLabelInfo, !LabelTables) :-
    InternalLabelInfo = internal_label_info(_ProcLabel, _LabelNum, LabelVars,
        Slot, InternalInfo),
    InternalInfo = internal_layout_info(Port, _, Return),
    ( if
        Return = yes(return_layout_info(TargetsContexts, _)),
        find_valid_return_context(TargetsContexts, Target, Context, _GoalPath)
    then
        ( if Target = code_label(TargetLabel) then
            IsReturn = known_callee(TargetLabel)
        else
            IsReturn = unknown_callee
        ),
        update_label_table_2(LabelVars, Slot, Context, IsReturn, !LabelTables)
    else if
        Port = yes(trace_port_layout_info(Context, _, _, _, _, _)),
        context_is_valid(Context)
    then
        update_label_table_2(LabelVars, Slot, Context, not_a_return,
            !LabelTables)
    else
        true
    ).

:- pred update_label_table_2(label_vars::in, int::in, context::in,
    is_label_return::in,
    map(string, file_label_table)::in, map(string, file_label_table)::out)
    is det.

update_label_table_2(LabelVars, Slot, Context, IsReturn, !LabelTables) :-
    term.context_file(Context, File),
    term.context_line(Context, Line),
    ( if map.search(!.LabelTables, File, LabelTable0) then
        LabelLayout = layout_slot(label_layout_array(LabelVars), Slot),
        ( if map.search(LabelTable0, Line, LineInfo0) then
            LineInfo = [LabelLayout - IsReturn | LineInfo0],
            map.det_update(Line, LineInfo, LabelTable0, LabelTable),
            map.det_update(File, LabelTable, !LabelTables)
        else
            LineInfo = [LabelLayout - IsReturn],
            map.det_insert(Line, LineInfo, LabelTable0, LabelTable),
            map.det_update(File, LabelTable, !LabelTables)
        )
    else
        ( if context_is_valid(Context) then
            LabelLayout = layout_slot(label_layout_array(LabelVars), Slot),
            LineInfo = [LabelLayout - IsReturn],
            LabelTable = map.singleton(Line, LineInfo),
            map.det_insert(File, LabelTable, !LabelTables)
        else
            % We don't have a valid context for this label,
            % so we don't enter it into any tables.
            true
        )
    ).

:- pred find_valid_return_context(
    assoc_list(code_addr, pair(prog_context, forward_goal_path))::in,
    code_addr::out, prog_context::out, forward_goal_path::out) is semidet.

find_valid_return_context([TargetContext | TargetContexts],
        ValidTarget, ValidContext, ValidGoalPath) :-
    TargetContext = Target - (Context - GoalPath),
    ( if context_is_valid(Context) then
        ValidTarget = Target,
        ValidContext = Context,
        ValidGoalPath = GoalPath
    else
        find_valid_return_context(TargetContexts, ValidTarget, ValidContext,
            ValidGoalPath)
    ).

:- pred context_is_valid(prog_context::in) is semidet.

context_is_valid(Context) :-
    term.context_file(Context, File),
    term.context_line(Context, Line),
    File \= "",
    Line > 0.

%---------------------------------------------------------------------------%
%
% Construct proc layouts.
%

    % Construct a procedure-specific layout.
    %
:- pred construct_proc_layout(stack_layout_params::in, proc_layout_info::in,
    layout_name::in, proc_layout_kind::in, list(internal_label_info)::in,
    var_num_map::in, label_layouts_info::in,
    string_table_info::in, string_table_info::out,
    type_table_info::in, type_table_info::out,
    static_cell_info::in, static_cell_info::out,
    proc_layouts_info::in, proc_layouts_info::out) is det.

construct_proc_layout(Params, PLI, ProcLayoutName, Kind, InternalLabelInfos,
        VarNumMap, LabelLayoutInfo, !StringTable, !TypeTable,
        !StaticCellInfo, !ProcLayoutInfo) :-
    PLI = proc_layout_info(RttiProcLabel, EntryLabel,
        Detism, StackSlots, SuccipLoc, EvalMethod, EffTraceLevel,
        MaybeCallLabel, MaxTraceRegR, MaxTraceRegF, HeadVars, ArgModes,
        Goal, NeedGoalRep, InstMap,
        TraceSlotInfo, _ForceProcIdLayout, VarSet, VarTypes,
        InternalMap, MaybeTableInfo, NeedsAllNames, OISUKindFors,
        MaybeDeepProfInfo),
    construct_proc_traversal(Params, EntryLabel, Detism, StackSlots,
        SuccipLoc, Traversal),
    PredId = RttiProcLabel ^ rpl_pred_id,
    ProcId = RttiProcLabel ^ rpl_proc_id,
    PredProcId = proc(PredId, ProcId),
    (
        MaybeTableInfo = no,
        MaybeTableSlotName = no
    ;
        MaybeTableInfo = yes(TableInfo),
        TableExecTraceInfo0 = !.ProcLayoutInfo ^ pli_exec_traces,
        construct_exec_trace_table_data(PredProcId, ProcLayoutName, TableInfo,
            MaybeTableSlotName, !StaticCellInfo,
            TableExecTraceInfo0, TableExecTraceInfo),
        !ProcLayoutInfo ^ pli_exec_traces := TableExecTraceInfo
    ),
    (
        Kind = proc_layout_traversal,
        More = no_proc_id_and_more
    ;
        Kind = proc_layout_proc_id(_),
        TraceStackLayout = Params ^ slp_trace_stack_layout,
        ( if
            TraceStackLayout = yes,
            not map.is_empty(InternalMap),
            valid_proc_layout(PLI)
        then
            ExecTraceInfo0 = !.ProcLayoutInfo ^ pli_exec_traces,
            construct_exec_trace_layout(Params, RttiProcLabel,
                EvalMethod, EffTraceLevel, MaybeCallLabel, MaybeTableSlotName,
                MaxTraceRegR, MaxTraceRegF, HeadVars, ArgModes, TraceSlotInfo,
                VarSet, VarTypes, MaybeTableInfo, NeedsAllNames,
                VarNumMap, InternalLabelInfos, ExecTraceSlotName,
                LabelLayoutInfo, !StringTable,
                ExecTraceInfo0, ExecTraceInfo),
            !ProcLayoutInfo ^ pli_exec_traces := ExecTraceInfo,
            MaybeExecTraceSlotName = yes(ExecTraceSlotName)
        else
            MaybeExecTraceSlotName = no
        ),
        ModuleInfo = Params ^ slp_module_info,
        module_info_get_name(ModuleInfo, ModuleName),
        DeepProfiling = Params ^ slp_deep_profiling,
        ( if
            ( NeedGoalRep = trace_needs_body_rep
            ; DeepProfiling = yes
            )
        then
            (
                DeepProfiling = yes,
                IncludeVarNameTable = include_var_name_table,
                (
                    OISUKindFors = [],
                    IncludeVarTypes = do_not_include_var_types
                ;
                    OISUKindFors = [_ | _],
                    IncludeVarTypes = include_var_types
                )
            ;
                DeepProfiling = no,
                IncludeVarNameTable = do_not_include_var_name_table,
                IncludeVarTypes = do_not_include_var_types
            ),

            % When the program is compiled with deep profiling, use
            % the version of the procedure saved *before* the deep profiling
            % transformation as the program representation.
            (
                MaybeDeepProfInfo = yes(DeepProfInfo),
                ProcStaticInfo0 = !.ProcLayoutInfo ^ pli_proc_statics,
                construct_proc_static_layout(DeepProfInfo, ProcStaticSlotName,
                    ProcStaticInfo0, ProcStaticInfo),
                !ProcLayoutInfo ^ pli_proc_statics := ProcStaticInfo,
                MaybeProcStaticSlotName = yes(ProcStaticSlotName),

                DeepOriginalBody = DeepProfInfo ^ pdpi_orig_body,
                DeepOriginalBody = deep_original_body(BytecodeBody,
                    BytecodeHeadVars, BytecodeInstMap, BytecodeVarTypes,
                    BytecodeDetism, BytecodeVarSet),
                compute_var_number_map(BytecodeHeadVars, BytecodeVarSet, [],
                    BytecodeBody, BytecodeVarNumMap)
            ;
                MaybeDeepProfInfo = no,
                MaybeProcStaticSlotName = no,
                BytecodeHeadVars = HeadVars,
                BytecodeBody = Goal,
                BytecodeInstMap = InstMap,
                BytecodeVarTypes = VarTypes,
                BytecodeDetism = Detism,
                BytecodeVarNumMap = VarNumMap
            ),
            represent_proc_as_bytecodes(BytecodeHeadVars, BytecodeBody,
                BytecodeInstMap, BytecodeVarTypes, BytecodeVarNumMap,
                ModuleInfo, IncludeVarNameTable, IncludeVarTypes,
                BytecodeDetism, !StringTable, !TypeTable, ProcBytes),

            % Calling find_sequence on ProcBytes is very unlikely to find
            % any matching sequences, though there is no reason why we
            % cannot try.
            list.reverse(ProcBytes, RevProcBytes),
            list.length(ProcBytes, NumProcBytes),

            RevAllProcBytes0 = !.ProcLayoutInfo ^ pli_rev_proc_bytes,
            RevAllProcBytes = RevProcBytes ++ RevAllProcBytes0,
            !ProcLayoutInfo ^ pli_rev_proc_bytes := RevAllProcBytes,

            NextProcByte0 = !.ProcLayoutInfo ^ pli_next_proc_byte,
            ProcByteSlot = NextProcByte0,
            NextProcByte = NumProcBytes + NextProcByte0,
            !ProcLayoutInfo ^ pli_next_proc_byte := NextProcByte,

            ProcBytesSlotName = layout_slot(proc_body_bytecodes_array,
                ProcByteSlot),
            MaybeProcBytesSlotName = yes(ProcBytesSlotName)
        else
            (
                MaybeDeepProfInfo = yes(DeepProfInfo),
                ProcStaticInfo0 = !.ProcLayoutInfo ^ pli_proc_statics,
                construct_proc_static_layout(DeepProfInfo, ProcStaticSlotName,
                    ProcStaticInfo0, ProcStaticInfo),
                !ProcLayoutInfo ^ pli_proc_statics := ProcStaticInfo,
                MaybeProcStaticSlotName = yes(ProcStaticSlotName)
            ;
                MaybeDeepProfInfo = no,
                MaybeProcStaticSlotName = no
            ),
            MaybeProcBytesSlotName = no
        ),
        ModuleLayoutName = module_layout(ModuleName),
        More = proc_id_and_more(MaybeProcStaticSlotName,
            MaybeExecTraceSlotName, MaybeProcBytesSlotName, ModuleLayoutName)
    ),

    ProcLayout = proc_layout_data(RttiProcLabel, Traversal, More),

    RevProcLayouts0 = !.ProcLayoutInfo ^ pli_rev_proc_layouts,
    RevProcLayouts = [ProcLayout | RevProcLayouts0],
    !ProcLayoutInfo ^ pli_rev_proc_layouts := RevProcLayouts,

    RevProcLayoutNames0 = !.ProcLayoutInfo ^ pli_rev_proc_layout_names,
    RevProcLayoutNames = [ProcLayoutName | RevProcLayoutNames0],
    !ProcLayoutInfo ^ pli_rev_proc_layout_names := RevProcLayoutNames,

    LabelToLayoutMap0 = !.ProcLayoutInfo ^ pli_p_label_to_layout_map,
    map.det_insert(EntryLabel, layout_id(ProcLayoutName),
        LabelToLayoutMap0, LabelToLayoutMap),
    !ProcLayoutInfo ^ pli_p_label_to_layout_map := LabelToLayoutMap.

:- pred construct_proc_traversal(stack_layout_params::in, label::in,
    determinism::in, int::in, maybe(int)::in, proc_layout_stack_traversal::out)
    is det.

construct_proc_traversal(Params, EntryLabel, Detism, NumStackSlots,
        MaybeSuccipLoc, Traversal) :-
    (
        MaybeSuccipLoc = yes(Location),
        ( if determinism_components(Detism, _, at_most_many) then
            SuccipLval = framevar(Location)
        else
            SuccipLval = stackvar(Location)
        ),
        represent_locn_as_int(locn_direct(SuccipLval), SuccipInt),
        MaybeSuccipInt = yes(SuccipInt)
    ;
        MaybeSuccipLoc = no,
        % Use a dummy location if there is no succip slot on the stack.
        %
        % This case can arise in two circumstances. First, procedures that
        % use the nondet stack have a special slot for the succip, so the
        % succip is not stored in a general purpose slot. Second, procedures
        % that use the det stack but which do not call other procedures
        % do not save the succip on the stack.
        %
        % The tracing system does not care about the location of the saved
        % succip. The accurate garbage collector does. It should know from
        % the determinism that the procedure uses the nondet stack, which
        % takes care of the first possibility above. Procedures that do not
        % call other procedures do not establish resumption points and thus
        % agc is not interested in them. As far as stack dumps go, calling
        % error counts as a call, so any procedure that may call error
        % (directly or indirectly) will have its saved succip location
        % recorded, so the stack dump will work.
        %
        % Future uses of stack layouts will have to have similar constraints.
        MaybeSuccipInt = no
    ),
    StaticCodeAddr = Params ^ slp_static_code_addresses,
    (
        StaticCodeAddr = yes,
        MaybeEntryLabel = yes(EntryLabel)
    ;
        StaticCodeAddr = no,
        MaybeEntryLabel = no
    ),
    Traversal = proc_layout_stack_traversal(MaybeEntryLabel,
        MaybeSuccipInt, NumStackSlots, Detism).

:- type proc_layouts_info
    --->    proc_layouts_info(
                pli_proc_statics            :: proc_statics_info,
                pli_exec_traces             :: exec_traces_info,

                pli_next_proc_byte          :: int,
                pli_rev_proc_bytes          :: list(int),

                % The list of proc_layouts in the module.
                pli_rev_proc_layouts        :: list(proc_layout_data),
                pli_rev_proc_layout_names   :: list(layout_name),

                % This maps each proc label with a layout
                % to the id of its layout structure.
                pli_p_label_to_layout_map   :: map(label, data_id)
            ).

:- func init_proc_layouts_info = proc_layouts_info.

init_proc_layouts_info = Info :-
    ProcStaticInfo = init_proc_statics_info,
    ExecTraceInfo = init_exec_traces_info,
    Info = proc_layouts_info(ProcStaticInfo, ExecTraceInfo,
        0, [], [], [], map.init).

%---------------------------------------------------------------------------%
%
% Construct proc static structures.
%

:- pred construct_proc_static_layout(proc_deep_prof_info::in,
    layout_slot_name::out,
    proc_statics_info::in, proc_statics_info::out) is det.

construct_proc_static_layout(DeepProfInfo, ProcStaticSlotName,
        !ProcStaticInfo) :-
    DeepProfInfo = proc_deep_prof_info(HLDSProcStatic, DeepExcpSlots,
        _OrigBody),
    HLDSProcStatic = hlds_proc_static(FileName, LineNumber, IsInInterface,
        CallSites, CoveragePoints),
    (
        CallSites = [],
        MaybeCallSitesTuple = no
    ;
        CallSites = [_ | _],
        list.reverse(CallSites, RevCallSites),
        list.length(CallSites, NumCallSites),

        RevAllCallSites0 = !.ProcStaticInfo ^ psi_rev_call_sites,
        RevAllCallSites = RevCallSites ++ RevAllCallSites0,
        !ProcStaticInfo ^ psi_rev_call_sites := RevAllCallSites,

        NextCallSite0 = !.ProcStaticInfo ^ psi_next_call_site,
        CallSiteSlot = NextCallSite0,
        NextCallSite = NextCallSite0 + NumCallSites,
        !ProcStaticInfo ^ psi_next_call_site := NextCallSite,

        MaybeCallSitesTuple = yes({CallSiteSlot, NumCallSites})
    ),
    (
        CoveragePoints = [],
        MaybeCoveragePointsTuple = no
    ;
        CoveragePoints = [_ | _],
        list.reverse(CoveragePoints, RevCoveragePoints),
        list.length(CoveragePoints, NumCoveragePoints),

        RevAllCoveragePoints0 = !.ProcStaticInfo ^ psi_rev_coverage_points,
        RevAllCoveragePoints = RevCoveragePoints ++ RevAllCoveragePoints0,
        !ProcStaticInfo ^ psi_rev_coverage_points := RevAllCoveragePoints,

        NextCoveragePoint0 = !.ProcStaticInfo ^ psi_next_coverage_point,
        CoveragePointSlot = NextCoveragePoint0,
        NextCoveragePoint = NextCoveragePoint0 + NumCoveragePoints,
        !ProcStaticInfo ^ psi_next_coverage_point := NextCoveragePoint,

        MaybeCoveragePointsTuple = yes({CoveragePointSlot, NumCoveragePoints})
    ),

    ProcStatic = proc_layout_proc_static(FileName, LineNumber, IsInInterface,
        DeepExcpSlots, MaybeCallSitesTuple, MaybeCoveragePointsTuple),

    RevProcStatics0 = !.ProcStaticInfo ^ psi_rev_proc_statics,
    RevProcStatics = [ProcStatic | RevProcStatics0],
    !ProcStaticInfo ^ psi_rev_proc_statics := RevProcStatics,

    ProcStaticCounter0 = !.ProcStaticInfo ^ psi_next_proc_static,
    counter.allocate(ProcStaticSlot, ProcStaticCounter0, ProcStaticCounter),
    ProcStaticSlotName = layout_slot(proc_static_array, ProcStaticSlot),
    !ProcStaticInfo ^ psi_next_proc_static := ProcStaticCounter.

%---------------------------------------------------------------------------%

:- type proc_statics_info
    --->    proc_statics_info(
                % The arrays that hold (components of) proc static structures.
                psi_next_call_site          :: int,
                psi_next_coverage_point     :: int,
                psi_next_proc_static        :: counter,

                psi_rev_call_sites          :: list(call_site_static_data),
                psi_rev_coverage_points     :: list(coverage_point_info),
                psi_rev_proc_statics        :: list(proc_layout_proc_static)
            ).

:- func init_proc_statics_info = proc_statics_info.

init_proc_statics_info = Info :-
    Info = proc_statics_info(0, 0, counter.init(0), [], [], []).

%---------------------------------------------------------------------------%
%
% Construct exec trace structures.
%

:- pred construct_exec_trace_layout(stack_layout_params::in,
    rtti_proc_label::in, eval_method::in, trace_level::in, maybe(label)::in,
    maybe(layout_slot_name)::in, int::in, int::in,
    list(prog_var)::in, list(mer_mode)::in,
    trace_slot_info::in, prog_varset::in, vartypes::in,
    maybe(proc_layout_table_info)::in, bool::in, var_num_map::in,
    list(internal_label_info)::in, layout_slot_name::out,
    label_layouts_info::in,
    string_table_info::in, string_table_info::out,
    exec_traces_info::in, exec_traces_info::out) is det.

construct_exec_trace_layout(Params, RttiProcLabel, EvalMethod,
        EffTraceLevel, MaybeCallLabel, MaybeTableSlotName,
        MaxTraceRegR, MaxTraceRegF,
        HeadVars, ArgModes, TraceSlotInfo, _VarSet, VarTypes, MaybeTableInfo,
        NeedsAllNames, VarNumMap, InternalLabelInfos, ExecTraceName,
        LabelLayoutInfo, !StringTable, !ExecTraceInfo) :-
    collect_event_data_addrs(InternalLabelInfos,
        [], RevInterfaceEventLayoutNames, [], RevInternalEventLayoutNames),
    list.reverse(RevInterfaceEventLayoutNames, InterfaceEventLayoutNames),
    list.reverse(RevInternalEventLayoutNames, InternalEventLayoutNames),
    construct_var_name_vector(VarNumMap,
        NeedsAllNames, MaxVarNum, VarNameVector, !StringTable),
    list.map(convert_var_to_int(VarNumMap), HeadVars, HeadVarNumVector),
    TraceSlotInfo = trace_slot_info(MaybeFromFullSlot, MaybeIoSeqSlot,
        MaybeTrailSlots, MaybeMaxfrSlot, MaybeCallTableSlot, MaybeTailRecSlot),
    ModuleInfo = Params ^ slp_module_info,
    (
        MaybeCallLabel = yes(CallLabel),
        map.lookup(LabelLayoutInfo ^ lli_i_label_to_layout_map, CallLabel,
            CallLabelSlotName),
        MaybeCallLabelSlotName = yes(CallLabelSlotName)
    ;
        MaybeCallLabel = no,
        MaybeCallLabelSlotName = no
    ),
    (
        MaybeTableInfo = no,
        MaybeTable = no
    ;
        MaybeTableInfo = yes(TableInfo),
        (
            TableInfo = proc_table_io_entry(_),
            (
                MaybeTableSlotName = yes(TableSlotName),
                MaybeTable = yes(data_or_slot_is_slot(TableSlotName))
            ;
                MaybeTableSlotName = no,
                unexpected($pred, "MaybeTableSlotName = no")
            )
        ;
            TableInfo = proc_table_struct(_),
            expect(unify(MaybeTableSlotName, no), $pred,
                "MaybeTableSlotName != no"),
            ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
            TableDataId = proc_tabling_data_id(ProcLabel, tabling_info),
            MaybeTable = yes(data_or_slot_is_data(TableDataId))
        )
    ),

    RevEventLayouts0 = !.ExecTraceInfo ^ eti_rev_proc_event_layouts,
    ProcEventLayouts = InterfaceEventLayoutNames ++ InternalEventLayoutNames,
    list.reverse(ProcEventLayouts, RevProcEventLayouts),
    RevEventLayouts = RevProcEventLayouts ++ RevEventLayouts0,
    !ExecTraceInfo ^ eti_rev_proc_event_layouts := RevEventLayouts,

    NextEventLayout0 = !.ExecTraceInfo ^ eti_next_proc_event_layout,
    EventLayoutsSlot = NextEventLayout0,
    list.length(ProcEventLayouts, NumProcEventLayouts),
    NextEventLayout = NextEventLayout0 + NumProcEventLayouts,
    !ExecTraceInfo ^ eti_next_proc_event_layout := NextEventLayout,
    EventLayoutsSlotName = layout_slot(proc_event_layouts_array,
        EventLayoutsSlot),

    CompressArrays = Params ^ slp_compress_arrays,
    (
        HeadVarNumVector = [_ | _],
        RevHeadVarNums0 = !.ExecTraceInfo ^ eti_rev_proc_head_var_nums,
        NextHeadVarNum0 = !.ExecTraceInfo ^ eti_next_proc_head_var_num,
        list.reverse(HeadVarNumVector, RevHeadVarNumVector),
        list.length(HeadVarNumVector, NumHeadVars),
        ( if
            some [CompressionLimit] (
                CompressArrays = yes(CompressionLimit),
                !.ExecTraceInfo ^ eti_next_proc_head_var_num =<
                    CompressionLimit
            ),
            find_sequence(RevHeadVarNumVector, RevHeadVarNums0,
                0, OldHeadVarNumOffset)
        then
            HeadVarNumSlot =
                NextHeadVarNum0 - OldHeadVarNumOffset - NumHeadVars
        else
            RevHeadVarNums = RevHeadVarNumVector ++ RevHeadVarNums0,
            !ExecTraceInfo ^ eti_rev_proc_head_var_nums := RevHeadVarNums,

            HeadVarNumSlot = NextHeadVarNum0,
            NextHeadVarNum = NextHeadVarNum0 + NumHeadVars,
            !ExecTraceInfo ^ eti_next_proc_head_var_num := NextHeadVarNum
        ),
        HeadVarNumSlotName = layout_slot(proc_head_var_nums_array,
            HeadVarNumSlot),
        MaybeHeadVarsSlotName = yes(HeadVarNumSlotName)
    ;
        HeadVarNumVector = [],
        MaybeHeadVarsSlotName = no,
        NumHeadVars = 0
    ),

    (
        VarNameVector = [_ | _],
        RevVarNames0 = !.ExecTraceInfo ^ eti_rev_proc_var_names,
        NextVarName0 = !.ExecTraceInfo ^ eti_next_proc_var_name,
        list.reverse(VarNameVector, RevVarNameVector),
        list.length(VarNameVector, NumVarNames),
        ( if
            some [CompressionLimit] (
                CompressArrays = yes(CompressionLimit),
                !.ExecTraceInfo ^ eti_next_proc_var_name =< CompressionLimit
            ),
            find_sequence(RevVarNameVector, RevVarNames0, 0, OldVarNameOffset)
        then
            VarNameSlot = NextVarName0 - OldVarNameOffset - NumVarNames
        else
            RevVarNames = RevVarNameVector ++ RevVarNames0,
            !ExecTraceInfo ^ eti_rev_proc_var_names := RevVarNames,

            VarNameSlot = NextVarName0,
            NextVarName = NextVarName0 + NumVarNames,
            !ExecTraceInfo ^ eti_next_proc_var_name := NextVarName
        ),
        VarNameSlotName = layout_slot(proc_var_names_array, VarNameSlot),
        MaybeVarNamesSlotName = yes(VarNameSlotName)
    ;
        VarNameVector = [],
        MaybeVarNamesSlotName = no
    ),

    encode_exec_trace_flags(ModuleInfo, HeadVars, ArgModes, VarTypes, Flags),
    ExecTrace = proc_layout_exec_trace(MaybeCallLabelSlotName,
        EventLayoutsSlotName, NumProcEventLayouts, MaybeTable,
        MaybeHeadVarsSlotName, NumHeadVars, MaybeVarNamesSlotName,
        MaxVarNum, MaxTraceRegR, MaxTraceRegF, MaybeFromFullSlot,
        MaybeIoSeqSlot, MaybeTrailSlots, MaybeMaxfrSlot, EvalMethod,
        MaybeCallTableSlot, MaybeTailRecSlot, EffTraceLevel, Flags),

    RevExecTraces0 = !.ExecTraceInfo ^ eti_rev_exec_traces,
    RevExecTraces = [ExecTrace | RevExecTraces0],
    !ExecTraceInfo ^ eti_rev_exec_traces := RevExecTraces,

    ExecTraceCounter0 = !.ExecTraceInfo ^ eti_next_exec_trace,
    counter.allocate(ExecTraceSlot, ExecTraceCounter0, ExecTraceCounter),
    ExecTraceName = layout_slot(proc_exec_trace_array, ExecTraceSlot),
    !ExecTraceInfo ^ eti_next_exec_trace := ExecTraceCounter.

%---------------------------------------------------------------------------%

:- pred construct_exec_trace_table_data(pred_proc_id::in, layout_name::in,
    proc_layout_table_info::in, maybe(layout_slot_name)::out,
    static_cell_info::in, static_cell_info::out,
    exec_traces_info::in, exec_traces_info::out) is det.

construct_exec_trace_table_data(PredProcId, ProcLayoutName, TableInfo,
        MaybeTableSlotName, !StaticCellInfo, !ExecTraceInfo) :-
    (
        TableInfo = proc_table_io_entry(TableIOInfo),
        TableIOInfo = proc_table_io_info(MaybeTableArgInfos),
        (
            MaybeTableArgInfos = no,
            TableIoEntryData = table_io_entry_data(ProcLayoutName, no)
        ;
            MaybeTableArgInfos = yes(TableArgInfos),
            convert_table_arg_info(TableArgInfos, NumPTIs, PTIVectorRval,
                TVarVectorRval, !StaticCellInfo),
            TableIoArgsData = table_io_args_data(NumPTIs, PTIVectorRval,
                TVarVectorRval),
            TableIoEntryData = table_io_entry_data(ProcLayoutName,
                yes(TableIoArgsData))
        ),

        RevTableIoEntryDatas0 = !.ExecTraceInfo ^ eti_rev_table_io_entry_datas,
        RevTableIoEntryDatas = [TableIoEntryData | RevTableIoEntryDatas0],
        !ExecTraceInfo ^ eti_rev_table_io_entry_datas := RevTableIoEntryDatas,

        TableDataCounter0 = !.ExecTraceInfo ^ eti_next_table_io_entry_data,
        counter.allocate(Slot, TableDataCounter0, TableDataCounter),
        !ExecTraceInfo ^ eti_next_table_io_entry_data := TableDataCounter,

        TableDataSlotName = layout_slot(proc_table_io_entry_array, Slot),
        MaybeTableSlotName = yes(TableDataSlotName),

        TableIoEntryMap0 = !.ExecTraceInfo ^ eti_table_io_entry_map,
        map.det_insert(PredProcId, TableDataSlotName,
            TableIoEntryMap0, TableIoEntryMap),
        !ExecTraceInfo ^ eti_table_io_entry_map := TableIoEntryMap
    ;
        TableInfo = proc_table_struct(_TableStructInfo),
        % This structure is generated by add_tabling_info_struct in proc_gen.m.
        MaybeTableSlotName = no
    ).

convert_table_arg_info(TableArgInfos, NumPTIs,
        PTIVectorRval, TVarVectorRval, !StaticCellInfo) :-
    TableArgInfos = table_arg_infos(Args, TVarSlotMap),
    list.length(Args, NumPTIs),
    list.map_foldl(construct_table_arg_pti_rval, Args, PTIRvalsTypes,
        !StaticCellInfo),
    add_scalar_static_cell(PTIRvalsTypes, PTIVectorAddr, !StaticCellInfo),
    PTIVectorRval = const(llconst_data_addr(PTIVectorAddr, no)),
    map.map_values_only(convert_slot_to_locn_map, TVarSlotMap, TVarLocnMap),
    construct_tvar_vector(TVarLocnMap, TVarVectorRval, !StaticCellInfo).

:- pred convert_slot_to_locn_map(table_locn::in, set(layout_locn)::out) is det.

convert_slot_to_locn_map(SlotLocn, LvalLocns) :-
    (
        SlotLocn = table_locn_direct(SlotNum),
        LvalLocn = locn_direct(reg(reg_r, SlotNum))
    ;
        SlotLocn = table_locn_indirect(SlotNum, Offset),
        LvalLocn = locn_indirect(reg(reg_r, SlotNum), Offset)
    ),
    LvalLocns = set.make_singleton_set(LvalLocn).

:- pred construct_table_arg_pti_rval(table_arg_info::in, typed_rval::out,
    static_cell_info::in, static_cell_info::out) is det.

construct_table_arg_pti_rval(ClosureArg, typed_rval(ArgRval, ArgRvalType),
        !StaticCellInfo) :-
    ClosureArg = table_arg_info(_, _, _, Type),
    ExistQTvars = [],
    NumUnivQTvars = -1,
    ll_pseudo_type_info.construct_typed_llds_pseudo_type_info(Type,
        NumUnivQTvars, ExistQTvars, !StaticCellInfo, ArgRval, ArgRvalType).

%---------------------------------------------------------------------------%

:- pred collect_event_data_addrs(list(internal_label_info)::in,
    list(layout_slot_name)::in, list(layout_slot_name)::out,
    list(layout_slot_name)::in, list(layout_slot_name)::out) is det.

collect_event_data_addrs([], !RevInterfaces, !RevInternals).
collect_event_data_addrs([Info | Infos], !RevInterfaces, !RevInternals) :-
    Info = internal_label_info(_ProcLabel, _LabelNum, LabelVars, Slot,
        InternalInfo),
    InternalInfo = internal_layout_info(MaybePortInfo, _, _),
    (
        MaybePortInfo = no
    ;
        MaybePortInfo = yes(PortInfo),
        Port = PortInfo ^ port_type,
        (
            ( Port = port_call
            ; Port = port_exit
            ; Port = port_redo
            ; Port = port_fail
            ; Port = port_tailrec_call
            ),
            LayoutName = layout_slot(label_layout_array(LabelVars), Slot),
            !:RevInterfaces = [LayoutName | !.RevInterfaces]
        ;
            ( Port = port_ite_cond
            ; Port = port_ite_then
            ; Port = port_ite_else
            ; Port = port_neg_enter
            ; Port = port_neg_success
            ; Port = port_neg_failure
            ; Port = port_disj_first
            ; Port = port_disj_later
            ; Port = port_switch
            ; Port = port_user
            ),
            LayoutName = layout_slot(label_layout_array(LabelVars), Slot),
            !:RevInternals = [LayoutName | !.RevInternals]
        ;
            Port = port_exception
            % This port is attached to call sites, so there is no event here.
        )
    ),
    collect_event_data_addrs(Infos, !RevInterfaces, !RevInternals).

%---------------------------------------------------------------------------%

:- pred encode_exec_trace_flags(module_info::in, list(prog_var)::in,
    list(mer_mode)::in, vartypes::in, int::out) is det.

encode_exec_trace_flags(ModuleInfo, HeadVars, ArgModes, VarTypes, !:Flags) :-
    % The values of the flags are defined in runtime/mercury_stack_layout.h;
    % look for the reference to this function.
    !:Flags = 0,
    ( if
        proc_info_has_io_state_pair_from_details(ModuleInfo, HeadVars,
            ArgModes, VarTypes, _, _)
    then
        !:Flags = !.Flags + 1
    else
        true
    ),
    ( if
        proc_info_has_higher_order_arg_from_details(ModuleInfo, VarTypes,
            HeadVars)
    then
        !:Flags = !.Flags + 2
    else
        true
    ).

%---------------------------------------------------------------------------%

:- pred construct_var_name_vector(var_num_map::in,
    bool::in, int::out, list(int)::out,
    string_table_info::in, string_table_info::out) is det.

construct_var_name_vector(VarNumMap, NeedsAllNames, MaxVarNum, Offsets,
        !StringTable) :-
    map.values(VarNumMap, VarNames0),
    (
        NeedsAllNames = yes,
        VarNames = VarNames0
    ;
        NeedsAllNames = no,
        list.filter(var_has_name, VarNames0, VarNames)
    ),
    list.sort(VarNames, SortedVarNames),
    (
        SortedVarNames = [FirstVarNum - _ | _],
        MaxVarNum0 = FirstVarNum,
        construct_var_name_rvals(SortedVarNames, 1, MaxVarNum0, MaxVarNum,
            Offsets, !StringTable)
    ;
        SortedVarNames = [],
        % Since variable numbers start at 1, MaxVarNum = 0 implies
        % an empty array.
        MaxVarNum = 0,
        Offsets = []
    ).

:- pred var_has_name(pair(int, string)::in) is semidet.

var_has_name(_VarNum - VarName) :-
    VarName \= "".

:- pred construct_var_name_rvals(assoc_list(int, string)::in,
    int::in, int::in, int::out, list(int)::out,
    string_table_info::in, string_table_info::out) is det.

construct_var_name_rvals([], _CurNum, MaxNum, MaxNum, [], !StringTable).
construct_var_name_rvals(VarNamesHeadTail @ [Var - Name | VarNamesTail],
        CurNum, !MaxNum, [Offset | OffsetsTail], !StringTable) :-
    ( if Var = CurNum then
        lookup_string_in_table(Name, Offset, !StringTable),
        !:MaxNum = Var,
        VarNames = VarNamesTail
    else
        Offset = 0,
        VarNames = VarNamesHeadTail
    ),
    construct_var_name_rvals(VarNames,
        CurNum + 1, !MaxNum, OffsetsTail, !StringTable).

%---------------------------------------------------------------------------%

compute_var_number_map(HeadVars, VarSet, Internals, Goal, VarNumMap) :-
    some [!VarNumMap, !Counter] (
        !:VarNumMap = map.init,
        !:Counter = counter.init(1), % to match term.var_supply_init
        goal_util.goal_vars(Goal, GoalVarSet),
        set_of_var.to_sorted_list(GoalVarSet, GoalVars),
        list.foldl2(add_var_to_var_number_map(VarSet), GoalVars,
            !VarNumMap, !Counter),
        list.foldl2(add_var_to_var_number_map(VarSet), HeadVars,
            !VarNumMap, !Counter),
        list.foldl2(internal_var_number_map(VarSet), Internals, !VarNumMap,
            !.Counter, _),
        VarNumMap = !.VarNumMap
    ).

:- pred internal_var_number_map(prog_varset::in,
    pair(int, internal_layout_info)::in,
    var_num_map::in, var_num_map::out, counter::in, counter::out) is det.

internal_var_number_map(VarSet, _Label - Internal, !VarNumMap, !Counter) :-
    Internal = internal_layout_info(MaybeTrace, MaybeResume, MaybeReturn),
    (
        MaybeTrace = yes(Trace),
        Trace = trace_port_layout_info(_, _, _, _, MaybeUser, TraceLayout),
        label_layout_var_number_map(TraceLayout, !VarNumMap, !Counter),
        (
            MaybeUser = no
        ;
            MaybeUser = yes(UserEvent),
            UserEvent = user_event_info(_UserEventNumber, Attributes),
            list.foldl2(user_attribute_var_num_map(VarSet), Attributes,
                !VarNumMap, !Counter)
        )
    ;
        MaybeTrace = no
    ),
    (
        MaybeResume = yes(ResumeLayout),
        label_layout_var_number_map(ResumeLayout, !VarNumMap, !Counter)
    ;
        MaybeResume = no
    ),
    (
        MaybeReturn = yes(Return),
        Return = return_layout_info(_, ReturnLayout),
        label_layout_var_number_map(ReturnLayout, !VarNumMap, !Counter)
    ;
        MaybeReturn = no
    ).

:- pred label_layout_var_number_map(layout_label_info::in,
    var_num_map::in, var_num_map::out, counter::in, counter::out) is det.

label_layout_var_number_map(LabelLayout, !VarNumMap, !Counter) :-
    LabelLayout = layout_label_info(VarInfoSet, _),
    VarInfos = set.to_sorted_list(VarInfoSet),
    FindVar = (pred(VarInfo::in, Var - Name::out) is semidet :-
        VarInfo = layout_var_info(_, LiveValueType, _),
        LiveValueType = live_value_var(Var, Name, _, _)
    ),
    list.filter_map(FindVar, VarInfos, VarsNames),
    list.foldl2(add_named_var_to_var_number_map, VarsNames,
        !VarNumMap, !Counter).

:- pred user_attribute_var_num_map(prog_varset::in, maybe(user_attribute)::in,
    var_num_map::in, var_num_map::out, counter::in, counter::out) is det.

user_attribute_var_num_map(VarSet, MaybeAttribute, !VarNumMap, !Counter) :-
    (
        MaybeAttribute = yes(Attribute),
        Attribute = user_attribute(_Locn, Var),
        add_var_to_var_number_map(VarSet, Var, !VarNumMap, !Counter)
    ;
        MaybeAttribute = no
    ).

:- pred add_var_to_var_number_map(prog_varset::in, prog_var::in,
    var_num_map::in, var_num_map::out, counter::in, counter::out) is det.

add_var_to_var_number_map(VarSet, Var, !VarNumMap, !Counter) :-
    ( if varset.search_name(VarSet, Var, VarName) then
        Name = VarName
    else
        Name = ""
    ),
    add_named_var_to_var_number_map(Var - Name, !VarNumMap, !Counter).

:- pred add_named_var_to_var_number_map(pair(prog_var, string)::in,
    var_num_map::in, var_num_map::out, counter::in, counter::out) is det.

add_named_var_to_var_number_map(Var - Name, !VarNumMap, !Counter) :-
    ( if map.search(!.VarNumMap, Var, _) then
        % Name shouldn't differ from the name recorded in !.VarNumMap.
        true
    else
        counter.allocate(VarNum, !Counter),
        map.det_insert(Var, VarNum - Name, !VarNumMap)
    ).

%---------------------------------------------------------------------------%

:- type exec_traces_info
    --->    exec_traces_info(
                % The arrays that hold (components of) exec trace structures.
                eti_next_proc_head_var_num      :: int,
                eti_next_proc_var_name          :: int,
                eti_next_proc_event_layout      :: int,
                eti_next_table_io_entry_data    :: counter,
                eti_next_exec_trace             :: counter,

                eti_rev_proc_head_var_nums      :: list(int),
                eti_rev_proc_var_names          :: list(int),
                eti_rev_proc_event_layouts      :: list(layout_slot_name),
                eti_rev_table_io_entry_datas    :: list(table_io_entry_data),
                eti_rev_exec_traces             :: list(
                                                    proc_layout_exec_trace),

                eti_table_io_entry_map          :: map(pred_proc_id,
                                                    layout_slot_name)
            ).

:- func init_exec_traces_info = exec_traces_info.

init_exec_traces_info = Info :-
    Info = exec_traces_info(0, 0, 0, counter.init(0), counter.init(0),
        [], [], [], [], [], map.init).

%---------------------------------------------------------------------------%
%
% Construct label layout structures.
%

    % Construct the layout describing a single internal label
    % for accurate GC and/or execution tracing.
    %
:- pred construct_internal_layout(stack_layout_params::in,
    proc_label::in, layout_name::in, var_num_map::in,
    pair(int, internal_layout_info)::in, internal_label_info::out,
    string_table_info::in, string_table_info::out,
    static_cell_info::in, static_cell_info::out,
    label_layouts_info::in, label_layouts_info::out) is det.

construct_internal_layout(Params, ProcLabel, ProcLayoutName, VarNumMap,
        LabelNum - Internal, LabelLayout,
        !StringTable, !StaticCellInfo, !LabelLayoutInfo) :-
    Internal = internal_layout_info(Trace, Resume, Return),
    (
        Trace = no,
        set.init(TraceLiveVarSet),
        map.init(TraceTypeVarMap),
        MaybeUserInfo = no
    ;
        Trace = yes(trace_port_layout_info(_, _, _, _, MaybeUserInfo,
            TraceLayout)),
        TraceLayout = layout_label_info(TraceLiveVarSet, TraceTypeVarMap)
    ),
    (
        Resume = no,
        set.init(ResumeLiveVarSet),
        map.init(ResumeTypeVarMap)
    ;
        Resume = yes(ResumeLayout),
        ResumeLayout = layout_label_info(ResumeLiveVarSet, ResumeTypeVarMap)
    ),
    (
        Trace = yes(trace_port_layout_info(_, Port, IsHidden, GoalPath, _, _)),
        Return = no,
        MaybePort = yes(Port),
        MaybeIsHidden = yes(IsHidden),
        GoalPathStr = goal_path_to_string(GoalPath),
        lookup_string_in_table(GoalPathStr, GoalPathNum, !StringTable),
        MaybeGoalPath = yes(GoalPathNum)
    ;
        Trace = no,
        Return = yes(ReturnInfo),
        % We only ever use the port fields of these layout structures
        % when we process exception events. (Since exception events are
        % interface events, the goal path field is not meaningful then.)
        MaybePort = yes(port_exception),
        MaybeIsHidden = yes(no),
        % We only ever use the goal path fields of these layout structures
        % when we process "fail" commands in the debugger.
        ReturnInfo = return_layout_info(TargetsContexts, _),
        ( if find_valid_return_context(TargetsContexts, _, _, GoalPath) then
            GoalPathStr = goal_path_to_string(GoalPath),
            lookup_string_in_table(GoalPathStr, GoalPathNum, !StringTable),
            MaybeGoalPath = yes(GoalPathNum)
        else
            % If tracing is enabled, then exactly one of the calls for which
            % this label is a return site would have had a valid context.
            % If none do, then tracing is not enabled, and therefore the goal
            % path of this label will not be accessed.
            MaybeGoalPath = no
        )
    ;
        Trace = no,
        Return = no,
        MaybePort = no,
        MaybeIsHidden = no,
        MaybeGoalPath = no
    ;
        Trace = yes(_),
        Return = yes(_),
        unexpected($pred, "label has both trace and return layout info")
    ),
    AgcStackLayout = Params ^ slp_agc_stack_layout,
    (
        Return = no,
        set.init(ReturnLiveVarSet),
        map.init(ReturnTypeVarMap)
    ;
        Return = yes(return_layout_info(_, ReturnLayout)),
        ReturnLayout = layout_label_info(ReturnLiveVarSet0, ReturnTypeVarMap),
        (
            AgcStackLayout = yes,
            ReturnLiveVarSet = ReturnLiveVarSet0
        ;
            AgcStackLayout = no,
            % This set of variables must be for uplevel printing in execution
            % tracing, so we are interested only in (a) variables, not
            % temporaries, (b) only named variables, and (c) only those
            % on the stack, not the return values.
            %
            % At the moment, we use ReturnTypeVarMap unchanged. Due to the
            % deletions done by this filtering, this map may be bigger than
            % necessary, but this fact does not compromise correctness.
            % We use the unchanged ReturnTypeVarMap to avoid having to scan
            % the types of all the selected layout_var_infos.
            set.filter(select_trace_return,
                ReturnLiveVarSet0, ReturnLiveVarSet)
        )
    ),
    ( if
        Trace = no,
        Resume = no,
        Return = no
    then
        MaybeVarInfo = no_var_info
    else
        % XXX Ignore differences in insts inside layout_var_infos.
        set.union(TraceLiveVarSet, ResumeLiveVarSet, LiveVarSet0),
        set.union(LiveVarSet0, ReturnLiveVarSet, LiveVarSet),
        map.union(set.intersect, TraceTypeVarMap, ResumeTypeVarMap,
            TypeVarMap0),
        map.union(set.intersect, TypeVarMap0, ReturnTypeVarMap, TypeVarMap),
        construct_label_var_info(Params, LiveVarSet, VarNumMap, TypeVarMap,
            MaybeVarInfo, !StaticCellInfo, !LabelLayoutInfo)
    ),
    (
        MaybeUserInfo = no,
        MaybeUserDataSlot = no
    ;
        MaybeUserInfo = yes(UserInfo),
        UserInfo = user_event_info(UserEventNumber, Attributes),
        construct_user_data_array(Params, VarNumMap, Attributes,
            UserLocnsArray, UserAttrMaybeVarNums, !StaticCellInfo),

        add_scalar_static_cell(UserLocnsArray, UserLocnsDataAddr,
            !StaticCellInfo),
        UserLocnsRval = const(llconst_data_addr(UserLocnsDataAddr, no)),

        list.length(UserAttrMaybeVarNums, NumVarNums),
        VarNumSlotNum0 = !.LabelLayoutInfo ^ lli_next_user_event_var_num,
        VarNumSlotNum = VarNumSlotNum0 + NumVarNums,
        !LabelLayoutInfo ^ lli_next_user_event_var_num := VarNumSlotNum,
        UserAttrVarNumsSlot = layout_slot(user_event_var_nums_array,
            VarNumSlotNum0),

        VarNums0 = !.LabelLayoutInfo ^ lli_user_event_var_nums,
        VarNums = VarNums0 ++ cord.from_list(UserAttrMaybeVarNums),
        !LabelLayoutInfo ^ lli_user_event_var_nums := VarNums,

        UserEventCounter0 = !.LabelLayoutInfo ^ lli_next_user_event,
        counter.allocate(UserEventSlotNum,
            UserEventCounter0, UserEventCounter),
        !LabelLayoutInfo ^ lli_next_user_event := UserEventCounter,
        UserEventSlot = layout_slot(user_event_layout_array, UserEventSlotNum),

        UserData = user_event_data(UserEventNumber, UserLocnsRval,
            UserAttrVarNumsSlot),
        UserEvents0 = !.LabelLayoutInfo ^ lli_user_events,
        UserEvents = UserEvents0 ++ cord.singleton(UserData),
        !LabelLayoutInfo ^ lli_user_events := UserEvents,

        MaybeUserDataSlot = yes(UserEventSlot)
    ),

    (
        Trace = yes(_),
        LabelNumCounter0 = !.LabelLayoutInfo ^ lli_label_counter,
        counter.allocate(LabelNumber0, LabelNumCounter0, LabelNumCounter),
        !LabelLayoutInfo ^ lli_label_counter := LabelNumCounter,

        % MR_ml_label_exec_count[0] is never written out; it is reserved for
        % cases like this, for labels without events, and for handwritten
        % labels.
        ( if LabelNumber0 < (1 << 16) then
            LabelNumber = LabelNumber0
        else
            LabelNumber = 0
        )
    ;
        Trace = no,
        LabelNumber = 0
    ),
    Label = internal_label(LabelNum, ProcLabel),
    BasicLayout = basic_label_layout(ProcLabel, LabelNum, ProcLayoutName,
        MaybePort, MaybeIsHidden, LabelNumber, MaybeGoalPath,
        MaybeUserDataSlot),
    (
        MaybeVarInfo = no_var_info,
        LabelVars = label_has_no_var_info,
        NoVarsLayout = label_layout_no_vars(BasicLayout),
        add_no_vars_internal_layout_data(Label, NoVarsLayout, Slot,
            !LabelLayoutInfo)
    ;
        MaybeVarInfo = short_var_info(SLayoutVarInfo),
        LabelVars = label_has_short_var_info,
        SVarsLayout = label_layout_short_vars(BasicLayout, SLayoutVarInfo),
        add_short_vars_internal_layout_data(Label, SVarsLayout, Slot,
            !LabelLayoutInfo)
    ;
        MaybeVarInfo = long_var_info(LLayoutVarInfo),
        LabelVars = label_has_long_var_info,
        LVarsLayout = label_layout_long_vars(BasicLayout, LLayoutVarInfo),
        add_long_vars_internal_layout_data(Label, LVarsLayout, Slot,
            !LabelLayoutInfo)
    ),
    LabelLayout = internal_label_info(ProcLabel, LabelNum, LabelVars, Slot,
        Internal).

:- pred add_no_vars_internal_layout_data(label::in, label_layout_no_vars::in,
    int::out, label_layouts_info::in, label_layouts_info::out) is det.

add_no_vars_internal_layout_data(Label, Layout, Slot, !LLI) :-
    RevLayouts0 = !.LLI ^ lli_rev_no_var_label_layouts,
    RevLayouts = [Layout | RevLayouts0],

    Counter0 = !.LLI ^ lli_next_no_var_label_layout,
    counter.allocate(Slot, Counter0, Counter),
    LayoutName = layout_slot(label_layout_array(label_has_no_var_info), Slot),

    LabelToLayoutMap0 = !.LLI ^ lli_i_label_to_layout_map,
    map.det_insert(Label, LayoutName, LabelToLayoutMap0, LabelToLayoutMap),

    !LLI ^ lli_rev_no_var_label_layouts := RevLayouts,
    !LLI ^ lli_next_no_var_label_layout := Counter,
    !LLI ^ lli_i_label_to_layout_map := LabelToLayoutMap.

:- pred add_short_vars_internal_layout_data(label::in,
    label_layout_short_vars::in, int::out,
    label_layouts_info::in, label_layouts_info::out) is det.

add_short_vars_internal_layout_data(Label, Layout, Slot, !LLI) :-
    RevLayouts0 = !.LLI ^ lli_rev_svar_label_layouts,
    RevLayouts = [Layout | RevLayouts0],

    Counter0 = !.LLI ^ lli_next_svar_label_layout,
    counter.allocate(Slot, Counter0, Counter),
    LayoutArray = label_layout_array(label_has_short_var_info),
    LayoutName = layout_slot(LayoutArray, Slot),

    LabelToLayoutMap0 = !.LLI ^ lli_i_label_to_layout_map,
    map.det_insert(Label, LayoutName, LabelToLayoutMap0, LabelToLayoutMap),

    !LLI ^ lli_rev_svar_label_layouts := RevLayouts,
    !LLI ^ lli_next_svar_label_layout := Counter,
    !LLI ^ lli_i_label_to_layout_map := LabelToLayoutMap.

:- pred add_long_vars_internal_layout_data(label::in,
    label_layout_long_vars::in, int::out,
    label_layouts_info::in, label_layouts_info::out) is det.

add_long_vars_internal_layout_data(Label, Layout, Slot, !LLI) :-
    RevLayouts0 = !.LLI ^ lli_rev_lvar_label_layouts,
    RevLayouts = [Layout | RevLayouts0],

    Counter0 = !.LLI ^ lli_next_lvar_label_layout,
    counter.allocate(Slot, Counter0, Counter),
    LayoutArray = label_layout_array(label_has_long_var_info),
    LayoutName = layout_slot(LayoutArray, Slot),

    LabelToLayoutMap0 = !.LLI ^ lli_i_label_to_layout_map,
    map.det_insert(Label, LayoutName, LabelToLayoutMap0, LabelToLayoutMap),

    !LLI ^ lli_rev_lvar_label_layouts := RevLayouts,
    !LLI ^ lli_next_lvar_label_layout := Counter,
    !LLI ^ lli_i_label_to_layout_map := LabelToLayoutMap.

:- pred construct_user_data_array(stack_layout_params::in, var_num_map::in,
    list(maybe(user_attribute))::in,
    list(typed_rval)::out, list(maybe(int))::out,
    static_cell_info::in, static_cell_info::out) is det.

construct_user_data_array(_, _, [], [], [], !Info).
construct_user_data_array(Params, VarNumMap, [MaybeAttr | MaybeAttrs],
        [LocnTypedRval | LocnTypedRvals], [MaybeVarNum | MaybeVarNums],
        !StaticCellInfo) :-
    (
        MaybeAttr = yes(Attr),
        Attr = user_attribute(Locn, Var),
        represent_locn_or_const_as_int_rval(Params, Locn, LocnRval,
            LocnRvalType, !StaticCellInfo),
        LocnTypedRval = typed_rval(LocnRval, LocnRvalType),
        convert_var_to_int(VarNumMap, Var, VarNum),
        MaybeVarNum = yes(VarNum)
    ;
        MaybeAttr = no,
        LocnTypedRval = typed_rval(const(llconst_int(0)),
            lt_int(int_type_uint)),
        MaybeVarNum = no
    ),
    construct_user_data_array(Params, VarNumMap, MaybeAttrs,
        LocnTypedRvals, MaybeVarNums, !StaticCellInfo).

%---------------------------------------------------------------------------%

    % Given a list of layout_var_infos and the type variables that occur
    % in them, select only the layout_var_infos that may be required
    % by up-level printing in the trace-based debugger.
    %
:- pred select_trace_return(layout_var_info::in) is semidet.

select_trace_return(LocnInfo) :-
    LocnInfo = layout_var_info(Locn, LvalType, _),
    LvalType = live_value_var(_, Name, _, _),
    Name \= "",
    (
        Locn = locn_direct(Lval)
    ;
        Locn = locn_indirect(Lval, _)
    ),
    (
        Lval = stackvar(_)
    ;
        Lval = framevar(_)
    ;
        Lval = double_stackvar(double_stackvar, _)
    ).

    % Given a list of layout_var_infos, put the ones that tracing can be
    % interested in (whether at an internal port or for uplevel printing)
    % in a block at the start, and both this block and the remaining
    % block. The division into two blocks can make the job of the
    % debugger somewhat easier, the sorting of the named var block makes
    % the output of the debugger look nicer, and the sorting of the both
    % blocks makes it more likely that different labels' layout structures
    % will have common parts (e.g. name vectors).
    %
:- pred sort_livevals(list(layout_var_info)::in, list(layout_var_info)::out)
    is det.

sort_livevals(OrigInfos, FinalInfos) :-
    IsNamedVar = (pred(LvalInfo::in) is semidet :-
        LvalInfo = layout_var_info(_Lval, LvalType, _),
        LvalType = live_value_var(_, Name, _, _),
        Name \= ""
    ),
    list.filter(IsNamedVar, OrigInfos, NamedVarInfos0, OtherInfos0),
    CompareVarInfos = (pred(Var1::in, Var2::in, Result::out) is det :-
        Var1 = layout_var_info(Lval1, LiveType1, _),
        Var2 = layout_var_info(Lval2, LiveType2, _),
        get_name_from_live_value_type(LiveType1, Name1),
        get_name_from_live_value_type(LiveType2, Name2),
        compare(NameResult, Name1, Name2),
        (
            NameResult = (=),
            compare(Result, Lval1, Lval2)
        ;
            ( NameResult = (<)
            ; NameResult = (>)
            ),
            Result = NameResult
        )
    ),
    list.sort(CompareVarInfos, NamedVarInfos0, NamedVarInfos),
    list.sort(CompareVarInfos, OtherInfos0, OtherInfos),
    FinalInfos = NamedVarInfos ++ OtherInfos.

:- pred get_name_from_live_value_type(live_value_type::in,
    string::out) is det.

get_name_from_live_value_type(LiveType, Name) :-
    ( if LiveType = live_value_var(_, NamePrime, _, _) then
        Name = NamePrime
    else
        Name = ""
    ).

%---------------------------------------------------------------------------%

    % Given a association list of type variables and their locations
    % sorted on the type variables, represent them in an array of
    % location descriptions indexed by the type variable. The next
    % slot to fill is given by the second argument.
    %
:- pred construct_type_param_locn_vector(
    assoc_list(tvar, set(layout_locn))::in,
    int::in, list(typed_rval)::out) is det.

construct_type_param_locn_vector([], _, []).
construct_type_param_locn_vector([TVar - Locns | TVarLocns], CurSlot,
        Vector) :-
    term.var_to_int(TVar, TVarNum),
    NextSlot = CurSlot + 1,
    ( if TVarNum = CurSlot then
        ( if set.remove_least(LeastLocn, Locns, _) then
            Locn = LeastLocn
        else
            unexpected($pred, "tvar has empty set of locations")
        ),
        represent_locn_as_int_rval(Locn, Rval),
        construct_type_param_locn_vector(TVarLocns, NextSlot, VectorTail),
        Vector = [typed_rval(Rval, lt_int(int_type_uint)) | VectorTail]
    else if TVarNum > CurSlot then
        construct_type_param_locn_vector([TVar - Locns | TVarLocns], NextSlot,
            VectorTail),
        % This slot will never be referred to.
        Vector = [typed_rval(const(llconst_int(0)),
            lt_int(int_type_uint)) | VectorTail]
    else
        unexpected($pred, "unsorted tvars")
    ).

%---------------------------------------------------------------------------%

:- type liveval_array_slot
    --->    liveval_array_slot(
                % This rval is the pseudo_type_info for the type of the
                % live value.
                lai_type            :: rval,

                % This is the variable number of a live value. Contains zero
                % if the live value is not a variable, and contains the
                % hightest possible uint_least16 value if the variable number
                % does not fit in 16 bits.
                lai_hlds_var_num    :: int,

                % This describes the location of the live value as either
                % a MR_LongLval or MR_ShortLval.
                lai_locn_desc       :: int
            ).

:- func project_array_slot_pti(liveval_array_slot) = rval.

project_array_slot_pti(liveval_array_slot(PTI, _, _)) = PTI.

:- func project_array_slot_hlds_var_num(liveval_array_slot) = int.

project_array_slot_hlds_var_num(liveval_array_slot(_, VarNum, _)) = VarNum.

:- func project_array_slot_locn(liveval_array_slot) = int.

project_array_slot_locn(liveval_array_slot(_, _, Locn)) = Locn.

%---------------------------------------------------------------------------%

:- type maybe_var_info
    --->    no_var_info
    ;       short_var_info(label_short_var_info)
    ;       long_var_info(label_long_var_info).

:- pred construct_label_var_info(stack_layout_params::in,
    set(layout_var_info)::in, var_num_map::in, map(tvar, set(layout_locn))::in,
    maybe_var_info::out, static_cell_info::in, static_cell_info::out,
    label_layouts_info::in, label_layouts_info::out) is det.

construct_label_var_info(Params, VarInfoSet, VarNumMap, TVarLocnMap,
        MaybeVarInfo, !StaticCellInfo, !LabelLayoutInfo) :-
    construct_tvar_vector(TVarLocnMap, TypeParamsRval, !StaticCellInfo),

    set.to_sorted_list(VarInfoSet, VarInfos0),
    sort_livevals(VarInfos0, VarInfos),

    int.pow(2, short_count_bits, BytesLimit),
    ModuleInfo = Params ^ slp_module_info,
    construct_liveval_array_slots(VarInfos, ModuleInfo, VarNumMap,
        0, BytesLimit, LongArrayInfos, ShortArrayInfos, !StaticCellInfo),

    AllArrayInfos = LongArrayInfos ++ ShortArrayInfos,
    PTIs = list.map(project_array_slot_pti, AllArrayInfos),
    HLDSVarNums = list.map(project_array_slot_hlds_var_num, AllArrayInfos),
    ShortLocns = list.map(project_array_slot_locn, ShortArrayInfos),
    LongLocns = list.map(project_array_slot_locn, LongArrayInfos),

    list.length(PTIs, NumPTIs),
    list.length(HLDSVarNums, NumHLDSVarNums),
    list.length(ShortLocns, NumShortLocns),
    list.length(LongLocns, NumLongLocns),
    expect(unify(NumPTIs, NumHLDSVarNums), $pred,
        "NumPTIs != NumHLDSVarNums"),
    expect(unify(NumPTIs, NumLongLocns + NumShortLocns), $pred,
        "NumPTIs != NumLongLocns + NumShortLocns"),

    EncodedLength = NumLongLocns << short_count_bits + NumShortLocns,

    % XXX As an optimization, instead of always adding PTIs, HLDSVarNums,
    % ShortLocns and LongLocns to their respective arrays, we could see
    % whether those arrays already contain the required sequences.

    CompressArrays = Params ^ slp_compress_arrays,
    ( if NumPTIs > 0 then
        AllRevPTIs0 = !.LabelLayoutInfo ^ lli_rev_ptis,
        NextPTISlot0 = !.LabelLayoutInfo ^ lli_next_pti,
        list.reverse(PTIs, RevPTIs),
        ( if
            some [CompressionLimit] (
                CompressArrays = yes(CompressionLimit),
                !.LabelLayoutInfo ^ lli_next_pti =< CompressionLimit
            ),
            find_sequence(RevPTIs, AllRevPTIs0, 0, OldPTIOffset)
        then
            PTISlot = NextPTISlot0 - OldPTIOffset - NumPTIs
        else
            AllRevPTIs = RevPTIs ++ AllRevPTIs0,
            !LabelLayoutInfo ^ lli_rev_ptis := AllRevPTIs,

            PTISlot = NextPTISlot0,
            NextPTISlot = NextPTISlot0 + NumPTIs,
            !LabelLayoutInfo ^ lli_next_pti := NextPTISlot
        )
    else
        PTISlot = -1
    ),
    % The garbage collector does not need HLDS variable numbers; only the
    % debugger does.
    ( if
        NumHLDSVarNums > 0,
        Params ^ slp_trace_stack_layout = yes
    then
        AllRevHLDSVarNums0 = !.LabelLayoutInfo ^ lli_rev_hlds_var_nums,
        NextHLDSVarNumSlot0 = !.LabelLayoutInfo ^ lli_next_hlds_var_num,
        list.reverse(HLDSVarNums, RevHLDSVarNums),
        ( if
            some [CompressionLimit] (
                CompressArrays = yes(CompressionLimit),
                !.LabelLayoutInfo ^ lli_next_hlds_var_num =< CompressionLimit
            ),
            find_sequence(RevHLDSVarNums, AllRevHLDSVarNums0,
                0, OldHLDSVarNumsOffset)
        then
            HLDSVarNumSlot = NextHLDSVarNumSlot0 - OldHLDSVarNumsOffset -
                NumHLDSVarNums
        else
            AllRevHLDSVarNums = RevHLDSVarNums ++ AllRevHLDSVarNums0,
            !LabelLayoutInfo ^ lli_rev_hlds_var_nums := AllRevHLDSVarNums,

            HLDSVarNumSlot = NextHLDSVarNumSlot0,
            NextHLDSVarNumSlot = NextHLDSVarNumSlot0 + NumHLDSVarNums,
            !LabelLayoutInfo ^ lli_next_hlds_var_num := NextHLDSVarNumSlot
        )
    else
        HLDSVarNumSlot = -1
    ),
    ( if NumShortLocns > 0 then
        AllRevShortLocns0 = !.LabelLayoutInfo ^ lli_rev_short_locns,
        NextShortLocnSlot0 = !.LabelLayoutInfo ^ lli_next_short_locn,
        list.reverse(ShortLocns, RevShortLocns),
        ( if
            some [CompressionLimit] (
                CompressArrays = yes(CompressionLimit),
                !.LabelLayoutInfo ^ lli_next_short_locn =< CompressionLimit
            ),
            find_sequence(RevShortLocns, AllRevShortLocns0,
                0, OldShortLocnsOffset)
        then
            ShortLocnSlot = NextShortLocnSlot0 - OldShortLocnsOffset -
                NumShortLocns
        else
            AllRevShortLocns = RevShortLocns ++ AllRevShortLocns0,
            !LabelLayoutInfo ^ lli_rev_short_locns := AllRevShortLocns,

            ShortLocnSlot = NextShortLocnSlot0,
            NextShortLocnSlot = NextShortLocnSlot0 + NumShortLocns,
            !LabelLayoutInfo ^ lli_next_short_locn := NextShortLocnSlot
        )
    else
        ShortLocnSlot = -1
    ),
    ( if NumLongLocns > 0 then
        AllRevLongLocns0 = !.LabelLayoutInfo ^ lli_rev_long_locns,
        NextLongLocnSlot0 = !.LabelLayoutInfo ^ lli_next_long_locn,
        list.reverse(LongLocns, RevLongLocns),
        ( if
            some [CompressionLimit] (
                CompressArrays = yes(CompressionLimit),
                !.LabelLayoutInfo ^ lli_next_long_locn =< CompressionLimit
            ),
            find_sequence(RevLongLocns, AllRevLongLocns0,
                0, OldLongLocnsOffset)
        then
            LongLocnSlot = NextLongLocnSlot0 - OldLongLocnsOffset -
                NumLongLocns
        else
            AllRevLongLocns = RevLongLocns ++ AllRevLongLocns0,
            !LabelLayoutInfo ^ lli_rev_long_locns := AllRevLongLocns,

            LongLocnSlot = NextLongLocnSlot0,
            NextLongLocnSlot = NextLongLocnSlot0 + NumLongLocns,
            !LabelLayoutInfo ^ lli_next_long_locn := NextLongLocnSlot
        ),

        LVarInfo = label_long_var_info(EncodedLength, TypeParamsRval,
            PTISlot, HLDSVarNumSlot, ShortLocnSlot, LongLocnSlot),
        MaybeVarInfo = long_var_info(LVarInfo)
    else
        SVarInfo = label_short_var_info(EncodedLength, TypeParamsRval,
            PTISlot, HLDSVarNumSlot, ShortLocnSlot),
        MaybeVarInfo = short_var_info(SVarInfo)
    ).

:- pred construct_liveval_array_slots(list(layout_var_info)::in,
    module_info::in, var_num_map::in, int::in, int::in,
    list(liveval_array_slot)::out, list(liveval_array_slot)::out,
    static_cell_info::in, static_cell_info::out) is det.

construct_liveval_array_slots([], _, _, _, _, [], [], !Info).
construct_liveval_array_slots([VarInfo | VarInfos], ModuleInfo, VarNumMap,
        BytesSoFar, BytesLimit, LongArraySlots, ShortArraySlots,
        !StaticCellInfo) :-
    VarInfo = layout_var_info(Locn, LiveValueType, _),
    represent_live_value_type_and_var_num(VarNumMap, LiveValueType,
        TypeRval, VarNum, !StaticCellInfo),
    ( if
        LiveValueType = live_value_var(_, _, Type, _),
        is_type_a_dummy(ModuleInfo, Type) = is_dummy_type,
        % We want to preserve I/O states in registers.
        not (
            Locn = locn_direct(reg(_, _))
        )
    then
        unexpected($pred, "unexpected reference to dummy value")
    else if
        BytesSoFar < BytesLimit,
        represent_locn_as_byte(Locn, ShortLocn)
    then
        ArraySlot = liveval_array_slot(TypeRval, VarNum, ShortLocn),
        construct_liveval_array_slots(VarInfos, ModuleInfo, VarNumMap,
            BytesSoFar + 1, BytesLimit, LongArraySlots, ShortArraySlots0,
            !StaticCellInfo),
        ShortArraySlots = [ArraySlot | ShortArraySlots0]
    else
        represent_locn_as_int(Locn, LongLocn),
        ArraySlot = liveval_array_slot(TypeRval, VarNum, LongLocn),
        construct_liveval_array_slots(VarInfos, ModuleInfo, VarNumMap,
            BytesSoFar, BytesLimit, LongArraySlots0, ShortArraySlots,
            !StaticCellInfo),
        LongArraySlots = [ArraySlot | LongArraySlots0]
    ).

:- pred construct_tvar_vector(map(tvar, set(layout_locn))::in,
    rval::out, static_cell_info::in, static_cell_info::out) is det.

construct_tvar_vector(TVarLocnMap, TypeParamRval, !StaticCellInfo) :-
    ( if map.is_empty(TVarLocnMap) then
        TypeParamRval = const(llconst_int(0))
    else
        construct_tvar_rvals(TVarLocnMap, Vector),
        add_scalar_static_cell(Vector, DataAddr, !StaticCellInfo),
        TypeParamRval = const(llconst_data_addr(DataAddr, no))
    ).

:- pred construct_tvar_rvals(map(tvar, set(layout_locn))::in,
    list(typed_rval)::out) is det.

construct_tvar_rvals(TVarLocnMap, Vector) :-
    map.to_assoc_list(TVarLocnMap, TVarLocns),
    construct_type_param_locn_vector(TVarLocns, 1, TypeParamLocs),
    list.length(TypeParamLocs, TypeParamsLength),
    LengthRval = const(llconst_int(TypeParamsLength)),
    Vector = [typed_rval(LengthRval, lt_int(int_type_uint)) | TypeParamLocs].

%---------------------------------------------------------------------------%
%
% Construct closure layout structures.
%

construct_closure_layout(CallerProcLabel, SeqNo,
        ClosureLayoutInfo, ClosureProcLabel, ModuleName,
        FileName, LineNumber, Origin, GoalPath, !StaticCellInfo,
        TypedRvals, Data) :-
    % The representation we build here should be kept in sync
    % with runtime/mercury_ho_call.h, which contains macros to access
    % the data structures we build here.
    %
    ClosureId = closure_proc_id(CallerProcLabel, SeqNo, ClosureProcLabel),
    DataId = layout_id(ClosureId),
    Data = closure_proc_id_data(CallerProcLabel, SeqNo, ClosureProcLabel,
        ModuleName, FileName, LineNumber, Origin, GoalPath),
    ProcIdRval = const(llconst_data_addr(DataId, no)),
    ProcIdTypedRval = typed_rval(ProcIdRval, lt_data_ptr),
    ClosureLayoutInfo = closure_layout_info(ClosureArgs, TVarLocnMap),
    construct_closure_arg_rvals(ClosureArgs,
        ClosureArgTypedRvals, !StaticCellInfo),
    construct_tvar_vector(TVarLocnMap, TVarVectorRval, !StaticCellInfo),
    TVarVectorTypedRval = typed_rval(TVarVectorRval, lt_data_ptr),
    TypedRvals = [ProcIdTypedRval, TVarVectorTypedRval | ClosureArgTypedRvals].

:- pred construct_closure_arg_rvals(list(closure_arg_info)::in,
    list(typed_rval)::out, static_cell_info::in, static_cell_info::out) is det.

construct_closure_arg_rvals(ClosureArgs, ClosureArgTypedRvals,
        !StaticCellInfo) :-
    list.map_foldl(construct_closure_arg_rval, ClosureArgs, ArgTypedRvals,
        !StaticCellInfo),
    list.length(ArgTypedRvals, Length),
    LengthTypedRval = typed_rval(const(llconst_int(Length)),
        lt_int(int_type_int)),
    ClosureArgTypedRvals = [LengthTypedRval| ArgTypedRvals].

:- pred construct_closure_arg_rval(closure_arg_info::in,
    typed_rval::out, static_cell_info::in, static_cell_info::out) is det.

construct_closure_arg_rval(ClosureArg, typed_rval(ArgRval, ArgRvalType),
        !StaticCellInfo) :-
    ClosureArg = closure_arg_info(Type, _Inst),
    % For a stack layout, we can treat all type variables as universally
    % quantified. This is not the argument of a constructor, so we do not need
    % to distinguish between type variables that are and aren't in scope;
    % we can take the variable number directly from the procedure's tvar set.
    ExistQTvars = [],
    NumUnivQTvars = -1,
    ll_pseudo_type_info.construct_typed_llds_pseudo_type_info(Type,
        NumUnivQTvars, ExistQTvars, !StaticCellInfo, ArgRval, ArgRvalType).

%---------------------------------------------------------------------------%

    % Construct a representation of the type of a value.
    %
    % For values representing variables, this will be a pseudo_type_info
    % describing the type of the variable.
    %
    % For the kinds of values used internally by the compiler,
    % this will be a pointer to a specific type_ctor_info (acting as a
    % type_info) defined by hand in builtin.m to stand for values of
    % each such kind; one for succips, one for hps, etc.
    %
:- pred represent_live_value_type_and_var_num(var_num_map::in,
    live_value_type::in, rval::out, int::out,
    static_cell_info::in, static_cell_info::out) is det.

represent_live_value_type_and_var_num(VarNumMap, LiveValueType, TypeRval,
        VarNum, !StaticCellInfo) :-
    (
        (
            LiveValueType = live_value_succip,
            Name = "succip"
        ;
            LiveValueType = live_value_hp,
            Name = "hp"
        ;
            LiveValueType = live_value_curfr,
            Name = "curfr"
        ;
            LiveValueType = live_value_maxfr,
            Name = "maxfr"
        ;
            LiveValueType = live_value_redofr,
            Name = "redofr"
        ;
            LiveValueType = live_value_redoip,
            Name = "redoip"
        ;
            LiveValueType = live_value_trail_ptr,
            Name = "trailptr"
        ;
            LiveValueType = live_value_ticket,
            Name = "ticket"
        ;
            % Neither the garbage collector nor the debugger need info about
            % regions.
            LiveValueType = live_value_region_ite,
            Name = "unwanted"
        ;
            LiveValueType = live_value_region_disj,
            Name = "unwanted"
        ;
            LiveValueType = live_value_region_commit,
            Name = "unwanted"
        ;
            LiveValueType = live_value_unwanted,
            Name = "unwanted"
        ),
        represent_special_live_value_type(Name, TypeRval),
        VarNum = 0
    ;
        LiveValueType = live_value_var(Var, _, Type, _),
        % For a stack layout, we can treat all type variables as universally
        % quantified. This is not the argument of a constructor, so we do not
        % need to distinguish between type variables that are and are not
        % in scope; we can take the variable number directly from the
        % procedure's tvar set.
        ExistQTvars = [],
        NumUnivQTvars = -1,
        ll_pseudo_type_info.construct_llds_pseudo_type_info(Type,
            NumUnivQTvars, ExistQTvars, !StaticCellInfo, TypeRval),
        convert_var_to_int(VarNumMap, Var, VarNum)
    ).

:- pred represent_special_live_value_type(string::in, rval::out) is det.

represent_special_live_value_type(SpecialTypeName, Rval) :-
    RttiTypeCtor = rtti_type_ctor(unqualified(""), SpecialTypeName, 0),
    DataId =
        rtti_data_id(ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info)),
    Rval = const(llconst_data_addr(DataId, no)).

:- pred convert_var_to_int(var_num_map::in, prog_var::in, int::out) is det.

convert_var_to_int(VarNumMap, Var, VarNum) :-
    map.lookup(VarNumMap, Var, VarNum0 - _),
    % The variable number has to fit into two bytes. We reserve the largest
    % such number (Limit) to mean that the variable number is too large
    % to be represented. This ought not to happen, since compilation
    % would be glacial at best for procedures with that many variables.
    Limit = (1 << (2 * byte_bits)) - 1,
    int.min(VarNum0, Limit, VarNum).

%---------------------------------------------------------------------------%

:- pred represent_locn_or_const_as_int_rval(stack_layout_params::in,
    rval::in, rval::out, llds_type::out,
    static_cell_info::in, static_cell_info::out) is det.

represent_locn_or_const_as_int_rval(Params, LvalOrConst, Rval, Type,
        !StaticCellInfo) :-
    (
        LvalOrConst = lval(Lval),
        represent_locn_as_int_rval(locn_direct(Lval), Rval),
        Type = lt_int(int_type_uint)
    ;
        LvalOrConst = const(_Const),
        UnboxedFloats = Params ^ slp_unboxed_floats,
        UnboxedInt64s = Params ^ slp_unboxed_int64s,
        NumWords = one_word,
        LLDSType = rval_type_as_arg(UnboxedFloats, UnboxedInt64s, NumWords,
            LvalOrConst),
        add_scalar_static_cell([typed_rval(LvalOrConst, LLDSType)], DataId,
            !StaticCellInfo),
        Rval = const(llconst_data_addr(DataId, no)),
        Type = lt_data_ptr
    ;
        LvalOrConst = mkword(Tag, LvalOrConstBase),
        represent_locn_or_const_as_int_rval(Params, LvalOrConstBase, BaseRval,
            Type, !StaticCellInfo),
        Rval = mkword(Tag, BaseRval)
    ;
        ( LvalOrConst = cast(_, _)
        ; LvalOrConst = unop(_, _)
        ; LvalOrConst = binop(_, _, _)
        ; LvalOrConst = mem_addr(_)
        ; LvalOrConst = var(_)
        ; LvalOrConst = mkword_hole(_)
        ),
        unexpected($pred, "bad rval")
    ).

%---------------------------------------------------------------------------%

    % Construct a representation of a variable location as a 32-bit integer.
    %
    % Most of the time, a layout specifies a location as an lval.
    % However, a type_info variable may be hidden inside a typeclass_info,
    % In this case, accessing the type_info requires indirection.
    % The address of the typeclass_info is given as an lval, and
    % the location of the typeinfo within the typeclass_info as an index;
    % private_builtin.type_info_from_typeclass_info interprets the index.
    %
    % This one level of indirection is sufficient, since type_infos
    % cannot be nested inside typeclass_infos any deeper than this.
    % A more general representation that would allow more indirection
    % would be much harder to fit into one machine word.
    %
:- pred represent_locn_as_int_rval(layout_locn::in, rval::out) is det.

represent_locn_as_int_rval(Locn, Rval) :-
    represent_locn_as_int(Locn, Word),
    Rval = const(llconst_int(Word)).

represent_locn_as_int(locn_direct(Lval), Word) :-
    represent_lval(Lval, Word).
represent_locn_as_int(locn_indirect(Lval, Offset), Word) :-
    represent_lval(Lval, BaseWord),
    expect((1 << long_lval_offset_bits) > Offset, $pred,
        "offset too large to be represented"),
    BaseAndOffset = (BaseWord << long_lval_offset_bits) + Offset,
    make_tagged_word(lval_indirect, BaseAndOffset, Word).

    % Construct a four byte representation of an lval.
    %
:- pred represent_lval(lval::in, int::out) is det.

represent_lval(reg(reg_r, Num), Word) :-
    make_tagged_word(lval_r_reg, Num, Word).
represent_lval(reg(reg_f, Num), Word) :-
    make_tagged_word(lval_f_reg, Num, Word).
represent_lval(stackvar(Num), Word) :-
    expect(Num > 0, $pred, "bad stackvar"),
    make_tagged_word(lval_stackvar, Num, Word).
represent_lval(parent_stackvar(Num), Word) :-
    expect(Num > 0, $pred, "bad parent_stackvar"),
    make_tagged_word(lval_parent_stackvar, Num, Word).
represent_lval(framevar(Num), Word) :-
    expect(Num > 0, $pred, "bad framevar"),
    make_tagged_word(lval_framevar, Num, Word).
represent_lval(double_stackvar(StackType, Num), Word) :-
    expect(Num > 0, $pred, "bad stackvar"),
    (
        StackType = double_stackvar,
        make_tagged_word(lval_double_stackvar, Num, Word)
    ;
        StackType = double_parent_stackvar,
        make_tagged_word(lval_double_parent_stackvar, Num, Word)
    ).
represent_lval(succip, Word) :-
    make_tagged_word(lval_succip, 0, Word).
represent_lval(maxfr, Word) :-
    make_tagged_word(lval_maxfr, 0, Word).
represent_lval(curfr, Word) :-
    make_tagged_word(lval_curfr, 0, Word).
represent_lval(hp, Word) :-
    make_tagged_word(lval_hp, 0, Word).
represent_lval(sp, Word) :-
    make_tagged_word(lval_sp, 0, Word).
represent_lval(parent_sp, Word) :-
    make_tagged_word(lval_parent_sp, 0, Word).

represent_lval(temp(_, _), _) :-
    unexpected($pred, "continuation live value stored in temp register").

represent_lval(succip_slot(_), _) :-
    unexpected($pred, "continuation live value stored in fixed slot").
represent_lval(redoip_slot(_), _) :-
    unexpected($pred, "continuation live value stored in fixed slot").
represent_lval(redofr_slot(_), _) :-
    unexpected($pred, "continuation live value stored in fixed slot").
represent_lval(succfr_slot(_), _) :-
    unexpected($pred, "continuation live value stored in fixed slot").
represent_lval(prevfr_slot(_), _) :-
    unexpected($pred, "continuation live value stored in fixed slot").

represent_lval(field(_, _, _), _) :-
    unexpected($pred, "continuation live value stored in field").
represent_lval(mem_ref(_), _) :-
    unexpected($pred, "continuation live value stored in mem_ref").
represent_lval(global_var_ref(_), _) :-
    unexpected($pred,
        "continuation live value stored in global_var_ref").
represent_lval(lvar(_), _) :-
    unexpected($pred, "continuation live value stored in lvar").

    % Some things in this module are encoded using a low tag. This is not done
    % using the normal compiler mkword, but by doing the bit shifting here.
    %
    % This allows us to use more than the usual 2 or 3 bits, but we have to
    % use low tags and cannot tag pointers this way.
    %
:- pred make_tagged_word(locn_type::in, int::in, int::out) is det.

make_tagged_word(Locn, Value, TaggedValue) :-
    locn_type_code(Locn, Tag),
    TaggedValue = (Value << long_lval_tag_bits) + Tag.

:- type locn_type
    --->    lval_r_reg
    ;       lval_f_reg
    ;       lval_stackvar
    ;       lval_framevar
    ;       lval_succip
    ;       lval_maxfr
    ;       lval_curfr
    ;       lval_hp
    ;       lval_sp
    ;       lval_indirect
    ;       lval_parent_sp
    ;       lval_parent_stackvar
    ;       lval_double_stackvar
    ;       lval_double_parent_stackvar
    ;       lval_double_framevar.

:- pred locn_type_code(locn_type::in, int::out) is det.

% The code of this predicate should be kept in sync with the enum type
% MR_LongLvalType in runtime/mercury_stack_layout.h. Note that the values
% equal to 0 modulo 4 are reserved for representing constants (aligned
% pointers to static data).

locn_type_code(lval_r_reg,           1).
locn_type_code(lval_f_reg,           2).
locn_type_code(lval_stackvar,        3).
locn_type_code(lval_parent_stackvar, 3).     % XXX placeholder only
locn_type_code(lval_framevar,        5).
locn_type_code(lval_succip,          6).
locn_type_code(lval_maxfr,           7).
locn_type_code(lval_curfr,           9).
locn_type_code(lval_hp,              10).
locn_type_code(lval_sp,              11).
locn_type_code(lval_parent_sp,       11).    % XXX placeholder only
locn_type_code(lval_double_stackvar, 13).
locn_type_code(lval_double_parent_stackvar, 13). % XXX placeholder only
locn_type_code(lval_double_framevar, 14).    % unused now
locn_type_code(lval_indirect,        15).

    % This number of tag bits must be able to encode all values of
    % locn_type_code.
    %
:- func long_lval_tag_bits = int.

long_lval_tag_bits = 5.

    % This number of tag bits must be able to encode the largest offset
    % of a type_info within a typeclass_info.
    %
:- func long_lval_offset_bits = int.

long_lval_offset_bits = 6.

%---------------------------------------------------------------------------%

    % Construct a representation of a variable location as a byte,
    % if this is possible.
    %
:- pred represent_locn_as_byte(layout_locn::in, int::out) is semidet.

represent_locn_as_byte(LayoutLocn, Byte) :-
    LayoutLocn = locn_direct(Lval),
    represent_lval_as_byte(Lval, Byte),
    0 =< Byte,
    Byte < 256.

    % Construct a representation of an lval in a byte, if possible.
    %
:- pred represent_lval_as_byte(lval::in, int::out) is semidet.

represent_lval_as_byte(reg(reg_r, Num), Byte) :-
    expect(Num > 0, $pred, "bad reg"),
    make_tagged_byte(0, Num, Byte).
represent_lval_as_byte(stackvar(Num), Byte) :-
    expect(Num > 0, $pred, "bad stackvar"),
    make_tagged_byte(1, Num, Byte).
represent_lval_as_byte(parent_stackvar(Num), Byte) :-
    expect(Num > 0, $pred, "bad parent_stackvar"),
    make_tagged_byte(1, Num, Byte). % XXX placeholder only
represent_lval_as_byte(framevar(Num), Byte) :-
    expect(Num > 0, $pred, "bad framevar"),
    make_tagged_byte(2, Num, Byte).
represent_lval_as_byte(succip, Byte) :-
    locn_type_code(lval_succip, Val),
    make_tagged_byte(3, Val, Byte).
represent_lval_as_byte(maxfr, Byte) :-
    locn_type_code(lval_maxfr, Val),
    make_tagged_byte(3, Val, Byte).
represent_lval_as_byte(curfr, Byte) :-
    locn_type_code(lval_curfr, Val),
    make_tagged_byte(3, Val, Byte).
represent_lval_as_byte(hp, Byte) :-
    locn_type_code(lval_hp, Val),
    make_tagged_byte(3, Val, Byte).
represent_lval_as_byte(sp, Byte) :-
    locn_type_code(lval_sp, Val),
    make_tagged_byte(3, Val, Byte).
represent_lval_as_byte(parent_sp, Byte) :-
    locn_type_code(lval_parent_sp, Val),
    make_tagged_byte(3, Val, Byte). % XXX placeholder only

:- pred make_tagged_byte(int::in, int::in, int::out) is det.

make_tagged_byte(Tag, Value, TaggedValue) :-
    TaggedValue = unchecked_left_shift(Value, short_lval_tag_bits) + Tag.

:- func short_lval_tag_bits = int.

short_lval_tag_bits = 2.

:- func short_count_bits = int.

short_count_bits = 10.

:- func byte_bits = int.

byte_bits = 8.

%---------------------------------------------------------------------------%

:- type label_layouts_info
    --->    label_layouts_info(
                % The arrays that hold components of label layouts.
                lli_next_pti                :: int,
                lli_next_long_locn          :: int,
                lli_next_short_locn         :: int,
                lli_next_hlds_var_num       :: int,
                lli_rev_ptis                :: list(rval),
                lli_rev_long_locns          :: list(int),
                lli_rev_short_locns         :: list(int),
                lli_rev_hlds_var_nums       :: list(int),

                lli_next_user_event         :: counter,
                lli_next_user_event_var_num :: int,
                lli_user_events             :: cord(user_event_data),
                lli_user_event_var_nums     :: cord(maybe(int)),

                % The list of slots in the with-var-info and without-var-info
                % label layout arrays.
                lli_next_no_var_label_layout:: counter,
                lli_next_svar_label_layout  :: counter,
                lli_next_lvar_label_layout  :: counter,
                lli_rev_no_var_label_layouts:: list(label_layout_no_vars),
                lli_rev_svar_label_layouts  :: list(label_layout_short_vars),
                lli_rev_lvar_label_layouts  :: list(label_layout_long_vars),

                lli_label_counter           :: counter,

                % This maps each internal label with a layout
                % to the id of its layout structure.
                lli_i_label_to_layout_map   :: map(label, layout_slot_name)
            ).

:- func init_label_layouts_info = label_layouts_info.

init_label_layouts_info = Info :-
    Info = label_layouts_info(0, 0, 0, 0, [], [], [], [],
        counter.init(0), 0, cord.empty, cord.empty,
        counter.init(0), counter.init(0), counter.init(0), [], [], [],
        counter.init(1), map.init).

%---------------------------------------------------------------------------%

    % Look for Search as a sub-sequence in the second argument, and
    % return its offset if it exists. We use this to avoid adding existing
    % sequences to arrays.
    %
    % When we stored information about pseudo-typeinfos, variable numbers,
    % and variable locations in separate structures (not arrays), our use
    % of common structures for them ensured that any identical copies of
    % such structures would be optimized away. This optimization is even
    % more effective, since it it allows the sequence we need for one label
    % to be found as (a) a subsequence of the longer sequence for another
    % label, and (b) straddling the sequences of two (or possibly even more)
    % other labels.
    %
    % We use the straightforward, brute force algorithm instead of more
    % sophisticated algorithms like Knuth-Morris-Pratt because (a) those
    % algorithms are nontrivial to adapt to working on lists instead of arrays
    % without losing their edge, (b) this algorithm is fast enough in most
    % cases, and when it isn't, even KMP is unlikely to be fast enough.
    %
    % Turning on this optimization reduces the size of the generated .c and .o
    % files by about 12% and 11% respectively (measured on the files in the
    % library, mdbcomp and compiler directories in grade asm_fast.gc.debug),
    % but even without the type_spec pragmas, it increases sequential bootcheck
    % time only a little bit, from 3hr2m to 3hr6m (on alys, which is a Dell
    % Inspiron 9400 laptop).
    %
    % Another possible optimization would be to look whether some sequence of
    % elements at the current end of the array could be extended into the
    % search sequence, instead of adding the whole search sequence to the
    % array.
    %
    % Because of the brute force nature of this algorithm, it should not
    % be invoked on lists that are longer than the compression limit.
    % Ironically, the bigger the array, the more we want to compress it,
    % but the less we can afford to do so.
    %
:- pred find_sequence(list(T)::in, list(T)::in, int::in, int::out) is semidet.
:- pragma type_spec(find_sequence/4, T = int).
:- pragma type_spec(find_sequence/4, T = rval).

find_sequence(Search, [Head | Tail], CurOffset, FoundAtOffset) :-
    ( if find_sequence_attempt(Search, [Head | Tail]) then
        FoundAtOffset = CurOffset
    else
        find_sequence(Search, Tail, CurOffset + 1, FoundAtOffset)
    ).

:- pred find_sequence_attempt(list(T)::in, list(T)::in) is semidet.
:- pragma type_spec(find_sequence_attempt/2, T = int).
:- pragma type_spec(find_sequence_attempt/2, T = rval).

find_sequence_attempt([], _).
find_sequence_attempt([SearchHead | SearchTail], [Head | Tail]) :-
    SearchHead = Head,
    find_sequence_attempt(SearchTail, Tail).

%---------------------------------------------------------------------------%

represent_determinism_rval(Detism,
    const(llconst_int(code_model.represent_determinism(Detism)))).

%---------------------------------------------------------------------------%
%
% The structure holding all the relevant parameters.
%

:- type stack_layout_params
    --->    stack_layout_params(
                slp_module_info             :: module_info,

                % Should we try to compress arrays? If yes, give the number
                % of elements in the biggest array we should try to compress.
                slp_compress_arrays         :: maybe(int),

                % What kind of execution tracing, if any, are we doing?
                slp_trace_level             :: trace_level,
                slp_trace_suppress          :: trace_suppress_items,

                % Is deep profiling enabled?
                slp_deep_profiling          :: bool,

                % Should we generate agc info?
                slp_agc_stack_layout        :: bool,

                % Should we generate tracing info?
                slp_trace_stack_layout      :: bool,

                % Should we generate proc id info?
                slp_procid_stack_layout     :: bool,

                % Do we have static code addresses or unboxed floats?
                slp_static_code_addresses   :: bool,
                slp_unboxed_floats          :: have_unboxed_floats,

                % Do we have unboxed 64-bit integer types?
                slp_unboxed_int64s          :: have_unboxed_int64s,

                % Do we want to include information about labels' line numbers?
                slp_rtti_line_numbers       :: bool
            ).

:- func init_stack_layout_params(module_info) = stack_layout_params.

init_stack_layout_params(ModuleInfo) = Params :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, common_layout_data, CommonLayoutData),
    globals.lookup_int_option(Globals, layout_compression_limit,
        CompressLimit),
    globals.get_trace_level(Globals, TraceLevel),
    globals.get_trace_suppress(Globals, TraceSuppress),
    globals.lookup_bool_option(Globals, profile_deep, DeepProfiling),
    globals.lookup_bool_option(Globals, agc_stack_layout, AgcLayout),
    globals.lookup_bool_option(Globals, trace_stack_layout, TraceLayout),
    globals.lookup_bool_option(Globals, procid_stack_layout, ProcIdLayout),
    globals.lookup_bool_option(Globals, static_code_addresses, StaticCodeAddr),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloatOpt),
    globals.lookup_bool_option(Globals, unboxed_int64s, UnboxedInt64sOpt),
    globals.lookup_bool_option(Globals, rtti_line_numbers, RttiLineNumbers),
    (
        CommonLayoutData = no,
        CompressArrays = no
    ;
        CommonLayoutData = yes,
        CompressArrays = yes(CompressLimit)
    ),
    (
        UnboxedFloatOpt = no,
        UnboxedFloat = do_not_have_unboxed_floats
    ;
        UnboxedFloatOpt = yes,
        UnboxedFloat = have_unboxed_floats
    ),
    (
        UnboxedInt64sOpt = no,
        UnboxedInt64s = do_not_have_unboxed_int64s
    ;
        UnboxedInt64sOpt = yes,
        UnboxedInt64s = have_unboxed_int64s
    ),
    Params = stack_layout_params(ModuleInfo, CompressArrays,
        TraceLevel, TraceSuppress, DeepProfiling,
        AgcLayout, TraceLayout, ProcIdLayout, StaticCodeAddr,
        UnboxedFloat, UnboxedInt64s, RttiLineNumbers).

%---------------------------------------------------------------------------%
:- end_module ll_backend.stack_layout.
%---------------------------------------------------------------------------%
