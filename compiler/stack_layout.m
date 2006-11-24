%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2006 University of Melbourne.
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

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.global_data.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%

    % Process all the continuation information stored in the HLDS,
    % converting it into LLDS data structures.
    %
:- pred generate_llds(module_info::in, global_data::in, global_data::out,
    list(layout_data)::out, map(label, data_addr)::out) is det.

:- pred construct_closure_layout(proc_label::in, int::in,
    closure_layout_info::in, proc_label::in, module_name::in,
    string::in, int::in, pred_origin::in, string::in,
    static_cell_info::in, static_cell_info::out,
    assoc_list(rval, llds_type)::out, layout_data::out) is det.

:- pred convert_table_arg_info(table_arg_infos::in, int::out,
    rval::out, rval::out, static_cell_info::in, static_cell_info::out) is det.

    % Construct a representation of a variable location as a 32-bit
    % integer.
    %
:- pred represent_locn_as_int(layout_locn::in, int::out) is det.

    % Construct a representation of the interface determinism of a procedure.
    %
:- pred represent_determinism_rval(determinism::in, rval::out) is det.

:- type stack_layout_info.

:- pred lookup_string_in_table(string::in, int::out,
    stack_layout_info::in, stack_layout_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module ll_backend.code_util.
:- import_module ll_backend.layout.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.ll_pseudo_type_info.
:- import_module ll_backend.prog_rep.
:- import_module ll_backend.trace_gen.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module char.
:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

generate_llds(ModuleInfo, !GlobalData, Layouts, LayoutLabels) :-
    global_data_get_all_proc_layouts(!.GlobalData, ProcLayoutList),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, agc_stack_layout, AgcLayout),
    globals.lookup_bool_option(Globals, trace_stack_layout, TraceLayout),
    globals.lookup_bool_option(Globals, procid_stack_layout,
        ProcIdLayout),
    globals.get_trace_level(Globals, TraceLevel),
    globals.get_trace_suppress(Globals, TraceSuppress),
    globals.have_static_code_addresses(Globals, StaticCodeAddr),
    map.init(LayoutLabels0),

    map.init(StringMap0),
    map.init(LabelTables0),
    StringTable0 = string_table(StringMap0, [], 0),
    global_data_get_static_cell_info(!.GlobalData, StaticCellInfo0),
    counter.init(1, LabelCounter0),
    LayoutInfo0 = stack_layout_info(ModuleInfo,
        AgcLayout, TraceLayout, ProcIdLayout, StaticCodeAddr,
        LabelCounter0, [], [], [], LayoutLabels0, [],
        StringTable0, LabelTables0, StaticCellInfo0, no),
    lookup_string_in_table("", _, LayoutInfo0, LayoutInfo1),
    lookup_string_in_table("<too many variables>", _,
        LayoutInfo1, LayoutInfo2),
    list.foldl(construct_layouts, ProcLayoutList, LayoutInfo2, LayoutInfo),
    LabelsCounter = LayoutInfo ^ label_counter,
    counter.allocate(NumLabels, LabelsCounter, _),
    TableIoDecls = LayoutInfo ^ table_infos,
    ProcLayouts = LayoutInfo ^ proc_layouts,
    InternalLayouts = LayoutInfo ^ internal_layouts,
    LayoutLabels = LayoutInfo ^ label_set,
    ProcLayoutNames = LayoutInfo ^ proc_layout_name_list,
    StringTable = LayoutInfo ^ string_table,
    LabelTables = LayoutInfo ^ label_tables,
    global_data_set_static_cell_info(LayoutInfo ^ static_cell_info,
        !GlobalData),
    StringTable = string_table(_, RevStringList, StringOffset),
    list.reverse(RevStringList, StringList),
    concat_string_list(StringList, StringOffset, ConcatStrings),

    list.condense([TableIoDecls, ProcLayouts, InternalLayouts], Layouts0),
    (
        TraceLayout = yes,
        module_info_get_name(ModuleInfo, ModuleName),
        globals.lookup_bool_option(Globals, rtti_line_numbers, LineNumbers),
        (
            LineNumbers = yes,
            EffLabelTables = LabelTables
        ;
            LineNumbers = no,
            map.init(EffLabelTables)
        ),
        format_label_tables(EffLabelTables, SourceFileLayouts),
        SuppressedEvents = encode_suppressed_events(TraceSuppress),
        HasUserEvent = LayoutInfo ^ has_user_event,
        (
            HasUserEvent = no,
            MaybeEventSpecs = no
        ;
            HasUserEvent = yes,
            module_info_get_event_spec_map(ModuleInfo, EventSpecMap),
            EventSpecs = event_set_description(EventSpecMap),
            MaybeEventSpecs = yes(EventSpecs)
        ),
        ModuleLayout = module_layout_data(ModuleName,
            StringOffset, ConcatStrings, ProcLayoutNames, SourceFileLayouts,
            TraceLevel, SuppressedEvents, NumLabels, MaybeEventSpecs),
        Layouts = [ModuleLayout | Layouts0]
    ;
        TraceLayout = no,
        Layouts = Layouts0
    ).

:- pred valid_proc_layout(proc_layout_info::in) is semidet.

valid_proc_layout(ProcLayoutInfo) :-
    EntryLabel = ProcLayoutInfo ^ entry_label,
    ProcLabel = get_proc_label(EntryLabel),
    (
        ProcLabel = ordinary_proc_label(_, _, DeclModule, Name, Arity, _),
        \+ no_type_info_builtin(DeclModule, Name, Arity)
    ;
        ProcLabel = special_proc_label(_, _, _, _, _, _)
    ).

%---------------------------------------------------------------------------%

    % concat_string_list appends a list of strings together,
    % appending a null character after each string.
    % The resulting string will contain embedded null characters,
:- pred concat_string_list(list(string)::in, int::in,
    string_with_0s::out) is det.

concat_string_list(Strings, Len, string_with_0s(Result)) :-
    concat_string_list_2(Strings, Len, Result).

:- pred concat_string_list_2(list(string)::in, int::in, string::out) is det.

:- pragma foreign_decl("C", "
    #include ""mercury_tags.h"" /* for MR_list_*() */
    #include ""mercury_heap.h"" /* for MR_offset_incr_hp_atomic*() */
    #include ""mercury_misc.h"" /* for MR_fatal_error() */
").

:- pragma foreign_proc("C",
    concat_string_list_2(StringList::in, ArenaSize::in, Arena::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"{
    MR_Word     cur_node;
    MR_Integer  cur_offset;
    MR_Word     tmp;

    MR_offset_incr_hp_atomic(tmp, 0,
        (ArenaSize + sizeof(MR_Word)) / sizeof(MR_Word));
    Arena = (char *) tmp;

    cur_offset = 0;
    cur_node = StringList;

    while (! MR_list_is_empty(cur_node)) {
        (void) strcpy(&Arena[cur_offset], (char *) MR_list_head(cur_node));
        cur_offset += strlen((char *) MR_list_head(cur_node)) + 1;
        cur_node = MR_list_tail(cur_node);
    }

    if (cur_offset != ArenaSize) {
        char    msg[256];

        sprintf(msg, ""internal error in creating string table;\\n""
            ""cur_offset = %ld, ArenaSize = %ld\\n"",
            (long) cur_offset, (long) ArenaSize);
        MR_fatal_error(msg);
    }
}").

% This version is only used if there is no matching foreign_proc version.
% Note that this version only works if the Mercury implementation's
% string representation allows strings to contain embedded null characters.
% So we check that.
concat_string_list_2(StringsList, _Len, StringWithNulls) :-
    (
        char.to_int(NullChar, 0),
        NullCharString = string.char_to_string(NullChar),
        string.length(NullCharString, 1)
    ->
        StringsWithNullsList = list.map(func(S) = S ++ NullCharString,
            StringsList),
        StringWithNulls = string.append_list(StringsWithNullsList)
    ;
        % the Mercury implementation's string representation
        % doesn't support strings containing null characters
        private_builtin.sorry("stack_layout.concat_string_list")
    ).

%---------------------------------------------------------------------------%

:- pred format_label_tables(map(string, label_table)::in,
    list(file_layout_data)::out) is det.

format_label_tables(LabelTableMap, SourceFileLayouts) :-
    map.to_assoc_list(LabelTableMap, LabelTableList),
    list.map(format_label_table, LabelTableList, SourceFileLayouts).

:- pred format_label_table(pair(string, label_table)::in,
    file_layout_data::out) is det.

format_label_table(FileName - LineNoMap,
        file_layout_data(FileName, FilteredList)) :-
    % This step should produce a list ordered on line numbers.
    map.to_assoc_list(LineNoMap, LineNoList),
    % And this step should preserve that order.
    flatten_label_table(LineNoList, [], FlatLineNoList),
    Filter = (pred(LineNoInfo::in, FilteredLineNoInfo::out) is det :-
        LineNoInfo = LineNo - (Label - _IsReturn),
        FilteredLineNoInfo = LineNo - Label
    ),
    list.map(Filter, FlatLineNoList, FilteredList).

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

    % Construct the layouts that concern a single procedure:
    % the procedure-specific layout and the layouts of the labels
    % inside that procedure. Also update the module-wide label table
    % with the labels defined in this procedure.
    %
:- pred construct_layouts(proc_layout_info::in,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_layouts(ProcLayoutInfo, !Info) :-
    ProcLayoutInfo = proc_layout_info(RttiProcLabel,
        EntryLabel,
        _Detism,
        _StackSlots,
        _SuccipLoc,
        _EvalMethod,
        _EffTraceLevel,
        _MaybeCallLabel,
        _MaxTraceReg,
        HeadVars,
        _ArgModes,
        Goal,
        _NeedGoalRep,
        _InstMap,
        _TraceSlotInfo,
        ForceProcIdLayout,
        VarSet,
        _VarTypes,
        InternalMap,
        MaybeTableIoDecl,
        _NeedsAllNames,
        _MaybeDeepProfInfo),
    map.to_assoc_list(InternalMap, Internals),
    compute_var_number_map(HeadVars, VarSet, Internals, Goal, VarNumMap),

    ProcLabel = get_proc_label(EntryLabel),
    get_procid_stack_layout(!.Info, ProcIdLayout0),
    bool.or(ProcIdLayout0, ForceProcIdLayout, ProcIdLayout),
    (
        ( ProcIdLayout = yes
        ; MaybeTableIoDecl = yes(_)
        )
    ->
        Kind = proc_layout_proc_id(proc_label_user_or_uci(ProcLabel))
    ;
        Kind = proc_layout_traversal
    ),
    ProcLayoutName = proc_layout(RttiProcLabel, Kind),
    (
        ( !.Info ^ agc_stack_layout = yes
        ; !.Info ^ trace_stack_layout = yes
        ),
        valid_proc_layout(ProcLayoutInfo)
    ->
        list.map_foldl(
            construct_internal_layout(ProcLabel, ProcLayoutName, VarNumMap),
            Internals, InternalLayouts, !Info)
    ;
        InternalLayouts = []
    ),
    get_label_tables(!.Info, LabelTables0),
    list.foldl(update_label_table, InternalLayouts,
        LabelTables0, LabelTables),
    set_label_tables(LabelTables, !Info),
    construct_proc_layout(ProcLayoutInfo, Kind, VarNumMap, !Info).

%---------------------------------------------------------------------------%

    % Add the given label layout to the module-wide label tables.

:- pred update_label_table(
    {proc_label, int, label_vars, internal_layout_info}::in,
    map(string, label_table)::in, map(string, label_table)::out) is det.

update_label_table({ProcLabel, LabelNum, LabelVars, InternalInfo},
        !LabelTables) :-
    InternalInfo = internal_layout_info(Port, _, Return),
    (
        Return = yes(return_layout_info(TargetsContexts, _)),
        find_valid_return_context(TargetsContexts, Target, Context, _GoalPath)
    ->
        ( Target = code_label(TargetLabel) ->
            IsReturn = known_callee(TargetLabel)
        ;
            IsReturn = unknown_callee
        ),
        update_label_table_2(ProcLabel, LabelNum,
            LabelVars, Context, IsReturn, !LabelTables)
    ;
        Port = yes(trace_port_layout_info(Context, _, _, _, _, _)),
        context_is_valid(Context)
    ->
        update_label_table_2(ProcLabel, LabelNum, LabelVars, Context,
            not_a_return, !LabelTables)
    ;
        true
    ).

:- pred update_label_table_2(proc_label::in, int::in,
    label_vars::in, context::in, is_label_return::in,
    map(string, label_table)::in, map(string, label_table)::out) is det.

update_label_table_2(ProcLabel, LabelNum, LabelVars, Context,
        IsReturn, !LabelTables) :-
    term.context_file(Context, File),
    term.context_line(Context, Line),
    ( map.search(!.LabelTables, File, LabelTable0) ->
        LabelLayout = label_layout(ProcLabel, LabelNum, LabelVars),
        ( map.search(LabelTable0, Line, LineInfo0) ->
            LineInfo = [LabelLayout - IsReturn | LineInfo0],
            map.det_update(LabelTable0, Line, LineInfo, LabelTable),
            svmap.det_update(File, LabelTable, !LabelTables)
        ;
            LineInfo = [LabelLayout - IsReturn],
            map.det_insert(LabelTable0, Line, LineInfo, LabelTable),
            svmap.det_update(File, LabelTable, !LabelTables)
        )
    ; context_is_valid(Context) ->
        map.init(LabelTable0),
        LabelLayout = label_layout(ProcLabel, LabelNum, LabelVars),
        LineInfo = [LabelLayout - IsReturn],
        map.det_insert(LabelTable0, Line, LineInfo, LabelTable),
        svmap.det_insert(File, LabelTable, !LabelTables)
    ;
        % We don't have a valid context for this label,
        % so we don't enter it into any tables.
        true
    ).

:- pred find_valid_return_context(
    assoc_list(code_addr, pair(prog_context, hlds_goal.goal_path))::in,
    code_addr::out, prog_context::out, hlds_goal.goal_path::out)
    is semidet.

find_valid_return_context([TargetContext | TargetContexts],
        ValidTarget, ValidContext, ValidGoalPath) :-
    TargetContext = Target - (Context - GoalPath),
    ( context_is_valid(Context) ->
        ValidTarget = Target,
        ValidContext = Context,
        ValidGoalPath = GoalPath
    ;
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

:- pred construct_proc_traversal(label::in, determinism::in,
    int::in, maybe(int)::in, proc_layout_stack_traversal::out,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_proc_traversal(EntryLabel, Detism, NumStackSlots,
        MaybeSuccipLoc, Traversal, !Info) :-
    (
        MaybeSuccipLoc = yes(Location),
        ( determinism_components(Detism, _, at_most_many) ->
            SuccipLval = framevar(Location)
        ;
            SuccipLval = stackvar(Location)
        ),
        represent_locn_as_int(direct(SuccipLval), SuccipInt),
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
    get_static_code_addresses(!.Info, StaticCodeAddr),
    (
        StaticCodeAddr = yes,
        MaybeEntryLabel = yes(EntryLabel)
    ;
        StaticCodeAddr = no,
        MaybeEntryLabel = no
    ),
    Traversal = proc_layout_stack_traversal(MaybeEntryLabel,
        MaybeSuccipInt, NumStackSlots, Detism).

    % Construct a procedure-specific layout.
    %
:- pred construct_proc_layout(proc_layout_info::in,
    proc_layout_kind::in, var_num_map::in,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_proc_layout(ProcLayoutInfo, Kind, VarNumMap, !Info) :-
    ProcLayoutInfo = proc_layout_info(RttiProcLabel,
        EntryLabel,
        Detism,
        StackSlots,
        SuccipLoc,
        EvalMethod,
        EffTraceLevel,
        MaybeCallLabel,
        MaxTraceReg,
        HeadVars,
        ArgModes,
        Goal,
        NeedGoalRep,
        InstMap,
        TraceSlotInfo,
        _ForceProcIdLayout,
        VarSet,
        VarTypes,
        _InternalMap,
        MaybeTableInfo,
        NeedsAllNames,
        MaybeProcStatic),
    construct_proc_traversal(EntryLabel, Detism, StackSlots,
        SuccipLoc, Traversal, !Info),
    (
        Kind = proc_layout_traversal,
        More = no_proc_id
    ;
        Kind = proc_layout_proc_id(_),
        get_trace_stack_layout(!.Info, TraceStackLayout),
        (
            TraceStackLayout = yes,
            given_trace_level_is_none(EffTraceLevel) = no,
            valid_proc_layout(ProcLayoutInfo)
        ->
            construct_trace_layout(RttiProcLabel, EvalMethod, EffTraceLevel,
                MaybeCallLabel, MaxTraceReg, HeadVars, ArgModes, Goal,
                NeedGoalRep, InstMap, TraceSlotInfo,
                VarSet, VarTypes, MaybeTableInfo,
                NeedsAllNames, VarNumMap, ExecTrace, !Info),
            MaybeExecTrace = yes(ExecTrace)
        ;
            MaybeExecTrace = no
        ),
        More = proc_id(MaybeProcStatic, MaybeExecTrace)
    ),
    ProcLayout = proc_layout_data(RttiProcLabel, Traversal, More),
    LayoutName = proc_layout(RttiProcLabel, Kind),
    add_proc_layout_data(ProcLayout, LayoutName, EntryLabel, !Info),
    (
        MaybeTableInfo = no
    ;
        MaybeTableInfo = yes(TableInfo),
        get_static_cell_info(!.Info, StaticCellInfo0),
        make_table_data(RttiProcLabel, Kind, TableInfo, MaybeTableData,
            StaticCellInfo0, StaticCellInfo),
        set_static_cell_info(StaticCellInfo, !Info),
        add_table_data(MaybeTableData, !Info)
    ).

:- pred construct_trace_layout(rtti_proc_label::in,
    eval_method::in, trace_level::in, maybe(label)::in, int::in,
    list(prog_var)::in, list(mer_mode)::in, hlds_goal::in, bool::in,
    instmap::in, trace_slot_info::in, prog_varset::in, vartypes::in,
    maybe(proc_table_info)::in, bool::in, var_num_map::in,
    proc_layout_exec_trace::out,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_trace_layout(RttiProcLabel, EvalMethod, EffTraceLevel,
        MaybeCallLabel, MaxTraceReg, HeadVars, ArgModes,
        Goal, NeedGoalRep, InstMap, TraceSlotInfo, _VarSet, VarTypes,
        MaybeTableInfo, NeedsAllNames, VarNumMap, ExecTrace, !Info) :-
    construct_var_name_vector(VarNumMap,
        NeedsAllNames, MaxVarNum, VarNameVector, !Info),
    list.map(convert_var_to_int(VarNumMap), HeadVars, HeadVarNumVector),
    ModuleInfo = !.Info ^ module_info,
    (
        NeedGoalRep = no,
        ProcBytes = []
    ;
        NeedGoalRep = yes,
        prog_rep.represent_proc(HeadVars, Goal, InstMap, VarTypes, VarNumMap,
            ModuleInfo, !Info, ProcBytes)
    ),
    (
        MaybeCallLabel = yes(CallLabelPrime),
        CallLabel = CallLabelPrime
    ;
        MaybeCallLabel = no,
        unexpected(this_file,
            "construct_trace_layout: call label not present")
    ),
    TraceSlotInfo = trace_slot_info(MaybeFromFullSlot, MaybeIoSeqSlot,
        MaybeTrailSlots, MaybeMaxfrSlot, MaybeCallTableSlot),
    % The label associated with an event must have variable info.
    (
        CallLabel = internal_label(CallLabelNum, CallProcLabel)
    ;
        CallLabel = entry_label(_, _),
        unexpected(this_file,
            "construct_trace_layout: entry call label")
    ),
    CallLabelLayout = label_layout(CallProcLabel, CallLabelNum,
        label_has_var_info),
    (
        MaybeTableInfo = no,
        MaybeTableDataAddr = no
    ;
        MaybeTableInfo = yes(TableInfo),
        (
            TableInfo = table_io_decl_info(_),
            MaybeTableDataAddr = yes(layout_addr(table_io_decl(RttiProcLabel)))
        ;
            TableInfo = table_gen_info(_, _, _, _, _),
            module_info_get_name(ModuleInfo, ModuleName),
            ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
            MaybeTableDataAddr = yes(data_addr(ModuleName,
                proc_tabling_ref(ProcLabel, tabling_info)))
        )
    ),
    encode_exec_trace_flags(ModuleInfo, HeadVars, ArgModes, VarTypes,
        0, Flags),
    ExecTrace = proc_layout_exec_trace(CallLabelLayout, ProcBytes,
        MaybeTableDataAddr, HeadVarNumVector, VarNameVector,
        MaxVarNum, MaxTraceReg, MaybeFromFullSlot, MaybeIoSeqSlot,
        MaybeTrailSlots, MaybeMaxfrSlot, EvalMethod,
        MaybeCallTableSlot, EffTraceLevel, Flags).

:- pred encode_exec_trace_flags(module_info::in, list(prog_var)::in,
    list(mer_mode)::in, vartypes::in, int::in, int::out) is det.

encode_exec_trace_flags(ModuleInfo, HeadVars, ArgModes, VarTypes, !Flags) :-
    (
        proc_info_has_io_state_pair_from_details(ModuleInfo, HeadVars,
            ArgModes, VarTypes, _, _)
    ->
        !:Flags = !.Flags + 1
    ;
        true
    ).

:- pred construct_var_name_vector(var_num_map::in,
    bool::in, int::out, list(int)::out,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_var_name_vector(VarNumMap, NeedsAllNames, MaxVarNum, Offsets,
        !Info) :-
    map.values(VarNumMap, VarNames0),
    (
        NeedsAllNames = yes,
        VarNames = VarNames0
    ;
        NeedsAllNames = no,
        list.filter(var_has_name, VarNames0, VarNames)
    ),
    list.sort(VarNames, SortedVarNames),
    ( SortedVarNames = [FirstVarNum - _ | _] ->
        MaxVarNum0 = FirstVarNum,
        construct_var_name_rvals(SortedVarNames, 1, MaxVarNum0, MaxVarNum,
            Offsets, !Info)
    ;
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
    stack_layout_info::in, stack_layout_info::out) is det.

construct_var_name_rvals([], _CurNum, MaxNum, MaxNum, [], !Info).
construct_var_name_rvals([Var - Name | VarNamesTail], CurNum,
        !MaxNum, [Offset | OffsetsTail], !Info) :-
    ( Var = CurNum ->
        lookup_string_in_table(Name, Offset, !Info),
        !:MaxNum = Var,
        VarNames = VarNamesTail
    ;
        Offset = 0,
        VarNames = [Var - Name | VarNamesTail]
    ),
    construct_var_name_rvals(VarNames, CurNum + 1,
        !MaxNum, OffsetsTail, !Info).

%---------------------------------------------------------------------------%

:- pred compute_var_number_map(list(prog_var)::in, prog_varset::in,
    assoc_list(int, internal_layout_info)::in, hlds_goal::in,
    var_num_map::out) is det.

compute_var_number_map(HeadVars, VarSet, Internals, Goal, VarNumMap) :-
    some [!VarNumMap, !Counter] (
        !:VarNumMap = map.init,
        !:Counter = counter.init(1), % to match term.var_supply_init
        goal_util.goal_vars(Goal, GoalVarSet),
        set.to_sorted_list(GoalVarSet, GoalVars),
        list.foldl2(add_var_to_var_number_map(VarSet), GoalVars,
            !VarNumMap, !Counter),
        list.foldl2(add_var_to_var_number_map(VarSet), HeadVars,
            !VarNumMap, !Counter),
        list.foldl2(internal_var_number_map, Internals, !VarNumMap,
            !.Counter, _),
        VarNumMap = !.VarNumMap
    ).

:- pred internal_var_number_map(pair(int, internal_layout_info)::in,
    var_num_map::in, var_num_map::out, counter::in, counter::out) is det.

internal_var_number_map(_Label - Internal, !VarNumMap, !Counter) :-
    Internal = internal_layout_info(MaybeTrace, MaybeResume, MaybeReturn),
    (
        MaybeTrace = yes(Trace),
        Trace = trace_port_layout_info(_, _, _, _, _, TraceLayout),
        label_layout_var_number_map(TraceLayout, !VarNumMap, !Counter)
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

:- pred add_var_to_var_number_map(prog_varset::in, prog_var::in,
    var_num_map::in, var_num_map::out, counter::in, counter::out) is det.

add_var_to_var_number_map(VarSet, Var, !VarNumMap, !Counter) :-
    ( varset.search_name(VarSet, Var, VarName) ->
        Name = VarName
    ;
        Name = ""
    ),
    add_named_var_to_var_number_map(Var - Name, !VarNumMap, !Counter).

:- pred add_named_var_to_var_number_map(pair(prog_var, string)::in,
    var_num_map::in, var_num_map::out, counter::in, counter::out) is det.

add_named_var_to_var_number_map(Var - Name, !VarNumMap, !Counter) :-
    ( map.search(!.VarNumMap, Var, _) ->
        % Name shouldn't differ from the name recorded in !.VarNumMap.
        true
    ;
        counter.allocate(VarNum, !Counter),
        map.det_insert(!.VarNumMap, Var, VarNum - Name, !:VarNumMap)
    ).

%---------------------------------------------------------------------------%

    % Construct the layout describing a single internal label
    % for accurate GC and/or execution tracing.
    %
:- pred construct_internal_layout(proc_label::in,
    layout_name::in, var_num_map::in, pair(int, internal_layout_info)::in,
    {proc_label, int, label_vars, internal_layout_info}::out,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_internal_layout(ProcLabel, ProcLayoutName, VarNumMap,
        LabelNum - Internal, LabelLayout, !Info) :-
    Internal = internal_layout_info(Trace, Resume, Return),
    (
        Trace = no,
        set.init(TraceLiveVarSet),
        map.init(TraceTypeVarMap),
        MaybeUserInfo = no
    ;
        Trace = yes(trace_port_layout_info(_,_,_,_, MaybeUserInfo,
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
        goal_path_to_string(GoalPath, GoalPathStr),
        lookup_string_in_table(GoalPathStr, GoalPathNum, !Info),
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
        ( find_valid_return_context(TargetsContexts, _, _, GoalPath) ->
            goal_path_to_string(GoalPath, GoalPathStr),
            lookup_string_in_table(GoalPathStr, GoalPathNum, !Info),
            MaybeGoalPath = yes(GoalPathNum)
        ;
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
        unexpected(this_file, "label has both trace and return layout info")
    ),
    get_agc_stack_layout(!.Info, AgcStackLayout),
    (
        Return = no,
        set.init(ReturnLiveVarSet),
        map.init(ReturnTypeVarMap)
    ;
        Return = yes(return_layout_info(_, ReturnLayout)),
        ReturnLayout = layout_label_info(ReturnLiveVarSet0, ReturnTypeVarMap0),
        (
            AgcStackLayout = yes,
            ReturnLiveVarSet = ReturnLiveVarSet0,
            ReturnTypeVarMap = ReturnTypeVarMap0
        ;
            AgcStackLayout = no,
            % This set of variables must be for uplevel printing in execution
            % tracing, so we are interested only in (a) variables, not
            % temporaries, (b) only named variables, and (c) only those
            % on the stack, not the return values.
            set.to_sorted_list(ReturnLiveVarSet0, ReturnLiveVarList0),
            select_trace_return(
                ReturnLiveVarList0, ReturnTypeVarMap0,
                ReturnLiveVarList, ReturnTypeVarMap),
            set.list_to_set(ReturnLiveVarList, ReturnLiveVarSet)
        )
    ),
    (
        Trace = no,
        Resume = no,
        Return = no
    ->
        MaybeVarInfo = no,
        LabelVars = label_has_no_var_info
    ;
        % XXX Ignore differences in insts inside layout_var_infos.
        set.union(TraceLiveVarSet, ResumeLiveVarSet, LiveVarSet0),
        set.union(LiveVarSet0, ReturnLiveVarSet, LiveVarSet),
        map.union(set.intersect, TraceTypeVarMap, ResumeTypeVarMap,
            TypeVarMap0),
        map.union(set.intersect, TypeVarMap0, ReturnTypeVarMap, TypeVarMap),
        construct_livelval_rvals(LiveVarSet, VarNumMap, TypeVarMap,
            EncodedLength, LiveValRval, NamesRval, TypeParamRval, !Info),
        VarInfo = label_var_info(EncodedLength, LiveValRval, NamesRval,
            TypeParamRval),
        MaybeVarInfo = yes(VarInfo),
        LabelVars = label_has_var_info
    ),
    (
        MaybeUserInfo = no,
        MaybeUserData = no
    ;
        MaybeUserInfo = yes(UserInfo),
        set_has_user_event(yes, !Info),
        UserInfo = user_event_info(UserEventNumber, UserEventName,
            Attributes),
        list.length(Attributes, NumAttributes),
        construct_user_data_array(Attributes,
            UserLocnsArray, UserTypesArray, UserAttrNames, !Info),

        get_static_cell_info(!.Info, StaticCellInfo0),
        add_scalar_static_cell(UserLocnsArray, UserLocnsDataAddr,
            StaticCellInfo0, StaticCellInfo1),
        add_scalar_static_cell(UserTypesArray, UserTypesDataAddr,
            StaticCellInfo1, StaticCellInfo),
        set_static_cell_info(StaticCellInfo, !Info),

        UserLocnsRval = const(llconst_data_addr(UserLocnsDataAddr, no)),
        UserTypesRval = const(llconst_data_addr(UserTypesDataAddr, no)),
        UserData = user_event_data(UserEventNumber, UserEventName,
            NumAttributes, UserLocnsRval, UserTypesRval, UserAttrNames),
        MaybeUserData = yes(UserData)
    ),

    (
        Trace = yes(_),
        allocate_label_number(LabelNumber0, !Info),
        % MR_ml_label_exec_count[0] is never written out; it is reserved for
        % cases like this, for labels without events, and for handwritten
        % labels.
        ( LabelNumber0 < (1 << 16) ->
            LabelNumber = LabelNumber0
        ;
            LabelNumber = 0
        )
    ;
        Trace = no,
        LabelNumber = 0
    ),
    LayoutData = label_layout_data(ProcLabel, LabelNum, ProcLayoutName,
        MaybePort, MaybeIsHidden, LabelNumber, MaybeGoalPath, MaybeUserData,
        MaybeVarInfo),
    LayoutName = label_layout(ProcLabel, LabelNum, LabelVars),
    Label = internal_label(LabelNum, ProcLabel),
    add_internal_layout_data(LayoutData, Label, LayoutName, !Info),
    LabelLayout = {ProcLabel, LabelNum, LabelVars, Internal}.

:- pred construct_user_data_array(list(user_attribute)::in,
    assoc_list(rval, llds_type)::out, assoc_list(rval, llds_type)::out,
    list(string)::out, stack_layout_info::in, stack_layout_info::out) is det.

construct_user_data_array([], [], [], [], !Info).
construct_user_data_array([Attr | Attrs],
        [LocnRvalAndType | LocnRvalAndTypes],
        [TypeRvalAndType | TypeRvalAndTypes], [Name | Names], !Info) :-
    Attr = user_attribute(Locn, Type, Name),
    represent_locn_or_const_as_int_rval(Locn, LocnRval, LocnRvalType, !Info),
    LocnRvalAndType = LocnRval - LocnRvalType,

    ExistQTvars = [],
    NumUnivQTvars = -1,
    get_static_cell_info(!.Info, StaticCellInfo0),
    ll_pseudo_type_info.construct_typed_llds_pseudo_type_info(Type,
        NumUnivQTvars, ExistQTvars, StaticCellInfo0, StaticCellInfo,
        TypeRval, TypeRvalType),
    set_static_cell_info(StaticCellInfo, !Info),
    TypeRvalAndType = TypeRval - TypeRvalType,

    construct_user_data_array(Attrs, LocnRvalAndTypes, TypeRvalAndTypes,
        Names, !Info).

%---------------------------------------------------------------------------%

:- pred construct_livelval_rvals(set(layout_var_info)::in,
    var_num_map::in, map(tvar, set(layout_locn))::in, int::out,
    rval::out, rval::out, rval::out,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_livelval_rvals(LiveLvalSet, VarNumMap, TVarLocnMap,
        EncodedLength, LiveValRval, NamesRval, TypeParamRval, !Info) :-
    set.to_sorted_list(LiveLvalSet, LiveLvals),
    sort_livevals(LiveLvals, SortedLiveLvals),
    construct_liveval_arrays(SortedLiveLvals, VarNumMap,
        EncodedLength, LiveValRval, NamesRval, !Info),
    StaticCellInfo0 = !.Info ^ static_cell_info,
    construct_tvar_vector(TVarLocnMap, TypeParamRval,
        StaticCellInfo0, StaticCellInfo),
    !:Info = !.Info ^ static_cell_info := StaticCellInfo.

:- pred construct_tvar_vector(map(tvar, set(layout_locn))::in,
    rval::out, static_cell_info::in, static_cell_info::out) is det.

construct_tvar_vector(TVarLocnMap, TypeParamRval, !StaticCellInfo) :-
    ( map.is_empty(TVarLocnMap) ->
        TypeParamRval = const(llconst_int(0))
    ;
        construct_tvar_rvals(TVarLocnMap, Vector),
        add_scalar_static_cell(Vector, DataAddr, !StaticCellInfo),
        TypeParamRval = const(llconst_data_addr(DataAddr, no))
    ).

:- pred construct_tvar_rvals(map(tvar, set(layout_locn))::in,
    assoc_list(rval, llds_type)::out) is det.

construct_tvar_rvals(TVarLocnMap, Vector) :-
    map.to_assoc_list(TVarLocnMap, TVarLocns),
    construct_type_param_locn_vector(TVarLocns, 1, TypeParamLocs),
    list.length(TypeParamLocs, TypeParamsLength),
    LengthRval = const(llconst_int(TypeParamsLength)),
    Vector = [LengthRval - uint_least32 | TypeParamLocs].

%---------------------------------------------------------------------------%

    % Given a list of layout_var_infos and the type variables that occur
    % in them, select only the layout_var_infos that may be required
    % by up-level printing in the trace-based debugger. At the moment
    % the typeinfo list we return may be bigger than necessary, but this
    % does not compromise correctness; we do this to avoid having to
    % scan the types of all the selected layout_var_infos.
    %
:- pred select_trace_return(
    list(layout_var_info)::in, map(tvar, set(layout_locn))::in,
    list(layout_var_info)::out, map(tvar, set(layout_locn))::out) is det.

select_trace_return(Infos, TVars, TraceReturnInfos, TVars) :-
    IsNamedReturnVar = (pred(LocnInfo::in) is semidet :-
        LocnInfo = layout_var_info(Locn, LvalType, _),
        LvalType = live_value_var(_, Name, _, _),
        Name \= "",
        ( Locn = direct(Lval) ; Locn = indirect(Lval, _)),
        ( Lval = stackvar(_) ; Lval = framevar(_) )
    ),
    list.filter(IsNamedReturnVar, Infos, TraceReturnInfos).

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
        ( NameResult = (=) ->
            compare(Result, Lval1, Lval2)
        ;
            Result = NameResult
        )
    ),
    list.sort(CompareVarInfos, NamedVarInfos0, NamedVarInfos),
    list.sort(CompareVarInfos, OtherInfos0, OtherInfos),
    list.append(NamedVarInfos, OtherInfos, FinalInfos).

:- pred get_name_from_live_value_type(live_value_type::in,
    string::out) is det.

get_name_from_live_value_type(LiveType, Name) :-
    ( LiveType = live_value_var(_, NamePrime, _, _) ->
        Name = NamePrime
    ;
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
    int::in, assoc_list(rval, llds_type)::out) is det.

construct_type_param_locn_vector([], _, []).
construct_type_param_locn_vector([TVar - Locns | TVarLocns], CurSlot,
        Vector) :-
    term.var_to_int(TVar, TVarNum),
    NextSlot = CurSlot + 1,
    ( TVarNum = CurSlot ->
        ( set.remove_least(Locns, LeastLocn, _) ->
            Locn = LeastLocn
        ;
            unexpected(this_file, "tvar has empty set of locations")
        ),
        represent_locn_as_int_rval(Locn, Rval),
        construct_type_param_locn_vector(TVarLocns, NextSlot, VectorTail),
        Vector = [Rval - uint_least32 | VectorTail]
    ; TVarNum > CurSlot ->
        construct_type_param_locn_vector([TVar - Locns | TVarLocns], NextSlot,
            VectorTail),
        % This slot will never be referred to.
        Vector = [const(llconst_int(0)) - uint_least32 | VectorTail]
    ;
        unexpected(this_file,
            "unsorted tvars in construct_type_param_locn_vector")
    ).

%---------------------------------------------------------------------------%

:- type liveval_array_info
    --->    live_array_info(
                rval,       % Rval describing the location of a live value.
                            % Always of llds type uint_least8 if the cell
                            % is in the byte array, and uint_least32 if it
                            % is in the int array.
                rval,       % Rval describing the type of a live value.
                llds_type,  % The llds type of the rval describing the type.
                rval        % Rval describing the variable number of a
                            % live value. Always of llds type uint_least16.
                            % Contains zero if the live value is not
                            % a variable. Contains the hightest possible
                            % uint_least16 value if the variable number
                            % does not fit in 16 bits.
            ).

    % Construct a vector of (locn, live_value_type) pairs,
    % and a corresponding vector of variable names.
    %
:- pred construct_liveval_arrays(list(layout_var_info)::in,
    var_num_map::in, int::out, rval::out, rval::out,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_liveval_arrays(VarInfos, VarNumMap, EncodedLength,
        TypeLocnVector, NumVector, !Info) :-
    int.pow(2, short_count_bits, BytesLimit),
    construct_liveval_array_infos(VarInfos, VarNumMap,
        0, BytesLimit, IntArrayInfo, ByteArrayInfo, !Info),

    list.length(IntArrayInfo, IntArrayLength),
    list.length(ByteArrayInfo, ByteArrayLength),
    list.append(IntArrayInfo, ByteArrayInfo, AllArrayInfo),

    EncodedLength = IntArrayLength << short_count_bits + ByteArrayLength,

    SelectLocns = (pred(ArrayInfo::in, LocnRval::out) is det :-
        ArrayInfo = live_array_info(LocnRval, _, _, _)
    ),
    SelectTypes = (pred(ArrayInfo::in, TypeRval - TypeType::out) is det :-
        ArrayInfo = live_array_info(_, TypeRval, TypeType, _)
    ),
    AddRevNums = (pred(ArrayInfo::in, NumRvals0::in, NumRvals::out) is det :-
        ArrayInfo = live_array_info(_, _, _, NumRval),
        NumRvals = [NumRval | NumRvals0]
    ),

    list.map(SelectTypes, AllArrayInfo, AllTypeRvalsTypes),
    list.map(SelectLocns, IntArrayInfo, IntLocns),
    list.map(associate_type(uint_least32), IntLocns, IntLocnsTypes),
    list.map(SelectLocns, ByteArrayInfo, ByteLocns),
    list.map(associate_type(uint_least8), ByteLocns, ByteLocnsTypes),
    list.append(IntLocnsTypes, ByteLocnsTypes, AllLocnsTypes),
    list.append(AllTypeRvalsTypes, AllLocnsTypes, TypeLocnVectorRvalsTypes),
    get_static_cell_info(!.Info, StaticCellInfo0),
    add_scalar_static_cell(TypeLocnVectorRvalsTypes, TypeLocnVectorAddr,
        StaticCellInfo0, StaticCellInfo1),
    TypeLocnVector = const(llconst_data_addr(TypeLocnVectorAddr, no)),
    set_static_cell_info(StaticCellInfo1, !Info),

    get_trace_stack_layout(!.Info, TraceStackLayout),
    (
        TraceStackLayout = yes,
        list.foldl(AddRevNums, AllArrayInfo, [], RevVarNumRvals),
        list.reverse(RevVarNumRvals, VarNumRvals),
        list.map(associate_type(uint_least16), VarNumRvals, VarNumRvalsTypes),
        get_static_cell_info(!.Info, StaticCellInfo2),
        add_scalar_static_cell(VarNumRvalsTypes, NumVectorAddr,
            StaticCellInfo2, StaticCellInfo),
        set_static_cell_info(StaticCellInfo, !Info),
        NumVector = const(llconst_data_addr(NumVectorAddr, no))
    ;
        TraceStackLayout = no,
        NumVector = const(llconst_int(0))
    ).

:- pred associate_type(llds_type::in, rval::in, pair(rval, llds_type)::out)
    is det.

associate_type(LldsType, Rval, Rval - LldsType).

:- pred construct_liveval_array_infos(list(layout_var_info)::in,
    var_num_map::in, int::in, int::in,
    list(liveval_array_info)::out, list(liveval_array_info)::out,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_liveval_array_infos([], _, _, _, [], [], !Info).
construct_liveval_array_infos([VarInfo | VarInfos], VarNumMap,
        BytesSoFar, BytesLimit, IntVars, ByteVars, !Info) :-
    VarInfo = layout_var_info(Locn, LiveValueType, _),
    represent_live_value_type(LiveValueType, TypeRval, TypeRvalType, !Info),
    construct_liveval_num_rval(VarNumMap, VarInfo, VarNumRval, !Info),
    (
        LiveValueType = live_value_var(_, _, Type, _),
        get_module_info(!.Info, ModuleInfo),
        is_dummy_argument_type(ModuleInfo, Type),
        % We want to preserve I/O states in registers.
        \+ (
            Locn = direct(reg(_, _))
        )
    ->
        unexpected(this_file, "construct_liveval_array_infos: " ++
            "unexpected reference to dummy value")
    ;
        BytesSoFar < BytesLimit,
        represent_locn_as_byte(Locn, LocnByteRval)
    ->
        Var = live_array_info(LocnByteRval, TypeRval, TypeRvalType,
            VarNumRval),
        construct_liveval_array_infos(VarInfos, VarNumMap,
            BytesSoFar + 1, BytesLimit, IntVars, ByteVars0, !Info),
        ByteVars = [Var | ByteVars0]
    ;
        represent_locn_as_int_rval(Locn, LocnRval),
        Var = live_array_info(LocnRval, TypeRval, TypeRvalType, VarNumRval),
        construct_liveval_array_infos(VarInfos, VarNumMap,
            BytesSoFar, BytesLimit, IntVars0, ByteVars, !Info),
        IntVars = [Var | IntVars0]
    ).

:- pred construct_liveval_num_rval(var_num_map::in,
    layout_var_info::in, rval::out,
    stack_layout_info::in, stack_layout_info::out) is det.

construct_liveval_num_rval(VarNumMap,
        layout_var_info(_, LiveValueType, _), VarNumRval, !Info) :-
    ( LiveValueType = live_value_var(Var, _, _, _) ->
        convert_var_to_int(VarNumMap, Var, VarNum),
        VarNumRval = const(llconst_int(VarNum))
    ;
        VarNumRval = const(llconst_int(0))
    ).

:- pred convert_var_to_int(var_num_map::in, prog_var::in,
    int::out) is det.

convert_var_to_int(VarNumMap, Var, VarNum) :-
    map.lookup(VarNumMap, Var, VarNum0 - _),
    % The variable number has to fit into two bytes. We reserve the largest
    % such number (Limit) to mean that the variable number is too large
    % to be represented. This ought not to happen, since compilation
    % would be glacial at best for procedures with that many variables.
    Limit = (1 << (2 * byte_bits)) - 1,
    int.min(VarNum0, Limit, VarNum).

%---------------------------------------------------------------------------%

    % The representation we build here should be kept in sync
    % with runtime/mercury_ho_call.h, which contains macros to access
    % the data structures we build here.
    %
construct_closure_layout(CallerProcLabel, SeqNo,
        ClosureLayoutInfo, ClosureProcLabel, ModuleName,
        FileName, LineNumber, Origin, GoalPath, !StaticCellInfo,
        RvalsTypes, Data) :-
    DataAddr = layout_addr(
        closure_proc_id(CallerProcLabel, SeqNo, ClosureProcLabel)),
    Data = closure_proc_id_data(CallerProcLabel, SeqNo, ClosureProcLabel,
        ModuleName, FileName, LineNumber, Origin, GoalPath),
    ProcIdRvalType = const(llconst_data_addr(DataAddr, no)) - data_ptr,
    ClosureLayoutInfo = closure_layout_info(ClosureArgs, TVarLocnMap),
    construct_closure_arg_rvals(ClosureArgs,
        ClosureArgRvalsTypes, !StaticCellInfo),
    construct_tvar_vector(TVarLocnMap, TVarVectorRval, !StaticCellInfo),
    RvalsTypes = [ProcIdRvalType, TVarVectorRval - data_ptr |
        ClosureArgRvalsTypes].

:- pred construct_closure_arg_rvals(list(closure_arg_info)::in,
    assoc_list(rval, llds_type)::out,
    static_cell_info::in, static_cell_info::out) is det.

construct_closure_arg_rvals(ClosureArgs, ClosureArgRvalsTypes,
        !StaticCellInfo) :-
    list.map_foldl(construct_closure_arg_rval, ClosureArgs, ArgRvalsTypes,
        !StaticCellInfo),
    list.length(ArgRvalsTypes, Length),
    ClosureArgRvalsTypes =
        [const(llconst_int(Length)) - integer | ArgRvalsTypes].

:- pred construct_closure_arg_rval(closure_arg_info::in,
    pair(rval, llds_type)::out,
    static_cell_info::in, static_cell_info::out) is det.

construct_closure_arg_rval(ClosureArg, ArgRval - ArgRvalType,
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

:- pred make_table_data(rtti_proc_label::in,
    proc_layout_kind::in, proc_table_info::in, maybe(layout_data)::out,
    static_cell_info::in, static_cell_info::out) is det.

make_table_data(RttiProcLabel, Kind, TableInfo, MaybeTableData,
        !StaticCellInfo) :-
    (
        TableInfo = table_io_decl_info(TableArgInfo),
        convert_table_arg_info(TableArgInfo, NumPTIs, PTIVectorRval,
            TVarVectorRval, !StaticCellInfo),
        TableData = table_io_decl_data(RttiProcLabel, Kind,
            NumPTIs, PTIVectorRval, TVarVectorRval),
        MaybeTableData = yes(TableData)
    ;
        TableInfo = table_gen_info(_NumInputs, _NumOutputs,
            _InputSteps, _MaybeOutputSteps, _TableArgInfo),
        % This structure is generated in add_tabling_info_struct in proc_gen.m.
        MaybeTableData = no
    ).

convert_table_arg_info(TableArgInfos, NumPTIs,
        PTIVectorRval, TVarVectorRval, !StaticCellInfo) :-
    TableArgInfos = table_arg_infos(Args, TVarSlotMap),
    list.length(Args, NumPTIs),
    list.map_foldl(construct_table_arg_pti_rval, Args, PTIRvalsTypes,
        !StaticCellInfo),
    add_scalar_static_cell(PTIRvalsTypes, PTIVectorAddr, !StaticCellInfo),
    PTIVectorRval = const(llconst_data_addr(PTIVectorAddr, no)),
    map.map_values(convert_slot_to_locn_map, TVarSlotMap, TVarLocnMap),
    construct_tvar_vector(TVarLocnMap, TVarVectorRval, !StaticCellInfo).

:- pred convert_slot_to_locn_map(tvar::in, table_locn::in,
    set(layout_locn)::out) is det.

convert_slot_to_locn_map(_TVar, SlotLocn, LvalLocns) :-
    (
        SlotLocn = direct(SlotNum),
        LvalLocn = direct(reg(reg_r, SlotNum))
    ;
        SlotLocn = indirect(SlotNum, Offset),
        LvalLocn = indirect(reg(reg_r, SlotNum), Offset)
    ),
    LvalLocns = set.make_singleton_set(LvalLocn).

:- pred construct_table_arg_pti_rval(
    table_arg_info::in, pair(rval, llds_type)::out,
    static_cell_info::in, static_cell_info::out) is det.

construct_table_arg_pti_rval(ClosureArg, ArgRval - ArgRvalType,
        !StaticCellInfo) :-
    ClosureArg = table_arg_info(_, _, Type),
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
:- pred represent_live_value_type(live_value_type::in, rval::out,
    llds_type::out, stack_layout_info::in, stack_layout_info::out) is det.

represent_live_value_type(live_value_succip, Rval, data_ptr, !Info) :-
    represent_special_live_value_type("succip", Rval).
represent_live_value_type(live_value_hp, Rval, data_ptr, !Info) :-
    represent_special_live_value_type("hp", Rval).
represent_live_value_type(live_value_curfr, Rval, data_ptr, !Info) :-
    represent_special_live_value_type("curfr", Rval).
represent_live_value_type(live_value_maxfr, Rval, data_ptr, !Info) :-
    represent_special_live_value_type("maxfr", Rval).
represent_live_value_type(live_value_redofr, Rval, data_ptr, !Info) :-
    represent_special_live_value_type("redofr", Rval).
represent_live_value_type(live_value_redoip, Rval, data_ptr, !Info) :-
    represent_special_live_value_type("redoip", Rval).
represent_live_value_type(live_value_trail_ptr, Rval, data_ptr, !Info) :-
    represent_special_live_value_type("trail_ptr", Rval).
represent_live_value_type(live_value_ticket, Rval, data_ptr, !Info) :-
    represent_special_live_value_type("ticket", Rval).
represent_live_value_type(live_value_unwanted, Rval, data_ptr, !Info) :-
    represent_special_live_value_type("unwanted", Rval).
represent_live_value_type(live_value_var(_, _, Type, _), Rval, LldsType,
        !Info) :-
    % For a stack layout, we can treat all type variables as universally
    % quantified. This is not the argument of a constructor, so we do not
    % need to distinguish between type variables that are and aren't in scope;
    % we can take the variable number directly from the procedure's tvar set.
    ExistQTvars = [],
    NumUnivQTvars = -1,
    get_static_cell_info(!.Info, StaticCellInfo0),
    ll_pseudo_type_info.construct_typed_llds_pseudo_type_info(Type,
        NumUnivQTvars, ExistQTvars, StaticCellInfo0, StaticCellInfo,
        Rval, LldsType),
    set_static_cell_info(StaticCellInfo, !Info).

:- pred represent_special_live_value_type(string::in, rval::out) is det.

represent_special_live_value_type(SpecialTypeName, Rval) :-
    RttiTypeCtor = rtti_type_ctor(unqualified(""), SpecialTypeName, 0),
    DataAddr = rtti_addr(ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info)),
    Rval = const(llconst_data_addr(DataAddr, no)).

%---------------------------------------------------------------------------%

:- pred represent_locn_or_const_as_int_rval(rval::in, rval::out,
    llds_type::out, stack_layout_info::in, stack_layout_info::out) is det.

represent_locn_or_const_as_int_rval(LvalOrConst, Rval, Type, !Info) :-
    (
        LvalOrConst = lval(Lval),
        represent_locn_as_int_rval(direct(Lval), Rval),
        Type = uint_least32
    ;
        LvalOrConst = const(_Const),
        get_module_info(!.Info, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloat),
        rval_type_as_arg(LvalOrConst, UnboxedFloat, LLDSType),

        get_static_cell_info(!.Info, StaticCellInfo0),
        add_scalar_static_cell([LvalOrConst - LLDSType], DataAddr,
            StaticCellInfo0, StaticCellInfo),
        set_static_cell_info(StaticCellInfo, !Info),
        Rval = const(llconst_data_addr(DataAddr, no)),
        Type = data_ptr
    ;
        ( LvalOrConst = binop(_, _, _)
        ; LvalOrConst = unop(_, _)
        ; LvalOrConst = mkword(_, _)
        ; LvalOrConst = mem_addr(_)
        ; LvalOrConst = var(_)
        ),
        unexpected(this_file, "represent_locn_or_const_as_int_rval: bad rval")
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

represent_locn_as_int(direct(Lval), Word) :-
    represent_lval(Lval, Word).
represent_locn_as_int(indirect(Lval, Offset), Word) :-
    represent_lval(Lval, BaseWord),
    expect((1 << long_lval_offset_bits) > Offset, this_file,
        "represent_locn: offset too large to be represented"),
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
    expect(Num > 0, this_file, "represent_lval: bad stackvar"),
    make_tagged_word(lval_stackvar, Num, Word).
represent_lval(parent_stackvar(Num), Word) :-
    expect(Num > 0, this_file, "represent_lval: bad parent_stackvar"),
    make_tagged_word(lval_parent_stackvar, Num, Word).
represent_lval(framevar(Num), Word) :-
    expect(Num > 0, this_file, "represent_lval: bad framevar"),
    make_tagged_word(lval_framevar, Num, Word).
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
    unexpected(this_file, "continuation live value stored in temp register").

represent_lval(succip_slot(_), _) :-
    unexpected(this_file, "continuation live value stored in fixed slot").
represent_lval(redoip_slot(_), _) :-
    unexpected(this_file, "continuation live value stored in fixed slot").
represent_lval(redofr_slot(_), _) :-
    unexpected(this_file, "continuation live value stored in fixed slot").
represent_lval(succfr_slot(_), _) :-
    unexpected(this_file, "continuation live value stored in fixed slot").
represent_lval(prevfr_slot(_), _) :-
    unexpected(this_file, "continuation live value stored in fixed slot").

represent_lval(field(_, _, _), _) :-
    unexpected(this_file, "continuation live value stored in field").
represent_lval(mem_ref(_), _) :-
    unexpected(this_file, "continuation live value stored in mem_ref").
represent_lval(global_var_ref(_), _) :-
    unexpected(this_file, "continuation live value stored in global_var_ref").
represent_lval(lvar(_), _) :-
    unexpected(this_file, "continuation live value stored in lvar").

    % Some things in this module are encoded using a low tag.  This is not
    % done using the normal compiler mkword, but by doing the bit shifting
    % here.
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
    ;       lval_parent_stackvar.

:- pred locn_type_code(locn_type::in, int::out) is det.

% The code of this predicate should be kept in sync with the enum type
% MR_Long_Lval_Type in runtime/mercury_stack_layout.h. Note that the values
% equal to 0 modulo 4 are reserved for representing constants.
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
locn_type_code(lval_indirect,        13).

    % This number of tag bits must be able to encode all values of
    % locn_type_code.
    %
:- func long_lval_tag_bits = int.

long_lval_tag_bits = 4.

    % This number of tag bits must be able to encode the largest offset
    % of a type_info within a typeclass_info.
    %
:- func long_lval_offset_bits = int.

long_lval_offset_bits = 6.

%---------------------------------------------------------------------------%

    % Construct a representation of a variable location as a byte,
    % if this is possible.
    %
:- pred represent_locn_as_byte(layout_locn::in, rval::out) is semidet.

represent_locn_as_byte(LayoutLocn, Rval) :-
    LayoutLocn = direct(Lval),
    represent_lval_as_byte(Lval, Byte),
    0 =< Byte,
    Byte < 256,
    Rval = const(llconst_int(Byte)).

    % Construct a representation of an lval in a byte, if possible.
    %
:- pred represent_lval_as_byte(lval::in, int::out) is semidet.

represent_lval_as_byte(reg(reg_r, Num), Byte) :-
    expect(Num > 0, this_file, "represent_lval_as_byte: bad reg"),
    make_tagged_byte(0, Num, Byte).
represent_lval_as_byte(stackvar(Num), Byte) :-
    expect(Num > 0, this_file, "represent_lval_as_byte: bad stackvar"),
    make_tagged_byte(1, Num, Byte).
represent_lval_as_byte(parent_stackvar(Num), Byte) :-
    expect(Num > 0, this_file, "represent_lval_as_byte: bad parent_stackvar"),
    make_tagged_byte(1, Num, Byte). % XXX placeholder only
represent_lval_as_byte(framevar(Num), Byte) :-
    expect(Num > 0, this_file, "represent_lval_as_byte: bad framevar"),
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

represent_determinism_rval(Detism,
    const(llconst_int(code_model.represent_determinism(Detism)))).

%---------------------------------------------------------------------------%

    % Access to the stack_layout data structure.

    % The per-sourcefile label table maps line numbers to the list of
    % labels that correspond to that line. Each label is accompanied
    % by a flag that says whether the label is the return site of a call
    % or not, and if it is, whether the called procedure is known.

:- type is_label_return
    --->    known_callee(label)
    ;       unknown_callee
    ;       not_a_return.

:- type line_no_info == pair(layout_name, is_label_return).

:- type label_table == map(int, list(line_no_info)).

:- type stack_layout_info
    --->    stack_layout_info(
                module_info             :: module_info,
                agc_stack_layout        :: bool, % generate agc info?
                trace_stack_layout      :: bool, % generate tracing info?
                procid_stack_layout     :: bool, % generate proc id info?
                static_code_addresses   :: bool, % have static code addresses?
                label_counter           :: counter,
                table_infos             :: list(layout_data),
                proc_layouts            :: list(layout_data),
                internal_layouts        :: list(layout_data),
                label_set               :: map(label, data_addr),
                                        % The set of labels (both entry
                                        % and internal) with layouts.
                proc_layout_name_list   :: list(layout_name),
                                        % The list of proc_layouts in
                                        % the module.
                string_table            :: string_table,
                label_tables            :: map(string, label_table),
                                        % Maps each filename that
                                        % contributes labels to this module
                                        % to a table describing those
                                        % labels.
                static_cell_info        :: static_cell_info,
                has_user_event          :: bool
            ).

:- pred get_module_info(stack_layout_info::in, module_info::out) is det.
:- pred get_agc_stack_layout(stack_layout_info::in, bool::out) is det.
:- pred get_trace_stack_layout(stack_layout_info::in, bool::out) is det.
:- pred get_procid_stack_layout(stack_layout_info::in, bool::out) is det.
:- pred get_static_code_addresses(stack_layout_info::in, bool::out) is det.
:- pred get_table_infos(stack_layout_info::in, list(layout_data)::out) is det.
:- pred get_proc_layout_data(stack_layout_info::in, list(layout_data)::out)
    is det.
:- pred get_internal_layout_data(stack_layout_info::in, list(layout_data)::out)
    is det.
:- pred get_label_set(stack_layout_info::in, map(label, data_addr)::out)
    is det.
:- pred get_string_table(stack_layout_info::in, string_table::out) is det.
:- pred get_label_tables(stack_layout_info::in, map(string, label_table)::out)
    is det.
:- pred get_static_cell_info(stack_layout_info::in, static_cell_info::out)
    is det.
:- pred get_has_user_event(stack_layout_info::in, bool::out) is det.

get_module_info(LI, LI ^ module_info).
get_agc_stack_layout(LI, LI ^ agc_stack_layout).
get_trace_stack_layout(LI, LI ^ trace_stack_layout).
get_procid_stack_layout(LI, LI ^ procid_stack_layout).
get_static_code_addresses(LI, LI ^ static_code_addresses).
get_table_infos(LI, LI ^ table_infos).
get_proc_layout_data(LI, LI ^ proc_layouts).
get_internal_layout_data(LI, LI ^ internal_layouts).
get_label_set(LI, LI ^ label_set).
get_string_table(LI, LI ^ string_table).
get_label_tables(LI, LI ^ label_tables).
get_static_cell_info(LI, LI ^ static_cell_info).
get_has_user_event(LI, LI ^ has_user_event).

:- pred allocate_label_number(int::out,
    stack_layout_info::in, stack_layout_info::out) is det.

allocate_label_number(LabelNum, !LI) :-
    Counter0 = !.LI ^ label_counter,
    counter.allocate(LabelNum, Counter0, Counter),
    !:LI = !.LI ^ label_counter := Counter.

:- pred add_table_data(maybe(layout_data)::in,
    stack_layout_info::in, stack_layout_info::out) is det.

add_table_data(MaybeTableIoDeclData, !LI) :-
    (
        MaybeTableIoDeclData = yes(TableIoDeclData),
        TableIoDecls0 = !.LI ^ table_infos,
        TableIoDecls = [TableIoDeclData | TableIoDecls0],
        !:LI = !.LI ^ table_infos := TableIoDecls
    ;
        MaybeTableIoDeclData = no
    ).

:- pred add_proc_layout_data(layout_data::in, layout_name::in, label::in,
    stack_layout_info::in, stack_layout_info::out) is det.

add_proc_layout_data(ProcLayout, ProcLayoutName, Label, !LI) :-
    ProcLayouts0 = !.LI ^ proc_layouts,
    ProcLayouts = [ProcLayout | ProcLayouts0],
    LabelSet0 = !.LI ^ label_set,
    map.det_insert(LabelSet0, Label, layout_addr(ProcLayoutName), LabelSet),
    ProcLayoutNames0 = !.LI ^ proc_layout_name_list,
    ProcLayoutNames = [ProcLayoutName | ProcLayoutNames0],
    !:LI = !.LI ^ proc_layouts := ProcLayouts,
    !:LI = !.LI ^ label_set := LabelSet,
    !:LI = !.LI ^ proc_layout_name_list := ProcLayoutNames.

:- pred add_internal_layout_data(layout_data::in,
    label::in, layout_name::in, stack_layout_info::in,
    stack_layout_info::out) is det.

add_internal_layout_data(InternalLayout, Label, LayoutName, !LI) :-
    InternalLayouts0 = !.LI ^ internal_layouts,
    InternalLayouts = [InternalLayout | InternalLayouts0],
    LabelSet0 = !.LI ^ label_set,
    map.det_insert(LabelSet0, Label, layout_addr(LayoutName), LabelSet),
    !:LI = !.LI ^ internal_layouts := InternalLayouts,
    !:LI = !.LI ^ label_set := LabelSet.

:- pred set_string_table(string_table::in,
    stack_layout_info::in, stack_layout_info::out) is det.

:- pred set_label_tables(map(string, label_table)::in,
    stack_layout_info::in, stack_layout_info::out) is det.

:- pred set_static_cell_info(static_cell_info::in,
    stack_layout_info::in, stack_layout_info::out) is det.

:- pred set_has_user_event(bool::in,
    stack_layout_info::in, stack_layout_info::out) is det.

set_string_table(ST, LI, LI ^ string_table := ST).
set_label_tables(LT, LI, LI ^ label_tables := LT).
set_static_cell_info(SCI, LI, LI ^ static_cell_info := SCI).
set_has_user_event(HUE, LI, LI ^ has_user_event := HUE).

%---------------------------------------------------------------------------%
%
% Access to the string_table data structure
%

:- type string_table
    --->    string_table(
                map(string, int),   % Maps strings to their offsets.
                list(string),       % List of strings so far,
                                    % in reverse order.
                int                 % Next available offset
            ).

lookup_string_in_table(String, Offset, !Info) :-
    StringTable0 = !.Info ^ string_table,
    StringTable0 = string_table(TableMap0, TableList0, TableOffset0),
    ( map.search(TableMap0, String, OldOffset) ->
        Offset = OldOffset
    ;
        string.length(String, Length),
        TableOffset = TableOffset0 + Length + 1,
        % We use a 32 bit unsigned integer to represent the offset.
        % Computing that limit exactly without getting an overflow
        % or using unportable code isn't trivial. The code below
        % is overly conservative, requiring the offset to be
        % representable in only 30 bits. The over-conservatism
        % should not be an issue; the machine will run out of
        % virtual memory before the test below fails, for the
        % next several years anyway. (Compiling a module that has
        % a 1 Gb string table will require several tens of Gb
        % of other compiler structures.)
        TableOffset < (1 << ((4 * byte_bits) - 2))
    ->
        Offset = TableOffset0,
        map.det_insert(TableMap0, String, TableOffset0, TableMap),
        TableList = [String | TableList0],
        StringTable = string_table(TableMap, TableList, TableOffset),
        set_string_table(StringTable, !Info)
    ;
        % Says that the name of the variable is "TOO_MANY_VARIABLES".
        Offset = 1
    ).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "stack_layout.m".

%---------------------------------------------------------------------------%
