%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2000,2002-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: continuation_info.m.
% Main author: trd.
% Extensive modifications by zs.
%
% This file defines the data structures the code generator uses to collect
% information that will later be converted into layout tables for accurate
% garbage collection, stack tracing, execution tracing, deep profiling and
% perhaps other purposes.
%
% Information is collected in several passes.
%
%   1 Before we start generating code for a procedure,
%     we initialize the set of internal labels for which we have
%     layout information to the empty set. This set is stored in
%     the code generator state.
%
%   2 During code generation for the procedure, provided the option
%     trace_stack_layouts is set, we add layout information for labels
%     that represent trace ports to the code generator state. If
%     agc_stack_layouts is set, we add layout information for the stack
%     label in each resumption point. And regardless of option settings,
%     we also generate layouts to be attached to any closures we create.
%
%   3 After we finish generating code for a procedure, we record
%     all the static information about the procedure (some of which
%     is available only after code generation), together with the
%     info about internal labels accumulated in the code generator state,
%     in the global_data structure.
%
%   4 If agc_stack_layouts is set, we make a pass over the
%     optimized code recorded in the final LLDS instructions.
%     In this pass, we collect information from call instructions
%     about the internal labels to which calls can return.
%     This info will also go straight into the global_data.
%
% This module defines the data structures used by all passes. It also
% implements the whole of pass 4, and various fractions of the other passes.
%
% stack_layout.m converts the information collected in this module into
% stack_layout tables.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.continuation_info.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.trace_params.
:- import_module ll_backend.global_data.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module ll_backend.trace_gen.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Information for any procedure, includes information about the
    % procedure itself, and any internal labels within it.
    %
:- type proc_layout_info
    --->    proc_layout_info(
                % Determines which stack is used.
                pli_detism              :: determinism,

                % The effective trace level of the procedure.
                pli_eff_trace_level     :: eff_trace_level,

                % The evaluation method of the procedure.
                pli_eval_method         :: eval_method,

                % Does the level of execution tracing of this procedure require
                % a representation of the procedure body in the layout
                % structures? Note that even if this field is set to
                % trace_does_not_need_body_rep, other options (such as deep
                % profiling) may still ask for the body to be included.
                pli_trace_body_rep      :: trace_needs_body_rep,

                % Do we require the procedure id section of the procedure
                % layout to be present, even if the option procid_stack_layout
                % is not set?
                pli_need_proc_id        :: bool,

                % True iff we need the names of all the variables.
                pli_need_all_names      :: bool,

                % The identity of the procedure.
                pli_rtti_proc_label     :: rtti_proc_label,

                pli_entry_label         :: label,

                % Number of stack slots.
                pli_stack_slot_count    :: int,

                % Location of succip on stack.
                pli_succip_slot         :: maybe(int),

                % If the trace level is not none, this contains the label
                % associated with the call event, whose stack layout says
                % which variables were live and where on entry.
                pli_call_label          :: maybe(label),

                % The number of the highest numbered rN and fN registers that
                % can contain useful information during a call to MR_trace from
                % within this procedure.
                pli_max_trace_reg_r     :: int,
                pli_max_trace_reg_f     :: int,

                % The head variables, in order, including the ones introduced
                % by the compiler.
                pli_head_vars           :: list(prog_var),

                % The modes of the head variables.
                pli_arg_modes           :: list(mer_mode),

                % The body of the procedure.
                pli_proc_body           :: hlds_goal,

                % The instmap at the start of the procedure body.
                pli_initial_instmap     :: instmap,

                % Info about the stack slots used for tracing.
                pli_trace_slot_info     :: trace_slot_info,

                % The names and types of all the variables.
                pli_var_table           :: var_table,

                % Info for each internal label, needed for basic_stack_layouts.
                pli_internal_map        :: proc_label_layout_info,

                pli_maybe_table_info    :: maybe(proc_layout_table_info),

                pli_oisu_kind_fors      :: list(oisu_pred_kind_for),
                pli_deep_prof           :: maybe(proc_deep_prof_info)
            ).

:- type proc_deep_prof_info
    --->    proc_deep_prof_info(
                pdpi_proc_static        :: hlds_proc_static,
                pdpi_excp_slots         :: deep_excp_slots,
                pdpi_orig_body          :: deep_original_body
            ).

:- type proc_layout_table_info
    --->    proc_table_io_entry(
                proc_table_io_info
            )
    ;       proc_table_struct(
                proc_table_struct_info
            ).

:- type trace_needs_body_rep
    --->    trace_needs_body_rep
    ;       trace_does_not_need_body_rep.

    % Information about the labels internal to a procedure.
    %
:- type proc_label_layout_info == map(int, internal_layout_info).

    % Information for an internal label.
    %
    % There are three ways for the compiler to generate labels for
    % which layouts may be required:
    %
    % (a) as the label associated with a trace port,
    % (b) as the label associated with resume point that gets stored
    %     as a redoip in a nondet stack frame, and
    % (c) as the return label of some kind of call (plain, method or h-o).
    %
    % Label optimizations may redirect a call return away from the
    % originally generated label to another label, possibly one
    % that is associated with a trace port. This optimization may
    % also direct returns from more than one call to the same label.
    %
    % We may be interested in the layout of things at a label for three
    % different reasons: for stack tracing, for accurate gc, and for
    % execution tracing (which may include up-level printing from the
    % debugger).
    %
    % - For stack tracing, we are interested only in call return labels.
    %   Even for these, we need only the pointer to the procedure layout
    %   info; we do not need any information about variables.
    %
    % - For accurate gc, we are interested only in resume point labels
    %   and call return labels. We need to know about all the variables
    %   that can be accessed after the label; this is the intersection of
    %   all the variables denoted as live in the respective labels.
    %   (Variables which are not in the intersection are not guaranteed
    %   to have a meaningful value on all execution paths that lead to the
    %   label.)
    %
    % - For execution tracing, our primary interest is in trace port
    %   labels. At these labels we only want info about named variables,
    %   but we may want this info even if the variable will never be
    %   referred to again.
    %
    %   When the trace level requires support for up-level printing,
    %   execution tracing also requires information about return labels.
    %   The variables about which we want info at these labels is a subset
    %   of the variables agc is interested in (the named subset).
    %   We do not collect this set explicitly. Instead, if we are doing
    %   execution tracing, we collect agc layout info as usual, and
    %   (if we not really doing agc) remove the unnamed variables
    %   in stack_layout.m.
    %
    % For labels which correspond to a trace port (part (a) above),
    % we record information in the first field. Since trace.m generates
    % a unique label for each trace port, this field is never updated
    % once it is set in pass 2.
    %
    % For labels which correspond to redoips (part (b) above), we record
    % information in the second field. Since code_info.m generates
    % unique labels for each resumption point, this field is never updated
    % once it is set in pass 2.
    %
    % For labels which correspond to a call return (part (c) above),
    % we record information in the third field during pass 4. If execution
    % tracing is turned on, then jumpopt.m will not redirect call return
    % addresses, and thus each label will correspond to at most one call
    % return. If execution tracing is turned off, jumpopt.m may redirect
    % call return addresses, which means that a label can serve as the
    % return label for more than one call. In that case, this field can be
    % updated after it is set. This updating requires taking the
    % intersection of the sets of live variables, and gathering up all the
    % contexts into a list. Later, stack_layout.m will pick one (valid)
    % context essentially at random, which is OK because the picked
    % context will not be used for anything, except possibly for debugging
    % native gc.
    %
    % Since a call may return to the label of an internal port, it is
    % possible for both fields to be set. In this case, stack_layout.m
    % will take the union of the relevant info. If neither field is set,
    % then the label's layout is required only for stack tracing.
    %
:- type internal_layout_info
    --->    internal_layout_info(
                maybe(trace_port_layout_info),
                maybe(layout_label_info),
                maybe(return_layout_info)
            ).

:- type trace_port_layout_info
    --->    trace_port_layout_info(
                port_context    :: prog_context,
                port_type       :: trace_port,
                port_is_hidden  :: bool,
                port_path       :: forward_goal_path,
                port_user       :: maybe(user_event_info),
                port_label      :: layout_label_info
            ).

:- type return_layout_info
    --->    return_layout_info(
                assoc_list(code_addr, pair(prog_context, forward_goal_path)),
                layout_label_info
            ).

    % Information about the layout of live data for a label.
    %
:- type layout_label_info
    --->    layout_label_info(
                set(layout_var_info),
                % Live vars and their locations/names.

                map(tvar, set(layout_locn))
                % Locations of polymorphic type vars.
            ).

:- type layout_var_info
    --->    layout_var_info(
                layout_locn,        % The location of the variable.
                live_value_type,    % Info about the variable.
                string              % Where in the compiler this
                                    % layout_var_info was created
            ).

:- type user_attribute
    --->    user_attribute(
                attr_locn               :: rval,
                attr_var                :: prog_var
            ).

:- type user_event_info
    --->    user_event_info(
                user_port_number      :: int,
                user_attributes       :: list(maybe(user_attribute))
            ).

:- type closure_layout_info
    --->    closure_layout_info(
                list(closure_arg_info),
                % There is one closure_arg_info for each argument of the called
                % procedure, even the args which are not in the closure

                map(tvar, set(layout_locn))
                % Locations of polymorphic type vars,
                % encoded so that rN refers to argument N.
            ).

:- type closure_arg_info
    --->    closure_arg_info(
                mer_type,   % The type of the argument.
                mer_inst    % The initial inst of the argument.

                            % It may be useful in the future to include
                            % info about the final insts and about
                            % the determinism. This would allow us
                            % to implement checked dynamic inst casts,
                            % which may be helpful for dynamic loading.
                            % It may also be useful for printing
                            % closures and for providing user-level
                            % RTTI access.
            ).

:- type slot_contents
    --->    slot_ticket             % A ticket (trail pointer).
    ;       slot_ticket_counter     % A copy of the ticket counter.
    ;       slot_trace_data
    ;       slot_lookup_disj_cur
    ;       slot_lookup_switch_cur
    ;       slot_lookup_switch_max
    ;       slot_sync_term          % A syncronization term used
                                    % at the end of par_conjs.
                                    % See par_conj_gen.m for details.
    ;       slot_region_ite
    ;       slot_region_disj
    ;       slot_region_commit
    ;       slot_success_record     % A record of whether a piece of code
                                    % has ever succeeded.
    ;       slot_lval(lval).

    % Call maybe_collect_call_continuations_in_cproc on every procedure
    % in the list.
    %
:- pred maybe_collect_call_continuations_in_cprocs(module_info::in,
    list(c_procedure)::in, global_data::in, global_data::out) is det.

    % Check whether this procedure ought to have any layout structures
    % generated for it. If yes, then update the global_data to
    % include all the continuation labels within a proc. Whether or not
    % the information about a continuation label includes the variables
    % live at that label depends on the values of options.
    %
:- pred maybe_collect_call_continuations_in_cproc(module_info::in,
    c_procedure::in, global_data::in, global_data::out) is det.

    % Check whether the given procedure should have at least (a) a basic
    % stack layout, and (b) a procedure id layout generated for it.
    % The two bools returned answer these two questions respectively.
    %
:- pred basic_stack_layout_for_proc(globals::in, pred_info::in,
    bool::out, bool::out) is det.

    % Generate the layout information we need for the return point of a call.
    %
:- pred cont_info_generate_return_live_lvalues(globals::in, proc_info::in,
    eff_trace_level::in, assoc_list(prog_var, arg_loc)::in,
    instmap::in, list(prog_var)::in, map(prog_var, set(lval))::in,
    assoc_list(lval, slot_contents)::in, bool::in, list(liveinfo)::out) is det.

    % Generate the layout information we need for a resumption point,
    % a label where forward execution can restart after backtracking.
    %
:- pred generate_resume_layout(proc_info::in, instmap::in,
    map(prog_var, set(lval))::in, assoc_list(lval, slot_contents)::in,
    layout_label_info::out) is det.

    % Generate the layout information we need to include in a closure.
    %
:- pred generate_closure_layout(module_info::in, pred_id::in, proc_id::in,
    closure_layout_info::out) is det.

    % For each type variable in the given list, find out where the
    % typeinfo var for that type variable is.
    %
:- pred find_typeinfos_for_tvars(var_table::in, rtti_varmaps::in,
    list(tvar)::in, map(prog_var, set(lval))::in,
    map(tvar, set(layout_locn))::out) is det.

:- pred generate_table_arg_type_info(var_table::in, rtti_varmaps::in,
    assoc_list(prog_var, int)::in, table_arg_infos::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_proc_util.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_scan.
:- import_module parse_tree.prog_type_test.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

maybe_collect_call_continuations_in_cprocs(_, [], !GlobalData).
maybe_collect_call_continuations_in_cprocs(ModuleInfo, [CProc | CProcs],
        !GlobalData) :-
    maybe_collect_call_continuations_in_cproc(ModuleInfo, CProc,
        !GlobalData),
    maybe_collect_call_continuations_in_cprocs(ModuleInfo, CProcs,
        !GlobalData).

maybe_collect_call_continuations_in_cproc(ModuleInfo, CProc, !GlobalData) :-
    CProc ^ cproc_id = proc(PredId, _),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    module_info_get_globals(ModuleInfo, Globals),
    basic_stack_layout_for_proc(Globals, PredInfo, Layout, _),
    (
        Layout = yes,
        EffTraceLevel = CProc ^ cproc_eff_trace_level,
        globals.want_return_var_layouts(Globals, EffTraceLevel,
            WantReturnLayout),
        collect_call_continuations_in_cproc(WantReturnLayout, CProc,
            !GlobalData)
    ;
        Layout = no
    ).

:- type call_info
    --->    call_info(
                call_return_label       :: label,
                call_target             :: code_addr,
                call_live_on_return     :: list(liveinfo),
                call_context            :: term.context,

                % The position of the call in the body if tracing is enabled.
                call_goal_path          :: maybe(forward_goal_path)
            ).

    % Process the list of instructions for this proc, adding
    % all internal label information to global_data.
    %
:- pred collect_call_continuations_in_cproc(bool::in, c_procedure::in,
    global_data::in, global_data::out) is det.

collect_call_continuations_in_cproc(WantReturnInfo, CProc, !GlobalData) :-
    PredProcId = CProc ^ cproc_id,
    global_data_get_proc_layout(!.GlobalData, PredProcId, ProcLayoutInfo0),
    Instrs = CProc ^ cproc_code,
    Internals0 = ProcLayoutInfo0 ^ pli_internal_map,
    list.filter_map(get_call_info, Instrs, CallInfos),
    list.foldl(collect_continuation(WantReturnInfo), CallInfos,
        Internals0, Internals),
    ProcLayoutInfo = ProcLayoutInfo0 ^ pli_internal_map := Internals,
    global_data_update_proc_layout(PredProcId, ProcLayoutInfo, !GlobalData).

:- pred get_call_info(instruction::in, call_info::out) is semidet.

get_call_info(Instr, CallInfo) :-
    Instr = llds_instr(Uinstr, _Comment),
    Uinstr = llcall(Target, Return, LiveInfo, Context, GoalPath, _),
    Return = code_label(ReturnLabel),
    CallInfo = call_info(ReturnLabel, Target, LiveInfo, Context, GoalPath).

%-----------------------------------------------------------------------------%

    % Collect the liveness information from a single return label
    % and add it to the internals.
    %
:- pred collect_continuation(bool::in, call_info::in,
    proc_label_layout_info::in, proc_label_layout_info::out) is det.

collect_continuation(WantReturnInfo, CallInfo, !Internals) :-
    CallInfo = call_info(ReturnLabel, Target, LiveInfoList, Context,
        MaybeGoalPath),
    % We could check not only that the return label is an internal label,
    % but also that it belongs to the current procedure, but this would be
    % serious paranoia.
    (
        ReturnLabel = internal_label(ReturnLabelNum, _)
    ;
        ReturnLabel = entry_label(_, _),
        unexpected($pred, "bad return")
    ),
    ( if map.search(!.Internals, ReturnLabelNum, Internal0) then
        Internal0 = internal_layout_info(Port0, Resume0, Return0)
    else
        Port0 = no,
        Resume0 = no,
        Return0 = no
    ),
    (
        WantReturnInfo = yes,
        (
            MaybeGoalPath = no,
            %  XXX We used to handle these situations by using an empty path.
            %  XXX Should we throw an exception?
            Return = Return0
        ;
            MaybeGoalPath = yes(GoalPath),
            convert_return_data(LiveInfoList, VarInfoSet, TypeInfoMap),
            (
                Return0 = no,
                Layout = layout_label_info(VarInfoSet, TypeInfoMap),
                ReturnInfo = return_layout_info(
                    [Target - (Context - GoalPath)], Layout),
                Return = yes(ReturnInfo)
            ;
                % If a var is known to be dead on return from one call,
                % it cannot be accessed on returning from the other calls
                % that reach the same return address either.
                Return0 = yes(ReturnInfo0),
                ReturnInfo0 = return_layout_info(TargetsContexts0, Layout0),
                Layout0 = layout_label_info(LV0, TV0),
                set.intersect(LV0, VarInfoSet, LV),
                map.intersect(set.intersect, TV0, TypeInfoMap, TV),
                Layout = layout_label_info(LV, TV),
                TargetContexts = [Target - (Context - GoalPath)
                    | TargetsContexts0],
                ReturnInfo = return_layout_info(TargetContexts, Layout),
                Return = yes(ReturnInfo)
            )
        )
    ;
        WantReturnInfo = no,
        Return = Return0
    ),
    Internal = internal_layout_info(Port0, Resume0, Return),
    map.set(ReturnLabelNum, Internal, !Internals).

:- pred convert_return_data(list(liveinfo)::in,
    set(layout_var_info)::out, map(tvar, set(layout_locn))::out) is det.

convert_return_data(LiveInfos, VarInfoSet, TypeInfoMap) :-
    GetVarInfo = (pred(LiveLval::in, VarInfo::out) is det :-
        LiveLval = live_lvalue(Lval, LiveValueType, _),
        VarInfo = layout_var_info(Lval, LiveValueType, "convert_return_data")
    ),
    list.map(GetVarInfo, LiveInfos, VarInfoList),
    GetTypeInfo = (pred(LiveLval::in, LiveTypeInfoMap::out) is det :-
        LiveLval = live_lvalue(_, _, LiveTypeInfoMap)
    ),
    list.map(GetTypeInfo, LiveInfos, TypeInfoMapList),
    map.init(Empty),
    list.foldl((pred(TIM1::in, TIM2::in, TIM::out) is det :-
            map.union(set.intersect, TIM1, TIM2, TIM)
        ), TypeInfoMapList, Empty, TypeInfoMap),
    set.list_to_set(VarInfoList, VarInfoSet).

%-----------------------------------------------------------------------------%

basic_stack_layout_for_proc(Globals, PredInfo, BasicLayout,
        ForceProcIdLayout) :-
    ( if
        globals.lookup_bool_option(Globals, stack_trace_higher_order, yes),
        some_arg_is_higher_order(PredInfo)
    then
        BasicLayout = yes,
        ForceProcIdLayout = yes
    else if
        globals.lookup_bool_option(Globals, basic_stack_layout, yes)
    then
        BasicLayout = yes,
        ForceProcIdLayout = no
    else
        BasicLayout = no,
        ForceProcIdLayout = no
    ).

:- pred some_arg_is_higher_order(pred_info::in) is semidet.

some_arg_is_higher_order(PredInfo) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    some [Type] (
        list.member(Type, ArgTypes),
        type_is_higher_order(Type)
    ).

%-----------------------------------------------------------------------------%

cont_info_generate_return_live_lvalues(Globals, ProcInfo,
        EffTraceLevel, OutputArgLocs, ReturnInstMap, Vars, VarLocs, Temps,
        OkToDeleteAny, LiveLvalues) :-
    globals.want_return_var_layouts(Globals, EffTraceLevel,
        WantReturnVarLayout),
    proc_info_get_stack_slots(ProcInfo, StackSlots),
    find_return_var_lvals(StackSlots, OkToDeleteAny, OutputArgLocs,
        Vars, VarLvals),
    proc_info_get_var_table(ProcInfo, VarTable),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    generate_var_live_lvalues(VarTable, RttiVarMaps, WantReturnVarLayout,
        ReturnInstMap, VarLocs, VarLvals, VarLiveLvalues),
    generate_temp_live_lvalues(Temps, TempLiveLvalues),
    LiveLvalues = VarLiveLvalues ++ TempLiveLvalues.

:- pred find_return_var_lvals(stack_slots::in, bool::in,
    assoc_list(prog_var, arg_loc)::in, list(prog_var)::in,
    assoc_list(prog_var, lval)::out) is det.

find_return_var_lvals(_, _, _, [], []).
find_return_var_lvals(StackSlots, OkToDeleteAny, OutputArgLocs,
        [Var | Vars], VarLvals) :-
    find_return_var_lvals(StackSlots, OkToDeleteAny, OutputArgLocs,
        Vars, TailVarLvals),
    ( if assoc_list.search(OutputArgLocs, Var, ArgLoc) then
        % On return, output arguments are in their registers.
        code_util.arg_loc_to_register(ArgLoc, Lval),
        VarLvals = [Var - Lval | TailVarLvals]
    else if map.search(StackSlots, Var, Slot) then
        % On return, other live variables are in their stack slots.
        VarLvals = [Var - stack_slot_to_lval(Slot) | TailVarLvals]
    else
        (
            OkToDeleteAny = yes,
            VarLvals = TailVarLvals
        ;
            OkToDeleteAny = no,
            unexpected($pred, "no slot")
        )
    ).

:- pred generate_temp_live_lvalues(assoc_list(lval, slot_contents)::in,
    list(liveinfo)::out) is det.

generate_temp_live_lvalues([], []).
generate_temp_live_lvalues([Temp | Temps], [Live | Lives]) :-
    Temp = Slot - Contents,
    live_value_type(Contents, LiveLvalueType),
    map.init(Empty),
    Live = live_lvalue(locn_direct(Slot), LiveLvalueType, Empty),
    generate_temp_live_lvalues(Temps, Lives).

:- pred generate_var_live_lvalues(var_table::in, rtti_varmaps::in, bool::in,
    instmap::in, map(prog_var, set(lval))::in,
    assoc_list(prog_var, lval)::in, list(liveinfo)::out) is det.

generate_var_live_lvalues(_, _, _, _, _, [], []).
generate_var_live_lvalues(VarTable, RttiVarMaps, WantReturnVarLayout,
        InstMap, VarLocs, [Var - Lval | VarLvals], [Live | Lives]) :-
    (
        WantReturnVarLayout = yes,
        generate_layout_for_var(VarTable, InstMap, Var, LiveValueType,
            TypeVars),
        find_typeinfos_for_tvars(VarTable, RttiVarMaps, TypeVars, VarLocs,
            TypeParams),
        Live = live_lvalue(locn_direct(Lval), LiveValueType, TypeParams)
    ;
        WantReturnVarLayout = no,
        map.init(Empty),
        Live = live_lvalue(locn_direct(Lval), live_value_unwanted, Empty)
    ),
    generate_var_live_lvalues(VarTable, RttiVarMaps, WantReturnVarLayout,
        InstMap, VarLocs, VarLvals, Lives).

%---------------------------------------------------------------------------%

generate_resume_layout(ProcInfo, InstMap, ResumeMap, Temps, Layout) :-
    map.to_assoc_list(ResumeMap, ResumeList),
    set.init(TVars0),
    proc_info_get_var_table(ProcInfo, VarTable),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    generate_resume_layout_for_vars(VarTable, InstMap, ResumeList,
        [], VarInfos, TVars0, TVars),
    set.list_to_set(VarInfos, VarInfoSet),
    set.to_sorted_list(TVars, TVarList),
    find_typeinfos_for_tvars(VarTable, RttiVarMaps, TVarList, ResumeMap,
        TVarInfoMap),
    generate_temp_var_infos(Temps, TempInfos),
    set.list_to_set(TempInfos, TempInfoSet),
    set.union(VarInfoSet, TempInfoSet, AllInfoSet),
    Layout = layout_label_info(AllInfoSet, TVarInfoMap).

:- pred generate_resume_layout_for_vars(var_table::in, instmap::in,
    assoc_list(prog_var, set(lval))::in,
    list(layout_var_info)::in, list(layout_var_info)::out,
    set(tvar)::in, set(tvar)::out) is det.

generate_resume_layout_for_vars(_, _, [], !VarInfos, !TVars).
generate_resume_layout_for_vars(VarTable, InstMap, [Var - LvalSet | VarLvals],
        !VarInfos, !TVars) :-
    lookup_var_entry(VarTable, Var, Entry),
    Entry = vte(_Name, _Type, IsDummy),
    (
        IsDummy = is_dummy_type
    ;
        IsDummy = is_not_dummy_type,
        generate_resume_layout_for_var(VarTable, InstMap, Var, LvalSet,
            VarInfo, TypeVars),
        set.insert_list(TypeVars, !TVars),
        !:VarInfos = [VarInfo | !.VarInfos]
    ),
    generate_resume_layout_for_vars(VarTable, InstMap, VarLvals,
        !VarInfos, !TVars).

:- pred generate_resume_layout_for_var(var_table::in, instmap::in,
    prog_var::in, set(lval)::in, layout_var_info::out, list(tvar)::out) is det.

generate_resume_layout_for_var(VarTable, InstMap, Var, LvalSet,
        VarInfo, TypeVars) :-
    set.to_sorted_list(LvalSet, LvalList),
    ( if LvalList = [LvalPrime] then
        Lval = LvalPrime
    else
        unexpected($pred, "var has more than one lval in stack resume map")
    ),
    ( if Lval = stackvar(N) then
        expect(N > 0, $pred, "bad stackvar")
    else if Lval = framevar(N) then
        expect(N > 0, $pred, "bad framevar")
    else if Lval = double_stackvar(_, N) then
        expect(N > 0, $pred, "bad double_stackvar")
    else
        true
    ),
    generate_layout_for_var(VarTable, InstMap, Var, LiveValueType, TypeVars),
    VarInfo = layout_var_info(locn_direct(Lval), LiveValueType,
        "generate_result_layout_for_var").

:- pred generate_temp_var_infos(assoc_list(lval, slot_contents)::in,
    list(layout_var_info)::out) is det.

generate_temp_var_infos([], []).
generate_temp_var_infos([Temp | Temps], [Live | Lives]) :-
    Temp = Slot - Contents,
    live_value_type(Contents, LiveLvalueType),
    Live = layout_var_info(locn_direct(Slot), LiveLvalueType,
        "generate_temp_var_infos"),
    generate_temp_var_infos(Temps, Lives).

%---------------------------------------------------------------------------%

:- pred generate_layout_for_var(var_table::in, instmap::in, prog_var::in,
    live_value_type::out, list(tvar)::out) is det.

generate_layout_for_var(VarTable, _InstMap, Var, LiveValueType, TypeVars) :-
    lookup_var_entry(VarTable, Var, VarEntry),
    VarEntry = vte(Name, Type, _IsDummy),

%   In some programs, specifically zm_enum.m, the Mercury program generated
%   for the enum.zinc g12 test case, this call to inst_is_ground can be
%   a huge performance problem. The reason for that is the following.
%
%   - The program builds a huge data structure.
%   - The nth variable in this data structure is built from the previous
%     n-1 variables,
%   - The size of the inst of the nth variable is therefore proportional to n.
%   - The total sizes of the n insts is therefore proportional to n^2.
%   - The value of n can be huge.
%
%   Since we do not yet use the inst field in live_value_vars, there is
%   no point in incurring the expense of filling it in.
%
%   instmap_lookup_var(InstMap, Var, Inst),
%   ( if inst_match.inst_is_ground(ModuleInfo, Inst) then
%       LldsInst = llds_inst_ground
%   else
%       LldsInst = llds_inst_partial(Inst)
%   ),

    LldsInst = llds_inst_better_be_ground,
    LiveValueType = live_value_var(Var, Name, Type, LldsInst),
    type_vars_in_type(Type, TypeVars).

%---------------------------------------------------------------------------%

generate_closure_layout(ModuleInfo, PredId, ProcId, ClosureLayout) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_arg_info(ProcInfo, ArgInfos),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    proc_info_get_initial_instmap(ModuleInfo, ProcInfo, InstMap),
    map.init(VarLocs0),
    set.init(TypeVars0),
    ( if
        build_closure_info(HeadVars, ArgTypes, ArgInfos, ArgLayouts, InstMap,
            UseFloatRegs, VarLocs0, VarLocs, TypeVars0, TypeVars)
    then
        set.to_sorted_list(TypeVars, TypeVarsList),
        find_typeinfos_for_tvars(VarTable, RttiVarMaps, TypeVarsList, VarLocs,
            TypeInfoDataMap),
        ClosureLayout = closure_layout_info(ArgLayouts, TypeInfoDataMap)
    else
        unexpected($pred, "proc headvars and pred argtypes disagree on arity")
    ).

:- pred build_closure_info(list(prog_var)::in,
    list(mer_type)::in, list(arg_info)::in,  list(closure_arg_info)::out,
    instmap::in, bool::in,
    map(prog_var, set(lval))::in, map(prog_var, set(lval))::out,
    set(tvar)::in, set(tvar)::out) is semidet.

build_closure_info([], [], [], [], _, _, !VarLocs, !TypeVars).
build_closure_info([Var | Vars], [Type0 | Types],
        [ArgInfo | ArgInfos], [Layout | Layouts], InstMap, UseFloatRegs,
        !VarLocs, !TypeVars) :-
    ArgInfo = arg_info(ArgLoc, _ArgMode),
    % If the float argument is passed via a regular register then replace the
    % type_ctor_info in the closure layout so that we can distinguish those
    % arguments from float arguments passed via float registers.
    ( if
        UseFloatRegs = yes,
        Type0 = float_type,
        ArgLoc = reg(reg_r, _)
    then
        Type = float_box_type
    else
        Type = Type0
    ),
    instmap_lookup_var(InstMap, Var, Inst),
    Layout = closure_arg_info(Type, Inst),
    arg_loc_to_register(ArgLoc, Reg),
    Locations = set.make_singleton_set(Reg),
    map.det_insert(Var, Locations, !VarLocs),
    type_vars_in_type(Type, VarTypeVars),
    set.insert_list(VarTypeVars, !TypeVars),
    build_closure_info(Vars, Types, ArgInfos, Layouts, InstMap, UseFloatRegs,
        !VarLocs, !TypeVars).

%---------------------------------------------------------------------------%

find_typeinfos_for_tvars(VarTable, RttiVarMaps, TypeVars, VarLocs,
        TypeInfoDataMap) :-
    list.foldl(
        gather_type_info_layout_locns_for_tvar(VarTable, RttiVarMaps, VarLocs),
        TypeVars, map.init, TypeInfoDataMap).

:- pred gather_type_info_layout_locns_for_tvar(var_table::in, rtti_varmaps::in,
    map(prog_var, set(lval))::in, tvar::in,
    map(tvar, set(layout_locn))::in, map(tvar, set(layout_locn))::out) is det.

gather_type_info_layout_locns_for_tvar(VarTable, RttiVarMaps, VarLocs, TypeVar,
        !TypeInfoDataMap) :-
    rtti_lookup_type_info_locn(RttiVarMaps, TypeVar, TypeInfoLocn),
    type_info_locn_var(TypeInfoLocn, TypeInfoVar),
    ( if map.search(VarLocs, TypeInfoVar, TypeInfoLvalSet) then
        set.fold(gather_type_info_layout_locn(TypeInfoLocn), TypeInfoLvalSet,
            set.init, Locns),
        map.det_insert(TypeVar, Locns, !TypeInfoDataMap)
    else
        TypeInfoVarName = var_table_entry_name(VarTable, TypeInfoVar),
        unexpected($pred,
            "can't find rval for type_info var " ++ TypeInfoVarName)
    ).

:- pred gather_type_info_layout_locn(type_info_locn::in, lval::in,
    set(layout_locn)::in, set(layout_locn)::out) is det.

gather_type_info_layout_locn(TypeInfoLocn, Lval, !Locns) :-
    (
        TypeInfoLocn = typeclass_info(_, FieldNum),
        Locn = locn_indirect(Lval, FieldNum)
    ;
        TypeInfoLocn = type_info(_),
        Locn = locn_direct(Lval)
    ),
    set.insert(Locn, !Locns).

%---------------------------------------------------------------------------%

generate_table_arg_type_info(VarTable, RttiVarMaps, NumberedVars,
        TableArgInfos) :-
    set.init(TypeVars0),
    build_table_arg_info(VarTable, NumberedVars, ArgLayouts,
        TypeVars0, TypeVars),
    set.to_sorted_list(TypeVars, TypeVarsList),
    find_typeinfos_for_tvars_table(VarTable, RttiVarMaps, TypeVarsList,
        NumberedVars, TypeInfoDataMap),
    TableArgInfos = table_arg_infos(ArgLayouts, TypeInfoDataMap).

:- pred build_table_arg_info(var_table::in,
    assoc_list(prog_var, int)::in, list(table_arg_info)::out,
    set(tvar)::in, set(tvar)::out) is det.

build_table_arg_info(_, [], [], !TypeVars).
build_table_arg_info(VarTable, [Var - SlotNum | NumberedVars],
        [ArgLayout | ArgLayouts], !TypeVars) :-
    term.var_to_int(Var, VarNum),
    lookup_var_entry(VarTable, Var, Entry),
    VarName = var_entry_name(Var, Entry),
    Type = Entry ^ vte_type,
    ArgLayout = table_arg_info(VarNum, VarName, SlotNum, Type),
    set_of_type_vars_in_type(Type, VarTypeVars),
    set.union(VarTypeVars, !TypeVars),
    build_table_arg_info(VarTable, NumberedVars, ArgLayouts, !TypeVars).

%---------------------------------------------------------------------------%

:- pred find_typeinfos_for_tvars_table(var_table::in, rtti_varmaps::in,
    list(tvar)::in, assoc_list(prog_var, int)::in,
    map(tvar, table_locn)::out) is det.

find_typeinfos_for_tvars_table(VarTable, RttiVarMaps, TypeVars, NumberedVars,
        TypeInfoDataMap) :-
    list.map(rtti_lookup_type_info_locn(RttiVarMaps), TypeVars,
        TypeInfoLocns),
    FindLocn =
        ( pred(TypeInfoLocn::in, Locn::out) is det :-
            ( if
                (
                    TypeInfoLocn = typeclass_info(TypeInfoVar, FieldNum),
                    assoc_list.search(NumberedVars, TypeInfoVar, Slot),
                    LocnPrime = table_locn_indirect(Slot, FieldNum)
                ;
                    TypeInfoLocn = type_info(TypeInfoVar),
                    assoc_list.search(NumberedVars, TypeInfoVar, Slot),
                    LocnPrime = table_locn_direct(Slot)
                )
            then
                Locn = LocnPrime
            else
                type_info_locn_var(TypeInfoLocn, TypeInfoVar),
                VarName = var_table_entry_name(VarTable, TypeInfoVar),
                unexpected($pred,
                    "can't find slot for type_info var " ++ VarName)
            )
        ),
    list.map(FindLocn, TypeInfoLocns, TypeInfoVarLocns),
    map.from_corresponding_lists(TypeVars, TypeInfoVarLocns, TypeInfoDataMap).

%-----------------------------------------------------------------------------%

:- pred live_value_type(slot_contents::in, live_value_type::out) is det.

live_value_type(slot_lval(succip), live_value_succip).
live_value_type(slot_lval(hp), live_value_hp).
live_value_type(slot_lval(maxfr), live_value_maxfr).
live_value_type(slot_lval(curfr), live_value_curfr).
live_value_type(slot_lval(succfr_slot(_)), live_value_unwanted).
live_value_type(slot_lval(prevfr_slot(_)), live_value_unwanted).
live_value_type(slot_lval(redofr_slot(_)), live_value_unwanted).
live_value_type(slot_lval(redoip_slot(_)), live_value_unwanted).
live_value_type(slot_lval(succip_slot(_)), live_value_unwanted).
live_value_type(slot_lval(sp), live_value_unwanted).
live_value_type(slot_lval(parent_sp), live_value_unwanted).
live_value_type(slot_lval(lvar(_)), live_value_unwanted).
live_value_type(slot_lval(field(_, _, _)), live_value_unwanted).
live_value_type(slot_lval(temp(_, _)), live_value_unwanted).
live_value_type(slot_lval(reg(_, _)), live_value_unwanted).
live_value_type(slot_lval(stackvar(_)), live_value_unwanted).
live_value_type(slot_lval(parent_stackvar(_)), live_value_unwanted).
live_value_type(slot_lval(framevar(_)), live_value_unwanted).
live_value_type(slot_lval(double_stackvar(_, _)), live_value_unwanted).
live_value_type(slot_lval(mem_ref(_)), live_value_unwanted). % XXX
live_value_type(slot_lval(global_var_ref(_)), live_value_unwanted).
live_value_type(slot_success_record, live_value_unwanted).
live_value_type(slot_ticket, live_value_unwanted).
    % XXX we may need to modify this, if the GC is going to garbage-collect
    % the trail.
live_value_type(slot_ticket_counter, live_value_unwanted).
live_value_type(slot_lookup_disj_cur, live_value_unwanted).
live_value_type(slot_lookup_switch_cur, live_value_unwanted).
live_value_type(slot_lookup_switch_max, live_value_unwanted).
live_value_type(slot_sync_term, live_value_unwanted).
live_value_type(slot_trace_data, live_value_unwanted).
live_value_type(slot_region_ite, live_value_region_ite).
live_value_type(slot_region_disj, live_value_region_disj).
live_value_type(slot_region_commit, live_value_region_commit).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.continuation_info.
%-----------------------------------------------------------------------------%
