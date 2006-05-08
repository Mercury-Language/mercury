%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: code_info.m.
% Main authors: conway, zs.

% This file defines the code_info type and various operations on it.
% The code_info structure is the 'state' of the code generator.
%
% This file is organized into nine submodules:
%
%   - the code_info structure and its access predicates
%   - simple wrappers around access predicates
%   - handling branched control structures
%   - handling failure continuations
%   - handling liveness issues
%   - saving and restoring heap pointers, trail tickets etc
%   - interfacing to var_locn
%   - managing the info required by garbage collection and value numbering
%   - managing stack slots

%---------------------------------------------------------------------------%

:- module ll_backend.code_info.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.globals.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.global_data.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module ll_backend.trace.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.
:- import_module term.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.proc_label.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.arg_info.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_rtti.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module libs.tree.
:- import_module ll_backend.code_util.
:- import_module ll_backend.exprn_aux.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.var_locn.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module char.
:- import_module int.
:- import_module pair.
:- import_module set.
:- import_module stack.
:- import_module string.
:- import_module varset.

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

    % Create a new code_info structure. Also return the
    % outermost resumption point, and info about the non-fixed
    % stack slots used for tracing purposes.
    %
:- pred code_info_init(bool::in, globals::in, pred_id::in, proc_id::in,
    pred_info::in, proc_info::in, abs_follow_vars::in, module_info::in,
    static_cell_info::in, resume_point_info::out, trace_slot_info::out,
    code_info::out) is det.

    % Get the globals table.
    %
:- pred get_globals(code_info::in, globals::out) is det.

    % Get the HLDS of the entire module.
    %
:- pred get_module_info(code_info::in, module_info::out) is det.

    % Get the id of the predicate we are generating code for.
    %
:- pred get_pred_id(code_info::in, pred_id::out) is det.

    % Get the id of the procedure we are generating code for.
    %
:- pred get_proc_id(code_info::in, proc_id::out) is det.

    % Get the HLDS of the procedure we are generating code for.
    %
:- pred get_proc_info(code_info::in, proc_info::out) is det.

    % Get the HLDS of the predicate containing the procedure
    % we are generating code for.
    %
:- pred get_pred_info(code_info::in, pred_info::out) is det.

    % Get the variables for the current procedure.
    %
:- pred get_varset(code_info::in, prog_varset::out) is det.

:- func get_var_types(code_info) = vartypes.

:- pred get_maybe_trace_info(code_info::in, maybe(trace_info)::out) is det.

:- pred get_emit_trail_ops(code_info::in, add_trail_ops::out) is det.

    % Get the set of currently forward-live variables.
    %
:- pred get_forward_live_vars(code_info::in, set(prog_var)::out) is det.

    % Set the set of currently forward-live variables.
    %
:- pred set_forward_live_vars(set(prog_var)::in,
    code_info::in, code_info::out) is det.

    % Get the table mapping variables to the current
    % instantiation states.
    %
:- pred get_instmap(code_info::in, instmap::out) is det.

    % Set the table mapping variables to the current
    % instantiation states.
    %
:- pred set_instmap(instmap::in, code_info::in, code_info::out) is det.

    % The number of the last local label allocated.
    %
:- pred get_label_counter(code_info::in, counter::out) is det.

    % Get the flag that indicates whether succip is used or not.
    %
:- pred get_succip_used(code_info::in, bool::out) is det.

    % Get the label layout information created by tracing
    % during code generation.
    %
:- pred get_layout_info(code_info::in, proc_label_layout_info::out) is det.

    % Get the global static data structures that have
    % been created during code generation for closure layouts.
    %
:- pred get_closure_layouts(code_info::in, list(layout_data)::out) is det.

:- pred get_max_reg_in_use_at_trace(code_info::in, int::out) is det.

:- pred set_max_reg_in_use_at_trace(int::in,
    code_info::in, code_info::out) is det.

    % Get the flag which is true iff the procedure has so far
    % emitted code that creates a temporary nondet stack frame.
    %
:- pred get_created_temp_frame(code_info::in, bool::out) is det.

:- pred get_static_cell_info(code_info::in, static_cell_info::out) is det.

:- pred set_static_cell_info(static_cell_info::in,
    code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred get_var_slot_count(code_info::in, int::out) is det.

:- pred set_maybe_trace_info(maybe(trace_info)::in,
    code_info::in, code_info::out) is det.

:- pred get_opt_no_return_calls(code_info::in, bool::out) is det.

:- pred get_opt_trail_ops(code_info::in, bool::out) is det.

:- pred get_zombies(code_info::in, set(prog_var)::out) is det.

:- pred set_zombies(set(prog_var)::in, code_info::in, code_info::out) is det.

:- pred get_var_locn_info(code_info::in, var_locn_info::out) is det.

:- pred set_var_locn_info(var_locn_info::in,
    code_info::in, code_info::out) is det.

:- pred get_temps_in_use(code_info::in, set(lval)::out) is det.

:- pred set_temps_in_use(set(lval)::in, code_info::in, code_info::out) is det.

:- pred get_fail_info(code_info::in, fail_info::out) is det.

:- pred set_fail_info(fail_info::in, code_info::in, code_info::out) is det.

:- pred set_label_counter(counter::in, code_info::in, code_info::out) is det.

:- pred set_succip_used(bool::in, code_info::in, code_info::out) is det.

:- pred set_layout_info(proc_label_layout_info::in,
    code_info::in, code_info::out) is det.

:- pred get_max_temp_slot_count(code_info::in, int::out) is det.

:- pred set_max_temp_slot_count(int::in, code_info::in, code_info::out) is det.

:- pred get_temp_content_map(code_info::in,
    map(lval, slot_contents)::out) is det.

:- pred set_temp_content_map(map(lval, slot_contents)::in,
    code_info::in, code_info::out) is det.

:- pred set_closure_layouts(list(layout_data)::in,
    code_info::in, code_info::out) is det.

:- pred get_closure_seq_counter(code_info::in, counter::out) is det.

:- pred set_closure_seq_counter(counter::in,
    code_info::in, code_info::out) is det.

:- pred set_created_temp_frame(bool::in, code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

    % The code_info structure has three groups of fields.
    %
    % Some fields are static; they are set when the code_info structure
    % is initialized, and never changed afterwards.
    %
    % Some fields record information about the state of the code generator
    % at a particular location in the HLDS code of the current procedure.
    % At the start of the branched control structure, the code generator
    % remembers the values of these fields, and starts generating code
    % for each branch from the same location-dependent state.
    %
    % Some fields record persistent information that does not depend
    % on a code location. Updates to these fields must remain effective
    % even when the code generator resets its location-dependent state.

:- type code_info
    --->    code_info(
                code_info_static        :: code_info_static,
                code_info_loc_dep       :: code_info_loc_dep,
                code_info_persistent    :: code_info_persistent
            ).

:- type code_info_static
    --->    code_info_static(
                globals             :: globals,
                                    % For the code generation options.

                module_info         :: module_info,
                                    % The module_info structure - you just
                                    % never know when you might need it.

                pred_id             :: pred_id,
                                    % The id of the current predicate.

                proc_id             :: proc_id,
                                    % The id of the current procedure.

                proc_info           :: proc_info,
                                    % The proc_info for this procedure.

                pred_info           :: pred_info,
                                    % The pred_info for the predicate containing
                                    % this procedure.

                varset              :: prog_varset,
                                    % The variables in this procedure.

                var_slot_count      :: int,
                                    % The number of stack slots allocated.
                                    % for storing variables.
                                    % (Some extra stack slots are used
                                    % for saving and restoring registers.)

                maybe_trace_info    :: maybe(trace_info),
                                    % Information about which stack slots
                                    % the call sequence number and depth
                                    % are stored in, provided tracing is
                                    % switched on.
                
                opt_no_resume_calls :: bool,
                                    % Should we optimize calls that cannot
                                    % return?

                emit_trail_ops      :: bool,
                                    % Should we emit trail operations?

                opt_trail_ops       :: bool
                                    % Should we try to avoid emiting trail
                                    % operations?
            ).

:- type code_info_loc_dep
    --->    code_info_loc_dep(
                forward_live_vars   :: set(prog_var),
                                    % Variables that are forward live
                                    % after this goal.

                instmap             :: instmap,
                                    % Current insts of the live variables.

                zombies             :: set(prog_var),
                                    % Zombie variables; variables that are not
                                    % forward live but which are protected by
                                    % an enclosing resume point.

                var_locn_info       :: var_locn_info,
                                    % A map storing the information about
                                    % the status of each known variable.
                                    % (Known vars = forward live vars
                                    % + zombies)

                temps_in_use        :: set(lval),
                                    % The set of temporary locations currently
                                    % in use. These lvals must be all be keys
                                    % in the map of temporary locations ever
                                    % used, which is one of the persistent
                                    % fields below. Any keys in that map which
                                    % are not in this set are free for reuse.

                fail_info           :: fail_info
                                    % Information about how to manage failures.
            ).

:- type code_info_persistent
    --->    code_info_persistent(
                label_num_src       :: counter,
                                    % Counter for the local labels used
                                    % by this procedure.

                store_succip        :: bool,
                                    % do we need to store succip?

                label_info          :: proc_label_layout_info,
                                    % Information on which values
                                    % are live and where at which labels,
                                    % for tracing and/or accurate gc.

                stackslot_max       :: int,
                                    % The maximum number of extra
                                    % temporary stackslots that have been
                                    % used during the procedure.

                temp_contents       :: map(lval, slot_contents),
                                    % The temporary locations that have ever
                                    % been used on the stack, and what they
                                    % contain. Once we have used a stack slot
                                    % to store e.g. a ticket, we never reuse
                                    % that slot to hold something else, e.g.
                                    % a saved hp. This policy prevents us
                                    % from making such conflicting choices
                                    % in parallel branches, which would make it
                                    % impossible to describe to gc what the
                                    % slot contains after the end of the
                                    % branched control structure.

                closure_layout_seq :: counter,

                closure_layouts     :: list(layout_data),
                                    % Closure layout structures generated
                                    % by this procedure.

                max_reg_used        :: int,
                                    % At each call to MR_trace, we compute the
                                    % highest rN register number that contains
                                    % a useful value. This slot contains the
                                    % maximum of these highest values.
                                    % Therefore at all calls to MR_trace in the
                                    % procedure, we need only save the
                                    % registers whose numbers are equal to or
                                    % smaller than this field. This slot
                                    % contains -1 if tracing is not enabled.

                created_temp_frame  :: bool,
                                    % True iff the procedure has created one or
                                    % more temporary nondet frames.

                static_cell_info    :: static_cell_info
            ).

%---------------------------------------------------------------------------%

code_info_init(SaveSuccip, Globals, PredId, ProcId, PredInfo, ProcInfo,
        FollowVars, ModuleInfo, StaticCellInfo, ResumePoint, TraceSlotInfo,
        CodeInfo) :-
    proc_info_get_initial_instmap(ProcInfo, ModuleInfo, InstMap),
    proc_info_get_liveness_info(ProcInfo, Liveness),
    proc_info_interface_code_model(ProcInfo, CodeModel),
    build_input_arg_list(ProcInfo, ArgList),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_stack_slots(ProcInfo, StackSlots),
    globals.get_options(Globals, Options),
    globals.get_trace_level(Globals, TraceLevel),
    ( eff_trace_level_is_none(PredInfo, ProcInfo, TraceLevel) = no ->
        trace.fail_vars(ModuleInfo, ProcInfo, FailVars),
        MaybeFailVars = yes(FailVars),
        set.union(Liveness, FailVars, EffLiveness)
    ;
        MaybeFailVars = no,
        EffLiveness = Liveness
    ),
    var_locn.init_state(ArgList, EffLiveness, VarSet, VarTypes, StackSlots,
        FollowVars, Options, VarLocnInfo),
    stack.init(ResumePoints),
    globals.lookup_bool_option(Globals, allow_hijacks, AllowHijack),
    (
        AllowHijack = yes,
        Hijack = allowed
    ;
        AllowHijack = no,
        Hijack = not_allowed
    ),
    DummyFailInfo = fail_info(ResumePoints, resume_point_unknown,
        may_be_different, not_inside_non_condition, Hijack),
    map.init(TempContentMap),
    set.init(TempsInUse),
    set.init(Zombies),
    map.init(LayoutMap),
    max_var_slot(StackSlots, VarSlotMax),
    trace.reserved_slots(ModuleInfo, PredInfo, ProcInfo, Globals,
        FixedSlots, _),
    int.max(VarSlotMax, FixedSlots, SlotMax),
    globals.lookup_bool_option(Globals, opt_no_return_calls,
        OptNoReturnCalls),
    globals.lookup_bool_option(Globals, use_trail, UseTrail),
    globals.lookup_bool_option(Globals, disable_trail_ops, DisableTrailOps),
    (
        UseTrail = yes,
        DisableTrailOps = no
    ->
        EmitTrailOps = yes
    ;
        EmitTrailOps = no
    ),
    globals.lookup_bool_option(Globals, optimize_trail_usage, OptTrailOps),
    CodeInfo0 = code_info(
        code_info_static(
            Globals,
            ModuleInfo,
            PredId,
            ProcId,
            ProcInfo,
            PredInfo,
            VarSet,
            SlotMax,
            no,
            OptNoReturnCalls,
            EmitTrailOps,
            OptTrailOps
        ),
        code_info_loc_dep(
            Liveness,
            InstMap,
            Zombies,
            VarLocnInfo,
            TempsInUse,
            DummyFailInfo   % init_fail_info will override this dummy value
        ),
        code_info_persistent(
            counter.init(1),
            SaveSuccip,
            LayoutMap,
            0,
            TempContentMap,
            counter.init(1),
            [],
            -1,
            no,
            StaticCellInfo
        )
    ),
    init_maybe_trace_info(TraceLevel, Globals, ModuleInfo,
        PredInfo, ProcInfo, TraceSlotInfo, CodeInfo0, CodeInfo1),
    init_fail_info(CodeModel, MaybeFailVars, ResumePoint,
        CodeInfo1, CodeInfo).

:- pred init_maybe_trace_info(trace_level::in, globals::in,
    module_info::in, pred_info::in, proc_info::in, trace_slot_info::out,
    code_info::in, code_info::out) is det.

init_maybe_trace_info(TraceLevel, Globals, ModuleInfo, PredInfo,
        ProcInfo, TraceSlotInfo, !CI) :-
    ( eff_trace_level_is_none(PredInfo, ProcInfo, TraceLevel) = no ->
        trace.setup(ModuleInfo, PredInfo, ProcInfo, Globals, TraceSlotInfo,
            TraceInfo, !CI),
        set_maybe_trace_info(yes(TraceInfo), !CI)
    ;
        TraceSlotInfo = trace_slot_info(no, no, no, no, no)
    ).

%---------------------------------------------------------------------------%

get_globals(CI, CI ^ code_info_static ^ globals).
get_module_info(CI, CI ^ code_info_static ^ module_info).
get_pred_id(CI, CI ^ code_info_static ^ pred_id).
get_proc_id(CI, CI ^ code_info_static ^ proc_id).
get_proc_info(CI, CI ^ code_info_static ^ proc_info).
get_pred_info(CI, CI ^ code_info_static ^ pred_info).
get_varset(CI, CI ^ code_info_static ^ varset).
get_var_slot_count(CI, CI ^ code_info_static ^ var_slot_count).
get_maybe_trace_info(CI, CI ^ code_info_static ^ maybe_trace_info).
get_opt_no_return_calls(CI, CI ^ code_info_static ^ opt_no_resume_calls).
get_emit_trail_ops(CI, CI ^ code_info_static ^ emit_trail_ops).
get_opt_trail_ops(CI, CI ^ code_info_static ^ opt_trail_ops).
get_forward_live_vars(CI, CI ^ code_info_loc_dep ^ forward_live_vars).
get_instmap(CI, CI ^ code_info_loc_dep ^ instmap).
get_zombies(CI, CI ^ code_info_loc_dep ^ zombies).
get_var_locn_info(CI, CI ^ code_info_loc_dep ^ var_locn_info).
get_temps_in_use(CI, CI ^ code_info_loc_dep ^ temps_in_use).
get_fail_info(CI, CI ^ code_info_loc_dep ^ fail_info).
get_label_counter(CI, CI ^ code_info_persistent ^ label_num_src).
get_succip_used(CI, CI ^ code_info_persistent ^ store_succip).
get_layout_info(CI, CI ^ code_info_persistent ^ label_info).
get_max_temp_slot_count(CI, CI ^ code_info_persistent ^ stackslot_max).
get_temp_content_map(CI, CI ^ code_info_persistent ^ temp_contents).
get_closure_seq_counter(CI, CI ^ code_info_persistent ^ closure_layout_seq).
get_closure_layouts(CI, CI ^ code_info_persistent ^ closure_layouts).
get_max_reg_in_use_at_trace(CI, CI ^ code_info_persistent ^ max_reg_used).
get_created_temp_frame(CI, CI ^ code_info_persistent ^ created_temp_frame).
get_static_cell_info(CI, CI ^ code_info_persistent ^ static_cell_info).

%---------------------------------------------------------------------------%

set_maybe_trace_info(TI, CI, CI ^ code_info_static ^ maybe_trace_info := TI).
set_forward_live_vars(LV, CI,
    CI ^ code_info_loc_dep ^ forward_live_vars := LV).
set_instmap(IM, CI, CI ^ code_info_loc_dep ^ instmap := IM).
set_zombies(Zs, CI, CI ^ code_info_loc_dep ^ zombies := Zs).
set_var_locn_info(EI, CI, CI ^ code_info_loc_dep ^ var_locn_info := EI).
set_temps_in_use(TI, CI, CI ^ code_info_loc_dep ^ temps_in_use := TI).
set_fail_info(FI, CI, CI ^ code_info_loc_dep ^ fail_info := FI).
set_label_counter(LC, CI, CI ^ code_info_persistent ^ label_num_src := LC).
set_succip_used(SU, CI, CI ^ code_info_persistent ^ store_succip := SU).
set_layout_info(LI, CI, CI ^ code_info_persistent ^ label_info := LI).
set_max_temp_slot_count(TM, CI,
    CI ^ code_info_persistent ^ stackslot_max := TM).
set_temp_content_map(CM, CI, CI ^ code_info_persistent ^ temp_contents := CM).
set_closure_seq_counter(CLS, CI,
    CI ^ code_info_persistent ^ closure_layout_seq := CLS).
set_closure_layouts(CG, CI, CI ^ code_info_persistent ^ closure_layouts := CG).
set_max_reg_in_use_at_trace(MR, CI,
    CI ^ code_info_persistent ^ max_reg_used := MR).
set_created_temp_frame(MR, CI,
    CI ^ code_info_persistent ^ created_temp_frame := MR).
set_static_cell_info(SCI, CI,
    CI ^ code_info_persistent ^ static_cell_info := SCI).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for simple wrappers around access predicates.

:- interface.

    % Get the hlds mapping from variables to stack slots
    %
:- pred get_stack_slots(code_info::in, stack_slots::out) is det.

    % Get the table that contains advice about where
    % variables should be put.
    %
:- pred get_follow_var_map(code_info::in, abs_follow_vars_map::out) is det.

    % Get the integer that gives the number of the next
    % non-reserved register.
    %
:- pred get_next_non_reserved(code_info::in, int::out) is det.

    % Set the table that contains advice about where
    % variables should be put.
:- pred set_follow_vars(abs_follow_vars::in,
    code_info::in, code_info::out) is det.

    % pre_goal_update(GoalInfo, Atomic, OldCodeInfo, NewCodeInfo)
    % updates OldCodeInfo to produce NewCodeInfo with the changes
    % specified by GoalInfo.
    %
:- pred pre_goal_update(hlds_goal_info::in, bool::in,
    code_info::in, code_info::out) is det.

    % post_goal_update(GoalInfo, OldCodeInfo, NewCodeInfo)
    % updates OldCodeInfo to produce NewCodeInfo with the changes described
    % by GoalInfo.
    %
:- pred post_goal_update(hlds_goal_info::in,
    code_info::in, code_info::out) is det.

    % Find out whether the body of the current procedure should use
    % typeinfo liveness.
    %
:- func body_typeinfo_liveness(code_info) = bool.

    % Find out the type of the given variable.
    %
:- func variable_type(code_info, prog_var) = mer_type.

:- func lookup_type_defn(code_info, mer_type) = hlds_type_defn.

    % Given a constructor id, and a variable (so that we can work out the
    % type of the constructor), determine correct tag (representation)
    % of that constructor.
    %
:- func cons_id_to_tag(code_info, prog_var, cons_id) = cons_tag.

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

    % Get the set of variables currently needed by the resume
    % points of enclosing goals.
    %
:- func current_resume_point_vars(code_info) = set(prog_var).

:- func variable_to_string(code_info, prog_var) = string.

    % Create a code address which holds the address of the specified
    % procedure.
    % The fourth argument should be `no' if the the caller wants the
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
:- func make_entry_label(code_info, module_info, pred_id, proc_id, bool)
    = code_addr.

    % Generate the next local label in sequence.
    %
:- pred get_next_label(label::out, code_info::in, code_info::out)
    is det.

    % Note that the succip slot is used, and thus cannot be optimized away.
    %
:- pred succip_is_used(code_info::in, code_info::out) is det.

:- pred add_trace_layout_for_label(label::in, term.context::in,
    trace_port::in, bool::in, goal_path::in, layout_label_info::in,
    code_info::in, code_info::out) is det.

:- pred get_cur_proc_label(code_info::in, proc_label::out) is det.

:- pred get_next_closure_seq_no(int::out,
    code_info::in, code_info::out) is det.

:- pred add_closure_layout(layout_data::in,
    code_info::in, code_info::out) is det.

:- pred add_scalar_static_cell(assoc_list(rval, llds_type)::in,
    data_addr::out, code_info::in, code_info::out) is det.

:- pred add_scalar_static_cell_natural_types(list(rval)::in,
    data_addr::out, code_info::in, code_info::out) is det.

:- pred add_vector_static_cell(list(llds_type)::in, list(list(rval))::in,
    data_addr::out, code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred add_resume_layout_for_label(label::in,
    layout_label_info::in, code_info::in, code_info::out) is det.

get_stack_slots(CI, StackSlots) :-
    get_var_locn_info(CI, VarLocnInfo),
    var_locn.get_stack_slots(VarLocnInfo, StackSlots).

get_follow_var_map(CI, FollowVarMap) :-
    get_var_locn_info(CI, VarLocnInfo),
    var_locn.get_follow_var_map(VarLocnInfo, FollowVarMap).

get_next_non_reserved(CI, NextNonReserved) :-
    get_var_locn_info(CI, VarLocnInfo),
    var_locn.get_next_non_reserved(VarLocnInfo, NextNonReserved).

set_follow_vars(FollowVars, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.set_follow_vars(FollowVars, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

%-----------------------------------------------------------------------------%

pre_goal_update(GoalInfo, Atomic, !CI) :-
    % The liveness pass puts resume_point annotations on some kinds
    % of goals. The parts of the code generator that handle those kinds
    % of goals should handle the resume point annotation as well;
    % when they do, they remove the annotation. The following code
    % is a sanity check to make sure that this has in fact been done.
    goal_info_get_resume_point(GoalInfo, ResumePoint),
    (
        ResumePoint = no_resume_point
    ;
        ResumePoint = resume_point(_, _),
        unexpected(this_file, "pre_goal_update with resume point")
    ),
    goal_info_get_follow_vars(GoalInfo, MaybeFollowVars),
    (
        MaybeFollowVars = yes(FollowVars),
        set_follow_vars(FollowVars, !CI)
    ;
        MaybeFollowVars = no
    ),
    % NOTE: we must be careful to apply deaths before births
    goal_info_get_pre_deaths(GoalInfo, PreDeaths),
    rem_forward_live_vars(PreDeaths, !CI),
    maybe_make_vars_forward_dead(PreDeaths, no, !CI),
    goal_info_get_pre_births(GoalInfo, PreBirths),
    add_forward_live_vars(PreBirths, !CI),
    (
        Atomic = yes,
        goal_info_get_post_deaths(GoalInfo, PostDeaths),
        rem_forward_live_vars(PostDeaths, !CI)
    ;
        Atomic = no
    ).

post_goal_update(GoalInfo, !CI) :-
    % note: we must be careful to apply deaths before births
    goal_info_get_post_deaths(GoalInfo, PostDeaths),
    rem_forward_live_vars(PostDeaths, !CI),
    maybe_make_vars_forward_dead(PostDeaths, no, !CI),
    goal_info_get_post_births(GoalInfo, PostBirths),
    add_forward_live_vars(PostBirths, !CI),
    make_vars_forward_live(PostBirths, !CI),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
    get_instmap(!.CI, InstMap0),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
    set_instmap(InstMap, !CI).

%---------------------------------------------------------------------------%

body_typeinfo_liveness(CI) = TypeInfoLiveness :-
    get_module_info(CI, ModuleInfo),
    get_pred_id(CI, PredId),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    get_globals(CI, Globals),
    body_should_use_typeinfo_liveness(PredInfo, Globals, TypeInfoLiveness).

get_var_types(CI) = VarTypes :-
    get_proc_info(CI, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes).

variable_type(CI, Var) = Type :-
    map.lookup(get_var_types(CI), Var, Type).

lookup_type_defn(CI, Type) = TypeDefn :-
    get_module_info(CI, ModuleInfo),
    ( type_to_ctor_and_args(Type, TypeCtorPrime, _) ->
        TypeCtor = TypeCtorPrime
    ;
        unexpected(this_file, "unknown type in lookup_type_defn")
    ),
    module_info_get_type_table(ModuleInfo, TypeTable),
    map.lookup(TypeTable, TypeCtor, TypeDefn).

cons_id_to_tag(CI, Var, ConsId) = ConsTag :-
    get_module_info(CI, ModuleInfo),
    ConsTag = cons_id_to_tag(ConsId, variable_type(CI, Var), ModuleInfo).

%---------------------------------------------------------------------------%

get_proc_model(CI) = CodeModel :-
    get_proc_info(CI, ProcInfo),
    proc_info_interface_code_model(ProcInfo, CodeModel).

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

current_resume_point_vars(CI) = ResumeVars :-
    get_fail_info(CI, FailInfo),
    FailInfo = fail_info(ResumePointStack, _, _, _, _),
    stack.top_det(ResumePointStack, ResumePointInfo),
    pick_first_resume_point(ResumePointInfo, ResumeMap, _),
    map.keys(ResumeMap, ResumeMapVarList),
    set.list_to_set(ResumeMapVarList, ResumeVars).

variable_to_string(CI, Var) = Name :-
    get_varset(CI, Varset),
    varset.lookup_name(Varset, Var, Name).

%---------------------------------------------------------------------------%

make_entry_label(CI, ModuleInfo, PredId, ProcId, Immed0) = PredAddress :-
    (
        Immed0 = no,
        Immed = no
    ;
        Immed0 = yes,
        get_globals(CI, Globals),
        globals.lookup_int_option(Globals, procs_per_c_function,
            ProcsPerFunc),
        get_pred_id(CI, CurPredId),
        get_proc_id(CI, CurProcId),
        Immed = yes(ProcsPerFunc - proc(CurPredId, CurProcId))
    ),
    make_entry_label(ModuleInfo, PredId, ProcId, Immed, PredAddress).

get_next_label(Label, !CI) :-
    get_module_info(!.CI, ModuleInfo),
    get_pred_id(!.CI, PredId),
    get_proc_id(!.CI, ProcId),
    get_label_counter(!.CI, C0),
    counter.allocate(N, C0, C),
    set_label_counter(C, !CI),
    make_internal_label(ModuleInfo, PredId, ProcId, N, Label).

succip_is_used(!CI) :-
    set_succip_used(yes, !CI).

add_trace_layout_for_label(Label, Context, Port, IsHidden, Path, Layout,
        !CI) :-
    get_layout_info(!.CI, Internals0),
    Exec = yes(trace_port_layout_info(Context, Port, IsHidden, Path, Layout)),
    (
        Label = internal(LabelNum, _)
    ;
        Label = entry(_, _),
        unexpected(this_file, "add_trace_layout_for_label: entry")
    ),
    ( map.search(Internals0, LabelNum, Internal0) ->
        Internal0 = internal_layout_info(Exec0, Resume, Return),
        (
            Exec0 = no
        ;
            Exec0 = yes(_),
            unexpected(this_file,
                "adding trace layout for already known label")
        ),
        Internal = internal_layout_info(Exec, Resume, Return),
        map.det_update(Internals0, LabelNum, Internal, Internals)
    ;
        Internal = internal_layout_info(Exec, no, no),
        map.det_insert(Internals0, LabelNum, Internal, Internals)
    ),
    set_layout_info(Internals, !CI).

add_resume_layout_for_label(Label, LayoutInfo, !CI) :-
    get_layout_info(!.CI, Internals0),
    Resume = yes(LayoutInfo),
    (
        Label = internal(LabelNum, _)
    ;
        Label = entry(_, _),
        unexpected(this_file, "add_trace_layout_for_label: entry")
    ),
    ( map.search(Internals0, LabelNum, Internal0) ->
        Internal0 = internal_layout_info(Exec, Resume0, Return),
        (
            Resume0 = no
        ;
            Resume0 = yes(_),
            unexpected(this_file, "adding gc layout for already known label")
        ),
        Internal = internal_layout_info(Exec, Resume, Return),
        map.det_update(Internals0, LabelNum, Internal, Internals)
    ;
        Internal = internal_layout_info(no, Resume, no),
        map.det_insert(Internals0, LabelNum, Internal, Internals)
    ),
    set_layout_info(Internals, !CI).

:- pred get_active_temps_data(code_info::in,
    assoc_list(lval, slot_contents)::out) is det.

get_active_temps_data(CI, Temps) :-
    get_temps_in_use(CI, TempsInUse),
    get_temp_content_map(CI, TempContentMap),
    map.select(TempContentMap, TempsInUse, TempsInUseContentMap),
    map.to_assoc_list(TempsInUseContentMap, Temps).

get_cur_proc_label(CI, ProcLabel) :-
    get_module_info(CI, ModuleInfo),
    get_pred_id(CI, PredId),
    get_proc_id(CI, ProcId),
    ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId).

get_next_closure_seq_no(SeqNo, !CI) :-
    get_closure_seq_counter(!.CI, C0),
    counter.allocate(SeqNo, C0, C),
    set_closure_seq_counter(C, !CI).

add_closure_layout(ClosureLayout, !CI) :-
    get_closure_layouts(!.CI, ClosureLayouts),
    set_closure_layouts([ClosureLayout | ClosureLayouts], !CI).

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for handling branched control structures.

:- interface.

:- type position_info.
:- type branch_end_info.

:- type branch_end  ==  maybe(branch_end_info).

:- pred remember_position(code_info::in, position_info::out) is det.

:- pred reset_to_position(position_info::in,
    code_info::in, code_info::out) is det.

:- pred reset_resume_known(position_info::in,
    code_info::in, code_info::out) is det.

:- pred generate_branch_end(abs_store_map::in, branch_end::in,
    branch_end::out, code_tree::out, code_info::in, code_info::out) is det.

:- pred after_all_branches(abs_store_map::in, branch_end::in,
    code_info::in, code_info::out) is det.

:- pred save_hp_in_branch(code_tree::out, lval::out,
    position_info::in, position_info::out) is det.

:- implementation.

:- type position_info
    --->    position_info(
                code_info   % The code_info at a given position
                            % in the code of the procedure.
            ).

:- type branch_end_info
    --->    branch_end_info(
                code_info   % The code_info at the end of a branch.
            ).

remember_position(CI, position_info(CI)).

reset_to_position(position_info(PosCI), CurCI, NextCI) :-
    PosCI  = code_info(_, LocDep, _),
    CurCI  = code_info(Static, _, Persistent),
    NextCI = code_info(Static, LocDep, Persistent).

reset_resume_known(BranchStart, !CI) :-
    BranchStart = position_info(BranchStartCI),
    get_fail_info(BranchStartCI, BranchStartFailInfo),
    BranchStartFailInfo = fail_info(_, BSResumeKnown, _, _, _),
    get_fail_info(!.CI, CurFailInfo),
    CurFailInfo = fail_info(CurFailStack, _, CurCurfMaxfr, CurCondEnv,
        CurHijack),
    NewFailInfo = fail_info(CurFailStack, BSResumeKnown, CurCurfMaxfr,
        CurCondEnv, CurHijack),
    set_fail_info(NewFailInfo, !CI).

generate_branch_end(StoreMap, MaybeEnd0, MaybeEnd, Code, !CI) :-
    % The code generator generates better code if it knows in advance where
    % each variable should go. We don't need to reset the follow_vars
    % afterwards, since every goal following a branched control structure
    % must in any case be annotated with its own follow_var set.
    map.to_assoc_list(StoreMap, AbsVarLocs),
    map.from_assoc_list(AbsVarLocs, FollowVarsMap),
    assoc_list.values(AbsVarLocs, AbsLocs),
    code_util.max_mentioned_abs_reg(AbsLocs, MaxMentionedReg),
    set_follow_vars(abs_follow_vars(FollowVarsMap, MaxMentionedReg + 1), !CI),
    get_instmap(!.CI, InstMap),
    ( instmap.is_reachable(InstMap) ->
        VarLocs = assoc_list.map_values(key_abs_locn_to_lval, AbsVarLocs),
        place_vars(VarLocs, Code, !CI)
    ;
        % With --opt-no-return-call, the variables that we would have
        % saved across a call that cannot return have had the last
        % of their code generation state destroyed, so calling
        % place_vars would cause a code generator abort. However,
        % pretending that all the variables are where the store map
        % says they should be is perfectly fine, since we can never
        % reach the end of *this* branch anyway.
        remake_with_store_map(StoreMap, !CI),
        Code = empty
    ),
    EndCodeInfo1 = !.CI,
    (
        MaybeEnd0 = no,
        EndCodeInfo = EndCodeInfo1
    ;
        MaybeEnd0 = yes(branch_end_info(EndCodeInfo0)),

        % Make sure the left context we leave the branched structure
        % with is valid for all branches.
        get_fail_info(EndCodeInfo0, FailInfo0),
        get_fail_info(EndCodeInfo1, FailInfo1),
        FailInfo0 = fail_info(_, ResumeKnown0, CurfrMaxfr0, CondEnv0, Hijack0),
        FailInfo1 = fail_info(R, ResumeKnown1, CurfrMaxfr1, CondEnv1, Hijack1),
        (
            ResumeKnown0 = resume_point_known(Redoip0),
            ResumeKnown1 = resume_point_known(Redoip1)
        ->
            ResumeKnown = resume_point_known(Redoip0),
            expect(unify(Redoip0, Redoip1), this_file,
                "redoip mismatch in generate_branch_end")
        ;
            ResumeKnown = resume_point_unknown
        ),
        (
            CurfrMaxfr0 = must_be_equal,
            CurfrMaxfr1 = must_be_equal
        ->
            CurfrMaxfr = must_be_equal
        ;
            CurfrMaxfr = may_be_different
        ),
        (
            Hijack0 = allowed,
            Hijack1 = allowed
        ->
            Hijack = allowed
        ;
            Hijack = not_allowed
        ),
        expect(unify(CondEnv0, CondEnv1), this_file,
            "some but not all branches inside a non condition"),
        FailInfo = fail_info(R, ResumeKnown, CurfrMaxfr, CondEnv0, Hijack),
        set_fail_info(FailInfo, EndCodeInfo1, EndCodeInfoA),

        % Make sure the "temps in use" set at the end of the branched control
        % structure includes every slot in use at the end of any branch.
        get_temps_in_use(EndCodeInfo0, TempsInUse0),
        get_temps_in_use(EndCodeInfo1, TempsInUse1),
        set.union(TempsInUse0, TempsInUse1, TempsInUse),
        set_temps_in_use(TempsInUse, EndCodeInfoA, EndCodeInfo)
    ),
    MaybeEnd = yes(branch_end_info(EndCodeInfo)).

after_all_branches(StoreMap, MaybeEnd, !CI) :-
    (
        MaybeEnd = yes(BranchEnd),
        BranchEnd = branch_end_info(BranchEndCodeInfo),
        reset_to_position(position_info(BranchEndCodeInfo), !CI),
        remake_with_store_map(StoreMap, !CI)
    ;
        MaybeEnd = no,
        unexpected(this_file, "no branches in branched control structure")
    ).

    % remake_with_store_map throws away the var_info data structure, forgetting
    % the current locations of all variables, and rebuilds it from scratch
    % based on the given store map. The new var_info will know about only
    % the variables present in the store map, and will believe they are
    % where the store map says they are.
    %
:- pred remake_with_store_map(abs_store_map::in,
    code_info::in, code_info::out) is det.

remake_with_store_map(StoreMap, !CI) :-
    map.to_assoc_list(StoreMap, VarLocns),
    VarLvals = assoc_list.map_values(key_abs_locn_to_lval, VarLocns),
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.reinit_state(VarLvals, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

save_hp_in_branch(Code, Slot, Pos0, Pos) :-
    Pos0 = position_info(CodeInfo0),
    save_hp(Code, Slot, CodeInfo0, CodeInfo),
    Pos  = position_info(CodeInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for the handling of failure continuations.

    % The principles underlying this submodule of code_info.m are
    % documented in the file compiler/notes/failure.html, which also
    % defines terms such as "quarter hijack"). Some parts of the submodule
    % also require knowledge of compiler/notes/allocation.html.

:- interface.

:- type resume_map.

:- type resume_point_info.

    % `prepare_for_disj_hijack' should be called before entering
    % a disjunction. It saves the values of any nondet stack slots
    % the disjunction may hijack, and if necessary, sets the redofr
    % slot of the top frame to point to this frame. The code at the
    % start of the individual disjuncts will override the redoip slot.
    %
    % `undo_disj_hijack' should be called before entering the last disjunct
    % of a disjunction. It undoes the effects of `prepare_for_disj_hijack'.
    %
:- type disj_hijack_info.

:- pred prepare_for_disj_hijack(code_model::in,
    disj_hijack_info::out, code_tree::out,
    code_info::in, code_info::out) is det.

:- pred undo_disj_hijack(disj_hijack_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

    % `prepare_for_ite_hijack' should be called before entering
    % an if-then-else. It saves the values of any nondet stack slots
    % the if-then-else may hijack, and if necessary, sets the redofr
    % slot of the top frame to point to this frame. Our caller
    % will then override the redoip slot to point to the start of
    % the else part before generating the code of the condition.
    %
    % `ite_enter_then', which should be called generating code for
    % the condition, sets up the failure state of the code generator
    % for generating the then-part, and returns the code sequences
    % to be used at the starts of the then-part and the else-part
    % to undo the effects of any hijacking.
    %
:- type ite_hijack_info.

:- pred prepare_for_ite_hijack(code_model::in, ite_hijack_info::out,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred ite_enter_then(ite_hijack_info::in,
    code_tree::out, code_tree::out, code_info::in, code_info::out) is det.

    % `enter_simple_neg' and `leave_simple_neg' should be called before
    % and after generating the code for a negated unification, in
    % situations where failure is a direct branch. We handle this case
    % specially, because it occurs frequently and should not require
    % a flushing of the expression cache, whereas the general way of
    % handling negations does require a flush. These two predicates
    % handle all aspects of the negation except for the unification
    % itself.
    %
:- type simple_neg_info.

:- pred enter_simple_neg(set(prog_var)::in, hlds_goal_info::in,
    simple_neg_info::out, code_info::in, code_info::out) is det.

:- pred leave_simple_neg(hlds_goal_info::in, simple_neg_info::in,
    code_info::in, code_info::out) is det.

    % `prepare_for_det_commit' and `generate_det_commit' should be
    % called before and after generating the code for the multi goal
    % being cut across. If the goal succeeds, the commit will cut
    % any choice points generated in the goal.
    %
:- type det_commit_info.

:- pred prepare_for_det_commit(add_trail_ops::in, det_commit_info::out,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred generate_det_commit(det_commit_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

    % `prepare_for_semi_commit' and `generate_semi_commit' should be
    % called before and after generating the code for the nondet goal
    % being cut across. If the goal succeeds, the commit will cut
    % any choice points generated in the goal.
    %
:- type semi_commit_info.

:- pred prepare_for_semi_commit(bool::in, semi_commit_info::out,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred generate_semi_commit(semi_commit_info::in,
    code_tree::out, code_info::in, code_info::out) is det.

    % Put the given resume point into effect, by pushing it on to
    % the resume point stack, and if necessary generating code to
    % override the redoip of the top nondet stack frame.
    %
:- pred effect_resume_point(resume_point_info::in, code_model::in,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred pop_resume_point(code_info::in, code_info::out) is det.

    % Return the details of the resume point currently on top of the
    % failure continuation stack.
    %
:- pred top_resume_point(code_info::in, resume_point_info::out) is det.

    % Call this predicate to say "we have just left a disjunction;
    % we don't know what address the following code will need to
    % backtrack to".
    %
:- pred set_resume_point_to_unknown(code_info::in, code_info::out) is det.

    % Call this predicate to say "we have just returned from a model_non
    % call; we don't know what address the following code will need to
    % backtrack to, and there may now be nondet frames on top of ours
    % that do not have their redofr slots pointing to our frame".
    %
:- pred set_resume_point_and_frame_to_unknown(code_info::in, code_info::out)
    is det.

    % Generate code for executing a failure that is appropriate for the
    % current failure environment.
    %
:- pred generate_failure(code_tree::out, code_info::in, code_info::out) is det.

    % Generate code that checks if the given rval is false, and if yes,
    % executes a failure that is appropriate for the current failure
    % environment.
    %
:- pred fail_if_rval_is_false(rval::in, code_tree::out,
    code_info::in, code_info::out) is det.

    % Checks whether the appropriate code for failure in the current
    % failure environment is a direct branch.
    %
:- pred failure_is_direct_branch(code_info::in, code_addr::out) is semidet.

    % Checks under what circumstances the current failure environment
    % would allow a model_non call at this point to be turned into a
    % tail call, provided of course that the return from the call is
    % followed immediately by succeed().
    %
:- pred may_use_nondet_tailcall(code_info::in, nondet_tail_call::out) is det.

    % Materialize the given variables into registers or stack slots.
    %
:- pred produce_vars(set(prog_var)::in, resume_map::out, code_tree::out,
    code_info::in, code_info::out) is det.

    % Put the variables needed in enclosing failure continuations
    % into their stack slots.
    %
:- pred flush_resume_vars_to_stack(code_tree::out,
    code_info::in, code_info::out) is det.

    % Set up the resume_point_info structure.
    %
:- pred make_resume_point(set(prog_var)::in, resume_locs::in, resume_map::in,
    resume_point_info::out, code_info::in, code_info::out) is det.

    % Generate the code for a resume point.
    %
:- pred generate_resume_point(resume_point_info::in, code_tree::out,
    code_info::in, code_info::out) is det.

    % List the variables that need to be preserved for the given resume point.
    %
:- pred resume_point_vars(resume_point_info::in, list(prog_var)::out) is det.

    % See whether the given resume point includes a code address that presumes
    % all the resume point variables to be in their stack slots. If yes,
    % return that code address; otherwise, abort the compiler.
    %
:- pred resume_point_stack_addr(resume_point_info::in, code_addr::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

    % The part of the code generator state that says how to handle
    % failures; also called the failure continuation stack.

:- type fail_info
    --->    fail_info(
                stack(resume_point_info),
                resume_point_known,
                curfr_vs_maxfr,
                condition_env,
                hijack_allowed
            ).

    % A resumption point has one or two labels associated with it.
    % Backtracking can arrive at either label. The code following
    % each label will assume that the variables needed at the resumption
    % point are in the locations given by the resume_map associated with
    % the given label and nowhere else. Any code that can cause
    % backtracking to a label must make sure that those variables are
    % in the positions expected by the label.
    %
    % The only time when a code_addr in a resume_point info is not a label
    % is when the code_addr is do_fail, which indicates that the resumption
    % point is not in (this invocation of) this procedure.
    %
:- type resume_point_info
    --->    orig_only(resume_map, code_addr)
    ;       stack_only(resume_map, code_addr)
    ;       orig_and_stack(resume_map, code_addr, resume_map, code_addr)
    ;       stack_and_orig(resume_map, code_addr, resume_map, code_addr).

    % A resume map maps the variables that will be needed at a resumption
    % point to the locations in which they will be.
    %
:- type resume_map      ==  map(prog_var, set(lval)).

:- type redoip_update
    --->    has_been_done
    ;       wont_be_done.

:- type resume_point_known
    --->    resume_point_known(redoip_update)
    ;       resume_point_unknown.

:- type curfr_vs_maxfr
    --->    must_be_equal
    ;       may_be_different.

:- type condition_env
    --->    inside_non_condition
    ;       not_inside_non_condition.

:- type hijack_allowed
    --->    allowed
    ;       not_allowed.

%---------------------------------------------------------------------------%

:- type disj_hijack_info
    --->    disj_no_hijack
    ;       disj_temp_frame
    ;       disj_quarter_hijack
    ;       disj_half_hijack(
                lval        % The stack slot in which we saved
                            % the value of the hijacked redoip.
            )
    ;       disj_full_hijack(
                lval,       % The stack slot in which we saved
                            % the value of the hijacked redoip.
                lval        % The stack slot in which we saved
                            % the value of the hijacked redofr.
            ).

prepare_for_disj_hijack(CodeModel, HijackInfo, Code, !CI) :-
    get_fail_info(!.CI, FailInfo),
    FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr, CondEnv,
        Allow),
    (
        CodeModel \= model_non
    ->
        HijackInfo = disj_no_hijack,
        Code = node([
            comment("disj no hijack") - ""
        ])
    ;
        CondEnv = inside_non_condition
    ->
        HijackInfo = disj_temp_frame,
        create_temp_frame(do_fail, "prepare for disjunction", Code, !CI)
    ;
        Allow = not_allowed
    ->
        (
            CurfrMaxfr = must_be_equal,
            ResumeKnown = resume_point_known(has_been_done),
            stack.pop(ResumePoints, TopResumePoint, RestResumePoints),
            stack.is_empty(RestResumePoints),
            TopResumePoint = stack_only(_, do_fail)
        ->
            HijackInfo = disj_quarter_hijack,
            Code = node([
                comment("disj quarter hijack of do_fail") - ""
            ])
        ;
            HijackInfo = disj_temp_frame,
            create_temp_frame(do_fail, "prepare for disjunction", Code, !CI)
        )
    ;
        CurfrMaxfr = must_be_equal,
        ResumeKnown = resume_point_known(has_been_done)
    ->
        HijackInfo = disj_quarter_hijack,
        Code = node([
            comment("disj quarter hijack") - ""
        ])
    ;
        CurfrMaxfr = must_be_equal
    ->
        % Here ResumeKnown must be resume_point_unknown
        % or resume_point_known(wont_be_done).
        acquire_temp_slot(lval(redoip_slot(lval(curfr))), RedoipSlot, !CI),
        HijackInfo = disj_half_hijack(RedoipSlot),
        Code = node([
            assign(RedoipSlot, lval(redoip_slot(lval(curfr))))
                - "prepare for half disj hijack"
        ])
    ;
        % Here CurfrMaxfr must be may_be_different.
        acquire_temp_slot(lval(redoip_slot(lval(maxfr))), RedoipSlot, !CI),
        acquire_temp_slot(lval(redofr_slot(lval(maxfr))), RedofrSlot, !CI),
        HijackInfo = disj_full_hijack(RedoipSlot, RedofrSlot),
        Code = node([
            assign(RedoipSlot, lval(redoip_slot(lval(maxfr))))
                - "prepare for full disj hijack",
            assign(RedofrSlot, lval(redofr_slot(lval(maxfr))))
                - "prepare for full disj hijack",
            assign(redofr_slot(lval(maxfr)), lval(curfr))
                - "prepare for full disj hijack"
        ])
    ).

undo_disj_hijack(HijackInfo, Code, !CI) :-
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr, CondEnv,
        Allow),
    (
        HijackInfo = disj_no_hijack,
        Code = empty
    ;
        HijackInfo = disj_temp_frame,
        Code = node([
            assign(maxfr, lval(prevfr_slot(lval(maxfr))))
                - "restore maxfr for temp frame disj"
        ])
    ;
        HijackInfo = disj_quarter_hijack,
        expect(unify(CurfrMaxfr, must_be_equal), this_file,
            "maxfr may differ from curfr in disj_quarter_hijack"),
        stack.top_det(ResumePoints, ResumePoint),
        pick_stack_resume_point(ResumePoint, _, StackLabel),
        LabelConst = const(code_addr_const(StackLabel)),
        % peephole.m looks for the "curfr==maxfr" pattern in the comment.
        Code = node([
            assign(redoip_slot(lval(curfr)), LabelConst)
                - "restore redoip for quarter disj hijack (curfr==maxfr)"
        ])
    ;
        HijackInfo = disj_half_hijack(RedoipSlot),
        expect(unify(ResumeKnown, resume_point_unknown), this_file,
            "resume point known in disj_half_hijack"),
        expect(unify(CurfrMaxfr, must_be_equal), this_file,
            "maxfr may differ from curfr in disj_half_hijack"),
        % peephole.m looks for the "curfr==maxfr" pattern in the comment.
        Code = node([
            assign(redoip_slot(lval(curfr)), lval(RedoipSlot))
                - "restore redoip for half disj hijack (curfr==maxfr)"
        ])
    ;
        HijackInfo = disj_full_hijack(RedoipSlot, RedofrSlot),
        expect(unify(CurfrMaxfr, may_be_different), this_file,
            "maxfr same as curfr in disj_full_hijack"),
        Code = node([
            assign(redoip_slot(lval(maxfr)), lval(RedoipSlot))
                - "restore redoip for full disj hijack",
            assign(redofr_slot(lval(maxfr)), lval(RedofrSlot))
                - "restore redofr for full disj hijack"
        ])
    ),
    (
        % HijackInfo \= disj_no_hijack if and only if the disjunction
        % is model_non.
        HijackInfo \= disj_no_hijack,
        CondEnv = inside_non_condition
    ->
        FailInfo = fail_info(ResumePoints, resume_point_unknown, CurfrMaxfr,
            CondEnv, Allow),
        set_fail_info(FailInfo, !CI)
    ;
        true
    ).

%---------------------------------------------------------------------------%

:- type ite_hijack_info
    --->    ite_info(
                resume_point_known,
                condition_env,
                ite_hijack_type
            ).

:- type ite_hijack_type
    --->    ite_no_hijack
    ;       ite_temp_frame(
                lval        % The stack slot in which we saved
                            % the value of maxfr.
            )
    ;       ite_quarter_hijack
    ;       ite_half_hijack(
                lval        % The stack slot in which we saved
                            % the value of the hijacked redoip.
            )
    ;       ite_full_hijack(
                lval,       % The stack slot in which we saved
                            % the value of the hijacked redoip.
                lval,       % The stack slot in which we saved
                            % the value of the hijacked redofr.
                lval        % The stack slot in which we saved
                            % the value of maxfr.
            ).

prepare_for_ite_hijack(EffCodeModel, HijackInfo, Code, !CI) :-
    get_fail_info(!.CI, FailInfo),
    FailInfo = fail_info(_, ResumeKnown, CurfrMaxfr, CondEnv, Allow),
    (
        EffCodeModel \= model_non
    ->
        HijackType = ite_no_hijack,
        Code = node([
            comment("ite no hijack") - ""
        ])
    ;
        ( Allow = not_allowed ; CondEnv = inside_non_condition )
    ->
        acquire_temp_slot(lval(maxfr), MaxfrSlot, !CI),
        HijackType = ite_temp_frame(MaxfrSlot),
        create_temp_frame(do_fail, "prepare for ite", TempFrameCode, !CI),
        MaxfrCode = node([
            assign(MaxfrSlot, lval(maxfr)) - "prepare for ite"
        ]),
        Code = tree(TempFrameCode, MaxfrCode)
    ;
        CurfrMaxfr = must_be_equal,
        ResumeKnown = resume_point_known(_)
    ->
        HijackType = ite_quarter_hijack,
        Code = node([
            comment("ite quarter hijack") - ""
        ])
    ;
        CurfrMaxfr = must_be_equal
    ->
        % Here ResumeKnown must be resume_point_unknown.
        acquire_temp_slot(lval(redoip_slot(lval(curfr))), RedoipSlot, !CI),
        HijackType = ite_half_hijack(RedoipSlot),
        Code = node([
            assign(RedoipSlot, lval(redoip_slot(lval(curfr))))
                - "prepare for half ite hijack"
        ])
    ;
        % Here CurfrMaxfr must be may_be_different.
        acquire_temp_slot(lval(redoip_slot(lval(maxfr))), RedoipSlot, !CI),
        acquire_temp_slot(lval(redofr_slot(lval(maxfr))), RedofrSlot, !CI),
        acquire_temp_slot(lval(maxfr), MaxfrSlot, !CI),
        HijackType = ite_full_hijack(RedoipSlot, RedofrSlot, MaxfrSlot),
        Code = node([
            assign(MaxfrSlot, lval(maxfr))
                - "prepare for full ite hijack",
            assign(RedoipSlot, lval(redoip_slot(lval(maxfr))))
                - "prepare for full ite hijack",
            assign(RedofrSlot, lval(redofr_slot(lval(maxfr))))
                - "prepare for full ite hijack",
            assign(redofr_slot(lval(maxfr)), lval(curfr))
                - "prepare for full ite hijack"
        ])
    ),
    HijackInfo = ite_info(ResumeKnown, CondEnv, HijackType),
    ( EffCodeModel = model_non ->
        inside_non_condition(!CI)
    ;
        true
    ).

ite_enter_then(HijackInfo, ThenCode, ElseCode, !CI) :-
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(ResumePoints0, ResumeKnown0, CurfrMaxfr, _, Allow),
    stack.pop_det(ResumePoints0, _, ResumePoints),
    HijackInfo = ite_info(HijackResumeKnown, OldCondEnv, HijackType),
    (
        HijackType = ite_no_hijack,
        ThenCode = empty,
        ElseCode = ThenCode
    ;
        HijackType = ite_temp_frame(MaxfrSlot),
        ThenCode = node([
            % We can't remove the frame, it may not be on top.
            assign(redoip_slot(lval(MaxfrSlot)),
                const(code_addr_const(do_fail)))
                - "soft cut for temp frame ite"
        ]),
        ElseCode = node([
            % XXX, search /assign(maxfr
            assign(maxfr, lval(prevfr_slot(lval(MaxfrSlot))))
                - "restore maxfr for temp frame ite"
        ])
    ;
        HijackType = ite_quarter_hijack,
        stack.top_det(ResumePoints, ResumePoint),
        ( maybe_pick_stack_resume_point(ResumePoint, _, StackLabel) ->
            LabelConst = const(code_addr_const(StackLabel)),
            ThenCode = node([
                assign(redoip_slot(lval(curfr)), LabelConst)
                    - "restore redoip for quarter ite hijack"
            ])
        ;
            % This can happen only if ResumePoint is unreachable from here.
            ThenCode = empty
        ),
        ElseCode = ThenCode
    ;
        HijackType = ite_half_hijack(RedoipSlot),
        ThenCode = node([
            assign(redoip_slot(lval(curfr)), lval(RedoipSlot))
                - "restore redoip for half ite hijack"
        ]),
        ElseCode = ThenCode
    ;
        HijackType = ite_full_hijack(RedoipSlot, RedofrSlot, MaxfrSlot),
        ThenCode = node([
            assign(redoip_slot(lval(MaxfrSlot)), lval(RedoipSlot))
                - "restore redoip for full ite hijack",
            assign(redofr_slot(lval(MaxfrSlot)), lval(RedofrSlot))
                - "restore redofr for full ite hijack"
        ]),
        ElseCode = node([
            assign(redoip_slot(lval(maxfr)), lval(RedoipSlot))
                - "restore redoip for full ite hijack",
            assign(redofr_slot(lval(maxfr)), lval(RedofrSlot))
                - "restore redofr for full ite hijack"
        ])
    ),
    ( ResumeKnown0 = resume_point_unknown ->
        ResumeKnown = resume_point_unknown
    ;
        ResumeKnown = HijackResumeKnown
    ),
    FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr, OldCondEnv,
        Allow),
    set_fail_info(FailInfo, !CI).

%---------------------------------------------------------------------------%

:- type simple_neg_info     ==  fail_info.

enter_simple_neg(ResumeVars, GoalInfo, FailInfo0, !CI) :-
    get_fail_info(!.CI, FailInfo0),
    % The only reason why we push a resume point at all is to protect
    % the variables in ResumeVars from becoming unknown; by including them
    % in the domain of the resume point, we guarantee that they will become
    % zombies instead of unknown if they die in the pre- or post-goal updates.
    % Therefore the only part of ResumePoint that matters is the set of
    % variables in the resume map; the other parts of ResumePoint
    % (the locations, the code address) will not be referenced.
    set.to_sorted_list(ResumeVars, ResumeVarList),
    map.init(ResumeMap0),
    make_fake_resume_map(ResumeVarList, ResumeMap0, ResumeMap),
    ResumePoint = orig_only(ResumeMap, do_redo),
    effect_resume_point(ResumePoint, model_semi, Code, !CI),
    expect(unify(Code, empty), this_file, "nonempty code for simple neg"),
    pre_goal_update(GoalInfo, yes, !CI).

leave_simple_neg(GoalInfo, FailInfo, !CI) :-
    post_goal_update(GoalInfo, !CI),
    set_fail_info(FailInfo, !CI).

:- pred make_fake_resume_map(list(prog_var)::in,
    map(prog_var, set(lval))::in, map(prog_var, set(lval))::out) is det.

make_fake_resume_map([], ResumeMap, ResumeMap).
make_fake_resume_map([Var | Vars], ResumeMap0, ResumeMap) :-
    % A visibly fake location.
    set.singleton_set(Locns, reg(r, -1)),
    map.det_insert(ResumeMap0, Var, Locns, ResumeMap1),
    make_fake_resume_map(Vars, ResumeMap1, ResumeMap).

%---------------------------------------------------------------------------%

:- type det_commit_info
    --->    det_commit_info(
                maybe(lval),        % Location of saved maxfr.
                maybe(pair(lval))   % Location of saved ticket
                                    % counter and trail pointer.
            ).

prepare_for_det_commit(AddTrailOps, DetCommitInfo, Code, !CI) :-
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(_, _, CurfrMaxfr, _, _),
    (
        CurfrMaxfr = may_be_different,
        acquire_temp_slot(lval(maxfr), MaxfrSlot, !CI),
        SaveMaxfrCode = node([
            save_maxfr(MaxfrSlot) - "save the value of maxfr"
        ]),
        MaybeMaxfrSlot = yes(MaxfrSlot)
    ;
        CurfrMaxfr = must_be_equal,
        SaveMaxfrCode = empty,
        MaybeMaxfrSlot = no
    ),
    maybe_save_trail_info(AddTrailOps, MaybeTrailSlots, SaveTrailCode, !CI),
    DetCommitInfo = det_commit_info(MaybeMaxfrSlot, MaybeTrailSlots),
    Code = tree(SaveMaxfrCode, SaveTrailCode).

generate_det_commit(DetCommitInfo, Code, !CI) :-
    DetCommitInfo = det_commit_info(MaybeMaxfrSlot, MaybeTrailSlots),
    (
        MaybeMaxfrSlot = yes(MaxfrSlot),
        RestoreMaxfrCode = node([
            restore_maxfr(MaxfrSlot)
                - "restore the value of maxfr - perform commit"
        ]),
        release_temp_slot(MaxfrSlot, !CI)
    ;
        MaybeMaxfrSlot = no,
        RestoreMaxfrCode = node([
            assign(maxfr, lval(curfr))
                - "restore the value of maxfr - perform commit"
        ])
    ),
    maybe_restore_trail_info(MaybeTrailSlots, CommitTrailCode, _, !CI),
    Code = tree(RestoreMaxfrCode, CommitTrailCode).

%---------------------------------------------------------------------------%

:- type semi_commit_info
    --->    semi_commit_info(
                fail_info,              % Fail_info on entry.
                resume_point_info,
                commit_hijack_info,
                maybe(pair(lval))       % Location of saved ticket
                                        % counter and trail pointer.
            ).

:- type commit_hijack_info
    --->    commit_temp_frame(
                lval,       % The stack slot in which we saved
                            % the old value of maxfr.
                bool        % Do we bracket the goal with
                            % MR_commit_mark and MR_commit_cut?
            )
    ;       commit_quarter_hijack
    ;       commit_half_hijack(
                lval        % The stack slot in which we saved
                            % the value of the hijacked redoip.
            )
    ;       commit_full_hijack(
                lval,       % The stack slot in which we saved
                            % the value of the hijacked redoip.
                lval,       % The stack slot in which we saved
                            % the value of the hijacked redofr.
                lval        % The stack slot in which we saved
                            % the value of maxfr.
            ).

prepare_for_semi_commit(AddTrailOps, SemiCommitInfo, Code, !CI) :-
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(ResumePoints0, ResumeKnown, CurfrMaxfr, CondEnv,
        Allow),
    stack.top_det(ResumePoints0, TopResumePoint),
    clone_resume_point(TopResumePoint, NewResumePoint, !CI),
    stack.push(ResumePoints0, NewResumePoint, ResumePoints),
    FailInfo = fail_info(ResumePoints, resume_point_known(has_been_done),
        CurfrMaxfr, CondEnv, Allow),
    set_fail_info(FailInfo, !CI),

    pick_stack_resume_point(NewResumePoint, _, StackLabel),
    StackLabelConst = const(code_addr_const(StackLabel)),
    (
        ( Allow = not_allowed ; CondEnv = inside_non_condition )
    ->
        acquire_temp_slot(lval(maxfr), MaxfrSlot, !CI),
        MaxfrCode = node([
            save_maxfr(MaxfrSlot)
                - "prepare for temp frame commit"
        ]),
        create_temp_frame(StackLabel,
            "prepare for temp frame commit", TempFrameCode, !CI),
        get_globals(!.CI, Globals),
        globals.lookup_bool_option(Globals, use_minimal_model_stack_copy_cut,
            UseMinimalModelStackCopyCut),
        HijackInfo = commit_temp_frame(MaxfrSlot, UseMinimalModelStackCopyCut),
        (
            UseMinimalModelStackCopyCut = yes,
            % If the code we are committing across starts but
            % does not complete the evaluation of a tabled subgoal,
            % the cut will remove the generator's choice point,
            % so that the evaluation of the subgoal will never
            % be completed. We handle such "dangling" generators
            % by removing them from the subgoal trie of the
            % tabled procedure. This requires knowing what
            % tabled subgoals are started inside commits,
            % which is why we wrap the goal being committed across
            % inside MR_commit_{mark,cut}.
            Components = [
                pragma_c_raw_code(
                    "\t\tMR_save_transient_registers();\n",
                    cannot_branch_away, live_lvals_info(set.init)),
                pragma_c_raw_code(
                    "\t\tMR_commit_mark();\n",
                    cannot_branch_away, live_lvals_info(set.init)),
                pragma_c_raw_code(
                    "\t\tMR_restore_transient_registers();\n",
                    cannot_branch_away, live_lvals_info(set.init))
            ],
            MarkCode = node([
                pragma_c([], Components, will_not_call_mercury,
                    no, no, no, no, no, yes) - ""
            ])
        ;
            UseMinimalModelStackCopyCut = no,
            MarkCode = empty
        ),
        HijackCode = tree(MaxfrCode, tree(TempFrameCode, MarkCode))
    ;
        ResumeKnown = resume_point_known(has_been_done),
        CurfrMaxfr = must_be_equal
    ->
        HijackInfo = commit_quarter_hijack,
        HijackCode = node([
            assign(redoip_slot(lval(curfr)), StackLabelConst)
                - "hijack the redofr slot"
        ])
    ;
        CurfrMaxfr = must_be_equal
    ->
        % Here ResumeKnown must be resume_point_unknown or
        % resume_point_known(wont_be_done).

        acquire_temp_slot(lval(redoip_slot(lval(curfr))), RedoipSlot, !CI),
        HijackInfo = commit_half_hijack(RedoipSlot),
        HijackCode = node([
            assign(RedoipSlot, lval(redoip_slot(lval(curfr))))
                - "prepare for half commit hijack",
            assign(redoip_slot(lval(curfr)), StackLabelConst)
                - "hijack the redofr slot"
        ])
    ;
        % Here CurfrMaxfr must be may_be_different.
        acquire_temp_slot(lval(redoip_slot(lval(maxfr))), RedoipSlot, !CI),
        acquire_temp_slot(lval(redofr_slot(lval(maxfr))), RedofrSlot, !CI),
        acquire_temp_slot(lval(maxfr), MaxfrSlot, !CI),
        HijackInfo = commit_full_hijack(RedoipSlot, RedofrSlot, MaxfrSlot),
        HijackCode = node([
            assign(RedoipSlot, lval(redoip_slot(lval(maxfr))))
                - "prepare for full commit hijack",
            assign(RedofrSlot, lval(redofr_slot(lval(maxfr))))
                - "prepare for full commit hijack",
            save_maxfr(MaxfrSlot)
                - "prepare for full commit hijack",
            assign(redofr_slot(lval(maxfr)), lval(curfr))
                - "hijack the redofr slot",
            assign(redoip_slot(lval(maxfr)), StackLabelConst)
                - "hijack the redoip slot"
        ])
    ),
    maybe_save_trail_info(AddTrailOps, MaybeTrailSlots, SaveTrailCode, !CI),
    SemiCommitInfo = semi_commit_info(FailInfo0, NewResumePoint,
        HijackInfo, MaybeTrailSlots),
    Code = tree(HijackCode, SaveTrailCode).

generate_semi_commit(SemiCommitInfo, Code, !CI) :-
    SemiCommitInfo = semi_commit_info(FailInfo, ResumePoint,
        HijackInfo, MaybeTrailSlots),

    set_fail_info(FailInfo, !CI),
    % XXX Should release the temp slots in each arm of the switch.
    (
        HijackInfo = commit_temp_frame(MaxfrSlot, UseMinimalModel),
        MaxfrCode = node([
            restore_maxfr(MaxfrSlot)
                - "restore maxfr for temp frame hijack"
        ]),
        (
            UseMinimalModel = yes,
            % See the comment in prepare_for_semi_commit above.
            Components = [
                pragma_c_raw_code("\t\tMR_commit_cut();\n",
                    cannot_branch_away, live_lvals_info(set.init))
            ],
            CutCode = node([
                pragma_c([], Components, will_not_call_mercury, no, no, no,
                    no, no, yes) - "commit for temp frame hijack"
            ])
        ;
            UseMinimalModel = no,
            CutCode = empty
        ),
        SuccessUndoCode = tree(MaxfrCode, CutCode),
        FailureUndoCode = tree(MaxfrCode, CutCode)
    ;
        HijackInfo = commit_quarter_hijack,
        FailInfo = fail_info(ResumePoints, _, _, _, _),
        stack.top_det(ResumePoints, TopResumePoint),
        pick_stack_resume_point(TopResumePoint, _, StackLabel),
        StackLabelConst = const(code_addr_const(StackLabel)),
        SuccessUndoCode = node([
            assign(maxfr, lval(curfr))
                - "restore maxfr for quarter commit hijack",
            assign(redoip_slot(lval(maxfr)), StackLabelConst)
                - "restore redoip for quarter commit hijack"
        ]),
        FailureUndoCode = node([
            assign(redoip_slot(lval(maxfr)), StackLabelConst)
                - "restore redoip for quarter commit hijack"
        ])
    ;
        HijackInfo = commit_half_hijack(RedoipSlot),
        SuccessUndoCode = node([
            assign(maxfr, lval(curfr))
                - "restore maxfr for half commit hijack",
            assign(redoip_slot(lval(maxfr)), lval(RedoipSlot))
                - "restore redoip for half commit hijack"
        ]),
        FailureUndoCode = node([
            assign(redoip_slot(lval(maxfr)), lval(RedoipSlot))
                - "restore redoip for half commit hijack"
        ])
    ;
        HijackInfo = commit_full_hijack(RedoipSlot, RedofrSlot, MaxfrSlot),
        SuccessUndoCode = node([
            restore_maxfr(MaxfrSlot)
                - "restore maxfr for full commit hijack",
            assign(redoip_slot(lval(maxfr)), lval(RedoipSlot))
                - "restore redoip for full commit hijack",
            assign(redofr_slot(lval(maxfr)), lval(RedofrSlot))
                - "restore redofr for full commit hijack"
        ]),
        FailureUndoCode = node([
            assign(redoip_slot(lval(maxfr)), lval(RedoipSlot))
                - "restore redoip for full commit hijack",
            assign(redofr_slot(lval(maxfr)), lval(RedofrSlot))
                - "restore redofr for full commit hijack"
        ])
    ),

    remember_position(!.CI, AfterCommit),
    generate_resume_point(ResumePoint, ResumePointCode, !CI),
    generate_failure(FailCode, !CI),
    reset_to_position(AfterCommit, !CI),

    maybe_restore_trail_info(MaybeTrailSlots, CommitTrailCode,
        RestoreTrailCode, !CI),

    get_next_label(SuccLabel, !CI),
    GotoSuccLabel = node([
        goto(label(SuccLabel)) - "Jump to success continuation"
    ]),
    SuccLabelCode = node([
        label(SuccLabel) - "Success continuation"
    ]),
    SuccessCode = tree(SuccessUndoCode, CommitTrailCode),
    FailureCode = tree_list([ResumePointCode, FailureUndoCode,
        RestoreTrailCode, FailCode]),
    Code = tree_list([SuccessCode, GotoSuccLabel,
        FailureCode, SuccLabelCode]).

%---------------------------------------------------------------------------%

:- pred inside_non_condition(code_info::in, code_info::out) is det.

inside_non_condition(!CI) :-
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr, _, Allow),
    FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr,
        inside_non_condition, Allow),
    set_fail_info(FailInfo, !CI).

:- pred create_temp_frame(code_addr::in, string::in, code_tree::out,
    code_info::in, code_info::out) is det.

create_temp_frame(Redoip, Comment, Code, !CI) :-
    ( get_proc_model(!.CI) = model_non ->
        Kind = nondet_stack_proc
    ;
        Kind = det_stack_proc
    ),
    Code = node([
        mkframe(temp_frame(Kind), yes(Redoip)) - Comment
    ]),
    set_created_temp_frame(yes, !CI),
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, ResumeKnown, _, CondEnv, Allow),
    FailInfo = fail_info(ResumePoints, ResumeKnown, may_be_different,
        CondEnv, Allow),
    set_fail_info(FailInfo, !CI).

%---------------------------------------------------------------------------%

effect_resume_point(ResumePoint, CodeModel, Code, !CI) :-
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(ResumePoints0, _ResumeKnown, CurfrMaxfr,
        CondEnv, Allow),
    ( stack.top(ResumePoints0, OldResumePoint) ->
        pick_first_resume_point(OldResumePoint, OldMap, _),
        pick_first_resume_point(ResumePoint, NewMap, _),
        map.keys(OldMap, OldKeys),
        map.keys(NewMap, NewKeys),
        set.list_to_set(OldKeys, OldKeySet),
        set.list_to_set(NewKeys, NewKeySet),
        expect(set.subset(OldKeySet, NewKeySet), this_file,
            "non-nested resume point variable sets")
    ;
        true
    ),
    stack.push(ResumePoints0, ResumePoint, ResumePoints),
    ( CodeModel = model_non ->
        pick_stack_resume_point(ResumePoint, _, StackLabel),
        LabelConst = const(code_addr_const(StackLabel)),
        Code = node([
            assign(redoip_slot(lval(maxfr)), LabelConst)
                - "hijack redoip to effect resume point"
        ]),
        RedoipUpdate = has_been_done
    ;
        Code = empty,
        RedoipUpdate = wont_be_done
    ),
    FailInfo = fail_info(ResumePoints, resume_point_known(RedoipUpdate),
        CurfrMaxfr, CondEnv, Allow),
    set_fail_info(FailInfo, !CI).

pop_resume_point(!CI) :-
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(ResumePoints0, ResumeKnown, CurfrMaxfr,
        CondEnv, Allow),
    stack.pop_det(ResumePoints0, _, ResumePoints),
    FailInfo = fail_info(ResumePoints, ResumeKnown, CurfrMaxfr,
        CondEnv, Allow),
    set_fail_info(FailInfo, !CI).

%---------------------------------------------------------------------------%

top_resume_point(CI, ResumePoint) :-
    get_fail_info(CI, FailInfo),
    FailInfo = fail_info(ResumePoints, _, _, _, _),
    stack.top_det(ResumePoints, ResumePoint).

set_resume_point_to_unknown(!CI) :-
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, _, CurfrMaxfr, CondEnv, Allow),
    FailInfo = fail_info(ResumePoints, resume_point_unknown,
        CurfrMaxfr, CondEnv, Allow),
    set_fail_info(FailInfo, !CI).

set_resume_point_and_frame_to_unknown(!CI) :-
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(ResumePoints, _, _, CondEnv, Allow),
    FailInfo = fail_info(ResumePoints, resume_point_unknown, may_be_different,
        CondEnv, Allow),
    set_fail_info(FailInfo, !CI).

%---------------------------------------------------------------------------%

generate_failure(Code, !CI) :-
    get_fail_info(!.CI, FailInfo),
    FailInfo = fail_info(ResumePoints, ResumeKnown, _, _, _),
    (
        ResumeKnown = resume_point_known(_),
        stack.top_det(ResumePoints, TopResumePoint),
        ( pick_matching_resume_addr(!.CI, TopResumePoint, FailureAddress0) ->
            FailureAddress = FailureAddress0,
            PlaceCode = empty
        ;
            pick_first_resume_point(TopResumePoint, Map, FailureAddress),
            map.to_assoc_list(Map, AssocList),
            remember_position(!.CI, CurPos),
            pick_and_place_vars(AssocList, _, PlaceCode, !CI),
            reset_to_position(CurPos, !CI)
        ),
        BranchCode = node([goto(FailureAddress) - "fail"]),
        Code = tree(PlaceCode, BranchCode)
    ;
        ResumeKnown = resume_point_unknown,
        Code = node([goto(do_redo) - "fail"])
    ).

fail_if_rval_is_false(Rval0, Code, !CI) :-
    get_fail_info(!.CI, FailInfo),
    FailInfo = fail_info(ResumePoints, ResumeKnown, _, _, _),
    (
        ResumeKnown = resume_point_known(_),
        stack.top_det(ResumePoints, TopResumePoint),
        ( pick_matching_resume_addr(!.CI, TopResumePoint, FailureAddress0) ->
            % We branch away if the test *fails*
            code_util.neg_rval(Rval0, Rval),
            Code = node([
                if_val(Rval, FailureAddress0) - "Test for failure"
            ])
        ;
            pick_first_resume_point(TopResumePoint, Map, FailureAddress),
            map.to_assoc_list(Map, AssocList),
            get_next_label(SuccessLabel, !CI),
            remember_position(!.CI, CurPos),
            pick_and_place_vars(AssocList, _, PlaceCode, !CI),
            reset_to_position(CurPos, !CI),
            SuccessAddress = label(SuccessLabel),
            % We branch away if the test *fails*, therefore if the test
            % succeeds, we branch around the code that moves variables to
            % their failure locations and branches away to the failure
            % continuation.
            TestCode = node([
                if_val(Rval0, SuccessAddress) - "Test for failure"
            ]),
            TailCode = node([
                goto(FailureAddress) - "Goto failure",
                label(SuccessLabel) - "Success continuation"
            ]),
            Code = tree(TestCode, tree(PlaceCode, TailCode))
        )
    ;
        ResumeKnown = resume_point_unknown,
        % We branch away if the test *fails*
        code_util.neg_rval(Rval0, Rval),
        Code = node([
            if_val(Rval, do_redo) - "Test for failure"
        ])
    ).

%---------------------------------------------------------------------------%

failure_is_direct_branch(CI, CodeAddr) :-
    get_fail_info(CI, FailInfo),
    FailInfo = fail_info(ResumePoints, resume_point_known(_), _, _, _),
    stack.top(ResumePoints, TopResumePoint),
    pick_matching_resume_addr(CI, TopResumePoint, CodeAddr).

may_use_nondet_tailcall(CI, TailCallStatus) :-
    get_fail_info(CI, FailInfo),
    FailInfo = fail_info(ResumePoints0, ResumeKnown, _, _, _),
    (
        stack.pop(ResumePoints0, ResumePoint1, ResumePoints1),
        stack.is_empty(ResumePoints1),
        ResumePoint1 = stack_only(_, do_fail)
    ->
        (
            ResumeKnown = resume_point_known(_),
            TailCallStatus = unchecked_tail_call
        ;
            ResumeKnown = resume_point_unknown,
            TailCallStatus = checked_tail_call
        )
    ;
        TailCallStatus = no_tail_call
    ).

%---------------------------------------------------------------------------%

    % See whether the current locations of variables match the locations
    % associated with any of the options in the given failure map.
    % If yes, return the code_addr of that option.
    %
:- pred pick_matching_resume_addr(code_info::in,
    resume_point_info::in, code_addr::out) is semidet.

pick_matching_resume_addr(CI, ResumeMaps, Addr) :-
    variable_locations(CI, CurLocs),
    (
        ResumeMaps = orig_only(Map1, Addr1),
        ( match_resume_loc(Map1, CurLocs) ->
            Addr = Addr1
        ;
            fail
        )
    ;
        ResumeMaps = stack_only(Map1, Addr1),
        ( match_resume_loc(Map1, CurLocs) ->
            Addr = Addr1
        ;
            fail
        )
    ;
        ResumeMaps = orig_and_stack(Map1, Addr1, Map2, Addr2),
        ( match_resume_loc(Map1, CurLocs) ->
            Addr = Addr1
        ; match_resume_loc(Map2, CurLocs) ->
            Addr = Addr2
        ;
            fail
        )
    ;
        ResumeMaps = stack_and_orig(Map1, Addr1, Map2, Addr2),
        ( match_resume_loc(Map1, CurLocs) ->
            Addr = Addr1
        ; match_resume_loc(Map2, CurLocs) ->
            Addr = Addr2
        ;
            fail
        )
    ).

:- pred match_resume_loc(resume_map::in, resume_map::in) is semidet.

match_resume_loc(Map, Locations0) :-
    map.keys(Map, KeyList),
    set.list_to_set(KeyList, Keys),
    map.select(Locations0, Keys, Locations),
    map.to_assoc_list(Locations, List),
    (
        list.member(Var - Actual, List)
    =>
        (
            map.search(Map, Var, Lvals),
            set.subset(Lvals, Actual)
        )
    ).

:- pred pick_first_resume_point(resume_point_info::in,
    resume_map::out, code_addr::out) is det.

pick_first_resume_point(orig_only(Map, Addr), Map, Addr).
pick_first_resume_point(stack_only(Map, Addr), Map, Addr).
pick_first_resume_point(orig_and_stack(Map, Addr, _, _), Map, Addr).
pick_first_resume_point(stack_and_orig(Map, Addr, _, _), Map, Addr).

:- pred pick_stack_resume_point(resume_point_info::in,
    resume_map::out, code_addr::out) is det.

pick_stack_resume_point(ResumePoint, Map, Addr) :-
    ( maybe_pick_stack_resume_point(ResumePoint, Map1, Addr1) ->
        Map = Map1,
        Addr = Addr1
    ;
        unexpected(this_file, "no stack resume point")
    ).

:- pred maybe_pick_stack_resume_point(resume_point_info::in,
    resume_map::out, code_addr::out) is semidet.

maybe_pick_stack_resume_point(stack_only(Map, Addr), Map, Addr).
maybe_pick_stack_resume_point(orig_and_stack(_, _, Map, Addr),
    Map, Addr).
maybe_pick_stack_resume_point(stack_and_orig(Map, Addr, _, _),
    Map, Addr).

%---------------------------------------------------------------------------%

produce_vars(Vars, Map, Code, !CI) :-
    set.to_sorted_list(Vars, VarList),
    produce_vars_2(VarList, Map, Code, !CI).

:- pred produce_vars_2(list(prog_var)::in,
    map(prog_var, set(lval))::out,
    code_tree::out, code_info::in, code_info::out) is det.

produce_vars_2([], Map, empty, !CI) :-
    map.init(Map).
produce_vars_2([V | Vs], Map, Code, !CI) :-
    produce_vars_2(Vs, Map0, Code0, !CI),
    produce_variable_in_reg_or_stack(V, Code1, Lval, !CI),
    set.singleton_set(Lvals, Lval),
    map.set(Map0, V, Lvals, Map),
    Code = tree(Code0, Code1).

flush_resume_vars_to_stack(Code, !CI) :-
    compute_resume_var_stack_locs(!.CI, VarLocs),
    place_vars(VarLocs, Code, !CI).

:- pred compute_resume_var_stack_locs(code_info::in,
    assoc_list(prog_var, lval)::out) is det.

compute_resume_var_stack_locs(CI, VarLocs) :-
    get_fail_info(CI, FailInfo),
    FailInfo = fail_info(ResumePointStack, _, _, _, _),
    stack.top_det(ResumePointStack, ResumePoint),
    pick_stack_resume_point(ResumePoint, StackMap, _),
    map.to_assoc_list(StackMap, VarLocSets),
    pick_var_places(VarLocSets, VarLocs).

%---------------------------------------------------------------------------%

:- pred init_fail_info(code_model::in, maybe(set(prog_var))::in,
    resume_point_info::out, code_info::in, code_info::out) is det.

init_fail_info(CodeModel, MaybeFailVars, ResumePoint, !CI) :-
    (
        CodeModel = model_det,
        get_next_label(ResumeLabel, !CI),
        ResumeAddress = label(ResumeLabel),
        ResumeKnown = resume_point_unknown,
        CurfrMaxfr = may_be_different
    ;
        CodeModel = model_semi,
        % The resume point for this label will be part of the procedure epilog.
        get_next_label(ResumeLabel, !CI),
        ResumeAddress = label(ResumeLabel),
        ResumeKnown = resume_point_known(wont_be_done),
        CurfrMaxfr = may_be_different
    ;
        CodeModel = model_non,
        (
            MaybeFailVars = yes(_),
            get_next_label(ResumeLabel, !CI),
            ResumeAddress = label(ResumeLabel)
        ;
            MaybeFailVars = no,
            ResumeAddress = do_fail
        ),
        ResumeKnown = resume_point_known(has_been_done),
        CurfrMaxfr = must_be_equal
    ),
    (
        MaybeFailVars = yes(FailVars),
        get_stack_slots(!.CI, StackSlots),
        map.select(StackSlots, FailVars, AbsStackMap),
        map.to_assoc_list(AbsStackMap, AbsStackList),
        StackList0 = assoc_list.map_values(key_stack_slot_to_lval,
            AbsStackList),
        make_singleton_sets(StackList0, StackList),
        map.from_assoc_list(StackList, StackMap)
    ;
        MaybeFailVars = no,
        map.init(StackMap)
    ),
    ResumePoint = stack_only(StackMap, ResumeAddress),
    stack.init(ResumeStack0),
    stack.push(ResumeStack0, ResumePoint, ResumeStack),
    get_fail_info(!.CI, FailInfo0),
    FailInfo0 = fail_info(_, _, _, _, Allow),
    FailInfo = fail_info(ResumeStack, ResumeKnown, CurfrMaxfr,
        not_inside_non_condition, Allow),
    set_fail_info(FailInfo, !CI).

%---------------------------------------------------------------------------%

make_resume_point(ResumeVars, ResumeLocs, FullMap, ResumePoint, !CI) :-
    get_stack_slots(!.CI, StackSlots),
    map.select(FullMap, ResumeVars, OrigMap),
    (
        ResumeLocs = orig_only,
        get_next_label(OrigLabel, !CI),
        OrigAddr = label(OrigLabel),
        ResumePoint = orig_only(OrigMap, OrigAddr)
    ;
        ResumeLocs = stack_only,
        make_stack_resume_map(ResumeVars, StackSlots, StackMap),
        get_next_label(StackLabel, !CI),
        StackAddr = label(StackLabel),
        ResumePoint = stack_only(StackMap, StackAddr)
    ;
        ResumeLocs = orig_and_stack,
        make_stack_resume_map(ResumeVars, StackSlots, StackMap),
        get_next_label(OrigLabel, !CI),
        OrigAddr = label(OrigLabel),
        get_next_label(StackLabel, !CI),
        StackAddr = label(StackLabel),
        ResumePoint = orig_and_stack(OrigMap, OrigAddr, StackMap, StackAddr)
    ;
        ResumeLocs = stack_and_orig,
        make_stack_resume_map(ResumeVars, StackSlots, StackMap),
        get_next_label(StackLabel, !CI),
        StackAddr = label(StackLabel),
        get_next_label(OrigLabel, !CI),
        OrigAddr = label(OrigLabel),
        ResumePoint = stack_and_orig(StackMap, StackAddr, OrigMap, OrigAddr)
    ).

:- pred make_stack_resume_map(set(prog_var)::in, stack_slots::in,
    map(prog_var, set(lval))::out) is det.

make_stack_resume_map(ResumeVars, StackSlots, StackMap) :-
    map.select(StackSlots, ResumeVars, StackMap0),
    map.to_assoc_list(StackMap0, AbsStackList),
    StackList0 = assoc_list.map_values(key_stack_slot_to_lval, AbsStackList),
    make_singleton_sets(StackList0, StackList),
    map.from_assoc_list(StackList, StackMap).

:- pred make_singleton_sets(assoc_list(prog_var, lval)::in,
    assoc_list(prog_var, set(lval))::out) is det.

make_singleton_sets([], []).
make_singleton_sets([V - L | Rest0], [V - Ls | Rest]) :-
    set.singleton_set(Ls, L),
    make_singleton_sets(Rest0, Rest).

%---------------------------------------------------------------------------%

    % The code we generate for a resumption point looks like this:
    %
    % label(StackLabel)
    % <assume variables are where StackMap says they are>
    % <copy variables to their locations according to OrigMap>
    % label(OrigLabel)
    % <assume variables are where OrigMap says they are>
    %
    % Failures at different points may cause control to arrive at
    % the resumption point via either label, which is why the last
    % line is necessary.
    %
    % The idea is that failures from other procedures will go to
    % StackLabel, and that failures from this procedure while
    % everything is in its original place will go to OrigLabel.
    % Failures from this procedure where not everything is in its
    % original place can go to either, after moving the resume variables
    % to the places where the label expects them.
    %
    % The above layout (stack, then orig) is the most common. However,
    % liveness.m may decide that one or other of the two labels will
    % never be referred to (e.g. because there are no calls inside
    % the range of effect of the resumption point or because a call
    % follows immediately after the establishment of the resumption
    % point), or that it would be more efficient to put the two labels
    % in the other order (e.g. because the code after the resumption point
    % needs most of the variables in their stack slots).

generate_resume_point(ResumePoint, Code, !CI) :-
    (
        ResumePoint = orig_only(Map1, Addr1),
        extract_label_from_code_addr(Addr1, Label1),
        Code = node([
            label(Label1) - "orig only failure continuation"
        ]),
        set_var_locations(Map1, !CI)
    ;
        ResumePoint = stack_only(Map1, Addr1),
        extract_label_from_code_addr(Addr1, Label1),
        Code = node([
            label(Label1) - "stack only failure continuation"
        ]),
        set_var_locations(Map1, !CI),
        generate_resume_layout(Label1, Map1, !CI)
    ;
        ResumePoint = stack_and_orig(Map1, Addr1, Map2, Addr2),
        extract_label_from_code_addr(Addr1, Label1),
        extract_label_from_code_addr(Addr2, Label2),
        Label1Code = node([
            label(Label1) - "stack failure continuation before orig"
        ]),
        set_var_locations(Map1, !CI),
        generate_resume_layout(Label1, Map1, !CI),
        map.to_assoc_list(Map2, AssocList2),
        place_resume_vars(AssocList2, PlaceCode, !CI),
        Label2Code = node([
            label(Label2) - "orig failure continuation after stack"
        ]),
        set_var_locations(Map2, !CI),
        Code = tree(Label1Code, tree(PlaceCode, Label2Code))
    ;
        ResumePoint = orig_and_stack(Map1, Addr1, Map2, Addr2),
        extract_label_from_code_addr(Addr1, Label1),
        extract_label_from_code_addr(Addr2, Label2),
        Label1Code = node([
            label(Label1) - "orig failure continuation before stack"
        ]),
        set_var_locations(Map1, !CI),
        map.to_assoc_list(Map2, AssocList2),
        place_resume_vars(AssocList2, PlaceCode, !CI),
        Label2Code = node([
            label(Label2) - "stack failure continuation after orig"
        ]),
        set_var_locations(Map2, !CI),
        generate_resume_layout(Label2, Map2, !CI),
        Code = tree(Label1Code, tree(PlaceCode, Label2Code))
    ).

:- pred extract_label_from_code_addr(code_addr::in, label::out) is det.

extract_label_from_code_addr(CodeAddr, Label) :-
    ( CodeAddr = label(Label0) ->
        Label = Label0
    ;
        unexpected(this_file, "extract_label_from_code_addr: non-label!")
    ).

:- pred place_resume_vars(assoc_list(prog_var, set(lval))::in,
    code_tree::out, code_info::in, code_info::out) is det.

place_resume_vars([], empty, !CI).
place_resume_vars([Var - TargetSet | Rest], Code, !CI) :-
    set.to_sorted_list(TargetSet, Targets),
    place_resume_var(Var, Targets, FirstCode, !CI),
    Code = tree(FirstCode, RestCode),
    place_resume_vars(Rest, RestCode, !CI).

:- pred place_resume_var(prog_var::in, list(lval)::in,
    code_tree::out, code_info::in, code_info::out) is det.

place_resume_var(_Var, [], empty, !CI).
place_resume_var(Var, [Target | Targets], Code, !CI) :-
    place_var(Var, Target, FirstCode, !CI),
    place_resume_var(Var, Targets, RestCode, !CI),
    Code = tree(FirstCode, RestCode).

    % Reset the code generator's database of what is where.
    % Remember that the variables in the map are available in their
    % associated rvals; forget about all other variables.
    %
:- pred set_var_locations(resume_map::in,
    code_info::in, code_info::out) is det.

set_var_locations(Map, !CI) :-
    map.to_assoc_list(Map, LvalList0),
    flatten_varlval_list(LvalList0, LvalList),
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.reinit_state(LvalList, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

:- pred flatten_varlval_list(assoc_list(prog_var, set(lval))::in,
    assoc_list(prog_var, lval)::out) is det.

flatten_varlval_list([], []).
flatten_varlval_list([V - Rvals | Rest0], All) :-
    flatten_varlval_list(Rest0, Rest),
    set.to_sorted_list(Rvals, RvalList),
    flatten_varlval_list_2(RvalList, V, Rest1),
    list.append(Rest1, Rest, All).

:- pred flatten_varlval_list_2(list(lval)::in, prog_var::in,
    assoc_list(prog_var, lval)::out) is det.

flatten_varlval_list_2([], _V, []).
flatten_varlval_list_2([R | Rs], V, [V - R | Rest]) :-
    flatten_varlval_list_2(Rs, V, Rest).

resume_point_vars(ResumePoint, Vars) :-
    pick_first_resume_point(ResumePoint, ResumeMap, _),
    map.keys(ResumeMap, Vars).

resume_point_stack_addr(ResumePoint, StackAddr) :-
    pick_stack_resume_point(ResumePoint, _, StackAddr).

%---------------------------------------------------------------------------%

:- pred maybe_save_trail_info(bool::in, maybe(pair(lval))::out,
    code_tree::out, code_info::in, code_info::out) is det.

maybe_save_trail_info(AddTrailOps, MaybeTrailSlots, SaveTrailCode, !CI) :-
    (
        AddTrailOps = yes,
        acquire_temp_slot(ticket_counter, CounterSlot, !CI),
        acquire_temp_slot(ticket, TrailPtrSlot, !CI),
        MaybeTrailSlots = yes(CounterSlot - TrailPtrSlot),
        SaveTrailCode = node([
            mark_ticket_stack(CounterSlot) - "save the ticket counter",
            store_ticket(TrailPtrSlot) - "save the trail pointer"
        ])
    ;
        AddTrailOps = no,
        MaybeTrailSlots = no,
        SaveTrailCode = empty
    ).

:- pred maybe_restore_trail_info(maybe(pair(lval))::in,
    code_tree::out, code_tree::out, code_info::in, code_info::out) is det.

maybe_restore_trail_info(MaybeTrailSlots, CommitCode, RestoreCode, !CI) :-
    (
        MaybeTrailSlots = no,
        CommitCode = empty,
        RestoreCode = empty
    ;
        MaybeTrailSlots = yes(CounterSlot - TrailPtrSlot),
        CommitCode = node([
            reset_ticket(lval(TrailPtrSlot), commit)
                - "discard trail entries and restore trail ptr",
            prune_tickets_to(lval(CounterSlot))
                - ("restore ticket counter (but not high water mark)")
        ]),
        RestoreCode = node([
            reset_ticket(lval(TrailPtrSlot), undo)
                - "apply trail entries and restore trail ptr",
            discard_ticket
                - "restore ticket counter and high water mark"
        ]),
        release_temp_slot(CounterSlot, !CI),
        release_temp_slot(TrailPtrSlot, !CI)
    ).

%---------------------------------------------------------------------------%

:- pred clone_resume_point(resume_point_info::in,
    resume_point_info::out, code_info::in, code_info::out) is det.

clone_resume_point(ResumePoint0, ResumePoint, !CI) :-
    (
        ResumePoint0 = orig_only(_, _),
        unexpected(this_file, "cloning orig_only resume point")
    ;
        ResumePoint0 = stack_only(Map1, _),
        get_next_label(Label1, !CI),
        Addr1 = label(Label1),
        ResumePoint = stack_only(Map1, Addr1)
    ;
        ResumePoint0 = stack_and_orig(Map1, _, Map2, _),
        get_next_label(Label1, !CI),
        Addr1 = label(Label1),
        get_next_label(Label2, !CI),
        Addr2 = label(Label2),
        ResumePoint = stack_and_orig(Map1, Addr1, Map2, Addr2)
    ;
        ResumePoint0 = orig_and_stack(Map1, _, Map2, _),
        get_next_label(Label2, !CI),
        Addr2 = label(Label2),
        get_next_label(Label1, !CI),
        Addr1 = label(Label1),
        ResumePoint = stack_and_orig(Map2, Addr2, Map1, Addr1)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule to deal with liveness issues.

    % The principles underlying this submodule of code_info.m are
    % documented in the file compiler/notes/allocation.html.

:- interface.

:- pred get_known_variables(code_info::in, list(prog_var)::out) is det.

:- pred variable_is_forward_live(code_info::in, prog_var::in) is semidet.

:- pred make_vars_forward_dead(set(prog_var)::in,
    code_info::in, code_info::out) is det.

:- pred pickup_zombies(set(prog_var)::out,
    code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- pred add_forward_live_vars(set(prog_var)::in,
    code_info::in, code_info::out) is det.

:- pred rem_forward_live_vars(set(prog_var)::in,
    code_info::in, code_info::out) is det.

    % Make these variables appear magically live.
    % We don't care where they are put.
    %
:- pred make_vars_forward_live(set(prog_var)::in,
    code_info::in, code_info::out) is det.

get_known_variables(CI, VarList) :-
    get_forward_live_vars(CI, ForwardLiveVars),
    ResumeVars = current_resume_point_vars(CI),
    set.union(ForwardLiveVars, ResumeVars, Vars),
    set.to_sorted_list(Vars, VarList).

variable_is_forward_live(CI, Var) :-
    get_forward_live_vars(CI, Liveness),
    set.member(Var, Liveness).

add_forward_live_vars(Births, !CI) :-
    get_forward_live_vars(!.CI, Liveness0),
    set.union(Liveness0, Births, Liveness),
    set_forward_live_vars(Liveness, !CI).

rem_forward_live_vars(Deaths, !CI) :-
    get_forward_live_vars(!.CI, Liveness0),
    set.difference(Liveness0, Deaths, Liveness),
    set_forward_live_vars(Liveness, !CI).

make_vars_forward_live(Vars, !CI) :-
    get_stack_slots(!.CI, StackSlots),
    get_var_locn_info(!.CI, VarLocnInfo0),
    set.to_sorted_list(Vars, VarList),
    make_vars_forward_live_2(VarList, StackSlots, 1,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

:- pred make_vars_forward_live_2(list(prog_var)::in,
    stack_slots::in, int::in, var_locn_info::in, var_locn_info::out)
    is det.

make_vars_forward_live_2([], _, _, !VarLocnInfo).
make_vars_forward_live_2([Var | Vars], StackSlots, N0, !VarLocnInfo) :-
    ( map.search(StackSlots, Var, Slot) ->
        Lval = stack_slot_to_lval(Slot),
        N1 = N0
    ;
        find_unused_reg(!.VarLocnInfo, N0, N1),
        Lval = reg(r, N1)
    ),
    var_locn.set_magic_var_location(Var, Lval, !VarLocnInfo),
    make_vars_forward_live_2(Vars, StackSlots, N1, !VarLocnInfo).

:- pred find_unused_reg(var_locn_info::in, int::in, int::out) is det.

find_unused_reg(VLI, N0, N) :-
    ( var_locn.lval_in_use(VLI, reg(r, N0)) ->
        find_unused_reg(VLI, N0 + 1, N)
    ;
        N = N0
    ).

make_vars_forward_dead(Vars, !CI) :-
    maybe_make_vars_forward_dead(Vars, yes, !CI).

:- pred maybe_make_vars_forward_dead(set(prog_var)::in, bool::in,
    code_info::in, code_info::out) is det.

maybe_make_vars_forward_dead(Vars0, FirstTime, !CI) :-
    ResumeVars = current_resume_point_vars(!.CI),
    set.intersect(Vars0, ResumeVars, FlushVars),
    get_zombies(!.CI, Zombies0),
    set.union(Zombies0, FlushVars, Zombies),
    set_zombies(Zombies, !CI),
    set.difference(Vars0, Zombies, Vars),
    set.to_sorted_list(Vars, VarList),
    maybe_make_vars_forward_dead_2(VarList, FirstTime, !CI).

:- pred maybe_make_vars_forward_dead_2(list(prog_var)::in, bool::in,
    code_info::in, code_info::out) is det.

maybe_make_vars_forward_dead_2([], _, !CI).
maybe_make_vars_forward_dead_2([V | Vs], FirstTime, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.var_becomes_dead(V, FirstTime, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI),
    maybe_make_vars_forward_dead_2(Vs, FirstTime, !CI).

pickup_zombies(Zombies, !CI) :-
    get_zombies(!.CI, Zombies),
    set_zombies(set.init, !CI).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for handling the saving and restoration
    % of trail tickets, heap pointers, stack pointers etc.

:- interface.

:- pred save_hp(code_tree::out, lval::out,
    code_info::in, code_info::out) is det.

:- pred restore_hp(lval::in, code_tree::out) is det.

:- pred release_hp(lval::in, code_info::in, code_info::out) is det.

:- pred restore_and_release_hp(lval::in, code_tree::out,
    code_info::in, code_info::out) is det.

:- pred maybe_save_hp(bool::in, code_tree::out, maybe(lval)::out,
    code_info::in, code_info::out) is det.

:- pred maybe_restore_hp(maybe(lval)::in, code_tree::out) is det.

:- pred maybe_release_hp(maybe(lval)::in,
    code_info::in, code_info::out) is det.

:- pred maybe_restore_and_release_hp(maybe(lval)::in,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred save_ticket(code_tree::out, lval::out,
    code_info::in, code_info::out) is det.

:- pred reset_ticket(lval::in, reset_trail_reason::in, code_tree::out) is det.

:- pred release_ticket(lval::in, code_info::in, code_info::out) is det.

:- pred reset_and_prune_ticket(lval::in, reset_trail_reason::in,
    code_tree::out) is det.

:- pred reset_prune_and_release_ticket(lval::in, reset_trail_reason::in,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred reset_and_discard_ticket(lval::in, reset_trail_reason::in,
    code_tree::out) is det.

:- pred reset_discard_and_release_ticket(lval::in, reset_trail_reason::in,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred discard_and_release_ticket(lval::in, code_tree::out,
    code_info::in, code_info::out) is det.

:- pred maybe_save_ticket(bool::in, code_tree::out,
    maybe(lval)::out, code_info::in, code_info::out) is det.

:- pred maybe_reset_ticket(maybe(lval)::in, reset_trail_reason::in,
    code_tree::out) is det.

:- pred maybe_release_ticket(maybe(lval)::in,
    code_info::in, code_info::out) is det.

:- pred maybe_reset_and_prune_ticket(maybe(lval)::in,
    reset_trail_reason::in, code_tree::out) is det.

:- pred maybe_reset_prune_and_release_ticket(maybe(lval)::in,
    reset_trail_reason::in, code_tree::out, code_info::in, code_info::out)
    is det.

:- pred maybe_reset_and_discard_ticket(maybe(lval)::in,
    reset_trail_reason::in, code_tree::out) is det.

:- pred maybe_reset_discard_and_release_ticket(maybe(lval)::in,
    reset_trail_reason::in, code_tree::out, code_info::in, code_info::out)
    is det.

:- pred maybe_discard_and_release_ticket(maybe(lval)::in, code_tree::out,
    code_info::in, code_info::out) is det.

    % Tests if we should add trail ops to the code we generate for the goal
    % with the given goalinfo. This will be 'no' unless we are compiling
    % in trailing grade. It may also be 'no' in trailing grades if we are
    % optimizing trail usage and trail usage analysis tells us that it is safe
    % to omit the trail ops.
    %
:- func should_add_trail_ops(code_info, hlds_goal_info) = add_trail_ops.

%---------------------------------------------------------------------------%

:- implementation.

save_hp(Code, HpSlot, !CI) :-
    acquire_temp_slot(lval(hp), HpSlot, !CI),
    Code = node([
        mark_hp(HpSlot) - "Save heap pointer"
    ]).

restore_hp(HpSlot, Code) :-
    Code = node([
        restore_hp(lval(HpSlot)) - "Restore heap pointer"
    ]).

release_hp(HpSlot, !CI) :-
    release_temp_slot(HpSlot, !CI).

restore_and_release_hp(HpSlot, Code, !CI) :-
    restore_hp(HpSlot, Code),
    release_hp(HpSlot, !CI).

%---------------------------------------------------------------------------%

maybe_save_hp(Maybe, Code, MaybeHpSlot, !CI) :-
    (
        Maybe = yes,
        save_hp(Code, HpSlot, !CI),
        MaybeHpSlot = yes(HpSlot)
    ;
        Maybe = no,
        Code = empty,
        MaybeHpSlot = no
    ).

maybe_restore_hp(MaybeHpSlot, Code) :-
    (
        MaybeHpSlot = yes(HpSlot),
        restore_hp(HpSlot, Code)
    ;
        MaybeHpSlot = no,
        Code = empty
    ).

maybe_release_hp(MaybeHpSlot, !CI) :-
    (
        MaybeHpSlot = yes(HpSlot),
        release_hp(HpSlot, !CI)
    ;
        MaybeHpSlot = no
    ).

maybe_restore_and_release_hp(MaybeHpSlot, Code, !CI) :-
    (
        MaybeHpSlot = yes(HpSlot),
        restore_and_release_hp(HpSlot, Code, !CI)
    ;
        MaybeHpSlot = no,
        Code = empty
    ).

%---------------------------------------------------------------------------%

save_ticket(Code, TicketSlot, !CI) :-
    acquire_temp_slot(ticket, TicketSlot, !CI),
    Code = node([
        store_ticket(TicketSlot) - "Save trail state"
    ]).

reset_ticket(TicketSlot, Reason, Code) :-
    Code = node([
        reset_ticket(lval(TicketSlot), Reason) - "Reset trail"
    ]).

release_ticket(TicketSlot, !CI) :-
    release_temp_slot(TicketSlot, !CI).

reset_and_prune_ticket(TicketSlot, Reason, Code) :-
    Code = node([
        reset_ticket(lval(TicketSlot), Reason) - "Restore trail",
        prune_ticket - "Prune ticket stack"
    ]).

reset_prune_and_release_ticket(TicketSlot, Reason, Code, !CI) :-
    Code = node([
        reset_ticket(lval(TicketSlot), Reason) - "Release trail",
        prune_ticket - "Prune ticket stack"
    ]),
    release_temp_slot(TicketSlot, !CI).

reset_and_discard_ticket(TicketSlot, Reason, Code) :-
    Code = node([
        reset_ticket(lval(TicketSlot), Reason) - "Restore trail",
        discard_ticket - "Pop ticket stack"
    ]).

reset_discard_and_release_ticket(TicketSlot, Reason, Code, !CI) :-
    Code = node([
        reset_ticket(lval(TicketSlot), Reason) - "Release trail",
        discard_ticket - "Pop ticket stack"
    ]),
    release_temp_slot(TicketSlot, !CI).

discard_and_release_ticket(TicketSlot, Code, !CI) :-
    Code = node([
        discard_ticket - "Pop ticket stack"
    ]),
    release_temp_slot(TicketSlot, !CI).

%---------------------------------------------------------------------------%

maybe_save_ticket(Maybe, Code, MaybeTicketSlot, !CI) :-
    (
        Maybe = yes,
        save_ticket(Code, TicketSlot, !CI),
        MaybeTicketSlot = yes(TicketSlot)
    ;
        Maybe = no,
        Code = empty,
        MaybeTicketSlot = no
    ).

maybe_reset_ticket(MaybeTicketSlot, Reason, Code) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_ticket(TicketSlot, Reason, Code)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_release_ticket(MaybeTicketSlot, !CI) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        release_ticket(TicketSlot, !CI)
    ;
        MaybeTicketSlot = no
    ).

maybe_reset_and_prune_ticket(MaybeTicketSlot, Reason, Code) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_and_prune_ticket(TicketSlot, Reason, Code)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_reset_prune_and_release_ticket(MaybeTicketSlot, Reason,
        Code, !CI) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_prune_and_release_ticket(TicketSlot, Reason,
            Code, !CI)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_reset_and_discard_ticket(MaybeTicketSlot, Reason, Code) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_and_discard_ticket(TicketSlot, Reason, Code)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_reset_discard_and_release_ticket(MaybeTicketSlot, Reason,
        Code, !CI) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        reset_discard_and_release_ticket(TicketSlot, Reason,
            Code, !CI)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

maybe_discard_and_release_ticket(MaybeTicketSlot, Code, !CI) :-
    (
        MaybeTicketSlot = yes(TicketSlot),
        discard_and_release_ticket(TicketSlot, Code, !CI)
    ;
        MaybeTicketSlot = no,
        Code = empty
    ).

should_add_trail_ops(CodeInfo, GoalInfo) = AddTrailOps :-
    get_emit_trail_ops(CodeInfo, EmitTrailOps),
    (
        EmitTrailOps = no,
        AddTrailOps = no
    ;
        EmitTrailOps = yes,
        get_opt_trail_ops(CodeInfo, OptTrailOps),
        (
            OptTrailOps = no,
            AddTrailOps = yes
        ;
            OptTrailOps = yes,
            AddTrailOps = goal_may_modify_trail(GoalInfo)
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule to deal with var_locn.

    % Most of these procedures just forward to the var_locn module.
    % See var_locn for documentation.

:- interface.

:- pred variable_locations(code_info::in,
    map(prog_var, set(lval))::out) is det.

:- pred set_var_location(prog_var::in, lval::in,
    code_info::in, code_info::out) is det.

:- pred assign_var_to_var(prog_var::in, prog_var::in,
    code_info::in, code_info::out) is det.

:- pred assign_lval_to_var(prog_var::in, lval::in, code_tree::out,
    code_info::in, code_info::out) is det.

:- pred assign_const_to_var(prog_var::in, rval::in,
    code_info::in, code_info::out) is det.

:- pred assign_expr_to_var(prog_var::in, rval::in, code_tree::out,
    code_info::in, code_info::out) is det.

    % assign_cell_to_var(Var, ReserveWordAtStart, Ptag, Vector,
    %   MaybeSize, TypeMsg, Where, Code, !CI).
    %
:- pred assign_cell_to_var(prog_var::in, bool::in, tag::in,
    list(maybe(rval))::in, maybe(term_size_value)::in, string::in,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred place_var(prog_var::in, lval::in, code_tree::out,
    code_info::in, code_info::out) is det.

:- pred produce_variable(prog_var::in, code_tree::out, rval::out,
    code_info::in, code_info::out) is det.

:- pred produce_variable_in_reg(prog_var::in, code_tree::out,
    lval::out, code_info::in, code_info::out) is det.

:- pred produce_variable_in_reg_or_stack(prog_var::in,
    code_tree::out, lval::out, code_info::in, code_info::out) is det.

:- pred materialize_vars_in_lval(lval::in, lval::out,
    code_tree::out, code_info::in, code_info::out) is det.

:- pred acquire_reg_for_var(prog_var::in, lval::out,
    code_info::in, code_info::out) is det.

:- pred acquire_reg_not_in_storemap(abs_store_map::in, lval::out,
    code_info::in, code_info::out) is det.

:- pred acquire_reg(reg_type::in, lval::out,
    code_info::in, code_info::out) is det.

:- pred release_reg(lval::in, code_info::in, code_info::out) is det.

:- pred reserve_r1(code_tree::out, code_info::in, code_info::out) is det.

:- pred clear_r1(code_tree::out, code_info::in, code_info::out) is det.

:- type call_direction ---> caller ; callee.

    % Move variables to where they need to be at the time of the call:
    %
    % - The variables that need to be saved across the call (either because
    %   they are forward live after the call or because they are protected
    %   by an enclosing resumption point) will be saved on the stack.
    %   Note that if the call cannot succeed and the trace level is none,
    %   then no variables need to be saved across the call. (If the call
    %   cannot succeed but the trace level is not none, then we still
    %   save the usual variables on the stack to make them available
    %   for up-level printing in the debugger.)
    %
    % - The input arguments will be moved to their registers.
    %
:- pred setup_call(hlds_goal_info::in, assoc_list(prog_var, arg_info)::in,
    set(lval)::out, code_tree::out, code_info::in, code_info::out) is det.

    % Move the output arguments of the current procedure to where
    % they need to be at return.
    %
:- pred setup_return(assoc_list(prog_var, arg_info)::in,
    set(lval)::out, code_tree::out, code_info::in, code_info::out) is det.

:- pred lock_regs(int::in, assoc_list(prog_var, lval)::in,
    code_info::in, code_info::out) is det.

:- pred unlock_regs(code_info::in, code_info::out) is det.

    % Record the fact that all the registers have been clobbered (as by a
    % call). If the bool argument is true, then the call cannot return, and
    % thus it is OK for this action to delete the last record of the state
    % of a variable.
    %
:- pred clear_all_registers(bool::in, code_info::in, code_info::out) is det.

:- pred clobber_regs(list(lval)::in, code_info::in, code_info::out) is det.

:- pred save_variables(set(prog_var)::in, set(lval)::out, code_tree::out,
    code_info::in, code_info::out) is det.

:- pred save_variables_on_stack(list(prog_var)::in, code_tree::out,
    code_info::in, code_info::out) is det.

:- pred max_reg_in_use(code_info::in, int::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

variable_locations(CI, Lvals) :-
    get_var_locn_info(CI, VarLocnInfo),
    var_locn.get_var_locations(VarLocnInfo, Lvals).

:- func rval_map_to_lval_map(prog_var, set(rval)) = set(lval).

rval_map_to_lval_map(_Var, Rvals) =
    set.filter_map(rval_is_lval, Rvals).

:- func rval_is_lval(rval) = lval is semidet.

rval_is_lval(lval(Lval)) = Lval.

set_var_location(Var, Lval, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.check_and_set_magic_var_location(Var, Lval,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

assign_var_to_var(Var, AssignedVar, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.assign_var_to_var(Var, AssignedVar, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

assign_lval_to_var(Var, Lval, Code, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    get_static_cell_info(!.CI, StaticCellInfo),
    get_module_info(!.CI, ModuleInfo),
    var_locn.assign_lval_to_var(ModuleInfo, Var, Lval, StaticCellInfo, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

assign_const_to_var(Var, ConstRval, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.assign_const_to_var(Var, ConstRval, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

assign_expr_to_var(Var, Rval, Code, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    (
        code_util.lvals_in_rval(Rval, Lvals),
        Lvals = []
    ->
        var_locn.assign_expr_to_var(Var, Rval, Code,
        VarLocnInfo0, VarLocnInfo)
    ;
        unexpected(this_file, "assign_expr_to_var: non-var lvals")
    ),
    set_var_locn_info(VarLocnInfo, !CI).

assign_cell_to_var(Var, ReserveWordAtStart, Ptag, Vector, MaybeSize,
        TypeMsg, Code, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    get_static_cell_info(!.CI, StaticCellInfo0),
    get_module_info(!.CI, ModuleInfo),
    var_locn.assign_cell_to_var(ModuleInfo, Var, ReserveWordAtStart, Ptag,
        Vector, MaybeSize, TypeMsg, Code, StaticCellInfo0, StaticCellInfo,
        VarLocnInfo0, VarLocnInfo),
    set_static_cell_info(StaticCellInfo, !CI),
    set_var_locn_info(VarLocnInfo, !CI).

place_var(Var, Lval, Code, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    get_module_info(!.CI, ModuleInfo),
    var_locn.place_var(ModuleInfo, Var, Lval, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

:- pred pick_and_place_vars(assoc_list(prog_var, set(lval))::in,
    set(lval)::out, code_tree::out, code_info::in, code_info::out) is det.

pick_and_place_vars(VarLocSets, LiveLocs, Code, !CI) :-
    pick_var_places(VarLocSets, VarLocs),
    assoc_list.values(VarLocs, Locs),
    set.list_to_set(Locs, LiveLocs),
    place_vars(VarLocs, Code, !CI).

:- pred pick_var_places(assoc_list(prog_var, set(lval))::in,
    assoc_list(prog_var, lval)::out) is det.

pick_var_places([], []).
pick_var_places([Var - LvalSet | VarLvalSets], VarLvals) :-
    pick_var_places(VarLvalSets, VarLvals0),
    (
        set.to_sorted_list(LvalSet, LvalList),
        LvalList = [Lval | _]
    ->
        VarLvals = [Var - Lval | VarLvals0]
    ;
        VarLvals = VarLvals0
    ).

:- pred place_vars(assoc_list(prog_var, lval)::in,
    code_tree::out, code_info::in, code_info::out) is det.

place_vars(VarLocs, Code, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    get_module_info(!.CI, ModuleInfo),
    var_locn.place_vars(ModuleInfo, VarLocs, Code, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

produce_variable(Var, Code, Rval, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    get_module_info(!.CI, ModuleInfo),
    var_locn.produce_var(ModuleInfo, Var, Rval, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

produce_variable_in_reg(Var, Code, Lval, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    get_module_info(!.CI, ModuleInfo),
    var_locn.produce_var_in_reg(ModuleInfo, Var, Lval, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

produce_variable_in_reg_or_stack(Var, Code, Lval, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    get_module_info(!.CI, ModuleInfo),
    var_locn.produce_var_in_reg_or_stack(ModuleInfo, Var, Lval, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

materialize_vars_in_lval(Lval0, Lval, Code, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    get_module_info(!.CI, ModuleInfo),
    var_locn.materialize_vars_in_lval(ModuleInfo, Lval0, Lval, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

acquire_reg_for_var(Var, Lval, !CI) :-
    get_follow_var_map(!.CI, FollowVarsMap),
    get_next_non_reserved(!.CI, NextNonReserved),
    get_var_locn_info(!.CI, VarLocnInfo0),
    (
        map.search(FollowVarsMap, Var, PrefLocn),
        PrefLocn = abs_reg(PrefRegNum),
        PrefRegNum >= 1
    ->
        var_locn.acquire_reg_prefer_given(PrefRegNum, Lval,
        VarLocnInfo0, VarLocnInfo)
    ;
        % XXX We should only get a register if the map.search
        % succeeded; otherwise we should put the var in its stack slot.
        var_locn.acquire_reg_start_at_given(NextNonReserved, Lval,
            VarLocnInfo0, VarLocnInfo)
    ),
    set_var_locn_info(VarLocnInfo, !CI).

acquire_reg_not_in_storemap(StoreMap, Lval, !CI) :-
    map.foldl(record_highest_used_reg, StoreMap, 0, HighestUsedRegNum),
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.acquire_reg_start_at_given(HighestUsedRegNum + 1, Lval,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

:- pred record_highest_used_reg(prog_var::in, abs_locn::in, int::in, int::out)
    is det.

record_highest_used_reg(_, AbsLocn, !HighestUsedRegNum) :-
    (
        AbsLocn = any_reg
    ;
        AbsLocn = abs_reg(N),
        ( N > !.HighestUsedRegNum ->
            !:HighestUsedRegNum = N
        ;
            true
        )
    ;
        AbsLocn = abs_stackvar(_)
    ;
        AbsLocn = abs_framevar(_)
    ).

acquire_reg(Type, Lval, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    expect(unify(Type, r), this_file, "acquire_reg: unknown reg type"),
    var_locn.acquire_reg(Lval, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

release_reg(Lval, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.release_reg(Lval, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

reserve_r1(Code, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    get_module_info(!.CI, ModuleInfo),
    var_locn.clear_r1(ModuleInfo, Code, VarLocnInfo0, VarLocnInfo1),
    var_locn.acquire_reg_require_given(reg(r, 1), VarLocnInfo1, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

clear_r1(empty, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.release_reg(reg(r, 1), VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

%---------------------------------------------------------------------------%

setup_return(VarArgInfos, OutLocs, Code, !CI) :-
    setup_call_args(VarArgInfos, callee, OutLocs, Code, !CI).

setup_call(GoalInfo, ArgInfos, LiveLocs, Code, !CI) :-
    partition_args(ArgInfos, InArgInfos, OutArgInfos, _UnusedArgInfos),
    assoc_list.keys(OutArgInfos, OutVars),
    set.list_to_set(OutVars, OutVarSet),
    goal_info_get_determinism(GoalInfo, Detism),
    get_opt_no_return_calls(!.CI, OptNoReturnCalls),
    get_module_info(!.CI, ModuleInfo),
    (
        Detism = erroneous,
        OptNoReturnCalls = yes
    ->
        RealStackVarLocs = [],
        DummyStackVarLocs = []
    ;
        compute_forward_live_var_saves(!.CI, OutVarSet, ForwardVarLocs),
        goal_info_get_code_model(GoalInfo, CodeModel),
        ( CodeModel = model_non ->
            % Save variables protected by the nearest resumption point on the
            % stack.
            % XXX This should be unnecessary; with the current setup, the code
            % that established the resume point should have saved those
            % variables on the stack already. However, later we should arrange
            % things so that this saving of the resume vars on the stack
            % is delayed until the first call after the setup of the
            % resume point.
            compute_resume_var_stack_locs(!.CI, ResumeVarLocs),
            list.append(ResumeVarLocs, ForwardVarLocs, StackVarLocs)
        ;
            StackVarLocs = ForwardVarLocs
        ),
        VarTypes = get_var_types(!.CI),
        list.filter(valid_stack_slot(ModuleInfo, VarTypes), StackVarLocs,
            RealStackVarLocs, DummyStackVarLocs)
    ),
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_arg_info_to_lval(InArgInfos, InArgLocs),
    list.append(RealStackVarLocs, InArgLocs, AllRealLocs),
    var_locn.place_vars(ModuleInfo, DummyStackVarLocs ++ AllRealLocs, Code,
        VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI),
    assoc_list.values(AllRealLocs, LiveLocList),
    set.list_to_set(LiveLocList, LiveLocs).

:- pred valid_stack_slot(module_info::in, vartypes::in,
    pair(prog_var, lval)::in) is semidet.

valid_stack_slot(ModuleInfo, VarTypes, Var - Lval) :-
    map.lookup(VarTypes, Var, Type),
    ( is_dummy_argument_type(ModuleInfo, Type) ->
        fail
    ;
        (
            ( Lval = stackvar(N)
            ; Lval = framevar(N)
            ),
            N < 0
        ->
            unexpected(this_file,
                "valid_stack_slot: nondummy var in dummy stack slot")
        ;
            true
        )
    ).

:- pred setup_call_args(assoc_list(prog_var, arg_info)::in,
    call_direction::in, set(lval)::out, code_tree::out,
    code_info::in, code_info::out) is det.

setup_call_args(AllArgsInfos, Direction, LiveLocs, Code, !CI) :-
    list.filter(call_arg_in_selected_dir(Direction), AllArgsInfos, ArgsInfos),
    var_arg_info_to_lval(ArgsInfos, ArgsLocns),
    get_module_info(!.CI, ModuleInfo),
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.place_vars(ModuleInfo, ArgsLocns, Code,
        VarLocnInfo0, VarLocnInfo1),
    set_var_locn_info(VarLocnInfo1, !CI),
    assoc_list.values(ArgsLocns, LiveLocList),
    set.list_to_set(LiveLocList, LiveLocs),
    assoc_list.keys(ArgsLocns, ArgVars),
    which_variables_are_forward_live(!.CI, ArgVars, set.init, DeadVars),
    make_vars_forward_dead(DeadVars, !CI).

:- pred var_arg_info_to_lval(assoc_list(prog_var, arg_info)::in,
    assoc_list(prog_var, lval)::out) is det.

var_arg_info_to_lval([], []).
var_arg_info_to_lval([Var - ArgInfo | RestInfos], [Var - Lval | RestLvals]) :-
    ArgInfo = arg_info(Loc, _Mode),
    code_util.arg_loc_to_register(Loc, Lval),
    var_arg_info_to_lval(RestInfos, RestLvals).

:- pred which_variables_are_forward_live(code_info::in,
    list(prog_var)::in, set(prog_var)::in, set(prog_var)::out) is det.

which_variables_are_forward_live(_, [], !DeadVars).
which_variables_are_forward_live(CI, [Var | Vars], !DeadVars) :-
    ( variable_is_forward_live(CI, Var) ->
        true
    ;
        set.insert(!.DeadVars, Var, !:DeadVars)
    ),
    which_variables_are_forward_live(CI, Vars, !DeadVars).

:- pred call_arg_in_selected_dir(call_direction::in,
    pair(prog_var, arg_info)::in) is semidet.

call_arg_in_selected_dir(Direction, _ - arg_info(_, Mode)) :-
    (
        Mode = top_in,
        Direction = caller
    ;
        Mode = top_out,
        Direction = callee
    ).

lock_regs(N, Exceptions, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.lock_regs(N, Exceptions, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

unlock_regs(!CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.unlock_regs(VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

clear_all_registers(OkToDeleteAny, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.clobber_all_regs(OkToDeleteAny, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

clobber_regs(Regs, !CI) :-
    get_var_locn_info(!.CI, VarLocnInfo0),
    var_locn.clobber_regs(Regs, VarLocnInfo0, VarLocnInfo),
    set_var_locn_info(VarLocnInfo, !CI).

save_variables(OutArgs, SavedLocs, Code, !CI) :-
    compute_forward_live_var_saves(!.CI, OutArgs, VarLocs),
    assoc_list.values(VarLocs, SavedLocList),
    set.list_to_set(SavedLocList, SavedLocs),
    place_vars(VarLocs, Code, !CI).

save_variables_on_stack(Vars, Code, !CI) :-
    list.map(associate_stack_slot(!.CI), Vars, VarLocs),
    place_vars(VarLocs, Code, !CI).

:- pred compute_forward_live_var_saves(code_info::in,
    set(prog_var)::in, assoc_list(prog_var, lval)::out) is det.

compute_forward_live_var_saves(CI, OutArgs, VarLocs) :-
    get_known_variables(CI, Variables0),
    set.list_to_set(Variables0, Vars0),
    TypeInfoLiveness = body_typeinfo_liveness(CI),
    get_proc_info(CI, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    proc_info_get_rtti_varmaps(ProcInfo, RttiVarMaps),
    maybe_complete_with_typeinfo_vars(Vars0, TypeInfoLiveness, VarTypes,
        RttiVarMaps, Vars1),
    set.difference(Vars1, OutArgs, Vars),
    set.to_sorted_list(Vars, Variables),
    list.map(associate_stack_slot(CI), Variables, VarLocs).

:- pred associate_stack_slot(code_info::in, prog_var::in,
    pair(prog_var, lval)::out) is det.

associate_stack_slot(CI, Var, Var - Slot) :-
    get_variable_slot(CI, Var, Slot).

max_reg_in_use(CI, Max) :-
    get_var_locn_info(CI, VarLocnInfo),
    var_locn.max_reg_in_use(VarLocnInfo, Max).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for dealing with the recording of variable liveness
    % information around calls.
    %
    % Value numbering needs to know what locations are live before calls;
    % the garbage collector and the debugger need to know what locations
    % are live containing what types of values after calls.

:- interface.

:- pred generate_call_vn_livevals(code_info::in, list(arg_loc)::in,
    set(prog_var)::in, set(lval)::out) is det.

:- pred generate_return_live_lvalues(code_info::in,
    assoc_list(prog_var, arg_loc)::in, instmap::in, bool::in,
    list(liveinfo)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

generate_call_vn_livevals(CI, InputArgLocs, OutputArgs, LiveVals) :-
    generate_call_stack_vn_livevals(CI, OutputArgs, StackLiveVals),
    generate_input_var_vn(InputArgLocs, StackLiveVals, LiveVals).

:- pred generate_call_stack_vn_livevals(code_info::in,
    set(prog_var)::in, set(lval)::out) is det.

generate_call_stack_vn_livevals(CI, OutputArgs, LiveVals) :-
    get_known_variables(CI, KnownVarList0),
    get_module_info(CI, ModuleInfo),
    VarTypes = get_var_types(CI),
    list.filter(var_is_of_dummy_type(ModuleInfo, VarTypes), KnownVarList0,
        _, KnownVarList),
    set.list_to_set(KnownVarList, KnownVars),
    set.difference(KnownVars, OutputArgs, LiveVars),
    set.to_sorted_list(LiveVars, LiveVarList),
    generate_stack_var_vn(CI, LiveVarList, set.init, LiveVals1),
    get_active_temps_data(CI, Temps),
    generate_call_temp_vn(Temps, LiveVals1, LiveVals).

:- pred generate_stack_var_vn(code_info::in, list(prog_var)::in,
    set(lval)::in, set(lval)::out) is det.

generate_stack_var_vn(_, [], !Vals).
generate_stack_var_vn(CI, [V | Vs], !Vals) :-
    get_variable_slot(CI, V, Lval),
    set.insert(!.Vals, Lval, !:Vals),
    generate_stack_var_vn(CI, Vs, !Vals).

:- pred generate_call_temp_vn(assoc_list(lval, slot_contents)::in,
    set(lval)::in, set(lval)::out) is det.

generate_call_temp_vn([], !Vals).
generate_call_temp_vn([Lval - _ | Temps], !Vals) :-
    set.insert(!.Vals, Lval, !:Vals),
    generate_call_temp_vn(Temps, !Vals).

:- pred generate_input_var_vn(list(arg_loc)::in,
    set(lval)::in, set(lval)::out) is det.

generate_input_var_vn([], !Vals).
generate_input_var_vn([InputArgLoc | InputArgLocs], !Vals) :-
    code_util.arg_loc_to_register(InputArgLoc, Lval),
    set.insert(!.Vals, Lval, !:Vals),
    generate_input_var_vn(InputArgLocs, !Vals).

%---------------------------------------------------------------------------%

generate_return_live_lvalues(CI, OutputArgLocs, ReturnInstMap,
        OkToDeleteAny, LiveLvalues) :-
    variable_locations(CI, VarLocs),
    get_known_variables(CI, Vars0),
    get_module_info(CI, ModuleInfo),
    VarTypes = get_var_types(CI),
    list.filter(var_is_of_dummy_type(ModuleInfo, VarTypes), Vars0, _, Vars),
    get_active_temps_data(CI, Temps),
    get_proc_info(CI, ProcInfo),
    get_globals(CI, Globals),
    continuation_info.generate_return_live_lvalues(OutputArgLocs,
        ReturnInstMap, Vars, VarLocs, Temps, ProcInfo, ModuleInfo,
        Globals, OkToDeleteAny, LiveLvalues).

:- pred generate_resume_layout(label::in, resume_map::in,
    code_info::in, code_info::out) is det.

generate_resume_layout(Label, ResumeMap, !CI) :-
    get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, agc_stack_layout, AgcStackLayout),
    (
        AgcStackLayout = yes,
        get_active_temps_data(!.CI, Temps),
        get_instmap(!.CI, InstMap),
        get_proc_info(!.CI, ProcInfo),
        get_module_info(!.CI, ModuleInfo),
        continuation_info.generate_resume_layout(ResumeMap, Temps, InstMap,
            ProcInfo, ModuleInfo, Layout),
        add_resume_layout_for_label(Label, Layout, !CI)
    ;
        AgcStackLayout = no
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Submodule for managing stack slots.

    % Det stack frames are organized as follows.
    %
    %       ... unused ...
    %   sp ---> <first unused slot>
    %       <space for local var 1>
    %       ... local vars ...
    %       <space for local var n>
    %       <space for temporary 1>
    %       ... temporaries ...
    %       <space for temporary n>
    %       <space for saved succip, if needed>
    %
    % The stack pointer points to the first free location at the
    % top of the stack.
    %
    % `succip_is_used' determines whether we need a slot to
    % hold the succip.
    %
    % Nondet stack frames also have the local variables above the
    % temporaries, but contain several fixed slots on top, and the
    % saved succip is stored in one of these.
    %
    % For both kinds of stack frames, the slots holding variables
    % are allocated during the live_vars pass, while the slots holding
    % temporaries are acquired (and if possible, released) on demand
    % during code generation.

:- interface.

    % Returns the total stackslot count, but not including space for
    % succip. This total can change in the future if this call is
    % followed by further allocations of temp slots.
    %
:- pred get_total_stackslot_count(code_info::in, int::out) is det.

    % Acquire a stack slot for storing a temporary. The slot_contents
    % description is for accurate gc.
    %
:- pred acquire_temp_slot(slot_contents::in, lval::out,
    code_info::in, code_info::out) is det.

    % Release a stack slot acquired earlier for a temporary value.
    %
:- pred release_temp_slot(lval::in, code_info::in, code_info::out) is det.

    % Return the lval of the stack slot in which the given variable
    % is stored. Aborts if the variable does not have a stack slot
    % an assigned to it.
    %
:- pred get_variable_slot(code_info::in, prog_var::in, lval::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

acquire_temp_slot(Item, StackVar, !CI) :-
    get_temps_in_use(!.CI, TempsInUse0),
    IsTempUsable = (pred(TempContent::in, Lval::out) is semidet :-
        TempContent = Lval - ContentType,
        ContentType = Item,
        \+ set.member(Lval, TempsInUse0)
    ),
    get_temp_content_map(!.CI, TempContentMap0),
    map.to_assoc_list(TempContentMap0, TempContentList),
    list.filter_map(IsTempUsable, TempContentList, UsableLvals),
    (
        UsableLvals = [UsableLval | _],
        StackVar = UsableLval
    ;
        UsableLvals = [],
        get_var_slot_count(!.CI, VarSlots),
        get_max_temp_slot_count(!.CI, TempSlots0),
        TempSlots = TempSlots0 + 1,
        Slot = VarSlots + TempSlots,
        stack_variable(!.CI, Slot, StackVar),
        set_max_temp_slot_count(TempSlots, !CI),
        map.det_insert(TempContentMap0, StackVar, Item, TempContentMap),
        set_temp_content_map(TempContentMap, !CI)
    ),
    set.insert(TempsInUse0, StackVar, TempsInUse),
    set_temps_in_use(TempsInUse, !CI).

release_temp_slot(StackVar, !CI) :-
    get_temps_in_use(!.CI, TempsInUse0),
    set.delete(TempsInUse0, StackVar, TempsInUse),
    set_temps_in_use(TempsInUse, !CI).

%---------------------------------------------------------------------------%

get_variable_slot(CI, Var, Slot) :-
    get_stack_slots(CI, StackSlots),
    ( map.search(StackSlots, Var, SlotLocn) ->
        Slot = stack_slot_to_lval(SlotLocn)
    ;
        Name = variable_to_string(CI, Var),
        term.var_to_int(Var, Num),
        string.int_to_string(Num, NumStr),
        string.append_list(["get_variable_slot: variable `",
            Name, "' (", NumStr, ") not found"], Str),
        unexpected(this_file, Str)
    ).

get_total_stackslot_count(CI, NumSlots) :-
    get_var_slot_count(CI, SlotsForVars),
    get_max_temp_slot_count(CI, SlotsForTemps),
    NumSlots = SlotsForVars + SlotsForTemps.

:- pred max_var_slot(stack_slots::in, int::out) is det.

max_var_slot(StackSlots, SlotCount) :-
    map.values(StackSlots, StackSlotList),
    max_var_slot_2(StackSlotList, 0, SlotCount).

:- pred max_var_slot_2(list(stack_slot)::in, int::in, int::out) is det.

max_var_slot_2([], !Max).
max_var_slot_2([L | Ls], !Max) :-
    (
        L = det_slot(N),
        int.max(N, !Max)
    ;
        L = nondet_slot(N),
        int.max(N, !Max)
    ),
    max_var_slot_2(Ls, !Max).

:- pred stack_variable(code_info::in, int::in, lval::out) is det.

stack_variable(CI, Num, Lval) :-
    ( get_proc_model(CI) = model_non ->
        Lval = framevar(Num)
    ;
        Lval = stackvar(Num)
    ).

:- pred stack_variable_reference(code_info::in, int::in, rval::out) is det.

stack_variable_reference(CI, Num, mem_addr(Ref)) :-
    ( get_proc_model(CI) = model_non ->
        Ref = framevar_ref(const(int_const(Num)))
    ;
        Ref = stackvar_ref(const(int_const(Num)))
    ).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "code_info.m".

%---------------------------------------------------------------------------%
