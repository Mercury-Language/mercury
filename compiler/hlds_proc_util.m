%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2023 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_proc_util.m.
%
% This module defines some utility operations on procedures.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_proc_util.
:- interface.

:- import_module check_hlds.
:- import_module check_hlds.mode_constraint_robdd.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.

:- import_module list.
:- import_module map.

%---------------------------------------------------------------------------%
%
% Nontrivial access predicates on proc_infos.
%

:- pred proc_info_head_modes_constraint(proc_info::in,
    mode_constraint::out) is det.

:- pred proc_info_declared_argmodes(proc_info::in, list(mer_mode)::out) is det.

    % See also proc_info_interface_code_model in code_model.m.
    %
:- pred proc_info_interface_determinism(proc_info::in, determinism::out)
    is det.

:- type can_proc_succeed
    --->    proc_can_maybe_succeed
    ;       proc_cannot_succeed.

    % Return whether the procedure can ever succeed according to
    % its declared determinism. If it has no declared determinism,
    % return proc_can_maybe_succeed.
    %
:- pred proc_info_never_succeeds(proc_info::in, can_proc_succeed::out) is det.

:- pred proc_info_arglives(module_info::in, proc_info::in,
    list(is_live)::out) is det.
:- pred proc_info_arg_info(proc_info::in, list(arg_info)::out) is det.
:- pred proc_info_get_initial_instmap(module_info::in, proc_info::in,
    instmap::out) is det.

    % Given a procedure, return a list of all its headvars which are
    % (further) instantiated by the procedure.
    %
:- pred proc_info_instantiated_head_vars(module_info::in, proc_info::in,
    list(prog_var)::out) is det.

    % Given a procedure, return a list of all its headvars which are
    % not (further) instantiated by the procedure.
    %
:- pred proc_info_uninstantiated_head_vars(module_info::in, proc_info::in,
    list(prog_var)::out) is det.

%---------------------------------------------------------------------------%
%
% Updating proc_infos.
%

    % Create a new variable of the given type to the procedure.
    %
:- pred proc_info_create_var_from_type(string::in, mer_type::in,
    is_dummy_type::in, prog_var::out, proc_info::in, proc_info::out) is det.

    % Create a new variable for each element of the list of types.
    %
:- pred proc_info_create_vars_from_types(module_info::in, list(mer_type)::in,
    list(prog_var)::out, proc_info::in, proc_info::out) is det.

    % Make sure that all headvars are named. This can be useful e.g.
    % because the debugger ignores unnamed variables.
    %
:- pred ensure_all_headvars_are_named(proc_info::in, proc_info::out) is det.

%---------------------------------------------------------------------------%
%
% Tests on proc_infos.
%

    % If the procedure has a input/output pair of io.state arguments,
    % return the positions of those arguments in the argument list.
    % The positions are given as argument numbers, with the first argument
    % in proc_info_get_headvars being position 1, and so on. The first output
    % argument gives the position of the input state, the second the
    % position of the output state.
    %
    % Note that the automatically constructed unify, index and compare
    % procedures for the io:state type are not counted as having io:state
    % args, since they do not fall into the scheme of one input and one
    % output arg. Since they should never be called, this should not matter.
    %
:- pred proc_info_has_io_state_pair(module_info::in, proc_info::in,
    int::out, int::out) is semidet.

:- pred proc_info_has_io_state_pair_from_details(module_info::in,
    var_table::in, list(prog_var)::in, list(mer_mode)::in,
    int::out, int::out) is semidet.

%---------------------------------------------------------------------------%
%
% Operations on proc_ids.
%

    % Given a procedure table and the id of a procedure in that table,
    % return a procedure id to be attached to a clone of that procedure.
    % (The task of creating the clone proc_info and inserting into the
    % procedure table is the task of the caller.)
    %
:- pred clone_proc_id(proc_table::in, proc_id::in, proc_id::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.

:- import_module assoc_list.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

proc_info_head_modes_constraint(ProcInfo, HeadModesConstraint) :-
    proc_info_get_maybe_head_modes_constr(ProcInfo, MaybeHeadModesConstraint),
    (
        MaybeHeadModesConstraint = yes(HeadModesConstraint)
    ;
        MaybeHeadModesConstraint = no,
        unexpected($pred, "no constraint")
    ).

proc_info_declared_argmodes(ProcInfo, ArgModes) :-
    proc_info_get_maybe_declared_argmodes(ProcInfo, MaybeArgModes),
    (
        MaybeArgModes = yes(ArgModes1),
        ArgModes = ArgModes1
    ;
        MaybeArgModes = no,
        proc_info_get_argmodes(ProcInfo, ArgModes)
    ).

proc_info_interface_determinism(ProcInfo, Determinism) :-
    proc_info_get_declared_determinism(ProcInfo, MaybeDeterminism),
    (
        MaybeDeterminism = no,
        proc_info_get_inferred_determinism(ProcInfo, Determinism)
    ;
        MaybeDeterminism = yes(Determinism)
    ).

proc_info_never_succeeds(ProcInfo, CanSucceed) :-
    proc_info_get_declared_determinism(ProcInfo, DeclaredDeterminism),
    (
        DeclaredDeterminism = no,
        CanSucceed = proc_can_maybe_succeed
    ;
        DeclaredDeterminism = yes(Determinism),
        determinism_components(Determinism, _, MaxSoln),
        (
            MaxSoln = at_most_zero,
            CanSucceed = proc_cannot_succeed
        ;
            ( MaxSoln = at_most_one
            ; MaxSoln = at_most_many
            ; MaxSoln = at_most_many_cc
            ),
            CanSucceed = proc_can_maybe_succeed
        )
    ).

proc_info_arglives(ModuleInfo, ProcInfo, ArgLives) :-
    proc_info_get_maybe_arglives(ProcInfo, MaybeArgLives),
    (
        MaybeArgLives = yes(ArgLives0),
        ArgLives = ArgLives0
    ;
        MaybeArgLives = no,
        proc_info_get_argmodes(ProcInfo, Modes),
        get_arg_lives(ModuleInfo, Modes, ArgLives)
    ).

proc_info_arg_info(ProcInfo, ArgInfo) :-
    proc_info_get_maybe_arg_info(ProcInfo, MaybeArgInfo0),
    (
        MaybeArgInfo0 = yes(ArgInfo)
    ;
        MaybeArgInfo0 = no,
        unexpected($pred, "arg_info not set")
    ).

proc_info_get_initial_instmap(ModuleInfo, ProcInfo, InstMap) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    mode_list_get_initial_insts(ModuleInfo, ArgModes, InitialInsts),
    assoc_list.from_corresponding_lists(HeadVars, InitialInsts, InstAL),
    InstMap = instmap_from_assoc_list(InstAL).

%---------------------%

proc_info_instantiated_head_vars(ModuleInfo, ProcInfo, ChangedInstHeadVars) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_var_table(ProcInfo, VarTable),
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarModes),
    IsInstChanged =
        ( pred(VarMode::in, Var::out) is semidet :-
            VarMode = Var - Mode,
            lookup_var_type(VarTable, Var, Type),
            mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
            not inst_matches_binding(ModuleInfo, Type, Inst1, Inst2)
        ),
    list.filter_map(IsInstChanged, HeadVarModes, ChangedInstHeadVars).

proc_info_uninstantiated_head_vars(ModuleInfo, ProcInfo,
        UnchangedInstHeadVars) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_var_table(ProcInfo, VarTable),
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarModes),
    IsInstUnchanged =
        ( pred(VarMode::in, Var::out) is semidet :-
            VarMode = Var - Mode,
            lookup_var_type(VarTable, Var, Type),
            mode_get_insts(ModuleInfo, Mode, Inst1, Inst2),
            inst_matches_binding(ModuleInfo, Type, Inst1, Inst2)
        ),
    list.filter_map(IsInstUnchanged, HeadVarModes, UnchangedInstHeadVars).

%---------------------------------------------------------------------------%

proc_info_create_var_from_type(Name, Type, IsDummy, Var, !ProcInfo) :-
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    Entry = vte(Name, Type, IsDummy),
    add_var_entry(Entry, Var, VarTable0, VarTable),
    proc_info_set_var_table(VarTable, !ProcInfo).

proc_info_create_vars_from_types(ModuleInfo, Types, Vars, !ProcInfo) :-
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    AddVar =
        ( pred(T::in, V::out, VT0::in, VT::out) is det :-
            IsDummy = is_type_a_dummy(ModuleInfo, T),
            Entry = vte("", T, IsDummy),
            add_var_entry(Entry, V, VT0, VT)
        ),
    list.map_foldl(AddVar, Types, Vars, VarTable0, VarTable),
    proc_info_set_var_table(VarTable, !ProcInfo).

%---------------------%

ensure_all_headvars_are_named(!ProcInfo) :-
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    ensure_all_headvars_are_named_loop(HeadVars, 1, VarTable0, VarTable),
    proc_info_set_var_table(VarTable, !ProcInfo).

:- pred ensure_all_headvars_are_named_loop(list(prog_var)::in, int::in,
    var_table::in, var_table::out) is det.

ensure_all_headvars_are_named_loop([], _, !VarTable).
ensure_all_headvars_are_named_loop([Var | Vars], SeqNum, !VarTable) :-
    lookup_var_entry(!.VarTable, Var, Entry0),
    Entry0 = vte(Name0, Type, IsDummy),
    ( if Name0 = "" then
        Name = "HeadVar__" ++ int_to_string(SeqNum),
        Entry = vte(Name, Type, IsDummy),
        update_var_entry(Var, Entry, !VarTable)
    else
        true
    ),
    ensure_all_headvars_are_named_loop(Vars, SeqNum + 1, !VarTable).

%---------------------------------------------------------------------------%

proc_info_has_io_state_pair(ModuleInfo, ProcInfo, InArgNum, OutArgNum) :-
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_var_table(ProcInfo, VarTable),
    proc_info_has_io_state_pair_from_details(ModuleInfo, VarTable,
        HeadVars, ArgModes, InArgNum, OutArgNum).

proc_info_has_io_state_pair_from_details(ModuleInfo, VarTable,
        HeadVars, ArgModes, InArgNum, OutArgNum) :-
    assoc_list.from_corresponding_lists(HeadVars, ArgModes, HeadVarsModes),
    proc_info_has_io_state_pair_2(ModuleInfo, VarTable, 1, HeadVarsModes,
        no, MaybeIn, no, MaybeOut),
    ( if
        MaybeIn = yes(In),
        MaybeOut = yes(Out)
    then
        InArgNum = In,
        OutArgNum = Out
    else
        fail
    ).

:- pred proc_info_has_io_state_pair_2(module_info::in, var_table::in,
    int::in, assoc_list(prog_var, mer_mode)::in,
    maybe(int)::in, maybe(int)::out, maybe(int)::in, maybe(int)::out)
    is semidet.

proc_info_has_io_state_pair_2(_, _, _, [], !MaybeIn, !MaybeOut).
proc_info_has_io_state_pair_2(ModuleInfo, VarTable, ArgNum,
        [Var - Mode | VarModes], !MaybeIn, !MaybeOut) :-
    ( if
        lookup_var_type(VarTable, Var, VarType),
        type_is_io_state(VarType)
    then
        ( if mode_is_fully_input(ModuleInfo, Mode) then
            (
                !.MaybeIn = no,
                !:MaybeIn = yes(ArgNum)
            ;
                !.MaybeIn = yes(_),
                % Procedures with two input arguments of type io.state
                % (e.g. the automatically generated unification or comparison
                % procedure for the io.state type) do not fall into the
                % one input/one output pattern we are looking for.
                fail
            )
        else if mode_is_fully_output(ModuleInfo, Mode) then
            (
                !.MaybeOut = no,
                !:MaybeOut = yes(ArgNum)
            ;
                !.MaybeOut = yes(_),
                % Procedures with two output arguments of type io.state
                % do not fall into the one input/one output pattern we are
                % looking for.
                fail
            )
        else
            fail
        )
    else
        true
    ),
    proc_info_has_io_state_pair_2(ModuleInfo, VarTable, ArgNum + 1,
        VarModes, !MaybeIn, !MaybeOut).

%---------------------------------------------------------------------------%

clone_proc_id(ProcTable, _ProcId, CloneProcId) :-
    find_lowest_unused_proc_id(ProcTable, CloneProcId).

:- pred find_lowest_unused_proc_id(proc_table::in, proc_id::out) is det.

find_lowest_unused_proc_id(ProcTable, CloneProcId) :-
    find_lowest_unused_proc_id_loop(ProcTable, 0, CloneProcId).

:- pred find_lowest_unused_proc_id_loop(proc_table::in, int::in,
    proc_id::out) is det.

find_lowest_unused_proc_id_loop(ProcTable, TrialProcIdInt, CloneProcId) :-
    proc_id_to_int(TrialProcId, TrialProcIdInt),
    ( if map.search(ProcTable, TrialProcId, _) then
        find_lowest_unused_proc_id_loop(ProcTable, TrialProcIdInt + 1,
            CloneProcId)
    else
        CloneProcId = TrialProcId
    ).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_proc_util.
%---------------------------------------------------------------------------%
