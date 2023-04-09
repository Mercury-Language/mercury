%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2007, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: complexity.m.
% Author: zs.
%
% This module performs a program transformation that gathers information about
% the relationship between the sizes of a procedure's input arguments and the
% performance cost of the procedure in terms of memory and time.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.complexity.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

    % read_spec_file(FileName, MaybeNumLinesProcMap, !IO):
    % Try to read in a complexity proc map from FileName. If successful,
    % return the proc map and the number of entries in it. If not, return an
    % error message.
    %
:- pred read_spec_file(string::in,
    maybe_error(pair(int, complexity_proc_map))::out, io::di, io::uo) is det.

    % is_in_complexity_proc_map(ProcMap, ModuleInfo, PredId, ProcId):
    % If PredId/ProcId in ModuleInfo is in ProcMap, return its slot number
    % in the complexity table.
    %
:- func is_in_complexity_proc_map(complexity_proc_map, module_info,
    pred_id, proc_id) = maybe(int).

    % Return the name of the given procedure in the format required by the
    % complexity map file.
    %
:- func complexity_proc_name(module_info, pred_id, proc_id) = string.

    % Transform the given procedure if it is in the complexity map.
    %
:- pred complexity_process_proc_msg(io.text_output_stream::in, int::in,
    complexity_proc_map::in, pred_proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_test.
:- import_module check_hlds.polymorphism_type_info.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.term_norm.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

read_spec_file(FileName, MaybeNumLinesProcMap, !IO) :-
    io.open_input(FileName, ResStream, !IO),
    (
        ResStream = error(Error),
        MaybeNumLinesProcMap = error(io.error_message(Error))
    ;
        ResStream = ok(Stream),
        read_spec_file_lines(Stream, 0, NumLines, MaybeError,
            map.init, ProcMap, !IO),
        (
            MaybeError = yes(Msg),
            MaybeNumLinesProcMap = error(Msg)
        ;
            MaybeError = no,
            MaybeNumLinesProcMap = ok(NumLines - ProcMap)
        )
    ).

:- pred read_spec_file_lines(io.input_stream::in, int::in, int::out,
    maybe(string)::out, map(string, int)::in, map(string, int)::out,
    io::di, io::uo) is det.

read_spec_file_lines(Stream, CurLineNum, NumLines, MaybeError, !ProcMap,
        !IO) :-
    io.read_line(Stream, ResLine, !IO),
    (
        ResLine = eof,
        NumLines = CurLineNum,
        MaybeError = no
    ;
        ResLine = error(Error),
        NumLines = CurLineNum,
        MaybeError = yes(io.error_message(Error))
    ;
        ResLine = ok(Chars0),
        list.filter(unify('\n'), Chars0, _, Chars),
        string.from_char_list(Chars, ProcName),
        ( if map.insert(ProcName, CurLineNum, !ProcMap) then
            read_spec_file_lines(Stream, CurLineNum + 1,
                NumLines, MaybeError, !ProcMap, !IO)
        else
            NumLines = CurLineNum,
            MaybeError = yes("repeated line: " ++ ProcName)
        )
    ).

%-----------------------------------------------------------------------------%

is_in_complexity_proc_map(ProcMap, ModuleInfo, PredId, ProcId) = IsInMap :-
    FullName = complexity_proc_name(ModuleInfo, PredId, ProcId),
    ( if map.search(ProcMap, FullName, ProcNum) then
        IsInMap = yes(ProcNum)
    else
        IsInMap = no
    ).

complexity_proc_name(ModuleInfo, PredId, ProcId) = FullName :-
    module_info_get_name(ModuleInfo, ModuleSymName),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredName = pred_info_name(PredInfo),
    QualifiedName = qualified(ModuleSymName, PredName),
    Arity = pred_info_orig_arity(PredInfo),
    NameAndArity =
        sym_name_arity_to_string(sym_name_arity(QualifiedName, Arity)),
    proc_id_to_int(ProcId, ProcIdInt),
    FullName = NameAndArity ++ "-" ++ int_to_string(ProcIdInt).

%-----------------------------------------------------------------------------%

complexity_process_proc_msg(ProgressStream, NumProcs, ProcMap, PredProcId,
        !ProcInfo, !ModuleInfo) :-
    PredProcId = proc(PredId, ProcId),
    IsInMap = is_in_complexity_proc_map(ProcMap, !.ModuleInfo,
        PredId, ProcId),
    (
        IsInMap = yes(ProcNum),
        FullName = complexity_proc_name(!.ModuleInfo, PredId, ProcId),
        module_info_get_globals(!.ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, verbose, Verbose),
        (
            Verbose = yes,
            trace [io(!IO)] (
                maybe_write_proc_progress_message(ProgressStream, !.ModuleInfo,
                    "Applying complexity experiment transformation to",
                    PredProcId, !IO)
            )
        ;
            Verbose = no
        ),
        complexity_process_proc(NumProcs, ProcNum, FullName, PredId,
            !ProcInfo, !ModuleInfo)
    ;
        IsInMap = no
    ).

% Example of transformation for model_det:
%
% p(In1, ..., InN, ...) :-
%   impure complexity_is_active(NumProcs, ProcNum, ProcName,
%       IsActive, Base),
%   (
%       IsActive = no,
%       impure complexity_call_proc(Slot, In1, ..., InN),
%       <original code>,
%       impure complexity_exit_proc(Slot)
%   ;
%       IsActive = yes,
%       <original code>
%   ).
%
% Example of transformation for model_semi:
%
% p(In1, ..., InN, ...) :-
%   impure complexity_is_active(NumProcs, ProcNum, ProcName,
%       IsActive, Base),
%   (
%       IsActive = no,
%       impure complexity_call_proc(Slot, In1, ..., InN),
%       (
%           <original code>,
%           impure complexity_exit_proc(Slot)
%       ;
%           impure complexity_fail_proc(Slot),
%           fail
%       )
%   ;
%       IsActive = yes,
%       <original code>
%   ).
%
% Example of transformation for model_non:
%
% p(In1, ..., InN, ...) :-
%   impure complexity_is_active(NumProcs, ProcNum, ProcName,
%       IsActive, Base),
%   (
%       IsActive = no,
%       impure complexity_call_proc(Slot, In1, ..., InN),
%       (
%           <original code>,
%           (
%               impure complexity_exit_proc(Slot)
%           ;
%               impure complexity_redo_proc(Slot),
%               fail
%           )
%       ;
%           impure complexity_fail_proc(Slot),
%           fail
%       )
%   ;
%       IsActive = yes,
%       <original code>
%   ).

:- func slot_var_name = string.

slot_var_name = "SlotVar".

:- pred complexity_process_proc(int::in, int::in, string::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out)
    is det.

complexity_process_proc(NumProcs, ProcNum, FullName, PredId,
        !ProcInfo, !ModuleInfo) :-
    proc_info_interface_determinism(!.ProcInfo, Detism),
    determinism_to_code_model(Detism, CodeModel),
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_argmodes(!.ProcInfo, ArgModes),
    proc_info_get_var_table(!.ProcInfo, VarTable),
    proc_info_get_goal(!.ProcInfo, OrigGoal),
    Context = goal_info_get_context(OrigGoalInfo),
    % Even if the original goal doesn't use all of the headvars, the code
    % generated by the transformation does, so we need to compute the
    % nonlocals from the headvars rather than getting it from the
    % nonlocals field in the original goal.
    set_of_var.list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = hlds_goal(_, OrigGoalInfo),
    OrigInstMapDelta = goal_info_get_instmap_delta(OrigGoalInfo),
    goal_info_set_purity(purity_impure, OrigGoalInfo, ImpureOrigGoalInfo),

    IsActiveVarName = "IsActive",
    generate_new_var(IsActiveVarName, is_active_type, is_not_dummy_type,
        IsActiveVar, !ProcInfo),

    classify_complexity_args(!.ModuleInfo, VarTable, HeadVars, ArgModes,
        VarInfos),
    allocate_slot_numbers_cl(VarInfos, 0, NumberedProfiledVars),
    list.length(NumberedProfiledVars, NumProfiledVars),
    generate_slot_goals(ProcNum, NumberedProfiledVars, NumProfiledVars,
        Context, PredId, !ProcInfo, !ModuleInfo, SlotVar, SlotVarName,
        SlotGoals),

    IsActiveOutputArg = foreign_arg(IsActiveVar,
        yes(foreign_arg_name_mode(IsActiveVarName, out_mode)),
        is_active_type, bp_native_if_possible),
    SlotInputArg = foreign_arg(SlotVar,
        yes(foreign_arg_name_mode(SlotVarName, in_mode)),
        int_type, bp_native_if_possible),

    ProcNumStr = int_to_string(ProcNum),

    IsActivePred = "complexity_is_active",
    IsActiveStr = "\tMR_" ++ IsActivePred ++ "(" ++
        int_to_string(NumProcs) ++ ", "
        ++ ProcNumStr ++ ", """ ++ FullName ++ """, " ++
        int_to_string(NumProfiledVars) ++ ", " ++
        IsActiveVarName ++ ");\n",

    complexity_generate_call_foreign_proc(IsActivePred, detism_det,
        [IsActiveOutputArg], [], IsActiveStr, [IsActiveVar],
        !.ModuleInfo, Context, IsActiveGoal),

    ExitPred = "complexity_exit_proc",
    ExitStr = "\tMR_" ++ ExitPred ++ "(" ++
        ProcNumStr ++ ", " ++ slot_var_name ++ ");\n",
    complexity_generate_call_foreign_proc(ExitPred, detism_det,
        [SlotInputArg], [], ExitStr, [],
        !.ModuleInfo, Context, ExitGoal),

    FailPred = "complexity_fail_proc",
    FailStr = "\tMR_" ++ FailPred ++ "(" ++
        ProcNumStr ++ ", " ++ slot_var_name ++ ");\n",
    complexity_generate_call_foreign_proc(FailPred, detism_failure,
        [SlotInputArg], [], FailStr, [],
        !.ModuleInfo, Context, FailGoal),

    RedoPred = "complexity_redo_proc",
    RedoStr = "\tMR_" ++ RedoPred ++ "(" ++
        ProcNumStr ++ ", " ++ slot_var_name ++ ");\n",
    complexity_generate_call_foreign_proc(RedoPred, detism_failure,
        [SlotInputArg], [], RedoStr, [],
        !.ModuleInfo, Context, RedoGoal0),

    (
        CodeModel = model_det,
        TransformedGoalExpr = conj(plain_conj,
            SlotGoals ++ [OrigGoal, ExitGoal]),
        TransGoal = hlds_goal(TransformedGoalExpr, ImpureOrigGoalInfo)
    ;
        CodeModel = model_semi,
        OrigAfterGoal = hlds_goal(conj(plain_conj, [OrigGoal, ExitGoal]),
            ImpureOrigGoalInfo),
        DisjGoal = hlds_goal(
            disj([OrigAfterGoal, FailGoal]),
            ImpureOrigGoalInfo),
        TransGoal = hlds_goal(
            conj(plain_conj, SlotGoals ++ [DisjGoal]),
            ImpureOrigGoalInfo)
    ;
        CodeModel = model_non,
        RedoGoal0 = hlds_goal(RedoGoalExpr, RedoGoalInfo0),
        goal_info_add_feature(feature_preserve_backtrack_into,
            RedoGoalInfo0, RedoGoalInfo),
        RedoGoal = hlds_goal(RedoGoalExpr, RedoGoalInfo),

        instmap_delta_init_reachable(AfterInstMapDelta),
        goal_info_init(set_of_var.make_singleton(SlotVar), AfterInstMapDelta,
            detism_multi, purity_impure, Context, AfterGoalInfo),
        AfterGoal = hlds_goal(disj([ExitGoal, RedoGoal]), AfterGoalInfo),

        OrigAfterGoal = hlds_goal(
            conj(plain_conj, [OrigGoal, AfterGoal]),
            ImpureOrigGoalInfo),
        DisjGoal = hlds_goal(
            disj([OrigAfterGoal, FailGoal]),
            ImpureOrigGoalInfo),
        TransGoal = hlds_goal(
            conj(plain_conj, SlotGoals ++ [DisjGoal]),
            ImpureOrigGoalInfo)
    ),

    TSPB = mercury_term_size_prof_builtin_module,
    TypeCtor = type_ctor(qualified(TSPB, "complexity_is_active"), 0),
    SwitchArms = [
        case(cons(qualified(TSPB, "is_inactive"), 0, TypeCtor), [], TransGoal),
        case(cons(qualified(TSPB, "is_active"), 0, TypeCtor), [], OrigGoal)
    ],

    SwitchExpr = switch(IsActiveVar, cannot_fail, SwitchArms),
    goal_info_init(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, SwitchGoalInfo),
    SwitchGoal = hlds_goal(SwitchExpr, SwitchGoalInfo),

    GoalExpr = conj(plain_conj, [IsActiveGoal, SwitchGoal]),
    goal_info_init(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),

    proc_info_set_goal(Goal, !ProcInfo),

    assoc_list.values(VarInfos, Infos),
    ComplexityInfo = complexity_proc_info(ProcNum, FullName, Infos),
    module_info_get_complexity_proc_infos(!.ModuleInfo, ComplexityInfos0),
    ComplexityInfos = [ComplexityInfo | ComplexityInfos0],
    module_info_set_complexity_proc_infos(ComplexityInfos, !ModuleInfo).

%-----------------------------------------------------------------------------%

    % Generate a foreign_proc goal of the form:
    %
    %   MR_ComplexityProc   *proc;
    %
    %   MR_complexity_call_proc(proc_num, slot);
    %   proc = &MR_complexity_procs[proc_num];
    %   MR_complexity_fill_size_slot(proc, slot, num_inputs, 1, size1);
    %   ...
    %   MR_complexity_fill_size_slot(proc, slot, num_inputs, N, sizeN);
    %
    % prefixed by the goals required to generate the typeinfos we need
    % to compute the sizes.
    %
:- pred generate_slot_goals(int::in, assoc_list(prog_var, int)::in,
    int::in, term.context::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    prog_var::out, string::out, list(hlds_goal)::out) is det.

generate_slot_goals(ProcNum, NumberedVars, NumProfiledVars, Context, PredId,
        !ProcInfo, !ModuleInfo, SlotVar, SlotVarName, Goals) :-
    SlotVarName = slot_var_name,
    generate_new_var(SlotVarName, int_type, is_not_dummy_type,
        SlotVar, !ProcInfo),
    ProcVarName = "proc",
    generate_size_goals(NumberedVars, Context, NumProfiledVars,
        ProcVarName, SlotVarName, PredId, !ProcInfo, !ModuleInfo,
        PrefixGoals, ForeignArgs, FillCodeStr),
    SlotVarArg = foreign_arg(SlotVar,
        yes(foreign_arg_name_mode(SlotVarName, out_mode)),
        int_type, bp_native_if_possible),
    PredName = "complexity_call_proc",
    DeclCodeStr = "\tMR_ComplexityProc *" ++ ProcVarName ++ ";\n",
    PredCodeStr = "\tMR_" ++ PredName ++ "(" ++
        int_to_string(ProcNum) ++ ", " ++ SlotVarName ++ ");\n",
    ProcStr = "\t" ++ ProcVarName ++ " = &MR_complexity_procs[" ++
        int_to_string(ProcNum) ++ "];\n",
    complexity_generate_call_foreign_proc(PredName, detism_det, [SlotVarArg],
        ForeignArgs, DeclCodeStr ++ PredCodeStr ++ ProcStr ++ FillCodeStr,
        [SlotVar], !.ModuleInfo, Context, CallGoal),
    list.append(PrefixGoals, [CallGoal], Goals).

:- pred generate_size_goals(assoc_list(prog_var, int)::in,
    term.context::in, int::in, string::in, string::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    list(hlds_goal)::out, list(foreign_arg)::out, string::out) is det.

generate_size_goals([], _, _, _, _, _, !ProcInfo, !ModuleInfo, [], [], "").
generate_size_goals([Var - VarSeqNum | NumberedVars], Context, NumProfiledVars,
        ProcVarName, SlotVarName, PredId, !ProcInfo, !ModuleInfo,
        Goals ++ RestGoals, ForeignArgs ++ RestForeignArgs,
        CodeStr ++ RestCodeStr) :-
    generate_size_goal(Var, VarSeqNum, Context, NumProfiledVars,
        ProcVarName, SlotVarName, PredId, !ProcInfo, !ModuleInfo,
        Goals, ForeignArgs, CodeStr),
    generate_size_goals(NumberedVars, Context, NumProfiledVars,
        ProcVarName, SlotVarName, PredId, !ProcInfo, !ModuleInfo,
        RestGoals, RestForeignArgs, RestCodeStr).

:- pred generate_size_goal(prog_var::in, int::in, term.context::in,
    int::in, string::in, string::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    list(hlds_goal)::out, list(foreign_arg)::out, string::out) is det.

generate_size_goal(ArgVar, VarSeqNum, Context, NumProfiledVars, ProcVarName,
        SlotVarName, PredId, !ProcInfo, !ModuleInfo, Goals,
        ForeignArgs, CodeStr) :-
    % XXX We should pass around !VarTables, not !ProcInfos.
    proc_info_get_var_table(!.ProcInfo, VarTable1),
    lookup_var_type(VarTable1, ArgVar, VarType),
    MacroName = "MR_complexity_fill_size_slot",
    % XXX This call is overkill; we can get a new type_info var
    % without updating !ProcInfo, much less !ModuleInfo.
    make_type_info_var(VarType, Context, PredId, !ProcInfo, !ModuleInfo,
        TypeInfoVar, Goals),
    % Since we just created TypeInfoVar, it isn't in VarTypes1.
    proc_info_get_var_table(!.ProcInfo, VarTable2),
    lookup_var_type(VarTable2, TypeInfoVar, TypeInfoType),
    ArgName = "arg" ++ int_to_string(VarSeqNum),
    TypeInfoArgName = "input_typeinfo" ++ int_to_string(VarSeqNum),
    ForeignArg = foreign_arg(ArgVar,
        yes(foreign_arg_name_mode(ArgName, in_mode)),
        VarType, bp_native_if_possible),
    ForeignTypeInfoArg = foreign_arg(TypeInfoVar,
        yes(foreign_arg_name_mode(TypeInfoArgName, in_mode)),
        TypeInfoType, bp_native_if_possible),
    ForeignArgs = [ForeignTypeInfoArg, ForeignArg],
    CodeStr = "\t" ++ MacroName ++ "(" ++
        ProcVarName ++ ", " ++
        SlotVarName ++ ", " ++
        int_to_string(NumProfiledVars) ++ ", " ++
        int_to_string(VarSeqNum) ++ ",\n\t\t" ++
        "MR_term_size((MR_TypeInfo) " ++
            TypeInfoArgName ++ ", " ++ ArgName ++ "));\n".

%-----------------------------------------------------------------------------%

:- pred generate_new_var(string::in, mer_type::in, is_dummy_type::in,
    prog_var::out, proc_info::in, proc_info::out) is det.

generate_new_var(Name, Type, IsDummy, Var, !ProcInfo) :-
    Entry = vte(Name, Type, IsDummy),
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    add_var_entry(Entry, Var, VarTable0, VarTable),
    proc_info_set_var_table(VarTable, !ProcInfo).

:- pred complexity_generate_call_foreign_proc(string::in, determinism::in,
    list(foreign_arg)::in, list(foreign_arg)::in, string::in,
    list(prog_var)::in, module_info::in, term.context::in, hlds_goal::out)
    is det.

complexity_generate_call_foreign_proc(PredName, Detism, Args, ExtraArgs,
        Code, BoundVars, ModuleInfo, Context, Goal) :-
    BuiltinModule = mercury_term_size_prof_builtin_module,
    Attrs0 = default_attributes(lang_c),
    set_may_call_mercury(proc_will_not_call_mercury, Attrs0, Attrs),
    MaybeTraceRuntimeCond = no,
    generate_call_foreign_proc(ModuleInfo, pf_predicate,
        BuiltinModule, PredName,
        [], Args, ExtraArgs, instmap_delta_bind_vars(BoundVars),
        only_mode, Detism, purity_impure, [], Attrs,
        MaybeTraceRuntimeCond, Code, Context, Goal).

%-----------------------------------------------------------------------------%

:- pred classify_complexity_args(module_info::in, var_table::in,
    list(prog_var)::in, list(mer_mode)::in,
    assoc_list(prog_var, complexity_arg_info)::out) is det.

classify_complexity_args(_, _, [], [], []).
classify_complexity_args(_, _, [_ | _], [], _) :-
    unexpected($pred, "lists not same length").
classify_complexity_args(_, _, [], [_ | _], _) :-
    unexpected($pred, "lists not same length").
classify_complexity_args(ModuleInfo, VarTable, [Var | Vars], [Mode | Modes],
        [Var - complexity_arg_info(MaybeName, Kind) | VarInfos]) :-
    classify_complexity_args(ModuleInfo, VarTable, Vars, Modes, VarInfos),
    lookup_var_entry(VarTable, Var, Entry),
    Entry = vte(Name, VarType, IsDummy),
    ( if Name = "" then
        MaybeName = no
    else
        MaybeName = yes(Name)
    ),
    ( if mode_is_fully_input(ModuleInfo, Mode) then
        ( if
            ( IsDummy = is_dummy_type
            ; zero_size_type(ModuleInfo, VarType)
            )
        then
            Kind = complexity_input_fixed_size
        else
            Kind = complexity_input_variable_size
        )
    else
        Kind = complexity_output
    ).

%-----------------------------------------------------------------------------%

:- pred allocate_slot_numbers_cl(assoc_list(prog_var, complexity_arg_info)::in,
    int::in, assoc_list(prog_var, int)::out) is det.

allocate_slot_numbers_cl([], _, []).
allocate_slot_numbers_cl([Var - Info | VarInfos], Offset,
        NumberedProfiledVars) :-
    Info = complexity_arg_info(_, Kind),
    (
        Kind = complexity_input_variable_size,
        allocate_slot_numbers_cl(VarInfos, Offset + 1,
            NumberedProfiledVarsTail),
        NumberedProfiledVars = [Var - Offset | NumberedProfiledVarsTail]
    ;
        ( Kind = complexity_input_fixed_size
        ; Kind = complexity_output
        ),
        allocate_slot_numbers_cl(VarInfos, Offset, NumberedProfiledVars)
    ).

%-----------------------------------------------------------------------------%

:- func is_active_type = mer_type.

is_active_type = Type :-
    M = mercury_term_size_prof_builtin_module,
    construct_type(type_ctor(qualified(M, "complexity_is_active"), 0), [],
        Type).

%-----------------------------------------------------------------------------%

:- pred make_type_info_var(mer_type::in, term.context::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    prog_var::out, list(hlds_goal)::out) is det.

make_type_info_var(Type, Context, PredId, !ProcInfo, !ModuleInfo,
        TypeInfoVar, TypeInfoGoals) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    polymorphism_make_type_info_var_mi(Type, Context,
        TypeInfoVar, TypeInfoGoals, !ModuleInfo,
        PredInfo0, PredInfo, !ProcInfo),
    expect(unify(PredInfo0, PredInfo), $pred, "modified pred_info").

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.complexity.
%-----------------------------------------------------------------------------%
