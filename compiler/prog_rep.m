%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 University of Melbourne.
% Copyright (C) 2015-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_rep.m.
% Authors: zs, maclarty.
%
% This module generates a representation of HLDS goals for the declarative
% debugger. Since this representation is to be included in debuggable
% executables, it should be as compact as possible, and therefore contains
% only the information required by the declarative debugger. The structure
% of this representation is defined by mdbcomp/program_representation.m.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.prog_rep.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module hlds.vartypes.
:- import_module ll_backend.prog_rep_tables.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module pair.

%-----------------------------------------------------------------------------%

    % A var_num_map maps each variable that occurs in any of a procedure's
    % layout structures to a number that uniquely identifies that variable,
    % and to its name.
    %
    % The integer returned by term.var_to_int are a dense set when we consider
    % all the original variables of a procedure. However, it can become less
    % dense when an optimization removes all references to a variable, and
    % becomes less dense still when we consider only variables that occur
    % in a layout structure. This is why we allocate our own id numbers.
    %
:- type var_num_map == map(prog_var, pair(int, string)).

    % Encode the information in a module's oisu pragmas into bytecode,
    % for use by the automatic parallelization feedback tool.
    %
:- pred encode_oisu_type_procs(module_info::in,
    assoc_list(type_ctor, oisu_preds)::in, int::out, cord(int)::out) is det.

    % Create the bytecodes for the given procedure.
    %
:- pred represent_proc_as_bytecodes(list(prog_var)::in, hlds_goal::in,
    instmap::in, vartypes::in, var_num_map::in, module_info::in,
    maybe_include_var_name_table::in, maybe_include_var_types::in,
    determinism::in, string_table_info::in, string_table_info::out,
    type_table_info::in, type_table_info::out, list(int)::out) is det.

%-----------------------------------------------------------------------------%

:- type prog_rep_info
    --->    prog_rep_info(
                pri_module_info         :: module_info,
                pri_filename            :: string,
                pri_vartypes            :: vartypes,
                pri_var_num_map         :: var_num_map,
                pri_var_num_rep         :: var_num_rep,
                pri_flatten_par_conjs   :: flatten_par_conjs
            ).

:- type flatten_par_conjs
    --->    flatten_par_conjs
    ;       expect_no_par_conjs.

:- pred goal_to_goal_rep(prog_rep_info::in, instmap::in, hlds_goal::in,
    goal_rep::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.proc_label.
:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_test.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.special_pred.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.rtti_access.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module unit.

%-----------------------------------------------------------------------------%

encode_oisu_type_procs(_ModuleInfo, [], 0, cord.init).
encode_oisu_type_procs(ModuleInfo, [Pair | Pairs], NumOISUTypes, Bytes) :-
    encode_oisu_type_procs(ModuleInfo, Pairs, TailNumOISUTypes, TailBytes),
    module_info_get_name(ModuleInfo, ModuleName),
    Pair = TypeCtor - Preds,
    TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
    (
        TypeCtorSymName = qualified(TypeCtorModuleName, TypeCtorName)
    ;
        TypeCtorSymName = unqualified(_),
        unexpected($pred, "unqualified type_ctor name")
    ),
    ( if TypeCtorModuleName = ModuleName then
        encode_len_string(TypeCtorName, TypeCtorNameBytes),
        Preds = oisu_preds(CreatorPreds, MutatorPreds, DestructorPreds),

        list.length(CreatorPreds, NumCreatorPreds),
        list.length(MutatorPreds, NumMutatorPreds),
        list.length(DestructorPreds, NumDestructorPreds),
        encode_num_det(NumCreatorPreds, NumCreatorPredsBytes),
        encode_num_det(NumMutatorPreds, NumMutatorPredsBytes),
        encode_num_det(NumDestructorPreds, NumDestructorPredsBytes),
        list.map(encode_oisu_proc(ModuleInfo), CreatorPreds,
            CreatorPredBytes),
        list.map(encode_oisu_proc(ModuleInfo), MutatorPreds,
            MutatorPredBytes),
        list.map(encode_oisu_proc(ModuleInfo), DestructorPreds,
            DestructorPredBytes),

        HeadBytes = cord.from_list(TypeCtorNameBytes)
            ++ cord.from_list(NumCreatorPredsBytes)
            ++ cord.cord_list_to_cord(CreatorPredBytes)
            ++ cord.from_list(NumMutatorPredsBytes)
            ++ cord.cord_list_to_cord(MutatorPredBytes)
            ++ cord.from_list(NumDestructorPredsBytes)
            ++ cord.cord_list_to_cord(DestructorPredBytes),

        NumOISUTypes = 1 + TailNumOISUTypes,
        Bytes = HeadBytes ++ TailBytes
    else
        NumOISUTypes = TailNumOISUTypes,
        Bytes = TailBytes
    ).

:- pred encode_oisu_proc(module_info::in, pred_id::in, cord(int)::out) is det.

encode_oisu_proc(ModuleInfo, PredId, BytesCord) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.to_assoc_list(ProcTable, Procs),
    ( if Procs = [ProcId - _ProcInfo] then
        ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
        encode_string_proc_label(ProcLabel, BytesCord)
    else
        unexpected($pred, "OISU pred should have exactly one proc")
    ).

:- pred encode_string_proc_label(proc_label::in, cord(int)::out) is det.

encode_string_proc_label(ProcLabel, BytesCord) :-
    (
        ProcLabel = ordinary_proc_label(DefModuleName, PredOrFunc,
            DeclModuleName, PredName, Arity, ModeNum),
        (
            PredOrFunc = pf_predicate,
            KindByte = string_proclabel_kind_user_predicate
        ;
            PredOrFunc = pf_function,
            KindByte = string_proclabel_kind_user_function
        ),
        encode_len_string(sym_name_to_string(DeclModuleName),
            DeclModuleNameBytes),
        encode_len_string(sym_name_to_string(DefModuleName),
            DefModuleNameBytes),
        encode_len_string(PredName, PredNameBytes),
        encode_num_det(Arity, ArityBytes),
        encode_num_det(ModeNum, ModeNumBytes),
        BytesCord = cord.from_list([KindByte | DeclModuleNameBytes])
            ++ cord.from_list(DefModuleNameBytes)
            ++ cord.from_list(PredNameBytes)
            ++ cord.from_list(ArityBytes)
            ++ cord.from_list(ModeNumBytes)
    ;
        ProcLabel = special_proc_label(DefModuleName, SpecialPredId,
            TypeModuleName, TypeName, TypeArity, ModeNum),
        TypeCtor = type_ctor(qualified(TypeModuleName, TypeName), TypeArity),
        PredName = special_pred_name(SpecialPredId, TypeCtor),

        KindByte = string_proclabel_kind_special,
        encode_len_string(TypeName, TypeNameBytes),
        encode_len_string(sym_name_to_string(TypeModuleName),
            TypeModuleNameBytes),
        encode_len_string(sym_name_to_string(DefModuleName),
            DefModuleNameBytes),
        encode_len_string(PredName, PredNameBytes),
        encode_num_det(TypeArity, TypeArityBytes),
        encode_num_det(ModeNum, ModeNumBytes),
        BytesCord = cord.from_list([KindByte | TypeNameBytes])
            ++ cord.from_list(TypeModuleNameBytes)
            ++ cord.from_list(DefModuleNameBytes)
            ++ cord.from_list(PredNameBytes)
            ++ cord.from_list(TypeArityBytes)
            ++ cord.from_list(ModeNumBytes)
    ).

    % The definitions of these functions should be kept in sync with
    % the definition of the MR_ProcLabelToken type in mercury_deep_profiling.h.
    %
:- func string_proclabel_kind_user_predicate = int.
:- func string_proclabel_kind_user_function = int.
:- func string_proclabel_kind_special = int.

string_proclabel_kind_user_predicate = 0.
string_proclabel_kind_user_function = 1.
string_proclabel_kind_special = 2.

%-----------------------------------------------------------------------------%

represent_proc_as_bytecodes(HeadVars, Goal, InstMap0, VarTypes, VarNumMap,
        ModuleInfo, IncludeVarNameTable, IncludeVarTypes, ProcDetism,
        !StringTable, !TypeTable, ProcRepBytes) :-
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    term.context_file(Context, FileName),
    represent_var_table_as_bytecode(IncludeVarNameTable, IncludeVarTypes,
        VarTypes, VarNumMap, VarNumRep, VarNameTableBytes,
        !StringTable, !TypeTable),
    Info = prog_rep_info(ModuleInfo, FileName, VarTypes, VarNumMap, VarNumRep,
        expect_no_par_conjs),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),

    encode_string_as_table_offset(FileName, FileNameBytes, !StringTable),
    goal_to_byte_list(Goal, InstMap0, Info, GoalBytes, !StringTable),
    DetismByte = represent_determinism(ProcDetism),
    ProcRepBytes0 = FileNameBytes ++ VarNameTableBytes ++
        encode_head_vars_func(Info, InstMap0, InstmapDelta, HeadVars) ++
        GoalBytes ++ [DetismByte],
    encode_int32_det(list.length(ProcRepBytes0) + 4, LimitBytes),
    ProcRepBytes = LimitBytes ++ ProcRepBytes0.

%-----------------------------------------------------------------------------%

    % Create bytecodes for the variable name table.
    %
    % If a variable name table is not requested, an empty table is created.
    % The variable name table also includes information about the
    % representation of variable numbers within the bytecode.
    %
    % The representation of variables and the variable name table
    % restricts the number of possible variables in a procedure to 2^31.
    %
:- pred represent_var_table_as_bytecode(maybe_include_var_name_table::in,
    maybe_include_var_types::in, vartypes::in,
    var_num_map::in, var_num_rep::out, list(int)::out,
    string_table_info::in, string_table_info::out,
    type_table_info::in, type_table_info::out) is det.

represent_var_table_as_bytecode(IncludeVarNameTable, IncludeVarTypes,
        VarTypes, VarNumMap, VarNumRep, Bytes, !StringTable, !TypeTable) :-
    map.foldl(max_var_num, VarNumMap, 0) = MaxVarNum,
    ( if MaxVarNum =< 127 then
        VarNumRep = var_num_1_byte
    else if MaxVarNum =< 32767 then
        VarNumRep = var_num_2_bytes
    else
        VarNumRep = var_num_4_bytes
    ),
    var_flag_byte(VarNumRep, IncludeVarNameTable, IncludeVarTypes, FlagByte),
    (
        IncludeVarNameTable = include_var_name_table,
        (
            IncludeVarTypes = do_not_include_var_types,
            % It is more efficient to make the switch over the representation
            % size just once here, than to make it when representing each
            % variable.
            (
                VarNumRep = var_num_1_byte,
                map.foldl3(encode_var_name_table_entry_1_byte, VarNumMap,
                    0, NumVars, [], VarNameTableEntriesBytes, !StringTable)
            ;
                VarNumRep = var_num_2_bytes,
                map.foldl3(encode_var_name_table_entry_2_byte, VarNumMap,
                    0, NumVars, [], VarNameTableEntriesBytes, !StringTable)
            ;
                VarNumRep = var_num_4_bytes,
                map.foldl3(encode_var_name_table_entry_4_byte, VarNumMap,
                    0, NumVars, [], VarNameTableEntriesBytes, !StringTable)
            )
        ;
            IncludeVarTypes = include_var_types,
            map.foldl4(encode_var_name_type_table_entry(VarNumRep, VarTypes),
                VarNumMap, 0, NumVars,
                [], VarNameTableEntriesBytes, !StringTable, !TypeTable)
        ),
        encode_num_det(NumVars, NumVarsBytes),
        Bytes = [FlagByte] ++ NumVarsBytes ++ VarNameTableEntriesBytes
    ;
        IncludeVarNameTable = do_not_include_var_name_table,
        expect(unify(IncludeVarTypes, do_not_include_var_types), $pred,
            "IncludeVarTypes but not IncludeVarNameTable"),
        Bytes = [FlagByte]
    ).

:- func max_var_num(prog_var, pair(int, string), int) = int.

max_var_num(_, VarNum1 - _, VarNum2) = Max :-
    Max = max(VarNum1, VarNum2).

%-----------------------------------------------------------------------------%
%
% To avoid repeated tests of the size of variable representations, we have
% a specialized version for each different variable number encoding size.
%
% Some variables that the compiler creates are named automatically,
% these and unnamed variables should not be included in the variable
% name table.
%

:- pred encode_var_name_table_entry_1_byte(prog_var::in, pair(int, string)::in,
    int::in, int::out, list(int)::in, list(int)::out,
    string_table_info::in, string_table_info::out) is det.

encode_var_name_table_entry_1_byte(_ProgVar, VarNum - VarName,
        !NumVars, !VarNameTableBytes, !StringTable) :-
    ( if compiler_introduced_varname(VarName) then
        true
    else
        !:NumVars = !.NumVars + 1,
        VarBytes = [VarNum],
        encode_string_as_table_offset(VarName, VarNameBytes, !StringTable),
        !:VarNameTableBytes = VarBytes ++ VarNameBytes ++ !.VarNameTableBytes
    ).

:- pred encode_var_name_table_entry_2_byte(prog_var::in, pair(int, string)::in,
    int::in, int::out, list(int)::in, list(int)::out,
    string_table_info::in, string_table_info::out) is det.

encode_var_name_table_entry_2_byte(_ProgVar, VarNum - VarName,
        !NumVars, !VarNameTableBytes, !StringTable) :-
    ( if compiler_introduced_varname(VarName) then
        true
    else
        !:NumVars = !.NumVars + 1,
        encode_short_det(VarNum, VarBytes),
        encode_string_as_table_offset(VarName, VarNameBytes, !StringTable),
        !:VarNameTableBytes = VarBytes ++ VarNameBytes ++ !.VarNameTableBytes
    ).

:- pred encode_var_name_table_entry_4_byte(prog_var::in, pair(int, string)::in,
    int::in, int::out, list(int)::in, list(int)::out,
    string_table_info::in, string_table_info::out) is det.

encode_var_name_table_entry_4_byte(_ProgVar, VarNum - VarName,
        !NumVars, !VarNameTableBytes, !StringTable) :-
    ( if compiler_introduced_varname(VarName) then
        true
    else
        !:NumVars = !.NumVars + 1,
        encode_int32_det(VarNum, VarBytes),
        encode_string_as_table_offset(VarName, VarNameBytes, !StringTable),
        !:VarNameTableBytes = VarBytes ++ VarNameBytes ++ !.VarNameTableBytes
    ).

:- pred encode_var_name_type_table_entry(var_num_rep::in, vartypes::in,
    prog_var::in, pair(int, string)::in, int::in, int::out,
    list(int)::in, list(int)::out,
    string_table_info::in, string_table_info::out,
    type_table_info::in, type_table_info::out) is det.

encode_var_name_type_table_entry(VarNumRep, VarTypes, Var, VarNum - VarName,
        !NumVars, !VarNameTableBytes, !StringTable, !TypeTable) :-
    !:NumVars = !.NumVars + 1,
    lookup_var_type(VarTypes, Var, Type),
    (
        VarNumRep = var_num_1_byte,
        VarBytes = [VarNum]
    ;
        VarNumRep = var_num_2_bytes,
        encode_short_det(VarNum, VarBytes)
    ;
        VarNumRep = var_num_4_bytes,
        encode_int32_det(VarNum, VarBytes)
    ),
    encode_string_as_table_offset(VarName, VarNameBytes, !StringTable),
    encode_type_as_table_ref(Type, TypeBytes, !StringTable, !TypeTable),
    !:VarNameTableBytes = VarBytes ++ VarNameBytes ++ TypeBytes
        ++ !.VarNameTableBytes.

%-----------------------------------------------------------------------------%

:- pred compiler_introduced_varname(string::in) is semidet.

compiler_introduced_varname("").
compiler_introduced_varname("ProcStaticLayout").
compiler_introduced_varname("TopCSD").
compiler_introduced_varname("MiddleCSD").
compiler_introduced_varname("ActivationPtr").
compiler_introduced_varname("SiteNum").
compiler_introduced_varname("MethodNum").
compiler_introduced_varname(VarName) :-
    ( Prefix = "V_"
    ; Prefix = "HeadVar__"
    ; Prefix = "TypeClassInfo_for_"
    ; Prefix = "TypeInfo_"
    ; Prefix = "TypeCtorInfo_"
    ; Prefix = "STATE_VARIABLE_"
    ; Prefix = "DCG_"
    ),
    prefix(VarName, Prefix).

%-----------------------------------------------------------------------------%

:- pred goal_to_byte_list(hlds_goal::in, instmap::in, prog_rep_info::in,
    list(int)::out, string_table_info::in, string_table_info::out) is det.

goal_to_byte_list(Goal, InstMap0, Info, Bytes, !StringTable) :-
    goal_to_goal_rep(Info, InstMap0, Goal, GoalRep),
    encode_goal_rep(Info, GoalRep, Bytes, !StringTable).

goal_to_goal_rep(Info, Instmap0, hlds_goal(GoalExpr, GoalInfo), GoalRep) :-
    Detism = goal_info_get_determinism(GoalInfo),
    detism_to_detism_rep(Detism, DetismRep),
    GoalRep = goal_rep(GoalExprRep, DetismRep, unit),
    (
        GoalExpr = conj(ConjType, Goals0),
        FlattenParConjs = Info ^ pri_flatten_par_conjs,
        (
            FlattenParConjs = flatten_par_conjs,
            % Flatten all conjunction types, the current conjunction may be a
            % plain conjunction with par conjunctions in it, or vice-versa.
            flatten_conj(Goals0, Goals)
        ;
            FlattenParConjs = expect_no_par_conjs,
            Goals = Goals0,
            expect(unify(ConjType, plain_conj), $pred,
                "non-plain conjunction and declarative debugging")
        ),
        conj_to_conj_rep(Info, Instmap0, Goals, GoalReps),
        GoalExprRep = conj_rep(GoalReps)
    ;
        GoalExpr = disj(Goals),
        % Since eash disjunct begins with the same instmap we can use map
        map(goal_to_goal_rep(Info, Instmap0), Goals, GoalReps),
        GoalExprRep = disj_rep(GoalReps)
    ;
        GoalExpr = negation(SubGoal),
        goal_to_goal_rep(Info, Instmap0, SubGoal, SubGoalRep),
        GoalExprRep = negation_rep(SubGoalRep)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        Cond = hlds_goal(_, CondGoalInfo),
        InstmapDelta = goal_info_get_instmap_delta(CondGoalInfo),
        apply_instmap_delta(InstmapDelta, Instmap0, InstmapAfterCond),
        goal_to_goal_rep(Info, Instmap0, Cond, CondRep),
        goal_to_goal_rep(Info, InstmapAfterCond, Then, ThenRep),
        goal_to_goal_rep(Info, Instmap0, Else, ElseRep),
        GoalExprRep = ite_rep(CondRep, ThenRep, ElseRep)
    ;
        GoalExpr = switch(Var, CanFail, Cases),
        map(case_to_case_rep(Info, Instmap0), Cases, CasesRep),
        (
            CanFail = can_fail,
            CanFailRep = switch_can_fail_rep
        ;
            CanFail = cannot_fail,
            CanFailRep = switch_can_not_fail_rep
        ),
        VarRep = var_to_var_rep(Info, Var),
        GoalExprRep = switch_rep(VarRep, CanFailRep, CasesRep)
    ;
        GoalExpr = scope(_, SubGoal),
        SubGoal = hlds_goal(_, SubGoalInfo),
        goal_to_goal_rep(Info, Instmap0, SubGoal, SubGoalRep),
        OuterDetism = goal_info_get_determinism(GoalInfo),
        InnerDetism = goal_info_get_determinism(SubGoalInfo),
        ( if InnerDetism = OuterDetism then
            MaybeCut = scope_is_no_cut
        else
            MaybeCut = scope_is_cut
        ),
        GoalExprRep = scope_rep(SubGoalRep, MaybeCut)
    ;
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        goal_info_to_atomic_goal_rep_fields(GoalInfo, Instmap0, Info,
            FileName, LineNo, BoundVars),
        BoundVarsRep = map(var_to_var_rep(Info), BoundVars),
        (
            GoalExpr = unify(_, _, _, Unification, _),
            (
                Unification = assign(Target, Source),
                AtomicGoalRep = unify_assign_rep(
                    var_to_var_rep(Info, Target),
                    var_to_var_rep(Info, Source))
            ;
                ( Unification = construct(Var, ConsId, Args, ArgModes, _, _, _)
                ; Unification = deconstruct(Var, ConsId, Args, ArgModes, _, _)
                ),
                VarRep = var_to_var_rep(Info, Var),
                ConsIdRep = cons_id_rep(ConsId),
                ArgsRep = map(var_to_var_rep(Info), Args),
                filter_input_args(Info, ArgModes, Args, MaybeArgs),
                MaybeArgsRep = map(map_maybe(var_to_var_rep(Info)), MaybeArgs),
                (
                    Unification = construct(_, _, _, _, _, _, _),
                    ( if
                        list.all_true(lhs_final_is_ground(Info), ArgModes)
                    then
                        AtomicGoalRep = unify_construct_rep(VarRep, ConsIdRep,
                            ArgsRep)
                    else
                        AtomicGoalRep = partial_construct_rep(VarRep,
                            ConsIdRep, MaybeArgsRep)
                    )
                ;
                    Unification = deconstruct(_, _, _, _, _, _),
                    ( if list.member(Var, BoundVars) then
                        AtomicGoalRep = partial_deconstruct_rep(VarRep,
                            ConsIdRep, MaybeArgsRep)
                    else
                        AtomicGoalRep = unify_deconstruct_rep(VarRep,
                            ConsIdRep, ArgsRep)
                    )
                )
            ;
                Unification = simple_test(Var1, Var2),
                AtomicGoalRep = unify_simple_test_rep(
                    var_to_var_rep(Info, Var1),
                    var_to_var_rep(Info, Var2))
            ;
                Unification = complicated_unify(_, _, _),
                unexpected($pred, "complicated_unify")
            )
        ;
            GoalExpr = generic_call(GenericCall, Args, _, _, _),
            ArgsRep = map(var_to_var_rep(Info), Args),
            (
                GenericCall = higher_order(PredVar, _, _, _),
                PredVarRep = var_to_var_rep(Info, PredVar),
                AtomicGoalRep = higher_order_call_rep(PredVarRep, ArgsRep)
            ;
                GenericCall = class_method(Var, Num, _, _),
                VarRep = var_to_var_rep(Info, Var),
                AtomicGoalRep = method_call_rep(VarRep, Num, ArgsRep)
            ;
                GenericCall = event_call(EventName),
                AtomicGoalRep = event_call_rep(EventName, ArgsRep)
            ;
                GenericCall = cast(_),
                ( if ArgsRep = [InputArgRep, OutputArgRep] then
                    AtomicGoalRep = cast_rep(OutputArgRep, InputArgRep)
                else
                    unexpected($pred, "cast/coerce arity != 2")
                )
            )
        ;
            GoalExpr = plain_call(PredId, _, Args, Builtin, _, _),
            module_info_pred_info(Info ^ pri_module_info, PredId, PredInfo),
            ModuleSymName = pred_info_module(PredInfo),
            ModuleName = sym_name_to_string(ModuleSymName),
            PredName = pred_info_name(PredInfo),
            ArgsRep = map(var_to_var_rep(Info), Args),
            (
                Builtin = not_builtin,
                AtomicGoalRep = plain_call_rep(ModuleName, PredName, ArgsRep)
            ;
                Builtin = inline_builtin,
                AtomicGoalRep = builtin_call_rep(ModuleName, PredName, ArgsRep)
            )
        ;
            GoalExpr = call_foreign_proc(_, _PredId, _, Args, _, _, _),
            ArgVarsRep = list.map(
                compose(var_to_var_rep(Info), foreign_arg_var), Args),
            AtomicGoalRep = pragma_foreign_code_rep(ArgVarsRep)
        ),
        GoalExprRep = atomic_goal_rep(FileName, LineNo, BoundVarsRep,
            AtomicGoalRep)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "unexpected shorthand")
    ).

:- pred conj_to_conj_rep(prog_rep_info::in, instmap::in, list(hlds_goal)::in,
    list(goal_rep)::out) is det.

conj_to_conj_rep(_, _, [], []).
conj_to_conj_rep(Info, Instmap0, [Conj | Conjs], [ConjRep | ConjReps]) :-
    goal_to_goal_rep(Info, Instmap0, Conj, ConjRep),
    GoalInfo = Conj ^ hg_info,
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstmapDelta, Instmap0, Instmap1),
    conj_to_conj_rep(Info, Instmap1, Conjs, ConjReps).

:- pred case_to_case_rep(prog_rep_info::in, instmap::in,
    case::in, case_rep::out) is det.

case_to_case_rep(Info, Instmap, case(FirstConsId, OtherConsIds, Goal),
        case_rep(FirstConsIdRep, OtherConsIdsRep, GoalRep)) :-
    goal_to_goal_rep(Info, Instmap, Goal, GoalRep),
    cons_id_to_cons_id_rep(FirstConsId, FirstConsIdRep),
    map(cons_id_to_cons_id_rep, OtherConsIds, OtherConsIdsRep).

:- pred cons_id_to_cons_id_rep(cons_id::in, cons_id_arity_rep::out) is det.

cons_id_to_cons_id_rep(ConsId, cons_id_arity_rep(ConsIdName, Arity)) :-
    ConsIdName = cons_id_rep(ConsId),
    MaybeArity = cons_id_maybe_arity(ConsId),
    (
        MaybeArity = yes(Arity)
    ;
        MaybeArity = no,
        Arity = 0
    ).

:- pred detism_to_detism_rep(determinism::in, detism_rep::out) is det.

detism_to_detism_rep(detism_det, det_rep).
detism_to_detism_rep(detism_semi, semidet_rep).
detism_to_detism_rep(detism_multi, multidet_rep).
detism_to_detism_rep(detism_non, nondet_rep).
detism_to_detism_rep(detism_cc_multi, cc_multidet_rep).
detism_to_detism_rep(detism_cc_non, cc_nondet_rep).
detism_to_detism_rep(detism_erroneous, erroneous_rep).
detism_to_detism_rep(detism_failure, failure_rep).

:- pred encode_goal_rep(prog_rep_info::in, goal_rep::in, list(int)::out,
    string_table_info::in, string_table_info::out) is det.

encode_goal_rep(Info, goal_rep(GoalExpr, Detism, _), Bytes, !StringTable) :-
    (
        GoalExpr = conj_rep(GoalReps),
        map_foldl(encode_goal_rep(Info), GoalReps, ConjBytesList,
            !StringTable),
        ExprBytes = [goal_type_to_byte(goal_conj)] ++
            encode_length_func(GoalReps) ++ condense(ConjBytesList)
    ;
        GoalExpr = disj_rep(GoalReps),
        map_foldl(encode_goal_rep(Info), GoalReps, DisjBytesList,
            !StringTable),
        ExprBytes = [goal_type_to_byte(goal_disj)] ++
            encode_length_func(GoalReps) ++ condense(DisjBytesList)
    ;
        GoalExpr = negation_rep(SubGoal),
        encode_goal_rep(Info, SubGoal, SubGoalBytes, !StringTable),
        ExprBytes = [goal_type_to_byte(goal_neg)] ++ SubGoalBytes
    ;
        GoalExpr = ite_rep(Cond, Then, Else),
        encode_goal_rep(Info, Cond, CondBytes, !StringTable),
        encode_goal_rep(Info, Then, ThenBytes, !StringTable),
        encode_goal_rep(Info, Else, ElseBytes, !StringTable),
        ExprBytes = [goal_type_to_byte(goal_ite)] ++
            CondBytes ++ ThenBytes ++ ElseBytes
    ;
        GoalExpr = atomic_goal_rep(FileName, Line, BoundVars, AtomicGoalRep),
        encode_string_as_table_offset(FileName, FileNameBytes, !StringTable),
        AtomicBytes = FileNameBytes ++ encode_lineno_func(Line) ++
            encode_var_reps_func(Info, BoundVars),
        (
            AtomicGoalRep = unify_assign_rep(Target, Source),
            ExprBytes = [goal_type_to_byte(goal_assign)] ++
                encode_var_rep_func(Info, Target) ++
                encode_var_rep_func(Info, Source) ++
                AtomicBytes
        ;
            ( AtomicGoalRep = unify_construct_rep(_, _, _)
            ; AtomicGoalRep = unify_deconstruct_rep(_, _, _)
            ; AtomicGoalRep = partial_construct_rep(_, _, _)
            ; AtomicGoalRep = partial_deconstruct_rep(_, _, _)
            ),
            (
                (
                    AtomicGoalRep = unify_construct_rep(Var, ConsId, Args),
                    AtomicTypeByte = goal_type_to_byte(goal_construct)
                ;
                    AtomicGoalRep = unify_deconstruct_rep(Var, ConsId, Args),
                    AtomicTypeByte = goal_type_to_byte(goal_deconstruct)
                ),
                ArgsBytes = encode_var_reps_func(Info, Args)
            ;
                (
                    AtomicGoalRep = partial_deconstruct_rep(Var, ConsId,
                        MaybeArgs),
                    AtomicTypeByte =
                        goal_type_to_byte(goal_partial_deconstruct)
                ;
                    AtomicGoalRep = partial_construct_rep(Var, ConsId,
                        MaybeArgs),
                    AtomicTypeByte = goal_type_to_byte(goal_partial_construct)
                ),
                ArgsBytes = encode_maybe_var_reps_func(Info, MaybeArgs)
            ),
            encode_string_as_table_offset(ConsId, ConsIdBytes, !StringTable),
            VarBytes = encode_var_rep_func(Info, Var),
            ExprBytes = [AtomicTypeByte] ++ VarBytes ++ ConsIdBytes ++
                ArgsBytes ++ AtomicBytes
        ;
            AtomicGoalRep = unify_simple_test_rep(Var1, Var2),
            ExprBytes = [goal_type_to_byte(goal_simple_test)] ++
                encode_var_rep_func(Info, Var1) ++
                encode_var_rep_func(Info, Var2) ++
                AtomicBytes
        ;
            AtomicGoalRep = higher_order_call_rep(PredVar, Args),
            ExprBytes = [goal_type_to_byte(goal_ho_call)] ++
                encode_var_rep_func(Info, PredVar) ++
                encode_var_reps_func(Info, Args) ++
                AtomicBytes
        ;
            AtomicGoalRep = method_call_rep(Var, MethodNum, Args),
            ExprBytes = [goal_type_to_byte(goal_method_call)] ++
                encode_var_rep_func(Info, Var) ++
                encode_method_num_func(MethodNum) ++
                encode_var_reps_func(Info, Args) ++
                AtomicBytes
        ;
            AtomicGoalRep = event_call_rep(EventName, Args),
            encode_string_as_table_offset(EventName, EventNameBytes,
                !StringTable),
            ExprBytes = [goal_type_to_byte(goal_event_call)] ++
                EventNameBytes ++
                encode_var_reps_func(Info, Args) ++
                AtomicBytes
        ;
            AtomicGoalRep = cast_rep(Target, Source),
            ExprBytes = [goal_type_to_byte(goal_cast)] ++
                encode_var_rep_func(Info, Target) ++
                encode_var_rep_func(Info, Source) ++
                AtomicBytes
        ;
            (
                AtomicGoalRep = plain_call_rep(ModuleName, PredName, Args),
                CallType = goal_plain_call
            ;
                AtomicGoalRep = builtin_call_rep(ModuleName, PredName, Args),
                CallType = goal_builtin_call
            ),
            encode_string_as_table_offset(ModuleName, ModuleNameBytes,
                !StringTable),
            encode_string_as_table_offset(PredName, PredNameBytes,
                !StringTable),
            ExprBytes = [goal_type_to_byte(CallType)] ++
                ModuleNameBytes ++
                PredNameBytes ++
                encode_var_reps_func(Info, Args) ++
                AtomicBytes
        ;
            AtomicGoalRep = pragma_foreign_code_rep(Args),
            ExprBytes = [goal_type_to_byte(goal_foreign)] ++
                encode_var_reps_func(Info, Args) ++ AtomicBytes
        )
    ;
        GoalExpr = switch_rep(SwitchVar, CanFail, Cases),
        list.map_foldl(encode_case_rep(Info), Cases, CasesBytesList,
            !StringTable),
        can_fail_byte(CanFail, CanFailByte),
        ExprBytes = [goal_type_to_byte(goal_switch)] ++
            [CanFailByte] ++
            encode_var_rep_func(Info, SwitchVar) ++
            encode_length_func(Cases) ++ list.condense(CasesBytesList)
    ;
        GoalExpr = scope_rep(SubGoal, MaybeCut),
        cut_byte(MaybeCut, MaybeCutByte),
        encode_goal_rep(Info, SubGoal, SubGoalBytes, !StringTable),
        ExprBytes = [goal_type_to_byte(goal_scope)] ++ [MaybeCutByte] ++
            SubGoalBytes
    ),
    determinism_representation(Detism, DetismByte),
    Bytes = ExprBytes ++ [DetismByte].

:- pred encode_case_rep(prog_rep_info::in, case_rep::in, list(int)::out,
    string_table_info::in, string_table_info::out) is det.

encode_case_rep(Info, Case, Bytes, !StringTable) :-
    Case = case_rep(MainConsId, OtherConsIds, Goal),
    encode_goal_rep(Info, Goal, GoalBytes, !StringTable),
    encode_cons_id_and_arity_rep(MainConsId, MainConsIdBytes, !StringTable),
    map_foldl(encode_cons_id_and_arity_rep,
        OtherConsIds, OtherConsIdsByteLists, !StringTable),
    Bytes = MainConsIdBytes ++ encode_length_func(OtherConsIds) ++
        list.condense(OtherConsIdsByteLists) ++ GoalBytes.

:- pred lhs_final_is_ground(prog_rep_info::in, unify_mode::in) is semidet.

lhs_final_is_ground(Info, UnifyMode) :-
    UnifyMode = unify_modes_li_lf_ri_rf(_, LHSFinalInst, _, _),
    inst_is_ground(Info ^ pri_module_info, LHSFinalInst).

:- pred rhs_is_input(prog_rep_info::in, unify_mode::in) is semidet.

rhs_is_input(Info, UnifyMode) :-
    UnifyMode = unify_modes_li_lf_ri_rf(_, _, RHSInitInst, _),
    init_inst_is_input(Info ^ pri_module_info, RHSInitInst).

:- pred filter_input_args(prog_rep_info::in, list(unify_mode)::in,
    list(prog_var)::in, list(maybe(prog_var))::out) is det.

filter_input_args(_, [], [], []).
filter_input_args(_, [], [_ | _], _) :-
    unexpected($pred, "mismatched lists").
filter_input_args(_, [_ | _], [], _) :-
    unexpected($pred, "mismatched lists").
filter_input_args(Info, [Mode | Modes], [Var | Vars],
        [MaybeVar | MaybeVars]) :-
    ( if rhs_is_input(Info, Mode) then
        MaybeVar = yes(Var)
    else
        MaybeVar = no
    ),
    filter_input_args(Info, Modes, Vars, MaybeVars).

%-----------------------------------------------------------------------------%

:- pred goal_info_to_atomic_goal_rep_fields(hlds_goal_info::in, instmap::in,
    prog_rep_info::in, string::out, int::out, list(prog_var)::out) is det.

goal_info_to_atomic_goal_rep_fields(GoalInfo, Instmap0, Info, FileName, LineNo,
        BoundVars) :-
    Context = goal_info_get_context(GoalInfo),
    term.context_file(Context, FileName0),
    ( if FileName0 = Info ^ pri_filename then
        FileName = ""
    else
        FileName = FileName0
    ),
    term.context_line(Context, LineNo),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
    instmap_delta_changed_vars(InstmapDelta, ChangedVarsSet),
    set_of_var.to_sorted_list(ChangedVarsSet, ChangedVars),
    ModuleInfo = Info ^ pri_module_info,
    list.negated_filter(var_is_ground_in_instmap(ModuleInfo, Instmap0),
        ChangedVars, BoundVars).

:- pred encode_cons_id_and_arity_rep(cons_id_arity_rep::in, list(int)::out,
    string_table_info::in, string_table_info::out) is det.

encode_cons_id_and_arity_rep(ConsIdArity, ConsIdBytes, !StringTable) :-
    ConsIdArity = cons_id_arity_rep(ConsId, Arity),
    encode_string_as_table_offset(ConsId, FunctorBytes, !StringTable),
    encode_short_det(Arity, ArityBytes),
    ConsIdBytes = FunctorBytes ++ ArityBytes.

:- func cons_id_rep(cons_id) = string.

cons_id_rep(cons(SymName, _, _)) =
    prog_rep.sym_base_name_to_string(SymName).
cons_id_rep(tuple_cons(_)) = "{}".
cons_id_rep(some_int_const(IntConst)) = Str :-
    int_const_to_string_and_suffix(IntConst, Str, _Suffix).
cons_id_rep(float_const(Float)) = string.float_to_string(Float).
cons_id_rep(char_const(Char)) = string.char_to_string(Char).
cons_id_rep(string_const(String)) = """" ++ String ++ """".
cons_id_rep(impl_defined_const(IDCKind)) =
    impl_defined_const_kind_to_str(IDCKind).
cons_id_rep(closure_cons(_, _)) = "$closure_cons".
cons_id_rep(type_ctor_info_const(_, _, _)) = "$type_ctor_info_const".
cons_id_rep(base_typeclass_info_const(_, _, _, _)) =
    "$base_typeclass_info_const".
cons_id_rep(type_info_cell_constructor(_)) = "$type_info_cell_constructor".
cons_id_rep(typeclass_info_cell_constructor) =
    "$typeclass_info_cell_constructor".
cons_id_rep(type_info_const(_)) = "$type_info_const".
cons_id_rep(typeclass_info_const(_)) = "$typeclass_info_const".
cons_id_rep(ground_term_const(_, _)) = "$ground_term_const".
cons_id_rep(tabling_info_const(_)) = "$tabling_info_const".
cons_id_rep(table_io_entry_desc(_)) = "$table_io_entry_desc".
cons_id_rep(deep_profiling_proc_layout(_)) = "$deep_profiling_proc_layout".

:- func sym_base_name_to_string(sym_name) = string.

sym_base_name_to_string(unqualified(String)) = String.
sym_base_name_to_string(qualified(_, String)) = String.

%-----------------------------------------------------------------------------%

% The operations to convert primitive constructs to bytecode.
%
% We use the operations defined in bytecode_data. Each of the functions below
% stands for a given primitive construct. If we need to expand the number of
% bytes we use to represent one of these, it should be sufficient to change
% the number of bits here and in mdbcomp/program_representation.m.
%
% Warning: the predicates we use from bytecode_data deal with signed integers,
% but we here use them to represent unsigned quantities. This effectively
% halves their range.

:- pred encode_string_as_table_offset(string::in, list(int)::out,
    string_table_info::in, string_table_info::out) is det.

encode_string_as_table_offset(String, Bytes, !StringTable) :-
    lookup_string_in_table(String, Index, !StringTable),
    encode_int32_det(Index, Bytes).

:- pred encode_type_as_table_ref(mer_type::in, list(int)::out,
    string_table_info::in, string_table_info::out,
    type_table_info::in, type_table_info::out) is det.

encode_type_as_table_ref(Type, Bytes, !StringTable, !TypeTable) :-
    lookup_type_in_table(Type, Index, !StringTable, !TypeTable),
    encode_num_det(Index, Bytes).

:- func encode_var_reps_func(prog_rep_info, list(var_rep)) = list(int).

encode_var_reps_func(Info, Vars) =
    encode_length_func(Vars) ++
    list.condense(list.map(encode_var_rep_func(Info), Vars)).

:- func var_to_var_rep(prog_rep_info, prog_var) = int.

var_to_var_rep(Info, Var) = Num :-
    map.lookup(Info ^ pri_var_num_map, Var, Num - _).

:- func encode_var_rep_func(prog_rep_info, var_rep) = list(int).

encode_var_rep_func(Info, Var) = Bytes :-
    VarNumRep = Info ^ pri_var_num_rep,
    (
        VarNumRep = var_num_1_byte,
        Bytes = [Var]
    ;
        VarNumRep = var_num_2_bytes,
        encode_short_det(Var, Bytes)
    ;
        VarNumRep = var_num_4_bytes,
        encode_int32_det(Var, Bytes)
    ).

:- func encode_maybe_var_reps_func(prog_rep_info, list(maybe(var_rep))) =
    list(int).

encode_maybe_var_reps_func(Info, Vars) =
    encode_length_func(Vars) ++
    list.condense(list.map(encode_maybe_var_rep_func(Info), Vars)).

:- func encode_maybe_var_rep_func(prog_rep_info, maybe(var_rep)) = list(int).

encode_maybe_var_rep_func(Info, MaybeVar) = Bytes :-
    % This is not the most efficient representation, however maybe(var_rep)s
    % are only used for partial unifications, which are rare.
    (
        MaybeVar = yes(Var),
        Bytes = [1 | encode_var_rep_func(Info, Var)]
    ;
        MaybeVar = no,
        Bytes = [0]
    ).

:- func encode_head_vars_func(prog_rep_info, instmap, instmap_delta,
    list(prog_var)) = list(int).

encode_head_vars_func(Info, InitialInstmap, InstmapDelta, Vars) =
    encode_length_func(Vars) ++
    list.condense(list.map(
        encode_head_var_func(Info, InitialInstmap, InstmapDelta), Vars)).

:- func encode_head_var_func(prog_rep_info, instmap, instmap_delta,
    prog_var) = list(int).

encode_head_var_func(Info, InitialInstmap, InstmapDelta, Var) = Bytes :-
    encode_var_rep_func(Info, var_to_var_rep(Info, Var)) = VarBytes,
    ModuleInfo = Info ^ pri_module_info,
    instmap_lookup_var(InitialInstmap, Var, InitialInst),
    ( if instmap_delta_search_var(InstmapDelta, Var, FinalInstPrime) then
        FinalInst = FinalInstPrime
    else
        % If the variable is not in the instmap delta, then its instantiation
        % cannot possibly change.
        FinalInst = InitialInst
    ),
    Bytes = VarBytes ++ [inst_to_byte(ModuleInfo, InitialInst),
        inst_to_byte(ModuleInfo, FinalInst)].

:- func inst_to_byte(module_info, mer_inst) = int.

inst_to_byte(ModuleInfo, MerInst) = Byte :-
    ( if
        ( MerInst = free
        ; MerInst = free(_)
        )
    then
        InstRep = ir_free_rep
    else if
        inst_is_ground(ModuleInfo, MerInst)
    then
        InstRep = ir_ground_rep
    else
        InstRep = ir_other_rep
    ),
    inst_representation(InstRep, Byte).

:- func encode_length_func(list(T)) = list(int).

encode_length_func(List) = Bytes :-
    encode_int32_det(list.length(List), Bytes).

:- func encode_lineno_func(int) = list(int).

encode_lineno_func(VarNum) = Bytes :-
    encode_int32_det(VarNum, Bytes).

:- func encode_method_num_func(int) = list(int).

encode_method_num_func(VarNum) = Bytes :-
    encode_short_det(VarNum, Bytes).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.prog_rep.
%-----------------------------------------------------------------------------%
