%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%---------------------------------------------------------------------------%

:- module ll_backend.prog_rep.
:- interface.

:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.instmap.
:- import_module ll_backend.stack_layout.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module pair.

%---------------------------------------------------------------------------%

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

    % Describe whether a variable name table should be included in the
    % bytecode.  The variable name table actually adds the strings into the
    % module's string table.
    %
:- type include_variable_table
    --->    include_variable_table
    ;       do_not_include_variable_table.

    % Create the bytecodes for the given procedure.
    %
:- pred represent_proc_as_bytecodes(list(prog_var)::in, hlds_goal::in,
    instmap::in, vartypes::in, var_num_map::in, module_info::in,
    include_variable_table::in, determinism::in, 
    string_table::in, string_table::out, list(int)::out) is det.

%---------------------------------------------------------------------------%

:- type prog_rep_info
    --->    prog_rep_info(
                pri_filename            :: string,
                pri_vartypes            :: vartypes,
                pri_var_num_map         :: var_num_map,
                pri_var_num_rep         :: var_num_rep,
                pri_module_info         :: module_info,
                pri_flatten_par_conjs   :: flatten_par_conjs
            ).

:- type flatten_par_conjs
    --->    flatten_par_conjs
    ;       expect_no_par_conjs.

:- pred goal_to_goal_rep(prog_rep_info::in, instmap::in, hlds_goal::in, 
    goal_rep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.bytecode_data.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module unit.

%---------------------------------------------------------------------------%

represent_proc_as_bytecodes(HeadVars, Goal, InstMap0, VarTypes, VarNumMap,
        ModuleInfo, IncludeVarTable, ProcDetism, !StringTable, ProcRepBytes) :-
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    term.context_file(Context, FileName),
    represent_var_table_as_bytecode(IncludeVarTable, VarNumMap, VarNumRep,
        VarTableBytes, !StringTable),
    Info = prog_rep_info(FileName, VarTypes, VarNumMap, VarNumRep, ModuleInfo,
        expect_no_par_conjs),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),

    string_to_byte_list(FileName, FileNameBytes, !StringTable),
    goal_to_byte_list(Goal, InstMap0, Info, GoalBytes, !StringTable),
    DetismByte = represent_determinism(ProcDetism),
    ProcRepBytes0 = FileNameBytes ++ VarTableBytes ++
        head_vars_to_byte_list(Info, InstMap0, InstmapDelta, HeadVars) ++
        GoalBytes ++ [DetismByte],
    int32_to_byte_list(list.length(ProcRepBytes0) + 4, LimitBytes),
    ProcRepBytes = LimitBytes ++ ProcRepBytes0.

%---------------------------------------------------------------------------%

    % Create bytecodes for the variable table.
    %
    % If a variable table is not requested, an empty table is created.
    % The variable table also includes information about the representation
    % of variable numbers within the bytecode.
    %
    % The representation of variables and the variable table restricts the
    % number of possible variables in a procedure to 2^31.
    %
:- pred represent_var_table_as_bytecode(include_variable_table::in,
    var_num_map::in, var_num_rep::out, list(int)::out, 
    string_table::in, string_table::out) is det.

represent_var_table_as_bytecode(IncludeVarTable, VarNumMap, VarNumRep,
        ByteList, !StringTable) :-
    map.foldl(max_var_num, VarNumMap, 0) = MaxVarNum,
    ( MaxVarNum =< 127 ->
        VarNumRep = var_num_1_byte
    ; MaxVarNum =< 32767 ->
        VarNumRep = var_num_2_bytes
    ;
        VarNumRep = var_num_4_bytes
    ),
    var_num_rep_byte(VarNumRep, VarNumRepByte),
    (
        IncludeVarTable = include_variable_table,
        map.foldl3(var_table_entry_bytelist(VarNumRep), VarNumMap, 0, NumVars, 
            [], VarTableEntriesBytes, !StringTable)
    ;
        IncludeVarTable = do_not_include_variable_table,
        NumVars = 0,
        VarTableEntriesBytes = []
    ),
    int32_to_byte_list(NumVars, NumVarsBytes),
    ByteList = [VarNumRepByte] ++ NumVarsBytes ++ VarTableEntriesBytes.

:- func max_var_num(prog_var, pair(int, string), int) = int.

max_var_num(_, VarNum1 - _, VarNum2) = Max :-
    Max = max(VarNum1, VarNum2).

:- pred var_table_entry_bytelist(var_num_rep::in, 
    prog_var::in, pair(int, string)::in, int::in, int::out, 
    list(int)::in, list(int)::out,
    string_table::in, string_table::out) is det.

var_table_entry_bytelist(VarNumRep, _ProgVar, VarNum - VarName, 
        !NumVars, !VarTableBytes, !StringTable) :-
    (
        % Some variables that the compiler creates are named automatically,
        % these and unnamed variables should not be included in the variable
        % table.
        compiler_introduced_varname(VarName)
    ->
        true
    ;
        !:NumVars = !.NumVars + 1,
        (
            VarNumRep = var_num_1_byte,
            VarBytes = [VarNum]
        ;
            VarNumRep = var_num_2_bytes,
            short_to_byte_list(VarNum, VarBytes)
        ;
            VarNumRep = var_num_4_bytes,
            int32_to_byte_list(VarNum, VarBytes)
        ),
        string_to_byte_list(VarName, VarNameBytes, !StringTable),
        !:VarTableBytes = VarBytes ++ VarNameBytes ++ !.VarTableBytes
    ).

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

%---------------------------------------------------------------------------%

:- pred goal_to_byte_list(hlds_goal::in, instmap::in, prog_rep_info::in,
    list(int)::out, string_table::in, string_table::out) is det.

goal_to_byte_list(Goal, InstMap0, Info, Bytes, !StringTable) :-
    goal_to_goal_rep(Info, InstMap0, Goal, GoalRep),
    goal_rep_to_byte_list(Info, GoalRep, Bytes, !StringTable).

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
            expect(unify(ConjType, plain_conj), $module, $pred,
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
        instmap.apply_instmap_delta(Instmap0, InstmapDelta, InstmapAfterCond),
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
        ( InnerDetism = OuterDetism ->
            MaybeCut = scope_is_no_cut 
        ;
            MaybeCut = scope_is_cut
        ),
        GoalExprRep = scope_rep(SubGoalRep, MaybeCut)
    ;
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        (
            GoalExpr = unify(_, _, _, Uni, _),
            (
                Uni = assign(Target, Source),
                AtomicGoalRep = unify_assign_rep(
                    var_to_var_rep(Info, Target), 
                    var_to_var_rep(Info, Source))
            ;
                ( Uni = construct(Var, ConsId, Args, ArgModes, _, _, _)
                ; Uni = deconstruct(Var, ConsId, Args, ArgModes, _, _)
                ),
                VarRep = var_to_var_rep(Info, Var),
                ConsIdRep = cons_id_rep(ConsId),
                ArgsRep = map(var_to_var_rep(Info), Args),
                filter_input_args(Info, ArgModes, Args, MaybeArgs),
                MaybeArgsRep = map(map_maybe(var_to_var_rep(Info)), MaybeArgs),
                (
                    Uni = construct(_, _, _, _, _, _, _),
                    ( list.all_true(lhs_final_is_ground(Info), ArgModes) ->
                        AtomicGoalRep = unify_construct_rep(VarRep, ConsIdRep, 
                            ArgsRep)
                    ;
                        AtomicGoalRep = partial_construct_rep(VarRep, ConsIdRep,
                            MaybeArgsRep)
                    )
                ;
                    Uni = deconstruct(_, _, _, _, _, _),
                    ( list.member(Var, BoundVars) ->
                        AtomicGoalRep = partial_deconstruct_rep(VarRep,
                            ConsIdRep, MaybeArgsRep)
                    ;
                        AtomicGoalRep = unify_deconstruct_rep(VarRep, ConsIdRep,
                            ArgsRep)
                    )
                )
            ;
                Uni = simple_test(Var1, Var2),
                AtomicGoalRep = unify_simple_test_rep(
                    var_to_var_rep(Info, Var1), 
                    var_to_var_rep(Info, Var2))
            ;
                Uni = complicated_unify(_, _, _),
                unexpected($module, $pred, "complicated_unify")
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
                ( ArgsRep = [InputArgRep, OutputArgRep] ->
                    AtomicGoalRep = cast_rep(OutputArgRep, InputArgRep) 
                ;
                    unexpected($module, $pred, "cast arity != 2")
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
            ;
                Builtin = out_of_line_builtin,
                unexpected($module, $pred, "out_of_line_builtin")
            )
        ;
            GoalExpr = call_foreign_proc(_, _PredId, _, Args, _, _, _),
            ArgVarsRep = list.map(
                compose(var_to_var_rep(Info), foreign_arg_var), Args),
            AtomicGoalRep = pragma_foreign_code_rep(ArgVarsRep)
        ),
        goal_info_to_atomic_goal_rep_fields(GoalInfo, Instmap0, Info, 
            FileName, LineNo, BoundVars),
        BoundVarsRep = map(var_to_var_rep(Info), BoundVars),
        GoalExprRep = atomic_goal_rep(FileName, LineNo, BoundVarsRep, 
            AtomicGoalRep)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($module, $pred, "unexpected shorthand")
    ).

:- pred conj_to_conj_rep(prog_rep_info::in, instmap::in, list(hlds_goal)::in,
    list(goal_rep)::out) is det.

conj_to_conj_rep(_, _, [], []).
conj_to_conj_rep(Info, Instmap0, [Conj | Conjs], [ConjRep | ConjReps]) :-
    goal_to_goal_rep(Info, Instmap0, Conj, ConjRep),
    GoalInfo = Conj ^ hlds_goal_info,
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
    instmap.apply_instmap_delta(Instmap0, InstmapDelta, Instmap1),
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

:- pred goal_rep_to_byte_list(prog_rep_info::in, goal_rep::in, 
    list(int)::out, string_table::in, string_table::out) is det.

goal_rep_to_byte_list(Info, goal_rep(GoalExpr, Detism, _), Bytes, !StringTable) :-
    (
        GoalExpr = conj_rep(GoalReps),
        map_foldl(goal_rep_to_byte_list(Info), GoalReps, ConjBytesList,
            !StringTable),
        ExprBytes = [goal_type_to_byte(goal_conj)] ++
            length_to_byte_list(GoalReps) ++ condense(ConjBytesList)
    ;
        GoalExpr = disj_rep(GoalReps),
        map_foldl(goal_rep_to_byte_list(Info), GoalReps, DisjBytesList,
            !StringTable),
        ExprBytes = [goal_type_to_byte(goal_disj)] ++
            length_to_byte_list(GoalReps) ++ condense(DisjBytesList)
    ;
        GoalExpr = negation_rep(SubGoal),
        goal_rep_to_byte_list(Info, SubGoal, SubGoalBytes, !StringTable),
        ExprBytes = [goal_type_to_byte(goal_neg)] ++ SubGoalBytes
    ;
        GoalExpr = ite_rep(Cond, Then, Else), 
        goal_rep_to_byte_list(Info, Cond, CondBytes, !StringTable),
        goal_rep_to_byte_list(Info, Then, ThenBytes, !StringTable),
        goal_rep_to_byte_list(Info, Else, ElseBytes, !StringTable),
        ExprBytes = [goal_type_to_byte(goal_ite)] ++
            CondBytes ++ ThenBytes ++ ElseBytes
    ;
        GoalExpr = atomic_goal_rep(FileName, Line, BoundVars, AtomicGoalRep),
        string_to_byte_list(FileName, FileNameBytes, !StringTable),
        AtomicBytes = FileNameBytes ++ lineno_to_byte_list(Line) ++
            var_reps_to_byte_list(Info, BoundVars), 
        (
            AtomicGoalRep = unify_assign_rep(Target, Source),
            ExprBytes = [goal_type_to_byte(goal_assign)] ++
                var_rep_to_byte_list(Info, Target) ++
                var_rep_to_byte_list(Info, Source) ++
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
                ArgsBytes = var_reps_to_byte_list(Info, Args)
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
                ArgsBytes = maybe_var_reps_to_byte_list(Info, MaybeArgs)
            ),
            string_to_byte_list(ConsId, ConsIdBytes, !StringTable),
            VarBytes = var_rep_to_byte_list(Info, Var),
            ExprBytes = [AtomicTypeByte] ++ VarBytes ++ ConsIdBytes ++
                ArgsBytes ++ AtomicBytes
        ;
            AtomicGoalRep = unify_simple_test_rep(Var1, Var2),
            ExprBytes = [goal_type_to_byte(goal_simple_test)] ++
                var_rep_to_byte_list(Info, Var1) ++
                var_rep_to_byte_list(Info, Var2) ++
                AtomicBytes
        ;
            AtomicGoalRep = higher_order_call_rep(PredVar, Args),
            ExprBytes = [goal_type_to_byte(goal_ho_call)] ++
                var_rep_to_byte_list(Info, PredVar) ++
                var_reps_to_byte_list(Info, Args) ++
                AtomicBytes
        ;
            AtomicGoalRep = method_call_rep(Var, MethodNum, Args),
            ExprBytes = [goal_type_to_byte(goal_method_call)] ++
                var_rep_to_byte_list(Info, Var) ++
                method_num_to_byte_list(MethodNum) ++
                var_reps_to_byte_list(Info, Args) ++
                AtomicBytes
        ;
            AtomicGoalRep = event_call_rep(EventName, Args),
            string_to_byte_list(EventName, EventNameBytes, !StringTable),
            ExprBytes = [goal_type_to_byte(goal_event_call)] ++
                EventNameBytes ++
                var_reps_to_byte_list(Info, Args) ++
                AtomicBytes
        ;
            AtomicGoalRep = cast_rep(Target, Source),
            ExprBytes = [goal_type_to_byte(goal_cast)] ++
                var_rep_to_byte_list(Info, Target) ++
                var_rep_to_byte_list(Info, Source) ++
                AtomicBytes
        ;
            (
                AtomicGoalRep = plain_call_rep(ModuleName, PredName, Args),
                CallType = goal_plain_call
            ;   
                AtomicGoalRep = builtin_call_rep(ModuleName, PredName, Args),
                CallType = goal_builtin_call
            ),
            string_to_byte_list(ModuleName, ModuleNameBytes, !StringTable),
            string_to_byte_list(PredName, PredNameBytes, !StringTable),
            ExprBytes = [goal_type_to_byte(CallType)] ++
                ModuleNameBytes ++
                PredNameBytes ++
                var_reps_to_byte_list(Info, Args) ++
                AtomicBytes
        ;
            AtomicGoalRep = pragma_foreign_code_rep(Args),
            ExprBytes = [goal_type_to_byte(goal_foreign)] ++
                var_reps_to_byte_list(Info, Args) ++ AtomicBytes
        )
    ;
        GoalExpr = switch_rep(SwitchVar, CanFail, Cases),
        map_foldl(case_rep_to_byte_list(Info), Cases, CasesBytesList,
            !StringTable),
        can_fail_byte(CanFail, CanFailByte),
        ExprBytes = [goal_type_to_byte(goal_switch)] ++
            [CanFailByte] ++
            var_rep_to_byte_list(Info, SwitchVar) ++
            length_to_byte_list(Cases) ++ condense(CasesBytesList) 
    ;
        GoalExpr = scope_rep(SubGoal, MaybeCut),
        cut_byte(MaybeCut, MaybeCutByte),
        goal_rep_to_byte_list(Info, SubGoal, SubGoalBytes, !StringTable),  
        ExprBytes = [goal_type_to_byte(goal_scope)] ++ [MaybeCutByte] ++ 
            SubGoalBytes
    ),
    determinism_representation(Detism, DetismByte),
    Bytes = ExprBytes ++ [DetismByte].

:- pred case_rep_to_byte_list(prog_rep_info::in, case_rep::in, list(int)::out,
    string_table::in, string_table::out) is det.

case_rep_to_byte_list(Info, Case, Bytes, !StringTable) :-
    Case = case_rep(MainConsId, OtherConsIds, Goal),
    goal_rep_to_byte_list(Info, Goal, GoalBytes, !StringTable),
    cons_id_and_arity_rep_to_byte_list(MainConsId, MainConsIdBytes,
        !StringTable),
    map_foldl(cons_id_and_arity_rep_to_byte_list, 
        OtherConsIds, OtherConsIdsByteLists, !StringTable),
    Bytes = MainConsIdBytes ++ length_to_byte_list(OtherConsIds) ++
        condense(OtherConsIdsByteLists) ++ GoalBytes.

:- pred lhs_final_is_ground(prog_rep_info::in, uni_mode::in) is semidet.

lhs_final_is_ground(Info, (_ - _) -> (LHSFinalInst - _)) :-
    inst_is_ground(Info ^ pri_module_info, LHSFinalInst).

:- pred rhs_is_input(prog_rep_info::in, uni_mode::in) is semidet.

rhs_is_input(Info, (_ - RHSInitialInst) -> (_ - RHSFinalInst)) :-
    mode_is_input(Info ^ pri_module_info, RHSInitialInst -> RHSFinalInst).

:- pred filter_input_args(prog_rep_info::in, list(uni_mode)::in,
    list(prog_var)::in, list(maybe(prog_var))::out) is det.

filter_input_args(_, [], [], []).
filter_input_args(Info, [Mode | Modes], [Var | Vars],
        [MaybeVar | MaybeVars]) :-
    ( rhs_is_input(Info, Mode) ->
        MaybeVar = yes(Var)
    ;
        MaybeVar = no
    ),
    filter_input_args(Info, Modes, Vars, MaybeVars).
filter_input_args(_, [], [_ | _], _) :-
    unexpected($module, $pred, "mismatched lists").
filter_input_args(_, [_ | _], [], _) :-
    unexpected($module, $pred, "mismatched lists").

%---------------------------------------------------------------------------%

:- pred goal_info_to_atomic_goal_rep_fields(hlds_goal_info::in, instmap::in,
    prog_rep_info::in, string::out, int::out, list(prog_var)::out) is det.

goal_info_to_atomic_goal_rep_fields(GoalInfo, Instmap0, Info, FileName, LineNo,
        BoundVars) :-
    Context = goal_info_get_context(GoalInfo),
    term.context_file(Context, FileName0),
    ( FileName0 = Info ^ pri_filename ->
        FileName = ""
    ;
        FileName = FileName0
    ),
    term.context_line(Context, LineNo),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),
    instmap.apply_instmap_delta(Instmap0, InstmapDelta, Instmap),
    instmap_changed_vars(Instmap0, Instmap, Info ^ pri_vartypes,
        Info ^ pri_module_info, ChangedVars),
    set_of_var.to_sorted_list(ChangedVars, BoundVars).

:- pred cons_id_and_arity_rep_to_byte_list(cons_id_arity_rep::in, 
    list(int)::out, string_table::in, string_table::out) is det.

cons_id_and_arity_rep_to_byte_list(ConsIdArity, ConsIdBytes, !StringTable) :-
    ConsIdArity = cons_id_arity_rep(ConsId, Arity),
    string_to_byte_list(ConsId, FunctorBytes, !StringTable),
    short_to_byte_list(Arity, ArityBytes),
    ConsIdBytes = FunctorBytes ++ ArityBytes.

:- pred cons_id_to_byte_list(cons_id::in, list(int)::out,
    string_table::in, string_table::out) is det.

cons_id_to_byte_list(SymName, Bytes, !StringTable) :-
    string_to_byte_list(cons_id_rep(SymName), Bytes, !StringTable).

:- func cons_id_rep(cons_id) = string.

cons_id_rep(cons(SymName, _, _)) =
    prog_rep.sym_base_name_to_string(SymName).
cons_id_rep(tuple_cons(_)) = "{}".
cons_id_rep(int_const(Int)) = string.int_to_string(Int).
cons_id_rep(float_const(Float)) = string.float_to_string(Float).
cons_id_rep(char_const(Char)) = string.char_to_string(Char).
cons_id_rep(string_const(String)) = """" ++ String ++ """".
cons_id_rep(impl_defined_const(Name)) = "$" ++ Name.
cons_id_rep(closure_cons(_, _)) = "$closure_cons".
cons_id_rep(type_ctor_info_const(_, _, _)) = "$type_ctor_info_const".
cons_id_rep(base_typeclass_info_const(_, _, _, _)) =
    "$base_typeclass_info_const".
cons_id_rep(type_info_cell_constructor(_)) = "$type_info_cell_constructor".
cons_id_rep(typeclass_info_cell_constructor) =
    "$typeclass_info_cell_constructor".
cons_id_rep(type_info_const(_)) = "$type_info_const".
cons_id_rep(typeclass_info_const(_)) = "$typeclass_info_const".
cons_id_rep(tabling_info_const(_)) = "$tabling_info_const".
cons_id_rep(table_io_decl(_)) = "$table_io_decl".
cons_id_rep(deep_profiling_proc_layout(_)) = "$deep_profiling_proc_layout".

:- func sym_base_name_to_string(sym_name) = string.

sym_base_name_to_string(unqualified(String)) = String.
sym_base_name_to_string(qualified(_, String)) = String.

%---------------------------------------------------------------------------%

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

:- pred string_to_byte_list(string::in, list(int)::out,
    string_table::in, string_table::out) is det.

string_to_byte_list(String, Bytes, !StringTable) :-
    stack_layout.lookup_string_in_table(String, Index, !StringTable),
    int32_to_byte_list(Index, Bytes).

:- func var_reps_to_byte_list(prog_rep_info, list(var_rep)) = list(int).

var_reps_to_byte_list(Info, Vars) =
    length_to_byte_list(Vars) ++
    list.condense(list.map(var_rep_to_byte_list(Info), Vars)).

:- func var_to_var_rep(prog_rep_info, prog_var) = int.

var_to_var_rep(Info, Var) = Num :-
    map.lookup(Info ^ pri_var_num_map, Var, Num - _).

:- func var_rep_to_byte_list(prog_rep_info, var_rep) = list(int).

var_rep_to_byte_list(Info, Var) = Bytes :-
    (
        Info ^ pri_var_num_rep = var_num_1_byte,
        Bytes = [Var]
    ; 
        Info ^ pri_var_num_rep = var_num_2_bytes,
        short_to_byte_list(Var, Bytes)
    ; 
        Info ^ pri_var_num_rep = var_num_4_bytes,
        int32_to_byte_list(Var, Bytes)
    ).

:- func maybe_var_reps_to_byte_list(prog_rep_info, list(maybe(var_rep))) =
    list(int).

maybe_var_reps_to_byte_list(Info, Vars) =
    length_to_byte_list(Vars) ++
    list.condense(list.map(maybe_var_rep_to_byte_list(Info), Vars)).

:- func maybe_var_rep_to_byte_list(prog_rep_info, maybe(var_rep)) = list(int).

maybe_var_rep_to_byte_list(Info, MaybeVar) = Bytes :-
    % This is not the most efficient representation, however maybe(var_rep)s
    % are only used for partial unifications which are rare.
    (
        MaybeVar = yes(Var),
        Bytes = [1 | var_rep_to_byte_list(Info, Var)]
    ;
        MaybeVar = no,
        Bytes = [0]
    ).

:- func head_vars_to_byte_list(prog_rep_info, instmap, instmap_delta,
    list(prog_var)) = list(int).

head_vars_to_byte_list(Info, InitialInstmap, InstmapDelta, Vars) =
    length_to_byte_list(Vars) ++
    list.condense(list.map(
        head_var_to_byte_list(Info, InitialInstmap, InstmapDelta), Vars)).

:- func head_var_to_byte_list(prog_rep_info, instmap, instmap_delta,
    prog_var) = list(int).

head_var_to_byte_list(Info, InitialInstmap, InstmapDelta, Var) = Bytes :-
    var_rep_to_byte_list(Info, var_to_var_rep(Info, Var)) = VarBytes,
    ModuleInfo = Info ^ pri_module_info,
    instmap_lookup_var(InitialInstmap, Var, InitialInst),
    ( instmap_delta_search_var(InstmapDelta, Var, FinalInstPrime) ->
        FinalInst = FinalInstPrime
    ;
        % if the variable is not in the instmap delta, then its instantiation
        % cannot possibly change.  It has the same instantiation that it begun
        % with.
        FinalInst = InitialInst
    ),
    Bytes = VarBytes ++ [inst_to_byte(ModuleInfo, InitialInst),
        inst_to_byte(ModuleInfo, FinalInst)].

:- func inst_to_byte(module_info, mer_inst) = int.

inst_to_byte(ModuleInfo, MerInst) = Byte :-
    (
        ( MerInst = free
        ; MerInst = free(_)
        )
    ->
        InstRep = ir_free_rep
    ;
        inst_is_ground(ModuleInfo, MerInst)
    ->
        InstRep = ir_ground_rep
    ;
        InstRep = ir_other_rep
    ),
    inst_representation(InstRep, Byte).

:- func length_to_byte_list(list(T)) = list(int).

length_to_byte_list(List) = Bytes :-
    int32_to_byte_list(list.length(List), Bytes).

:- func lineno_to_byte_list(int) = list(int).

lineno_to_byte_list(VarNum) = Bytes :-
    int32_to_byte_list(VarNum, Bytes).

:- func method_num_to_byte_list(int) = list(int).

method_num_to_byte_list(VarNum) = Bytes :-
    short_to_byte_list(VarNum, Bytes).

%---------------------------------------------------------------------------%
:- end_module ll_backend.prog_rep.
%---------------------------------------------------------------------------%
