%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2009 University of Melbourne.
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
    stack_layout_info::in, stack_layout_info::out, list(int)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.bytecode_data.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module maybe.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

:- type prog_rep_info
    --->    prog_rep_info(
                pri_filename    :: string,
                pri_vartypes    :: vartypes,
                pri_var_num_map :: var_num_map,
                pri_var_num_rep :: var_num_rep,
                pri_module_info :: module_info
            ).

represent_proc_as_bytecodes(HeadVars, Goal, InstMap0, VarTypes, VarNumMap,
        ModuleInfo, IncludeVarTable, ProcDetism, !StackInfo, ProcRepBytes) :-
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),
    term.context_file(Context, FileName),
    represent_var_table_as_bytecode(IncludeVarTable, VarNumMap, VarNumRep,
        VarTableBytes, !StackInfo),
    Info = prog_rep_info(FileName, VarTypes, VarNumMap, VarNumRep, ModuleInfo),
    InstmapDelta = goal_info_get_instmap_delta(GoalInfo),

    string_to_byte_list(FileName, FileNameBytes, !StackInfo),
    goal_to_byte_list(Goal, InstMap0, Info, GoalBytes, !StackInfo),
    DetismByte = represent_determinism(ProcDetism),
    ProcRepBytes0 = FileNameBytes ++ VarTableBytes ++
        head_vars_to_byte_list(Info, InstMap0, InstmapDelta, HeadVars) ++
        GoalBytes ++ [DetismByte],
    int32_to_byte_list(list.length(ProcRepBytes0) + 4, LimitBytes),
    ProcRepBytes = LimitBytes ++ ProcRepBytes0.

%---------------------------------------------------------------------------%

    % Create bytecodes for the variable table.
    %
    % If a variable table is not requested, an empty table is created.  The
    % variable table also includes information about the representation of
    % variable numbers within the bytecode.
    %
    % The representation of variables and the variable table restricts the
    % number of possible variables in a procedure to 2^15.
    %
:- pred represent_var_table_as_bytecode(include_variable_table::in,
    var_num_map::in, var_num_rep::out, list(int)::out, 
    stack_layout_info::in, stack_layout_info::out) is det.

represent_var_table_as_bytecode(IncludeVarTable, VarNumMap, VarNumRep,
        ByteList, !StackInfo) :-
    map.foldl(max_var_num, VarNumMap, 0) = MaxVarNum,
    ( MaxVarNum =< 255 ->
        VarNumRep = byte
    ;
        VarNumRep = short
    ),
    var_num_rep_byte(VarNumRep, VarNumRepByte),
    (
        IncludeVarTable = include_variable_table,
        map.foldl3(var_table_entry_bytelist(VarNumRep), VarNumMap, 0, NumVars, 
            [], VarTableEntriesBytes, !StackInfo)
    ;
        IncludeVarTable = do_not_include_variable_table,
        NumVars = 0,
        VarTableEntriesBytes = []
    ),
    short_to_byte_list(NumVars, NumVarsBytes),
    ByteList = [VarNumRepByte] ++ NumVarsBytes ++ VarTableEntriesBytes.

:- func max_var_num(prog_var, pair(int, string), int) = int.

max_var_num(_, VarNum1 - _, VarNum2) = Max :-
    Max = max(VarNum1, VarNum2).

:- pred var_table_entry_bytelist(var_num_rep::in, 
    prog_var::in, pair(int, string)::in, int::in, int::out, 
    list(int)::in, list(int)::out,
    stack_layout_info::in, stack_layout_info::out) is det.

var_table_entry_bytelist(VarNumRep, _ProgVar, VarNum - VarName, 
        !NumVars, !VarTableBytes, !StackInfo) :-
    (
        % Some variables that the compiler creates are named automatically,
        % these and unamed variables should not be included in the variable
        % table.
        compiler_introduced_varname(VarName)
    ->
        true
    ;
        !:NumVars = !.NumVars + 1,
        (
            VarNumRep = byte,
            VarBytes = [VarNum]
        ;
            VarNumRep = short,
            short_to_byte_list(VarNum, VarBytes)
        ),
        string_to_byte_list(VarName, VarNameBytes, !StackInfo),
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
    ),
    prefix(VarName, Prefix).

%---------------------------------------------------------------------------%

:- pred goal_to_byte_list(hlds_goal::in, instmap::in, prog_rep_info::in,
    list(int)::out, stack_layout_info::in, stack_layout_info::out) is det.

goal_to_byte_list(hlds_goal(GoalExpr, GoalInfo), InstMap0, Info, Bytes,
        !StackInfo) :-
    (
        GoalExpr = conj(ConjType, Goals),
        expect(unify(ConjType, plain_conj), this_file,
            "non-plain conjunction and declarative debugging"),
        conj_to_byte_list(Goals, InstMap0, Info, ConjBytes, !StackInfo),
        ExprBytes = [goal_type_to_byte(goal_conj)] ++
            length_to_byte_list(Goals) ++ ConjBytes
    ;
        GoalExpr = disj(Goals),
        disj_to_byte_list(Goals, InstMap0, Info, DisjBytes, !StackInfo),
        ExprBytes = [goal_type_to_byte(goal_disj)] ++
            length_to_byte_list(Goals) ++ DisjBytes
    ;
        GoalExpr = negation(SubGoal),
        goal_to_byte_list(SubGoal, InstMap0, Info, SubGoalBytes, !StackInfo),
        ExprBytes = [goal_type_to_byte(goal_neg)] ++ SubGoalBytes
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        Cond = hlds_goal(_, CondGoalInfo),
        InstMapDelta = goal_info_get_instmap_delta(CondGoalInfo),
        instmap.apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
        goal_to_byte_list(Cond, InstMap0, Info, CondBytes, !StackInfo),
        goal_to_byte_list(Then, InstMap1, Info, ThenBytes, !StackInfo),
        goal_to_byte_list(Else, InstMap0, Info, ElseBytes, !StackInfo),
        ExprBytes = [goal_type_to_byte(goal_ite)] ++
            CondBytes ++ ThenBytes ++ ElseBytes
    ;
        GoalExpr = unify(_, _, _, Uni, _),
        atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info,
            AtomicBytes, BoundVars, !StackInfo),
        (
            Uni = assign(Target, Source),
            ExprBytes = [goal_type_to_byte(goal_assign)] ++
                var_to_byte_list(Info, Target) ++
                var_to_byte_list(Info, Source) ++
                AtomicBytes
        ;
            Uni = construct(Var, ConsId, Args, ArgModes, _, _, _),
            cons_id_to_byte_list(ConsId, ConsIdBytes, !StackInfo),
            ( list.all_true(lhs_final_is_ground(Info), ArgModes) ->
                ExprBytes = [goal_type_to_byte(goal_construct)] ++
                    var_to_byte_list(Info, Var) ++
                    ConsIdBytes ++
                    vars_to_byte_list(Info, Args) ++
                    AtomicBytes
            ;
                filter_input_args(Info, ArgModes, Args, MaybeArgs),
                ExprBytes = [goal_type_to_byte(goal_partial_construct)] ++
                    var_to_byte_list(Info, Var) ++
                    ConsIdBytes ++
                    maybe_vars_to_byte_list(Info, MaybeArgs) ++
                    AtomicBytes
            )
        ;
            Uni = deconstruct(Var, ConsId, Args, ArgModes, _, _),
            cons_id_to_byte_list(ConsId, ConsIdBytes, !StackInfo),
            ( list.member(Var, BoundVars) ->
                filter_input_args(Info, ArgModes, Args, MaybeArgs),
                ExprBytes = [goal_type_to_byte(goal_partial_deconstruct)]++
                    var_to_byte_list(Info, Var) ++
                    ConsIdBytes ++
                    maybe_vars_to_byte_list(Info, MaybeArgs) ++
                    AtomicBytes
            ;
                ExprBytes = [goal_type_to_byte(goal_deconstruct)] ++
                    var_to_byte_list(Info, Var) ++
                    ConsIdBytes ++
                    vars_to_byte_list(Info, Args) ++
                    AtomicBytes
            )
        ;
            Uni = simple_test(Var1, Var2),
            ExprBytes = [goal_type_to_byte(goal_simple_test)] ++
                var_to_byte_list(Info, Var1) ++
                var_to_byte_list(Info, Var2) ++
                AtomicBytes
        ;
            Uni = complicated_unify(_, _, _),
            unexpected(this_file, "goal_expr_to_byte_list: complicated_unify")
        )
    ;
        GoalExpr = switch(SwitchVar, CanFail, Cases),
        cases_to_byte_list(Cases, InstMap0, Info, CasesBytes, !StackInfo),
        CanFailByte = can_fail_to_byte(CanFail),
        ExprBytes = [goal_type_to_byte(goal_switch)] ++
            [CanFailByte] ++
            var_to_byte_list(Info, SwitchVar) ++
            length_to_byte_list(Cases) ++ CasesBytes
    ;
        GoalExpr = scope(_, SubGoal),
        SubGoal = hlds_goal(_, SuboalInfo),
        OuterDetism = goal_info_get_determinism(GoalInfo),
        InnerDetism = goal_info_get_determinism(SuboalInfo),
        ( InnerDetism = OuterDetism ->
            MaybeCut = 0
        ;
            MaybeCut = 1
        ),
        goal_to_byte_list(SubGoal, InstMap0, Info, GoalBytes, !StackInfo),
        ExprBytes = [goal_type_to_byte(goal_scope)] ++ [MaybeCut] ++ GoalBytes
    ;
        GoalExpr = generic_call(GenericCall, Args, _, _),
        atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info,
            AtomicBytes, _BoundVars, !StackInfo),
        (
            GenericCall = higher_order(PredVar, _, _, _),
            ExprBytes = [goal_type_to_byte(goal_ho_call)] ++
                var_to_byte_list(Info, PredVar) ++
                vars_to_byte_list(Info, Args) ++
                AtomicBytes
        ;
            GenericCall = class_method(Var, MethodNum, _, _),
            ExprBytes = [goal_type_to_byte(goal_method_call)] ++
                var_to_byte_list(Info, Var) ++
                method_num_to_byte_list(MethodNum) ++
                vars_to_byte_list(Info, Args) ++
                AtomicBytes
        ;
            GenericCall = event_call(EventName),
            string_to_byte_list(EventName, EventNameBytes, !StackInfo),
            ExprBytes = [goal_type_to_byte(goal_event_call)] ++
                EventNameBytes ++
                vars_to_byte_list(Info, Args) ++
                AtomicBytes
        ;
            GenericCall = cast(_),
            ( Args = [InputArg, OutputArg] ->
                ExprBytes = [goal_type_to_byte(goal_cast)] ++
                    var_to_byte_list(Info, OutputArg) ++
                    var_to_byte_list(Info, InputArg) ++
                    AtomicBytes
            ;
                unexpected(this_file, "goal_expr_to_byte_list: cast arity != 2")
            )
        )
    ;
        GoalExpr = plain_call(PredId, _, Args, Builtin, _, _),
        atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info,
            AtomicBytes, _BoundVars, !StackInfo),
        module_info_pred_info(Info ^ pri_module_info, PredId, PredInfo),
        ModuleSymName = pred_info_module(PredInfo),
        ModuleName = sym_name_to_string(ModuleSymName),
        PredName = pred_info_name(PredInfo),
        string_to_byte_list(ModuleName, ModuleNameBytes, !StackInfo),
        string_to_byte_list(PredName, PredNameBytes, !StackInfo),
        (
            Builtin = not_builtin,
            ExprBytes = [goal_type_to_byte(goal_plain_call)] ++
                ModuleNameBytes ++
                PredNameBytes ++
                vars_to_byte_list(Info, Args) ++
                AtomicBytes
        ;
            Builtin = inline_builtin,
            ExprBytes = [goal_type_to_byte(goal_builtin_call)] ++
                ModuleNameBytes ++
                PredNameBytes ++
                vars_to_byte_list(Info, Args) ++
                AtomicBytes
        ;
            Builtin = out_of_line_builtin,
            unexpected(this_file,
                "goal_expr_to_byte_list: out_of_line_builtin")
        )
    ;
        GoalExpr = call_foreign_proc(_, _PredId, _, Args, _, _, _),
        ArgVars = list.map(foreign_arg_var, Args),
        atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info,
            AtomicBytes, _BoundVars, !StackInfo),
        ExprBytes = [goal_type_to_byte(goal_foreign)] ++
            vars_to_byte_list(Info, ArgVars) ++ AtomicBytes
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected(this_file, "goal_expr_to_byte_list: unexpected shorthand")
    ),
    
    % Include determinism in the bytecode.
    Detism = goal_info_get_determinism(GoalInfo),
    DetismByte = represent_determinism(Detism),

    Bytes = ExprBytes ++ [DetismByte].

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
    unexpected(this_file, "filter_input_args: mismatched lists").
filter_input_args(_, [_ | _], [], _) :-
    unexpected(this_file, "filter_input_args: mismatched lists").

%---------------------------------------------------------------------------%

:- pred atomic_goal_info_to_byte_list(hlds_goal_info::in, instmap::in,
    prog_rep_info::in, list(int)::out, list(prog_var)::out,
    stack_layout_info::in, stack_layout_info::out) is det.

atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info, Bytes, BoundVars,
        !StackInfo) :-
    Context = goal_info_get_context(GoalInfo),
    term.context_file(Context, FileName0),
    ( FileName0 = Info ^ pri_filename ->
        FileName = ""
    ;
        FileName = FileName0
    ),
    term.context_line(Context, LineNo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
    instmap_changed_vars(InstMap0, InstMap, Info ^ pri_vartypes,
        Info ^ pri_module_info, ChangedVars),
    set.to_sorted_list(ChangedVars, BoundVars),
    string_to_byte_list(FileName, FileNameBytes, !StackInfo),
    Bytes = FileNameBytes ++
        lineno_to_byte_list(LineNo) ++
        vars_to_byte_list(Info, BoundVars).

:- pred cons_id_and_arity_to_byte_list(cons_id::in, list(int)::out,
    stack_layout_info::in, stack_layout_info::out) is det.

cons_id_and_arity_to_byte_list(ConsId, ConsIdBytes, !StackInfo) :-
    cons_id_to_byte_list(ConsId, FunctorBytes, !StackInfo),
    MaybeArity = cons_id_maybe_arity(ConsId),
    (
        MaybeArity = yes(Arity)
    ;
        MaybeArity = no,
        Arity = 0
    ),
    short_to_byte_list(Arity, ArityBytes),
    ConsIdBytes = FunctorBytes ++ ArityBytes.

:- pred cons_id_to_byte_list(cons_id::in, list(int)::out,
    stack_layout_info::in, stack_layout_info::out) is det.

cons_id_to_byte_list(SymName, Bytes, !StackInfo) :-
    string_to_byte_list(cons_id_rep(SymName), Bytes, !StackInfo).

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
cons_id_rep(tabling_info_const(_)) = "$tabling_info_const".
cons_id_rep(table_io_decl(_)) = "$table_io_decl".
cons_id_rep(deep_profiling_proc_layout(_)) = "$deep_profiling_proc_layout".

:- func sym_base_name_to_string(sym_name) = string.

sym_base_name_to_string(unqualified(String)) = String.
sym_base_name_to_string(qualified(_, String)) = String.

%---------------------------------------------------------------------------%

:- pred conj_to_byte_list(hlds_goals::in, instmap::in, prog_rep_info::in,
    list(int)::out, stack_layout_info::in, stack_layout_info::out) is det.

conj_to_byte_list([], _, _, [], !StackInfo).
conj_to_byte_list([Goal | Goals], InstMap0, Info, Bytes, !StackInfo) :-
    goal_to_byte_list(Goal, InstMap0, Info, GoalBytes, !StackInfo),
    Goal = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
    conj_to_byte_list(Goals, InstMap1, Info, GoalsBytes, !StackInfo),
    Bytes = GoalBytes ++ GoalsBytes.

%---------------------------------------------------------------------------%

:- pred disj_to_byte_list(hlds_goals::in, instmap::in, prog_rep_info::in,
    list(int)::out, stack_layout_info::in, stack_layout_info::out) is det.

disj_to_byte_list([], _, _, [], !StackInfo).
disj_to_byte_list([Goal | Goals], InstMap0, Info, Bytes, !StackInfo) :-
    goal_to_byte_list(Goal, InstMap0, Info, GoalBytes, !StackInfo),
    disj_to_byte_list(Goals, InstMap0, Info, GoalsBytes, !StackInfo),
    Bytes = GoalBytes ++ GoalsBytes.

%---------------------------------------------------------------------------%

:- pred cases_to_byte_list(list(case)::in, instmap::in, prog_rep_info::in,
    list(int)::out, stack_layout_info::in, stack_layout_info::out) is det.

cases_to_byte_list([], _, _, [], !StackInfo).
cases_to_byte_list([Case | Cases], InstMap0, Info, Bytes, !StackInfo) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    cons_id_and_arity_to_byte_list(MainConsId, MainConsIdBytes, !StackInfo),
    list.map_foldl(cons_id_and_arity_to_byte_list, OtherConsIds,
        OtherConsIdsByteLists, !StackInfo),
    list.condense(OtherConsIdsByteLists, OtherConsIdsBytes),
    NumOtherConsIdBytes = length_to_byte_list(OtherConsIds),
    goal_to_byte_list(Goal, InstMap0, Info, GoalBytes, !StackInfo),
    cases_to_byte_list(Cases, InstMap0, Info, CasesBytes, !StackInfo),
    Bytes = MainConsIdBytes ++ NumOtherConsIdBytes ++ OtherConsIdsBytes
        ++ GoalBytes ++ CasesBytes.

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
    stack_layout_info::in, stack_layout_info::out) is det.

string_to_byte_list(String, Bytes, !StackInfo) :-
    stack_layout.lookup_string_in_table(String, Index, !StackInfo),
    int32_to_byte_list(Index, Bytes).

:- func vars_to_byte_list(prog_rep_info, list(prog_var)) = list(int).

vars_to_byte_list(Info, Vars) =
    length_to_byte_list(Vars) ++
    list.condense(list.map(var_to_byte_list(Info), Vars)).

:- func var_to_byte_list(prog_rep_info, prog_var) = list(int).

var_to_byte_list(Info, Var) = Bytes :-
    map.lookup(Info ^ pri_var_num_map, Var, VarNum - _),
    (
        Info ^ pri_var_num_rep = byte,
        Bytes = [VarNum]
    ;
        Info ^ pri_var_num_rep = short,
        short_to_byte_list(VarNum, Bytes)
    ).

:- func maybe_vars_to_byte_list(prog_rep_info, list(maybe(prog_var))) =
    list(int).

maybe_vars_to_byte_list(Info, Vars) =
    length_to_byte_list(Vars) ++
    list.condense(list.map(maybe_var_to_byte_list(Info), Vars)).

:- func maybe_var_to_byte_list(prog_rep_info, maybe(prog_var)) = list(int).

maybe_var_to_byte_list(Info, MaybeVar) = Bytes :-
    % This is not the most efficient representation, however maybe(prog_var)s
    % are only used for partial unifications which are rare.
    (
        MaybeVar = yes(Var),
        Bytes = [1 | var_to_byte_list(Info, Var)]
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
    var_to_byte_list(Info, Var) = VarBytes,
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
    short_to_byte_list(list.length(List), Bytes).

:- func lineno_to_byte_list(int) = list(int).

lineno_to_byte_list(VarNum) = Bytes :-
    short_to_byte_list(VarNum, Bytes).

:- func method_num_to_byte_list(int) = list(int).

method_num_to_byte_list(VarNum) = Bytes :-
    short_to_byte_list(VarNum, Bytes).

:- func can_fail_to_byte(can_fail) = int.

can_fail_to_byte(can_fail) = 0.
can_fail_to_byte(cannot_fail) = 1.

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_rep.m".

%---------------------------------------------------------------------------%
:- end_module prog_rep.
%---------------------------------------------------------------------------%
