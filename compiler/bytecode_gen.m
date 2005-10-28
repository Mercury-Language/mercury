%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates bytecode, which is intended to be used by a
% (not yet implemented) bytecode interpreter/debugger.
%
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module bytecode_backend__bytecode_gen.

:- interface.

:- import_module bytecode_backend.bytecode.
:- import_module hlds.hlds_module.

:- import_module io.
:- import_module list.

:- pred gen_module(module_info::in, list(byte_code)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

% We make use of some stuff from the LLDS back-end, in particular the stuff
% relating to the argument passing convention in arg_info.m and call_gen.m.
% The intent here is to use the same argument passing convention as for
% the LLDS, to allow interoperability between code compiled to bytecode
% and code compiled to machine code.
%
% XXX It might be nice to move the argument passing related stuff
% in call_gen.m that we use here into arg_info.m, and to then rework
% arg_info.m so that it didn't depend on the LLDS.

:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.arg_info.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.tree.
:- import_module ll_backend.call_gen.  % XXX for arg passing convention
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

gen_module(ModuleInfo, Code, !IO) :-
    module_info_predids(ModuleInfo, PredIds),
    gen_preds(PredIds, ModuleInfo, CodeTree, !IO),
    tree__flatten(CodeTree, CodeList),
    list__condense(CodeList, Code).

:- pred gen_preds(list(pred_id)::in, module_info::in,
    byte_tree::out, io::di, io::uo) is det.

gen_preds([], _ModuleInfo, empty, !IO).
gen_preds([PredId | PredIds], ModuleInfo, Code, !IO) :-
    module_info_preds(ModuleInfo, PredTable),
    map__lookup(PredTable, PredId, PredInfo),
    ProcIds = pred_info_non_imported_procids(PredInfo),
    (
        ProcIds = [],
        PredCode = empty
    ;
        ProcIds = [_ | _],
        gen_pred(PredId, ProcIds, PredInfo, ModuleInfo, ProcsCode, !IO),
        predicate_name(ModuleInfo, PredId, PredName),
        list__length(ProcIds, ProcsCount),
        Arity = pred_info_orig_arity(PredInfo),
        get_is_func(PredInfo, IsFunc),
        EnterCode = node([enter_pred(PredName, Arity, IsFunc,
            ProcsCount)]),
        EndofCode = node([endof_pred]),
        PredCode = tree(EnterCode, tree(ProcsCode, EndofCode))
    ),
    gen_preds(PredIds, ModuleInfo, OtherCode, !IO),
    Code = tree(PredCode, OtherCode).

:- pred gen_pred(pred_id::in, list(proc_id)::in, pred_info::in,
    module_info::in, byte_tree::out, io::di, io::uo) is det.

gen_pred(_PredId, [], _PredInfo, _ModuleInfo, empty, !IO).
gen_pred(PredId, [ProcId | ProcIds], PredInfo, ModuleInfo, Code, !IO) :-
    write_proc_progress_message("% Generating bytecode for ",
        PredId, ProcId, ModuleInfo, !IO),
    gen_proc(ProcId, PredInfo, ModuleInfo, ProcCode),
    gen_pred(PredId, ProcIds, PredInfo, ModuleInfo, ProcsCode, !IO),
    Code = tree(ProcCode, ProcsCode).

:- pred gen_proc(proc_id::in, pred_info::in,
    module_info::in, byte_tree::out) is det.

gen_proc(ProcId, PredInfo, ModuleInfo, Code) :-
    pred_info_procedures(PredInfo, ProcTable),
    map__lookup(ProcTable, ProcId, ProcInfo),

    proc_info_goal(ProcInfo, Goal),
    proc_info_vartypes(ProcInfo, VarTypes),
    proc_info_varset(ProcInfo, VarSet),
    proc_info_interface_determinism(ProcInfo, Detism),
    determinism_to_code_model(Detism, CodeModel),

    goal_util__goal_vars(Goal, GoalVars),
    proc_info_headvars(ProcInfo, ArgVars),
    set__insert_list(GoalVars, ArgVars, Vars),
    set__to_sorted_list(Vars, VarList),
    map__init(VarMap0),
    create_varmap(VarList, VarSet, VarTypes, 0, VarMap0, VarMap, VarInfos),

    init_byte_info(ModuleInfo, VarMap, VarTypes, ByteInfo0),
    get_next_label(ZeroLabel, ByteInfo0, ByteInfo1),

    proc_info_arg_info(ProcInfo, ArgInfo),
    assoc_list__from_corresponding_lists(ArgVars, ArgInfo, Args),

    call_gen__input_arg_locs(Args, InputArgs),
    gen_pickups(InputArgs, ByteInfo, PickupCode),

    call_gen__output_arg_locs(Args, OutputArgs),
    gen_places(OutputArgs, ByteInfo, PlaceCode),

    % If semideterministic, reserve temp slot 0 for the return value
    ( CodeModel = model_semi ->
        get_next_temp(_FrameTemp, ByteInfo1, ByteInfo2)
    ;
        ByteInfo2 = ByteInfo1
    ),

    gen_goal(Goal, ByteInfo2, ByteInfo3, GoalCode),
    get_next_label(EndLabel, ByteInfo3, ByteInfo),
    get_counts(ByteInfo, LabelCount, TempCount),

    ZeroLabelCode = node([label(ZeroLabel)]),
    BodyTree = tree_list([PickupCode, ZeroLabelCode, GoalCode, PlaceCode]),
    tree__flatten(BodyTree, BodyList),
    list__condense(BodyList, BodyCode0),
    ( list__member(not_supported, BodyCode0) ->
        BodyCode = node([not_supported])
    ;
        BodyCode = node(BodyCode0)
    ),
    proc_id_to_int(ProcId, ProcInt),
    EnterCode = node([enter_proc(ProcInt, Detism, LabelCount, EndLabel,
        TempCount, VarInfos)]),
    ( CodeModel = model_semi ->
        EndofCode = node([semidet_succeed, label(EndLabel), endof_proc])
    ;
        EndofCode = node([label(EndLabel), endof_proc])
    ),
    Code = tree_list([EnterCode, BodyCode, EndofCode]).

%---------------------------------------------------------------------------%

:- pred gen_goal(hlds_goal::in, byte_info::in, byte_info::out,
    byte_tree::out) is det.

gen_goal(GoalExpr - GoalInfo, !ByteInfo, Code) :-
    gen_goal_expr(GoalExpr, GoalInfo, !ByteInfo, GoalCode),
    goal_info_get_context(GoalInfo, Context),
    term__context_line(Context, Line),
    Code = tree(node([context(Line)]), GoalCode).

:- pred gen_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
    byte_info::in, byte_info::out, byte_tree::out) is det.

gen_goal_expr(GoalExpr, GoalInfo, !ByteInfo, Code) :-
    (
        GoalExpr = generic_call(GenericCallType,
            ArgVars, ArgModes, Detism),
        ( GenericCallType = higher_order(PredVar, _, _, _) ->
            gen_higher_order_call(PredVar, ArgVars, ArgModes, Detism,
                !.ByteInfo, Code)
        ;
            % XXX
            % string__append_list([
            % "bytecode for ", GenericCallFunctor, " calls"], Msg),
            % sorry(this_file, Msg)
            functor(GenericCallType, _GenericCallFunctor, _),
            Code = node([not_supported])
        )
    ;
        GoalExpr = call(PredId, ProcId, ArgVars, BuiltinState, _, _),
        ( BuiltinState = not_builtin ->
            goal_info_get_determinism(GoalInfo, Detism),
            gen_call(PredId, ProcId, ArgVars, Detism, !.ByteInfo, Code)
        ;
            gen_builtin(PredId, ProcId, ArgVars, !.ByteInfo, Code)
        )
    ;
        GoalExpr = unify(Var, RHS, _Mode, Unification, _),
        gen_unify(Unification, Var, RHS, !.ByteInfo, Code)
    ;
        GoalExpr = not(Goal),
        gen_goal(Goal, !ByteInfo, SomeCode),
        get_next_label(EndLabel, !ByteInfo),
        get_next_temp(FrameTemp, !ByteInfo),
        EnterCode = node([enter_negation(FrameTemp, EndLabel)]),
        EndofCode = node([endof_negation_goal(FrameTemp),
            label(EndLabel), endof_negation]),
        Code =  tree_list([EnterCode, SomeCode, EndofCode])
    ;
        GoalExpr = scope(_, Goal),
        gen_goal(Goal, !ByteInfo, SomeCode),
        get_next_temp(Temp, !ByteInfo),
        EnterCode = node([enter_commit(Temp)]),
        EndofCode = node([endof_commit(Temp)]),
        Code = tree_list([EnterCode, SomeCode, EndofCode])
    ;
        GoalExpr = conj(GoalList),
        gen_conj(GoalList, !ByteInfo, Code)
    ;
        GoalExpr = par_conj(_GoalList),
        sorry(this_file, "bytecode_gen of parallel conjunction")
    ;
        GoalExpr = disj(GoalList),
        (
            GoalList = [],
            Code = node([fail])
        ;
            GoalList = [_ | _],
            get_next_label(EndLabel, !ByteInfo),
            gen_disj(GoalList, EndLabel, !ByteInfo, DisjCode),
            EnterCode = node([enter_disjunction(EndLabel)]),
            EndofCode = node([endof_disjunction, label(EndLabel)]),
            Code = tree_list([EnterCode, DisjCode, EndofCode])
        )
    ;
        GoalExpr = switch(Var, _, CasesList),
        get_next_label(EndLabel, !ByteInfo),
        gen_switch(CasesList, Var, EndLabel, !ByteInfo, SwitchCode),
        map_var(!.ByteInfo, Var, ByteVar),
        EnterCode = node([enter_switch(ByteVar, EndLabel)]),
        EndofCode = node([endof_switch, label(EndLabel)]),
        Code = tree_list([EnterCode, SwitchCode, EndofCode])
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        get_next_label(EndLabel, !ByteInfo),
        get_next_label(ElseLabel, !ByteInfo),
        get_next_temp(FrameTemp, !ByteInfo),
        gen_goal(Cond, !ByteInfo, CondCode),
        gen_goal(Then, !ByteInfo, ThenCode),
        gen_goal(Else, !ByteInfo, ElseCode),
        EnterIfCode = node([enter_if(ElseLabel, EndLabel, FrameTemp)]),
        EnterThenCode = node([enter_then(FrameTemp)]),
        EndofThenCode = node([endof_then(EndLabel), label(ElseLabel),
            enter_else(FrameTemp)]),
        EndofIfCode = node([endof_if, label(EndLabel)]),
        Code = tree_list([EnterIfCode, CondCode, EnterThenCode, ThenCode,
            EndofThenCode, ElseCode, EndofIfCode])
    ;
        GoalExpr = foreign_proc(_, _, _, _, _, _),
        Code = node([not_supported])
    ;
        GoalExpr = shorthand(_),
        % these should have been expanded out by now
        unexpected(this_file, "goal_expr: unexpected shorthand")
    ).

%---------------------------------------------------------------------------%

:- pred gen_places(list(pair(prog_var, arg_loc))::in,
    byte_info::in, byte_tree::out) is det.

gen_places([], _, empty).
gen_places([Var - Loc | OutputArgs], ByteInfo, Code) :-
    gen_places(OutputArgs, ByteInfo, OtherCode),
    map_var(ByteInfo, Var, ByteVar),
    Code = tree(node([place_arg(r, Loc, ByteVar)]), OtherCode).

:- pred gen_pickups(list(pair(prog_var, arg_loc))::in,
    byte_info::in, byte_tree::out) is det.

gen_pickups([], _, empty).
gen_pickups([Var - Loc | OutputArgs], ByteInfo, Code) :-
    gen_pickups(OutputArgs, ByteInfo, OtherCode),
    map_var(ByteInfo, Var, ByteVar),
    Code = tree(node([pickup_arg(r, Loc, ByteVar)]), OtherCode).

%---------------------------------------------------------------------------%

    % Generate bytecode for a higher order call.

:- pred gen_higher_order_call(prog_var::in, list(prog_var)::in,
    list(mer_mode)::in, determinism::in, byte_info::in, byte_tree::out) is det.

gen_higher_order_call(PredVar, ArgVars, ArgModes, Detism, ByteInfo, Code) :-
    determinism_to_code_model(Detism, CodeModel),
    get_module_info(ByteInfo, ModuleInfo),
    list__map(get_var_type(ByteInfo), ArgVars, ArgTypes),
    make_arg_infos(ArgTypes, ArgModes, CodeModel, ModuleInfo, ArgInfo),
    assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgVarsInfos),

    arg_info__partition_args(ArgVarsInfos, InVars, OutVars),
    list__length(InVars, NInVars),
    list__length(OutVars, NOutVars),

    call_gen__input_arg_locs(ArgVarsInfos, InputArgs),
    gen_places(InputArgs, ByteInfo, PlaceArgs),

    call_gen__output_arg_locs(ArgVarsInfos, OutputArgs),
    gen_pickups(OutputArgs, ByteInfo, PickupArgs),

    map_var(ByteInfo, PredVar, BytePredVar),
    Call = node([higher_order_call(BytePredVar, NInVars, NOutVars, Detism)]),
    ( CodeModel = model_semi ->
        Check = node([semidet_success_check])
    ;
        Check = empty
    ),
    Code = tree(PlaceArgs, tree(Call, tree(Check, PickupArgs))).

    % Generate bytecode for an ordinary call.
    %
:- pred gen_call(pred_id::in, proc_id::in, list(prog_var)::in,
    determinism::in, byte_info::in, byte_tree::out) is det.

gen_call(PredId, ProcId, ArgVars, Detism, ByteInfo, Code) :-
    get_module_info(ByteInfo, ModuleInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_arg_info(ProcInfo, ArgInfo),
    assoc_list__from_corresponding_lists(ArgVars, ArgInfo, ArgVarsInfos),

    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    get_is_func(PredInfo, IsFunc),

    call_gen__input_arg_locs(ArgVarsInfos, InputArgs),
    gen_places(InputArgs, ByteInfo, PlaceArgs),

    call_gen__output_arg_locs(ArgVarsInfos, OutputArgs),
    gen_pickups(OutputArgs, ByteInfo, PickupArgs),

    predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity),
    proc_id_to_int(ProcId, ProcInt),
    Call = node([call(ModuleName, PredName, Arity, IsFunc, ProcInt)]),
    determinism_to_code_model(Detism, CodeModel),
    ( CodeModel = model_semi ->
        Check = node([semidet_success_check])
    ;
        Check = empty
    ),
    Code = tree(PlaceArgs, tree(Call, tree(Check, PickupArgs))).

    % Generate bytecode for a call to a builtin.
    %
:- pred gen_builtin(pred_id::in, proc_id::in, list(prog_var)::in,
    byte_info::in, byte_tree::out) is det.

gen_builtin(PredId, ProcId, Args, ByteInfo, Code) :-
    get_module_info(ByteInfo, ModuleInfo),
    predicate_module(ModuleInfo, PredId, ModuleName),
    predicate_name(ModuleInfo, PredId, PredName),
    (
        builtin_ops__translate_builtin(ModuleName, PredName, ProcId,
            Args, SimpleCode)
    ->
        (
            SimpleCode = test(Test),
            map_test(ByteInfo, Test, Code)
        ;
            SimpleCode = assign(Var, Expr),
            map_assign(ByteInfo, Var, Expr, Code)
        ;
            SimpleCode = ref_assign(_Var, _Expr),
            unexpected(this_file, "ref_assign")
        )
    ;
        string__append("unknown builtin predicate ", PredName, Msg),
        unexpected(this_file, Msg)
    ).

:- pred map_test(byte_info::in, simple_expr(prog_var)::in(simple_test_expr),
    byte_tree::out) is det.

map_test(ByteInfo, TestExpr, Code) :-
    (
        TestExpr = binary(Binop, X, Y),
        map_arg(ByteInfo, X, ByteX),
        map_arg(ByteInfo, Y, ByteY),
        Code = node([builtin_bintest(Binop, ByteX, ByteY)])
    ;
        TestExpr = unary(Unop, X),
        map_arg(ByteInfo, X, ByteX),
        Code = node([builtin_untest(Unop, ByteX)])
    ).

:- pred map_assign(byte_info::in, prog_var::in,
    simple_expr(prog_var)::in(simple_assign_expr), byte_tree::out) is det.

map_assign(ByteInfo, Var, Expr, Code) :-
    (
        Expr = binary(Binop, X, Y),
        map_arg(ByteInfo, X, ByteX),
        map_arg(ByteInfo, Y, ByteY),
        map_var(ByteInfo, Var, ByteVar),
        Code = node([builtin_binop(Binop, ByteX, ByteY, ByteVar)])
    ;
        Expr = unary(Unop, X),
        map_arg(ByteInfo, X, ByteX),
        map_var(ByteInfo, Var, ByteVar),
        Code = node([builtin_unop(Unop, ByteX, ByteVar)])
    ;
        Expr = leaf(X),
        map_var(ByteInfo, X, ByteX),
        map_var(ByteInfo, Var, ByteVar),
        Code = node([assign(ByteVar, ByteX)])
    ).

:- pred map_arg(byte_info::in, simple_expr(prog_var)::in(simple_arg_expr),
    byte_arg::out) is det.

map_arg(ByteInfo, Expr, ByteArg) :-
    (
        Expr = leaf(Var),
        map_var(ByteInfo, Var, ByteVar),
        ByteArg = var(ByteVar)
    ;
        Expr = int_const(IntVal),
        ByteArg = int_const(IntVal)
    ;
        Expr = float_const(FloatVal),
        ByteArg = float_const(FloatVal)
    ).

%---------------------------------------------------------------------------%

    % Generate bytecode for a unification.
    %
:- pred gen_unify(unification::in, prog_var::in, unify_rhs::in,
    byte_info::in, byte_tree::out) is det.

gen_unify(construct(Var, ConsId, Args, UniModes, _, _, _), _, _,
        ByteInfo, Code) :-
    map_var(ByteInfo, Var, ByteVar),
    map_vars(ByteInfo, Args, ByteArgs),
    map_cons_id(ByteInfo, Var, ConsId, ByteConsId),
    ( ByteConsId = pred_const(_, _, _, _, _) ->
        Code = node([construct(ByteVar, ByteConsId, ByteArgs)])
    ;
        % Don't call map_uni_modes until after
        % the pred_const test fails, since the arg-modes on
        % unifications that create closures aren't like other arg-modes.
        map_uni_modes(UniModes, Args, ByteInfo, Dirs),
        ( all_dirs_same(Dirs, to_var) ->
            Code = node([construct(ByteVar, ByteConsId, ByteArgs)])
        ;
            assoc_list__from_corresponding_lists(ByteArgs, Dirs, Pairs),
            Code = node([complex_construct(ByteVar, ByteConsId, Pairs)])
        )
    ).
gen_unify(deconstruct(Var, ConsId, Args, UniModes, _, _), _, _,
        ByteInfo, Code) :-
    map_var(ByteInfo, Var, ByteVar),
    map_vars(ByteInfo, Args, ByteArgs),
    map_cons_id(ByteInfo, Var, ConsId, ByteConsId),
    map_uni_modes(UniModes, Args, ByteInfo, Dirs),
    ( all_dirs_same(Dirs, to_arg) ->
        Code = node([deconstruct(ByteVar, ByteConsId, ByteArgs)])
    ;
        assoc_list__from_corresponding_lists(ByteArgs, Dirs, Pairs),
        Code = node([complex_deconstruct(ByteVar, ByteConsId, Pairs)])
    ).
gen_unify(assign(Target, Source), _, _, ByteInfo, Code) :-
    map_var(ByteInfo, Target, ByteTarget),
    map_var(ByteInfo, Source, ByteSource),
    Code = node([assign(ByteTarget, ByteSource)]).
gen_unify(simple_test(Var1, Var2), _, _, ByteInfo, Code) :-
    map_var(ByteInfo, Var1, ByteVar1),
    map_var(ByteInfo, Var2, ByteVar2),
    get_var_type(ByteInfo, Var1, Var1Type),
    get_var_type(ByteInfo, Var2, Var2Type),
    (
        type_to_ctor_and_args(Var1Type, TypeCtor1, _),
        type_to_ctor_and_args(Var2Type, TypeCtor2, _)
    ->
        ( TypeCtor2 = TypeCtor1 ->
            TypeCtor = TypeCtor1
        ;   unexpected(this_file,
                "simple_test between different types")
        )
    ;
        unexpected(this_file, "failed lookup of type id")
    ),
    ByteInfo = byte_info(_, _, ModuleInfo, _, _),
    TypeCategory = classify_type_ctor(ModuleInfo, TypeCtor),
    (
        TypeCategory = int_type,
        TestId = int_test
    ;
        TypeCategory = char_type,
        TestId = char_test
    ;
        TypeCategory = str_type,
        TestId = string_test
    ;
        TypeCategory = float_type,
        TestId = float_test
    ;
        TypeCategory = dummy_type,
        TestId = dummy_test
    ;
        TypeCategory = enum_type,
        TestId = enum_test
    ;
        TypeCategory = higher_order_type,
        unexpected(this_file, "higher_order_type in simple_test")
    ;
        TypeCategory = tuple_type,
        unexpected(this_file, "tuple_type in simple_test")
    ;
        TypeCategory = user_ctor_type,
        unexpected(this_file, "user_ctor_type in simple_test")
    ;
        TypeCategory = variable_type,
        unexpected(this_file, "variable_type in simple_test")
    ;
        TypeCategory = void_type,
        unexpected(this_file, "void_type in simple_test")
    ;
        TypeCategory = type_info_type,
        unexpected(this_file, "type_info_type in simple_test")
    ;
        TypeCategory = type_ctor_info_type,
        unexpected(this_file, "type_ctor_info_type in simple_test")
    ;
        TypeCategory = typeclass_info_type,
        unexpected(this_file, "typeclass_info_type in simple_test")
    ;
        TypeCategory = base_typeclass_info_type,
        unexpected(this_file, "base_typeclass_info_type in simple_test")
    ),
    Code = node([test(ByteVar1, ByteVar2, TestId)]).
gen_unify(complicated_unify(_,_,_), _Var, _RHS, _ByteInfo, _Code) :-
    unexpected(this_file, "complicated unifications " ++
        "should have been handled by polymorphism.m").

:- pred map_uni_modes(list(uni_mode)::in, list(prog_var)::in,
    byte_info::in, list(byte_dir)::out) is det.

map_uni_modes([], [], _, []).
map_uni_modes([UniMode | UniModes], [Arg | Args], ByteInfo, [Dir | Dirs]) :-
    UniMode = ((VarInitial - ArgInitial) -> (VarFinal - ArgFinal)),
    get_module_info(ByteInfo, ModuleInfo),
    get_var_type(ByteInfo, Arg, Type),
    mode_to_arg_mode(ModuleInfo, (VarInitial -> VarFinal), Type, VarMode),
    mode_to_arg_mode(ModuleInfo, (ArgInitial -> ArgFinal), Type, ArgMode),
    (
        VarMode = top_in,
        ArgMode = top_out
    ->
        Dir = to_arg
    ;
        VarMode = top_out,
        ArgMode = top_in
    ->
        Dir = to_var
    ;
        VarMode = top_unused,
        ArgMode = top_unused
    ->
        Dir = to_none
    ;
        unexpected(this_file,
            "invalid mode for (de)construct unification")
    ),
    map_uni_modes(UniModes, Args, ByteInfo, Dirs).
map_uni_modes([], [_|_], _, _) :-
    unexpected(this_file, "map_uni_modes: length mismatch").
map_uni_modes([_|_], [], _, _) :-
    unexpected(this_file, "map_uni_modes: length mismatch").

:- pred all_dirs_same(list(byte_dir)::in, byte_dir::in)
    is semidet.

all_dirs_same([], _).
all_dirs_same([Dir | Dirs], Dir) :-
    all_dirs_same(Dirs, Dir).

%---------------------------------------------------------------------------%

    % Generate bytecode for a conjunction
    %
:- pred gen_conj(list(hlds_goal)::in, byte_info::in, byte_info::out,
    byte_tree::out) is det.

gen_conj([], !ByteInfo, empty).
gen_conj([Goal | Goals], !ByteInfo, Code) :-
    gen_goal(Goal, !ByteInfo, ThisCode),
    gen_conj(Goals, !ByteInfo, OtherCode),
    Code = tree(ThisCode, OtherCode).

%---------------------------------------------------------------------------%

    % Generate bytecode for each disjunct of a disjunction.
    %
:- pred gen_disj(list(hlds_goal)::in, int::in,
    byte_info::in, byte_info::out,  byte_tree::out) is det.

gen_disj([], _, _, _, _) :-
    unexpected(this_file, "empty disjunction in disj").
gen_disj([Disjunct | Disjuncts], EndLabel, !ByteInfo, Code) :-
    gen_goal(Disjunct, !ByteInfo, ThisCode),
    (
        Disjuncts = [],
        EnterCode = node([enter_disjunct(-1)]),
        EndofCode = node([endof_disjunct(EndLabel)]),
        Code = tree_list([EnterCode, ThisCode, EndofCode])
    ;
        Disjuncts = [_ | _],
        gen_disj(Disjuncts, EndLabel, !ByteInfo, OtherCode),
        get_next_label(NextLabel, !ByteInfo),
        EnterCode = node([enter_disjunct(NextLabel)]),
        EndofCode = node([endof_disjunct(EndLabel), label(NextLabel)]),
        Code = tree_list([EnterCode, ThisCode, EndofCode, OtherCode])
    ).

%---------------------------------------------------------------------------%

    % Generate bytecode for each arm of a switch.
    %
:- pred gen_switch(list(case)::in, prog_var::in, int::in,
    byte_info::in, byte_info::out, byte_tree::out) is det.

gen_switch([], _, _, !ByteInfo, empty).
gen_switch([case(ConsId, Goal) | Cases], Var, EndLabel,
        !ByteInfo, Code) :-
    map_cons_id(!.ByteInfo, Var, ConsId, ByteConsId),
    gen_goal(Goal, !ByteInfo, ThisCode),
    gen_switch(Cases, Var, EndLabel, !ByteInfo, OtherCode),
    get_next_label(NextLabel, !ByteInfo),
    EnterCode = node([enter_switch_arm(ByteConsId, NextLabel)]),
    EndofCode = node([endof_switch_arm(EndLabel), label(NextLabel)]),
    Code = tree_list([EnterCode, ThisCode, EndofCode, OtherCode]).

%---------------------------------------------------------------------------%

:- pred map_cons_id(byte_info::in, prog_var::in, cons_id::in,
    byte_cons_id::out) is det.

map_cons_id(ByteInfo, Var, ConsId, ByteConsId) :-
    get_module_info(ByteInfo, ModuleInfo),
    (
        ConsId = cons(Functor, Arity),
        get_var_type(ByteInfo, Var, Type),
        (
            % Everything other than characters and tuples should
            % be module qualified.
            Functor = unqualified(FunctorName),
            \+ type_is_tuple(Type, _)
        ->
            string__to_char_list(FunctorName, FunctorList),
            ( FunctorList = [Char] ->
                ByteConsId = char_const(Char)
            ;
                unexpected(this_file, "map_cons_id: " ++
                    "unqualified cons_id is not a char_const")
            )
        ;
            (
                Functor = unqualified(FunctorName),
                ModuleName = unqualified("builtin")
            ;
                Functor = qualified(ModuleName, FunctorName)
            ),
            ConsTag = cons_id_to_tag(ConsId, Type, ModuleInfo),
            map_cons_tag(ConsTag, ByteConsTag),
            ByteConsId = cons(ModuleName, FunctorName, Arity, ByteConsTag)
        )
    ;
        ConsId = int_const(IntVal),
        ByteConsId = int_const(IntVal)
    ;
        ConsId = string_const(StringVal),
        ByteConsId = string_const(StringVal)
    ;
        ConsId = float_const(FloatVal),
        ByteConsId = float_const(FloatVal)
    ;
        ConsId = pred_const(ShroudedPredProcId, EvalMethod),
        proc(PredId, ProcId) = unshroud_pred_proc_id(ShroudedPredProcId),
        (
            EvalMethod = lambda_normal,
            predicate_id(ModuleInfo, PredId, ModuleName, PredName, Arity),

            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            get_is_func(PredInfo, IsFunc),

            proc_id_to_int(ProcId, ProcInt),
            ByteConsId = pred_const(ModuleName, PredName, Arity, IsFunc,
                ProcInt)
        ;
            EvalMethod = lambda_aditi_bottom_up,
            % XXX
            sorry(this_file, "bytecode for Aditi lambda expressions")
        )
    ;
        ConsId = type_ctor_info_const(ModuleName, TypeName, TypeArity),
        ByteConsId = type_ctor_info_const(ModuleName, TypeName, TypeArity)
    ;
        ConsId = base_typeclass_info_const(ModuleName, ClassId, _, Instance),
        ByteConsId = base_typeclass_info_const(ModuleName, ClassId, Instance)
    ;
        ConsId = type_info_cell_constructor(_),
        ByteConsId = type_info_cell_constructor
    ;
        ConsId = typeclass_info_cell_constructor,
        ByteConsId = typeclass_info_cell_constructor
    ;
        ConsId = tabling_pointer_const(_),
        sorry(this_file, "bytecode cannot implement tabling")
    ;
        ConsId = table_io_decl(_),
        sorry(this_file, "bytecode cannot implement table io decl")
    ;
        ConsId = deep_profiling_proc_layout(_),
        sorry(this_file, "bytecode cannot implement deep profiling")
    ).

:- pred map_cons_tag(cons_tag::in, byte_cons_tag::out) is det.

map_cons_tag(no_tag, no_tag).
    % `single_functor' is just an optimized version of `unshared_tag(0)'
    % this optimization is not important for the bytecode
map_cons_tag(single_functor, unshared_tag(0)).
map_cons_tag(unshared_tag(Primary), unshared_tag(Primary)).
map_cons_tag(shared_remote_tag(Primary, Secondary),
    shared_remote_tag(Primary, Secondary)).
map_cons_tag(shared_local_tag(Primary, Secondary),
    shared_local_tag(Primary, Secondary)).
map_cons_tag(string_constant(_), _) :-
    unexpected(this_file, "string_constant cons tag " ++
        "for non-string_constant cons id").
map_cons_tag(int_constant(IntVal), enum_tag(IntVal)).
map_cons_tag(float_constant(_), _) :-
    unexpected(this_file, "float_constant cons tag " ++
        "for non-float_constant cons id").
map_cons_tag(pred_closure_tag(_, _, _), _) :-
    unexpected(this_file, "pred_closure_tag cons tag " ++
        "for non-pred_const cons id").
map_cons_tag(type_ctor_info_constant(_, _, _), _) :-
    unexpected(this_file, "type_ctor_info_constant cons tag " ++
        "for non-type_ctor_info_constant cons id").
map_cons_tag(base_typeclass_info_constant(_, _, _), _) :-
    unexpected(this_file, "base_typeclass_info_constant cons tag " ++
        "for non-base_typeclass_info_constant cons id").
map_cons_tag(tabling_pointer_constant(_, _), _) :-
    unexpected(this_file, "tabling_pointer_constant cons tag " ++
        "for non-tabling_pointer_constant cons id").
map_cons_tag(deep_profiling_proc_layout_tag(_, _), _) :-
    unexpected(this_file, "deep_profiling_proc_layout_tag cons tag " ++
        "for non-deep_profiling_proc_static cons id").
map_cons_tag(table_io_decl_tag(_, _), _) :-
    unexpected(this_file, "table_io_decl_tag cons tag " ++
        "for non-table_io_decl cons id").
map_cons_tag(reserved_address(_), _) :-
    % These should only be generated if the --num-reserved-addresses
    % or --num-reserved-objects options are used.
    sorry(this_file, "bytecode with --num-reserved-addresses " ++
        "or --num-reserved-objects").
map_cons_tag(shared_with_reserved_addresses(_, _), _) :-
    % These should only be generated if the --num-reserved-addresses
    % or --num-reserved-objects options are used.
    sorry(this_file, "bytecode with --num-reserved-addresses " ++
        "or --num-reserved-objects").

%---------------------------------------------------------------------------%

:- pred create_varmap(list(prog_var)::in, prog_varset::in,
    vartypes::in, int::in, map(prog_var, byte_var)::in,
    map(prog_var, byte_var)::out, list(byte_var_info)::out) is det.

create_varmap([], _, _, _, !VarMap, []).
create_varmap([Var | VarList], VarSet, VarTypes, N0, !VarMap, VarInfos) :-
    map__det_insert(!.VarMap, Var, N0, !:VarMap),
    N1 = N0 + 1,
    varset__lookup_name(VarSet, Var, VarName),
    map__lookup(VarTypes, Var, VarType),
    create_varmap(VarList, VarSet, VarTypes, N1, !VarMap, VarInfosTail),
    VarInfos = [var_info(VarName, VarType) | VarInfosTail].

%---------------------------------------------------------------------------%(

:- type byte_info
    --->    byte_info(
                byteinfo_varmap         :: map(prog_var, byte_var),
                byteinfo_vartypes       :: vartypes,
                byteinfo_moduleinfo     :: module_info,
                byteinfo_label_counter  :: counter,
                byteinfo_temp_counter   :: counter
            ).

:- pred init_byte_info(module_info::in, map(prog_var, byte_var)::in,
    vartypes::in, byte_info::out) is det.

init_byte_info(ModuleInfo, VarMap, VarTypes, ByteInfo) :-
    ByteInfo = byte_info(VarMap, VarTypes, ModuleInfo,
        counter__init(0), counter__init(0)).

:- pred get_module_info(byte_info::in, module_info::out) is det.

get_module_info(ByteInfo, ByteInfo ^ byteinfo_moduleinfo).

:- pred map_vars(byte_info::in,
    list(prog_var)::in, list(byte_var)::out) is det.

map_vars(ByteInfo, Vars, ByteVars) :-
    map_vars_2(ByteInfo ^ byteinfo_varmap, Vars, ByteVars).

:- pred map_vars_2(map(prog_var, byte_var)::in,
    list(prog_var)::in, list(byte_var)::out) is det.

map_vars_2(_VarMap, [], []).
map_vars_2(VarMap, [Var | Vars], [ByteVar | ByteVars]) :-
    map__lookup(VarMap, Var, ByteVar),
    map_vars_2(VarMap, Vars, ByteVars).

:- pred map_var(byte_info::in, prog_var::in,
    byte_var::out) is det.

map_var(ByteInfo, Var, ByteVar) :-
    map__lookup(ByteInfo ^ byteinfo_varmap, Var, ByteVar).

:- pred get_var_type(byte_info::in, prog_var::in,
    mer_type::out) is det.

get_var_type(ByteInfo, Var, Type) :-
    map__lookup(ByteInfo ^ byteinfo_vartypes, Var, Type).

:- pred get_next_label(int::out, byte_info::in, byte_info::out)
    is det.

get_next_label(Label, !ByteInfo) :-
    LabelCounter0 = !.ByteInfo ^ byteinfo_label_counter,
    counter__allocate(Label, LabelCounter0, LabelCounter),
    !:ByteInfo = !.ByteInfo ^ byteinfo_label_counter := LabelCounter.

:- pred get_next_temp(int::out, byte_info::in, byte_info::out)
    is det.

get_next_temp(Temp, !ByteInfo) :-
    TempCounter0 = !.ByteInfo ^ byteinfo_temp_counter,
    counter__allocate(Temp, TempCounter0, TempCounter),
    !:ByteInfo = !.ByteInfo ^ byteinfo_temp_counter := TempCounter.

:- pred get_counts(byte_info::in, int::out, int::out) is det.

get_counts(ByteInfo0, Label, Temp) :-
    LabelCounter0 = ByteInfo0 ^ byteinfo_label_counter,
    counter__allocate(Label, LabelCounter0, _LabelCounter),
    TempCounter0 = ByteInfo0 ^ byteinfo_temp_counter,
    counter__allocate(Temp, TempCounter0, _TempCounter).

%---------------------------------------------------------------------------%

:- pred get_is_func(pred_info::in, byte_is_func::out) is det.

get_is_func(PredInfo, IsFunc) :-
    ( pred_info_is_pred_or_func(PredInfo) = predicate ->
        IsFunc = 0
    ;
        IsFunc = 1
    ).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "bytecode_gen.m".

%---------------------------------------------------------------------------%
