%---------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------e
% Copyright (C) 1994-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: unify_gen.m.
%
% This module handles code generation for "simple" unifications,
% i.e. those unifications which are simple enough for us to generate
% inline code.
%
% For "complicated" unifications, we generate a call to an out-of-line
% unification predicate (the call is handled in call_gen.m) - and then
% eventually generate the out-of-line code (unify_proc.m).
%
%---------------------------------------------------------------------------%

:- module ll_backend.unify_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%

:- type test_sense
    --->    branch_on_success
    ;       branch_on_failure.

:- pred generate_unification(code_model::in, unification::in,
    hlds_goal_info::in, code_tree::out, code_info::in, code_info::out) is det.

:- pred generate_tag_test(prog_var::in, cons_id::in, test_sense::in,
    label::out, code_tree::out, code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module backend_libs.type_class_info.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.tree.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.layout.
:- import_module ll_backend.stack_layout.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

:- type uni_val
    --->    ref(prog_var)
    ;       lval(lval).

%---------------------------------------------------------------------------%

generate_unification(CodeModel, Uni, GoalInfo, Code, !CI) :-
    ( CodeModel = model_non ->
        unexpected(this_file, "nondet unification in generate_unification")
    ;
        true
    ),
    (
        Uni = assign(Left, Right),
        ( code_info.variable_is_forward_live(!.CI, Left) ->
            generate_assignment(Left, Right, Code, !CI)
        ;
            Code = empty
        )
    ;
        Uni = construct(Var, ConsId, Args, Modes, _, _, SubInfo),
        (
            SubInfo = no_construct_sub_info,
            MaybeTakeAddr = no,
            MaybeSize = no
        ;
            SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize)
        ),
        (
            ( code_info.variable_is_forward_live(!.CI, Var)
            ; MaybeTakeAddr = yes(_)
            )
        ->
            (
                MaybeTakeAddr = yes(TakeAddr)
            ;
                MaybeTakeAddr = no,
                TakeAddr = []
            ),
            generate_construction(Var, ConsId, Args, Modes,
                TakeAddr, MaybeSize, GoalInfo, Code, !CI)
        ;
            Code = empty
        )
    ;
        Uni = deconstruct(Var, ConsId, Args, Modes, _CanFail, _CanCGC),
        ( CodeModel = model_det ->
            generate_det_deconstruction(Var, ConsId, Args, Modes, Code, !CI)
        ;
            generate_semi_deconstruction(Var, ConsId, Args, Modes, Code, !CI)
        )
    ;
        Uni = simple_test(Var1, Var2),
        ( CodeModel = model_det ->
            unexpected(this_file, "det simple_test during code generation")
        ;
            generate_test(Var1, Var2, Code, !CI)
        )
    ;
        % These should have been transformed into calls to unification
        % procedures by polymorphism.m.
        Uni = complicated_unify(_UniMode, _CanFail, _TypeInfoVars),
        unexpected(this_file, "complicated unify during code generation")
    ).

%---------------------------------------------------------------------------%

    % Assignment unifications are generated by simply caching the bound
    % variable as the expression that generates the free variable.
    % No immediate code is generated.
    %
:- pred generate_assignment(prog_var::in, prog_var::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_assignment(VarA, VarB, empty, !CI) :-
    ( code_info.variable_is_forward_live(!.CI, VarA) ->
        code_info.assign_var_to_var(VarA, VarB, !CI)
    ;
        % For free-free unifications, the mode analysis reports them as
        % assignment to the dead variable. For such unifications we of course
        % do not generate any code.
        true
    ).

%---------------------------------------------------------------------------%

    % A [simple] test unification is generated by flushing both variables
    % from the cache, and producing code that branches to the fall-through
    % point if the two values are not the same. Simple tests are in-in
    % unifications on enumerations, integers, strings and floats.
    %
:- pred generate_test(prog_var::in, prog_var::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_test(VarA, VarB, Code, !CI) :-
    code_info.produce_variable(VarA, CodeA, ValA, !CI),
    code_info.produce_variable(VarB, CodeB, ValB, !CI),
    CodeAB = tree(CodeA, CodeB),
    Type = code_info.variable_type(!.CI, VarA),
    ( Type = builtin_type(builtin_type_string) ->
        Op = str_eq
    ; Type = builtin_type(builtin_type_float) ->
        Op = float_eq
    ;
        Op = eq
    ),
    code_info.fail_if_rval_is_false(binop(Op, ValA, ValB), FailCode, !CI),
    Code = tree(CodeAB, FailCode).

%---------------------------------------------------------------------------%

generate_tag_test(Var, ConsId, Sense, ElseLab, Code, !CI) :-
    code_info.produce_variable(Var, VarCode, Rval, !CI),
    % As an optimization, for data types with exactly two alternatives,
    % one of which is a constant, we make sure that we test against the
    % constant (negating the result of the test, if needed),
    % since a test against a constant is cheaper than a tag test.
    (
        ConsId = cons(_, Arity),
        Arity > 0
    ->
        Type = code_info.variable_type(!.CI, Var),
        TypeDefn = code_info.lookup_type_defn(!.CI, Type),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        ( ConsTable = TypeBody ^ du_type_cons_tag_values ->
            map.to_assoc_list(ConsTable, ConsList),
            (
                ConsList = [ConsId - _, OtherConsId - _],
                OtherConsId = cons(_, 0)
            ->
                Reverse = yes(OtherConsId)
            ;
                ConsList = [OtherConsId - _, ConsId - _],
                OtherConsId = cons(_, 0)
            ->
                Reverse = yes(OtherConsId)
            ;
                Reverse = no
            )
        ;
            Reverse = no
        )
    ;
        Reverse = no
    ),
    VarName = code_info.variable_to_string(!.CI, Var),
    ConsIdName = hlds_out.cons_id_to_string(ConsId),
    (
        Reverse = no,
        string.append_list(["checking that ", VarName, " has functor ",
            ConsIdName], Comment),
        CommentCode = node([comment(Comment) - ""]),
        Tag = cons_id_to_tag_for_var(!.CI, Var, ConsId),
        generate_tag_test_rval_2(Tag, Rval, TestRval)
    ;
        Reverse = yes(TestConsId),
        string.append_list(["checking that ", VarName, " has functor ",
            ConsIdName, " (inverted test)"], Comment),
        CommentCode = node([comment(Comment) - ""]),
        Tag = cons_id_to_tag_for_var(!.CI, Var, TestConsId),
        generate_tag_test_rval_2(Tag, Rval, NegTestRval),
        code_util.neg_rval(NegTestRval, TestRval)
    ),
    code_info.get_next_label(ElseLab, !CI),
    (
        Sense = branch_on_success,
        TheRval = TestRval
    ;
        Sense = branch_on_failure,
        code_util.neg_rval(TestRval, TheRval)
    ),
    TestCode = node([if_val(TheRval, code_label(ElseLab)) - "tag test"]),
    Code = tree(VarCode, tree(CommentCode, TestCode)).

%---------------------------------------------------------------------------%

:- pred generate_tag_test_rval(prog_var::in, cons_id::in,
    rval::out, code_tree::out, code_info::in, code_info::out) is det.

generate_tag_test_rval(Var, ConsId, TestRval, Code, !CI) :-
    code_info.produce_variable(Var, Code, Rval, !CI),
    Tag = cons_id_to_tag_for_var(!.CI, Var, ConsId),
    generate_tag_test_rval_2(Tag, Rval, TestRval).

:- pred generate_tag_test_rval_2(cons_tag::in, rval::in, rval::out)
    is det.

generate_tag_test_rval_2(ConsTag, Rval, TestRval) :-
    (
        ConsTag = string_tag(String),
        TestRval = binop(str_eq, Rval, const(llconst_string(String)))
    ;
        ConsTag = float_tag(Float),
        TestRval = binop(float_eq, Rval, const(llconst_float(Float)))
    ;
        ConsTag = int_tag(Int),
        TestRval = binop(eq, Rval, const(llconst_int(Int)))
    ;
        ConsTag = pred_closure_tag(_, _, _),
        % This should never happen, since the error will be detected
        % during mode checking.
        unexpected(this_file, "Attempted higher-order unification")
    ;
        ConsTag = type_ctor_info_tag(_, _, _),
        unexpected(this_file, "Attempted type_ctor_info unification")
    ;
        ConsTag = base_typeclass_info_tag(_, _, _),
        unexpected(this_file, "Attempted base_typeclass_info unification")
    ;
        ConsTag = tabling_info_tag(_, _),
        unexpected(this_file, "Attempted tabling_info unification")
    ;
        ConsTag = deep_profiling_proc_layout_tag(_, _),
        unexpected(this_file,
            "Attempted deep_profiling_proc_layout_tag unification")
    ;
        ConsTag = table_io_decl_tag(_, _),
        unexpected(this_file, "Attempted table_io_decl_tag unification")
    ;
        ConsTag = no_tag,
        TestRval = const(llconst_true)
    ;
        ConsTag = single_functor_tag,
        TestRval = const(llconst_true)
    ;
        ConsTag = unshared_tag(UnsharedTag),
        VarPtag = unop(tag, Rval),
        ConstPtag = unop(mktag, const(llconst_int(UnsharedTag))),
        TestRval = binop(eq, VarPtag, ConstPtag)
    ;
        ConsTag = shared_remote_tag(Bits, Num),
        VarPtag = unop(tag, Rval),
        ConstPtag = unop(mktag, const(llconst_int(Bits))),
        PtagTestRval = binop(eq, VarPtag, ConstPtag),
        VarStag = lval(field(yes(Bits), Rval, const(llconst_int(0)))),
        ConstStag = const(llconst_int(Num)),
        StagTestRval = binop(eq, VarStag, ConstStag),
        TestRval = binop(logical_and, PtagTestRval, StagTestRval)
    ;
        ConsTag = shared_local_tag(Bits, Num),
        ConstStag = mkword(Bits, unop(mkbody, const(llconst_int(Num)))),
        TestRval = binop(eq, Rval, ConstStag)
    ;
        ConsTag = reserved_address_tag(RA),
        TestRval = binop(eq, Rval, generate_reserved_address(RA))
    ;
        ConsTag = shared_with_reserved_addresses_tag(ReservedAddrs, ThisTag),
        % We first check that the Rval doesn't match any of the ReservedAddrs,
        % and then check that it matches ThisTag.
        CheckReservedAddrs = (func(RA, InnerTestRval0) = InnerTestRval :-
            generate_tag_test_rval_2(reserved_address_tag(RA), Rval, EqualRA),
            InnerTestRval = binop(logical_and,
                unop(logical_not, EqualRA), InnerTestRval0)
        ),
        generate_tag_test_rval_2(ThisTag, Rval, MatchesThisTag),
        TestRval = list.foldr(CheckReservedAddrs, ReservedAddrs,
            MatchesThisTag)
    ).

:- func generate_reserved_address(reserved_address) = rval.

generate_reserved_address(null_pointer) = const(llconst_int(0)).
generate_reserved_address(small_pointer(N)) = const(llconst_int(N)).
generate_reserved_address(reserved_object(_, _, _)) = _ :-
    % These should only be used for the MLDS back-end.
    unexpected(this_file, "reserved_object").

%---------------------------------------------------------------------------%

    % A construction unification is implemented as a simple assignment
    % of a function symbol if the function symbol has arity zero.
    % If the function symbol's arity is greater than zero, and all its
    % arguments are constants, the construction is implemented by
    % constructing the new term statically. If not all the argumemts are
    % constants, the construction is implemented as a heap-increment
    % to create a term, and a series of [optional] assignments to
    % instantiate the arguments of that term.
    %
:- pred generate_construction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(uni_mode)::in, list(int)::in,
    maybe(term_size_value)::in, hlds_goal_info::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_construction(Var, ConsId, Args, Modes, TakeAddr, MaybeSize, GoalInfo,
        Code, !CI) :-
    Tag = cons_id_to_tag_for_var(!.CI, Var, ConsId),
    generate_construction_2(Tag, Var, Args, Modes, TakeAddr, MaybeSize,
        GoalInfo, Code, !CI).

:- pred generate_construction_2(cons_tag::in, prog_var::in,
    list(prog_var)::in, list(uni_mode)::in, list(int)::in,
    maybe(term_size_value)::in, hlds_goal_info::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_construction_2(ConsTag, Var, Args, Modes, TakeAddr, MaybeSize,
        GoalInfo, Code, !CI) :-
    (
        ConsTag = string_tag(String),
        code_info.assign_const_to_var(Var, const(llconst_string(String)), !CI),
        Code = empty
    ;
        ConsTag = int_tag(Int),
        code_info.assign_const_to_var(Var, const(llconst_int(Int)), !CI),
        Code = empty
    ;
        ConsTag = float_tag(Float),
        code_info.assign_const_to_var(Var, const(llconst_float(Float)), !CI),
        Code = empty
    ;
        ConsTag = no_tag,
        (
            Args = [Arg],
            Modes = [Mode]
        ->
            (
                TakeAddr = [],
                Type = code_info.variable_type(!.CI, Arg),
                generate_sub_unify(ref(Var), ref(Arg), Mode, Type, Code, !CI)
            ;
                TakeAddr = [_ | _],
                unexpected(this_file,
                    "generate_construction_2: notag: take_addr")
            )
        ;
            unexpected(this_file,
                "generate_construction_2: no_tag: arity != 1")
        )
    ;
        ConsTag = single_functor_tag,
        % Treat single_functor the same as unshared_tag(0).
        generate_construction_2(unshared_tag(0),
            Var, Args, Modes, TakeAddr, MaybeSize, GoalInfo, Code, !CI)
    ;
        ConsTag = unshared_tag(Ptag),
        code_info.get_module_info(!.CI, ModuleInfo),
        var_types(!.CI, Args, ArgTypes),
        generate_cons_args(Args, ArgTypes, Modes, 0, 1, TakeAddr, ModuleInfo,
            MaybeRvals, FieldAddrs, MayUseAtomic),
        construct_cell(Var, Ptag, MaybeRvals, MaybeSize, FieldAddrs,
            MayUseAtomic, Code, !CI)
    ;
        ConsTag = shared_remote_tag(Ptag, Sectag),
        code_info.get_module_info(!.CI, ModuleInfo),
        var_types(!.CI, Args, ArgTypes),
        generate_cons_args(Args, ArgTypes, Modes, 1, 1, TakeAddr, ModuleInfo,
            MaybeRvals0, FieldAddrs, MayUseAtomic),
        % The first field holds the secondary tag.
        MaybeRvals = [yes(const(llconst_int(Sectag))) | MaybeRvals0],
        construct_cell(Var, Ptag, MaybeRvals, MaybeSize, FieldAddrs,
            MayUseAtomic, Code, !CI)
    ;
        ConsTag = shared_local_tag(Bits1, Num1),
        code_info.assign_const_to_var(Var,
            mkword(Bits1, unop(mkbody, const(llconst_int(Num1)))), !CI),
        Code = empty
    ;
        ConsTag = type_ctor_info_tag(ModuleName, TypeName, TypeArity),
        expect(unify(Args, []), this_file,
            "generate_construction_2: type_ctor_info constant has args"),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
        DataAddr = rtti_addr(ctor_rtti_id(RttiTypeCtor,
            type_ctor_type_ctor_info)),
        code_info.assign_const_to_var(Var,
            const(llconst_data_addr(DataAddr, no)), !CI),
        Code = empty
    ;
        ConsTag = base_typeclass_info_tag(ModuleName, ClassId, Instance),
        expect(unify(Args, []), this_file,
            "generate_construction_2: base_typeclass_info constant has args"),
        TCName = generate_class_name(ClassId),
        code_info.assign_const_to_var(Var,
            const(llconst_data_addr(rtti_addr(tc_rtti_id(TCName,
                type_class_base_typeclass_info(ModuleName, Instance))), no)),
                !CI),
        Code = empty
    ;
        ConsTag = tabling_info_tag(PredId, ProcId),
        expect(unify(Args, []), this_file,
            "generate_construction_2: tabling_info constant has args"),
        code_info.get_module_info(!.CI, ModuleInfo),
        ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
        module_info_get_name(ModuleInfo, ModuleName),
        DataAddr = data_addr(ModuleName,
            proc_tabling_ref(ProcLabel, tabling_info)),
        code_info.assign_const_to_var(Var,
            const(llconst_data_addr(DataAddr, no)), !CI),
        Code = empty
    ;
        ConsTag = deep_profiling_proc_layout_tag(PredId, ProcId),
        expect(unify(Args, []), this_file,
            "generate_construction_2: deep_profiling_proc_static has args"),
        code_info.get_module_info(!.CI, ModuleInfo),
        RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
        Origin = RttiProcLabel ^ pred_info_origin,
        ( Origin = origin_special_pred(_) ->
            UserOrUCI = uci
        ;
            UserOrUCI = user
        ),
        ProcKind = proc_layout_proc_id(UserOrUCI),
        DataAddr = layout_addr(proc_layout(RttiProcLabel, ProcKind)),
        code_info.assign_const_to_var(Var,
            const(llconst_data_addr(DataAddr, no)), !CI),
        Code = empty
    ;
        ConsTag = table_io_decl_tag(PredId, ProcId),
        expect(unify(Args, []), this_file,
            "generate_construction_2: table_io_decl has args"),
        code_info.get_module_info(!.CI, ModuleInfo),
        RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
        DataAddr = layout_addr(table_io_decl(RttiProcLabel)),
        code_info.assign_const_to_var(Var,
            const(llconst_data_addr(DataAddr, no)), !CI),
        Code = empty
    ;
        ConsTag = reserved_address_tag(RA),
        expect(unify(Args, []), this_file,
            "generate_construction_2: reserved_address constant has args"),
        code_info.assign_const_to_var(Var, generate_reserved_address(RA), !CI),
        Code = empty
    ;
        ConsTag = shared_with_reserved_addresses_tag(_RAs, ThisTag),
        % For shared_with_reserved_address, the sharing is only important
        % for tag tests, not for constructions, so here we just recurse
        % on the real representation.
        generate_construction_2(ThisTag,
            Var, Args, Modes, TakeAddr, MaybeSize, GoalInfo, Code, !CI)
    ;
        ConsTag = pred_closure_tag(PredId, ProcId, EvalMethod),
        expect(unify(TakeAddr, []), this_file,
            "generate_construction_2: pred_closure_tag has take_addr"),
        expect(unify(MaybeSize, no), this_file,
            "generate_construction_2: pred_closure_tag has size"),
        generate_closure(PredId, ProcId, EvalMethod, Var, Args, GoalInfo,
            Code, !CI)
    ).

    % This predicate constructs or extends a closure.
    % The structure of closures is defined in runtime/mercury_ho_call.h.
    %
:- pred generate_closure(pred_id::in, proc_id::in, lambda_eval_method::in,
    prog_var::in, list(prog_var)::in, hlds_goal_info::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_closure(PredId, ProcId, EvalMethod, Var, Args, GoalInfo, Code, !CI) :-
    code_info.get_module_info(!.CI, ModuleInfo),
    module_info_preds(ModuleInfo, Preds),
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_procedures(PredInfo, Procs),
    map.lookup(Procs, ProcId, ProcInfo),

    % We handle currying of a higher-order pred variable as a special case.
    % We recognize
    %
    %   P = l(P0, X, Y, Z)
    %
    % where
    %
    %   l(P0, A, B, C, ...) :- P0(A, B, C, ...).    % higher-order call
    %
    % as a special case, and generate special code to construct the
    % new closure P from the old closure P0 by appending the args X, Y, Z.
    % The advantage of this optimization is that when P is called, we
    % will only need to do one indirect call rather than two.
    % Its disadvantage is that the cost of creating the closure P is greater.
    % Whether this is a net win depend on the number of times P is called.
    %
    % The pattern that this optimization looks for happens rarely at the
    % moment. The reason is that although we allow the creation of closures
    % with a simple syntax (e.g. P0 = append4([1])), we don't allow their
    % extension with a similarly simple syntax (e.g. P = call(P0, [2])).
    % In fact, typecheck.m contains code to detect such constructs, because
    % it does not have code to typecheck them (you get a message about call/2
    % should be used as a goal, not an expression).

    proc_info_get_goal(ProcInfo, ProcInfoGoal),
    proc_info_interface_code_model(ProcInfo, CodeModel),
    proc_info_get_headvars(ProcInfo, ProcHeadVars),
    (
        EvalMethod = lambda_normal,
        Args = [CallPred | CallArgs],
        ProcHeadVars = [ProcPred | ProcArgs],
        ProcInfoGoal = generic_call(higher_order(ProcPred, _, _, _),
            ProcArgs, _, CallDeterminism) - _GoalInfo,
        determinism_to_code_model(CallDeterminism, CallCodeModel),
        % Check that the code models are compatible. Note that det is not
        % compatible with semidet, and semidet is not compatible with nondet,
        % since the arguments go in different registers.
        % But det is compatible with nondet.
        (
            CodeModel = CallCodeModel
        ;
            CodeModel = model_non,
            CallCodeModel = model_det
        ),
        % This optimization distorts deep profiles, so don't perform it
        % in deep profiling grades.
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, profile_deep, Deep),
        Deep = no
    ->
        (
            CallArgs = [],
            % If there are no new arguments, we can just use the old closure.
            code_info.assign_var_to_var(Var, CallPred, !CI),
            Code = empty
        ;
            CallArgs = [_ | _],
            code_info.get_next_label(LoopStart, !CI),
            code_info.get_next_label(LoopTest, !CI),
            code_info.acquire_reg(reg_r, LoopCounter, !CI),
            code_info.acquire_reg(reg_r, NumOldArgs, !CI),
            code_info.acquire_reg(reg_r, NewClosure, !CI),
            Zero = const(llconst_int(0)),
            One = const(llconst_int(1)),
            Two = const(llconst_int(2)),
            Three = const(llconst_int(3)),
            list.length(CallArgs, NumNewArgs),
            NumNewArgs_Rval = const(llconst_int(NumNewArgs)),
            NumNewArgsPlusThree = NumNewArgs + 3,
            NumNewArgsPlusThree_Rval = const(llconst_int(NumNewArgsPlusThree)),
            code_info.produce_variable(CallPred, OldClosureCode,
                OldClosure, !CI),
            % The new closure contains a pointer to the old closure.
            NewClosureMayUseAtomic = may_not_use_atomic_alloc,
            NewClosureCode = node([
                comment("build new closure from old closure") - "",
                assign(NumOldArgs, lval(field(yes(0), OldClosure, Two)))
                    - "get number of arguments",
                incr_hp(NewClosure, no, no,
                    binop(int_add, lval(NumOldArgs), NumNewArgsPlusThree_Rval),
                    "closure", NewClosureMayUseAtomic)
                    - "allocate new closure",
                assign(field(yes(0), lval(NewClosure), Zero),
                    lval(field(yes(0), OldClosure, Zero)))
                    - "set closure layout structure",
                assign(field(yes(0), lval(NewClosure), One),
                    lval(field(yes(0), OldClosure, One)))
                    - "set closure code pointer",
                assign(field(yes(0), lval(NewClosure), Two),
                    binop(int_add, lval(NumOldArgs), NumNewArgs_Rval))
                    - "set new number of arguments",
                assign(NumOldArgs, binop(int_add, lval(NumOldArgs), Three))
                    - "set up loop limit",
                assign(LoopCounter, Three)
                    - "initialize loop counter",
                % It is possible for the number of hidden arguments to be zero,
                % in which case the body of this loop should not be executed
                % at all. This is why we jump to the loop condition test.
                goto(code_label(LoopTest))
                    - ("enter the copy loop at the conceptual top"),
                label(LoopStart) - "start of loop",
                assign(field(yes(0), lval(NewClosure), lval(LoopCounter)),
                    lval(field(yes(0), OldClosure, lval(LoopCounter))))
                    - "copy old hidden argument",
                assign(LoopCounter, binop(int_add, lval(LoopCounter), One))
                    - "increment loop counter",
                label(LoopTest)
                    - ("do we have more old arguments to copy?"),
                if_val(binop(int_lt, lval(LoopCounter), lval(NumOldArgs)),
                    code_label(LoopStart))
                    - "repeat the loop?"
            ]),
            generate_extra_closure_args(CallArgs, LoopCounter, NewClosure,
                ExtraArgsCode, !CI),
            code_info.release_reg(LoopCounter, !CI),
            code_info.release_reg(NumOldArgs, !CI),
            code_info.release_reg(NewClosure, !CI),
            code_info.assign_lval_to_var(Var, NewClosure, AssignCode, !CI),
            Code = tree_list([OldClosureCode, NewClosureCode, ExtraArgsCode,
                 AssignCode])
        )
    ;
        CodeAddr = make_proc_entry_label(!.CI, ModuleInfo, PredId, ProcId, no),
        ProcLabel = extract_proc_label_from_code_addr(CodeAddr),
        CodeAddrRval = const(llconst_code_addr(CodeAddr)),
        continuation_info.generate_closure_layout( ModuleInfo, PredId, ProcId,
            ClosureInfo),
        module_info_get_name(ModuleInfo, ModuleName),
        goal_info_get_context(GoalInfo, Context),
        term.context_file(Context, FileName),
        term.context_line(Context, LineNumber),
        goal_info_get_goal_path(GoalInfo, GoalPath),
        GoalPathStr = goal_path_to_string(GoalPath),
        code_info.get_cur_proc_label(!.CI, CallerProcLabel),
        code_info.get_next_closure_seq_no(SeqNo, !CI),
        code_info.get_static_cell_info(!.CI, StaticCellInfo0),
        hlds.hlds_pred.pred_info_get_origin(PredInfo, PredOrigin),
        stack_layout.construct_closure_layout(CallerProcLabel,
            SeqNo, ClosureInfo, ProcLabel, ModuleName, FileName, LineNumber,
            PredOrigin, GoalPathStr, StaticCellInfo0, StaticCellInfo,
            ClosureLayoutRvalsTypes, Data),
        code_info.set_static_cell_info(StaticCellInfo, !CI),
        code_info.add_closure_layout(Data, !CI),
        % For now, closures always have zero size, and the size slot
        % is never looked at.
        code_info.add_scalar_static_cell(ClosureLayoutRvalsTypes,
            ClosureDataAddr, !CI),
        ClosureLayoutRval = const(llconst_data_addr(ClosureDataAddr, no)),
        list.length(Args, NumArgs),
        proc_info_arg_info(ProcInfo, ArgInfo),
        VarTypes = get_var_types(!.CI),
        MayUseAtomic0 = initial_may_use_atomic(ModuleInfo),
        generate_pred_args(ModuleInfo, VarTypes, Args, ArgInfo, PredArgs,
            MayUseAtomic0, MayUseAtomic),
        Vector = [
            yes(ClosureLayoutRval),
            yes(CodeAddrRval),
            yes(const(llconst_int(NumArgs)))
            | PredArgs
        ],
        code_info.assign_cell_to_var(Var, no, 0, Vector, no, "closure",
            MayUseAtomic, Code, !CI)
    ).

:- pred generate_extra_closure_args(list(prog_var)::in, lval::in,
    lval::in, code_tree::out, code_info::in, code_info::out) is det.

generate_extra_closure_args([], _, _, empty, !CI).
generate_extra_closure_args([Var | Vars], LoopCounter, NewClosure, Code,
        !CI) :-
    code_info.produce_variable(Var, Code0, Value, !CI),
    One = const(llconst_int(1)),
    Code1 = node([
        assign(field(yes(0), lval(NewClosure), lval(LoopCounter)), Value)
            - "set new argument field",
        assign(LoopCounter, binop(int_add, lval(LoopCounter), One))
            - "increment argument counter"
    ]),
    generate_extra_closure_args(Vars, LoopCounter, NewClosure, Code2, !CI),
    Code = tree_list([Code0, Code1, Code2]).

:- pred generate_pred_args(module_info::in, vartypes::in, list(prog_var)::in,
    list(arg_info)::in, list(maybe(rval))::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

generate_pred_args(_, _, [], _, [], !MayUseAtomic).
generate_pred_args(_, _, [_ | _], [], _, !MayUseAtomic) :-
    unexpected(this_file, "generate_pred_args: insufficient args").
generate_pred_args(ModuleInfo, VarTypes, [Var | Vars], [ArgInfo | ArgInfos],
        [Rval | Rvals], !MayUseAtomic) :-
    ArgInfo = arg_info(_, ArgMode),
    ( ArgMode = top_in ->
        Rval = yes(var(Var))
    ;
        Rval = no
    ),
    map.lookup(VarTypes, Var, Type),
    update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic),
    generate_pred_args(ModuleInfo, VarTypes, Vars, ArgInfos, Rvals,
        !MayUseAtomic).

:- pred generate_cons_args(list(prog_var)::in, list(mer_type)::in,
    list(uni_mode)::in, int::in, int::in, list(int)::in, module_info::in,
    list(maybe(rval))::out, assoc_list(int, prog_var)::out,
    may_use_atomic_alloc::out) is det.

generate_cons_args(Vars, Types, Modes, FirstOffset, FirstArgNum, TakeAddr,
        ModuleInfo, !:Args, !:FieldAddrs, !:MayUseAtomic) :-
    !:MayUseAtomic = initial_may_use_atomic(ModuleInfo),
    (
        generate_cons_args_2(Vars, Types, Modes, FirstOffset, FirstArgNum,
            TakeAddr, ModuleInfo, !:Args, !:FieldAddrs, !MayUseAtomic)
    ->
        true
    ;
        unexpected(this_file, "generate_cons_args: length mismatch")
    ).

    % Create a list of maybe(rval) for the arguments for a construction
    % unification. For each argument which is input to the construction
    % unification, we produce `yes(var(Var))', but if the argument is free,
    % we just produce `no', meaning don't generate an assignment to that field.
    %
:- pred generate_cons_args_2(list(prog_var)::in, list(mer_type)::in,
    list(uni_mode)::in, int::in, int::in, list(int)::in, module_info::in,
    list(maybe(rval))::out, assoc_list(int, prog_var)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is semidet.

generate_cons_args_2([], [], [], _, _, [], _, [], [], !MayUseAtomic).
generate_cons_args_2([Var | Vars], [Type | Types], [UniMode | UniModes],
        FirstOffset, CurArgNum, !.TakeAddr, ModuleInfo, [Rval | Rvals],
        FieldAddrs, !MayUseAtomic) :-
    update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic),
    ( !.TakeAddr = [CurArgNum | !:TakeAddr] ->
        Rval = no,
        !:MayUseAtomic = may_not_use_atomic_alloc,
        generate_cons_args_2(Vars, Types, UniModes, FirstOffset, CurArgNum + 1,
            !.TakeAddr, ModuleInfo, Rvals, FieldAddrs1, !MayUseAtomic),
        % Whereas CurArgNum starts numbering the arguments from 1, offsets
        % into fields start from zero. However, if FirstOffset = 1, then the
        % first word in the cell is the secondary tag.
        Offset = CurArgNum - 1 + FirstOffset,
        FieldAddrs = [Offset - Var | FieldAddrs1]
    ;
        UniMode = ((_LI - RI) -> (_LF - RF)),
        mode_to_arg_mode(ModuleInfo, (RI -> RF), Type, ArgMode),
        ( ArgMode = top_in ->
            Rval = yes(var(Var))
        ;
            Rval = no
        ),
        generate_cons_args_2(Vars, Types, UniModes, FirstOffset, CurArgNum + 1,
            !.TakeAddr, ModuleInfo, Rvals, FieldAddrs, !MayUseAtomic)
    ).

:- func initial_may_use_atomic(module_info) = may_use_atomic_alloc.

initial_may_use_atomic(ModuleInfo) = InitMayUseAtomic :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, use_atomic_cells, UseAtomicCells),
    (
        UseAtomicCells = no,
        InitMayUseAtomic = may_not_use_atomic_alloc
    ;
        UseAtomicCells = yes,
        InitMayUseAtomic = may_use_atomic_alloc
    ).

:- pred construct_cell(prog_var::in, tag::in, list(maybe(rval))::in,
    maybe(term_size_value)::in, assoc_list(int, prog_var)::in,
    may_use_atomic_alloc::in, code_tree::out, code_info::in, code_info::out)
    is det.

construct_cell(Var, Ptag, MaybeRvals, MaybeSize, FieldAddrs, MayUseAtomic,
        Code, !CI) :-
    VarType = code_info.variable_type(!.CI, Var),
    var_type_msg(VarType, VarTypeMsg),
    % If we're doing accurate GC, then for types which hold RTTI that
    % will be traversed by the collector at GC-time, we need to allocate
    % an extra word at the start, to hold the forwarding pointer.
    % Normally we would just overwrite the first word of the object
    % in the "from" space, but this can't be done for objects which will be
    % referenced during the garbage collection process.
    (
        code_info.get_globals(!.CI, Globals),
        globals.get_gc_method(Globals, GCMethod),
        GCMethod = gc_accurate,
        is_introduced_type_info_type(VarType)
    ->
        ReserveWordAtStart = yes
    ;
        ReserveWordAtStart = no
    ),
    code_info.assign_cell_to_var(Var, ReserveWordAtStart, Ptag, MaybeRvals,
        MaybeSize, VarTypeMsg, MayUseAtomic, CellCode, !CI),
    (
        FieldAddrs = [],
        % Optimize common case.
        Code = CellCode
    ;
        FieldAddrs = [_ | _],
        % Any field whose address we take will be represented by a `no'
        % in MaybeRvals, which should prevent the cell from being made
        % into static data.
        generate_field_take_address_assigns(FieldAddrs, Var, Ptag,
            FieldCode, !CI),
        Code = tree(CellCode, FieldCode)
    ).

:- pred generate_field_take_address_assigns(assoc_list(int, prog_var)::in,
    prog_var::in, int::in, code_tree::out, code_info::in, code_info::out)
    is det.

generate_field_take_address_assigns([], _, _, empty, !CI).
generate_field_take_address_assigns([FieldNum - Var | FieldAddrs],
        CellVar, CellPtag, tree(ThisCode, RestCode), !CI) :-
    FieldNumRval = const(llconst_int(FieldNum)),
    Addr = mem_addr(heap_ref(var(CellVar), CellPtag, FieldNumRval)),
    assign_expr_to_var(Var, Addr, ThisCode, !CI),
    generate_field_take_address_assigns(FieldAddrs, CellVar, CellPtag,
        RestCode, !CI).

%---------------------------------------------------------------------------%

:- pred var_types(code_info::in, list(prog_var)::in, list(mer_type)::out)
    is det.

var_types(CI, Vars, Types) :-
    code_info.get_proc_info(CI, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    map.apply_to_list(Vars, VarTypes, Types).

%---------------------------------------------------------------------------%

    % Construct a pair of lists that associates the fields of a term
    % with variables.
    %
:- pred make_fields_and_argvars(list(prog_var)::in, rval::in,
    int::in, int::in, list(uni_val)::out, list(uni_val)::out) is det.

make_fields_and_argvars([], _, _, _, [], []).
make_fields_and_argvars([Var | Vars], Rval, Field0, TagNum,
        [F | Fs], [A | As]) :-
    F = lval(field(yes(TagNum), Rval, const(llconst_int(Field0)))),
    A = ref(Var),
    Field1 = Field0 + 1,
    make_fields_and_argvars(Vars, Rval, Field1, TagNum, Fs, As).

%---------------------------------------------------------------------------%

    % Generate a deterministic deconstruction. In a deterministic
    % deconstruction, we know the value of the tag, so we don't
    % need to generate a test.

    % Deconstructions are generated semi-eagerly. Any test sub-unifications
    % are generated eagerly (they _must_ be), but assignment unifications
    % are cached.
    %
:- pred generate_det_deconstruction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(uni_mode)::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_det_deconstruction(Var, Cons, Args, Modes, Code, !CI) :-
    Tag = cons_id_to_tag_for_var(!.CI, Var, Cons),
    generate_det_deconstruction_2(Var, Cons, Args, Modes, Tag, Code, !CI).

:- pred generate_det_deconstruction_2(prog_var::in, cons_id::in,
    list(prog_var)::in, list(uni_mode)::in, cons_tag::in,
    code_tree::out, code_info::in, code_info::out) is det.

generate_det_deconstruction_2(Var, Cons, Args, Modes, Tag, Code, !CI) :-
    % For constants, if the deconstruction is det, then we already know
    % the value of the constant, so Code = empty.
    (
        ( Tag = string_tag(_String)
        ; Tag = int_tag(_Int)
        ; Tag = float_tag(_Float)
        ; Tag = pred_closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = shared_local_tag(_Ptag, _Sectag2)
        ; Tag = reserved_address_tag(_RA)
        ),
        Code = empty
    ;
        Tag = table_io_decl_tag(_, _),
        unexpected(this_file, "generate_det_deconstruction: table_io_decl_tag")
    ;
        Tag = no_tag,
        (
            Args = [Arg],
            Modes = [Mode]
        ->
            VarType = code_info.variable_type(!.CI, Var),
            code_info.get_module_info(!.CI, ModuleInfo),
            ( is_dummy_argument_type(ModuleInfo, VarType) ->
                % We must handle this case specially. If we didn't, the
                % generated code would copy the reference to the Var's
                % current location, which may be stackvar(N) or framevar(N)
                % for negative N, to be the location of Arg, and since Arg
                % may not be a dummy type, it would actually use that location.
                % This can happen in the unify/compare routines for e.g.
                % io.state.
                ( variable_is_forward_live(!.CI, Arg) ->
                    code_info.assign_const_to_var(Arg, const(llconst_int(0)),
                        !CI)
                ;
                    true
                ),
                Code = empty
            ;
                ArgType = code_info.variable_type(!.CI, Arg),
                generate_sub_unify(ref(Var), ref(Arg), Mode, ArgType, Code,
                    !CI)
            )
        ;
            unexpected(this_file,
                "generate_det_deconstruction: no_tag: arity != 1")
        )
    ;
        Tag = single_functor_tag,
        % Treat single_functor the same as unshared_tag(0).
        generate_det_deconstruction_2(Var, Cons, Args, Modes, unshared_tag(0),
            Code, !CI)
    ;
        Tag = unshared_tag(Ptag),
        Rval = var(Var),
        make_fields_and_argvars(Args, Rval, 0, Ptag, Fields, ArgVars),
        var_types(!.CI, Args, ArgTypes),
        generate_unify_args(Fields, ArgVars, Modes, ArgTypes, Code, !CI)
    ;
        Tag = shared_remote_tag(Ptag, _Sectag1),
        Rval = var(Var),
        make_fields_and_argvars(Args, Rval, 1, Ptag, Fields, ArgVars),
        var_types(!.CI, Args, ArgTypes),
        generate_unify_args(Fields, ArgVars, Modes, ArgTypes, Code, !CI)
    ;
        % For shared_with_reserved_address, the sharing is only important
        % for tag tests, not for det deconstructions, so here we just recurse
        % on the real representation.
        Tag = shared_with_reserved_addresses_tag(_RAs, ThisTag),
        generate_det_deconstruction_2(Var, Cons, Args, Modes, ThisTag, Code,
            !CI)
    ).

%---------------------------------------------------------------------------%

    % Generate a semideterministic deconstruction.
    % A semideterministic deconstruction unification is tag-test
    % followed by a deterministic deconstruction.
    %
:- pred generate_semi_deconstruction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(uni_mode)::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_semi_deconstruction(Var, Tag, Args, Modes, Code, !CI) :-
    generate_tag_test(Var, Tag, branch_on_success, SuccLab, TagTestCode, !CI),
    code_info.remember_position(!.CI, AfterUnify),
    code_info.generate_failure(FailCode, !CI),
    code_info.reset_to_position(AfterUnify, !CI),
    generate_det_deconstruction(Var, Tag, Args, Modes, DeconsCode, !CI),
    SuccessLabelCode = node([label(SuccLab) - ""]),
    Code = tree_list([TagTestCode, FailCode, SuccessLabelCode, DeconsCode]).

%---------------------------------------------------------------------------%

    % Generate code to perform a list of deterministic subunifications
    % for the arguments of a construction.
    %
:- pred generate_unify_args(list(uni_val)::in, list(uni_val)::in,
    list(uni_mode)::in, list(mer_type)::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_unify_args(Ls, Rs, Ms, Ts, Code, !CI) :-
    ( generate_unify_args_2(Ls, Rs, Ms, Ts, Code0, !CI) ->
        Code = Code0
    ;
        unexpected(this_file, "generate_unify_args: length mismatch")
    ).

:- pred generate_unify_args_2(list(uni_val)::in, list(uni_val)::in,
    list(uni_mode)::in, list(mer_type)::in, code_tree::out,
    code_info::in, code_info::out) is semidet.

generate_unify_args_2([], [], [], [], empty, !CI).
generate_unify_args_2([L | Ls], [R | Rs], [M | Ms], [T | Ts], Code, !CI) :-
    generate_sub_unify(L, R, M, T, CodeA, !CI),
    generate_unify_args_2(Ls, Rs, Ms, Ts, CodeB, !CI),
    Code = tree(CodeA, CodeB).

%---------------------------------------------------------------------------%

    % Generate a subunification between two [field | variable].
    %
:- pred generate_sub_unify(uni_val::in, uni_val::in, uni_mode::in,
    mer_type::in, code_tree::out, code_info::in, code_info::out) is det.

generate_sub_unify(L, R, Mode, Type, Code, !CI) :-
    Mode = ((LI - RI) -> (LF - RF)),
    code_info.get_module_info(!.CI, ModuleInfo),
    mode_to_arg_mode(ModuleInfo, (LI -> LF), Type, LeftMode),
    mode_to_arg_mode(ModuleInfo, (RI -> RF), Type, RightMode),
    (
        % Input - input == test unification
        LeftMode = top_in,
        RightMode = top_in
    ->
        % This shouldn't happen, since mode analysis should
        % avoid creating any tests in the arguments
        % of a construction or deconstruction unification.
        unexpected(this_file, "test in arg of [de]construction")
    ;
        % Input - Output== assignment ->
        LeftMode = top_in,
        RightMode = top_out
    ->
        generate_sub_assign(R, L, Code, !CI)
    ;
            % Output - Input== assignment <-
        LeftMode = top_out,
        RightMode = top_in
    ->
        generate_sub_assign(L, R, Code, !CI)
    ;
        LeftMode = top_unused,
        RightMode = top_unused
    ->
        Code = empty
        % free-free - ignore
        % XXX I think this will have to change if we start to support aliasing.
    ;
        unexpected(this_file, "generate_sub_unify: some strange unify")
    ).

%---------------------------------------------------------------------------%

:- pred generate_sub_assign(uni_val::in, uni_val::in, code_tree::out,
    code_info::in, code_info::out) is det.

generate_sub_assign(Left, Right, Code, !CI) :-
    (
        Left = lval(_Lval),
        Right = lval(_Rval),
        % Assignment between two lvalues - cannot happen.
        unexpected(this_file, "generate_sub_assign: lval/lval")
    ;
        Left = lval(Lval0),
        Right = ref(Var),
        % Assignment from a variable to an lvalue - cannot cache
        % so generate immediately.
        code_info.produce_variable(Var, SourceCode, Source, !CI),
        code_info.materialize_vars_in_lval(Lval0, Lval, MaterializeCode, !CI),
        CopyCode = node([assign(Lval, Source) - "Copy value"]),
        Code = tree_list([SourceCode, MaterializeCode, CopyCode])
    ;
        Left = ref(Lvar),
        ( code_info.variable_is_forward_live(!.CI, Lvar) ->
            (
                Right = lval(Lval),
                % Assignment of a value to a variable, generate now.
                code_info.assign_lval_to_var(Lvar, Lval, Code, !CI)
            ;
                Right = ref(Rvar),
                % Assignment of a variable to a variable, so cache it.
                code_info.assign_var_to_var(Lvar, Rvar, !CI),
                Code = empty
            )
        ;
            Code = empty
        )
    ).

%---------------------------------------------------------------------------%

:- pred var_type_msg(mer_type::in, string::out) is det.

var_type_msg(Type, Msg) :-
    ( type_to_ctor_and_args(Type, TypeCtor, _) ->
        TypeCtor = type_ctor(TypeSym, TypeArity),
        TypeSymStr = sym_name_to_string(TypeSym),
        string.int_to_string(TypeArity, TypeArityStr),
        string.append_list([TypeSymStr, "/", TypeArityStr], Msg)
    ;
        unexpected(this_file, "type is still a type variable in var_type_msg")
    ).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "unify_gen.m".

%---------------------------------------------------------------------------%
:- end_module unify_gen.
%---------------------------------------------------------------------------%
