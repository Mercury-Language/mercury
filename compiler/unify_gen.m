%---------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------e
% Copyright (C) 1994-2009 The University of Melbourne.
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
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.llds.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type test_sense
    --->    branch_on_success
    ;       branch_on_failure.

:- pred generate_unification(code_model::in, unification::in,
    hlds_goal_info::in, llds_code::out, code_info::in, code_info::out) is det.

:- pred generate_tag_test(prog_var::in, cons_id::in,
    maybe_cheaper_tag_test::in, test_sense::in, label::out, llds_code::out,
    code_info::in, code_info::out) is det.

:- pred generate_raw_tag_test_case(rval::in, mer_type::in, string::in,
    tagged_cons_id::in, list(tagged_cons_id)::in, maybe_cheaper_tag_test::in,
    test_sense::in, label::out, llds_code::out, code_info::in, code_info::out)
    is det.

:- pred generate_ground_term(prog_var::in, hlds_goal::in,
    code_info::in, code_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module backend_libs.type_class_info.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.global_data.
:- import_module ll_backend.layout.
:- import_module ll_backend.stack_layout.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module term.

%---------------------------------------------------------------------------%

:- type uni_val
    --->    ref(prog_var)
    ;       lval(lval).

%---------------------------------------------------------------------------%

generate_unification(CodeModel, Uni, GoalInfo, Code, !CI) :-
    (
        CodeModel = model_det
    ;
        CodeModel = model_semi
    ;
        CodeModel = model_non,
        unexpected(this_file, "nondet unification in generate_unification")
    ),
    (
        Uni = assign(Left, Right),
        ( variable_is_forward_live(!.CI, Left) ->
            generate_assignment(Left, Right, Code, !CI)
        ;
            Code = empty
        )
    ;
        Uni = construct(Var, ConsId, Args, Modes, HowToConstruct, _, SubInfo),
        (
            SubInfo = no_construct_sub_info,
            MaybeTakeAddr = no,
            MaybeSize = no
        ;
            SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize)
        ),
        (
            ( variable_is_forward_live(!.CI, Var)
            ; MaybeTakeAddr = yes(_)
            )
        ->
            (
                MaybeTakeAddr = yes(TakeAddr)
            ;
                MaybeTakeAddr = no,
                TakeAddr = []
            ),
            generate_construction(Var, ConsId, Args, Modes, HowToConstruct,
                TakeAddr, MaybeSize, GoalInfo, Code, !CI)
        ;
            Code = empty
        )
    ;
        Uni = deconstruct(Var, ConsId, Args, Modes, _CanFail, CanCGC),
        ( CodeModel = model_det ->
            generate_det_deconstruction(Var, ConsId, Args, Modes, Code0, !CI)
        ;
            generate_semi_deconstruction(Var, ConsId, Args, Modes, Code0, !CI)
        ),
        (
            CanCGC = can_cgc,
            VarName = variable_name(!.CI, Var),
            produce_variable(Var, ProduceVar, VarRval, !CI),
            ( VarRval = lval(VarLval) ->
                save_reused_cell_fields(Var, VarLval, SaveArgs, Regs, !CI),
                % This seems to be fine.
                list.foldl(release_reg, Regs, !CI),
                % XXX avoid strip_tag when we know what tag it will have
                FreeVar = singleton(
                    llds_instr(free_heap(unop(strip_tag, VarRval)),
                        "Free " ++ VarName)
                ),
                Code = Code0 ++ ProduceVar ++ SaveArgs ++ FreeVar
            ;
                Code = Code0
            )
        ;
            CanCGC = cannot_cgc,
            Code = Code0
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
:- pred generate_assignment(prog_var::in, prog_var::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_assignment(VarA, VarB, empty, !CI) :-
    ( variable_is_forward_live(!.CI, VarA) ->
        assign_var_to_var(VarA, VarB, !CI)
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
:- pred generate_test(prog_var::in, prog_var::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_test(VarA, VarB, Code, !CI) :-
    IsDummy = variable_is_of_dummy_type(!.CI, VarA),
    (
        IsDummy = is_dummy_type,
        Code = empty
    ;
        IsDummy = is_not_dummy_type,
        produce_variable(VarA, CodeA, ValA, !CI),
        produce_variable(VarB, CodeB, ValB, !CI),
        Type = variable_type(!.CI, VarA),
        ( Type = builtin_type(builtin_type_string) ->
            Op = str_eq
        ; Type = builtin_type(builtin_type_float) ->
            Op = float_eq
        ;
            Op = eq
        ),
        fail_if_rval_is_false(binop(Op, ValA, ValB), FailCode, !CI),
        Code = CodeA ++ CodeB ++ FailCode
    ).

%---------------------------------------------------------------------------%

generate_raw_tag_test_case(VarRval, VarType, VarName,
        MainTaggedConsId, OtherTaggedConsIds, CheaperTagTest,
        Sense, ElseLabel, Code, !CI) :-
    (
        OtherTaggedConsIds = [],
        MainTaggedConsId = tagged_cons_id(MainConsId, MainConsTag),
        generate_raw_tag_test(VarRval, VarType, VarName,
            MainConsId, yes(MainConsTag), CheaperTagTest, Sense, ElseLabel,
            Code, !CI)
    ;
        OtherTaggedConsIds = [_ | _],
        % The cheaper tag test optimization doesn't apply.
        project_cons_name_and_tag(MainTaggedConsId, MainConsName, MainConsTag),
        list.map2(project_cons_name_and_tag, OtherTaggedConsIds,
            OtherConsNames, OtherConsTags),
        Comment = branch_sense_comment(Sense) ++
            case_comment(VarName, MainConsName, OtherConsNames),
        raw_tag_test(VarRval, MainConsTag, MainTagTestRval),
        list.map(raw_tag_test(VarRval), OtherConsTags, OtherTagTestRvals),
        disjoin_tag_tests(MainTagTestRval, OtherTagTestRvals, TestRval),
        get_next_label(ElseLabel, !CI),
        (
            Sense = branch_on_success,
            TheRval = TestRval
        ;
            Sense = branch_on_failure,
            code_util.neg_rval(TestRval, TheRval)
        ),
        Code = singleton(
            llds_instr(if_val(TheRval, code_label(ElseLabel)), Comment)
        )
    ).

:- pred disjoin_tag_tests(rval::in, list(rval)::in, rval::out) is det.

disjoin_tag_tests(CurTestRval, OtherTestRvals, TestRval) :-
    (
        OtherTestRvals = [],
        TestRval = CurTestRval
    ;
        OtherTestRvals = [HeadTestRval | TailTestRvals],
        NextTestRval = binop(logical_or, CurTestRval, HeadTestRval),
        disjoin_tag_tests(NextTestRval, TailTestRvals, TestRval)
    ).

%---------------------------------------------------------------------------%

generate_tag_test(Var, ConsId, CheaperTagTest, Sense, ElseLabel, Code, !CI) :-
    produce_variable(Var, VarCode, VarRval, !CI),
    VarType = variable_type(!.CI, Var),
    VarName = variable_name(!.CI, Var),
    generate_raw_tag_test(VarRval, VarType, VarName, ConsId, no,
        CheaperTagTest, Sense, ElseLabel, TestCode, !CI),
    Code = VarCode ++ TestCode.

:- pred generate_raw_tag_test(rval::in, mer_type::in, string::in,
    cons_id::in, maybe(cons_tag)::in,
    maybe_cheaper_tag_test::in, test_sense::in, label::out, llds_code::out,
    code_info::in, code_info::out) is det.

generate_raw_tag_test(VarRval, _VarType, VarName, ConsId, MaybeConsTag,
        CheaperTagTest, Sense, ElseLabel, Code, !CI) :-
    ConsIdName = cons_id_and_arity_to_string(ConsId),
    % As an optimization, for data types with exactly two alternatives,
    % one of which is a constant, we make sure that we test against the
    % constant (negating the result of the test, if needed),
    % since a test against a constant is cheaper than a tag test.
    (
        CheaperTagTest = cheaper_tag_test(ExpensiveConsId, _ExpensiveConsTag,
            _CheapConsId, CheapConsTag),
        ConsId = ExpensiveConsId
    ->
        Comment = branch_sense_comment(Sense) ++ VarName ++
            " has functor " ++ ConsIdName ++ " (inverted test)",
        raw_tag_test(VarRval, CheapConsTag, NegTestRval),
        code_util.neg_rval(NegTestRval, TestRval)
    ;
        Comment = branch_sense_comment(Sense) ++ VarName ++
            " has functor " ++ ConsIdName,
        (
            MaybeConsTag = yes(ConsTag)
            % Our caller has already computed ConsTag.
        ;
            MaybeConsTag = no,
            get_module_info(!.CI, ModuleInfo),
            ConsTag = cons_id_to_tag(ModuleInfo, ConsId)
        ),
        raw_tag_test(VarRval, ConsTag, TestRval)
    ),
    get_next_label(ElseLabel, !CI),
    (
        Sense = branch_on_success,
        TheRval = TestRval
    ;
        Sense = branch_on_failure,
        code_util.neg_rval(TestRval, TheRval)
    ),
    Code = singleton(
        llds_instr(if_val(TheRval, code_label(ElseLabel)), Comment)
    ).

:- func branch_sense_comment(test_sense) = string.

branch_sense_comment(branch_on_success) =
    "branch away if ".
branch_sense_comment(branch_on_failure) =
    "branch away unless ".

%---------------------------------------------------------------------------%

:- pred raw_tag_test(rval::in, cons_tag::in, rval::out) is det.

raw_tag_test(Rval, ConsTag, TestRval) :-
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
        ConsTag = foreign_tag(ForeignLang, ForeignVal),
        expect(unify(ForeignLang, lang_c), this_file,
            "foreign tag for language other than C"),
        TestRval = binop(eq, Rval,
            const(llconst_foreign(ForeignVal, lt_integer)))
    ;
        ConsTag = closure_tag(_, _, _),
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
            raw_tag_test(Rval, reserved_address_tag(RA), EqualRA),
            InnerTestRval = binop(logical_and,
                unop(logical_not, EqualRA), InnerTestRval0)
        ),
        raw_tag_test(Rval, ThisTag, MatchesThisTag),
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
    % constructing the new term statically. If not all the arguments are
    % constants, the construction is implemented as a heap-increment
    % to create a term, and a series of [optional] assignments to
    % instantiate the arguments of that term.
    %
:- pred generate_construction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(uni_mode)::in, how_to_construct::in,
    list(int)::in, maybe(term_size_value)::in, hlds_goal_info::in,
    llds_code::out, code_info::in, code_info::out) is det.

generate_construction(Var, ConsId, Args, Modes, HowToConstruct,
        TakeAddr, MaybeSize, GoalInfo, Code, !CI) :-
    get_module_info(!.CI, ModuleInfo),
    Tag = cons_id_to_tag(ModuleInfo, ConsId),
    generate_construction_2(Tag, Var, Args, Modes, HowToConstruct,
        TakeAddr, MaybeSize, GoalInfo, Code, !CI).

:- pred generate_construction_2(cons_tag::in, prog_var::in,
    list(prog_var)::in, list(uni_mode)::in, how_to_construct::in,
    list(int)::in, maybe(term_size_value)::in, hlds_goal_info::in,
    llds_code::out, code_info::in, code_info::out) is det.

generate_construction_2(ConsTag, Var, Args, Modes, HowToConstruct,
        TakeAddr, MaybeSize, GoalInfo, Code, !CI) :-
    (
        ConsTag = string_tag(String),
        assign_const_to_var(Var, const(llconst_string(String)), !CI),
        Code = empty
    ;
        ConsTag = int_tag(Int),
        assign_const_to_var(Var, const(llconst_int(Int)), !CI),
        Code = empty
    ;
        ConsTag = foreign_tag(Lang, Val),
        expect(unify(Lang, lang_c), this_file,
            "foreign_tag for language other than C"),
        ForeignConst = const(llconst_foreign(Val, lt_integer)),
        assign_const_to_var(Var, ForeignConst, !CI),
        Code = empty
    ;
        ConsTag = float_tag(Float),
        assign_const_to_var(Var, const(llconst_float(Float)), !CI),
        Code = empty
    ;
        ConsTag = no_tag,
        (
            Args = [Arg],
            Modes = [Mode]
        ->
            (
                TakeAddr = [],
                Type = variable_type(!.CI, Arg),
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
        (
            ConsTag = single_functor_tag,
            % Treat single_functor the same as unshared_tag(0).
            Ptag = 0
        ;
            ConsTag = unshared_tag(Ptag)
        ),
        var_types(!.CI, Args, ArgTypes),
        generate_cons_args(Args, ArgTypes, Modes, 0, 1, TakeAddr, !.CI,
            MaybeRvals, FieldAddrs, MayUseAtomic),
        construct_cell(Var, Ptag, MaybeRvals, HowToConstruct,
            MaybeSize, FieldAddrs, MayUseAtomic, Code, !CI)
    ;
        ConsTag = shared_remote_tag(Ptag, Sectag),
        var_types(!.CI, Args, ArgTypes),
        generate_cons_args(Args, ArgTypes, Modes, 1, 1, TakeAddr, !.CI,
            MaybeRvals0, FieldAddrs, MayUseAtomic),
        % The first field holds the secondary tag.
        MaybeRvals = [yes(const(llconst_int(Sectag))) | MaybeRvals0],
        construct_cell(Var, Ptag, MaybeRvals, HowToConstruct,
            MaybeSize, FieldAddrs, MayUseAtomic, Code, !CI)
    ;
        ConsTag = shared_local_tag(Ptag, Sectag),
        assign_const_to_var(Var,
            mkword(Ptag, unop(mkbody, const(llconst_int(Sectag)))), !CI),
        Code = empty
    ;
        ConsTag = type_ctor_info_tag(ModuleName, TypeName, TypeArity),
        expect(unify(Args, []), this_file,
            "generate_construction_2: type_ctor_info constant has args"),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
        DataId = rtti_data_id(ctor_rtti_id(RttiTypeCtor,
            type_ctor_type_ctor_info)),
        assign_const_to_var(Var, const(llconst_data_addr(DataId, no)), !CI),
        Code = empty
    ;
        ConsTag = base_typeclass_info_tag(ModuleName, ClassId, Instance),
        expect(unify(Args, []), this_file,
            "generate_construction_2: base_typeclass_info constant has args"),
        TCName = generate_class_name(ClassId),
        DataId = rtti_data_id(tc_rtti_id(TCName,
            type_class_base_typeclass_info(ModuleName, Instance))),
        assign_const_to_var(Var, const(llconst_data_addr(DataId, no)), !CI),
        Code = empty
    ;
        ConsTag = tabling_info_tag(PredId, ProcId),
        expect(unify(Args, []), this_file,
            "generate_construction_2: tabling_info constant has args"),
        get_module_info(!.CI, ModuleInfo),
        ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
        DataId = proc_tabling_data_id(ProcLabel, tabling_info),
        assign_const_to_var(Var, const(llconst_data_addr(DataId, no)), !CI),
        Code = empty
    ;
        ConsTag = deep_profiling_proc_layout_tag(PredId, ProcId),
        expect(unify(Args, []), this_file,
            "generate_construction_2: deep_profiling_proc_static has args"),
        get_module_info(!.CI, ModuleInfo),
        RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
        Origin = RttiProcLabel ^ rpl_pred_info_origin,
        ( Origin = origin_special_pred(_) ->
            UserOrUCI = uci
        ;
            UserOrUCI = user
        ),
        ProcKind = proc_layout_proc_id(UserOrUCI),
        DataId = layout_id(proc_layout(RttiProcLabel, ProcKind)),
        assign_const_to_var(Var, const(llconst_data_addr(DataId, no)), !CI),
        Code = empty
    ;
        ConsTag = table_io_decl_tag(PredId, ProcId),
        expect(unify(Args, []), this_file,
            "generate_construction_2: table_io_decl has args"),
        PredProcId = proc(PredId, ProcId),
        DataId = layout_slot_id(table_io_decl_id, PredProcId),
        assign_const_to_var(Var, const(llconst_data_addr(DataId, no)), !CI),
        Code = empty
    ;
        ConsTag = reserved_address_tag(RA),
        expect(unify(Args, []), this_file,
            "generate_construction_2: reserved_address constant has args"),
        assign_const_to_var(Var, generate_reserved_address(RA), !CI),
        Code = empty
    ;
        ConsTag = shared_with_reserved_addresses_tag(_RAs, ThisTag),
        % For shared_with_reserved_address, the sharing is only important
        % for tag tests, not for constructions, so here we just recurse
        % on the real representation.
        generate_construction_2(ThisTag, Var, Args, Modes, HowToConstruct,
            TakeAddr, MaybeSize, GoalInfo, Code, !CI)
    ;
        ConsTag = closure_tag(PredId, ProcId, EvalMethod),
        expect(unify(TakeAddr, []), this_file,
            "generate_construction_2: closure_tag has take_addr"),
        expect(unify(MaybeSize, no), this_file,
            "generate_construction_2: closure_tag has size"),
        generate_closure(PredId, ProcId, EvalMethod, Var, Args, GoalInfo,
            Code, !CI)
    ).

    % This predicate constructs or extends a closure.
    % The structure of closures is defined in runtime/mercury_ho_call.h.
    %
:- pred generate_closure(pred_id::in, proc_id::in, lambda_eval_method::in,
    prog_var::in, list(prog_var)::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_closure(PredId, ProcId, EvalMethod, Var, Args, GoalInfo, Code, !CI) :-
    get_module_info(!.CI, ModuleInfo),
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
    CodeModel = proc_info_interface_code_model(ProcInfo),
    proc_info_get_headvars(ProcInfo, ProcHeadVars),
    (
        EvalMethod = lambda_normal,
        Args = [CallPred | CallArgs],
        ProcHeadVars = [ProcPred | ProcArgs],
        ProcInfoGoal = hlds_goal(generic_call(higher_order(ProcPred, _, _, _),
            ProcArgs, _, CallDeterminism), _GoalInfo),
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
            assign_var_to_var(Var, CallPred, !CI),
            Code = empty
        ;
            CallArgs = [_ | _],
            get_next_label(LoopStart, !CI),
            get_next_label(LoopTest, !CI),
            acquire_reg(reg_r, LoopCounter, !CI),
            acquire_reg(reg_r, NumOldArgs, !CI),
            acquire_reg(reg_r, NewClosure, !CI),
            Zero = const(llconst_int(0)),
            One = const(llconst_int(1)),
            Two = const(llconst_int(2)),
            Three = const(llconst_int(3)),
            list.length(CallArgs, NumNewArgs),
            NumNewArgs_Rval = const(llconst_int(NumNewArgs)),
            NumNewArgsPlusThree = NumNewArgs + 3,
            NumNewArgsPlusThree_Rval = const(llconst_int(NumNewArgsPlusThree)),
            produce_variable(CallPred, OldClosureCode, OldClosure, !CI),
            % The new closure contains a pointer to the old closure.
            NewClosureMayUseAtomic = may_not_use_atomic_alloc,
            NewClosureCode = from_list([
                llds_instr(comment("build new closure from old closure"), ""),
                llds_instr(
                    assign(NumOldArgs, lval(field(yes(0), OldClosure, Two))),
                    "get number of arguments"),
                llds_instr(incr_hp(NewClosure, no, no,
                    binop(int_add, lval(NumOldArgs), NumNewArgsPlusThree_Rval),
                    "closure", NewClosureMayUseAtomic, no, no_llds_reuse),
                    "allocate new closure"),
                llds_instr(assign(field(yes(0), lval(NewClosure), Zero),
                    lval(field(yes(0), OldClosure, Zero))),
                    "set closure layout structure"),
                llds_instr(assign(field(yes(0), lval(NewClosure), One),
                    lval(field(yes(0), OldClosure, One))),
                    "set closure code pointer"),
                llds_instr(assign(field(yes(0), lval(NewClosure), Two),
                    binop(int_add, lval(NumOldArgs), NumNewArgs_Rval)),
                    "set new number of arguments"),
                llds_instr(
                    assign(NumOldArgs,
                        binop(int_add, lval(NumOldArgs), Three)),
                    "set up loop limit"),
                llds_instr(assign(LoopCounter, Three),
                    "initialize loop counter"),
                % It is possible for the number of hidden arguments to be zero,
                % in which case the body of this loop should not be executed
                % at all. This is why we jump to the loop condition test.
                llds_instr(goto(code_label(LoopTest)),
                    "enter the copy loop at the conceptual top"),
                llds_instr(label(LoopStart), "start of loop"),
                llds_instr(
                    assign(field(yes(0), lval(NewClosure), lval(LoopCounter)),
                        lval(field(yes(0), OldClosure, lval(LoopCounter)))),
                    "copy old hidden argument"),
                llds_instr(
                    assign(LoopCounter,
                        binop(int_add, lval(LoopCounter), One)),
                    "increment loop counter"),
                llds_instr(label(LoopTest),
                    "do we have more old arguments to copy?"),
                llds_instr(
                    if_val(binop(int_lt, lval(LoopCounter), lval(NumOldArgs)),
                        code_label(LoopStart)),
                    "repeat the loop?")
            ]),
            generate_extra_closure_args(CallArgs, LoopCounter, NewClosure,
                ExtraArgsCode, !CI),
            release_reg(LoopCounter, !CI),
            release_reg(NumOldArgs, !CI),
            release_reg(NewClosure, !CI),
            assign_lval_to_var(Var, NewClosure, AssignCode, !CI),
            Code = OldClosureCode ++ NewClosureCode ++ ExtraArgsCode ++
                 AssignCode
        )
    ;
        CodeAddr = make_proc_entry_label(!.CI, ModuleInfo, PredId, ProcId, no),
        ProcLabel = extract_proc_label_from_code_addr(CodeAddr),
        CodeAddrRval = const(llconst_code_addr(CodeAddr)),
        continuation_info.generate_closure_layout( ModuleInfo, PredId, ProcId,
            ClosureInfo),
        module_info_get_name(ModuleInfo, ModuleName),
        Context = goal_info_get_context(GoalInfo),
        term.context_file(Context, FileName),
        term.context_line(Context, LineNumber),
        GoalPath = goal_info_get_goal_path(GoalInfo),
        GoalPathStr = goal_path_to_string(GoalPath),
        get_cur_proc_label(!.CI, CallerProcLabel),
        get_next_closure_seq_no(SeqNo, !CI),
        get_static_cell_info(!.CI, StaticCellInfo0),
        hlds.hlds_pred.pred_info_get_origin(PredInfo, PredOrigin),
        stack_layout.construct_closure_layout(CallerProcLabel,
            SeqNo, ClosureInfo, ProcLabel, ModuleName, FileName, LineNumber,
            PredOrigin, GoalPathStr, StaticCellInfo0, StaticCellInfo,
            ClosureLayoutRvalsTypes, Data),
        set_static_cell_info(StaticCellInfo, !CI),
        add_closure_layout(Data, !CI),
        % For now, closures always have zero size, and the size slot
        % is never looked at.
        add_scalar_static_cell(ClosureLayoutRvalsTypes, ClosureDataAddr, !CI),
        ClosureLayoutRval = const(llconst_data_addr(ClosureDataAddr, no)),
        list.length(Args, NumArgs),
        proc_info_arg_info(ProcInfo, ArgInfo),
        VarTypes = get_var_types(!.CI),
        MayUseAtomic0 = initial_may_use_atomic(ModuleInfo),
        generate_pred_args(!.CI, VarTypes, Args, ArgInfo, PredArgs,
            MayUseAtomic0, MayUseAtomic),
        Vector = [
            yes(ClosureLayoutRval),
            yes(CodeAddrRval),
            yes(const(llconst_int(NumArgs)))
            | PredArgs
        ],
        % XXX construct_dynamically is just a dummy value. We just want
        % something which is not construct_in_region(_).
        assign_cell_to_var(Var, no, 0, Vector, construct_dynamically, no, [],
            "closure", MayUseAtomic, Code, !CI)
    ).

:- pred generate_extra_closure_args(list(prog_var)::in, lval::in,
    lval::in, llds_code::out, code_info::in, code_info::out) is det.

generate_extra_closure_args([], _, _, empty, !CI).
generate_extra_closure_args([Var | Vars], LoopCounter, NewClosure, Code,
        !CI) :-
    FieldLval = field(yes(0), lval(NewClosure), lval(LoopCounter)),
    IsDummy = variable_is_of_dummy_type(!.CI, Var),
    (
        IsDummy = is_dummy_type,
        ProduceCode = empty,
        AssignCode = singleton(
            llds_instr(assign(FieldLval, const(llconst_int(0))),
                "set new argument field (dummy type)")
        )
    ;
        IsDummy = is_not_dummy_type,
        produce_variable(Var, ProduceCode, Value, !CI),
        AssignCode = singleton(
            llds_instr(assign(FieldLval, Value),
                "set new argument field")
        )
    ),
    IncrCode = singleton(
        llds_instr(assign(LoopCounter,
            binop(int_add, lval(LoopCounter), const(llconst_int(1)))),
            "increment argument counter")
    ),
    generate_extra_closure_args(Vars, LoopCounter, NewClosure, VarsCode, !CI),
    Code = ProduceCode ++ AssignCode ++ IncrCode ++ VarsCode.

:- pred generate_pred_args(code_info::in, vartypes::in, list(prog_var)::in,
    list(arg_info)::in, list(maybe(rval))::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

generate_pred_args(_, _, [], _, [], !MayUseAtomic).
generate_pred_args(_, _, [_ | _], [], _, !MayUseAtomic) :-
    unexpected(this_file, "generate_pred_args: insufficient args").
generate_pred_args(CI, VarTypes, [Var | Vars], [ArgInfo | ArgInfos],
        [MaybeRval | MaybeRvals], !MayUseAtomic) :-
    ArgInfo = arg_info(_, ArgMode),
    (
        ArgMode = top_in,
        IsDummy = variable_is_of_dummy_type(CI, Var),
        (
            IsDummy = is_dummy_type,
            Rval = const(llconst_int(0))
        ;
            IsDummy = is_not_dummy_type,
            Rval = var(Var)
        ),
        MaybeRval = yes(Rval)
    ;
        ( ArgMode = top_out
        ; ArgMode = top_unused
        ),
        MaybeRval = no
    ),
    map.lookup(VarTypes, Var, Type),
    get_module_info(CI, ModuleInfo),
    update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic),
    generate_pred_args(CI, VarTypes, Vars, ArgInfos, MaybeRvals,
        !MayUseAtomic).

:- pred generate_cons_args(list(prog_var)::in, list(mer_type)::in,
    list(uni_mode)::in, int::in, int::in, list(int)::in, code_info::in,
    list(maybe(rval))::out, assoc_list(int, prog_var)::out,
    may_use_atomic_alloc::out) is det.

generate_cons_args(Vars, Types, Modes, FirstOffset, FirstArgNum, TakeAddr,
        CI, !:Args, !:FieldAddrs, !:MayUseAtomic) :-
    get_module_info(CI, ModuleInfo),
    !:MayUseAtomic = initial_may_use_atomic(ModuleInfo),
    (
        generate_cons_args_2(Vars, Types, Modes, FirstOffset, FirstArgNum,
            TakeAddr, CI, !:Args, !:FieldAddrs, !MayUseAtomic)
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
    list(uni_mode)::in, int::in, int::in, list(int)::in, code_info::in,
    list(maybe(rval))::out, assoc_list(int, prog_var)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is semidet.

generate_cons_args_2([], [], [], _, _, [], _, [], [], !MayUseAtomic).
generate_cons_args_2([Var | Vars], [Type | Types], [UniMode | UniModes],
        FirstOffset, CurArgNum, !.TakeAddr, CI, [MaybeRval | MaybeRvals],
        FieldAddrs, !MayUseAtomic) :-
    get_module_info(CI, ModuleInfo),
    update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic),
    ( !.TakeAddr = [CurArgNum | !:TakeAddr] ->
        get_lcmc_null(CI, LCMCNull),
        (
            LCMCNull = no,
            MaybeRval = no
        ;
            LCMCNull = yes,
            MaybeRval = yes(const(llconst_int(0)))
        ),
        !:MayUseAtomic = may_not_use_atomic_alloc,
        generate_cons_args_2(Vars, Types, UniModes, FirstOffset, CurArgNum + 1,
            !.TakeAddr, CI, MaybeRvals, FieldAddrs1, !MayUseAtomic),
        % Whereas CurArgNum starts numbering the arguments from 1, offsets
        % into fields start from zero. However, if FirstOffset = 1, then the
        % first word in the cell is the secondary tag.
        Offset = CurArgNum - 1 + FirstOffset,
        FieldAddrs = [Offset - Var | FieldAddrs1]
    ;
        UniMode = ((_LI - RI) -> (_LF - RF)),
        mode_to_arg_mode(ModuleInfo, (RI -> RF), Type, ArgMode),
        (
            ArgMode = top_in,
            MaybeRval = yes(var(Var))
        ;
            ( ArgMode = top_out
            ; ArgMode = top_unused
            ),
            MaybeRval = no
        ),
        generate_cons_args_2(Vars, Types, UniModes, FirstOffset, CurArgNum + 1,
            !.TakeAddr, CI, MaybeRvals, FieldAddrs, !MayUseAtomic)
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
    how_to_construct::in, maybe(term_size_value)::in,
    assoc_list(int, prog_var)::in, may_use_atomic_alloc::in,
    llds_code::out, code_info::in, code_info::out) is det.

construct_cell(Var, Ptag, MaybeRvals, HowToConstruct, MaybeSize, FieldAddrs,
        MayUseAtomic, Code, !CI) :-
    VarType = variable_type(!.CI, Var),
    var_type_msg(VarType, VarTypeMsg),
    % If we're doing accurate GC, then for types which hold RTTI that
    % will be traversed by the collector at GC-time, we need to allocate
    % an extra word at the start, to hold the forwarding pointer.
    % Normally we would just overwrite the first word of the object
    % in the "from" space, but this can't be done for objects which will be
    % referenced during the garbage collection process.
    (
        get_globals(!.CI, Globals),
        globals.get_gc_method(Globals, GCMethod),
        GCMethod = gc_accurate,
        is_introduced_type_info_type(VarType)
    ->
        ReserveWordAtStart = yes
    ;
        ReserveWordAtStart = no
    ),
    FieldNums = list.map(fst, FieldAddrs),
    assign_cell_to_var(Var, ReserveWordAtStart, Ptag, MaybeRvals,
        HowToConstruct, MaybeSize, FieldNums, VarTypeMsg, MayUseAtomic,
        CellCode, !CI),
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
        Code = CellCode ++ FieldCode
    ).

:- pred generate_field_take_address_assigns(assoc_list(int, prog_var)::in,
    prog_var::in, int::in, llds_code::out, code_info::in, code_info::out)
    is det.

generate_field_take_address_assigns([], _, _, empty, !CI).
generate_field_take_address_assigns([FieldNum - Var | FieldAddrs],
        CellVar, CellPtag, ThisCode ++ RestCode, !CI) :-
    FieldNumRval = const(llconst_int(FieldNum)),
    Addr = mem_addr(heap_ref(var(CellVar), CellPtag, FieldNumRval)),
    assign_expr_to_var(Var, Addr, ThisCode, !CI),
    generate_field_take_address_assigns(FieldAddrs, CellVar, CellPtag,
        RestCode, !CI).

%---------------------------------------------------------------------------%

:- pred var_types(code_info::in, list(prog_var)::in, list(mer_type)::out)
    is det.

var_types(CI, Vars, Types) :-
    get_proc_info(CI, ProcInfo),
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
    list(prog_var)::in, list(uni_mode)::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_det_deconstruction(Var, Cons, Args, Modes, Code, !CI) :-
    get_module_info(!.CI, ModuleInfo),
    Tag = cons_id_to_tag(ModuleInfo, Cons),
    generate_det_deconstruction_2(Var, Cons, Args, Modes, Tag, Code, !CI).

:- pred generate_det_deconstruction_2(prog_var::in, cons_id::in,
    list(prog_var)::in, list(uni_mode)::in, cons_tag::in,
    llds_code::out, code_info::in, code_info::out) is det.

generate_det_deconstruction_2(Var, Cons, Args, Modes, Tag, Code, !CI) :-
    % For constants, if the deconstruction is det, then we already know
    % the value of the constant, so Code = empty.
    (
        ( Tag = string_tag(_String)
        ; Tag = int_tag(_Int)
        ; Tag = foreign_tag(_, _)
        ; Tag = float_tag(_Float)
        ; Tag = closure_tag(_, _, _)
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
            VarType = variable_type(!.CI, Var),
            get_module_info(!.CI, ModuleInfo),
            IsDummy = check_dummy_type(ModuleInfo, VarType),
            (
                IsDummy = is_dummy_type,
                % We must handle this case specially. If we didn't, the
                % generated code would copy the reference to the Var's
                % current location, which may be stackvar(N) or framevar(N)
                % for negative N, to be the location of Arg, and since Arg
                % may not be a dummy type, it would actually use that location.
                % This can happen in the unify/compare routines for e.g.
                % io.state.
                ( variable_is_forward_live(!.CI, Arg) ->
                    assign_const_to_var(Arg, const(llconst_int(0)), !CI)
                ;
                    true
                ),
                Code = empty
            ;
                IsDummy = is_not_dummy_type,
                ArgType = variable_type(!.CI, Arg),
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
    list(prog_var)::in, list(uni_mode)::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_semi_deconstruction(Var, Tag, Args, Modes, Code, !CI) :-
    VarType = variable_type(!.CI, Var),
    CheaperTagTest = lookup_cheaper_tag_test(!.CI, VarType),
    generate_tag_test(Var, Tag, CheaperTagTest, branch_on_success, SuccLabel,
        TagTestCode, !CI),
    remember_position(!.CI, AfterUnify),
    generate_failure(FailCode, !CI),
    reset_to_position(AfterUnify, !CI),
    generate_det_deconstruction(Var, Tag, Args, Modes, DeconsCode, !CI),
    SuccessLabelCode = singleton(llds_instr(label(SuccLabel), "")),
    Code = TagTestCode ++ FailCode ++ SuccessLabelCode ++ DeconsCode.

%---------------------------------------------------------------------------%

    % Generate code to perform a list of deterministic subunifications
    % for the arguments of a construction.
    %
:- pred generate_unify_args(list(uni_val)::in, list(uni_val)::in,
    list(uni_mode)::in, list(mer_type)::in, llds_code::out,
    code_info::in, code_info::out) is det.

generate_unify_args(Ls, Rs, Ms, Ts, Code, !CI) :-
    ( generate_unify_args_2(Ls, Rs, Ms, Ts, Code0, !CI) ->
        Code = Code0
    ;
        unexpected(this_file, "generate_unify_args: length mismatch")
    ).

:- pred generate_unify_args_2(list(uni_val)::in, list(uni_val)::in,
    list(uni_mode)::in, list(mer_type)::in, llds_code::out,
    code_info::in, code_info::out) is semidet.

generate_unify_args_2([], [], [], [], empty, !CI).
generate_unify_args_2([L | Ls], [R | Rs], [M | Ms], [T | Ts], Code, !CI) :-
    generate_sub_unify(L, R, M, T, CodeA, !CI),
    generate_unify_args_2(Ls, Rs, Ms, Ts, CodeB, !CI),
    Code = CodeA ++ CodeB.

%---------------------------------------------------------------------------%

    % Generate a subunification between two [field | variable].
    %
:- pred generate_sub_unify(uni_val::in, uni_val::in, uni_mode::in,
    mer_type::in, llds_code::out, code_info::in, code_info::out) is det.

generate_sub_unify(L, R, Mode, Type, Code, !CI) :-
    Mode = ((LI - RI) -> (LF - RF)),
    get_module_info(!.CI, ModuleInfo),
    mode_to_arg_mode(ModuleInfo, (LI -> LF), Type, LeftMode),
    mode_to_arg_mode(ModuleInfo, (RI -> RF), Type, RightMode),
    (
        % Input - input == test unification
        LeftMode = top_in,
        RightMode = top_in
    ->
        % This shouldn't happen, since mode analysis should avoid creating
        % any tests in the arguments of a construction or deconstruction
        % unification.
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

:- pred generate_sub_assign(uni_val::in, uni_val::in, llds_code::out,
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
        produce_variable(Var, SourceCode, Source, !CI),
        materialize_vars_in_lval(Lval0, Lval, MaterializeCode, !CI),
        CopyCode = singleton(llds_instr(assign(Lval, Source), "Copy value")),
        Code = SourceCode ++ MaterializeCode ++ CopyCode
    ;
        Left = ref(Lvar),
        ( variable_is_forward_live(!.CI, Lvar) ->
            (
                Right = lval(Lval),
                % Assignment of a value to a variable, generate now.
                assign_lval_to_var(Lvar, Lval, Code, !CI)
            ;
                Right = ref(Rvar),
                % Assignment of a variable to a variable, so cache it.
                assign_var_to_var(Lvar, Rvar, !CI),
                Code = empty
            )
        ;
            Code = empty
        )
    ).

%---------------------------------------------------------------------------%

:- type active_ground_term == pair(rval, llds_type).
:- type active_ground_term_map == map(prog_var, active_ground_term).

generate_ground_term(TermVar, Goal, !CI) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    set.to_sorted_list(NonLocals, NonLocalList),
    (
        NonLocalList = []
        % The term being constructed by the scope is not needed, so there is
        % nothing to do.
    ;
        NonLocalList = [NonLocal],
        ( NonLocal = TermVar ->
            ( GoalExpr = conj(plain_conj, Conjuncts) ->
                get_module_info(!.CI, ModuleInfo),
                get_exprn_opts(!.CI, ExprnOpts),
                UnboxedFloats = get_unboxed_floats(ExprnOpts),
                get_static_cell_info(!.CI, StaticCellInfo0),
                map.init(ActiveMap0),
                generate_ground_term_conjuncts(ModuleInfo, Conjuncts,
                    UnboxedFloats, StaticCellInfo0, StaticCellInfo,
                    ActiveMap0, ActiveMap),
                map.to_assoc_list(ActiveMap, ActivePairs),
                ( ActivePairs = [TermVar - GroundTerm] ->
                    add_forward_live_vars(NonLocals, !CI),
                    set_static_cell_info(StaticCellInfo, !CI),
                    GroundTerm = Rval - _,
                    assign_const_to_var(TermVar, Rval, !CI)
                ;
                    unexpected(this_file,
                        "generate_ground_term: no active pairs")
                )
            ;
                unexpected(this_file, "generate_ground_term: malformed goal")
            )
        ;
            unexpected(this_file, "generate_ground_term: unexpected nonlocal")
        )
    ;
        NonLocalList = [_, _ | _],
        unexpected(this_file, "generate_ground_term: unexpected nonlocals")
    ).

:- pred generate_ground_term_conjuncts(module_info::in,
    list(hlds_goal)::in, have_unboxed_floats::in,
    static_cell_info::in, static_cell_info::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_conjuncts(_ModuleInfo, [],
        _UnboxedFloats, !StaticCellInfo, !ActiveMap).
generate_ground_term_conjuncts(ModuleInfo, [Goal | Goals],
        UnboxedFloats, !StaticCellInfo, !ActiveMap) :-
    generate_ground_term_conjunct(ModuleInfo, Goal, UnboxedFloats,
        !StaticCellInfo, !ActiveMap),
    generate_ground_term_conjuncts(ModuleInfo, Goals, UnboxedFloats,
        !StaticCellInfo, !ActiveMap).

:- pred generate_ground_term_conjunct(module_info::in,
    hlds_goal::in, have_unboxed_floats::in,
    static_cell_info::in, static_cell_info::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_conjunct(ModuleInfo, Goal, UnboxedFloats,
        !StaticCellInfo, !ActiveMap) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(_, _, _, Unify, _),
        Unify = construct(Var, ConsId, Args, _, _, _, SubInfo),
        SubInfo = no_construct_sub_info
    ->
        ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
        generate_ground_term_conjunct_tag(Var, ConsTag, Args, UnboxedFloats,
            !StaticCellInfo, !ActiveMap)
    ;
        unexpected(this_file,
            "generate_ground_term_conjunct: malformed goal")
    ).

:- pred generate_ground_term_conjunct_tag(prog_var::in, cons_tag::in,
    list(prog_var)::in, have_unboxed_floats::in,
    static_cell_info::in, static_cell_info::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_conjunct_tag(Var, ConsTag, Args, UnboxedFloats,
        !StaticCellInfo, !ActiveMap) :-
    (
        (
            ConsTag = string_tag(String),
            Const = llconst_string(String),
            Type = lt_string
        ;
            ConsTag = int_tag(Int),
            Const = llconst_int(Int),
            Type = lt_integer
        ;
            ConsTag = foreign_tag(Lang, Val),
            expect(unify(Lang, lang_c), this_file,
                "foreign_tag for language other than C"),
            Const = llconst_foreign(Val, lt_integer),
            Type = lt_integer
        ;
            ConsTag = float_tag(Float),
            Const = llconst_float(Float),
            (
                UnboxedFloats = have_unboxed_floats,
                Type = lt_float
            ;
                UnboxedFloats = do_not_have_unboxed_floats,
                Type = lt_data_ptr
            )
        ),
        ActiveGroundTerm = const(Const) - Type,
        svmap.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = shared_local_tag(Ptag, Stag),
        Rval = mkword(Ptag, unop(mkbody, const(llconst_int(Stag)))),
        ActiveGroundTerm = Rval - lt_data_ptr,
        svmap.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = reserved_address_tag(RA),
        Rval = generate_reserved_address(RA),
        rval_type(Rval, RvalType),
        ActiveGroundTerm = Rval - RvalType,
        svmap.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = shared_with_reserved_addresses_tag(_, ActualConsTag),
        generate_ground_term_conjunct_tag(Var, ActualConsTag, Args,
            UnboxedFloats, !StaticCellInfo, !ActiveMap)
    ;
        ConsTag = no_tag,
        (
            Args = [],
            unexpected(this_file,
                "generate_ground_term_conjunct_tag: no_tag arity != 1")
        ;
            Args = [Arg],
            svmap.det_remove(Arg, RvalType, !ActiveMap),
            svmap.det_insert(Var, RvalType, !ActiveMap)
        ;
            Args = [_, _ | _],
            unexpected(this_file,
                "generate_ground_term_conjunct_tag: no_tag arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            Ptag = 0
        ;
            ConsTag = unshared_tag(Ptag)
        ),
        generate_ground_term_args(Args, ArgRvalsTypes, !ActiveMap),
        add_scalar_static_cell(ArgRvalsTypes, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        Rval = mkword(Ptag, CellPtrConst),
        ActiveGroundTerm = Rval - lt_data_ptr,
        svmap.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = shared_remote_tag(Ptag, Stag),
        generate_ground_term_args(Args, ArgRvalsTypes, !ActiveMap),
        StagRvalType = const(llconst_int(Stag)) - lt_integer,
        AllRvalsTypes = [StagRvalType | ArgRvalsTypes],
        add_scalar_static_cell(AllRvalsTypes, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        Rval = mkword(Ptag, CellPtrConst),
        ActiveGroundTerm = Rval - lt_data_ptr,
        svmap.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ( ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_decl_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ),
        unexpected(this_file,
            "generate_ground_term_conjunct_tag: unexpected tag")
    ).

:- pred generate_ground_term_args(list(prog_var)::in,
    assoc_list(rval, llds_type)::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_args([], [], !ActiveMap).
generate_ground_term_args([Var | Vars], [RvalType | RvalsTypes], !ActiveMap) :-
    svmap.det_remove(Var, RvalType, !ActiveMap),
    generate_ground_term_args(Vars, RvalsTypes, !ActiveMap).

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
