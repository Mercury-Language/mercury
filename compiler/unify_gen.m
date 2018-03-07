%---------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------e
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
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

:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type test_sense
    --->    branch_on_success
    ;       branch_on_failure.

:- pred generate_unification(code_model::in, unification::in,
    hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_tag_test(prog_var::in, cons_id::in,
    maybe_cheaper_tag_test::in, test_sense::in, label::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_raw_tag_test_case(rval::in, mer_type::in, string::in,
    tagged_cons_id::in, list(tagged_cons_id)::in, maybe_cheaper_tag_test::in,
    test_sense::in, label::out, llds_code::out, code_info::in, code_info::out)
    is det.

:- pred generate_ground_term(prog_var::in, hlds_goal::in,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_const_structs(module_info::in, const_struct_map::out,
    global_data::in, global_data::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.arg_pack.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module backend_libs.type_class_info.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.closure_gen.
:- import_module ll_backend.code_util.
:- import_module ll_backend.layout.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module unit.

%---------------------------------------------------------------------------%

:- type uni_val
    --->    uv_var(prog_var)
    ;       uv_field(uni_field).

:- type uni_field
    --->    uni_field(ptag, rval, int, arg_width).
            % The first three arguments (Ptag, BaseRval and Offset) represent
            % the lval of the field, which is
            % field(yes(Ptag), BaseRval, const(llconst_int(Offset))).
            % The last argument represents the size of the argument in the
            % field, which may be a word, two words, or only part of a word.

:- type field_and_arg_var
    --->    field_and_arg_var(
                uni_val,            % The field (or var).
                prog_var,           % The arg_var.
                mer_type            % Their shared type.
            ).

:- type field_addr
    --->    field_addr(
                fa_offset   :: int,
                fa_var      :: prog_var
            ).

%---------------------------------------------------------------------------%

generate_unification(CodeModel, Uni, GoalInfo, Code, !CI, !CLD) :-
    (
        CodeModel = model_det
    ;
        CodeModel = model_semi
    ;
        CodeModel = model_non,
        unexpected($pred, "nondet unification")
    ),
    (
        Uni = assign(LHSVar, RHSVar),
        ( if variable_is_forward_live(!.CLD, LHSVar) then
            generate_assignment(LHSVar, RHSVar, Code, !CLD)
        else
            Code = empty
        )
    ;
        Uni = construct(LHSVar, ConsId, RHSVars, ArgModes, HowToConstruct, _,
            SubInfo),
        (
            SubInfo = no_construct_sub_info,
            MaybeTakeAddr = no,
            MaybeSize = no
        ;
            SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSize)
        ),
        ( if
            ( variable_is_forward_live(!.CLD, LHSVar)
            ; MaybeTakeAddr = yes(_)
            )
        then
            (
                MaybeTakeAddr = yes(TakeAddr)
            ;
                MaybeTakeAddr = no,
                TakeAddr = []
            ),
            get_module_info(!.CI, ModuleInfo),
            get_cons_arg_widths(ModuleInfo, ConsId, RHSVars, RHSVarsWidths),
            generate_construction(LHSVar, ConsId, RHSVarsWidths, ArgModes,
                HowToConstruct, TakeAddr, MaybeSize, GoalInfo, Code, !CI, !CLD)
        else
            Code = empty
        )
    ;
        Uni = deconstruct(LHSVar, ConsId, RHSVars, ArgModes, _CanFail, CanCGC),
        get_module_info(!.CI, ModuleInfo),
        get_cons_arg_widths(ModuleInfo, ConsId, RHSVars, RHSVarsWidths),
        (
            CodeModel = model_det,
            generate_det_deconstruction(LHSVar, ConsId,
                RHSVarsWidths, ArgModes, Code0, !.CI, !CLD)
        ;
            CodeModel = model_semi,
            generate_semi_deconstruction(LHSVar, ConsId,
                RHSVarsWidths, ArgModes, Code0, !CI, !CLD)
        ),
        (
            CanCGC = can_cgc,
            LHSVarName = variable_name(!.CI, LHSVar),
            produce_variable(LHSVar, ProduceVar, VarRval, !.CI, !CLD),
            ( if VarRval = lval(VarLval) then
                save_reused_cell_fields(LHSVar, VarLval, SaveArgs, Regs,
                    !.CI, !CLD),
                % This seems to be fine.
                list.foldl(release_reg, Regs, !CLD),
                % XXX avoid strip_tag when we know what ptag it will have
                FreeVar = singleton(
                    llds_instr(free_heap(unop(strip_tag, VarRval)),
                        "Free " ++ LHSVarName)
                ),
                Code = Code0 ++ ProduceVar ++ SaveArgs ++ FreeVar
            else
                Code = Code0
            )
        ;
            CanCGC = cannot_cgc,
            Code = Code0
        )
    ;
        Uni = simple_test(VarA, VarB),
        (
            CodeModel = model_det,
            unexpected($pred, "det simple_test")
        ;
            CodeModel = model_semi,
            generate_test(VarA, VarB, Code, !CI, !CLD)
        )
    ;
        % These should have been transformed into calls to unification
        % procedures by polymorphism.m.
        Uni = complicated_unify(_Mode, _CanFail, _TypeInfoVars),
        unexpected($pred, "complicated unify")
    ).

:- pred get_cons_arg_widths(module_info::in, cons_id::in,
    list(T)::in, assoc_list(T, arg_width)::out) is det.

get_cons_arg_widths(ModuleInfo, ConsId, AllArgs, AllArgsWidths) :-
    ( if get_cons_repn_defn(ModuleInfo, ConsId, ConsRepnDefn) then
        ConsArgRepns = ConsRepnDefn ^ cr_args,
        ArgWidths = list.map((func(C) = C ^ car_width), ConsArgRepns),
        list.length(AllArgs, NumAllArgs),
        list.length(ConsArgRepns, NumConsArgs),
        NumExtraArgs = NumAllArgs - NumConsArgs,
        ( if NumExtraArgs = 0 then
            assoc_list.from_corresponding_lists(AllArgs, ArgWidths,
                AllArgsWidths)
        else if NumExtraArgs > 0 then
            list.det_split_list(NumExtraArgs, AllArgs, ExtraArgs, ConsArgs),
            assoc_list.from_corresponding_lists(ConsArgs, ArgWidths,
                ConsArgsWidths),
            list.map(pair_with_width(full_word), ExtraArgs, ExtraArgsWidths),
            AllArgsWidths = ExtraArgsWidths ++ ConsArgsWidths
        else
            unexpected($pred, "too few arguments")
        )
    else
        list.map(pair_with_width(full_word), AllArgs, AllArgsWidths)
    ).

%---------------------------------------------------------------------------%

    % Assignment unifications are generated by simply caching the bound
    % variable as the expression that generates the free variable.
    % No immediate code is generated.
    %
:- pred generate_assignment(prog_var::in, prog_var::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_assignment(VarA, VarB, empty, !CLD) :-
    ( if variable_is_forward_live(!.CLD, VarA) then
        assign_var_to_var(VarA, VarB, !CLD)
    else
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
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_test(VarA, VarB, Code, !CI, !CLD) :-
    IsDummy = variable_is_of_dummy_type(!.CI, VarA),
    (
        IsDummy = is_dummy_type,
        Code = empty
    ;
        IsDummy = is_not_dummy_type,
        produce_variable(VarA, CodeA, ValA, !.CI, !CLD),
        produce_variable(VarB, CodeB, ValB, !.CI, !CLD),
        Type = variable_type(!.CI, VarA),
        ( if Type = builtin_type(BuiltinType) then
            (
                BuiltinType = builtin_type_string,
                Op = str_eq
            ;
                BuiltinType = builtin_type_float,
                Op = float_eq
            ;
                BuiltinType = builtin_type_char,
                Op = eq(int_type_int)
            ;
                BuiltinType = builtin_type_int(IntType),
                Op = eq(IntType)
            )
        else
            % The else branch handles enumerations.
            Op = eq(int_type_int)
        ),
        fail_if_rval_is_false(binop(Op, ValA, ValB), FailCode, !CI, !CLD),
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

generate_tag_test(Var, ConsId, CheaperTagTest, Sense, ElseLabel, Code,
        !CI, !CLD) :-
    produce_variable(Var, VarCode, VarRval, !.CI, !CLD),
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
    ( if
        CheaperTagTest = cheaper_tag_test(ExpensiveConsId, _ExpensiveConsTag,
            _CheapConsId, CheapConsTag),
        ConsId = ExpensiveConsId
    then
        Comment = branch_sense_comment(Sense) ++ VarName ++
            " has functor " ++ ConsIdName ++ " (inverted test)",
        raw_tag_test(VarRval, CheapConsTag, NegTestRval),
        code_util.neg_rval(NegTestRval, TestRval)
    else
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
        ConsTag = int_tag(IntTag),
        int_tag_to_const_and_int_type(IntTag, Const, IntType),
        TestRval = binop(eq(IntType), Rval, const(Const))
    ;
        ConsTag = foreign_tag(ForeignLang, ForeignVal),
        expect(unify(ForeignLang, lang_c), $pred,
            "foreign tag for language other than C"),
        TestRval = binop(eq(int_type_int), Rval,
            const(llconst_foreign(ForeignVal, lt_int(int_type_int))))
    ;
        ConsTag = closure_tag(_, _, _),
        % This should never happen, since the error will be detected
        % during mode checking.
        unexpected($pred, "Attempted higher-order unification")
    ;
        ConsTag = type_ctor_info_tag(_, _, _),
        unexpected($pred, "Attempted type_ctor_info unification")
    ;
        ConsTag = base_typeclass_info_tag(_, _, _),
        unexpected($pred, "Attempted base_typeclass_info unification")
    ;
        ConsTag = type_info_const_tag(_),
        unexpected($pred, "Attempted type_info_const_tag unification")
    ;
        ConsTag = typeclass_info_const_tag(_),
        unexpected($pred, "Attempted typeclass_info_const_tag unification")
    ;
        ConsTag = ground_term_const_tag(_, _),
        unexpected($pred, "Attempted ground_term_const_tag unification")
    ;
        ConsTag = tabling_info_tag(_, _),
        unexpected($pred, "Attempted tabling_info unification")
    ;
        ConsTag = deep_profiling_proc_layout_tag(_, _),
        unexpected($pred,
            "Attempted deep_profiling_proc_layout_tag unification")
    ;
        ConsTag = table_io_entry_tag(_, _),
        unexpected($pred, "Attempted table_io_entry_tag unification")
    ;
        ConsTag = no_tag,
        TestRval = const(llconst_true)
    ;
        ConsTag = single_functor_tag,
        TestRval = const(llconst_true)
    ;
        ( ConsTag = unshared_tag(UnsharedTag)
        ; ConsTag = direct_arg_tag(UnsharedTag)
        ),
        VarPtag = unop(tag, Rval),
        ConstPtag = unop(mktag, const(llconst_int(UnsharedTag))),
        TestRval = binop(eq(int_type_int), VarPtag, ConstPtag)
    ;
        ConsTag = shared_remote_tag(Bits, Num),
        VarPtag = unop(tag, Rval),
        ConstPtag = unop(mktag, const(llconst_int(Bits))),
        PtagTestRval = binop(eq(int_type_int), VarPtag, ConstPtag),
        VarStag = lval(field(yes(Bits), Rval, const(llconst_int(0)))),
        ConstStag = const(llconst_int(Num)),
        StagTestRval = binop(eq(int_type_int), VarStag, ConstStag),
        TestRval = binop(logical_and, PtagTestRval, StagTestRval)
    ;
        ConsTag = shared_local_tag(Bits, Num),
        ConstStag = mkword(Bits, unop(mkbody, const(llconst_int(Num)))),
        TestRval = binop(eq(int_type_int), Rval, ConstStag)
    ).

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
    assoc_list(prog_var, arg_width)::in, list(unify_mode)::in,
    how_to_construct::in,
    list(int)::in, maybe(term_size_value)::in, hlds_goal_info::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_construction(LHSVar, ConsId, RHSVarsWidths, ArgModes,
        HowToConstruct0, TakeAddr, MaybeSize, GoalInfo, Code, !CI, !CLD) :-
    get_module_info(!.CI, ModuleInfo),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    (
        ConsTag = string_tag(String),
        assign_const_to_var(LHSVar, const(llconst_string(String)), !.CI, !CLD),
        Code = empty
    ;
        ConsTag = int_tag(IntTag),
        int_tag_to_const_and_int_type(IntTag, Const, _),
        assign_const_to_var(LHSVar, const(Const), !.CI, !CLD),
        Code = empty
    ;
        ConsTag = foreign_tag(Lang, Val),
        expect(unify(Lang, lang_c), $pred,
            "foreign_tag for language other than C"),
        ForeignConst = const(llconst_foreign(Val, lt_int(int_type_int))),
        assign_const_to_var(LHSVar, ForeignConst, !.CI, !CLD),
        Code = empty
    ;
        ConsTag = float_tag(Float),
        assign_const_to_var(LHSVar, const(llconst_float(Float)), !.CI, !CLD),
        Code = empty
    ;
        ConsTag = no_tag,
        ( if
            RHSVarsWidths = [RHSVar - _Width],
            ArgModes = [ArgMode]
        then
            (
                TakeAddr = [],
                Type = variable_type(!.CI, RHSVar),
                FieldAndArgVar =
                    field_and_arg_var(uv_var(LHSVar), RHSVar, Type),
                generate_sub_unify(FieldAndArgVar, ArgMode, Code, !.CI, !CLD)
            ;
                TakeAddr = [_ | _],
                unexpected($pred, "notag: take_addr")
            )
        else
            unexpected($pred, "no_tag: arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            % Treat single_functor the same as unshared_tag(0).
            Ptag = 0
        ;
            ConsTag = unshared_tag(Ptag)
        ;
            ConsTag = shared_remote_tag(Ptag, _Sectag)
        ),
        get_proc_info(!.CI, ProcInfo),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        get_may_use_atomic_alloc(!.CI, MayUseAtomic0),
        FirstArgNum = 1,
        generate_cons_args(VarTypes, RHSVarsWidths, ArgModes, FirstArgNum,
            TakeAddr, !.CI, CellArgs0, MayUseAtomic0, MayUseAtomic),

        assoc_list.values(RHSVarsWidths, ArgWidths),
        pack_cell_rvals(ArgWidths, CellArgs0, CellArgs1, PackCode, !.CI, !CLD),
        pack_how_to_construct(ArgWidths, HowToConstruct0, HowToConstruct),
        (
            ( ConsTag = single_functor_tag
            ; ConsTag = unshared_tag(_Ptag)
            ),
            CellArgs = CellArgs1
        ;
            ConsTag = shared_remote_tag(_Ptag, Sectag),
            TagArg = cell_arg_full_word(const(llconst_int(Sectag)), complete),
            CellArgs = [TagArg | CellArgs1]
        ),
        Context = goal_info_get_context(GoalInfo),
        construct_cell(LHSVar, Ptag, CellArgs, HowToConstruct, MaybeSize,
            Context, MayUseAtomic, ConstructCode, !CI, !CLD),
        Code = PackCode ++ ConstructCode
    ;
        ConsTag = direct_arg_tag(Ptag),
        ( if
            RHSVarsWidths = [RHSVar - _Width],
            ArgModes = [ArgMode]
        then
            (
                TakeAddr = [],
                Type = variable_type(!.CI, RHSVar),
                generate_direct_arg_construct(LHSVar, RHSVar, Ptag,
                    ArgMode, Type, Code, !.CI, !CLD)
            ;
                TakeAddr = [_ | _],
                unexpected($pred, "direct_arg_tag: take_addr")
            )
        else
            unexpected($pred, "direct_arg_tag: arity != 1")
        )
    ;
        ConsTag = shared_local_tag(Ptag, Sectag),
        assign_const_to_var(LHSVar,
            mkword(Ptag, unop(mkbody, const(llconst_int(Sectag)))),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = type_ctor_info_tag(ModuleName, TypeName, TypeArity),
        expect(unify(RHSVarsWidths, []), $pred,
            "type_ctor_info constant has args"),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
        DataId = rtti_data_id(ctor_rtti_id(RttiTypeCtor,
            type_ctor_type_ctor_info)),
        assign_const_to_var(LHSVar, const(llconst_data_addr(DataId, no)),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = base_typeclass_info_tag(ModuleName, ClassId, Instance),
        expect(unify(RHSVarsWidths, []), $pred,
            "base_typeclass_info constant has args"),
        TCName = generate_class_name(ClassId),
        DataId = rtti_data_id(tc_rtti_id(TCName,
            type_class_base_typeclass_info(ModuleName, Instance))),
        assign_const_to_var(LHSVar, const(llconst_data_addr(DataId, no)),
            !.CI, !CLD),
        Code = empty
    ;
        ( ConsTag = type_info_const_tag(ConstNum)
        ; ConsTag = typeclass_info_const_tag(ConstNum)
        ; ConsTag = ground_term_const_tag(ConstNum, _)
        ),
        get_const_struct_map(!.CI, ConstStructMap),
        map.lookup(ConstStructMap, ConstNum, typed_rval(Rval, _Type)),
        assign_expr_to_var(LHSVar, Rval, Code, !CLD)
    ;
        ConsTag = tabling_info_tag(PredId, ProcId),
        expect(unify(RHSVarsWidths, []), $pred,
            "tabling_info constant has args"),
        ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
        DataId = proc_tabling_data_id(ProcLabel, tabling_info),
        assign_const_to_var(LHSVar, const(llconst_data_addr(DataId, no)),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = deep_profiling_proc_layout_tag(PredId, ProcId),
        expect(unify(RHSVarsWidths, []), $pred,
            "deep_profiling_proc_static has args"),
        RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
        Origin = RttiProcLabel ^ rpl_pred_info_origin,
        ( if Origin = origin_special_pred(_, _) then
            UserOrUCI = uci
        else
            UserOrUCI = user
        ),
        ProcKind = proc_layout_proc_id(UserOrUCI),
        DataId = layout_id(proc_layout(RttiProcLabel, ProcKind)),
        assign_const_to_var(LHSVar, const(llconst_data_addr(DataId, no)),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = table_io_entry_tag(PredId, ProcId),
        expect(unify(RHSVarsWidths, []), $pred, "table_io_entry has args"),
        PredProcId = proc(PredId, ProcId),
        DataId = layout_slot_id(table_io_entry_id, PredProcId),
        assign_const_to_var(LHSVar, const(llconst_data_addr(DataId, no)),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = closure_tag(PredId, ProcId, EvalMethod),
        expect(unify(TakeAddr, []), $pred,
            "closure_tag has take_addr"),
        expect(unify(MaybeSize, no), $pred,
            "closure_tag has size"),
        % XXX TYPE_REPN
        assoc_list.keys(RHSVarsWidths, RHSVars),
        generate_closure(PredId, ProcId, EvalMethod, LHSVar, RHSVars, GoalInfo,
            Code, !CI, !CLD)
    ).

    % Create a list of maybe(rval) for the arguments for a construction
    % unification. For each argument which is input to the construction
    % unification, we produce `yes(var(Var))', but if the argument is free,
    % we just produce `no', meaning don't generate an assignment to that field.
    %
:- pred generate_cons_args(vartypes::in,
    assoc_list(prog_var, arg_width)::in, list(unify_mode)::in, int::in,
    list(int)::in, code_info::in, list(cell_arg)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

generate_cons_args(_, [], [], _, _, _, [], !MayUseAtomic).
generate_cons_args(_, [], [_ | _], _, _, _, _, !MayUseAtomic) :-
    unexpected($pred, "length mismatch").
generate_cons_args(_, [_ | _], [], _, _, _, _, !MayUseAtomic) :-
    unexpected($pred, "length mismatch").
generate_cons_args(VarTypes, [VarWidth | VarsWidths], [ArgMode | ArgModes],
        CurArgNum, !.TakeAddr, CI, [CellArg | CellArgs], !MayUseAtomic) :-
    generate_cons_arg(VarTypes, VarWidth, ArgMode, CurArgNum,
        !.TakeAddr, CI, CellArg, !MayUseAtomic),
    generate_cons_args(VarTypes, VarsWidths, ArgModes, CurArgNum + 1,
        !.TakeAddr, CI, CellArgs, !MayUseAtomic).

:- pred generate_cons_arg(vartypes::in, pair(prog_var, arg_width)::in,
    unify_mode::in, int::in, list(int)::in, code_info::in, cell_arg::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

generate_cons_arg(VarTypes, Var - Width, ArgMode, CurArgNum,
        !.TakeAddr, CI, CellArg, !MayUseAtomic) :-
    lookup_var_type(VarTypes, Var, Type),
    get_module_info(CI, ModuleInfo),
    update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic),
    ( if !.TakeAddr = [CurArgNum | !:TakeAddr] then
        get_lcmc_null(CI, LCMCNull),
        (
            LCMCNull = no,
            MaybeNull = no
        ;
            LCMCNull = yes,
            MaybeNull = yes(const(llconst_int(0)))
        ),
        CellArg = cell_arg_take_addr(Var, MaybeNull),
        !:MayUseAtomic = may_not_use_atomic_alloc
    else
        ArgMode = unify_modes_lhs_rhs(_LHSMode, RHSInsts),
        from_to_insts_to_top_functor_mode(ModuleInfo, RHSInsts, Type,
            RHSTopFunctorMode),
        (
            RHSTopFunctorMode = top_in,
            (
                ( Width = full_word
                ; Width = partial_word_first(_)
                ; Width = partial_word_shifted(_, _)
                ),
                CellArg = cell_arg_full_word(var(Var), complete)
            ;
                Width = double_word,
                CellArg = cell_arg_double_word(var(Var))
            )
        ;
            ( RHSTopFunctorMode = top_out
            ; RHSTopFunctorMode = top_unused
            ),
            CellArg = cell_arg_skip
        )
    ).

:- pred construct_cell(prog_var::in, ptag::in, list(cell_arg)::in,
    how_to_construct::in, maybe(term_size_value)::in, prog_context::in,
    may_use_atomic_alloc::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

construct_cell(Var, Ptag, CellArgs, HowToConstruct, MaybeSize, Context,
        MayUseAtomic, Code, !CI, !CLD) :-
    VarType = variable_type(!.CI, Var),
    var_type_msg(VarType, VarTypeMsg),
    % If we are doing accurate GC, then for types which hold RTTI that
    % will be traversed by the collector at GC-time, we need to allocate
    % an extra word at the start, to hold the forwarding pointer.
    % Normally we would just overwrite the first word of the object
    % in the "from" space, but this can't be done for objects which will be
    % referenced during the garbage collection process.
    get_gc_method(!.CI, GCMethod),
    ( if
        GCMethod = gc_accurate,
        is_introduced_type_info_type(VarType)
    then
        ReserveWordAtStart = yes
    else
        ReserveWordAtStart = no
    ),
    Size = size_of_cell_args(CellArgs),
    maybe_add_alloc_site_info(Context, VarTypeMsg, Size, MaybeAllocId, !CI),
    assign_cell_to_var(Var, ReserveWordAtStart, Ptag, CellArgs, HowToConstruct,
        MaybeSize, MaybeAllocId, MayUseAtomic, CellCode, !CI, !CLD),
    generate_field_addrs(CellArgs, FieldAddrs),
    (
        FieldAddrs = [],
        % Optimize common case.
        Code = CellCode
    ;
        FieldAddrs = [_ | _],
        % Any field whose address we take will be represented by a
        % `cell_arg_take_addr' which should prevent the cell from being made
        % into static data.
        generate_field_take_address_assigns(FieldAddrs, Var, Ptag,
            FieldCode, !CLD),
        Code = CellCode ++ FieldCode
    ).

:- pred generate_field_addrs(list(cell_arg)::in, list(field_addr)::out) is det.

generate_field_addrs(CellArgs, FieldAddrs) :-
    list.foldl2(generate_field_addr, CellArgs, 0, _, [], RevFieldAddrs),
    list.reverse(RevFieldAddrs, FieldAddrs).

:- pred generate_field_addr(cell_arg::in, int::in, int::out,
    list(field_addr)::in, list(field_addr)::out) is det.

generate_field_addr(CellArg, ArgOffset, NextOffset, !RevFieldAddrs) :-
    (
        ( CellArg = cell_arg_full_word(_, _)
        ; CellArg = cell_arg_skip
        ),
        NextOffset = ArgOffset + 1
    ;
        CellArg = cell_arg_double_word(_),
        NextOffset = ArgOffset + 2
    ;
        CellArg = cell_arg_take_addr(Var, _),
        NextOffset = ArgOffset + 1,
        FieldAddr = field_addr(ArgOffset, Var),
        !:RevFieldAddrs = [FieldAddr | !.RevFieldAddrs]
    ).

:- pred generate_field_take_address_assigns(list(field_addr)::in,
    prog_var::in, int::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_field_take_address_assigns([], _, _, empty, !CLD).
generate_field_take_address_assigns([FieldAddr | FieldAddrs],
        CellVar, CellPtag, ThisCode ++ RestCode, !CLD) :-
    FieldAddr = field_addr(FieldNum, Var),
    FieldNumRval = const(llconst_int(FieldNum)),
    Addr = mem_addr(heap_ref(var(CellVar), yes(CellPtag), FieldNumRval)),
    assign_expr_to_var(Var, Addr, ThisCode, !CLD),
    generate_field_take_address_assigns(FieldAddrs, CellVar, CellPtag,
        RestCode, !CLD).

%---------------------------------------------------------------------------%

    % Construct a pair of lists that associates the fields of a term
    % with variables.
    %
:- pred make_fields_and_arg_vars(vartypes::in,
    assoc_list(prog_var, arg_width)::in, rval::in, int::in, int::in,
    list(field_and_arg_var)::out) is det.

make_fields_and_arg_vars(_, [], _, _, _, []).
make_fields_and_arg_vars(VarTypes, [VarWidth | VarsWidths],
        Rval, PrevOffset0, Ptag, [FieldAndArgVar | FieldsAndArgVars]) :-
    VarWidth = Var - Width,
    (
        ( Width = full_word
        ; Width = partial_word_first(_Mask)
        ),
        Offset = PrevOffset0 + 1,
        PrevOffset = Offset
    ;
        Width = partial_word_shifted(_Shift, _Mask),
        Offset = PrevOffset0,
        PrevOffset = Offset
    ;
        Width = double_word,
        Offset = PrevOffset0 + 1,
        PrevOffset = Offset + 1
    ),
    Field = uv_field(uni_field(Ptag, Rval, Offset, Width)),
    lookup_var_type(VarTypes, Var, Type),
    FieldAndArgVar = field_and_arg_var(Field, Var, Type),
    make_fields_and_arg_vars(VarTypes, VarsWidths,
        Rval, PrevOffset, Ptag, FieldsAndArgVars).

%---------------------------------------------------------------------------%

    % Generate a deterministic deconstruction. In a deterministic
    % deconstruction, we know the value of the ptag, so we don't
    % need to generate a test.

    % Deconstructions are generated semi-eagerly. Any test sub-unifications
    % are generated eagerly (they _must_ be), but assignment unifications
    % are cached.
    %
:- pred generate_det_deconstruction(prog_var::in, cons_id::in,
    assoc_list(prog_var, arg_width)::in, list(unify_mode)::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_det_deconstruction(Var, ConsId, ArgVarsWidths, Modes, Code,
        CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    (
        ( ConsTag = string_tag(_String)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = float_tag(_Float)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = shared_local_tag(_Ptag, _Sectag2)
        ),
        % For constants, if the deconstruction is det, then we already know
        % the value of the constant.
        Code = empty
    ;
        ConsTag = type_info_const_tag(_),
        unexpected($pred, "type_info_const_tag")
    ;
        ConsTag = typeclass_info_const_tag(_),
        unexpected($pred, "typeclass_info_const_tag")
    ;
        ConsTag = ground_term_const_tag(_, _),
        unexpected($pred, "ground_term_const_tag")
    ;
        ConsTag = table_io_entry_tag(_, _),
        unexpected($pred, "table_io_entry_tag")
    ;
        ConsTag = no_tag,
        ( if
            ArgVarsWidths = [ArgVar - _Width],
            Modes = [Mode]
        then
            VarType = variable_type(CI, Var),
            IsDummy = is_type_a_dummy(ModuleInfo, VarType),
            (
                IsDummy = is_dummy_type,
                % We must handle this case specially. If we didn't, the
                % generated code would copy the reference to the Var's
                % current location, which may be stackvar(N) or framevar(N)
                % for negative N, to be the location of Arg, and since Arg
                % may not be a dummy type, it would actually use that location.
                % This can happen in the unify/compare routines for e.g.
                % io.state.
                ( if variable_is_forward_live(!.CLD, ArgVar) then
                    assign_const_to_var(ArgVar, const(llconst_int(0)),
                        CI, !CLD)
                else
                    true
                ),
                Code = empty
            ;
                IsDummy = is_not_dummy_type,
                ArgType = variable_type(CI, ArgVar),
                FieldAndArgVar =
                    field_and_arg_var(uv_var(Var), ArgVar, ArgType),
                generate_sub_unify(FieldAndArgVar, Mode, Code, CI, !CLD)
            )
        else
            unexpected($pred, "no_tag: arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            % Treat single_functor the same as unshared_tag(0).
            Ptag = 0,
            PrevOffset = -1     % There is no secondary tag.
        ;
            ConsTag = unshared_tag(Ptag),
            PrevOffset = -1     % There is no secondary tag.
        ;
            ConsTag = shared_remote_tag(Ptag, _Sectag1),
            PrevOffset = 0      % There is a secondary tag.
        ),
        Rval = var(Var),
        get_proc_info(CI, ProcInfo),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        make_fields_and_arg_vars(VarTypes, ArgVarsWidths,
            Rval, PrevOffset, Ptag, FieldsAndArgVars),
        generate_unify_args(FieldsAndArgVars, Modes, Code, CI, !CLD)
    ;
        ConsTag = direct_arg_tag(Ptag),
        ( if
            ArgVarsWidths = [ArgVar - _Width],
            Modes = [Mode]
        then
            Type = variable_type(CI, ArgVar),
            generate_direct_arg_deconstruct(Var, ArgVar, Ptag, Mode, Type,
                Code, CI, !CLD)
        else
            unexpected($pred, "direct_arg_tag: arity != 1")
        )
    ).

%---------------------------------------------------------------------------%

    % Generate a semideterministic deconstruction.
    % A semideterministic deconstruction unification is tag-test
    % followed by a deterministic deconstruction.
    %
:- pred generate_semi_deconstruction(prog_var::in, cons_id::in,
    assoc_list(prog_var, arg_width)::in, list(unify_mode)::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_semi_deconstruction(Var, Tag, ArgVarsWidths, Modes, Code,
        !CI, !CLD) :-
    VarType = variable_type(!.CI, Var),
    CheaperTagTest = lookup_cheaper_tag_test(!.CI, VarType),
    generate_tag_test(Var, Tag, CheaperTagTest, branch_on_success, SuccLabel,
        TagTestCode, !CI, !CLD),
    remember_position(!.CLD, AfterUnify),
    generate_failure(FailCode, !CI, !.CLD),
    reset_to_position(AfterUnify, !.CI, !:CLD),
    generate_det_deconstruction(Var, Tag, ArgVarsWidths, Modes, DeconsCode,
        !.CI, !CLD),
    SuccessLabelCode = singleton(llds_instr(label(SuccLabel), "")),
    Code = TagTestCode ++ FailCode ++ SuccessLabelCode ++ DeconsCode.

%---------------------------------------------------------------------------%

    % Generate code to perform a list of deterministic subunifications
    % for the arguments of a construction.
    %
:- pred generate_unify_args(list(field_and_arg_var)::in, list(unify_mode)::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_unify_args([], [], empty, _CI, !CLD).
generate_unify_args([], [_ | _], _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_unify_args([_ | _], [], _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_unify_args([FieldAndArgVar | FieldsAndArgVars], [Mode | Modes],
        Code, CI, !CLD) :-
    generate_sub_unify(FieldAndArgVar, Mode, CodeA, CI, !CLD),
    generate_unify_args(FieldsAndArgVars, Modes, CodeB, CI, !CLD),
    Code = CodeA ++ CodeB.

    % Generate a subunification between two [field | variable].
    %
:- pred generate_sub_unify(field_and_arg_var::in, unify_mode::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_sub_unify(FieldAndArgVar, ArgMode, Code, CI, !CLD) :-
    FieldAndArgVar = field_and_arg_var(LeftUniVal, RightVar, Type),
    get_module_info(CI, ModuleInfo),
    ArgMode = unify_modes_lhs_rhs(LeftFromToInsts, RightFromToInsts),
    from_to_insts_to_top_functor_mode(ModuleInfo, LeftFromToInsts, Type,
        LeftTopFunctorMode),
    from_to_insts_to_top_functor_mode(ModuleInfo, RightFromToInsts, Type,
        RightTopFunctorMode),
    (
        LeftTopFunctorMode = top_in,
        (
            RightTopFunctorMode = top_out,
            % Input - Output== assignment ->
            ( if variable_is_forward_live(!.CLD, RightVar) then
                (
                    LeftUniVal = uv_var(LeftVar),
                    generate_sub_assign_to_var_from_var(RightVar, LeftVar,
                        Code, CI, !CLD)
                ;
                    LeftUniVal = uv_field(LeftField),
                    generate_sub_assign_to_var_from_field(RightVar, LeftField,
                        Code, CI, !CLD)
                )
            else
                Code = empty
            )
        ;
            ( RightTopFunctorMode = top_in
            ; RightTopFunctorMode = top_unused
            ),
            unexpected($pred, "unexpected right arg in left-in sub_unify")
        )
    ;
        LeftTopFunctorMode = top_out,
        (
            RightTopFunctorMode = top_in,
            % Output - Input== assignment <-
            (
                LeftUniVal = uv_var(LeftVar),
                ( if variable_is_forward_live(!.CLD, LeftVar) then
                    generate_sub_assign_to_var_from_var(LeftVar, RightVar,
                        Code, CI, !CLD)
                else
                    Code = empty
                )
            ;
                LeftUniVal = uv_field(LeftField),
                % Fields are always considered forward live.
                generate_sub_assign_to_field_from_var(LeftField, RightVar,
                    Code, CI, !CLD)
            )
        ;
            ( RightTopFunctorMode = top_out
            ; RightTopFunctorMode = top_unused
            ),
            unexpected($pred, "unexpected right arg in left-out sub_unify")
        )
    ;
        LeftTopFunctorMode = top_unused,
        (
            RightTopFunctorMode = top_unused,
            % Free-free unification: ignore.
            % XXX This will have to change if we start to support aliasing.
            Code = empty
        ;
            ( RightTopFunctorMode = top_in
            ; RightTopFunctorMode = top_out
            ),
            unexpected($pred, "unexpected right arg in left-ununsed sub_unify")
        )
    ).

:- pred generate_sub_assign_to_var_from_var(prog_var::in, prog_var::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_sub_assign_to_var_from_var(LeftVar, RightVar, Code, _CI, !CLD) :-
    % Cache the assignment.
    assign_var_to_var(LeftVar, RightVar, !CLD),
    Code = empty.

:- pred generate_sub_assign_to_var_from_field(prog_var::in, uni_field::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_sub_assign_to_var_from_field(LeftVar, RightField, Code, CI, !CLD) :-
    RightField = uni_field(RightPtag, RightBaseRval, RightOffset, RightWidth),
    (
        RightWidth = full_word,
        RightLval = field(yes(RightPtag), RightBaseRval,
            const(llconst_int(RightOffset))),
        assign_lval_to_var(LeftVar, RightLval, Code, CI, !CLD)
    ;
        (
            RightWidth = partial_word_first(Mask),
            RightLval = field(yes(RightPtag), RightBaseRval,
                const(llconst_int(RightOffset))),
            RightRval0 = lval(RightLval)
        ;
            RightWidth = partial_word_shifted(Shift, Mask),
            RightLval = field(yes(RightPtag), RightBaseRval,
                const(llconst_int(RightOffset))),
            RightRval0 = right_shift_rval(lval(RightLval), Shift)
        ),
        RightRval = binop(bitwise_and(int_type_int), RightRval0,
            const(llconst_int(Mask))),
        assign_field_lval_expr_to_var(LeftVar, [RightLval], RightRval,
            Code, !CLD)
    ;
        RightWidth = double_word,
        RightLvalA = field(yes(RightPtag), RightBaseRval,
            const(llconst_int(RightOffset))),
        RightLvalB = field(yes(RightPtag), RightBaseRval,
            const(llconst_int(RightOffset + 1))),
        RightRval = binop(float_from_dword,
            lval(RightLvalA), lval(RightLvalB)),
        assign_field_lval_expr_to_var(LeftVar, [RightLvalA, RightLvalB],
            RightRval, Code, !CLD)
    ).

:- pred generate_sub_assign_to_field_from_var(uni_field::in, prog_var::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_sub_assign_to_field_from_var(LeftField, RightVar, Code, CI, !CLD) :-
    LeftField = uni_field(LeftPtag, LeftBaseRval0, LeftOffset, LeftWidth),
    % Assignment from a variable to an lvalue - cannot cache
    % so generate immediately.
    produce_variable(RightVar, ProduceRightVarCode, RightRval, CI, !CLD),
    materialize_vars_in_rval(LeftBaseRval0, LeftBaseRval,
        MaterializeLeftBaseCode, CI, !CLD),
    (
        LeftWidth = full_word,
        LeftLval = field(yes(LeftPtag), LeftBaseRval,
            const(llconst_int(LeftOffset))),
        AssignCode = singleton(llds_instr(assign(LeftLval, RightRval),
            "Copy value"))
    ;
        (
            LeftWidth = partial_word_first(Mask),
            Shift = 0
        ;
            LeftWidth = partial_word_shifted(Shift, Mask)
        ),
        LeftLval = field(yes(LeftPtag), LeftBaseRval,
            const(llconst_int(LeftOffset))),
        ComplementMask = const(llconst_int(\(Mask << Shift))),
        MaskOld = binop(bitwise_and(int_type_int),
            lval(LeftLval), ComplementMask),
        ShiftNew = maybe_left_shift_rval(RightRval, Shift),
        Combined = binop(bitwise_or(int_type_int), MaskOld, ShiftNew),
        AssignCode = singleton(llds_instr(assign(LeftLval, Combined),
            "Update part of word"))
    ;
        LeftWidth = double_word,
        LeftLvalA = field(yes(LeftPtag), LeftBaseRval,
            const(llconst_int(LeftOffset))),
        LeftLvalB = field(yes(LeftPtag), LeftBaseRval,
            const(llconst_int(LeftOffset + 1))),
        SrcA = binop(float_word_bits, RightRval, const(llconst_int(0))),
        SrcB = binop(float_word_bits, RightRval, const(llconst_int(1))),
        Comment = "Update double word",
        AssignCode = from_list([
            llds_instr(assign(LeftLvalA, SrcA), Comment),
            llds_instr(assign(LeftLvalB, SrcB), Comment)
        ])
    ),
    Code = ProduceRightVarCode ++ MaterializeLeftBaseCode ++ AssignCode.

%---------------------------------------------------------------------------%

    % Generate a direct arg unification between
    % - the left-hand-side (the whole term), and
    % - the right-hand-side (the one argument).
    %
:- pred generate_direct_arg_construct(prog_var::in, prog_var::in, ptag::in,
    unify_mode::in, mer_type::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_direct_arg_construct(Var, Arg, Ptag, ArgMode, Type, Code, CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    ArgMode = unify_modes_lhs_rhs(LeftFromToInsts, RightFromToInsts),
    from_to_insts_to_top_functor_mode(ModuleInfo, LeftFromToInsts, Type,
        LeftTopFunctorMode),
    from_to_insts_to_top_functor_mode(ModuleInfo, RightFromToInsts, Type,
        RightTopFunctorMode),
    (
        LeftTopFunctorMode = top_in,
        unexpected($pred, "left arg input in construction")
    ;
        LeftTopFunctorMode = top_out,
        (
            RightTopFunctorMode = top_in,
            % Output - Input == assignment <-
            assign_expr_to_var(Var, mkword(Ptag, var(Arg)), Code, !CLD)
        ;
            ( RightTopFunctorMode = top_out
            ; RightTopFunctorMode = top_unused
            ),
            unexpected($pred, "unexpected right arg in construction")
        )
    ;
        LeftTopFunctorMode = top_unused,
        (
            RightTopFunctorMode = top_unused,
            % Construct a tagged pointer to a pointer value
            % which is as yet unknown.
            % XXX This will have to change if we start to support aliasing.
            assign_const_to_var(Var, mkword_hole(Ptag), CI, !CLD),
            Code = empty
        ;
            ( RightTopFunctorMode = top_in
            ; RightTopFunctorMode = top_out
            ),
            unexpected($pred, "unexpected right arg in unused construction")
        )
    ).

    % Generate a direct arg unification between
    % - the left-hand-side (the whole term), and
    % - the right-hand-side (the one argument).
    %
:- pred generate_direct_arg_deconstruct(prog_var::in, prog_var::in,
    ptag::in, unify_mode::in, mer_type::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_direct_arg_deconstruct(Var, ArgVar, Ptag, ArgMode, Type, Code,
        CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    ArgMode = unify_modes_lhs_rhs(LeftFromToInsts, RightFromToInsts),
    from_to_insts_to_top_functor_mode(ModuleInfo, LeftFromToInsts, Type,
        LeftTopFunctorMode),
    from_to_insts_to_top_functor_mode(ModuleInfo, RightFromToInsts, Type,
        RightTopFunctorMode),
    (
        LeftTopFunctorMode = top_in,
        (
            RightTopFunctorMode = top_out,
            % Input - Output == assignment ->
            ( if variable_is_forward_live(!.CLD, ArgVar) then
                BodyRval = binop(body, var(Var), const(llconst_int(Ptag))),
                assign_expr_to_var(ArgVar, BodyRval, Code, !CLD)
            else
                Code = empty
            )
        ;
            ( RightTopFunctorMode = top_in
            ; RightTopFunctorMode = top_unused
            ),
            unexpected($pred, "unexpected right arg in deconstruction")
        )
    ;
        LeftTopFunctorMode = top_out,
        (
            RightTopFunctorMode = top_in,
            % Output - Input == assignment <-
            reassign_mkword_hole_var(Var, Ptag, var(ArgVar), Code, !CLD)
        ;
            ( RightTopFunctorMode = top_out
            ; RightTopFunctorMode = top_unused
            ),
            unexpected($pred, "unexpected right arg in reverse deconstruction")
        )
    ;
        LeftTopFunctorMode = top_unused,
        (
            RightTopFunctorMode = top_unused,
            % Free-free unification: ignore.
            % XXX This will have to change if we start to support aliasing.
            Code = empty
        ;
            ( RightTopFunctorMode = top_in
            ; RightTopFunctorMode = top_out
            ),
            unexpected($pred, "unexpected right arg in unused deconstruction")
        )
    ).

%---------------------------------------------------------------------------%

generate_const_structs(ModuleInfo, ConstStructMap, !GlobalData) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, unboxed_float, OptUnboxedFloats),
    (
        OptUnboxedFloats = yes,
        UnboxedFloats = have_unboxed_floats
    ;
        OptUnboxedFloats = no,
        UnboxedFloats = do_not_have_unboxed_floats
    ),
    globals.lookup_bool_option(Globals, unboxed_int64s, OptUnboxedInt64s),
    (
        OptUnboxedInt64s = yes,
        UnboxedInt64s = have_unboxed_int64s
    ;
        OptUnboxedInt64s = no,
        UnboxedInt64s = do_not_have_unboxed_int64s
    ),
    module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
    const_struct_db_get_structs(ConstStructDb, ConstStructs),
    global_data_get_static_cell_info(!.GlobalData, StaticCellInfo0),
    list.foldl2(
        generate_const_struct(ModuleInfo, UnboxedFloats, UnboxedInt64s),
        ConstStructs, map.init, ConstStructMap,
        StaticCellInfo0, StaticCellInfo),
    global_data_set_static_cell_info(StaticCellInfo, !GlobalData).

:- pred generate_const_struct(module_info::in,
    have_unboxed_floats::in, have_unboxed_int64s::in,
    pair(int, const_struct)::in,
    const_struct_map::in, const_struct_map::out,
    static_cell_info::in, static_cell_info::out) is det.

generate_const_struct(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstNum - ConstStruct, !ConstStructMap, !StaticCellInfo) :-
    ConstStruct = const_struct(ConsId, ConstArgs, _, _),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    get_cons_arg_widths(ModuleInfo, ConsId, ConstArgs, ConsArgsWidths),
    generate_const_struct_rval(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        !.ConstStructMap, ConsTag, ConsArgsWidths, Rval, !StaticCellInfo),
    map.det_insert(ConstNum, Rval, !ConstStructMap).

:- pred generate_const_struct_rval(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in, cons_tag::in,
    assoc_list(const_struct_arg, arg_width)::in, typed_rval::out,
    static_cell_info::in, static_cell_info::out) is det.

generate_const_struct_rval(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConsTag, ConstArgsWidths, TypedRval,
        !StaticCellInfo) :-
    (
        ConsTag = no_tag,
        (
            ConstArgsWidths = [ConstArgWidth],
            generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
                ConstStructMap, ConstArgWidth, ArgTypedRval),
            TypedRval = ArgTypedRval
        ;
            ( ConstArgsWidths = []
            ; ConstArgsWidths = [_, _ | _]
            ),
            unexpected($pred, "no_tag arity != 1")
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        (
            ConstArgsWidths = [ConstArgWidth],
            generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
                ConstStructMap, ConstArgWidth, ArgTypedRval),
            ArgTypedRval = typed_rval(ArgRval, _RvalType),
            Rval = mkword(Ptag, ArgRval),
            TypedRval = typed_rval(Rval, lt_data_ptr)
        ;
            ( ConstArgsWidths = []
            ; ConstArgsWidths = [_, _ | _]
            ),
            unexpected($pred, "direct_arg_tag: arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            Ptag = 0
        ;
            ConsTag = unshared_tag(Ptag)
        ),
        generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgsWidths, ArgTypedRvals),
        assoc_list.values(ConstArgsWidths, ConsArgWidths),
        pack_ground_term_args(ConsArgWidths, ArgTypedRvals, PackArgTypedRvals),
        add_scalar_static_cell(PackArgTypedRvals, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        Rval = mkword(Ptag, CellPtrConst),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ConsTag = shared_remote_tag(Ptag, Stag),
        generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgsWidths, ArgTypedRvals),
        assoc_list.values(ConstArgsWidths, ConsArgWidths),
        pack_ground_term_args(ConsArgWidths, ArgTypedRvals, PackArgTypedRvals),
        StagTypedRval = typed_rval(const(llconst_int(Stag)),
            lt_int(int_type_int)),
        AllTypedRvals = [StagTypedRval | PackArgTypedRvals],
        add_scalar_static_cell(AllTypedRvals, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        Rval = mkword(Ptag, CellPtrConst),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ( ConsTag = string_tag(_)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = float_tag(_)
        ; ConsTag = shared_local_tag(_, _)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ),
        unexpected($pred, "unexpected tag")
    ).

:- pred generate_const_struct_args(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in,
    assoc_list(const_struct_arg, arg_width)::in, list(typed_rval)::out) is det.

generate_const_struct_args(_, _, _, _, [], []) .
generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, [ConstArgWidth | ConstArgsWidths],
        [TypedRval | TypedRvals]) :-
    generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConstArgWidth, TypedRval),
    generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConstArgsWidths, TypedRvals).

:- pred generate_const_struct_arg(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in,
    pair(const_struct_arg, arg_width)::in, typed_rval::out) is det.

generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConstArg - ArgWidth, TypedRval) :-
    (
        ConstArg = csa_const_struct(ConstNum),
        map.lookup(ConstStructMap, ConstNum, TypedRval)
    ;
        ConstArg = csa_constant(ConsId, _),
        ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
        generate_const_struct_arg_tag(UnboxedFloats, UnboxedInt64s,
            ConsTag, ArgWidth, TypedRval)
    ).

:- pred generate_const_struct_arg_tag(have_unboxed_floats::in,
    have_unboxed_int64s::in, cons_tag::in, arg_width::in,
    typed_rval::out) is det.

generate_const_struct_arg_tag(UnboxedFloats, UnboxedInt64s,
        ConsTag, ArgWidth, TypedRval) :-
    (
        (
            ConsTag = string_tag(String),
            Const = llconst_string(String),
            Type = lt_string
        ;
            ConsTag = int_tag(IntTag),
            int_tag_to_const_and_int_type(IntTag, Const, IntType),
            (
                ( IntType = int_type_int64
                ; IntType = int_type_uint64
                ),
                (
                    UnboxedInt64s = have_unboxed_int64s,
                    Type = lt_int(IntType)
                ;
                    UnboxedInt64s = do_not_have_unboxed_int64s,
                    Type = lt_data_ptr
                )
            ;
                ( IntType = int_type_int
                ; IntType = int_type_int8
                ; IntType = int_type_int16
                ; IntType = int_type_int32
                ),
                Type = lt_int(int_type_int)
            ;
                ( IntType = int_type_uint
                ; IntType = int_type_uint8
                ; IntType = int_type_uint16
                ; IntType = int_type_uint32
                ),
                Type = lt_int(int_type_uint)
            )
        ;
            ConsTag = foreign_tag(Lang, Val),
            expect(unify(Lang, lang_c), $pred,
                "foreign_tag for language other than C"),
            Const = llconst_foreign(Val, lt_int(int_type_int)),
            Type = lt_int(int_type_int)
        ;
            ConsTag = float_tag(Float),
            Const = llconst_float(Float),
            (
                UnboxedFloats = have_unboxed_floats,
                Type = lt_float
            ;
                UnboxedFloats = do_not_have_unboxed_floats,
                % Though a standalone float might have needed to boxed, it may
                % be stored in unboxed form as a constructor argument.
                ( if ArgWidth = double_word then
                    Type = lt_float
                else
                    Type = lt_data_ptr
                )
            )
        ),
        TypedRval = typed_rval(const(Const), Type)
    ;
        ConsTag = shared_local_tag(Ptag, Stag),
        Rval = mkword(Ptag, unop(mkbody, const(llconst_int(Stag)))),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ConsTag = type_ctor_info_tag(ModuleName, TypeName, TypeArity),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
        DataId = rtti_data_id(ctor_rtti_id(RttiTypeCtor,
            type_ctor_type_ctor_info)),
        Rval = const(llconst_data_addr(DataId, no)),
        Type = lt_data_ptr,
        TypedRval = typed_rval(Rval, Type)
    ;
        ConsTag = base_typeclass_info_tag(ModuleName, ClassId, Instance),
        TCName = generate_class_name(ClassId),
        DataId = rtti_data_id(tc_rtti_id(TCName,
            type_class_base_typeclass_info(ModuleName, Instance))),
        Rval = const(llconst_data_addr(DataId, no)),
        Type = lt_data_ptr,
        TypedRval = typed_rval(Rval, Type)
    ;
        ( ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ; ConsTag = single_functor_tag
        ; ConsTag = unshared_tag(_)
        ; ConsTag = shared_remote_tag(_, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ),
        unexpected($pred, "unexpected tag")
    ).

%ZZZ
:- pred det_single_arg_width(list(arg_width)::in, arg_width::out) is det.

det_single_arg_width(ArgWidths, ArgWidth) :-
    (
        ArgWidths = [ArgWidth]
    ;
        ( ArgWidths = []
        ; ArgWidths = [_, _ | _]
        ),
        unexpected($pred, "unexpected arg_width list")
    ).

%---------------------------------------------------------------------------%

generate_ground_term(TermVar, Goal, !CI, !CLD) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    set_of_var.to_sorted_list(NonLocals, NonLocalList),
    (
        NonLocalList = []
        % The term being constructed by the scope is not needed, so there is
        % nothing to do.
    ;
        NonLocalList = [NonLocal],
        ( if NonLocal = TermVar then
            ( if GoalExpr = conj(plain_conj, Conjuncts) then
                get_module_info(!.CI, ModuleInfo),
                get_exprn_opts(!.CI, ExprnOpts),
                UnboxedFloats = get_unboxed_floats(ExprnOpts),
                get_static_cell_info(!.CI, StaticCellInfo0),
                map.init(ActiveMap0),
                generate_ground_term_conjuncts(ModuleInfo, Conjuncts,
                    UnboxedFloats, StaticCellInfo0, StaticCellInfo,
                    ActiveMap0, ActiveMap),
                map.to_assoc_list(ActiveMap, ActivePairs),
                ( if ActivePairs = [TermVar - GroundTerm] then
                    add_forward_live_vars(NonLocals, !CLD),
                    set_static_cell_info(StaticCellInfo, !CI),
                    GroundTerm = typed_rval(Rval, _),
                    assign_const_to_var(TermVar, Rval, !.CI, !CLD)
                else
                    unexpected($pred, "no active pairs")
                )
            else
                unexpected($pred, "malformed goal")
            )
        else
            unexpected($pred, "unexpected nonlocal")
        )
    ;
        NonLocalList = [_, _ | _],
        unexpected($pred, "unexpected nonlocals")
    ).

:- type active_ground_term_map == map(prog_var, typed_rval).

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
    ( if
        GoalExpr = unify(_, _, _, Unify, _),
        Unify = construct(Var, ConsId, ArgVars, _, _, _, SubInfo),
        SubInfo = no_construct_sub_info
    then
        ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
        get_cons_arg_widths(ModuleInfo, ConsId, ArgVars, ArgVarsWidths),
        generate_ground_term_conjunct_tag(Var, ConsTag, ArgVarsWidths,
            UnboxedFloats, !StaticCellInfo, !ActiveMap)
    else
        unexpected($pred, "malformed goal")
    ).

:- pred generate_ground_term_conjunct_tag(prog_var::in, cons_tag::in,
    assoc_list(prog_var, arg_width)::in, have_unboxed_floats::in,
    static_cell_info::in, static_cell_info::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_conjunct_tag(Var, ConsTag, ArgVarsWidths,
        UnboxedFloats, !StaticCellInfo, !ActiveMap) :-
    (
        (
            ConsTag = string_tag(String),
            Const = llconst_string(String),
            Type = lt_string
        ;
            ConsTag = int_tag(IntTag),
            int_tag_to_const_and_int_type(IntTag, Const, IntType),
            Type = lt_int(IntType)
        ;
            ConsTag = foreign_tag(Lang, Val),
            expect(unify(Lang, lang_c), $pred,
                "foreign_tag for language other than C"),
            Const = llconst_foreign(Val, lt_int(int_type_int)),
            Type = lt_int(int_type_int)
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
        ActiveGroundTerm = typed_rval(const(Const), Type),
        map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = shared_local_tag(Ptag, Stag),
        Rval = mkword(Ptag, unop(mkbody, const(llconst_int(Stag)))),
        ActiveGroundTerm = typed_rval(Rval, lt_data_ptr),
        map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = no_tag,
        (
            ArgVarsWidths = [],
            unexpected($pred, "no_tag arity != 1")
        ;
            ArgVarsWidths = [ArgVar - _ArgWidth],
            map.det_remove(ArgVar, RvalType, !ActiveMap),
            map.det_insert(Var, RvalType, !ActiveMap)
        ;
            ArgVarsWidths = [_, _ | _],
            unexpected($pred, "no_tag arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            Ptag = 0
        ;
            ConsTag = unshared_tag(Ptag)
        ),
        generate_ground_term_args(ArgVarsWidths, ArgTypedRvals, !ActiveMap),
        assoc_list.values(ArgVarsWidths, ConsArgWidths),
        pack_ground_term_args(ConsArgWidths, ArgTypedRvals, PackArgTypedRvals),
        add_scalar_static_cell(PackArgTypedRvals, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        Rval = mkword(Ptag, CellPtrConst),
        ActiveGroundTerm = typed_rval(Rval, lt_data_ptr),
        map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = direct_arg_tag(Ptag),
        (
            ArgVarsWidths = [ArgVar - _ArgWidth],
            map.det_remove(ArgVar, typed_rval(ArgRval, _RvalType), !ActiveMap),
            Rval = mkword(Ptag, ArgRval),
            ActiveGroundTerm = typed_rval(Rval, lt_data_ptr),
            map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
        ;
            ( ArgVarsWidths = []
            ; ArgVarsWidths = [_, _ | _]
            ),
            unexpected($pred, "direct_arg_tag: arity != 1")
        )
    ;
        ConsTag = shared_remote_tag(Ptag, Stag),
        generate_ground_term_args(ArgVarsWidths, ArgTypedRvals, !ActiveMap),
        assoc_list.values(ArgVarsWidths, ConsArgWidths),
        pack_ground_term_args(ConsArgWidths, ArgTypedRvals, PackArgTypedRvals),
        StagTypedRval = typed_rval(const(llconst_int(Stag)),
            lt_int(int_type_int)),
        AllTypedRvals = [StagTypedRval | PackArgTypedRvals],
        add_scalar_static_cell(AllTypedRvals, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        Rval = mkword(Ptag, CellPtrConst),
        ActiveGroundTerm = typed_rval(Rval, lt_data_ptr),
        map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ( ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ),
        unexpected($pred, "unexpected tag")
    ).

:- pred int_tag_to_const_and_int_type(int_tag::in, rval_const::out,
    int_type::out) is det.

int_tag_to_const_and_int_type(IntTag, Const, Type) :-
    (
        IntTag = int_tag_int(Int),
        Const = llconst_int(Int),
        Type = int_type_int
    ;
        IntTag = int_tag_uint(UInt),
        Const = llconst_uint(UInt),
        Type = int_type_uint
    ;
        IntTag = int_tag_int8(Int8),
        Const = llconst_int8(Int8),
        Type = int_type_int8
    ;
        IntTag = int_tag_uint8(UInt8),
        Const = llconst_uint8(UInt8),
        Type = int_type_uint8
    ;
        IntTag = int_tag_int16(Int16),
        Const = llconst_int16(Int16),
        Type = int_type_int16
    ;
        IntTag = int_tag_uint16(UInt16),
        Const = llconst_uint16(UInt16),
        Type = int_type_uint16
    ;
        IntTag = int_tag_int32(Int32),
        Const = llconst_int32(Int32),
        Type = int_type_int32
    ;
        IntTag = int_tag_uint32(UInt32),
        Const = llconst_uint32(UInt32),
        Type = int_type_uint32
    ;
        IntTag = int_tag_int64(Int64),
        Const = llconst_int64(Int64),
        Type = int_type_int64
    ;
        IntTag = int_tag_uint64(UInt64),
        Const = llconst_uint64(UInt64),
        Type = int_type_uint64
    ).

:- pred generate_ground_term_args(assoc_list(prog_var, arg_width)::in,
    list(typed_rval)::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_args([], [], !ActiveMap).
generate_ground_term_args([ArgVarWidth | ArgVarsWidths],
        [TypedRval | TypedRvals], !ActiveMap) :-
    generate_ground_term_arg(ArgVarWidth, TypedRval, !ActiveMap),
    generate_ground_term_args(ArgVarsWidths, TypedRvals, !ActiveMap).

:- pred generate_ground_term_arg(pair(prog_var, arg_width)::in,
    typed_rval::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_arg(Var - ConsArgWidth, TypedRval, !ActiveMap) :-
    map.det_remove(Var, TypedRval0, !ActiveMap),
    % Though a standalone float might have needed to boxed, it may be stored
    % in unboxed form as a constructor argument.
    ( if
        ConsArgWidth = double_word,
        TypedRval0 = typed_rval(Rval, lt_data_ptr)
    then
        TypedRval = typed_rval(Rval, lt_float)
    else
        TypedRval = TypedRval0
    ).

%---------------------------------------------------------------------------%

:- pred pack_cell_rvals(list(arg_width)::in,
    list(cell_arg)::in, list(cell_arg)::out,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

pack_cell_rvals(ArgWidths, CellArgs0, CellArgs, Code, CI, !CLD) :-
    pack_args(shift_combine_arg(CI), ArgWidths, CellArgs0, CellArgs,
        empty, Code, !CLD).

:- pred pack_ground_term_args(list(arg_width)::in,
    list(typed_rval)::in, list(typed_rval)::out) is det.

pack_ground_term_args(Widths, !TypedRvals) :-
    pack_args(shift_combine_rval_type, Widths, !TypedRvals, unit, _, unit, _).

%---------------------------------------------------------------------------%

:- pred shift_combine_arg(code_info::in, cell_arg::in, int::in,
    maybe(cell_arg)::in, cell_arg::out, llds_code::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

shift_combine_arg(CI, CellArgA, Shift, MaybeCellArgB, FinalCellArg, !Code,
        !CLD) :-
    ( if
        Shift = 0,
        MaybeCellArgB = no
    then
        FinalCellArg = CellArgA
    else
        (
            CellArgA = cell_arg_full_word(RvalA, Completeness),
            ( if RvalA = var(Var) then
                IsDummy = variable_is_of_dummy_type(CI, Var),
                (
                    IsDummy = is_dummy_type,
                    ShiftCellArgA = cell_arg_skip
                ;
                    IsDummy = is_not_dummy_type,
                    produce_variable(Var, VarCode, VarRval, CI, !CLD),
                    ShiftCellArgA = cell_arg_full_word(
                        maybe_left_shift_rval(VarRval, Shift),
                        Completeness),
                    !:Code = !.Code ++ VarCode
                )
            else if RvalA = const(llconst_int(Int)) then
                NewInt = maybe_left_shift_int(Int, Shift),
                ShiftCellArgA = cell_arg_full_word(const(llconst_int(NewInt)),
                    Completeness)
            else
                unexpected($pred, "non-var or int argument")
            )
        ;
            CellArgA = cell_arg_double_word(RvalA),
            expect(unify(Shift, 0), $pred,
                "double word rval cannot be shifted"),
            ( if RvalA = var(Var) then
                produce_variable(Var, VarCode, VarRval, CI, !CLD),
                ShiftCellArgA = cell_arg_double_word(VarRval),
                !:Code = !.Code ++ VarCode
            else if RvalA = const(llconst_float(_)) then
                ShiftCellArgA = CellArgA
            else
                unexpected($pred, "non-var or float argument")
            )
        ;
            CellArgA = cell_arg_skip,
            ShiftCellArgA = cell_arg_skip
        ;
            CellArgA = cell_arg_take_addr(_, _),
            unexpected($pred, "cell_arg_take_addr")
        ),
        (
            MaybeCellArgB = yes(CellArgB),
            FinalCellArg = bitwise_or_cell_arg(ShiftCellArgA, CellArgB)
        ;
            MaybeCellArgB = no,
            FinalCellArg = ShiftCellArgA
        )
    ).

:- pred shift_combine_rval_type(typed_rval::in, int::in,
    maybe(typed_rval)::in, typed_rval::out,
    unit::in, unit::out, unit::in, unit::out) is det.

shift_combine_rval_type(ArgA, Shift, MaybeArgB, FinalArg, !Acc1, !Acc2) :-
    ArgA = typed_rval(RvalA, TypeA),
    ShiftRvalA = maybe_left_shift_rval(RvalA, Shift),
    (
        MaybeArgB = yes(typed_rval(RvalB, TypeB)),
        ( if TypeA = TypeB then
            FinalRval = binop(bitwise_or(int_type_int), ShiftRvalA, RvalB)
        else
            unexpected($pred, "mismatched llds_types")
        )
    ;
        MaybeArgB = no,
        FinalRval = ShiftRvalA
    ),
    FinalArg = typed_rval(FinalRval, TypeA).

:- func maybe_left_shift_rval(rval, int) = rval.

maybe_left_shift_rval(Rval, Shift) =
    ( if Shift = 0 then
        Rval
    else
        binop(unchecked_left_shift(int_type_int), Rval,
            const(llconst_int(Shift)))
    ).

:- func maybe_left_shift_int(int, int) = int.

maybe_left_shift_int(X, Shift) =
    ( if Shift = 0 then
        X
    else
        X << Shift
    ).

:- func right_shift_rval(rval, int) = rval.

right_shift_rval(Rval, Shift) =
    binop(unchecked_right_shift(int_type_int), Rval,
        const(llconst_int(Shift))).

:- func bitwise_or_cell_arg(cell_arg, cell_arg) = cell_arg.

bitwise_or_cell_arg(CellArgA, CellArgB) = CellArg :-
    ( if maybe_bitwise_or_cell_arg(CellArgA, CellArgB, CellArgPrime) then
        CellArg = CellArgPrime
    else
        unexpected($pred, "invalid combination")
    ).

:- pred maybe_bitwise_or_cell_arg(cell_arg::in, cell_arg::in, cell_arg::out)
    is semidet.

maybe_bitwise_or_cell_arg(CellArgA, CellArgB, CellArg) :-
    (
        CellArgA = cell_arg_full_word(RvalA, CompletenessA),
        CellArgB = cell_arg_full_word(RvalB, CompletenessB),
        Expr = binop(bitwise_or(int_type_int), RvalA, RvalB),
        Completeness = combine_completeness(CompletenessA, CompletenessB),
        CellArg = cell_arg_full_word(Expr, Completeness)
    ;
        CellArgA = cell_arg_full_word(Rval, _),
        CellArgB = cell_arg_skip,
        CellArg = cell_arg_full_word(Rval, incomplete)
    ;
        CellArgA = cell_arg_skip,
        CellArgB = cell_arg_full_word(Rval, _),
        CellArg = cell_arg_full_word(Rval, incomplete)
    ;
        CellArgA = cell_arg_skip,
        CellArgB = cell_arg_skip,
        CellArg = cell_arg_skip
    ).

:- func combine_completeness(completeness, completeness) = completeness.

combine_completeness(complete,   complete) =   complete.
combine_completeness(incomplete, complete) =   incomplete.
combine_completeness(complete,   incomplete) = incomplete.
combine_completeness(incomplete, incomplete) = incomplete.

%---------------------------------------------------------------------------%

:- pred pack_how_to_construct(list(arg_width)::in,
    how_to_construct::in, how_to_construct::out) is det.

pack_how_to_construct(ArgWidths, !HowToConstruct) :-
    (
        ( !.HowToConstruct = construct_statically
        ; !.HowToConstruct = construct_dynamically
        ; !.HowToConstruct = construct_in_region(_)
        )
    ;
        !.HowToConstruct = reuse_cell(CellToReuse0),
        % If any argument packed into a word needs updating,
        % the whole word needs updating.
        % XXX This code changes the meaning of the third argument of
        % cell_to_reuse, from having one element for each *argument*,
        % to one element for each *word* or *double word*.
        % I (zs) see two problems with this. First, the change in
        % meaning is not reflected in the data structure anywhere,
        % and second, given the potential presence of double-word floats,
        % you cannot say simply that the Nth element of NeedsUpdates
        % corresponds to the Nth word of the memory cell.
        % Given that the different ConsIds may have double-word floats
        % in different word positions, I don't see how a correctness
        % argument for this code could be made.
        CellToReuse0 = cell_to_reuse(Var, ConsIds, NeedsUpdates0),
        needs_update_args_to_words(ArgWidths, NeedsUpdates0, NeedsUpdates),
        CellToReuse = cell_to_reuse(Var, ConsIds, NeedsUpdates),
        !:HowToConstruct = reuse_cell(CellToReuse)
    ).

:- pred needs_update_args_to_words(list(arg_width)::in,
    list(needs_update)::in, list(needs_update)::out) is det.

needs_update_args_to_words([], [], []).
needs_update_args_to_words([], [_ | _], _) :-
    unexpected($module, $pred, "mismatched lists").
needs_update_args_to_words([_ | _], [], []) :-
    unexpected($module, $pred, "mismatched lists").
needs_update_args_to_words([Width | Widths], [ArgNU | ArgNUs],
        [WordNU | WordNUs]) :-
    (
        ( Width = full_word
        ; Width = double_word
        ),
        WordNU = ArgNU,
        needs_update_args_to_words(Widths, ArgNUs, WordNUs)
    ;
        Width = partial_word_first(_),
        does_any_arg_in_word_need_update(Widths, ArgNUs, ArgNU, WordNU,
            LaterWordWidths, LaterWordArgNUs),
        needs_update_args_to_words(LaterWordWidths, LaterWordArgNUs, WordNUs)
    ;
        Width = partial_word_shifted(_, _),
        unexpected($pred, "partial_word_shifted")
    ).

:- pred does_any_arg_in_word_need_update(list(arg_width)::in,
    list(needs_update)::in, needs_update::in, needs_update::out,
    list(arg_width)::out, list(needs_update)::out) is det.

does_any_arg_in_word_need_update([], [], !NU, [], []).
does_any_arg_in_word_need_update([], [_ | _], !NU, _, _) :-
    unexpected($module, $pred, "mismatched lists").
does_any_arg_in_word_need_update([_ | _], [], !NU, _, _) :-
    unexpected($module, $pred, "mismatched lists").
does_any_arg_in_word_need_update([Width | Widths], [ArgNU | ArgNUs], !NU,
        LaterWordWidths, LaterWordArgNUs) :-
    (
        ( Width = full_word
        ; Width = double_word
        ; Width = partial_word_first(_)
        ),
        LaterWordWidths = [Width | Widths],
        LaterWordArgNUs = [ArgNU | ArgNUs]
    ;
        Width = partial_word_shifted(_, _),
        (
            ArgNU = needs_update,
            !:NU = needs_update
        ;
            ArgNU = does_not_need_update
        ),
        does_any_arg_in_word_need_update(Widths, ArgNUs, !NU,
            LaterWordWidths, LaterWordArgNUs)
    ).

%---------------------------------------------------------------------------%

:- pred pair_with_width(arg_width::in, T::in, pair(T, arg_width)::out) is det.

pair_with_width(Width, Arg, Arg - Width).

%---------------------------------------------------------------------------%

:- pred var_type_msg(mer_type::in, string::out) is det.

var_type_msg(Type, Msg) :-
    type_to_ctor_det(Type, TypeCtor),
    TypeCtor = type_ctor(TypeSym, TypeArity),
    TypeSymStr = sym_name_to_string(TypeSym),
    string.int_to_string(TypeArity, TypeArityStr),
    string.append_list([TypeSymStr, "/", TypeArityStr], Msg).

%---------------------------------------------------------------------------%
:- end_module ll_backend.unify_gen.
%---------------------------------------------------------------------------%
