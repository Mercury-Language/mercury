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
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.code_util.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.layout.
:- import_module ll_backend.stack_layout.
:- import_module mdbcomp.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

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
    --->    ref(prog_var)
    ;       lval(lval, arg_width).
            % The argument may occupy a word, two words, or only part of a
            % word.

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
        unexpected($module, $pred, "nondet unification")
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
            get_cons_arg_widths(ModuleInfo, ConsId, RHSVars, ConsArgWidths),
            generate_construction(LHSVar, ConsId, RHSVars, ArgModes,
                ConsArgWidths, HowToConstruct, TakeAddr, MaybeSize, GoalInfo,
                Code, !CI, !CLD)
        else
            Code = empty
        )
    ;
        Uni = deconstruct(LHSVar, ConsId, RHSVars, ArgModes, _CanFail, CanCGC),
        get_module_info(!.CI, ModuleInfo),
        get_cons_arg_widths(ModuleInfo, ConsId, RHSVars, ConsArgWidths),
        (
            CodeModel = model_det,
            generate_det_deconstruction(LHSVar, ConsId, RHSVars, ArgModes,
                ConsArgWidths, Code0, !.CI, !CLD)
        ;
            CodeModel = model_semi,
            generate_semi_deconstruction(LHSVar, ConsId, RHSVars, ArgModes,
                ConsArgWidths, Code0, !CI, !CLD)
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
                % XXX avoid strip_tag when we know what tag it will have
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
            unexpected($module, $pred, "det simple_test")
        ;
            CodeModel = model_semi,
            generate_test(VarA, VarB, Code, !CI, !CLD)
        )
    ;
        % These should have been transformed into calls to unification
        % procedures by polymorphism.m.
        Uni = complicated_unify(_Mode, _CanFail, _TypeInfoVars),
        unexpected($module, $pred, "complicated unify")
    ).

:- pred get_cons_arg_widths(module_info::in, cons_id::in,
    list(T)::in, list(arg_width)::out) is det.

get_cons_arg_widths(ModuleInfo, ConsId, Args, AllArgWidths) :-
    ( if
        ConsId = cons(_, _, TypeCtor),
        get_cons_repn_defn(ModuleInfo, TypeCtor, ConsId, ConsRepnDefn)
    then
        ConsArgRepns = ConsRepnDefn ^ cr_args,
        ArgWidths = list.map((func(C) = C ^ car_width), ConsArgRepns),
        list.length(Args, NumArgs),
        list.length(ConsArgRepns, NumConsArgs),
        NumExtraArgs = NumArgs - NumConsArgs,
        ( if NumExtraArgs = 0 then
            AllArgWidths = ArgWidths
        else if NumExtraArgs > 0 then
            ExtraArgWidths = list.duplicate(NumExtraArgs, full_word),
            AllArgWidths = ExtraArgWidths ++ ArgWidths
        else
            unexpected($module, $pred, "too few arguments")
        )
    else
        AllArgWidths = list.duplicate(length(Args), full_word)
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
        expect(unify(ForeignLang, lang_c), $module, $pred,
            "foreign tag for language other than C"),
        TestRval = binop(eq(int_type_int), Rval,
            const(llconst_foreign(ForeignVal, lt_int(int_type_int))))
    ;
        ConsTag = closure_tag(_, _, _),
        % This should never happen, since the error will be detected
        % during mode checking.
        unexpected($module, $pred, "Attempted higher-order unification")
    ;
        ConsTag = type_ctor_info_tag(_, _, _),
        unexpected($module, $pred, "Attempted type_ctor_info unification")
    ;
        ConsTag = base_typeclass_info_tag(_, _, _),
        unexpected($module, $pred, "Attempted base_typeclass_info unification")
    ;
        ConsTag = type_info_const_tag(_),
        unexpected($module, $pred, "Attempted type_info_const_tag unification")
    ;
        ConsTag = typeclass_info_const_tag(_),
        unexpected($module, $pred,
            "Attempted typeclass_info_const_tag unification")
    ;
        ConsTag = ground_term_const_tag(_, _),
        unexpected($module, $pred,
            "Attempted ground_term_const_tag unification")
    ;
        ConsTag = tabling_info_tag(_, _),
        unexpected($module, $pred, "Attempted tabling_info unification")
    ;
        ConsTag = deep_profiling_proc_layout_tag(_, _),
        unexpected($module, $pred,
            "Attempted deep_profiling_proc_layout_tag unification")
    ;
        ConsTag = table_io_entry_tag(_, _),
        unexpected($module, $pred, "Attempted table_io_entry_tag unification")
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
    ;
        ConsTag = reserved_address_tag(RA),
        TestRval = binop(eq(int_type_int), Rval, generate_reserved_address(RA))
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
generate_reserved_address(small_int_as_pointer(N)) = const(llconst_int(N)).

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
    list(prog_var)::in, list(unify_mode)::in, list(arg_width)::in,
    how_to_construct::in,
    list(int)::in, maybe(term_size_value)::in, hlds_goal_info::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_construction(LHSVar, ConsId, RHSVars, ArgModes, ArgWidths,
        HowToConstruct, TakeAddr, MaybeSize, GoalInfo, Code, !CI, !CLD) :-
    get_module_info(!.CI, ModuleInfo),
    Tag = cons_id_to_tag(ModuleInfo, ConsId),
    generate_construction_2(Tag, LHSVar, RHSVars, ArgModes, ArgWidths,
        HowToConstruct, TakeAddr, MaybeSize, GoalInfo, Code, !CI, !CLD).

:- pred generate_construction_2(cons_tag::in, prog_var::in,
    list(prog_var)::in, list(unify_mode)::in, list(arg_width)::in,
    how_to_construct::in,
    list(int)::in, maybe(term_size_value)::in, hlds_goal_info::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_construction_2(ConsTag, LHSVar, RHSVars, ArgModes, ArgWidths,
        HowToConstruct0, TakeAddr, MaybeSize, GoalInfo, Code, !CI, !CLD) :-
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
        expect(unify(Lang, lang_c), $module, $pred,
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
            RHSVars = [RHSVar],
            ArgModes = [ArgMode]
        then
            (
                TakeAddr = [],
                Type = variable_type(!.CI, RHSVar),
                generate_sub_unify(ref(LHSVar), ref(RHSVar), ArgMode, Type,
                    Code, !.CI, !CLD)
            ;
                TakeAddr = [_ | _],
                unexpected($module, $pred, "notag: take_addr")
            )
        else
            unexpected($module, $pred, "no_tag: arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            % Treat single_functor the same as unshared_tag(0).
            Ptag = 0
        ;
            ConsTag = unshared_tag(Ptag)
        ),
        var_types(!.CI, RHSVars, ArgTypes),
        generate_cons_args(RHSVars, ArgTypes, ArgModes, ArgWidths, TakeAddr,
            !.CI, CellArgs0, MayUseAtomic),
        pack_cell_rvals(ArgWidths, CellArgs0, CellArgs, PackCode, !.CI, !CLD),
        pack_how_to_construct(ArgWidths, HowToConstruct0, HowToConstruct),
        Context = goal_info_get_context(GoalInfo),
        construct_cell(LHSVar, Ptag, CellArgs, HowToConstruct, MaybeSize,
            Context, MayUseAtomic, ConstructCode, !CI, !CLD),
        Code = PackCode ++ ConstructCode
    ;
        ConsTag = direct_arg_tag(Ptag),
        ( if
            RHSVars = [RHSVar],
            ArgModes = [ArgMode]
        then
            (
                TakeAddr = [],
                Type = variable_type(!.CI, RHSVar),
                generate_direct_arg_construct(LHSVar, RHSVar, Ptag,
                    ArgMode, Type, Code, !.CI, !CLD)
            ;
                TakeAddr = [_ | _],
                unexpected($module, $pred, "direct_arg_tag: take_addr")
            )
        else
            unexpected($module, $pred, "direct_arg_tag: arity != 1")
        )
    ;
        ConsTag = shared_remote_tag(Ptag, Sectag),
        var_types(!.CI, RHSVars, ArgTypes),
        generate_cons_args(RHSVars, ArgTypes, ArgModes, ArgWidths, TakeAddr,
            !.CI, CellArgs0, MayUseAtomic),
        pack_cell_rvals(ArgWidths, CellArgs0, CellArgs1, PackCode, !.CI, !CLD),
        pack_how_to_construct(ArgWidths, HowToConstruct0, HowToConstruct),
        CellArgs = [cell_arg_full_word(const(llconst_int(Sectag)), complete)
            | CellArgs1],
        Context = goal_info_get_context(GoalInfo),
        construct_cell(LHSVar, Ptag, CellArgs, HowToConstruct, MaybeSize,
            Context, MayUseAtomic, ConstructCode, !CI, !CLD),
        Code = PackCode ++ ConstructCode
    ;
        ConsTag = shared_local_tag(Ptag, Sectag),
        assign_const_to_var(LHSVar,
            mkword(Ptag, unop(mkbody, const(llconst_int(Sectag)))),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = type_ctor_info_tag(ModuleName, TypeName, TypeArity),
        expect(unify(RHSVars, []), $module, $pred,
            "type_ctor_info constant has args"),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
        DataId = rtti_data_id(ctor_rtti_id(RttiTypeCtor,
            type_ctor_type_ctor_info)),
        assign_const_to_var(LHSVar, const(llconst_data_addr(DataId, no)),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = base_typeclass_info_tag(ModuleName, ClassId, Instance),
        expect(unify(RHSVars, []), $module, $pred,
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
        expect(unify(RHSVars, []), $module, $pred,
            "tabling_info constant has args"),
        get_module_info(!.CI, ModuleInfo),
        ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
        DataId = proc_tabling_data_id(ProcLabel, tabling_info),
        assign_const_to_var(LHSVar, const(llconst_data_addr(DataId, no)),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = deep_profiling_proc_layout_tag(PredId, ProcId),
        expect(unify(RHSVars, []), $module, $pred,
            "deep_profiling_proc_static has args"),
        get_module_info(!.CI, ModuleInfo),
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
        expect(unify(RHSVars, []), $module, $pred, "table_io_entry has args"),
        PredProcId = proc(PredId, ProcId),
        DataId = layout_slot_id(table_io_entry_id, PredProcId),
        assign_const_to_var(LHSVar, const(llconst_data_addr(DataId, no)),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = reserved_address_tag(RA),
        expect(unify(RHSVars, []), $module, $pred,
            "reserved_address constant has args"),
        assign_const_to_var(LHSVar, generate_reserved_address(RA), !.CI, !CLD),
        Code = empty
    ;
        ConsTag = shared_with_reserved_addresses_tag(_RAs, ThisTag),
        % For shared_with_reserved_address, the sharing is only important
        % for tag tests, not for constructions, so here we just recurse
        % on the real representation.
        generate_construction_2(ThisTag, LHSVar, RHSVars, ArgModes, ArgWidths,
            HowToConstruct0, TakeAddr, MaybeSize, GoalInfo, Code, !CI, !CLD)
    ;
        ConsTag = closure_tag(PredId, ProcId, EvalMethod),
        expect(unify(TakeAddr, []), $module, $pred,
            "closure_tag has take_addr"),
        expect(unify(MaybeSize, no), $module, $pred,
            "closure_tag has size"),
        generate_closure(PredId, ProcId, EvalMethod, LHSVar, RHSVars, GoalInfo,
            Code, !CI, !CLD)
    ).

    % This predicate constructs or extends a closure.
    % The structure of closures is defined in runtime/mercury_ho_call.h.
    %
:- pred generate_closure(pred_id::in, proc_id::in, lambda_eval_method::in,
    prog_var::in, list(prog_var)::in, hlds_goal_info::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_closure(PredId, ProcId, EvalMethod, Var, Args, GoalInfo, Code,
        !CI, !CLD) :-
    get_module_info(!.CI, ModuleInfo),
    module_info_get_preds(ModuleInfo, Preds),
    map.lookup(Preds, PredId, PredInfo),
    pred_info_get_proc_table(PredInfo, Procs),
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
    ( if
        EvalMethod = lambda_normal,
        Args = [CallPred | CallArgs],
        ProcHeadVars = [ProcPred | ProcArgs],
        ProcInfoGoal = hlds_goal(generic_call(higher_order(ProcPred, _, _, _),
            ProcArgs, _, _, CallDeterminism), _GoalInfo),
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
        Deep = no,
        % XXX If float registers are used, float register arguments are placed
        % after regular register arguments in the hidden arguments vector.
        % The code below does not handle that layout.
        globals.lookup_bool_option(Globals, use_float_registers, UseFloatRegs),
        UseFloatRegs = no
    then
        (
            CallArgs = [],
            % If there are no new arguments, we can just use the old closure.
            assign_var_to_var(Var, CallPred, !CLD),
            Code = empty
        ;
            CallArgs = [_ | _],
            get_next_label(LoopStart, !CI),
            get_next_label(LoopTest, !CI),
            acquire_reg(reg_r, LoopCounter, !CLD),
            acquire_reg(reg_r, NumOldArgs, !CLD),
            acquire_reg(reg_r, NewClosure, !CLD),
            Zero = const(llconst_int(0)),
            One = const(llconst_int(1)),
            Two = const(llconst_int(2)),
            Three = const(llconst_int(3)),
            list.length(CallArgs, NumNewArgs),
            NumNewArgs_Rval = const(llconst_int(NumNewArgs)),
            NumNewArgsPlusThree = NumNewArgs + 3,
            NumNewArgsPlusThree_Rval = const(llconst_int(NumNewArgsPlusThree)),
            produce_variable(CallPred, OldClosureCode, OldClosure, !.CI, !CLD),
            Context = goal_info_get_context(GoalInfo),
            maybe_add_alloc_site_info(Context, "closure", NumNewArgsPlusThree,
                MaybeAllocId, !CI),
            % The new closure contains a pointer to the old closure.
            NewClosureMayUseAtomic = may_not_use_atomic_alloc,
            NewClosureCode = from_list([
                llds_instr(comment("build new closure from old closure"), ""),
                llds_instr(
                    assign(NumOldArgs, lval(field(yes(0), OldClosure, Two))),
                    "get number of arguments"),
                llds_instr(incr_hp(NewClosure, no, no,
                    binop(int_add(int_type_int), lval(NumOldArgs),
                        NumNewArgsPlusThree_Rval),
                    MaybeAllocId, NewClosureMayUseAtomic, no, no_llds_reuse),
                    "allocate new closure"),
                llds_instr(assign(field(yes(0), lval(NewClosure), Zero),
                    lval(field(yes(0), OldClosure, Zero))),
                    "set closure layout structure"),
                llds_instr(assign(field(yes(0), lval(NewClosure), One),
                    lval(field(yes(0), OldClosure, One))),
                    "set closure code pointer"),
                llds_instr(assign(field(yes(0), lval(NewClosure), Two),
                    binop(int_add(int_type_int), lval(NumOldArgs),
                        NumNewArgs_Rval)),
                    "set new number of arguments"),
                llds_instr(
                    assign(NumOldArgs,
                        binop(int_add(int_type_int), lval(NumOldArgs), Three)),
                    "set up loop limit"),
                llds_instr(assign(LoopCounter, Three),
                    "initialize loop counter"),
                % It is possible for the number of hidden arguments to be zero,
                % in which case the body of this loop should not be executed
                % at all. This is why we jump to the loop condition test.
                llds_instr(goto(code_label(LoopTest)),
                    "enter the copy loop at the conceptual top"),
                llds_instr(label(LoopStart),
                    "start of loop, nofulljump"),
                llds_instr(
                    assign(field(yes(0), lval(NewClosure), lval(LoopCounter)),
                        lval(field(yes(0), OldClosure, lval(LoopCounter)))),
                    "copy old hidden argument"),
                llds_instr(
                    assign(LoopCounter,
                        binop(int_add(int_type_int), lval(LoopCounter), One)),
                    "increment loop counter"),
                llds_instr(label(LoopTest),
                    "do we have more old arguments to copy? nofulljump"),
                llds_instr(
                    if_val(binop(int_lt(int_type_int),
                        lval(LoopCounter), lval(NumOldArgs)),
                        code_label(LoopStart)),
                    "repeat the loop?")
            ]),
            generate_extra_closure_args(CallArgs, LoopCounter, NewClosure,
                ExtraArgsCode, !.CI, !CLD),
            release_reg(LoopCounter, !CLD),
            release_reg(NumOldArgs, !CLD),
            release_reg(NewClosure, !CLD),
            assign_lval_to_var(Var, NewClosure, AssignCode, !.CI, !CLD),
            Code = OldClosureCode ++ NewClosureCode ++ ExtraArgsCode ++
                 AssignCode
        )
    else
        CodeAddr = make_proc_entry_label(!.CI, ModuleInfo, PredId, ProcId, no),
        ProcLabel = extract_proc_label_from_code_addr(CodeAddr),
        CodeAddrRval = const(llconst_code_addr(CodeAddr)),
        continuation_info.generate_closure_layout( ModuleInfo, PredId, ProcId,
            ClosureInfo),
        module_info_get_name(ModuleInfo, ModuleName),
        Context = goal_info_get_context(GoalInfo),
        term.context_file(Context, FileName),
        term.context_line(Context, LineNumber),
        GoalId = goal_info_get_goal_id(GoalInfo),
        GoalId = goal_id(GoalIdNum),
        GoalIdStr = string.int_to_string(GoalIdNum),
        get_proc_label(!.CI, CallerProcLabel),
        get_next_closure_seq_no(SeqNo, !CI),
        get_static_cell_info(!.CI, StaticCellInfo0),
        hlds.hlds_pred.pred_info_get_origin(PredInfo, PredOrigin),
        stack_layout.construct_closure_layout(CallerProcLabel,
            SeqNo, ClosureInfo, ProcLabel, ModuleName, FileName, LineNumber,
            PredOrigin, GoalIdStr, StaticCellInfo0, StaticCellInfo,
            ClosureLayoutTypedRvals, Data),
        set_static_cell_info(StaticCellInfo, !CI),
        add_closure_layout(Data, !CI),
        % For now, closures always have zero size, and the size slot
        % is never looked at.
        add_scalar_static_cell(ClosureLayoutTypedRvals, ClosureDataAddr, !CI),
        ClosureLayoutRval = const(llconst_data_addr(ClosureDataAddr, no)),
        proc_info_arg_info(ProcInfo, ArgInfo),
        get_vartypes(!.CI, VarTypes),
        MayUseAtomic0 = initial_may_use_atomic(ModuleInfo),
        generate_pred_args(!.CI, VarTypes, Args, ArgInfo, ArgsR, ArgsF,
            MayUseAtomic0, MayUseAtomic),
        list.length(ArgsR, NumArgsR),
        list.length(ArgsF, NumArgsF),
        NumArgsRF = encode_num_generic_call_vars(NumArgsR, NumArgsF),
        list.append(ArgsR, ArgsF, ArgsRF),
        Vector = [
            cell_arg_full_word(ClosureLayoutRval, complete),
            cell_arg_full_word(CodeAddrRval, complete),
            cell_arg_full_word(const(llconst_int(NumArgsRF)), complete)
            | ArgsRF
        ],
        % XXX construct_dynamically is just a dummy value. We just want
        % something which is not construct_in_region(_).
        HowToConstruct = construct_dynamically,
        MaybeSize = no,
        maybe_add_alloc_site_info(Context, "closure", length(Vector),
            MaybeAllocId, !CI),
        assign_cell_to_var(Var, no, 0, Vector, HowToConstruct,
            MaybeSize, MaybeAllocId, MayUseAtomic, Code, !CI, !CLD)
    ).

:- pred generate_extra_closure_args(list(prog_var)::in, lval::in,
    lval::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_extra_closure_args([], _, _, empty, _CI, !CLD).
generate_extra_closure_args([Var | Vars], LoopCounter, NewClosure, Code,
        CI, !CLD) :-
    FieldLval = field(yes(0), lval(NewClosure), lval(LoopCounter)),
    IsDummy = variable_is_of_dummy_type(CI, Var),
    (
        IsDummy = is_dummy_type,
        ProduceCode = empty,
        AssignCode = singleton(
            llds_instr(assign(FieldLval, const(llconst_int(0))),
                "set new argument field (dummy type)")
        )
    ;
        IsDummy = is_not_dummy_type,
        produce_variable(Var, ProduceCode, Value, CI, !CLD),
        AssignCode = singleton(
            llds_instr(assign(FieldLval, Value),
                "set new argument field")
        )
    ),
    IncrCode = singleton(
        llds_instr(assign(LoopCounter,
            binop(int_add(int_type_int), lval(LoopCounter),
                const(llconst_int(1)))),
            "increment argument counter")
    ),
    generate_extra_closure_args(Vars, LoopCounter, NewClosure, VarsCode,
        CI, !CLD),
    Code = ProduceCode ++ AssignCode ++ IncrCode ++ VarsCode.

:- pred generate_pred_args(code_info::in, vartypes::in, list(prog_var)::in,
    list(arg_info)::in, list(cell_arg)::out, list(cell_arg)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

generate_pred_args(_, _, [], _, [], [], !MayUseAtomic).
generate_pred_args(_, _, [_ | _], [], _, _, !MayUseAtomic) :-
    unexpected($module, $pred, "insufficient args").
generate_pred_args(CI, VarTypes, [Var | Vars], [ArgInfo | ArgInfos],
        ArgsR, ArgsF, !MayUseAtomic) :-
    ArgInfo = arg_info(reg(RegType, _), ArgMode),
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
        CellArg = cell_arg_full_word(Rval, complete)
    ;
        ( ArgMode = top_out
        ; ArgMode = top_unused
        ),
        CellArg = cell_arg_skip
    ),
    lookup_var_type(VarTypes, Var, Type),
    get_module_info(CI, ModuleInfo),
    update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic),
    generate_pred_args(CI, VarTypes, Vars, ArgInfos, ArgsR0, ArgsF0,
        !MayUseAtomic),
    (
        RegType = reg_r,
        ArgsR = [CellArg | ArgsR0],
        ArgsF = ArgsF0
    ;
        RegType = reg_f,
        ArgsR = ArgsR0,
        ArgsF = [CellArg | ArgsF0]
    ).

:- pred generate_cons_args(list(prog_var)::in, list(mer_type)::in,
    list(unify_mode)::in, list(arg_width)::in, list(int)::in,
    code_info::in, list(cell_arg)::out, may_use_atomic_alloc::out) is det.

generate_cons_args(Vars, Types, Modes, Widths, TakeAddr, CI, !:Args,
        !:MayUseAtomic) :-
    get_module_info(CI, ModuleInfo),
    !:MayUseAtomic = initial_may_use_atomic(ModuleInfo),
    ( if
        FirstArgNum = 1,
        generate_cons_args_2(Vars, Types, Modes, Widths, FirstArgNum, TakeAddr,
            CI, !:Args, !MayUseAtomic)
    then
        true
    else
        unexpected($module, $pred, "length mismatch")
    ).

    % Create a list of maybe(rval) for the arguments for a construction
    % unification. For each argument which is input to the construction
    % unification, we produce `yes(var(Var))', but if the argument is free,
    % we just produce `no', meaning don't generate an assignment to that field.
    %
:- pred generate_cons_args_2(list(prog_var)::in, list(mer_type)::in,
    list(unify_mode)::in, list(arg_width)::in, int::in, list(int)::in,
    code_info::in, list(cell_arg)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is semidet.

generate_cons_args_2([], [], [], [], _CurArgNum, _TakeAddr, _CI, [],
        !MayUseAtomic).
generate_cons_args_2([Var | Vars], [Type | Types], [ArgMode | ArgModes],
        [Width | Widths], CurArgNum, !.TakeAddr, CI, [CellArg | CellArgs],
        !MayUseAtomic) :-
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
        !:MayUseAtomic = may_not_use_atomic_alloc,
        generate_cons_args_2(Vars, Types, ArgModes, Widths, CurArgNum + 1,
            !.TakeAddr, CI, CellArgs, !MayUseAtomic)
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
        ),
        generate_cons_args_2(Vars, Types, ArgModes, Widths, CurArgNum + 1,
            !.TakeAddr, CI, CellArgs, !MayUseAtomic)
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

:- pred pack_cell_rvals(list(arg_width)::in,
    list(cell_arg)::in, list(cell_arg)::out,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

pack_cell_rvals(ArgWidths, CellArgs0, CellArgs, Code, CI, !CLD) :-
    pack_args(shift_combine_arg(CI), ArgWidths, CellArgs0, CellArgs,
        empty, Code, !CLD).

:- pred pack_how_to_construct(list(arg_width)::in,
    how_to_construct::in, how_to_construct::out) is det.

pack_how_to_construct(ArgWidths, !HowToConstruct) :-
    (
        !.HowToConstruct = construct_statically
    ;
        !.HowToConstruct = construct_dynamically
    ;
        !.HowToConstruct = construct_in_region(_)
    ;
        !.HowToConstruct = reuse_cell(CellToReuse0),
        % If an argument within a packed field needs updating,
        % the field needs updating.
        CellToReuse0 = cell_to_reuse(Var, ConsIds, NeedsUpdates0),
        group_same_word_elements(ArgWidths, NeedsUpdates0, NeedsUpdates1),
        list.map(condense_needs_updates, NeedsUpdates1) = NeedsUpdates,
        CellToReuse = cell_to_reuse(Var, ConsIds, NeedsUpdates),
        !:HowToConstruct = reuse_cell(CellToReuse)
    ).

:- func condense_needs_updates(list(needs_update)) = needs_update.

condense_needs_updates(NeedsUpdatess) =
    ( if list.member(needs_update, NeedsUpdatess) then
        needs_update
    else
        does_not_need_update
    ).

:- pred construct_cell(prog_var::in, tag::in, list(cell_arg)::in,
    how_to_construct::in, maybe(term_size_value)::in, prog_context::in,
    may_use_atomic_alloc::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

construct_cell(Var, Ptag, CellArgs, HowToConstruct, MaybeSize, Context,
        MayUseAtomic, Code, !CI, !CLD) :-
    VarType = variable_type(!.CI, Var),
    var_type_msg(VarType, VarTypeMsg),
    % If we're doing accurate GC, then for types which hold RTTI that
    % will be traversed by the collector at GC-time, we need to allocate
    % an extra word at the start, to hold the forwarding pointer.
    % Normally we would just overwrite the first word of the object
    % in the "from" space, but this can't be done for objects which will be
    % referenced during the garbage collection process.
    ( if
        get_globals(!.CI, Globals),
        globals.get_gc_method(Globals, GCMethod),
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

:- pred maybe_add_alloc_site_info(prog_context::in, string::in, int::in,
    maybe(alloc_site_id)::out, code_info::in, code_info::out) is det.

maybe_add_alloc_site_info(Context, VarTypeMsg, Size, MaybeAllocId, !CI) :-
    get_globals(!.CI, Globals),
    globals.lookup_bool_option(Globals, profile_memory, ProfileMemory),
    (
        ProfileMemory = yes,
        add_alloc_site_info(Context, VarTypeMsg, Size, AllocId, !CI),
        MaybeAllocId = yes(AllocId)
    ;
        ProfileMemory = no,
        MaybeAllocId = no
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

:- pred var_types(code_info::in, list(prog_var)::in, list(mer_type)::out)
    is det.

var_types(CI, Vars, Types) :-
    get_proc_info(CI, ProcInfo),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    lookup_var_types(VarTypes, Vars, Types).

%---------------------------------------------------------------------------%

    % Construct a pair of lists that associates the fields of a term
    % with variables.
    %
:- pred make_fields_and_argvars(list(prog_var)::in, list(arg_width)::in,
    rval::in, int::in, int::in, list(uni_val)::out, list(uni_val)::out) is det.

make_fields_and_argvars([], [], _, _, _, [], []).
make_fields_and_argvars([Var | Vars], [Width | Widths], Rval, PrevOffset0,
        TagNum, [F | Fs], [A | As]) :-
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
    F = lval(field(yes(TagNum), Rval, const(llconst_int(Offset))), Width),
    A = ref(Var),
    make_fields_and_argvars(Vars, Widths, Rval, PrevOffset, TagNum, Fs, As).
make_fields_and_argvars([], [_ | _], _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched lists").
make_fields_and_argvars([_ | _], [], _, _, _, _, _) :-
    unexpected($module, $pred, "mismatched lists").

%---------------------------------------------------------------------------%

    % Generate a deterministic deconstruction. In a deterministic
    % deconstruction, we know the value of the tag, so we don't
    % need to generate a test.

    % Deconstructions are generated semi-eagerly. Any test sub-unifications
    % are generated eagerly (they _must_ be), but assignment unifications
    % are cached.
    %
:- pred generate_det_deconstruction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, list(arg_width)::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_det_deconstruction(Var, Cons, Args, Modes, ArgWidths, Code,
        CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    Tag = cons_id_to_tag(ModuleInfo, Cons),
    generate_det_deconstruction_2(Var, Cons, Args, Modes, ArgWidths, Tag,
        Code, CI, !CLD).

:- pred generate_det_deconstruction_2(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, list(arg_width)::in,
    cons_tag::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_det_deconstruction_2(Var, Cons, Args, Modes, ArgWidths, Tag, Code,
        CI, !CLD) :-
    % For constants, if the deconstruction is det, then we already know
    % the value of the constant, so Code = empty.
    (
        ( Tag = string_tag(_String)
        ; Tag = int_tag(_)
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
        Tag = type_info_const_tag(_),
        unexpected($module, $pred, "type_info_const_tag")
    ;
        Tag = typeclass_info_const_tag(_),
        unexpected($module, $pred, "typeclass_info_const_tag")
    ;
        Tag = ground_term_const_tag(_, _),
        unexpected($module, $pred, "ground_term_const_tag")
    ;
        Tag = table_io_entry_tag(_, _),
        unexpected($module, $pred, "table_io_entry_tag")
    ;
        Tag = no_tag,
        ( if
            Args = [Arg],
            Modes = [Mode],
            ArgWidths = [_ArgWidth]
        then
            VarType = variable_type(CI, Var),
            get_module_info(CI, ModuleInfo),
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
                ( if variable_is_forward_live(!.CLD, Arg) then
                    assign_const_to_var(Arg, const(llconst_int(0)), CI, !CLD)
                else
                    true
                ),
                Code = empty
            ;
                IsDummy = is_not_dummy_type,
                ArgType = variable_type(CI, Arg),
                generate_sub_unify(ref(Var), ref(Arg), Mode, ArgType,
                    Code, CI, !CLD)
            )
        else
            unexpected($module, $pred, "no_tag: arity != 1")
        )
    ;
        Tag = single_functor_tag,
        % Treat single_functor the same as unshared_tag(0).
        generate_det_deconstruction_2(Var, Cons, Args, Modes, ArgWidths,
            unshared_tag(0), Code, CI, !CLD)
    ;
        Tag = unshared_tag(Ptag),
        Rval = var(Var),
        make_fields_and_argvars(Args, ArgWidths, Rval, -1, Ptag,
            Fields, ArgVars),
        var_types(CI, Args, ArgTypes),
        generate_unify_args(Fields, ArgVars, Modes, ArgTypes, Code, CI, !CLD)
    ;
        Tag = direct_arg_tag(Ptag),
        ( if
            Args = [Arg],
            Modes = [Mode],
            ArgWidths = [_]
        then
            Type = variable_type(CI, Arg),
            generate_direct_arg_deconstruct(Var, Arg, Ptag, Mode, Type, Code,
                CI, !CLD)
        else
            unexpected($module, $pred, "direct_arg_tag: arity != 1")
        )
    ;
        Tag = shared_remote_tag(Ptag, _Sectag1),
        Rval = var(Var),
        make_fields_and_argvars(Args, ArgWidths, Rval, 0, Ptag,
            Fields, ArgVars),
        var_types(CI, Args, ArgTypes),
        generate_unify_args(Fields, ArgVars, Modes, ArgTypes, Code, CI, !CLD)
    ;
        % For shared_with_reserved_address, the sharing is only important
        % for tag tests, not for det deconstructions, so here we just recurse
        % on the real representation.
        Tag = shared_with_reserved_addresses_tag(_RAs, ThisTag),
        generate_det_deconstruction_2(Var, Cons, Args, Modes, ArgWidths,
            ThisTag, Code, CI, !CLD)
    ).

%---------------------------------------------------------------------------%

    % Generate a semideterministic deconstruction.
    % A semideterministic deconstruction unification is tag-test
    % followed by a deterministic deconstruction.
    %
:- pred generate_semi_deconstruction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, list(arg_width)::in,
    llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

generate_semi_deconstruction(Var, Tag, Args, Modes, ArgWidths, Code,
        !CI, !CLD) :-
    VarType = variable_type(!.CI, Var),
    CheaperTagTest = lookup_cheaper_tag_test(!.CI, VarType),
    generate_tag_test(Var, Tag, CheaperTagTest, branch_on_success, SuccLabel,
        TagTestCode, !CI, !CLD),
    remember_position(!.CLD, AfterUnify),
    generate_failure(FailCode, !CI, !.CLD),
    reset_to_position(AfterUnify, !.CI, !:CLD),
    generate_det_deconstruction(Var, Tag, Args, Modes, ArgWidths, DeconsCode,
        !.CI, !CLD),
    SuccessLabelCode = singleton(llds_instr(label(SuccLabel), "")),
    Code = TagTestCode ++ FailCode ++ SuccessLabelCode ++ DeconsCode.

%---------------------------------------------------------------------------%

    % Generate code to perform a list of deterministic subunifications
    % for the arguments of a construction.
    %
:- pred generate_unify_args(list(uni_val)::in, list(uni_val)::in,
    list(unify_mode)::in, list(mer_type)::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_unify_args(Ls, Rs, Ms, Ts, Code, CI, !CLD) :-
    ( if generate_unify_args_2(Ls, Rs, Ms, Ts, Code0, CI, !CLD) then
        Code = Code0
    else
        unexpected($module, $pred, "length mismatch")
    ).

:- pred generate_unify_args_2(list(uni_val)::in, list(uni_val)::in,
    list(unify_mode)::in, list(mer_type)::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is semidet.

generate_unify_args_2([], [], [], [], empty, _CI, !CLD).
generate_unify_args_2([L | Ls], [R | Rs], [M | Ms], [T | Ts], Code,
        CI, !CLD) :-
    generate_sub_unify(L, R, M, T, CodeA, CI, !CLD),
    generate_unify_args_2(Ls, Rs, Ms, Ts, CodeB, CI, !CLD),
    Code = CodeA ++ CodeB.

%---------------------------------------------------------------------------%

    % Generate a subunification between two [field | variable].
    %
:- pred generate_sub_unify(uni_val::in, uni_val::in, unify_mode::in,
    mer_type::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_sub_unify(L, R, ArgMode, Type, Code, CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    ArgMode = unify_modes_lhs_rhs(LeftFromToInsts, RightFromToInsts),
    from_to_insts_to_top_functor_mode(ModuleInfo, LeftFromToInsts, Type,
        LeftTopFunctorMode),
    from_to_insts_to_top_functor_mode(ModuleInfo, RightFromToInsts, Type,
        RightTopFunctorMode),
    ( if
        % Input - input == test unification
        LeftTopFunctorMode = top_in,
        RightTopFunctorMode = top_in
    then
        % This shouldn't happen, since mode analysis should avoid creating
        % any tests in the arguments of a construction or deconstruction
        % unification.
        unexpected($module, $pred, "test in arg of [de]construction")
    else if
        % Input - Output== assignment ->
        LeftTopFunctorMode = top_in,
        RightTopFunctorMode = top_out
    then
        generate_sub_assign(R, L, Code, CI, !CLD)
    else if
        % Output - Input== assignment <-
        LeftTopFunctorMode = top_out,
        RightTopFunctorMode = top_in
    then
        generate_sub_assign(L, R, Code, CI, !CLD)
    else if
        LeftTopFunctorMode = top_unused,
        RightTopFunctorMode = top_unused
    then
        Code = empty
        % free-free - ignore
        % XXX I think this will have to change if we start to support aliasing.
    else
        unexpected($module, $pred, "some strange unify")
    ).

:- pred generate_sub_assign(uni_val::in, uni_val::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_sub_assign(Left, Right, Code, CI, !CLD) :-
    (
        Left = lval(_Lval, _),
        Right = lval(_Rval, _),
        % Assignment between two lvalues - cannot happen.
        unexpected($module, $pred, "lval/lval")
    ;
        Left = lval(Lval0, LeftWidth),
        Right = ref(Var),
        % Assignment from a variable to an lvalue - cannot cache
        % so generate immediately.
        produce_variable(Var, SourceCode, Source, CI, !CLD),
        materialize_vars_in_lval(Lval0, Lval, MaterializeCode, CI, !CLD),
        (
            LeftWidth = full_word,
            AssignCode = singleton(llds_instr(assign(Lval, Source),
                "Copy value"))
        ;
            (
                LeftWidth = partial_word_first(Mask),
                Shift = 0
            ;
                LeftWidth = partial_word_shifted(Shift, Mask)
            ),
            ComplementMask = const(llconst_int(\(Mask << Shift))),
            MaskOld = binop(bitwise_and(int_type_int), lval(Lval),
                ComplementMask),
            ShiftNew = maybe_left_shift_rval(Source, Shift),
            Combined = binop(bitwise_or(int_type_int), MaskOld, ShiftNew),
            AssignCode = singleton(llds_instr(assign(Lval, Combined),
                "Update part of word"))
        ;
            LeftWidth = double_word,
            ( if field_offset_pair(Lval, LvalA, LvalB) then
                SrcA = binop(float_word_bits, Source, const(llconst_int(0))),
                SrcB = binop(float_word_bits, Source, const(llconst_int(1))),
                Comment = "Update double word",
                AssignCode = from_list([
                    llds_instr(assign(LvalA, SrcA), Comment),
                    llds_instr(assign(LvalB, SrcB), Comment)
                ])
            else
                sorry($module, $pred, "double_word: non-field lval")
            )
        ),
        Code = SourceCode ++ MaterializeCode ++ AssignCode
    ;
        Left = ref(Lvar),
        ( if variable_is_forward_live(!.CLD, Lvar) then
            (
                Right = lval(Lval, RightWidth),
                % Assignment of a value to a variable, generate now.
                (
                    RightWidth = full_word,
                    assign_lval_to_var(Lvar, Lval, Code, CI, !CLD)
                ;
                    (
                        RightWidth = partial_word_first(Mask),
                        Rval0 = lval(Lval)
                    ;
                        RightWidth = partial_word_shifted(Shift, Mask),
                        Rval0 = right_shift_rval(lval(Lval), Shift)
                    ),
                    Rval = binop(bitwise_and(int_type_int), Rval0,
                        const(llconst_int(Mask))),
                    assign_field_lval_expr_to_var(Lvar, [Lval], Rval, Code,
                        !CLD)
                ;
                    RightWidth = double_word,
                    ( if field_offset_pair(Lval, LvalA, LvalB) then
                        Rval = binop(float_from_dword,
                            lval(LvalA), lval(LvalB)),
                        assign_field_lval_expr_to_var(Lvar, [LvalA, LvalB],
                            Rval, Code, !CLD)
                    else
                        sorry($module, $pred, "double_word: non-field lval")
                    )
                )
            ;
                Right = ref(Rvar),
                % Assignment of a variable to a variable, so cache it.
                assign_var_to_var(Lvar, Rvar, !CLD),
                Code = empty
            )
        else
            Code = empty
        )
    ).

:- pred field_offset_pair(lval::in, lval::out, lval::out) is semidet.

field_offset_pair(LvalA, LvalA, LvalB) :-
    LvalA = field(Ptag, Address, const(llconst_int(Offset))),
    LvalB = field(Ptag, Address, const(llconst_int(Offset + 1))).

%---------------------------------------------------------------------------%

    % Generate a direct arg unification between
    % - the left-hand-side (the whole term), and
    % - the right-hand-side (the one argument).
    %
:- pred generate_direct_arg_construct(prog_var::in, prog_var::in, tag_bits::in,
    unify_mode::in, mer_type::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_direct_arg_construct(Var, Arg, Ptag, ArgMode, Type, Code, CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    ArgMode = unify_modes_lhs_rhs(LeftFromToInsts, RightFromToInsts),
    from_to_insts_to_top_functor_mode(ModuleInfo, LeftFromToInsts, Type,
        LeftTopFunctorMode),
    from_to_insts_to_top_functor_mode(ModuleInfo, RightFromToInsts, Type,
        RightTopFunctorMode),
    ( if
        % Input - input == test unification
        LeftTopFunctorMode = top_in,
        RightTopFunctorMode = top_in
    then
        % This shouldn't happen, since mode analysis should avoid creating
        % any tests in the arguments of a construction or deconstruction
        % unification.
        unexpected($module, $pred, "test in arg of [de]construction")
    else if
        % Input - Output == assignment ->
        LeftTopFunctorMode = top_in,
        RightTopFunctorMode = top_out
    then
        unexpected($module, $pred, "left-to-right data flow in construction")
    else if
        % Output - Input == assignment <-
        LeftTopFunctorMode = top_out,
        RightTopFunctorMode = top_in
    then
        assign_expr_to_var(Var, mkword(Ptag, var(Arg)), Code, !CLD)
    else if
        LeftTopFunctorMode = top_unused,
        RightTopFunctorMode = top_unused
    then
        % XXX I think this will have to change if we start to support aliasing.
        % Construct a tagged pointer to a pointer value which is unknown yet.
        assign_const_to_var(Var, mkword_hole(Ptag), CI, !CLD),
        Code = empty
    else
        unexpected($module, $pred, "some strange unify")
    ).

    % Generate a direct arg unification between
    % - the left-hand-side (the whole term), and
    % - the right-hand-side (the one argument).
    %
:- pred generate_direct_arg_deconstruct(prog_var::in, prog_var::in,
    tag_bits::in, unify_mode::in, mer_type::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_direct_arg_deconstruct(Var, ArgVar, Ptag, ArgMode, Type, Code,
        CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    ArgMode = unify_modes_lhs_rhs(LeftFromToInsts, RightFromToInsts),
    from_to_insts_to_top_functor_mode(ModuleInfo, LeftFromToInsts, Type,
        LeftTopFunctorMode),
    from_to_insts_to_top_functor_mode(ModuleInfo, RightFromToInsts, Type,
        RightTopFunctorMode),
    ( if
        % Input - Output == assignment ->
        LeftTopFunctorMode = top_in,
        RightTopFunctorMode = top_out
    then
        ( if variable_is_forward_live(!.CLD, ArgVar) then
            BodyRval = binop(body, var(Var), const(llconst_int(Ptag))),
            assign_expr_to_var(ArgVar, BodyRval, Code, !CLD)
        else
            Code = empty
        )
    else if
        % Output - Input == assignment <-
        LeftTopFunctorMode = top_out,
        RightTopFunctorMode = top_in
    then
        reassign_mkword_hole_var(Var, Ptag, var(ArgVar), Code, !CLD)
    else if
        LeftTopFunctorMode = top_unused,
        RightTopFunctorMode = top_unused
    then
        Code = empty
        % free-free - ignore
        % XXX I think this will have to change if we start to support aliasing.
    else if
        % Input - input == test unification
        LeftTopFunctorMode = top_in,
        RightTopFunctorMode = top_in
    then
        % This shouldn't happen, since mode analysis should avoid creating
        % any tests in the arguments of a construction or deconstruction
        % unification.
        unexpected($module, $pred, "test in arg of [de]construction")
    else
        unexpected($module, $pred, "some strange unify")
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
    get_cons_arg_widths(ModuleInfo, ConsId, ConstArgs, ConsArgWidths),
    generate_const_struct_rval(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        !.ConstStructMap, ConsId, ConsTag, ConstArgs, ConsArgWidths, Rval,
        !StaticCellInfo),
    map.det_insert(ConstNum, Rval, !ConstStructMap).

:- pred generate_const_struct_rval(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in, cons_id::in, cons_tag::in,
    list(const_struct_arg)::in, list(arg_width)::in, typed_rval::out,
    static_cell_info::in, static_cell_info::out) is det.

generate_const_struct_rval(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConsId, ConsTag, ConstArgs, ConsArgWidths, TypedRval,
        !StaticCellInfo) :-
    (
        ConsTag = shared_with_reserved_addresses_tag(_, ActualConsTag),
        generate_const_struct_rval(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConsId, ActualConsTag, ConstArgs, ConsArgWidths,
            TypedRval, !StaticCellInfo)
    ;
        ConsTag = no_tag,
        (
            ConstArgs = [ConstArg],
            det_single_arg_width(ConsArgWidths, ConsArgWidth),
            generate_const_struct_arg(ModuleInfo, UnboxedFloats,
                UnboxedInt64s, ConstStructMap, ConstArg, ConsArgWidth,
                ArgTypedRval),
            TypedRval = ArgTypedRval
        ;
            ( ConstArgs = []
            ; ConstArgs = [_, _ | _]
            ),
            unexpected($module, $pred, "no_tag arity != 1")
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        (
            ConstArgs = [ConstArg],
            det_single_arg_width(ConsArgWidths, ConsArgWidth),
            generate_const_struct_arg(ModuleInfo, UnboxedFloats,
                UnboxedInt64s, ConstStructMap, ConstArg, ConsArgWidth,
                ArgTypedRval),
            ArgTypedRval = typed_rval(ArgRval, _RvalType),
            Rval = mkword(Ptag, ArgRval),
            TypedRval = typed_rval(Rval, lt_data_ptr)
        ;
            ( ConstArgs = []
            ; ConstArgs = [_, _ | _]
            ),
            unexpected($module, $pred, "direct_arg_tag: arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            Ptag = 0
        ;
            ConsTag = unshared_tag(Ptag)
        ),
        generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgs, ConsArgWidths, ArgTypedRvals),
        pack_ground_term_args(ConsArgWidths, ArgTypedRvals, PackArgTypedRvals),
        add_scalar_static_cell(PackArgTypedRvals, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        Rval = mkword(Ptag, CellPtrConst),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ConsTag = shared_remote_tag(Ptag, Stag),
        generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgs, ConsArgWidths, ArgTypedRvals),
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
        ; ConsTag = reserved_address_tag(_)
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
        unexpected($module, $pred, "unexpected tag")
    ).

:- pred generate_const_struct_args(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in,
    list(const_struct_arg)::in, list(arg_width)::in, list(typed_rval)::out)
    is det.

generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConstArgs, ArgWidths, TypedRvals) :-
    list.map_corresponding(
        generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap),
        ConstArgs, ArgWidths, TypedRvals).

:- pred generate_const_struct_arg(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in, const_struct_arg::in,
    arg_width::in, typed_rval::out) is det.

generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConstArg, ArgWidth, TypedRval) :-
    (
        ConstArg = csa_const_struct(ConstNum),
        map.lookup(ConstStructMap, ConstNum, TypedRval)
    ;
        ConstArg = csa_constant(ConsId, _),
        ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
        generate_const_struct_arg_tag(ModuleInfo, UnboxedFloats,
            UnboxedInt64s, ConstStructMap, ConsTag, ArgWidth, TypedRval)
    ).

:- pred generate_const_struct_arg_tag(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in, cons_tag::in,
    arg_width::in, typed_rval::out) is det.

generate_const_struct_arg_tag(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConsTag, ArgWidth, TypedRval) :-
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
                ; IntType = int_type_uint
                ; IntType = int_type_int8
                ; IntType = int_type_uint8
                ; IntType = int_type_int16
                ; IntType = int_type_uint16
                ; IntType = int_type_int32
                ; IntType = int_type_uint32
                ),
                Type = lt_int(IntType)
            )
        ;
            ConsTag = foreign_tag(Lang, Val),
            expect(unify(Lang, lang_c), $module, $pred,
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
        ConsTag = reserved_address_tag(RA),
        Rval = generate_reserved_address(RA),
        rval_type(Rval, Type),
        TypedRval = typed_rval(Rval, Type)
    ;
        ConsTag = shared_with_reserved_addresses_tag(_, ActualConsTag),
        generate_const_struct_arg_tag(ModuleInfo, UnboxedFloats,
            UnboxedInt64s, ConstStructMap, ActualConsTag, ArgWidth, TypedRval)
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
        unexpected($module, $pred, "unexpected tag")
    ).

:- pred det_single_arg_width(list(arg_width)::in, arg_width::out) is det.

det_single_arg_width(ArgWidths, ArgWidth) :-
    (
        ArgWidths = [ArgWidth]
    ;
        ( ArgWidths = []
        ; ArgWidths = [_, _ | _]
        ),
        unexpected($module, $pred, "unexpected arg_width list")
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
                    unexpected($module, $pred, "no active pairs")
                )
            else
                unexpected($module, $pred, "malformed goal")
            )
        else
            unexpected($module, $pred, "unexpected nonlocal")
        )
    ;
        NonLocalList = [_, _ | _],
        unexpected($module, $pred, "unexpected nonlocals")
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
        Unify = construct(Var, ConsId, Args, _, _, _, SubInfo),
        SubInfo = no_construct_sub_info
    then
        ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
        get_cons_arg_widths(ModuleInfo, ConsId, Args, ConsArgWidths),
        generate_ground_term_conjunct_tag(Var, ConsTag, Args, ConsArgWidths,
            UnboxedFloats, !StaticCellInfo, !ActiveMap)
    else
        unexpected($module, $pred, "malformed goal")
    ).

:- pred generate_ground_term_conjunct_tag(prog_var::in, cons_tag::in,
    list(prog_var)::in, list(arg_width)::in, have_unboxed_floats::in,
    static_cell_info::in, static_cell_info::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_conjunct_tag(Var, ConsTag, Args, ConsArgWidths,
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
            expect(unify(Lang, lang_c), $module, $pred,
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
        ConsTag = reserved_address_tag(RA),
        Rval = generate_reserved_address(RA),
        rval_type(Rval, RvalType),
        ActiveGroundTerm = typed_rval(Rval, RvalType),
        map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = shared_with_reserved_addresses_tag(_, ActualConsTag),
        generate_ground_term_conjunct_tag(Var, ActualConsTag, Args,
            ConsArgWidths, UnboxedFloats, !StaticCellInfo, !ActiveMap)
    ;
        ConsTag = no_tag,
        (
            Args = [],
            unexpected($module, $pred, "no_tag arity != 1")
        ;
            Args = [Arg],
            map.det_remove(Arg, RvalType, !ActiveMap),
            map.det_insert(Var, RvalType, !ActiveMap)
        ;
            Args = [_, _ | _],
            unexpected($module, $pred, "no_tag arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            Ptag = 0
        ;
            ConsTag = unshared_tag(Ptag)
        ),
        generate_ground_term_args(Args, ConsArgWidths, ArgTypedRvals,
            !ActiveMap),
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
            Args = [Arg],
            map.det_remove(Arg, typed_rval(ArgRval, _RvalType), !ActiveMap),
            Rval = mkword(Ptag, ArgRval),
            ActiveGroundTerm = typed_rval(Rval, lt_data_ptr),
            map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
        ;
            ( Args = []
            ; Args = [_, _ | _]
            ),
            unexpected($module, $pred, "direct_arg_tag: arity != 1")
        )
    ;
        ConsTag = shared_remote_tag(Ptag, Stag),
        generate_ground_term_args(Args, ConsArgWidths, ArgTypedRvals,
            !ActiveMap),
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
        unexpected($module, $pred, "unexpected tag")
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

:- pred generate_ground_term_args(list(prog_var)::in, list(arg_width)::in,
    list(typed_rval)::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_args(Vars, ConsArgWidths, TypedRvals, !ActiveMap) :-
    list.map_corresponding_foldl(generate_ground_term_arg, Vars, ConsArgWidths,
        TypedRvals, !ActiveMap).

:- pred generate_ground_term_arg(prog_var::in, arg_width::in,
    typed_rval::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_arg(Var, ConsArgWidth, TypedRval, !ActiveMap) :-
    map.det_remove(Var, TypedRval0, !ActiveMap),
    % Though a standalone float might have needed to boxed, it may be stored in
    % unboxed form as a constructor argument.
    ( if
        ConsArgWidth = double_word,
        TypedRval0 = typed_rval(Rval, lt_data_ptr)
    then
        TypedRval = typed_rval(Rval, lt_float)
    else
        TypedRval = TypedRval0
    ).

:- pred pack_ground_term_args(list(arg_width)::in,
    list(typed_rval)::in, list(typed_rval)::out) is det.

pack_ground_term_args(Widths, !TypedRvals) :-
    pack_args(shift_combine_rval_type, Widths, !TypedRvals, unit, _, unit, _).

%-----------------------------------------------------------------------------%

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
                unexpected($module, $pred, "non-var or int argument")
            )
        ;
            CellArgA = cell_arg_double_word(RvalA),
            expect(unify(Shift, 0), $module, $pred,
                "double word rval cannot be shifted"),
            ( if RvalA = var(Var) then
                produce_variable(Var, VarCode, VarRval, CI, !CLD),
                ShiftCellArgA = cell_arg_double_word(VarRval),
                !:Code = !.Code ++ VarCode
            else if RvalA = const(llconst_float(_)) then
                ShiftCellArgA = CellArgA
            else
                unexpected($module, $pred, "non-var or float argument")
            )
        ;
            CellArgA = cell_arg_skip,
            ShiftCellArgA = cell_arg_skip
        ;
            CellArgA = cell_arg_take_addr(_, _),
            unexpected($module, $pred, "cell_arg_take_addr")
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
            unexpected($module, $pred, "mismatched llds_types")
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
    ( if bitwise_or_cell_arg(CellArgA, CellArgB, CellArgPrime) then
        CellArg = CellArgPrime
    else
        unexpected($module, $pred, "invalid combination")
    ).

:- pred bitwise_or_cell_arg(cell_arg::in, cell_arg::in, cell_arg::out)
    is semidet.

bitwise_or_cell_arg(CellArgA, CellArgB, CellArg) :-
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

combine_completeness(complete, complete) = complete.
combine_completeness(incomplete, complete) = incomplete.
combine_completeness(complete, incomplete) = incomplete.
combine_completeness(incomplete, incomplete) = incomplete.

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
