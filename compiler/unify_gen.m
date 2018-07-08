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
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

    % XXX ARG_PACK: We always know which of these two we pass to predicates
    % inside field_and_arg_vars. Specialize the modes of predicates,
    % to avoid runtime tests.
    % Note that uv_var is for implementing no_tag types, and thus
    % we won't need predicates that take a *list* of uv_vars.
:- type uni_val
    --->    uv_var(prog_var)
    ;       uv_field(uni_field).

:- inst uni_val_var for uni_val/0
    --->    uv_var(ground).
:- inst uni_val_field for uni_val/0
    --->    uv_field(ground).

:- type uni_field
    --->    uni_field(ptag, rval, int, arg_pos_width).
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

:- inst var_and_arg_var for field_and_arg_var/0
    --->    field_and_arg_var(uni_val_var, ground, ground).
:- inst field_and_arg_var for field_and_arg_var/0
    --->    field_and_arg_var(uni_val_field, ground, ground).

:- type field_addr
    --->    field_addr(
                fa_offset   :: int,
                fa_var      :: prog_var
            ).

%---------------------------------------------------------------------------%

generate_unification(CodeModel, Uni, GoalInfo, Code, !CI, !CLD) :-
    (
        Uni = assign(LHSVar, RHSVar),
        (
            CodeModel = model_det
        ;
            ( CodeModel = model_semi
            ; CodeModel = model_non
            ),
            unexpected($pred, "assign is not model_det")
        ),
        ( if variable_is_forward_live(!.CLD, LHSVar) then
            generate_assignment(LHSVar, RHSVar, Code, !CLD)
        else
            Code = empty
        )
    ;
        Uni = construct(LHSVar, ConsId, RHSVars, ArgModes, HowToConstruct, _,
            SubInfo),
        (
            CodeModel = model_det
        ;
            ( CodeModel = model_semi
            ; CodeModel = model_non
            ),
            unexpected($pred, "construct is not model_det")
        ),
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
        ;
            CodeModel = model_non,
            unexpected($pred, "deconstruct is model_non")
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
            CodeModel = model_semi,
            generate_test(VarA, VarB, Code, !CI, !CLD)
        ;
            ( CodeModel = model_det
            ; CodeModel = model_non
            ),
            unexpected($pred, "simple_test is not model_semi")
        )
    ;
        % These should have been transformed into calls to unification
        % procedures by polymorphism.m.
        Uni = complicated_unify(_Mode, _CanFail, _TypeInfoVars),
        unexpected($pred, "complicated unify")
    ).

:- pred get_cons_arg_widths(module_info::in, cons_id::in,
    list(T)::in, assoc_list(T, arg_pos_width)::out) is det.

get_cons_arg_widths(ModuleInfo, ConsId, AllArgs, AllArgsPosWidths) :-
    ( if get_cons_repn_defn(ModuleInfo, ConsId, ConsRepnDefn) then
        ConsArgRepns = ConsRepnDefn ^ cr_args,
        ConsTag = ConsRepnDefn ^ cr_tag,
        ArgPosWidths = list.map((func(C) = C ^ car_pos_width), ConsArgRepns),
        list.length(AllArgs, NumAllArgs),
        list.length(ConsArgRepns, NumConsArgs),
        NumExtraArgs = NumAllArgs - NumConsArgs,
        ( if NumExtraArgs = 0 then
            assoc_list.from_corresponding_lists(AllArgs, ArgPosWidths,
                AllArgsPosWidths)
        else if NumExtraArgs > 0 then
            list.det_split_list(NumExtraArgs, AllArgs, ExtraArgs, ConsArgs),
            ( if ConsTag = shared_remote_tag(_, RemoteSecTag) then
                RemoteSecTag = remote_sectag(_, AddedBy),
                expect(unify(AddedBy, sectag_added_by_unify), $pred,
                    "AddedBy != sectag_added_by_unify"),
                InitOffset = 1
            else
                InitOffset = 0
            ),
            allocate_consecutive_full_words(InitOffset,
                ExtraArgs, ExtraArgsPosWidths),
            assoc_list.from_corresponding_lists(ConsArgs, ArgPosWidths,
                ConsArgsPosWidths),
            AllArgsPosWidths = ExtraArgsPosWidths ++ ConsArgsPosWidths
        else
            unexpected($pred, "too few arguments")
        )
    else
        allocate_consecutive_full_words(0, AllArgs, AllArgsPosWidths)
    ).

    % The initial offset that our callers should specify
    % depends on the absence/presence of a secondary tag.
    %
:- pred allocate_consecutive_full_words(int::in,
    list(T)::in, assoc_list(T, arg_pos_width)::out) is det.

allocate_consecutive_full_words(_, [], []).
allocate_consecutive_full_words(CurOffset,
        [Arg | Args], [ArgPosWidth | ArgsPosWidths]) :-
    PosWidth = apw_full(arg_only_offset(CurOffset), cell_offset(CurOffset)),
    ArgPosWidth = Arg - PosWidth,
    allocate_consecutive_full_words(CurOffset + 1, Args, ArgsPosWidths).

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
        % Mode analysis reports free-free unifications as assignments
        % to a dead variable. For such unifications, we of course
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
        raw_tag_test(!.CI, VarRval, MainConsTag, MainTagTestRval),
        list.map(raw_tag_test(!.CI, VarRval),
            OtherConsTags, OtherTagTestRvals),
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
        raw_tag_test(!.CI, VarRval, CheapConsTag, NegTestRval),
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
        raw_tag_test(!.CI, VarRval, ConsTag, TestRval)
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

:- pred raw_tag_test(code_info::in, rval::in, cons_tag::in, rval::out) is det.

raw_tag_test(CI, Rval, ConsTag, TestRval) :-
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
        ( ConsTag = dummy_tag
        ; ConsTag = no_tag
        ; ConsTag = single_functor_tag
        ),
        % In a type with only one value, all equality tests succeed.
        % In a type with only one ptag value, all equality tests on ptags
        % succeed.
        TestRval = const(llconst_true)
    ;
        ( ConsTag = unshared_tag(Ptag)
        ; ConsTag = direct_arg_tag(Ptag)
        ),
        VarPtag = unop(tag, Rval),
        Ptag = ptag(PtagUint8),
        ConstPtag = const(llconst_int(uint8.cast_to_int(PtagUint8))),
        TestRval = binop(eq(int_type_int), VarPtag, ConstPtag)
    ;
        ConsTag = shared_remote_tag(Ptag, RemoteSectag),
        VarPtag = unop(tag, Rval),
        Ptag = ptag(PtagUint8),
        ConstPtag = const(llconst_int(uint8.cast_to_int(PtagUint8))),
        PtagTestRval = binop(eq(int_type_int), VarPtag, ConstPtag),
        VarStag = lval(field(yes(Ptag), Rval, const(llconst_int(0)))),
        RemoteSectag = remote_sectag(SecTagUint, _),
        ConstStag = const(llconst_int(uint.cast_to_int(SecTagUint))),
        StagTestRval = binop(eq(int_type_int), VarStag, ConstStag),
        TestRval = binop(logical_and, PtagTestRval, StagTestRval)
    ;
        ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, MustMask),
        LocalSectag = local_sectag(_Sectag, PrimSec, SectagBits),
        ConstPrimSec = const(llconst_int(uint.cast_to_int(PrimSec))),
        (
            MustMask = lsectag_always_rest_of_word,
            TestRval = binop(eq(int_type_int), Rval, ConstPrimSec)
        ;
            MustMask = lsectag_must_be_masked,
            % We generate the same test as for shared_local_tag_with_args.
            code_info.get_num_ptag_bits(CI, NumPtagBits),
            SectagBits = sectag_bits(NumSectagBits, _SectagMask),
            NumPtagSectagBits = uint8.cast_to_int(NumPtagBits + NumSectagBits),
            PrimSecMask = (1u << NumPtagSectagBits) - 1u,
            MaskedRval = binop(bitwise_and(int_type_uint),
                Rval, const(llconst_uint(PrimSecMask))),
            TestRval = binop(eq(int_type_int), MaskedRval, ConstPrimSec)
        )
    ;
        ConsTag = shared_local_tag_with_args(_Ptag, LocalSectag),
        % We generate the same test as for shared_local_tag_no_args
        % with lsectag_must_be_masked.
        LocalSectag = local_sectag(_Sectag, PrimSec, SectagBits),
        ConstPrimSec = const(llconst_int(uint.cast_to_int(PrimSec))),
        code_info.get_num_ptag_bits(CI, NumPtagBits),
        SectagBits = sectag_bits(NumSectagBits, _SectagMask),
        NumPtagSectagBits = uint8.cast_to_int(NumPtagBits + NumSectagBits),
        PrimSecMask = (1u << NumPtagSectagBits) - 1u,
        MaskedRval = binop(bitwise_and(int_type_uint),
            Rval, const(llconst_uint(PrimSecMask))),
        TestRval = binop(eq(int_type_int), MaskedRval, ConstPrimSec)
    ;
        ( ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "unexpected ConsTag")
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
    assoc_list(prog_var, arg_pos_width)::in, list(unify_mode)::in,
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
        ConsTag = dummy_tag,
        % XXX The assignment is likely to be dead code, but *proving*
        % that the assigned-to variable is never used is difficult.
        assign_const_to_var(LHSVar, const(llconst_int(0)), !.CI, !CLD),
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
                % Assignments can have information flow to the left
                % as well as to the right.
                generate_deconstruct_unify_arg(FieldAndArgVar, ArgMode, Code,
                    !.CI, !CLD)
            ;
                TakeAddr = [_ | _],
                unexpected($pred, "notag: take_addr")
            )
        else
            unexpected($pred, "no_tag: arity != 1")
        )
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
        (
            ConsTag = single_functor_tag,
            % Treat single_functor the same as unshared_tag(0).
            Ptag = ptag(0u8)
        ;
            ConsTag = unshared_tag(Ptag)
        ;
            ConsTag = shared_remote_tag(Ptag, _)
        ),
        get_may_use_atomic_alloc(!.CI, MayUseAtomic0),
        FirstArgNum = 1,
        generate_and_pack_construct_args(RHSVarsWidths, ArgModes,
            FirstArgNum, TakeAddr, CellArgs0, MayUseAtomic0, MayUseAtomic,
            empty, PackCode, !.CI, !CLD),
        pack_how_to_construct(RHSVarsWidths, HowToConstruct0, HowToConstruct),
        (
            ( ConsTag = single_functor_tag
            ; ConsTag = unshared_tag(_Ptag)
            ),
            CellArgs = CellArgs0
        ;
            ConsTag = shared_remote_tag(_Ptag, RemoteSectag),
            RemoteSectag = remote_sectag(SecTagUint, _),
            Sectag = uint.cast_to_int(SecTagUint),
            TagArg = cell_arg_full_word(const(llconst_int(Sectag)), complete),
            CellArgs = [TagArg | CellArgs0]
        ),
        Context = goal_info_get_context(GoalInfo),
        construct_cell(LHSVar, Ptag, CellArgs, HowToConstruct, MaybeSize,
            Context, MayUseAtomic, ConstructCode, !CI, !CLD),
        Code = PackCode ++ ConstructCode
    ;
        ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, _MustMask),
        LocalSectag = local_sectag(_, PrimSec, _),
        assign_const_to_var(LHSVar,
            const(llconst_int(uint.cast_to_int(PrimSec))),
            !.CI, !CLD),
        Code = empty
    ;
        ConsTag = shared_local_tag_with_args(_Ptag, LocalSectag),
        LocalSectag = local_sectag(_, PrimSec, _),
        RevToOrRvals0 = [const(llconst_uint(PrimSec))],
        generate_and_pack_tagword(RHSVarsWidths, ArgModes,
            RevToOrRvals0, RevToOrRvals, !.CI),
        expect(unify(TakeAddr, []), $pred,
            "shared_local_tag_with_args, TakeAddr != []"),
        list.reverse(RevToOrRvals, ToOrRvals),
        or_packed_rvals(ToOrRvals, PackedRval),
        assign_expr_to_var(LHSVar, PackedRval, Code, !CLD)
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
        assign_const_to_var(LHSVar, Rval, !.CI, !CLD),
        Code = empty
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

    % Create a list of cell_args for the argument words or double words
    % for a construction unification, while packing sub-word arguments
    % into words.
    %
:- pred generate_and_pack_construct_args(
    assoc_list(prog_var, arg_pos_width)::in, list(unify_mode)::in,
    int::in, list(int)::in, list(cell_arg)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out,
    llds_code::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_and_pack_construct_args([], [], _, !.TakeAddr, [],
        !MayUseAtomic, !Code, _, !CLD) :-
    expect(unify(!.TakeAddr, []), $pred, "TakeAddr != [] at end").
generate_and_pack_construct_args([], [_ | _], _, _, _,
        !MayUseAtomic, !Code, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_and_pack_construct_args([_ | _], [], _, _, _,
        !MayUseAtomic, !Code, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_and_pack_construct_args([VarWidth | VarsWidths], [ArgMode | ArgModes],
        !.CurArgNum, !.TakeAddr, CellArgs,
        !MayUseAtomic, !Code, CI, !CLD) :-
    VarWidth = Var - Width,
    (
        ( Width = apw_full(_, _)
        ; Width = apw_double(_, _, _)
        ; Width = apw_partial_first(_, _, _, _, _)
        ; Width = apw_partial_shifted(_, _, _, _, _, _)
        ; Width = apw_none_shifted(_, _)
        ),
        generate_and_pack_construct_word(Var, Width, VarsWidths,
            ArgMode, ArgModes, LeftOverVarsWidths, LeftOverArgModes,
            !CurArgNum, !TakeAddr, HeadCellArg, !MayUseAtomic, !Code,
            CI, !CLD),
        generate_and_pack_construct_args(LeftOverVarsWidths, LeftOverArgModes,
            !.CurArgNum, !.TakeAddr,
            TailCellArgs, !MayUseAtomic, !Code, CI, !CLD),
        CellArgs = [HeadCellArg | TailCellArgs]
    ;
        Width = apw_none_nowhere,
        ( if !.TakeAddr = [!.CurArgNum | _] then
            unexpected($pred, "taking address of dummy")
        else
            !:CurArgNum = !.CurArgNum + 1,
            generate_and_pack_construct_args(VarsWidths, ArgModes,
                !.CurArgNum, !.TakeAddr, CellArgs,
                !MayUseAtomic, !Code, CI, !CLD)
        )
    ).

:- inst not_nowhere for arg_pos_width/0
    --->    apw_full(ground, ground)
    ;       apw_double(ground, ground, ground)
    ;       apw_partial_first(ground, ground, ground, ground, ground)
    ;       apw_partial_shifted(ground, ground, ground, ground, ground, ground)
    ;       apw_none_shifted(ground, ground).

:- pred generate_and_pack_construct_word(
    prog_var::in, arg_pos_width::in(not_nowhere),
    assoc_list(prog_var, arg_pos_width)::in,
    unify_mode::in, list(unify_mode)::in,
    assoc_list(prog_var, arg_pos_width)::out, list(unify_mode)::out,
    int::in, int::out, list(int)::in, list(int)::out, cell_arg::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out,
    llds_code::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_and_pack_construct_word(Var, Width, VarsWidths, ArgMode, ArgModes,
        LeftOverVarsWidths, LeftOverArgModes, CurArgNum, LeftOverArgNum,
        !TakeAddr, CellArg, !MayUseAtomic, !Code, CI, !CLD) :-
    get_vartypes(CI, VarTypes),
    lookup_var_type(VarTypes, Var, Type),
    get_module_info(CI, ModuleInfo),
    % XXX Should we update !MayUseAtomic for dummy types?
    update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic),
    ( if !.TakeAddr = [CurArgNum | !:TakeAddr] then
        LeftOverVarsWidths = VarsWidths,
        LeftOverArgModes = ArgModes,
        LeftOverArgNum = CurArgNum + 1,
        get_lcmc_null(CI, LCMCNull),
        (
            Width = apw_full(_, _),
            (
                LCMCNull = no,
                MaybeNull = no
            ;
                LCMCNull = yes,
                MaybeNull = yes(const(llconst_int(0)))
            ),
            CellArg = cell_arg_take_addr_one_word(Var, MaybeNull)
        ;
            Width = apw_double(_, _, _),
            (
                LCMCNull = no,
                MaybeNulls = no
            ;
                LCMCNull = yes,
                Null = const(llconst_int(0)),
                MaybeNulls = yes({Null, Null})
            ),
            CellArg = cell_arg_take_addr_two_words(Var, MaybeNulls)
        ;
            ( Width = apw_partial_first(_, _, _, _, _)
            ; Width = apw_partial_shifted(_, _, _, _, _, _)
            ),
            unexpected($pred, "taking address of partial word")
        ;
            Width = apw_none_shifted(_, _),
            % Even if the variable in this field is produced *after*
            % the recursive call, we know in advance what its value
            % will be, so we should be able to fill in this field
            % *before* the recursive call.
            unexpected($pred, "taking address of dummy")
        ),
        !:MayUseAtomic = may_not_use_atomic_alloc
    else
        generate_construct_arg_rval(ModuleInfo, Var, Type, ArgMode,
            IsReal, Rval, !Code, CI, !CLD),
        (
            (
                Width = apw_full(_, _),
                (
                    IsReal = not_real_input_arg,
                    CellArg = cell_arg_skip_one_word
                ;
                    IsReal = real_input_arg,
                    CellArg = cell_arg_full_word(Rval, complete)
                )
            ;
                Width = apw_double(_, _, _),
                (
                    IsReal = not_real_input_arg,
                    CellArg = cell_arg_skip_two_words
                ;
                    IsReal = real_input_arg,
                    CellArg = cell_arg_double_word(Rval)
                )
            ),
            LeftOverVarsWidths = VarsWidths,
            LeftOverArgModes = ArgModes,
            LeftOverArgNum = CurArgNum + 1
        ;
            Width = apw_partial_first(_, _, _, _, Fill),
            (
                IsReal = not_real_input_arg,
                Completeness0 = incomplete,
                RevToOrRvals0 = []
            ;
                IsReal = real_input_arg,
                Completeness0 = complete,
                cast_away_any_sign_extend_bits(Fill, Rval, CastRval),
                RevToOrRvals0 = [CastRval]
            ),
            NextArgNum = CurArgNum + 1,
            generate_and_pack_one_cons_word(VarsWidths, ArgModes,
                LeftOverVarsWidths, LeftOverArgModes,
                NextArgNum, LeftOverArgNum, !TakeAddr,
                RevToOrRvals0, RevToOrRvals, Completeness0, Completeness,
                !MayUseAtomic, !Code, CI, !CLD),
            list.reverse(RevToOrRvals, ToOrRvals),
            or_packed_rvals(ToOrRvals, PackedRval),
            CellArg = cell_arg_full_word(PackedRval, Completeness)
        ;
            Width = apw_partial_shifted(_, _, _, _, _, _),
            unexpected($pred, "apw_partial_shifted")
        ;
            Width = apw_none_shifted(_, _),
            unexpected($pred, "apw_none_shifted")
        )
    ).

:- pred generate_and_pack_one_cons_word(
    assoc_list(prog_var, arg_pos_width)::in, list(unify_mode)::in,
    assoc_list(prog_var, arg_pos_width)::out, list(unify_mode)::out,
    int::in, int::out, list(int)::in, list(int)::out,
    list(rval)::in, list(rval)::out, completeness::in, completeness::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out,
    llds_code::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_and_pack_one_cons_word([], [], [], [], CurArgNum, LeftOverArgNum,
        !TakeAddr, !RevToOrRvals, !Completeness, !MayUseAtomic,
        !Code, _, !CLD) :-
    LeftOverArgNum = CurArgNum.
generate_and_pack_one_cons_word([], [_ | _], _, _, _, _,
        !TakeAddr, !RevToOrRvals, !Completeness, !MayUseAtomic,
        !Code, _, !CLD) :-
    unexpected($pred, "length misnatch").
generate_and_pack_one_cons_word([_ | _], [], _, _, _, _,
        !TakeAddr, !RevToOrRvals, !Completeness, !MayUseAtomic,
        !Code, _, !CLD) :-
    unexpected($pred, "length misnatch").
generate_and_pack_one_cons_word([VarWidth | VarsWidths], [ArgMode | ArgModes],
        LeftOverVarsWidths, LeftOverArgModes, CurArgNum, LeftOverArgNum,
        !TakeAddr, !RevToOrRvals, !Completeness, !MayUseAtomic,
        !Code, CI, !CLD) :-
    VarWidth = Var - Width,
    (
        ( Width = apw_full(_, _)
        ; Width = apw_double(_, _, _)
        ; Width = apw_none_nowhere
        ; Width = apw_partial_first(_, _, _, _, _)
        ),
        % This argument is not part of this word.
        LeftOverVarsWidths = [VarWidth | VarsWidths],
        LeftOverArgModes = [ArgMode | ArgModes],
        LeftOverArgNum = CurArgNum
    ;
        ( Width = apw_partial_shifted(_, _, _, _, _, _)
        ; Width = apw_none_shifted(_, _)
        ),
        % This argument *is* part of this word.
        get_vartypes(CI, VarTypes),
        lookup_var_type(VarTypes, Var, Type),
        get_module_info(CI, ModuleInfo),
        update_type_may_use_atomic_alloc(ModuleInfo, Type, !MayUseAtomic),
        ( if !.TakeAddr = [CurArgNum | !:TakeAddr] then
            unexpected($pred, "taking address of partial word")
        else
            true
        ),
        generate_construct_arg_rval(ModuleInfo, Var, Type, ArgMode,
            IsReal, ArgRval, !Code, CI, !CLD),
        (
            Width = apw_partial_shifted(_, _, Shift, _, _, Fill),
            (
                IsReal = not_real_input_arg,
                !:Completeness = incomplete
            ;
                IsReal = real_input_arg,
                ShiftedArgRval = left_shift_rval(ArgRval, Shift, Fill),
                !:RevToOrRvals = [ShiftedArgRval | !.RevToOrRvals]
            )
        ;
            Width = apw_none_shifted(_, _)
            % We change neither !Completeness nor !RevToOrRvals.
        ),
        NextArgNum = CurArgNum + 1,
        generate_and_pack_one_cons_word(VarsWidths, ArgModes,
            LeftOverVarsWidths, LeftOverArgModes, NextArgNum, LeftOverArgNum,
            !TakeAddr, !RevToOrRvals, !Completeness, !MayUseAtomic,
            !Code, CI, !CLD)
    ).

:- pred generate_and_pack_tagword(
    assoc_list(prog_var, arg_pos_width)::in, list(unify_mode)::in,
    list(rval)::in, list(rval)::out, code_info::in) is det.

generate_and_pack_tagword([], [], !RevToOrRvals, _).
generate_and_pack_tagword([], [_ | _], !RevToOrRvals, _) :-
    unexpected($pred, "length misnatch").
generate_and_pack_tagword([_ | _], [], !RevToOrRvals, _) :-
    unexpected($pred, "length misnatch").
generate_and_pack_tagword([VarWidth | VarsWidths], [ArgMode | ArgModes],
        !RevToOrRvals, CI) :-
    VarWidth = Var - Width,
    (
        ( Width = apw_full(_, _)
        ; Width = apw_double(_, _, _)
        ; Width = apw_none_nowhere
        ; Width = apw_partial_first(_, _, _, _, _)
        ),
        unexpected($pred, "Width is not a packed arg_pos_width")
    ;
        Width = apw_partial_shifted(_, _, Shift, _, _, Fill),
        ArgMode = unify_modes_lhs_rhs(_LHSInsts, RHSInsts),
        get_module_info(CI, ModuleInfo),
        get_vartypes(CI, VarTypes),
        lookup_var_type(VarTypes, Var, Type),
        from_to_insts_to_top_functor_mode(ModuleInfo, RHSInsts, Type,
            RHSTopFunctorMode),
        (
            RHSTopFunctorMode = top_in,
            IsDummy = variable_is_of_dummy_type(CI, Var),
            (
                IsDummy = is_dummy_type,
                ArgRval = const(llconst_int(0))     % Dummy.
            ;
                IsDummy = is_not_dummy_type,
                ArgRval = var(Var)
            )
        ;
            ( RHSTopFunctorMode = top_out
            ; RHSTopFunctorMode = top_unused
            ),
            ArgRval = const(llconst_int(0))         % Dummy.
        ),
        ShiftedArgRval = left_shift_rval(ArgRval, Shift, Fill),
        !:RevToOrRvals = [ShiftedArgRval | !.RevToOrRvals]
    ;
        Width = apw_none_shifted(_, _)
    ),
    generate_and_pack_tagword(VarsWidths, ArgModes, !RevToOrRvals, CI).

:- type maybe_real_input_arg
    --->    not_real_input_arg
            % The argument is either input to the construction unification
            % but of a dummy type, or it is not an input to the construction.
            % The rval next to this is a dummy.
    ;       real_input_arg.
            % The argument is an input to the construction unification
            % and its type is not a dummy type. The rval next to this is real.
            % (The reason why we don't store the rval as an argument of
            % real_input_arg, making this type a synonym for the maybe type,
            % is to avoid the memory allocation that this would require;
            % construction unifications are one of the most frequent types
            % of goals.)

:- pred generate_construct_arg_rval(module_info::in, prog_var::in,
    mer_type::in, unify_mode::in, maybe_real_input_arg::out, rval::out,
    llds_code::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_construct_arg_rval(ModuleInfo, Var, Type, ArgMode, IsReal, Rval,
        !Code, CI, !CLD) :-
    ArgMode = unify_modes_lhs_rhs(_LHSInsts, RHSInsts),
    from_to_insts_to_top_functor_mode(ModuleInfo, RHSInsts, Type,
        RHSTopFunctorMode),
    (
        RHSTopFunctorMode = top_in,
        IsDummy = variable_is_of_dummy_type(CI, Var),
        (
            IsDummy = is_dummy_type,
            IsReal = not_real_input_arg,
            Rval = const(llconst_int(0))    % Dummy.
        ;
            IsDummy = is_not_dummy_type,
            IsReal = real_input_arg,
            produce_variable(Var, VarCode, Rval, CI, !CLD),
            !:Code = !.Code ++ VarCode
        )
    ;
        ( RHSTopFunctorMode = top_out
        ; RHSTopFunctorMode = top_unused
        ),
        IsReal = not_real_input_arg,
        Rval = const(llconst_int(0))    % Dummy.
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
        ; CellArg = cell_arg_skip_one_word
        ),
        NextOffset = ArgOffset + 1
    ;
        ( CellArg = cell_arg_double_word(_)
        ; CellArg = cell_arg_skip_two_words
        ),
        NextOffset = ArgOffset + 2
    ;
        (
            CellArg = cell_arg_take_addr_one_word(Var, _),
            NextOffset = ArgOffset + 1
        ;
            CellArg = cell_arg_take_addr_two_words(Var, _),
            NextOffset = ArgOffset + 2
        ),
        FieldAddr = field_addr(ArgOffset, Var),
        !:RevFieldAddrs = [FieldAddr | !.RevFieldAddrs]
    ).

:- pred generate_field_take_address_assigns(list(field_addr)::in,
    prog_var::in, ptag::in, llds_code::out,
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
:- pred make_fields_and_arg_vars(vartypes::in, rval::in, ptag::in,
    assoc_list(prog_var, arg_pos_width)::in,
    list(field_and_arg_var)::out(list_skel(field_and_arg_var))) is det.

make_fields_and_arg_vars(_, _, _, [], []).
make_fields_and_arg_vars(VarTypes, Rval, Ptag, [VarPosWidth | VarsPosWidths],
        [FieldAndArgVar | FieldsAndArgVars]) :-
    VarPosWidth = Var - PosWidth,
    (
        ( PosWidth = apw_full(_, CellOffset)
        ; PosWidth = apw_partial_first(_, CellOffset, _, _, _)
        ; PosWidth = apw_double(_, CellOffset, _)
        ; PosWidth = apw_partial_shifted(_, CellOffset, _, _, _, _)
        ; PosWidth = apw_none_shifted(_, CellOffset)
        ),
        CellOffset = cell_offset(CellOffsetInt)
    ;
        PosWidth = apw_none_nowhere,
        CellOffsetInt = -1
    ),
    % The CellOffsetInt duplicates information that is also in PosWidth,
    % but this way, we compute it in just one place (here), instead of
    % all of the (about ten) places where PosWidth is used.
    Field = uv_field(uni_field(Ptag, Rval, CellOffsetInt, PosWidth)),
    lookup_var_type(VarTypes, Var, Type),
    FieldAndArgVar = field_and_arg_var(Field, Var, Type),
    make_fields_and_arg_vars(VarTypes, Rval, Ptag, VarsPosWidths,
        FieldsAndArgVars).

%---------------------------------------------------------------------------%

    % Generate a deterministic deconstruction. In a deterministic
    % deconstruction, we know the value of the ptag, so we don't need
    % to generate a test.

    % Deconstructions are generated semi-eagerly. Any test sub-unifications
    % are generated eagerly (they _must_ be), but assignment unifications
    % are cached.
    %
:- pred generate_det_deconstruction(prog_var::in, cons_id::in,
    assoc_list(prog_var, arg_pos_width)::in, list(unify_mode)::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_det_deconstruction(Var, ConsId, ArgVarsWidths, ArgModes, Code,
        CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    (
        ( ConsTag = string_tag(_String)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = float_tag(_Float)
        ; ConsTag = dummy_tag
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ),
        % For constants, if the deconstruction is det, then we already know
        % the value of the constant.
        Code = empty
    ;
        ( ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "unexpected tag")
    ;
        ConsTag = no_tag,
        ( if
            ArgVarsWidths = [ArgVar - _Width],
            ArgModes = [ArgMode]
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
                generate_deconstruct_unify_arg(FieldAndArgVar, ArgMode, Code,
                    CI, !CLD)
            )
        else
            unexpected($pred, "no_tag: arity != 1")
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        ( if
            ArgVarsWidths = [ArgVar - _Width],
            ArgModes = [ArgMode]
        then
            generate_direct_arg_deconstruct(Var, ArgVar, Ptag, ArgMode, Code,
                CI, !CLD)
        else
            unexpected($pred, "direct_arg_tag: arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            % Treat single_functor the same as unshared_tag(0).
            Ptag = ptag(0u8)
        ;
            ConsTag = unshared_tag(Ptag)
        ;
            ConsTag = shared_remote_tag(Ptag, RemoteSectag),
            AddedBy = RemoteSectag ^ rsectag_added,
            expect(unify(AddedBy, sectag_added_by_unify), $pred,
                "AddedBy != sectag_added_by_unify")
        ),
        Rval = var(Var),
        get_vartypes(CI, VarTypes),
        make_fields_and_arg_vars(VarTypes, Rval, Ptag, ArgVarsWidths,
            FieldsAndArgVars),
        generate_deconstruct_unify_args(FieldsAndArgVars, ArgModes, Code,
            CI, !CLD)
    ;
        ConsTag = shared_local_tag_with_args(_, _),
        generate_deconstruct_tagword_unify_args(Var, ArgVarsWidths, ArgModes,
            [], ToOrRvals, 0u, ToOrMask, AssignRightCode, CI, !CLD),
        (
            ToOrRvals = [],
            Code = AssignRightCode
        ;
            ToOrRvals = [_ | _],
            or_packed_rvals(ToOrRvals, ToOrRval),
            reassign_tagword_var(Var, ToOrMask, ToOrRval, AssignLeftCode,
                CI, !CLD),
            Code = AssignRightCode ++ AssignLeftCode
        )
    ).

%---------------------------------------------------------------------------%

    % Generate a semideterministic deconstruction.
    % A semideterministic deconstruction unification is tag-test
    % followed by a deterministic deconstruction.
    %
:- pred generate_semi_deconstruction(prog_var::in, cons_id::in,
    assoc_list(prog_var, arg_pos_width)::in, list(unify_mode)::in,
    llds_code::out, code_info::in, code_info::out,
    code_loc_dep::in, code_loc_dep::out) is det.

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
:- pred generate_deconstruct_unify_args(
    list(field_and_arg_var)::in(list_skel(field_and_arg_var)),
    list(unify_mode)::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_unify_args([], [], empty, _CI, !CLD).
generate_deconstruct_unify_args([], [_ | _], _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_unify_args([_ | _], [], _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_unify_args([FieldAndArgVar | FieldsAndArgVars],
        [Mode | Modes], Code, CI, !CLD) :-
    generate_deconstruct_unify_arg(FieldAndArgVar, Mode, CodeA, CI, !CLD),
    generate_deconstruct_unify_args(FieldsAndArgVars, Modes, CodeB, CI, !CLD),
    Code = CodeA ++ CodeB.

:- pred generate_deconstruct_tagword_unify_args(prog_var::in,
    assoc_list(prog_var, arg_pos_width)::in, list(unify_mode)::in,
    list(rval)::in, list(rval)::out, uint::in, uint::out,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_tagword_unify_args(_LeftVar, [], [],
        !ToOrRvals, !ToOrMask, empty, _CI, !CLD).
generate_deconstruct_tagword_unify_args(_LeftVar, [], [_ | _],
        !ToOrRvals, !ToOrMask, _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_tagword_unify_args(_LeftVar, [_ | _], [],
        !ToOrRvals, !ToOrMask, _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_tagword_unify_args(LeftVar,
        [ArgVarWidth | ArgVarsWidths], [ArgMode | ArgModes],
        !ToOrRvals, !ToOrMask, Code, CI, !CLD) :-
    ArgVarWidth = RightVar - LeftWidth,
    generate_deconstruct_tagword_unify_arg(LeftVar, LeftWidth, RightVar,
        ArgMode, !ToOrRvals, !ToOrMask, CodeA, CI, !CLD),
    generate_deconstruct_tagword_unify_args(LeftVar, ArgVarsWidths, ArgModes,
        !ToOrRvals, !ToOrMask, CodeB, CI, !CLD),
    Code = CodeA ++ CodeB.

    % Generate a subunification between two [field | variable].
    %
:- pred generate_deconstruct_unify_arg(field_and_arg_var, unify_mode,
    llds_code, code_info, code_loc_dep, code_loc_dep).
:- mode generate_deconstruct_unify_arg(in(var_and_arg_var), in,
    out, in, in, out) is det.
:- mode generate_deconstruct_unify_arg(in(field_and_arg_var), in,
    out, in, in, out) is det.

generate_deconstruct_unify_arg(FieldAndArgVar, ArgMode, Code, CI, !CLD) :-
    FieldAndArgVar = field_and_arg_var(LeftUniVal, RightVar, Type),
    get_module_info(CI, ModuleInfo),
    compute_assign_direction(ModuleInfo, ArgMode, Type, Dir),
    (
        Dir = assign_right,
        ( if variable_is_forward_live(!.CLD, RightVar) then
            (
                LeftUniVal = uv_var(LeftVar),
                assign_var_to_var(RightVar, LeftVar, !CLD),
                Code = empty
            ;
                LeftUniVal = uv_field(LeftField),
                generate_deconstruct_assign_right(LeftField, RightVar,
                    Code, CI, !CLD)
            )
        else
            Code = empty
        )
    ;
        Dir = assign_left,
        (
            LeftUniVal = uv_var(LeftVar),
            ( if variable_is_forward_live(!.CLD, LeftVar) then
                assign_var_to_var(LeftVar, RightVar, !CLD)
            else
                true
            ),
            Code = empty
        ;
            LeftUniVal = uv_field(LeftField),
            % Fields are always considered forward live.
            generate_deconstruct_assign_left(LeftField, RightVar,
                Code, CI, !CLD)
        )
    ;
        Dir = assign_unused,
        % XXX This will have to change if we start to support aliasing.
        Code = empty
    ).

    % Unify (on the left)a word containing tags and packed arguments, and
    % (on the right) a sequence of argument variables. Generate code for
    % the assignments to the right, and update the state variables to help
    % our caller generate a single assignment to the left.
    %
:- pred generate_deconstruct_tagword_unify_arg(prog_var::in,
    arg_pos_width::in, prog_var::in, unify_mode::in,
    list(rval)::in, list(rval)::out, uint::in, uint::out,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_tagword_unify_arg(LeftVar, LeftWidth, RightVar, ArgMode,
        !ToOrRvals, !ToOrMask, Code, CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    get_vartypes(CI, VarTypes),
    lookup_var_type(VarTypes, RightVar, Type),
    compute_assign_direction(ModuleInfo, ArgMode, Type, Dir),
    (
        Dir = assign_right,
        ( if variable_is_forward_live(!.CLD, RightVar) then
            generate_deconstruct_tagword_assign_right(LeftVar, LeftWidth,
                RightVar, Code, !CLD)
        else
            Code = empty
        )
    ;
        Dir = assign_left,
        generate_deconstruct_tagword_assign_left(LeftWidth, RightVar,
            !ToOrRvals, !ToOrMask),
        Code = empty
    ;
        Dir = assign_unused,
        % XXX This will have to change if we start to support aliasing.
        Code = empty
    ).

:- pred generate_deconstruct_assign_right(uni_field::in, prog_var::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_assign_right(RightField, LeftVar, Code, CI, !CLD) :-
    % XXX ARG_PACK The LeftX variables should be named RightX, and vice versa.
    RightField = uni_field(RightPtag, RightBaseRval, RightOffset, RightWidth),
    (
        RightWidth = apw_full(_, _),
        RightLval = field(yes(RightPtag), RightBaseRval,
            const(llconst_int(RightOffset))),
        assign_lval_to_var(LeftVar, RightLval, Code, CI, !CLD)
    ;
        RightWidth = apw_double(_, _, _),
        RightLvalA = field(yes(RightPtag), RightBaseRval,
            const(llconst_int(RightOffset))),
        RightLvalB = field(yes(RightPtag), RightBaseRval,
            const(llconst_int(RightOffset + 1))),
        RightRval = binop(float_from_dword,
            lval(RightLvalA), lval(RightLvalB)),
        assign_field_lval_expr_to_var(LeftVar, [RightLvalA, RightLvalB],
            RightRval, Code, !CLD)
    ;
        (
            RightWidth = apw_partial_first(_, _, _, Mask, Fill),
            RightLval = field(yes(RightPtag), RightBaseRval,
                const(llconst_int(RightOffset))),
            RightRval0 = lval(RightLval)
        ;
            RightWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill),
            RightLval = field(yes(RightPtag), RightBaseRval,
                const(llconst_int(RightOffset))),
            RightRval0 = right_shift_rval(lval(RightLval), Shift)
        ),
        Mask = arg_mask(MaskInt),
        MaskedRightRval0 = binop(bitwise_and(int_type_uint), RightRval0,
            const(llconst_int(MaskInt))),
        maybe_cast_masked_off_rval(Fill, MaskedRightRval0, MaskedRightRval),
        assign_field_lval_expr_to_var(LeftVar, [RightLval], MaskedRightRval,
            Code, !CLD)
    ;
        ( RightWidth = apw_none_nowhere
        ; RightWidth = apw_none_shifted(_, _)
        ),
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
        Code = empty
    ).

:- pred generate_deconstruct_tagword_assign_right(prog_var::in,
    arg_pos_width::in, prog_var::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_tagword_assign_right(LeftVar, LeftWidth, RightVar, Code,
        !CLD) :-
    (
        LeftWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill),
        LeftRval0 = right_shift_rval(var(LeftVar), Shift),
        Mask = arg_mask(MaskInt),
        MaskedLeftRval0 = binop(bitwise_and(int_type_uint), LeftRval0,
            const(llconst_int(MaskInt))),
        maybe_cast_masked_off_rval(Fill, MaskedLeftRval0, MaskedLeftRval),
        assign_expr_to_var(RightVar, MaskedLeftRval, Code, !CLD)
    ;
        LeftWidth = apw_none_shifted(_, _),
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
        Code = empty
    ;
        ( LeftWidth = apw_full(_, _)
        ; LeftWidth = apw_double(_, _, _)
        ; LeftWidth = apw_partial_first(_, _, _, _, _)
        ; LeftWidth = apw_none_nowhere
        ),
        unexpected($pred, "LeftWidth does not belong in tagword")
    ).

:- pred generate_deconstruct_assign_left(uni_field::in, prog_var::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_assign_left(LeftField, RightVar, Code, CI, !CLD) :-
    LeftField = uni_field(LeftPtag, LeftBaseRval0, LeftOffset, LeftWidth),
    % Assignment from a variable to an lvalue - cannot cache
    % so generate immediately.
    produce_variable(RightVar, ProduceRightVarCode, RightRval, CI, !CLD),
    materialize_vars_in_rval(LeftBaseRval0, LeftBaseRval,
        MaterializeLeftBaseCode, CI, !CLD),
    (
        LeftWidth = apw_full(_, _),
        LeftLval = field(yes(LeftPtag), LeftBaseRval,
            const(llconst_int(LeftOffset))),
        AssignCode = singleton(llds_instr(assign(LeftLval, RightRval),
            "Copy value"))
    ;
        LeftWidth = apw_double(_, _, _),
        LeftLvalA = field(yes(LeftPtag), LeftBaseRval,
            const(llconst_int(LeftOffset))),
        LeftLvalB = field(yes(LeftPtag), LeftBaseRval,
            const(llconst_int(LeftOffset + 1))),
        SrcA = unop(dword_float_get_word0, RightRval),
        SrcB = unop(dword_float_get_word1, RightRval),
        Comment = "Update double word",
        AssignCode = from_list([
            llds_instr(assign(LeftLvalA, SrcA), Comment),
            llds_instr(assign(LeftLvalB, SrcB), Comment)
        ])
    ;
        (
            LeftWidth = apw_partial_first(_, _, _, Mask, Fill),
            Shift = arg_shift(0)
        ;
            LeftWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill)
        ),
        Shift = arg_shift(ShiftInt),
        Mask = arg_mask(MaskInt),
        % XXX ARG_PACK
        % In the usual case where the heap cell we are assigning to
        % is freshly created, this code is *seriously* suboptimal.
        % Instead of filling in the bits belonging to each variable
        % as if the other bits were already there, first looking them up
        % and then putting them back, we should just compute the bits
        % for each variable (shifted by the appropriate amount),
        % and OR them together.
        LeftLval = field(yes(LeftPtag), LeftBaseRval,
            const(llconst_int(LeftOffset))),
        ComplementMask = const(llconst_int(\ (MaskInt << ShiftInt))),
        MaskOld = binop(bitwise_and(int_type_uint),
            lval(LeftLval), ComplementMask),
        ShiftedRightRval = left_shift_rval(RightRval, Shift, Fill),
        CombinedRval = or_two_rvals(MaskOld, ShiftedRightRval),
        AssignCode = singleton(llds_instr(assign(LeftLval, CombinedRval),
            "Update part of word"))
    ;
        ( LeftWidth = apw_none_nowhere
        ; LeftWidth = apw_none_shifted(_, _)
        ),
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
        % XXX Should we try to avoid generating ProduceRightVarCode
        % and MaterializeLeftBaseCode as well? MaterializeLeftBaseCode
        % is probably needed by other, non-dummy fields, and
        % ProduceRightVarCode is probably very cheap, so probably not.
        AssignCode = empty
    ),
    Code = ProduceRightVarCode ++ MaterializeLeftBaseCode ++ AssignCode.

:- pred generate_deconstruct_tagword_assign_left(arg_pos_width::in,
    prog_var::in, list(rval)::in, list(rval)::out, uint::in, uint::out) is det.

generate_deconstruct_tagword_assign_left(LeftWidth, RightVar,
        !ToOrRvals, !ToOrMask) :-
    (
        LeftWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill),
        Shift = arg_shift(ShiftInt),
        Mask = arg_mask(MaskInt),
        LeftShiftedRightRval = left_shift_rval(var(RightVar), Shift, Fill),
        !:ToOrRvals = [LeftShiftedRightRval | !.ToOrRvals],
        !:ToOrMask = (uint.cast_from_int(MaskInt) << ShiftInt) \/ !.ToOrMask
    ;
        LeftWidth = apw_none_shifted(_, _)
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
    ;
        ( LeftWidth = apw_full(_, _)
        ; LeftWidth = apw_double(_, _, _)
        ; LeftWidth = apw_partial_first(_, _, _, _, _)
        ; LeftWidth = apw_none_nowhere
        ),
        unexpected($pred, "LeftWidth is not a packed arg_pos_width")
    ).

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
    compute_assign_direction(ModuleInfo, ArgMode, Type, Dir),
    (
        Dir = assign_right,
        unexpected($pred, "assign right in construction")
    ;
        Dir = assign_left,
        assign_expr_to_var(Var, mkword(Ptag, var(Arg)), Code, !CLD)
    ;
        Dir = assign_unused,
        % Construct a tagged pointer to a pointer value
        % which is as yet unknown.
        % XXX This will have to change if we start to support aliasing.
        assign_const_to_var(Var, mkword_hole(Ptag), CI, !CLD),
        Code = empty
    ).

    % Generate a direct arg unification between
    % - the left-hand-side (the whole term), and
    % - the right-hand-side (the one argument).
    %
:- pred generate_direct_arg_deconstruct(prog_var::in, prog_var::in,
    ptag::in, unify_mode::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_direct_arg_deconstruct(Var, ArgVar, Ptag, ArgMode, Code,
        CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    Type = variable_type(CI, ArgVar),
    compute_assign_direction(ModuleInfo, ArgMode, Type, Dir),
    (
        Dir = assign_right,
        ( if variable_is_forward_live(!.CLD, ArgVar) then
            Ptag = ptag(PtagUint8),
            BodyRval = binop(body, var(Var),
                const(llconst_int(uint8.cast_to_int(PtagUint8)))),
            assign_expr_to_var(ArgVar, BodyRval, Code, !CLD)
        else
            Code = empty
        )
    ;
        Dir = assign_left,
        reassign_mkword_hole_var(Var, Ptag, var(ArgVar), Code, !CLD)
    ;
        Dir = assign_unused,
        % XXX This will have to change if we start to support aliasing.
        Code = empty
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
    get_cons_arg_widths(ModuleInfo, ConsId, ConstArgs, ConsArgsPosWidths),
    generate_const_struct_rval(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        !.ConstStructMap, ConsTag, ConsArgsPosWidths, Rval, !StaticCellInfo),
    map.det_insert(ConstNum, Rval, !ConstStructMap).

:- pred generate_const_struct_rval(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in, cons_tag::in,
    assoc_list(const_struct_arg, arg_pos_width)::in, typed_rval::out,
    static_cell_info::in, static_cell_info::out) is det.

generate_const_struct_rval(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConsTag, ConstArgsPosWidths, TypedRval,
        !StaticCellInfo) :-
    (
        ConsTag = no_tag,
        (
            ConstArgsPosWidths = [ConstArgPosWidth],
            generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
                ConstStructMap, ConstArgPosWidth, ArgTypedRval),
            TypedRval = ArgTypedRval
        ;
            ( ConstArgsPosWidths = []
            ; ConstArgsPosWidths = [_, _ | _]
            ),
            unexpected($pred, "no_tag arity != 1")
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        (
            ConstArgsPosWidths = [ConstArgPosWidth],
            generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
                ConstStructMap, ConstArgPosWidth, ArgTypedRval),
            ArgTypedRval = typed_rval(ArgRval, _RvalType),
            Rval = mkword(Ptag, ArgRval),
            TypedRval = typed_rval(Rval, lt_data_ptr)
        ;
            ( ConstArgsPosWidths = []
            ; ConstArgsPosWidths = [_, _ | _]
            ),
            unexpected($pred, "direct_arg_tag: arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            Ptag = ptag(0u8)
        ;
            ConsTag = unshared_tag(Ptag)
        ;
            ConsTag = shared_remote_tag(Ptag, _)
        ),
        generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgsPosWidths, PackedArgTypedRvals),
        (
            ( ConsTag = single_functor_tag
            ; ConsTag = unshared_tag(_Ptag)
            ),
            AllTypedRvals = PackedArgTypedRvals
        ;
            ConsTag = shared_remote_tag(_Ptag, RemoteSectag),
            SectagUint = RemoteSectag ^ rsectag_value,
            StagTypedRval = typed_rval(
                const(llconst_int(uint.cast_to_int(SectagUint))),
                lt_int(int_type_int)),
            AllTypedRvals = [StagTypedRval | PackedArgTypedRvals]
        ),
        add_scalar_static_cell(AllTypedRvals, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        Rval = mkword(Ptag, CellPtrConst),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ConsTag = shared_local_tag_with_args(_Ptag, LocalSectag),
        expect_not(unify(ConstArgsPosWidths, []), $pred,
            "shared_local_tag_with_args has no args"),
        LocalSectag = local_sectag(_, PrimSec, _),
        Rval0 = const(llconst_int(uint.cast_to_int(PrimSec))),
        generate_const_struct_args_for_one_word(ModuleInfo,
            UnboxedFloats, UnboxedInt64s, ConstStructMap,
            ConstArgsPosWidths, LeftOverConstArgsPosWidths, Rval0, Rval),
        expect(unify(LeftOverConstArgsPosWidths, []), $pred, "left over args"),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ( ConsTag = string_tag(_)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = float_tag(_)
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = dummy_tag
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
    assoc_list(const_struct_arg, arg_pos_width)::in, list(typed_rval)::out)
    is det.

generate_const_struct_args(_, _, _, _, [], []) .
generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, [ConstArgPosWidth | ConstArgsPosWidths], TypedRvals) :-
    ConstArgPosWidth = _ConstArg - ArgPosWidth,
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ),
        % For the reason why we handle double word arguments the same as
        % full word arguments, see the comment in ml_unify_gen.m in the
        % predicate ml_pack_ground_term_args_into_word_inits.
        generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgPosWidth, HeadTypedRval),
        generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgsPosWidths, TailTypedRvals),
        TypedRvals = [HeadTypedRval | TailTypedRvals]
    ;
        ArgPosWidth = apw_partial_first(_, _, _, _, ArgFill),
        generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgPosWidth, FirstTypedRval),
        FirstTypedRval = typed_rval(FirstRval, _FirstRvalType),
        cast_away_any_sign_extend_bits(ArgFill, FirstRval, CastFirstRval),
        generate_const_struct_args_for_one_word(ModuleInfo,
            UnboxedFloats, UnboxedInt64s, ConstStructMap,
            ConstArgsPosWidths, LeftOverConstArgsPosWidths,
            CastFirstRval, HeadRval),
        HeadTypedRval = typed_rval(HeadRval, lt_int(int_type_uint)),
        generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, LeftOverConstArgsPosWidths, TailTypedRvals),
        TypedRvals = [HeadTypedRval | TailTypedRvals]
    ;
        ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _),
        unexpected($pred, "apw_partial_shifted")
    ;
        ArgPosWidth = apw_none_shifted(_, _),
        unexpected($pred, "apw_none_shifted")
    ;
        ArgPosWidth = apw_none_nowhere,
        % Generate nothing for this argument.
        generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgsPosWidths, TypedRvals)
    ).

:- pred generate_const_struct_args_for_one_word(module_info::in,
    have_unboxed_floats::in, have_unboxed_int64s::in, const_struct_map::in,
    assoc_list(const_struct_arg, arg_pos_width)::in,
    assoc_list(const_struct_arg, arg_pos_width)::out,
    rval::in, rval::out) is det.

generate_const_struct_args_for_one_word(_, _, _, _, [], [],
        Rval, Rval).
generate_const_struct_args_for_one_word(ModuleInfo,
        UnboxedFloats, UnboxedInt64s, ConstStructMap,
        [ConstArgPosWidth | ConstArgsPosWidths], LeftOverConstArgsPosWidths,
        CurRval, WordRval) :-
    ConstArgPosWidth = _ConstArg - ArgPosWidth,
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        % These are not part of the current word.
        LeftOverConstArgsPosWidths = [ConstArgPosWidth | ConstArgsPosWidths],
        WordRval = CurRval
    ;
        (
            ArgPosWidth = apw_partial_shifted(_, _, ArgShift, _, _, Fill),
            generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
                ConstStructMap, ConstArgPosWidth, ArgTypedRval),
            ArgTypedRval = typed_rval(ArgRval, _ArgRvalType),
            add_shifted_rval(CurRval, ArgRval, ArgShift, Fill, NextRval)
        ;
            ArgPosWidth = apw_none_shifted(_, _),
            NextRval = CurRval
        ),
        generate_const_struct_args_for_one_word(ModuleInfo,
            UnboxedFloats, UnboxedInt64s, ConstStructMap,
            ConstArgsPosWidths, LeftOverConstArgsPosWidths, NextRval, WordRval)
    ).

:- pred generate_const_struct_arg(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in,
    pair(const_struct_arg, arg_pos_width)::in, typed_rval::out) is det.

generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConstArg - ArgPosWidth, TypedRval) :-
    (
        ConstArg = csa_const_struct(ConstNum),
        map.lookup(ConstStructMap, ConstNum, TypedRval)
    ;
        ConstArg = csa_constant(ConsId, _),
        ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
        generate_const_struct_arg_tag(UnboxedFloats, UnboxedInt64s,
            ConsTag, ArgPosWidth, TypedRval)
    ).

:- pred generate_const_struct_arg_tag(have_unboxed_floats::in,
    have_unboxed_int64s::in, cons_tag::in, arg_pos_width::in,
    typed_rval::out) is det.

generate_const_struct_arg_tag(UnboxedFloats, UnboxedInt64s,
        ConsTag, ArgPosWidth, TypedRval) :-
    % The code of this predicate is very similar to the code of
    % generate_ground_term_conjunct_tag. Any changes here may also
    % require similar changes there.
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
                    % Though a standalone int64 or uint64 value might have
                    % needed to boxed, it may be stored in unboxed form
                    % as a constructor argument.
                    ( if ArgPosWidth = apw_double(_, _, _) then
                        Type = lt_int(IntType)
                    else
                        Type = lt_data_ptr
                    )
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
            ConsTag = dummy_tag,
            Const = llconst_int(0),
            Type = lt_int(int_type_int)
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
                % Though a standalone float might have needed to boxed,
                % it may be stored in unboxed form as a constructor argument.
                ( if ArgPosWidth = apw_double(_, _, _) then
                    Type = lt_float
                else
                    Type = lt_data_ptr
                )
            )
        ),
        TypedRval = typed_rval(const(Const), Type)
    ;
        ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, _MustMask),
        LocalSectag = local_sectag(_, PrimSec, _),
        Rval = const(llconst_int(uint.cast_to_int(PrimSec))),
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
        ; ConsTag = shared_local_tag_with_args(_, _)
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
    assoc_list(prog_var, arg_pos_width)::in, have_unboxed_floats::in,
    static_cell_info::in, static_cell_info::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_conjunct_tag(Var, ConsTag, ArgVarsWidths,
        UnboxedFloats, !StaticCellInfo, !ActiveMap) :-
    % The code of this predicate is very similar to the code of
    % generate_const_struct_arg_tag. Any changes here may also
    % require similar changes there.
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
            ConsTag = dummy_tag,
            Const = llconst_int(0),
            Type = lt_int(int_type_int)
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
        expect(unify(ArgVarsWidths, []), $pred, "constant has args"),
        ActiveGroundTerm = typed_rval(const(Const), Type),
        map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, _MustMask),
        expect(unify(ArgVarsWidths, []), $pred,
            "shared_local_tag_no_args has args"),
        LocalSectag = local_sectag(_, PrimSec, _),
        Rval = const(llconst_int(uint.cast_to_int(PrimSec))),
        ActiveGroundTerm = typed_rval(Rval, lt_data_ptr),
        map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = shared_local_tag_with_args(_Ptag, LocalSectag),
        expect_not(unify(ArgVarsWidths, []), $pred,
            "shared_local_tag_with_args has no args"),
        LocalSectag = local_sectag(_, PrimSec, _),
        Rval0 = const(llconst_int(uint.cast_to_int(PrimSec))),
        generate_ground_term_args_for_one_word(ArgVarsWidths,
            LeftOverArgVarsWidths, Rval0, Rval, !ActiveMap),
        expect(unify(LeftOverArgVarsWidths, []), $pred, "left over args"),
        ActiveGroundTerm = typed_rval(Rval, lt_data_ptr),
        map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = no_tag,
        (
            ArgVarsWidths = [ArgVar - _ArgPosWidth],
            map.det_remove(ArgVar, RvalType, !ActiveMap),
            map.det_insert(Var, RvalType, !ActiveMap)
        ;
            ( ArgVarsWidths = []
            ; ArgVarsWidths = [_, _ | _]
            ),
            unexpected($pred, "no_tag arity != 1")
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        (
            ArgVarsWidths = [ArgVar - _ArgPosWidth],
            map.det_remove(ArgVar, typed_rval(ArgRval, _RvalType), !ActiveMap),
            Rval = mkword(Ptag, ArgRval),
            ActiveGroundTerm = typed_rval(Rval, lt_data_ptr),
            map.det_insert(Var, ActiveGroundTerm, !ActiveMap)
        ;
            ( ArgVarsWidths = []
            ; ArgVarsWidths = [_, _ | _]
            ),
            unexpected($pred, "direct_arg_tag arity != 1")
        )
    ;
        (
            ConsTag = single_functor_tag,
            Ptag = ptag(0u8)
        ;
            ConsTag = unshared_tag(Ptag)
        ;
            ConsTag = shared_remote_tag(Ptag, _)
        ),
        generate_ground_term_args(ArgVarsWidths, PackedArgTypedRvals,
            !ActiveMap),
        (
            ( ConsTag = single_functor_tag
            ; ConsTag = unshared_tag(_Ptag)
            ),
            AllTypedRvals = PackedArgTypedRvals
        ;
            ConsTag = shared_remote_tag(_Ptag, RemoteSectag),
            SectagUint = RemoteSectag ^ rsectag_value,
            StagTypedRval = typed_rval(
                const(llconst_int(uint.cast_to_int(SectagUint))),
                lt_int(int_type_int)),
            AllTypedRvals = [StagTypedRval | PackedArgTypedRvals]
        ),
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

:- pred generate_ground_term_args(assoc_list(prog_var, arg_pos_width)::in,
    list(typed_rval)::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_args([], [], !ActiveMap).
generate_ground_term_args([ArgVarWidth | ArgVarsWidths],
        [TypedRval | TypedRvals], !ActiveMap) :-
    ArgVarWidth = ArgVar - ArgPosWidth,
    map.det_remove(ArgVar, ArgTypedRval, !ActiveMap),
    (
        ArgPosWidth = apw_full(_, _),
        TypedRval = ArgTypedRval,
        generate_ground_term_args(ArgVarsWidths, TypedRvals, !ActiveMap)
    ;
        ArgPosWidth = apw_double(_, _, DoubleWordKind),
        % Though a standalone value of type float, int64 or int64
        % might have needed to boxed, it may be stored in unboxed form
        % as a constructor argument.
        ( if ArgTypedRval = typed_rval(ArgRval, lt_data_ptr) then
            (
                DoubleWordKind = dw_float,
                TypedRval = typed_rval(ArgRval, lt_float)
            ;
                DoubleWordKind = dw_int64,
                TypedRval = typed_rval(ArgRval, lt_int(int_type_int64))
            ;
                DoubleWordKind = dw_uint64,
                TypedRval = typed_rval(ArgRval, lt_int(int_type_uint64))
            )
        else
            TypedRval = ArgTypedRval
        ),
        generate_ground_term_args(ArgVarsWidths, TypedRvals, !ActiveMap)
    ;
        ArgPosWidth = apw_partial_first(_, _, _, _, _),
        ArgTypedRval = typed_rval(ArgRval, _),
        generate_ground_term_args_for_one_word(ArgVarsWidths,
            LeftOverArgVarsWidths, ArgRval, WordRval, !ActiveMap),
        TypedRval = typed_rval(WordRval, lt_int(int_type_uint)),
        generate_ground_term_args(LeftOverArgVarsWidths, TypedRvals,
            !ActiveMap)
    ;
        ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _),
        unexpected($pred, "apw_partial_shifted")
    ;
        ArgPosWidth = apw_none_shifted(_, _),
        unexpected($pred, "apw_none_shifted")
    ;
        ArgPosWidth = apw_none_nowhere,
        unexpected($pred, "apw_none_nowhere")
    ).

:- pred generate_ground_term_args_for_one_word(
    assoc_list(prog_var, arg_pos_width)::in,
    assoc_list(prog_var, arg_pos_width)::out,
    rval::in, rval::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_args_for_one_word([], [], CurRval, CurRval, !ActiveMap).
generate_ground_term_args_for_one_word([ArgVarWidth | ArgVarsWidths],
        LeftOverArgVarsWidths, CurRval, WordRval, !ActiveMap) :-
    ArgVarWidth = ArgVar - ArgPosWidth,
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        % These are not part of the current word.
        LeftOverArgVarsWidths = [ArgVarWidth | ArgVarsWidths],
        WordRval = CurRval
    ;
        (
            ArgPosWidth = apw_partial_shifted(_, _, ArgShift, _, _, Fill),
            map.det_remove(ArgVar, ArgTypedRval, !ActiveMap),
            ArgTypedRval = typed_rval(ArgRval, _ArgRvalType),
            add_shifted_rval(CurRval, ArgRval, ArgShift, Fill, NextRval)
        ;
            ArgPosWidth = apw_none_shifted(_, _),
            map.det_remove(ArgVar, _ArgTypedRval, !ActiveMap),
            NextRval = CurRval
        ),
        generate_ground_term_args_for_one_word(ArgVarsWidths,
            LeftOverArgVarsWidths, NextRval, WordRval, !ActiveMap)
    ).

%---------------------------------------------------------------------------%

    % OR together the given rvals.
    %
    % We currently do this a linear fashion, starting at the rightmost
    % arguments, and moving towards the left.
    %
    % We should explore whether other strategies, such as balanced trees,
    % (or rather, trees that are as balanced as possible) would work better.
    %
:- pred or_packed_rvals(list(rval)::in, rval::out) is det.

or_packed_rvals(Rvals, OrAllRval) :-
    (
        Rvals = [],
        OrAllRval = const(llconst_int(0))
    ;
        Rvals = [HeadRval | TailRvals],
        or_packed_rvals_lag(HeadRval, TailRvals, OrAllRval)
    ).

:- pred or_packed_rvals_lag(rval::in, list(rval)::in, rval::out) is det.

or_packed_rvals_lag(HeadRval, TailRvals, OrAllRval) :-
    (
        TailRvals = [],
        OrAllRval = HeadRval
    ;
        TailRvals = [HeadTailRval | TailTailRvals],
        or_packed_rvals_lag(HeadTailRval, TailTailRvals, TailOrAllRval),
        OrAllRval = or_two_rvals(HeadRval, TailOrAllRval)
    ).

%---------------------------------------------------------------------------%

:- pred add_shifted_rval(rval::in, rval::in, arg_shift::in, fill_kind::in,
    rval::out) is det.

add_shifted_rval(CurRval, ArgRval, ArgShift, ArgFill, ResultRval) :-
    ArgShift = arg_shift(ArgShiftInt),
    cast_away_any_sign_extend_bits(ArgFill, ArgRval, CastArgRval),
    ( if ArgShiftInt = 0 then
        ShiftedArgRval = CastArgRval
    else if ArgRval = const(llconst_int(0)) then
        ShiftedArgRval = CastArgRval
    else
        ShiftedArgRval = binop(unchecked_left_shift(int_type_uint),
            CastArgRval, const(llconst_int(ArgShiftInt)))
    ),
    ResultRval = or_two_rvals(CurRval, ShiftedArgRval).

%---------------------------------------------------------------------------%

:- pred pack_how_to_construct(assoc_list(prog_var, arg_pos_width)::in,
    how_to_construct::in, how_to_construct::out) is det.

pack_how_to_construct(ArgVarsWidths, !HowToConstruct) :-
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
        % argument for this code could be made if we allowed reuse of a cell
        % that originally stored a term with one cons_id for a term with
        % a different cons_id. This is why we currently don't allow such reuse
        % even though such reuse would significantly expand the set of
        % opportunities for reuse.
        CellToReuse0 = cell_to_reuse(Var, ConsIds, NeedsUpdates0),
        needs_update_args_to_words(ArgVarsWidths, NeedsUpdates0, NeedsUpdates),
        CellToReuse = cell_to_reuse(Var, ConsIds, NeedsUpdates),
        !:HowToConstruct = reuse_cell(CellToReuse)
    ).

:- pred needs_update_args_to_words(assoc_list(prog_var, arg_pos_width)::in,
    list(needs_update)::in, list(needs_update)::out) is det.

needs_update_args_to_words([], [], []).
needs_update_args_to_words([], [_ | _], _) :-
    unexpected($module, $pred, "mismatched lists").
needs_update_args_to_words([_ | _], [], []) :-
    unexpected($module, $pred, "mismatched lists").
needs_update_args_to_words([VarWidth | VarsWidths], [ArgNU | ArgNUs],
        WordNUs) :-
    VarWidth = _Var - Width,
    (
        ( Width = apw_full(_, _)
        ; Width = apw_double(_, _, _)
        ),
        needs_update_args_to_words(VarsWidths, ArgNUs, TailWordNUs),
        WordNUs = [ArgNU | TailWordNUs]
    ;
        Width = apw_partial_first(_, _, _, _, _),
        does_any_arg_in_word_need_update(VarsWidths, ArgNUs, ArgNU, WordNU,
            LaterWordVarsWidths, LaterWordArgNUs),
        needs_update_args_to_words(LaterWordVarsWidths, LaterWordArgNUs,
            TailWordNUs),
        WordNUs = [WordNU | TailWordNUs]
    ;
        Width = apw_none_nowhere,
        needs_update_args_to_words(VarsWidths, ArgNUs, WordNUs)
    ;
        Width = apw_partial_shifted(_, _, _, _, _, _),
        unexpected($pred, "apw_partial_shifted")
    ;
        Width = apw_none_shifted(_, _),
        unexpected($pred, "none_shifted")
    ).

:- pred does_any_arg_in_word_need_update(
    assoc_list(prog_var, arg_pos_width)::in, list(needs_update)::in,
    needs_update::in, needs_update::out,
    assoc_list(prog_var, arg_pos_width)::out, list(needs_update)::out) is det.

does_any_arg_in_word_need_update([], [], !NU, [], []).
does_any_arg_in_word_need_update([], [_ | _], !NU, _, _) :-
    unexpected($module, $pred, "mismatched lists").
does_any_arg_in_word_need_update([_ | _], [], !NU, _, _) :-
    unexpected($module, $pred, "mismatched lists").
does_any_arg_in_word_need_update([VarWidth | VarsWidths], [ArgNU | ArgNUs],
        !NU, LaterWordVarsWidths, LaterWordArgNUs) :-
    VarWidth = _Var - Width,
    (
        ( Width = apw_full(_, _)
        ; Width = apw_double(_, _, _)
        ; Width = apw_partial_first(_, _, _, _, _)
        ; Width = apw_none_nowhere
        ),
        LaterWordVarsWidths = [VarWidth | VarsWidths],
        LaterWordArgNUs = [ArgNU | ArgNUs]
    ;
        ( Width = apw_partial_shifted(_, _, _, _, _, _)
        ; Width = apw_none_shifted(_, _)
        ),
        (
            ArgNU = needs_update,
            !:NU = needs_update
        ;
            ArgNU = does_not_need_update
        ),
        does_any_arg_in_word_need_update(VarsWidths, ArgNUs, !NU,
            LaterWordVarsWidths, LaterWordArgNUs)
    ).

%---------------------------------------------------------------------------%

:- pred var_type_msg(mer_type::in, string::out) is det.

var_type_msg(Type, Msg) :-
    type_to_ctor_det(Type, TypeCtor),
    TypeCtor = type_ctor(TypeSym, TypeArity),
    TypeSymStr = sym_name_to_string(TypeSym),
    string.int_to_string(TypeArity, TypeArityStr),
    Msg = TypeSymStr ++ "/" ++ TypeArityStr.

%---------------------------------------------------------------------------%

:- func or_two_rvals(rval, rval) = rval.

or_two_rvals(RvalA, RvalB) = OrRval :-
    % OR-ing anything with zero has no effect.
    ( if
        ( RvalA = const(llconst_int(0))
        ; RvalA = const(llconst_uint(0u))
        )
    then
        OrRval = RvalB
    else if
        ( RvalB = const(llconst_int(0))
        ; RvalB = const(llconst_uint(0u))
        )
    then
        OrRval = RvalA
    else
        OrRval = binop(bitwise_or(int_type_uint), RvalA, RvalB)
    ).

:- func left_shift_rval(rval, arg_shift, fill_kind) = rval.

left_shift_rval(Rval, Shift, Fill) = ShiftedRval :-
    Shift = arg_shift(ShiftInt),
    cast_away_any_sign_extend_bits(Fill, Rval, CastRval),
    ( if ShiftInt = 0 then
        % Shifting anything by zero bits has no effect.
        ShiftedRval = CastRval
    else if Rval = const(llconst_int(0)) then
        % Shifting zero any number of bits has no effect.
        ShiftedRval = CastRval
    else
        ShiftedRval = binop(unchecked_left_shift(int_type_uint),
            CastRval, const(llconst_int(ShiftInt)))
    ).

:- func right_shift_rval(rval, arg_shift) = rval.

right_shift_rval(Rval, Shift) = ShiftedRval :-
    Shift = arg_shift(ShiftInt),
    % Shifting anything by zero bits has no effect.
    % Shifting zero any number of bits has no effect.
    % However, our caller won't give us either a zero shift amount
    % or a constant zero rval to shift.
    ShiftedRval = binop(unchecked_right_shift(int_type_uint),
        Rval, const(llconst_int(ShiftInt))).

    % If a sub-word-sized signed integer has a negative value, then it will
    % have sign-extend bits *beyond* its usual size. OR-ing the raw form
    % of that sub-word-sized integer with the values of the other fields
    % may thus stomp all over the bits assigned to store the other fields
    % that are to the left of the sub-word-sized signed integer.
    %
    % Prevent this by casting sub-word-sized signed integers to their
    % unsigned counterparts before the shift and the OR operations.
    %
:- pred cast_away_any_sign_extend_bits(fill_kind::in, rval::in, rval::out)
    is det.

cast_away_any_sign_extend_bits(Fill, Rval0, Rval) :-
    (
        ( Fill = fill_enum
        ; Fill = fill_uint8
        ; Fill = fill_uint16
        ; Fill = fill_uint32
        ),
        Rval = Rval0
    ;
        Fill = fill_int8,
        Rval = cast(lt_int(int_type_uint8), Rval0)
    ;
        Fill = fill_int16,
        Rval = cast(lt_int(int_type_uint16), Rval0)
    ;
        Fill = fill_int32,
        Rval = cast(lt_int(int_type_uint32), Rval0)
    ).

:- pred maybe_cast_masked_off_rval(fill_kind::in, rval::in, rval::out) is det.

maybe_cast_masked_off_rval(Fill, MaskedRval0, MaskedRval) :-
    (
        Fill = fill_enum,
        MaskedRval = MaskedRval0
    ;
        Fill = fill_int8,
        MaskedRval = cast(lt_int(int_type_int8), MaskedRval0)
    ;
        Fill = fill_uint8,
        MaskedRval = cast(lt_int(int_type_uint8), MaskedRval0)
    ;
        Fill = fill_int16,
        MaskedRval = cast(lt_int(int_type_int16), MaskedRval0)
    ;
        Fill = fill_uint16,
        MaskedRval = cast(lt_int(int_type_uint16), MaskedRval0)
    ;
        Fill = fill_int32,
        MaskedRval = cast(lt_int(int_type_int32), MaskedRval0)
    ;
        Fill = fill_uint32,
        MaskedRval = cast(lt_int(int_type_uint32), MaskedRval0)
    ).

%---------------------------------------------------------------------------%

:- type assign_dir
    --->    assign_left
    ;       assign_right
    ;       assign_unused.

:- pred compute_assign_direction(module_info::in, unify_mode::in, mer_type::in,
    assign_dir::out) is det.
:- pragma inline(compute_assign_direction/4).

compute_assign_direction(ModuleInfo, ArgMode, ArgType, Dir) :-
    ArgMode = unify_modes_lhs_rhs(LeftFromToInsts, RightFromToInsts),
    from_to_insts_to_top_functor_mode(ModuleInfo, LeftFromToInsts, ArgType,
        LeftTopMode),
    from_to_insts_to_top_functor_mode(ModuleInfo, RightFromToInsts, ArgType,
        RightTopMode),
    (
        LeftTopMode = top_in,
        (
            RightTopMode = top_in,
            % Both input: it is a test unification.
            % This shouldn't happen, since mode analysis should avoid
            % creating any tests in the arguments of a construction
            % or deconstruction unification.
            unexpected($pred, "test in arg of [de]construction")
        ;
            RightTopMode = top_out,
            % Input - output: it is an assignment to the RHS.
            Dir = assign_right
        ;
            RightTopMode = top_unused,
            unexpected($pred, "some strange unify")
        )
    ;
        LeftTopMode = top_out,
        (
            RightTopMode = top_in,
            % Output - input: it is an assignment to the LHS.
            Dir = assign_left
        ;
            ( RightTopMode = top_out
            ; RightTopMode = top_unused
            ),
            unexpected($pred, "some strange unify")
        )
    ;
        LeftTopMode = top_unused,
        (
            RightTopMode = top_unused,
            % Unused - unused: the unification has no effect.
            Dir = assign_unused
        ;
            ( RightTopMode = top_in
            ; RightTopMode = top_out
            ),
            unexpected($pred, "some strange unify")
        )
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.unify_gen.
%---------------------------------------------------------------------------%
