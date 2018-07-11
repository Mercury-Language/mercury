%---------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------e
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module ll_backend.unify_gen_test.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

%---------------------------------------------------------------------------%

:- type test_sense
    --->    branch_on_success
    ;       branch_on_failure.

:- pred generate_tag_test(prog_var::in, cons_id::in,
    maybe_cheaper_tag_test::in, test_sense::in, label::out, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

:- pred generate_raw_tag_test_case(rval::in, mer_type::in, string::in,
    tagged_cons_id::in, list(tagged_cons_id)::in, maybe_cheaper_tag_test::in,
    test_sense::in, label::out, llds_code::out, code_info::in, code_info::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module ll_backend.code_util.
:- import_module ll_backend.unify_gen_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_out.

:- import_module cord.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

generate_tag_test(Var, ConsId, CheaperTagTest, Sense, ElseLabel, Code,
        !CI, !CLD) :-
    produce_variable(Var, VarCode, VarRval, !.CI, !CLD),
    VarType = variable_type(!.CI, Var),
    VarName = variable_name(!.CI, Var),
    generate_raw_tag_test(VarRval, VarType, VarName, ConsId, no,
        CheaperTagTest, Sense, ElseLabel, TestCode, !CI),
    Code = VarCode ++ TestCode.

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
:- end_module ll_backend.unify_gen_test.
%---------------------------------------------------------------------------%
