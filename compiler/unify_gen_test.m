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
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type test_sense
    --->    branch_on_success
    ;       branch_on_failure.

:- pred generate_test_var_has_cons_id(rval::in, string::in, cons_id::in,
    maybe_cheaper_tag_test::in, test_sense::in,
    label::out, llds_code::out, code_info::in, code_info::out) is det.

:- pred generate_test_var_has_one_tagged_cons_id(rval::in, string::in,
    tagged_cons_id::in, list(tagged_cons_id)::in,
    maybe_cheaper_tag_test::in, test_sense::in,
    label::out, llds_code::out, code_info::in, code_info::out) is det.

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
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

generate_test_var_has_cons_id(VarRval, VarName,
        ConsId, CheaperTagTest, Sense, ElseLabel, Code, !CI) :-
    get_module_info(!.CI, ModuleInfo),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    generate_test_var_has_cons_id_tag(VarRval, VarName, ConsId, ConsTag,
        CheaperTagTest, Sense, ElseLabel, Code, !CI).

%---------------------------------------------------------------------------%

generate_test_var_has_one_tagged_cons_id(VarRval, VarName,
        MainTaggedConsId, OtherTaggedConsIds, CheaperTagTest, Sense,
        ElseLabel, Code, !CI) :-
    (
        OtherTaggedConsIds = [],
        % Try applying the cheaper tag test optimization.
        MainTaggedConsId = tagged_cons_id(MainConsId, MainConsTag),
        generate_test_var_has_cons_id_tag(VarRval, VarName,
            MainConsId, MainConsTag, CheaperTagTest, Sense, ElseLabel,
            Code, !CI)
    ;
        OtherTaggedConsIds = [_ | _],
        % The cheaper tag test optimization doesn't apply.
        generate_test_rval_has_tagged_cons_id(!.CI, VarRval,
            MainTaggedConsId, MainTagTestRval),
        list.map(generate_test_rval_has_tagged_cons_id(!.CI, VarRval),
            OtherTaggedConsIds, OtherTagTestRvals),
        logical_or_rvals(MainTagTestRval, OtherTagTestRvals, TestRval),
        project_cons_name_and_tag(MainTaggedConsId, MainConsName, _),
        list.map2(project_cons_name_and_tag, OtherTaggedConsIds,
            OtherConsNames, _),
        Comment = branch_sense_comment(Sense) ++
            case_comment(VarName, MainConsName, OtherConsNames),
        generate_test_sense_branch(Sense, TestRval, Comment,
            ElseLabel, Code, !CI)
    ).

:- pred logical_or_rvals(rval::in, list(rval)::in, rval::out) is det.

logical_or_rvals(CurTestRval, OtherTestRvals, TestRval) :-
    (
        OtherTestRvals = [],
        TestRval = CurTestRval
    ;
        OtherTestRvals = [HeadTestRval | TailTestRvals],
        NextTestRval = binop(logical_or, CurTestRval, HeadTestRval),
        logical_or_rvals(NextTestRval, TailTestRvals, TestRval)
    ).

%---------------------------------------------------------------------------%

:- pred generate_test_var_has_cons_id_tag(rval::in, string::in,
    cons_id::in, cons_tag::in,
    maybe_cheaper_tag_test::in, test_sense::in, label::out, llds_code::out,
    code_info::in, code_info::out) is det.

generate_test_var_has_cons_id_tag(VarRval, VarName, ConsId, ConsTag,
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
        generate_test_rval_has_cons_tag(!.CI, VarRval, CheapConsTag,
            NegTestRval),
        code_util.neg_rval(NegTestRval, TestRval)
    else
        Comment = branch_sense_comment(Sense) ++ VarName ++
            " has functor " ++ ConsIdName,
        generate_test_rval_has_cons_tag(!.CI, VarRval, ConsTag, TestRval)
    ),
    generate_test_sense_branch(Sense, TestRval, Comment, ElseLabel, Code, !CI).

:- func branch_sense_comment(test_sense) = string.

branch_sense_comment(branch_on_success) =
    "branch away if ".
branch_sense_comment(branch_on_failure) =
    "branch away unless ".

:- pred generate_test_sense_branch(test_sense::in, rval::in, string::in,
    label::out, llds_code::out, code_info::in, code_info::out) is det.

generate_test_sense_branch(Sense, TestRval, Comment, ElseLabel, Code, !CI) :-
    get_next_label(ElseLabel, !CI),
    (
        Sense = branch_on_success,
        BranchRval = TestRval
    ;
        Sense = branch_on_failure,
        code_util.neg_rval(TestRval, BranchRval)
    ),
    Code = singleton(
        llds_instr(if_val(BranchRval, code_label(ElseLabel)), Comment)
    ).

%---------------------------------------------------------------------------%

:- pred generate_test_rval_has_tagged_cons_id(code_info::in, rval::in,
    tagged_cons_id::in, rval::out) is det.

generate_test_rval_has_tagged_cons_id(CI, VarRval, TaggedConsId, TestRval) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    generate_test_rval_has_cons_tag(CI, VarRval, ConsTag, TestRval).

    % generate_test_rval_has_cons_tag(CI, VarRval, Type, ConsTag, TestRval):
    %
    % TestRval is an rval of type bool which evaluates to true if VarRval has
    % the specified ConsTag, and false otherwise. Type is the type of VarRval.
    %
:- pred generate_test_rval_has_cons_tag(code_info::in, rval::in, cons_tag::in,
    rval::out) is det.

generate_test_rval_has_cons_tag(CI, VarRval, ConsTag, TestRval) :-
    (
        ConsTag = int_tag(IntTag),
        int_tag_to_const_and_int_type(IntTag, Const, IntType),
        TestRval = binop(eq(IntType), VarRval, const(Const))
    ;
        ConsTag = float_tag(Float),
        TestRval = binop(float_eq, VarRval, const(llconst_float(Float)))
    ;
        ConsTag = string_tag(String),
        TestRval = binop(str_eq, VarRval, const(llconst_string(String)))
    ;
        ConsTag = foreign_tag(ForeignLang, ForeignVal),
        expect(unify(ForeignLang, lang_c), $pred,
            "foreign tag for language other than C"),
        TestRval = binop(eq(int_type_int), VarRval,
            const(llconst_foreign(ForeignVal, lt_int(int_type_int))))
    ;
        ( ConsTag = dummy_tag
        ; ConsTag = no_tag
        ),
        % In a type with only one cons_id, all vars have that one cons_id.
        TestRval = const(llconst_true)
    ;
        ConsTag = direct_arg_tag(Ptag),
        VarPtag = unop(tag, VarRval),
        Ptag = ptag(PtagUint8),
        PtagConstRval = const(llconst_int(uint8.cast_to_int(PtagUint8))),
        TestRval = binop(eq(int_type_int), VarPtag, PtagConstRval)
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            RemoteArgsTagInfo = remote_args_only_functor,
            % In a type with only one cons_id, all vars have that one cons_id.
            TestRval = const(llconst_true)
        ;
            RemoteArgsTagInfo = remote_args_unshared(Ptag),
            VarPtag = unop(tag, VarRval),
            Ptag = ptag(PtagUint8),
            PtagConstRval = const(llconst_int(uint8.cast_to_int(PtagUint8))),
            TestRval = binop(eq(int_type_int), VarPtag, PtagConstRval)
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            VarPtag = unop(tag, VarRval),
            Ptag = ptag(PtagUint8),
            ConstPtagRval = const(llconst_int(uint8.cast_to_int(PtagUint8))),
            PtagTestRval = binop(eq(int_type_int), VarPtag, ConstPtagRval),
            VarSectagWordRval =
                lval(field(yes(Ptag), VarRval, const(llconst_int(0)))),
            RemoteSectag = remote_sectag(SecTagUint, SectagSize),
            (
                SectagSize = rsectag_word,
                VarSectagRval = VarSectagWordRval
            ;
                SectagSize = rsectag_subword(SectagBits),
                SectagBits = sectag_bits(_NumSectagBits, SectagMask),
                VarSectagRval = binop(bitwise_and(int_type_uint),
                    VarSectagWordRval, const(llconst_uint(SectagMask)))
            ),
            ConstSectagRval = const(llconst_int(uint.cast_to_int(SecTagUint))),
            SectagTestRval = binop(eq(int_type_int),
                VarSectagRval, ConstSectagRval),
            TestRval = binop(logical_and, PtagTestRval, SectagTestRval)
        ;
            RemoteArgsTagInfo = remote_args_ctor(_Data),
            % These are supported only on the MLDS backend.
            unexpected($pred, "remote_args_ctor")
        )
    ;
        ConsTag = local_args_tag(LocalArgsTagInfo),
        (
            LocalArgsTagInfo = local_args_only_functor,
            % In a type with only one cons_id, all vars have that one cons_id.
            TestRval = const(llconst_true)
        ;
            LocalArgsTagInfo = local_args_not_only_functor(_Ptag, LocalSectag),
            % We generate the same test as for shared_local_tag_no_args
            % with lsectag_must_be_masked.
            LocalSectag = local_sectag(_Sectag, PrimSec, SectagBits),
            ConstPrimSecRval = const(llconst_uint(PrimSec)),

            code_info.get_num_ptag_bits(CI, NumPtagBits),
            SectagBits = sectag_bits(NumSectagBits, _SectagMask),
            NumPtagSectagBits = uint8.cast_to_int(NumPtagBits + NumSectagBits),
            PrimSecMask = (1u << NumPtagSectagBits) - 1u,
            MaskedVarRval = binop(bitwise_and(int_type_uint),
                VarRval, const(llconst_uint(PrimSecMask))),

            TestRval = binop(eq(int_type_uint),
                MaskedVarRval, ConstPrimSecRval)
        )
    ;
        ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, MustMask),
        LocalSectag = local_sectag(_Sectag, PrimSec, SectagBits),
        ConstPrimSecRval = const(llconst_int(uint.cast_to_int(PrimSec))),
        (
            MustMask = lsectag_always_rest_of_word,
            TestRval = binop(eq(int_type_int), VarRval, ConstPrimSecRval)
        ;
            MustMask = lsectag_must_be_masked,
            % We generate the same test as for shared_local_tag_with_args.
            code_info.get_num_ptag_bits(CI, NumPtagBits),
            SectagBits = sectag_bits(NumSectagBits, _SectagMask),
            NumPtagSectagBits = uint8.cast_to_int(NumPtagBits + NumSectagBits),
            PrimSecMask = (1u << NumPtagSectagBits) - 1u,
            MaskedVarRval = binop(bitwise_and(int_type_uint),
                VarRval, const(llconst_uint(PrimSecMask))),
            TestRval = binop(eq(int_type_uint),
                MaskedVarRval, ConstPrimSecRval)
        )
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
