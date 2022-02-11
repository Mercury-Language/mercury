%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012, 2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module ml_backend.ml_unify_gen_test.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % ml_generate_test_var_has_cons_id(Var, ConsId, TestRval, !Info):
    %
    % We generate the boolean rval TestRval, which will evaluate to true
    % iff Var has the functor specified by ConsId.
    %
:- pred ml_generate_test_var_has_cons_id(prog_var::in, cons_id::in,
    mlds_rval::out, ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%

    % ml_generate_test_var_has_tagged_cons_id(Var,
    %   MainTaggedConsId, OtherTaggedConsIds, TestRval, !Info):
    %
    % We generate the boolean rval TestRval, which will evaluate to true
    % iff Var's functor is one of those specified by MainTaggedConsId
    % or OtherTaggedConsIds.
    %
    % Exported for use by ml_switch_gen.m.
    %
:- pred ml_generate_test_var_has_one_tagged_cons_id(prog_var::in,
    tagged_cons_id::in, list(tagged_cons_id)::in, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_unify_gen_util.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_type.

:- import_module maybe.
:- import_module require.
:- import_module term.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

ml_generate_test_var_has_cons_id(Var, ConsId, TestRval, !Info) :-
    % NOTE: Keep in sync with ml_generate_test_var_has_tagged_cons_id below.
    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),
    ml_variable_type(!.Info, Var, VarType),
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
    ml_get_maybe_cheaper_tag_test(!.Info, VarType, CheaperTagTest),
    ml_generate_test_rval_has_cons_tag(!.Info, VarRval, VarType,
        CheaperTagTest, ConsTag, TestRval).

%---------------------------------------------------------------------------%

ml_generate_test_var_has_one_tagged_cons_id(Var,
        MainTaggedConsId, OtherTaggedConsIds, TestRval, !Info) :-
    % NOTE: Keep in sync with ml_generate_test_var_has_cons_id above.

    ml_gen_var(!.Info, Var, VarLval),
    VarRval = ml_lval(VarLval),
    ml_variable_type(!.Info, Var, VarType),
    ml_get_maybe_cheaper_tag_test(!.Info, VarType, CheaperTagTest),

    ml_generate_test_rval_has_tagged_cons_id(!.Info, VarRval, VarType,
        CheaperTagTest, MainTaggedConsId, MainTestRval),
    list.map(
        ml_generate_test_rval_has_tagged_cons_id(!.Info, VarRval, VarType,
            CheaperTagTest),
        OtherTaggedConsIds, OtherTestRvals),
    ml_logical_or_rvals(MainTestRval, OtherTestRvals, TestRval).

    % logical_or_rvals(FirstRval, LaterRvals, Rval):
    %
    % Rval is true iff any one of FirstRval and LaterRvals is true.
    %
:- pred ml_logical_or_rvals(mlds_rval::in, list(mlds_rval)::in, mlds_rval::out)
    is det.

ml_logical_or_rvals(FirstRval, LaterRvals, Rval) :-
    (
        LaterRvals = [],
        Rval = FirstRval
    ;
        LaterRvals = [SecondRval | OtherRvals],
        FirstSecondRval = ml_binop(logical_or, FirstRval, SecondRval),
        ml_logical_or_rvals(FirstSecondRval, OtherRvals, Rval)
    ).

%---------------------------------------------------------------------------%

:- pred ml_generate_test_rval_has_tagged_cons_id(ml_gen_info::in,
    mlds_rval::in, mer_type::in, maybe_cheaper_tag_test::in,
    tagged_cons_id::in, mlds_rval::out) is det.

ml_generate_test_rval_has_tagged_cons_id(Info, Rval, Type, CheaperTagTest,
        TaggedConsId, TestRval) :-
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    ml_generate_test_rval_has_cons_tag(Info, Rval, Type, CheaperTagTest,
        ConsTag, TestRval).

    % ml_generate_test_rval_has_cons_tag(Info, VarRval, VarType, ConsTag,
    %   TestRval):
    %
    % TestRval is an rval of type bool which evaluates to true if VarRval,
    % which has type VarType, has the specified ConsTag, and false otherwise.
    %
:- pred ml_generate_test_rval_has_cons_tag(ml_gen_info::in,
    mlds_rval::in, mer_type::in, maybe_cheaper_tag_test::in, cons_tag::in,
    mlds_rval::out) is det.

ml_generate_test_rval_has_cons_tag(Info, VarRval, VarType, CheaperTagTest,
        ConsTag, TestRval) :-
    ( if
        CheaperTagTest = cheaper_tag_test(_ExpensiveConsId, ExpensiveConsTag,
            _CheapConsId, CheapConsTag),
        ConsTag = ExpensiveConsTag
    then
        ml_generate_test_rval_has_cons_tag_direct(Info, VarRval, VarType,
            CheapConsTag, CheapConsTagTestRval),
        ( if
            CheapConsTagTestRval = ml_binop(eq(IntType), SubRvalA, SubRvalB)
        then
            TestRval = ml_binop(ne(IntType), SubRvalA, SubRvalB)
        else
            TestRval = ml_unop(logical_not, CheapConsTagTestRval)
        )
    else
        ml_generate_test_rval_has_cons_tag_direct(Info, VarRval, VarType,
            ConsTag, TestRval)
    ).

:- pred ml_generate_test_rval_has_cons_tag_direct(ml_gen_info::in,
    mlds_rval::in, mer_type::in, cons_tag::in, mlds_rval::out) is det.

ml_generate_test_rval_has_cons_tag_direct(Info, VarRval, Type,
        ConsTag, TestRval) :-
    (
        ConsTag = int_tag(IntTag),
        ml_gen_info_get_module_info(Info, ModuleInfo),
        ml_generate_test_rval_is_int_tag(ModuleInfo, VarRval, Type, IntTag,
            TestRval)
    ;
        ConsTag = float_tag(Float),
        TestRval = ml_binop(float_eq, VarRval, ml_const(mlconst_float(Float)))
    ;
        ConsTag = string_tag(String),
        TestRval = ml_binop(str_eq, VarRval, ml_const(mlconst_string(String)))
    ;
        ConsTag = foreign_tag(ForeignLang, ForeignVal),
        ml_gen_info_get_module_info(Info, ModuleInfo),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
        TestRval = ml_binop(eq(int_type_int), VarRval,
            ml_const(mlconst_foreign(ForeignLang, ForeignVal, MLDS_Type)))
    ;
        ( ConsTag = dummy_tag
        ; ConsTag = no_tag
        ),
        % In a type with only one cons_id, all vars have that one cons_id.
        TestRval = ml_const(mlconst_true)
    ;
        ConsTag = direct_arg_tag(Ptag),
        VarPtag = ml_unop(tag, VarRval),
        Ptag = ptag(PtagUint8),
        % XXX ARG_PACK We should get the tag unop to return an unsigned int,
        % to make using an unsigned comparison here simpler.
        PtagConstRval = ml_const(mlconst_int(uint8.cast_to_int(PtagUint8))),
        TestRval = ml_binop(eq(int_type_int), VarPtag, PtagConstRval)
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            RemoteArgsTagInfo = remote_args_only_functor,
            % In a type with only one cons_id, all vars have that one cons_id.
            TestRval = ml_const(mlconst_true)
        ;
            RemoteArgsTagInfo = remote_args_unshared(Ptag),
            VarPtag = ml_unop(tag, VarRval),
            Ptag = ptag(PtagUint8),
            PtagConstRval =
                ml_const(mlconst_int(uint8.cast_to_int(PtagUint8))),
            TestRval = ml_binop(eq(int_type_int), VarPtag, PtagConstRval)
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            VarPtag = ml_unop(tag, VarRval),
            Ptag = ptag(PtagUint8),
            ConstPtagRval =
                ml_const(mlconst_int(uint8.cast_to_int(PtagUint8))),
            PtagTestRval = ml_binop(eq(int_type_int), VarPtag, ConstPtagRval),
            ml_gen_secondary_tag_rval(Info, Type, VarRval, Ptag,
                VarSectagWordRval),
            RemoteSectag = remote_sectag(SectagUint, SectagSize),
            (
                SectagSize = rsectag_word,
                VarSectagRval = VarSectagWordRval
            ;
                SectagSize = rsectag_subword(SectagBits),
                SectagBits = sectag_bits(_NumSectagBits, SectagMask),
                VarSectagRval = ml_binop(bitwise_and(int_type_uint),
                    VarSectagWordRval, ml_const(mlconst_uint(SectagMask)))
            ),
            ConstSectagRval =
                ml_const(mlconst_int(uint.cast_to_int(SectagUint))),
            SectagTestRval = ml_binop(eq(int_type_int),
                VarSectagRval, ConstSectagRval),
            TestRval = ml_binop(logical_and, PtagTestRval, SectagTestRval)
        ;
            RemoteArgsTagInfo = remote_args_ctor(Data),
            Ptag = ptag(0u8),
            ml_gen_secondary_tag_rval(Info, Type, VarRval, Ptag,
                VarSectagRval),
            ConstSectagRval = ml_const(mlconst_int(uint.cast_to_int(Data))),
            TestRval = ml_binop(eq(int_type_int),
                VarSectagRval, ConstSectagRval)
        )
    ;
        ConsTag = local_args_tag(LocalArgsTagInfo),
        (
            LocalArgsTagInfo = local_args_only_functor,
            % In a type with only one cons_id, all vars have that one cons_id.
            TestRval = ml_const(mlconst_true)
        ;
            LocalArgsTagInfo = local_args_not_only_functor(_Ptag, LocalSectag),
            % We generate the same test as for shared_local_tag_no_args
            % with lsectag_must_be_masked.
            LocalSectag = local_sectag(_Sectag, PrimSec, SectagBits),
            ConstPrimSecRval = ml_const(mlconst_uint(PrimSec)),

            ml_gen_info_get_num_ptag_bits(Info, NumPtagBits),
            SectagBits = sectag_bits(NumSectagBits, _SectagMask),
            NumPtagSectagBits = uint8.cast_to_int(NumPtagBits + NumSectagBits),
            PrimSecMask = (1u << NumPtagSectagBits) - 1u,
            MaskedVarRval = ml_binop(bitwise_and(int_type_uint),
                VarRval, ml_const(mlconst_uint(PrimSecMask))),

            % There is no need for a cast, since the Java backend
            % does not support local secondary tags that must be masked.
            TestRval = ml_binop(eq(int_type_uint),
                MaskedVarRval, ConstPrimSecRval)
        )
    ;
        ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, MustMask),
        LocalSectag = local_sectag(_Sectag, PrimSec, SectagBits),
        ConstPrimSecRval = ml_const(mlconst_uint(PrimSec)),
        (
            MustMask = lsectag_always_rest_of_word,
            ml_gen_info_get_module_info(Info, ModuleInfo),
            MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
            % The cast is needed only by the Java backend.
            TestRval = ml_binop(eq(int_type_int),
                VarRval, ml_cast(MLDS_Type, ConstPrimSecRval))
        ;
            MustMask = lsectag_must_be_masked,
            % We generate the same test as for shared_local_tag_with_args.
            ml_gen_info_get_num_ptag_bits(Info, NumPtagBits),
            SectagBits = sectag_bits(NumSectagBits, _SectagMask),
            NumPtagSectagBits = uint8.cast_to_int(NumPtagBits + NumSectagBits),
            PrimSecMask = (1u << NumPtagSectagBits) - 1u,
            MaskedVarRval = ml_binop(bitwise_and(int_type_uint),
                VarRval, ml_const(mlconst_uint(PrimSecMask))),
            % There is no need for a cast, since the Java backend
            % does not support local secondary tags that must be masked.
            TestRval = ml_binop(eq(int_type_uint),
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
        unexpected($pred, "unexpacted ConsTag")
    ).

:- pred ml_generate_test_rval_is_int_tag(module_info::in, mlds_rval::in,
    mer_type::in, int_tag::in, mlds_rval::out) is det.

ml_generate_test_rval_is_int_tag(ModuleInfo, Rval, Type, IntTag, TestRval) :-
    % Keep this code in sync with ml_int_tag_to_rval_const in ml_code_util.m.
    (
        IntTag = int_tag_int(Int),
        ( if Type = int_type then
            Const = mlconst_int(Int)
        else if Type = char_type then
            Const = mlconst_char(Int)
        else
            MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
            Const = mlconst_enum(Int, MLDS_Type)
        ),
        EqType = int_type_int
    ;
        IntTag = int_tag_uint(UInt),
        EqType = int_type_uint,
        Const = mlconst_uint(UInt)
    ;
        IntTag = int_tag_int8(Int8),
        EqType = int_type_int8,
        Const = mlconst_int8(Int8)
    ;
        IntTag = int_tag_uint8(UInt8),
        EqType = int_type_uint8,
        Const = mlconst_uint8(UInt8)
    ;
        IntTag = int_tag_int16(Int16),
        EqType = int_type_int16,
        Const = mlconst_int16(Int16)
    ;
        IntTag = int_tag_uint16(UInt16),
        EqType = int_type_uint16,
        Const = mlconst_uint16(UInt16)
    ;
        IntTag = int_tag_int32(Int32),
        EqType = int_type_int32,
        Const = mlconst_int32(Int32)
    ;
        IntTag = int_tag_uint32(UInt32),
        EqType = int_type_uint32,
        Const = mlconst_uint32(UInt32)
    ;
        IntTag = int_tag_int64(Int64),
        EqType = int_type_int64,
        Const = mlconst_int64(Int64)
    ;
        IntTag = int_tag_uint64(UInt64),
        EqType = int_type_uint64,
        Const = mlconst_uint64(UInt64)
    ),
    TestRval = ml_binop(eq(EqType), Rval, ml_const(Const)).

%---------------------------------------------------------------------------%

:- pred ml_get_maybe_cheaper_tag_test(ml_gen_info::in, mer_type::in,
    maybe_cheaper_tag_test::out) is det.

ml_get_maybe_cheaper_tag_test(Info, Type, CheaperTagTest) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    type_to_ctor_det(Type, TypeCtor),
    module_info_get_type_table(ModuleInfo, TypeTable),
    ( if
        search_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_body(TypeDefn, TypeBody),
        TypeBody = hlds_du_type(type_body_du(_, _, _, MaybeRepn, _)),
        MaybeRepn = yes(Repn)
    then
        CheaperTagTest = Repn ^ dur_cheaper_tag_test
    else
        CheaperTagTest = no_cheaper_tag_test
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_unify_gen_test.
%---------------------------------------------------------------------------%
