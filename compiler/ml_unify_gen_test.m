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

%---------------------------------------------------------------------------%

    % ml_gen_tag_test(Var, ConsId, Rval, !Info):
    %
    % Generate code to perform a tag test.
    %
    % The test checks whether Var has the functor specified by ConsId.
    % The generated code will not contain Defns or Stmts. It will be
    % the boolean Rval, which will evaluate to true iff the Var has
    % the functor specified by ConsId.
    %
:- pred ml_gen_tag_test(prog_var::in, cons_id::in, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%

    % ml_gen_known_tag_test(Var, TaggedConsId, Rval, !Info):
    %
    % Generate code to perform a tag test.
    %
    % The test checks whether Var has the functor specified by TaggedConsId.
    % The generated code will not contain Defns or Stmts. It will be
    % the boolean Rval, which will evaluate to true iff the Var has
    % the functor specified by ConsId.
    %
    % (The "known" part of the name refers to the fact that the tag of
    % the cons_id is already known.)
    %
    % Exported for use by ml_switch_gen.m.
    %
:- pred ml_gen_known_tag_test(prog_var::in, tagged_cons_id::in, mlds_rval::out,
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

:- import_module require.
:- import_module term.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

ml_gen_tag_test(Var, ConsId, TagTestExpr, !Info) :-
    % NOTE: Keep in sync with ml_gen_known_tag_test below.

    % TODO: apply the reverse tag test optimization for types with two
    % functors (see unify_gen.m).

    ml_gen_var(!.Info, Var, VarLval),
    ml_variable_type(!.Info, Var, Type),
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
    TagTestExpr =
        ml_gen_tag_test_rval(!.Info, ConsTag, Type, ml_lval(VarLval)).

%---------------------------------------------------------------------------%

ml_gen_known_tag_test(Var, TaggedConsId, TagTestExpr, !Info) :-
    % NOTE: Keep in sync with ml_gen_tag_test above.

    % TODO: apply the reverse tag test optimization for types with two
    % functors (see unify_gen.m).

    ml_gen_var(!.Info, Var, VarLval),
    ml_variable_type(!.Info, Var, Type),
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    TagTestExpr =
        ml_gen_tag_test_rval(!.Info, ConsTag, Type, ml_lval(VarLval)).

%---------------------------------------------------------------------------%

    % ml_gen_tag_test_rval(Info, ConsTag, Type, VarRval) = TestRval:
    %
    % TestRval is an rval of type bool which evaluates to true if VarRval has
    % the specified ConsTag, and false otherwise. Type is the type of VarRval.
    %
:- func ml_gen_tag_test_rval(ml_gen_info, cons_tag, mer_type, mlds_rval)
    = mlds_rval.

ml_gen_tag_test_rval(Info, ConsTag, Type, Rval) = TagTestRval :-
    (
        ConsTag = string_tag(String),
        TagTestRval = ml_binop(str_eq, Rval, ml_const(mlconst_string(String)))
    ;
        ConsTag = float_tag(Float),
        TagTestRval = ml_binop(float_eq, Rval, ml_const(mlconst_float(Float)))
    ;
        ConsTag = int_tag(IntTag),
        ml_gen_info_get_module_info(Info, ModuleInfo),
        TagTestRval = ml_gen_int_tag_test_rval(IntTag, Type, ModuleInfo, Rval)
    ;
        ConsTag = foreign_tag(ForeignLang, ForeignVal),
        ml_gen_info_get_module_info(Info, ModuleInfo),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
        Const = ml_const(mlconst_foreign(ForeignLang, ForeignVal, MLDS_Type)),
        TagTestRval = ml_binop(eq(int_type_int), Rval, Const)
    ;
        ( ConsTag = dummy_tag
        ; ConsTag = no_tag
        ; ConsTag = single_functor_tag
        ),
        % In a type with only one value, all equality tests succeed.
        % In a type with only one ptag value, all equality tests on ptags
        % succeed.
        TagTestRval = ml_const(mlconst_true)
    ;
        ( ConsTag = unshared_tag(Ptag)
        ; ConsTag = direct_arg_tag(Ptag)
        ),
        RvalTag = ml_unop(tag, Rval),
        Ptag = ptag(PtagUint8),
        PrimaryTagRval = ml_const(mlconst_int(uint8.cast_to_int(PtagUint8))),
        TagTestRval = ml_binop(eq(int_type_int), RvalTag, PrimaryTagRval)
    ;
        ConsTag = shared_remote_tag(Ptag, RemoteSectag),
        ml_gen_secondary_tag_rval(Info, Type, Rval, Ptag,
            SecondaryTagFieldRval),
        RemoteSectag = remote_sectag(SectagUint, _),
        SecondaryTagTestRval = ml_binop(eq(int_type_int),
            SecondaryTagFieldRval,
            ml_const(mlconst_int(uint.cast_to_int(SectagUint)))),
        ml_gen_info_get_num_ptag_bits(Info, NumPtagBits),
        ( if NumPtagBits = 0u8 then
            % No need to test the primary tag.
            TagTestRval = SecondaryTagTestRval
        else
            RvalPtag = ml_unop(tag, Rval),
            Ptag = ptag(PtagUint8),
            PrimaryTagRval =
                ml_const(mlconst_int(uint8.cast_to_int(PtagUint8))),
            PrimaryTagTestRval = ml_binop(eq(int_type_int), RvalPtag,
                PrimaryTagRval),
            TagTestRval = ml_binop(logical_and,
                PrimaryTagTestRval, SecondaryTagTestRval)
        )
    ;
        ConsTag = shared_local_tag_with_args(_Ptag, LocalSectag),
        % We handle this the same was as the lsectag_must_be_masked case
        % of shared_local_tag_no_args below.
        LocalSectag = local_sectag(_Sectag, PrimSec, SectagBits),
        ml_gen_info_get_num_ptag_bits(Info, NumPtagBits),
        SectagBits = sectag_bits(NumSectagBits, _SectagMask),
        NumPtagSectagBits = uint8.cast_to_int(NumPtagBits + NumSectagBits),
        PrimSecMask = (1u << NumPtagSectagBits) - 1u,
        % There is no need for a cast, since the Java backend
        % does not support local secondary tags that must be masked.
        TagTestRval = ml_binop(eq(int_type_uint),
            ml_binop(bitwise_and(int_type_uint),
                Rval, ml_const(mlconst_uint(PrimSecMask))),
            ml_const(mlconst_uint(PrimSec)))
    ;
        ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, MustMask),
        LocalSectag = local_sectag(_Sectag, PrimSec, SectagBits),
        (
            MustMask = lsectag_always_rest_of_word,
            ml_gen_info_get_module_info(Info, ModuleInfo),
            MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
            % The cast is needed only by the Java backend.
            TagTestRval = ml_binop(eq(int_type_int), Rval,
                ml_cast(MLDS_Type, ml_const(mlconst_uint(PrimSec))))
        ;
            MustMask = lsectag_must_be_masked,
            % We handle this the same was as shared_local_tag_with_args above.
            ml_gen_info_get_num_ptag_bits(Info, NumPtagBits),
            SectagBits = sectag_bits(NumSectagBits, _SectagMask),
            NumPtagSectagBits = uint8.cast_to_int(NumPtagBits + NumSectagBits),
            PrimSecMask = (1u << NumPtagSectagBits) - 1u,
            % There is no need for a cast, since the Java backend
            % does not support local secondary tags that must be masked.
            TagTestRval = ml_binop(eq(int_type_uint),
                ml_binop(bitwise_and(int_type_uint),
                    Rval, ml_const(mlconst_uint(PrimSecMask))),
                ml_const(mlconst_uint(PrimSec)))
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
        unexpected($pred, "bad tag")
    ).

:- func ml_gen_int_tag_test_rval(int_tag, mer_type, module_info, mlds_rval) =
    mlds_rval.

ml_gen_int_tag_test_rval(IntTag, Type, ModuleInfo, Rval) = TagTestRval :-
    (
        IntTag = int_tag_int(Int),
        ( if Type = int_type then
            ConstRval = ml_const(mlconst_int(Int))
        else if Type = char_type then
            ConstRval = ml_const(mlconst_char(Int))
        else
            MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
            ConstRval = ml_const(mlconst_enum(Int, MLDS_Type))
        ),
        TagTestRval = ml_binop(eq(int_type_int), Rval, ConstRval)
    ;
        IntTag = int_tag_uint(UInt),
        TagTestRval = ml_binop(eq(int_type_uint), Rval,
            ml_const(mlconst_uint(UInt)))
    ;
        IntTag = int_tag_int8(Int8),
        TagTestRval = ml_binop(eq(int_type_int8), Rval,
            ml_const(mlconst_int8(Int8)))
    ;
        IntTag = int_tag_uint8(UInt8),
        TagTestRval = ml_binop(eq(int_type_uint8), Rval,
            ml_const(mlconst_uint8(UInt8)))
    ;
        IntTag = int_tag_int16(Int16),
        TagTestRval = ml_binop(eq(int_type_int16), Rval,
            ml_const(mlconst_int16(Int16)))
    ;
        IntTag = int_tag_uint16(UInt16),
        TagTestRval = ml_binop(eq(int_type_uint16), Rval,
            ml_const(mlconst_uint16(UInt16)))
    ;
        IntTag = int_tag_int32(Int32),
        TagTestRval = ml_binop(eq(int_type_int32), Rval,
            ml_const(mlconst_int32(Int32)))
    ;
        IntTag = int_tag_uint32(UInt32),
        TagTestRval = ml_binop(eq(int_type_uint32), Rval,
            ml_const(mlconst_uint32(UInt32)))
    ;
        IntTag = int_tag_int64(Int64),
        TagTestRval = ml_binop(eq(int_type_int64), Rval,
            ml_const(mlconst_int64(Int64)))
    ;
        IntTag = int_tag_uint64(UInt64),
        TagTestRval = ml_binop(eq(int_type_uint64), Rval,
            ml_const(mlconst_uint64(UInt64)))
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_unify_gen_test.
%---------------------------------------------------------------------------%
