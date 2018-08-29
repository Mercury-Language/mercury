%---------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------e
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module ll_backend.unify_gen_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred int_tag_to_const_and_int_type(int_tag::in, rval_const::out,
    int_type::out) is det.

%---------------------------------------------------------------------------%

:- type arg_and_width(Arg)
    --->    arg_and_width(Arg, arg_pos_width).

:- pred associate_cons_id_args_with_widths(module_info::in, cons_id::in,
    list(Arg)::in, list(arg_and_width(Arg))::out) is det.

%---------------------------------------------------------------------------%

    % OR together the given rvals.
    %
:- func bitwise_or_rvals(list(rval)) = rval.
:- func bitwise_or_some_rvals(rval, list(rval)) = rval.
:- func bitwise_or_two_rvals(rval, rval) = rval.

:- func left_shift_rval(rval, arg_shift, fill_kind) = rval.

:- func right_shift_rval(rval, arg_shift) = rval.

%---------------------------------------------------------------------------%

:- type maybe_zero_const
    --->    is_not_zero_const
    ;       is_zero_const.

:- func is_zero_const(rval_const) = maybe_zero_const.

%---------------------------------------------------------------------------%

    % If a sub-word-sized signed integer has a negative value, then it will
    % have sign-extend bits *beyond* its usual size. OR-ing the raw form
    % of that sub-word-sized signed integer with the values of the other fields
    % may thus stomp all over the bits assigned to store the other fields
    % that are to the left of the sub-word-sized signed integer.
    %
    % Prevent this by casting sub-word-sized signed integers to their
    % unsigned counterparts before casting them to the word-sized unsigned type
    % that is the usual input type of shift and OR operations.
    %
:- pred cast_to_unsigned_without_sign_extend(fill_kind::in,
    rval::in, rval::out) is det.

:- pred maybe_cast_masked_off_rval(fill_kind::in, rval::in, rval::out) is det.

%---------------------------------------------------------------------------%

:- type assign_dir
    --->    assign_left
    ;       assign_right
    ;       assign_unused.

    % Figure out in which direction the assignment goes
    % between a field of a term, and the corresponding argument.
    %
:- pred compute_assign_direction(module_info::in, unify_mode::in, mer_type::in,
    assign_dir::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module term.

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

associate_cons_id_args_with_widths(ModuleInfo, ConsId, AllArgs,
        AllArgsPosWidths) :-
    ( if get_cons_repn_defn(ModuleInfo, ConsId, ConsRepnDefn) then
        ConsArgRepns = ConsRepnDefn ^ cr_args,
        ConsTag = ConsRepnDefn ^ cr_tag,
        list.length(AllArgs, NumAllArgs),
        list.length(ConsArgRepns, NumConsArgs),
        NumExtraArgs = NumAllArgs - NumConsArgs,
        ( if NumExtraArgs = 0 then
            zip_args_widths(AllArgs, ConsArgRepns, AllArgsPosWidths)
        else if NumExtraArgs > 0 then
            list.det_split_list(NumExtraArgs, AllArgs, ExtraArgs, ConsArgs),
            ( if
                ConsTag = remote_args_tag(RemoteArgsTagInfo),
                RemoteArgsTagInfo = remote_args_shared(_, RemoteSecTag),
                RemoteSecTag = remote_sectag(_, SectagSize),
                SectagSize = rsectag_word
            then
                InitOffset = 1
            else
                InitOffset = 0
            ),
            allocate_consecutive_full_words(InitOffset,
                ExtraArgs, ExtraArgsPosWidths),
            zip_args_widths(ConsArgs, ConsArgRepns, ConsArgsPosWidths),
            AllArgsPosWidths = ExtraArgsPosWidths ++ ConsArgsPosWidths
        else
            unexpected($pred, "too few arguments")
        )
    else
        allocate_consecutive_full_words(0, AllArgs, AllArgsPosWidths)
    ).

:- pred zip_args_widths(list(Arg)::in,
    list(constructor_arg_repn)::in, list(arg_and_width(Arg))::out) is det.

zip_args_widths([], [], []).
zip_args_widths([], [_ | _], _) :-
    unexpected($pred, "length mismatch").
zip_args_widths([_ | _], [], _) :-
    unexpected($pred, "length mismatch").
zip_args_widths([Arg | Args], [ConsArgRepn | ConsArgRepns],
        [ArgTypeWidth | ArgsTypesWidth]) :-
    ArgTypeWidth = arg_and_width(Arg, ConsArgRepn ^ car_pos_width),
    zip_args_widths(Args, ConsArgRepns, ArgsTypesWidth).

    % The initial offset that our callers should specify
    % depends on the absence/presence of a secondary tag.
    %
:- pred allocate_consecutive_full_words(int::in,
    list(Arg)::in, list(arg_and_width(Arg))::out) is det.

allocate_consecutive_full_words(_, [], []).
allocate_consecutive_full_words(CurOffset,
        [Arg | Args], [ArgPosWidth | ArgsPosWidths]) :-
    PosWidth = apw_full(arg_only_offset(CurOffset), cell_offset(CurOffset)),
    ArgPosWidth = arg_and_width(Arg, PosWidth),
    allocate_consecutive_full_words(CurOffset + 1, Args, ArgsPosWidths).

%---------------------------------------------------------------------------%

bitwise_or_rvals(Rvals) = OrAllRval :-
    (
        Rvals = [],
        OrAllRval = const(llconst_int(0))
    ;
        Rvals = [HeadRval | TailRvals],
        OrAllRval = bitwise_or_some_rvals(HeadRval, TailRvals)
    ).

bitwise_or_some_rvals(HeadRval, TailRvals) = OrAllRval :-
    % We currently do this a linear fashion, starting at the rightmost
    % arguments, and moving towards the left.
    %
    % We could explore whether other strategies, such as balanced trees,
    % (or rather, trees that are as balanced as possible) would work better.
    (
        TailRvals = [],
        OrAllRval = HeadRval
    ;
        TailRvals = [HeadTailRval | TailTailRvals],
        TailOrAllRval = bitwise_or_some_rvals(HeadTailRval, TailTailRvals),
        OrAllRval = bitwise_or_two_rvals(HeadRval, TailOrAllRval)
    ).

bitwise_or_two_rvals(RvalA, RvalB) = OrRval :-
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

left_shift_rval(Rval, Shift, Fill) = ShiftedUnsignedRval :-
    Shift = arg_shift(ShiftInt),
    cast_to_unsigned_without_sign_extend(Fill, Rval, UnsignedRval),
    ( if
        (
            % Shifting anything by zero bits has no effect.
            ShiftInt = 0
        ;
            % Shifting zero any number of bits has no effect.
            Rval = const(Const),
            is_zero_const(Const) = is_zero_const
        )
    then
        ShiftedUnsignedRval = UnsignedRval
    else
        ShiftedUnsignedRval = binop(unchecked_left_shift(int_type_uint),
            UnsignedRval, const(llconst_int(ShiftInt)))
    ).

right_shift_rval(Rval, Shift) = ShiftedRval :-
    Shift = arg_shift(ShiftInt),
    % Shifting anything by zero bits has no effect.
    % Shifting zero any number of bits has no effect.
    % However, our caller won't give us either a zero shift amount
    % or a constant zero rval to shift.
    % XXX ARG_PACK Should we cast Rval to unsigned like left_shift_rval?
    ShiftedRval = binop(unchecked_right_shift(int_type_uint),
        Rval, const(llconst_int(ShiftInt))).

%---------------------------------------------------------------------------%

is_zero_const(Const) = IsZero :-
    (
        Const = llconst_int(Int),
        IsZero = (if Int = 0 then is_zero_const else is_not_zero_const)
    ;
        Const = llconst_uint(Uint),
        IsZero = (if Uint = 0u then is_zero_const else is_not_zero_const)
    ;
        Const = llconst_int8(Int8),
        IsZero = (if Int8 = 0i8 then is_zero_const else is_not_zero_const)
    ;
        Const = llconst_uint8(Uint8),
        IsZero = (if Uint8 = 0u8 then is_zero_const else is_not_zero_const)
    ;
        Const = llconst_int16(Int16),
        IsZero = (if Int16 = 0i16 then is_zero_const else is_not_zero_const)
    ;
        Const = llconst_uint16(Uint16),
        IsZero = (if Uint16 = 0u16 then is_zero_const else is_not_zero_const)
    ;
        Const = llconst_int32(Int32),
        IsZero = (if Int32 = 0i32 then is_zero_const else is_not_zero_const)
    ;
        Const = llconst_uint32(Uint32),
        IsZero = (if Uint32 = 0u32 then is_zero_const else is_not_zero_const)
    ;
        Const = llconst_int64(Int64),
        IsZero = (if Int64 = 0i64 then is_zero_const else is_not_zero_const)
    ;
        Const = llconst_uint64(Uint64),
        IsZero = (if Uint64 = 0u64 then is_zero_const else is_not_zero_const)
    ;
        ( Const = llconst_true
        ; Const = llconst_false
        ; Const = llconst_foreign(_, _)
        ; Const = llconst_float(_)
        ; Const = llconst_string(_)
        ; Const = llconst_multi_string(_)
        ; Const = llconst_code_addr(_)
        ; Const = llconst_data_addr(_, _)
        ),
        IsZero = is_not_zero_const
    ).

%---------------------------------------------------------------------------%

cast_to_unsigned_without_sign_extend(Fill, Rval0, Rval) :-
    (
        ( Fill = fill_enum
        ; Fill = fill_uint8
        ; Fill = fill_uint16
        ; Fill = fill_uint32
        ),
        Rval1 = Rval0
    ;
        Fill = fill_int8,
        Rval1 = cast(lt_int(int_type_uint8), Rval0)
    ;
        Fill = fill_int16,
        Rval1 = cast(lt_int(int_type_uint16), Rval0)
    ;
        Fill = fill_int32,
        Rval1 = cast(lt_int(int_type_uint32), Rval0)
    ),
    Rval = cast(lt_int(int_type_uint), Rval1).

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

compute_assign_direction(ModuleInfo, ArgMode, ArgType, Dir) :-
    % Any change here will require a corresponding change
    % in ml_compute_assign_direction.
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
:- end_module ll_backend.unify_gen_util.
%---------------------------------------------------------------------------%
