%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012, 2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module ml_backend.ml_unify_gen_deconstruct.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.ml_unify_gen_util.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.

%---------------------------------------------------------------------------%

    % Generate a semidet deconstruction. A semidet deconstruction unification
    % is a tag test, followed by a deterministic deconstruction which is
    % executed only if the tag test succeeds.
    %
    %   semidet (can_fail) deconstruction:
    %       <succeeded = (X => f(A1, A2, ...))>
    %   ===>
    %       <succeeded = (X => f(_, _, _, _))>  % tag test
    %       if (succeeded) {
    %           A1 = arg(X, f, 1);      % extract arguments
    %           A2 = arg(X, f, 2);
    %           ...
    %       }
    %
:- pred ml_gen_semi_deconstruct(prog_var::in, cons_id::in, list(prog_var)::in,
    list(unify_mode)::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%

    % Generate a deterministic deconstruction. In a deterministic
    % deconstruction, we know the value of the tag, so we don't need
    % to generate a test.
    %
    %   det (cannot_fail) deconstruction:
    %       <do (X => f(A1, A2, ...))>
    %   ===>
    %       A1 = arg(X, f, 1);      % extract arguments
    %       A2 = arg(X, f, 2);
    %       ...
    %
:- pred ml_gen_det_deconstruct(prog_var::in, cons_id::in, list(prog_var)::in,
    list(unify_mode)::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%
% We exxported ml_gen_dynamic_deconstruct_args for use
% by ml_unify_gen_construct.m when handling reused cells.
%
% While deconstruct unifications cannot take the addresses of any arguments,
% construction unifications with reuse can.
%

:- type take_addr_info
    --->    take_addr_info(
                % The variable we record the address in.
                tai_address_var             :: prog_var,

                % The offset of the field. This must take into account
                % extra arguments and argument packing.
                tai_offset                  :: cell_offset,

                % The type of the field variable.
                tai_field_var_type          :: mlds_type,

                % The type of the field, possibly after boxing.
                tai_maybe_boxed_field_type  :: mlds_type
            ).

:- pred ml_gen_dynamic_deconstruct_args(field_gen,
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    int, prog_context, list(int), list(take_addr_info),
    list(mlds_local_var_defn), list(mlds_stmt), ml_gen_info, ml_gen_info).
:- mode ml_gen_dynamic_deconstruct_args(in, in, in, in, in,
    in(bound([])), out, out, out, in, out) is det.
:- mode ml_gen_dynamic_deconstruct_args(in, in, in, in, in,
    in, out, out, out, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_unify_gen_test.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data_foreign.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

ml_gen_semi_deconstruct(Var, ConsId, ArgVars, ArgModes, Context,
        Defns, Stmts, !Info) :-
    ml_gen_tag_test(Var, ConsId, TagTestRval, !Info),
    ml_gen_set_success(TagTestRval, Context, SetTagTestResult, !Info),
    ml_gen_test_success(SucceededRval, !Info),
    ml_gen_det_deconstruct(Var, ConsId, ArgVars, ArgModes, Context,
        Defns, GetArgsStmts, !Info),
    (
        GetArgsStmts = [],
        Stmts = [SetTagTestResult]
    ;
        GetArgsStmts = [_ | _],
        GetArgs = ml_gen_block([], [], GetArgsStmts, Context),
        IfStmt = ml_stmt_if_then_else(SucceededRval, GetArgs, no, Context),
        Stmts = [SetTagTestResult, IfStmt]
    ).

%---------------------------------------------------------------------------%

ml_gen_det_deconstruct(Var, ConsId, ArgVars, Modes, Context,
        Defns, Stmts, !Info) :-
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
    (
        ( ConsTag = string_tag(_String)
        ; ConsTag = int_tag(_IntTag)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = float_tag(_Float)
        ; ConsTag = dummy_tag
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ),
        % For constants, if the deconstruction is det, then we already know
        % the value of the constant, so Stmts = [].
        Defns = [],
        Stmts = []
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
        unexpected($pred, "unexpected tag")
    ;
        ConsTag = no_tag,
        ( if
            ArgVars = [ArgVar],
            Modes = [Mode]
        then
            ml_gen_dynamic_deconstruct_no_tag(!.Info, Mode, ArgVar, Var,
                Context, Stmts),
            Defns = []
        else
            unexpected($pred, "no_tag: arity != 1")
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        ( if
            ArgVars = [ArgVar],
            Modes = [Mode]
        then
            ml_gen_dynamic_deconstruct_direct_arg(!.Info, Ptag, Mode,
                ArgVar, Var, Context, Stmts),
            Defns = []
        else
            unexpected($pred, "direct_arg_tag: arity != 1")
        )
    ;
        ( ConsTag = single_functor_tag
        ; ConsTag = unshared_tag(_Ptag)
        ; ConsTag = shared_remote_tag(_Ptag, _)
        ),
        ml_variable_type(!.Info, Var, VarType),
        ml_gen_var(!.Info, Var, VarLval),
        decide_field_gen(!.Info, VarLval, VarType, ConsId, ConsTag, FieldGen),
        ml_tag_initial_offset_and_argnum(ConsTag, _, InitOffSet, ArgNum),
        ml_field_names_and_types(!.Info, VarType, ConsId, InitOffSet, ArgVars,
            ArgVarRepns),
        ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, Modes,
            ArgNum, Context, [], _, Defns, Stmts, !Info)
    ;
        ConsTag = shared_local_tag_with_args(_, _),
        ml_gen_var(!.Info, Var, VarLval),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        get_cons_repn_defn_det(ModuleInfo, ConsId, ConsRepnDefn),
        CtorArgRepns = ConsRepnDefn ^ cr_args,
        assoc_list.from_corresponding_lists(ArgVars, CtorArgRepns,
            ArgVarRepns),
        ml_gen_deconstruct_tagword_args(!.Info, ml_lval(VarLval),
            ArgVarRepns, Modes, Context, [], ToOrRvals, 0u, ToOrMask,
            RightStmts),
        (
            ToOrRvals = [],
            Stmts = RightStmts
        ;
            ToOrRvals = [HeadToOrRval | TailToOrRvals],
            ComplementMask = ml_const(mlconst_uint(\ ToOrMask)),
            MaskedOldVarRval = ml_binop(bitwise_and(int_type_uint),
                ml_lval(VarLval), ComplementMask),
            or_rvals(HeadToOrRval, TailToOrRvals, ToOrRval),
            NewVarRval = ml_binop(bitwise_or(int_type_uint),
                MaskedOldVarRval, ToOrRval),
            LeftStmt = ml_gen_assign(VarLval, NewVarRval, Context),
            Stmts = [LeftStmt | RightStmts]
        ),
        Defns = []
    ).

ml_gen_dynamic_deconstruct_args(_, [], [], _, _, TakeAddr,
        [], [], [], !Info) :-
    expect(unify(TakeAddr, []), $pred, "TakeAddr != []").
ml_gen_dynamic_deconstruct_args(_, [], [_ | _], _, _, _, _, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args(_, [_ | _], [], _, _, _, _, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args(FieldGen,
        [ArgVarRepn | ArgVarRepns], [Mode | Modes], CurArgNum,
        Context, TakeAddr, TakeAddrInfos, Defns, Stmts, !Info) :-
    ArgVarRepn = ArgVar - CtorArgRepn,
    NextArgNum = CurArgNum + 1,
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    ( if
        TakeAddr = [CurArgNum | TailTakeAddr]
    then
        ( if ArgPosWidth = apw_full(_, CellOffsetPrime) then
            CellOffset = CellOffsetPrime
        else
            unexpected($pred,
                "taking address of something other than a full word")
        ),
        ml_gen_take_addr_of_arg(!.Info, ArgVar, CtorArgRepn,
            CellOffset, TakeAddrInfo),
        ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, Modes,
            NextArgNum, Context, TailTakeAddr, TakeAddrInfosTail,
            Defns, Stmts, !Info),
        TakeAddrInfos = [TakeAddrInfo | TakeAddrInfosTail]
    else if
        ArgPosWidth = apw_partial_first(_, _, _, _, _),
        % Without field_via_offset, we have no way to get a whole word
        % from a memory cell at once.
        FieldGen = field_gen(_MaybePtag, _AddrRval, _AddrType, FieldVia),
        FieldVia = field_via_offset
    then
        ml_gen_dynamic_deconstruct_args_in_word(FieldGen,
            ArgVar, CtorArgRepn, Mode,
            ArgVarRepns, Modes, LeftOverArgVarRepns, LeftOverModes,
            CurArgNum, LeftOverArgNum,
            Context, TakeAddr, HeadDefns, HeadStmts, !Info),
        ml_gen_dynamic_deconstruct_args(FieldGen,
            LeftOverArgVarRepns, LeftOverModes, LeftOverArgNum,
            Context, TakeAddr, TakeAddrInfos, TailDefns, TailStmts, !Info),
        Defns = HeadDefns ++ TailDefns,
        Stmts = HeadStmts ++ TailStmts
    else
        ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
            CurArgNum, Context, _PackedArgVars, HeadStmts, !Info),
        ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, Modes,
            NextArgNum, Context, TakeAddr, TakeAddrInfos,
            Defns, TailStmts, !Info),
        Stmts = HeadStmts ++ TailStmts
    ).

:- pred ml_gen_dynamic_deconstruct_args_in_word(field_gen,
    prog_var, constructor_arg_repn, unify_mode,
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    int, int, prog_context, list(int),
    list(mlds_local_var_defn), list(mlds_stmt), ml_gen_info, ml_gen_info).
:- mode ml_gen_dynamic_deconstruct_args_in_word(in, in, in, in, in, in,
    out, out, in, out, in, in(bound([])), out, out, in, out) is det.
:- mode ml_gen_dynamic_deconstruct_args_in_word(in, in, in, in, in, in,
    out, out, in, out, in, in, out, out, in, out) is det.

ml_gen_dynamic_deconstruct_args_in_word(FieldGen, ArgVar, CtorArgRepn, Mode,
        ArgVarRepns, Modes, LeftOverArgVarRepns, LeftOverModes,
        CurArgNum, LeftOverArgNum, Context, TakeAddr, Defns, Stmts, !Info) :-
    ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
        CurArgNum, Context, HeadPackedArgVars, HeadStmts, !Info),
    (
        HeadPackedArgVars = [],
        AllPartialsRight0 = not_all_partials_assign_right
    ;
        HeadPackedArgVars = [_ | _],
        AllPartialsRight0 = all_partials_assign_right
    ),
    NextArgNum = CurArgNum + 1,
    ml_gen_dynamic_deconstruct_args_in_word_loop(FieldGen,
        ArgVarRepns, Modes, LeftOverArgVarRepns, LeftOverModes,
        NextArgNum, LeftOverArgNum,
        Context, TakeAddr, AllPartialsRight0, AllPartialsRight,
        TailPackedArgVars, TailStmts, !Info),
    % XXX ARG_PACK
    % We could get ml_gen_dynamic_deconstruct_args_in_word_loop to tell us
    % when all the args in the word assign left, as in that case,
    % we could generate better code than the one generated by
    % ml_gen_dynamic_deconstruct_arg_unify_assign_left.
    (
        AllPartialsRight = not_all_partials_assign_right,
        Defns = [],
        Stmts = HeadStmts ++ TailStmts
    ;
        AllPartialsRight = all_partials_assign_right,
        PackedArgVars = HeadPackedArgVars ++ TailPackedArgVars,
        ml_gen_info_new_packed_args_var(WordCompVar, !Info),

        WordVar = lvn_comp_var(WordCompVar),
        UnsignedType= mlds_int_type_uint,
        WordVarDefn = mlds_local_var_defn(WordVar, Context, UnsignedType,
            no_initializer, gc_no_stmt),
        Defns = [WordVarDefn],
        ArgPosWidth = CtorArgRepn ^ car_pos_width,
        ( if ArgPosWidth = apw_partial_first(_, CellOffset, _, _, _) then
            CellOffset = cell_offset(CellOffsetInt)
        else
            unexpected($pred, "no apw_partial_first")
        ),

        FieldId = ml_field_offset(ml_const(mlconst_int(CellOffsetInt))),
        FieldGen = field_gen(MaybePtag, AddrRval, AddrType, _),
        FieldLval = ml_field(MaybePtag, AddrRval, AddrType,
            FieldId, mlds_generic_type),
        CastFieldRval = ml_cast(UnsignedType, ml_lval(FieldLval)),

        WordVarLval = ml_local_var(WordVar, UnsignedType),
        WordAssignStmt = ml_gen_assign(WordVarLval, CastFieldRval, Context),
        Stmts = [WordAssignStmt | HeadStmts] ++ TailStmts,

        ml_gen_info_get_packed_args_map(!.Info, PackedArgsMap0),
        % Since this unification *defines* the variables in PackedArgVars,
        % none of them could have defined by another deconstruct unification
        % earlier on this path.
        map.det_insert(PackedArgVars, ml_lval(WordVarLval),
            PackedArgsMap0, PackedArgsMap),
        ml_gen_info_set_packed_args_map(PackedArgsMap, !Info)
    ).

:- type do_all_partials_assign_right
    --->    not_all_partials_assign_right
    ;       all_partials_assign_right.

:- pred ml_gen_dynamic_deconstruct_args_in_word_loop(field_gen,
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    int, int, prog_context, list(int),
    do_all_partials_assign_right, do_all_partials_assign_right,
    list(packed_arg_var), list(mlds_stmt), ml_gen_info, ml_gen_info).
:- mode ml_gen_dynamic_deconstruct_args_in_word_loop(in, in, in, out, out,
    in, out, in, in(bound([])), in, out, out, out, in, out) is det.
:- mode ml_gen_dynamic_deconstruct_args_in_word_loop(in, in, in, out, out,
    in, out, in, in, in, out, out, out, in, out) is det.

ml_gen_dynamic_deconstruct_args_in_word_loop(_FieldGen, [], [], [], [],
        CurArgNum, LeftOverArgNum,
        _Context, _TakeAddr, !AllPartialsRight, [], [], !Info) :-
    LeftOverArgNum = CurArgNum.
ml_gen_dynamic_deconstruct_args_in_word_loop(_FieldGen, [], [_ | _], _, _,
        _, _, _, _, !AllPartialsRight, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args_in_word_loop(_FieldGen, [_ | _], [], _, _,
        _, _, _, _, !AllPartialsRight, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args_in_word_loop(FieldGen,
        [ArgVarRepn | ArgVarRepns], [Mode | Modes],
        LeftOverArgVarRepns, LeftOverModes, CurArgNum, LeftOverArgNum,
        Context, TakeAddr, !AllPartialsRight,
        PackedArgVars, Stmts, !Info) :-
    ArgVarRepn = ArgVar - CtorArgRepn,
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    (
        (
            ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _),
            ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
                CurArgNum, Context, HeadPackedArgVars, HeadStmts, !Info),
            (
                HeadPackedArgVars = [],
                !:AllPartialsRight = not_all_partials_assign_right
            ;
                HeadPackedArgVars = [_ | _]
            )
        ;
            ArgPosWidth = apw_none_shifted(_, _),
            ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
                CurArgNum, Context, HeadPackedArgVars, HeadStmts, !Info),
            expect(unify(HeadPackedArgVars, []), $pred,
                "HeadPackedArgVars != [] for apw_none_shifted")
        ),
        ( if TakeAddr = [CurArgNum | _TailTakeAddr] then
            unexpected($pred,
                "taking address of something other than a full word")
        else
            true
        ),
        NextArgNum = CurArgNum + 1,
        ml_gen_dynamic_deconstruct_args_in_word_loop(FieldGen,
            ArgVarRepns, Modes, LeftOverArgVarRepns, LeftOverModes,
            NextArgNum, LeftOverArgNum,
            Context, TakeAddr, !AllPartialsRight,
            TailPackedArgVars, TailStmts, !Info),
        PackedArgVars = HeadPackedArgVars ++ TailPackedArgVars,
        Stmts = HeadStmts ++ TailStmts
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        LeftOverArgVarRepns = [ArgVarRepn | ArgVarRepns],
        LeftOverModes = [Mode | Modes],
        LeftOverArgNum = CurArgNum,
        PackedArgVars = [],
        Stmts = []
    ).

:- pred ml_gen_dynamic_deconstruct_arg(field_gen::in,
    prog_var::in, constructor_arg_repn::in, unify_mode::in,
    int::in, prog_context::in,
    list(packed_arg_var)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
        ArgNum, Context, PackedArgVars, Stmts, !Info) :-
    FieldGen = field_gen(MaybePrimaryTag, AddrRval, AddrType, FieldVia),
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    (
        FieldVia = field_via_offset,
        (
            ( ArgPosWidth = apw_full(_, CellOffset)
            ; ArgPosWidth = apw_double(_, CellOffset, _)
            ; ArgPosWidth = apw_partial_first(_, CellOffset, _, _, _)
            ; ArgPosWidth = apw_partial_shifted(_, CellOffset, _, _, _, _)
            ; ArgPosWidth = apw_none_shifted(_, CellOffset)
            ),
            CellOffset = cell_offset(CellOffsetInt)
        ;
            ArgPosWidth = apw_none_nowhere,
            % The FieldId we generate from this will *not* be used.
            CellOffsetInt = -1
        ),
        FieldId = ml_field_offset(ml_const(mlconst_int(CellOffsetInt)))
    ;
        FieldVia = field_via_name(FieldQualifier, ClassPtrType),
        MaybeFieldName = CtorArgRepn ^ car_field_name,
        FieldName = ml_gen_hld_field_name(MaybeFieldName, ArgNum),
        QualifiedFieldName =
            qual_field_var_name(FieldQualifier, type_qual, FieldName),
        FieldId = ml_field_named(QualifiedFieldName, ClassPtrType)
    ),
    % Box the field type, if needed.
    % XXX ARG_PACK For sub-word-sized fields, this should *never* be needed,
    % so we should do this only for full- and double-word arguments.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_info_get_high_level_data(!.Info, HighLevelData),
    FieldWidth = arg_pos_width_to_width_only(ArgPosWidth),
    FieldRawType = CtorArgRepn ^ car_type,
    ml_type_as_field(ModuleInfo, HighLevelData, FieldRawType, FieldWidth,
        FieldType),

    % Generate lvals for the LHS ...
    ml_gen_type(!.Info, FieldType, MLDS_FieldType),
    FieldLval = ml_field(MaybePrimaryTag, AddrRval, AddrType,
        FieldId, MLDS_FieldType),
    % ... and the RHS.
    ml_gen_var(!.Info, ArgVar, ArgLval),
    ml_variable_type(!.Info, ArgVar, ArgType),

    % Now generate code to unify them.
    % Figure out the direction of data-flow from the mode,
    % and generate code accordingly.
    %
    % Note that in some cases, the code we generate assigns to a variable
    % that is never referred to. This happens quite often for deconstruct
    % unifications that implement field access; the argument variables that
    % correspond to the fields other than the one being accessed end up
    % being assigned to but not used. While we generate suboptimal C code,
    % ml_unused_assign.m can delete both the unused assignments, and the
    % declarations of the unused variables, in most cases.

    compute_assign_direction(ModuleInfo, Mode, ArgType, FieldType, Dir),
    (
        Dir = assign_nondummy_right,
        ml_gen_dynamic_deconstruct_arg_unify_assign_right(ModuleInfo,
            FieldLval, FieldType, ArgPosWidth, ArgVar, ArgLval, ArgType,
            Context, PackedArgVars, Stmts)
    ;
        Dir = assign_nondummy_left,
        PackedArgVars = [],
        ml_gen_dynamic_deconstruct_arg_unify_assign_left(ModuleInfo,
            HighLevelData, FieldLval, FieldType, ArgPosWidth,
            ArgLval, ArgType, Context, Stmts)
    ;
        ( Dir = assign_nondummy_unused
        ; Dir = assign_dummy
        ),
        % The unification has no effect.
        PackedArgVars = [],
        Stmts = []
    ).

:- pred ml_gen_dynamic_deconstruct_arg_unify_assign_right(module_info::in,
    mlds_lval::in, mer_type::in, arg_pos_width::in,
    prog_var::in, mlds_lval::in, mer_type::in, prog_context::in,
    list(packed_arg_var)::out, list(mlds_stmt)::out) is det.

ml_gen_dynamic_deconstruct_arg_unify_assign_right(ModuleInfo,
        FieldLval, FieldType, ArgPosWidth, ArgVar, ArgLval, ArgType,
        Context, PackedArgVars, Stmts) :-
    (
        ArgPosWidth = apw_double(_, _, _),
        PackedArgVars = [],
        ( if ml_field_offset_pair(FieldLval, FieldLvalA, FieldLvalB) then
            FieldRval = ml_binop(float_from_dword,
                ml_lval(FieldLvalA), ml_lval(FieldLvalB))
        else
            ml_gen_box_or_unbox_rval(ModuleInfo, FieldType, ArgType,
                bp_native_if_possible, ml_lval(FieldLval), FieldRval)
        ),
        Stmt = ml_gen_assign(ArgLval, FieldRval, Context),
        Stmts = [Stmt]
    ;
        ArgPosWidth = apw_full(_, _),
        PackedArgVars = [],
        ml_gen_box_or_unbox_rval(ModuleInfo, FieldType, ArgType,
            bp_native_if_possible, ml_lval(FieldLval), FieldRval),
        Stmt = ml_gen_assign(ArgLval, FieldRval, Context),
        Stmts = [Stmt]
    ;
        (
            ArgPosWidth = apw_partial_first(_, _, NumBits, Mask, Fill),
            Shift = arg_shift(0)
        ;
            ArgPosWidth = apw_partial_shifted(_, _, Shift, NumBits, Mask,
                Fill)
        ),
        PackedArgVar = packed_arg_var(ArgVar, Shift, NumBits, Fill),
        PackedArgVars = [PackedArgVar],
        ml_extract_subword_value(ml_lval(FieldLval), Shift, Mask, Fill,
            ToAssignRval),
        Stmt = ml_gen_assign(ArgLval, ToAssignRval, Context),
        Stmts = [Stmt]
    ;
        ( ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_none_shifted(_, _)
        ),
        % Generate no code.
        PackedArgVars = [],
        Stmts = []
    ).

:- pred ml_gen_dynamic_deconstruct_arg_unify_assign_left(module_info::in,
    bool::in, mlds_lval::in, mer_type::in, arg_pos_width::in,
    mlds_lval::in, mer_type::in, prog_context::in,
    list(mlds_stmt)::out) is det.

ml_gen_dynamic_deconstruct_arg_unify_assign_left(ModuleInfo, HighLevelData,
        FieldLval, FieldType, ArgPosWidth, ArgLval, ArgType, Context,
        Stmts) :-
    (
        ArgPosWidth = apw_double(_, _, _),
        ml_gen_box_or_unbox_rval(ModuleInfo, ArgType, FieldType,
            bp_native_if_possible, ml_lval(ArgLval), ArgRval),
        ( if ml_field_offset_pair(FieldLval, FieldLvalA, FieldLvalB) then
            FloatWordA = ml_unop(dword_float_get_word0, ArgRval),
            FloatWordB = ml_unop(dword_float_get_word1, ArgRval),
            ml_type_as_field(ModuleInfo, HighLevelData, int_type,
                aw_full_word, IntFieldType),
            ml_gen_box_or_unbox_rval(ModuleInfo, int_type, IntFieldType,
                bp_native_if_possible, FloatWordA, ArgRvalA),
            ml_gen_box_or_unbox_rval(ModuleInfo, int_type, IntFieldType,
                bp_native_if_possible, FloatWordB, ArgRvalB),
            StmtA = ml_gen_assign(FieldLvalA, ArgRvalA, Context),
            StmtB = ml_gen_assign(FieldLvalB, ArgRvalB, Context),
            Stmts = [StmtA, StmtB]
        else
            Stmt = ml_gen_assign(FieldLval, ArgRval, Context),
            Stmts = [Stmt]
        )
    ;
        ArgPosWidth = apw_full(_, _),
        ml_gen_box_or_unbox_rval(ModuleInfo, ArgType, FieldType,
            bp_native_if_possible, ml_lval(ArgLval), ArgRval),
        Stmt = ml_gen_assign(FieldLval, ArgRval, Context),
        Stmts = [Stmt]
    ;
        (
            ArgPosWidth = apw_partial_first(_, _, _, Mask, Fill),
            Shift = arg_shift(0)
        ;
            ArgPosWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill)
        ),
        % XXX ARG_PACK Optimize this when replacing the whole word.
        ArgRval = ml_lval(ArgLval),
        Shift = arg_shift(ShiftInt),
        Mask = arg_mask(MaskInt),
        CastFieldRVal = ml_unbox(mlds_int_type_uint, ml_lval(FieldLval)),
        OldFieldBits = ml_bitwise_and(CastFieldRVal, \ (MaskInt << ShiftInt)),
        NewFieldBits = ml_lshift(ArgRval, Shift, Fill),
        UpdatedFieldBits = ml_cast(mlds_generic_type,
            ml_bitwise_or(OldFieldBits, NewFieldBits)),
        Stmt = ml_gen_assign(FieldLval, UpdatedFieldBits, Context),
        Stmts = [Stmt]
    ;
        ( ArgPosWidth = apw_none_shifted(_, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        % Nothing to do.
        Stmts = []
    ).

:- pred ml_gen_deconstruct_tagword_args(ml_gen_info::in, mlds_rval::in,
    assoc_list(prog_var, constructor_arg_repn)::in, list(unify_mode)::in,
    prog_context::in,
    list(mlds_rval)::in, list(mlds_rval)::out, uint::in, uint::out,
    list(mlds_stmt)::out) is det.

ml_gen_deconstruct_tagword_args(_, _, [], [], _, !ToOrRvals, !ToOrMask, []).
ml_gen_deconstruct_tagword_args(_, _, [], [_ | _],
        _, !ToOrRvals, !ToOrMask, _) :-
    unexpected($pred, "length mismatch").
ml_gen_deconstruct_tagword_args(_, _, [_ | _], [],
        _, !ToOrRvals, !ToOrMask, _) :-
    unexpected($pred, "length mismatch").
ml_gen_deconstruct_tagword_args(Info, WordRval,
        [ArgVarRepn | ArgVarRepns], [Mode | Modes],
        Context, !ToOrRvals, !ToOrMask, Stmts) :-
    ml_gen_deconstruct_tagword_arg(Info, WordRval, ArgVarRepn, Mode,
        Context, !ToOrRvals, !ToOrMask, HeadStmts),
    ml_gen_deconstruct_tagword_args(Info, WordRval, ArgVarRepns, Modes,
        Context, !ToOrRvals, !ToOrMask, TailStmts),
    Stmts = HeadStmts ++ TailStmts.

:- pred ml_gen_deconstruct_tagword_arg(ml_gen_info::in, mlds_rval::in,
    pair(prog_var, constructor_arg_repn)::in, unify_mode::in, prog_context::in,
    list(mlds_rval)::in, list(mlds_rval)::out, uint::in, uint::out,
    list(mlds_stmt)::out) is det.

ml_gen_deconstruct_tagword_arg(Info, WordRval,
        ArgVar - CtorArgRepn, Mode, Context, !ToOrRvals, !ToOrMask, Stmts) :-
    ml_gen_var(Info, ArgVar, ArgLval),
    ml_variable_type(Info, ArgVar, ArgType),

    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    FieldWidth = arg_pos_width_to_width_only(ArgPosWidth),
    FieldRawType = CtorArgRepn ^ car_type,
    ml_type_as_field(ModuleInfo, HighLevelData, FieldRawType, FieldWidth,
        FieldType),

    compute_assign_direction(ModuleInfo, Mode, ArgType, FieldType, Dir),
    (
        Dir = assign_nondummy_right,
        ml_gen_deconstruct_tagword_arg_assign_right(WordRval,
            ArgPosWidth, ArgLval, Context, Stmts)
    ;
        Dir = assign_nondummy_left,
        ml_gen_deconstruct_tagword_arg_assign_left(WordRval,
            ArgPosWidth, ArgLval, !ToOrRvals, !ToOrMask),
        Stmts = []
    ;
        ( Dir = assign_nondummy_unused
        ; Dir = assign_dummy
        ),
        % The unification has no effect.
        Stmts = []
    ).

:- pred ml_gen_deconstruct_tagword_arg_assign_right(mlds_rval::in,
    arg_pos_width::in, mlds_lval::in, prog_context::in,
    list(mlds_stmt)::out) is det.

ml_gen_deconstruct_tagword_arg_assign_right(WordRval, ArgPosWidth, ArgLval,
        Context, Stmts) :-
    (
        ArgPosWidth = apw_partial_shifted(_, _, Shift, _NumBits, Mask, Fill),
        ml_extract_subword_value(WordRval, Shift, Mask, Fill, ToAssignRval),
        Stmt = ml_gen_assign(ArgLval, ToAssignRval, Context),
        Stmts = [Stmt]
    ;
        ArgPosWidth = apw_none_shifted(_, _),
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
        Stmts = []
    ;
        ( ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        unexpected($pred, "ArgPosWidth does not belong in tagword")
    ).

:- pred ml_gen_deconstruct_tagword_arg_assign_left(mlds_rval::in,
    arg_pos_width::in, mlds_lval::in,
    list(mlds_rval)::in, list(mlds_rval)::out, uint::in, uint::out) is det.

ml_gen_deconstruct_tagword_arg_assign_left(_WordRval, ArgPosWidth, ArgLval,
        !ToOrRvals, !ToOrMask) :-
    (
        ArgPosWidth = apw_partial_shifted(_, _, Shift, _NumBits, Mask, Fill),
        Shift = arg_shift(ShiftInt),
        Mask = arg_mask(MaskInt),
        LeftShiftedArgRval = ml_lshift(ml_lval(ArgLval), Shift, Fill),
        !:ToOrRvals = [LeftShiftedArgRval | !.ToOrRvals],
        !:ToOrMask = (uint.cast_from_int(MaskInt) << ShiftInt) \/ !.ToOrMask
    ;
        ArgPosWidth = apw_none_shifted(_, _)
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
    ;
        ( ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        unexpected($pred, "ArgPosWidth does not belong in tagword")
    ).

:- pred ml_gen_dynamic_deconstruct_direct_arg(ml_gen_info::in, ptag::in,
    unify_mode::in, prog_var::in, prog_var::in,
    prog_context::in, list(mlds_stmt)::out) is det.

ml_gen_dynamic_deconstruct_direct_arg(Info, Ptag, Mode, ArgVar, Var,
        Context, Stmts) :-
    ml_variable_type(Info, ArgVar, ArgType),
    ml_variable_type(Info, Var, VarType),
    ml_gen_var(Info, ArgVar, ArgLval),
    ml_gen_var(Info, Var, VarLval),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    compute_assign_direction(ModuleInfo, Mode, ArgType, VarType, Dir),
    (
        Dir = assign_nondummy_right,
        ml_gen_box_or_unbox_rval(ModuleInfo, VarType, ArgType,
            bp_native_if_possible, ml_lval(VarLval), VarRval),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, ArgType),
        Ptag = ptag(PtagUint8),
        PtagInt = uint8.cast_to_int(PtagUint8),
        CastRval = ml_cast(MLDS_Type,
            ml_binop(body, VarRval, ml_const(mlconst_int(PtagInt)))),
        Stmt = ml_gen_assign(ArgLval, CastRval, Context),
        Stmts = [Stmt]
    ;
        Dir = assign_nondummy_left,
        ml_gen_box_or_unbox_rval(ModuleInfo, ArgType, VarType,
            bp_native_if_possible, ml_lval(ArgLval), ArgRval),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, VarType),
        CastRval = ml_cast(MLDS_Type, ml_mkword(Ptag, ArgRval)),
        Stmt = ml_gen_assign(VarLval, CastRval, Context),
        Stmts = [Stmt]
    ;
        Dir = assign_nondummy_unused,
        Stmts = []
    ;
        Dir = assign_dummy,
        unexpected($pred, "dummy unify")
    ).

:- pred ml_gen_dynamic_deconstruct_no_tag(ml_gen_info::in, unify_mode::in,
    prog_var::in, prog_var::in, prog_context::in, list(mlds_stmt)::out) is det.

ml_gen_dynamic_deconstruct_no_tag(Info, Mode, ArgVar, Var, Context, Stmts) :-
    ml_variable_type(Info, ArgVar, ArgType),
    ml_variable_type(Info, Var, VarType),
    ml_gen_var(Info, ArgVar, ArgLval),
    ml_gen_var(Info, Var, VarLval),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    ArgPosWidth = apw_full(arg_only_offset(0), cell_offset(0)),
    compute_assign_direction(ModuleInfo, Mode, ArgType, VarType, Dir),
    (
        Dir = assign_nondummy_right,
        ml_gen_dynamic_deconstruct_arg_unify_assign_right(ModuleInfo,
            VarLval, VarType, ArgPosWidth, ArgVar, ArgLval, ArgType,
            Context, _PackedArgVars, Stmts)
    ;
        Dir = assign_nondummy_left,
        ml_gen_dynamic_deconstruct_arg_unify_assign_left(ModuleInfo,
            HighLevelData, VarLval, VarType, ArgPosWidth, ArgLval, ArgType,
            Context, Stmts)
    ;
        ( Dir = assign_nondummy_unused
        ; Dir = assign_dummy
        ),
        % The unification has no effect.
        Stmts = []
    ).

%---------------------------------------------------------------------------%

:- pred ml_gen_take_addr_of_arg(ml_gen_info::in,
    prog_var::in, constructor_arg_repn::in, cell_offset::in,
    take_addr_info::out) is det.

ml_gen_take_addr_of_arg(Info, ArgVar, CtorArgRepn, CurOffset, TakeAddrInfo) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    FieldType = CtorArgRepn ^ car_type,
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    FieldWidth = arg_pos_width_to_width_only(ArgPosWidth),
    ml_type_as_field(ModuleInfo, HighLevelData, FieldType, FieldWidth,
        BoxedFieldType),
    ml_gen_type(Info, FieldType, MLDS_FieldType),
    ml_gen_type(Info, BoxedFieldType, MLDS_BoxedFieldType),
    TakeAddrInfo = take_addr_info(ArgVar, CurOffset, MLDS_FieldType,
        MLDS_BoxedFieldType).

:- pred ml_field_offset_pair(mlds_lval::in, mlds_lval::out, mlds_lval::out)
    is semidet.

ml_field_offset_pair(FieldLval, FieldLvalA, FieldLvalB) :-
    FieldLval = ml_field(Ptag, PtrRval, PtrType, FieldIdA, _),
    FieldIdA = ml_field_offset(FieldOffsetA),
    ( if FieldOffsetA = ml_const(mlconst_int(Offset)) then
        FieldIdB = ml_field_offset(ml_const(mlconst_int(Offset + 1))),
        SubstType = mlds_generic_type,
        FieldLvalA = ml_field(Ptag, PtrRval, PtrType, FieldIdA, SubstType),
        FieldLvalB = ml_field(Ptag, PtrRval, PtrType, FieldIdB, SubstType)
    else
        sorry($pred, "unexpected field offset")
    ).

:- pred ml_extract_subword_value(mlds_rval::in, arg_shift::in, arg_mask::in,
    fill_kind::in, mlds_rval::out) is det.

ml_extract_subword_value(WordRval, Shift, Mask, Fill, Rval) :-
    UnsignedWordRval = ml_cast(mlds_int_type_uint, WordRval),
    Mask = arg_mask(MaskInt),
    MaskedRval = ml_bitwise_and(ml_rshift(UnsignedWordRval, Shift), MaskInt),
    (
        Fill = fill_enum,
        Rval = MaskedRval
    ;
        ( Fill = fill_int8,   CastMLDSType = mlds_int_type_int8
        ; Fill = fill_uint8,  CastMLDSType = mlds_int_type_uint8
        ; Fill = fill_int16,  CastMLDSType = mlds_int_type_int16
        ; Fill = fill_uint16, CastMLDSType = mlds_int_type_uint16
        ; Fill = fill_int32,  CastMLDSType = mlds_int_type_int32
        ; Fill = fill_uint32, CastMLDSType = mlds_int_type_uint32
        ),
        Rval = ml_cast(CastMLDSType, MaskedRval)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_unify_gen_deconstruct.
%---------------------------------------------------------------------------%
