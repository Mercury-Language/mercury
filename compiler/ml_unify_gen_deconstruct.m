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
:- import_module hlds.code_model.
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

:- pred ml_generate_deconstruction_unification(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, can_fail::in, can_cgc::in,
    code_model::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%
% We export ml_gen_dynamic_deconstruct_args for use by ml_unify_gen_construct.m
% when handling reused cells.
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
:- import_module hlds.goal_form.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_unify_gen_test.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data_foreign.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

ml_generate_deconstruction_unification(LHSVar, ConsId, RHSVars, ArgModes,
        CanFail, CanCGC, CodeModel, Context, Defns, Stmts, !Info) :-
    (
        CanFail = can_fail,
        ExpectedCodeModel = model_semi,
        ml_generate_semi_deconstruction(LHSVar, ConsId, RHSVars, ArgModes,
            Context, Defns, UnifyStmts, !Info)
    ;
        CanFail = cannot_fail,
        ExpectedCodeModel = model_det,
        ml_generate_det_deconstruction(LHSVar, ConsId, RHSVars, ArgModes,
            Context, Defns, UnifyStmts, !Info)
    ),
    (
        % Note that we can deallocate a cell even if the unification fails;
        % it is the responsibility of the structure reuse phase to ensure
        % that this is safe.
        CanCGC = can_cgc,
        ml_gen_var(!.Info, LHSVar, LHSVarLval),
        % XXX Avoid strip_tag when we know what tag it will have.
        Delete = delete_object(ml_unop(strip_tag, ml_lval(LHSVarLval))),
        CGCStmt = ml_stmt_atomic(Delete, Context),
        Stmts0 = UnifyStmts ++ [CGCStmt]
    ;
        CanCGC = cannot_cgc,
        Stmts0 = UnifyStmts
    ),

    % We used to require that CodeModel = ExpectedCodeModel. But the
    % determinism field in the goal_info is allowed to be a conservative
    % approximation, so we need to handle the case were CodeModel is less
    % precise than ExpectedCodeModel.
    ml_gen_maybe_convert_goal_code_model(CodeModel, ExpectedCodeModel,
        Context, Stmts0, Stmts, !Info).

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
:- pred ml_generate_semi_deconstruction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_semi_deconstruction(LHSVar, ConsId, RHSVars, ArgModes, Context,
        Defns, Stmts, !Info) :-
    ml_generate_test_var_has_cons_id(LHSVar, ConsId, TestRval, !Info),
    ml_gen_set_success(TestRval, Context, SetTestResultStmt, !Info),
    ml_gen_test_success(SucceededRval, !Info),
    ml_generate_det_deconstruction(LHSVar, ConsId, RHSVars, ArgModes, Context,
        Defns, DetDeconstructStmts, !Info),
    (
        DetDeconstructStmts = [],
        Stmts = [SetTestResultStmt]
    ;
        (
            DetDeconstructStmts = [DetDeconstructStmt]
        ;
            DetDeconstructStmts = [_, _ | _],
            DetDeconstructStmt =
                ml_gen_block([], [], DetDeconstructStmts, Context)
        ),
        IfStmt = ml_stmt_if_then_else(SucceededRval, DetDeconstructStmt,
            no, Context),
        Stmts = [SetTestResultStmt, IfStmt]
    ).

%---------------------------------------------------------------------------%

    % Generate a deterministic deconstruction. In a deterministic
    % deconstruction, we know the value of the cons_id that X is bound to,
    % so we do not need to generate a test for it.
    %
    %   det (cannot_fail) deconstruction:
    %       <do (X => f(A1, A2, ...))>
    %   ===>
    %       A1 = arg(X, f, 1);      % extract arguments
    %       A2 = arg(X, f, 2);
    %       ...
    %
:- pred ml_generate_det_deconstruction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_det_deconstruction(LHSVar, ConsId, RHSVars, ArgModes, Context,
        Defns, Stmts, !Info) :-
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
    (
        ( ConsTag = int_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = string_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = dummy_tag
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ),
        % For constants, if the deconstruction is det, then we already know
        % the value of the constant, so Stmts = [].
        Defns = [],
        Stmts = []
    ;
        ( ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = closure_tag(_, _, _)
        ),
        unexpected($pred, "unexpected tag")
    ;
        ConsTag = no_tag,
        get_notag_or_direct_arg_arg_mode(RHSVars, ArgModes, RHSVar, ArgMode),
        ml_gen_dynamic_deconstruct_no_tag(!.Info, LHSVar, RHSVar, ArgMode,
            Context, Stmts),
        Defns = []
    ;
        ConsTag = direct_arg_tag(Ptag),
        get_notag_or_direct_arg_arg_mode(RHSVars, ArgModes, RHSVar, ArgMode),
        ml_gen_dynamic_deconstruct_direct_arg(!.Info, Ptag, LHSVar, RHSVar,
            ArgMode, Context, Stmts),
        Defns = []
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8)
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag)
            ;
                RemoteArgsTagInfo = remote_args_ctor(_Data),
                Ptag = ptag(0u8)
            ),
            TagwordArgs = no,
            InitOffset = cell_offset(0)
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            RemoteSectag = remote_sectag(SectagUint, SectagSize),
            (
                SectagSize = rsectag_word,
                TagwordArgs = no,
                InitOffset = cell_offset(1)
            ;
                SectagSize = rsectag_subword(SectagBits),
                remote_sectag_filled_bitfield(SectagUint, SectagBits,
                    TagFilledBitfield0),
                TagwordArgs = yes(TagFilledBitfield0),
                % The value of InitOffset is used only for tuples and
                % extra type_info/typeclass_info args. If a function
                % symbols has a sub-word-sized sectag, it cannot be
                % a tuple constructor, and it cannot have any extra args,
                % so this (obviously wrong) value won't be used.
                InitOffset = cell_offset(-42)
            )
        ),
        ml_variable_type(!.Info, LHSVar, LHSVarType),
        ml_gen_var(!.Info, LHSVar, LHSVarLval),
        decide_field_gen(!.Info, LHSVarLval, LHSVarType, ConsId, ConsTag, Ptag,
            FieldGen),
        ml_field_names_and_types(!.Info, LHSVarType, ConsId, InitOffset,
            RHSVars, RHSVarRepns),
        (
            TagwordArgs = yes(TagFilledBitfield),
            ml_take_tagword_args(RHSVarRepns, ArgModes,
                TagwordRHSVarRepns, TagwordArgModes,
                NonTagwordRHSVarRepns, NonTagwordArgModes,
                1, FirstNonTagwordArgNum),

            FieldGen = field_gen(MaybePtag, AddrRval, AddrType, FieldVia),
            expect(unify(FieldVia, field_via_offset), $pred,
                "not field_via_offset for tagword"),
            TagwordFieldId = ml_field_offset(ml_const(mlconst_int(0))),
            TagwordLval = ml_field(MaybePtag, AddrRval, AddrType,
                TagwordFieldId, mlds_generic_type),
            UintType = mlds_builtin_type_int(int_type_uint),
            CastTagwordRval = ml_cast(UintType, ml_lval(TagwordLval)),

            ml_gen_deconstruct_tagword_args(TagwordLval, CastTagwordRval,
                mlds_generic_type, TagFilledBitfield,
                TagwordRHSVarRepns, TagwordArgModes, Context,
                TagwordDefns, TagwordStmts, !Info),
            ml_gen_dynamic_deconstruct_args(FieldGen,
                NonTagwordRHSVarRepns, NonTagwordArgModes,
                FirstNonTagwordArgNum, Context, [], _,
                NonTagwordDefns, NonTagwordStmts, !Info),
            Defns = TagwordDefns ++ NonTagwordDefns,
            Stmts = TagwordStmts ++ NonTagwordStmts
        ;
            TagwordArgs = no,
            FirstNonTagwordArgNum = 1,
            ml_gen_dynamic_deconstruct_args(FieldGen, RHSVarRepns, ArgModes,
                FirstNonTagwordArgNum, Context, [], _, Defns, Stmts, !Info)
        )
    ;
        ConsTag = local_args_tag(LocalArgsTagInfo),
        ml_gen_var(!.Info, LHSVar, LHSVarLval),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        get_cons_repn_defn_det(ModuleInfo, ConsId, ConsRepnDefn),
        CtorArgRepns = ConsRepnDefn ^ cr_args,
        assoc_list.from_corresponding_lists(RHSVars, CtorArgRepns,
            RHSVarRepns),
        local_primsectag_filled_bitfield(!.Info, LocalArgsTagInfo,
            TagFilledBitfield),
        ml_gen_deconstruct_tagword_args(LHSVarLval, ml_lval(LHSVarLval),
            mlds_builtin_type_int(int_type_uint), TagFilledBitfield,
            RHSVarRepns, ArgModes, Context, Defns, Stmts, !Info)
    ).

:- pred ml_gen_deconstruct_tagword_args(mlds_lval::in, mlds_rval::in,
    mlds_type::in, filled_bitfield::in,
    assoc_list(prog_var, constructor_arg_repn)::in, list(unify_mode)::in,
    prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_deconstruct_tagword_args(LHSTagwordLval, CastTagwordRval,
        TagwordType, TagFilledBitfield, RHSVarRepns, ArgModes,
        Context, Defns, Stmts, !Info) :-
    ml_gen_deconstruct_tagword_args_loop(!.Info, CastTagwordRval,
        RHSVarRepns, ArgModes, Context, [], ToOrRvals, 0u, ToOrMask,
        [], RevArgFilledBitfields,
        all_partials_assign_right, AllPartialsRight, RightStmts),
    (
        AllPartialsRight = all_partials_assign_right,
        expect(unify(ToOrRvals, []), $pred,
            "all_partials_assign_right but ToOrRvals != []"),
        list.reverse(RevArgFilledBitfields, ArgFilledBitfields),
        FilledBitfields = [TagFilledBitfield | ArgFilledBitfields],
        record_packed_word(FilledBitfields, CastTagwordRval, Context,
            Defns, WordVarStmts, !Info),
        Stmts = WordVarStmts ++ RightStmts
    ;
        AllPartialsRight = not_all_partials_assign_right,
        % We could think about adding calling record_packed_word on the
        % (updated version of) the LHS word. It may help optimize some later
        % construction unifications, though this is far less likely to happen
        % than with all_partials_assign_right deconstructions. (The common
        % use case motivating calls to record_packed_word, field updates,
        % always leads to all_partials_assign_right deconstructions.)
        (
            ToOrRvals = [],
            % If the unifications of some arguments assign to the left,
            % but the value being assigned to the left is zero, then
            % we do not include it in ToOrRvals. Thus it is possible
            % for ToOrRvals to be empty.
            Defns = [],
            Stmts = RightStmts
        ;
            ToOrRvals = [HeadToOrRval | TailToOrRvals],
            Defns = [],
            ToOrRval = ml_bitwise_or_some_rvals(HeadToOrRval, TailToOrRvals),
            ComplementMask = ml_const(mlconst_uint(\ ToOrMask)),
            MaskedOldTagwordRval = ml_binop(bitwise_and(int_type_uint),
                CastTagwordRval, ComplementMask),
            NewTagwordRval = ml_binop(bitwise_or(int_type_uint),
                MaskedOldTagwordRval, ToOrRval),
            ( if TagwordType = mlds_builtin_type_int(int_type_uint) then
                CastNewTagwordRval = NewTagwordRval
            else
                CastNewTagwordRval = ml_cast(TagwordType, NewTagwordRval)
            ),
            LeftStmt =
                ml_gen_assign(LHSTagwordLval, CastNewTagwordRval, Context),
            Stmts = [LeftStmt | RightStmts]
        )
    ).

ml_gen_dynamic_deconstruct_args(_, [], [], _, _, TakeAddr,
        [], [], [], !Info) :-
    expect(unify(TakeAddr, []), $pred, "TakeAddr != []").
ml_gen_dynamic_deconstruct_args(_, [], [_ | _], _, _, _, _, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args(_, [_ | _], [], _, _, _, _, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args(FieldGen,
        [ArgVarRepn | ArgVarRepns], [ArgMode | ArgModes], CurArgNum,
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
        ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, ArgModes,
            NextArgNum, Context, TailTakeAddr, TakeAddrInfosTail,
            Defns, Stmts, !Info),
        TakeAddrInfos = [TakeAddrInfo | TakeAddrInfosTail]
    else if
        ArgPosWidth = apw_partial_first(_, CellOffset, _, _, _, _),
        % Without field_via_offset, we have no way to get a whole word
        % from a memory cell at once.
        FieldGen = field_gen(_MaybePtag, _AddrRval, _AddrType, FieldVia),
        FieldVia = field_via_offset
    then
        ml_gen_dynamic_deconstruct_args_in_word(FieldGen,
            ArgVar, CtorArgRepn, ArgMode,
            ArgVarRepns, ArgModes, LeftOverArgVarRepns, LeftOverArgModes,
            CurArgNum, LeftOverArgNum,
            CellOffset, Context, TakeAddr, HeadDefns, HeadStmts, !Info),
        ml_gen_dynamic_deconstruct_args(FieldGen,
            LeftOverArgVarRepns, LeftOverArgModes, LeftOverArgNum,
            Context, TakeAddr, TakeAddrInfos, TailDefns, TailStmts, !Info),
        Defns = HeadDefns ++ TailDefns,
        Stmts = HeadStmts ++ TailStmts
    else
        ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, ArgMode,
            CurArgNum, Context, _FilledBitfields, HeadStmts, !Info),
        ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, ArgModes,
            NextArgNum, Context, TakeAddr, TakeAddrInfos,
            Defns, TailStmts, !Info),
        Stmts = HeadStmts ++ TailStmts
    ).

:- pred ml_gen_dynamic_deconstruct_args_in_word(field_gen,
    prog_var, constructor_arg_repn, unify_mode,
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    int, int, cell_offset, prog_context, list(int),
    list(mlds_local_var_defn), list(mlds_stmt), ml_gen_info, ml_gen_info).
:- mode ml_gen_dynamic_deconstruct_args_in_word(in, in, in, in, in, in,
    out, out, in, out, in, in, in(bound([])), out, out, in, out) is det.
:- mode ml_gen_dynamic_deconstruct_args_in_word(in, in, in, in, in, in,
    out, out, in, out, in, in, in, out, out, in, out) is det.

ml_gen_dynamic_deconstruct_args_in_word(FieldGen, ArgVar, CtorArgRepn, ArgMode,
        ArgVarRepns, ArgModes, LeftOverArgVarRepns, LeftOverArgModes,
        CurArgNum, LeftOverArgNum, CellOffset, Context, TakeAddr,
        Defns, Stmts, !Info) :-
    ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, ArgMode,
        CurArgNum, Context, FirstFilledBitfields, HeadStmts, !Info),
    (
        FirstFilledBitfields = [],
        AllPartialsRight0 = not_all_partials_assign_right
    ;
        FirstFilledBitfields = [_ | _],
        AllPartialsRight0 = all_partials_assign_right
    ),
    NextArgNum = CurArgNum + 1,
    ml_gen_dynamic_deconstruct_args_in_word_loop(FieldGen,
        ArgVarRepns, ArgModes, LeftOverArgVarRepns, LeftOverArgModes,
        NextArgNum, LeftOverArgNum,
        Context, TakeAddr, AllPartialsRight0, AllPartialsRight,
        LaterFilledBitfields, TailStmts, !Info),
    % XXX ARG_PACK
    % We could get ml_gen_dynamic_deconstruct_args_in_word_loop to tell us
    % when all the args in the word assign left, as in that case,
    % we could generate better code than the one generated by
    % ml_gen_dynamic_deconstruct_arg_unify_assign_left.
    Stmts0 = HeadStmts ++ TailStmts,
    (
        AllPartialsRight = not_all_partials_assign_right,
        Defns = [],
        Stmts = Stmts0
    ;
        AllPartialsRight = all_partials_assign_right,
        FilledBitfields = FirstFilledBitfields ++ LaterFilledBitfields,

        CellOffset = cell_offset(CellOffsetInt),
        FieldId = ml_field_offset(ml_const(mlconst_int(CellOffsetInt))),
        FieldGen = field_gen(MaybePtag, AddrRval, AddrType, _),
        FieldLval = ml_field(MaybePtag, AddrRval, AddrType, FieldId,
            mlds_generic_type),
        WordRval = ml_lval(FieldLval),

        record_packed_word(FilledBitfields, WordRval, Context,
            Defns, WordVarStmts, !Info),
        Stmts = WordVarStmts ++ Stmts0
    ).

:- pred record_packed_word(list(filled_bitfield)::in, mlds_rval::in,
    prog_context::in, list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

record_packed_word(FilledBitfields, WordRval, Context,
        WordVarDefns, WordVarStmts, !Info) :-
    (
        FilledBitfields = [],
        WordVarDefns = [],
        WordVarStmts = []
    ;
        FilledBitfields = [HeadFilledBitfields | TailFilledBitfields],
        ml_gen_info_new_packed_word_var(WordCompVar, !Info),
        WordVar = lvn_comp_var(WordCompVar),
        WordVarType = mlds_builtin_type_int(int_type_uint),
        WordVarDefn = mlds_local_var_defn(WordVar, Context, WordVarType,
            no_initializer, gc_no_stmt),
        WordVarDefns = [WordVarDefn],

        WordVarLval = ml_local_var(WordVar, WordVarType),
        CastWordRval = ml_cast(WordVarType, WordRval),
        WordAssignStmt = ml_gen_assign(WordVarLval, CastWordRval, Context),
        WordVarStmts = [WordAssignStmt],

        get_unfilled_filled_packed_words(
            HeadFilledBitfields, TailFilledBitfields,
            PackedWord, FilledPackedWord),
        Instance = packed_word_instance(FilledPackedWord,
            ml_lval(WordVarLval)),

        ml_gen_info_get_packed_word_map(!.Info, PackedWordMap0),
        % Since this unification *defines* the variables in PackedArgVars,
        % none of them could have defined by another deconstruction
        % unification earlier on this path. However, there may have been
        % previous deconstruction unifications that involved the same
        % packing scheme.
        ( if map.search(PackedWordMap0, PackedWord, OldInstances) then
            OldInstances = one_or_more(HeadOldInstance, TailOldInstances),
            NewInstances = one_or_more(Instance,
                [HeadOldInstance | TailOldInstances]),
            map.det_update(PackedWord, NewInstances,
                PackedWordMap0, PackedWordMap)
        else
            map.det_insert(PackedWord, one_or_more(Instance, []),
                PackedWordMap0, PackedWordMap)
        ),
        ml_gen_info_set_packed_word_map(PackedWordMap, !Info)
    ).

:- type do_all_partials_assign_right
    --->    not_all_partials_assign_right
    ;       all_partials_assign_right.

:- pred ml_gen_dynamic_deconstruct_args_in_word_loop(field_gen,
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    int, int, prog_context, list(int),
    do_all_partials_assign_right, do_all_partials_assign_right,
    list(filled_bitfield), list(mlds_stmt), ml_gen_info, ml_gen_info).
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
        [ArgVarRepn | ArgVarRepns], [ArgMode | ArgModes],
        LeftOverArgVarRepns, LeftOverArgModes, CurArgNum, LeftOverArgNum,
        Context, TakeAddr, !AllPartialsRight,
        FilledBitfields, Stmts, !Info) :-
    ArgVarRepn = ArgVar - CtorArgRepn,
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    (
        (
            ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _),
            ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn,
                ArgMode, CurArgNum, Context,
                HeadFilledBitfields, HeadStmts, !Info),
            (
                HeadFilledBitfields = [],
                !:AllPartialsRight = not_all_partials_assign_right
            ;
                HeadFilledBitfields = [_ | _]
            )
        ;
            ArgPosWidth = apw_none_shifted(_, _),
            ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn,
                ArgMode, CurArgNum, Context,
                HeadFilledBitfields, HeadStmts, !Info),
            expect(unify(HeadFilledBitfields, []), $pred,
                "HeadFilledBitfields != [] for apw_none_shifted")
        ),
        ( if TakeAddr = [CurArgNum | _TailTakeAddr] then
            unexpected($pred,
                "taking address of something other than a full word")
        else
            true
        ),
        NextArgNum = CurArgNum + 1,
        ml_gen_dynamic_deconstruct_args_in_word_loop(FieldGen,
            ArgVarRepns, ArgModes, LeftOverArgVarRepns, LeftOverArgModes,
            NextArgNum, LeftOverArgNum,
            Context, TakeAddr, !AllPartialsRight,
            TailFilledBitfields, TailStmts, !Info),
        FilledBitfields = HeadFilledBitfields ++ TailFilledBitfields,
        Stmts = HeadStmts ++ TailStmts
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        LeftOverArgVarRepns = [ArgVarRepn | ArgVarRepns],
        LeftOverArgModes = [ArgMode | ArgModes],
        LeftOverArgNum = CurArgNum,
        FilledBitfields = [],
        Stmts = []
    ).

:- pred ml_gen_dynamic_deconstruct_arg(field_gen::in,
    prog_var::in, constructor_arg_repn::in, unify_mode::in,
    int::in, prog_context::in,
    list(filled_bitfield)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, ArgMode,
        ArgNum, Context, FilledBitfields, Stmts, !Info) :-
    FieldGen = field_gen(MaybePrimaryTag, AddrRval, AddrType, FieldVia),
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    (
        FieldVia = field_via_offset,
        (
            ( ArgPosWidth = apw_full(_, CellOffset)
            ; ArgPosWidth = apw_double(_, CellOffset, _)
            ; ArgPosWidth = apw_partial_first(_, CellOffset, _, _, _, _)
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

    ml_compute_assign_direction(ModuleInfo, ArgMode, ArgType, FieldType, Dir),
    (
        Dir = assign_nondummy_right,
        ml_gen_dynamic_deconstruct_arg_unify_assign_right(ModuleInfo,
            FieldLval, FieldType, ArgVar, ArgLval, ArgType, ArgPosWidth,
            Context, FilledBitfields, Stmts)
    ;
        Dir = assign_nondummy_left,
        FilledBitfields = [],
        ml_gen_dynamic_deconstruct_arg_unify_assign_left(ModuleInfo,
            HighLevelData, FieldLval, FieldType, ArgLval, ArgType,
            ArgPosWidth, Context, Stmts)
    ;
        ( Dir = assign_nondummy_unused
        ; Dir = assign_dummy
        ),
        % The unification has no effect.
        FilledBitfields = [],
        Stmts = []
    ).

:- pred ml_gen_dynamic_deconstruct_arg_unify_assign_right(module_info::in,
    mlds_lval::in, mer_type::in, prog_var::in, mlds_lval::in, mer_type::in,
    arg_pos_width::in, prog_context::in,
    list(filled_bitfield)::out, list(mlds_stmt)::out) is det.

ml_gen_dynamic_deconstruct_arg_unify_assign_right(ModuleInfo,
        LHSLval, LHSType, RHSVar, RHSLval, RHSType, ArgPosWidth,
        Context, FilledBitfields, Stmts) :-
    (
        ArgPosWidth = apw_double(_, _, _),
        FilledBitfields = [],
        ( if ml_field_offset_pair(LHSLval, LHSLvalA, LHSLvalB) then
            LHSRval = ml_binop(float_from_dword,
                ml_lval(LHSLvalA), ml_lval(LHSLvalB))
        else
            ml_gen_box_or_unbox_rval(ModuleInfo, LHSType, RHSType,
                bp_native_if_possible, ml_lval(LHSLval), LHSRval)
        ),
        Stmt = ml_gen_assign(RHSLval, LHSRval, Context),
        Stmts = [Stmt]
    ;
        ArgPosWidth = apw_full(_, _),
        FilledBitfields = [],
        ml_gen_box_or_unbox_rval(ModuleInfo, LHSType, RHSType,
            bp_native_if_possible, ml_lval(LHSLval), LHSRval),
        Stmt = ml_gen_assign(RHSLval, LHSRval, Context),
        Stmts = [Stmt]
    ;
        (
            ArgPosWidth = apw_partial_first(_, _, Shift, NumBits, Mask, Fill)
        ;
            ArgPosWidth = apw_partial_shifted(_, _, Shift, NumBits, Mask, Fill)
        ),
        Bitfield = bitfield(Shift, NumBits, Fill),
        FilledBitfields = [filled_bitfield(Bitfield, bv_var(RHSVar))],
        ml_extract_subword_value(ml_lval(LHSLval), Shift, Mask, Fill,
            ToAssignRval),
        Stmt = ml_gen_assign(RHSLval, ToAssignRval, Context),
        Stmts = [Stmt]
    ;
        ( ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_none_shifted(_, _)
        ),
        % Generate no code.
        FilledBitfields = [],
        Stmts = []
    ).

:- pred ml_gen_dynamic_deconstruct_arg_unify_assign_left(module_info::in,
    bool::in, mlds_lval::in, mer_type::in, mlds_lval::in, mer_type::in,
    arg_pos_width::in, prog_context::in, list(mlds_stmt)::out) is det.

ml_gen_dynamic_deconstruct_arg_unify_assign_left(ModuleInfo, HighLevelData,
        LHSLval, LHSType, RHSLval, RHSType, ArgPosWidth, Context, Stmts) :-
    (
        ArgPosWidth = apw_double(_, _, _),
        ml_gen_box_or_unbox_rval(ModuleInfo, RHSType, LHSType,
            bp_native_if_possible, ml_lval(RHSLval), RHSRval),
        ( if ml_field_offset_pair(LHSLval, LHSLvalA, LHSLvalB) then
            FloatWordA = ml_unop(dword_float_get_word0, RHSRval),
            FloatWordB = ml_unop(dword_float_get_word1, RHSRval),
            ml_type_as_field(ModuleInfo, HighLevelData, int_type,
                aw_full_word, IntLHSType),
            ml_gen_box_or_unbox_rval(ModuleInfo, int_type, IntLHSType,
                bp_native_if_possible, FloatWordA, RHSRvalA),
            ml_gen_box_or_unbox_rval(ModuleInfo, int_type, IntLHSType,
                bp_native_if_possible, FloatWordB, RHSRvalB),
            StmtA = ml_gen_assign(LHSLvalA, RHSRvalA, Context),
            StmtB = ml_gen_assign(LHSLvalB, RHSRvalB, Context),
            Stmts = [StmtA, StmtB]
        else
            Stmt = ml_gen_assign(LHSLval, RHSRval, Context),
            Stmts = [Stmt]
        )
    ;
        ArgPosWidth = apw_full(_, _),
        ml_gen_box_or_unbox_rval(ModuleInfo, RHSType, LHSType,
            bp_native_if_possible, ml_lval(RHSLval), RHSRval),
        Stmt = ml_gen_assign(LHSLval, RHSRval, Context),
        Stmts = [Stmt]
    ;
        (
            ArgPosWidth = apw_partial_first(_, _, Shift, _, Mask, Fill)
        ;
            ArgPosWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill)
        ),
        % XXX ARG_PACK Optimize this when replacing the whole word.
        RHSRval = ml_lval(RHSLval),
        Shift = arg_shift(ShiftInt),
        Mask = arg_mask(MaskInt),
        CastLHSRVal = ml_unbox(mlds_builtin_type_int(int_type_uint),
            ml_lval(LHSLval)),
        OldLHSBits = ml_bitwise_mask(CastLHSRVal, \ (MaskInt << ShiftInt)),
        NewLHSBits = ml_left_shift_rval(RHSRval, Shift, Fill),
        UpdatedLHSBits = ml_cast(mlds_generic_type,
            ml_bitwise_or_two_rvals(OldLHSBits, NewLHSBits)),
        Stmt = ml_gen_assign(LHSLval, UpdatedLHSBits, Context),
        Stmts = [Stmt]
    ;
        ( ArgPosWidth = apw_none_shifted(_, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        % Nothing to do.
        Stmts = []
    ).

:- pred ml_gen_deconstruct_tagword_args_loop(ml_gen_info::in, mlds_rval::in,
    assoc_list(prog_var, constructor_arg_repn)::in, list(unify_mode)::in,
    prog_context::in,
    list(mlds_rval)::in, list(mlds_rval)::out, uint::in, uint::out,
    list(filled_bitfield)::in, list(filled_bitfield)::out,
    do_all_partials_assign_right::in, do_all_partials_assign_right::out,
    list(mlds_stmt)::out) is det.

ml_gen_deconstruct_tagword_args_loop(_, _, [], [],
        _, !ToOrRvals, !ToOrMask, !RevFilledBitfields, !AllPartialsRight, []).
ml_gen_deconstruct_tagword_args_loop(_, _, [], [_ | _],
        _, !ToOrRvals, !ToOrMask, !RevFilledBitfields, !AllPartialsRight, _) :-
    unexpected($pred, "length mismatch").
ml_gen_deconstruct_tagword_args_loop(_, _, [_ | _], [],
        _, !ToOrRvals, !ToOrMask, !RevFilledBitfields, !AllPartialsRight, _) :-
    unexpected($pred, "length mismatch").
ml_gen_deconstruct_tagword_args_loop(Info, WordRval,
        [ArgVarRepn | ArgVarRepns], [ArgMode | ArgModes],
        Context, !ToOrRvals, !ToOrMask,
        !RevFilledBitfields, !AllPartialsRight, Stmts) :-
    ml_gen_deconstruct_tagword_arg(Info, WordRval, ArgVarRepn, ArgMode,
        Context, !ToOrRvals, !ToOrMask,
        !RevFilledBitfields, !AllPartialsRight, HeadStmts),
    ml_gen_deconstruct_tagword_args_loop(Info, WordRval, ArgVarRepns, ArgModes,
        Context, !ToOrRvals, !ToOrMask,
        !RevFilledBitfields, !AllPartialsRight, TailStmts),
    Stmts = HeadStmts ++ TailStmts.

:- pred ml_gen_deconstruct_tagword_arg(ml_gen_info::in, mlds_rval::in,
    pair(prog_var, constructor_arg_repn)::in, unify_mode::in, prog_context::in,
    list(mlds_rval)::in, list(mlds_rval)::out, uint::in, uint::out,
    list(filled_bitfield)::in, list(filled_bitfield)::out,
    do_all_partials_assign_right::in, do_all_partials_assign_right::out,
    list(mlds_stmt)::out) is det.

ml_gen_deconstruct_tagword_arg(Info, WordRval, ArgVar - CtorArgRepn, ArgMode,
        Context, !ToOrRvals, !ToOrMask,
        !RevFilledBitfields, !AllPartialsRight, Stmts) :-
    ml_gen_var(Info, ArgVar, ArgLval),
    ml_variable_type(Info, ArgVar, ArgType),

    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    ArgPosWidth = CtorArgRepn ^ car_pos_width,
    FieldWidth = arg_pos_width_to_width_only(ArgPosWidth),
    FieldRawType = CtorArgRepn ^ car_type,
    ml_type_as_field(ModuleInfo, HighLevelData, FieldRawType, FieldWidth,
        FieldType),

    ml_compute_assign_direction(ModuleInfo, ArgMode, ArgType, FieldType, Dir),
    (
        Dir = assign_nondummy_right,
        ml_gen_deconstruct_tagword_arg_assign_right(WordRval,
            ArgPosWidth, ArgVar, ArgLval, Context, !RevFilledBitfields, Stmts)
    ;
        Dir = assign_nondummy_left,
        ml_gen_deconstruct_tagword_arg_assign_left(WordRval,
            ArgPosWidth, ArgLval, !ToOrRvals, !ToOrMask),
        !:AllPartialsRight = not_all_partials_assign_right,
        Stmts = []
    ;
        ( Dir = assign_nondummy_unused
        ; Dir = assign_dummy
        ),
        % The unification has no effect.
        !:AllPartialsRight = not_all_partials_assign_right,
        Stmts = []
    ).

:- pred ml_gen_deconstruct_tagword_arg_assign_right(mlds_rval::in,
    arg_pos_width::in, prog_var::in, mlds_lval::in, prog_context::in,
    list(filled_bitfield)::in, list(filled_bitfield)::out,
    list(mlds_stmt)::out) is det.

ml_gen_deconstruct_tagword_arg_assign_right(WordRval, ArgPosWidth,
        ArgVar, ArgLval, Context, !RevFilledBitfields, Stmts) :-
    (
        ArgPosWidth = apw_partial_shifted(_, _, Shift, NumBits, Mask, Fill),
        Bitfield = bitfield(Shift, NumBits, Fill),
        BitfieldValue = bv_var(ArgVar),
        FilledBitfield = filled_bitfield(Bitfield, BitfieldValue),
        !:RevFilledBitfields = [FilledBitfield | !.RevFilledBitfields],

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
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
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
        LeftShiftedArgRval = ml_left_shift_rval(ml_lval(ArgLval), Shift, Fill),
        !:ToOrRvals = [LeftShiftedArgRval | !.ToOrRvals],
        !:ToOrMask = (uint.cast_from_int(MaskInt) << ShiftInt) \/ !.ToOrMask
    ;
        ArgPosWidth = apw_none_shifted(_, _)
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
    ;
        ( ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        unexpected($pred, "ArgPosWidth does not belong in tagword")
    ).

:- pred ml_gen_dynamic_deconstruct_direct_arg(ml_gen_info::in, ptag::in,
    prog_var::in, prog_var::in, unify_mode::in,
    prog_context::in, list(mlds_stmt)::out) is det.

ml_gen_dynamic_deconstruct_direct_arg(Info, Ptag, LHSVar, RHSVar, ArgMode,
        Context, Stmts) :-
    ml_variable_type(Info, RHSVar, RHSType),
    ml_variable_type(Info, LHSVar, LHSType),
    ml_gen_var(Info, RHSVar, RHSLval),
    ml_gen_var(Info, LHSVar, LHSLval),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_compute_assign_direction(ModuleInfo, ArgMode, RHSType, LHSType, Dir),
    (
        Dir = assign_nondummy_right,
        ml_gen_box_or_unbox_rval(ModuleInfo, LHSType, RHSType,
            bp_native_if_possible, ml_lval(LHSLval), LHSRval),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, RHSType),
        Ptag = ptag(PtagUint8),
        ( if PtagUint8 = 0u8 then
            % Masking off the ptag would be a null operation, since it is
            % already all zeroes.
            CastRval = ml_cast(MLDS_Type, LHSRval)
        else
            PtagInt = uint8.cast_to_int(PtagUint8),
            CastRval = ml_cast(MLDS_Type,
                ml_binop(body, LHSRval, ml_const(mlconst_int(PtagInt))))
        ),
        Stmt = ml_gen_assign(RHSLval, CastRval, Context),
        Stmts = [Stmt]
    ;
        Dir = assign_nondummy_left,
        ml_gen_box_or_unbox_rval(ModuleInfo, RHSType, LHSType,
            bp_native_if_possible, ml_lval(RHSLval), RHSRval),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, LHSType),
        CastRval = ml_cast(MLDS_Type, ml_mkword(Ptag, RHSRval)),
        Stmt = ml_gen_assign(LHSLval, CastRval, Context),
        Stmts = [Stmt]
    ;
        Dir = assign_nondummy_unused,
        Stmts = []
    ;
        Dir = assign_dummy,
        unexpected($pred, "dummy unify")
    ).

:- pred ml_gen_dynamic_deconstruct_no_tag(ml_gen_info::in,
    prog_var::in, prog_var::in, unify_mode::in, prog_context::in,
    list(mlds_stmt)::out) is det.

ml_gen_dynamic_deconstruct_no_tag(Info, LHSVar, RHSVar, ArgMode, Context,
        Stmts) :-
    ml_variable_type(Info, RHSVar, RHSType),
    ml_variable_type(Info, LHSVar, LHSType),
    ml_gen_var(Info, RHSVar, RHSLval),
    ml_gen_var(Info, LHSVar, LHSLval),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    ArgPosWidth = apw_full(arg_only_offset(0), cell_offset(0)),
    ml_compute_assign_direction(ModuleInfo, ArgMode, RHSType, LHSType, Dir),
    (
        Dir = assign_nondummy_right,
        ml_gen_dynamic_deconstruct_arg_unify_assign_right(ModuleInfo,
            LHSLval, LHSType, RHSVar, RHSLval, RHSType,
            ArgPosWidth, Context, _FilledBitfields, Stmts)
    ;
        Dir = assign_nondummy_left,
        ml_gen_dynamic_deconstruct_arg_unify_assign_left(ModuleInfo,
            HighLevelData, LHSLval, LHSType, RHSLval, RHSType,
            ArgPosWidth, Context, Stmts)
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
    UnsignedWordRval = ml_cast(mlds_builtin_type_int(int_type_uint), WordRval),
    Mask = arg_mask(MaskInt),
    MaskedRval = ml_bitwise_mask(
        ml_right_shift_rval(UnsignedWordRval, Shift), MaskInt),
    (
        ( Fill = fill_enum
        ; Fill = fill_char21
        ),
        Rval = MaskedRval
    ;
        ( Fill = fill_int8,   CastIntType = int_type_int8
        ; Fill = fill_uint8,  CastIntType = int_type_uint8
        ; Fill = fill_int16,  CastIntType = int_type_int16
        ; Fill = fill_uint16, CastIntType = int_type_uint16
        ; Fill = fill_int32,  CastIntType = int_type_int32
        ; Fill = fill_uint32, CastIntType = int_type_uint32
        ),
        CastMLDSType = mlds_builtin_type_int(CastIntType),
        Rval = ml_cast(CastMLDSType, MaskedRval)
    ).

%---------------------------------------------------------------------------%

:- pred ml_take_tagword_args(
    assoc_list(prog_var, constructor_arg_repn)::in, list(unify_mode)::in,
    assoc_list(prog_var, constructor_arg_repn)::out, list(unify_mode)::out,
    assoc_list(prog_var, constructor_arg_repn)::out, list(unify_mode)::out,
    int::in, int::out) is det.

ml_take_tagword_args([], [], [], [], [], [], !FirstNonTagwordArgNum).
ml_take_tagword_args([], [_ | _], _, _, _, _, !FirstNonTagwordArgNum) :-
    unexpected($pred, "length mismatch").
ml_take_tagword_args([_ | _], [], _, _, _, _, !FirstNonTagwordArgNum) :-
    unexpected($pred, "length mismatch").
ml_take_tagword_args([RHSVarRepn | RHSVarRepns], [ArgMode | ArgModes],
        TagwordRHSVarRepns, TagwordArgModes,
        NonTagwordRHSVarRepns, NonTagwordArgModes, !FirstNonTagwordArgNum) :-
    RHSVarRepn = _ - Repn,
    ArgPosWidth = Repn ^ car_pos_width,
    (
        ( ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, _)
        ),
        !:FirstNonTagwordArgNum = !.FirstNonTagwordArgNum + 1,
        ml_take_tagword_args(RHSVarRepns, ArgModes,
            TailTagwordRHSVarRepns, TailTagwordArgModes,
            NonTagwordRHSVarRepns, NonTagwordArgModes,
            !FirstNonTagwordArgNum),
        TagwordRHSVarRepns = [RHSVarRepn | TailTagwordRHSVarRepns],
        TagwordArgModes = [ArgMode | TailTagwordArgModes]
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        TagwordRHSVarRepns = [],
        TagwordArgModes = [],
        NonTagwordRHSVarRepns = [RHSVarRepn | RHSVarRepns],
        NonTagwordArgModes = [ArgMode | ArgModes]
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_unify_gen_deconstruct.
%---------------------------------------------------------------------------%
