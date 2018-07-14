%---------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------e
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module ll_backend.unify_gen_deconstruct.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred generate_deconstruction_unification(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, can_fail::in, can_cgc::in,
    llds_code::out, code_info::in, code_info::out,
    code_loc_dep::in, code_loc_dep::out) is det.

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

%---------------------------------------------------------------------------%

    % Generate a subunification between two [field | variable].
    %
:- pred generate_deconstruct_unify_arg(field_and_arg_var, unify_mode,
    llds_code, code_info, code_loc_dep, code_loc_dep).
:- mode generate_deconstruct_unify_arg(in(var_and_arg_var), in,
    out, in, in, out) is det.
:- mode generate_deconstruct_unify_arg(in(field_and_arg_var), in,
    out, in, in, out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module ll_backend.unify_gen_test.
:- import_module ll_backend.unify_gen_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.

:- import_module cord.
:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

generate_deconstruction_unification(LHSVar, ConsId, RHSVars, ArgModes,
        CanFail, CanCGC, Code, !CI, !CLD) :-
    (
        CanFail = can_fail,
        generate_semi_deconstruction(LHSVar, ConsId, RHSVars, ArgModes,
            DeconstructCode, !CI, !CLD)
    ;
        CanFail = cannot_fail,
        generate_det_deconstruction(LHSVar, ConsId, RHSVars, ArgModes,
            DeconstructCode, !.CI, !CLD)
    ),
    (
        CanCGC = can_cgc,
        LHSVarName = variable_name(!.CI, LHSVar),
        produce_variable(LHSVar, ProduceVarCode, VarRval, !.CI, !CLD),
        ( if VarRval = lval(VarLval) then
            save_reused_cell_fields(LHSVar, VarLval, SaveArgsCode, Regs,
                !.CI, !CLD),
            % This seems to be fine.
            list.foldl(release_reg, Regs, !CLD),
            % XXX avoid strip_tag when we know what ptag it will have
            FreeVarCode = singleton(
                llds_instr(free_heap(unop(strip_tag, VarRval)),
                    "Free " ++ LHSVarName)
            ),
            Code = DeconstructCode ++
                ProduceVarCode ++ SaveArgsCode ++ FreeVarCode
        else
            Code = DeconstructCode
        )
    ;
        CanCGC = cannot_cgc,
        Code = DeconstructCode
    ).

%---------------------------------------------------------------------------%

    % Generate a semideterministic deconstruction.
    % A semideterministic deconstruction unification is tag-test
    % followed by a deterministic deconstruction.
    %
:- pred generate_semi_deconstruction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in,
    llds_code::out, code_info::in, code_info::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_semi_deconstruction(LHSVar, ConsId, RHSVars, Modes, Code,
        !CI, !CLD) :-
    produce_variable(LHSVar, LHSVarCode, LHSVarRval, !.CI, !CLD),
    LHSVarName = variable_name(!.CI, LHSVar),
    LHSVarType = variable_type(!.CI, LHSVar),
    CheaperTagTest = lookup_cheaper_tag_test(!.CI, LHSVarType),
    generate_test_var_has_cons_id(LHSVarRval, LHSVarName, ConsId,
        CheaperTagTest, branch_on_success, SuccLabel, TagTestCode, !CI),
    remember_position(!.CLD, AfterUnify),
    generate_failure(FailCode, !CI, !.CLD),
    reset_to_position(AfterUnify, !.CI, !:CLD),
    generate_det_deconstruction(LHSVar, ConsId, RHSVars, Modes,
        DetDeconstructCode, !.CI, !CLD),
    SuccessLabelCode = singleton(llds_instr(label(SuccLabel), "")),
    Code = LHSVarCode ++ TagTestCode ++ FailCode ++
        SuccessLabelCode ++ DetDeconstructCode.

%---------------------------------------------------------------------------%

    % Generate a deterministic deconstruction. In a deterministic
    % deconstruction, we know the value of the ptag, so we don't need
    % to generate a test.
    %
    % Deconstructions are generated semi-eagerly. Any test sub-unifications
    % are generated eagerly (they _must_ be), but assignment unifications
    % are cached.
    %
:- pred generate_det_deconstruction(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_det_deconstruction(LHSVar, ConsId, RHSVars, ArgModes, Code,
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
            RHSVars = [RHSVar],
            ArgModes = [ArgMode]
        then
            VarType = variable_type(CI, LHSVar),
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
                ( if variable_is_forward_live(!.CLD, RHSVar) then
                    assign_const_to_var(RHSVar, const(llconst_int(0)),
                        CI, !CLD)
                else
                    true
                ),
                Code = empty
            ;
                IsDummy = is_not_dummy_type,
                ArgType = variable_type(CI, RHSVar),
                FieldAndArgVar =
                    field_and_arg_var(uv_var(LHSVar), RHSVar, ArgType),
                generate_deconstruct_unify_arg(FieldAndArgVar, ArgMode, Code,
                    CI, !CLD)
            )
        else
            unexpected($pred, "no_tag: arity != 1")
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        ( if
            RHSVars = [RHSVar],
            ArgModes = [ArgMode]
        then
            generate_direct_arg_deconstruct(LHSVar, RHSVar, Ptag, ArgMode,
                Code, CI, !CLD)
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
        LHSRval = var(LHSVar),
        get_vartypes(CI, VarTypes),
        associate_cons_id_args_with_widths(ModuleInfo, ConsId,
            RHSVars, RHSVarsWidths),
        make_fields_and_arg_vars(VarTypes, LHSRval, Ptag, RHSVarsWidths,
            FieldsAndArgVars),
        generate_deconstruct_unify_args(FieldsAndArgVars, ArgModes, Code,
            CI, !CLD)
    ;
        ConsTag = shared_local_tag_with_args(_, _),
        associate_cons_id_args_with_widths(ModuleInfo, ConsId,
            RHSVars, RHSVarsWidths),
        generate_deconstruct_tagword_unify_args(LHSVar, RHSVarsWidths,
            ArgModes, [], ToOrRvals, 0u, ToOrMask, AssignRightCode, CI, !CLD),
        (
            ToOrRvals = [],
            Code = AssignRightCode
        ;
            ToOrRvals = [HeadToOrRval | TailToOrRvals],
            ToOrRval = bitwise_or_some_rvals(HeadToOrRval, TailToOrRvals),
            reassign_tagword_var(LHSVar, ToOrMask, ToOrRval, AssignLeftCode,
                CI, !CLD),
            Code = AssignRightCode ++ AssignLeftCode
        )
    ).

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
        [ArgMode | ArgModes], Code, CI, !CLD) :-
    generate_deconstruct_unify_arg(FieldAndArgVar, ArgMode, HeadCode,
        CI, !CLD),
    generate_deconstruct_unify_args(FieldsAndArgVars, ArgModes, TailCode,
        CI, !CLD),
    Code = HeadCode ++ TailCode.

:- pred generate_deconstruct_tagword_unify_args(prog_var::in,
    list(arg_and_width(prog_var))::in, list(unify_mode)::in,
    list(rval)::in, list(rval)::out, uint::in, uint::out,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_tagword_unify_args(_LHSVar, [], [],
        !ToOrRvals, !ToOrMask, empty, _CI, !CLD).
generate_deconstruct_tagword_unify_args(_LHSVar, [], [_ | _],
        !ToOrRvals, !ToOrMask, _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_tagword_unify_args(_LHSVar, [_ | _], [],
        !ToOrRvals, !ToOrMask, _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_tagword_unify_args(LHSVar,
        [RRHSVarWidth | RRHSVarsWidths], [ArgMode | ArgModes],
        !ToOrRvals, !ToOrMask, Code, CI, !CLD) :-
    RRHSVarWidth = arg_and_width(RHSVar, ArgPosWidth),
    generate_deconstruct_tagword_unify_arg(LHSVar, RHSVar, ArgPosWidth,
        ArgMode, !ToOrRvals, !ToOrMask, HeadCode, CI, !CLD),
    generate_deconstruct_tagword_unify_args(LHSVar, RRHSVarsWidths, ArgModes,
        !ToOrRvals, !ToOrMask, TailCode, CI, !CLD),
    Code = HeadCode ++ TailCode.

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
:- pred generate_deconstruct_tagword_unify_arg(prog_var::in, prog_var::in,
    arg_pos_width::in, unify_mode::in,
    list(rval)::in, list(rval)::out, uint::in, uint::out,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_tagword_unify_arg(LHSVar, RHSVar, ArgPosWidth, ArgMode,
        !ToOrRvals, !ToOrMask, Code, CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    get_vartypes(CI, VarTypes),
    lookup_var_type(VarTypes, RHSVar, RHSType),
    compute_assign_direction(ModuleInfo, ArgMode, RHSType, Dir),
    (
        Dir = assign_right,
        ( if variable_is_forward_live(!.CLD, RHSVar) then
            generate_deconstruct_tagword_assign_right(LHSVar, RHSVar,
                ArgPosWidth, Code, !CLD)
        else
            Code = empty
        )
    ;
        Dir = assign_left,
        generate_deconstruct_tagword_assign_left(RHSVar, ArgPosWidth,
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
    RightField = uni_field(RightPtag, RightBaseRval, RightOffset, ArgPosWidth),
    (
        ArgPosWidth = apw_full(_, _),
        RightLval = field(yes(RightPtag), RightBaseRval,
            const(llconst_int(RightOffset))),
        assign_lval_to_var(LeftVar, RightLval, Code, CI, !CLD)
    ;
        ArgPosWidth = apw_double(_, _, _),
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
            ArgPosWidth = apw_partial_first(_, _, Shift, _, Mask, Fill)
        ;
            ArgPosWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill)
        ),
        % XXX ARG_PACK factor out the following code
        RightLval = field(yes(RightPtag), RightBaseRval,
            const(llconst_int(RightOffset))),
        RightRval0 = right_shift_rval(lval(RightLval), Shift),
        Mask = arg_mask(MaskInt),
        MaskedRightRval0 = binop(bitwise_and(int_type_uint), RightRval0,
            const(llconst_int(MaskInt))),
        maybe_cast_masked_off_rval(Fill, MaskedRightRval0, MaskedRightRval),
        assign_field_lval_expr_to_var(LeftVar, [RightLval], MaskedRightRval,
            Code, !CLD)
    ;
        ( ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_none_shifted(_, _)
        ),
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
        Code = empty
    ).

:- pred generate_deconstruct_tagword_assign_right(prog_var::in, prog_var::in,
    arg_pos_width::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_tagword_assign_right(LHSVar, RHSVar, ArgPosWidth, Code,
        !CLD) :-
    (
        ArgPosWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill),
        LeftRval0 = right_shift_rval(var(LHSVar), Shift),
        Mask = arg_mask(MaskInt),
        MaskedLeftRval0 = binop(bitwise_and(int_type_uint), LeftRval0,
            const(llconst_int(MaskInt))),
        maybe_cast_masked_off_rval(Fill, MaskedLeftRval0, MaskedLeftRval),
        assign_expr_to_var(RHSVar, MaskedLeftRval, Code, !CLD)
    ;
        ArgPosWidth = apw_none_shifted(_, _),
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
        Code = empty
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        unexpected($pred, "ArgPosWidth does not belong in tagword")
    ).

:- pred generate_deconstruct_assign_left(uni_field::in, prog_var::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_assign_left(LHSField, RHSVar, Code, CI, !CLD) :-
    LHSField = uni_field(LHSPtag, LHSBaseRval0, LHSOffset, ArgPosWidth),
    % Assignment from a variable to an lvalue - cannot cache
    % so generate immediately.
    produce_variable(RHSVar, ProduceRHSVarCode, RHSRval, CI, !CLD),
    materialize_vars_in_rval(LHSBaseRval0, LHSBaseRval,
        MaterializeLHSBaseCode, CI, !CLD),
    (
        ArgPosWidth = apw_full(_, _),
        LHSLval = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSOffset))),
        AssignCode = singleton(llds_instr(assign(LHSLval, RHSRval),
            "Copy value"))
    ;
        ArgPosWidth = apw_double(_, _, _),
        LHSLvalA = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSOffset))),
        LHSLvalB = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSOffset + 1))),
        SrcA = unop(dword_float_get_word0, RHSRval),
        SrcB = unop(dword_float_get_word1, RHSRval),
        Comment = "Update double word",
        AssignCode = from_list([
            llds_instr(assign(LHSLvalA, SrcA), Comment),
            llds_instr(assign(LHSLvalB, SrcB), Comment)
        ])
    ;
        (
            ArgPosWidth = apw_partial_first(_, _, Shift, _, Mask, Fill)
        ;
            ArgPosWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill)
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
        LHSLval = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSOffset))),
        ComplementMask = const(llconst_int(\ (MaskInt << ShiftInt))),
        MaskOld = binop(bitwise_and(int_type_uint),
            lval(LHSLval), ComplementMask),
        ShiftedRHSRval = left_shift_rval(RHSRval, Shift, Fill),
        CombinedRval = bitwise_or_two_rvals(MaskOld, ShiftedRHSRval),
        AssignCode = singleton(llds_instr(assign(LHSLval, CombinedRval),
            "Update part of word"))
    ;
        ( ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_none_shifted(_, _)
        ),
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
        % XXX Should we try to avoid generating ProduceRHSVarCode
        % and MaterializeLHSBaseCode as well? MaterializeLHSBaseCode
        % is probably needed by other, non-dummy fields, and
        % ProduceRHSVarCode is probably very cheap, so probably not.
        AssignCode = empty
    ),
    Code = ProduceRHSVarCode ++ MaterializeLHSBaseCode ++ AssignCode.

:- pred generate_deconstruct_tagword_assign_left(prog_var::in,
    arg_pos_width::in,
    list(rval)::in, list(rval)::out, uint::in, uint::out) is det.

generate_deconstruct_tagword_assign_left(RightVar, ArgPosWidth,
        !ToOrRvals, !ToOrMask) :-
    (
        ArgPosWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill),
        Shift = arg_shift(ShiftInt),
        Mask = arg_mask(MaskInt),
        LeftShiftedRightRval = left_shift_rval(var(RightVar), Shift, Fill),
        !:ToOrRvals = [LeftShiftedRightRval | !.ToOrRvals],
        !:ToOrMask = (uint.cast_from_int(MaskInt) << ShiftInt) \/ !.ToOrMask
    ;
        ArgPosWidth = apw_none_shifted(_, _)
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        unexpected($pred, "ArgPosWidth is not a packed arg_pos_width")
    ).

%---------------------------------------------------------------------------%

    % Generate a direct arg unification between
    % - the left-hand-side (the whole term), and
    % - the right-hand-side (the one argument).
    %
:- pred generate_direct_arg_deconstruct(prog_var::in, prog_var::in,
    ptag::in, unify_mode::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_direct_arg_deconstruct(LHSVar, RHSVar, Ptag, ArgMode, Code,
        CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    RHSType = variable_type(CI, RHSVar),
    compute_assign_direction(ModuleInfo, ArgMode, RHSType, Dir),
    (
        Dir = assign_right,
        ( if variable_is_forward_live(!.CLD, RHSVar) then
            Ptag = ptag(PtagUint8),
            LHSBodyRval = binop(body, var(LHSVar),
                const(llconst_int(uint8.cast_to_int(PtagUint8)))),
            assign_expr_to_var(RHSVar, LHSBodyRval, Code, !CLD)
        else
            Code = empty
        )
    ;
        Dir = assign_left,
        reassign_mkword_hole_var(LHSVar, Ptag, var(RHSVar), Code, !CLD)
    ;
        Dir = assign_unused,
        % XXX This will have to change if we start to support aliasing.
        Code = empty
    ).

%---------------------------------------------------------------------------%

:- pred make_fields_and_arg_vars(vartypes::in, rval::in, ptag::in,
    list(arg_and_width(prog_var))::in,
    list(field_and_arg_var)::out(list_skel(field_and_arg_var))) is det.

make_fields_and_arg_vars(_, _, _, [], []).
make_fields_and_arg_vars(VarTypes, Rval, Ptag, [VarWidth | VarsWidths],
        [FieldAndArgVar | FieldsAndArgVars]) :-
    VarWidth = arg_and_width(Var, ArgPosWidth),
    (
        ( ArgPosWidth = apw_full(_, CellOffset)
        ; ArgPosWidth = apw_partial_first(_, CellOffset, _, _, _, _)
        ; ArgPosWidth = apw_double(_, CellOffset, _)
        ; ArgPosWidth = apw_partial_shifted(_, CellOffset, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, CellOffset)
        ),
        CellOffset = cell_offset(CellOffsetInt)
    ;
        ArgPosWidth = apw_none_nowhere,
        CellOffsetInt = -1
    ),
    % The CellOffsetInt duplicates information that is also in ArgPosWidth,
    % but this way, we compute it in just one place (here), instead of
    % all of the (about ten) places where ArgPosWidth is used.
    Field = uv_field(uni_field(Ptag, Rval, CellOffsetInt, ArgPosWidth)),
    lookup_var_type(VarTypes, Var, Type),
    FieldAndArgVar = field_and_arg_var(Field, Var, Type),
    make_fields_and_arg_vars(VarTypes, Rval, Ptag, VarsWidths,
        FieldsAndArgVars).

%---------------------------------------------------------------------------%
:- end_module ll_backend.unify_gen_deconstruct.
%---------------------------------------------------------------------------%
