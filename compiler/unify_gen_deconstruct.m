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

    % Generate a subunification between two [field | variable].
    %
:- pred generate_deconstruct_no_tag_unify_arg(prog_var::in, prog_var::in,
    mer_type::in, unify_mode::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module ll_backend.unify_gen_test.
:- import_module ll_backend.unify_gen_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.

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
        produce_variable(LHSVar, ProduceVarCode, VarRval, !CLD),
        ( if VarRval = lval(VarLval) then
            save_reused_cell_fields(LHSVar, VarLval, SaveArgsCode, Regs, !CLD),
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
    produce_variable(LHSVar, LHSVarCode, LHSVarRval, !CLD),
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
    % deconstruction, we know the value of the cons_id, so we don't need
    % to generate a test for it.
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
        get_notag_or_direct_arg_arg_mode(RHSVars, ArgModes, RHSVar, ArgMode),
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
                assign_const_to_var(RHSVar, const(llconst_int(0)), CI, !CLD)
            else
                true
            ),
            Code = empty
        ;
            IsDummy = is_not_dummy_type,
            RHSType = variable_type(CI, RHSVar),
            generate_deconstruct_no_tag_unify_arg(LHSVar, RHSVar, RHSType,
                ArgMode, Code, CI, !CLD)
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        get_notag_or_direct_arg_arg_mode(RHSVars, ArgModes, RHSVar, ArgMode),
        generate_direct_arg_deconstruct(LHSVar, RHSVar, Ptag, ArgMode,
            Code, CI, !CLD)
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        LHSBaseRval = var(LHSVar),
        get_vartypes(CI, VarTypes),
        associate_cons_id_args_with_widths(ModuleInfo, ConsId,
            RHSVars, RHSVarsWidths),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                LHSPtag = ptag(0u8)
            ;
                RemoteArgsTagInfo = remote_args_unshared(LHSPtag)
            ),
            generate_deconstruct_unify_args(VarTypes, LHSPtag, LHSBaseRval,
                RHSVarsWidths, ArgModes, Code, CI, !CLD)
        ;
            RemoteArgsTagInfo = remote_args_shared(LHSPtag, RemoteSectag),
            RemoteSectag = remote_sectag(_SectagUint, SectagSize),
            (
                SectagSize = rsectag_word,
                generate_deconstruct_unify_args(VarTypes, LHSPtag, LHSBaseRval,
                    RHSVarsWidths, ArgModes, Code, CI, !CLD)
            ;
                SectagSize = rsectag_subword(_),
                take_tagword_args(RHSVarsWidths, ArgModes,
                    TagwordRHSVarsWidths, TagwordArgModes,
                    NonTagwordRHSVarsWidths, NonTagwordArgModes),
                LHSSectagWordLval = field(yes(LHSPtag), LHSBaseRval,
                    const(llconst_int(0))),
                LHSSectagWordRval0 = lval(LHSSectagWordLval),
                LHSSectagWordRval = LHSSectagWordRval0,
                MaterializeTagwordCode = empty,
                generate_deconstruct_tagword_unify_args(LHSSectagWordRval,
                    TagwordRHSVarsWidths, TagwordArgModes, [LHSSectagWordLval],
                    [], ToOrRvals, 0u, ToOrMask, AssignRightCode, CI, !CLD),
                (
                    ToOrRvals = [],
                    TagwordCode = MaterializeTagwordCode ++ AssignRightCode
                ;
                    ToOrRvals = [HeadToOrRval | TailToOrRvals],
                    ToOrRval0 =
                        bitwise_or_some_rvals(HeadToOrRval, TailToOrRvals),
                    materialize_vars_in_rval(ToOrRval0, ToOrRval,
                        ToOrRvalCode, !CLD),
                    ComplementMask = const(llconst_uint(\ ToOrMask)),
                    MaskedOldSectagWordRval = binop(bitwise_and(int_type_uint),
                        lval(LHSSectagWordLval), ComplementMask),
                    NewSectagWordRval = binop(bitwise_or(int_type_uint),
                        MaskedOldSectagWordRval, ToOrRval),
                    Comment = "updating tagword",
                    AssignLeftCode = singleton(llds_instr(
                        assign(LHSSectagWordLval, NewSectagWordRval),
                        Comment)),
                    TagwordCode = MaterializeTagwordCode ++
                        AssignRightCode ++ ToOrRvalCode ++ AssignLeftCode
                ),
                generate_deconstruct_unify_args(VarTypes, LHSPtag, LHSBaseRval,
                    NonTagwordRHSVarsWidths, NonTagwordArgModes,
                    NonTagwordCode, CI, !CLD),
                Code = TagwordCode ++ NonTagwordCode
            )
        ;
            RemoteArgsTagInfo = remote_args_ctor(_Data),
            % These are supported only on the MLDS backend.
            unexpected($pred, "remote_args_ctor")
        )
    ;
        ConsTag = local_args_tag(_),
        associate_cons_id_args_with_widths(ModuleInfo, ConsId,
            RHSVars, RHSVarsWidths),
        generate_deconstruct_tagword_unify_args(var(LHSVar),
            RHSVarsWidths, ArgModes, [], [], ToOrRvals, 0u, ToOrMask,
            AssignRightCode, CI, !CLD),
        (
            ToOrRvals = [],
            Code = AssignRightCode
        ;
            ToOrRvals = [HeadToOrRval | TailToOrRvals],
            ToOrRval = bitwise_or_some_rvals(HeadToOrRval, TailToOrRvals),
            reassign_tagword_var(LHSVar, ToOrMask, ToOrRval, AssignLeftCode,
                !CLD),
            Code = AssignRightCode ++ AssignLeftCode
        )
    ).

%---------------------------------------------------------------------------%

    % Generate code to perform a list of deterministic subunifications
    % for the arguments of a construction.
    %
:- pred generate_deconstruct_unify_args(vartypes::in, ptag::in, rval::in,
    list(arg_and_width(prog_var))::in, list(unify_mode)::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_unify_args(_, _, _, [], [], empty, _CI, !CLD).
generate_deconstruct_unify_args(_, _, _, [], [_ | _], _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_unify_args(_, _, _, [_ | _], [], _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_unify_args(VarTypes, LHSPtag, LHSBaseRval,
        [RHSVarLHSWidth | RHSVarsLHSWidths], [ArgMode | ArgModes],
        Code, CI, !CLD) :-
    RHSVarLHSWidth = arg_and_width(RHSVar, LHSArgPosWidth),
    lookup_var_type(VarTypes, RHSVar, RHSType),
    generate_deconstruct_unify_arg(LHSPtag, LHSBaseRval, LHSArgPosWidth,
        RHSVar, RHSType, ArgMode, HeadCode, CI, !CLD),
    generate_deconstruct_unify_args(VarTypes, LHSPtag, LHSBaseRval,
        RHSVarsLHSWidths, ArgModes, TailCode, CI, !CLD),
    Code = HeadCode ++ TailCode.

:- pred generate_deconstruct_unify_arg(ptag::in, rval::in, arg_pos_width::in,
    prog_var::in, mer_type::in, unify_mode::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_unify_arg(LHSPtag, LHSBaseRval, LHSArgPosWidth,
        RHSVar, RHSType, ArgMode, Code, CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    compute_assign_direction(ModuleInfo, ArgMode, RHSType, Dir),
    (
        Dir = assign_right,
        ( if variable_is_forward_live(!.CLD, RHSVar) then
            generate_deconstruct_assign_right(LHSPtag, LHSBaseRval,
                LHSArgPosWidth, RHSVar, Code, CI, !CLD)
        else
            Code = empty
        )
    ;
        Dir = assign_left,
        % Fields are always considered forward live.
        generate_deconstruct_assign_left(LHSPtag, LHSBaseRval, LHSArgPosWidth,
            RHSVar, Code, !CLD)
    ;
        Dir = assign_unused,
        % XXX This will have to change if we start to support aliasing.
        Code = empty
    ).

generate_deconstruct_no_tag_unify_arg(LHSVar, RHSVar, RHSType, ArgMode,
        Code, CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    compute_assign_direction(ModuleInfo, ArgMode, RHSType, Dir),
    (
        Dir = assign_right,
        ( if variable_is_forward_live(!.CLD, RHSVar) then
            assign_var_to_var(RHSVar, LHSVar, !CLD),
            Code = empty
        else
            Code = empty
        )
    ;
        Dir = assign_left,
        ( if variable_is_forward_live(!.CLD, LHSVar) then
            assign_var_to_var(LHSVar, RHSVar, !CLD)
        else
            true
        ),
        Code = empty
    ;
        Dir = assign_unused,
        % XXX This will have to change if we start to support aliasing.
        Code = empty
    ).

:- pred generate_deconstruct_tagword_unify_args(rval::in,
    list(arg_and_width(prog_var))::in, list(unify_mode)::in, list(lval)::in,
    list(rval)::in, list(rval)::out, uint::in, uint::out,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_tagword_unify_args(_LHSRval, [], [], _,
        !ToOrRvals, !ToOrMask, empty, _CI, !CLD).
generate_deconstruct_tagword_unify_args(_LHSRval, [], [_ | _], _,
        !ToOrRvals, !ToOrMask, _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_tagword_unify_args(_LHSRval, [_ | _], [], _,
        !ToOrRvals, !ToOrMask, _, _, !CLD) :-
    unexpected($pred, "length mismatch").
generate_deconstruct_tagword_unify_args(LHSRval,
        [RHSVarWidth | RHSVarsWidths], [ArgMode | ArgModes], FieldLvals,
        !ToOrRvals, !ToOrMask, Code, CI, !CLD) :-
    generate_deconstruct_tagword_unify_arg(LHSRval, RHSVarWidth,
        ArgMode, FieldLvals, !ToOrRvals, !ToOrMask, HeadCode, CI, !CLD),
    generate_deconstruct_tagword_unify_args(LHSRval, RHSVarsWidths,
        ArgModes, FieldLvals, !ToOrRvals, !ToOrMask, TailCode, CI, !CLD),
    Code = HeadCode ++ TailCode.

    % Unify (on the left)a word containing tags and packed arguments, and
    % (on the right) a sequence of argument variables. Generate code for
    % the assignments to the right, and update the state variables to help
    % our caller generate a single assignment to the left.
    %
:- pred generate_deconstruct_tagword_unify_arg(rval::in,
    arg_and_width(prog_var)::in, unify_mode::in, list(lval)::in,
    list(rval)::in, list(rval)::out, uint::in, uint::out,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_tagword_unify_arg(LHSRval, RHSVarWidth, ArgMode,
        FieldLvals, !ToOrRvals, !ToOrMask, Code, CI, !CLD) :-
    RHSVarWidth = arg_and_width(RHSVar, ArgPosWidth),
    get_module_info(CI, ModuleInfo),
    get_vartypes(CI, VarTypes),
    lookup_var_type(VarTypes, RHSVar, RHSType),
    compute_assign_direction(ModuleInfo, ArgMode, RHSType, Dir),
    (
        Dir = assign_right,
        ( if variable_is_forward_live(!.CLD, RHSVar) then
            generate_deconstruct_tagword_assign_right(LHSRval, RHSVar,
                ArgPosWidth, FieldLvals, Code, !CLD)
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

:- pred generate_deconstruct_assign_right(ptag::in, rval::in,
    arg_pos_width::in, prog_var::in,
    llds_code::out, code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_assign_right(LHSPtag, LHSBaseRval, LHSArgPosWidth,
        RHSVar, Code, CI, !CLD) :-
    (
        LHSArgPosWidth = apw_full(_, cell_offset(LHSCellOffset)),
        LHSLval = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSCellOffset))),
        assign_lval_to_var(RHSVar, LHSLval, Code, CI, !CLD)
    ;
        LHSArgPosWidth = apw_double(_, cell_offset(LHSCellOffset), _),
        LHSLvalA = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSCellOffset))),
        LHSLvalB = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSCellOffset + 1))),
        LHSRval = binop(float_from_dword, lval(LHSLvalA), lval(LHSLvalB)),
        assign_field_lval_expr_to_var(RHSVar, [LHSLvalA, LHSLvalB],
            LHSRval, Code, !CLD)
    ;
        (
            LHSArgPosWidth = apw_partial_first(_, cell_offset(LHSCellOffset),
                Shift, _, arg_mask(Mask), Fill)
        ;
            LHSArgPosWidth = apw_partial_shifted(_, cell_offset(LHSCellOffset),
                Shift, _, arg_mask(Mask), Fill)
        ),
        LHSLval = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSCellOffset))),
        LHSRval0 = right_shift_rval(lval(LHSLval), Shift),
        MaskedLHSRval0 = binop(bitwise_and(int_type_uint), LHSRval0,
            const(llconst_int(Mask))),
        maybe_cast_masked_off_rval(Fill, MaskedLHSRval0, MaskedLHSRval),
        assign_field_lval_expr_to_var(RHSVar, [LHSLval], MaskedLHSRval,
            Code, !CLD)
    ;
        ( LHSArgPosWidth = apw_none_nowhere
        ; LHSArgPosWidth = apw_none_shifted(_, _)
        ),
        % The value being assigned is of a dummy type, so no assignment
        % is actually necessary.
        Code = empty
    ).

:- pred generate_deconstruct_tagword_assign_right(rval::in, prog_var::in,
    arg_pos_width::in, list(lval)::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_tagword_assign_right(LHSRval, RHSVar, ArgPosWidth,
        FieldLvals, Code, !CLD) :-
    (
        ArgPosWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill),
        LeftRval0 = right_shift_rval(LHSRval, Shift),
        Mask = arg_mask(MaskInt),
        MaskedLeftRval0 = binop(bitwise_and(int_type_uint), LeftRval0,
            const(llconst_int(MaskInt))),
        maybe_cast_masked_off_rval(Fill, MaskedLeftRval0, MaskedLeftRval),
        (
            FieldLvals = [],
            assign_expr_to_var(RHSVar, MaskedLeftRval, Code, !CLD)
        ;
            FieldLvals = [_ | _],
            assign_field_lval_expr_to_var(RHSVar, FieldLvals, MaskedLeftRval,
                Code, !CLD)
        )
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

:- pred generate_deconstruct_assign_left(ptag::in, rval::in, arg_pos_width::in,
    prog_var::in, llds_code::out,
    code_loc_dep::in, code_loc_dep::out) is det.

generate_deconstruct_assign_left(LHSPtag, LHSBaseRval0, LHSArgPosWidth,
        RHSVar, Code, !CLD) :-
    % Assignment from a variable to an field in a memory cell;
    % we cannot cache this, so generate code for it immediately.
    produce_variable(RHSVar, ProduceRHSVarCode, RHSRval, !CLD),
    materialize_vars_in_rval(LHSBaseRval0, LHSBaseRval,
        MaterializeLHSBaseCode, !CLD),
    (
        LHSArgPosWidth = apw_full(_, cell_offset(LHSCellOffset)),
        LHSLval = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSCellOffset))),
        AssignCode = singleton(llds_instr(assign(LHSLval, RHSRval),
            "Copy value"))
    ;
        LHSArgPosWidth = apw_double(_, cell_offset(LHSCellOffset), _),
        LHSLvalA = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSCellOffset))),
        LHSLvalB = field(yes(LHSPtag), LHSBaseRval,
            const(llconst_int(LHSCellOffset + 1))),
        SrcA = unop(dword_float_get_word0, RHSRval),
        SrcB = unop(dword_float_get_word1, RHSRval),
        Comment = "Update double word",
        AssignCode = from_list([
            llds_instr(assign(LHSLvalA, SrcA), Comment),
            llds_instr(assign(LHSLvalB, SrcB), Comment)
        ])
    ;
        (
            LHSArgPosWidth = apw_partial_first(_, cell_offset(LHSCellOffset),
                Shift, _, Mask, Fill)
        ;
            LHSArgPosWidth = apw_partial_shifted(_, cell_offset(LHSCellOffset),
                Shift, _, Mask, Fill)
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
            const(llconst_int(LHSCellOffset))),
        ComplementMask = const(llconst_int(\ (MaskInt << ShiftInt))),
        MaskOld = binop(bitwise_and(int_type_uint),
            lval(LHSLval), ComplementMask),
        ShiftedRHSRval = left_shift_rval(RHSRval, Shift, Fill),
        CombinedRval = bitwise_or_two_rvals(MaskOld, ShiftedRHSRval),
        AssignCode = singleton(llds_instr(assign(LHSLval, CombinedRval),
            "Update part of word"))
    ;
        ( LHSArgPosWidth = apw_none_nowhere
        ; LHSArgPosWidth = apw_none_shifted(_, _)
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

generate_deconstruct_tagword_assign_left(RHSVar, ArgPosWidth,
        !ToOrRvals, !ToOrMask) :-
    (
        ArgPosWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill),
        Shift = arg_shift(ShiftInt),
        Mask = arg_mask(MaskInt),
        LeftShiftedRHSRval = left_shift_rval(var(RHSVar), Shift, Fill),
        !:ToOrRvals = [LeftShiftedRHSRval | !.ToOrRvals],
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

:- pred take_tagword_args(
    list(arg_and_width(prog_var))::in, list(unify_mode)::in,
    list(arg_and_width(prog_var))::out, list(unify_mode)::out,
    list(arg_and_width(prog_var))::out, list(unify_mode)::out) is det.

take_tagword_args([], [], [], [], [], []).
take_tagword_args([], [_ | _], _, _, _, _) :-
    unexpected($pred, "length mismatch").
take_tagword_args([_ | _], [], _, _, _, _) :-
    unexpected($pred, "length mismatch").
take_tagword_args([VarWidth | VarsWidths], [ArgMode | ArgModes],
        TagwordVarsWidths, TagwordArgModes,
        NonTagwordVarsWidths, NonTagwordArgModes) :-
    VarWidth = arg_and_width(_, ArgPosWidth),
    (
        ( ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, _)
        ),
        take_tagword_args(VarsWidths, ArgModes,
            TailTagwordVarsWidths, TailTagwordArgModes,
            NonTagwordVarsWidths, NonTagwordArgModes),
        TagwordVarsWidths = [VarWidth | TailTagwordVarsWidths],
        TagwordArgModes = [ArgMode | TailTagwordArgModes]
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        TagwordVarsWidths = [],
        TagwordArgModes = [],
        NonTagwordVarsWidths = [VarWidth | VarsWidths],
        NonTagwordArgModes = [ArgMode | ArgModes]
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
            ( if PtagUint8 = 0u8 then
                % Masking off the ptag would be a null operation,
                % since it is already all zeroes.
                assign_var_to_var(RHSVar, LHSVar, !CLD),
                Code = empty
            else
                LHSBodyRval = binop(body, var(LHSVar),
                    const(llconst_int(uint8.cast_to_int(PtagUint8)))),
                assign_expr_to_var(RHSVar, LHSBodyRval, Code, !CLD)
            )
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
:- end_module ll_backend.unify_gen_deconstruct.
%---------------------------------------------------------------------------%
