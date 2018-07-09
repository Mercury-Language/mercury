%---------------------------------------------------------------------------e
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------e
% Copyright (C) 1994-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module ll_backend.unify_gen_construct.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module ll_backend.code_info.
:- import_module ll_backend.code_loc_dep.
:- import_module ll_backend.global_data.
:- import_module ll_backend.llds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module maybe.

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

%---------------------------------------------------------------------------%

:- pred generate_ground_term(prog_var::in, hlds_goal::in,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

%---------------------------------------------------------------------------%

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
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.closure_gen.
:- import_module ll_backend.code_util.
:- import_module ll_backend.layout.
:- import_module ll_backend.unify_gen_deconstruct.
:- import_module ll_backend.unify_gen_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint.

%---------------------------------------------------------------------------%

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
                % Information can flow to the left as well as to the right
                % in deconstructions.
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
    VarWidth = Var - ArgPosWidth,
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ; ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, _)
        ),
        generate_and_pack_construct_word(Var, ArgPosWidth, VarsWidths,
            ArgMode, ArgModes, LeftOverVarsWidths, LeftOverArgModes,
            !CurArgNum, !TakeAddr, HeadCellArg, !MayUseAtomic, !Code,
            CI, !CLD),
        generate_and_pack_construct_args(LeftOverVarsWidths, LeftOverArgModes,
            !.CurArgNum, !.TakeAddr,
            TailCellArgs, !MayUseAtomic, !Code, CI, !CLD),
        CellArgs = [HeadCellArg | TailCellArgs]
    ;
        ArgPosWidth = apw_none_nowhere,
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

generate_and_pack_construct_word(Var, ArgPosWidth, VarsWidths,
        ArgMode, ArgModes, LeftOverVarsWidths, LeftOverArgModes,
        CurArgNum, LeftOverArgNum, !TakeAddr, CellArg,
        !MayUseAtomic, !Code, CI, !CLD) :-
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
            ArgPosWidth = apw_full(_, _),
            (
                LCMCNull = no,
                MaybeNull = no
            ;
                LCMCNull = yes,
                MaybeNull = yes(const(llconst_int(0)))
            ),
            CellArg = cell_arg_take_addr_one_word(Var, MaybeNull)
        ;
            ArgPosWidth = apw_double(_, _, _),
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
            ( ArgPosWidth = apw_partial_first(_, _, _, _, _)
            ; ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
            ),
            unexpected($pred, "taking address of partial word")
        ;
            ArgPosWidth = apw_none_shifted(_, _),
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
                ArgPosWidth = apw_full(_, _),
                (
                    IsReal = not_real_input_arg,
                    CellArg = cell_arg_skip_one_word
                ;
                    IsReal = real_input_arg,
                    CellArg = cell_arg_full_word(Rval, complete)
                )
            ;
                ArgPosWidth = apw_double(_, _, _),
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
            ArgPosWidth = apw_partial_first(_, _, _, _, Fill),
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
            ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _),
            unexpected($pred, "apw_partial_shifted")
        ;
            ArgPosWidth = apw_none_shifted(_, _),
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
    VarWidth = Var - ArgPosWidth,
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ),
        % This argument is not part of this word.
        LeftOverVarsWidths = [VarWidth | VarsWidths],
        LeftOverArgModes = [ArgMode | ArgModes],
        LeftOverArgNum = CurArgNum
    ;
        ( ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, _)
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
            ArgPosWidth = apw_partial_shifted(_, _, Shift, _, _, Fill),
            (
                IsReal = not_real_input_arg,
                !:Completeness = incomplete
            ;
                IsReal = real_input_arg,
                ShiftedArgRval = left_shift_rval(ArgRval, Shift, Fill),
                !:RevToOrRvals = [ShiftedArgRval | !.RevToOrRvals]
            )
        ;
            ArgPosWidth = apw_none_shifted(_, _)
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
    VarWidth = Var - ArgPosWidth,
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ),
        unexpected($pred, "ArgPosWidth is not a packed arg_pos_width")
    ;
        ArgPosWidth = apw_partial_shifted(_, _, Shift, _, _, Fill),
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
        ArgPosWidth = apw_none_shifted(_, _)
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

:- pred var_type_msg(mer_type::in, string::out) is det.

var_type_msg(Type, Msg) :-
    type_to_ctor_det(Type, TypeCtor),
    TypeCtor = type_ctor(TypeSym, TypeArity),
    TypeSymStr = sym_name_to_string(TypeSym),
    string.int_to_string(TypeArity, TypeArityStr),
    Msg = TypeSymStr ++ "/" ++ TypeArityStr.

%---------------------------------------------------------------------------%

:- type field_addr
    --->    field_addr(
                fa_offset   :: int,
                fa_var      :: prog_var
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
    VarWidth = _Var - ArgPosWidth,
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ),
        needs_update_args_to_words(VarsWidths, ArgNUs, TailWordNUs),
        WordNUs = [ArgNU | TailWordNUs]
    ;
        ArgPosWidth = apw_partial_first(_, _, _, _, _),
        does_any_arg_in_word_need_update(VarsWidths, ArgNUs, ArgNU, WordNU,
            LaterWordVarsWidths, LaterWordArgNUs),
        needs_update_args_to_words(LaterWordVarsWidths, LaterWordArgNUs,
            TailWordNUs),
        WordNUs = [WordNU | TailWordNUs]
    ;
        ArgPosWidth = apw_none_nowhere,
        needs_update_args_to_words(VarsWidths, ArgNUs, WordNUs)
    ;
        ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _),
        unexpected($pred, "apw_partial_shifted")
    ;
        ArgPosWidth = apw_none_shifted(_, _),
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
    VarWidth = _Var - ArgPosWidth,
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        LaterWordVarsWidths = [VarWidth | VarsWidths],
        LaterWordArgNUs = [ArgNU | ArgNUs]
    ;
        ( ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, _)
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
:- end_module ll_backend.unify_gen_construct.
%---------------------------------------------------------------------------%
