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
:- pred generate_construction_unification(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, how_to_construct::in,
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
:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module backend_libs.type_class_info.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.const_struct.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module ll_backend.closure_gen.
:- import_module ll_backend.code_util.
:- import_module ll_backend.layout.
:- import_module ll_backend.unify_gen_deconstruct.
:- import_module ll_backend.unify_gen_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module uint16.

%---------------------------------------------------------------------------%

generate_construction_unification(LHSVar, ConsId, RHSVars, ArgModes,
        HowToConstruct0, TakeAddr, MaybeSize, GoalInfo, Code, !CI, !CLD) :-
    get_module_info(!.CI, ModuleInfo),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    associate_cons_id_args_with_widths(ModuleInfo, ConsId,
        RHSVars, RHSVarsWidths),
    (
        % Constants.
        ( ConsTag = int_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = string_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = dummy_tag
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        expect(unify(RHSVars, []), $pred, "constant has arguments"),
        (
            ConsTag = int_tag(IntTag),
            int_tag_to_const_and_int_type(IntTag, Const, _),
            ConstRval = const(Const)
        ;
            ConsTag = float_tag(Float),
            ConstRval = const(llconst_float(Float))
        ;
            ConsTag = string_tag(String),
            ConstRval = const(llconst_string(String))
        ;
            ConsTag = foreign_tag(Lang, Value),
            expect(unify(Lang, lang_c), $pred,
                "foreign_tag for language other than C"),
            ConstRval = const(llconst_foreign(Value, lt_int(int_type_int)))
        ;
            ConsTag = dummy_tag,
            % XXX The assignment is likely to be dead code, but *proving*
            % that the assigned-to variable is never used is difficult.
            ConstRval = const(llconst_int(0))
        ;
            ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, _MustMask),
            LocalSectag = local_sectag(_, PrimSec, _),
            ConstRval = const(llconst_uint(PrimSec))
        ;
            ( ConsTag = type_info_const_tag(ConstNum)
            ; ConsTag = typeclass_info_const_tag(ConstNum)
            ; ConsTag = ground_term_const_tag(ConstNum, _)
            ),
            get_const_struct_map(!.CI, ConstStructMap),
            map.lookup(ConstStructMap, ConstNum, TypedRval),
            TypedRval = typed_rval(ConstRval, _Type)
        ;
            ConsTag = type_ctor_info_tag(ModuleName, TypeName, TypeArity),
            RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName,
                uint16.det_from_int(TypeArity)),
            DataId = rtti_data_id(ctor_rtti_id(RttiTypeCtor,
                type_ctor_type_ctor_info)),
            ConstRval = const(llconst_data_addr(DataId, no))
        ;
            ConsTag = base_typeclass_info_tag(ModuleName, ClassId, Instance),
            TCName = generate_class_name(ClassId),
            DataId = rtti_data_id(tc_rtti_id(TCName,
                type_class_base_typeclass_info(ModuleName, Instance))),
            ConstRval = const(llconst_data_addr(DataId, no))
        ;
            ConsTag = deep_profiling_proc_layout_tag(PredId, ProcId),
            RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId),
            Origin = RttiProcLabel ^ rpl_pred_info_origin,
            ( if Origin = origin_special_pred(_, _) then
                UserOrUCI = uci
            else
                UserOrUCI = user
            ),
            ProcKind = proc_layout_proc_id(UserOrUCI),
            DataId = layout_id(proc_layout(RttiProcLabel, ProcKind)),
            ConstRval = const(llconst_data_addr(DataId, no))
        ;
            ConsTag = tabling_info_tag(PredId, ProcId),
            ProcLabel = make_proc_label(ModuleInfo, PredId, ProcId),
            DataId = proc_tabling_data_id(ProcLabel, tabling_info),
            ConstRval = const(llconst_data_addr(DataId, no))
        ;
            ConsTag = table_io_entry_tag(PredId, ProcId),
            PredProcId = proc(PredId, ProcId),
            DataId = layout_slot_id(table_io_entry_id, PredProcId),
            ConstRval = const(llconst_data_addr(DataId, no))
        ),
        assign_const_to_var(LHSVar, ConstRval, !.CI, !CLD),
        Code = empty
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        FirstArgNum0 = 1,
        get_may_use_atomic_alloc(!.CI, MayUseAtomic0),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8)
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag)
            ),
            TagwordCode = empty,
            generate_and_pack_construct_args(RHSVarsWidths, ArgModes,
                FirstArgNum0, TakeAddr, CellArgs, MayUseAtomic0, MayUseAtomic,
                empty, NonTagwordCode, !.CI, !CLD)
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            RemoteSectag = remote_sectag(SectagUint, SectagSize),
            (
                SectagSize = rsectag_word,
                FirstArgNum = FirstArgNum0,
                TagwordRval = const(llconst_uint(SectagUint)),
                NonTagwordRHSVarsWidths = RHSVarsWidths,
                NonTagwordArgModes = ArgModes,
                TagwordCode = empty
            ;
                SectagSize = rsectag_subword(_),
                take_tagword_args_widths_modes(RHSVarsWidths, ArgModes,
                    TagwordRHSVarsWidths, TagwordArgModes,
                    NonTagwordRHSVarsWidths, NonTagwordArgModes,
                    FirstArgNum0, FirstArgNum),
                ( if SectagUint = 0u then
                    RevToOrRvals0 = []
                else
                    RevToOrRvals0 = [const(llconst_uint(SectagUint))]
                ),
                generate_and_pack_tagword(
                    TagwordRHSVarsWidths, TagwordArgModes,
                    RevToOrRvals0, RevToOrRvals, !.CI),
                list.reverse(RevToOrRvals, ToOrRvals),
                TagwordRval0 = bitwise_or_rvals(ToOrRvals),
                materialize_vars_in_rval(TagwordRval0, TagwordRval,
                    TagwordCode, !CLD)
            ),
            TagwordArg = cell_arg_full_word(TagwordRval, complete),
            generate_and_pack_construct_args(
                NonTagwordRHSVarsWidths, NonTagwordArgModes,
                FirstArgNum, TakeAddr, CellArgs0,
                MayUseAtomic0, MayUseAtomic, empty, NonTagwordCode,
                !.CI, !CLD),
            CellArgs = [TagwordArg | CellArgs0]
        ;
            RemoteArgsTagInfo = remote_args_ctor(_),
            % These are supported only on the MLDS backend.
            unexpected($pred, "remote_args_ctor")
        ),
        pack_how_to_construct(RHSVarsWidths, HowToConstruct0, HowToConstruct),
        Context = goal_info_get_context(GoalInfo),
        construct_cell(LHSVar, Ptag, CellArgs, HowToConstruct, MaybeSize,
            Context, MayUseAtomic, ConstructCode, !CI, !CLD),
        Code = TagwordCode ++ NonTagwordCode ++ ConstructCode
    ;
        ConsTag = local_args_tag(LocalArgsTagInfo),
        expect(unify(TakeAddr, []), $pred, "local_args_tag, TakeAddr != []"),
        maybe_accumulate_local_sectag(LocalArgsTagInfo, RevToOrRvals0),
        generate_and_pack_tagword(RHSVarsWidths, ArgModes,
            RevToOrRvals0, RevToOrRvals, !.CI),
        list.reverse(RevToOrRvals, ToOrRvals),
        PackedRval = bitwise_or_rvals(ToOrRvals),
        assign_expr_to_var(LHSVar, PackedRval, Code, !CLD)
    ;
        ConsTag = no_tag,
        expect(unify(TakeAddr, []), $pred, "notag: take_addr"),
        get_notag_or_direct_arg_arg_mode(RHSVars, ArgModes, RHSVar, ArgMode),
        RHSType = variable_type(!.CI, RHSVar),
        % Information can flow to the left as well as to the right
        % in deconstructions.
        generate_deconstruct_no_tag_unify_arg(LHSVar, RHSVar, RHSType, ArgMode,
            Code, !.CI, !CLD)
    ;
        ConsTag = direct_arg_tag(Ptag),
        expect(unify(TakeAddr, []), $pred, "direct_arg_tag: take_addr"),
        get_notag_or_direct_arg_arg_mode(RHSVars, ArgModes, RHSVar, ArgMode),
        Type = variable_type(!.CI, RHSVar),
        generate_direct_arg_construct(LHSVar, RHSVar, Ptag,
            ArgMode, Type, Code, !.CI, !CLD)
    ;
        ConsTag = closure_tag(PredId, ProcId, EvalMethod),
        expect(unify(TakeAddr, []), $pred, "closure_tag has take_addr"),
        expect(unify(MaybeSize, no), $pred, "closure_tag has size"),
        construct_closure(PredId, ProcId, EvalMethod, LHSVar, RHSVars,
            GoalInfo, Code, !CI, !CLD)
    ).

    % Create a list of cell_args for the argument words or double words
    % for a construction unification, while packing sub-word arguments
    % into words.
    %
:- pred generate_and_pack_construct_args(
    list(arg_and_width(prog_var))::in, list(unify_mode)::in,
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
generate_and_pack_construct_args([RHSVarWidth | RHSVarsWidths],
        [ArgMode | ArgModes], CurArgNum, !.TakeAddr, CellArgs,
        !MayUseAtomic, !Code, CI, !CLD) :-
    RHSVarWidth = arg_and_width(RHSVar, ArgPosWidth),
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ),
        ( if !.TakeAddr = [CurArgNum | !:TakeAddr] then
            get_lcmc_null(CI, LCMCNull),
            (
                ArgPosWidth = apw_full(_, _),
                (
                    LCMCNull = do_not_opt_lcmc_null,
                    MaybeNull = no
                ;
                    LCMCNull = opt_lcmc_null,
                    MaybeNull = yes(const(llconst_int(0)))
                ),
                HeadCellArgs = [cell_arg_take_addr_one_word(RHSVar, MaybeNull)]
            ;
                ArgPosWidth = apw_double(_, _, _),
                (
                    LCMCNull = do_not_opt_lcmc_null,
                    MaybeNulls = no
                ;
                    LCMCNull = opt_lcmc_null,
                    Null = const(llconst_int(0)),
                    MaybeNulls = yes({Null, Null})
                ),
                HeadCellArgs =
                    [cell_arg_take_addr_two_words(RHSVar, MaybeNulls)]
            ),
            !:MayUseAtomic = may_not_use_atomic_alloc
        else
            generate_construct_arg_rval(RHSVar, ArgMode, RHSType, IsReal,
                RHSRval, !Code, CI, !CLD),
            get_module_info(CI, ModuleInfo),
            % XXX Should we update !MayUseAtomic for dummy types?
            update_type_may_use_atomic_alloc(ModuleInfo, RHSType,
                !MayUseAtomic),
            (
                ArgPosWidth = apw_full(_, _),
                (
                    IsReal = not_real_input_arg,
                    HeadCellArgs = [cell_arg_skip_one_word]
                ;
                    IsReal = real_input_arg,
                    HeadCellArgs = [cell_arg_full_word(RHSRval, complete)]
                )
            ;
                ArgPosWidth = apw_double(_, _, _),
                (
                    IsReal = not_real_input_arg,
                    HeadCellArgs = [cell_arg_skip_two_words]
                ;
                    IsReal = real_input_arg,
                    HeadCellArgs = [cell_arg_double_word(RHSRval)]
                )
            )
        ),
        LeftOverRHSVarsWidths = RHSVarsWidths,
        LeftOverArgModes = ArgModes,
        LeftOverArgNum = CurArgNum + 1
    ;
        ArgPosWidth = apw_partial_first(_, _, Shift, _, _, Fill),
        expect(not_taking_addr_of_cur_arg(!.TakeAddr, CurArgNum), $pred,
            "taking address of partial word"),
        generate_construct_arg_rval(RHSVar, ArgMode, RHSType, IsReal,
            RHSRval, !Code, CI, !CLD),
        get_module_info(CI, ModuleInfo),
        update_type_may_use_atomic_alloc(ModuleInfo, RHSType, !MayUseAtomic),
        (
            IsReal = not_real_input_arg,
            Completeness0 = incomplete,
            RevToOrRvals0 = []
        ;
            IsReal = real_input_arg,
            Completeness0 = complete,
            maybe_shift_and_accumulate_or_rval(RHSRval, Shift, Fill,
                [], RevToOrRvals0)
        ),
        NextArgNum = CurArgNum + 1,
        % Since we define a word to be the same size as a pointer,
        % a sub-word-sized argument cannot possibly hold a pointer.
        % this is why we don't need to update !MayUseAtomic here.
        generate_and_pack_one_cons_word(RHSVarsWidths, ArgModes,
            LeftOverRHSVarsWidths, LeftOverArgModes,
            NextArgNum, LeftOverArgNum, !TakeAddr,
            RevToOrRvals0, RevToOrRvals, Completeness0, Completeness,
            !Code, CI, !CLD),
        list.reverse(RevToOrRvals, ToOrRvals),
        PackedRval = bitwise_or_rvals(ToOrRvals),
        % ARG_PACK: Attach Completeness to the *vector* of cell args,
        % not to each *individual* cell arg.
        HeadCellArgs = [cell_arg_full_word(PackedRval, Completeness)]
    ;
        ArgPosWidth = apw_none_nowhere,
        expect(not_taking_addr_of_cur_arg(!.TakeAddr, CurArgNum), $pred,
            "taking address of dummy"),
        HeadCellArgs = [],
        LeftOverRHSVarsWidths = RHSVarsWidths,
        LeftOverArgModes = ArgModes,
        LeftOverArgNum = CurArgNum + 1
    ;
        ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _),
        unexpected($pred, "apw_partial_shifted")
    ;
        ArgPosWidth = apw_none_shifted(_, _),
        unexpected($pred, "apw_none_shifted")
    ),
    generate_and_pack_construct_args(LeftOverRHSVarsWidths, LeftOverArgModes,
        LeftOverArgNum, !.TakeAddr, TailCellArgs,
        !MayUseAtomic, !Code, CI, !CLD),
    CellArgs = HeadCellArgs ++ TailCellArgs.

:- pred generate_and_pack_one_cons_word(
    list(arg_and_width(prog_var))::in, list(unify_mode)::in,
    list(arg_and_width(prog_var))::out, list(unify_mode)::out,
    int::in, int::out, list(int)::in, list(int)::out,
    list(rval)::in, list(rval)::out, completeness::in, completeness::out,
    llds_code::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_and_pack_one_cons_word([], [], [], [], CurArgNum, LeftOverArgNum,
        !TakeAddr, !RevToOrRvals, !Completeness, !Code, _, !CLD) :-
    LeftOverArgNum = CurArgNum.
generate_and_pack_one_cons_word([], [_ | _], _, _, _, _,
        !TakeAddr, !RevToOrRvals, !Completeness, !Code, _, !CLD) :-
    unexpected($pred, "length misnatch").
generate_and_pack_one_cons_word([_ | _], [], _, _, _, _,
        !TakeAddr, !RevToOrRvals, !Completeness, !Code, _, !CLD) :-
    unexpected($pred, "length misnatch").
generate_and_pack_one_cons_word([RHSVarWidth | RHSVarsWidths],
        [ArgMode | ArgModes], LeftOverRHSVarsWidths, LeftOverArgModes,
        CurArgNum, LeftOverArgNum,
        !TakeAddr, !RevToOrRvals, !Completeness, !Code, CI, !CLD) :-
    RHSVarWidth = arg_and_width(RHSVar, ArgPosWidth),
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ),
        % This argument is not part of this word.
        LeftOverRHSVarsWidths = [RHSVarWidth | RHSVarsWidths],
        LeftOverArgModes = [ArgMode | ArgModes],
        LeftOverArgNum = CurArgNum
    ;
        ( ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, _)
        ),
        % This argument *is* part of this word.
        expect(not_taking_addr_of_cur_arg(!.TakeAddr, CurArgNum), $pred,
            "taking address of partial word"),
        generate_construct_arg_rval(RHSVar, ArgMode, _RHSType, IsReal, RHSRval,
            !Code, CI, !CLD),
        (
            ArgPosWidth = apw_partial_shifted(_, _, Shift, _, _, Fill),
            (
                IsReal = not_real_input_arg,
                !:Completeness = incomplete
            ;
                IsReal = real_input_arg,
                maybe_shift_and_accumulate_or_rval(RHSRval, Shift, Fill,
                    !RevToOrRvals)
            )
        ;
            ArgPosWidth = apw_none_shifted(_, _)
            % We change neither !Completeness nor !RevToOrRvals.
        ),
        NextArgNum = CurArgNum + 1,
        generate_and_pack_one_cons_word(RHSVarsWidths, ArgModes,
            LeftOverRHSVarsWidths, LeftOverArgModes,
            NextArgNum, LeftOverArgNum, !TakeAddr, !RevToOrRvals,
            !Completeness, !Code, CI, !CLD)
    ).

:- pred generate_and_pack_tagword(
    list(arg_and_width(prog_var))::in, list(unify_mode)::in,
    list(rval)::in, list(rval)::out, code_info::in) is det.

generate_and_pack_tagword([], [], !RevToOrRvals, _).
generate_and_pack_tagword([], [_ | _], !RevToOrRvals, _) :-
    unexpected($pred, "length misnatch").
generate_and_pack_tagword([_ | _], [], !RevToOrRvals, _) :-
    unexpected($pred, "length misnatch").
generate_and_pack_tagword([RHSVarWidth | RHSVarsWidths], [ArgMode | ArgModes],
        !RevToOrRvals, CI) :-
    RHSVarWidth = arg_and_width(RHSVar, ArgPosWidth),
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ),
        unexpected($pred, "ArgPosWidth is not a packed arg_pos_width")
    ;
        ArgPosWidth = apw_partial_shifted(_, _, Shift, _, _, Fill),
        is_arg_unify_real(CI, RHSVar, ArgMode, _RHSType, IsReal),
        (
            IsReal = not_real_input_arg,
            RHSRval = const(llconst_uint(0u))     % Dummy.
        ;
            IsReal = real_input_arg,
            RHSRval = var(RHSVar)
        ),
        ShiftedRHSRval = left_shift_rval(RHSRval, Shift, Fill),
        !:RevToOrRvals = [ShiftedRHSRval | !.RevToOrRvals]
    ;
        ArgPosWidth = apw_none_shifted(_, _)
    ),
    generate_and_pack_tagword(RHSVarsWidths, ArgModes, !RevToOrRvals, CI).

:- pred generate_construct_arg_rval(prog_var::in, unify_mode::in,
    mer_type::out, maybe_real_input_arg::out, rval::out,
    llds_code::in, llds_code::out,
    code_info::in, code_loc_dep::in, code_loc_dep::out) is det.

generate_construct_arg_rval(RHSVar, ArgMode, RHSType, IsReal, RHSRval,
        !Code, CI, !CLD) :-
    is_arg_unify_real(CI, RHSVar, ArgMode, RHSType, IsReal),
    (
        IsReal = not_real_input_arg,
        RHSRval = const(llconst_uint(0u))    % Dummy.
    ;
        IsReal = real_input_arg,
        produce_variable(RHSVar, RHSVarCode, RHSRval, !CLD),
        !:Code = !.Code ++ RHSVarCode
    ).

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

:- pred is_arg_unify_real(code_info::in, prog_var::in,
    unify_mode::in, mer_type::out, maybe_real_input_arg::out) is det.

is_arg_unify_real(CI, RHSVar, ArgMode, RHSType, IsReal) :-
    get_module_info(CI, ModuleInfo),
    ArgMode = unify_modes_li_lf_ri_rf(_, _, RHSInitInst, RHSFinalInst),
    get_vartypes(CI, VarTypes),
    lookup_var_type(VarTypes, RHSVar, RHSType),
    init_final_insts_to_top_functor_mode(ModuleInfo, RHSInitInst, RHSFinalInst,
        RHSType, RHSTopFunctorMode),
    (
        RHSTopFunctorMode = top_in,
        IsDummy = variable_is_of_dummy_type(CI, RHSVar),
        (
            IsDummy = is_dummy_type,
            IsReal = not_real_input_arg
        ;
            IsDummy = is_not_dummy_type,
            IsReal = real_input_arg
        )
    ;
        ( RHSTopFunctorMode = top_out
        ; RHSTopFunctorMode = top_unused
        ),
        IsReal = not_real_input_arg
    ).

:- pred construct_cell(prog_var::in, ptag::in, list(cell_arg)::in,
    how_to_construct::in, maybe(term_size_value)::in, prog_context::in,
    may_use_atomic_alloc::in, llds_code::out,
    code_info::in, code_info::out, code_loc_dep::in, code_loc_dep::out) is det.

construct_cell(LHSVar, Ptag, CellArgs, HowToConstruct, MaybeSize, Context,
        MayUseAtomic, Code, !CI, !CLD) :-
    LHSType = variable_type(!.CI, LHSVar),
    var_type_msg(LHSType, VarTypeMsg),
    % If we are doing accurate GC, then for types which hold RTTI that
    % will be traversed by the collector at GC-time, we need to allocate
    % an extra word at the start, to hold the forwarding pointer.
    % Normally we would just overwrite the first word of the object
    % in the "from" space, but this can't be done for objects which will be
    % referenced during the garbage collection process.
    get_gc_method(!.CI, GCMethod),
    ( if
        GCMethod = gc_accurate,
        is_introduced_type_info_type(LHSType)
    then
        ReserveWordAtStart = yes
    else
        ReserveWordAtStart = no
    ),
    Size = size_of_cell_args(CellArgs),
    maybe_add_alloc_site_info(Context, VarTypeMsg, Size, MaybeAllocId, !CI),
    assign_cell_to_var(LHSVar, ReserveWordAtStart, Ptag, CellArgs,
        HowToConstruct, MaybeSize, MaybeAllocId, MayUseAtomic, CellCode,
        !CI, !CLD),
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
        generate_field_take_address_assigns(FieldAddrs, LHSVar, Ptag,
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
    % XXX ARG_PACK We should be able to take the offsets for FieldAddrs
    % from the ArgPosWidth of the field whose address is being taken.
    % Counting offsets should *not* be necessary.
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

generate_direct_arg_construct(Var, ArgVar, Ptag, ArgMode, Type, Code,
        CI, !CLD) :-
    get_module_info(CI, ModuleInfo),
    compute_assign_direction(ModuleInfo, ArgMode, Type, Dir),
    (
        Dir = assign_right,
        unexpected($pred, "assign right in construction")
    ;
        Dir = assign_left,
        ( if Ptag = ptag(0u8) then
            assign_var_to_var(Var, ArgVar, !CLD),
            Code = empty
        else
            assign_expr_to_var(Var, mkword(Ptag, var(ArgVar)), Code, !CLD)
        )
    ;
        Dir = assign_unused,
        % Construct a tagged pointer to a pointer value
        % which is as yet unknown.
        % XXX This will have to change if we start to support aliasing.
        assign_const_to_var(Var, mkword_hole(Ptag), CI, !CLD),
        Code = empty
    ).

%---------------------------------------------------------------------------%

:- pred pack_how_to_construct(list(arg_and_width(prog_var))::in,
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

:- pred needs_update_args_to_words(list(arg_and_width(prog_var))::in,
    list(needs_update)::in, list(needs_update)::out) is det.

needs_update_args_to_words([], [], []).
needs_update_args_to_words([], [_ | _], _) :-
    unexpected($pred, "mismatched lists").
needs_update_args_to_words([_ | _], [], []) :-
    unexpected($pred, "mismatched lists").
needs_update_args_to_words([VarWidth | VarsWidths], [ArgNU | ArgNUs],
        WordNUs) :-
    VarWidth = arg_and_width(_Var, ArgPosWidth),
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ),
        needs_update_args_to_words(VarsWidths, ArgNUs, TailWordNUs),
        WordNUs = [ArgNU | TailWordNUs]
    ;
        ArgPosWidth = apw_partial_first(_, _, _, _, _, _),
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
    list(arg_and_width(prog_var))::in, list(needs_update)::in,
    needs_update::in, needs_update::out,
    list(arg_and_width(prog_var))::out, list(needs_update)::out) is det.

does_any_arg_in_word_need_update([], [], !NU, [], []).
does_any_arg_in_word_need_update([], [_ | _], !NU, _, _) :-
    unexpected($pred, "mismatched lists").
does_any_arg_in_word_need_update([_ | _], [], !NU, _, _) :-
    unexpected($pred, "mismatched lists").
does_any_arg_in_word_need_update([VarWidth | VarsWidths], [ArgNU | ArgNUs],
        !NU, LaterWordVarsWidths, LaterWordArgNUs) :-
    VarWidth = arg_and_width(_Var, ArgPosWidth),
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
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
    get_from_ground_term_construct_info(TermVar, Goal,
        TermVarIsNeeded, Conjuncts, GoalInfo),
    (
        TermVarIsNeeded = termvar_is_not_needed
        % There is nothing to do.
    ;
        TermVarIsNeeded = termvar_is_needed,
        get_module_info(!.CI, ModuleInfo),
        get_exprn_opts(!.CI, ExprnOpts),
        get_static_cell_info(!.CI, StaticCellInfo0),
        map.init(ActiveMap0),
        generate_ground_term_conjuncts(ModuleInfo, ExprnOpts,
            Conjuncts, StaticCellInfo0, StaticCellInfo,
            ActiveMap0, ActiveMap),
        map.to_assoc_list(ActiveMap, ActivePairs),
        ( if ActivePairs = [TermVar - GroundTerm] then
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            add_forward_live_vars(NonLocals, !CLD),
            set_static_cell_info(StaticCellInfo, !CI),
            GroundTerm = typed_rval(Rval, _),
            assign_const_to_var(TermVar, Rval, !.CI, !CLD)
        else
            unexpected($pred, "no active pairs")
        )
    ).

:- type active_ground_term_map == map(prog_var, typed_rval).

:- pred generate_ground_term_conjuncts(module_info::in, exprn_opts::in,
    list(hlds_goal)::in, static_cell_info::in, static_cell_info::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_conjuncts(_ModuleInfo, _ExprnOpts, [],
        !StaticCellInfo, !ActiveMap).
generate_ground_term_conjuncts(ModuleInfo, ExprnOpts, [Goal | Goals],
        !StaticCellInfo, !ActiveMap) :-
    generate_ground_term_conjunct(ModuleInfo, ExprnOpts, Goal,
        !StaticCellInfo, !ActiveMap),
    generate_ground_term_conjuncts(ModuleInfo, ExprnOpts, Goals,
        !StaticCellInfo, !ActiveMap).

:- pred generate_ground_term_conjunct(module_info::in, exprn_opts::in,
    hlds_goal::in, static_cell_info::in, static_cell_info::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_conjunct(ModuleInfo, ExprnOpts, Goal,
        !StaticCellInfo, !ActiveMap) :-
    get_from_ground_term_construct_conjunct_info(Goal, LHSVar, ConsId, RHSVars,
        _GoalInfo),
    % The code of this predicate is very similar to the code of
    % generate_const_struct_arg_tag. Any changes here may also
    % require similar changes there.
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    (
        (
            ConsTag = int_tag(IntTag),
            int_tag_to_const_and_int_type(IntTag, Const, IntType),
            UnboxedInt64s = get_unboxed_int64s(ExprnOpts),
            store_int_tag_statically(IntType, UnboxedInt64s,
                may_not_store_double_width_natively, Type)
        ;
            ConsTag = float_tag(Float),
            Const = llconst_float(Float),
            UnboxedFloats = get_unboxed_floats(ExprnOpts),
            store_float_tag_statically(UnboxedFloats,
                may_not_store_double_width_natively, Type)
        ;
            ConsTag = string_tag(String),
            Const = llconst_string(String),
            Type = lt_string
        ;
            ConsTag = foreign_tag(Lang, Val),
            expect(unify(Lang, lang_c), $pred,
                "foreign_tag for language other than C"),
            Const = llconst_foreign(Val, lt_int(int_type_int)),
            Type = lt_int(int_type_int)
        ;
            ConsTag = dummy_tag,
            Const = llconst_int(0),
            Type = lt_int(int_type_int)
        ;
            ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, _MustMask),
            LocalSectag = local_sectag(_, PrimSec, _),
            Const = llconst_uint(PrimSec),
            Type = lt_data_ptr
        ),
        expect(unify(RHSVars, []), $pred, "constant has args"),
        ActiveGroundTerm = typed_rval(const(Const), Type),
        map.det_insert(LHSVar, ActiveGroundTerm, !ActiveMap)
    ;
        ( ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "unexpected constant")
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        associate_cons_id_args_with_widths(ModuleInfo, ConsId,
            RHSVars, RHSVarsWidths),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8)
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag)
            ),
            generate_ground_term_args(RHSVarsWidths, PackedRHSTypedRvals,
                !ActiveMap),
            AllRHSTypedRvals = PackedRHSTypedRvals
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            RemoteSectag = remote_sectag(SectagUint, SectagSize),
            (
                SectagSize = rsectag_word,
                TagwordTypedRval = typed_rval(const(llconst_uint(SectagUint)),
                    lt_int(int_type_uint)),
                NonTagwordRHSVarsWidths = RHSVarsWidths
            ;
                SectagSize = rsectag_subword(_),
                % XXX ARG_PACK Factor out this code pattern.
                ( if SectagUint = 0u then
                    RevToOrRvals0 = []
                else
                    RevToOrRvals0 = [const(llconst_uint(SectagUint))]
                ),
                generate_ground_term_args_for_one_word(RHSVarsWidths,
                    NonTagwordRHSVarsWidths, RevToOrRvals0, RevToOrRvals,
                    !ActiveMap),
                list.reverse(RevToOrRvals, ToOrRvals),
                TagwordRval = bitwise_or_rvals(ToOrRvals),
                TagwordTypedRval = typed_rval(TagwordRval,
                    lt_int(int_type_uint))
            ),
            generate_ground_term_args(NonTagwordRHSVarsWidths,
                NonTagwordPackedRHSTypedRvals, !ActiveMap),
            AllRHSTypedRvals =
                [TagwordTypedRval | NonTagwordPackedRHSTypedRvals]
        ;
            RemoteArgsTagInfo = remote_args_ctor(_),
            % These are supported only on the MLDS backend.
            unexpected($pred, "remote_args_ctor")
        ),
        add_scalar_static_cell(AllRHSTypedRvals, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        LHSRval = mkword(Ptag, CellPtrConst),
        ActiveGroundTerm = typed_rval(LHSRval, lt_data_ptr),
        map.det_insert(LHSVar, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = local_args_tag(LocalArgsTagInfo),
        expect_not(unify(RHSVars, []), $pred, "local_args_tag has no args"),
        maybe_accumulate_local_sectag(LocalArgsTagInfo, RevToOrRvals0),
        associate_cons_id_args_with_widths(ModuleInfo, ConsId,
            RHSVars, RHSVarsWidths),
        generate_ground_term_args_for_one_word(RHSVarsWidths,
            LeftOverRHSVarsWidths, RevToOrRvals0, RevToOrRvals, !ActiveMap),
        expect(unify(LeftOverRHSVarsWidths, []), $pred, "left over args"),
        list.reverse(RevToOrRvals, ToOrRvals),
        PackedRval = bitwise_or_rvals(ToOrRvals),
        ActiveGroundTerm = typed_rval(PackedRval, lt_data_ptr),
        map.det_insert(LHSVar, ActiveGroundTerm, !ActiveMap)
    ;
        ConsTag = no_tag,
        get_notag_or_direct_arg_arg(RHSVars, RHSVar),
        map.det_remove(RHSVar, RvalType, !ActiveMap),
        map.det_insert(LHSVar, RvalType, !ActiveMap)
    ;
        ConsTag = direct_arg_tag(Ptag),
        get_notag_or_direct_arg_arg(RHSVars, RHSVar),
        map.det_remove(RHSVar, typed_rval(RHSRval, _RvalType), !ActiveMap),
        ( if Ptag = ptag(0u8) then
            LHSRval = RHSRval
        else
            LHSRval = mkword(Ptag, RHSRval)
        ),
        ActiveGroundTerm = typed_rval(LHSRval, lt_data_ptr),
        map.det_insert(LHSVar, ActiveGroundTerm, !ActiveMap)
    ;
        % Lambda expressions cannot occur in from_ground_term_construct scopes
        % during code generation, because if they do occur there originally,
        % semantic analysis will change the scope reason to something else.
        ConsTag = closure_tag(_, _, _),
        unexpected($pred, "unexpected closure")
    ).

:- pred generate_ground_term_args(list(arg_and_width(prog_var))::in,
    list(typed_rval)::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_args([], [], !ActiveMap).
generate_ground_term_args([ArgVarWidth | ArgVarsWidths],
        [TypedRval | TypedRvals], !ActiveMap) :-
    ArgVarWidth = arg_and_width(ArgVar, ArgPosWidth),
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
        ArgPosWidth = apw_partial_first(_, _, Shift, _, _, Fill),
        ArgTypedRval = typed_rval(ArgRval, _),
        maybe_shift_and_accumulate_or_rval(ArgRval, Shift, Fill,
            [], RevToOrRvals0),
        generate_ground_term_args_for_one_word(ArgVarsWidths,
            LeftOverArgVarsWidths, RevToOrRvals0, RevToOrRvals, !ActiveMap),
        list.reverse(RevToOrRvals, ToOrRvals),
        PackedRval = bitwise_or_rvals(ToOrRvals),
        TypedRval = typed_rval(PackedRval, lt_int(int_type_uint)),
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
    list(arg_and_width(prog_var))::in, list(arg_and_width(prog_var))::out,
    list(rval)::in, list(rval)::out,
    active_ground_term_map::in, active_ground_term_map::out) is det.

generate_ground_term_args_for_one_word([], [], !RevToOrRvals, !ActiveMap).
generate_ground_term_args_for_one_word([ArgVarWidth | ArgVarsWidths],
        LeftOverArgVarsWidths, !RevToOrRvals, !ActiveMap) :-
    ArgVarWidth = arg_and_width(ArgVar, ArgPosWidth),
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        % These are not part of the current word.
        LeftOverArgVarsWidths = [ArgVarWidth | ArgVarsWidths]
    ;
        (
            ArgPosWidth = apw_partial_shifted(_, _, Shift, _, _, Fill),
            map.det_remove(ArgVar, ArgTypedRval, !ActiveMap),
            ArgTypedRval = typed_rval(ArgRval, _ArgRvalType),
            maybe_shift_and_accumulate_or_rval(ArgRval, Shift, Fill,
                !RevToOrRvals)
        ;
            ArgPosWidth = apw_none_shifted(_, _),
            map.det_remove(ArgVar, _ArgTypedRval, !ActiveMap)
        ),
        generate_ground_term_args_for_one_word(ArgVarsWidths,
            LeftOverArgVarsWidths, !RevToOrRvals, !ActiveMap)
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
    associate_cons_id_args_with_widths(ModuleInfo, ConsId,
        ConstArgs, ConsArgsPosWidths),
    generate_const_struct_rval(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        !.ConstStructMap, ConsTag, ConsArgsPosWidths, Rval, !StaticCellInfo),
    map.det_insert(ConstNum, Rval, !ConstStructMap).

:- pred generate_const_struct_rval(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in, cons_tag::in,
    list(arg_and_width(const_struct_arg))::in, typed_rval::out,
    static_cell_info::in, static_cell_info::out) is det.

generate_const_struct_rval(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, ConsTag, ConstArgsPosWidths, TypedRval,
        !StaticCellInfo) :-
    (
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            (
                RemoteArgsTagInfo = remote_args_only_functor,
                Ptag = ptag(0u8)
            ;
                RemoteArgsTagInfo = remote_args_unshared(Ptag)
            ),
            generate_const_struct_args(ModuleInfo, UnboxedFloats,
                UnboxedInt64s, ConstStructMap, ConstArgsPosWidths,
                RHSTypedRvals)
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            RemoteSectag = remote_sectag(SectagUint, SectagSize),
            (
                SectagSize = rsectag_word,
                TypedTagwordRval = typed_rval(const(llconst_uint(SectagUint)),
                    lt_int(int_type_uint)),
                NonTagwordConstArgsPosWidths = ConstArgsPosWidths
            ;
                SectagSize = rsectag_subword(_),
                ( if SectagUint = 0u then
                    RevToOrRvals0 = []
                else
                    RevToOrRvals0 = [const(llconst_uint(SectagUint))]
                ),
                generate_const_struct_args_for_one_word(ModuleInfo,
                    UnboxedFloats, UnboxedInt64s, ConstStructMap,
                    ConstArgsPosWidths, NonTagwordConstArgsPosWidths,
                    RevToOrRvals0, RevToOrRvals),
                list.reverse(RevToOrRvals, ToOrRvals),
                TagwordRval = bitwise_or_rvals(ToOrRvals),
                TypedTagwordRval = typed_rval(TagwordRval,
                    lt_int(int_type_uint))
            ),
            generate_const_struct_args(ModuleInfo, UnboxedFloats,
                UnboxedInt64s, ConstStructMap, NonTagwordConstArgsPosWidths,
                NonTagwordTypedRvals),
            RHSTypedRvals = [TypedTagwordRval | NonTagwordTypedRvals]
        ;
            RemoteArgsTagInfo = remote_args_ctor(_),
            % These are supported only on the MLDS backend.
            unexpected($pred, "remote_args_ctor")
        ),
        add_scalar_static_cell(RHSTypedRvals, DataAddr, !StaticCellInfo),
        MaybeOffset = no,
        CellPtrConst = const(llconst_data_addr(DataAddr, MaybeOffset)),
        Rval = mkword(Ptag, CellPtrConst),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ConsTag = local_args_tag(LocalArgsTagInfo),
        expect_not(unify(ConstArgsPosWidths, []), $pred,
            "local_args_tag has no args"),
        maybe_accumulate_local_sectag(LocalArgsTagInfo, RevToOrRvals0),
        generate_const_struct_args_for_one_word(ModuleInfo,
            UnboxedFloats, UnboxedInt64s, ConstStructMap,
            ConstArgsPosWidths, LeftOverConstArgsPosWidths,
            RevToOrRvals0, RevToOrRvals),
        list.reverse(RevToOrRvals, ToOrRvals),
        Rval = bitwise_or_rvals(ToOrRvals),
        expect(unify(LeftOverConstArgsPosWidths, []), $pred, "left over args"),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ConsTag = no_tag,
        get_notag_or_direct_arg_arg(ConstArgsPosWidths, ConstArgPosWidth),
        generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgPosWidth, ArgTypedRval),
        TypedRval = ArgTypedRval
    ;
        ConsTag = direct_arg_tag(Ptag),
        get_notag_or_direct_arg_arg(ConstArgsPosWidths, ConstArgPosWidth),
        generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgPosWidth, ArgTypedRval),
        ArgTypedRval = typed_rval(ArgRval, _RvalType),
        ( if Ptag = ptag(0u8) then
            Rval = ArgRval
        else
            Rval = mkword(Ptag, ArgRval)
        ),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ( ConsTag = int_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = string_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = dummy_tag
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = closure_tag(_, _, _)
        ),
        unexpected($pred, "unexpected tag")
    ).

:- pred generate_const_struct_args(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in,
    list(arg_and_width(const_struct_arg))::in, list(typed_rval)::out) is det.

generate_const_struct_args(_, _, _, _, [], []) .
generate_const_struct_args(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, [ConstArgPosWidth | ConstArgsPosWidths], TypedRvals) :-
    ConstArgPosWidth = arg_and_width(_ConstArg, ArgPosWidth),
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
        ArgPosWidth = apw_partial_first(_, _, Shift, _, _, Fill),
        generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
            ConstStructMap, ConstArgPosWidth, FirstTypedRval),
        FirstTypedRval = typed_rval(FirstRval, _FirstRvalType),
        maybe_shift_and_accumulate_or_rval(FirstRval, Shift, Fill,
            [], RevToOrRvals0),
        generate_const_struct_args_for_one_word(ModuleInfo,
            UnboxedFloats, UnboxedInt64s, ConstStructMap,
            ConstArgsPosWidths, LeftOverConstArgsPosWidths,
            RevToOrRvals0, RevToOrRvals),
        list.reverse(RevToOrRvals, ToOrRvals),
        HeadRval = bitwise_or_rvals(ToOrRvals),
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
    list(arg_and_width(const_struct_arg))::in,
    list(arg_and_width(const_struct_arg))::out,
    list(rval)::in, list(rval)::out) is det.

generate_const_struct_args_for_one_word(_, _, _, _, [], [], !RevToOrRvals).
generate_const_struct_args_for_one_word(ModuleInfo,
        UnboxedFloats, UnboxedInt64s, ConstStructMap,
        [ConstArgPosWidth | ConstArgsPosWidths], LeftOverConstArgsPosWidths,
        !RevToOrRvals) :-
    ConstArgPosWidth = arg_and_width(_ConstArg, ArgPosWidth),
    (
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        % These are not part of the current word.
        LeftOverConstArgsPosWidths = [ConstArgPosWidth | ConstArgsPosWidths]
    ;
        (
            ArgPosWidth = apw_partial_shifted(_, _, Shift, _, _, Fill),
            generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
                ConstStructMap, ConstArgPosWidth, ArgTypedRval),
            ArgTypedRval = typed_rval(ArgRval, _ArgRvalType),
            maybe_shift_and_accumulate_or_rval(ArgRval, Shift, Fill,
                !RevToOrRvals)
        ;
            ArgPosWidth = apw_none_shifted(_, _)
        ),
        generate_const_struct_args_for_one_word(ModuleInfo,
            UnboxedFloats, UnboxedInt64s, ConstStructMap,
            ConstArgsPosWidths, LeftOverConstArgsPosWidths, !RevToOrRvals)
    ).

:- pred generate_const_struct_arg(module_info::in, have_unboxed_floats::in,
    have_unboxed_int64s::in, const_struct_map::in,
    arg_and_width(const_struct_arg)::in, typed_rval::out) is det.

generate_const_struct_arg(ModuleInfo, UnboxedFloats, UnboxedInt64s,
        ConstStructMap, arg_and_width(ConstArg, ArgPosWidth), TypedRval) :-
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
            ConsTag = int_tag(IntTag),
            int_tag_to_const_and_int_type(IntTag, Const, IntType),
            store_int_tag_statically(IntType, UnboxedInt64s,
                may_store_double_width_natively(ArgPosWidth), Type)
        ;
            ConsTag = float_tag(Float),
            Const = llconst_float(Float),
            store_float_tag_statically(UnboxedFloats,
                may_store_double_width_natively(ArgPosWidth), Type)
        ;
            ConsTag = string_tag(String),
            Const = llconst_string(String),
            Type = lt_string
        ;
            ConsTag = foreign_tag(Lang, Val),
            expect(unify(Lang, lang_c), $pred,
                "foreign_tag for language other than C"),
            Const = llconst_foreign(Val, lt_int(int_type_int)),
            Type = lt_int(int_type_int)
        ;
            ConsTag = dummy_tag,
            Const = llconst_int(0),
            Type = lt_int(int_type_int)
        ),
        TypedRval = typed_rval(const(Const), Type)
    ;
        ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, _MustMask),
        LocalSectag = local_sectag(_, PrimSec, _),
        Rval = const(llconst_uint(PrimSec)),
        TypedRval = typed_rval(Rval, lt_data_ptr)
    ;
        ConsTag = type_ctor_info_tag(ModuleName, TypeName, TypeArity),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName,
            uint16.det_from_int(TypeArity)),
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
        % XXX ARG_PACK Document why these should not occur here ...
        ( ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)

        % These should not occur here because the structures that these tags
        % point to are writeable.
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)

        % These tags have arguments, and thus should be handled in
        % generate_const_struct_rval.
        ; ConsTag = remote_args_tag(_)
        ; ConsTag = local_args_tag(_)
        ; ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ; ConsTag = closure_tag(_, _, _)
        ),
        unexpected($pred, "unexpected tag")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred maybe_accumulate_local_sectag(local_args_tag_info::in,
    list(rval)::out) is det.

maybe_accumulate_local_sectag(LocalArgsTagInfo, RevToOrRvals0) :-
    (
        LocalArgsTagInfo = local_args_only_functor,
        PrimSec = 0u
    ;
        LocalArgsTagInfo = local_args_not_only_functor(_Ptag, LocalSectag),
        LocalSectag = local_sectag(_, PrimSec, _)
    ),
    ( if PrimSec = 0u then
        RevToOrRvals0 = []
    else
        RevToOrRvals0 = [const(llconst_uint(PrimSec))]
    ).

:- pred maybe_shift_and_accumulate_or_rval(rval::in, arg_shift::in,
    fill_kind::in, list(rval)::in, list(rval)::out) is det.

maybe_shift_and_accumulate_or_rval(Rval, Shift, Fill, !RevToOrRvals) :-
    ( if
        Rval = const(Const),
        is_zero_const(Const) = is_zero_const
    then
        % We may get zeros from constant fields. Since OR with zero is a noop,
        % do not include them in the list of rvals to be OR-ed later.
        true
    else
        ShiftedUnsignedRval = left_shift_rval(Rval, Shift, Fill),
        !:RevToOrRvals = [ShiftedUnsignedRval | !.RevToOrRvals]
    ).

%---------------------------------------------------------------------------%

:- type may_store_double_width_natively
    --->    may_not_store_double_width_natively
    ;       may_store_double_width_natively(arg_pos_width).

:- pred store_int_tag_statically(int_type::in, have_unboxed_int64s::in,
    may_store_double_width_natively::in, llds_type::out) is det.

store_int_tag_statically(IntType, UnboxedInt64s, MayStoreDoubleWidthStatically,
        Type) :-
    (
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
    ;
        ( IntType = int_type_int64
        ; IntType = int_type_uint64
        ),
        (
            UnboxedInt64s = have_unboxed_int64s,
            Type = lt_int(IntType)
        ;
            UnboxedInt64s = do_not_have_unboxed_int64s,
            ( if
                MayStoreDoubleWidthStatically =
                    may_store_double_width_natively(ArgPosWidth),
                ArgPosWidth = apw_double(_, _, _)
            then
                Type = lt_int(IntType)
            else
                Type = lt_data_ptr
            )
        )
    ).

:- pred store_float_tag_statically(have_unboxed_floats::in,
    may_store_double_width_natively::in, llds_type::out) is det.

store_float_tag_statically(UnboxedFloats, MayStoreDoubleWidthStatically,
        Type) :-
    (
        UnboxedFloats = have_unboxed_floats,
        Type = lt_float
    ;
        UnboxedFloats = do_not_have_unboxed_floats,
        ( if
            MayStoreDoubleWidthStatically =
                may_store_double_width_natively(ArgPosWidth),
            ArgPosWidth = apw_double(_, _, _)
        then
            Type = lt_float
        else
            Type = lt_data_ptr
        )
    ).

:- pred not_taking_addr_of_cur_arg(list(int)::in, int::in) is semidet.

not_taking_addr_of_cur_arg(TakeAddr, CurArgNum) :-
    ( if TakeAddr = [CurArgNum | _TailTakeAddr] then
        fail
    else
        true
    ).

:- pred take_tagword_args_widths_modes(
    list(arg_and_width(ArgType))::in, list(unify_mode)::in,
    list(arg_and_width(ArgType))::out, list(unify_mode)::out,
    list(arg_and_width(ArgType))::out, list(unify_mode)::out,
    int::in, int::out) is det.

take_tagword_args_widths_modes([], [], [], [], [], [], !CurArgNum).
take_tagword_args_widths_modes([], [_ | _], _, _, _, _, !CurArgNum) :-
    unexpected($pred, "length mismatch").
take_tagword_args_widths_modes([_ | _], [], _, _, _, _, !CurArgNum) :-
    unexpected($pred, "length mismatch").
take_tagword_args_widths_modes(
        [ArgWidth | ArgsWidths], [ArgMode | ArgModes],
        TagwordArgsWidths, TagwordArgModes,
        NonTagwordArgsWidths, NonTagwordArgModes, !CurArgNum) :-
    ArgWidth = arg_and_width(_Arg, ArgPosWidth),
    (
        ( ArgPosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, _)
        ),
        !:CurArgNum = !.CurArgNum + 1,
        take_tagword_args_widths_modes(ArgsWidths, ArgModes,
            TailTagwordArgsWidths, TailTagwordArgModes,
            NonTagwordArgsWidths, NonTagwordArgModes, !CurArgNum),
        TagwordArgsWidths = [ArgWidth | TailTagwordArgsWidths],
        TagwordArgModes = [ArgMode | TailTagwordArgModes]
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        TagwordArgsWidths = [],
        TagwordArgModes = [],
        NonTagwordArgsWidths = [ArgWidth | ArgsWidths],
        NonTagwordArgModes = [ArgMode | ArgModes]
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.unify_gen_construct.
%---------------------------------------------------------------------------%
