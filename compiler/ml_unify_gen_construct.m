%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012, 2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module ml_backend.ml_unify_gen_construct.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % ml_generate_construction_unification generates code
    % for a construction unification.
    %
:- pred ml_generate_construction_unification(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, list(int)::in,
    how_to_construct::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % We use values of this type when constructing a compound term.
    % The first three arguments give an arguments value, its type,
    % and its position and width in the term being constructed.
    % The fourth argument is meaningful only when constructing a term
    % dynamically (i.e. not when constructing it statically); if set to `yes',
    % it contains the information needed to look up this value, together
    % with any others packed together with it in a single word, in the
    % packed_arg_map. As such, it should be set to `yes' only for arguments
    % whose arg_pos_width is apw_partial_first or apw_partial_shifted.
:- type mlds_rval_type_and_width
    --->    rval_type_and_width(mlds_rval, mlds_type, arg_pos_width,
                maybe(packed_arg_var)).

    % ml_gen_new_object(MaybeConsId, MaybeCtorName, Ptag, ExplicitSectag, Var,
    %   ExtraRvalsTypesWidths, ArgVars, ArgModes, TakeAddr, HowToConstruct,
    %   Context, Stmts, !Info):
    %
    % Generate a `new_object' statement, or a static constant, depending on the
    % value of the how_to_construct argument. The `ExtraRvalsTypesWidths'
    % argument specifies additional constants to insert at the start of the
    % argument list.
    %
    % Exported for use by ml_closure_gen.m.
    %
:- pred ml_gen_new_object(maybe(cons_id)::in, maybe(qual_ctor_id)::in,
    ptag::in, bool::in, prog_var::in, list(mlds_rval_type_and_width)::in,
    list(prog_var)::in, list(unify_mode)::in, list(int)::in,
    how_to_construct::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%

    % Generate MLDS code for a scope that constructs a ground term.
    %
:- pred ml_generate_ground_term(prog_var::in, hlds_goal::in,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%

    % Record the contents of the module's const_struct_db in !GlobalData.
    %
:- pred ml_gen_const_structs(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::out, ml_global_data::in, ml_global_data::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module backend_libs.type_class_info.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.const_struct.
:- import_module hlds.goal_form.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_closure_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_type_gen.
:- import_module ml_backend.ml_unify_gen_deconstruct.
:- import_module ml_backend.ml_unify_gen_util.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module uint.
:- import_module uint8.

%---------------------------------------------------------------------------%

ml_generate_construction_unification(LHSVar, ConsId, RHSVars, ArgModes,
        TakeAddr, HowToConstruct, Context, Defns, Stmts, !Info) :-
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
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
        ml_variable_type(!.Info, LHSVar, LHSType),
        ml_gen_var(!.Info, LHSVar, LHSLval),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        LHS_MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, LHSType),
        (
            ConsTag = int_tag(IntTag),
            ConstRval = ml_int_tag_to_rval_const(IntTag, LHSType,
                LHS_MLDS_Type)
        ;
            ConsTag = float_tag(Float),
            ConstRval = ml_const(mlconst_float(Float))
        ;
            ConsTag = string_tag(String),
            ConstRval = ml_const(mlconst_string(String))
        ;
            ConsTag = foreign_tag(ForeignLang, ForeignTag),
            ConstRval = ml_const(mlconst_foreign(ForeignLang, ForeignTag,
                LHS_MLDS_Type))
        ;
            ConsTag = dummy_tag,
            % The type information is needed by the Java backend.
            ConstRval = ml_int_tag_to_rval_const(int_tag_int(0), LHSType,
                LHS_MLDS_Type)
        ;
            ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, _),
            LocalSectag = local_sectag(_, PrimSec, _),
            ConstRval = ml_cast(LHS_MLDS_Type, ml_const(mlconst_uint(PrimSec)))
        ;
            ( ConsTag = type_info_const_tag(ConstNum)
            ; ConsTag = typeclass_info_const_tag(ConstNum)
            ; ConsTag = ground_term_const_tag(ConstNum, _)
            ),
            ml_gen_info_get_const_struct_map(!.Info, ConstStructMap),
            map.lookup(ConstStructMap, ConstNum, GroundTerm0),
            GroundTerm0 = ml_ground_term(ConstRval, _Type, _MLDS_Type)
        ;
            ConsTag = type_ctor_info_tag(ModuleName0, TypeName, TypeArity),
            ModuleName = fixup_builtin_module(ModuleName0),
            MLDS_Module = mercury_module_name_to_mlds(ModuleName),
            RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
            RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
            Const = mlconst_data_addr_rtti(MLDS_Module, RttiId),
            ConstRval = ml_cast(LHS_MLDS_Type, ml_const(Const))
        ;
            ConsTag = base_typeclass_info_tag(ModuleName, ClassId, Instance),
            MLDS_Module = mercury_module_name_to_mlds(ModuleName),
            TCName = generate_class_name(ClassId),
            RttiId = tc_rtti_id(TCName,
                type_class_base_typeclass_info(ModuleName, Instance)),
            Const = mlconst_data_addr_rtti(MLDS_Module, RttiId),
            ConstRval = ml_cast(LHS_MLDS_Type, ml_const(Const))
        ;
            ConsTag = deep_profiling_proc_layout_tag(_, _),
            unexpected($pred, "deep_profiling_proc_layout_tag NYI")
        ;
            ConsTag = tabling_info_tag(PredId, ProcId),
            ml_gen_pred_label(ModuleInfo, proc(PredId, ProcId),
                PredLabel, PredModule),
            ProcLabel = mlds_proc_label(PredLabel, ProcId),
            QualProcLabel = qual_proc_label(PredModule, ProcLabel),
            Const = mlconst_data_addr_tabling(QualProcLabel, tabling_info),
            ConstRval = ml_cast(LHS_MLDS_Type, ml_const(Const))
        ;
            ConsTag = table_io_entry_tag(_, _),
            unexpected($pred, "table_io_entry_tag NYI")
        ),
        GroundTerm = ml_ground_term(ConstRval, LHSType, LHS_MLDS_Type),
        ml_gen_info_set_const_var(LHSVar, GroundTerm, !Info),
        Stmt = ml_gen_assign(LHSLval, ConstRval, Context),
        Stmts = [Stmt],
        Defns = []
    ;
        % Ordinary compound terms.
        ( ConsTag = single_functor_tag
        ; ConsTag = unshared_tag(_)
        ; ConsTag = shared_remote_tag(_, _)
        ),
        ml_generate_dynamic_construct_compound(LHSVar, ConsId, ConsTag,
            RHSVars, ArgModes, TakeAddr, HowToConstruct, Context,
            Defns, Stmts, !Info)
    ;
        ConsTag = shared_local_tag_with_args(_Ptag, LocalSectag),
        expect(unify(TakeAddr, []), $pred,
            "taking address of non word-sized argument"),
        ml_generate_dynamic_construct_tagword_compound(ConsId, LocalSectag,
            LHSVar, RHSVars, ArgModes, HowToConstruct, Context, Stmts, !Info),
        Defns = []
    ;
        ( ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ),
        expect(unify(TakeAddr, []), $pred,
            "notag or direct_arg_tag: take_addr"),
        ml_genenate_dynamic_construct_notag_direct_arg(LHSVar, ConsTag, RHSVars,
            ArgModes, Context, Stmts, !Info),
        Defns = []
    ;
        ConsTag = closure_tag(PredId, ProcId, _EvalMethod),
        ml_construct_closure(PredId, ProcId, LHSVar, RHSVars, ArgModes,
            HowToConstruct, Context, Defns, Stmts, !Info)
    ).

%---------------------------------------------------------------------------%

:- pred ml_generate_dynamic_construct_compound(prog_var::in,
    cons_id::in, cons_tag::in(memory_cell_tag),
    list(prog_var)::in, list(unify_mode)::in, list(int)::in,
    how_to_construct::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_dynamic_construct_compound(LHSVar, ConsId, ConsTag, RHSVars,
        ArgModes, TakeAddr, HowToConstruct, Context, Defns, Stmts, !Info) :-
    % Figure out which class name to construct.
    ml_gen_info_get_target(!.Info, CompilationTarget),
    UsesBaseClass = ml_tag_uses_base_class(ConsTag),
    (
        UsesBaseClass = tag_uses_base_class,
        MaybeCtorName = no
    ;
        UsesBaseClass = tag_does_not_use_base_class,
        ml_cons_name(CompilationTarget, ConsId, CtorName),
        MaybeCtorName = yes(CtorName)
    ),

    % If there is a secondary tag, it goes in the first field.
    (
        ConsTag = shared_remote_tag(Ptag, RemoteSectag),
        RemoteSectag = remote_sectag(SectagUint, AddedBy),
        UsesConstructors = ml_target_uses_constructors(CompilationTarget),
        (
            UsesConstructors = no,
            % XXX ARG_PACK
            expect(unify(AddedBy, sectag_added_by_unify), $pred,
                "AddedBy != sectag_added_by_unify"),
            ExplicitSectag = yes,
            SectagRval0 = ml_const(mlconst_int(uint.cast_to_int(SectagUint))),
            SectagType0 = mlds_native_int_type,
            % With the low-level data representation, all fields -- even the
            % secondary tag -- are boxed, and so we need box it here.
            SectagRval = ml_box(SectagType0, SectagRval0),
            SectagType = mlds_generic_type,
            ExtraRvalsTypesWidths = [rval_type_and_width(SectagRval, SectagType,
                apw_full(arg_only_offset(0), cell_offset(0)), no)]
        ;
            UsesConstructors = yes,
            % Secondary tag is implicitly initialised by the constructor.
            % XXX ARG_PACK
            expect(unify(AddedBy, sectag_added_by_constructor), $pred,
                "AddedBy != sectag_added_by_constructor"),
            ExplicitSectag = no,
            ExtraRvalsTypesWidths = []
        )
    ;
        (
            ConsTag = single_functor_tag,
            Ptag = ptag(0u8)
        ;
            ConsTag = unshared_tag(Ptag)
        ),
        ExplicitSectag = no,
        ExtraRvalsTypesWidths = []
    ),
    ml_gen_new_object(yes(ConsId), MaybeCtorName, Ptag, ExplicitSectag,
        LHSVar, ExtraRvalsTypesWidths, RHSVars, ArgModes, TakeAddr,
        HowToConstruct, Context, Defns, Stmts, !Info).

:- pred ml_generate_dynamic_construct_tagword_compound(cons_id::in,
    local_sectag::in, prog_var::in, list(prog_var)::in, list(unify_mode)::in,
    how_to_construct::in, prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_generate_dynamic_construct_tagword_compound(ConsId, LocalSectag, Var,
        ArgVars, ArgModes, HowToConstruct, Context, Stmts, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_info_get_var_types(!.Info, VarTypes),
    lookup_var_type(VarTypes, Var, VarType),
    associate_cons_id_args_with_types_widths(ModuleInfo,
        lookup_var_type_func(VarTypes), may_not_have_extra_args,
        VarType, ConsId, ArgVars, ArgVarsTypesWidths),
    (
        list.reverse(RevOrRvals, OrRvals),
        LocalSectag = local_sectag(_Sectag, PrimSec, _SectagBits),
        TagwordRval = ml_bitwise_or_some_rvals(
            ml_const(mlconst_uint(PrimSec)), OrRvals),
        (
            HowToConstruct = construct_dynamically,
            ml_gen_tagword_dynamically(!.Info, ArgVarsTypesWidths, ArgModes,
                [], RevOrRvals)
        ;
            HowToConstruct = construct_statically,
            ml_gen_tagword_statically(!.Info, ArgVarsTypesWidths,
                [], RevOrRvals)
        ),
        ml_gen_var(!.Info, Var, VarLval),
        Stmt = ml_gen_assign(VarLval, TagwordRval, Context),
        Stmts = [Stmt],
        (
            HowToConstruct = construct_dynamically
        ;
            HowToConstruct = construct_statically,
            MLDS_Type = mlds_native_uint_type,
            GroundTerm = ml_ground_term(TagwordRval, VarType, MLDS_Type),
            ml_gen_info_set_const_var(Var, GroundTerm, !Info)
        )
    ;
        HowToConstruct = reuse_cell(_),
        unexpected($pred, "reuse_cell")
    ;
        HowToConstruct = construct_in_region(_RegVar),
        unexpected($pred, "construct_in_region")
    ).

%---------------------------------------------------------------------------%

ml_gen_new_object(MaybeConsId, MaybeCtorName, Ptag, ExplicitSectag, Var,
        ExtraRvalsTypesWidths, ArgVars, ArgModes, TakeAddr,
        HowToConstruct, Context, Defns, Stmts, !Info) :-
    (
        MaybeConsId = yes(ConsId),
        ConsIdOrClosure = ordinary_cons_id(ConsId)
    ;
        MaybeConsId = no,
        expect(unify(ExplicitSectag, no), $pred, "sectag on closure"),
        list.length(ExtraRvalsTypesWidths, NumExtras),
        ConsIdOrClosure = closure_object(NumExtras)
    ),
    (
        HowToConstruct = construct_dynamically,
        ml_gen_new_object_dynamically(ConsIdOrClosure, MaybeCtorName,
            Ptag, ExplicitSectag, Var, ExtraRvalsTypesWidths,
            ArgVars, ArgModes, TakeAddr, Context, Stmts, !Info),
        Defns = []
    ;
        HowToConstruct = construct_statically,
        expect(unify(TakeAddr, []), $pred,
            "cannot take address of static object's field"),
        ml_gen_new_object_statically(ConsIdOrClosure, MaybeCtorName, Ptag,
            Var, ExtraRvalsTypesWidths, ArgVars, Context, Stmts, !Info),
        Defns = []
    ;
        HowToConstruct = reuse_cell(CellToReuse),
        ml_gen_new_object_reuse_cell(ConsIdOrClosure, MaybeCtorName,
            Ptag, ExplicitSectag, Var, ExtraRvalsTypesWidths,
            ArgVars, ArgModes, TakeAddr, CellToReuse, Context,
            Defns, Stmts, !Info)
    ;
        HowToConstruct = construct_in_region(_RegVar),
        sorry($pred, "construct_in_region NYI")
    ).

:- pred ml_gen_new_object_dynamically(cons_id_or_closure::in,
    maybe(qual_ctor_id)::in, ptag::in, bool::in, prog_var::in,
    list(mlds_rval_type_and_width)::in,
    list(prog_var)::in, list(unify_mode)::in,
    list(int)::in, prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_new_object_dynamically(ConsIdOrClosure, MaybeCtorName, Ptag,
        ExplicitSectag, Var, ExtraRvalsTypesWidths, ArgVars, ArgModes,
        TakeAddr, Context, Stmts, !Info) :-
    % Find out the types of the constructor arguments and generate rvals
    % for them (boxing/unboxing if needed).
    ml_variable_type(!.Info, Var, VarType),
    associate_maybe_cons_id_args_with_types_widths(!.Info, VarType,
        ConsIdOrClosure, ArgVars, ArgVarsTypesWidths),
    ml_gen_info_get_use_atomic_cells(!.Info, UseAtomicCells),
    (
        UseAtomicCells = yes,
        MayUseAtomic0 = may_use_atomic_alloc
    ;
        UseAtomicCells = no,
        MayUseAtomic0 = may_not_use_atomic_alloc
    ),
    FirstArgNum = 1,
    % XXX ARG_PACK Merge ml_gen_dynamic_construct_args and
    % ml_expand_or_pack_into_words into one predicate.
    ml_gen_dynamic_construct_args(!.Info, ArgVarsTypesWidths, ArgModes,
        FirstArgNum, TakeAddr, TakeAddrInfos, ArgRvalsTypesWidths0,
        MayUseAtomic0, MayUseAtomic),

    % Replace double-word and packed arguments by uniform single word rvals.
    ml_expand_or_pack_into_words(!.Info, ArgRvalsTypesWidths0,
        PackedArgRvalsTypesWidths),

    % Add the extra rvals to the start.
    ArgRvalsTypesWidths = ExtraRvalsTypesWidths ++ PackedArgRvalsTypesWidths,

    % Compute the number of words to allocate.
    list.length(ArgRvalsTypesWidths, Size),
    SizeInWordsRval = ml_const(mlconst_int(Size)),

    % Generate an allocation site id.
    ml_gen_info_get_profile_memory(!.Info, ProfileMemory),
    (
        ProfileMemory = yes,
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        ml_gen_info_get_pred_proc_id(!.Info, PredProcId),
        ml_gen_proc_label(ModuleInfo, PredProcId, _Module, ProcLabel),
        ml_gen_info_get_global_data(!.Info, GlobalData0),
        (
            ConsIdOrClosure = ordinary_cons_id(ConsId),
            MaybeConsId = yes(ConsId)
        ;
            ConsIdOrClosure = closure_object(_),
            MaybeConsId = no
        ),
        ml_gen_alloc_site(mlds_function_name(ProcLabel), MaybeConsId, Size,
            Context, AllocId, GlobalData0, GlobalData),
        ml_gen_info_set_global_data(GlobalData, !Info),
        MaybeAllocId = yes(AllocId)
    ;
        ProfileMemory = no,
        MaybeAllocId = no
    ),

    % Generate a `new_object' statement to dynamically allocate the memory
    % for this term from the heap. The `new_object' statement will also
    % initialize the fields of this term with the specified arguments.
    ml_gen_var(!.Info, Var, VarLval),
    ConvFunc = (func(rval_type_and_width(Rv, T, _, _)) = ml_typed_rval(Rv, T)),
    ArgRvalsTypes = list.map(ConvFunc, ArgRvalsTypesWidths),
    ml_gen_type(!.Info, VarType, MLDS_VarType),
    MakeNewObject = new_object(VarLval, Ptag, ExplicitSectag, MLDS_VarType,
        yes(SizeInWordsRval), MaybeCtorName, ArgRvalsTypes,
        MayUseAtomic, MaybeAllocId),
    MakeNewObjStmt = ml_stmt_atomic(MakeNewObject, Context),

    MaybePtag = yes(Ptag),
    ml_gen_field_take_address_assigns(TakeAddrInfos, VarLval, MLDS_VarType,
        MaybePtag, Context, !.Info, TakeAddrStmts),
    Stmts = [MakeNewObjStmt | TakeAddrStmts].

:- pred ml_gen_new_object_statically(cons_id_or_closure::in,
    maybe(qual_ctor_id)::in, ptag::in, prog_var::in,
    list(mlds_rval_type_and_width)::in, list(prog_var)::in,
    prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_new_object_statically(ConsIdOrClosure, MaybeCtorName, Ptag,
        Var, ExtraRvalsTypesWidths, ArgVars, Context, Stmts, !Info) :-
    % Find out the types of the constructor arguments.
    ml_variable_type(!.Info, Var, VarType),
    associate_maybe_cons_id_args_with_types_widths(!.Info, VarType,
        ConsIdOrClosure, ArgVars, ArgVarsTypesWidths),

    some [!GlobalData] (
        % Generate rvals for the arguments.
        ml_gen_info_get_global_data(!.Info, !:GlobalData),

        % Box or unbox the arguments, if needed, and insert the extra rvals
        % at the start.
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        ml_gen_info_get_high_level_data(!.Info, HighLevelData),
        (
            HighLevelData = no,
            % Box *all* the arguments, including the extra rvals.
            ml_gen_box_extra_const_rval_list_lld(ModuleInfo, Context,
                ExtraRvalsTypesWidths, ExtraArgRvalsTypesWidths, !GlobalData),
            ml_gen_box_const_rval_list_lld(!.Info, Context,
                ArgVarsTypesWidths, ArgRvalsTypesWidths, !GlobalData)
        ;
            HighLevelData = yes,
            ml_gen_box_or_unbox_const_rval_list_hld(!.Info, Context,
                ArgVarsTypesWidths, ArgRvalsTypesWidths, !GlobalData),
            % For --high-level-data, the ExtraRvalsTypesWidths should
            % already have the right type, so we do not need to worry
            % about boxing or unboxing them.
            ExtraArgRvalsTypesWidths = ExtraRvalsTypesWidths
        ),

        % Generate a static constant for this term.
        (
            MaybeCtorName = yes(_),
            UsesBaseClass = tag_does_not_use_base_class
        ;
            MaybeCtorName = no,
            UsesBaseClass = tag_uses_base_class
        ),
        ExtraArgRvals = list.map((func(rval_type_and_width(Rv, _, _, _)) = Rv),
            ExtraArgRvalsTypesWidths),
        ml_gen_info_get_target(!.Info, Target),
        ml_gen_type(!.Info, VarType, MLDS_VarType),

        construct_static_ground_term(ModuleInfo, Target, HighLevelData,
            Context, VarType, MLDS_VarType, ConsIdOrClosure, UsesBaseClass,
            Ptag, ExtraArgRvals, ArgRvalsTypesWidths, GroundTerm, !GlobalData),

        ml_gen_info_set_global_data(!.GlobalData, !Info)
    ),

    ml_gen_info_set_const_var(Var, GroundTerm, !Info),

    ml_gen_var(!.Info, Var, VarLval),
    GroundTerm = ml_ground_term(Rval, _, _),
    AssignStmt = ml_gen_assign(VarLval, Rval, Context),
    Stmts = [AssignStmt].

:- pred ml_gen_new_object_reuse_cell(cons_id_or_closure::in,
    maybe(qual_ctor_id)::in, ptag::in, bool::in, prog_var::in,
    list(mlds_rval_type_and_width)::in, list(prog_var)::in,
    list(unify_mode)::in, list(int)::in, cell_to_reuse::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_new_object_reuse_cell(ConsIdOrClosure, MaybeCtorName,
        Ptag, ExplicitSectag, Var, ExtraRvalsTypesWidths, ArgVars, ArgModes,
        TakeAddr, CellToReuse, Context, Defns, Stmts, !Info) :-
    % NOTE: if it is ever used, NeedsUpdates needs to be modified to take into
    % account argument packing, as in unify_gen.m.
    CellToReuse = cell_to_reuse(ReuseVar, ReuseConsIds, _NeedsUpdates),
    (
        ConsIdOrClosure = ordinary_cons_id(ConsId)
    ;
        ConsIdOrClosure = closure_object(_),
        unexpected($pred, "attempt to reuse closure")
    ),
    list.map(
        ( pred(ReuseConsId::in, ReusePrimTag::out) is det :-
            ml_cons_id_to_tag(!.Info, ReuseConsId, ReuseConsTag),
            ml_tag_ptag_and_initial_offset(ReuseConsTag, ReusePrimTag,
                _ReuseOffSet)
        ), ReuseConsIds, ReusePrimaryTags0),
    list.remove_dups(ReusePrimaryTags0, ReusePrimaryTags),

    ml_variable_type(!.Info, Var, VarType),
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
    ml_tag_ptag_and_initial_offset(ConsTag, PrimaryTag, InitOffSet),
    ml_field_names_and_types(!.Info, VarType, ConsId, InitOffSet, ArgVars,
        ArgVarRepns),

    ml_gen_var(!.Info, ReuseVar, ReuseVarLval),

    list.filter(
        ( pred(ReuseTag::in) is semidet :-
            ReuseTag \= PrimaryTag
        ), ReusePrimaryTags, DifferentTags),
    (
        DifferentTags = [],
        ReuseVarRval = ml_lval(ReuseVarLval)
    ;
        DifferentTags = [ReusePrimaryTag],
        % The body operator is slightly more efficient than the strip_tag
        % operator, so we use it when the old tag is known.
        % XXX We should avoid stripping the old tags and putting on
        % the new one when they are known to be the same.
        ReusePrimaryTag = ptag(ReusePrimaryTagUint8),
        ReusePrimaryTagInt = uint8.cast_to_int(ReusePrimaryTagUint8),
        ReuseVarRval = ml_mkword(PrimaryTag,
            ml_binop(body,
                ml_lval(ReuseVarLval),
                ml_const(mlconst_int(ReusePrimaryTagInt))))
    ;
        DifferentTags = [_, _ | _],
        ReuseVarRval = ml_mkword(PrimaryTag,
            ml_unop(strip_tag, ml_lval(ReuseVarLval)))
    ),

    ml_gen_var(!.Info, Var, VarLval),
    CastReuseVarRval = ml_cast(MLDS_VarType, ReuseVarRval),
    HeapTestStmt = ml_stmt_atomic(assign_if_in_heap(VarLval, CastReuseVarRval),
        Context),

    % For each field in the construction unification we need to generate
    % an rval. ExtraRvalsTypesWidths need to be inserted at the start
    % of the object.
    ml_gen_type(!.Info, VarType, MLDS_VarType),
    MaybePtag = yes(Ptag),
    ml_gen_extra_arg_assigns(VarLval, MLDS_VarType, MaybePtag,
        0, ExtraRvalsTypesWidths, Context, ExtraRvalStmts, !Info),
    decide_field_gen(!.Info, VarLval, VarType, ConsId, ConsTag, FieldGen),
    % XXX We do more work than we need to here, as some of the cells
    % may already contain the correct values.
    FirstArgNum = 1,
    ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, ArgModes,
        FirstArgNum, Context, TakeAddr, TakeAddrInfos,
        FieldDefns, FieldStmts, !Info),
    ml_gen_field_take_address_assigns(TakeAddrInfos, VarLval, MLDS_VarType,
        MaybePtag, Context, !.Info, TakeAddrStmts),
    ThenStmts = ExtraRvalStmts ++ FieldStmts ++ TakeAddrStmts,
    ThenStmt = ml_stmt_block([], [], ThenStmts, Context),

    % If the reassignment isn't possible because the target is statically
    % allocated, then fall back to dynamic allocation.
    ml_gen_new_object(yes(ConsId), MaybeCtorName, Ptag, ExplicitSectag, Var,
        ExtraRvalsTypesWidths, ArgVars, ArgModes, TakeAddr,
        construct_dynamically, Context, DynamicDefns, DynamicStmts, !Info),
    Defns = FieldDefns ++ DynamicDefns,
    ElseStmt = ml_stmt_block([], [], DynamicStmts, Context),
    IfStmt = ml_stmt_if_then_else(ml_lval(VarLval), ThenStmt, yes(ElseStmt),
        Context),
    Stmts = [HeapTestStmt, IfStmt].

:- pred ml_gen_field_take_address_assigns(list(take_addr_info)::in,
    mlds_lval::in, mlds_type::in, maybe(ptag)::in, prog_context::in,
    ml_gen_info::in, list(mlds_stmt)::out) is det.

ml_gen_field_take_address_assigns([], _, _, _, _, _, []).
ml_gen_field_take_address_assigns([TakeAddrInfo | TakeAddrInfos],
        CellLval, CellType, MaybePtag, Context, Info, [Assign | Assigns]) :-
    TakeAddrInfo = take_addr_info(AddrVar, Offset, _ConsArgType, FieldType),
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    (
        HighLevelData = no,
        % XXX I am not sure that the types specified here are always the right
        % ones, particularly in cases where the field whose address we are
        % taking has a non-du type such as int or float. However, I can't think
        % of a test case in which a predicate fills in a field of such a type
        % after a *recursive* call, since recursive calls tend to generate
        % values of recursive (i.e. discriminated union) types. -zs
        Offset = cell_offset(OffsetInt),
        FieldId = ml_field_offset(ml_const(mlconst_int(OffsetInt))),
        SourceRval = ml_mem_addr(ml_field(MaybePtag,
            ml_lval(CellLval), CellType, FieldId, FieldType)),
        ml_gen_var(Info, AddrVar, AddrLval),
        ml_variable_type(Info, AddrVar, AddrVarType),
        ml_gen_info_get_module_info(Info, ModuleInfo),
        MLDS_AddrVarType = mercury_type_to_mlds_type(ModuleInfo, AddrVarType),
        CastSourceRval = ml_cast(MLDS_AddrVarType, SourceRval),
        Assign = ml_gen_assign(AddrLval, CastSourceRval, Context)
    ;
        HighLevelData = yes,
        % For high-level data lco.m uses a different transformation where we
        % simply pass the base address of the cell. The transformation does not
        % generate unifications.
        ml_gen_var(Info, AddrVar, AddrLval),
        Assign = ml_gen_assign(AddrLval, ml_lval(CellLval), Context)
    ),
    ml_gen_field_take_address_assigns(TakeAddrInfos, CellLval, CellType,
        MaybePtag, Context, Info, Assigns).

%---------------------------------------------------------------------------%

:- pred ml_gen_box_or_unbox_const_rval_list_hld(ml_gen_info::in,
    prog_context::in, list(arg_var_type_and_width)::in,
    list(mlds_rval_type_and_width)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_box_or_unbox_const_rval_list_hld(_, _, [], [], !GlobalData).
ml_gen_box_or_unbox_const_rval_list_hld(Info, Context,
        [ArgVarTypeWidth | ArgVarsTypesWidths],
        [FieldRvalTypeWidth | FieldRvalsTypesWidths], !GlobalData) :-
    ArgVarTypeWidth = arg_type_and_width(ArgVar, ConsArgType, ConsArgPosWidth),
    ml_variable_type(Info, ArgVar, ArgType),
    ml_gen_info_lookup_const_var_rval(Info, ArgVar, ArgRval),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    HighLevelData = yes,
    ConsWidth = arg_pos_width_to_width_only(ConsArgPosWidth),
    ml_type_as_field(ModuleInfo, HighLevelData, ConsArgType, ConsWidth,
        FieldType),
    ml_gen_box_or_unbox_const_rval_hld(ModuleInfo, Context,
        ArgType, FieldType, ArgRval, FieldRval, !GlobalData),
    FieldRvalTypeWidth =
        rval_type_and_width(FieldRval, mlds_generic_type, ConsArgPosWidth, no),
    ml_gen_box_or_unbox_const_rval_list_hld(Info, Context,
        ArgVarsTypesWidths, FieldRvalsTypesWidths, !GlobalData).

:- pred ml_gen_box_or_unbox_const_rval_hld(module_info::in, prog_context::in,
    mer_type::in, mer_type::in, mlds_rval::in, mlds_rval::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_box_or_unbox_const_rval_hld(ModuleInfo, Context, ArgType, FieldType,
        ArgRval, FieldRval, !GlobalData) :-
    (
        % Handle the case where the field type is a boxed type
        % -- in that case, we can just box the argument type.
        FieldType = type_variable(_, _),
        MLDS_ArgType = mercury_type_to_mlds_type(ModuleInfo, ArgType),
        ml_gen_box_const_rval(ModuleInfo, Context, MLDS_ArgType, aw_full_word,
            ArgRval, FieldRval, !GlobalData)
    ;
        ( FieldType = defined_type(_, _, _)
        ; FieldType = builtin_type(_)
        ; FieldType = tuple_type(_, _)
        ; FieldType = higher_order_type(_, _, _, _, _)
        ; FieldType = apply_n_type(_, _, _)
        ; FieldType = kinded_type(_, _)
        ),
        % Otherwise, fall back on ml_gen_box_or_unbox_rval in ml_call_gen.m.
        % XXX This might generate an rval which is not legal in a static
        % initializer!
        ml_gen_box_or_unbox_rval(ModuleInfo, ArgType, FieldType,
            bp_native_if_possible, ArgRval, FieldRval)
    ).

:- pred ml_gen_box_const_rval_list_lld(ml_gen_info::in, prog_context::in,
    list(arg_var_type_and_width)::in, list(mlds_rval_type_and_width)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_box_const_rval_list_lld(_, _, [], [], !GlobalData).
ml_gen_box_const_rval_list_lld(Info, Context,
        [ArgVarTypeWidth | ArgVarsTypesWidths],
        [BoxedRvalTypeWidth | BoxedRvalsTypesWidths], !GlobalData) :-
    ArgVarTypeWidth = arg_type_and_width(ArgVar, _ArgVarType, ArgPosWidth),
    ml_gen_info_lookup_const_var(Info, ArgVar, GroundTerm),
    GroundTerm = ml_ground_term(Rval, _MercuryType, MLDS_Type),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    Width = arg_pos_width_to_width_only(ArgPosWidth),
    ml_gen_box_const_rval(ModuleInfo, Context, MLDS_Type, Width, Rval,
        BoxedRval, !GlobalData),
    BoxedRvalTypeWidth =
        rval_type_and_width(BoxedRval, mlds_generic_type, ArgPosWidth, no),
    ml_gen_box_const_rval_list_lld(Info, Context, ArgVarsTypesWidths,
        BoxedRvalsTypesWidths, !GlobalData).

:- pred ml_gen_box_extra_const_rval_list_lld(module_info::in, prog_context::in,
    list(mlds_rval_type_and_width)::in, list(mlds_rval_type_and_width)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_box_extra_const_rval_list_lld(_, _, [], [], !GlobalData).
ml_gen_box_extra_const_rval_list_lld(ModuleInfo, Context,
        [RvalTypeWidth | RvalsTypesWidths],
        [BoxedRvalTypeWidth | BoxedRvalsTypesWidths], !GlobalData) :-
    RvalTypeWidth = rval_type_and_width(Rval, MLDS_Type, PosWidth,
        MaybePackedArgVar),
    Width = arg_pos_width_to_width_only(PosWidth),
    % Extras are always a single word.
    expect(unify(Width, aw_full_word), $pred, "Width != aw_full_word"),
    ml_gen_box_const_rval(ModuleInfo, Context, MLDS_Type, Width,
        Rval, BoxedRval, !GlobalData),
    BoxedRvalTypeWidth = rval_type_and_width(BoxedRval, MLDS_Type, PosWidth,
        MaybePackedArgVar),
    ml_gen_box_extra_const_rval_list_lld(ModuleInfo, Context,
        RvalsTypesWidths, BoxedRvalsTypesWidths, !GlobalData).

%---------------------------------------------------------------------------%

    % Create a list of rval_type_widths for the arguments of a
    % construction unification.
    %
    % For each argument which is input to the construction unification,
    % we produce the corresponding lval, boxed or unboxed if needed,
    % but if the argument is free, we produce a null value.
    %
:- pred ml_gen_dynamic_construct_args(ml_gen_info::in,
    list(arg_var_type_and_width)::in, list(unify_mode)::in,
    int::in, list(int)::in,
    list(take_addr_info)::out, list(mlds_rval_type_and_width)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

ml_gen_dynamic_construct_args(_, [], [], _, _, [], [], !MayUseAtomic).
ml_gen_dynamic_construct_args(_, [], [_ | _], _, _, _, _, !MayUseAtomic) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_construct_args(_, [_ | _], [], _, _, _, _, !MayUseAtomic) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_construct_args(Info, [ArgVarTypeWidth | ArgVarsTypesWidths],
        [ArgMode | ArgModes], CurArgNum, !.TakeAddr, TakeAddrInfos,
        RvalsMLDSTypesWidths, !MayUseAtomic) :-
    ArgVarTypeWidth = arg_type_and_width(ArgVar, ConsArgType, ArgPosWidth),
    ml_gen_var(Info, ArgVar, Lval),
    ml_variable_type(Info, ArgVar, ArgType),
    % It is important to use ArgType instead of ConsArgType here. ConsArgType
    % is the declared type of the argument of the cons_id, while ArgType is
    % the actual type of the variable being assigned to the given slot.
    % ConsArgType may be a type such as pred_id, which is a user-defined type
    % that may not appear in atomic cells, while ArgType may be a type such
    % as int, which may appear in atomic cells. This is because the actual type
    % may see behind abstraction barriers, and may thus see that e.g. pred_id
    % is actually the same as integer.
    ml_gen_info_get_module_info(Info, ModuleInfo),
    update_type_may_use_atomic_alloc(ModuleInfo, ArgType, !MayUseAtomic),

    % Figure out the type of the field.
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    ArgWidth = arg_pos_width_to_width_only(ArgPosWidth),
    ml_type_as_field(ModuleInfo, HighLevelData, ConsArgType, ArgWidth,
        BoxedArgType),
    MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, BoxedArgType),
    (
        ( ArgPosWidth = apw_full(_, CellOffset)
        ; ArgPosWidth = apw_double(_, CellOffset, _)
        ; ArgPosWidth = apw_partial_first(_, CellOffset, _, _, _, _)
        ; ArgPosWidth = apw_partial_shifted(_, CellOffset, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, CellOffset)
        )
    ;
        ArgPosWidth = apw_none_nowhere,
        % There *is* no offset at which the current argument is stored.
        CellOffset = cell_offset(-1)
    ),

    % Compute the value of the field.
    ( if !.TakeAddr = [CurArgNum | !:TakeAddr] then
        expect(unify(ArgWidth, aw_full_word), $pred,
            "taking address of non word-sized argument"),
        Rval = ml_const(mlconst_null(MLDS_Type)),
        RvalMLDSTypeWidth =
            rval_type_and_width(Rval, MLDS_Type, ArgPosWidth, no),
        % XXX ARG_PACK This call, the AllArgVarsTypesWidths and NumExtraArgs
        % arguments, and the ml_calc_field_offset function should not be needed
        % when we switch to taking offsets *only* from arg_pos_widths.
        OrigMLDS_Type = mercury_type_to_mlds_type(ModuleInfo, ConsArgType),
        TakeAddrInfo =
            take_addr_info(ArgVar, CellOffset, OrigMLDS_Type, MLDS_Type),
        ml_gen_dynamic_construct_args(Info, ArgVarsTypesWidths, ArgModes,
            CurArgNum + 1, !.TakeAddr, TakeAddrInfosTail,
            TailRvalsMLDSTypesWidths, !MayUseAtomic),
        TakeAddrInfos = [TakeAddrInfo | TakeAddrInfosTail],
        RvalsMLDSTypesWidths = [RvalMLDSTypeWidth | TailRvalsMLDSTypesWidths]
    else if ArgPosWidth = apw_none_nowhere then
        ml_gen_dynamic_construct_args(Info, ArgVarsTypesWidths, ArgModes,
            CurArgNum + 1, !.TakeAddr, TakeAddrInfos,
            RvalsMLDSTypesWidths, !MayUseAtomic)
    else
        ArgMode = unify_modes_lhs_rhs(_LHSInsts, RHSInsts),
        ( if
            from_to_insts_to_top_functor_mode(ModuleInfo, RHSInsts, ArgType,
                top_in),
            is_either_type_a_dummy(ModuleInfo, ArgType, ConsArgType) =
                neither_is_dummy_type
        then
            ml_gen_box_or_unbox_rval(ModuleInfo, ArgType, BoxedArgType,
                bp_native_if_possible, ml_lval(Lval), Rval)
        else
            Rval = ml_const(mlconst_null(MLDS_Type))
        ),
        (
            (
                ArgPosWidth = apw_partial_first(_, _, Shift, NumBits, _, Fill)
            ;
                ArgPosWidth =
                    apw_partial_shifted(_, _, Shift, NumBits, _, Fill)
            ),
            PackedArgVar = packed_arg_var(ArgVar, Shift, NumBits, Fill),
            MaybePackedArgVar = yes(PackedArgVar)
        ;
            ( ArgPosWidth = apw_full(_, _)
            ; ArgPosWidth = apw_double(_, _, _)
            ; ArgPosWidth = apw_none_shifted(_, _)
            ; ArgPosWidth = apw_none_nowhere
            ),
            MaybePackedArgVar = no
        ),
        RvalMLDSTypeWidth = rval_type_and_width(Rval, MLDS_Type, ArgPosWidth,
            MaybePackedArgVar),
        ml_gen_dynamic_construct_args(Info, ArgVarsTypesWidths, ArgModes,
            CurArgNum + 1, !.TakeAddr, TakeAddrInfos,
            TailRvalsMLDSTypesWidths, !MayUseAtomic),
        RvalsMLDSTypesWidths = [RvalMLDSTypeWidth | TailRvalsMLDSTypesWidths]
    ).

    % Generate assignment statements for each of ExtraRvals into the object at
    % VarLval, beginning at Offset.
    %
:- pred ml_gen_extra_arg_assigns(mlds_lval::in, mlds_type::in, maybe(ptag)::in,
    int::in, list(mlds_rval_type_and_width)::in, prog_context::in,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_extra_arg_assigns(_, _, _, _, [], _, [], !Info).
ml_gen_extra_arg_assigns(VarLval, MLDS_VarType, MaybePrimaryTag,
        CurOffset, [ExtraRvalTypeWidth | ExtraRvalsTypesWidths], Context,
        [Stmt | Stmts], !Info) :-
    ml_gen_info_get_high_level_data(!.Info, HighLevelData),
    expect(unify(HighLevelData, no), $pred, "high-level data"),

    FieldId = ml_field_offset(ml_const(mlconst_int(CurOffset))),
    ExtraRvalTypeWidth = rval_type_and_width(ExtraRval, ExtraType,
        ArgPosWidth, _MaybePackedArgVar),
    expect(is_apw_full(ArgPosWidth), $pred, "ArgPosWidth != apw_full(_)"),
    NextOffset = CurOffset + 1,
    FieldLval = ml_field(MaybePrimaryTag, ml_lval(VarLval), MLDS_VarType,
        FieldId, ExtraType),
    Stmt = ml_gen_assign(FieldLval, ExtraRval, Context),

    ml_gen_extra_arg_assigns(VarLval, MLDS_VarType, MaybePrimaryTag,
        NextOffset, ExtraRvalsTypesWidths, Context, Stmts, !Info).

:- pred ml_gen_tagword_dynamically(ml_gen_info::in,
    list(arg_var_type_and_width)::in, list(unify_mode)::in,
    list(mlds_rval)::in, list(mlds_rval)::out) is det.

ml_gen_tagword_dynamically(_, [], [], !RevOrRvals).
ml_gen_tagword_dynamically(_, [], [_ | _], !RevOrRvals) :-
    unexpected($pred, "length mismatch").
ml_gen_tagword_dynamically(_, [_ | _], [], !RevOrRvals) :-
    unexpected($pred, "length mismatch").
ml_gen_tagword_dynamically(Info, [RHSVarTypeWidth | RHSVarsTypesWidths],
        [ArgMode | ArgModes], !RevOrRvals) :-
    RHSVarTypeWidth = arg_type_and_width(RHSVar, ConsArgType, ArgPosWidth),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    ArgWidth = arg_pos_width_to_width_only(ArgPosWidth),
    ml_type_as_field(ModuleInfo, HighLevelData, ConsArgType, ArgWidth,
        BoxedRHSType),

    ml_variable_type(Info, RHSVar, RHSType),
    ArgMode = unify_modes_lhs_rhs(_LHSInsts, RHSInsts),
    ( if
        from_to_insts_to_top_functor_mode(ModuleInfo, RHSInsts, RHSType,
            top_in),
        is_either_type_a_dummy(ModuleInfo, RHSType, ConsArgType) =
            neither_is_dummy_type
    then
        ml_gen_var(Info, RHSVar, RHSLval),
        ml_gen_box_or_unbox_rval(ModuleInfo, RHSType, BoxedRHSType,
            bp_native_if_possible, ml_lval(RHSLval), RHSRval)
    else
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, BoxedRHSType),
        RHSRval = ml_const(mlconst_null(MLDS_Type))
    ),

    ml_maybe_shift_and_accumulate_packed_arg_rval(ArgPosWidth, RHSRval,
        !RevOrRvals),
    ml_gen_tagword_dynamically(Info, RHSVarsTypesWidths, ArgModes,
        !RevOrRvals).

:- pred ml_gen_tagword_statically(ml_gen_info::in,
    list(arg_var_type_and_width)::in,
    list(mlds_rval)::in, list(mlds_rval)::out) is det.

ml_gen_tagword_statically(_, [], !RevOrRvals).
ml_gen_tagword_statically(Info, [RHSVarTypeWidth | RHSVarsTypesWidths],
        !RevOrRvals) :-
    RHSVarTypeWidth = arg_type_and_width(RHSVar, _ConsArgType, ArgPosWidth),
    ml_gen_info_lookup_const_var(Info, RHSVar, GroundTerm),
    GroundTerm = ml_ground_term(RHSRval, _MercuryType, _MLDS_Type),
    ml_maybe_shift_and_accumulate_packed_arg_rval(ArgPosWidth, RHSRval,
        !RevOrRvals),
    ml_gen_tagword_statically(Info, RHSVarsTypesWidths, !RevOrRvals).

%---------------------------------------------------------------------------%

:- pred ml_genenate_dynamic_construct_notag_direct_arg(
    prog_var::in, cons_tag::in(no_or_direct_arg_tag), list(prog_var)::in,
    list(unify_mode)::in, prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_genenate_dynamic_construct_notag_direct_arg(LHSVar, ConsTag, RHSVars,
        ArgModes, Context, Stmts, !Info) :-
    get_notag_or_direct_arg_arg_mode(RHSVars, ArgModes, RHSVar, ArgMode),
    ml_variable_type(!.Info, LHSVar, LHSType),
    ml_gen_var(!.Info, LHSVar, LHSLval),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    LHS_MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, LHSType),
    ( if ml_gen_info_search_const_var(!.Info, RHSVar, RHSGroundTerm) then
        RHSGroundTerm = ml_ground_term(RHSRval0, _RHSType, RHS_MLDS_Type),
        ml_gen_info_get_global_data(!.Info, GlobalData0),
        ml_gen_box_const_rval(ModuleInfo, Context, RHS_MLDS_Type,
            aw_full_word, RHSRval0, RHSRval, GlobalData0, GlobalData),
        ml_gen_info_set_global_data(GlobalData, !Info),
        LHSRval = ml_cast_cons_tag(LHS_MLDS_Type, ConsTag, RHSRval),
        LHSGroundTerm = ml_ground_term(LHSRval, LHSType, LHS_MLDS_Type),
        ml_gen_info_set_const_var(LHSVar, LHSGroundTerm, !Info),
        Stmt = ml_gen_assign(LHSLval, RHSRval, Context),
        Stmts = [Stmt]
    else
        ml_gen_var(!.Info, RHSVar, RHSLval),
        ml_variable_type(!.Info, RHSVar, RHSType),
        (
            ConsTag = no_tag,
            RHSRval0 = ml_lval(RHSLval),
            ml_gen_box_or_unbox_rval(ModuleInfo, RHSType, LHSType,
                bp_native_if_possible, RHSRval0, RHSRval)
        ;
            ConsTag = direct_arg_tag(Ptag),
            % The reason this case needs a switch on Dir is the arm below
            % for assign_nondummy_unused. We don't need to put a ptag on
            % a dummy value if either we have a nondummy value (the then
            % part above), or if there is no ptag (the arm for no_tag above).
            ml_compute_assign_direction(ModuleInfo, ArgMode, RHSType, LHSType,
                Dir),
            (
                Dir = assign_nondummy_right,
                unexpected($pred, "left-to-right data flow in construction")
            ;
                Dir = assign_nondummy_left,
                RHSRval0 = ml_lval(RHSLval)
            ;
                Dir = assign_nondummy_unused,
                % Unused - unused: it is a partial assignment to the LHS
                % where the tag is known but the argument isn't.
                RHS_MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, RHSType),
                RHSRval0 = ml_const(mlconst_null(RHS_MLDS_Type))
            ;
                Dir = assign_dummy,
                unexpected($pred, "dummy unify")
            ),
            ml_gen_box_or_unbox_rval(ModuleInfo, RHSType, LHSType,
                bp_native_if_possible, RHSRval0, RHSRval1),
            RHSRval = ml_cast(LHS_MLDS_Type, ml_mkword(Ptag, RHSRval1))
        ),
        Stmt = ml_gen_assign(LHSLval, RHSRval, Context),
        Stmts = [Stmt]
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

ml_generate_ground_term(TermVar, Goal, Stmts, !Info) :-
    get_from_ground_term_construct_info(TermVar, Goal,
        TermVarIsNeeded, Conjuncts, GoalInfo),
    (
        TermVarIsNeeded = termvar_is_not_needed,
        % There is nothing to do.
        Stmts = []
    ;
        TermVarIsNeeded = termvar_is_needed,
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        ml_gen_info_get_target(!.Info, Target),
        ml_gen_info_get_high_level_data(!.Info, HighLevelData),
        ml_gen_info_get_var_types(!.Info, VarTypes),

        ml_gen_info_get_global_data(!.Info, GlobalData0),
        ml_generate_ground_term_conjuncts(ModuleInfo, Target,
            HighLevelData, VarTypes, Conjuncts,
            GlobalData0, GlobalData, map.init, GroundTermMap),
        ml_gen_info_set_global_data(GlobalData, !Info),

        map.lookup(GroundTermMap, TermVar, TermVarGroundTerm),
        ml_gen_info_set_const_var(TermVar, TermVarGroundTerm, !Info),

        ml_gen_var(!.Info, TermVar, TermVarLval),
        TermVarGroundTerm = ml_ground_term(TermVarRval, _, _),
        Context = goal_info_get_context(GoalInfo),
        Stmt = ml_gen_assign(TermVarLval, TermVarRval, Context),
        Stmts = [Stmt]
    ).

:- pred ml_generate_ground_term_conjuncts(module_info::in,
    mlds_target_lang::in, bool::in, vartypes::in, list(hlds_goal)::in,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

ml_generate_ground_term_conjuncts(_, _, _, _, [],
        !GlobalData, !GroundTermMap).
ml_generate_ground_term_conjuncts(ModuleInfo, Target, HighLevelData,
        VarTypes, [Goal | Goals], !GlobalData, !GroundTermMap) :-
    ml_generate_ground_term_conjunct(ModuleInfo, Target, HighLevelData,
        VarTypes, Goal, !GlobalData, !GroundTermMap),
    ml_generate_ground_term_conjuncts(ModuleInfo, Target, HighLevelData,
        VarTypes, Goals, !GlobalData, !GroundTermMap).

:- pred ml_generate_ground_term_conjunct(module_info::in,
    mlds_target_lang::in, bool::in, vartypes::in, hlds_goal::in,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

ml_generate_ground_term_conjunct(ModuleInfo, Target, HighLevelData, VarTypes,
        Goal, !GlobalData, !GroundTermMap) :-
    get_from_ground_term_construct_conjunct_info(Goal, LHSVar, ConsId, RHSVars,
        GoalInfo),
    lookup_var_type(VarTypes, LHSVar, LHSType),
    LHS_MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, LHSType),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    Context = goal_info_get_context(GoalInfo),
    (
        % Constants.
        (
            ConsTag = int_tag(IntTag),
            IntConst =
                int_tag_to_mlds_rval_const(LHSType, LHS_MLDS_Type, IntTag),
            ConstRval = ml_const(IntConst)
        ;
            ConsTag = float_tag(Float),
            ConstRval = ml_const(mlconst_float(Float))
        ;
            ConsTag = string_tag(String),
            ConstRval = ml_const(mlconst_string(String))
        ;
            ConsTag = foreign_tag(ForeignLang, ForeignTag),
            ConstRval = ml_const(mlconst_foreign(ForeignLang, ForeignTag,
                LHS_MLDS_Type))
        ;
            ConsTag = dummy_tag,
            % The type information is needed by the Java backend.
            IntTag = int_tag_int(0),
            IntConst =
                int_tag_to_mlds_rval_const(LHSType, LHS_MLDS_Type, IntTag),
            ConstRval = ml_const(IntConst)
        ;
            ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, _),
            LocalSectag = local_sectag(_, PrimSec, _),
            ConstRval = ml_cast(LHS_MLDS_Type, ml_const(mlconst_uint(PrimSec)))
        ),
        expect(unify(RHSVars, []), $pred, "constant has args"),
        ConstGroundTerm = ml_ground_term(ConstRval, LHSType, LHS_MLDS_Type),
        map.det_insert(LHSVar, ConstGroundTerm, !GroundTermMap)
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
        ConsTag = shared_local_tag_with_args(_Ptag, LocalSectag),
        LocalSectag = local_sectag(_Sectag, PrimSec, _SectagBits),
        associate_cons_id_args_with_types_widths(ModuleInfo,
            lookup_var_type_func(VarTypes), may_not_have_extra_args,
            LHSType, ConsId, RHSVars, RHSVarsTypesWidths),
        expect(unify(HighLevelData, no), $pred, "HighLevelData = yes"),
        list.foldl2(construct_ground_term_tagword_initializer_lld,
            RHSVarsTypesWidths, [], RevOrRvals, !GroundTermMap),
        list.reverse(RevOrRvals, OrRvals),
        TagwordRval = ml_bitwise_or_some_rvals(
            ml_const(mlconst_uint(PrimSec)), OrRvals),
        ConstGroundTerm = ml_ground_term(TagwordRval, LHSType, LHS_MLDS_Type),
        map.det_insert(LHSVar, ConstGroundTerm, !GroundTermMap)
    ;
        % Ordinary compound terms.
        ( ConsTag = single_functor_tag
        ; ConsTag = unshared_tag(_)
        ; ConsTag = shared_remote_tag(_, _)
        ),
        ml_generate_ground_term_memory_cell(ModuleInfo, Target,
            HighLevelData, VarTypes, LHSVar, LHSType, LHS_MLDS_Type,
            ConsId, ConsTag, RHSVars, Context,
            !GlobalData, !GroundTermMap)
    ;
        ( ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ),
        get_notag_or_direct_arg_arg(RHSVars, RHSVar),
        map.det_remove(RHSVar, RHSGroundTerm, !GroundTermMap),
        RHSGroundTerm = ml_ground_term(RHSRval, _RHSType, RHS_MLDS_Type),
        ml_gen_box_const_rval(ModuleInfo, Context, RHS_MLDS_Type,
            aw_full_word, RHSRval, BoxedRHSRval0, !GlobalData),
        BoxedRHSRval =
            ml_cast_cons_tag(LHS_MLDS_Type, ConsTag, BoxedRHSRval0),
        GroundTerm = ml_ground_term(BoxedRHSRval, LHSType, LHS_MLDS_Type),
        map.det_insert(LHSVar, GroundTerm, !GroundTermMap)
    ;
        % Lambda expressions cannot occur in from_ground_term_construct scopes
        % during code generation, because if they do occur there originally,
        % semantic analysis will change the scope reason to something else.
        ConsTag = closure_tag(_, _, _),
        unexpected($pred, "unexpected closure")
    ).

:- pred ml_generate_ground_term_memory_cell(module_info::in,
    mlds_target_lang::in, bool::in, vartypes::in,
    prog_var::in, mer_type::in, mlds_type::in,
    cons_id::in, cons_tag::in(memory_cell_tag), list(prog_var)::in,
    prog_context::in, ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

ml_generate_ground_term_memory_cell(ModuleInfo, Target, HighLevelData,
        VarTypes, LHSVar, LHSType, LHS_MLDS_Type, ConsId, ConsTag,
        RHSVars, Context, !GlobalData, !GroundTermMap) :-
    % This code (loosely) follows the code of ml_gen_compound.
    (
        ConsTag = single_functor_tag,
        Ptag = ptag(0u8),
        ExtraRHSRvals = []
    ;
        ConsTag = unshared_tag(Ptag),
        ExtraRHSRvals = []
    ;
        ConsTag = shared_remote_tag(Ptag, RemoteSectag),
        RemoteSectag = remote_sectag(SectagUint, AddedBy),
        UsesConstructors = ml_target_uses_constructors(Target),
        (
            UsesConstructors = no,
            % XXX ARG_PACK
            expect(unify(AddedBy, sectag_added_by_unify), $pred,
                "AddedBy != sectag_added_by_unify"),
            StagRval0 = ml_const(mlconst_int(uint.cast_to_int(SectagUint))),
            (
                HighLevelData = no,
                % XXX why is this cast here?
                StagRval = ml_box(mlds_native_char_type, StagRval0)
            ;
                HighLevelData = yes,
                StagRval = StagRval0
            ),
            ExtraRHSRvals = [StagRval]
        ;
            UsesConstructors = yes,
            % XXX ARG_PACK
            expect(unify(AddedBy, sectag_added_by_constructor), $pred,
                "AddedBy != sectag_added_by_constructor"),
            ExtraRHSRvals = []
        )
    ),

    % This code (loosely) follows the code of ml_gen_new_object.

    % If the scope contains existentially typed constructions,
    % then polymorphism should have changed its scope_reason away from
    % from_ground_term_construct. Therefore the static struct we are
    % constructing should not need any extra type_info or typeclass_info args.
    associate_cons_id_args_with_types_widths(ModuleInfo,
        lookup_var_type_func(VarTypes), may_not_have_extra_args,
        LHSType, ConsId, RHSVars, RHSVarsTypesWidths),
    (
        HighLevelData = yes,
        construct_ground_term_initializers_hld(ModuleInfo, Context,
            RHSVarsTypesWidths, RHSRvalsTypesWidths,
            !GlobalData, !GroundTermMap)
    ;
        HighLevelData = no,
        construct_ground_term_initializers_lld(ModuleInfo, Context,
            RHSVarsTypesWidths, RHSRvalsTypesWidths,
            !GlobalData, !GroundTermMap)
    ),
    % XXX ARG_PACK move to top, and inline in each branch.
    UsesBaseClass = ml_tag_uses_base_class(ConsTag),
    construct_static_ground_term(ModuleInfo, Target, HighLevelData,
        Context, LHSType, LHS_MLDS_Type, ordinary_cons_id(ConsId),
        UsesBaseClass, Ptag, ExtraRHSRvals, RHSRvalsTypesWidths, GroundTerm,
        !GlobalData),
    map.det_insert(LHSVar, GroundTerm, !GroundTermMap).

%---------------------------------------------------------------------------%

:- pred construct_ground_term_initializers_hld(module_info::in,
    prog_context::in, list(arg_var_type_and_width)::in,
    list(mlds_rval_type_and_width)::out,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

construct_ground_term_initializers_hld(_, _, [], [],
        !GlobalData, !GroundTermMap).
construct_ground_term_initializers_hld(ModuleInfo, Context,
        [RHSVarTypeWidth | RHSVarsTypesWidths],
        [RHSRvalTypeWidth | RHSRvalsTypesWidths],
        !GlobalData, !GroundTermMap) :-
    construct_ground_term_initializer_hld(ModuleInfo, Context,
        RHSVarTypeWidth, RHSRvalTypeWidth, !GlobalData, !GroundTermMap),
    construct_ground_term_initializers_hld(ModuleInfo, Context,
        RHSVarsTypesWidths, RHSRvalsTypesWidths, !GlobalData, !GroundTermMap).

:- pred construct_ground_term_initializer_hld(module_info::in,
    prog_context::in, arg_var_type_and_width::in,
    mlds_rval_type_and_width::out,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

construct_ground_term_initializer_hld(ModuleInfo, Context,
        RHSVarTypeWidth, RHSRvalTypeWidth, !GlobalData, !GroundTermMap) :-
    RHSVarTypeWidth = arg_type_and_width(RHSVar, ConsArgType, ArgPosWidth),
    map.det_remove(RHSVar, RHSGroundTerm, !GroundTermMap),
    RHSGroundTerm = ml_ground_term(RHSRval0, RHSType, _RHS_MLDS_Type),
    ArgWidth = arg_pos_width_to_width_only(ArgPosWidth),
    ml_type_as_field(ModuleInfo, yes, ConsArgType, ArgWidth, BoxedRHSType),
    ml_gen_box_or_unbox_const_rval_hld(ModuleInfo, Context,
        RHSType, BoxedRHSType, RHSRval0, RHSRval, !GlobalData),
    RHSRvalTypeWidth =
        rval_type_and_width(RHSRval, mlds_generic_type, ArgPosWidth, no).

:- pred construct_ground_term_initializers_lld(module_info::in,
    prog_context::in, list(arg_var_type_and_width)::in,
    list(mlds_rval_type_and_width)::out,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

construct_ground_term_initializers_lld(_, _, [], [],
        !GlobalData, !GroundTermMap).
construct_ground_term_initializers_lld(ModuleInfo, Context,
        [RHSVarTypeWidth | RHSVarsTypesWidths],
        [RHSRvalTypeWidth | RHSRvalsTypesWidths],
        !GlobalData, !GroundTermMap) :-
    construct_ground_term_initializer_lld(ModuleInfo, Context,
        RHSVarTypeWidth, RHSRvalTypeWidth, !GlobalData, !GroundTermMap),
    construct_ground_term_initializers_lld(ModuleInfo, Context,
        RHSVarsTypesWidths, RHSRvalsTypesWidths, !GlobalData, !GroundTermMap).

:- pred construct_ground_term_initializer_lld(module_info::in,
    prog_context::in, arg_var_type_and_width::in,
    mlds_rval_type_and_width::out, ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

construct_ground_term_initializer_lld(ModuleInfo, Context,
        RHSVarTypeWidth, RHSRvalTypeWidth, !GlobalData, !GroundTermMap) :-
    RHSVarTypeWidth = arg_type_and_width(RHSVar, _ConsArgType, ArgPosWidth),
    map.det_remove(RHSVar, RHSGroundTerm, !GroundTermMap),
    RHSGroundTerm = ml_ground_term(RHSRval0, _RHSType, RHS_MLDS_Type),
    ArgWidth = arg_pos_width_to_width_only(ArgPosWidth),
    ml_gen_box_const_rval(ModuleInfo, Context, RHS_MLDS_Type, ArgWidth,
        RHSRval0, RHSRval, !GlobalData),
    RHSRvalTypeWidth =
        rval_type_and_width(RHSRval, mlds_generic_type, ArgPosWidth, no).

:- pred construct_ground_term_tagword_initializer_lld(
    arg_var_type_and_width::in, list(mlds_rval)::in, list(mlds_rval)::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

construct_ground_term_tagword_initializer_lld(RHSVarTypeWidth,
        !RevOrRvals, !GroundTermMap) :-
    RHSVarTypeWidth = arg_type_and_width(RHSVar, _ConsArgType, ArgPosWidth),
    map.det_remove(RHSVar, RHSGroundTerm, !GroundTermMap),
    % Boxing cannot be applicable to subword rvals.
    RHSGroundTerm = ml_ground_term(RHSRval, _RHSType, _RHS_MLDS_Type),
    ml_maybe_shift_and_accumulate_packed_arg_rval(ArgPosWidth, RHSRval,
        !RevOrRvals).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

ml_gen_const_structs(ModuleInfo, Target, ConstStructMap, !GlobalData) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    Info = ml_const_struct_info(ModuleInfo, Target, HighLevelData),

    module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
    const_struct_db_get_structs(ConstStructDb, ConstStructs),
    list.foldl2(ml_gen_const_struct(Info), ConstStructs,
        map.init, ConstStructMap, !GlobalData).

:- type ml_const_struct_info
    --->    ml_const_struct_info(
                mcsi_module_info            :: module_info,
                mcsi_target                 :: mlds_target_lang,
                mcsi_high_level_data        :: bool
            ).

:- pred ml_gen_const_struct(ml_const_struct_info::in,
    pair(int, const_struct)::in,
    ml_const_struct_map::in, ml_const_struct_map::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_const_struct(Info, ConstNum - ConstStruct, !ConstStructMap,
        !GlobalData) :-
    ConstStruct = const_struct(ConsId, Args, Type, _Inst),
    ModuleInfo = Info ^ mcsi_module_info,
    MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
    (
        % These tags don't build heap cells.
        ( ConsTag = int_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = string_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = dummy_tag
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        % These tags should never occur in constant data in this position.
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        % These tags should never occur in constant data.
        ; ConsTag = tabling_info_tag(_, _)
        % These tags should never occur in MLDS grades.
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "unexpected tag")
    ;
        % Ordinary compound terms.
        % This code (loosely) follows the code of ml_gen_compound.
        (
            ConsTag = single_functor_tag,
            Ptag = ptag(0u8),
            ExtraRvals = []
        ;
            ConsTag = unshared_tag(Ptag),
            ExtraRvals = []
        ;
            ConsTag = shared_remote_tag(Ptag, RemoteSectag),
            RemoteSectag = remote_sectag(SectagUint, AddedBy),
            Target = Info ^ mcsi_target,
            UsesConstructors = ml_target_uses_constructors(Target),
            (
                UsesConstructors = no,
                % XXX ARG_PACK
                expect(unify(AddedBy, sectag_added_by_unify), $pred,
                    "AddedBy != sectag_added_by_unify"),
                StagRval0 =
                    ml_const(mlconst_int(uint.cast_to_int(SectagUint))),
                HighLevelData = Info ^ mcsi_high_level_data,
                (
                    HighLevelData = no,
                    % XXX why is this cast here?
                    StagRval = ml_box(mlds_native_char_type, StagRval0)
                ;
                    HighLevelData = yes,
                    StagRval = StagRval0
                ),
                ExtraRvals = [StagRval]
            ;
                UsesConstructors = yes,
                % XXX ARG_PACK
                expect(unify(AddedBy, sectag_added_by_constructor), $pred,
                    "AddedBy != sectag_added_by_constructor"),
                ExtraRvals = []
            )
        ),
        ml_gen_const_static_compound(Info, ConstNum, Type, MLDS_Type,
            ConsId, ConsTag, Ptag, ExtraRvals, Args,
            !ConstStructMap, !GlobalData)
    ;
        ConsTag = shared_local_tag_with_args(_Ptag, LocalSectag),
        LocalSectag = local_sectag(_Sectag, PrimSec, _SectagBits),
        ml_gen_const_static_args_widths(Info, Type, ConsId, Args,
            ArgsTypesWidths),
        HighLevelData = Info ^ mcsi_high_level_data,
        (
            HighLevelData = yes,
            unexpected($pred, "HighLevelData = yes")
        ;
            HighLevelData = no
        ),
        list.foldl(ml_gen_const_tagword_arg(Info), ArgsTypesWidths,
            [], RevOrRvals),
        list.reverse(RevOrRvals, OrRvals),
        TagwordRval = ml_bitwise_or_some_rvals(
            ml_const(mlconst_uint(PrimSec)), OrRvals),
        GroundTerm = ml_ground_term(TagwordRval, Type, MLDS_Type),
        map.det_insert(ConstNum, GroundTerm, !ConstStructMap)
    ;
        ( ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ),
        get_notag_or_direct_arg_arg(Args, Arg),
        ml_gen_const_struct_arg(Info, !.ConstStructMap, Arg,
            apw_full(arg_only_offset(0), cell_offset(0)),
            ArgRvalTypeWidth, !GlobalData),
        ArgRvalTypeWidth = rval_type_and_width(ArgRval, _RvalMLDSType,
            _Width, _MaybePackedArgVar),
        Rval = ml_cast_cons_tag(MLDS_Type, ConsTag, ArgRval),
        GroundTerm = ml_ground_term(Rval, Type, MLDS_Type),
        map.det_insert(ConstNum, GroundTerm, !ConstStructMap)
    ;
        ConsTag = closure_tag(_, _, _),
        % We do not put closures into static data.
        unexpected($pred, "unexpected closure")
    ).

:- pred ml_gen_const_static_compound(ml_const_struct_info::in,
    int::in, mer_type::in, mlds_type::in, cons_id::in, cons_tag::in,
    ptag::in, list(mlds_rval)::in, list(const_struct_arg)::in,
    ml_const_struct_map::in, ml_const_struct_map::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_const_static_compound(Info, ConstNum, VarType, MLDS_Type, ConsId,
        ConsTag, Ptag, ExtraRvals, Args, !ConstStructMap, !GlobalData) :-
    % This code (loosely) follows the code of
    % ml_generate_ground_term_compound.

    ml_gen_const_static_args_widths(Info, VarType, ConsId, Args,
        ArgsTypesWidths),
    ModuleInfo = Info ^ mcsi_module_info,
    Target = Info ^ mcsi_target,
    HighLevelData = Info ^ mcsi_high_level_data,
    ml_gen_const_struct_args(Info, !.ConstStructMap,
        ArgsTypesWidths, RvalsTypesWidths, !GlobalData),
    UsesBaseClass = ml_tag_uses_base_class(ConsTag),
    construct_static_ground_term(ModuleInfo, Target, HighLevelData,
        term.context_init, VarType, MLDS_Type, ordinary_cons_id(ConsId),
        UsesBaseClass, Ptag, ExtraRvals, RvalsTypesWidths, GroundTerm,
        !GlobalData),
    map.det_insert(ConstNum, GroundTerm, !ConstStructMap).

:- pred ml_gen_const_static_args_widths(ml_const_struct_info::in,
    mer_type::in, cons_id::in, list(const_struct_arg)::in,
    list(arg_const_type_and_width)::out) is det.

ml_gen_const_static_args_widths(Info, VarType, ConsId, Args,
        ArgsTypesWidths) :-
    % This code (loosely) follows the code of
    % ml_generate_ground_term_compound.

    HighLevelData = Info ^ mcsi_high_level_data,
    Target = Info ^ mcsi_target,
    ( if
        (
            HighLevelData = no
        ;
            HighLevelData = yes,
            Target = ml_target_java
        )
    then
        ModuleInfo = Info ^ mcsi_module_info,
        % It is ok to specify the wrong type for Args by passing AllTypesVoid,
        % because all uses of ArgsTypesWidths later on ignore the types
        % inside it.
        AllTypesVoid = (func(_Arg) = void_type),
        % XXX TYPE_REPN The may_not_have_extra_args preserves old behavior,
        % but may be a bug. It depends on whether we ever put into the
        % const struct db terms that need extra args.
        associate_cons_id_args_with_types_widths(ModuleInfo, AllTypesVoid,
            may_not_have_extra_args, VarType, ConsId, Args, ArgsTypesWidths)
    else
        unexpected($pred,
            "constant structures are not supported for this grade")
    ).

:- pred ml_gen_const_struct_args(ml_const_struct_info::in,
    ml_const_struct_map::in, list(arg_const_type_and_width)::in,
    list(mlds_rval_type_and_width)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_const_struct_args(_, _, [], [], !GlobalData).
ml_gen_const_struct_args(Info, ConstStructMap,
        [ArgTypeWidth | ArgsTypesWidths], [RvalTypeWidth | RvalsTypesWidths],
        !GlobalData) :-
    ArgTypeWidth = arg_type_and_width(ConstArg, _Type, PosWidth),
    ml_gen_const_struct_arg(Info, ConstStructMap,
        ConstArg, PosWidth, RvalTypeWidth, !GlobalData),
    ml_gen_const_struct_args(Info, ConstStructMap,
        ArgsTypesWidths, RvalsTypesWidths, !GlobalData).

:- pred ml_gen_const_struct_arg(ml_const_struct_info::in,
    ml_const_struct_map::in, const_struct_arg::in, arg_pos_width::in,
    mlds_rval_type_and_width::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_const_struct_arg(Info, ConstStructMap, ConstArg, PosWidth,
        RvalTypeWidth, !GlobalData) :-
    ModuleInfo = Info ^ mcsi_module_info,
    (
        ConstArg = csa_const_struct(StructNum),
        map.lookup(ConstStructMap, StructNum, GroundTerm),
        GroundTerm = ml_ground_term(Rval0, _MerType, MLDS_Type)
    ;
        ConstArg = csa_constant(ConsId, Type),
        ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
        ml_gen_const_struct_arg_tag(ConsTag, Type, MLDS_Type, Rval0)
    ),
    Width = arg_pos_width_to_width_only(PosWidth),
    ml_gen_box_const_rval(ModuleInfo, term.context_init, MLDS_Type,
        Width, Rval0, Rval, !GlobalData),
    RvalTypeWidth = rval_type_and_width(Rval, MLDS_Type, PosWidth, no).

:- pred ml_gen_const_tagword_arg(ml_const_struct_info::in,
    arg_const_type_and_width::in,
    list(mlds_rval)::in, list(mlds_rval)::out) is det.

ml_gen_const_tagword_arg(Info, RHSTypeWidth, !RevOrRvals) :-
    RHSTypeWidth = arg_type_and_width(ConstArg, _Type, ArgPosWidth),
    ModuleInfo = Info ^ mcsi_module_info,
    (
        ConstArg = csa_const_struct(_StructNum),
        unexpected($pred, "csa_const_struct in tagword")
    ;
        ConstArg = csa_constant(ConsId, Type),
        ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
        ml_gen_const_struct_arg_tag(ConsTag, Type, MLDS_Type, RHSRval)
    ),
    ml_maybe_shift_and_accumulate_packed_arg_rval(ArgPosWidth, RHSRval,
        !RevOrRvals).

:- pred ml_gen_const_struct_arg_tag(cons_tag::in, mer_type::in, mlds_type::in,
    mlds_rval::out) is det.

ml_gen_const_struct_arg_tag(ConsTag, Type, MLDS_Type, Rval) :-
    (
        ConsTag = int_tag(IntTag),
        RvalConst = int_tag_to_mlds_rval_const(Type, MLDS_Type, IntTag),
        Rval = ml_const(RvalConst)
    ;
        ConsTag = float_tag(Float),
        Rval = ml_const(mlconst_float(Float))
    ;
        ConsTag = string_tag(String),
        Rval = ml_const(mlconst_string(String))
    ;
        ConsTag = shared_local_tag_no_args(_Ptag, LocalSectag, _),
        LocalSectag = local_sectag(_, PrimSec, _),
        Rval = ml_cast(MLDS_Type, ml_const(mlconst_uint(PrimSec)))
    ;
        ConsTag = foreign_tag(ForeignLang, ForeignTag),
        Rval = ml_const(mlconst_foreign(ForeignLang, ForeignTag, MLDS_Type))
    ;
        ConsTag = dummy_tag,
        % The type information is needed by the Java backend.
        IntTag = int_tag_int(0),
        RvalConst = int_tag_to_mlds_rval_const(Type, MLDS_Type, IntTag),
        Rval = ml_const(RvalConst)
    ;
        ConsTag = type_ctor_info_tag(ModuleName0, TypeName, TypeArity),
        ModuleName = fixup_builtin_module(ModuleName0),
        MLDS_Module = mercury_module_name_to_mlds(ModuleName),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
        RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
        Const = mlconst_data_addr_rtti(MLDS_Module, RttiId),
        Rval = ml_cast(MLDS_Type, ml_const(Const))
    ;
        ConsTag = base_typeclass_info_tag(ModuleName, ClassId, Instance),
        MLDS_Module = mercury_module_name_to_mlds(ModuleName),
        TCName = generate_class_name(ClassId),
        RttiId = tc_rtti_id(TCName,
            type_class_base_typeclass_info(ModuleName, Instance)),
        Const = mlconst_data_addr_rtti(MLDS_Module, RttiId),
        Rval = ml_cast(MLDS_Type, ml_const(Const))
    ;
        % Instead of these tags in csa_constants, polymorphism.m builds
        % csa_const_structs.
        ( ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        % These tags build heap cells, not constants.
        ; ConsTag = single_functor_tag
        ; ConsTag = unshared_tag(_)
        ; ConsTag = shared_remote_tag(_, _)
        ; ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        % This tag *looks like* it builds heap cells (since it *does* build
        % non-constant terms), so it is handled with the other tags that
        % build structures.
        ; ConsTag = shared_local_tag_with_args(_, _)
        % These tag should never occur in constant data.
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = tabling_info_tag(_, _)
        % These tags should never occur in MLDS grades.
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "unexpected tag")
    ).

:- func int_tag_to_mlds_rval_const(mer_type, mlds_type, int_tag)
    = mlds_rval_const.

int_tag_to_mlds_rval_const(Type, MLDS_Type, IntTag) = Const :-
    (
        IntTag = int_tag_int(Int),
        ( if Type = int_type then
            Const = mlconst_int(Int)
        else if Type = char_type then
            Const = mlconst_char(Int)
        else
            Const = mlconst_enum(Int, MLDS_Type)
        )
    ;
        IntTag = int_tag_uint(UInt),
        Const = mlconst_uint(UInt)
    ;
        IntTag = int_tag_int8(Int8),
        Const = mlconst_int8(Int8)
    ;
        IntTag = int_tag_uint8(UInt8),
        Const = mlconst_uint8(UInt8)
    ;
        IntTag = int_tag_int16(Int16),
        Const = mlconst_int16(Int16)
    ;
        IntTag = int_tag_uint16(UInt16),
        Const = mlconst_uint16(UInt16)
    ;
        IntTag = int_tag_int32(Int32),
        Const = mlconst_int32(Int32)
    ;
        IntTag = int_tag_uint32(UInt32),
        Const = mlconst_uint32(UInt32)
    ;
        IntTag = int_tag_int64(Int64),
        Const = mlconst_int64(Int64)
    ;
        IntTag = int_tag_uint64(UInt64),
        Const = mlconst_uint64(UInt64)
    ).

%---------------------------------------------------------------------------%

    % Construct a static ground term with the specified contents,
    % and add it to !GlobalData.
    %
    % This predicate ignores the types inside ArgRvalsTypesWidths.
    %
    % XXX Give a more detailed description.
    %
:- pred construct_static_ground_term(module_info::in, mlds_target_lang::in,
    bool::in, prog_context::in, mer_type::in, mlds_type::in,
    cons_id_or_closure::in, tag_uses_base_class::in, ptag::in,
    list(mlds_rval)::in, list(mlds_rval_type_and_width)::in,
    ml_ground_term::out, ml_global_data::in, ml_global_data::out) is det.

construct_static_ground_term(ModuleInfo, Target, HighLevelData,
        Context, VarType, MLDS_Type, ConsIdOrClosure, UsesBaseClass, Ptag,
        ExtraRvals, RvalsTypesWidths, GroundTerm, !GlobalData) :-
    % Generate a local static constant for this term.
    ml_pack_ground_term_args_into_word_inits(RvalsTypesWidths, NonExtraInits),

    % By construction, boxing the ExtraRvals would be a no-op.
    %
    % XXX If the secondary tag is in a base class, then ideally its
    % initializer should be wrapped in `init_struct([init_obj(X)])'
    % rather than just `init_obj(X)' -- the fact that we don't leads to
    % some warnings from GNU C about missing braces in initializers.
    ExtraInits = list.map(func(Rv) = init_obj(Rv), ExtraRvals),
    AllInits = ExtraInits ++ NonExtraInits,
    ConstType = get_const_type_for_cons_id(Target, HighLevelData, MLDS_Type,
        UsesBaseClass, ConsIdOrClosure),
    ( if ConstType = mlds_array_type(_) then
        Initializer = init_array(AllInits)
    else
        Initializer = init_struct(ConstType, AllInits)
    ),
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    ml_gen_static_scalar_const_addr(MLDS_ModuleName, mgcv_const_var, ConstType,
        Initializer, Context, ConstDataAddrRval, !GlobalData),

    % Assign the (possibly tagged) address of the local static constant
    % to the variable.
    ( if Ptag = ptag(0u8) then
        TaggedRval = ConstDataAddrRval
    else
        TaggedRval = ml_mkword(Ptag, ConstDataAddrRval)
    ),
    Rval = ml_cast(MLDS_Type, TaggedRval),
    GroundTerm = ml_ground_term(Rval, VarType, MLDS_Type).

    % Return the MLDS type suitable for constructing a constant static
    % ground term with the specified cons_id.
    %
    % In all cases, mlds_array_type(mlds_generic_type) is provisional.
    % ml_gen_static_scalar_const* will replace it by a more specialized type,
    % mlds_mostly_generic_array_type(_), if required by the elements.
    %
:- func get_const_type_for_cons_id(mlds_target_lang, bool, mlds_type,
    tag_uses_base_class, cons_id_or_closure) = mlds_type.

get_const_type_for_cons_id(Target, HighLevelData, MLDS_Type, UsesBaseClass,
        ConsIdOrClosure) = ConstType :-
    (
        HighLevelData = no,
        ConstType = mlds_array_type(mlds_generic_type)
    ;
        HighLevelData = yes,
        ( if
            % Check for type_infos and typeclass_infos, since these
            % need to be handled specially; their Mercury type definitions
            % are lies on C backends.
            MLDS_Type = mercury_type(_, _, TypeCtorCategory),
            TypeCtorCategory = ctor_cat_system(_),
            Target = ml_target_c
        then
            ConstType = mlds_array_type(mlds_generic_type)
        else if
            % Check if we are constructing a value for a discriminated union
            % where the specified cons_id which is represented as a derived
            % class that is derived from the base class for this discriminated
            % union type.
            UsesBaseClass = tag_does_not_use_base_class,
            ConsIdOrClosure = ordinary_cons_id(ConsId),
            ConsId = cons(CtorSymName, CtorArity, _TypeCtor),
            (
                MLDS_Type =
                    mlds_class_type(mlds_class_id(QualTypeName, TypeArity, _))
            ;
                MLDS_Type = mercury_type(MercuryType, _, ctor_cat_user(_)),
                type_to_ctor(MercuryType, TypeCtor),
                ml_gen_type_name(TypeCtor, QualTypeName, TypeArity)
            )
        then
            % If so, append the name of the derived class to the name of the
            % base class for this type (since the derived class will also be
            % nested inside the base class).
            QualTypeName = qual_class_name(_, _, UnqualTypeName),
            CtorName = ml_gen_du_ctor_name_unqual_type(Target, UnqualTypeName,
                TypeArity, CtorSymName, CtorArity),
            QualTypeName = qual_class_name(MLDS_Module, _QualKind, TypeName),
            ClassQualifier = mlds_append_class_qualifier_module_qual(
                MLDS_Module, TypeName, TypeArity),
            QualClassName =
                qual_class_name(ClassQualifier, type_qual, CtorName),
            ClassId = mlds_class_id(QualClassName, CtorArity, mlds_class),
            ConstType = mlds_class_type(ClassId)
        else if
            % Convert mercury_types for user-defined types to the corresponding
            % `mlds_class_type'. This is needed because these types get
            % mapped to `mlds_ptr_type(mlds_class_type(...))', but when
            % declaring static constants we want just the class type,
            % not the pointer type.
            MLDS_Type = mercury_type(MercuryType, _, ctor_cat_user(_)),
            type_to_ctor(MercuryType, TypeCtor)
        then
            ml_gen_type_name(TypeCtor, ClassName, ClassArity),
            ClassId = mlds_class_id(ClassName, ClassArity, mlds_class),
            ConstType = mlds_class_type(ClassId)
        else if
            % For tuples, a similar issue arises; we want tuple constants
            % to have array type, not the pointer type MR_Tuple.
            MLDS_Type = mercury_type(_, _, ctor_cat_tuple)
        then
            ConstType = mlds_array_type(mlds_generic_type)
        else if
            % Likewise for closures, we need to use an array type rather than
            % the pointer type MR_ClosurePtr. Note that we use a low-level
            % data representation for closures, even when --high-level-data
            % is enabled.
            MLDS_Type = mercury_type(_, _, ctor_cat_higher_order)
        then
            ConstType = mlds_array_type(mlds_generic_type)
        else
            ConstType = MLDS_Type
        )
    ).

%---------------------------------------------------------------------------%
%
% Packing arguments into words.
%

:- pred ml_pack_ground_term_args_into_word_inits(
    list(mlds_rval_type_and_width)::in, list(mlds_initializer)::out) is det.

ml_pack_ground_term_args_into_word_inits([], []).
ml_pack_ground_term_args_into_word_inits([RvalTypeWidth | RvalsTypesWidths],
        Inits) :-
    RvalTypeWidth = rval_type_and_width(Rval, _Type, PosWidth,
        _MaybePackedArgVar),
    (
        ( PosWidth = apw_full(_, _)
        ; PosWidth = apw_double(_, _, _)
        ),
        % XXX We treat double word fields the same as single word fields,
        % and generate a single initializer for them. This works on x86s
        % because of two things:
        %
        % - a field in a cell can use two words *only* if its type is
        %   statically known (float, or later int64/uint64), and thus
        %   *every* constant structure for this cons_id will have
        %   the exact same Mercury and MLDS type for this field; and
        % - on x86s, double word primitive types can be stored at any
        %   address that is *word* aligned; it does not need to be
        %   *double word* aligned.
        %
        % On 32 bit platforms other than x86s, we cannot access double word
        % floats (and later int64s and uint64s) in heap terms directly
        % due to the possibility of misalignment. Instead, we access them
        % via macros such as MR_float_from_dword, which explicitly construct
        % the double-word primitive value from its two halves. We ensure that
        % the target language (C) compiler does not put padding before
        % the double word field by having mlds_to_c.m replace MR_Float
        % with MR_Float_Aligned in the structure type's definition.
        %
        % Storing such double word fields in two halves (as well as
        % accessing them as two halves) would be conceptually cleaner.
        % It would also avoid an inconsistency: the code for filling in
        % the contents of *heap* cells *does* fill in the two halves
        % separately. The problem is that for constant structures,
        % we would have to do this at compile time, and it not clear
        % (at least to me -zs) how we could ensure that a cross-compiler
        % would do this the same way, in terms of endian-ness, as the
        % target machine.
        HeadInit = init_obj(Rval),
        ml_pack_ground_term_args_into_word_inits(RvalsTypesWidths, TailInits),
        Inits = [HeadInit | TailInits]
    ;
        PosWidth = apw_partial_first(_, _, Shift, _, _, Fill),
        ml_maybe_shift_and_accumulate_or_rval(Rval, Shift, Fill,
            [], RevOrRvals0),
        ml_pack_into_one_word(RvalsTypesWidths, LeftOverRvalsTypesWidths,
            RevOrRvals0, OrAllRval, [], _, no, _),
        HeadInit = init_obj(OrAllRval),
        ml_pack_ground_term_args_into_word_inits(LeftOverRvalsTypesWidths,
            TailInits),
        Inits = [HeadInit | TailInits]
    ;
        ( PosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; PosWidth = apw_none_shifted(_, _)
        ),
        % There should be an apw_partial_first argument first.
        unexpected($pred, "apw_partial_shifted or apw_none_shifted")
    ;
        PosWidth = apw_none_nowhere,
        % Ignore RvalTypeWidth; don't convert it into an initializer.
        ml_pack_ground_term_args_into_word_inits(RvalsTypesWidths, Inits)
    ).

:- pred ml_expand_or_pack_into_words(ml_gen_info::in,
    list(mlds_rval_type_and_width)::in,
    list(mlds_rval_type_and_width)::out) is det.

ml_expand_or_pack_into_words(_, [], []).
ml_expand_or_pack_into_words(Info, [RvalTypeWidth | RvalsTypesWidths],
        PackedRvalsTypesWidths) :-
    RvalTypeWidth = rval_type_and_width(Rval, Type, PosWidth,
        MaybePackedArgVar),
    (
        PosWidth = apw_full(_, _),
        ml_expand_or_pack_into_words(Info, RvalsTypesWidths,
            TailPackedRvalsTypesWidths),
        PackedRvalsTypesWidths = [RvalTypeWidth | TailPackedRvalsTypesWidths]
    ;
        PosWidth = apw_double(arg_only_offset(AOOffset),
            cell_offset(CellOffset), DoubleWordKind),
        ml_expand_or_pack_into_words(Info, RvalsTypesWidths,
            TailPackedRvalsTypesWidths),
        ( if Rval = ml_const(mlconst_null(_)) then
            SubstType = mlds_generic_type,
            RvalA = ml_const(mlconst_null(SubstType)),
            RvalB = ml_const(mlconst_null(SubstType))
        else
            SubstType = mlds_native_int_type,
            (
                DoubleWordKind = dw_float,
                RvalA = ml_unop(dword_float_get_word0, Rval),
                RvalB = ml_unop(dword_float_get_word1, Rval)
            ;
                DoubleWordKind = dw_int64,
                RvalA = ml_unop(dword_int64_get_word0, Rval),
                RvalB = ml_unop(dword_int64_get_word1, Rval)
            ;
                DoubleWordKind = dw_uint64,
                RvalA = ml_unop(dword_uint64_get_word0, Rval),
                RvalB = ml_unop(dword_uint64_get_word1, Rval)
            )
        ),
        AOOffsetA = arg_only_offset(AOOffset),
        AOOffsetB = arg_only_offset(AOOffset + 1),
        CellOffsetA = cell_offset(CellOffset),
        CellOffsetB = cell_offset(CellOffset + 1),
        RvalTypeWidthA = rval_type_and_width(RvalA, SubstType,
            apw_full(AOOffsetA, CellOffsetA), no),
        RvalTypeWidthB = rval_type_and_width(RvalB, SubstType,
            apw_full(AOOffsetB, CellOffsetB), no),
        PackedRvalsTypesWidths =
            [RvalTypeWidthA, RvalTypeWidthB | TailPackedRvalsTypesWidths]
    ;
        PosWidth = apw_partial_first(AOOffset, CellOffset, Shift, _, _, Fill),
        ml_maybe_shift_and_accumulate_or_rval(Rval, Shift, Fill,
            [], RevOrRvals0),
        (
            MaybePackedArgVar = no,
            RevPackedArgVars0 = [],
            AllPartialsHavePackedArgVars0 = no
        ;
            MaybePackedArgVar = yes(PackedArgVar),
            RevPackedArgVars0 = [PackedArgVar],
            AllPartialsHavePackedArgVars0 = yes
        ),
        ml_pack_into_one_word(RvalsTypesWidths, LeftOverRvalsTypesWidths,
            RevOrRvals0, OrAllRval,
            RevPackedArgVars0, RevPackedArgVars,
            AllPartialsHavePackedArgVars0, AllPartialsHavePackedArgVars),
        (
            AllPartialsHavePackedArgVars = no,
            WordRval = OrAllRval
        ;
            AllPartialsHavePackedArgVars = yes,
            list.reverse(RevPackedArgVars, PackedArgVars),
            ml_gen_info_get_packed_args_map(Info, PackedArgsMap),
            ( if map.search(PackedArgsMap, PackedArgVars, OldWordRval) then
                WordRval = ml_cast(mlds_generic_type, OldWordRval)
            else
                WordRval = OrAllRval
            )
        ),
        % XXX TYPE_REPN Using the type of the first rval for the type
        % of the whole word preserves old behavior, but seems strange.
        PackedRvalTypeWidth = rval_type_and_width(WordRval, Type,
            apw_full(AOOffset, CellOffset), no),
        ml_expand_or_pack_into_words(Info, LeftOverRvalsTypesWidths,
            TailPackedRvalsTypesWidths),
        PackedRvalsTypesWidths =
            [PackedRvalTypeWidth | TailPackedRvalsTypesWidths]
    ;
        ( PosWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; PosWidth = apw_none_shifted(_, _)
        ),
        % There should be an apw_partial_first argument first.
        unexpected($pred, "apw_partial_shifted or apw_none_shifted")
    ;
        PosWidth = apw_none_nowhere,
        ml_expand_or_pack_into_words(Info, RvalsTypesWidths,
            PackedRvalsTypesWidths)
    ).

:- pred ml_pack_into_one_word(
    list(mlds_rval_type_and_width)::in, list(mlds_rval_type_and_width)::out,
    list(mlds_rval)::in, mlds_rval::out,
    list(packed_arg_var)::in, list(packed_arg_var)::out,
    bool::in, bool::out) is det.

ml_pack_into_one_word(RvalsTypesWidths, LeftOverRvalsTypesWidths,
        RevOrRvals0, BoxedOrAllRval,
        !RevPackedArgVars, !AllPartialsHavePackedArgVars) :-
    ml_pack_into_one_word_loop(RvalsTypesWidths, LeftOverRvalsTypesWidths,
        RevOrRvals0, RevOrRvals,
        !RevPackedArgVars, !AllPartialsHavePackedArgVars),
    list.reverse(RevOrRvals, OrRvals),
    (
        OrRvals = [],
        BoxedOrAllRval = ml_const(mlconst_int(0))
    ;
        OrRvals = [HeadOrRval | TailOrRvals],
        OrAllRval = ml_bitwise_or_some_rvals(HeadOrRval, TailOrRvals),
        BoxedOrAllRval = ml_cast(mlds_generic_type, OrAllRval)
    ).

:- pred ml_pack_into_one_word_loop(
    list(mlds_rval_type_and_width)::in, list(mlds_rval_type_and_width)::out,
    list(mlds_rval)::in, list(mlds_rval)::out,
    list(packed_arg_var)::in, list(packed_arg_var)::out,
    bool::in, bool::out) is det.

ml_pack_into_one_word_loop([], [], !RevOrRvals,
        !RevPackedArgVars, !AllPartialsHavePackedArgVars).
ml_pack_into_one_word_loop([RvalTypeWidth | RvalsTypesWidths],
        LeftOverRvalsTypesWidths, !RevOrRvals,
        !RevPackedArgVars, !AllPartialsHavePackedArgVars) :-
    RvalTypeWidth = rval_type_and_width(Rval, _Type, PosWidth,
        MaybePackedArgVar),
    (
        ( PosWidth = apw_full(_, _)
        ; PosWidth = apw_double(_, _, _)
        ; PosWidth = apw_partial_first(_, _, _, _, _, _)
        ; PosWidth = apw_none_nowhere
        ),
        LeftOverRvalsTypesWidths = [RvalTypeWidth | RvalsTypesWidths]
    ;
        (
            PosWidth = apw_partial_shifted(_, _, Shift, _, _, Fill),
            ml_maybe_shift_and_accumulate_or_rval(Rval, Shift, Fill,
                !RevOrRvals),
            (
                MaybePackedArgVar = no,
                !:AllPartialsHavePackedArgVars = no
            ;
                MaybePackedArgVar = yes(PackedArgVar),
                !:RevPackedArgVars = [PackedArgVar | !.RevPackedArgVars]
            )
        ;
            PosWidth = apw_none_shifted(_, _)
        ),
        ml_pack_into_one_word_loop(RvalsTypesWidths, LeftOverRvalsTypesWidths,
            !RevOrRvals, !RevPackedArgVars, !AllPartialsHavePackedArgVars)
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates.
%

:- pred ml_maybe_shift_and_accumulate_packed_arg_rval(arg_pos_width::in,
    mlds_rval::in, list(mlds_rval)::in, list(mlds_rval)::out) is det.

ml_maybe_shift_and_accumulate_packed_arg_rval(ArgPosWidth, RHSRval,
        !RevOrRvals) :-
    (
        ArgPosWidth = apw_partial_shifted(_, _, Shift, _, _, Fill),
        ml_maybe_shift_and_accumulate_or_rval(RHSRval, Shift, Fill,
            !RevOrRvals)
    ;
        ArgPosWidth = apw_none_shifted(_, _)
    ;
        ( ArgPosWidth = apw_full(_, _)
        ; ArgPosWidth = apw_double(_, _, _)
        ; ArgPosWidth = apw_partial_first(_, _, _, _, _, _)
        ; ArgPosWidth = apw_none_nowhere
        ),
        unexpected($pred, "not apw_partial_shifted or apw_none_shifted")
    ).

:- pred ml_maybe_shift_and_accumulate_or_rval(mlds_rval::in, arg_shift::in,
    fill_kind::in, list(mlds_rval)::in, list(mlds_rval)::out) is det.

ml_maybe_shift_and_accumulate_or_rval(Rval, Shift, Fill, !RevOrRvals) :-
    ( if
        Rval = ml_const(RvalConst),
        ( RvalConst = mlconst_null(_)
        ; ml_is_zero_const(RvalConst) = ml_is_zero_const
        )
    then
        % We may get nulls from unfilled fields, and zeros from constant
        % fields. Since OR with zero is a noop, do not include them
        % in the list of rvals to be OR-ed later.
        true
    else
        ShiftedRval = ml_left_shift_rval(Rval, Shift, Fill),
        !:RevOrRvals = [ShiftedRval | !.RevOrRvals]
    ).

:- pred ml_gen_info_lookup_const_var_rval(ml_gen_info::in, prog_var::in,
    mlds_rval::out) is det.

ml_gen_info_lookup_const_var_rval(Info, Var, Rval) :-
    ml_gen_info_lookup_const_var(Info, Var, GroundTerm),
    GroundTerm = ml_ground_term(Rval, _, _).

:- pred ml_cons_name(mlds_target_lang::in, cons_id::in, qual_ctor_id::out)
    is det.

ml_cons_name(CompilationTarget, HLDS_ConsId, QualifiedConsId) :-
    ( if
        HLDS_ConsId = cons(ConsSymName, ConsArity, TypeCtor),
        ConsSymName = qualified(SymModuleName, _)
    then
        ConsName = ml_gen_du_ctor_name(CompilationTarget, TypeCtor,
            ConsSymName, ConsArity),
        ConsId = ctor_id(ConsName, ConsArity),
        ModuleName = mercury_module_name_to_mlds(SymModuleName)
    else
        ConsName = cons_id_and_arity_to_string(HLDS_ConsId),
        ConsId = ctor_id(ConsName, 0),
        ModuleName = mercury_module_name_to_mlds(unqualified(""))
    ),
    QualifiedConsId = qual_ctor_id(ModuleName, module_qual, ConsId).

:- pred is_apw_full(arg_pos_width::in) is semidet.

is_apw_full(apw_full(_, _)).

:- inst no_or_direct_arg_tag for cons_tag/0
    --->    no_tag
    ;       direct_arg_tag(ground).

:- func ml_cast_cons_tag(mlds_type::in, cons_tag::in(no_or_direct_arg_tag),
    mlds_rval::in) = (mlds_rval::out) is det.

ml_cast_cons_tag(Type, ConsTag, Rval) = CastRval :-
    (
        ConsTag = no_tag,
        TagRval = Rval
    ;
        ConsTag = direct_arg_tag(Ptag),
        TagRval = ml_mkword(Ptag, Rval)
    ),
    CastRval = ml_cast(Type, TagRval).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_unify_gen_construct.
%---------------------------------------------------------------------------%
