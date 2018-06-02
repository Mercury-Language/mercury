%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012, 2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_unify_gen.m
% Main author: fjh
%
% This module generates MLDS code generation for unifications.
%
% Code for deconstruction unifications
%
%   det (cannot_fail) deconstruction:
%       <succeeded = (X => f(A1, A2, ...))>
%   ===>
%       A1 = arg(X, f, 1);                  % extract arguments
%       A2 = arg(X, f, 2);
%       ...
%
%   semidet (can_fail) deconstruction:
%       <X => f(A1, A2, ...)>
%   ===>
%       <succeeded = (X => f(_, _, _, _))>  % tag test
%       if (succeeded) {
%           A1 = arg(X, f, 1);              % extract arguments
%           A2 = arg(X, f, 2);
%           ...
%       }
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_unify_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.code_model.
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

    % Generate MLDS code for a unification.
    %
:- pred ml_gen_unification(unification::in, code_model::in, prog_context::in,
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

    % ml_gen_new_object(MaybeConsId, MaybeCtorName, Ptag, ExplicitSecTag, Var,
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

    % ml_gen_known_tag_test(Var, TaggedConsId, Expr, !Info):
    %
    % Generate code to perform a tag test.
    %
    % The test checks whether Var has the functor specified by TaggedConsId.
    % The generated code will not contain Defns or Stmts; it will be
    % only an Expr, which will be a boolean rval. Expr will evaluate to true
    % iff the Var has the functor specified by ConsId.
    %
    % (The "known" part of the name refers to the fact that the tag of
    % the cons_id is already known.)
    %
    % Exported for use ml_switch_gen.m.
    %
:- pred ml_gen_known_tag_test(prog_var::in, tagged_cons_id::in, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % ml_gen_secondary_tag_rval(Info, VarType, VarRval, Ptag, StagRval):
    %
    % Return the rval for the secondary tag field of VarRval, assuming that
    % VarRval has the specified VarType and PrimaryTag.
    %
    % Exported for use ml_tag_switch.m.
    %
:- pred ml_gen_secondary_tag_rval(ml_gen_info::in, mer_type::in, mlds_rval::in,
    ptag::in, mlds_rval::out) is det.

    % Generate MLDS code for a scope that constructs a ground term.
    %
:- pred ml_gen_ground_term(prog_var::in, hlds_goal::in,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

    % Record the contents of the module's const_struct_db in !GlobalData.
    %
:- pred ml_gen_const_structs(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::out, ml_global_data::in, ml_global_data::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.arg_pack.
:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module backend_libs.type_class_info.
:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_closure_gen.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_type_gen.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module varset.

:- inst no_or_direct_arg_tag for cons_tag/0
    --->    no_tag
    ;       direct_arg_tag(ground).

%---------------------------------------------------------------------------%

ml_gen_unification(Unification, CodeModel, Context, Defns, Stmts, !Info) :-
    (
        Unification = assign(TargetVar, SourceVar),
        expect(unify(CodeModel, model_det), $pred, "assign not det"),
        ml_variable_type(!.Info, TargetVar, Type),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        IsDummyType = is_type_a_dummy(ModuleInfo, Type),
        (
            % Skip dummy argument types, since they will not have been
            % declared.
            IsDummyType = is_dummy_type,
            Stmts = []
        ;
            IsDummyType = is_not_dummy_type,
            ml_gen_var(!.Info, TargetVar, TargetLval),
            ml_gen_var(!.Info, SourceVar, SourceLval),
            Stmt = ml_gen_assign(TargetLval, ml_lval(SourceLval), Context),
            Stmts = [Stmt]
        ),
        ( if ml_gen_info_search_const_var(!.Info, SourceVar, GroundTerm) then
            % If the source variable is a constant, so is the target after
            % this assignment.
            %
            % The mark_static_terms assumes that if SourceVar is a constant
            % term, then after this assignment unification TargetVar is a
            % constant term also. Therefore later constant terms may contain
            % TargetVar among their arguments. If we didn't copy the constant
            % info here, the construction of the later constant could cause
            % a code generator abort.
            ml_gen_info_set_const_var(TargetVar, GroundTerm, !Info)
        else
            true
        ),
        Defns = []
    ;
        Unification = simple_test(VarA, VarB),
        expect(unify(CodeModel, model_semi), $pred, "simple_test not semidet"),
        ml_variable_type(!.Info, VarA, Type),
        % XXX this should be a switch.
        ( if Type = builtin_type(builtin_type_string) then
            EqualityOp = str_eq
        else if Type = builtin_type(builtin_type_float) then
            EqualityOp = float_eq
        else if Type = builtin_type(builtin_type_int(IntType)) then
            EqualityOp = eq(IntType)
        else
            EqualityOp = eq(int_type_int)
        ),
        ml_gen_var(!.Info, VarA, VarALval),
        ml_gen_var(!.Info, VarB, VarBLval),
        Test = ml_binop(EqualityOp, ml_lval(VarALval), ml_lval(VarBLval)),
        ml_gen_set_success(Test, Context, Stmt, !Info),
        Defns = [],
        Stmts = [Stmt]
    ;
        Unification = construct(Var, ConsId, Args, ArgModes, HowToConstruct,
            _CellIsUnique, SubInfo),
        expect(unify(CodeModel, model_det), $pred, "construct not det"),
        (
            SubInfo = no_construct_sub_info,
            TakeAddr = []
        ;
            SubInfo = construct_sub_info(MaybeTakeAddr, MaybeSizeProfInfo),
            (
                MaybeTakeAddr = no,
                TakeAddr = []
            ;
                MaybeTakeAddr = yes(TakeAddr)
            ),
            expect(unify(MaybeSizeProfInfo, no), $pred,
                "term size profiling not yet supported")
        ),
        ml_gen_construct(Var, ConsId, Args, ArgModes, TakeAddr,
            HowToConstruct, Context, Defns, Stmts, !Info)
    ;
        Unification = deconstruct(Var, ConsId, Args, ArgModes, CanFail,
            CanCGC),
        (
            CanFail = can_fail,
            ExpectedCodeModel = model_semi,
            ml_gen_semi_deconstruct(Var, ConsId, Args, ArgModes, Context,
                Defns, UnifyStmts, !Info)
        ;
            CanFail = cannot_fail,
            ExpectedCodeModel = model_det,
            ml_gen_det_deconstruct(Var, ConsId, Args, ArgModes, Context,
                Defns, UnifyStmts, !Info)
        ),
        (
            % Note that we can deallocate a cell even if the unification fails;
            % it is the responsibility of the structure reuse phase to ensure
            % that this is safe.
            CanCGC = can_cgc,
            ml_gen_var(!.Info, Var, VarLval),
            % XXX Avoid strip_tag when we know what tag it will have.
            Delete = delete_object(ml_unop(strip_tag, ml_lval(VarLval))),
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
            Context, Stmts0, Stmts, !Info)
    ;
        Unification = complicated_unify(_, _, _),
        % Simplify.m should have converted these into procedure calls.
        unexpected($pred, "complicated unify")
    ).

    % ml_gen_construct generates code for a construction unification.
    %
:- pred ml_gen_construct(prog_var::in, cons_id::in, list(prog_var)::in,
    list(unify_mode)::in, list(int)::in, how_to_construct::in,
    prog_context::in, list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_construct(Var, ConsId, ArgVars, ArgModes, TakeAddr, HowToConstruct,
        Context, Defns, Stmts, !Info) :-
    % Figure out how this cons_id is represented.
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
    (
        ( ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ),
        ( if
            ArgVars = [ArgVar],
            ArgModes = [ArgMode]
        then
            ml_variable_type(!.Info, Var, VarType),
            ml_gen_var(!.Info, Var, VarLval),
            ml_gen_info_get_module_info(!.Info, ModuleInfo),
            MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, VarType),
            ( if
                ml_gen_info_search_const_var(!.Info, ArgVar, ArgGroundTerm)
            then
                ArgGroundTerm = ml_ground_term(ArgRval, _ArgType,
                    MLDS_ArgType),
                ml_gen_info_get_global_data(!.Info, GlobalData0),
                ml_gen_box_const_rval(ModuleInfo, Context, MLDS_ArgType,
                    aw_full_word, ArgRval, Rval0, GlobalData0, GlobalData),
                ml_gen_info_set_global_data(GlobalData, !Info),
                Rval = ml_cast_cons_tag(MLDS_Type, ConsTag, Rval0),
                GroundTerm = ml_ground_term(Rval, VarType, MLDS_Type),
                ml_gen_info_set_const_var(Var, GroundTerm, !Info),
                Stmt = ml_gen_assign(VarLval, Rval, Context),
                Stmts = [Stmt]
            else
                ml_gen_var(!.Info, ArgVar, ArgLval),
                ml_variable_type(!.Info, ArgVar, ArgType),
                (
                    ConsTag = no_tag,
                    ArgRval = ml_lval(ArgLval),
                    ml_gen_box_or_unbox_rval(ModuleInfo, ArgType, VarType,
                        bp_native_if_possible, ArgRval, Rval),
                    Stmt = ml_gen_assign(VarLval, Rval, Context),
                    Stmts = [Stmt]
                ;
                    ConsTag = direct_arg_tag(Ptag),
                    ml_gen_dynamic_construct_direct_arg(ModuleInfo, Ptag,
                        ArgMode, ArgLval, ArgType, VarLval, VarType,
                        Context, Stmts)
                )
            )
        else
            (
                ConsTag = no_tag,
                unexpected($pred, "no_tag: arity != 1")
            ;
                ConsTag = direct_arg_tag(_),
                unexpected($pred, "direct_arg_tag: arity != 1")
            )
        ),
        Defns = []
    ;
        % Ordinary compound terms.
        (
            ConsTag = single_functor_tag,
            Ptag = 0,
            MaybeStag = no
        ;
            ConsTag = unshared_tag(Ptag),
            MaybeStag = no
        ;
            ConsTag = shared_remote_tag(Ptag, Stag, AddedBy),
            MaybeStag = yes({Stag, AddedBy})
        ),
        UsesBaseClass = ml_tag_uses_base_class(ConsTag),
        ml_gen_construct_compound(ConsId, Ptag, MaybeStag, UsesBaseClass, Var,
            ArgVars, ArgModes, TakeAddr, HowToConstruct, Context,
            Defns, Stmts, !Info)
    ;
        % Lambda expressions.
        ConsTag = closure_tag(PredId, ProcId, _EvalMethod),
        ml_gen_closure(PredId, ProcId, Var, ArgVars, ArgModes, HowToConstruct,
            Context, Defns, Stmts, !Info)
    ;
        ( ConsTag = type_info_const_tag(ConstNum)
        ; ConsTag = typeclass_info_const_tag(ConstNum)
        ; ConsTag = ground_term_const_tag(ConstNum, _)
        ),
        ml_gen_info_get_const_struct_map(!.Info, ConstStructMap),
        map.lookup(ConstStructMap, ConstNum, GroundTerm0),
        GroundTerm0 = ml_ground_term(Rval, _Type, _MLDS_Type),
        ml_variable_type(!.Info, Var, VarType),
        ml_gen_var(!.Info, Var, VarLval),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, VarType),
        GroundTerm = ml_ground_term(Rval, VarType, MLDS_Type),
        ml_gen_info_set_const_var(Var, GroundTerm, !Info),
        Defns = [],
        Stmt = ml_gen_assign(VarLval, Rval, Context),
        Stmts = [Stmt]
    ;
        % Constants.
        ( ConsTag = int_tag(_)
        ; ConsTag = dummy_tag
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = float_tag(_)
        ; ConsTag = string_tag(_)
        ; ConsTag = shared_local_tag(_, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        (
            ArgVars = [],
            ml_variable_type(!.Info, Var, VarType),
            ml_gen_var(!.Info, Var, VarLval),
            ml_gen_info_get_module_info(!.Info, ModuleInfo),
            MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, VarType),
            (
                ConsTag = int_tag(IntTag),
                Rval = ml_int_tag_to_rval_const(IntTag, VarType, MLDS_Type)
            ;
                ConsTag = dummy_tag,
                % The type information is needed by the Java backend.
                Rval = ml_int_tag_to_rval_const(int_tag_int(0), VarType,
                    MLDS_Type)
            ;
                ConsTag = float_tag(Float),
                Rval = ml_const(mlconst_float(Float))
            ;
                ConsTag = string_tag(String),
                Rval = ml_const(mlconst_string(String))
            ;
                ConsTag = foreign_tag(ForeignLang, ForeignTag),
                Rval = ml_const(mlconst_foreign(ForeignLang, ForeignTag,
                    MLDS_Type))
            ;
                ConsTag = shared_local_tag(Ptag, SecTag),
                Rval = ml_cast(MLDS_Type, ml_mkword(Ptag,
                    ml_unop(mkbody, ml_const(mlconst_int(SecTag)))))
            ;
                ConsTag = type_ctor_info_tag(ModuleName0, TypeName, TypeArity),
                ModuleName = fixup_builtin_module(ModuleName0),
                MLDS_Module = mercury_module_name_to_mlds(ModuleName),
                RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
                RttiId = ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info),
                Const = mlconst_data_addr_rtti(MLDS_Module, RttiId),
                Rval = ml_cast(MLDS_Type, ml_const(Const))
            ;
                ConsTag = base_typeclass_info_tag(ModuleName, ClassId,
                    Instance),
                MLDS_Module = mercury_module_name_to_mlds(ModuleName),
                TCName = generate_class_name(ClassId),
                RttiId = tc_rtti_id(TCName,
                    type_class_base_typeclass_info(ModuleName, Instance)),
                Const = mlconst_data_addr_rtti(MLDS_Module, RttiId),
                Rval = ml_cast(MLDS_Type, ml_const(Const))
            ;
                ConsTag = tabling_info_tag(PredId, ProcId),
                ml_gen_pred_label(ModuleInfo, proc(PredId, ProcId),
                    PredLabel, PredModule),
                ProcLabel = mlds_proc_label(PredLabel, ProcId),
                QualProcLabel = qual_proc_label(PredModule, ProcLabel),
                Const = mlconst_data_addr_tabling(QualProcLabel, tabling_info),
                Rval = ml_cast(MLDS_Type, ml_const(Const))
            ;
                ConsTag = deep_profiling_proc_layout_tag(_, _),
                unexpected($pred, "deep_profiling_proc_layout_tag NYI")
            ;
                ConsTag = table_io_entry_tag(_, _),
                unexpected($pred, "table_io_entry_tag NYI")
            ),
            GroundTerm = ml_ground_term(Rval, VarType, MLDS_Type),
            ml_gen_info_set_const_var(Var, GroundTerm, !Info),
            Stmt = ml_gen_assign(VarLval, Rval, Context),
            Stmts = [Stmt]
        ;
            ArgVars = [_ | _],
            unexpected($pred, "bad constant term")
        ),
        Defns = []
    ).

:- pred ml_gen_info_lookup_const_var_rval(ml_gen_info::in, prog_var::in,
    mlds_rval::out) is det.

ml_gen_info_lookup_const_var_rval(Info, Var, Rval) :-
    ml_gen_info_lookup_const_var(Info, Var, GroundTerm),
    GroundTerm = ml_ground_term(Rval, _, _).

%---------------------------------------------------------------------------%

    % Generate code to construct a new object.
    %
:- pred ml_gen_construct_compound(cons_id::in,
    ptag::in, maybe({sectag, sectag_added_by})::in, tag_uses_base_class::in,
    prog_var::in, list(prog_var)::in, list(unify_mode)::in, list(int)::in,
    how_to_construct::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_construct_compound(ConsId, Ptag, MaybeStag, UsesBaseClass, Var,
        ArgVars, ArgModes, TakeAddr, HowToConstruct, Context,
        Defns, Stmts, !Info) :-
    ml_gen_info_get_target(!.Info, CompilationTarget),

    % Figure out which class name to construct.
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
        MaybeStag = yes({Stag, AddedBy}),
        UsesConstructors = ml_target_uses_constructors(CompilationTarget),
        (
            UsesConstructors = no,
            % XXX ARG_PACK
            expect(unify(AddedBy, sectag_added_by_unify), $pred,
                "AddedBy != sectag_added_by_unify"),
            ExplicitSecTag = yes,
            StagRval0 = ml_const(mlconst_int(Stag)),
            StagType0 = mlds_native_int_type,
            % With the low-level data representation, all fields -- even the
            % secondary tag -- are boxed, and so we need box it here.
            StagRval = ml_box(StagType0, StagRval0),
            StagType = mlds_generic_type,
            ExtraRvalsTypesWidths = [rval_type_and_width(StagRval, StagType,
                apw_full(arg_only_offset(0), cell_offset(0)), no)]
        ;
            UsesConstructors = yes,
            % Secondary tag is implicitly initialised by the constructor.
            % XXX ARG_PACK
            expect(unify(AddedBy, sectag_added_by_constructor), $pred,
                "AddedBy != sectag_added_by_constructor"),
            ExplicitSecTag = no,
            ExtraRvalsTypesWidths = []
        )
    ;
        MaybeStag = no,
        ExplicitSecTag = no,
        ExtraRvalsTypesWidths = []
    ),
    ml_gen_new_object(yes(ConsId), MaybeCtorName, Ptag, ExplicitSecTag,
        Var, ExtraRvalsTypesWidths, ArgVars, ArgModes, TakeAddr,
        HowToConstruct, Context, Defns, Stmts, !Info).

ml_gen_new_object(MaybeConsId, MaybeCtorName, Ptag, ExplicitSecTag, Var,
        ExtraRvalsTypesWidths, ArgVars, ArgModes, TakeAddr,
        HowToConstruct, Context, Defns, Stmts, !Info) :-
    (
        MaybeConsId = yes(ConsId),
        ConsIdOrClosure = ordinary_cons_id(ConsId)
    ;
        MaybeConsId = no,
        expect(unify(ExplicitSecTag, no), $pred, "sectag on closure"),
        list.length(ExtraRvalsTypesWidths, NumExtras),
        ConsIdOrClosure = closure_object(NumExtras)
    ),
    (
        HowToConstruct = construct_dynamically,
        ml_gen_new_object_dynamically(ConsIdOrClosure, MaybeCtorName,
            Ptag, ExplicitSecTag, Var, ExtraRvalsTypesWidths,
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
            Ptag, ExplicitSecTag, Var, ExtraRvalsTypesWidths,
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
        ExplicitSecTag, Var, ExtraRvalsTypesWidths, ArgVars, ArgModes,
        TakeAddr, Context, Stmts, !Info) :-
    % Find out the types of the constructor arguments and generate rvals
    % for them (boxing/unboxing if needed).
    ml_variable_type(!.Info, Var, VarType),
    maybe_cons_id_arg_types_and_widths(!.Info, VarType, ConsIdOrClosure,
        ArgVars, ArgVarsTypesWidths),
    ml_gen_info_get_use_atomic_cells(!.Info, UseAtomicCells),
    (
        UseAtomicCells = yes,
        MayUseAtomic0 = may_use_atomic_alloc
    ;
        UseAtomicCells = no,
        MayUseAtomic0 = may_not_use_atomic_alloc
    ),
    FirstArgNum = 1,
    NumExtraRvals = list.length(ExtraRvalsTypesWidths),
    PrevOffset = NumExtraRvals - 1,
    ml_gen_dynamic_construct_args(!.Info, ArgVarsTypesWidths, ArgModes,
        NumExtraRvals, FirstArgNum, PrevOffset, ArgVarsTypesWidths, TakeAddr,
        TakeAddrInfos, ArgRvalsTypesWidths0, MayUseAtomic0, MayUseAtomic),

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
    MakeNewObject = new_object(VarLval, Ptag, ExplicitSecTag, MLDS_VarType,
        yes(SizeInWordsRval), MaybeCtorName, ArgRvalsTypes,
        MayUseAtomic, MaybeAllocId),
    MakeNewObjStmt = ml_stmt_atomic(MakeNewObject, Context),

    MaybePtag = yes(Ptag),
    ml_gen_field_take_address_assigns(TakeAddrInfos, VarLval, MLDS_VarType,
        MaybePtag, Context, !.Info, TakeAddrStmts),
    Stmts = [MakeNewObjStmt | TakeAddrStmts].

%---------------------------------------------------------------------------%

:- pred ml_gen_new_object_statically(cons_id_or_closure::in,
    maybe(qual_ctor_id)::in, ptag::in, prog_var::in,
    list(mlds_rval_type_and_width)::in, list(prog_var)::in,
    prog_context::in, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_new_object_statically(ConsIdOrClosure, MaybeCtorName, Ptag,
        Var, ExtraRvalsTypesWidths, ArgVars, Context, Stmts, !Info) :-
    % Find out the types of the constructor arguments.
    ml_variable_type(!.Info, Var, VarType),
    maybe_cons_id_arg_types_and_widths(!.Info, VarType, ConsIdOrClosure,
        ArgVars, ArgVarsTypesWidths),

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
        Ptag, ExplicitSecTag, Var, ExtraRvalsTypesWidths, ArgVars, ArgModes,
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
            ml_tag_initial_offset_and_argnum(ReuseConsTag, ReusePrimTag,
                _ReuseOffSet, _ReuseArgNum)
        ), ReuseConsIds, ReusePrimaryTags0),
    list.remove_dups(ReusePrimaryTags0, ReusePrimaryTags),

    ml_variable_type(!.Info, Var, VarType),
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
    ml_tag_initial_offset_and_argnum(ConsTag, PrimaryTag,
        InitOffSet, ArgNum),
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
        ReuseVarRval = ml_mkword(PrimaryTag,
            ml_binop(body,
                ml_lval(ReuseVarLval),
                ml_gen_mktag(ReusePrimaryTag)))
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
    % XXX we do more work than we need to here, as some of the cells
    % may already contain the correct values.
    ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, ArgModes,
        InitOffSet, ArgNum, Context, TakeAddr, TakeAddrInfos,
        FieldDefns, FieldStmts, !Info),
    ml_gen_field_take_address_assigns(TakeAddrInfos, VarLval, MLDS_VarType,
        MaybePtag, Context, !.Info, TakeAddrStmts),
    ThenStmts = ExtraRvalStmts ++ FieldStmts ++ TakeAddrStmts,
    ThenStmt = ml_stmt_block([], [], ThenStmts, Context),

    % If the reassignment isn't possible because the target is statically
    % allocated, then fall back to dynamic allocation.
    ml_gen_new_object(yes(ConsId), MaybeCtorName, Ptag, ExplicitSecTag, Var,
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
        Offset = offset(OffsetInt),
        SourceRval = ml_mem_addr(ml_field(MaybePtag, ml_lval(CellLval),
            ml_field_offset(ml_const(mlconst_int(OffsetInt))),
            FieldType, CellType)),
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

:- pred ml_type_as_field(module_info::in, bool::in, mer_type::in,
    arg_width::in, mer_type::out) is det.

ml_type_as_field(ModuleInfo, HighLevelData, FieldType, FieldWidth,
        BoxedFieldType) :-
    ( if
        (
            HighLevelData = no,
            % With the low-level data representation, we store all fields
            % except for double-width floats as "boxed" so we ignore the
            % original field type and instead generate a polymorphic type
            % BoxedFieldType which we use for the type of the field. This type
            % is used in the calls to ml_gen_box_or_unbox_rval to ensure that
            % we box values when storing them into fields and unbox them when
            % extracting them from fields.
            FieldWidth \= aw_double_word
        ;
            HighLevelData = yes,
            % With the high-level data representation, we don't box everything,
            % but for the MLDS->C backend, we still need to box floating point
            % fields if they are wider than a word.
            ml_must_box_field_type(ModuleInfo, FieldType, FieldWidth)
        )
    then
        % XXX zs: I do not see any reason why TypeVar cannot be confused with
        % other type variables (whether constructed the same way or not),
        % nor do I see any reason why such confusion would not lead to errors.
        varset.init(TypeVarSet0),
        varset.new_var(TypeVar, TypeVarSet0, _TypeVarSet),
        % The kind is `star' since there are values with this type.
        BoxedFieldType = type_variable(TypeVar, kind_star)
    else
        BoxedFieldType = FieldType
    ).

:- func ml_gen_mktag(int) = mlds_rval.

ml_gen_mktag(Ptag) = ml_unop(mktag, ml_const(mlconst_int(Ptag))).

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

:- type take_addr_info
    --->    take_addr_info(
                % The variable we record the address in.
                tai_address_var             :: prog_var,

                % The offset of the field. This must take into account
                % extra arguments and argument packing.
                tai_offset                  :: field_offset,

                % The type of the field variable.
                tai_field_var_type          :: mlds_type,

                % The type of the field, possibly after boxing.
                tai_maybe_boxed_field_type  :: mlds_type
            ).

:- type field_offset
    --->    offset(int).

    % Create a list of rval_type_widths for the arguments of a
    % construction unification.
    %
    % For each argument which is input to the construction unification,
    % we produce the corresponding lval, boxed or unboxed if needed,
    % but if the argument is free, we produce a null value.
    %
:- pred ml_gen_dynamic_construct_args(ml_gen_info::in,
    list(arg_var_type_and_width)::in, list(unify_mode)::in,
    int::in, int::in, int::in,
    list(arg_var_type_and_width)::in, list(int)::in,
    list(take_addr_info)::out, list(mlds_rval_type_and_width)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out) is det.

ml_gen_dynamic_construct_args(_, [], [], _, _, _, _, _, [], [], !MayUseAtomic).
ml_gen_dynamic_construct_args(_, [], [_ | _], _, _, _, _, _, _, _,
        !MayUseAtomic) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_construct_args(_, [_ | _], [], _, _, _, _, _, _, _,
        !MayUseAtomic) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_construct_args(Info, [ArgVarTypeWidth | ArgVarsTypesWidths],
        [ArgMode | ArgModes], NumExtraArgs, CurArgNum, PrevOffset0,
        AllArgVarsTypesWidths, !.TakeAddr, TakeAddrInfos, RvalsMLDSTypesWidths,
        !MayUseAtomic) :-
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
        ArgPosWidth = apw_full(_, CellOffset),
        CurOffset = PrevOffset0 + 1,
        CellOffset = cell_offset(CellOffsetInt),
        expect(unify(CurOffset, CellOffsetInt), $pred, "full"),
        PrevOffset = PrevOffset0 + 1
    ;
        ArgPosWidth = apw_double(_, CellOffset, _),
        CurOffset = PrevOffset0 + 1,
        CellOffset = cell_offset(CellOffsetInt),
        expect(unify(CurOffset, CellOffsetInt), $pred, "double"),
        PrevOffset = PrevOffset0 + 2
    ;
        ArgPosWidth = apw_partial_first(_, CellOffset, _, _, _),
        CurOffset = PrevOffset0 + 1,
        CellOffset = cell_offset(CellOffsetInt),
        expect(unify(CurOffset, CellOffsetInt), $pred, "first"),
        PrevOffset = PrevOffset0 + 1
    ;
        ( ArgPosWidth = apw_partial_shifted(_, CellOffset, _, _, _, _)
        ; ArgPosWidth = apw_none_shifted(_, CellOffset)
        ),
        CurOffset = PrevOffset0,
        CellOffset = cell_offset(CellOffsetInt),
        expect(unify(CurOffset, CellOffsetInt), $pred, "shifted"),
        PrevOffset = PrevOffset0
    ;
        ArgPosWidth = apw_none_nowhere,
        % There *is* no offset at which the current argument is stored.
        CurOffset = -1,
        PrevOffset = PrevOffset0
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
        Offset = ml_calc_field_offset(NumExtraArgs, AllArgVarsTypesWidths,
            CurArgNum),
        expect(unify(Offset, offset(CurOffset)), $pred, "Offset != CurOffset"),
        OrigMLDS_Type = mercury_type_to_mlds_type(ModuleInfo, ConsArgType),
        TakeAddrInfo =
            take_addr_info(ArgVar, Offset, OrigMLDS_Type, MLDS_Type),
        ml_gen_dynamic_construct_args(Info, ArgVarsTypesWidths, ArgModes,
            NumExtraArgs, CurArgNum + 1, PrevOffset, AllArgVarsTypesWidths,
            !.TakeAddr, TakeAddrInfosTail, TailRvalsMLDSTypesWidths,
            !MayUseAtomic),
        TakeAddrInfos = [TakeAddrInfo | TakeAddrInfosTail],
        RvalsMLDSTypesWidths = [RvalMLDSTypeWidth | TailRvalsMLDSTypesWidths]
    else if ArgPosWidth = apw_none_nowhere then
        ml_gen_dynamic_construct_args(Info, ArgVarsTypesWidths, ArgModes,
            NumExtraArgs, CurArgNum + 1, PrevOffset, AllArgVarsTypesWidths,
            !.TakeAddr, TakeAddrInfos, RvalsMLDSTypesWidths, !MayUseAtomic)
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
                ArgPosWidth = apw_partial_first(_, _, NumBits, _, Fill),
                Shift = arg_shift(0)
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
            NumExtraArgs, CurArgNum + 1, PrevOffset, AllArgVarsTypesWidths,
            !.TakeAddr, TakeAddrInfos, TailRvalsMLDSTypesWidths,
            !MayUseAtomic),
        RvalsMLDSTypesWidths = [RvalMLDSTypeWidth | TailRvalsMLDSTypesWidths]
    ).

:- func ml_calc_field_offset(int, list(arg_var_type_and_width), int)
    = field_offset.

ml_calc_field_offset(NumExtraArgs, ArgVarsTypesWidths, ArgNum) = Offset :-
    ( if
        list.take(ArgNum - 1, ArgVarsTypesWidths, ArgVarsTypesWidthsBeforeArg)
    then
        PosWidthsBeforeArg = list.map(func(arg_type_and_width(_, _, W)) = W,
            ArgVarsTypesWidthsBeforeArg),
        WordsBeforeArg = count_distinct_words(PosWidthsBeforeArg),
        Offset = offset(NumExtraArgs + WordsBeforeArg)
    else
        unexpected($pred, "more fields than arg_widths")
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
    expect(is_apw_full(ArgPosWidth), $pred,
        "ArgPosWidth != apw_full(_)"),
    NextOffset = CurOffset + 1,
    FieldLval = ml_field(MaybePrimaryTag, ml_lval(VarLval), FieldId,
        ExtraType, MLDS_VarType),
    Stmt = ml_gen_assign(FieldLval, ExtraRval, Context),

    ml_gen_extra_arg_assigns(VarLval, MLDS_VarType, MaybePrimaryTag,
        NextOffset, ExtraRvalsTypesWidths, Context, Stmts, !Info).

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

ml_gen_det_deconstruct(Var, ConsId, ArgVars, Modes, Context,
        Defns, Stmts, !Info) :-
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
    (
        ( ConsTag = string_tag(_String)
        ; ConsTag = int_tag(_IntTag)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = float_tag(_Float)
        ; ConsTag = dummy_tag
        ; ConsTag = shared_local_tag(_Bits1, _Num1)
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
        ; ConsTag = unshared_tag(_UnsharedPtag)
        ; ConsTag = shared_remote_tag(_PrimaryTag, _SecondaryTag, _AddedBy)
        ),
        ml_variable_type(!.Info, Var, VarType),
        ml_gen_var(!.Info, Var, VarLval),
        decide_field_gen(!.Info, VarLval, VarType, ConsId, ConsTag, FieldGen),
        ml_tag_initial_offset_and_argnum(ConsTag, _, InitOffSet, ArgNum),
        ml_field_names_and_types(!.Info, VarType, ConsId, InitOffSet, ArgVars,
            ArgVarRepns),
        ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, Modes,
            InitOffSet, ArgNum, Context, [], _, Defns, Stmts, !Info)
    ).

    % Calculate the integer offset used to reference the first field of a
    % structure for lowlevel data or the first argument number to access
    % the field using the highlevel data representation. Abort if the tag
    % indicates that the data doesn't have any fields.
    %
:- pred ml_tag_initial_offset_and_argnum(cons_tag::in, ptag::out,
    field_offset::out, int::out) is det.

ml_tag_initial_offset_and_argnum(ConsTag, Ptag, InitOffset, ArgNum) :-
    (
        ConsTag = single_functor_tag,
        Ptag = 0,
        InitOffset = offset(0),
        ArgNum = 1
    ;
        ( ConsTag = unshared_tag(UnsharedPtag)
        ; ConsTag = direct_arg_tag(UnsharedPtag)
        ),
        Ptag = UnsharedPtag,
        InitOffset = offset(0),
        ArgNum = 1
    ;
        ConsTag = shared_remote_tag(PrimaryTag, _SecondaryTag, AddedBy),
        Ptag = PrimaryTag,
        (
            AddedBy = sectag_added_by_unify,
            InitOffset = offset(1)
        ;
            AddedBy = sectag_added_by_constructor,
            InitOffset = offset(0)
        ),
        ArgNum = 1
    ;
        ConsTag = ground_term_const_tag(_, SubTag),
        ml_tag_initial_offset_and_argnum(SubTag, Ptag, InitOffset, ArgNum)
    ;
        ( ConsTag = string_tag(_String)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = float_tag(_Float)
        ; ConsTag = dummy_tag
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = no_tag
        ; ConsTag = shared_local_tag(_Bits1, _Num1)
        ),
        unexpected($pred, "unexpected tag")
    ).

    % Given a type and a cons_id, and also the types of the actual arguments
    % of that cons_id in some particular use of it, look up the original types
    % of the fields of that cons_id from the type definition. Note that the
    % field types need not be the same as the actual argument types; for
    % polymorphic types, the types of the actual arguments can be an instance
    % of the field types.
    %
:- pred ml_field_names_and_types(ml_gen_info::in, mer_type::in,
    cons_id::in, field_offset::in, list(prog_var)::in,
    assoc_list(prog_var, constructor_arg_repn)::out) is det.

ml_field_names_and_types(Info, Type, ConsId, InitOffset, ArgVars,
        ArgVarRepns) :-
    % Lookup the field types for the arguments of this cons_id.
    InitOffset = offset(InitOffsetInt),
    ( if type_is_tuple(Type, _) then
        % Fields in tuples are all word-sized, and have no extra type_infos
        % and/or typeclass_infos in front of them. Their types are all
        % unbound type variables.
        allocate_consecutive_full_word_ctor_arg_repns_boxed(InitOffsetInt,
            ArgVars, ArgVarRepns)
    else
        ml_gen_info_get_module_info(Info, ModuleInfo),
        get_cons_repn_defn_det(ModuleInfo, ConsId, ConsRepnDefn),
        CtorArgRepns = ConsRepnDefn ^ cr_args,

        % Add the fields for any type_infos and/or typeclass_infos inserted
        % for existentially quantified data types. For these, we just copy
        % the types of the initial ArgVars.
        list.length(ArgVars, NumArgVars),
        list.length(CtorArgRepns, NumCtorArgs),
        NumExtraArgVars = NumArgVars - NumCtorArgs,
        ( if NumExtraArgVars > 0 then
            list.split_upto(NumExtraArgVars, ArgVars,
                ExtraArgVars, NonExtraArgVars),
            % The extra type_infos and/or typeclass_infos are all stored
            % in one full word each.
            allocate_consecutive_full_word_ctor_arg_repns_lookup(Info,
                InitOffsetInt, ExtraArgVars, ExtraArgVarRepns),
            assoc_list.from_corresponding_lists(NonExtraArgVars, CtorArgRepns,
                NonExtraArgVarRepns),
            ArgVarRepns =
                ExtraArgVarRepns ++ NonExtraArgVarRepns
        else
            assoc_list.from_corresponding_lists(ArgVars, CtorArgRepns,
                ArgVarRepns)
        )
    ).

:- type field_gen
    --->    field_gen(
                % The primary tag, if any, for the field reference.
                maybe(ptag),

                % The value and the MLDS type of the pointer to the cell.
                mlds_rval,
                mlds_type,

                % How we identify the field in the cell.
                field_via
            ).

:- type field_via
    --->    field_via_offset
            % We identify the field via ml_field_offset.

    ;       field_via_name(
                % We identify the field via ml_field_named.

                % The MLDS module name that is the qualifier
                % in the qual_field_var_name in the first argument
                % of ml_field_named. (The mlds_qual_kind is type_qual.)
                mlds_module_name,

                % The class pointer type that is second argument of
                % ml_field_named.
                mlds_type
            ).

:- pred decide_field_gen(ml_gen_info::in, mlds_lval::in, mer_type::in,
    cons_id::in, cons_tag::in, field_gen::out) is det.

decide_field_gen(Info, VarLval, VarType, ConsId, ConsTag, FieldGen) :-
    MaybePrimaryTag = get_maybe_primary_tag(ConsTag),
    AddrRval = ml_lval(VarLval),
    ml_gen_type(Info, VarType, AddrType),

    ml_gen_info_get_high_level_data(Info, HighLevelData),
    (
        HighLevelData = no,
        % With the low-level data representation, we access all fields
        % using offsets.
        FieldVia = field_via_offset
    ;
        HighLevelData = yes,
        % With the high-level data representation, we always use named fields,
        % except for tuple types.
        ( if type_is_tuple(VarType, _) then
            FieldVia = field_via_offset
        else if ConsId = cons(ConsSymName, ConsArity, TypeCtor) then
            ml_gen_info_get_target(Info, Target),
            ConsName = ml_gen_du_ctor_name(Target, TypeCtor,
                ConsSymName, ConsArity),
            % XXX ARG_PACK Delete this sanity test after it has been tested
            % for a while.
            type_to_ctor_det(VarType, VarTypeCtor),
            expect(unify(TypeCtor, VarTypeCtor), $pred,
                "TypeCtor != VarTypeCtor"),
            ml_gen_type_name(TypeCtor, QualTypeName, TypeArity),
            QualTypeName = qual_class_name(MLDS_Module, QualKind, TypeName),
            TypeQualifier = mlds_append_class_qualifier(Target, MLDS_Module,
                QualKind, TypeName, TypeArity),

            UsesBaseClass = ml_tag_uses_base_class(ConsTag),
            (
                UsesBaseClass = tag_uses_base_class,
                % There is only one functor for the type, and so
                % the class name is determined by the type name.
                ClassId = mlds_class_id(QualTypeName, TypeArity, mlds_class),
                FieldQualifier = TypeQualifier
            ;
                UsesBaseClass = tag_does_not_use_base_class,
                % The class name is determined by the constructor.
                QualConsName =
                    qual_class_name(TypeQualifier, type_qual, ConsName),
                ClassId = mlds_class_id(QualConsName, ConsArity, mlds_class),
                FieldQualifier = mlds_append_class_qualifier(Target,
                    TypeQualifier, type_qual, ConsName, ConsArity)
            ),
            ClassPtrType = mlds_ptr_type(mlds_class_type(ClassId)),
            FieldVia = field_via_name(FieldQualifier, ClassPtrType)
        else
            unexpected($pred, "unexpected cons_id")
        )
    ),
    FieldGen = field_gen(MaybePrimaryTag, AddrRval, AddrType, FieldVia).

:- pred ml_gen_dynamic_deconstruct_args(field_gen,
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    field_offset, int, prog_context, list(int), list(take_addr_info),
    list(mlds_local_var_defn), list(mlds_stmt), ml_gen_info, ml_gen_info).
:- mode ml_gen_dynamic_deconstruct_args(in, in, in, in, in, in,
    in(bound([])), out, out, out, in, out) is det.
:- mode ml_gen_dynamic_deconstruct_args(in, in, in, in, in, in,
    in, out, out, out, in, out) is det.

ml_gen_dynamic_deconstruct_args(_, [], [], _, _, _, TakeAddr,
        [], [], [], !Info) :-
    expect(unify(TakeAddr, []), $pred, "TakeAddr != []").
ml_gen_dynamic_deconstruct_args(_, [], [_ | _], _, _, _, _, _, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args(_, [_ | _], [], _, _, _, _, _, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args(FieldGen,
        [ArgVarRepn | ArgVarRepns], [Mode | Modes], CurOffset, CurArgNum,
        Context, TakeAddr, TakeAddrInfos, Defns, Stmts, !Info) :-
    ArgVarRepn = ArgVar - CtorArgRepn,
    ml_next_field_offset(CtorArgRepn, ArgVarRepns, CurOffset, NextOffset),
    NextArgNum = CurArgNum + 1,
    FieldPosWidth = CtorArgRepn ^ car_pos_width,
    ( if
        TakeAddr = [CurArgNum | TailTakeAddr]
    then
        expect(is_apw_full(FieldPosWidth), $pred,
            "taking address of something other than a full word"),
        ml_gen_take_addr_of_arg(!.Info, ArgVar, CtorArgRepn,
            CurOffset, TakeAddrInfo),
        ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, Modes,
            NextOffset, NextArgNum, Context, TailTakeAddr, TakeAddrInfosTail,
            Defns, Stmts, !Info),
        TakeAddrInfos = [TakeAddrInfo | TakeAddrInfosTail]
    else if
        FieldPosWidth = apw_partial_first(_, _, _, _, _),
        % Without field_via_offset, we have no way to get a whole word
        % from a memory cell at once.
        FieldGen = field_gen(_MaybePtag, _AddrRval, _AddrType, FieldVia),
        FieldVia = field_via_offset
    then
        ml_gen_dynamic_deconstruct_args_in_word(FieldGen,
            ArgVar, CtorArgRepn, Mode,
            ArgVarRepns, Modes, LeftOverArgVarRepns, LeftOverModes,
            CurOffset, LeftOverOffset, CurArgNum, LeftOverArgNum,
            Context, TakeAddr, HeadDefns, HeadStmts, !Info),
        ml_gen_dynamic_deconstruct_args(FieldGen,
            LeftOverArgVarRepns, LeftOverModes, LeftOverOffset, LeftOverArgNum,
            Context, TakeAddr, TakeAddrInfos, TailDefns, TailStmts, !Info),
        Defns = HeadDefns ++ TailDefns,
        Stmts = HeadStmts ++ TailStmts
    else
        ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
            CurOffset, CurArgNum, Context, _PackedArgVars, HeadStmts, !Info),
        ml_gen_dynamic_deconstruct_args(FieldGen, ArgVarRepns, Modes,
            NextOffset, NextArgNum, Context, TakeAddr, TakeAddrInfos,
            Defns, TailStmts, !Info),
        Stmts = HeadStmts ++ TailStmts
    ).

:- pred ml_gen_dynamic_deconstruct_args_in_word(field_gen,
    prog_var, constructor_arg_repn, unify_mode,
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    assoc_list(prog_var, constructor_arg_repn), list(unify_mode),
    field_offset, field_offset, int, int, prog_context, list(int),
    list(mlds_local_var_defn), list(mlds_stmt), ml_gen_info, ml_gen_info).
:- mode ml_gen_dynamic_deconstruct_args_in_word(in, in, in, in, in, in,
    out, out, in, out, in, out, in, in(bound([])), out, out, in, out) is det.
:- mode ml_gen_dynamic_deconstruct_args_in_word(in, in, in, in, in, in,
    out, out, in, out, in, out, in, in, out, out, in, out) is det.

ml_gen_dynamic_deconstruct_args_in_word(FieldGen, ArgVar, CtorArgRepn, Mode,
        ArgVarRepns, Modes, LeftOverArgVarRepns, LeftOverModes,
        CurOffset, LeftOverOffset, CurArgNum, LeftOverArgNum,
        Context, TakeAddr, Defns, Stmts, !Info) :-
    ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
        CurOffset, CurArgNum, Context, HeadPackedArgVars, HeadStmts, !Info),
    (
        HeadPackedArgVars = [],
        AllPartialsRight0 = not_all_partials_assign_right
    ;
        HeadPackedArgVars = [_ | _],
        AllPartialsRight0 = all_partials_assign_right
    ),
    ml_next_field_offset(CtorArgRepn, ArgVarRepns, CurOffset, NextOffset),
    NextArgNum = CurArgNum + 1,
    ml_gen_dynamic_deconstruct_args_in_word_loop(FieldGen,
        ArgVarRepns, Modes, LeftOverArgVarRepns, LeftOverModes,
        NextOffset, LeftOverOffset, NextArgNum, LeftOverArgNum,
        Context, TakeAddr, AllPartialsRight0, AllPartialsRight,
        TailPackedArgVars, TailStmts, !Info),
    % XXX ARG_PACK
    % We could get ml_gen_dynamic_deconstruct_args_in_word_loop to tell us
    % when all the args in the word assign left, as in that case,
    % we could generate better code than the one generates by
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
        FieldPosWidth = CtorArgRepn ^ car_pos_width,
        ( if FieldPosWidth = apw_partial_first(_, CellOffset, _, _, _) then
            CurOffset = offset(CurOffsetInt),
            CellOffset = cell_offset(CellOffsetInt),
            expect(unify(CurOffsetInt, CellOffsetInt), $pred,
                "CurOffset != CellOffset")
        else
            unexpected($pred, "no apw_partial_first")
        ),

        FieldId = ml_field_offset(ml_const(mlconst_int(CellOffsetInt))),
        FieldGen = field_gen(MaybePtag, AddrRval, AddrType, _),
        FieldLval = ml_field(MaybePtag, AddrRval, FieldId,
            mlds_generic_type, AddrType),
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
    field_offset, field_offset, int, int, prog_context, list(int),
    do_all_partials_assign_right, do_all_partials_assign_right,
    list(packed_arg_var), list(mlds_stmt), ml_gen_info, ml_gen_info).
:- mode ml_gen_dynamic_deconstruct_args_in_word_loop(in, in, in, out, out,
    in, out, in, out, in, in(bound([])), in, out, out, out, in, out) is det.
:- mode ml_gen_dynamic_deconstruct_args_in_word_loop(in, in, in, out, out,
    in, out, in, out, in, in, in, out, out, out, in, out) is det.

ml_gen_dynamic_deconstruct_args_in_word_loop(_FieldGen, [], [], [], [],
        CurOffset, LeftOverOffset, CurArgNum, LeftOverArgNum,
        _Context, _TakeAddr, !AllPartialsRight, [], [], !Info) :-
    LeftOverOffset = CurOffset,
    LeftOverArgNum = CurArgNum.
ml_gen_dynamic_deconstruct_args_in_word_loop(_FieldGen, [], [_ | _], _, _,
        _, _, _, _, _, _, !AllPartialsRight, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args_in_word_loop(_FieldGen, [_ | _], [], _, _,
        _, _, _, _, _, _, !AllPartialsRight, _, _, !Info) :-
    unexpected($pred, "length mismatch").
ml_gen_dynamic_deconstruct_args_in_word_loop(FieldGen,
        [ArgVarRepn | ArgVarRepns], [Mode | Modes],
        LeftOverArgVarRepns, LeftOverModes,
        CurOffset, LeftOverOffset, CurArgNum, LeftOverArgNum,
        Context, TakeAddr, !AllPartialsRight,
        PackedArgVars, Stmts, !Info) :-
    ArgVarRepn = ArgVar - CtorArgRepn,
    FieldPosWidth = CtorArgRepn ^ car_pos_width,
    (
        (
            FieldPosWidth = apw_partial_shifted(_, _, _, _, _, _),
            ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
                CurOffset, CurArgNum, Context,
                HeadPackedArgVars, HeadStmts, !Info),
            (
                HeadPackedArgVars = [],
                !:AllPartialsRight = not_all_partials_assign_right
            ;
                HeadPackedArgVars = [_ | _]
            )
        ;
            FieldPosWidth = apw_none_shifted(_, _),
            ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
                CurOffset, CurArgNum, Context,
                HeadPackedArgVars, HeadStmts, !Info),
            expect(unify(HeadPackedArgVars, []), $pred,
                "HeadPackedArgVars != [] for apw_none_shifted")
        ),
        ( if TakeAddr = [CurArgNum | _TailTakeAddr] then
            unexpected($pred,
                "taking address of something other than a full word")
        else
            true
        ),
        ml_next_field_offset(CtorArgRepn, ArgVarRepns, CurOffset, NextOffset),
        NextArgNum = CurArgNum + 1,
        ml_gen_dynamic_deconstruct_args_in_word_loop(FieldGen,
            ArgVarRepns, Modes, LeftOverArgVarRepns, LeftOverModes,
            NextOffset, LeftOverOffset, NextArgNum, LeftOverArgNum,
            Context, TakeAddr, !AllPartialsRight,
            TailPackedArgVars, TailStmts, !Info),
        PackedArgVars = HeadPackedArgVars ++ TailPackedArgVars,
        Stmts = HeadStmts ++ TailStmts
    ;
        ( FieldPosWidth = apw_full(_, _)
        ; FieldPosWidth = apw_double(_, _, _)
        ; FieldPosWidth = apw_partial_first(_, _, _, _, _)
        ; FieldPosWidth = apw_none_nowhere
        ),
        LeftOverArgVarRepns = [ArgVarRepn | ArgVarRepns],
        LeftOverModes = [Mode | Modes],
        LeftOverOffset = CurOffset,
        LeftOverArgNum = CurArgNum,
        PackedArgVars = [],
        Stmts = []
    ).

:- pred ml_gen_take_addr_of_arg(ml_gen_info::in,
    prog_var::in, constructor_arg_repn::in, field_offset::in,
    take_addr_info::out) is det.

ml_gen_take_addr_of_arg(Info, ArgVar, CtorArgRepn, CurOffset, TakeAddrInfo) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    FieldType = CtorArgRepn ^ car_type,
    FieldPosWidth = CtorArgRepn ^ car_pos_width,
    FieldWidth = arg_pos_width_to_width_only(FieldPosWidth),
    ml_type_as_field(ModuleInfo, HighLevelData, FieldType, FieldWidth,
        BoxedFieldType),
    ml_gen_type(Info, FieldType, MLDS_FieldType),
    ml_gen_type(Info, BoxedFieldType, MLDS_BoxedFieldType),
    TakeAddrInfo = take_addr_info(ArgVar, CurOffset, MLDS_FieldType,
        MLDS_BoxedFieldType).

:- pred ml_gen_dynamic_deconstruct_arg(field_gen::in,
    prog_var::in, constructor_arg_repn::in, unify_mode::in,
    field_offset::in, int::in, prog_context::in,
    list(packed_arg_var)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_dynamic_deconstruct_arg(FieldGen, ArgVar, CtorArgRepn, Mode,
        Offset, ArgNum, Context, PackedArgVars, Stmts, !Info) :-
    FieldGen = field_gen(MaybePrimaryTag, AddrRval, AddrType, FieldVia),
    (
        FieldVia = field_via_offset,
        % XXX We should check whether the cell offset in FieldPosWidth,
        % if any, matches Offset.
        Offset = offset(OffsetInt),
        FieldId = ml_field_offset(ml_const(mlconst_int(OffsetInt)))
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
    FieldPosWidth = CtorArgRepn ^ car_pos_width,
    FieldWidth = arg_pos_width_to_width_only(FieldPosWidth),
    FieldRawType = CtorArgRepn ^ car_type,
    ml_type_as_field(ModuleInfo, HighLevelData, FieldRawType, FieldWidth,
        FieldType),

    % Generate lvals for the LHS ...
    ml_gen_type(!.Info, FieldType, MLDS_FieldType),
    FieldLval = ml_field(MaybePrimaryTag, AddrRval, FieldId,
        MLDS_FieldType, AddrType),
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
            FieldLval, FieldType, FieldPosWidth, ArgVar, ArgLval, ArgType,
            Context, PackedArgVars, Stmts)
    ;
        Dir = assign_nondummy_left,
        PackedArgVars = [],
        ml_gen_dynamic_deconstruct_arg_unify_assign_left(ModuleInfo,
            HighLevelData, FieldLval, FieldType, FieldPosWidth,
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
        FieldLval, FieldType, FieldPosWidth, ArgVar, ArgLval, ArgType,
        Context, PackedArgVars, Stmts) :-
    (
        FieldPosWidth = apw_double(_, _, _),
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
        FieldPosWidth = apw_full(_, _),
        PackedArgVars = [],
        ml_gen_box_or_unbox_rval(ModuleInfo, FieldType, ArgType,
            bp_native_if_possible, ml_lval(FieldLval), FieldRval),
        Stmt = ml_gen_assign(ArgLval, FieldRval, Context),
        Stmts = [Stmt]
    ;
        (
            FieldPosWidth = apw_partial_first(_, _, NumBits, Mask, Fill),
            Shift = arg_shift(0)
        ;
            FieldPosWidth = apw_partial_shifted(_, _, Shift, NumBits, Mask,
                Fill)
        ),
        PackedArgVar = packed_arg_var(ArgVar, Shift, NumBits, Fill),
        PackedArgVars = [PackedArgVar],
        UnsignedMLDSType = mlds_int_type_uint,
        FieldRval = ml_cast(UnsignedMLDSType, ml_lval(FieldLval)),
        Mask = arg_mask(MaskInt),
        MaskedRval = ml_bitwise_and(ml_rshift(FieldRval, Shift), MaskInt),
        (
            Fill = fill_enum,
            ToAssignRval = MaskedRval
        ;
            ( Fill = fill_int8,   CastMLDSType = mlds_int_type_int8
            ; Fill = fill_uint8,  CastMLDSType = mlds_int_type_uint8
            ; Fill = fill_int16,  CastMLDSType = mlds_int_type_int16
            ; Fill = fill_uint16, CastMLDSType = mlds_int_type_uint16
            ; Fill = fill_int32,  CastMLDSType = mlds_int_type_int32
            ; Fill = fill_uint32, CastMLDSType = mlds_int_type_uint32
            ),
            ToAssignRval = ml_cast(CastMLDSType, MaskedRval)
        ),
        Stmt = ml_gen_assign(ArgLval, ToAssignRval, Context),
        Stmts = [Stmt]
    ;
        ( FieldPosWidth = apw_none_nowhere
        ; FieldPosWidth = apw_none_shifted(_, _)
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
        FieldLval, FieldType, FieldPosWidth, ArgLval, ArgType, Context,
        Stmts) :-
    (
        FieldPosWidth = apw_double(_, _, _),
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
        FieldPosWidth = apw_full(_, _),
        ml_gen_box_or_unbox_rval(ModuleInfo, ArgType, FieldType,
            bp_native_if_possible, ml_lval(ArgLval), ArgRval),
        Stmt = ml_gen_assign(FieldLval, ArgRval, Context),
        Stmts = [Stmt]
    ;
        (
            FieldPosWidth = apw_partial_first(_, _, _, Mask, Fill),
            Shift = arg_shift(0)
        ;
            FieldPosWidth = apw_partial_shifted(_, _, Shift, _, Mask, Fill)
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
        ( FieldPosWidth = apw_none_shifted(_, _)
        ; FieldPosWidth = apw_none_nowhere
        ),
        % Nothing to do.
        Stmts = []
    ).

:- pred ml_field_offset_pair(mlds_lval::in, mlds_lval::out, mlds_lval::out)
    is semidet.

ml_field_offset_pair(FieldLval, FieldLvalA, FieldLvalB) :-
    FieldLval = ml_field(Ptag, Address, FieldIdA, _, PtrType),
    FieldIdA = ml_field_offset(FieldOffsetA),
    ( if FieldOffsetA = ml_const(mlconst_int(Offset)) then
        FieldIdB = ml_field_offset(ml_const(mlconst_int(Offset + 1))),
        SubstType = mlds_generic_type,
        FieldLvalA = ml_field(Ptag, Address, FieldIdA, SubstType, PtrType),
        FieldLvalB = ml_field(Ptag, Address, FieldIdB, SubstType, PtrType)
    else
        sorry($pred, "unexpected field offset")
    ).

:- pred ml_gen_dynamic_construct_direct_arg(module_info::in, ptag::in,
    unify_mode::in, mlds_lval::in, mer_type::in, mlds_lval::in, mer_type::in,
    prog_context::in, list(mlds_stmt)::out) is det.

ml_gen_dynamic_construct_direct_arg(ModuleInfo, Ptag,
        ArgMode, ArgLval, ArgType, VarLval, VarType, Context, Stmts) :-
    compute_assign_direction(ModuleInfo, ArgMode, ArgType, VarType, Dir),
    (
        Dir = assign_nondummy_right,
        unexpected($pred, "left-to-right data flow in construction")
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
        % Unused - unused: it is a partial assignment to the LHS
        % where the tag is known but the argument isn't.
        MLDS_ArgType = mercury_type_to_mlds_type(ModuleInfo, ArgType),
        ml_gen_box_or_unbox_rval(ModuleInfo, ArgType, VarType,
            bp_native_if_possible, ml_const(mlconst_null(MLDS_ArgType)),
            ArgRval),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, VarType),
        CastRval = ml_cast(MLDS_Type, ml_mkword(Ptag, ArgRval)),
        Stmt = ml_gen_assign(VarLval, CastRval, Context),
        Stmts = [Stmt]
    ;
        Dir = assign_dummy,
        unexpected($pred, "dummy unify")
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
        CastRval = ml_cast(MLDS_Type,
            ml_binop(body, VarRval, ml_const(mlconst_int(Ptag)))),
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
    FieldPosWidth = apw_full(arg_only_offset(0), cell_offset(0)),
    compute_assign_direction(ModuleInfo, Mode, ArgType, VarType, Dir),
    (
        Dir = assign_nondummy_right,
        ml_gen_dynamic_deconstruct_arg_unify_assign_right(ModuleInfo,
            VarLval, VarType, FieldPosWidth, ArgVar, ArgLval, ArgType,
            Context, _PackedArgVars, Stmts)
    ;
        Dir = assign_nondummy_left,
        ml_gen_dynamic_deconstruct_arg_unify_assign_left(ModuleInfo,
            HighLevelData, VarLval, VarType, FieldPosWidth, ArgLval, ArgType,
            Context, Stmts)
    ;
        ( Dir = assign_nondummy_unused
        ; Dir = assign_dummy
        ),
        % The unification has no effect.
        Stmts = []
    ).

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

ml_gen_semi_deconstruct(Var, ConsId, ArgVars, ArgModes, Context,
        Defns, Stmts, !Info) :-
    ml_gen_tag_test(Var, ConsId, TagTestExpr, !Info),
    ml_gen_set_success(TagTestExpr, Context, SetTagTestResult, !Info),
    ml_gen_test_success(SucceededExpr, !Info),
    ml_gen_det_deconstruct(Var, ConsId, ArgVars, ArgModes, Context,
        Defns, GetArgsStmts, !Info),
    (
        GetArgsStmts = [],
        Stmts = [SetTagTestResult]
    ;
        GetArgsStmts = [_ | _],
        GetArgs = ml_gen_block([], [], GetArgsStmts, Context),
        IfStmt = ml_stmt_if_then_else(SucceededExpr, GetArgs, no, Context),
        Stmts = [SetTagTestResult, IfStmt]
    ).

    % ml_gen_tag_test(Var, ConsId, Expr, !Info):
    %
    % Generate code to perform a tag test.
    %
    % The test checks whether Var has the functor specified by ConsId.
    % The generated code will not contain Defns or Stmts; it will be
    % only an Expr, which will be a boolean rval. Expr will evaluate to true
    % iff the Var has the functor specified by ConsId.
    %
:- pred ml_gen_tag_test(prog_var::in, cons_id::in, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_tag_test(Var, ConsId, TagTestExpr, !Info) :-
    % NOTE: Keep in sync with ml_gen_known_tag_test below.

    % TODO: apply the reverse tag test optimization for types with two
    % functors (see unify_gen.m).

    ml_gen_var(!.Info, Var, VarLval),
    ml_variable_type(!.Info, Var, Type),
    ml_cons_id_to_tag(!.Info, ConsId, ConsTag),
    TagTestExpr =
        ml_gen_tag_test_rval(!.Info, ConsTag, Type, ml_lval(VarLval)).

ml_gen_known_tag_test(Var, TaggedConsId, TagTestExpr, !Info) :-
    % NOTE: Keep in sync with ml_gen_tag_test above.

    % TODO: apply the reverse tag test optimization for types with two
    % functors (see unify_gen.m).

    ml_gen_var(!.Info, Var, VarLval),
    ml_variable_type(!.Info, Var, Type),
    TaggedConsId = tagged_cons_id(_ConsId, ConsTag),
    TagTestExpr =
        ml_gen_tag_test_rval(!.Info, ConsTag, Type, ml_lval(VarLval)).

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
        ConsTag = dummy_tag,
        TagTestRval = ml_const(mlconst_true)
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
    ;
        ConsTag = no_tag,
        TagTestRval = ml_const(mlconst_true)
    ;
        ConsTag = single_functor_tag,
        TagTestRval = ml_const(mlconst_true)
    ;
        ( ConsTag = unshared_tag(UnsharedPtag)
        ; ConsTag = direct_arg_tag(UnsharedPtag)
        ),
        RvalTag = ml_unop(tag, Rval),
        UnsharedTag = ml_unop(mktag, ml_const(mlconst_int(UnsharedPtag))),
        TagTestRval = ml_binop(eq(int_type_int), RvalTag, UnsharedTag)
    ;
        ConsTag = shared_remote_tag(PrimaryTag, SecondaryTag, _AddedBy),
        ml_gen_secondary_tag_rval(Info, Type, Rval, PrimaryTag,
            SecondaryTagFieldRval),
        SecondaryTagTestRval = ml_binop(eq(int_type_int),
            SecondaryTagFieldRval, ml_const(mlconst_int(SecondaryTag))),
        ml_gen_info_get_num_ptag_bits(Info, NumPtagBits),
        ( if NumPtagBits = 0 then
            % No need to test the primary tag.
            TagTestRval = SecondaryTagTestRval
        else
            RvalPtag = ml_unop(tag, Rval),
            PrimaryTagRval = ml_unop(mktag, ml_const(mlconst_int(PrimaryTag))),
            PrimaryTagTestRval = ml_binop(eq(int_type_int), RvalPtag,
                PrimaryTagRval),
            TagTestRval = ml_binop(logical_and,
                PrimaryTagTestRval, SecondaryTagTestRval)
        )
    ;
        ConsTag = shared_local_tag(Ptag, Num),
        ml_gen_info_get_module_info(Info, ModuleInfo),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
        TagTestRval = ml_binop(eq(int_type_int), Rval,
            ml_cast(MLDS_Type,
                ml_mkword(Ptag, ml_unop(mkbody, ml_const(mlconst_int(Num))))))
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

ml_gen_secondary_tag_rval(Info, VarType, Rval, PrimaryTag, StagFieldRval) :-
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    MLDS_VarType = mercury_type_to_mlds_type(ModuleInfo, VarType),
    (
        HighLevelData = no,
        % Note: with the low-level data representation, all fields are boxed,
        % even the secondary tag, and so we need to unbox (i.e. cast) it
        % back to the right type here.
        StagFieldRval =
            ml_unbox(mlds_native_int_type,
                ml_lval(ml_field(yes(PrimaryTag), Rval,
                    ml_field_offset(ml_const(mlconst_int(0))),
                    mlds_generic_type, MLDS_VarType)))
    ;
        HighLevelData = yes,
        ml_gen_info_get_target(Info, Target),
        FieldId = ml_gen_hl_tag_field_id(ModuleInfo, Target, VarType),
        StagFieldRval = ml_lval(ml_field(yes(PrimaryTag), Rval,
            FieldId, mlds_native_int_type, MLDS_VarType))
    ).

    % Return the field_id for the "data_tag" field of the specified
    % Mercury type, which holds the secondary tag.
    %
:- func ml_gen_hl_tag_field_id(module_info, mlds_target_lang, mer_type)
    = mlds_field_id.

ml_gen_hl_tag_field_id(ModuleInfo, Target, Type) = FieldId :-
    % Figure out the type name and arity.
    type_to_ctor_det(Type, TypeCtor),
    ml_gen_type_name(TypeCtor, QualifiedTypeName, TypeArity),
    QualifiedTypeName = qual_class_name(MLDS_Module, TypeQualKind, TypeName),

    % Figure out whether this type has constructors both with and without
    % secondary tags. If so, then the secondary tag field is in a class
    % "tag_type" that is derived from the base class for this type,
    % rather than in the base class itself.
    module_info_get_type_table(ModuleInfo, TypeTable),
    lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_body(TypeDefn, TypeDefnBody),
    (
        TypeDefnBody = hlds_du_type(_, _, MaybeRepn, _),
        (
            MaybeRepn = no,
            unexpected($pred, "MaybeRepn = no")
        ;
            MaybeRepn = yes(Repn)
        ),
        CtorRepns = Repn ^ dur_ctor_repns,
        ctors_with_and_without_secondary_tag(CtorRepns, NumWith, NumWithout),
        ( if
            NumWith > 0,
            NumWithout > 0
        then
            ClassQualifier = mlds_append_class_qualifier_module_qual(
                MLDS_Module, TypeName, TypeArity),
            ClassQualKind = TypeQualKind,
            ClassName = "tag_type",
            ClassArity = 0
        else
            ClassQualifier = MLDS_Module,
            ClassQualKind = module_qual,
            ClassName = TypeName,
            ClassArity = TypeArity
        )
    ;
        ( TypeDefnBody = hlds_eqv_type(_)
        ; TypeDefnBody = hlds_foreign_type(_)
        ; TypeDefnBody = hlds_solver_type(_)
        ; TypeDefnBody = hlds_abstract_type(_)
        ),
        unexpected($pred, "non-du type")
    ),

    % Put it all together.
    QualClassName = qual_class_name(ClassQualifier, ClassQualKind, ClassName),
    ClassId = mlds_class_id(QualClassName, ClassArity, mlds_class),
    ClassPtrType = mlds_ptr_type(mlds_class_type(ClassId)),
    FieldQualifier = mlds_append_class_qualifier(Target, ClassQualifier,
        ClassQualKind, ClassName, ClassArity),
    QualifiedFieldName =
        qual_field_var_name(FieldQualifier, type_qual, fvn_data_tag),
    FieldId = ml_field_named(QualifiedFieldName, ClassPtrType).

%---------------------------------------------------------------------------%

ml_gen_ground_term(TermVar, Goal, Stmts, !Info) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    set_of_var.to_sorted_list(NonLocals, NonLocalList),
    (
        NonLocalList = [],
        % The term being constructed by the scope is not needed, so there is
        % nothing to do.
        Stmts = []
    ;
        NonLocalList = [NonLocal],
        ( if NonLocal = TermVar then
            ( if GoalExpr = conj(plain_conj, Conjuncts) then
                ml_gen_info_get_module_info(!.Info, ModuleInfo),
                ml_gen_info_get_target(!.Info, Target),
                ml_gen_info_get_high_level_data(!.Info, HighLevelData),
                ml_gen_info_get_var_types(!.Info, VarTypes),

                ml_gen_info_get_global_data(!.Info, GlobalData0),
                ml_gen_ground_term_conjuncts(ModuleInfo, Target, HighLevelData,
                    VarTypes, Conjuncts, GlobalData0, GlobalData,
                    map.init, GroundTermMap),
                ml_gen_info_set_global_data(GlobalData, !Info),

                map.lookup(GroundTermMap, TermVar, TermVarGroundTerm),
                ml_gen_info_set_const_var(TermVar, TermVarGroundTerm, !Info),

                ml_gen_var(!.Info, TermVar, TermVarLval),
                TermVarGroundTerm = ml_ground_term(TermVarRval, _, _),
                Context = goal_info_get_context(GoalInfo),
                Stmt = ml_gen_assign(TermVarLval, TermVarRval, Context),
                Stmts = [Stmt]
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

:- pred ml_gen_ground_term_conjuncts(module_info::in, mlds_target_lang::in,
    bool::in, vartypes::in, list(hlds_goal)::in,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

ml_gen_ground_term_conjuncts(_, _, _, _, [], !GlobalData, !GroundTermMap).
ml_gen_ground_term_conjuncts(ModuleInfo, Target, HighLevelData, VarTypes,
        [Goal | Goals], !GlobalData, !GroundTermMap) :-
    ml_gen_ground_term_conjunct(ModuleInfo, Target, HighLevelData, VarTypes,
        Goal, !GlobalData, !GroundTermMap),
    ml_gen_ground_term_conjuncts(ModuleInfo, Target, HighLevelData, VarTypes,
        Goals, !GlobalData, !GroundTermMap).

:- pred ml_gen_ground_term_conjunct(module_info::in, mlds_target_lang::in,
    bool::in, vartypes::in, hlds_goal::in,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

ml_gen_ground_term_conjunct(ModuleInfo, Target, HighLevelData, VarTypes,
        Goal, !GlobalData, !GroundTermMap) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    ( if
        GoalExpr = unify(_, _, _, Unify, _),
        Unify = construct(Var, ConsId, ArgVars, _, _HowToConstruct,
            _, SubInfo),
        SubInfo = no_construct_sub_info
    then
        lookup_var_type(VarTypes, Var, VarType),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, VarType),
        ConsTag = cons_id_to_tag(ModuleInfo, ConsId),
        Context = goal_info_get_context(GoalInfo),
        ml_gen_ground_term_conjunct_tag(ModuleInfo, Target, HighLevelData,
            VarTypes, Var, VarType, MLDS_Type, ConsId, ConsTag, ArgVars,
            Context, !GlobalData, !GroundTermMap)
    else
        unexpected($pred, "malformed goal")
    ).

:- pred ml_gen_ground_term_conjunct_tag(module_info::in,
    mlds_target_lang::in, bool::in, vartypes::in,
    prog_var::in, mer_type::in, mlds_type::in, cons_id::in, cons_tag::in,
    list(prog_var)::in, prog_context::in,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

ml_gen_ground_term_conjunct_tag(ModuleInfo, Target, HighLevelData, VarTypes,
        Var, VarType, MLDS_Type, ConsId, ConsTag, ArgVars, Context,
        !GlobalData, !GroundTermMap) :-
    (
        % Constants.
        (
            ConsTag = int_tag(IntTag),
            IntConst = int_tag_to_mlds_rval_const(VarType, MLDS_Type, IntTag),
            ConstRval = ml_const(IntConst)
        ;
            ConsTag = dummy_tag,
            % The type information is needed by the Java backend.
            IntTag = int_tag_int(0),
            IntConst = int_tag_to_mlds_rval_const(VarType, MLDS_Type, IntTag),
            ConstRval = ml_const(IntConst)
        ;
            ConsTag = float_tag(Float),
            ConstRval = ml_const(mlconst_float(Float))
        ;
            ConsTag = string_tag(String),
            ConstRval = ml_const(mlconst_string(String))
        ;
            ConsTag = shared_local_tag(Ptag, Stag),
            ConstRval = ml_cast(MLDS_Type, ml_mkword(Ptag,
                ml_unop(mkbody, ml_const(mlconst_int(Stag)))))
        ;
            ConsTag = foreign_tag(ForeignLang, ForeignTag),
            ConstRval = ml_const(mlconst_foreign(ForeignLang, ForeignTag,
                MLDS_Type))
        ),
        expect(unify(ArgVars, []), $pred, "constant tag with args"),
        ConstGroundTerm = ml_ground_term(ConstRval, VarType, MLDS_Type),
        map.det_insert(Var, ConstGroundTerm, !GroundTermMap)
    ;
        ( ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "bad constant")
    ;
        ( ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ),
        (
            ArgVars = [ArgVar],
            map.det_remove(ArgVar, ArgGroundTerm, !GroundTermMap),
            ArgGroundTerm = ml_ground_term(ArgRval, _ArgType, MLDS_ArgType),
            ml_gen_box_const_rval(ModuleInfo, Context, MLDS_ArgType,
                aw_full_word, ArgRval, Rval0, !GlobalData),
            Rval = ml_cast_cons_tag(MLDS_Type, ConsTag, Rval0),
            GroundTerm = ml_ground_term(Rval, VarType, MLDS_Type),
            map.det_insert(Var, GroundTerm, !GroundTermMap)
        ;
            ( ArgVars = []
            ; ArgVars = [_, _ | _]
            ),
            unexpected($pred, "no_tag arity != 1")
        )
    ;
        % Lambda expressions cannot occur in from_ground_term_construct scopes
        % during code generation, because if they do occur there originally,
        % semantic analysis will change the scope reason to something else.
        ConsTag = closure_tag(_PredId, _ProcId, _EvalMethod),
        unexpected($pred, "pred_closure_tag")
    ;
        % Ordinary compound terms.
        % This code (loosely) follows the code of ml_gen_compound.
        (
            ConsTag = single_functor_tag,
            Ptag = 0,
            ExtraRvals = []
        ;
            ConsTag = unshared_tag(Ptag),
            ExtraRvals = []
        ;
            ConsTag = shared_remote_tag(Ptag, Stag, AddedBy),
            UsesConstructors = ml_target_uses_constructors(Target),
            (
                UsesConstructors = no,
                % XXX ARG_PACK
                expect(unify(AddedBy, sectag_added_by_unify), $pred,
                    "AddedBy != sectag_added_by_unify"),
                StagRval0 = ml_const(mlconst_int(Stag)),
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
        ml_gen_ground_term_conjunct_compound(ModuleInfo, Target, HighLevelData,
            VarTypes, Var, VarType, MLDS_Type, ConsId, ConsTag, Ptag,
            ExtraRvals, ArgVars, Context, !GlobalData, !GroundTermMap)
    ).

:- pred ml_gen_ground_term_conjunct_compound(module_info::in,
    mlds_target_lang::in, bool::in, vartypes::in,
    prog_var::in, mer_type::in, mlds_type::in, cons_id::in, cons_tag::in,
    int::in, list(mlds_rval)::in, list(prog_var)::in,
    prog_context::in, ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

ml_gen_ground_term_conjunct_compound(ModuleInfo, Target, HighLevelData,
        VarTypes, Var, VarType, MLDS_Type, ConsId, ConsTag, Ptag,
        ExtraRvals, ArgVars, Context, !GlobalData, !GroundTermMap) :-
    % This code (loosely) follows the code of ml_gen_new_object.

    % If the scope contains existentially typed constructions,
    % then polymorphism should have changed its scope_reason away from
    % from_ground_term_construct. Therefore the static struct we are
    % constructing should not need any extra type_info or typeclass_info args.
    cons_id_arg_types_and_widths(ModuleInfo, lookup_var_type_func(VarTypes),
        may_not_have_extra_args, VarType, ConsId, ArgVars, ArgVarsTypesWidths),
    (
        HighLevelData = yes,
        construct_ground_term_initializers_hld(ModuleInfo, Context,
            ArgVarsTypesWidths, ArgRvalsTypesWidths,
            !GlobalData, !GroundTermMap)
    ;
        HighLevelData = no,
        construct_ground_term_initializers_lld(ModuleInfo, Context,
            ArgVarsTypesWidths, ArgRvalsTypesWidths,
            !GlobalData, !GroundTermMap)
    ),
    UsesBaseClass = ml_tag_uses_base_class(ConsTag),
    construct_static_ground_term(ModuleInfo, Target, HighLevelData,
        Context, VarType, MLDS_Type, ordinary_cons_id(ConsId),
        UsesBaseClass, Ptag, ExtraRvals, ArgRvalsTypesWidths, GroundTerm,
        !GlobalData),
    map.det_insert(Var, GroundTerm, !GroundTermMap).

%---------------------------------------------------------------------------%

:- pred construct_ground_term_initializers_hld(module_info::in,
    prog_context::in, list(arg_var_type_and_width)::in,
    list(mlds_rval_type_and_width)::out,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

construct_ground_term_initializers_hld(_, _, [], [],
        !GlobalData, !GroundTermMap).
construct_ground_term_initializers_hld(ModuleInfo, Context,
        [ArgVarTypeWidth | ArgVarsTypesWidths],
        [RvalTypeWidth | RvalsTypesWidths], !GlobalData, !GroundTermMap) :-
    construct_ground_term_initializer_hld(ModuleInfo, Context,
        ArgVarTypeWidth, RvalTypeWidth, !GlobalData, !GroundTermMap),
    construct_ground_term_initializers_hld(ModuleInfo, Context,
        ArgVarsTypesWidths, RvalsTypesWidths, !GlobalData, !GroundTermMap).

:- pred construct_ground_term_initializer_hld(module_info::in,
    prog_context::in, arg_var_type_and_width::in,
    mlds_rval_type_and_width::out,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

construct_ground_term_initializer_hld(ModuleInfo, Context,
        ArgVarTypeWidth, RvalTypeWidth, !GlobalData, !GroundTermMap) :-
    ArgVarTypeWidth = arg_type_and_width(ArgVar, ConsArgType, ArgPosWidth),
    map.det_remove(ArgVar, ArgGroundTerm, !GroundTermMap),
    ArgGroundTerm = ml_ground_term(ArgRval0, ArgType, _MLDS_ArgType),
    ArgWidth = arg_pos_width_to_width_only(ArgPosWidth),
    ml_type_as_field(ModuleInfo, yes, ConsArgType, ArgWidth, BoxedArgType),
    ml_gen_box_or_unbox_const_rval_hld(ModuleInfo, Context,
        ArgType, BoxedArgType, ArgRval0, ArgRval, !GlobalData),
    RvalTypeWidth =
        rval_type_and_width(ArgRval, mlds_generic_type, ArgPosWidth, no).

:- pred construct_ground_term_initializers_lld(module_info::in,
    prog_context::in, list(arg_var_type_and_width)::in,
    list(mlds_rval_type_and_width)::out,
    ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

construct_ground_term_initializers_lld(_, _, [], [],
        !GlobalData, !GroundTermMap).
construct_ground_term_initializers_lld(ModuleInfo, Context,
        [ArgVarTypeWidth | ArgVarsTypesWidths],
        [RvalTypeWidth | RvalsTypesWidths], !GlobalData, !GroundTermMap) :-
    construct_ground_term_initializer_lld(ModuleInfo, Context,
        ArgVarTypeWidth, RvalTypeWidth, !GlobalData, !GroundTermMap),
    construct_ground_term_initializers_lld(ModuleInfo, Context,
        ArgVarsTypesWidths, RvalsTypesWidths, !GlobalData, !GroundTermMap).

:- pred construct_ground_term_initializer_lld(module_info::in,
    prog_context::in, arg_var_type_and_width::in,
    mlds_rval_type_and_width::out, ml_global_data::in, ml_global_data::out,
    ml_ground_term_map::in, ml_ground_term_map::out) is det.

construct_ground_term_initializer_lld(ModuleInfo, Context,
        ArgVarTypeWidth, RvalTypeWidth, !GlobalData, !GroundTermMap) :-
    ArgVarTypeWidth = arg_type_and_width(ArgVar, _ConsArgType, ArgPosWidth),
    map.det_remove(ArgVar, ArgGroundTerm, !GroundTermMap),
    ArgGroundTerm = ml_ground_term(ArgRval0, _ArgType, MLDS_ArgType),
    ArgWidth = arg_pos_width_to_width_only(ArgPosWidth),
    ml_gen_box_const_rval(ModuleInfo, Context, MLDS_ArgType, ArgWidth,
        ArgRval0, ArgRval, !GlobalData),
    RvalTypeWidth =
        rval_type_and_width(ArgRval, mlds_generic_type, ArgPosWidth, no).

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
    ml_gen_const_struct_tag(Info, ConstNum, Type, MLDS_Type, ConsId, ConsTag,
        Args, !ConstStructMap, !GlobalData).

:- pred ml_gen_const_struct_tag(ml_const_struct_info::in, int::in,
    mer_type::in, mlds_type::in, cons_id::in, cons_tag::in,
    list(const_struct_arg)::in,
    ml_const_struct_map::in, ml_const_struct_map::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_const_struct_tag(Info, ConstNum, Type, MLDS_Type, ConsId, ConsTag,
        Args, !ConstStructMap, !GlobalData) :-
    (
        ( ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ),
        (
            Args = [Arg],
            ml_gen_const_struct_arg(Info, !.ConstStructMap, Arg,
                apw_full(arg_only_offset(0), cell_offset(0)),
                ArgRvalTypeWidth, !GlobalData),
            ArgRvalTypeWidth = rval_type_and_width(ArgRval, _RvalMLDSType,
                _Width, _MaybePackedArgVar),
            Rval = ml_cast_cons_tag(MLDS_Type, ConsTag, ArgRval),
            GroundTerm = ml_ground_term(Rval, Type, MLDS_Type),
            map.det_insert(ConstNum, GroundTerm, !ConstStructMap)
        ;
            ( Args = []
            ; Args = [_, _ | _]
            ),
            unexpected($pred, "no_tag arity != 1")
        )
    ;
        % Ordinary compound terms.
        % This code (loosely) follows the code of ml_gen_compound.
        (
            ConsTag = single_functor_tag,
            Ptag = 0,
            ExtraRvals = []
        ;
            ConsTag = unshared_tag(Ptag),
            ExtraRvals = []
        ;
            ConsTag = shared_remote_tag(Ptag, Stag, AddedBy),
            Target = Info ^ mcsi_target,
            UsesConstructors = ml_target_uses_constructors(Target),
            (
                UsesConstructors = no,
                % XXX ARG_PACK
                expect(unify(AddedBy, sectag_added_by_unify), $pred,
                    "AddedBy != sectag_added_by_unify"),
                StagRval0 = ml_const(mlconst_int(Stag)),
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
        % These tags don't build heap cells.
        ( ConsTag = int_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = string_tag(_)
        ; ConsTag = dummy_tag
        ; ConsTag = shared_local_tag(_, _)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        % These tags should never occur in constant data.
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = tabling_info_tag(_, _)
        % These tags should never occur in constant data in this position.
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        % These tags should never occur in MLDS grades.
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ),
        unexpected($pred, "unexpected tag")
    ).

:- pred ml_gen_const_static_compound(ml_const_struct_info::in,
    int::in, mer_type::in, mlds_type::in, cons_id::in, cons_tag::in,
    int::in, list(mlds_rval)::in, list(const_struct_arg)::in,
    ml_const_struct_map::in, ml_const_struct_map::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_const_static_compound(Info, ConstNum, VarType, MLDS_Type, ConsId,
        ConsTag, Ptag, ExtraRvals, Args, !ConstStructMap, !GlobalData) :-
    % This code (loosely) follows the code of
    % ml_gen_ground_term_conjunct_compound.

    Target = Info ^ mcsi_target,
    ModuleInfo = Info ^ mcsi_module_info,
    % It is ok to specify the wrong type for Args by passing AllTypesVoid,
    % because all uses of ArgsTypesWidths later on ignore the types inside it.
    AllTypesVoid = (func(_Arg) = void_type),
    % XXX TYPE_REPN The may_not_have_extra_args preserves old behavior,
    % but may be a bug. It depends on whether we ever put into the
    % const struct db terms that need extra args.
    cons_id_arg_types_and_widths(ModuleInfo, AllTypesVoid,
        may_not_have_extra_args, VarType, ConsId, Args, ArgsTypesWidths),
    HighLevelData = Info ^ mcsi_high_level_data,
    ( if
        (
            HighLevelData = no
        ;
            HighLevelData = yes,
            Target = ml_target_java
        )
    then
        true
    else
        unexpected($pred,
            "Constant structures are not supported for this grade")
    ),
    ml_gen_const_struct_args(Info, !.ConstStructMap,
        ArgsTypesWidths, RvalsTypesWidths, !GlobalData),
    UsesBaseClass = ml_tag_uses_base_class(ConsTag),
    construct_static_ground_term(ModuleInfo, Target, HighLevelData,
        term.context_init, VarType, MLDS_Type, ordinary_cons_id(ConsId),
        UsesBaseClass, Ptag, ExtraRvals, RvalsTypesWidths, GroundTerm,
        !GlobalData),
    map.det_insert(ConstNum, GroundTerm, !ConstStructMap).

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
        ConsTag = shared_local_tag(Ptag, Stag),
        Rval = ml_cast(MLDS_Type, ml_mkword(Ptag,
            ml_unop(mkbody, ml_const(mlconst_int(Stag)))))
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
        ; ConsTag = no_tag
        ; ConsTag = direct_arg_tag(_)
        ; ConsTag = single_functor_tag
        ; ConsTag = unshared_tag(_)
        ; ConsTag = shared_remote_tag(_, _, _)
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

:- type cons_id_or_closure
    --->    ordinary_cons_id(cons_id)
    ;       closure_object(int).            % The initial offset of the args.

:- type may_have_extra_args
    --->    may_not_have_extra_args
    ;       may_have_extra_args.

:- type arg_type_and_width(Arg)
    --->    arg_type_and_width(Arg, mer_type, arg_pos_width).

:- type arg_var_type_and_width == arg_type_and_width(prog_var).
:- type arg_const_type_and_width == arg_type_and_width(const_struct_arg).

:- pred maybe_cons_id_arg_types_and_widths(ml_gen_info::in,
    mer_type::in, cons_id_or_closure::in, list(prog_var)::in,
    list(arg_var_type_and_width)::out) is det.

maybe_cons_id_arg_types_and_widths(Info, VarType, ConsIdOrClosure, ArgVars,
        ArgVarsTypesWidths) :-
    (
        ConsIdOrClosure = ordinary_cons_id(ConsId),
        ml_gen_info_get_module_info(Info, ModuleInfo),
        ml_gen_info_get_var_types(Info, VarTypes),
        cons_id_arg_types_and_widths(ModuleInfo,
            lookup_var_type_func(VarTypes), may_have_extra_args,
            VarType, ConsId, ArgVars, ArgVarsTypesWidths)
    ;
        ConsIdOrClosure = closure_object(InitOffset),
        % It is a closure. In this case, the arguments are all boxed.
        specified_arg_types_and_consecutive_full_words(ml_make_boxed_type,
            InitOffset, ArgVars, ArgVarsTypesWidths)
    ).

:- type arg_to_type(Arg) == (func(Arg) = mer_type).

    % cons_id_arg_types_and_widths(ModuleInfo, ArgToType, MayHaveExtraArgs,
    %   VarType, ConsId, Args, ArgTypesWidths):
    %
    % We are constructing a structure (either on the heap or in static memory).
    % VarType is the type of the whole structure, ConsId is the functor,
    % and Args specifies the functor's visible arguments. If MayHaveExtraArgs
    % is may_have_extra_args, then the visible arguments may be prefaced
    % by extra type_info and/or typeclass_info arguments added to describe
    % some existentially typed visible arguments. Both Args and ArgsTypesWidths
    % will include these extra arguments.
    %
    % The Args will usually be variables, but will be const_struct_args
    % in some cases.
    %
    % The job of this predicate is to associate each argument with
    % its type as an argument (which, due to type instantiation and/or boxing,
    % may be different from the type of the corresponding constructor argument)
    % and with its width.
    %
    % In some circumstances, the types of the non-extra arguments are taken
    % from applyin ArgToType to the given argument. One of our callers
    % does not need the types inside ArgsTypesWidths; such callers can supply
    % dummy values for ArgToTypes, if they also pass may_not_have_extra_args.
    %
:- pred cons_id_arg_types_and_widths(module_info, arg_to_type(Arg),
    may_have_extra_args, mer_type, cons_id, list(Arg),
    list(arg_type_and_width(Arg))).
:- mode cons_id_arg_types_and_widths(in, in,
    in(bound(may_have_extra_args)), in, in, in, out) is det.
:- mode cons_id_arg_types_and_widths(in, in,
    in(bound(may_not_have_extra_args)), in, in, in, out) is det.

cons_id_arg_types_and_widths(ModuleInfo, ArgToType, MayHaveExtraArgs,
        VarType, ConsId, Args, ArgsTypesWidths) :-
    ( if
        ConsId = cons(_, _, _),
        not is_introduced_type_info_type(VarType)
    then
        ( if get_cons_repn_defn(ModuleInfo, ConsId, ConsRepnDefn) then
            ConsArgRepns = ConsRepnDefn ^ cr_args,
            NumExtraArgs = list.length(Args) - list.length(ConsArgRepns),
            ( if NumExtraArgs = 0 then
                zip_args_types_widths(Args, ConsArgRepns, ArgsTypesWidths)
            else
                expect(unify(MayHaveExtraArgs, may_have_extra_args), $pred,
                    "extra args in static struct"),
                % There may have been additional types inserted to hold the
                % type_infos and type_class_infos for existentially quantified
                % types. We can get the type of these from VarTypes.
                det_split_list(NumExtraArgs, Args, ExtraArgs, NonExtraArgs),
                ( if
                    ConsRepnDefn ^ cr_tag = shared_remote_tag(_, _, AddedBy),
                    AddedBy = sectag_added_by_unify
                then
                    InitOffset = 1
                else
                    InitOffset = 0
                ),
                lookup_type_and_allocate_consecutive_full_words(ArgToType,
                    InitOffset, ExtraArgs, ExtraArgsTypesWidths),
                zip_args_types_widths(NonExtraArgs, ConsArgRepns,
                    NonExtraArgsTypesWidths),
                ArgsTypesWidths =
                    ExtraArgsTypesWidths ++ NonExtraArgsTypesWidths
            )
        else if
            % If we didn't find a constructor definition, maybe that is
            % because this type was a built-in type.
            type_is_tuple(VarType, _)
        then
            % In this case, the argument types are all fresh variables.
            % Note that we do not need to worry about using the right varset
            % here, since all we really care about at this point is whether
            % something is a type variable or not, not which type variable
            % it is.
            InitOffset = 0,
            specified_arg_types_and_consecutive_full_words(ml_make_boxed_type,
                InitOffset, Args, ArgsTypesWidths)
        else
            % The only builtin types that can allocate structures
            % are tuples and the RTTI-related types. Both should have been
            % handled by code above.
            unexpected($pred, "get_cons_defn failed")
        )
    else
        % For cases when ConsId \= hlds_cons(_, _) and it is not a tuple,
        % as can happen e.g. for closures and type_infos, we assume that
        % the arguments all have the right type already, and that there
        % is no secondary tag.
        InitOffset = 0,
        lookup_type_and_allocate_consecutive_full_words(ArgToType,
            InitOffset, Args, ArgsTypesWidths)
    ).

:- pred zip_args_types_widths(list(Arg)::in,
    list(constructor_arg_repn)::in, list(arg_type_and_width(Arg))::out) is det.

zip_args_types_widths([], [], []).
zip_args_types_widths([], [_ | _], _) :-
    unexpected($pred, "length mismatch").
zip_args_types_widths([_ | _], [], _) :-
    unexpected($pred, "length mismatch").
zip_args_types_widths([Arg | Args], [ConsArgRepn | ConsArgRepns],
        [ArgTypeWidth | ArgsTypesWidth]) :-
    ArgTypeWidth = arg_type_and_width(Arg,
        ConsArgRepn ^ car_type, ConsArgRepn ^ car_pos_width),
    zip_args_types_widths(Args, ConsArgRepns, ArgsTypesWidth).

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
    cons_id_or_closure::in, tag_uses_base_class::in, int::in,
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
    ( if Ptag = 0 then
        TaggedRval = ConstDataAddrRval
    else
        TaggedRval = ml_mkword(Ptag, ConstDataAddrRval)
    ),
    Rval = ml_cast(MLDS_Type, TaggedRval),
    GroundTerm = ml_ground_term(Rval, VarType, MLDS_Type).

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
        PosWidth = apw_partial_first(_, _, _, _, Fill),
        maybe_shift_and_accumulate_or_rval(Rval, arg_shift(0), Fill,
            [], RevOrRvals0),
        ml_pack_into_one_word(RvalsTypesWidths, LeftOverRvalsTypesWidths,
            RevOrRvals0, OrAllRval, [], _, no, _),
        HeadInit = init_obj(OrAllRval),
        ml_pack_ground_term_args_into_word_inits(LeftOverRvalsTypesWidths,
            TailInits),
        Inits = [HeadInit | TailInits]
    ;
        PosWidth = apw_partial_shifted(_, _, _, _, _, _),
        % There should be an apw_partial_first argument first.
        unexpected($pred, "apw_partial_shifted")
    ;
        PosWidth = apw_none_shifted(_, _),
        % There should be an apw_partial_first argument first.
        unexpected($pred, "apw_none_shifted")
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
        PosWidth = apw_partial_first(AOOffset, CellOffset, _, _, Fill),
        maybe_shift_and_accumulate_or_rval(Rval, arg_shift(0), Fill,
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
        PosWidth = apw_partial_shifted(_, _, _, _, _, _),
        % There should be an apw_*_first argument first.
        unexpected($pred, "apw_partial_shifted")
    ;
        PosWidth = apw_none_shifted(_, _),
        % There should be an apw_*_first argument first.
        unexpected($pred, "apw_none_shifted")
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
        or_packed_rvals(HeadOrRval, TailOrRvals, OrAllRval),
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
        ; PosWidth = apw_partial_first(_, _, _, _, _)
        ; PosWidth = apw_none_nowhere
        ),
        LeftOverRvalsTypesWidths = [RvalTypeWidth | RvalsTypesWidths]
    ;
        (
            PosWidth = apw_partial_shifted(_, _, Shift, _, _, Fill),
            maybe_shift_and_accumulate_or_rval(Rval, Shift, Fill, !RevOrRvals),
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

    % OR together the given rvals.
    %
    % We currently do this a linear fashion, starting at the rightmost
    % arguments, and moving towards the left.
    %
    % We should explore whether other strategies, such as balanced trees,
    % (or rather, trees that are as balanced as possible) would work better.
    %
:- pred or_packed_rvals(mlds_rval::in, list(mlds_rval)::in,
    mlds_rval::out) is det.

or_packed_rvals(HeadRval, TailRvals, OrAllRval) :-
    (
        TailRvals = [],
        OrAllRval = HeadRval
    ;
        TailRvals = [HeadTailRval | TailTailRvals],
        or_packed_rvals(HeadTailRval, TailTailRvals, TailOrAllRval),
        OrAllRval = ml_bitwise_or(HeadRval, TailOrAllRval)
    ).

%---------------------------------------------------------------------------%

:- pred maybe_shift_and_accumulate_or_rval(mlds_rval::in, arg_shift::in,
    fill_kind::in, list(mlds_rval)::in, list(mlds_rval)::out) is det.

maybe_shift_and_accumulate_or_rval(Rval, Shift, Fill, !RevOrRvals) :-
    Shift = arg_shift(ShiftInt),
    ml_cast_to_unsigned_without_sign_extend(Fill, Rval, CastRval),
    ( if
        Rval = ml_const(RvalConst),
        ( RvalConst = mlconst_null(_)
        ; RvalConst = mlconst_int(0)
        )
    then
        % We may get nulls from unfilled fields, and zeros from constant
        % fields. Since OR with zero is a noop, do not include them
        % in the list of rvals to be OR-ed later.
        true
    else
        ( if ShiftInt = 0 then
            ShiftedRval = CastRval
        else if CastRval = ml_box(Type, SubRval) then
            ShiftedSubRval = ml_binop(unchecked_left_shift(int_type_uint),
                SubRval, ml_const(mlconst_int(ShiftInt))),
            ShiftedRval = ml_box(Type, ShiftedSubRval)
        else
            ShiftedRval = ml_binop(unchecked_left_shift(int_type_uint),
                CastRval, ml_const(mlconst_int(ShiftInt)))
        ),
        !:RevOrRvals = [ShiftedRval | !.RevOrRvals]
    ).

:- func ml_lshift(mlds_rval, arg_shift, fill_kind) = mlds_rval.

ml_lshift(Rval, Shift, Fill) = ShiftedRval :-
    Shift = arg_shift(ShiftInt),
    ml_cast_to_unsigned_without_sign_extend(Fill, Rval, CastRval),
    ( if Rval = ml_const(mlconst_null(_)) then
        % We may get nulls from unfilled fields. Replace them with zeroes
        % so we don't get type errors from the C compiler.
        ShiftedRval = ml_const(mlconst_int(0))
    else if Rval = ml_const(mlconst_int(0)) then
        % Shifting a zero by any amount is a noop.
        ShiftedRval = CastRval
    else if ShiftInt = 0 then
        % Shifting anything by zero bits is a noop.
        ShiftedRval = CastRval
    else
        ( if CastRval = ml_box(Type, SubRval) then
            ShiftedSubRval = ml_binop(unchecked_left_shift(int_type_uint),
                SubRval, ml_const(mlconst_int(ShiftInt))),
            ShiftedRval = ml_box(Type, ShiftedSubRval)
        else
            ShiftedRval = ml_binop(unchecked_left_shift(int_type_uint),
                CastRval, ml_const(mlconst_int(ShiftInt)))
        )
    ).

:- func ml_rshift(mlds_rval, arg_shift) = mlds_rval.

ml_rshift(Rval, Shift) = ShiftedRval :-
    % While ml_lshift may be called on a boxed Rval, ml_rshift will never
    % be called that way, which is why we don't handle that as a special case.
    Shift = arg_shift(ShiftInt),
    ( if Rval = ml_const(mlconst_int(0)) then
        % Shifting a zero by any amount is a noop.
        ShiftedRval = Rval
    else if ShiftInt = 0 then
        % Shifting anything by zero bits is a noop.
        ShiftedRval = Rval
    else
        ShiftedRval = ml_binop(unchecked_right_shift(int_type_uint),
            Rval, ml_const(mlconst_int(ShiftInt)))
    ).

:- func ml_bitwise_or(mlds_rval, mlds_rval) = mlds_rval.

ml_bitwise_or(RvalA, RvalB) = Rval :-
    some [!MaybeType] (
        !:MaybeType = no,
        ( if RvalA = ml_box(TypeA, UnboxRvalA0) then
            UnboxRvalA = UnboxRvalA0,
            !:MaybeType = yes(TypeA)
        else
            UnboxRvalA = RvalA
        ),
        ( if RvalB = ml_box(TypeB, UnboxRvalB0) then
            UnboxRvalB = UnboxRvalB0,
            !:MaybeType = yes(TypeB)
        else
            UnboxRvalB = RvalB
        ),
        UnboxRval = ml_binop(bitwise_or(int_type_uint),
            UnboxRvalA, UnboxRvalB),
        (
            !.MaybeType = yes(BoxType),
            Rval = ml_box(BoxType, UnboxRval)
        ;
            !.MaybeType = no,
            Rval = UnboxRval
        )
    ).

:- func ml_bitwise_and(mlds_rval, int) = mlds_rval.

ml_bitwise_and(Rval, Mask) =
    ml_binop(bitwise_and(int_type_uint), Rval, ml_const(mlconst_int(Mask))).

:- pred ml_cast_to_unsigned_without_sign_extend(fill_kind::in,
    mlds_rval::in, mlds_rval::out) is det.

ml_cast_to_unsigned_without_sign_extend(Fill, Rval0, Rval) :-
    (
        ( Fill = fill_enum
        ; Fill = fill_uint8
        ; Fill = fill_uint16
        ; Fill = fill_uint32
        ),
        Rval1 = Rval0
    ;
        (
            Fill = fill_int8,
            FromMLDSType = mlds_int_type_int8,
            ToMLDSType = mlds_int_type_uint8
        ;
            Fill = fill_int16,
            FromMLDSType = mlds_int_type_int16,
            ToMLDSType = mlds_int_type_uint16
        ;
            Fill = fill_int32,
            FromMLDSType = mlds_int_type_int32,
            ToMLDSType = mlds_int_type_uint32
        ),
        % XXX ARG_PACK It would be better if instead of undoing the boxing
        % here, we could *avoid* boxing sub-word-sized arguments. However,
        % that would require treating sub-word-sized arguments differently
        % from full- or double-word-sized arguments, which is a more involved
        % change.
        ( if Rval0 = ml_box(FromMLDSType, SubRval) then
            % We can't apply this cast to Rval0 without getting a gcc warning:
            % "warning: cast from pointer to integer of different size
            % [-Wpointer-to-int-cast]".
            Rval1 = ml_cast(ToMLDSType, SubRval)
        else
            Rval1 = ml_cast(ToMLDSType, Rval0)
        )
    ),
    UnsignedMLDSType = mlds_int_type_uint,
    Rval = ml_cast(UnsignedMLDSType, Rval1).

%---------------------------------------------------------------------------%

:- pred ml_next_field_offset(constructor_arg_repn::in,
    assoc_list(prog_var, constructor_arg_repn)::in,
    field_offset::in, field_offset::out) is det.

ml_next_field_offset(_, [], Offset, Offset).
ml_next_field_offset(CurArgRepn, [NextArg | _], PrevOffset, NextOffset) :-
    CurArgRepn = ctor_arg_repn(_, _, CurWidth, _),
    NextArg = _ - ctor_arg_repn(_, _, NextWidth, _),
    % XXX ARG_PACK We *could* use the same algorithm for incrementing Offset
    % here as we use elsewhere, but better still, we should not need this
    % predicate once we switch over to using the offsets of constructor
    % arguments' representations exclusively.
    (
        CurWidth = apw_full(_, _),
        (
            ( NextWidth = apw_full(_, _)
            ; NextWidth = apw_double(_, _, _)
            ; NextWidth = apw_partial_first(_, _, _, _, _)
            ; NextWidth = apw_none_nowhere
            ),
            PrevOffset = offset(Int),
            NextOffset = offset(Int + 1)
        ;
            ( NextWidth = apw_partial_shifted(_, _, _, _, _, _)
            ; NextWidth = apw_none_shifted(_, _)
            ),
            unexpected($pred, "shifted follows full")
        )
    ;
        CurWidth = apw_double(_, _, _),
        (
            ( NextWidth = apw_full(_, _)
            ; NextWidth = apw_double(_, _, _)
            ; NextWidth = apw_partial_first(_, _, _, _, _)
            ; NextWidth = apw_none_nowhere
            ),
            PrevOffset = offset(Int),
            NextOffset = offset(Int + 2)
        ;
            ( NextWidth = apw_partial_shifted(_, _, _, _, _, _)
            ; NextWidth = apw_none_shifted(_, _)
            ),
            unexpected($pred, "shifted follows double")
        )
    ;
        CurWidth = apw_partial_first(_, _, _, _, _),
        (
            ( NextWidth = apw_partial_shifted(_, _, _, _, _, _)
            ; NextWidth = apw_none_shifted(_, _)
            ),
            NextOffset = PrevOffset
        ;
            ( NextWidth = apw_full(_, _)
            ; NextWidth = apw_double(_, _, _)
            ; NextWidth = apw_partial_first(_, _, _, _, _)
            ; NextWidth = apw_none_nowhere
            ),
            unexpected($pred,
                "partial_first not followed by partial_shifted")
        )
    ;
        ( CurWidth = apw_partial_shifted(_, _, _, _, _, _)
        ; CurWidth = apw_none_shifted(_, _)
        ),
        (
            ( NextWidth = apw_full(_, _)
            ; NextWidth = apw_double(_, _, _)
            ; NextWidth = apw_partial_first(_, _, _, _, _)
            ; NextWidth = apw_none_nowhere
            ),
            PrevOffset = offset(Int),
            NextOffset = offset(Int + 1)
        ;
            ( NextWidth = apw_partial_shifted(_, _, _, _, _, _)
            ; NextWidth = apw_none_shifted(_, _)
            ),
            NextOffset = PrevOffset
        )
    ;
        CurWidth = apw_none_nowhere,
        NextOffset = PrevOffset
    ).

%---------------------------------------------------------------------------%
%
% Utility predicates.
%

    % Convert a cons_id for a given type to a cons_tag.
    %
:- pred ml_cons_id_to_tag(ml_gen_info::in, cons_id::in, cons_tag::out) is det.

ml_cons_id_to_tag(Info, ConsId, ConsTag) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId).

:- pred is_apw_full(arg_pos_width::in) is semidet.

is_apw_full(apw_full(_, _)).

:- pred allocate_consecutive_full_word_ctor_arg_repns_boxed(int::in,
    list(prog_var)::in,
    assoc_list(prog_var, constructor_arg_repn)::out) is det.

allocate_consecutive_full_word_ctor_arg_repns_boxed(_, [], []).
allocate_consecutive_full_word_ctor_arg_repns_boxed(CurOffset,
        [Var | Vars], [VarArgRepn | VarArgRepns]) :-
    Type = ml_make_boxed_type,
    ArgPosWidth = apw_full(arg_only_offset(CurOffset), cell_offset(CurOffset)),
    ArgRepn = ctor_arg_repn(no, Type, ArgPosWidth, term.context_init),
    VarArgRepn = Var - ArgRepn,
    allocate_consecutive_full_word_ctor_arg_repns_boxed(CurOffset + 1,
        Vars, VarArgRepns).

:- pred allocate_consecutive_full_word_ctor_arg_repns_lookup(ml_gen_info::in,
    int::in, list(prog_var)::in,
    assoc_list(prog_var, constructor_arg_repn)::out) is det.

allocate_consecutive_full_word_ctor_arg_repns_lookup(_, _, [], []).
allocate_consecutive_full_word_ctor_arg_repns_lookup(Info, CurOffset,
        [Var | Vars], [VarArgRepn | VarArgRepns]) :-
    ml_variable_type(Info, Var, Type),
    ArgPosWidth = apw_full(arg_only_offset(CurOffset), cell_offset(CurOffset)),
    ArgRepn = ctor_arg_repn(no, Type, ArgPosWidth, term.context_init),
    VarArgRepn = Var - ArgRepn,
    allocate_consecutive_full_word_ctor_arg_repns_lookup(Info, CurOffset + 1,
        Vars, VarArgRepns).

:- pred lookup_type_and_allocate_consecutive_full_words(arg_to_type(Arg)::in,
    int::in, list(Arg)::in, list(arg_type_and_width(Arg))::out) is det.

lookup_type_and_allocate_consecutive_full_words(_, _, [], []).
lookup_type_and_allocate_consecutive_full_words(ArgToType, CurOffset,
        [Arg | Args], [ArgTypeWidth | ArgsTypesWidths]) :-
    PosWidth = apw_full(arg_only_offset(CurOffset), cell_offset(CurOffset)),
    ArgTypeWidth = arg_type_and_width(Arg, ArgToType(Arg), PosWidth),
    lookup_type_and_allocate_consecutive_full_words(ArgToType, CurOffset + 1,
        Args, ArgsTypesWidths).

:- pred specified_arg_types_and_consecutive_full_words(mer_type::in, int::in,
    list(Arg)::in, list(arg_type_and_width(Arg))::out) is det.

specified_arg_types_and_consecutive_full_words(_, _, [], []).
specified_arg_types_and_consecutive_full_words(Type, CurOffset,
        [Arg | Args], [ArgTypeWidth | ArgsTypesWidths]) :-
    PosWidth = apw_full(arg_only_offset(CurOffset), cell_offset(CurOffset)),
    ArgTypeWidth = arg_type_and_width(Arg, Type, PosWidth),
    specified_arg_types_and_consecutive_full_words(Type, CurOffset + 1,
        Args, ArgsTypesWidths).

:- type assign_dir
    --->    assign_nondummy_left
    ;       assign_nondummy_right
    ;       assign_nondummy_unused
    ;       assign_dummy.

:- pred compute_assign_direction(module_info::in, unify_mode::in,
    mer_type::in, mer_type::in, assign_dir::out) is det.
:- pragma inline(compute_assign_direction/5).

compute_assign_direction(ModuleInfo, ArgMode, ArgType, FieldType, Dir) :-
    ArgMode = unify_modes_lhs_rhs(LeftFromToInsts, RightFromToInsts),
    from_to_insts_to_top_functor_mode(ModuleInfo, LeftFromToInsts, ArgType,
        LeftTopMode),
    from_to_insts_to_top_functor_mode(ModuleInfo, RightFromToInsts, ArgType,
        RightTopMode),
    ( if
        is_either_type_a_dummy(ModuleInfo, ArgType, FieldType) =
            at_least_one_is_dummy_type
    then
        Dir = assign_dummy
    else
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
                Dir = assign_nondummy_right
            ;
                RightTopMode = top_unused,
                unexpected($pred, "some strange unify")
            )
        ;
            LeftTopMode = top_out,
            (
                RightTopMode = top_in,
                % Output - input: it is an assignment to the LHS.
                Dir = assign_nondummy_left
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
                Dir = assign_nondummy_unused
            ;
                ( RightTopMode = top_in
                ; RightTopMode = top_out
                ),
                unexpected($pred, "some strange unify")
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_unify_gen.
%---------------------------------------------------------------------------%
