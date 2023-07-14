%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009,2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_accurate_gc.m.
% Main author: fjh.
%
% This module is part of the MLDS code generator.
% It generates the data and code required for accurate gc.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_accurate_gc.
:- interface.

:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

%---------------------------------------------------------------------------%
%
% Code to handle accurate GC.
%

    % ml_gen_gc_statement(Var, Type, Context, Code):
    %
    % If accurate GC is enabled, and the specified variable might contain
    % pointers, generate code to call "private_builtin.gc_trace" to trace
    % the variable.
    %
:- pred ml_gen_gc_statement(mlds_local_var_name::in, mer_type::in,
    prog_context::in, mlds_gc_statement::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % ml_gen_gc_statement_poly(Var, DeclType, ActualType, Context, Code):
    %
    % This is the same as ml_gen_gc_statement, except that it takes two
    % type arguments, rather than one. The first (DeclType) is the type that
    % the variable was declared with, while the second (ActualType) is that
    % type that the variable is known to have. This is used to generate GC
    % tracing code for the temporaries variables used when calling procedures
    % with polymorphically-typed output arguments. In that case, DeclType
    % may be a type variable from the callee's type declaration, but ActualType
    % will be the type from the caller.
    %
    % We can't just use DeclType to generate the GC trace code, because
    % there is no way to compute the type_info for type variables that
    % come from the callee rather than the current procedure. And we can't
    % just use ActualType, since DeclType may contain pointers even when
    % ActualType doesn't (e.g. because DeclType may be a boxed float).
    % So we need to pass both.
    %
:- pred ml_gen_gc_statement_poly(mlds_local_var_name::in,
    mer_type::in, mer_type::in, prog_context::in,
    mlds_gc_statement::out, ml_gen_info::in, ml_gen_info::out) is det.

    % ml_gen_gc_statement_with_typeinfo(Var, DeclType, TypeInfoRval,
    %   Context, Code):
    %
    % This is the same as ml_gen_gc_statement_poly, except that rather
    % than passing ActualType, the caller constructs the typeinfo itself,
    % and just passes the rval for it to this routine.
    %
    % This is used by ml_closure_gen.m to generate GC tracing code
    % for the local variables in closure wrapper functions.
    %
:- pred ml_gen_gc_statement_with_typeinfo(mlds_local_var_name::in,
    mer_type::in, mlds_rval::in, prog_context::in,
    mlds_gc_statement::out, ml_gen_info::in, ml_gen_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.polymorphism_type_info.
:- import_module hlds.
:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%
%
% Code to handle accurate GC.
%

ml_gen_gc_statement(VarName, Type, Context, GCStmt, !Info) :-
    ml_gen_gc_statement_poly(VarName, Type, Type, Context, GCStmt, !Info).

ml_gen_gc_statement_poly(VarName, DeclType, ActualType, Context, GCStmt,
        !Info) :-
    ml_gen_info_get_gc(!.Info, GC),
    ( if GC = gc_accurate then
        HowToGetTypeInfo = construct_from_type(ActualType),
        ml_do_gen_gc_statement(VarName, DeclType, HowToGetTypeInfo, Context,
            GCStmt, !Info)
    else
        GCStmt = gc_no_stmt
    ).

ml_gen_gc_statement_with_typeinfo(VarName, DeclType, TypeInfoRval, Context,
        GCStmt, !Info) :-
    ml_gen_info_get_gc(!.Info, GC),
    ( if GC = gc_accurate then
        HowToGetTypeInfo = already_provided(TypeInfoRval),
        ml_do_gen_gc_statement(VarName, DeclType, HowToGetTypeInfo, Context,
            GCStmt, !Info)
    else
        GCStmt = gc_no_stmt
    ).

:- type how_to_get_type_info
    --->    construct_from_type(mer_type)
    ;       already_provided(mlds_rval).

:- pred ml_do_gen_gc_statement(mlds_local_var_name::in, mer_type::in,
    how_to_get_type_info::in, prog_context::in,
    mlds_gc_statement::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_do_gen_gc_statement(VarName, DeclType, HowToGetTypeInfo, Context, GCStmt,
        !Info) :-
    ( if
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        MLDS_DeclType = mercury_type_to_mlds_type(ModuleInfo, DeclType),
        ml_type_might_contain_pointers_for_gc(MLDS_DeclType) = yes,
        % Don't generate GC tracing code in no_type_info_builtins.
        ml_gen_info_get_pred_proc_id(!.Info, proc(PredId, _ProcId)),
        predicate_id(ModuleInfo, PredId, PredModule, PredName, PredFormArity),
        PredFormArity = pred_form_arity(PredFormArityInt),
        not no_type_info_builtin(PredModule, PredName, PredFormArityInt)
    then
        (
            HowToGetTypeInfo = construct_from_type(ActualType0),
            % We need to handle type_info/1 and typeclass_info/1
            % types specially, to avoid infinite recursion here...
            ( if trace_type_info_type(ActualType0, ActualType1) then
                ActualType = ActualType1
            else
                ActualType = ActualType0
            ),
            ml_gen_gc_trace_code(VarName, DeclType, ActualType,
                Context, GC_TraceCode, !Info)
        ;
            HowToGetTypeInfo = already_provided(TypeInfoRval),
            ml_gen_trace_var(!.Info, VarName, DeclType, TypeInfoRval,
                Context, GC_TraceCode)
        ),
        GCStmt = gc_trace_code(GC_TraceCode)
    else
        GCStmt = gc_no_stmt
    ).

    % Return `yes' if the type needs to be traced by the accurate garbage
    % collector, i.e. if it might contain pointers.
    %
    % Any type for which we return `yes' here must be word-sized, because
    % we will call private_builtin.gc_trace with its address, and that
    % procedure assumes that its argument is an `MR_Word *'.
    %
    % For floats, we can (and must) return `no' even though they might
    % get boxed in some circumstances, because if they are boxed then they will
    % be represented as mlds_generic_type.
    %
    % The only pointers in type_ctor_infos and base_typeclass_infos are
    % to static code and/or static data, which do not need to be traced.
    %
:- func ml_type_might_contain_pointers_for_gc(mlds_type) = bool.

ml_type_might_contain_pointers_for_gc(Type) = MightContainPointers :-
    (
        Type = mercury_nb_type(_, TypeCategory),
        MightContainPointers =
            ml_type_category_might_contain_pointers(TypeCategory)
    ;
        ( Type = mlds_mercury_array_type(_)
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_class_type(_)
        ; Type = mlds_env_type(_)
        ; Type = mlds_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ; Type = mlds_type_info_type
        ; Type = mlds_pseudo_type_info_type
        ; Type = mlds_rtti_type(_)
        ; Type = mlds_unknown_type
        ),
        MightContainPointers = yes
    ;
        ( Type = mlds_builtin_type_int(_)
        ; Type = mlds_builtin_type_float
        ; Type = mlds_builtin_type_string
        ; Type = mlds_builtin_type_char
        ; Type = mlds_native_bool_type
        ; Type = mlds_foreign_type(_)
        % We assume that foreign types are not allowed to contain pointers
        % to the Mercury heap.  XXX is this requirement too strict?
        ; Type = mlds_enum_class_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_tabling_type(_)
        % Values of mlds_tabling_type types may contain pointers, but
        % they won't exist if we are using accurate GC.
        ),
        MightContainPointers = no
    ).

:- func ml_type_category_might_contain_pointers(type_ctor_category) = bool.

ml_type_category_might_contain_pointers(CtorCat) = MayContainPointers :-
    (
        ( CtorCat = ctor_cat_builtin(cat_builtin_int(_))
        ; CtorCat = ctor_cat_builtin(cat_builtin_char)
        ; CtorCat = ctor_cat_builtin(cat_builtin_float)
        ; CtorCat = ctor_cat_builtin_dummy
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_system(cat_system_type_ctor_info)
        ; CtorCat = ctor_cat_system(cat_system_base_typeclass_info)
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ; CtorCat = ctor_cat_user(cat_user_abstract_dummy)
        ),
        MayContainPointers = no
    ;
        ( CtorCat = ctor_cat_builtin(cat_builtin_string)
        ; CtorCat = ctor_cat_system(cat_system_type_info)
        ; CtorCat = ctor_cat_system(cat_system_typeclass_info)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_variable
        ; CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_user(cat_user_abstract_notag)
        ; CtorCat = ctor_cat_user(cat_user_general)
        ),
        MayContainPointers = yes
    ).

    % trace_type_info_type(Type, RealType):
    %
    % Succeed iff Type is a type_info-related type which needs to be copied
    % as if it were some other type, binding RealType to that other type.
    %
:- pred trace_type_info_type(mer_type::in, mer_type::out) is semidet.

trace_type_info_type(Type, RealType) :-
    Type = defined_type(TypeName, _, _),
    TypeName = qualified(PrivateBuiltin, Name),
    PrivateBuiltin = mercury_private_builtin_module,
    ( Name = "type_info", RealType = sample_type_info_type
    ; Name = "type_ctor_info", RealType = c_pointer_type
    ; Name = "typeclass_info", RealType = sample_typeclass_info_type
    ; Name = "base_typeclass_info", RealType = c_pointer_type
    ; Name = "zero_type_info", RealType = sample_type_info_type
    ; Name = "zero_type_ctor_info", RealType = c_pointer_type
    ; Name = "zero_typeclass_info", RealType = sample_typeclass_info_type
    ; Name = "zero_base_typeclass_info", RealType = c_pointer_type
    ).

    % Generate code to call to `private_builtin.gc_trace'
    % to trace the specified variable.
    %
:- pred ml_gen_gc_trace_code(mlds_local_var_name::in, mer_type::in,
    mer_type::in, prog_context::in, mlds_stmt::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_gc_trace_code(VarName, DeclType, ActualType, Context, GC_TraceCode,
        !Info) :-
    % Build HLDS code to construct the type_info for this type.
    ml_gen_make_type_info_var(ActualType, Context,
        TypeInfoVar, HLDS_TypeInfoGoals, !Info),
    NonLocalsList = list.map(
        (func(hlds_goal(_GX, GI)) = goal_info_get_nonlocals(GI)),
        HLDS_TypeInfoGoals),
    NonLocals = set_of_var.union_list(NonLocalsList),
    InstMapDelta = instmap_delta_bind_var(TypeInfoVar),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_impure,
        GoalInfo),
    conj_list_to_goal(HLDS_TypeInfoGoals, GoalInfo, Conj),

    % Convert this HLDS code to MLDS.
    ml_gen_goal_as_block(model_det, Conj, MLDS_TypeInfoStmt0, !Info),

    % Replace all heap allocation (new_object instructions) with stack
    % allocation (local variable declarations) in the code to construct
    % type_infos. This is safe because those type_infos will only be used
    % in the immediately following call to gc_trace/1.
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_name(ModuleInfo, ModuleName),
    fixup_newobj(mercury_module_name_to_mlds(ModuleName),
        MLDS_TypeInfoStmt0, MLDS_TypeInfoStmt, NewObjLocalVarDefns),

    % Build MLDS code to trace the variable.
    ml_gen_var_direct(!.Info, TypeInfoVar, TypeInfoLval),
    ml_gen_trace_var(!.Info, VarName, DeclType, ml_lval(TypeInfoLval), Context,
        MLDS_TraceStmt),

    % Generate declarations for any type_info variables used.
    %
    % Note: this will generate local declarations even for type_info variables
    % which are not local to this goal. However, fortunately ml_elim_nested.m
    % will transform the GC code to use the original definitions, which will
    % get put in the GC frame, rather than these declarations, which will get
    % ignored.
    % XXX This is not a very robust way of doing things...
    ml_gen_info_get_var_table(!.Info, VarTable),
    GenLocalVarDecl =
        ( func(Var) = VarDefn :-
            lookup_var_entry(VarTable, Var, Entry),
            LocalVarName = ml_gen_local_var_name(Var, Entry),
            LocalVarType = Entry ^ vte_type,
            VarDefn = ml_gen_mlds_var_decl(LocalVarName,
                mercury_type_to_mlds_type(ModuleInfo, LocalVarType),
                gc_no_stmt, Context)
        ),
    set_of_var.to_sorted_list(NonLocals, NonLocalVarList),
    NonLocalVarDefns = list.map(GenLocalVarDecl, NonLocalVarList),

    % Combine the MLDS code fragments together.
    % XXX MLDS_DEFN
    GC_TraceCode = ml_gen_block(NewObjLocalVarDefns ++ NonLocalVarDefns, [],
        [MLDS_TypeInfoStmt, MLDS_TraceStmt], Context).

    % ml_gen_trace_var(VarName, DeclType, TypeInfo, Context, Code):
    % Generate a call to `private_builtin.gc_trace' for the specified variable,
    % given the variable's name, type, and the already-constructed type_info
    % for that type.
    %
:- pred ml_gen_trace_var(ml_gen_info::in, mlds_local_var_name::in,
    mer_type::in, mlds_rval::in, prog_context::in, mlds_stmt::out) is det.

ml_gen_trace_var(Info, VarName, Type, TypeInfoRval, Context, TraceStmt) :-
    % Generate the lval for Var.
    ml_gen_info_get_module_info(Info, ModuleInfo),
    MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
    VarLval = ml_local_var(VarName, MLDS_Type),

    % Generate the address of `private_builtin.gc_trace/1#0'.
    PredName = "gc_trace",
    PredFormArity = pred_form_arity(1),
    PredLabel = mlds_user_pred_label(pf_predicate, no, PredName, PredFormArity,
        model_det, no),
    ProcId = hlds_pred.initial_proc_id,
    PredModule = mercury_private_builtin_module,
    MLDS_Module = mercury_module_name_to_mlds(PredModule),
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    FuncLabel = mlds_func_label(ProcLabel, proc_func),
    QualFuncLabel = qual_func_label(MLDS_Module, FuncLabel),
    CPointerType = mercury_nb_type(c_pointer_type,
        ctor_cat_user(cat_user_general)),
    ArgTypes = [mlds_pseudo_type_info_type, CPointerType],
    Signature = mlds_func_signature(ArgTypes, []),
    FuncAddr = ml_const(mlconst_code_addr(
        mlds_code_addr(QualFuncLabel, Signature))),

    % Generate the call
    % `private_builtin.gc_trace(TypeInfo, (MR_C_Pointer) &Var);'.
    CastVarAddr = ml_cast(CPointerType, ml_mem_addr(VarLval)),
    TraceStmt = ml_stmt_call(Signature, FuncAddr,
        [TypeInfoRval, CastVarAddr], [], ordinary_call, Context).

    % Generate HLDS code to construct the type_info for this type.
    %
:- pred ml_gen_make_type_info_var(mer_type::in, prog_context::in,
    prog_var::out, list(hlds_goal)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_make_type_info_var(Type, Context, TypeInfoVar, TypeInfoGoals, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo0),
    ml_gen_info_get_pred_proc_id(!.Info, PredProcId),
    module_info_pred_proc_info(ModuleInfo0, PredProcId, PredInfo0, ProcInfo0),
    % Generate the HLDS code to create the type_infos.
    polymorphism_make_type_info_var_mi(Type, Context,
        TypeInfoVar, TypeInfoGoals, ModuleInfo0, ModuleInfo1,
        PredInfo0, PredInfo, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PredProcId, PredInfo, ProcInfo,
        ModuleInfo1, ModuleInfo),
    % Save the new information back in the ml_gen_info.
    proc_info_get_var_table(ProcInfo, VarTable),
    ml_gen_info_set_module_info(ModuleInfo, !Info),
    ml_gen_info_set_var_table(VarTable, !Info).

%---------------------------------------------------------------------------%

:- type fixup_newobj_info
    --->    fixup_newobj_info(
                % The current module.
                fnoi_module_name    :: mlds_module_name,

                % The local variable declarations accumulated so far.
                fnoi_locals         :: cord(mlds_local_var_defn),

                % A counter used to allocate variable names.
                fnoi_next_id        :: counter
            ).

    % Replace all heap allocation (new_object instructions) with stack
    % allocation (local variable declarations) in the specified statement,
    % returning the local variable declarations needed for the stack
    % allocation.
    %
:- pred fixup_newobj(mlds_module_name::in, mlds_stmt::in, mlds_stmt::out,
    list(mlds_local_var_defn)::out) is det.

fixup_newobj(ModuleName, Stmt0, Stmt, Defns) :-
    Info0 = fixup_newobj_info(ModuleName, cord.init, counter.init(0)),
    fixup_newobj_in_stmt(Stmt0, Stmt, Info0, Info),
    Defns = cord.to_list(Info ^ fnoi_locals).

:- pred fixup_newobj_in_stmt(mlds_stmt::in, mlds_stmt::out,
    fixup_newobj_info::in, fixup_newobj_info::out) is det.

fixup_newobj_in_stmt(Stmt0, Stmt, !Fixup) :-
    (
        Stmt0 = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts0, Context),
        list.map_foldl(fixup_newobj_in_stmt, SubStmts0, SubStmts, !Fixup),
        Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context)
    ;
        Stmt0 = ml_stmt_while(Kind, Rval, BodyStmt0, LoopLocalVars, Context),
        fixup_newobj_in_stmt(BodyStmt0, BodyStmt, !Fixup),
        Stmt = ml_stmt_while(Kind, Rval, BodyStmt, LoopLocalVars, Context)
    ;
        Stmt0 = ml_stmt_if_then_else(Cond, Then0, MaybeElse0, Context),
        fixup_newobj_in_stmt(Then0, Then, !Fixup),
        fixup_newobj_in_maybe_statement(MaybeElse0, MaybeElse, !Fixup),
        Stmt = ml_stmt_if_then_else(Cond, Then, MaybeElse, Context)
    ;
        Stmt0 = ml_stmt_switch(Type, Val, Range, Cases0, Default0, Context),
        list.map_foldl(fixup_newobj_in_case, Cases0, Cases, !Fixup),
        fixup_newobj_in_default(Default0, Default, !Fixup),
        Stmt = ml_stmt_switch(Type, Val, Range, Cases, Default, Context)
    ;
        ( Stmt0 = ml_stmt_label(_Label, _Context)
        ; Stmt0 = ml_stmt_goto(_Target, _Context)
        ; Stmt0 = ml_stmt_computed_goto(_Rval, _Labels, _Context)
        ; Stmt0 = ml_stmt_call(_Sig, _Func, _Args, _RetLvals,
            _TailCall, _Context)
        ; Stmt0 = ml_stmt_return(_Rvals, _Context)
        ; Stmt0 = ml_stmt_do_commit(_Ref, _Context)
        ),
        Stmt = Stmt0
    ;
        Stmt0 = ml_stmt_try_commit(Ref, BodyStmt0, HandlerStmt0, Context),
        fixup_newobj_in_stmt(BodyStmt0, BodyStmt, !Fixup),
        fixup_newobj_in_stmt(HandlerStmt0, HandlerStmt, !Fixup),
        Stmt = ml_stmt_try_commit(Ref, BodyStmt, HandlerStmt, Context)
    ;
        Stmt0 = ml_stmt_atomic(AtomicStmt0, Context),
        fixup_newobj_in_atomic_statement(AtomicStmt0, Context, Stmt, !Fixup)
    ).

:- pred fixup_newobj_in_case(mlds_switch_case::in, mlds_switch_case::out,
    fixup_newobj_info::in, fixup_newobj_info::out) is det.

fixup_newobj_in_case(Case0, Case, !Fixup) :-
    Case0 = mlds_switch_case(FirstCond, LaterConds, Stmt0),
    fixup_newobj_in_stmt(Stmt0, Stmt, !Fixup),
    Case  = mlds_switch_case(FirstCond, LaterConds, Stmt).

:- pred fixup_newobj_in_maybe_statement(
    maybe(mlds_stmt)::in, maybe(mlds_stmt)::out,
    fixup_newobj_info::in, fixup_newobj_info::out) is det.

fixup_newobj_in_maybe_statement(no, no, !Fixup).
fixup_newobj_in_maybe_statement(yes(Stmt0), yes(Stmt), !Fixup) :-
    fixup_newobj_in_stmt(Stmt0, Stmt, !Fixup).

:- pred fixup_newobj_in_default(mlds_switch_default::in,
    mlds_switch_default::out,
    fixup_newobj_info::in, fixup_newobj_info::out) is det.

fixup_newobj_in_default(Default0, Default, !Fixup) :-
    (
        ( Default0 = default_is_unreachable
        ; Default0 = default_do_nothing
        ),
        Default = Default0
    ;
        Default0 = default_case(Stmt0),
        fixup_newobj_in_stmt(Stmt0, Stmt, !Fixup),
        Default = default_case(Stmt)
    ).

:- pred fixup_newobj_in_atomic_statement(mlds_atomic_statement::in,
    prog_context::in, mlds_stmt::out,
    fixup_newobj_info::in, fixup_newobj_info::out) is det.

fixup_newobj_in_atomic_statement(AtomicStmt0, Context, Stmt, !Fixup) :-
    ( if
        AtomicStmt0 = new_object(Lval, Ptag, _ExplicitSecTag,
            PointerType, _MaybeSizeInWordsRval, _MaybeCtorName,
            ArgRvalsTypes,  _MayUseAtomic, _AllocId)
    then
        % Generate the declaration of the new local variable.
        %
        % XXX Using array(generic_type) is wrong for --high-level-data.
        %
        % We need to specify an initializer to tell the C back-end what the
        % length of the array is. We initialize it with null pointers and then
        % later generate assignment statements to fill in the values properly
        % (see below).
        counter.allocate(Id, !.Fixup ^ fnoi_next_id, NextId),
        VarName = lvn_comp_var(lvnc_new_obj(Id)),
        VarType = mlds_array_type(mlds_generic_type),
        NullPointers = list.duplicate(list.length(ArgRvalsTypes),
            init_obj(ml_const(mlconst_null(mlds_generic_type)))),
        Initializer = init_array(NullPointers),
        % This is used for the type_infos allocated during tracing,
        % and we don't need to trace them.
        GCStmt = gc_no_stmt,
        VarDecl = ml_gen_mlds_var_decl_init(VarName, VarType,
            Initializer, GCStmt, Context),
        !Fixup ^ fnoi_next_id := NextId,

        % XXX We should keep a more structured representation of the local
        % variables, such as a map from variable names.
        Locals0 = !.Fixup ^ fnoi_locals,
        Locals = cord.snoc(Locals0, VarDecl),
        !Fixup ^ fnoi_locals := Locals,

        % Generate code to initialize the variable.
        %
        % Note that we need to use assignment statements, rather than an
        % initializer, to initialize the local variable, because the
        % initialization code needs to occur at exactly the point where the
        % atomic_statement occurs, rather than at the local variable
        % declaration.

        VarLval = ml_local_var(VarName, VarType),
        PtrRval = ml_cast(PointerType, ml_mem_addr(VarLval)),
        list.map_foldl(init_field_n(PointerType, PtrRval, Context),
            ArgRvalsTypes, ArgInitStmts, 0, _NumFields),

        % Generate code to assign the address of the new local variable
        % to the Lval.
        ( if Ptag = ptag(0u8) then
            TaggedPtrRval = PtrRval
        else
            TaggedPtrRval = ml_cast(PointerType, ml_mkword(Ptag, PtrRval))
        ),
        AssignStmt = ml_stmt_atomic(assign(Lval, TaggedPtrRval), Context),
        Stmt = ml_stmt_block([], [], ArgInitStmts ++ [AssignStmt], Context)
    else
        Stmt = ml_stmt_atomic(AtomicStmt0, Context)
    ).

:- pred init_field_n(mlds_type::in, mlds_rval::in, prog_context::in,
    mlds_typed_rval::in, mlds_stmt::out, int::in, int::out) is det.

init_field_n(PointerType, PointerRval, Context, ArgRvalType, Stmt,
        FieldNum, FieldNum + 1) :-
    ArgRvalType = ml_typed_rval(ArgRval, _ArgType),
    FieldId = ml_field_offset(ml_const(mlconst_int(FieldNum))),
    % XXX FieldType is wrong for --high-level-data: should this be _ArgType?
    FieldType = mlds_generic_type,
    MaybePtag = yes(ptag(0u8)),
    Field = ml_field(MaybePtag, PointerRval, PointerType, FieldId, FieldType),
    Stmt = ml_stmt_atomic(assign(Field, ArgRval), Context).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_accurate_gc.
%---------------------------------------------------------------------------%
