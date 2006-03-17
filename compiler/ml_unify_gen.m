%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: ml_unify_gen.m
% Main author: fjh

% This module is part of the MLDS code generator.
% It handles MLDS code generation for unifications.

%-----------------------------------------------------------------------------%

:- module ml_backend.ml_unify_gen.
:- interface.

:- import_module hlds.code_model.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module std_util.

%-----------------------------------------------------------------------------%

    % Generate MLDS code for a unification.
    %
:- pred ml_gen_unification(unification::in, code_model::in, prog_context::in,
    mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Convert a cons_id for a given type to a cons_tag.
    %
:- pred ml_cons_id_to_tag(ml_gen_info::in, cons_id::in, mer_type::in,
    cons_tag::out) is det.

    % ml_gen_tag_test(Var, ConsId, Defns, Statements, Expression):
    %   Generate code to perform a tag test.
    %
    % The test checks whether Var has the functor specified by ConsId.
    % The generated code may contain Defns, Statements and an Expression.
    % The Expression is a boolean rval. After execution of the Statements,
    % Expression will evaluate to true iff the Var has the functor specified
    % by ConsId.
    %
:- pred ml_gen_tag_test(prog_var::in, cons_id::in,
    mlds_defns::out, statements::out, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % ml_gen_secondary_tag_rval(PrimaryTag, VarType, ModuleInfo, VarRval):
    %
    % Return the rval for the secondary tag field of VarRval, assuming that
    % VarRval has the specified VarType and PrimaryTag.
    %
:- func ml_gen_secondary_tag_rval(tag_bits, mer_type, module_info, mlds_rval)
    = mlds_rval.

    % Generate an MLDS rval for a given reserved address,
    % cast to the appropriate type.
    %
:- func ml_gen_reserved_address(module_info, reserved_address, mlds_type) =
    mlds_rval.

    % ml_gen_new_object(MaybeConsId, Tag, HasSecTag, MaybeCtorName, Var,
    %   ExtraRvals, ExtraTypes, ArgVars, ArgModes, TakeAddr, HowToConstruct,
    %   Context, Decls, Statements, !Info):
    %
    % Generate a `new_object' statement, or a static constant, depending on the
    % value of the how_to_construct argument. The `ExtraRvals' and `ExtraTypes'
    % arguments specify additional constants to insert at the start of the
    % argument list.
    %
:- pred ml_gen_new_object(maybe(cons_id)::in, mlds_tag::in, bool::in,
    maybe(ctor_name)::in, prog_var::in, list(mlds_rval)::in,
    list(mlds_type)::in, prog_vars::in, list(uni_mode)::in, list(int)::in,
    how_to_construct::in, prog_context::in, mlds_defns::out,
    statements::out, ml_gen_info::in, ml_gen_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.rtti.
:- import_module backend_libs.type_class_info.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_call_gen.
:- import_module ml_backend.ml_closure_gen.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_type_gen.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module int.
:- import_module map.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

ml_gen_unification(Unification, CodeModel, Context, [], Statements, !Info) :-
    Unification = assign(TargetVar, SourceVar),
    expect(unify(CodeModel, model_det), this_file,
        "ml_code_gen: assign not det"),
    (
        % Skip dummy argument types, since they will not have been declared.
        ml_variable_type(!.Info, TargetVar, Type),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        is_dummy_argument_type(ModuleInfo, Type)
    ->
        Statements = []
    ;
        ml_gen_var(!.Info, TargetVar, TargetLval),
        ml_gen_var(!.Info, SourceVar, SourceLval),
        Statement = ml_gen_assign(TargetLval, lval(SourceLval), Context),
        Statements = [Statement]
    ),
    ( ml_gen_info_search_const_var_name(!.Info, SourceVar, Name) ->
        % If the source variable is a constant, so is the target after
        % this assignment.
        %
        % The mark_static_terms assumes that if SourceVar is a constant term,
        % then after this assignment unification TargetVar is a constant term
        % also. Therefore later constant terms may contain TargetVar among
        % their arguments. If we didn't copy the constant info here, the
        % construction of the later constant could cause a code generator
        % abort.
        ml_gen_info_set_const_var_name(TargetVar, Name, !Info)
    ;
        true
    ).

ml_gen_unification(Unification, CodeModel, Context, [], [Statement], !Info) :-
    Unification = simple_test(Var1, Var2),
    expect(unify(CodeModel, model_semi), this_file,
        "ml_code_gen: simple_test not semidet"),
    ml_variable_type(!.Info, Var1, Type),
    ( Type = builtin(string) ->
        EqualityOp = str_eq
    ; Type = builtin(float) ->
        EqualityOp = float_eq
    ;
        EqualityOp = eq
    ),
    ml_gen_var(!.Info, Var1, Var1Lval),
    ml_gen_var(!.Info, Var2, Var2Lval),
    Test = binop(EqualityOp, lval(Var1Lval), lval(Var2Lval)),
    ml_gen_set_success(!.Info, Test, Context, Statement).

ml_gen_unification(Unification, CodeModel, Context, Decls, Statements,
        !Info) :-
    Unification = construct(Var, ConsId, Args, ArgModes, HowToConstruct,
        _CellIsUnique, SubInfo),
    expect(unify(CodeModel, model_det), this_file,
        "ml_code_gen: construct not det"),
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
        expect(unify(MaybeSizeProfInfo, no), this_file,
            "ml_code_gen: term size profiling not yet supported")
    ),
    ml_gen_construct(Var, ConsId, Args, ArgModes, TakeAddr, HowToConstruct,
        Context, Decls, Statements, !Info).

ml_gen_unification(Unification, CodeModel, Context, Decls, Statements,
        !Info) :-
    Unification = deconstruct(Var, ConsId, Args, ArgModes, CanFail, CanCGC),
    (
        CanFail = can_fail,
        ExpectedCodeModel = model_semi,
        ml_gen_semi_deconstruct(Var, ConsId, Args, ArgModes, Context,
            Decls, Unif_Statements, !Info)
    ;
        CanFail = cannot_fail,
        ExpectedCodeModel = model_det,
        ml_gen_det_deconstruct(Var, ConsId, Args, ArgModes, Context,
            Decls, Unif_Statements, !Info)
    ),
    (
        % Note that we can deallocate a cell even if the unification fails,
        % it is the responsibility of the structure reuse phase to ensure that
        % this is safe.
        CanCGC = can_cgc,
        ml_gen_var(!.Info, Var, VarLval),
        Stmt = atomic(delete_object(VarLval)),
        CGC_Statements = [statement(Stmt, mlds_make_context(Context)) ]
    ;
        CanCGC = cannot_cgc,
        CGC_Statements = []
    ),
    Statements0 = Unif_Statements ++ CGC_Statements,

    % We used to require that CodeModel = ExpectedCodeModel. But the
    % determinism field in the goal_info is allowed to be a conservative
    % approximation, so we need to handle the case were CodeModel is less
    % precise than ExpectedCodeModel.
    %
    ml_gen_wrap_goal(CodeModel, ExpectedCodeModel, Context,
        Statements0, Statements, !Info).

ml_gen_unification(complicated_unify(_, _, _), _, _, [], [], !Info) :-
    % Simplify.m should have converted these into procedure calls.
    unexpected(this_file, "ml_code_gen: complicated unify").

    % ml_gen_construct generates code for a construction unification.
    %
    % Note that the code for ml_gen_static_const_arg is very similar to
    % the code here, and any changes may need to be done in both places.
    %
:- pred ml_gen_construct(prog_var::in, cons_id::in, prog_vars::in,
    list(uni_mode)::in, list(int)::in, how_to_construct::in, prog_context::in,
    mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_construct(Var, ConsId, Args, ArgModes, TakeAddr, HowToConstruct,
        Context, Decls, Statements, !Info) :-
    % Figure out how this cons_id is represented.
    ml_variable_type(!.Info, Var, Type),
    ml_cons_id_to_tag(!.Info, ConsId, Type, Tag),
    ml_gen_construct_2(Tag, Type, Var, ConsId, Args, ArgModes, TakeAddr,
        HowToConstruct, Context, Decls, Statements, !Info).

:- pred ml_gen_construct_2(cons_tag::in, mer_type::in, prog_var::in,
    cons_id::in, prog_vars::in, list(uni_mode)::in, list(int)::in,
    how_to_construct::in, prog_context::in, mlds_defns::out,
    statements::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_construct_2(Tag, Type, Var, ConsId, Args, ArgModes, TakeAddr,
        HowToConstruct, Context, Decls, Statements, !Info) :-
    (
        % Types for which some other constructor has a reserved_address
        % -- that only makes a difference when deconstructing, so here we
        % ignore that, and just recurse on the representation for this
        % constructor.

        Tag = shared_with_reserved_addresses(_, ThisTag),
        ml_gen_construct_2(ThisTag, Type, Var, ConsId, Args, ArgModes,
            TakeAddr, HowToConstruct, Context, Decls, Statements, !Info)
    ;
        Tag = no_tag,
        (
            Args = [Arg],
            ArgModes = [ArgMode]
        ->
            ml_variable_type(!.Info, Arg, ArgType),
            ml_variable_type(!.Info, Var, VarType),
            ml_gen_var(!.Info, Arg, ArgLval),
            ml_gen_var(!.Info, Var, VarLval),
            ml_gen_sub_unify(ArgMode, ArgLval, ArgType, VarLval,
                VarType, Context, [], Statements, !Info),
            Decls = []
        ;
            unexpected(this_file, "ml_code_gen: no_tag: arity != 1")
        )
    ;
        % Lambda expressions.
        Tag = pred_closure_tag(PredId, ProcId, _EvalMethod),
        ml_gen_closure(PredId, ProcId, Var, Args, ArgModes, HowToConstruct,
            Context, Decls, Statements, !Info)
    ;
        % Ordinary compound terms.
        ( Tag = single_functor
        ; Tag = unshared_tag(_TagVal)
        ; Tag = shared_remote_tag(_PrimaryTag, _SecondaryTag)
        ),
        ml_gen_compound(Tag, ConsId, Var, Args, ArgModes, TakeAddr,
            HowToConstruct, Context, Decls, Statements, !Info)
    ;
        ( Tag = int_constant(_)
        ; Tag = float_constant(_)
        ; Tag = string_constant(_)
        ; Tag = reserved_address(_)
        ; Tag = shared_local_tag(_, _)
        ; Tag = type_ctor_info_constant(_, _, _)
        ; Tag = base_typeclass_info_constant(_, _, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = tabling_pointer_constant(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ),
        (
            % Constants.
            Args = []
        ->
            ml_gen_var(!.Info, Var, VarLval),
            ml_gen_constant(Tag, Type, Rval, !Info),
            Statement = ml_gen_assign(VarLval, Rval, Context),
            Decls = [],
            Statements = [Statement]
        ;
            unexpected(this_file, "ml_gen_construct: bad constant term")
        )
    ).

    % ml_gen_static_const_arg is similar to ml_gen_construct with
    % HowToConstruct = construct_statically(_), except that for compound terms,
    % rather than generating a new static constant, it just generates a
    % reference to one that has already been defined.
    %
    % Note that any changes here may require similar changes to
    % ml_gen_construct.
    %
:- pred ml_gen_static_const_arg(prog_var::in, static_cons::in, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_static_const_arg(Var, StaticCons, Rval, !Info) :-
    % Figure out how this argument is represented.
    StaticCons = static_cons(ConsId, _ArgVars, _StaticArgs),
    ml_variable_type(!.Info, Var, VarType),
    ml_cons_id_to_tag(!.Info, ConsId, VarType, Tag),
    ml_gen_static_const_arg_2(Tag, VarType, Var, StaticCons, Rval, !Info).

:- pred ml_gen_static_const_arg_2(cons_tag::in, mer_type::in, prog_var::in,
    static_cons::in, mlds_rval::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_static_const_arg_2(Tag, VarType, Var, StaticCons, Rval, !Info) :-
    StaticCons = static_cons(ConsId, ArgVars, StaticArgs),
    (
        % Types for which some other constructor has a reserved_address
        % -- that only makes a difference when % constructing, so here
        % we ignore that, and just recurse on the representation for
        % this constructor.
        Tag = shared_with_reserved_addresses(_, ThisTag)
    ->
        ml_gen_static_const_arg_2(ThisTag, VarType, Var, StaticCons,
            Rval, !Info)
    ;
        Tag = no_tag
    ->
        (
            ArgVars = [Arg],
            StaticArgs = [StaticArg]
        ->
            % Construct (statically) the argument, and then convert it
            % to the appropriate type.
            ml_gen_static_const_arg(Arg, StaticArg, ArgRval, !Info),
            ml_variable_type(!.Info, Arg, ArgType),
            ml_gen_box_or_unbox_rval(ArgType, VarType, native_if_possible,
                ArgRval, Rval, !Info)
        ;
            unexpected(this_file, "ml_code_gen: no_tag: arity != 1")
        )
    ;
        % Compound terms, including lambda expressions.
        ( Tag = pred_closure_tag(_, _, _), TagVal = 0
        ; Tag = single_functor, TagVal = 0
        ; Tag = unshared_tag(TagVal)
        ; Tag = shared_remote_tag(TagVal, _SecondaryTag)
        )
    ->
        % If this argument is something that would normally be allocated
        % on the heap, just generate a reference to the static constant
        % that we must have already generated for it.

        ml_gen_type(!.Info, VarType, MLDS_VarType),
        ml_gen_info_get_globals(!.Info, Globals),
        globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
        UsesBaseClass = (ml_tag_uses_base_class(Tag) -> yes ; no),
        ConstType = get_type_for_cons_id(MLDS_VarType, UsesBaseClass,
            yes(ConsId), HighLevelData, Globals),
        ml_gen_static_const_addr(!.Info, Var, ConstType, ConstAddrRval),
        ( TagVal = 0 ->
            TaggedRval = ConstAddrRval
        ;
            TaggedRval = mkword(TagVal, ConstAddrRval)
        ),
        Rval = unop(cast(MLDS_VarType), TaggedRval)
    ;
        % If this argument is just a constant, then generate the rval
        % for the constant.
        StaticArgs = []
    ->
        ml_gen_constant(Tag, VarType, Rval, !Info)
    ;
        unexpected(this_file, "ml_gen_static_const_arg: unknown compound term")
    ).

    % Generate the rval for a given constant.
    %
:- pred ml_gen_constant(cons_tag::in, mer_type::in, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_constant(string_constant(String), _, const(string_const(String)),
        !Info).
ml_gen_constant(int_constant(Int), _, const(int_const(Int)), !Info).
ml_gen_constant(float_constant(Float), _, const(float_const(Float)), !Info).
ml_gen_constant(shared_local_tag(Bits1, Num1), VarType, Rval, !Info) :-
    ml_gen_type(!.Info, VarType, MLDS_Type),
    Rval = unop(cast(MLDS_Type), mkword(Bits1,
        unop(std_unop(mkbody), const(int_const(Num1))))).

ml_gen_constant(type_ctor_info_constant(ModuleName0, TypeName, TypeArity),
        VarType, Rval, !Info) :-
    ml_gen_type(!.Info, VarType, MLDS_VarType),
    ModuleName = fixup_builtin_module(ModuleName0),
    MLDS_Module = mercury_module_name_to_mlds(ModuleName),
    RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
    DataAddr = data_addr(MLDS_Module,
        rtti(ctor_rtti_id(RttiTypeCtor, type_ctor_info))),
    Rval = unop(cast(MLDS_VarType), const(data_addr_const(DataAddr))).

ml_gen_constant(base_typeclass_info_constant(ModuleName, ClassId, Instance),
        VarType, Rval, !Info) :-
    ml_gen_type(!.Info, VarType, MLDS_VarType),
    MLDS_Module = mercury_module_name_to_mlds(ModuleName),
    TCName = generate_class_name(ClassId),
    DataAddr = data_addr(MLDS_Module, rtti(tc_rtti_id(TCName,
        base_typeclass_info(ModuleName, Instance)))),
    Rval = unop(cast(MLDS_VarType), const(data_addr_const(DataAddr))).

ml_gen_constant(tabling_pointer_constant(PredId, ProcId), VarType, Rval,
        !Info) :-
    ml_gen_type(!.Info, VarType, MLDS_VarType),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_pred_label(ModuleInfo, PredId, ProcId, PredLabel, PredModule),
    DataAddr = data_addr(PredModule, tabling_pointer(PredLabel - ProcId)),
    Rval = unop(cast(MLDS_VarType), const(data_addr_const(DataAddr))).

ml_gen_constant(deep_profiling_proc_layout_tag(_, _), _, _, !Info) :-
    unexpected(this_file,
        "ml_gen_constant: deep_profiling_proc_layout_tag not yet supported").

ml_gen_constant(table_io_decl_tag(_, _), _, _, !Info) :-
    unexpected(this_file,
        "ml_gen_constant: table_io_decl_tag not yet supported").

ml_gen_constant(reserved_address(ReservedAddr), VarType, Rval, !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    ml_gen_type(!.Info, VarType, MLDS_VarType),
    Rval = ml_gen_reserved_address(ModuleInfo, ReservedAddr, MLDS_VarType).

ml_gen_constant(shared_with_reserved_addresses(_, ThisTag), VarType, Rval,
        !Info) :-
    % For shared_with_reserved_address, the sharing is only important for
    % tag tests, not for constructions, so here we just recurse on the
    % real representation.
    ml_gen_constant(ThisTag, VarType, Rval, !Info).

% These tags, which are not (necessarily) constants, are handled
% in ml_gen_construct and ml_gen_static_const_arg,
% so we don't need to handle them here.

ml_gen_constant(no_tag, _, _, !Info) :-
    unexpected(this_file, "ml_gen_constant: no_tag").
ml_gen_constant(single_functor, _, _, !Info) :-
    unexpected(this_file, "ml_gen_constant: single_functor").
ml_gen_constant(unshared_tag(_), _, _, !Info) :-
    unexpected(this_file, "ml_gen_constant: unshared_tag").
ml_gen_constant(shared_remote_tag(_, _), _, _, !Info) :-
    unexpected(this_file, "ml_gen_constant: shared_remote_tag").
ml_gen_constant(pred_closure_tag(_, _, _), _, _, !Info) :-
    unexpected(this_file, "ml_gen_constant: pred_closure_tag").

%-----------------------------------------------------------------------------%

ml_gen_reserved_address(_, null_pointer, MLDS_Type) = const(null(MLDS_Type)).
ml_gen_reserved_address(_, small_pointer(Int), MLDS_Type) =
        unop(cast(MLDS_Type), const(int_const(Int))).
ml_gen_reserved_address(ModuleInfo, reserved_object(TypeCtor, QualCtorName,
        CtorArity), _Type) = Rval :-
    ( QualCtorName = qualified(ModuleName, CtorName) ->
        module_info_get_globals(ModuleInfo, Globals),
        MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
        TypeCtor = TypeName - TypeArity,
        unqualify_name(TypeName, UnqualTypeName),
        MLDS_TypeName = mlds_append_class_qualifier(MLDS_ModuleName,
            module_qual, Globals, UnqualTypeName, TypeArity),
        Name = ml_format_reserved_object_name(CtorName, CtorArity),
        Rval0 = const(data_addr_const(
            data_addr(MLDS_TypeName, var(Name)))),

        % The MLDS type of the reserved object may be a class derived from
        % the base class for this Mercury type. So for some back-ends,
        % we need to insert a (down-)cast here to convert from the derived
        % class to the base class. In particular, this is needed to avoid
        % compiler warnings in the C code generated by the MLDS->C back-end.
        % But inserting the cast could slow down the generated code for the
        % .NET back-end (where the JIT probably doesn't optimize downcasts).
        %% So we only do it if the back-end requires it.
        %
        globals.get_target(Globals, Target),
        ( target_supports_inheritence(Target) = yes ->
            Rval = Rval0
        ;
            MLDS_Type = mlds_ptr_type(mlds_class_type(
                qual(MLDS_ModuleName, module_qual, UnqualTypeName),
                TypeArity, mlds_class)),
            Rval = unop(cast(MLDS_Type), Rval0)
        )
    ;
        unexpected(this_file,
            "unqualified ctor name in reserved_object")
    ).

    % This should return `yes' iff downcasts are not needed.
    %
:- func target_supports_inheritence(compilation_target) = bool.

target_supports_inheritence(c) = no.
target_supports_inheritence(il) = yes.
target_supports_inheritence(java) = yes.
target_supports_inheritence(asm) = no.

%-----------------------------------------------------------------------------%

    % Convert a cons_id for a given type to a cons_tag.
    %
ml_cons_id_to_tag(Info, ConsId, Type, Tag) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    Tag = cons_id_to_tag(ConsId, Type, ModuleInfo).

    % Generate code to construct a new object.
    %
:- pred ml_gen_compound(cons_tag::in, cons_id::in, prog_var::in, prog_vars::in,
    list(uni_mode)::in, list(int)::in, how_to_construct::in, prog_context::in,
    mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_compound(Tag, ConsId, Var, ArgVars, ArgModes, TakeAddr, HowToConstruct,
        Context, Decls, Statements, !Info) :-
    % Get the primary and secondary tags.
    ( get_primary_tag(Tag) = yes(PrimaryTag0) ->
        PrimaryTag = PrimaryTag0
    ;
        unexpected(this_file, "ml_gen_compound: primary tag unknown")
    ),
    MaybeSecondaryTag = get_secondary_tag(Tag),

    % Figure out which class name to construct.
    ( ml_tag_uses_base_class(Tag) ->
        MaybeCtorName = no
    ;
        ml_cons_name(ConsId, CtorName),
        MaybeCtorName = yes(CtorName)
    ),

    % If there is a secondary tag, it goes in the first field.
    (
        MaybeSecondaryTag = yes(SecondaryTag),
        HasSecTag = yes,
        SecondaryTagRval0 = const(int_const(SecondaryTag)),
        SecondaryTagType0 = mlds_native_int_type,

        % With the low-level data representation, all fields -- even the
        % secondary tag -- are boxed, and so we need box it here.

        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
        (
            HighLevelData = no,
            SecondaryTagRval = unop(box(SecondaryTagType0), SecondaryTagRval0),
            SecondaryTagType = mlds_generic_type
        ;
            HighLevelData = yes,
            SecondaryTagRval = SecondaryTagRval0,
            SecondaryTagType = SecondaryTagType0
        ),
        ExtraRvals = [SecondaryTagRval],
        ExtraArgTypes = [SecondaryTagType]
    ;
        MaybeSecondaryTag = no,
        HasSecTag = no,
        ExtraRvals = [],
        ExtraArgTypes = []
    ),
    ml_gen_new_object(yes(ConsId), PrimaryTag, HasSecTag, MaybeCtorName,
        Var, ExtraRvals, ExtraArgTypes, ArgVars, ArgModes, TakeAddr,
        HowToConstruct, Context, Decls, Statements, !Info).

    % ml_gen_new_object: Generate a `new_object' statement, or a static
    % constant, depending on the value of the how_to_construct argument.
    % The `ExtraRvals' and `ExtraTypes' arguments specify additional constants
    % to insert at the start of the argument list.
    %
ml_gen_new_object(MaybeConsId, Tag, HasSecTag, MaybeCtorName, Var,
        ExtraRvals, ExtraTypes, ArgVars, ArgModes, TakeAddr, HowToConstruct,
        Context, Decls, Statements, !Info) :-
    % Determine the variable's type and lval, the tag to use, and the types
    % of the argument vars.
    ml_variable_type(!.Info, Var, Type),
    ml_gen_type(!.Info, Type, MLDS_Type),
    ml_gen_var(!.Info, Var, VarLval),
    ( Tag = 0 ->
        MaybeTag = no
    ;
        MaybeTag = yes(Tag)
    ),
    ml_variable_types(!.Info, ArgVars, ArgTypes),

    (
        HowToConstruct = construct_dynamically,

        % Find out the types of the constructor arguments and generate rvals
        % for them (boxing/unboxing if needed).
        ml_gen_var_list(!.Info, ArgVars, ArgLvals),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        get_maybe_cons_id_arg_types(MaybeConsId, ArgTypes, Type,
            ModuleInfo, ConsArgTypes),
        FirstOffset = length(ExtraRvals),
        ml_gen_cons_args(ArgVars, ArgLvals, ArgTypes, ConsArgTypes, ArgModes,
            FirstOffset, 1, TakeAddr, ModuleInfo, ArgRvals0, MLDS_ArgTypes0,
            TakeAddrInfos, !Info),

        % Insert the extra rvals at the start.
        ArgRvals = ExtraRvals ++ ArgRvals0,
        MLDS_ArgTypes = ExtraTypes ++ MLDS_ArgTypes0,

        % Compute the number of words to allocate.
        list.length(ArgRvals, NumArgs),
        SizeInWordsRval = const(int_const(NumArgs)),

        % Generate a `new_object' statement to dynamically allocate the memory
        % for this term from the heap. The `new_object' statement will also
        % initialize the fields of this term with the specified arguments.
        MakeNewObject = new_object(VarLval, MaybeTag, HasSecTag, MLDS_Type,
            yes(SizeInWordsRval), MaybeCtorName, ArgRvals, MLDS_ArgTypes),
        Stmt = atomic(MakeNewObject),
        Statement = statement(Stmt, mlds_make_context(Context)),

        ml_gen_field_take_address_assigns(TakeAddrInfos, VarLval, MLDS_Type,
            MaybeTag, Context, !.Info, FieldAssigns),
        Statements = [Statement | FieldAssigns],
        Decls = []
    ;
        HowToConstruct = construct_statically(StaticArgs),
        expect(unify(TakeAddr, []), this_file, 
            "ml_gen_new_object: cannot take address of static object's field"),

        % Find out the types of the constructor arguments.
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        get_maybe_cons_id_arg_types(MaybeConsId, ArgTypes, Type,
            ModuleInfo, ConsArgTypes),
        list.map(ml_gen_field_type(!.Info), ConsArgTypes, FieldTypes),

        % Generate rvals for the arguments.
        list.map(ml_gen_type(!.Info), ArgTypes, MLDS_ArgTypes0),
        ml_gen_static_const_arg_list(ArgVars, StaticArgs, ArgRvals0, !Info),

        % Box or unbox the arguments, if needed, and insert the extra rvals
        % at the start.
        ml_gen_info_get_globals(!.Info, Globals),
        globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
        (
            HighLevelData = no,
            % Box *all* the arguments, including the ExtraRvals.
            ArgRvals1 = ExtraRvals ++ ArgRvals0,
            MLDS_ArgTypes = ExtraTypes ++ MLDS_ArgTypes0,
            ml_gen_box_const_rval_list(MLDS_ArgTypes, ArgRvals1,
                Context, BoxConstDefns, ArgRvals, !Info)
        ;
            HighLevelData = yes,
            ml_gen_box_or_unbox_const_rval_list(ArgTypes, FieldTypes,
                ArgRvals0, Context, BoxConstDefns, ArgRvals1, !Info),
            % For --high-level-data, the ExtraRvals should already have
            % the right type, so we don't need to worry about boxing
            % or unboxing them.
            ArgRvals = ExtraRvals ++ ArgRvals1
        ),

        % Generate a local static constant for this term.
        ml_gen_static_const_name(Var, ConstName, !Info),
        UsesBaseClass = (MaybeCtorName = yes(_) -> no ; yes),
        ConstType = get_type_for_cons_id(MLDS_Type, UsesBaseClass,
            MaybeConsId, HighLevelData, Globals),
        % XXX If the secondary tag is in a base class, then ideally its
        % initializer should be wrapped in `init_struct([init_obj(X)])'
        % rather than just `init_obj(X)' -- the fact that we don't leads to
        % some warnings from GNU C about missing braces in initializers.
        ArgInits = list.map(func(X) = init_obj(X), ArgRvals),
        ( ConstType = mlds_array_type(_) ->
            Initializer = init_array(ArgInits)
        ;
            Initializer = init_struct(ConstType, ArgInits)
        ),
        ConstDefn = ml_gen_static_const_defn(ConstName, ConstType,
            local, Initializer, Context),

        % Assign the address of the local static constant to the variable.
        ml_gen_static_const_addr(!.Info, Var, ConstType, ConstAddrRval),
        (
            MaybeTag = no,
            TaggedRval = ConstAddrRval
        ;
            MaybeTag = yes(_),
            TaggedRval = mkword(Tag, ConstAddrRval)
        ),
        Rval = unop(cast(MLDS_Type), TaggedRval),
        AssignStatement = ml_gen_assign(VarLval, Rval, Context),
        Decls = BoxConstDefns ++ [ConstDefn],
        Statements = [AssignStatement]
    ;
        HowToConstruct = reuse_cell(CellToReuse),
        CellToReuse = cell_to_reuse(ReuseVar, ReuseConsIds, _),
        (
            MaybeConsId = yes(ConsId0),
            ConsId = ConsId0
        ;
            MaybeConsId = no,
            unexpected(this_file, "ml_gen_new_object: unknown cons id")
        ),
        list.map(
            (pred(ReuseConsId::in, ReusePrimTag::out) is det :-
                ml_variable_type(!.Info, ReuseVar, ReuseType),
                ml_cons_id_to_tag(!.Info, ReuseConsId,
                    ReuseType, ReuseConsIdTag),
                ml_tag_offset_and_argnum(ReuseConsIdTag, ReusePrimTag,
                    _ReuseOffSet, _ReuseArgNum)
            ), ReuseConsIds, ReusePrimaryTags0),
        list.remove_dups(ReusePrimaryTags0, ReusePrimaryTags),

        ml_cons_id_to_tag(!.Info, ConsId, Type, ConsIdTag),
        ml_field_names_and_types(!.Info, Type, ConsId, ArgTypes, Fields),
        ml_tag_offset_and_argnum(ConsIdTag, PrimaryTag, OffSet, ArgNum),

        ml_gen_var(!.Info, Var, Var1Lval),
        ml_gen_var(!.Info, ReuseVar, Var2Lval),

        list.filter((pred(ReuseTag::in) is semidet :-
                ReuseTag \= PrimaryTag
            ), ReusePrimaryTags, DifferentTags),
        (
            DifferentTags = [],
            Var2Rval = lval(Var2Lval)
        ;
            DifferentTags = [ReusePrimaryTag],
            % The body operator is slightly more efficient than the strip_tag
            % operator so we use it when the old tag is known.
            Var2Rval = mkword(PrimaryTag,
                binop(body, lval(Var2Lval), ml_gen_mktag(ReusePrimaryTag)))
        ;
            DifferentTags = [_, _ | _],
            Var2Rval = mkword(PrimaryTag,
                unop(std_unop(strip_tag), lval(Var2Lval)))
        ),

        Statement = ml_gen_assign(Var1Lval, Var2Rval, Context),

        % For each field in the construction unification we need to generate
        % an rval.
        % XXX we do more work than we need to here, as some of the cells
        % may already contain the correct values.
        %
        ml_gen_unify_args(ConsId, ArgVars, ArgModes, ArgTypes, Fields, Type,
            VarLval, OffSet, ArgNum, ConsIdTag, Context, Statements0, !Info),

        Decls = [],
        Statements = [Statement | Statements0]
    ).

:- pred ml_gen_field_take_address_assigns(list(take_addr_info)::in,
    mlds_lval::in, mlds_type::in, maybe(mlds_tag)::in, prog_context::in,
    ml_gen_info::in, list(statement)::out) is det.

ml_gen_field_take_address_assigns([], _, _, _, _, _, []).
ml_gen_field_take_address_assigns([TakeAddrInfo | TakeAddrInfos],
        CellLval, CellType, MaybeTag, Context, Info, [Assign | Assigns]) :-
    TakeAddrInfo = take_addr_info(AddrVar, Offset, ConsArgType, FieldType),
    % XXX
    % I am not sure that the types specified here are always the right ones,
    % particularly in cases where the field whose address we are taking has
    % a non-du type such as int or float. However, I can't think of a test case
    % in which a predicate fills in a field of such a type after a *recursive*
    % call, since recursive calls tend to generate values of recursive (i.e.
    % discriminated union) types. -zs
    SourceRval = mem_addr(field(MaybeTag, lval(CellLval),
        offset(const(int_const(Offset))), FieldType, CellType)),
    ml_gen_var(Info, AddrVar, AddrLval),
    CastSourceRval = unop(cast(mlds_ptr_type(ConsArgType)), SourceRval),
    Assign = ml_gen_assign(AddrLval, CastSourceRval, Context),
    ml_gen_field_take_address_assigns(TakeAddrInfos, CellLval, CellType,
        MaybeTag, Context, Info, Assigns).

    % Return the MLDS type suitable for constructing a constant static
    % ground term with the specified cons_id.
    %
:- func get_type_for_cons_id(mlds_type, bool, maybe(cons_id), bool, globals)
    = mlds_type.

get_type_for_cons_id(MLDS_Type, UsesBaseClass, MaybeConsId, HighLevelData,
        Globals) = ConstType :-
    (
        HighLevelData = no,
        ConstType = mlds_array_type(mlds_generic_type)
    ;
        HighLevelData = yes,
        (
            % Check for type_infos and typeclass_infos,
            % since these need to be handled specially;
            % their Mercury type definitions are lies.
            MLDS_Type = mercury_type(_, TypeCategory, _),
            ( TypeCategory = type_cat_type_info
            ; TypeCategory = type_cat_type_ctor_info
            ; TypeCategory = type_cat_typeclass_info
            ; TypeCategory = type_cat_base_typeclass_info
            )
        ->
            ConstType = mlds_array_type(mlds_generic_type)
        ;
            % Check if we're constructing a value for a discriminated union
            % where the specified cons_id which is represented as a derived
            % class that is derived from the base class for this discriminated
            % union type.
            UsesBaseClass = no,
            MaybeConsId = yes(ConsId),
            ConsId = cons(CtorSymName, CtorArity),
            (
                MLDS_Type = mlds_class_type(QualTypeName, TypeArity, _)
            ;
                MLDS_Type = mercury_type(MercuryType, type_cat_user_ctor, _),
                type_to_ctor_and_args(MercuryType, TypeCtor, _ArgsTypes),
                ml_gen_type_name(TypeCtor, QualTypeName, TypeArity)
            )
        ->
            % If so, append the name of the derived class to the name of the
            % base class for this type (since the derived class will also be
            % nested inside the base class).
            unqualify_name(CtorSymName, CtorName),
            QualTypeName = qual(MLDS_Module, _QualKind, TypeName),
            ClassQualifier = mlds_append_class_qualifier(MLDS_Module,
                module_qual, Globals, TypeName, TypeArity),
            ConstType = mlds_class_type(
                qual(ClassQualifier, type_qual, CtorName),
                CtorArity, mlds_class)
        ;
            % Convert mercury_types for user-defined types to the corresponding
            % `mlds_class_type'. This is needed because these types get
            % mapped to `mlds_ptr_type(mlds_class_type(...))', but when
            % declarating static constants we want just the class type,
            % not the pointer type.
            MLDS_Type = mercury_type(MercuryType, type_cat_user_ctor, _),
            type_to_ctor_and_args(MercuryType, TypeCtor, _ArgsTypes)
        ->
            ml_gen_type_name(TypeCtor, ClassName, ClassArity),
            ConstType = mlds_class_type(ClassName, ClassArity, mlds_class)
        ;
            % For tuples, a similar issue arises; we want tuple constants
            % to have array type, not the pointer type MR_Tuple.
            MLDS_Type = mercury_type(_, type_cat_tuple, _)
        ->
            ConstType = mlds_array_type(mlds_generic_type)
        ;
            % Likewise for closures, we need to use an array type rather than
            % the pointer type MR_ClosurePtr. Note that we use a low-level
            % data representation for closures, even when --high-level-data
            % is enabled.
            MLDS_Type = mercury_type(_, type_cat_higher_order, _)
        ->
            ConstType = mlds_array_type(mlds_generic_type)
        ;
            ConstType = MLDS_Type
        )
    ).

:- pred ml_gen_field_type(ml_gen_info::in, mer_type::in, mer_type::out)
    is det.

ml_gen_field_type(Info, Type, FieldType) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    ml_type_as_field(Type, ModuleInfo, HighLevelData, FieldType).

:- pred ml_type_as_field(mer_type::in, module_info::in, bool::in,
    mer_type::out) is det.

ml_type_as_field(FieldType, ModuleInfo, HighLevelData, BoxedFieldType) :-
    (
        % With the low-level data representation, we store all fields as boxed,
        % so we ignore the original field type and instead generate a
        % polymorphic type BoxedFieldType which we use for the type of the
        % field. This type is used in the calls to ml_gen_box_or_unbox_rval
        % to ensure that we box values when storing them into fields and
        % unbox them when extracting them from fields.
        %
        % With the high-level data representation, we don't box everything,
        % but for the MLDS->C and MLDS->asm back-ends we still need to box
        % floating point fields.
        %
        (
            HighLevelData = no
        ;
            HighLevelData = yes,
            ml_must_box_field_type(FieldType, ModuleInfo)
        )
    ->
        varset.init(TypeVarSet0),
        varset.new_var(TypeVarSet0, TypeVar, _TypeVarSet),
        % The kind is `star' since there are values with this type.
        BoxedFieldType = variable(TypeVar, star)
    ;
        BoxedFieldType = FieldType
    ).

:- pred get_maybe_cons_id_arg_types(maybe(cons_id)::in, list(mer_type)::in,
    mer_type::in, module_info::in, list(mer_type)::out) is det.

get_maybe_cons_id_arg_types(MaybeConsId, ArgTypes, Type, ModuleInfo,
        ConsArgTypes) :-
    (
        MaybeConsId = yes(ConsId),
        ConsArgTypes = constructor_arg_types(ConsId, ArgTypes, Type,
            ModuleInfo)
    ;
        MaybeConsId = no,
        % It's a closure. In this case, the arguments are all boxed.
        ConsArgTypes = ml_make_boxed_types(list.length(ArgTypes))
    ).

:- func constructor_arg_types(cons_id, list(mer_type), mer_type, module_info)
    = list(mer_type).

constructor_arg_types(CtorId, ArgTypes, Type, ModuleInfo) = ConsArgTypes :-
    (
        CtorId = cons(_, _),
        \+ is_introduced_type_info_type(Type)
    ->
        % Use the type to determine the type_ctor
        ( type_to_ctor_and_args(Type, TypeCtor0, _) ->
            TypeCtor = TypeCtor0
        ;
            % The type-checker should ensure that this never happens:
            % the type for a ctor_id should never be a free type variable.
            unexpected(this_file, "constructor_arg_types: invalid type")
        ),

        % Given the type_ctor, lookup up the constructor.
        (
            type_util.get_cons_defn(ModuleInfo, TypeCtor, CtorId, ConsDefn)
        ->
            ConsDefn = hlds_cons_defn(_, _, ConsArgDefns, _, _),
            assoc_list.values(ConsArgDefns, ConsArgTypes0),

            % There may have been additional types inserted to hold the
            % type_infos and type_class_infos for existentially quantified
            % types. We can get these from the ArgTypes.

            NumExtraArgs = list.length(ArgTypes) - list.length(ConsArgTypes0),
            ExtraArgTypes = list.take_upto(NumExtraArgs, ArgTypes),
            ConsArgTypes = ExtraArgTypes ++ ConsArgTypes0
        ;
            % If we didn't find a constructor definition, maybe that is because
            % this type was a built-in tuple type.
            type_is_tuple(Type, _)
        ->
            % In this case, the argument types are all fresh variables.
            % Note that we don't need to worry about using the right varset
            % here, since all we really care about at this point is whether
            % something is a type variable or not, not which type variable it
            % is.
            ConsArgTypes = ml_make_boxed_types(list.length(ArgTypes))
        ;
            % Type_util.get_cons_defn shouldn't have failed.
            unexpected(this_file, "cons_id_to_arg_types: get_cons_defn failed")
        )
    ;
        % For cases when CtorId \= cons(_, _) and it is not a tuple, as can
        % happen e.g. for closures and type_infos, we assume that the arguments
        % all have the right type already.
        % XXX is this the right thing to do?
        ArgTypes = ConsArgTypes
    ).

:- func ml_gen_mktag(int) = mlds_rval.

ml_gen_mktag(Tag) = unop(std_unop(mktag), const(int_const(Tag))).

:- pred ml_gen_box_or_unbox_const_rval_list(list(mer_type)::in,
    list(mer_type)::in, list(mlds_rval)::in, prog_context::in,
    mlds_defns::out, list(mlds_rval)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_box_or_unbox_const_rval_list(ArgTypes, FieldTypes, ArgRvals,
        Context, BoxConstDefns, FieldRvals, !Info) :-
    (
        ArgTypes = [],
        FieldTypes = [],
        ArgRvals = []
    ->
        BoxConstDefns = [], FieldRvals = []
    ;
        ArgTypes = [ArgType | ArgTypes1],
        FieldTypes = [FieldType | FieldTypes1],
        ArgRvals = [ArgRval | ArgRvals1]
    ->
        (
            % Handle the case where the field type is a boxed type
            % -- in that case, we can just box the argument type.
            FieldType = variable(_, _)
        ->
            ml_gen_type(!.Info, ArgType, MLDS_ArgType),
            ml_gen_box_const_rval(MLDS_ArgType, ArgRval, Context,
                BoxConstDefns0, FieldRval, !Info)
        ;
            % Otherwise, fall back on ml_gen_box_or_unbox_rval.
            % XXX This might still generate stuff which is not legal
            % in a static initializer!
            ml_gen_box_or_unbox_rval(ArgType, FieldType, native_if_possible,
                ArgRval, FieldRval, !Info),
            BoxConstDefns0 = []
        ),
        ml_gen_box_or_unbox_const_rval_list(ArgTypes1, FieldTypes1, ArgRvals1,
            Context, BoxConstDefns1, FieldRvals1, !Info),
        BoxConstDefns = BoxConstDefns0 ++ BoxConstDefns1,
        FieldRvals = [FieldRval | FieldRvals1]
    ;
        unexpected(this_file,
            "ml_gen_box_or_unbox_const_rval_list: list length mismatch")
    ).

:- pred ml_gen_box_const_rval_list(list(mlds_type)::in, list(mlds_rval)::in,
    prog_context::in, mlds_defns::out, list(mlds_rval)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_box_const_rval_list([], [], _, [], [], !Info).
ml_gen_box_const_rval_list([Type | Types], [Rval | Rvals], Context,
        ConstDefns, [BoxedRval | BoxedRvals], !Info) :-
    ml_gen_box_const_rval(Type, Rval, Context, ConstDefns1, BoxedRval, !Info),
    ml_gen_box_const_rval_list(Types, Rvals, Context, ConstDefns2,
        BoxedRvals, !Info),
    ConstDefns = ConstDefns1 ++ ConstDefns2.
ml_gen_box_const_rval_list([], [_|_], _, _, _, !Info) :-
    unexpected(this_file, "ml_gen_box_const_rval_list: length mismatch").
ml_gen_box_const_rval_list([_|_], [], _, _, _, !Info) :-
    unexpected(this_file, "ml_gen_box_const_rval_list: length mismatch").

:- pred ml_gen_box_const_rval(mlds_type::in, mlds_rval::in, prog_context::in,
    mlds_defns::out, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_box_const_rval(Type, Rval, Context, ConstDefns, BoxedRval, !Info) :-
    (
        ( Type = mercury_type(variable(_, _), _, _)
        ; Type = mlds_generic_type
        )
    ->
        BoxedRval = Rval,
        ConstDefns = []
    ;
        % For the MLDS->C and MLDS->asm back-ends, we need to handle floats
        % specially, since boxed floats normally get heap allocated, whereas
        % for other types boxing is just a cast (casts are OK in static
        % initializers, but calls to malloc() are not).
        %
        % [For the .NET and Java back-ends, this code currently never gets
        % called, since currently we don't support static ground term
        % optimization for those back-ends.]
        %
        ( Type = mercury_type(builtin(float), _, _)
        ; Type = mlds_native_float_type
        )
    ->
        % Generate a local static constant for this float.
        ml_gen_info_new_const(SequenceNum, !Info),
        ml_gen_info_get_pred_id(!.Info, PredId),
        ml_gen_info_get_proc_id(!.Info, ProcId),
        pred_id_to_int(PredId, PredIdNum),
        proc_id_to_int(ProcId, ProcIdNum),
        ConstName = mlds_var_name(string.format("float_%d_%d_%d",
            [i(PredIdNum), i(ProcIdNum), i(SequenceNum)]), no),
        Initializer = init_obj(Rval),
        ConstDefn = ml_gen_static_const_defn(ConstName, Type,
            local, Initializer, Context),
        ConstDefns = [ConstDefn],

        % Return as the boxed rval the address of that constant,
        % cast to mlds_generic_type.
        ml_gen_var_lval(!.Info, ConstName, Type, ConstLval),
        ConstAddrRval = mem_addr(ConstLval),
        BoxedRval = unop(cast(mlds_generic_type), ConstAddrRval)
    ;
        BoxedRval = unop(box(Type), Rval),
        ConstDefns = []
    ).

:- pred ml_gen_static_const_arg_list(list(prog_var)::in, list(static_cons)::in,
    list(mlds_rval)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_static_const_arg_list([], [], [], !Info).
ml_gen_static_const_arg_list([Var | Vars], [StaticCons | StaticConses],
        [Rval | Rvals], !Info) :-
    ml_gen_static_const_arg(Var, StaticCons, Rval, !Info),
    ml_gen_static_const_arg_list(Vars, StaticConses, Rvals, !Info).
ml_gen_static_const_arg_list([_|_], [], _, !Info) :-
    unexpected(this_file, "ml_gen_static_const_arg_list: length mismatch").
ml_gen_static_const_arg_list([], [_|_], _, !Info) :-
    unexpected(this_file, "ml_gen_static_const_arg_list: length mismatch").

    % Generate the name of the local static constant for a given variable.
    %
:- pred ml_gen_static_const_name(prog_var::in, mlds_var_name::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_static_const_name(Var, ConstName, !Info) :-
    ml_gen_info_new_const(SequenceNum, !Info),
    ml_gen_info_get_varset(!.Info, VarSet),
    VarName = ml_gen_var_name(VarSet, Var),
    ml_format_static_const_name(!.Info, ml_var_name_to_string(VarName),
        SequenceNum, ConstName),
    ml_gen_info_set_const_var_name(Var, ConstName, !Info).

:- pred ml_lookup_static_const_name(ml_gen_info::in, prog_var::in,
    mlds_var_name::out) is det.

ml_lookup_static_const_name(Info, Var, ConstName) :-
    ml_gen_info_lookup_const_var_name(Info, Var, ConstName).

    % Generate an rval containing the address of the local static constant
    % for a given variable.
    %
:- pred ml_gen_static_const_addr(ml_gen_info::in, prog_var::in,
    mlds_type::in, mlds_rval::out) is det.

ml_gen_static_const_addr(Info, Var, Type, ConstAddrRval) :-
    ml_lookup_static_const_name(Info, Var, ConstName),
    ml_gen_var_lval(Info, ConstName, Type, ConstLval),
    ConstAddrRval = mem_addr(ConstLval).

:- pred ml_cons_name(cons_id::in, ctor_name::out) is det.

ml_cons_name(HLDS_ConsId, QualifiedConsId) :-
    (
        HLDS_ConsId = cons(SymName, Arity),
        SymName = qualified(SymModuleName, ConsName)
    ->
        ConsId = ctor_id(ConsName, Arity),
        ModuleName = mercury_module_name_to_mlds(SymModuleName)
    ;
        ConsName = hlds_out.cons_id_to_string(HLDS_ConsId),
        ConsId = ctor_id(ConsName, 0),
        ModuleName = mercury_module_name_to_mlds(unqualified(""))
    ),
    QualifiedConsId = qual(ModuleName, module_qual, ConsId).

:- type take_addr_info
    --->    take_addr_info(
                prog_var,           % The variable we record the address in.
                int,                % The offset of the field
                mlds_type,          % The type of the field variable.
                mlds_type           % The type of the field, possibly
                                    % after boxing.
            ).

    % Create a list of rvals for the arguments for a construction unification.
    % For each argument which is input to the construction unification,
    % we produce the corresponding lval, boxed or unboxed if needed,
    % but if the argument is free, we produce a null value.
    %
:- pred ml_gen_cons_args(list(prog_var)::in, list(mlds_lval)::in,
    list(mer_type)::in, list(mer_type)::in, list(uni_mode)::in,
    int::in, int::in, list(int)::in, module_info::in, list(mlds_rval)::out,
    list(mlds_type)::out, list(take_addr_info)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_cons_args(Vars, Lvals, ArgTypes, ConsArgTypes, UniModes, FirstOffset,
        FirstArgNum, TakeAddr, ModuleInfo, Rvals, MLDS_Types, TakeAddrInfos,
        !Info) :-
    (
        ml_gen_cons_args_2(Vars, Lvals, ArgTypes, ConsArgTypes, UniModes,
            FirstOffset, FirstArgNum, TakeAddr, ModuleInfo, RvalsPrime,
            MLDS_TypesPrime, TakeAddrInfosPrime, !Info)
    ->
        Rvals = RvalsPrime,
        MLDS_Types = MLDS_TypesPrime,
        TakeAddrInfos = TakeAddrInfosPrime
    ;
        unexpected(this_file, "ml_gen_cons_args: length mismatch")
    ).

:- pred ml_gen_cons_args_2(list(prog_var)::in, list(mlds_lval)::in,
    list(mer_type)::in, list(mer_type)::in, list(uni_mode)::in,
    int::in, int::in, list(int)::in, module_info::in, list(mlds_rval)::out,
    list(mlds_type)::out, list(take_addr_info)::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

ml_gen_cons_args_2([], [], [], [], [], _FirstOffset, _FirstArgNum, _TakeAddr,
        _ModuleInfo, [], [], [], !Info).
ml_gen_cons_args_2([Var | Vars], [Lval | Lvals], [ArgType | ArgTypes],
        [ConsArgType | ConsArgTypes], [UniMode | UniModes], FirstOffset,
        CurArgNum, !.TakeAddr, ModuleInfo, [Rval | Rvals],
        [MLDS_Type | MLDS_Types], TakeAddrInfos, !Info) :-
    % Figure out the type of the field. Note that for the MLDS->C and
    % MLDS->asm back-ends, we need to box floating point fields.
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    ml_type_as_field(ConsArgType, ModuleInfo, HighLevelData, BoxedArgType),
    MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, BoxedArgType),

    % Compute the value of the field.
    UniMode = ((_LI - RI) -> (_LF - RF)),
    ( !.TakeAddr = [CurArgNum | !:TakeAddr] ->
        Rval = const(null(MLDS_Type)),
        ml_gen_cons_args_2(Vars, Lvals, ArgTypes, ConsArgTypes, UniModes,
            FirstOffset, CurArgNum + 1, !.TakeAddr, ModuleInfo, Rvals,
            MLDS_Types, TakeAddrInfosTail, !Info),
        % Whereas CurArgNum starts numbering the arguments from 1, offsets
        % into fields start from zero. However, if FirstOffset > 0, then the
        % cell contains FirstOffset other things (e.g. a secondary tag) before
        % the first argument.
        Offset = CurArgNum - 1 + FirstOffset,
        OrigMLDS_Type = mercury_type_to_mlds_type(ModuleInfo, ConsArgType),
        TakeAddrInfo = take_addr_info(Var, Offset, OrigMLDS_Type, MLDS_Type),
        TakeAddrInfos = [TakeAddrInfo | TakeAddrInfosTail]
    ;
        (
            mode_to_arg_mode(ModuleInfo, (RI -> RF), ArgType, top_in),
            not is_dummy_argument_type(ModuleInfo, ArgType),
            not is_dummy_argument_type(ModuleInfo, ConsArgType)
        ->
            ml_gen_box_or_unbox_rval(ArgType, BoxedArgType, native_if_possible,
                lval(Lval), Rval, !Info)
        ;
            Rval = const(null(MLDS_Type))
        ),
        ml_gen_cons_args_2(Vars, Lvals, ArgTypes, ConsArgTypes, UniModes,
            FirstOffset, CurArgNum + 1, !.TakeAddr, ModuleInfo, Rvals,
            MLDS_Types, TakeAddrInfos, !Info)
    ).

%-----------------------------------------------------------------------------%

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
:- pred ml_gen_det_deconstruct(prog_var::in, cons_id::in, prog_vars::in,
    list(uni_mode)::in, prog_context::in,
    mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_det_deconstruct(Var, ConsId, Args, Modes, Context, Decls, Statements,
        !Info) :-
    Decls = [],
    ml_variable_type(!.Info, Var, Type),
    ml_cons_id_to_tag(!.Info, ConsId, Type, Tag),
    ml_gen_det_deconstruct_2(Tag, Type, Var, ConsId, Args, Modes, Context,
        Statements, !Info).

:- pred ml_gen_det_deconstruct_2(cons_tag::in, mer_type::in, prog_var::in,
    cons_id::in, prog_vars::in, list(uni_mode)::in, prog_context::in,
    statements::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_det_deconstruct_2(Tag, Type, Var, ConsId, Args, Modes, Context,
        Statements, !Info) :-
    % For constants, if the deconstruction is det, then we already know
    % the value of the constant, so Statements = [].
    (
        Tag = string_constant(_String),
        Statements = []
    ;
        Tag = int_constant(_Int),
        Statements = []
    ;
        Tag = float_constant(_Float),
        Statements = []
    ;
        Tag = pred_closure_tag(_, _, _),
        Statements = []
    ;
        Tag = type_ctor_info_constant(_, _, _),
        Statements = []
    ;
        Tag = base_typeclass_info_constant(_, _, _),
        Statements = []
    ;
        Tag = tabling_pointer_constant(_, _),
        Statements = []
    ;
        Tag = deep_profiling_proc_layout_tag(_, _),
        Statements = []
    ;
        Tag = table_io_decl_tag(_, _),
        Statements = []
    ;
        Tag = no_tag,
        (
            Args = [Arg],
            Modes = [Mode]
        ->
            ml_variable_type(!.Info, Arg, ArgType),
            ml_gen_var(!.Info, Arg, ArgLval),
            ml_gen_var(!.Info, Var, VarLval),
            ml_gen_sub_unify(Mode, ArgLval, ArgType, VarLval, Type,
                Context, [], Statements, !Info)
        ;
            unexpected(this_file, "ml_code_gen: no_tag: arity != 1")
        )
    ;
        Tag = single_functor,
        ml_gen_var(!.Info, Var, VarLval),
        ml_variable_types(!.Info, Args, ArgTypes),
        ml_field_names_and_types(!.Info, Type, ConsId, ArgTypes, Fields),
        ml_tag_offset_and_argnum(Tag, _, OffSet, ArgNum),
        ml_gen_unify_args(ConsId, Args, Modes, ArgTypes, Fields, Type,
            VarLval, OffSet, ArgNum, Tag, Context, Statements, !Info)
    ;
        Tag = unshared_tag(_UnsharedTag),
        ml_gen_var(!.Info, Var, VarLval),
        ml_variable_types(!.Info, Args, ArgTypes),
        ml_field_names_and_types(!.Info, Type, ConsId, ArgTypes, Fields),
        ml_tag_offset_and_argnum(Tag, _, OffSet, ArgNum),
        ml_gen_unify_args(ConsId, Args, Modes, ArgTypes, Fields, Type,
            VarLval, OffSet, ArgNum, Tag, Context, Statements, !Info)
    ;
        Tag = shared_remote_tag(_PrimaryTag, _SecondaryTag),
        ml_gen_var(!.Info, Var, VarLval),
        ml_variable_types(!.Info, Args, ArgTypes),
        ml_field_names_and_types(!.Info, Type, ConsId, ArgTypes, Fields),
        ml_tag_offset_and_argnum(Tag, _, OffSet, ArgNum),
        ml_gen_unify_args(ConsId, Args, Modes, ArgTypes, Fields, Type,
            VarLval, OffSet, ArgNum, Tag, Context, Statements, !Info)
    ;
        % For constants, if the deconstruction is det, then we already
        % know the value of the constant, so Statements = [].
        Tag = shared_local_tag(_Bits1, _Num1),
        Statements = []
    ;
        % For constants, if the deconstruction is det, then we already
        % know the value of the constant, so Statements = [].
        Tag = reserved_address(_),
        Statements = []
    ;
        % For shared_with_reserved_address, the sharing is only important
        % for tag tests, not for det deconstructions, so here we just recurse
        % on the real representation.
        Tag = shared_with_reserved_addresses(_, ThisTag),
        ml_gen_det_deconstruct_2(ThisTag, Type, Var, ConsId, Args,
            Modes, Context, Statements, !Info)
    ).

    % Calculate the integer offset used to reference the first field of a
    % structure for lowlevel data or the first argument number to access
    % the field using the highlevel data representation. Abort if the tag
    % indicates that the data doesn't have any fields.
    %
:- pred ml_tag_offset_and_argnum(cons_tag::in, tag_bits::out,
    int::out, int::out) is det.

ml_tag_offset_and_argnum(Tag, TagBits, OffSet, ArgNum) :-
    (
        Tag = single_functor,
        TagBits = 0,
        OffSet = 0,
        ArgNum = 1
    ;
        Tag = unshared_tag(UnsharedTag),
        TagBits = UnsharedTag,
        OffSet = 0,
        ArgNum = 1
    ;
        Tag = shared_remote_tag(PrimaryTag, _SecondaryTag),
        TagBits = PrimaryTag,
        OffSet = 1,
        ArgNum = 1
    ;
        Tag = shared_with_reserved_addresses(_, ThisTag),
        % Just recurse on ThisTag.
        ml_tag_offset_and_argnum(ThisTag, TagBits, OffSet, ArgNum)
    ;
        Tag = string_constant(_String),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = int_constant(_Int),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = float_constant(_Float),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = pred_closure_tag(_, _, _),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = type_ctor_info_constant(_, _, _),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = base_typeclass_info_constant(_, _, _),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = tabling_pointer_constant(_, _),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = deep_profiling_proc_layout_tag(_, _),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = table_io_decl_tag(_, _),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = no_tag,
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = shared_local_tag(_Bits1, _Num1),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ;
        Tag = reserved_address(_),
        unexpected(this_file, "ml_tag_offset_and_argnum")
    ).

    % Given a type and a cons_id, and also the types of the actual arguments
    % of that cons_id in some particular use of it, look up the original types
    % of the fields of that cons_id from the type definition. Note that the
    % field types need not be the same as the actual argument types; for
    % polymorphic types, the types of the actual arguments can be an instance
    % of the field types.
    %
:- pred ml_field_names_and_types(ml_gen_info::in, mer_type::in, cons_id::in,
    list(mer_type)::in, list(constructor_arg)::out) is det.

ml_field_names_and_types(Info, Type, ConsId, ArgTypes, Fields) :-
    % Lookup the field types for the arguments of this cons_id.
    MakeUnnamedField = (func(FieldType) = no - FieldType),
    (
        type_is_tuple(Type, _),
        list.length(ArgTypes, TupleArity)
    ->
        % The argument types for tuples are unbound type variables.
        FieldTypes = ml_make_boxed_types(TupleArity),
        Fields = list.map(MakeUnnamedField, FieldTypes)
    ;
        ml_gen_info_get_module_info(Info, ModuleInfo),
        type_util.get_type_and_cons_defn(ModuleInfo, Type, ConsId,
            _TypeDefn, ConsDefn),
        ConsDefn = hlds_cons_defn(_, _, Fields0, _, _),

        % Add the fields for any type_infos and/or typeclass_infos inserted
        % for existentially quantified data types. For these, we just copy
        % the types from the ArgTypes.
        NumArgs = list.length(ArgTypes),
        NumFieldTypes0 = list.length(Fields0),
        NumExtraTypes = NumArgs - NumFieldTypes0,
        ExtraFieldTypes = list.take_upto(NumExtraTypes, ArgTypes),
        ExtraFields = list.map(MakeUnnamedField, ExtraFieldTypes),
        Fields = ExtraFields ++ Fields0
    ).

:- pred ml_gen_unify_args(cons_id::in, prog_vars::in, list(uni_mode)::in,
    list(mer_type)::in, list(constructor_arg)::in, mer_type::in,
    mlds_lval::in, int::in, int::in, cons_tag::in, prog_context::in,
    statements::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_unify_args(ConsId, Args, Modes, ArgTypes, Fields, VarType, VarLval,
        Offset, ArgNum, Tag, Context, Statements, !Info) :-
    (
        ml_gen_unify_args_2(ConsId, Args, Modes, ArgTypes, Fields,
            VarType, VarLval, Offset, ArgNum, Tag, Context,
            [], Statements0, !Info)
    ->
        Statements = Statements0
    ;
        unexpected(this_file, "ml_gen_unify_args: length mismatch")
    ).

:- pred ml_gen_unify_args_2(cons_id::in, prog_vars::in, list(uni_mode)::in,
    list(mer_type)::in, list(constructor_arg)::in, mer_type::in,
    mlds_lval::in, int::in, int::in, cons_tag::in, prog_context::in,
    statements::in, statements::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

ml_gen_unify_args_2(_, [], [], [], _, _, _, _, _, _, _, !Statements, !Info).
ml_gen_unify_args_2(ConsId, [Arg | Args], [Mode | Modes], [ArgType | ArgTypes],
        [Field | Fields], VarType, VarLval, Offset, ArgNum, Tag,
        Context, !Statements, !Info) :-
    Offset1 = Offset + 1,
    ArgNum1 = ArgNum + 1,
    ml_gen_unify_args_2(ConsId, Args, Modes, ArgTypes, Fields, VarType,
        VarLval, Offset1, ArgNum1, Tag, Context, !Statements, !Info),
    ml_gen_unify_arg(ConsId, Arg, Mode, ArgType, Field, VarType, VarLval,
        Offset, ArgNum, Tag, Context, !Statements, !Info).

:- pred ml_gen_unify_arg(cons_id::in, prog_var::in, uni_mode::in, mer_type::in,
    constructor_arg::in, mer_type::in, mlds_lval::in, int::in, int::in,
    cons_tag::in, prog_context::in,
    statements::in, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_unify_arg(ConsId, Arg, Mode, ArgType, Field, VarType, VarLval,
        Offset, ArgNum, Tag, Context, !Statements, !Info) :-
    Field = MaybeFieldName - FieldType,
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    (
        % With the low-level data representation, we access all fields
        % using offsets.
        HighLevelData = no,
        FieldId = offset(const(int_const(Offset)))
    ;
        % With the high-level data representation, we always used named fields,
        % except for tuple types.
        HighLevelData = yes,
        globals.get_target(Globals, Target),
        (
            ( type_is_tuple(VarType, _)
            ; type_needs_lowlevel_rep(Target, VarType)
            )
        ->
            FieldId = offset(const(int_const(Offset)))
        ;
            FieldName = ml_gen_field_name(MaybeFieldName, ArgNum),
            (
                ConsId = cons(ConsName, ConsArity)
            ->
                unqualify_name(ConsName, UnqualConsName),
                FieldId = ml_gen_field_id(VarType, Tag, UnqualConsName,
                    ConsArity, FieldName, Globals)
            ;
                unexpected(this_file, "ml_gen_unify_args: invalid cons_id")
            )
        )
    ),
    % Box the field type, if needed.
    ml_type_as_field(FieldType, ModuleInfo, HighLevelData,
        BoxedFieldType),

    % Generate lvals for the LHS and the RHS.
    ml_gen_type(!.Info, VarType, MLDS_VarType),
    ml_gen_type(!.Info, BoxedFieldType, MLDS_BoxedFieldType),
    MaybePrimaryTag = get_primary_tag(Tag),
    FieldLval = field(MaybePrimaryTag, lval(VarLval), FieldId,
        MLDS_BoxedFieldType, MLDS_VarType),
    ml_gen_var(!.Info, Arg, ArgLval),

    % Now generate code to unify them.
    ml_gen_sub_unify(Mode, ArgLval, ArgType, FieldLval, BoxedFieldType,
        Context, !Statements, !Info).

:- pred ml_gen_sub_unify(uni_mode::in, mlds_lval::in, mer_type::in,
    mlds_lval::in, mer_type::in, prog_context::in,
    statements::in, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_sub_unify(Mode, ArgLval, ArgType, FieldLval, FieldType, Context,
        !Statements, !Info) :-
    % Figure out the direction of data-flow from the mode,
    % and generate code accordingly.
    Mode = ((LI - RI) -> (LF - RF)),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    mode_to_arg_mode(ModuleInfo, (LI -> LF), ArgType, LeftMode),
    mode_to_arg_mode(ModuleInfo, (RI -> RF), ArgType, RightMode),
    (
        % Skip dummy argument types, since they will not have been declared.
        ( is_dummy_argument_type(ModuleInfo, ArgType)
        ; is_dummy_argument_type(ModuleInfo, FieldType)
        )
    ->
        true
    ;
        % Both input: it's a test unification.
        LeftMode = top_in,
        RightMode = top_in
    ->
        % This shouldn't happen, since mode analysis should avoid creating
        % any tests in the arguments of a construction or deconstruction
        % unification.
        unexpected(this_file, "test in arg of [de]construction")
    ;
        % Input - output: it's an assignment to the RHS.
        LeftMode = top_in,
        RightMode = top_out
    ->
        ml_gen_box_or_unbox_rval(FieldType, ArgType, native_if_possible,
            lval(FieldLval), FieldRval, !Info),
        Statement = ml_gen_assign(ArgLval, FieldRval, Context),
        !:Statements = [Statement | !.Statements]
    ;
        % Output - input: it's an assignment to the LHS.
        LeftMode = top_out,
        RightMode = top_in
    ->
        ml_gen_box_or_unbox_rval(ArgType, FieldType, native_if_possible,
            lval(ArgLval), ArgRval, !Info),
        Statement = ml_gen_assign(FieldLval, ArgRval, Context),
        !:Statements = [Statement | !.Statements]
    ;
        % Unused - unused: the unification has no effect.
        LeftMode = top_unused,
        RightMode = top_unused
    ->
        true
    ;
        unexpected(this_file, "ml_gen_sub_unify: some strange unify")
    ).

%-----------------------------------------------------------------------------%

    % Generate a semidet deconstruction. A semidet deconstruction unification
    % is tag test followed by a deterministic deconstruction (which is executed
    % only if the tag test succeeds).
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
:- pred ml_gen_semi_deconstruct(prog_var::in, cons_id::in, prog_vars::in,
    list(uni_mode)::in, prog_context::in,
    mlds_defns::out, statements::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_semi_deconstruct(Var, ConsId, Args, ArgModes, Context,
        Decls, Statements, !Info) :-
    ml_gen_tag_test(Var, ConsId, TagTestDecls, TagTestStatements,
        TagTestExpression, !Info),
    ml_gen_set_success(!.Info, TagTestExpression, Context,
        SetTagTestResult),
    ml_gen_test_success(!.Info, SucceededExpression),
    ml_gen_det_deconstruct(Var, ConsId, Args, ArgModes, Context,
        GetArgsDecls, GetArgsStatements, !Info),
    (
        GetArgsDecls = [],
        GetArgsStatements = []
    ->
        Decls = TagTestDecls,
        Statements = TagTestStatements ++ [SetTagTestResult]
    ;
        GetArgs = ml_gen_block(GetArgsDecls, GetArgsStatements, Context),
        IfStmt = if_then_else(SucceededExpression, GetArgs, no),
        IfStatement = statement(IfStmt, mlds_make_context(Context)),
        Decls = TagTestDecls,
        Statements = TagTestStatements ++ [SetTagTestResult, IfStatement]
    ).

    % ml_gen_tag_test(Var, ConsId, Defns, Statements, Expression):
    %
    % Generate code to perform a tag test.
    %
    % The test checks whether Var has the functor specified by ConsId.
    % The generated code may contain Defns, Statements and an Expression.
    % The Expression is a boolean rval. After execution of the Statements,
    % Expression will evaluate to true iff the Var has the functor
    % specified by ConsId.
    %
    % TODO: apply the reverse tag test optimization for types with two
    % functors (see unify_gen.m).
    %
ml_gen_tag_test(Var, ConsId, TagTestDecls, TagTestStatements,
        TagTestExpression, !Info) :-
    ml_gen_var(!.Info, Var, VarLval),
    ml_variable_type(!.Info, Var, Type),
    ml_cons_id_to_tag(!.Info, ConsId, Type, Tag),
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    TagTestExpression = ml_gen_tag_test_rval(Tag, Type, ModuleInfo,
        lval(VarLval)),
    TagTestDecls = [],
    TagTestStatements = [].

    % ml_gen_tag_test_rval(Tag, VarType, ModuleInfo, VarRval) = TestRval:
    %
    % TestRval is a Rval of type bool which evaluates to true if VarRval has
    % the specified Tag and false otherwise. VarType is the type of VarRval.
    %
:- func ml_gen_tag_test_rval(cons_tag, mer_type, module_info, mlds_rval)
    = mlds_rval.

ml_gen_tag_test_rval(string_constant(String), _, _, Rval) =
    binop(str_eq, Rval, const(string_const(String))).
ml_gen_tag_test_rval(float_constant(Float), _, _, Rval) =
    binop(float_eq, Rval, const(float_const(Float))).
ml_gen_tag_test_rval(int_constant(Int), _, _, Rval) =
    binop(eq, Rval, const(int_const(Int))).
ml_gen_tag_test_rval(pred_closure_tag(_, _, _), _, _, _Rval) = _TestRval :-
    % This should never happen, since the error will be detected
    % during mode checking.
    unexpected(this_file, "Attempted higher-order unification").
ml_gen_tag_test_rval(type_ctor_info_constant(_, _, _), _, _, _) = _ :-
    unexpected(this_file, "Attempted type_ctor_info unification").
ml_gen_tag_test_rval(base_typeclass_info_constant(_, _, _), _, _, _) = _ :-
    unexpected(this_file, "Attempted base_typeclass_info unification").
ml_gen_tag_test_rval(tabling_pointer_constant(_, _), _, _, _) = _ :-
    unexpected(this_file, "Attempted tabling_pointer unification").
ml_gen_tag_test_rval(deep_profiling_proc_layout_tag(_, _), _, _, _) = _ :-
    unexpected(this_file, "Attempted deep_profiling_proc_layout unification").
ml_gen_tag_test_rval(table_io_decl_tag(_, _), _, _, _) = _ :-
    unexpected(this_file, "Attempted table_io_decl unification").
ml_gen_tag_test_rval(no_tag, _, _, _Rval) = const(true).
ml_gen_tag_test_rval(single_functor, _, _, _Rval) = const(true).
ml_gen_tag_test_rval(unshared_tag(UnsharedTag), _, _, Rval) =
    binop(eq, unop(std_unop(tag), Rval),
        unop(std_unop(mktag), const(int_const(UnsharedTag)))).
ml_gen_tag_test_rval(shared_remote_tag(PrimaryTagVal, SecondaryTagVal),
        VarType, ModuleInfo, Rval) = TagTest :-
    SecondaryTagField = ml_gen_secondary_tag_rval(PrimaryTagVal, VarType,
        ModuleInfo, Rval),
    SecondaryTagTest = binop(eq, SecondaryTagField,
        const(int_const(SecondaryTagVal))),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),
    ( NumTagBits = 0 ->
        % No need to test the primary tag.
        TagTest = SecondaryTagTest
    ;
        PrimaryTagTest = binop(eq,
            unop(std_unop(tag), Rval),
            unop(std_unop(mktag), const(int_const(PrimaryTagVal)))),
        TagTest = binop(logical_and, PrimaryTagTest, SecondaryTagTest)
    ).
ml_gen_tag_test_rval(shared_local_tag(Bits, Num), VarType, ModuleInfo, Rval) =
        TestRval :-
    MLDS_VarType = mercury_type_to_mlds_type(ModuleInfo, VarType),
    TestRval = binop(eq, Rval,
        unop(cast(MLDS_VarType), mkword(Bits,
        unop(std_unop(mkbody), const(int_const(Num)))))).
ml_gen_tag_test_rval(reserved_address(ReservedAddr), VarType, ModuleInfo,
        Rval) = TestRval :-
    MLDS_VarType = mercury_type_to_mlds_type(ModuleInfo, VarType),
    ReservedAddrRval = ml_gen_reserved_address(ModuleInfo, ReservedAddr,
        MLDS_VarType),
    TestRval = binop(eq, Rval, ReservedAddrRval).
ml_gen_tag_test_rval(shared_with_reserved_addresses(ReservedAddrs, ThisTag),
        VarType, ModuleInfo, Rval) = FinalTestRval :-
    % We first check that the Rval doesn't match any of the ReservedAddrs,
    % and then check that it matches ThisTag.
    CheckReservedAddrs = (func(RA, TestRval0) = TestRval :-
        EqualRA = ml_gen_tag_test_rval(reserved_address(RA), VarType,
            ModuleInfo, Rval),
        TestRval = ml_gen_and(ml_gen_not(EqualRA), TestRval0)
    ),
    MatchesThisTag = ml_gen_tag_test_rval(ThisTag, VarType, ModuleInfo, Rval),
    FinalTestRval = list.foldr(CheckReservedAddrs, ReservedAddrs,
        MatchesThisTag).

    % ml_gen_secondary_tag_rval(PrimaryTag, VarType, ModuleInfo, VarRval):
    %
    % Return the rval for the secondary tag field of VarRval, assuming that
    % VarRval has the specified VarType and PrimaryTag.
    %
ml_gen_secondary_tag_rval(PrimaryTagVal, VarType, ModuleInfo, Rval) =
        SecondaryTagField :-
    MLDS_VarType = mercury_type_to_mlds_type(ModuleInfo, VarType),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    (
        ( HighLevelData = no
        ; type_needs_lowlevel_rep(Target, VarType)
        )
    ->
        % Note: with the low-level data representation, all fields -- even
        % the secondary tag -- are boxed, and so we need to unbox (i.e. cast)
        % it back to the right type here.
        SecondaryTagField =
            unop(unbox(mlds_native_int_type),
                lval(field(yes(PrimaryTagVal), Rval,
                    offset(const(int_const(0))),
                    mlds_generic_type, MLDS_VarType)))
    ;
        FieldId = ml_gen_hl_tag_field_id(VarType, ModuleInfo),
        SecondaryTagField = lval(field(yes(PrimaryTagVal), Rval,
            FieldId, mlds_native_int_type, MLDS_VarType))
    ).

    % Return the field_id for the "data_tag" field of the specified
    % Mercury type, which holds the secondary tag.
    %
:- func ml_gen_hl_tag_field_id(mer_type, module_info) = mlds_field_id.

ml_gen_hl_tag_field_id(Type, ModuleInfo) = FieldId :-
    FieldName = "data_tag",
    % Figure out the type name and arity.
    ( type_to_ctor_and_args(Type, TypeCtor0, _) ->
        TypeCtor = TypeCtor0
    ;
        unexpected(this_file, "ml_gen_hl_tag_field_id: invalid type")
    ),
    ml_gen_type_name(TypeCtor, QualifiedTypeName, TypeArity),
    QualifiedTypeName = qual(MLDS_Module, TypeQualKind, TypeName),

    % Figure out whether this type has constructors both with and without
    % secondary tags. If so, then the secondary tag field is in a class
    % "tag_type" that is derived from the base class for this type,
    % rather than in the base class itself.
    module_info_get_globals(ModuleInfo, Globals),
    module_info_get_type_table(ModuleInfo, TypeTable),
    TypeDefn = map.lookup(TypeTable, TypeCtor),
    hlds_data.get_type_defn_body(TypeDefn, TypeDefnBody),
    ( TypeDefnBody = du_type(Ctors, TagValues, _, _, _ReservedTag, _) ->
        % XXX we probably shouldn't ignore ReservedTag here
        (
            some [Ctor] (
                list.member(Ctor, Ctors),
                ml_uses_secondary_tag(TagValues, Ctor, _)
            ),
            some [Ctor] (
                list.member(Ctor, Ctors),
                \+ ml_uses_secondary_tag(TagValues, Ctor, _)
            )
        ->
            ClassQualifier = mlds_append_class_qualifier(MLDS_Module,
                module_qual, Globals, TypeName, TypeArity),
            ClassQualKind = TypeQualKind,
            ClassName = "tag_type",
            ClassArity = 0
        ;
            ClassQualifier = MLDS_Module,
            ClassQualKind = module_qual,
            ClassName = TypeName,
            ClassArity = TypeArity
        )
    ;
        unexpected(this_file, "ml_gen_hl_tag_field_id: non-du type")
    ),

    % Put it all together.
    QualClassName = qual(ClassQualifier, ClassQualKind, ClassName),
    ClassPtrType = mlds_ptr_type(mlds_class_type(QualClassName, ClassArity,
        mlds_class)),
    FieldQualifier = mlds_append_class_qualifier(ClassQualifier,
        ClassQualKind, Globals, ClassName, ClassArity),
    QualifiedFieldName = qual(FieldQualifier, type_qual, FieldName),
    FieldId = named_field(QualifiedFieldName, ClassPtrType).

:- func ml_gen_field_id(mer_type, cons_tag, mlds_class_name, arity,
    mlds_field_name, globals) = mlds_field_id.

ml_gen_field_id(Type, Tag, ConsName, ConsArity, FieldName, Globals)
        = FieldId :-
    ( type_to_ctor_and_args(Type, TypeCtor, _) ->
        ml_gen_type_name(TypeCtor, QualTypeName, TypeArity),
        QualTypeName = qual(MLDS_Module, QualKind, TypeName),
        TypeQualifier = mlds_append_class_qualifier(
            MLDS_Module, QualKind, Globals, TypeName, TypeArity),

        ( ml_tag_uses_base_class(Tag) ->
            % In this case, there's only one functor for the type (other than
            % reserved_address constants), and so the class name is determined
            % by the type name.
            ClassPtrType = mlds_ptr_type(mlds_class_type(QualTypeName,
                TypeArity, mlds_class)),
            QualifiedFieldName = qual(TypeQualifier, type_qual, FieldName)
        ;
            % In this case, the class name is determined by the constructor.
            QualConsName = qual(TypeQualifier, type_qual, ConsName),
            ClassPtrType = mlds_ptr_type(mlds_class_type(QualConsName,
                ConsArity, mlds_class)),
            FieldQualifier = mlds_append_class_qualifier(TypeQualifier,
                type_qual, Globals, ConsName, ConsArity),
            QualifiedFieldName = qual(FieldQualifier, type_qual, FieldName)
        ),
        FieldId = named_field(QualifiedFieldName, ClassPtrType)
    ;
        unexpected(this_file, "ml_gen_field_id: invalid type")
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_unify_gen.m".

:- end_module ml_unify_gen.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
