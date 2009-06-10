%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_unify_gen.m
% Main author: fjh
%
% This module is part of the MLDS code generator.
% It handles MLDS code generation for unifications.
%
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
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Generate MLDS code for a unification.
    %
:- pred ml_gen_unification(unification::in, code_model::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

    % Convert a cons_id for a given type to a cons_tag.
    %
:- pred ml_cons_id_to_tag(ml_gen_info::in, cons_id::in, mer_type::in,
    cons_tag::out) is det.

    % ml_gen_tag_test(Var, ConsId, Defns, Statements, Expression):
    %
    % Generate code to perform a tag test.
    %
    % The test checks whether Var has the functor specified by ConsId.
    % The generated code may contain Defns, Statements and an Expression.
    % The Expression is a boolean rval. After execution of the Statements,
    % Expression will evaluate to true iff the Var has the functor specified
    % by ConsId.
    %
:- pred ml_gen_tag_test(prog_var::in, cons_id::in,
    list(mlds_defn)::out, list(statement)::out, mlds_rval::out,
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
    how_to_construct::in, prog_context::in, list(mlds_defn)::out,
    list(statement)::out, ml_gen_info::in, ml_gen_info::out) is det.

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

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

ml_gen_unification(Unification, CodeModel, Context, Decls, Statements,
        !Info) :-
    (
        Unification = assign(TargetVar, SourceVar),
        expect(unify(CodeModel, model_det), this_file,
            "ml_gen_unification: assign not det"),
        ml_variable_type(!.Info, TargetVar, Type),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        IsDummyType = check_dummy_type(ModuleInfo, Type),
        (
            % Skip dummy argument types, since they will not have been
            % declared.
            IsDummyType = is_dummy_type,
            Statements = []
        ;
            IsDummyType = is_not_dummy_type,
            ml_gen_var(!.Info, TargetVar, TargetLval),
            ml_gen_var(!.Info, SourceVar, SourceLval),
            Statement = ml_gen_assign(TargetLval, ml_lval(SourceLval),
                Context),
            Statements = [Statement]
        ),
        ( ml_gen_info_search_const_var_name(!.Info, SourceVar, Name) ->
            % If the source variable is a constant, so is the target after
            % this assignment.
            %
            % The mark_static_terms assumes that if SourceVar is a constant
            % term, then after this assignment unification TargetVar is a
            % constant term also. Therefore later constant terms may contain
            % TargetVar among their arguments. If we didn't copy the constant
            % info here, the construction of the later constant could cause
            % a code generator abort.
            ml_gen_info_set_const_var_name(TargetVar, Name, !Info)
        ;
            true
        ),
        Decls = []
    ;
        Unification = simple_test(VarA, VarB),
        expect(unify(CodeModel, model_semi), this_file,
            "ml_gen_unification: simple_test not semidet"),
        ml_variable_type(!.Info, VarA, Type),
        ( Type = builtin_type(builtin_type_string) ->
            EqualityOp = str_eq
        ; Type = builtin_type(builtin_type_float) ->
            EqualityOp = float_eq
        ;
            EqualityOp = eq
        ),
        ml_gen_var(!.Info, VarA, VarALval),
        ml_gen_var(!.Info, VarB, VarBLval),
        Test = ml_binop(EqualityOp, ml_lval(VarALval), ml_lval(VarBLval)),
        ml_gen_set_success(!.Info, Test, Context, Statement),
        Statements = [Statement],
        Decls = []
    ;
        Unification = construct(Var, ConsId, Args, ArgModes, HowToConstruct,
            _CellIsUnique, SubInfo),
        expect(unify(CodeModel, model_det), this_file,
            "ml_gen_unification: construct not det"),
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
                "ml_gen_unification: term size profiling not yet supported")
        ),
        ml_gen_construct(Var, ConsId, Args, ArgModes, TakeAddr, HowToConstruct,
            Context, Decls, Statements, !Info)
    ;
        Unification = deconstruct(Var, ConsId, Args, ArgModes, CanFail,
            CanCGC),
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
            % Note that we can deallocate a cell even if the unification fails;
            % it is the responsibility of the structure reuse phase to ensure
            % that this is safe.
            CanCGC = can_cgc,
            ml_gen_var(!.Info, Var, VarLval),
            % XXX Avoid strip_tag when we know what tag it will have.
            Delete = delete_object(
                ml_unop(std_unop(strip_tag), ml_lval(VarLval))),
            Stmt = ml_stmt_atomic(Delete),
            CGC_Statement = statement(Stmt, mlds_make_context(Context)),
            Statements0 = Unif_Statements ++ [CGC_Statement]
        ;
            CanCGC = cannot_cgc,
            Statements0 = Unif_Statements
        ),

        % We used to require that CodeModel = ExpectedCodeModel. But the
        % determinism field in the goal_info is allowed to be a conservative
        % approximation, so we need to handle the case were CodeModel is less
        % precise than ExpectedCodeModel.
        ml_gen_wrap_goal(CodeModel, ExpectedCodeModel, Context,
            Statements0, Statements, !Info)
    ;
        Unification = complicated_unify(_, _, _),
        % Simplify.m should have converted these into procedure calls.
        unexpected(this_file, "ml_gen_unification: complicated unify")
    ).

    % ml_gen_construct generates code for a construction unification.
    %
    % Note that the code for ml_gen_static_const_arg is very similar to
    % the code here, and any changes may need to be done in both places.
    %
:- pred ml_gen_construct(prog_var::in, cons_id::in, prog_vars::in,
    list(uni_mode)::in, list(int)::in, how_to_construct::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
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
    how_to_construct::in, prog_context::in, list(mlds_defn)::out,
    list(statement)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_construct_2(Tag, Type, Var, ConsId, Args, ArgModes, TakeAddr,
        HowToConstruct, Context, Decls, Statements, !Info) :-
    (
        % Types for which some other constructor has a reserved_address
        % -- that only makes a difference when deconstructing, so here we
        % ignore that, and just recurse on the representation for this
        % constructor.

        Tag = shared_with_reserved_addresses_tag(_, ThisTag),
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
            unexpected(this_file, "ml_gen_construct_2: no_tag: arity != 1")
        )
    ;
        % Lambda expressions.
        Tag = pred_closure_tag(PredId, ProcId, _EvalMethod),
        ml_gen_closure(PredId, ProcId, Var, Args, ArgModes, HowToConstruct,
            Context, Decls, Statements, !Info)
    ;
        % Ordinary compound terms.
        ( Tag = single_functor_tag
        ; Tag = unshared_tag(_TagVal)
        ; Tag = shared_remote_tag(_PrimaryTag, _SecondaryTag)
        ),
        type_to_ctor_and_args_det(Type, TypeCtor, _),
        ml_gen_compound(Tag, TypeCtor, ConsId, Var, Args, ArgModes, TakeAddr,
            HowToConstruct, Context, Decls, Statements, !Info)
    ;
        % Constants.
        ( Tag = int_tag(_)
        ; Tag = foreign_tag(_, _)
        ; Tag = float_tag(_)
        ; Tag = string_tag(_)
        ; Tag = reserved_address_tag(_)
        ; Tag = shared_local_tag(_, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ),
        (
            Args = [],
            ml_gen_var(!.Info, Var, VarLval),
            ml_gen_constant(Tag, Type, Rval, !Info),
            Statement = ml_gen_assign(VarLval, Rval, Context),
            Decls = [],
            Statements = [Statement]
        ;
            Args = [_ | _],
            unexpected(this_file, "ml_gen_construct_2: bad constant term")
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
:- pred ml_gen_static_const_arg(prog_var::in, static_cons::in, prog_context::in,
    list(mlds_defn)::out, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_static_const_arg(Var, StaticCons, Context, Defns, Rval, !Info) :-
    % Figure out how this argument is represented.
    StaticCons = static_cons(ConsId, _ArgVars, _StaticArgs),
    ml_variable_type(!.Info, Var, VarType),
    ml_cons_id_to_tag(!.Info, ConsId, VarType, Tag),
    ml_gen_static_const_arg_2(Tag, VarType, Var, StaticCons, Context, Defns,
        Rval, !Info).

:- pred ml_gen_static_const_arg_2(cons_tag::in, mer_type::in, prog_var::in,
    static_cons::in, prog_context::in, list(mlds_defn)::out, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_static_const_arg_2(Tag, VarType, Var, StaticCons, Context, Defns, Rval,
        !Info) :-
    StaticCons = static_cons(ConsId, ArgVars, StaticArgs),
    (
        % Types for which some other constructor has a reserved_address
        % -- that only makes a difference when constructing, so here
        % we ignore that, and just recurse on the representation for
        % this constructor.
        Tag = shared_with_reserved_addresses_tag(_, ThisTag),
        ml_gen_static_const_arg_2(ThisTag, VarType, Var, StaticCons,
            Context, Defns, Rval, !Info)
    ;
        Tag = no_tag,
        (
            ArgVars = [Arg],
            StaticArgs = [StaticArg]
        ->
            % Construct (statically) the argument, and then convert it
            % to the appropriate type.
            ml_gen_static_const_arg(Arg, StaticArg, Context, ArgDefns, ArgRval,
                !Info),
            ml_variable_type(!.Info, Arg, ArgType),
            ml_gen_info_get_module_info(!.Info, ModuleInfo),
            MLDS_ArgType = mercury_type_to_mlds_type(ModuleInfo, ArgType),
            ml_gen_box_const_rval(MLDS_ArgType, ArgRval, Context, BoxDefns,
                Rval, !Info),
            Defns = ArgDefns ++ BoxDefns
        ;
            unexpected(this_file,
                "ml_gen_static_const_arg_2: no_tag: arity != 1")
        )
    ;
        % Compound terms, including lambda expressions.
        ( Tag = pred_closure_tag(_, _, _), TagVal = 0
        ; Tag = single_functor_tag, TagVal = 0
        ; Tag = unshared_tag(TagVal)
        ; Tag = shared_remote_tag(TagVal, _SecondaryTag)
        ),
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
            TaggedRval = ml_mkword(TagVal, ConstAddrRval)
        ),
        Rval = ml_unop(cast(MLDS_VarType), TaggedRval),
        Defns = []
    ;
        ( Tag = string_tag(_)
        ; Tag = int_tag(_)
        ; Tag = float_tag(_)
        ; Tag = shared_local_tag(_, _)
        ; Tag = reserved_address_tag(_)
        ; Tag = foreign_tag(_, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ),
        (
            % If this argument is just a constant, then generate the rval
            % for the constant.
            StaticArgs = [],
            ml_gen_constant(Tag, VarType, Rval, !Info),
            Defns = []
        ;
            StaticArgs = [_ | _],
            unexpected(this_file,
                "ml_gen_static_const_arg_2: unknown compound term")
        )
    ).

    % Generate the rval for a given constant.
    %
:- pred ml_gen_constant(cons_tag::in, mer_type::in, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_constant(Tag, VarType, Rval, !Info) :-
    (
        Tag = string_tag(String),
        Rval = ml_const(mlconst_string(String))
    ;
        Tag = int_tag(Int),
        Rval = ml_const(mlconst_int(Int))
    ;
        Tag = foreign_tag(ForeignLang, ForeignTag),
        Rval = ml_const(mlconst_foreign(ForeignLang, ForeignTag,
            mlds_native_int_type))
    ;
        Tag = float_tag(Float),
        Rval = ml_const(mlconst_float(Float))
    ;
        Tag = shared_local_tag(Bits1, Num1),
        ml_gen_type(!.Info, VarType, MLDS_Type),
        Rval = ml_unop(cast(MLDS_Type), ml_mkword(Bits1,
            ml_unop(std_unop(mkbody), ml_const(mlconst_int(Num1)))))
    ;
        Tag = type_ctor_info_tag(ModuleName0, TypeName, TypeArity),
        ml_gen_type(!.Info, VarType, MLDS_VarType),
        ModuleName = fixup_builtin_module(ModuleName0),
        MLDS_Module = mercury_module_name_to_mlds(ModuleName),
        RttiTypeCtor = rtti_type_ctor(ModuleName, TypeName, TypeArity),
        DataAddr = data_addr(MLDS_Module,
            mlds_rtti(ctor_rtti_id(RttiTypeCtor, type_ctor_type_ctor_info))),
        Rval = ml_unop(cast(MLDS_VarType),
            ml_const(mlconst_data_addr(DataAddr)))
    ;
        Tag = base_typeclass_info_tag(ModuleName, ClassId, Instance),
        ml_gen_type(!.Info, VarType, MLDS_VarType),
        MLDS_Module = mercury_module_name_to_mlds(ModuleName),
        TCName = generate_class_name(ClassId),
        DataAddr = data_addr(MLDS_Module, mlds_rtti(tc_rtti_id(TCName,
            type_class_base_typeclass_info(ModuleName, Instance)))),
        Rval = ml_unop(cast(MLDS_VarType),
            ml_const(mlconst_data_addr(DataAddr)))
    ;
        Tag = tabling_info_tag(PredId, ProcId),
        ml_gen_type(!.Info, VarType, MLDS_VarType),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        ml_gen_pred_label(ModuleInfo, PredId, ProcId, PredLabel, PredModule),
        DataAddr = data_addr(PredModule,
            mlds_tabling_ref(mlds_proc_label(PredLabel, ProcId), tabling_info)),
        Rval = ml_unop(cast(MLDS_VarType),
            ml_const(mlconst_data_addr(DataAddr)))
    ;
        Tag = deep_profiling_proc_layout_tag(_, _),
        unexpected(this_file,
            "ml_gen_constant: deep_profiling_proc_layout_tag NYI")
    ;
        Tag = table_io_decl_tag(_, _),
        unexpected(this_file,
            "ml_gen_constant: table_io_decl_tag NYI")
    ;
        Tag = reserved_address_tag(ReservedAddr),
        ml_gen_info_get_module_info(!.Info, ModuleInfo),
        ml_gen_type(!.Info, VarType, MLDS_VarType),
        Rval = ml_gen_reserved_address(ModuleInfo, ReservedAddr, MLDS_VarType)
    ;
        Tag = shared_with_reserved_addresses_tag(_, ThisTag),
        % For shared_with_reserved_address, the sharing is only important for
        % tag tests, not for constructions, so here we just recurse on the
        % real representation.
        ml_gen_constant(ThisTag, VarType, Rval, !Info)
    ;
        % These tags, which are not (necessarily) constants, are handled
        % in ml_gen_construct and ml_gen_static_const_arg,
        % so we don't need to handle them here.
        (
            Tag = no_tag,
            unexpected(this_file, "ml_gen_constant: no_tag")
        ;
            Tag = single_functor_tag,
            unexpected(this_file, "ml_gen_constant: single_functor")
        ;
            Tag = unshared_tag(_),
            unexpected(this_file, "ml_gen_constant: unshared_tag")
        ;
            Tag = shared_remote_tag(_, _),
            unexpected(this_file, "ml_gen_constant: shared_remote_tag")
        ;
            Tag = pred_closure_tag(_, _, _),
            unexpected(this_file, "ml_gen_constant: pred_closure_tag")
        )
    ).

%-----------------------------------------------------------------------------%

ml_gen_reserved_address(ModuleInfo, ResAddr, MLDS_Type) = Rval :-
    (
        ResAddr = null_pointer,
        Rval = ml_const(mlconst_null(MLDS_Type))
    ;
        ResAddr = small_pointer(Int),
        Rval = ml_unop(cast(MLDS_Type), ml_const(mlconst_int(Int)))
    ;
        ResAddr = reserved_object(TypeCtor, QualCtorName, CtorArity),
        (
            QualCtorName = qualified(ModuleName, CtorName),
            module_info_get_globals(ModuleInfo, Globals),
            MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
            TypeCtor = type_ctor(TypeName, TypeArity),
            UnqualTypeName = unqualify_name(TypeName),
            MLDS_TypeName = mlds_append_class_qualifier(MLDS_ModuleName,
                module_qual, Globals, UnqualTypeName, TypeArity),
            Name = ml_format_reserved_object_name(CtorName, CtorArity),
            Rval0 = ml_const(mlconst_data_addr(
                data_addr(MLDS_TypeName, mlds_data_var(Name)))),

            % The MLDS type of the reserved object may be a class derived from
            % the base class for this Mercury type. So for some back-ends,
            % we need to insert a (down-)cast here to convert from the derived
            % class to the base class. In particular, this is needed to avoid
            % compiler warnings in the C code generated by the MLDS->C
            % back-end. But inserting the cast could slow down the generated
            % code for the .NET back-end (where the JIT probably doesn't
            % optimize downcasts). So we only do it if the back-end
            % requires it.

            globals.get_target(Globals, Target),
            SupportsInheritance = target_supports_inheritence(Target),
            (
                SupportsInheritance = yes,
                Rval = Rval0
            ;
                SupportsInheritance = no,
                CastMLDS_Type = mlds_ptr_type(mlds_class_type(
                    qual(MLDS_ModuleName, module_qual, UnqualTypeName),
                    TypeArity, mlds_class)),
                Rval = ml_unop(cast(CastMLDS_Type), Rval0)
            )
        ;
            QualCtorName = unqualified(_),
            unexpected(this_file,
                "unqualified ctor name in reserved_object")
        )
    ).

    % This should return `yes' iff downcasts are not needed.
    %
:- func target_supports_inheritence(compilation_target) = bool.

target_supports_inheritence(target_c) = no.
target_supports_inheritence(target_il) = yes.
target_supports_inheritence(target_java) = yes.
target_supports_inheritence(target_asm) = no.
target_supports_inheritence(target_x86_64) =
    unexpected(this_file, "target_x86_64 and --high-level-code").
target_supports_inheritence(target_erlang) =
    unexpected(this_file, "target erlang").

%-----------------------------------------------------------------------------%

    % Convert a cons_id for a given type to a cons_tag.
    %
ml_cons_id_to_tag(Info, ConsId, Type, Tag) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    Tag = cons_id_to_tag(ModuleInfo, Type, ConsId).

    % Generate code to construct a new object.
    %
:- pred ml_gen_compound(cons_tag::in, type_ctor::in, cons_id::in, prog_var::in,
    prog_vars::in, list(uni_mode)::in, list(int)::in, how_to_construct::in,
    prog_context::in, list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_compound(Tag, TypeCtor, ConsId, Var, ArgVars, ArgModes, TakeAddr,
        HowToConstruct, Context, Decls, Statements, !Info) :-
    % Get the primary and secondary tags.
    ( get_primary_tag(Tag) = yes(PrimaryTag0) ->
        PrimaryTag = PrimaryTag0
    ;
        unexpected(this_file, "ml_gen_compound: primary tag unknown")
    ),
    MaybeSecondaryTag = get_secondary_tag(Tag),

    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),

    % Figure out which class name to construct.
    ( ml_tag_uses_base_class(Tag) ->
        MaybeCtorName = no
    ;
        globals.get_target(Globals, CompilationTarget),
        ml_cons_name(CompilationTarget, TypeCtor, ConsId, CtorName),
        MaybeCtorName = yes(CtorName)
    ),

    % If there is a secondary tag, it goes in the first field.
    (
        MaybeSecondaryTag = yes(SecondaryTag),
        HasSecTag = yes,
        SecondaryTagRval0 = ml_const(mlconst_int(SecondaryTag)),
        SecondaryTagType0 = mlds_native_int_type,

        % With the low-level data representation, all fields -- even the
        % secondary tag -- are boxed, and so we need box it here.

        globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
        (
            HighLevelData = no,
            SecondaryTagRval = ml_unop(box(SecondaryTagType0),
                SecondaryTagRval0),
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
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, use_atomic_cells, UseAtomicCells),
        (
            UseAtomicCells = yes,
            MayUseAtomic0 = may_use_atomic_alloc
        ;
            UseAtomicCells = no,
            MayUseAtomic0 = may_not_use_atomic_alloc
        ),
        ml_gen_cons_args(ArgVars, ArgLvals, ArgTypes, ConsArgTypes, ArgModes,
            FirstOffset, 1, TakeAddr, ModuleInfo, ArgRvals0, MLDS_ArgTypes0,
            TakeAddrInfos, MayUseAtomic0, MayUseAtomic, !Info),

        % Insert the extra rvals at the start.
        ArgRvals = ExtraRvals ++ ArgRvals0,
        MLDS_ArgTypes = ExtraTypes ++ MLDS_ArgTypes0,

        % Compute the number of words to allocate.
        list.length(ArgRvals, NumArgs),
        SizeInWordsRval = ml_const(mlconst_int(NumArgs)),

        % Generate a `new_object' statement to dynamically allocate the memory
        % for this term from the heap. The `new_object' statement will also
        % initialize the fields of this term with the specified arguments.
        MakeNewObject = new_object(VarLval, MaybeTag, HasSecTag, MLDS_Type,
            yes(SizeInWordsRval), MaybeCtorName, ArgRvals, MLDS_ArgTypes,
            MayUseAtomic),
        Stmt = ml_stmt_atomic(MakeNewObject),
        Statement = statement(Stmt, mlds_make_context(Context)),

        ml_gen_field_take_address_assigns(TakeAddrInfos, VarLval, MLDS_Type,
            MaybeTag, Context, !.Info, TakeAddrStatements),
        Statements = [Statement | TakeAddrStatements],
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
        ml_gen_static_const_arg_list(ArgVars, StaticArgs, Context,
            StaticArgDefns, ArgRvals0, !Info),

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
        (
            MaybeCtorName = yes(_),
            UsesBaseClass = no
        ;
            MaybeCtorName = no,
            UsesBaseClass = yes
        ),
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
            acc_local, Initializer, Context),

        % Assign the address of the local static constant to the variable.
        ml_gen_static_const_addr(!.Info, Var, ConstType, ConstAddrRval),
        (
            MaybeTag = no,
            TaggedRval = ConstAddrRval
        ;
            MaybeTag = yes(_),
            TaggedRval = ml_mkword(Tag, ConstAddrRval)
        ),
        Rval = ml_unop(cast(MLDS_Type), TaggedRval),
        AssignStatement = ml_gen_assign(VarLval, Rval, Context),
        Decls = StaticArgDefns ++ BoxConstDefns ++ [ConstDefn],
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
        ml_variable_type(!.Info, ReuseVar, ReuseType),
        list.map(
            (pred(ReuseConsId::in, ReusePrimTag::out) is det :-
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

        list.filter(
            (pred(ReuseTag::in) is semidet :-
                ReuseTag \= PrimaryTag
            ), ReusePrimaryTags, DifferentTags),
        (
            DifferentTags = [],
            Var2Rval = ml_lval(Var2Lval)
        ;
            DifferentTags = [ReusePrimaryTag],
            % The body operator is slightly more efficient than the strip_tag
            % operator so we use it when the old tag is known.
            Var2Rval = ml_mkword(PrimaryTag,
                ml_binop(body,
                    ml_lval(Var2Lval),
                    ml_gen_mktag(ReusePrimaryTag)))
        ;
            DifferentTags = [_, _ | _],
            Var2Rval = ml_mkword(PrimaryTag,
                ml_unop(std_unop(strip_tag), ml_lval(Var2Lval)))
        ),

        ml_gen_type(!.Info, Type, MLDS_DestType),
        CastVar2Rval = ml_unop(cast(MLDS_DestType), Var2Rval),
        MLDS_Context = mlds_make_context(Context),
        AssignStatement = statement(
            ml_stmt_atomic(assign_if_in_heap(Var1Lval, CastVar2Rval)),
            MLDS_Context),

        % For each field in the construction unification we need to generate
        % an rval.  ExtraRvals need to be inserted at the start of the object.
        ml_gen_extra_arg_assign(ExtraRvals, ExtraTypes, Type, VarLval,
            0, ConsIdTag, Context, ExtraRvalStatements, !Info),
        % XXX we do more work than we need to here, as some of the cells
        % may already contain the correct values.
        ml_gen_unify_args_for_reuse(ConsId, ArgVars, ArgModes, ArgTypes,
            Fields, TakeAddr, Type, VarLval, OffSet, ArgNum, ConsIdTag,
            Context, FieldStatements, TakeAddrInfos, !Info),
        ml_gen_field_take_address_assigns(TakeAddrInfos, VarLval, MLDS_Type,
            MaybeTag, Context, !.Info, TakeAddrStatements),
        IfBody = statement(ml_stmt_block([],
            ExtraRvalStatements ++ FieldStatements ++ TakeAddrStatements),
            MLDS_Context),

        % If the reassignment isn't possible because the target is statically
        % allocated then fall back to dynamic allocation.
        ml_gen_new_object(MaybeConsId, Tag, HasSecTag, MaybeCtorName, Var,
            ExtraRvals, ExtraTypes, ArgVars, ArgModes, TakeAddr,
            construct_dynamically, Context, DynamicDecls, DynamicStmts, !Info),
        IfElse = statement(
            ml_stmt_block(DynamicDecls, DynamicStmts),
            MLDS_Context),

        IfStatement = statement(
            ml_stmt_if_then_else(ml_lval(Var1Lval), IfBody, yes(IfElse)),
            mlds_make_context(Context)),

        Statements = [AssignStatement, IfStatement],
        Decls = []
    ;
        HowToConstruct = construct_in_region(_RegVar),
        sorry(this_file, "ml_gen_new_object: " ++
            "implementation for construct_in_region is not available")
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
    SourceRval = ml_mem_addr(ml_field(MaybeTag, ml_lval(CellLval),
        ml_field_offset(ml_const(mlconst_int(Offset))), FieldType, CellType)),
    ml_gen_var(Info, AddrVar, AddrLval),
    CastSourceRval = ml_unop(cast(mlds_ptr_type(ConsArgType)), SourceRval),
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
            % Check for type_infos and typeclass_infos, since these
            % need to be handled specially; their Mercury type definitions
            % are lies.
            MLDS_Type = mercury_type(_, TypeCtorCategory, _),
            TypeCtorCategory = ctor_cat_system(_)
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
                MLDS_Type = mercury_type(MercuryType, ctor_cat_user(_), _),
                type_to_ctor_and_args(MercuryType, TypeCtor, _ArgsTypes),
                ml_gen_type_name(TypeCtor, QualTypeName, TypeArity)
            )
        ->
            % If so, append the name of the derived class to the name of the
            % base class for this type (since the derived class will also be
            % nested inside the base class).
            globals.get_target(Globals, CompilationTarget),
            QualTypeName = qual(_, _, UnqualTypeName),
            CtorName = ml_gen_du_ctor_name_unqual_type(CompilationTarget,
                UnqualTypeName, TypeArity, CtorSymName, CtorArity),
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
            MLDS_Type = mercury_type(MercuryType, ctor_cat_user(_), _),
            type_to_ctor_and_args(MercuryType, TypeCtor, _ArgsTypes)
        ->
            ml_gen_type_name(TypeCtor, ClassName, ClassArity),
            ConstType = mlds_class_type(ClassName, ClassArity, mlds_class)
        ;
            % For tuples, a similar issue arises; we want tuple constants
            % to have array type, not the pointer type MR_Tuple.
            MLDS_Type = mercury_type(_, ctor_cat_tuple, _)
        ->
            ConstType = mlds_array_type(mlds_generic_type)
        ;
            % Likewise for closures, we need to use an array type rather than
            % the pointer type MR_ClosurePtr. Note that we use a low-level
            % data representation for closures, even when --high-level-data
            % is enabled.
            MLDS_Type = mercury_type(_, ctor_cat_higher_order, _)
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
        BoxedFieldType = type_variable(TypeVar, kind_star)
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
            ConsArgDefns = ConsDefn ^ cons_args,
            ConsArgTypes0 = list.map(func(C) = C ^ arg_type, ConsArgDefns),

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

ml_gen_mktag(Tag) = ml_unop(std_unop(mktag), ml_const(mlconst_int(Tag))).

:- pred ml_gen_box_or_unbox_const_rval_list(list(mer_type)::in,
    list(mer_type)::in, list(mlds_rval)::in, prog_context::in,
    list(mlds_defn)::out, list(mlds_rval)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_box_or_unbox_const_rval_list(ArgTypes, FieldTypes, ArgRvals,
        Context, BoxConstDefns, FieldRvals, !Info) :-
    (
        ArgTypes = [],
        FieldTypes = [],
        ArgRvals = []
    ->
        BoxConstDefns = [],
        FieldRvals = []
    ;
        ArgTypes = [ArgType | ArgTypesTail],
        FieldTypes = [FieldType | FieldTypesTail],
        ArgRvals = [ArgRval | ArgRvalsTail]
    ->
        (
            % Handle the case where the field type is a boxed type
            % -- in that case, we can just box the argument type.
            FieldType = type_variable(_, _),
            ml_gen_type(!.Info, ArgType, MLDS_ArgType),
            ml_gen_box_const_rval(MLDS_ArgType, ArgRval, Context,
                BoxConstDefns0, FieldRval, !Info)
        ;
            ( FieldType = defined_type(_, _, _)
            ; FieldType = builtin_type(_)
            ; FieldType = tuple_type(_, _)
            ; FieldType = higher_order_type(_, _, _, _)
            ; FieldType = apply_n_type(_, _, _)
            ; FieldType = kinded_type(_, _)
            ),
            % Otherwise, fall back on ml_gen_box_or_unbox_rval.
            % XXX This might still generate stuff which is not legal
            % in a static initializer!
            ml_gen_box_or_unbox_rval(ArgType, FieldType, native_if_possible,
                ArgRval, FieldRval, !Info),
            BoxConstDefns0 = []
        ),
        ml_gen_box_or_unbox_const_rval_list(ArgTypesTail, FieldTypesTail,
            ArgRvalsTail, Context, BoxConstDefnsTail, FieldRvalsTail, !Info),
        BoxConstDefns = BoxConstDefns0 ++ BoxConstDefnsTail,
        FieldRvals = [FieldRval | FieldRvalsTail]
    ;
        unexpected(this_file,
            "ml_gen_box_or_unbox_const_rval_list: list length mismatch")
    ).

:- pred ml_gen_box_const_rval_list(list(mlds_type)::in, list(mlds_rval)::in,
    prog_context::in, list(mlds_defn)::out, list(mlds_rval)::out,
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
    list(mlds_defn)::out, mlds_rval::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_box_const_rval(Type, Rval, Context, ConstDefns, BoxedRval, !Info) :-
    (
        ( Type = mercury_type(type_variable(_, _), _, _)
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

        ( Type = mercury_type(builtin_type(builtin_type_float), _, _)
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
            acc_local, Initializer, Context),
        ConstDefns = [ConstDefn],

        % Return as the boxed rval the address of that constant,
        % cast to mlds_generic_type.
        ml_gen_var_lval(!.Info, ConstName, Type, ConstLval),
        ConstAddrRval = ml_mem_addr(ConstLval),
        BoxedRval = ml_unop(cast(mlds_generic_type), ConstAddrRval)
    ;
        BoxedRval = ml_unop(box(Type), Rval),
        ConstDefns = []
    ).

:- pred ml_gen_static_const_arg_list(list(prog_var)::in, list(static_cons)::in,
    prog_context::in, list(mlds_defn)::out, list(mlds_rval)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_static_const_arg_list([], [], _, [], [], !Info).
ml_gen_static_const_arg_list([Var | Vars], [StaticCons | StaticConses],
        Context, Defns, [Rval | Rvals], !Info) :-
    ml_gen_static_const_arg(Var, StaticCons, Context, VarDefns,
        Rval, !Info),
    ml_gen_static_const_arg_list(Vars, StaticConses, Context, VarsDefns,
        Rvals, !Info),
    Defns = VarDefns ++ VarsDefns.
ml_gen_static_const_arg_list([_ | _], [], _, _, _, !Info) :-
    unexpected(this_file, "ml_gen_static_const_arg_list: length mismatch").
ml_gen_static_const_arg_list([], [_ | _], _, _, _, !Info) :-
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
    ConstAddrRval = ml_mem_addr(ConstLval).

:- pred ml_cons_name(compilation_target::in, type_ctor::in,
    cons_id::in, ctor_name::out) is det.

ml_cons_name(CompilationTarget, TypeCtor, HLDS_ConsId, QualifiedConsId) :-
    (
        HLDS_ConsId = cons(ConsSymName, ConsArity),
        ConsSymName = qualified(SymModuleName, _)
    ->
        ConsName = ml_gen_du_ctor_name(CompilationTarget, TypeCtor,
            ConsSymName, ConsArity),
        ConsId = ctor_id(ConsName, ConsArity),
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
    may_use_atomic_alloc::in, may_use_atomic_alloc::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_cons_args(Vars, Lvals, ArgTypes, ConsArgTypes, UniModes, FirstOffset,
        FirstArgNum, TakeAddr, ModuleInfo, !:Rvals, !:MLDS_Types,
        !:TakeAddrInfos, !MayUseAtomic, !Info) :-
    (
        ml_gen_cons_args_2(Vars, Lvals, ArgTypes, ConsArgTypes, UniModes,
            FirstOffset, FirstArgNum, TakeAddr, ModuleInfo, !:Rvals,
            !:MLDS_Types, !:TakeAddrInfos, !MayUseAtomic, !Info)
    ->
        true
    ;
        unexpected(this_file, "ml_gen_cons_args: length mismatch")
    ).

:- pred ml_gen_cons_args_2(list(prog_var)::in, list(mlds_lval)::in,
    list(mer_type)::in, list(mer_type)::in, list(uni_mode)::in,
    int::in, int::in, list(int)::in, module_info::in, list(mlds_rval)::out,
    list(mlds_type)::out, list(take_addr_info)::out,
    may_use_atomic_alloc::in, may_use_atomic_alloc::out,
    ml_gen_info::in, ml_gen_info::out) is semidet.

ml_gen_cons_args_2([], [], [], [], [], _FirstOffset, _FirstArgNum, _TakeAddr,
        _ModuleInfo, [], [], [], !MayUseAtomic, !Info).
ml_gen_cons_args_2([Var | Vars], [Lval | Lvals], [ArgType | ArgTypes],
        [ConsArgType | ConsArgTypes], [UniMode | UniModes], FirstOffset,
        CurArgNum, !.TakeAddr, ModuleInfo, [Rval | Rvals],
        [MLDS_Type | MLDS_Types], TakeAddrInfos, !MayUseAtomic, !Info) :-
    % It is important to use ArgType instead of ConsArgType here. ConsArgType
    % is the declared type of the argument of the cons_id, while ArgType is
    % the actual type of the variable being assigned to the given slot.
    % ConsArgType may be a type such as pred_id, which is a user-defined type
    % that may not appear in atomic cells, while ArgType may be a type such
    % as int, which may appear in atomic cells. This is because the actual type
    % may see behind abstraction barriers, and may thus see that e.g. pred_id
    % is actually the same as integer.
    update_type_may_use_atomic_alloc(ModuleInfo, ArgType, !MayUseAtomic),

    % Figure out the type of the field. Note that for the MLDS->C and
    % MLDS->asm back-ends, we need to box floating point fields.
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    ml_type_as_field(ConsArgType, ModuleInfo, HighLevelData, BoxedArgType),
    MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, BoxedArgType),

    % Compute the value of the field.
    UniMode = ((_LI - RI) -> (_LF - RF)),
    ( !.TakeAddr = [CurArgNum | !:TakeAddr] ->
        Rval = ml_const(mlconst_null(MLDS_Type)),
        ml_gen_cons_args_2(Vars, Lvals, ArgTypes, ConsArgTypes, UniModes,
            FirstOffset, CurArgNum + 1, !.TakeAddr, ModuleInfo, Rvals,
            MLDS_Types, TakeAddrInfosTail, !MayUseAtomic, !Info),
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
            check_dummy_type(ModuleInfo, ArgType) = is_not_dummy_type,
            check_dummy_type(ModuleInfo, ConsArgType) = is_not_dummy_type
        ->
            ml_gen_box_or_unbox_rval(ArgType, BoxedArgType, native_if_possible,
                ml_lval(Lval), Rval, !Info)
        ;
            Rval = ml_const(mlconst_null(MLDS_Type))
        ),
        ml_gen_cons_args_2(Vars, Lvals, ArgTypes, ConsArgTypes, UniModes,
            FirstOffset, CurArgNum + 1, !.TakeAddr, ModuleInfo, Rvals,
            MLDS_Types, TakeAddrInfos, !MayUseAtomic, !Info)
    ).

    % Generate assignment statements for each of ExtraRvals into the object at
    % VarLval, beginning at Offset.
    %
:- pred ml_gen_extra_arg_assign(list(mlds_rval)::in,
    list(mlds_type)::in, mer_type::in, mlds_lval::in, int::in, cons_tag::in,
    prog_context::in, list(statement)::out, ml_gen_info::in, ml_gen_info::out)
    is det.

ml_gen_extra_arg_assign([_ | _], [], _, _, _, _, _, _, !Info) :-
    unexpected(this_file, "ml_gen_extra_arg_assign: length mismatch").
ml_gen_extra_arg_assign([], [_ | _], _, _, _, _, _, _, !Info) :-
    unexpected(this_file, "ml_gen_extra_arg_assign: length mismatch").
ml_gen_extra_arg_assign([], [], _, _, _, _, _, [], !Info).
ml_gen_extra_arg_assign([ExtraRval | ExtraRvals], [ExtraType | ExtraTypes],
        VarType, VarLval, Offset, ConsIdTag, Context,
        [Statement | Statements], !Info) :-
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    (
        HighLevelData = no
    ;
        HighLevelData = yes,
        sorry(this_file, "ml_gen_extra_arg_assign: high-level data")
    ),

    ml_gen_type(!.Info, VarType, MLDS_VarType),
    FieldId = ml_field_offset(ml_const(mlconst_int(Offset))),
    MaybePrimaryTag = get_primary_tag(ConsIdTag),
    FieldLval = ml_field(MaybePrimaryTag, ml_lval(VarLval), FieldId,
        ExtraType, MLDS_VarType),
    Statement = ml_gen_assign(FieldLval, ExtraRval, Context),

    ml_gen_extra_arg_assign(ExtraRvals, ExtraTypes, VarType, VarLval,
        Offset + 1, ConsIdTag, Context, Statements, !Info).

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
    list(mlds_defn)::out, list(statement)::out,
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
    list(statement)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_det_deconstruct_2(Tag, Type, Var, ConsId, Args, Modes, Context,
        Statements, !Info) :-
    % For constants, if the deconstruction is det, then we already know
    % the value of the constant, so Statements = [].
    (
        ( Tag = string_tag(_String)
        ; Tag = int_tag(_Int)
        ; Tag = foreign_tag(_, _)
        ; Tag = float_tag(_Float)
        ; Tag = pred_closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ; Tag = shared_local_tag(_Bits1, _Num1)
        ; Tag = reserved_address_tag(_)
        ),
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
        ( Tag = single_functor_tag
        ; Tag = unshared_tag(_UnsharedTag)
        ; Tag = shared_remote_tag(_PrimaryTag, _SecondaryTag)
        ),
        ml_gen_var(!.Info, Var, VarLval),
        ml_variable_types(!.Info, Args, ArgTypes),
        ml_field_names_and_types(!.Info, Type, ConsId, ArgTypes, Fields),
        ml_tag_offset_and_argnum(Tag, _, OffSet, ArgNum),
        ml_gen_unify_args(ConsId, Args, Modes, ArgTypes, Fields, Type,
            VarLval, OffSet, ArgNum, Tag, Context, Statements, !Info)
    ;
        % For shared_with_reserved_address, the sharing is only important
        % for tag tests, not for det deconstructions, so here we just recurse
        % on the real representation.
        Tag = shared_with_reserved_addresses_tag(_, ThisTag),
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
        Tag = single_functor_tag,
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
        Tag = shared_with_reserved_addresses_tag(_, ThisTag),
        % Just recurse on ThisTag.
        ml_tag_offset_and_argnum(ThisTag, TagBits, OffSet, ArgNum)
    ;
        ( Tag = string_tag(_String)
        ; Tag = int_tag(_Int)
        ; Tag = foreign_tag(_, _)
        ; Tag = float_tag(_Float)
        ; Tag = pred_closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ; Tag = no_tag
        ; Tag = shared_local_tag(_Bits1, _Num1)
        ; Tag = reserved_address_tag(_)
        ),
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
    Context = term.context_init,
    MakeUnnamedField = (func(FieldType) = ctor_arg(no, FieldType, Context)),
    (
        type_is_tuple(Type, _),
        list.length(ArgTypes, TupleArity)
    ->
        % The argument types for tuples are unbound type variables.
        FieldTypes = ml_make_boxed_types(TupleArity),
        Fields = list.map(MakeUnnamedField, FieldTypes)
    ;
        ml_gen_info_get_module_info(Info, ModuleInfo),
        type_to_ctor_det(Type, TypeCtor),
        get_cons_defn_det(ModuleInfo, TypeCtor, ConsId, ConsDefn),
        Fields0 = ConsDefn ^ cons_args,

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
    list(statement)::out, ml_gen_info::in, ml_gen_info::out) is det.

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
    list(statement)::in, list(statement)::out,
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

:- pred ml_gen_unify_args_for_reuse(cons_id::in, prog_vars::in,
    list(uni_mode)::in, list(mer_type)::in, list(constructor_arg)::in,
    list(int)::in, mer_type::in, mlds_lval::in, int::in, int::in, cons_tag::in,
    prog_context::in, list(statement)::out, list(take_addr_info)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_unify_args_for_reuse(ConsId, Args, Modes, ArgTypes, Fields, TakeAddr,
        VarType, VarLval, Offset, ArgNum, Tag, Context,
        Statements, TakeAddrInfos, !Info) :-
    (
        Args = [],
        Modes = [],
        ArgTypes = [],
        Fields = []
    ->
        Statements = [],
        TakeAddrInfos = []
    ;
        Args = [Arg | Args1],
        Modes = [Mode | Modes1],
        ArgTypes = [ArgType | ArgTypes1],
        Fields = [Field | Fields1]
    ->
        Offset1 = Offset + 1,
        ArgNum1 = ArgNum + 1,
        ( TakeAddr = [ArgNum | TakeAddr1] ->
            ml_gen_unify_args_for_reuse(ConsId, Args1, Modes1, ArgTypes1,
                Fields1, TakeAddr1, VarType, VarLval, Offset1, ArgNum1,
                Tag, Context, Statements, TakeAddrInfos0, !Info),

            FieldType = Field ^ arg_type,
            ml_gen_info_get_module_info(!.Info, ModuleInfo),
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
            ml_type_as_field(FieldType, ModuleInfo, HighLevelData,
                BoxedFieldType),
            ml_gen_type(!.Info, FieldType, MLDS_FieldType),
            ml_gen_type(!.Info, BoxedFieldType, MLDS_BoxedFieldType),
            TakeAddrInfo = take_addr_info(Arg, Offset, MLDS_FieldType,
                MLDS_BoxedFieldType),
            TakeAddrInfos = [TakeAddrInfo | TakeAddrInfos0]
        ;
            ml_gen_unify_args_for_reuse(ConsId, Args1, Modes1, ArgTypes1,
                Fields1, TakeAddr, VarType, VarLval, Offset1, ArgNum1,
                Tag, Context, Statements0, TakeAddrInfos, !Info),
            ml_gen_unify_arg(ConsId, Arg, Mode, ArgType, Field,
                VarType, VarLval, Offset, ArgNum, Tag, Context,
                Statements0, Statements, !Info)
        )
    ;
        unexpected(this_file, "ml_gen_unify_args_for_reuse: length mismatch")
    ).

:- pred ml_gen_unify_arg(cons_id::in, prog_var::in, uni_mode::in, mer_type::in,
    constructor_arg::in, mer_type::in, mlds_lval::in, int::in, int::in,
    cons_tag::in, prog_context::in,
    list(statement)::in, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_unify_arg(ConsId, Arg, Mode, ArgType, Field, VarType, VarLval,
        Offset, ArgNum, Tag, Context, !Statements, !Info) :-
    MaybeFieldName = Field ^ arg_field_name,
    FieldType = Field ^ arg_type,
    ml_gen_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, highlevel_data, HighLevelData),
    (
        % With the low-level data representation, we access all fields
        % using offsets.
        HighLevelData = no,
        FieldId = ml_field_offset(ml_const(mlconst_int(Offset)))
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
            FieldId = ml_field_offset(ml_const(mlconst_int(Offset)))
        ;
            FieldName = ml_gen_field_name(MaybeFieldName, ArgNum),
            ( ConsId = cons(ConsName, ConsArity) ->
                globals.get_target(Globals, CompilationTarget),
                type_to_ctor_and_args_det(VarType, TypeCtor, _),
                UnqualConsName = ml_gen_du_ctor_name(CompilationTarget,
                    TypeCtor, ConsName, ConsArity),
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
    FieldLval = ml_field(MaybePrimaryTag, ml_lval(VarLval), FieldId,
        MLDS_BoxedFieldType, MLDS_VarType),
    ml_gen_var(!.Info, Arg, ArgLval),

    % Now generate code to unify them.
    ml_gen_sub_unify(Mode, ArgLval, ArgType, FieldLval, BoxedFieldType,
        Context, !Statements, !Info).

:- pred ml_gen_sub_unify(uni_mode::in, mlds_lval::in, mer_type::in,
    mlds_lval::in, mer_type::in, prog_context::in,
    list(statement)::in, list(statement)::out,
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
        ( check_dummy_type(ModuleInfo, ArgType) = is_dummy_type
        ; check_dummy_type(ModuleInfo, FieldType) = is_dummy_type
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
            ml_lval(FieldLval), FieldRval, !Info),
        Statement = ml_gen_assign(ArgLval, FieldRval, Context),
        !:Statements = [Statement | !.Statements]
    ;
        % Output - input: it's an assignment to the LHS.
        LeftMode = top_out,
        RightMode = top_in
    ->
        ml_gen_box_or_unbox_rval(ArgType, FieldType, native_if_possible,
            ml_lval(ArgLval), ArgRval, !Info),
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
:- pred ml_gen_semi_deconstruct(prog_var::in, cons_id::in, prog_vars::in,
    list(uni_mode)::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out,
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
        is_empty(GetArgsDecls),
        is_empty(GetArgsStatements)
    ->
        Decls = TagTestDecls,
        Statements = TagTestStatements ++ [SetTagTestResult]
    ;
        GetArgs = ml_gen_block(GetArgsDecls, GetArgsStatements, Context),
        IfStmt = ml_stmt_if_then_else(SucceededExpression, GetArgs, no),
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
        ml_lval(VarLval)),
    TagTestDecls = [],
    TagTestStatements = [].

    % ml_gen_tag_test_rval(Tag, VarType, ModuleInfo, VarRval) = TestRval:
    %
    % TestRval is a Rval of type bool which evaluates to true if VarRval has
    % the specified Tag and false otherwise. VarType is the type of VarRval.
    %
:- func ml_gen_tag_test_rval(cons_tag, mer_type, module_info, mlds_rval)
    = mlds_rval.

ml_gen_tag_test_rval(Tag, Type, ModuleInfo, Rval) = TagTestRval :-
    (
        Tag = string_tag(String),
        TagTestRval = ml_binop(str_eq, Rval, ml_const(mlconst_string(String)))
    ;
        Tag = float_tag(Float),
        TagTestRval = ml_binop(float_eq, Rval, ml_const(mlconst_float(Float)))
    ;
        Tag = int_tag(Int),
        TagTestRval = ml_binop(eq, Rval, ml_const(mlconst_int(Int)))
    ;
        Tag = foreign_tag(ForeignLang, ForeignVal),
        Const = ml_const(mlconst_foreign(ForeignLang, ForeignVal,
            mlds_native_int_type)),
        TagTestRval = ml_binop(eq, Rval, Const)
    ;
        ( Tag = pred_closure_tag(_, _, _)
        ; Tag = type_ctor_info_tag(_, _, _)
        ; Tag = base_typeclass_info_tag(_, _, _)
        ; Tag = tabling_info_tag(_, _)
        ; Tag = deep_profiling_proc_layout_tag(_, _)
        ; Tag = table_io_decl_tag(_, _)
        ),
        unexpected(this_file, "ml_gen_tag_test_rval: bad tag")
    ;
        Tag = no_tag,
        TagTestRval = ml_const(mlconst_true)
    ;
        Tag = single_functor_tag,
        TagTestRval = ml_const(mlconst_true)
    ;
        Tag = unshared_tag(UnsharedTagNum),
        RvalTag = ml_unop(std_unop(tag), Rval),
        UnsharedTag = ml_unop(std_unop(mktag),
            ml_const(mlconst_int(UnsharedTagNum))),
        TagTestRval = ml_binop(eq, RvalTag, UnsharedTag)
    ;
        Tag = shared_remote_tag(PrimaryTagNum, SecondaryTagNum),
        SecondaryTagField = ml_gen_secondary_tag_rval(PrimaryTagNum, Type,
            ModuleInfo, Rval),
        SecondaryTagTestRval = ml_binop(eq, SecondaryTagField,
            ml_const(mlconst_int(SecondaryTagNum))),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_int_option(Globals, num_tag_bits, NumTagBits),
        ( NumTagBits = 0 ->
            % No need to test the primary tag.
            TagTestRval = SecondaryTagTestRval
        ;
            RvalPTag = ml_unop(std_unop(tag), Rval),
            PrimaryTagRval = ml_unop(std_unop(mktag),
                ml_const(mlconst_int(PrimaryTagNum))),
            PrimaryTagTestRval = ml_binop(eq, RvalPTag, PrimaryTagRval),
            TagTestRval = ml_binop(logical_and,
                PrimaryTagTestRval, SecondaryTagTestRval)
        )
    ;
        Tag = shared_local_tag(Bits, Num),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
        TagTestRval = ml_binop(eq, Rval,
            ml_unop(cast(MLDS_Type),
                ml_mkword(Bits,
                    ml_unop(std_unop(mkbody), ml_const(mlconst_int(Num))))))
    ;
        Tag = reserved_address_tag(ReservedAddr),
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type),
        ReservedAddrRval = ml_gen_reserved_address(ModuleInfo, ReservedAddr,
            MLDS_Type),
        TagTestRval = ml_binop(eq, Rval, ReservedAddrRval)
    ;
        Tag = shared_with_reserved_addresses_tag(ReservedAddrs, ThisTag),
        % We first check that the Rval doesn't match any of the ReservedAddrs,
        % and then check that it matches ThisTag.
        CheckReservedAddrs = (func(RA, TestRval0) = TestRval :-
            EqualRA = ml_gen_tag_test_rval(reserved_address_tag(RA), Type,
                ModuleInfo, Rval),
            TestRval = ml_gen_and(ml_gen_not(EqualRA), TestRval0)
        ),
        MatchesThisTag = ml_gen_tag_test_rval(ThisTag, Type, ModuleInfo, Rval),
        TagTestRval = list.foldr(CheckReservedAddrs, ReservedAddrs,
            MatchesThisTag)
    ).

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
            ml_unop(unbox(mlds_native_int_type),
                ml_lval(ml_field(yes(PrimaryTagVal), Rval,
                    ml_field_offset(ml_const(mlconst_int(0))),
                    mlds_generic_type, MLDS_VarType)))
    ;
        FieldId = ml_gen_hl_tag_field_id(VarType, ModuleInfo),
        SecondaryTagField = ml_lval(ml_field(yes(PrimaryTagVal), Rval,
            FieldId, mlds_native_int_type, MLDS_VarType))
    ).

    % Return the field_id for the "data_tag" field of the specified
    % Mercury type, which holds the secondary tag.
    %
:- func ml_gen_hl_tag_field_id(mer_type, module_info) = mlds_field_id.

ml_gen_hl_tag_field_id(Type, ModuleInfo) = FieldId :-
    FieldName = "data_tag",
    % Figure out the type name and arity.
    type_to_ctor_and_args_det(Type, TypeCtor, _),
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
    (
        TypeDefnBody =
            hlds_du_type(Ctors, TagValues, _, _, _, _ReservedTag, _, _),
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
        ( TypeDefnBody = hlds_eqv_type(_)
        ; TypeDefnBody = hlds_foreign_type(_)
        ; TypeDefnBody = hlds_solver_type(_, _)
        ; TypeDefnBody = hlds_abstract_type(_)
        ),
        unexpected(this_file, "ml_gen_hl_tag_field_id: non-du type")
    ),

    % Put it all together.
    QualClassName = qual(ClassQualifier, ClassQualKind, ClassName),
    ClassPtrType = mlds_ptr_type(mlds_class_type(QualClassName, ClassArity,
        mlds_class)),
    FieldQualifier = mlds_append_class_qualifier(ClassQualifier,
        ClassQualKind, Globals, ClassName, ClassArity),
    QualifiedFieldName = qual(FieldQualifier, type_qual, FieldName),
    FieldId = ml_field_named(QualifiedFieldName, ClassPtrType).

:- func ml_gen_field_id(mer_type, cons_tag, mlds_class_name, arity,
    mlds_field_name, globals) = mlds_field_id.

ml_gen_field_id(Type, Tag, ConsName, ConsArity, FieldName, Globals)
        = FieldId :-
    type_to_ctor_and_args_det(Type, TypeCtor, _),
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
    FieldId = ml_field_named(QualifiedFieldName, ClassPtrType).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_unify_gen.m".

%-----------------------------------------------------------------------------%
:- end_module ml_unify_gen.
%-----------------------------------------------------------------------------%
