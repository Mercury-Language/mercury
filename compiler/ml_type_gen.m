%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: ml_type_gen.m
% Main author: fjh
% 
% MLDS type generation -- convert HLDS types to MLDS.
% 
% For enumerations, we use a Java-style emulation: we convert them
% to classes with a single int member, plus a bunch of static (one_copy)
% const members for the different enumerations consts.
%
% For discriminated unions, we create an MLDS base class type corresponding
% to the HLDS type, and we also create MLDS derived class types corresponding
% to each of the constructors which are defined from the base class type.
% For constructors which are represented as the addresses of specially reserved
% objects, we generate the static (one_copy) members for those objects.
% 
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_type_gen.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module io.

%-----------------------------------------------------------------------------%

    % Generate MLDS definitions for all the types in the HLDS.
    %
:- pred ml_gen_types(module_info::in, mlds_defns::out, io::di, io::uo) is det.

    % Given an HLDS type_ctor, generate the MLDS class name and arity
    % for the corresponding MLDS type.
    %
:- pred ml_gen_type_name(type_ctor::in, mlds_class::out, arity::out) is det.

    % Return the declaration flags appropriate for a type.
    %
:- func ml_gen_type_decl_flags = mlds_decl_flags.

    % Return the declaration flags appropriate for an enumeration constant.
    %
:- func ml_gen_enum_constant_decl_flags = mlds_decl_flags.

    % Return the declaration flags appropriate for a member variable.
    %
:- func ml_gen_member_decl_flags = mlds_decl_flags.

    % Return the declaration flags appropriate for a member of a class
    % that was transformed from a special predicate.  These differ
    % from normal members in that their finality is `final'.
    %
:- func ml_gen_special_member_decl_flags = mlds_decl_flags.

    % ml_uses_secondary_tag(ConsTagValues, Ctor, SecondaryTag):
    % Check if this constructor uses a secondary tag,
    % and if so, return the secondary tag value.
    %
:- pred ml_uses_secondary_tag(cons_tag_values::in, constructor::in, int::out)
    is semidet.

    % A constructor is represented using the base class rather than a derived
    % class if there is only a single functor, or if there is a single
    % functor and some constants represented using reserved addresses.
    %
:- pred ml_tag_uses_base_class(cons_tag::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.polymorphism.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

ml_gen_types(ModuleInfo, MLDS_TypeDefns, !IO) :-
    globals.io_lookup_bool_option(highlevel_data, HighLevelData, !IO),
    globals.io_get_target(Target, !IO),
    (
        HighLevelData = yes,
        module_info_get_type_table(ModuleInfo, TypeTable),
        map.keys(TypeTable, TypeCtors0),
        list.filter((pred(TypeCtor::in) is semidet :-
                \+ type_ctor_needs_lowlevel_rep(Target, TypeCtor)
            ), TypeCtors0, TypeCtors),
        list.foldl(ml_gen_type_defn(ModuleInfo, TypeTable), TypeCtors,
            [], MLDS_TypeDefns)
    ;
        HighLevelData = no,
        MLDS_TypeDefns = []
    ).

:- pred ml_gen_type_defn(module_info::in, type_table::in, type_ctor::in,
    mlds_defns::in, mlds_defns::out) is det.

ml_gen_type_defn(ModuleInfo, TypeTable, TypeCtor, MLDS_Defns0, MLDS_Defns) :-
    map.lookup(TypeTable, TypeCtor, TypeDefn),
    hlds_data.get_type_defn_status(TypeDefn, Status),
    DefinedThisModule = status_defined_in_this_module(Status),
    (
        DefinedThisModule = yes,
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        ml_gen_type_2(TypeBody, ModuleInfo, TypeCtor, TypeDefn,
            MLDS_Defns0, MLDS_Defns)
    ;
        DefinedThisModule = no,
        MLDS_Defns = MLDS_Defns0
    ).

:- pred ml_gen_type_2(hlds_type_body::in, module_info::in, type_ctor::in,
    hlds_type_defn::in, mlds_defns::in, mlds_defns::out) is det.

ml_gen_type_2(hlds_abstract_type(_), _, _, _, !Defns).
ml_gen_type_2(hlds_eqv_type(_EqvType), _, _, _, !Defns).
    % XXX Fixme!
    % For a description of the problems with equivalence types,
    % see our BABEL'01 paper "Compiling Mercury to the .NET CLR".
ml_gen_type_2(hlds_du_type(Ctors, TagValues, EnumDummy, MaybeUserEqComp,
        _ReservedTag, _), ModuleInfo, TypeCtor, TypeDefn, !Defns) :-
    % XXX we probably shouldn't ignore _ReservedTag
    ml_gen_equality_members(MaybeUserEqComp, MaybeEqualityMembers),
    (
        EnumDummy = is_enum,
        ml_gen_enum_type(TypeCtor, TypeDefn, Ctors, TagValues,
            MaybeEqualityMembers, !Defns)
    ;
        EnumDummy = is_dummy,
        % XXX We shouldn't have to generate an MLDS type for these types,
        % but it is not easy to ensure that we never refer to that type.
        ml_gen_enum_type(TypeCtor, TypeDefn, Ctors, TagValues,
            MaybeEqualityMembers, !Defns)
    ;
        EnumDummy = not_enum_or_dummy,
        ml_gen_du_parent_type(ModuleInfo, TypeCtor, TypeDefn,
            Ctors, TagValues, MaybeEqualityMembers, !Defns)
    ).
    % XXX Fixme!  Same issues here as for eqv_type/1.
ml_gen_type_2(hlds_foreign_type(_), _, _, _, !Defns).
ml_gen_type_2(hlds_solver_type(_, _), _, _, _, !Defns).

%-----------------------------------------------------------------------------%
%
% Enumeration types.
%

    % For each enumeration, we generate an MLDS type of the following form:
    %
    %   struct <ClassName> {
    %       static final const int <ctor1> = 0;
    %       static final const int <ctor2> = 1;
    %       ...
    %       int value;
    %   };
    %
    % It is marked as an mlds_enum so that the MLDS -> target code
    % generator can treat it specially if need be (e.g. generating
    % a C enum rather than a class).
    %
:- pred ml_gen_enum_type(type_ctor::in, hlds_type_defn::in,
    list(constructor)::in, cons_tag_values::in, mlds_defns::in,
    mlds_defns::in, mlds_defns::out) is det.

ml_gen_enum_type(TypeCtor, TypeDefn, Ctors, TagValues,
        MaybeEqualityMembers, MLDS_Defns0, MLDS_Defns) :-
    hlds_data.get_type_defn_context(TypeDefn, Context),
    MLDS_Context = mlds_make_context(Context),

    % Generate the class name.
    ml_gen_type_name(TypeCtor, QualifiedClassName, MLDS_ClassArity),
    QualifiedClassName = qual(_, _, MLDS_ClassName),

    % Generate the class members.
    ValueMember = ml_gen_enum_value_member(Context),
    EnumConstMembers = list.map(ml_gen_enum_constant(Context, TagValues),
        Ctors),
    Members = MaybeEqualityMembers ++ [ValueMember | EnumConstMembers],

    % Enums don't import or inherit anything.
    Imports = [],
    Inherits = [],
    Implements = [],

    % Put it all together.
    MLDS_TypeName = entity_type(MLDS_ClassName, MLDS_ClassArity),
    MLDS_TypeFlags = ml_gen_type_decl_flags,
    MLDS_TypeDefnBody = mlds_class(mlds_class_defn(mlds_enum,
        Imports, Inherits, Implements, [], Members)),
    MLDS_TypeDefn = mlds_defn(MLDS_TypeName, MLDS_Context, MLDS_TypeFlags,
        MLDS_TypeDefnBody),

    MLDS_Defns = [MLDS_TypeDefn | MLDS_Defns0].

:- func ml_gen_enum_value_member(prog_context) = mlds_defn.

ml_gen_enum_value_member(Context) =
    mlds_defn(entity_data(var(mlds_var_name("value", no))),
        mlds_make_context(Context),
        ml_gen_member_decl_flags,
        mlds_data(mlds_native_int_type, no_initializer, no)).

:- func ml_gen_enum_constant(prog_context, cons_tag_values, constructor)
    = mlds_defn.

ml_gen_enum_constant(Context, ConsTagValues, Ctor) = MLDS_Defn :-
    % Figure out the value of this enumeration constant.
    Ctor = ctor(_ExistQTVars, _Constraints, Name, Args),
    list.length(Args, Arity),
    map.lookup(ConsTagValues, cons(Name, Arity), TagVal),
    ( TagVal = int_tag(Int) ->
        ConstValue = const(mlconst_int(Int))
    ;
        unexpected(this_file,
            "ml_gen_enum_constant: enum constant needs int tag")
    ),
    % Sanity check.
    expect(unify(Arity, 0), this_file,
        "ml_gen_enum_constant: arity != []"),

    % Generate an MLDS definition for this enumeration constant.
    UnqualifiedName = unqualify_name(Name),
    MLDS_Defn = mlds_defn(entity_data(var(mlds_var_name(UnqualifiedName, no))),
        mlds_make_context(Context),
        ml_gen_enum_constant_decl_flags,
        mlds_data(mlds_native_int_type, init_obj(ConstValue), no)).

%-----------------------------------------------------------------------------%
%
% Discriminated union types.
%

    % For each discriminated union type, we generate an MLDS type of the
    % following form:
    %
    %   static class <ClassName> {
    %   public:
    % #if some_but_not_all_ctors_use_secondary_tag
    %       /* A nested derived class for the secondary tag */
    %       static class tag_type : public <ClassName> {
    %       public:
    % #endif
    % #if some_ctors_use_secondary_tag
    %           int data_tag;
    %   #if 0
    %   /*
    %   ** XXX we don't yet bother with these;
    %   ** mlds_to_c.m doesn't support static members.
    %   */
    %           /* constants used for data_tag */
    %           static const int <ctor1> = 0;
    %           static const int <ctor2> = 1;
    %   #endif
    % #endif
    % #if some_but_not_all_ctors_use_secondary_tag
    %       };
    % #endif
    %       ...
    %
    %       /*
    %       ** Reserved objects and/or derived classes,
    %       ** one for each constructor.
    %       **
    %       ** Reserved objects are generated for any constructors
    %       ** that use a `reserved_address(reserved_object(...))'
    %       ** representation.
    %       **
    %       ** Derived classes are generated for any other
    %       ** constructors; these are generated as nested classes
    %       ** avoid name clashes.
    %       ** These will derive either directly from
    %       ** <ClassName> or from <ClassName>::tag_type
    %       ** (which in turn derives from <ClassName>),
    %       ** depending on whether they need a secondary
    %       ** tag.  If all the ctors for a type need a
    %       ** secondary tag, we put the secondary tag members
    %       ** directly in the base class.
    %       */
    %       */
    % #if ctor1_uses_reserved_object
    %       static <ClassName> obj_<ctor1>;
    % #else
    %       static class <ctor1> : public <ClassName> {
    %       public:
    %           /*
    %           ** fields, one for each argument of this
    %           ** constructor
    %           */
    %           MR_Word F1;
    %           MR_Word F2;
    %           ...
    %           /*
    %           ** A constructor to initialize the fields
    %           */
    %           <ctor1>(MR_Word F1, MR_Word F2, ...) {
    %               this->F1 = F1;
    %               this->F2 = F2;
    %               ...
    %           }
    %       };
    % #endif
    %       static class <ctor2> : public <ClassName>::tag_type {
    %       public:
    %           ...
    %       };
    %       ...
    %
    %   };
    %
    %
    % If there is only one constructor which is not represented
    % as a reserved_object, then we don't generate a nested derived
    % class for that constructor, instead we just allocate the fields
    % in the base class.
    %
:- pred ml_gen_du_parent_type(module_info::in, type_ctor::in,
    hlds_type_defn::in, list(constructor)::in, cons_tag_values::in,
    mlds_defns::in, mlds_defns::in, mlds_defns::out) is det.

ml_gen_du_parent_type(ModuleInfo, TypeCtor, TypeDefn, Ctors, TagValues,
        MaybeEqualityMembers, MLDS_Defns0, MLDS_Defns) :-
    hlds_data.get_type_defn_context(TypeDefn, Context),
    MLDS_Context = mlds_make_context(Context),

    % generate the class name
    ml_gen_type_name(TypeCtor, QualBaseClassName, BaseClassArity),
    BaseClassId = mlds_class_type(QualBaseClassName, BaseClassArity,
        mlds_class),
    QualBaseClassName = qual(BaseClassModuleName, QualKind, BaseClassName),
    module_info_get_globals(ModuleInfo, Globals),
    BaseClassQualifier = mlds_append_class_qualifier(
        BaseClassModuleName, QualKind, Globals,
        BaseClassName, BaseClassArity),

    (
        % If none of the constructors for this type need a secondary tag,
        % then we don't need the members for the secondary tag.
        %
        \+ (some [Ctor] (
            list.member(Ctor, Ctors),
            ml_needs_secondary_tag(TagValues, Ctor)
        ))
    ->
        TagMembers = [],
        TagClassId = BaseClassId
    ;
        % Generate the members for the secondary tag.
        TagDataMember = ml_gen_tag_member("data_tag", Context),
        TagConstMembers = [],
        % XXX we don't yet bother with these;
        % mlds_to_c.m doesn't support static (one_copy) members.
        %   TagConstMembers = list.condense(list.map(
        %       ml_gen_tag_constant(Context, TagValues), Ctors)),
        TagMembers0 = [TagDataMember | TagConstMembers],

        % If all the constructors for this type need a secondary tag, then
        % we put the secondary tag members directly in the base class,
        % otherwise we put it in a separate nested derived class.
        %
        (
            (all [Ctor] (
                list.member(Ctor, Ctors)
            =>
                ml_needs_secondary_tag(TagValues, Ctor)
            ))
        ->
            TagMembers = TagMembers0,
            TagClassId = BaseClassId
        ;
            globals.get_target(Globals, Target),
            ml_gen_secondary_tag_class(MLDS_Context, BaseClassQualifier,
                BaseClassId, TagMembers0, Target, TagTypeDefn, TagClassId),
            TagMembers = [TagTypeDefn]
        )
    ),

    % Generate the nested derived classes for the constructors,
    % or static (one_copy) member objects for constructors with
    % reserved_object representations, or fields and a constructor method
    % for the single_functor case.
    list.foldl2(ml_gen_du_ctor_member(ModuleInfo, BaseClassId,
        BaseClassQualifier, TagClassId, TypeDefn, TagValues),
        Ctors, [], CtorMembers, [], BaseClassCtorMethods),

    % The base class doesn't import or inherit anything.
    Imports = [],
    Inherits = [],
    Implements = [],

    % Put it all together.
    Members = MaybeEqualityMembers ++ TagMembers ++ CtorMembers,
    MLDS_TypeName = entity_type(BaseClassName, BaseClassArity),
    MLDS_TypeFlags = ml_gen_type_decl_flags,
    MLDS_TypeDefnBody = mlds_class(mlds_class_defn(mlds_class,
        Imports, Inherits, Implements, BaseClassCtorMethods, Members)),
    MLDS_TypeDefn = mlds_defn(MLDS_TypeName, MLDS_Context, MLDS_TypeFlags,
        MLDS_TypeDefnBody),

    MLDS_Defns = [MLDS_TypeDefn | MLDS_Defns0].

    % Generate the declaration for the field that holds the secondary tag.
    %
:- func ml_gen_tag_member(string, prog_context) = mlds_defn.

ml_gen_tag_member(Name, Context) =
    mlds_defn(entity_data(var(mlds_var_name(Name, no))),
        mlds_make_context(Context),
        ml_gen_member_decl_flags,
        mlds_data(mlds_native_int_type, no_initializer, no)).

:- func ml_gen_tag_constant(prog_context, cons_tag_values, constructor)
    = mlds_defns.

ml_gen_tag_constant(Context, ConsTagValues, Ctor) = MLDS_Defns :-
    % Check if this constructor uses a secondary tag.
    ( ml_uses_secondary_tag(ConsTagValues, Ctor, SecondaryTag) ->
        % Generate an MLDS definition for this secondary tag constant.
        % We do this mainly for readability and interoperability. Note that
        % we don't do the same thing for primary tags, so this is most useful
        % in the `--tags none' case, where there will be no primary tags.

        Ctor = ctor(_ExistQTVars, _Constraints, Name, _Args),
        UnqualifiedName = unqualify_name(Name),
        ConstValue = const(mlconst_int(SecondaryTag)),
        MLDS_Defn = mlds_defn(
            entity_data(var(mlds_var_name(UnqualifiedName, no))),
            mlds_make_context(Context),
            ml_gen_enum_constant_decl_flags,
            mlds_data(mlds_native_int_type, init_obj(ConstValue), no)),
        MLDS_Defns = [MLDS_Defn]
    ;
        MLDS_Defns = []
    ).

    % Check if this constructor's representation uses a secondary tag,
    % and if so, return the secondary tag value.
    % BEWARE that this is not the same as ml_needs_secondary_tag, below.
    %
ml_uses_secondary_tag(ConsTagValues, Ctor, SecondaryTag) :-
    TagVal = get_tagval(ConsTagValues, Ctor),
    get_secondary_tag(TagVal) = yes(SecondaryTag).

    % Check if this constructor needs a secondary tag. This is true if its
    % representation uses a secondary tag, obviously. But it is also true
    % if its representation is the address of a reserved object; in that case,
    % for some back-ends (e.g. C) we need a field of some kind to ensure
    % that the reserved object had non-zero size, which in turn is needed
    % to ensure that its address is distinct from any other reserved objects
    % for the same type.
    %
:- pred ml_needs_secondary_tag(cons_tag_values::in, constructor::in)
    is semidet.

ml_needs_secondary_tag(TagValues, Ctor) :-
    TagVal = get_tagval(TagValues, Ctor),
    ( get_secondary_tag(TagVal) = yes(_)
    ; tagval_is_reserved_addr(TagVal, reserved_object(_, _, _))
    ).

    % Check if this constructor is a constant whose value is represented
    % as a reserved address.
    %
:- pred ml_uses_reserved_addr(cons_tag_values::in, constructor::in,
    reserved_address::out) is semidet.

ml_uses_reserved_addr(ConsTagValues, Ctor, RA) :-
    TagVal = get_tagval(ConsTagValues, Ctor),
    tagval_is_reserved_addr(TagVal, RA).

:- pred tagval_is_reserved_addr(cons_tag::in, reserved_address::out)
    is semidet.

tagval_is_reserved_addr(reserved_address_tag(RA), RA).
tagval_is_reserved_addr(shared_with_reserved_addresses_tag(_, TagVal), RA) :-
    tagval_is_reserved_addr(TagVal, RA).

:- func get_tagval(cons_tag_values, constructor) = cons_tag.

get_tagval(ConsTagValues, Ctor) = TagVal :-
    Ctor = ctor(_ExistQTVars, _Constraints, Name, Args),
    list.length(Args, Arity),
    map.lookup(ConsTagValues, cons(Name, Arity), TagVal).

    % Generate a definition for the class used for the secondary tag type.
    % This is needed for discriminated unions for which some but not all
    % constructors use secondary tags.
    %
:- pred ml_gen_secondary_tag_class(mlds_context::in, mlds_module_name::in,
    mlds_class_id::in, mlds_defns::in, compilation_target::in,
    mlds_defn::out, mlds_class_id::out) is det.

ml_gen_secondary_tag_class(MLDS_Context, BaseClassQualifier, BaseClassId,
        Members, Target, MLDS_TypeDefn, SecondaryTagClassId) :-
    % Generate the class name for the secondary tag class.
    % Note: the secondary tag class is nested inside the
    % base class for this type.
    UnqualClassName = "tag_type",
    ClassName = qual(BaseClassQualifier, type_qual, UnqualClassName),
    ClassArity = 0,
    SecondaryTagClassId = mlds_class_type(ClassName, ClassArity, mlds_class),

    % The secondary tag class inherits the base class for this type,
    % unless we're compiling to C -- in that case, we omit it,
    % since it is empty, and we don't want to include empty base
    % classes when compiling to C.
    Imports = [],
    ( target_uses_empty_base_classes(Target) = yes ->
        Inherits = [BaseClassId]
    ;
        Inherits = []
    ),
    Implements = [],
    Ctors = [],

    % Put it all together.
    MLDS_TypeName = entity_type(UnqualClassName, ClassArity),
    MLDS_TypeFlags = ml_gen_type_decl_flags,
    MLDS_TypeDefnBody = mlds_class(mlds_class_defn(mlds_class,
        Imports, Inherits, Implements, Ctors, Members)),
    MLDS_TypeDefn = mlds_defn(MLDS_TypeName, MLDS_Context, MLDS_TypeFlags,
        MLDS_TypeDefnBody).

    % Generate definitions corresponding to a constructor of a discriminated
    % union type. This will be one of the following:
    %
    % - (in the usual case) a nested derived class definition
    % - (for reserved_object) a one_copy (static) member object
    % - (for the single_functor case) a bunch of fields and
    %   a constructor method.
    %
:- pred ml_gen_du_ctor_member(module_info::in, mlds_class_id::in,
    mlds_module_name::in, mlds_class_id::in, hlds_type_defn::in,
    cons_tag_values::in, constructor::in, mlds_defns::in,
    mlds_defns::out, mlds_defns::in, mlds_defns::out) is det.

ml_gen_du_ctor_member(ModuleInfo, BaseClassId, BaseClassQualifier,
        SecondaryTagClassId, TypeDefn, ConsTagValues, Ctor,
        MLDS_Members0, MLDS_Members, MLDS_CtorMethods0, MLDS_CtorMethods) :-
    Ctor = ctor(ExistQTVars, Constraints, CtorName, Args),

    % XXX We should keep a context for the constructor,
    % but we don't, so we just use the context from the type.
    hlds_data.get_type_defn_context(TypeDefn, Context),
    MLDS_Context = mlds_make_context(Context),

    % Generate the class name for this constructor.
    UnqualCtorName = unqualify_name(CtorName),
    list.length(Args, CtorArity),

    TagVal = get_tagval(ConsTagValues, Ctor),
    ( tagval_is_reserved_addr(TagVal, ReservedAddr) ->
        ( ReservedAddr = reserved_object(_, _, _) ->
            % Generate a reserved object for this constructor.
            % Note that we use the SecondaryTagClassId for the type of this
            % reserved object; we can't use the BaseClassId because for some
            % back-ends, we need to ensure that the type used for the reserved
            % object has at least one data member, to make sure that each
            % reserved object gets a distinct address.
            %
            MLDS_ReservedObjName = ml_format_reserved_object_name(
                UnqualCtorName, CtorArity),
            MLDS_ReservedObjDefn = ml_gen_static_const_defn(
                MLDS_ReservedObjName, SecondaryTagClassId,
                public, no_initializer, Context),
            MLDS_Members = [MLDS_ReservedObjDefn | MLDS_Members0]
        ;
            % For reserved numeric addresses, we don't need to generate
            % any objects or types.
            MLDS_Members = MLDS_Members0
        ),
        MLDS_CtorMethods = MLDS_CtorMethods0
    ;
        % Generate the members for this constructor.

        % Number any unnamed fields starting from 1.
        ArgNum0 = 1,

        % Generate class members for the type_infos and typeclass_infos
        % that hold information about existentially quantified
        % type variables and type class constraints.
        % Note that the order of fields is as follows:
        %   - first typeinfos (for unconstrained type variables)
        %   - then typeclassinfos (for class constraints)
        %   - finally the ordinary members
        (
            ExistQTVars = [],
            % optimize common case
            ExtraMembers = [],
            ArgNum2 = ArgNum0
        ;
            ExistQTVars = [_ | _],
            constraint_list_get_tvars(Constraints, ConstrainedTVars),
            list.delete_elems(ExistQTVars, ConstrainedTVars,
                UnconstrainedTVars),
            list.map_foldl(ml_gen_type_info_member(ModuleInfo, Context),
                UnconstrainedTVars, TypeInfoMembers, ArgNum0, ArgNum1),
            list.map_foldl(ml_gen_typeclass_info_member(ModuleInfo, Context),
                Constraints, TypeClassInfoMembers, ArgNum1, ArgNum2),
            ExtraMembers = TypeInfoMembers ++ TypeClassInfoMembers
        ),

        % Generate the class members for the ordinary fields
        % of this constructor.
        list.map_foldl(ml_gen_du_ctor_field(ModuleInfo, Context),
            Args, OrdinaryMembers, ArgNum2, _ArgNum3),

        list.append(ExtraMembers, OrdinaryMembers, Members),

        % Generate a constructor function to initialize the fields, if needed
        % (not all back-ends use constructor functions).
        MaybeSecTagVal = get_secondary_tag(TagVal),
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        ( target_uses_constructors(Target) = yes ->
            ( ml_tag_uses_base_class(TagVal) ->
                CtorClassType = BaseClassId,
                CtorClassQualifier = BaseClassQualifier
            ;
                CtorClassType = mlds_class_type(
                    qual(BaseClassQualifier, type_qual, UnqualCtorName),
                    CtorArity, mlds_class),
                CtorClassQualifier = mlds_append_class_qualifier(
                    BaseClassQualifier, type_qual,
                    Globals, UnqualCtorName, CtorArity)
            ),
            CtorFunction = gen_constructor_function(Globals,
                BaseClassId, CtorClassType, CtorClassQualifier,
                SecondaryTagClassId, MaybeSecTagVal, Members, MLDS_Context),
            % If this constructor is going to go in the base class, then we may
            % also need to generate an additional zero-argument constructor,
            % which is used to construct the class that is used for
            % reserved_objects.
            (
                TagVal = shared_with_reserved_addresses_tag(RAs,
                    single_functor_tag),
                some [RA] (
                    list.member(RA, RAs),
                    RA = reserved_object(_, _, _)
                ),
                Members = [_ | _]
            ->
                ZeroArgCtor = gen_constructor_function(Globals, BaseClassId,
                    CtorClassType, CtorClassQualifier, SecondaryTagClassId,
                    no, [], MLDS_Context),
                Ctors = [ZeroArgCtor, CtorFunction]
            ;
                Ctors = [CtorFunction]
            )
        ;
            Ctors = []
        ),

        ( ml_tag_uses_base_class(TagVal) ->
            % Put the members for this constructor directly in the base class.
            MLDS_Members = Members ++ MLDS_Members0,
            MLDS_CtorMethods = Ctors ++ MLDS_CtorMethods0
        ;
            % Generate a nested derived class for this constructor,
            % and put the members for this constructor in that class.

            % We inherit either the base class for this type, or the secondary
            % tag class, depending on whether we need a secondary tag.
            % But when targetting C, we want to omit empty base classes.
            % So if targetting C, don't include any base class if there is
            % no secondary tag.
            ( MaybeSecTagVal = yes(_) ->
                Inherits = [SecondaryTagClassId]
            ; target_uses_empty_base_classes(Target) = yes ->
                Inherits = [BaseClassId]
            ;
                Inherits = []
            ),
            Imports = [],
            Implements = [],

            % Put it all together.
            MLDS_TypeName = entity_type(UnqualCtorName, CtorArity),
            MLDS_TypeFlags = ml_gen_type_decl_flags,
            MLDS_TypeDefnBody = mlds_class(mlds_class_defn(
                mlds_class, Imports, Inherits, Implements, Ctors, Members)),
            MLDS_TypeDefn = mlds_defn(MLDS_TypeName, MLDS_Context,
                MLDS_TypeFlags, MLDS_TypeDefnBody),
            MLDS_Members = [MLDS_TypeDefn | MLDS_Members0],
            MLDS_CtorMethods = MLDS_CtorMethods0
        )
    ).

% A constructor is represented using the base class rather than a derived
% class if there is only a single functor, or if there is a single
% functor and some constants represented using reserved addresses.
ml_tag_uses_base_class(single_functor_tag).
ml_tag_uses_base_class(shared_with_reserved_addresses_tag(_RAs, Tag)) :-
    ml_tag_uses_base_class(Tag).

:- func target_uses_constructors(compilation_target) = bool.

target_uses_constructors(target_c) = no.
target_uses_constructors(target_il) = yes.
target_uses_constructors(target_java) = yes.
target_uses_constructors(target_asm) = no.

:- func target_uses_empty_base_classes(compilation_target) = bool.

target_uses_empty_base_classes(target_c) = no.
target_uses_empty_base_classes(target_il) = yes.
target_uses_empty_base_classes(target_java) = yes.
target_uses_empty_base_classes(target_asm) = no.

    % This should return yes if references to function parameters in
    % constructor functions must be qualified with the module name,
    % not the class name.
    % We need to do this for the Java back-end, since MLDS names which
    % are qualified with the module name get unqualified when output
    % as Java, and parameter names must all be unqualified.
    % XXX perhaps we should do the same for all back-ends?
    %
:- func target_requires_module_qualified_params(compilation_target) = bool.

target_requires_module_qualified_params(target_c) = no.
target_requires_module_qualified_params(target_il) = no.
target_requires_module_qualified_params(target_java) = yes.
target_requires_module_qualified_params(target_asm) = no.

:- func gen_constructor_function(globals, mlds_class_id,
    mlds_type, mlds_module_name, mlds_class_id, maybe(int), mlds_defns,
    mlds_context) = mlds_defn.

gen_constructor_function(Globals, BaseClassId, ClassType, ClassQualifier,
        SecondaryTagClassId, MaybeTag, Members, Context) = CtorDefn :-
    Args = list.map(make_arg, Members),
    ReturnValues = [],

    globals.get_target(Globals, Target),
    InitMembers0 = list.map(gen_init_field(Target, BaseClassId,
        ClassType, ClassQualifier), Members),
    (
        MaybeTag = yes(TagVal),
        InitTag = gen_init_tag(ClassType, SecondaryTagClassId, TagVal,
            Context, Globals),
        InitMembers = [InitTag | InitMembers0]
    ;
        MaybeTag = no,
        InitMembers = InitMembers0
    ),

    Stmt = statement(block([], InitMembers), Context),
    Attributes = [],
    EnvVarNames = set.init,
    Ctor = mlds_function(no, mlds_func_params(Args, ReturnValues),
        body_defined_here(Stmt), Attributes, EnvVarNames),
    CtorFlags = init_decl_flags(public, per_instance, non_virtual,
        overridable, modifiable, concrete),

    % Note that the name of constructor is determined by the backend
    % convention.
    CtorDefn = mlds_defn(entity_export("<constructor>"), Context, CtorFlags,
        Ctor).

    % Get the name and type from the field definition, for use as a
    % constructor argument name and type.
    %
:- func make_arg(mlds_defn) = mlds_argument is det.

make_arg(mlds_defn(Name, _Context, _Flags, Defn)) = Arg :-
    ( Defn = mlds_data(Type, _Init, GC_TraceCode) ->
        Arg = mlds_argument(Name, Type, GC_TraceCode)
    ;
        unexpected(this_file, "make_arg: non-data member")
    ).

    % Generate "this-><fieldname> = <fieldname>;".
    %
:- func gen_init_field(compilation_target, mlds_class_id, mlds_type,
    mlds_module_name, mlds_defn) = statement is det.

gen_init_field(Target, BaseClassId, ClassType, ClassQualifier, Member) =
        Statement :-
    Member = mlds_defn(EntityName, Context, _Flags, Defn),
    ( Defn = mlds_data(Type0, _Init, _GC_TraceCode) ->
        Type = Type0
    ;
        unexpected(this_file, "gen_init_field: non-data member")
    ),
    (
        EntityName = entity_data(var(VarName0)),
        VarName0 = mlds_var_name(Name0, no)
    ->
        Name = Name0,
        VarName = VarName0
    ;
        unexpected(this_file, "gen_init_field: non-var member")
    ),
    (
        target_requires_module_qualified_params(Target) = yes
    ->
        ( BaseClassId = mlds_class_type(qual(ModuleName, _, _), _, _) ->
            QualVarName = qual(ModuleName, module_qual, VarName)
        ;
            unexpected(this_file,
                "gen_init_field: invalid BaseClassId")
        )
    ;
        QualVarName = qual(ClassQualifier, type_qual, VarName)
    ),
    Param = lval(var(QualVarName, Type)),
    Field = field(yes(0), self(ClassType),
        named_field(qual(ClassQualifier, type_qual, Name),
            mlds_ptr_type(ClassType)),
            % XXX we should use ClassType rather than BaseClassId here.
            % But doing so breaks the IL back-end, because then the hack in
            % fixup_class_qualifiers doesn't work.
        Type, BaseClassId),
    Statement = statement(atomic(assign(Field, Param)), Context).

    % Generate "this->data_tag = <TagVal>;".
    %
:- func gen_init_tag(mlds_type, mlds_class_id, int, mlds_context, globals)
    = statement.

gen_init_tag(ClassType, SecondaryTagClassId, TagVal, Context, Globals)
        = Statement :-
    ( SecondaryTagClassId = mlds_class_type(TagClass, TagArity, _) ->
        TagClass = qual(BaseClassQualifier, QualKind, TagClassName),
        TagClassQualifier = mlds_append_class_qualifier(BaseClassQualifier,
            QualKind, Globals, TagClassName, TagArity)
    ;
        unexpected(this_file, "gen_init_tag: class_id should be a class")
    ),
    Name = "data_tag",
    Type = mlds_native_int_type,
    Val = const(mlconst_int(TagVal)),
    Field = field(yes(0), self(ClassType),
        named_field(qual(TagClassQualifier, type_qual, Name),
            mlds_ptr_type(SecondaryTagClassId)),
        Type, ClassType),
    Statement = statement(atomic(assign(Field, Val)), Context).

:- pred ml_gen_typeclass_info_member(module_info::in, prog_context::in,
    prog_constraint::in, mlds_defn::out, int::in, int::out) is det.

ml_gen_typeclass_info_member(ModuleInfo, Context, Constraint, MLDS_Defn,
        ArgNum0, ArgNum) :-
    polymorphism.build_typeclass_info_type(Constraint, Type),
    ml_gen_field(ModuleInfo, Context, no, Type, MLDS_Defn, ArgNum0, ArgNum).

:- pred ml_gen_type_info_member(module_info::in, prog_context::in, tvar::in,
    mlds_defn::out, int::in, int::out) is det.

ml_gen_type_info_member(ModuleInfo, Context, TypeVar, MLDS_Defn,
        ArgNum0, ArgNum) :-
    % We don't have access to the correct kind here. This won't matter though,
    % since the type will only be checked to see that it is a variable,
    % and won't be used in any other way.
    Kind = kind_star,
    polymorphism.build_type_info_type(type_variable(TypeVar, Kind), Type),
    ml_gen_field(ModuleInfo, Context, no, Type, MLDS_Defn, ArgNum0, ArgNum).

:- pred ml_gen_du_ctor_field(module_info::in, prog_context::in,
    constructor_arg::in, mlds_defn::out, int::in, int::out) is det.

ml_gen_du_ctor_field(ModuleInfo, Context, MaybeFieldName - Type, MLDS_Defn,
        ArgNum0, ArgNum) :-
    ml_gen_field(ModuleInfo, Context, MaybeFieldName, Type, MLDS_Defn,
        ArgNum0, ArgNum).

:- pred ml_gen_field(module_info::in, prog_context::in,
    maybe(ctor_field_name)::in, mer_type::in, mlds_defn::out,
    int::in, int::out) is det.

ml_gen_field(ModuleInfo, Context, MaybeFieldName, Type, MLDS_Defn,
        ArgNum0, ArgNum) :-
    ( ml_must_box_field_type(Type, ModuleInfo) ->
        MLDS_Type = mlds_generic_type
    ;
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type)
    ),
    FieldName = ml_gen_field_name(MaybeFieldName, ArgNum0),
    MLDS_Defn = ml_gen_mlds_field_decl(var(mlds_var_name(FieldName, no)),
        MLDS_Type, mlds_make_context(Context)),
    ArgNum = ArgNum0 + 1.

:- func ml_gen_mlds_field_decl(mlds_data_name, mlds_type, mlds_context)
    = mlds_defn.

ml_gen_mlds_field_decl(DataName, MLDS_Type, Context) = MLDS_Defn :-
    Name = entity_data(DataName),
    % We only need GC tracing code for top-level variables, not for fields
    GC_TraceCode = no,
    Defn = mlds_data(MLDS_Type, no_initializer, GC_TraceCode),
    DeclFlags = ml_gen_public_field_decl_flags,
    MLDS_Defn = mlds_defn(Name, Context, DeclFlags, Defn).

%-----------------------------------------------------------------------------%
%
% Miscellaneous helper routines.
%

ml_gen_type_name(type_ctor(Name, Arity), QualifiedTypeName, Arity) :-
    (
        Name = qualified(ModuleName, TypeName)
    ;
        % Builtin types like `int' may be still unqualified at this point.
        Name = unqualified(TypeName),
        ModuleName = mercury_public_builtin_module
    ),
    MLDS_Module = mercury_module_name_to_mlds(ModuleName),
    QualifiedTypeName = qual(MLDS_Module, module_qual, TypeName).

    % For interoperability, we ought to generate an `==' member for types
    % which have a user-defined equality, if the target language supports it
    % (as do e.g. C++, Java).
    %
:- pred ml_gen_equality_members(maybe(unify_compare)::in,
    list(mlds_defn)::out) is det.

% XXX generation of `==' members is not yet implemented.
ml_gen_equality_members(_, []).

%-----------------------------------------------------------------------------%
%
% Routines for generating declaration flags.
%

ml_gen_type_decl_flags = MLDS_DeclFlags :-
    % XXX are these right?
    Access = public,
    PerInstance = one_copy,
    Virtuality = non_virtual,
    Finality = overridable,
    Constness = modifiable,
    Abstractness = concrete,
    MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
        Virtuality, Finality, Constness, Abstractness).

ml_gen_member_decl_flags = MLDS_DeclFlags :-
    Access = public,
    PerInstance = per_instance,
    Virtuality = non_virtual,
    Finality = overridable,
    Constness = modifiable,
    Abstractness = concrete,
    MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
        Virtuality, Finality, Constness, Abstractness).

ml_gen_enum_constant_decl_flags = MLDS_DeclFlags :-
    Access = public,
    PerInstance = one_copy,
    Virtuality = non_virtual,
    Finality = final,
    Constness = const,
    Abstractness = concrete,
    MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
        Virtuality, Finality, Constness, Abstractness).

ml_gen_special_member_decl_flags = MLDS_DeclFlags :-
    Access = public,
    PerInstance = per_instance,
    Virtuality = non_virtual,
    Finality = final,
    Constness = const,
    Abstractness = concrete,
    MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
        Virtuality, Finality, Constness, Abstractness).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "ml_type_gen.m".

:- end_module ml_type_gen.

%-----------------------------------------------------------------------------%
