%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_type_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Generate MLDS definitions for all the types in the HLDS.
    %
:- pred ml_gen_types(module_info::in, mlds_target_lang::in,
    list(mlds_class_defn)::out, list(mlds_enum_class_defn)::out) is det.

%---------------------------------------------------------------------------%

    % Generate a constructor function to initialise the given fields in a
    % class representing a compiler generated data structure.
    %
    % The input we take as the description of each field is a bespoke type,
    % mlds_field_info, not the field's mlds_defn. This is because not all
    % mlds_defns define fields, and we don't want this predicate to have to
    % test whether the data it is given makes sense.
    %
:- func ml_gen_constructor_function(mlds_target_lang, mlds_class_id,
    mlds_class_id, mlds_module_name, mlds_class_id, maybe(int),
    list(mlds_field_info), prog_context) = mlds_function_defn.
:- func ml_gen_struct_constructor_function(mlds_struct_id, mlds_module_name,
    list(mlds_field_info), prog_context) = mlds_function_defn.

%---------------------------------------------------------------------------%

    % Given an HLDS type_ctor, generate the MLDS class name and arity
    % for the corresponding MLDS type.
    %
:- pred ml_gen_class_name(type_ctor::in, qual_class_name::out, arity::out)
    is det.
:- pred ml_gen_type_name(type_ctor::in, mlds_module_name::out, string::out,
    arity::out) is det.

    % Generate a data constructor name given the type constructor.
    %
:- func ml_gen_du_ctor_name(mlds_target_lang, type_ctor, sym_name, int)
    = string.

    % As above but pass the unqualified type name directly.
    %
:- func ml_gen_du_ctor_name_unqual_type(mlds_target_lang, string, int,
    sym_name, int) = string.

%---------------------------------------------------------------------------%

    % ctors_with_and_without_secondary_tag(CtorRepns, NumWith, NumWithout):
    %
    % Return the number of constructors with and without secondary tags.
    %
:- pred ctors_with_and_without_secondary_tag(list(constructor_repn)::in,
    int::out, int::out) is det.

:- type tag_uses_base_class
    --->    tag_does_not_use_base_class
    ;       tag_uses_base_class.

    % A constructor is represented using the base class rather than a derived
    % class if there is only a single functor.
    %
:- func ml_tag_uses_base_class(cons_tag) = tag_uses_base_class.

    % Return whether this compilation target uses object constructors.
    %
:- func ml_target_uses_constructors(mlds_target_lang) = bool.

    % A description of a field in a compiler-generated data structure.
:- type mlds_field_info
    --->    mlds_field_info(
                % The compiler generated field name. It should be of a type
                % that is separate from the type of variables, but that change
                % is for later.
                mlds_field_var_name,

                % The type of the field.
                mlds_type,

                % What tracing, if any, is required for the field.
                mlds_gc_statement,

                % The context we will use for code derived from this field.
                % This *ought* to be the context of the field itself
                % in the type definition, but there is no guarantee
                % that it will be; at the moment, it may be the context
                % of the whole type definition. Since pretty much noone
                % ever looks at the contexts of the generated code,
                % such minor differences don't really matter.
                prog_context
            ).

    % Exported enumeration info in the HLDS is converted into an MLDS
    % specific representation. The target specific code generators may
    % further transform it.
    %
:- pred ml_gen_exported_enums(module_info::in, list(mlds_exported_enum)::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.builtin_modules.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module uint.

%---------------------------------------------------------------------------%

ml_gen_types(ModuleInfo, Target, ClassDefns, EnumClassDefns) :-
    HighLevelData = mlds_target_high_level_data(Target),
    (
        HighLevelData = yes,
        module_info_get_type_table(ModuleInfo, TypeTable),
        get_all_type_ctor_defns(TypeTable, TypeCtorDefns),
        list.foldl2(ml_gen_hld_type_defn_if_local(ModuleInfo, Target),
            TypeCtorDefns, [], ClassDefns, [], EnumClassDefns)
    ;
        HighLevelData = no,
        ClassDefns = [],
        EnumClassDefns = []
    ).

:- pred ml_gen_hld_type_defn_if_local(module_info::in, mlds_target_lang::in,
    pair(type_ctor, hlds_type_defn)::in,
    list(mlds_class_defn)::in, list(mlds_class_defn)::out,
    list(mlds_enum_class_defn)::in, list(mlds_enum_class_defn)::out) is det.

ml_gen_hld_type_defn_if_local(ModuleInfo, Target, TypeCtor - TypeDefn,
        !ClassDefns, !EnumClassDefns) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    DefinedThisModule = type_status_defined_in_this_module(TypeStatus),
    (
        DefinedThisModule = yes,
        ml_gen_hld_type_defn(ModuleInfo, Target, TypeCtor, TypeDefn,
            !ClassDefns, !EnumClassDefns)
    ;
        DefinedThisModule = no
    ).

:- pred ml_gen_hld_type_defn(module_info::in, mlds_target_lang::in,
    type_ctor::in, hlds_type_defn::in,
    list(mlds_class_defn)::in, list(mlds_class_defn)::out,
    list(mlds_enum_class_defn)::in, list(mlds_enum_class_defn)::out) is det.

ml_gen_hld_type_defn(ModuleInfo, Target, TypeCtor, TypeDefn,
        !ClassDefns, !EnumClassDefns) :-
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    (
        ( TypeBody = hlds_abstract_type(_)
        ; TypeBody = hlds_foreign_type(_)
        ; TypeBody = hlds_solver_type(_)
        )
    ;
        TypeBody = hlds_eqv_type(_EqvType)
        % XXX Fixme!
        % For a description of the problems with equivalence types,
        % see our BABEL'01 paper "Compiling Mercury to the .NET CLR".
        % The same issue arises for some of the other kinds of types.
    ;
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(_Ctors, MaybeSuperType, MaybeUserEqComp,
            MaybeRepn, _Foreign),
        (
            MaybeRepn = no,
            unexpected($pred, "MaybeRepn = no")
        ;
            MaybeRepn = yes(Repn)
        ),
        (
            MaybeSuperType = subtype_of(_)
            % In high-level data grades, a subtype uses the same class as its
            % base type ctor.
        ;
            MaybeSuperType = not_a_subtype,
            Repn = du_type_repn(CtorRepns, _ConsCtorMap, _CheaperTagTest,
                DuTypeKind, _MaybeDirectArgCtors),
            ml_gen_equality_members(MaybeUserEqComp, MaybeEqualityMembers),
            (
                ( DuTypeKind = du_type_kind_mercury_enum
                ; DuTypeKind = du_type_kind_foreign_enum(_)
                ),
                ml_gen_hld_enum_type(Target, TypeCtor, TypeDefn, CtorRepns,
                    MaybeEqualityMembers, EnumClassDefn),
                !:EnumClassDefns = [EnumClassDefn | !.EnumClassDefns]
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                % XXX We shouldn't have to generate an MLDS type for these
                % types, but it is not easy to ensure that we never refer to
                % that type.
                ml_gen_hld_enum_type(Target, TypeCtor, TypeDefn, CtorRepns,
                    MaybeEqualityMembers, EnumClassDefn),
                !:EnumClassDefns = [EnumClassDefn | !.EnumClassDefns]
            ;
                ( DuTypeKind = du_type_kind_notag(_, _, _)
                ; DuTypeKind = du_type_kind_general
                ),
                ml_gen_hld_du_type(ModuleInfo, Target, TypeCtor, TypeDefn,
                    CtorRepns, MaybeEqualityMembers, ClassDefn),
                !:ClassDefns = [ClassDefn | !.ClassDefns]
            )
        )
    ).

%---------------------------------------------------------------------------%
%
% Enumeration types.
%

    % For each enumeration, we generate an MLDS type of the following form:
    %
    %   struct <ClassName> {
    %       static final const int <ctor1> = 0;
    %       static final const int <ctor2> = 1;
    %       ...
    %       int MR_value;
    %   };
    %
    % It is marked as an mlds_enum so that the MLDS -> target code generator
    % can treat it specially if need be (e.g. generating a C# enum rather than
    % a class).
    %
    % Note that for Java, the MR_value field is inherited from the
    % MercuryEnum class.
    %
:- pred ml_gen_hld_enum_type(mlds_target_lang::in, type_ctor::in,
    hlds_type_defn::in, list(constructor_repn)::in,
    list(mlds_field_var_defn)::in, mlds_enum_class_defn::out) is det.

ml_gen_hld_enum_type(Target, TypeCtor, TypeDefn, CtorRepns,
        MaybeEqualityMembers, EnumClassDefn) :-
    hlds_data.get_type_defn_context(TypeDefn, Context),

    % Generate the class name.
    ml_gen_type_name(TypeCtor, EnumModuleName, EnumName, EnumArity),

    % Generate the class members.
    ValueMember = mlds_field_var_defn(fvn_mr_value, Context,
        ml_gen_member_data_decl_flags, mlds_builtin_type_int(int_type_int),
        no_initializer, gc_no_stmt),
    MLDS_Type = mlds_enum_class_type(
        mlds_enum_class_id(EnumModuleName, EnumName, EnumArity)),
    EnumConstMembers = list.map(ml_gen_hld_enum_constant(Context, MLDS_Type),
        CtorRepns),
    expect(unify(MaybeEqualityMembers, []), $pred,
        "MaybeEqualityMembers != []"),

    (
        Target = ml_target_java,
        % Java classes implementing enums extend the MercuryEnum class.
        Inherits = inherits_class(ml_java_mercury_enum_class),
        % All Java classes corresponding to types implement the MercuryType
        % interface.
        Implements = yes(ml_java_mercury_type_interface)
    ;
        ( Target = ml_target_c
        ; Target = ml_target_csharp
        ),
        Inherits = inherits_nothing,
        Implements = no
    ),

    % Put it all together.
    get_type_defn_tparams(TypeDefn, TypeVars),
    EnumClassDefn = mlds_enum_class_defn(EnumName, EnumArity, Context,
        Inherits, Implements, TypeVars, ValueMember, EnumConstMembers, []).

:- func ml_gen_hld_enum_constant(prog_context, mlds_type, constructor_repn)
    = mlds_enum_const_defn.

ml_gen_hld_enum_constant(Context, MLDS_Type, CtorRepn) = EnumConstDefn :-
    % Figure out the value of this enumeration constant.
    CtorRepn = ctor_repn(_, _, QualSymName, ConsTag, _, Arity, _),
    Name = unqualify_name(QualSymName),
    VarName = fvn_enum_const(Name),
    expect(unify(Arity, 0), $pred, "arity != []"),
    enum_cons_tag_to_ml_const_rval(MLDS_Type, ConsTag, EnumConst, _ConstRval),
    EnumConstDefn = mlds_enum_const_defn(VarName, Context, EnumConst).

%---------------------------------------------------------------------------%
%
% Discriminated union types.
%

    % XXX This comment seems to be out-of-date. It shows how we translate
    % the high-level data representation of a discriminated union type
    % to C code, but we have stopped supporting that representation
    % when targeting C in May of 2020.
    %
    % For each discriminated union type, we generate an MLDS type of the
    % following form:
    %
    %   static class <ClassName> {
    %   public:
    % #if some_but_not_all_ctors_use_secondary_tag
    %       // A nested derived class for the secondary tag.
    %       static class tag_type : public <ClassName> {
    %       public:
    % #endif
    % #if some_ctors_use_secondary_tag
    %           int data_tag;
    %   #if 0
    %   // XXX we don't yet bother with these;
    %   // mlds_to_c.m doesn't support static members.
    %           // constants used for data_tag
    %           static const int <ctor1> = 0;
    %           static const int <ctor2> = 1;
    %   #endif
    % #endif
    % #if some_but_not_all_ctors_use_secondary_tag
    %       };
    % #endif
    %       ...
    %
    %       // Derived classes, one for each constructor;
    %       // these are generated as nested classes to avoid name clashes.
    %       // These will derive either directly from
    %       // <ClassName> or from <ClassName>::tag_type
    %       // (which in turn derives from <ClassName>),
    %       // depending on whether they need a secondary tag.
    %       // If all the ctors for a type need a secondary tag,
    %       // we put the secondary tag members directly in the base class.
    %
    %       static class <ctor1> : public <ClassName> {
    %       public:
    %           // Fields, one for each argument of this constructor.
    %           MR_Word F1;
    %           MR_Word F2;
    %           ...
    %           // A constructor to initialize the fields.
    %           <ctor1>(MR_Word F1, MR_Word F2, ...) {
    %               this->F1 = F1;
    %               this->F2 = F2;
    %               ...
    %           }
    %       };
    %
    %       static class <ctor2> : public <ClassName>::tag_type {
    %       public:
    %           ...
    %       };
    %       ...
    %
    %   };
    %
    % If there is only one constructor, then we don't generate a nested
    % derived class for that constructor, instead we just allocate the fields
    % in the base class.
    %
:- pred ml_gen_hld_du_type(module_info::in, mlds_target_lang::in,
    type_ctor::in, hlds_type_defn::in, list(constructor_repn)::in,
    list(mlds_field_var_defn)::in, mlds_class_defn::out) is det.

ml_gen_hld_du_type(ModuleInfo, Target, TypeCtor, TypeDefn, CtorRepns,
        MaybeEqualityMembers, ClassDefn) :-
    hlds_data.get_type_defn_context(TypeDefn, Context),

    % Generate the class name.
    ml_gen_class_name(TypeCtor, QualBaseClassName, BaseClassArity),
    BaseClassId = mlds_class_id(QualBaseClassName, BaseClassArity, mlds_class),
    QualBaseClassName =
        qual_class_name(BaseClassModuleName, QualKind, BaseClassName),
    BaseClassQualifier = mlds_append_class_qualifier(Target,
        BaseClassModuleName, QualKind, BaseClassName, BaseClassArity),

    ctors_with_and_without_secondary_tag(CtorRepns,
        NumWithSecTag, NumWithoutSecTag),
    ( if NumWithSecTag > 0 then
        % Generate the members for the secondary tag.
        TagVarMember = mlds_field_var_defn(fvn_data_tag, Context,
            ml_gen_member_data_decl_flags,
            mlds_builtin_type_int(int_type_int),
            no_initializer, gc_no_stmt),
        TagConstMembers = [],
        % XXX we don't yet bother with these;
        % mlds_to_c.m doesn't support static (one_copy) members.
        %   TagConstMembers = list.condense(list.map(
        %       ml_gen_tag_constant(Context, TypeCtor), CtorRepns)),
        TagFieldVarMembers0 = [TagVarMember | TagConstMembers],

        % If all the constructors for this type need a secondary tag,
        % then we put the secondary tag members directly in the base class;
        % otherwise, we put it in a separate nested derived class.
        ( if NumWithoutSecTag = 0 then
            TagFieldVarMembers = TagFieldVarMembers0,
            TagClassMembers = [],
            TagClassId = BaseClassId
        else
            ml_gen_hld_secondary_tag_class(Context, BaseClassQualifier,
                BaseClassId, TagFieldVarMembers0, Target,
                TagTypeDefn, TagClassId),
            TagFieldVarMembers = [],
            TagClassMembers = [TagTypeDefn]
        )
    else
        % If none of the constructors for this type need a secondary tag,
        % then we don't need the members for the secondary tag.
        TagFieldVarMembers = [],
        TagClassMembers = [],
        TagClassId = BaseClassId
    ),

    % Generate the nested derived classes for the constructors,
    % or static (one_copy) member objects for constructors with
    % reserved_object representations, or fields and a constructor method
    % for the single_functor case.
    list.foldl3(
        ml_gen_hld_du_ctor_member(ModuleInfo, Target, BaseClassId,
            BaseClassQualifier, TagClassId, TypeCtor, TypeDefn),
        CtorRepns, [], CtorMemberFields, [], CtorMemberClasses,
        [], BaseClassCtorMethods),

    % The base class doesn't import or inherit anything.
    Imports = [],
    Inherits = inherits_nothing,

    (
        Target = ml_target_java,
        % All Java classes corresponding to types implement the MercuryType
        % interface.
        Implements = [ml_java_mercury_type_interface]
    ;
        ( Target = ml_target_c
        ; Target = ml_target_csharp
        ),
        Implements = []
    ),

    get_type_defn_tparams(TypeDefn, TypeParams),

    % Put it all together.
    MemberFields =
        MaybeEqualityMembers ++ TagFieldVarMembers ++ CtorMemberFields,
    MemberClasses = TagClassMembers ++ CtorMemberClasses,
    MLDS_ClassFlags = ml_gen_type_decl_flags,
    ClassDefn = mlds_class_defn(BaseClassName, BaseClassArity, Context,
        MLDS_ClassFlags, mlds_class, Imports, Inherits, Implements, TypeParams,
        MemberFields, MemberClasses, [], BaseClassCtorMethods).

%---------------------------------------------------------------------------%

    % Generate a definition for the class used for the secondary tag type.
    % This is needed for discriminated unions for which some but not all
    % constructors use secondary tags.
    %
:- pred ml_gen_hld_secondary_tag_class(prog_context::in, mlds_module_name::in,
    mlds_class_id::in, list(mlds_field_var_defn)::in, mlds_target_lang::in,
    mlds_class_defn::out, mlds_class_id::out) is det.

ml_gen_hld_secondary_tag_class(Context, BaseClassQualifier, BaseClassId,
        Members, Target, MLDS_ClassDefn, SecondaryTagClassId) :-
    % Generate the class name for the secondary tag class.
    % Note: the secondary tag class is nested inside the
    % base class for this type.
    UnqualClassName = "tag_type",
    ClassName =
        qual_class_name(BaseClassQualifier, type_qual, UnqualClassName),
    ClassArity = 0,
    SecondaryTagClassId = mlds_class_id(ClassName, ClassArity, mlds_class),

    % The secondary tag class inherits the base class for this type,
    % unless we are compiling to C -- in that case, we omit it,
    % since it is empty, and we don't want to include empty base classes
    % when compiling to C.
    Imports = [],
    EmptyBaseClasses = target_uses_empty_base_classes(Target),
    (
        EmptyBaseClasses = yes,
        Inherits = inherits_class(BaseClassId)
    ;
        EmptyBaseClasses = no,
        Inherits = inherits_nothing
    ),
    Implements = [],
    Ctors = [],

    % Type parameters are only used by the Java backend, which doesn't use
    % secondary tag classes.
    TypeParams = [],

    % Put it all together.
    MLDS_ClassFlags = ml_gen_type_decl_flags,
    MLDS_ClassDefn = mlds_class_defn(UnqualClassName, ClassArity, Context,
        MLDS_ClassFlags, mlds_class, Imports, Inherits, Implements,
        TypeParams, Members, [], [], Ctors).

    % Generate definitions corresponding to a constructor of a discriminated
    % union type. This will be one of the following:
    %
    % - (in the usual case) a nested derived class definition
    % - (for reserved_object) a one_copy (static) member object
    % - (for the single_functor case) a bunch of fields and
    %   a constructor method.
    %
:- pred ml_gen_hld_du_ctor_member(module_info::in, mlds_target_lang::in,
    mlds_class_id::in, mlds_module_name::in, mlds_class_id::in,
    type_ctor::in, hlds_type_defn::in, constructor_repn::in,
    list(mlds_field_var_defn)::in, list(mlds_field_var_defn)::out,
    list(mlds_class_defn)::in, list(mlds_class_defn)::out,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out) is det.

ml_gen_hld_du_ctor_member(ModuleInfo, Target, BaseClassId, BaseClassQualifier,
        SecondaryTagClassId, TypeCtor, TypeDefn, CtorRepn,
        BaseClassFields0, BaseClassFields, BaseClassClasses0, BaseClassClasses,
        BaseClassCtors0, BaseClassCtors) :-
    CtorRepn = ctor_repn(_Ordinal, MaybeExistConstraints, CtorName, ConsTag,
        ArgRepns, CtorArity, _Ctxt),

    % XXX We should keep a context for the constructor,
    % but we don't, so we just use the context from the type.
    hlds_data.get_type_defn_context(TypeDefn, Context),

    % Generate the class name for this constructor.
    UnqualCtorName =
        ml_gen_du_ctor_name(Target, TypeCtor, CtorName, CtorArity),

    % Generate the members for this data constructor.

    % Number any unnamed fields starting from 1.
    ArgNum0 = 1,

    % Generate class members for any type_infos and typeclass_infos
    % that hold information about existentially quantified
    % type variables and type class constraints.
    % The order of fields is as follows:
    %
    %   - first typeinfos (for unconstrained type variables),
    %   - then typeclassinfos (for class constraints),
    %   - finally the ordinary members.
    (
        MaybeExistConstraints = no_exist_constraints,
        % Optimize this common case.
        PolyFields = [],
        PolyFieldInfos = [],
        ArgNum2 = ArgNum0
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(ExistQTVars, Constraints,
            UnconstrainedTVarsEC, _ConstrainedTVars),
        constraint_list_get_tvars(Constraints, ConstrainedTVars),
        list.delete_elems(ExistQTVars, ConstrainedTVars, UnconstrainedTVars),
        expect(unify(UnconstrainedTVars, UnconstrainedTVarsEC), $pred,
            "UnconstrainedTVars != UnconstrainedTVarsEC"),
        list.map2_foldl(
            ml_gen_hld_du_ctor_type_info_field(ModuleInfo, Context),
            UnconstrainedTVars, TypeInfoFields, TypeInfoFieldInfos,
            ArgNum0, ArgNum1),
        list.map2_foldl(
            ml_gen_hld_du_ctor_typeclass_info_field(ModuleInfo, Context),
            Constraints, TypeClassInfoFields, TypeClassInfoFieldInfos,
            ArgNum1, ArgNum2),
        PolyFields = TypeInfoFields ++ TypeClassInfoFields,
        PolyFieldInfos = TypeInfoFieldInfos ++ TypeClassInfoFieldInfos
    ),

    % Generate the class members for the ordinary fields
    % of this constructor.
    list.map2_foldl(ml_gen_hld_du_ctor_field(ModuleInfo, Context),
        ArgRepns, OrdinaryFields, OrdinaryFieldInfos, ArgNum2, _ArgNum3),

    SubClassFields = PolyFields ++ OrdinaryFields,
    SubClassFieldInfos = PolyFieldInfos ++ OrdinaryFieldInfos,

    % Generate a constructor function to initialize the fields, if needed
    % (not all back-ends use constructor functions).
    MaybeSecTagVal = get_maybe_secondary_tag(ConsTag),
    UsesConstructors = ml_target_uses_constructors(Target),
    UsesBaseClass = ml_tag_uses_base_class(ConsTag),
    (
        UsesConstructors = yes,
        (
            UsesBaseClass = tag_uses_base_class,
            CtorClassId = BaseClassId,
            CtorClassQualifier = BaseClassQualifier
        ;
            UsesBaseClass = tag_does_not_use_base_class,
            CtorClassId = mlds_class_id(
                qual_class_name(BaseClassQualifier, type_qual,
                    UnqualCtorName),
                CtorArity, mlds_class),
            CtorClassQualifier = mlds_append_class_qualifier(Target,
                BaseClassQualifier, type_qual, UnqualCtorName, CtorArity)
        ),
        SubClassCtorFunc = ml_gen_constructor_function(Target,
            BaseClassId, CtorClassId, CtorClassQualifier,
            SecondaryTagClassId, MaybeSecTagVal, SubClassFieldInfos, Context),
        % If this constructor is going to go in the base class, then we may
        % also need to generate an additional zero-argument constructor,
        % which is used to construct the class that is used for
        % reserved_objects.
        % XXX We don't support reserved_object tags anymore.
        %
        % The implementation of deep copy in Java grades also depends on
        % zero-argument constructors.
        ( if
            Target = ml_target_java,
            SubClassFields = [_ | _]
        then
            ZeroArgCtorFunc = ml_gen_constructor_function(Target,
                BaseClassId, CtorClassId, CtorClassQualifier,
                SecondaryTagClassId, no, [], Context),
            SubClassCtors = [ZeroArgCtorFunc, SubClassCtorFunc]
        else
            SubClassCtors = [SubClassCtorFunc]
        )
    ;
        UsesConstructors = no,
        SubClassCtors = []
    ),

    (
        UsesBaseClass = tag_uses_base_class,
        % Put the members for this constructor directly in the base class.
        BaseClassFields = SubClassFields ++ BaseClassFields0,
        BaseClassClasses = BaseClassClasses0,
        BaseClassCtors = SubClassCtors ++ BaseClassCtors0
    ;
        UsesBaseClass = tag_does_not_use_base_class,
        % Generate a nested derived class for this constructor,
        % and put the members for this constructor in that class.

        % We inherit either the base class for this type, or the secondary
        % tag class, depending on whether we need a secondary tag.
        % But when targetting C, we want to omit empty base classes.
        % So if targetting C, don't include any base class if there is
        % no secondary tag.
        % XXX We haven't targeted C with high level data since May 2020.
        % However, target_uses_empty_base_classes return "no" for C#
        % as well as for C.
        (
            MaybeSecTagVal = yes(_),
            Inherits = inherits_class(SecondaryTagClassId)
        ;
            MaybeSecTagVal = no,
            UsesEmptyBaseClasses = target_uses_empty_base_classes(Target),
            (
                UsesEmptyBaseClasses = yes,
                Inherits = inherits_class(BaseClassId)
            ;
                UsesEmptyBaseClasses = no,
                Inherits = inherits_nothing
            )
        ),
        Imports = [],
        Implements = [],
        get_type_defn_tparams(TypeDefn, TypeParams),

        % Put it all together.
        SubClassFlags = ml_gen_type_decl_flags,
        SubClassDefn = mlds_class_defn(UnqualCtorName, CtorArity, Context,
            SubClassFlags, mlds_class, Imports, Inherits, Implements,
            TypeParams, SubClassFields, [], [], SubClassCtors),

        BaseClassFields = BaseClassFields0,
        BaseClassClasses = [SubClassDefn | BaseClassClasses0],
        BaseClassCtors = BaseClassCtors0
    ).

:- func target_uses_empty_base_classes(mlds_target_lang) = bool.

target_uses_empty_base_classes(ml_target_c) = no.
target_uses_empty_base_classes(ml_target_csharp) = no.
target_uses_empty_base_classes(ml_target_java) = yes.

ml_gen_constructor_function(Target, BaseClassId, CtorClassId, ClassQualifier,
        SecondaryTagClassId, MaybeTag, FieldInfos, Context) = CtorDefn :-
    Args = list.map(make_arg, FieldInfos),
    ReturnValues = [],

    InitMembers0 = list.map(
        gen_init_field(BaseClassId, CtorClassId, ClassQualifier),
        FieldInfos),
    (
        MaybeTag = yes(TagVal),
        InitTag = gen_init_tag(Target, CtorClassId, SecondaryTagClassId,
            TagVal, Context),
        InitMembers = [InitTag | InitMembers0]
    ;
        MaybeTag = no,
        InitMembers = InitMembers0
    ),

    % Note that the name of the constructor is determined by the backend
    % convention.
    FunctionName = mlds_function_export("<constructor>"),
    CtorFlags = mlds_function_decl_flags(func_public, per_instance),
    Params = mlds_func_params(Args, ReturnValues),
    Stmt = ml_stmt_block([], [], InitMembers, Context),
    EnvVarNames = set.init,
    CtorDefn = mlds_function_defn(FunctionName, Context,
        CtorFlags, no, Params, body_defined_here(Stmt), EnvVarNames, no).

ml_gen_struct_constructor_function(StructId, ClassQualifier, FieldInfos,
        Context) = CtorDefn :-
    Args = list.map(make_arg, FieldInfos),
    ReturnValues = [],

    InitMembers = list.map(gen_init_struct_field(StructId, ClassQualifier),
        FieldInfos),

    % Note that the name of the constructor is determined by the backend
    % convention.
    FunctionName = mlds_function_export("<constructor>"),
    CtorFlags = mlds_function_decl_flags(func_public, per_instance),
    % XXX We probably don't want func_public.
    Params = mlds_func_params(Args, ReturnValues),
    Stmt = ml_stmt_block([], [], InitMembers, Context),
    EnvVarNames = set.init,
    CtorDefn = mlds_function_defn(FunctionName, Context,
        CtorFlags, no, Params, body_defined_here(Stmt), EnvVarNames, no).

    % Get the name and type from the field description, for use as a
    % constructor argument name and type.
    %
:- func make_arg(mlds_field_info) = mlds_argument is det.

make_arg(FieldInfo) = Arg :-
    FieldInfo = mlds_field_info(FieldVarName, Type, GcStmt, _Context),
    Arg = mlds_argument(lvn_field_var_as_local(FieldVarName), Type, GcStmt).

    % Generate "this-><fieldname> = <fieldname>;".
    %
:- func gen_init_field(mlds_class_id, mlds_class_id,
    mlds_module_name, mlds_field_info) = mlds_stmt is det.

gen_init_field(BaseClassId, CtorClassId, ClassQualifier, FieldInfo) = Stmt :-
    FieldInfo = mlds_field_info(FieldVarName, FieldType, _GcStmt, Context),
    LocalVarName = lvn_field_var_as_local(FieldVarName),
    Param = ml_lval(ml_local_var(LocalVarName, FieldType)),
    CtorClassType = mlds_class_type(CtorClassId),
    FieldId = ml_field_named(
        qual_field_var_name(ClassQualifier, type_qual, FieldVarName),
        mlds_ptr_type(CtorClassType)),
    FieldLval = ml_field(yes(ptag(0u8)),
        % XXX We should use ClassType rather than BaseClassId here.
        % But doing so breaks the IL back-end, because then the hack in
        % fixup_class_qualifiers doesn't work.
        % XXX That isn't an issue anymore.
        ml_self(CtorClassType), mlds_class_type(BaseClassId),
        FieldId, FieldType),
    Stmt = ml_stmt_atomic(assign(FieldLval, Param), Context).

    % Generate "this-><fieldname> = <fieldname>;".
    %
:- func gen_init_struct_field(mlds_struct_id, mlds_module_name,
    mlds_field_info) = mlds_stmt is det.

gen_init_struct_field(StructId, ClassQualifier, FieldInfo) = Stmt :-
    FieldInfo = mlds_field_info(FieldVarName, FieldType, _GcStmt, Context),
    LocalVarName = lvn_field_var_as_local(FieldVarName),
    Param = ml_lval(ml_local_var(LocalVarName, FieldType)),
    StructType = mlds_struct_type(StructId),
    FieldId = ml_field_named(
        qual_field_var_name(ClassQualifier, type_qual, FieldVarName),
        mlds_ptr_type(StructType)),
    FieldLval = ml_field(yes(ptag(0u8)), ml_self(StructType), StructType,
        FieldId, FieldType),
    Stmt = ml_stmt_atomic(assign(FieldLval, Param), Context).

    % Generate "this->data_tag = <TagVal>;".
    %
:- func gen_init_tag(mlds_target_lang, mlds_class_id, mlds_class_id, int,
    prog_context) = mlds_stmt.

gen_init_tag(Target, CtorClassId, SecondaryTagClassId, TagVal, Context)
        = Stmt :-
    SecondaryTagClassId = mlds_class_id(TagClass, TagArity, _),
    TagClass = qual_class_name(BaseClassQualifier, QualKind, TagClassName),
    TagClassQualifier = mlds_append_class_qualifier(Target,
        BaseClassQualifier, QualKind, TagClassName, TagArity),
    Rval = ml_const(mlconst_int(TagVal)),
    CtorClassType = mlds_class_type(CtorClassId),
    FieldId = ml_field_named(
        qual_field_var_name(TagClassQualifier, type_qual, fvn_data_tag),
        mlds_ptr_type(mlds_class_type(SecondaryTagClassId))),
    FieldLval = ml_field(yes(ptag(0u8)), ml_self(CtorClassType), CtorClassType,
        FieldId, mlds_builtin_type_int(int_type_int)),
    Stmt = ml_stmt_atomic(assign(FieldLval, Rval), Context).

%---------------------%
%
% Wrappers around ml_gen_hld_du_ctor_field_gen that each generate definitions
% for particular kinds of fields. They increment the current argument number
% because they are called from folds over lists of arguments.
%

:- pred ml_gen_hld_du_ctor_typeclass_info_field(module_info::in,
    prog_context::in, prog_constraint::in,
    mlds_field_var_defn::out, mlds_field_info::out, int::in, int::out) is det.

ml_gen_hld_du_ctor_typeclass_info_field(ModuleInfo, Context, _Constraint,
        Defn, FieldInfo, !ArgNum) :-
    Type = typeclass_info_type,
    ml_gen_hld_du_ctor_field_gen(ModuleInfo, Context, !.ArgNum,
        no, Type, aw_full_word, Defn, FieldInfo),
    !:ArgNum = !.ArgNum + 1.

:- pred ml_gen_hld_du_ctor_type_info_field(module_info::in,
    prog_context::in, tvar::in,
    mlds_field_var_defn::out, mlds_field_info::out, int::in, int::out) is det.

ml_gen_hld_du_ctor_type_info_field(ModuleInfo, Context, TypeVar,
        Defn, FieldInfo, !ArgNum) :-
    % We don't have access to the correct kind here. This won't matter though,
    % since the type will only be checked to see that it is a variable,
    % and won't be used in any other way.
    Type = build_type_info_type(type_variable(TypeVar, kind_star)),
    ml_gen_hld_du_ctor_field_gen(ModuleInfo, Context, !.ArgNum,
        no, Type, aw_full_word, Defn, FieldInfo),
    !:ArgNum = !.ArgNum + 1.

:- pred ml_gen_hld_du_ctor_field(module_info::in, prog_context::in,
    constructor_arg_repn::in,
    mlds_field_var_defn::out, mlds_field_info::out, int::in, int::out) is det.

ml_gen_hld_du_ctor_field(ModuleInfo, Context, ArgRepn, Defn, FieldInfo,
        !ArgNum) :-
    ArgRepn = ctor_arg_repn(MaybeFieldName, Type, PosWidth, _Context),
    Width = arg_pos_width_to_width_only(PosWidth),
    ml_gen_hld_du_ctor_field_gen(ModuleInfo, Context, !.ArgNum,
        MaybeFieldName, Type, Width, Defn, FieldInfo),
    !:ArgNum = !.ArgNum + 1.

%---------------------%

:- pred ml_gen_hld_du_ctor_field_gen(module_info::in, prog_context::in,
    int::in, maybe(ctor_field_name)::in, mer_type::in, arg_width::in,
    mlds_field_var_defn::out, mlds_field_info::out) is det.

ml_gen_hld_du_ctor_field_gen(ModuleInfo, Context, ArgNum,
        MaybeFieldName, Type, Width, FieldVarDefn, FieldInfo) :-
    FieldVarName = ml_gen_hld_field_name(MaybeFieldName, ArgNum),
    DeclFlags = ml_gen_public_field_decl_flags,
    ( if ml_must_box_field_type(ModuleInfo, Type, Width) then
        MLDS_Type = mlds_generic_type
    else
        MLDS_Type = mercury_type_to_mlds_type(ModuleInfo, Type)
    ),
    % We only need GC tracing code for top-level variables, not for fields.
    GcStmt = gc_no_stmt,
    FieldVarDefn = mlds_field_var_defn(FieldVarName, Context, DeclFlags,
        MLDS_Type, no_initializer, GcStmt),
    FieldInfo = mlds_field_info(FieldVarName, MLDS_Type, GcStmt, Context).

%---------------------------------------------------------------------------%
%
% Miscellaneous helper routines.
%

ml_gen_class_name(TypeCtor, QualifiedTypeName, Arity) :-
    ml_gen_type_name(TypeCtor, MLDS_Module, TypeName, Arity),
    QualifiedTypeName = qual_class_name(MLDS_Module, module_qual, TypeName).

ml_gen_type_name(type_ctor(Name, Arity), MLDS_Module, TypeName, Arity) :-
    (
        Name = qualified(ModuleName, TypeName)
    ;
        % Builtin types like `int' may be still unqualified at this point.
        Name = unqualified(TypeName),
        ModuleName = mercury_public_builtin_module
    ),
    MLDS_Module = mercury_module_name_to_mlds(ModuleName).

ml_gen_du_ctor_name(CompilationTarget, TypeCtor, Name, Arity) = CtorName :-
    TypeCtor = type_ctor(TypeName, TypeArity),
    UnqualTypeName = unqualify_name(TypeName),
    CtorName = ml_gen_du_ctor_name_unqual_type(CompilationTarget,
        UnqualTypeName, TypeArity, Name, Arity).

ml_gen_du_ctor_name_unqual_type(CompilationTarget, UnqualTypeName, TypeArity,
        Name, Arity) = CtorName :-
    UnqualName = unqualify_name(Name),
    ( if
        ( CompilationTarget = ml_target_java
        ; CompilationTarget = ml_target_csharp
        ),
        UnqualName = UnqualTypeName,
        Arity = TypeArity
    then
        % In Java and C# we must not generate a class with the same name as its
        % enclosing class. We add the prefix to avoid that situation arising.
        % (A user may name another functor of the same type with "mr_" to
        % trigger the problem.)
        CtorName = "mr_" ++ UnqualName
    else
        CtorName = UnqualName
    ).

    % For interoperability, we ought to generate an `==' member for types
    % which have a user-defined equality, if the target language supports it
    % (as do e.g. C++, Java).
    % XXX I (zs) don't understand what kind of entity this member
    % is supposed to be.
    %
:- pred ml_gen_equality_members(maybe_canonical::in,
    list(mlds_field_var_defn)::out) is det.

% XXX generation of `==' members is not yet implemented.
ml_gen_equality_members(_, []).

%---------------------------------------------------------------------------%
%
% Routines for generating declaration flags.
%

    % Return the declaration flags appropriate for a type.
    %
:- func ml_gen_type_decl_flags = mlds_class_decl_flags.

ml_gen_type_decl_flags =
    mlds_class_decl_flags(class_public, overridable, modifiable).

    % Return the declaration flags appropriate for a member variable.
    %
:- func ml_gen_member_data_decl_flags = mlds_field_var_decl_flags.

ml_gen_member_data_decl_flags =
    mlds_field_var_decl_flags(per_instance, modifiable).

%---------------------------------------------------------------------------%

ctors_with_and_without_secondary_tag(CtorRepns, NumWith, NumWithout) :-
    ctors_with_and_without_secondary_tag_loop(CtorRepns,
        0, NumWith, 0, NumWithout).

:- pred ctors_with_and_without_secondary_tag_loop(list(constructor_repn)::in,
    int::in, int::out, int::in, int::out) is det.

ctors_with_and_without_secondary_tag_loop([],
        !NumWith, !NumWithout).
ctors_with_and_without_secondary_tag_loop([CtorRepn | CtorRepns],
        !NumWith, !NumWithout) :-
    TagVal = CtorRepn ^ cr_tag,
    MaybeSecTag = get_maybe_secondary_tag(TagVal),
    (
        MaybeSecTag = yes(_),
        !:NumWith = !.NumWith + 1
    ;
        MaybeSecTag = no,
        !:NumWithout = !.NumWithout + 1
    ),
    ctors_with_and_without_secondary_tag_loop(CtorRepns,
        !NumWith, !NumWithout).

%---------------------------------------------------------------------------%

ml_tag_uses_base_class(ConsTag) = UsesBaseClass :-
    % A constructor is represented using the base class (rather than
    % a derived class) if there is only a single functor.

    (
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        UsesBaseClass = ml_remote_args_tag_uses_base_class(RemoteArgsTagInfo)
    ;
        ConsTag = ground_term_const_tag(_ConstNum, SubTag),
        UsesBaseClass = ml_tag_uses_base_class(SubTag)
    ;
        ( ConsTag = string_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = int_tag(_)
        ; ConsTag = foreign_tag(_, _)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = direct_arg_tag(_)
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = local_args_tag(_)
        ; ConsTag = no_tag
        ; ConsTag = dummy_tag
        ),
        UsesBaseClass = tag_does_not_use_base_class
    ).

:- func ml_remote_args_tag_uses_base_class(remote_args_tag_info)
    = tag_uses_base_class.

ml_remote_args_tag_uses_base_class(RemoteArgsTagInfo) = UsesBaseClass :-
    % The ml_unify_gen_construct.m module has (currently) three predicates
    % that all do the job of this predicate interspersed with other jobs.
    % If this code changes, code there will have to change as well.
    (
        RemoteArgsTagInfo = remote_args_only_functor,
        UsesBaseClass = tag_uses_base_class
    ;
        ( RemoteArgsTagInfo = remote_args_unshared(_)
        ; RemoteArgsTagInfo = remote_args_shared(_, _)
        ; RemoteArgsTagInfo = remote_args_ctor(_)
        ),
        UsesBaseClass = tag_does_not_use_base_class
    ).

ml_target_uses_constructors(ml_target_c) = no.
ml_target_uses_constructors(ml_target_csharp) = yes.
ml_target_uses_constructors(ml_target_java) = yes.
% NOTE The information here is repeated in target_uses_constructors in
% du_type_layout.m; any changes here will require corresponding changes there.

%---------------------------------------------------------------------------%

ml_gen_exported_enums(ModuleInfo, MLDS_ExportedEnums) :-
     module_info_get_exported_enums(ModuleInfo, ExportedEnumInfo),
     list.map(ml_gen_exported_enum, ExportedEnumInfo, MLDS_ExportedEnums).

:- pred ml_gen_exported_enum(exported_enum_info::in,
    mlds_exported_enum::out) is det.

ml_gen_exported_enum(ExportedEnumInfo, MLDS_ExportedEnum) :-
    ExportedEnumInfo = exported_enum_info(TypeCtor, CtorRepns, Lang,
        Mapping, Context),
    ml_gen_type_name(TypeCtor, EnumModuleName, EnumClassName, EnumArity),
    MLDS_Type = mlds_enum_class_type(
        mlds_enum_class_id(EnumModuleName, EnumClassName, EnumArity)),
    list.map(
        generate_foreign_enum_constant(Mapping, MLDS_Type),
        CtorRepns, ExportConstants),
    MLDS_ExportedEnum = mlds_exported_enum(Lang, Context, TypeCtor,
        ExportConstants).

:- pred generate_foreign_enum_constant(map(string, string)::in,
    mlds_type::in, constructor_repn::in, mlds_exported_enum_constant::out)
    is det.

generate_foreign_enum_constant(Mapping, MLDS_Type, CtorRepn,
        ExportConstant) :-
    CtorRepn = ctor_repn(_, _, SymName, ConsTag, _, Arity, _),
    expect(unify(Arity, 0), $pred, "enum constant arity != 0"),
    enum_cons_tag_to_ml_const_rval(MLDS_Type, ConsTag, _EnumValue, ConstRval),

    Name = unqualify_name(SymName),
    map.lookup(Mapping, Name, ForeignName),
    ExportConstant = mlds_exported_enum_constant(ForeignName,
        init_obj(ConstRval)).

:- pred enum_cons_tag_to_ml_const_rval(mlds_type::in, cons_tag::in,
    mlds_enum_const::out, mlds_rval::out) is det.

enum_cons_tag_to_ml_const_rval(MLDS_Type, ConsTag, EnumConst, ConstRval) :-
    (
        ConsTag = int_tag(IntTag),
        (
            IntTag = int_tag_int(Int),
            EnumConst = mlds_enum_const_uint(uint.det_from_int(Int)),
            ConstRval = ml_const(mlconst_enum(Int, MLDS_Type))
        ;
            ( IntTag = int_tag_int8(_)
            ; IntTag = int_tag_int16(_)
            ; IntTag = int_tag_int32(_)
            ; IntTag = int_tag_int64(_)
            ; IntTag = int_tag_uint(_)
            ; IntTag = int_tag_uint8(_)
            ; IntTag = int_tag_uint16(_)
            ; IntTag = int_tag_uint32(_)
            ; IntTag = int_tag_uint64(_)
            ),
            unexpected($pred, "enum constant requires an int or foreign tag")
        )
    ;
        ConsTag = foreign_tag(Lang, String),
        EnumConst = mlds_enum_const_foreign(Lang, String, MLDS_Type),
        ConstRval = ml_const(mlconst_foreign(Lang, String, MLDS_Type))
    ;
        ConsTag = dummy_tag,
        % Dummy tags should never occur in an enum type, but we also generate
        % an enum class for dummy types.
        EnumConst = mlds_enum_const_uint(0u),
        ConstRval = ml_const(mlconst_enum(0, MLDS_Type))
    ;
        ( ConsTag = string_tag(_)
        ; ConsTag = float_tag(_)
        ; ConsTag = closure_tag(_, _, _)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = direct_arg_tag(_)
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = local_args_tag(_)
        ; ConsTag = remote_args_tag(_)
        ; ConsTag = no_tag
        ),
        unexpected($pred, "enum constant requires an int or foreign tag")
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_type_gen.
%---------------------------------------------------------------------------%
