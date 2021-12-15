%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2012, 2014 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module ml_backend.ml_unify_gen_util.
:- interface.

:- import_module hlds.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Convert a cons_id for a given type to a cons_tag.
    %
:- pred ml_cons_id_to_tag(ml_gen_info::in, cons_id::in, cons_tag::out) is det.

%---------------------------------------------------------------------------%

:- pred ml_type_as_field(module_info::in, bool::in, mer_type::in,
    arg_width::in, mer_type::out) is det.

%---------------------------------------------------------------------------%

    % Given a type and a cons_id, and also the types of the actual arguments
    % of that cons_id in some particular use of it, look up the original types
    % of the fields of that cons_id from the type definition. Note that the
    % field types need not be the same as the actual argument types; for
    % polymorphic types, the types of the actual arguments can be an instance
    % of the field types.
    %
:- pred ml_field_names_and_types(ml_gen_info::in, mer_type::in,
    cons_id::in, cell_offset::in, list(prog_var)::in,
    assoc_list(prog_var, constructor_arg_repn)::out) is det.

%---------------------------------------------------------------------------%

:- type may_have_extra_args
    --->    may_not_have_extra_args
    ;       may_have_extra_args.

:- type arg_type_and_width(Arg)
    --->    arg_type_and_width(Arg, mer_type, arg_pos_width).

:- type arg_var_type_and_width == arg_type_and_width(prog_var).
:- type arg_const_type_and_width == arg_type_and_width(const_struct_arg).

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
:- pred associate_cons_id_args_with_types_widths(module_info, arg_to_type(Arg),
    may_have_extra_args, mer_type, cons_id, list(Arg),
    list(arg_type_and_width(Arg))).
:- mode associate_cons_id_args_with_types_widths(in, in,
    in(bound(may_have_extra_args)), in, in, in, out) is det.
:- mode associate_cons_id_args_with_types_widths(in, in,
    in(bound(may_not_have_extra_args)), in, in, in, out) is det.

:- pred specified_arg_types_and_consecutive_full_words(mer_type::in, int::in,
    list(Arg)::in, list(arg_type_and_width(Arg))::out) is det.

%---------------------------------------------------------------------------%

    % Given a cons_tag, return its primary tag, and the integer offset
    % used to reference the first field of a structure for lowlevel data.
    % Abort if the tag indicates that the data doesn't have any fields.
    %
:- pred ml_tag_ptag_and_initial_offset(cons_tag::in, ptag::out,
    cell_offset::out) is det.

%---------------------------------------------------------------------------%

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
    cons_id::in, cons_tag::in, ptag::in, field_gen::out) is det.

%---------------------------------------------------------------------------%

    % ml_gen_secondary_tag_rval(Info, VarType, VarRval, Ptag, SectagRval):
    %
    % Return the rval for the secondary tag field of VarRval, assuming that
    % VarRval has the specified VarType and Ptag.
    %
    % Exported for use ml_tag_switch.m.
    %
:- pred ml_gen_secondary_tag_rval(ml_gen_info::in, mer_type::in, mlds_rval::in,
    ptag::in, mlds_rval::out) is det.

%---------------------------------------------------------------------------%

    % OR together the given rvals.
    %
:- func ml_bitwise_or_rvals(list(mlds_rval)) = mlds_rval.
:- func ml_bitwise_or_some_rvals(mlds_rval, list(mlds_rval)) = mlds_rval.
:- func ml_bitwise_or_two_rvals(mlds_rval, mlds_rval) = mlds_rval.

:- func ml_bitwise_mask(mlds_rval, int) = mlds_rval.

:- func ml_left_shift_rval(mlds_rval, arg_shift, fill_kind) = mlds_rval.

:- func ml_right_shift_rval(mlds_rval, arg_shift) = mlds_rval.

:- type ml_maybe_zero_const
    --->    ml_is_not_zero_const
    ;       ml_is_zero_const.

:- func ml_is_zero_const(mlds_rval_const) = ml_maybe_zero_const.

%---------------------------------------------------------------------------%

:- type assign_dir
    --->    assign_nondummy_left
    ;       assign_nondummy_right
    ;       assign_nondummy_unused
    ;       assign_dummy.

    % Figure out in which direction the assignment goes
    % between a field of a term, and the corresponding argument.
    %
    % This predicate differs from compute_assign_direction, because
    % the MLDS backend never declares MLDS variables for HLDS variables
    % of dummy types. It must therefore avoid all mention of such variables.
    % This is why it must distinguish assignments involving dummy values
    % even from assignments where the target is unused.
    %
:- pred ml_compute_assign_direction(module_info::in, unify_mode::in,
    mer_type::in, mer_type::in, assign_dir::out) is det.

%---------------------------------------------------------------------------%

:- pred local_primsectag_filled_bitfield(ml_gen_info::in,
    local_args_tag_info::in, filled_bitfield::out) is det.

:- pred remote_sectag_filled_bitfield(uint::in, sectag_bits::in,
    filled_bitfield::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module check_hlds.
:- import_module check_hlds.mode_top_functor.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_code_util.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_type_gen.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module term.
:- import_module uint8.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%

ml_cons_id_to_tag(Info, ConsId, ConsTag) :-
    ml_gen_info_get_module_info(Info, ModuleInfo),
    ConsTag = cons_id_to_tag(ModuleInfo, ConsId).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

ml_field_names_and_types(Info, Type, ConsId, InitOffset, ArgVars,
        ArgVarRepns) :-
    % Lookup the field types for the arguments of this cons_id.
    InitOffset = cell_offset(InitOffsetInt),
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
            ArgVarRepns = ExtraArgVarRepns ++ NonExtraArgVarRepns
        else
            assoc_list.from_corresponding_lists(ArgVars, CtorArgRepns,
                ArgVarRepns)
        )
    ).

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

%---------------------------------------------------------------------------%

associate_cons_id_args_with_types_widths(ModuleInfo, ArgToType,
        MayHaveExtraArgs, VarType, ConsId, Args, ArgsTypesWidths) :-
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
                    ConsRepnDefn ^ cr_tag = remote_args_tag(RemoteArgsTagInfo),
                    RemoteArgsTagInfo = remote_args_shared(_, RemoteSectag),
                    RemoteSectag = remote_sectag(_, SectagSize),
                    SectagSize = rsectag_word
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

:- pred lookup_type_and_allocate_consecutive_full_words(arg_to_type(Arg)::in,
    int::in, list(Arg)::in, list(arg_type_and_width(Arg))::out) is det.

lookup_type_and_allocate_consecutive_full_words(_, _, [], []).
lookup_type_and_allocate_consecutive_full_words(ArgToType, CurOffset,
        [Arg | Args], [ArgTypeWidth | ArgsTypesWidths]) :-
    PosWidth = apw_full(arg_only_offset(CurOffset), cell_offset(CurOffset)),
    ArgTypeWidth = arg_type_and_width(Arg, ArgToType(Arg), PosWidth),
    lookup_type_and_allocate_consecutive_full_words(ArgToType, CurOffset + 1,
        Args, ArgsTypesWidths).

specified_arg_types_and_consecutive_full_words(_, _, [], []).
specified_arg_types_and_consecutive_full_words(Type, CurOffset,
        [Arg | Args], [ArgTypeWidth | ArgsTypesWidths]) :-
    PosWidth = apw_full(arg_only_offset(CurOffset), cell_offset(CurOffset)),
    ArgTypeWidth = arg_type_and_width(Arg, Type, PosWidth),
    specified_arg_types_and_consecutive_full_words(Type, CurOffset + 1,
        Args, ArgsTypesWidths).

%---------------------------------------------------------------------------%

ml_tag_ptag_and_initial_offset(ConsTag, Ptag, InitOffset) :-
    % XXX ARG_PACK Check our callers whether this predicate is actually needed,
    % or whether our callers could get Ptag and InitOffset more cheaply.
    (
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            RemoteArgsTagInfo = remote_args_only_functor,
            Ptag = ptag(0u8),
            InitOffset = cell_offset(0)
        ;
            RemoteArgsTagInfo = remote_args_unshared(Ptag),
            InitOffset = cell_offset(0)
        ;
            RemoteArgsTagInfo = remote_args_shared(Ptag, RemoteSectag),
            RemoteSectag = remote_sectag(_, SectagSize),
            (
                SectagSize = rsectag_word,
                InitOffset = cell_offset(1)
            ;
                SectagSize = rsectag_subword(_),
                InitOffset = cell_offset(0)
            )
        ;
            RemoteArgsTagInfo = remote_args_ctor(_Data),
            Ptag = ptag(0u8),
            InitOffset = cell_offset(0)
        )
    ;
        ConsTag = direct_arg_tag(Ptag),
        InitOffset = cell_offset(0)
    ;
        ConsTag = ground_term_const_tag(_, SubTag),
        ml_tag_ptag_and_initial_offset(SubTag, Ptag, InitOffset)
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
        ; ConsTag = shared_local_tag_no_args(_, _, _)
        ; ConsTag = local_args_tag(_)
        ),
        unexpected($pred, "unexpected tag")
    ).

%---------------------------------------------------------------------------%

decide_field_gen(Info, VarLval, VarType, ConsId, ConsTag, Ptag, FieldGen) :-
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
        else if ConsId = cons(ConsSymName, ConsArity, ConsTypeCtor) then
            ml_gen_info_get_module_info(Info, ModuleInfo),
            ml_gen_info_get_target(Info, Target),
            % XXX ARG_PACK Delete this sanity test after it has been tested
            % for a while.
            type_to_ctor_det(VarType, VarTypeCtor),
            expect(unify(ConsTypeCtor, VarTypeCtor), $pred,
                "ConsTypeCtor != VarTypeCtor"),
            % With the high-level data representation, subtypes use the same
            % class as their base type constructor, whose field names are
            % derived from the base type constructor.
            module_info_get_type_table(ModuleInfo, TypeTable),
            ( if get_base_type_ctor(TypeTable, ConsTypeCtor, BaseTypeCtor) then
                TypeCtor = BaseTypeCtor
            else
                TypeCtor = ConsTypeCtor
            ),

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
                ConsName = ml_gen_du_ctor_name(Target, TypeCtor,
                    ConsSymName, ConsArity),
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
    FieldGen = field_gen(yes(Ptag), AddrRval, AddrType, FieldVia).

%---------------------------------------------------------------------------%

ml_gen_secondary_tag_rval(Info, VarType, Rval, Ptag, SectagFieldRval) :-
    ml_gen_info_get_high_level_data(Info, HighLevelData),
    ml_gen_info_get_module_info(Info, ModuleInfo),
    MLDS_VarType = mercury_type_to_mlds_type(ModuleInfo, VarType),
    IntType = mlds_builtin_type_int(int_type_int),
    (
        HighLevelData = no,
        % Note: with the low-level data representation, all fields are boxed,
        % even the secondary tag, and so we need to unbox (i.e. cast) it
        % back to the right type here.
        SectagFieldRval =
            ml_unbox(IntType,
                ml_lval(ml_field(yes(Ptag), Rval, MLDS_VarType,
                    ml_field_offset(ml_const(mlconst_int(0))),
                    mlds_generic_type)))
    ;
        HighLevelData = yes,
        ml_gen_info_get_target(Info, Target),
        FieldId = ml_gen_hl_tag_field_id(ModuleInfo, Target, VarType),
        SectagFieldRval = ml_lval(ml_field(yes(Ptag), Rval, MLDS_VarType,
            FieldId, IntType))
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
        TypeDefnBody = hlds_du_type(type_body_du(_, _, _, MaybeRepn, _)),
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

ml_bitwise_or_rvals(Rvals) = OrAllRval :-
    (
        Rvals = [],
        OrAllRval = ml_const(mlconst_int(0))
    ;
        Rvals = [HeadRval | TailRvals],
        OrAllRval = ml_bitwise_or_some_rvals(HeadRval, TailRvals)
    ).

ml_bitwise_or_some_rvals(HeadRval, TailRvals) = OrAllRval :-
    % We currently do this a linear fashion, starting at the rightmost
    % arguments, and moving towards the left.
    %
    % We should explore whether other strategies, such as balanced trees,
    % (or rather, trees that are as balanced as possible) would work better.
    (
        TailRvals = [],
        OrAllRval = HeadRval
    ;
        TailRvals = [HeadTailRval | TailTailRvals],
        TailOrAllRval = ml_bitwise_or_some_rvals(HeadTailRval, TailTailRvals),
        OrAllRval = ml_bitwise_or_two_rvals(HeadRval, TailOrAllRval)
    ).

ml_bitwise_or_two_rvals(RvalA, RvalB) = OrRval :-
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
        % OR-ing anything with zero has no effect.
        ( if
            ( RvalA = ml_const(mlconst_int(0))
            ; RvalA = ml_const(mlconst_uint(0u))
            )
        then
            UnboxRval = UnboxRvalB
        else if
            ( RvalB = ml_const(mlconst_int(0))
            ; RvalB = ml_const(mlconst_uint(0u))
            )
        then
            UnboxRval = UnboxRvalA
        else
            UnboxRval = ml_binop(bitwise_or(int_type_uint),
                UnboxRvalA, UnboxRvalB)
        ),
        (
            !.MaybeType = yes(BoxType),
            OrRval = ml_box(BoxType, UnboxRval)
        ;
            !.MaybeType = no,
            OrRval = UnboxRval
        )
    ).

ml_bitwise_mask(Rval, Mask) =
    ml_binop(bitwise_and(int_type_uint), Rval, ml_const(mlconst_int(Mask))).

%---------------------------------------------------------------------------%

ml_left_shift_rval(Rval, Shift, Fill) = ShiftedRval :-
    Shift = arg_shift(ShiftInt),
    ml_cast_to_unsigned_without_sign_extend(Fill, Rval, CastRval),
    ( if
        Rval = ml_const(mlconst_null(_))
    then
        % We may get nulls from unfilled fields. Replace them with zeroes
        % so we don't get type errors from the C compiler.
        % The shift amount does not matter, since shifting a zero
        % by any amount is a noop.
        ShiftedRval = ml_const(mlconst_uint(0u))
    else if
        (
            % Shifting anything by zero bits has no effect.
            ShiftInt = 0
        ;
            % Shifting a zero any number of bits has no effect.
            Rval = ml_const(Const),
            ml_is_zero_const(Const) = ml_is_zero_const
        )
    then
        ShiftedRval = CastRval
    else
        ShiftedRval =
            ml_binop(unchecked_left_shift(int_type_uint, shift_by_int),
                CastRval, ml_const(mlconst_int(ShiftInt)))
    ).

ml_right_shift_rval(Rval, Shift) = ShiftedRval :-
    % While ml_lshift may be called on a boxed Rval, ml_rshift will never
    % be called that way, which is why we don't handle that as a special case.
    Shift = arg_shift(ShiftInt),
    % XXX ARG_PACK Should we cast Rval to unsigned like left_shift_rval?
    ( if
        (
            % Shifting anything by zero bits has no effect.
            ShiftInt = 0
        ;
            % Shifting a zero any number of bits has no effect.
            Rval = ml_const(Const),
            ml_is_zero_const(Const) = ml_is_zero_const
        )
    then
        ShiftedRval = Rval
    else
        ShiftedRval =
            ml_binop(unchecked_right_shift(int_type_uint, shift_by_int),
                Rval, ml_const(mlconst_int(ShiftInt)))
    ).

%---------------------------------------------------------------------------%

ml_is_zero_const(Const) = IsZero :-
    (
        Const = mlconst_int(Int),
        IsZero =
            (if Int = 0 then ml_is_zero_const else ml_is_not_zero_const)
    ;
        Const = mlconst_uint(Uint),
        IsZero =
            (if Uint = 0u then ml_is_zero_const else ml_is_not_zero_const)
    ;
        Const = mlconst_int8(Int8),
        IsZero =
            (if Int8 = 0i8 then ml_is_zero_const else ml_is_not_zero_const)
    ;
        Const = mlconst_uint8(Uint8),
        IsZero =
            (if Uint8 = 0u8 then ml_is_zero_const else ml_is_not_zero_const)
    ;
        Const = mlconst_int16(Int16),
        IsZero =
            (if Int16 = 0i16 then ml_is_zero_const else ml_is_not_zero_const)
    ;
        Const = mlconst_uint16(Uint16),
        IsZero =
            (if Uint16 = 0u16 then ml_is_zero_const else ml_is_not_zero_const)
    ;
        Const = mlconst_int32(Int32),
        IsZero =
            (if Int32 = 0i32 then ml_is_zero_const else ml_is_not_zero_const)
    ;
        Const = mlconst_uint32(Uint32),
        IsZero =
            (if Uint32 = 0u32 then ml_is_zero_const else ml_is_not_zero_const)
    ;
        Const = mlconst_int64(Int64),
        IsZero =
            (if Int64 = 0i64 then ml_is_zero_const else ml_is_not_zero_const)
    ;
        Const = mlconst_uint64(Uint64),
        IsZero =
            (if Uint64 = 0u64 then ml_is_zero_const else ml_is_not_zero_const)
    ;
        ( Const = mlconst_null(_)
        % For the purposes of bit manipulation, null pointers are *not* zero,
        % because they need to be replaced by an int before they can take part
        % in bitwise operations.
        ; Const = mlconst_true
        ; Const = mlconst_false
        ; Const = mlconst_enum(_, _)
        ; Const = mlconst_foreign(_, _, _)
        ; Const = mlconst_named_const(_, _)
        ; Const = mlconst_float(_)
        ; Const = mlconst_char(_)
        ; Const = mlconst_string(_)
        ; Const = mlconst_multi_string(_)
        ; Const = mlconst_data_addr_global_var(_, _)
        ; Const = mlconst_data_addr_local_var(_)
        ; Const = mlconst_data_addr_rtti(_, _)
        ; Const = mlconst_data_addr_tabling(_, _)
        ; Const = mlconst_code_addr(_)
        ),
        IsZero = ml_is_not_zero_const
    ).

%---------------------------------------------------------------------------%

    % If a sub-word-sized signed integer has a negative value, then it will
    % have sign-extend bits *beyond* its usual size. OR-ing the raw form
    % of that sub-word-sized signed integer with the values of the other fields
    % may thus stomp all over the bits assigned to store the other fields
    % that are to the left of the sub-word-sized signed integer.
    %
    % Prevent this by casting sub-word-sized signed integers to their
    % unsigned counterparts before casting them to the word-sized unsigned type
    % that is the usual input type of shift and OR operations.
    %
:- pred ml_cast_to_unsigned_without_sign_extend(fill_kind::in,
    mlds_rval::in, mlds_rval::out) is det.

ml_cast_to_unsigned_without_sign_extend(Fill, Rval0, Rval) :-
    (
        Fill = fill_enum,
        % If we can (because the value to be cast is a constant),
        % make it unnecessary to add an explicit cast below.
        ( if Rval0 = ml_const(mlconst_enum(EnumInt, _Type)) then
            EnumUint = uint.det_from_int(EnumInt),
            Rval1 = ml_const(mlconst_uint(EnumUint))
        else
            Rval1 = Rval0
        )
    ;
        ( Fill = fill_uint8
        ; Fill = fill_uint16
        ; Fill = fill_uint32
        ; Fill = fill_char21
        ),
        Rval1 = Rval0
    ;
        (
            Fill = fill_int8,
            ToMLDSType = mlds_builtin_type_int(int_type_uint8)
        ;
            Fill = fill_int16,
            ToMLDSType = mlds_builtin_type_int(int_type_uint16)
        ;
            Fill = fill_int32,
            ToMLDSType = mlds_builtin_type_int(int_type_uint32)
        ),
        Rval1 = ml_cast(ToMLDSType, Rval0)
    ),
    ( if
        % Don't cast Rval1 to unsigned if it is *already* of that type.
        % Of course, other kinds of rvals may also be known to be unsigned,
        % but these two patterns cover the rvals that our callers give us.
        (
            % Unsigned constants are unsigned.
            Rval1 = ml_const(mlconst_uint(_))
        ;
            % Shifted unsigned constants are also unsigned.
            Rval1 = ml_binop(Binop, ml_const(mlconst_uint(_)), _),
            ( Binop = unchecked_left_shift(int_type_uint, _)
            ; Binop = unchecked_right_shift(int_type_uint, _)
            )
        )
    then
        Rval = Rval1
    else
        Rval = ml_cast(mlds_builtin_type_int(int_type_uint), Rval1)
    ).

%---------------------------------------------------------------------------%

ml_compute_assign_direction(ModuleInfo, ArgMode, ArgType, FieldType, Dir) :-
    ( if
        % XXX ARG_PACK We should not need to check here whether
        % FieldType is a dummy type; the arg_pos_width should tell us that.
        % Computing FieldType is expensive for our callers.
        is_either_type_a_dummy(ModuleInfo, ArgType, FieldType) =
            at_least_one_is_dummy_type
    then
        Dir = assign_dummy
    else
        % The test of the code in this predicate is the same as
        % the code of compute_assign_direction, with one exception
        % that prevents any simple kind of code reuse: the fact that
        % we return assign_nondummy_X instead of assign_X.
        %
        % Any change here will require a corresponding change there.
        ArgMode = unify_modes_li_lf_ri_rf(LeftInitInst, LeftFinalInst,
            RightInitInst, RightFinalInst),
        init_final_insts_to_top_functor_mode(ModuleInfo,
            LeftInitInst, LeftFinalInst, ArgType, LeftTopMode),
        init_final_insts_to_top_functor_mode(ModuleInfo,
            RightInitInst, RightFinalInst, ArgType, RightTopMode),
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

local_primsectag_filled_bitfield(Info, LocalArgsTagInfo, FilledBitfield) :-
    (
        LocalArgsTagInfo = local_args_only_functor,
        PrimSec = 0u,
        NumPrimSecBits = 0
    ;
        LocalArgsTagInfo = local_args_not_only_functor(_Ptag, LocalSectag),
        LocalSectag = local_sectag(_Sectag, PrimSec, SectagBits),
        ml_gen_info_get_num_ptag_bits(Info, NumPtagsBitsUint8),
        SectagBits = sectag_bits(SectagNumBitsUint8, _SectagMaskUint),
        NumPrimSecBits =
            uint8.cast_to_int(NumPtagsBitsUint8 + SectagNumBitsUint8)
    ),
    ArgNumBits = arg_num_bits(NumPrimSecBits),
    Bitfield = bitfield(arg_shift(0), ArgNumBits, fill_enum),
    BitfieldValue = bv_const(PrimSec),
    FilledBitfield = filled_bitfield(Bitfield, BitfieldValue).

remote_sectag_filled_bitfield(SectagUint, SectagBits, FilledBitfield) :-
    SectagBits = sectag_bits(SectagNumBitsUint8, _SectagMaskUint),
    ArgNumBits = arg_num_bits(uint8.cast_to_int(SectagNumBitsUint8)),
    Bitfield = bitfield(arg_shift(0), ArgNumBits, fill_enum),
    BitfieldValue = bv_const(SectagUint),
    FilledBitfield = filled_bitfield(Bitfield, BitfieldValue).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_unify_gen_util.
%---------------------------------------------------------------------------%
