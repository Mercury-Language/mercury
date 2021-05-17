%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_global_data.m.
% Main author: zs.
%
% This module is part of the MLDS code generator. It handles the generation
% of data structures that are "born global", i.e. they belong to the generated
% module as a whole, not to any particular function in it.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_global_data.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bimap.
:- import_module cord.
:- import_module counter.
:- import_module list.
:- import_module map.
:- import_module maybe.

    % This abstract type represents the MLDS code generator's repository of
    % data structures that are "born global", i.e. the ones for which we
    % known from the start that they will be defined at file scope.
    %
:- type ml_global_data.

:- type use_common_cells
    --->    do_not_use_common_cells
    ;       use_common_cells.

:- type have_unboxed_floats
    --->    have_unboxed_floats
    ;       do_not_have_unboxed_floats.

:- type have_unboxed_int64s
    --->    have_unboxed_int64s
    ;       do_not_have_unboxed_int64s.

:- type ml_scalar_cell_map ==
    map(ml_scalar_common_type_num, ml_scalar_cell_group).

:- type ml_scalar_cell_group
    --->    ml_scalar_cell_group(
                mscg_type           :: mlds_type,
                mscg_array_size     :: initializer_array_size,

                mscg_counter        :: counter, % next cell number
                mscg_members        :: bimap(mlds_initializer,
                                        mlds_scalar_common),
                mscg_rows           :: cord(mlds_initializer)
            ).

:- type ml_vector_cell_map ==
    map(ml_vector_common_type_num, ml_vector_cell_group).

:- type ml_vector_cell_group
    --->    ml_vector_cell_group(
                mvcg_type           :: mlds_type,
                mvcg_type_defn      :: mlds_class_defn,
                mvcg_field_ids      :: list(mlds_field_id),

                mvcg_next_row       :: int,
                mvcg_rows           :: cord(mlds_initializer)
            ).

:- type ml_alloc_site_data
    --->    ml_alloc_site_data(
                masd_proc_label     :: mlds_function_name,
                masd_context        :: prog_context,
                masd_type           :: string,
                masd_size           :: int
            ).

    % Initialize the ml_global_data structure to a value that represents
    % no global data structures known yet.
    %
:- func ml_global_data_init(mlds_target_lang, use_common_cells,
    have_unboxed_floats, have_unboxed_int64s) = ml_global_data.

    % ml_global_data_get_all_global_defns(GlobalData,
    %     ScalarCellGroupMap, VectorCellGroupMap, AllocIds,
    %     RttiDefns, CellDefns, ClosureWrapperFuncDefns):
    %
    % Get all the global definitions implicit in the argument.
    %
:- pred ml_global_data_get_all_global_defns(ml_global_data::in,
    ml_scalar_cell_map::out, ml_vector_cell_map::out,
    assoc_list(mlds_alloc_id, ml_alloc_site_data)::out,
    list(mlds_global_var_defn)::out, list(mlds_global_var_defn)::out,
    list(mlds_function_defn)::out) is det.

:- pred ml_global_data_get_target(ml_global_data::in,
    mlds_target_lang::out) is det.
:- pred ml_global_data_have_unboxed_floats(ml_global_data::in,
    have_unboxed_floats::out) is det.
:- pred ml_global_data_have_unboxed_int64s(ml_global_data::in,
    have_unboxed_int64s::out) is det.

    % This type maps the names of rtti data structures that have already been
    % generated to the rval that refers to that data structure, and its type.
    %
    % The code generator looks up this map whenever it is needs a reference to
    % an RTTI data structure that may or may not have been generated before.
    % If it finds the id of that data structure in this map, it uses the
    % corresponding value *without* generating the duplicate definition.
    % Of course, this requires the code generator to add the original
    % definition of the data structure to the ml_globals_data whenever it adds
    % new entries to this map.
    %
    % At the moment, the only data structures that are potentially duplicated
    % are the representations of type_infos and pseudo_type_infos, so only
    % these rtti_ids will be in this map.
    %
:- type ml_rtti_rval_type_map == map(rtti_id, ml_rval_and_type).
:- type ml_rval_and_type
    --->    ml_rval_and_type(
                mlds_rval,
                mlds_type
            ).

    % Return the component of the given ml_global_data that contains
    % information about potentially duplicated RTTI global definitions.
    %
:- pred ml_global_data_get_pdup_rval_type_map(ml_global_data::in,
    ml_rtti_rval_type_map::out) is det.

    % ml_global_data_get_closure_wrapper_func_defns(GlobalData,
    %   ClosureWrapperFuncDefns):
    % ml_global_data_set_closure_wrapper_func_defns(ClosureWrapperFuncDefns,
    %   !GlobalData):
    %
    % Get and set the cord of closure wrapper function definitions.
    % Like other function definitions, these may need flattening
    % by ml_elim_nested.m.
    %
:- pred ml_global_data_get_closure_wrapper_func_defns(ml_global_data::in,
    cord(mlds_function_defn)::out) is det.
:- pred ml_global_data_set_closure_wrapper_func_defns(
    cord(mlds_function_defn)::in,
    ml_global_data::in, ml_global_data::out) is det.

    % Map the given rtti_id to the given ml_rval_and_type, and record the
    % generation of the given global data definitions.
    %
:- pred ml_global_data_add_pdup_rtti_id(rtti_id::in, ml_rval_and_type::in,
    ml_global_data::in, ml_global_data::out) is det.

    % Add to the global data structure one or more definitions that are
    % "unique by construction". Some of these are guaranteed to be flat
    % definitions (definitions for which ml_elim_nested is an identity
    % operation), while some have no such guarantee.
    %
:- pred ml_global_data_add_rtti_defn(mlds_global_var_defn::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_add_closure_wrapper_func_defns(
    list(mlds_function_defn)::in,
    ml_global_data::in, ml_global_data::out) is det.

    % Generate a definition for a static scalar constant, given the constant's
    % name prefix, type, and initializer (access is always acc_private),
    % and return a reference to the constant itself (not its address).
    %
:- pred ml_gen_static_scalar_const_value(mlds_module_name::in,
    mlds_global_const_var::in, mlds_type::in, mlds_initializer::in,
    prog_context::in, mlds_rval::out,
    ml_global_data::in, ml_global_data::out) is det.

    % Generate a definition for a static scalar constant, given the constant's
    % name prefix, type, and initializer (access is always acc_private),
    % and return a reference to the constant's address.
    %
:- pred ml_gen_static_scalar_const_addr(mlds_module_name::in,
    mlds_global_const_var::in, mlds_type::in, mlds_initializer::in,
    prog_context::in, mlds_rval::out,
    ml_global_data::in, ml_global_data::out) is det.

    % Look up (and if necessary, create) the type of the structure needed
    % to hold a vector of values of the given types. Return the number of the
    % type, so that it can be given to ml_gen_static_vector_defn later,
    % and both the type itself and the names of its fields, so that they
    % can be used in code that builds and/or uses the vector.
    %
:- pred ml_gen_static_vector_type(mlds_module_name::in, prog_context::in,
    mlds_target_lang::in, list(mlds_type)::in,
    ml_vector_common_type_num::out, mlds_type::out, list(mlds_field_id)::out,
    ml_global_data::in, ml_global_data::out) is det.

    % Generate a definition for a static vector constant, given the constant's
    % type and an initializer for each row.
    %
:- pred ml_gen_static_vector_defn(mlds_module_name::in,
    ml_vector_common_type_num::in, list(mlds_initializer)::in,
    mlds_vector_common::out, ml_global_data::in, ml_global_data::out) is det.

    % Generate or look up an allocation site.
    %
:- pred ml_gen_alloc_site(mlds_function_name::in, maybe(cons_id)::in, int::in,
    prog_context::in, mlds_alloc_id::out,
    ml_global_data::in, ml_global_data::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module ml_backend.ml_type_gen.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- type ml_scalar_cell_type
    --->    ml_scalar_cell_type(mlds_type, initializer_array_size).
:- type ml_scalar_cell_type_map
    == map(ml_scalar_cell_type, ml_scalar_common_type_num).

:- type ml_vector_cell_type_map
    == map(list(mlds_type), ml_vector_common_type_num).

:- type ml_alloc_id_map
    == bimap(mlds_alloc_id, ml_alloc_site_data).

:- type ml_global_data
    --->    ml_global_data(
                mgd_pdup_rval_type_map          :: ml_rtti_rval_type_map,
                mgd_target                      :: mlds_target_lang,
                mgd_use_common_cells            :: use_common_cells,
                mgd_have_unboxed_floats         :: have_unboxed_floats,
                mgd_have_unboxed_int64s         :: have_unboxed_int64s,
                mgd_const_counter               :: counter,

                mgd_cell_defns                  :: cord(mlds_global_var_defn),
                mgd_rtti_defns                  :: cord(mlds_global_var_defn),
                mgd_closure_wrapper_funcs       :: cord(mlds_function_defn),

                mgd_cell_type_counter           :: counter,

                mgd_scalar_type_num_map         :: ml_scalar_cell_type_map,
                mgd_scalar_cell_group_map       :: ml_scalar_cell_map,

                mgd_vector_type_num_map         :: ml_vector_cell_type_map,
                mgd_vector_cell_group_map       :: ml_vector_cell_map,

                mgd_alloc_id_counter            :: counter,
                mgd_alloc_id_map                :: ml_alloc_id_map
            ).

%---------------------------------------------------------------------------%

ml_global_data_init(Target, UseCommonCells,
        HaveUnboxedFloats, HaveUnboxedInt64s) = GlobalData :-
    GlobalData = ml_global_data(map.init, Target, UseCommonCells,
        HaveUnboxedFloats, HaveUnboxedInt64s, counter.init(1),
        cord.init, cord.init, cord.init,
        counter.init(1), map.init, map.init, map.init, map.init,
        counter.init(0), bimap.init).

ml_global_data_get_all_global_defns(GlobalData,
        ScalarCellGroupMap, VectorCellGroupMap, AllocIds,
        RttiDefns, CellDefns, ClosureWrapperFuncDefns) :-
    GlobalData = ml_global_data(_PDupRvalTypeMap, _Target, _UseCommonCells,
        _HaveUnboxedFloats, _HaveUnboxedInt64s, _ConstCounter,
        CellDefnsCord, RttiDefnsCord, ClosureWrapperFuncDefnsCord,
        _TypeNumCounter,
        _ScalarTypeNumMap, ScalarCellGroupMap,
        _VectorTypeNumMap, VectorCellGroupMap,
        _AllocIdNumCounter, AllocIdMap),
    bimap.to_assoc_list(AllocIdMap, AllocIds),
    RttiDefns = cord.to_list(RttiDefnsCord),
    CellDefns = cord.to_list(CellDefnsCord),
    ClosureWrapperFuncDefns = cord.to_list(ClosureWrapperFuncDefnsCord).

%---------------------------------------------------------------------------%
%
% Access predicates for the ml_global_data type.
%

:- pred ml_global_data_get_const_counter(ml_global_data::in,
    counter::out) is det.
:- pred ml_global_data_get_cell_defns(ml_global_data::in,
    cord(mlds_global_var_defn)::out) is det.
:- pred ml_global_data_get_rtti_defns(ml_global_data::in,
    cord(mlds_global_var_defn)::out) is det.

ml_global_data_get_target(GlobalData, X) :-
    X = GlobalData ^ mgd_target.
ml_global_data_have_unboxed_floats(GlobalData, X) :-
    X = GlobalData ^ mgd_have_unboxed_floats.
ml_global_data_have_unboxed_int64s(GlobalData, X) :-
    X = GlobalData ^ mgd_have_unboxed_int64s.
ml_global_data_get_pdup_rval_type_map(GlobalData, X) :-
    X = GlobalData ^ mgd_pdup_rval_type_map.
ml_global_data_get_const_counter(GlobalData, X) :-
    X = GlobalData ^ mgd_const_counter.
ml_global_data_get_cell_defns(GlobalData, X) :-
    X = GlobalData ^ mgd_cell_defns.
ml_global_data_get_rtti_defns(GlobalData, X) :-
    X = GlobalData ^ mgd_rtti_defns.
ml_global_data_get_closure_wrapper_func_defns(GlobalData, X) :-
    X = GlobalData ^ mgd_closure_wrapper_funcs.

:- pred ml_global_data_set_pdup_rval_type_map(ml_rtti_rval_type_map::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_set_const_counter(counter::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_set_cell_defns(cord(mlds_global_var_defn)::in,
    ml_global_data::in, ml_global_data::out) is det.
:- pred ml_global_data_set_rtti_defns(cord(mlds_global_var_defn)::in,
    ml_global_data::in, ml_global_data::out) is det.

ml_global_data_set_pdup_rval_type_map(X, !GlobalData) :-
    !GlobalData ^ mgd_pdup_rval_type_map := X.
ml_global_data_set_const_counter(X, !GlobalData) :-
    !GlobalData ^ mgd_const_counter := X.
ml_global_data_set_cell_defns(X, !GlobalData) :-
    !GlobalData ^ mgd_cell_defns := X.
ml_global_data_set_rtti_defns(X, !GlobalData) :-
    !GlobalData ^ mgd_rtti_defns := X.
ml_global_data_set_closure_wrapper_func_defns(X, !GlobalData) :-
    !GlobalData ^ mgd_closure_wrapper_funcs := X.

%---------------------------------------------------------------------------%

ml_global_data_add_pdup_rtti_id(RttiId, RvalType, !GlobalData) :-
    ml_global_data_get_pdup_rval_type_map(!.GlobalData, PDupRvalTypeMap0),
    map.det_insert(RttiId, RvalType, PDupRvalTypeMap0, PDupRvalTypeMap),
    ml_global_data_set_pdup_rval_type_map(PDupRvalTypeMap, !GlobalData).

ml_global_data_add_rtti_defn(Defn, !GlobalData) :-
    ml_global_data_get_rtti_defns(!.GlobalData, RttiDefns0),
    RttiDefns = cord.snoc(RttiDefns0, Defn),
    ml_global_data_set_rtti_defns(RttiDefns, !GlobalData).

ml_global_data_add_closure_wrapper_func_defns(FuncDefns, !GlobalData) :-
    ml_global_data_get_closure_wrapper_func_defns(!.GlobalData,
        ClosureWrapperDefns0),
    ClosureWrapperDefns = ClosureWrapperDefns0 ++ cord.from_list(FuncDefns),
    ml_global_data_set_closure_wrapper_func_defns(ClosureWrapperDefns,
        !GlobalData).

%---------------------------------------------------------------------------%

ml_gen_static_scalar_const_value(MLDS_ModuleName, ConstVarKind, ConstType0,
        Initializer0, Context, DataRval, !GlobalData) :-
    ml_maybe_specialize_generic_array_type(ConstType0, ConstType,
        Initializer0, Initializer),
    UseCommonCells = !.GlobalData ^ mgd_use_common_cells,
    (
        UseCommonCells = use_common_cells,
        ml_gen_scalar_static_defn(MLDS_ModuleName, ConstType, Initializer,
            Common, !GlobalData),
        DataRval = ml_scalar_common(Common)
    ;
        UseCommonCells = do_not_use_common_cells,
        ml_gen_plain_static_defn(ConstVarKind, ConstType, Initializer,
            Context, VarName, !GlobalData),
        QualVarName = qual_global_var_name(MLDS_ModuleName, VarName),
        DataVar = ml_global_var(QualVarName, ConstType),
        DataRval = ml_lval(DataVar)
    ).

ml_gen_static_scalar_const_addr(MLDS_ModuleName, ConstVarKind, ConstType0,
        Initializer0, Context, DataAddrRval, !GlobalData) :-
    ml_maybe_specialize_generic_array_type(ConstType0, ConstType,
        Initializer0, Initializer),
    UseCommonCells = !.GlobalData ^ mgd_use_common_cells,
    (
        UseCommonCells = use_common_cells,
        ml_gen_scalar_static_defn(MLDS_ModuleName, ConstType, Initializer,
            Common, !GlobalData),
        DataAddrRval = ml_scalar_common_addr(Common)
    ;
        UseCommonCells = do_not_use_common_cells,
        ml_gen_plain_static_defn(ConstVarKind, ConstType, Initializer,
            Context, VarName, !GlobalData),
        DataAddrRval =
            ml_const(mlconst_data_addr_global_var(MLDS_ModuleName, VarName))
    ).

:- pred ml_gen_scalar_static_defn(mlds_module_name::in, mlds_type::in,
    mlds_initializer::in, mlds_scalar_common::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_scalar_static_defn(MLDS_ModuleName, ConstType, Initializer, Common,
        !GlobalData) :-
    InitArraySize = get_initializer_array_size(Initializer),
    CellType = ml_scalar_cell_type(ConstType, InitArraySize),
    some [!CellGroup] (
        TypeNumMap0 = !.GlobalData ^ mgd_scalar_type_num_map,
        CellGroupMap0 = !.GlobalData ^ mgd_scalar_cell_group_map,
        ( if map.search(TypeNumMap0, CellType, OldTypeNum) then
            TypeNum = OldTypeNum,
            ( if map.search(CellGroupMap0, TypeNum, !:CellGroup) then
                true
            else
                !:CellGroup = ml_scalar_cell_group(ConstType,
                    InitArraySize, counter.init(0), bimap.init, cord.empty)
            )
        else
            TypeNumCounter0 = !.GlobalData ^ mgd_cell_type_counter,
            counter.allocate(TypeRawNum, TypeNumCounter0, TypeNumCounter),
            TypeNum = ml_scalar_common_type_num(TypeRawNum),
            !GlobalData ^ mgd_cell_type_counter := TypeNumCounter,

            map.det_insert(CellType, TypeNum, TypeNumMap0, TypeNumMap),
            !GlobalData ^ mgd_scalar_type_num_map := TypeNumMap,

            !:CellGroup = ml_scalar_cell_group(ConstType,
                InitArraySize, counter.init(0), bimap.init, cord.empty)
        ),

        RowCounter0 = !.CellGroup ^ mscg_counter,
        counter.allocate(RowNum, RowCounter0, RowCounter),
        MembersMap0 = !.CellGroup ^ mscg_members,
        NewCommon =
            mlds_scalar_common(MLDS_ModuleName, ConstType, TypeNum, RowNum),
        bimap.search_insert(Initializer, NewCommon, MaybeOldCommon,
            MembersMap0, MembersMap),
        (
            MaybeOldCommon = yes(OldCommon),
            % We cannot get here if !.CellGroup wasn't found in CellGroupMap0.
            Common = OldCommon
        ;
            MaybeOldCommon = no,
            Common = NewCommon,
            !CellGroup ^ mscg_counter := RowCounter,
            !CellGroup ^ mscg_members := MembersMap,

            Rows0 = !.CellGroup ^ mscg_rows,
            Rows = cord.snoc(Rows0, Initializer),
            !CellGroup ^ mscg_rows := Rows,

            map.set(TypeNum, !.CellGroup, CellGroupMap0, CellGroupMap),
            !GlobalData ^ mgd_scalar_cell_group_map := CellGroupMap
        )
    ).

:- pred ml_gen_plain_static_defn(mlds_global_const_var::in, mlds_type::in,
    mlds_initializer::in, prog_context::in, mlds_global_var_name::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_plain_static_defn(ConstVarKind, ConstType,
        Initializer, Context, VarName, !GlobalData) :-
    ml_global_data_get_const_counter(!.GlobalData, ConstCounter0),
    counter.allocate(ConstNum, ConstCounter0, ConstCounter),
    ml_global_data_set_const_counter(ConstCounter, !GlobalData),

    VarName = gvn_const_var(ConstVarKind, ConstNum),
    % The GC never needs to trace static constants, because they can never
    % point into the heap; they can point only to other static constants.
    GCStatement = gc_no_stmt,
    DeclFlags = mlds_global_var_decl_flags(gvar_acc_module_only, const),
    DataDefn = mlds_global_var_defn(VarName, Context, DeclFlags, ConstType,
        Initializer, GCStatement),

    ml_global_data_get_cell_defns(!.GlobalData, CellDefns0),
    CellDefns = cord.snoc(CellDefns0, DataDefn),
    ml_global_data_set_cell_defns(CellDefns, !GlobalData).

:- pred ml_maybe_specialize_generic_array_type(mlds_type::in, mlds_type::out,
    mlds_initializer::in, mlds_initializer::out) is det.

ml_maybe_specialize_generic_array_type(ConstType0, ConstType,
        Initializer0, Initializer) :-
    ( if
        ConstType0 = mlds_array_type(mlds_generic_type),
        Initializer0 = init_array(Inits0),
        list.map2(ml_specialize_generic_array_init, Inits0, Inits, Types),
        % XXX ARG_PACK Specialize int64 and uint64 elements as well.
        list.member(mlds_builtin_type_float, Types)
    then
        ConstType = mlds_mostly_generic_array_type(Types),
        Initializer = init_array(Inits)
    else
        ConstType = ConstType0,
        Initializer = Initializer0
    ).

:- pred ml_specialize_generic_array_init(mlds_initializer::in,
    mlds_initializer::out, mlds_type::out) is det.

ml_specialize_generic_array_init(Init0, Init, Type) :-
    ( if
        Init0 = init_obj(Rval0),
        % XXX ARG_PACK Specialize int64 and uint64 elements as well.
        ml_specialize_generic_array_rval(Rval0, Rval)
    then
        Init = init_obj(Rval),
        Type = mlds_builtin_type_float
    else
        Init = Init0,
        Type = mlds_generic_type
    ).

:- pred ml_specialize_generic_array_rval(mlds_rval::in, mlds_rval::out)
    is semidet.

ml_specialize_generic_array_rval(!Rval) :-
    (
        !.Rval = ml_const(mlconst_float(_))
    ;
        ( !.Rval = ml_box(Type, SubRval)
        ; !.Rval = ml_unbox(Type, SubRval)
        ; !.Rval = ml_cast(Type, SubRval)
        ),
        % XXX ARG_PACK Specialize for int64s and uint64s as well.
        (
            Type = mlds_builtin_type_float,
            !:Rval = SubRval
        ;
            Type = mercury_nb_type(_, CtorCat),
            CtorCat = ctor_cat_user(cat_user_notag),
            ml_specialize_generic_array_rval(SubRval, !:Rval)
        )
    ;
        !.Rval = ml_binop(Op, _, _),
        ml_specialize_generic_array_binop(Op, yes)
    ).

:- pred ml_specialize_generic_array_binop(binary_op::in, bool::out) is det.

ml_specialize_generic_array_binop(Op, IsFloat) :-
    (
        ( Op = int_add(_)
        ; Op = int_sub(_)
        ; Op = int_mul(_)
        ; Op = int_div(_)
        ; Op = int_mod(_)
        ; Op = unchecked_left_shift(_, _)
        ; Op = unchecked_right_shift(_, _)
        ; Op = bitwise_and(_)
        ; Op = bitwise_or(_)
        ; Op = bitwise_xor(_)
        ; Op = logical_and
        ; Op = logical_or
        ; Op = eq(_)
        ; Op = ne(_)
        ; Op = offset_str_eq(_)
        ; Op = str_eq
        ; Op = str_ne
        ; Op = str_lt
        ; Op = str_gt
        ; Op = str_le
        ; Op = str_ge
        ; Op = str_cmp
        ; Op = int_lt(_)
        ; Op = int_gt(_)
        ; Op = int_le(_)
        ; Op = int_ge(_)
        ; Op = unsigned_lt
        ; Op = unsigned_le
        ; Op = float_eq
        ; Op = float_ne
        ; Op = float_lt
        ; Op = float_gt
        ; Op = float_le
        ; Op = float_ge
        ; Op = body
        ; Op = array_index(_)   % should not be an initializer anyway
        ; Op = string_unsafe_index_code_unit
        ; Op = compound_eq
        ; Op = compound_lt
        ; Op = pointer_equal_conservative
        ; Op = int64_from_dword
        ; Op = uint64_from_dword
        ),
        IsFloat = no
    ;
        ( Op = float_add
        ; Op = float_sub
        ; Op = float_mul
        ; Op = float_div
        ; Op = float_from_dword
        ),
        IsFloat = yes
    ).

%---------------------------------------------------------------------------%

ml_gen_static_vector_type(MLDS_ModuleName, Context, Target, ArgTypes,
        TypeNum, StructType, FieldIds, !GlobalData) :-
    TypeNumMap0 = !.GlobalData ^ mgd_vector_type_num_map,
    ( if map.search(TypeNumMap0, ArgTypes, OldTypeNum) then
        TypeNum = OldTypeNum,
        CellGroupMap = !.GlobalData ^ mgd_vector_cell_group_map,
        map.lookup(CellGroupMap, TypeNum, CellGroup),
        CellGroup = ml_vector_cell_group(StructType, _TypeDefn, FieldIds,
            _, _)
    else
        TypeNumCounter0 = !.GlobalData ^ mgd_cell_type_counter,
        counter.allocate(TypeRawNum, TypeNumCounter0, TypeNumCounter),
        TypeRawNumStr = string.int_to_string(TypeRawNum),
        TypeNum = ml_vector_common_type_num(TypeRawNum),
        !GlobalData ^ mgd_cell_type_counter := TypeNumCounter,

        map.det_insert(ArgTypes, TypeNum, TypeNumMap0, TypeNumMap),
        !GlobalData ^ mgd_vector_type_num_map := TypeNumMap,

        FieldFlags = mlds_field_var_decl_flags(per_instance, const),
        ml_gen_vector_cell_field_types(Context, FieldFlags,
            TypeRawNum, 0, ArgTypes, FieldNames, FieldDefns, FieldInfos),

        StructClassName = "vector_common_type_" ++ TypeRawNumStr,
        QualStructClassName =
            qual_class_name(MLDS_ModuleName, module_qual, StructClassName),
        StructClassId = mlds_class_id(QualStructClassName, 0, mlds_struct),
        % The "modifiable" is only to shut up a gcc warning about constant
        % fields.
        StructClassFlags =
            mlds_class_decl_flags(class_private, sealed, modifiable),
        (
            Target = ml_target_c,
            ClassKind = mlds_struct,
            CtorDefns = []
        ;
            (
                Target = ml_target_java,
                ClassKind = mlds_class
            ;
                Target = ml_target_csharp,
                ClassKind = mlds_struct
            ),
            CtorDefn = ml_gen_constructor_function(Target, StructClassId,
                StructClassId, MLDS_ModuleName, StructClassId, no, FieldInfos,
                Context),
            CtorDefns = [CtorDefn]
        ),
        StructClassDefn = mlds_class_defn(StructClassName, 0, Context,
            StructClassFlags, ClassKind, [], inherits_nothing, [], [],
            FieldDefns, [], [], CtorDefns),

        MLDS_ClassModuleName = mlds_append_class_qualifier_module_qual(
            MLDS_ModuleName, StructClassName, 0),
        StructType = mlds_class_type(StructClassId),
        make_named_fields(MLDS_ClassModuleName, StructType, FieldNames,
           FieldIds),

        CellGroup = ml_vector_cell_group(StructType, StructClassDefn,
            FieldIds, 0, cord.empty),

        CellGroupMap0 = !.GlobalData ^ mgd_vector_cell_group_map,
        map.det_insert(TypeNum, CellGroup, CellGroupMap0, CellGroupMap),
        !GlobalData ^ mgd_vector_cell_group_map := CellGroupMap
    ).

:- pred ml_gen_vector_cell_field_types(prog_context::in,
    mlds_field_var_decl_flags::in, int::in, int::in, list(mlds_type)::in,
    list(mlds_field_var_name)::out, list(mlds_field_var_defn)::out,
    list(mlds_field_info)::out) is det.

ml_gen_vector_cell_field_types(_, _, _, _, [], [], [], []).
ml_gen_vector_cell_field_types(Context, Flags, TypeRawNum, FieldNum,
        [Type | Types], [FieldVarName | FieldVarNames],
        [FieldDataDefn | FieldDataDefns], [FieldInfo | FieldInfos]) :-
    FieldVarName = fvn_global_data_field(TypeRawNum, FieldNum),
    FieldDataDefn = mlds_field_var_defn(FieldVarName, Context, Flags,
        Type, no_initializer, gc_no_stmt),
    FieldInfo = mlds_field_info(FieldVarName, Type, gc_no_stmt, Context),
    ml_gen_vector_cell_field_types(Context, Flags, TypeRawNum,
        FieldNum + 1, Types, FieldVarNames, FieldDataDefns, FieldInfos).

:- pred make_named_fields(mlds_module_name::in, mlds_type::in,
    list(mlds_field_var_name)::in, list(mlds_field_id)::out) is det.

make_named_fields(_, _, [], []).
make_named_fields(MLDS_ModuleName, StructType, [FieldName | FieldNames],
        [FieldId | FieldIds]) :-
    QualName = qual_field_var_name(MLDS_ModuleName, module_qual, FieldName),
    FieldId = ml_field_named(QualName, StructType),
    make_named_fields(MLDS_ModuleName, StructType, FieldNames, FieldIds).

ml_gen_static_vector_defn(MLDS_ModuleName, TypeNum, RowInitializers, Common,
        !GlobalData) :-
    some [!CellGroup] (
        list.length(RowInitializers, NumRows),
        CellGroupMap0 = !.GlobalData ^ mgd_vector_cell_group_map,
        map.lookup(CellGroupMap0, TypeNum, !:CellGroup),

        NextRow0 = !.CellGroup ^ mvcg_next_row,
        StartRowNum = NextRow0,
        NextRow = NextRow0 + NumRows,
        !CellGroup ^ mvcg_next_row := NextRow,

        StructType = !.CellGroup ^ mvcg_type,
        Common = mlds_vector_common(MLDS_ModuleName, StructType, TypeNum,
            StartRowNum, NumRows),

        Rows0 = !.CellGroup ^ mvcg_rows,
        Rows = Rows0 ++ cord.from_list(RowInitializers),
        !CellGroup ^ mvcg_rows := Rows,

        map.det_update(TypeNum, !.CellGroup, CellGroupMap0, CellGroupMap),
        !GlobalData ^ mgd_vector_cell_group_map := CellGroupMap
    ).

%---------------------------------------------------------------------------%

ml_gen_alloc_site(ProcLabel, MaybeConsId, Size, Context, AllocId,
        !GlobalData) :-
    (
        MaybeConsId = yes(ConsId),
        TypeStr = cons_id_to_alloc_site_string(ConsId)
    ;
        MaybeConsId = no,
        TypeStr = "unknown"
    ),
    AllocData = ml_alloc_site_data(ProcLabel, Context, TypeStr, Size),
    Map0 = !.GlobalData ^ mgd_alloc_id_map,
    ( if bimap.search(Map0, AllocId0, AllocData) then
        AllocId = AllocId0
    else
        Counter0 = !.GlobalData ^ mgd_alloc_id_counter,
        counter.allocate(AllocIdNum, Counter0, Counter),
        AllocId = mlds_alloc_id(AllocIdNum),
        bimap.det_insert(AllocId, AllocData, Map0, Map),
        !GlobalData ^ mgd_alloc_id_counter := Counter,
        !GlobalData ^ mgd_alloc_id_map := Map
    ).

:- func cons_id_to_alloc_site_string(cons_id) = string.

cons_id_to_alloc_site_string(ConsId) = TypeStr :-
    (
        ConsId = cons(_, _, TypeCtor),
        TypeStr = type_ctor_to_string(TypeCtor)
    ;
        ConsId = tuple_cons(Arity),
        TypeStr = "{}/" ++ string.from_int(Arity)
    ;
        ConsId = closure_cons(_, _),
        TypeStr = "closure"
    ;
        ConsId = type_info_cell_constructor(_),
        TypeStr = "private_builtin.type_info/0"
    ;
        ConsId = typeclass_info_cell_constructor,
        TypeStr = "typeclass_info"
    ;
        ConsId = type_info_const(_),
        TypeStr = "type_info_const"
    ;
        ConsId = typeclass_info_const(_),
        TypeStr = "typeclass_info_const"
    ;
        ConsId = ground_term_const(_, _),
        TypeStr = "ground_term_const"
    ;
        ( ConsId = int_const(_)
        ; ConsId = uint_const(_)
        ; ConsId = int8_const(_)
        ; ConsId = uint8_const(_)
        ; ConsId = int16_const(_)
        ; ConsId = uint16_const(_)
        ; ConsId = int32_const(_)
        ; ConsId = uint32_const(_)
        ; ConsId = int64_const(_)
        ; ConsId = uint64_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = tabling_info_const(_)
        ; ConsId = table_io_entry_desc(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ),
        unexpected($pred, "unexpected cons_id")
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_global_data.
%---------------------------------------------------------------------------%
