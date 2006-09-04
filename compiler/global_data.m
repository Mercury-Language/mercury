%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: global_data.m.
% Author: zs.
%
% This module manages global data structures for the LLDS backend.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module ll_backend.global_data.
:- interface.

:- import_module hlds.hlds_pred.
:- import_module ll_backend.continuation_info.
:- import_module ll_backend.exprn_aux.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.     % for module_name
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type global_data.

:- pred global_data_init(static_cell_info::in, global_data::out) is det.

:- pred global_data_add_new_proc_var(pred_proc_id::in, tabling_info_struct::in,
    global_data::in, global_data::out) is det.

:- pred global_data_add_new_proc_layout(pred_proc_id::in, proc_layout_info::in,
    global_data::in, global_data::out) is det.

:- pred global_data_update_proc_layout(pred_proc_id::in, proc_layout_info::in,
    global_data::in, global_data::out) is det.

:- pred global_data_add_new_closure_layouts(list(layout_data)::in,
    global_data::in, global_data::out) is det.

:- pred global_data_maybe_get_proc_layout(global_data::in, pred_proc_id::in,
    proc_layout_info::out) is semidet.

:- pred global_data_get_proc_layout(global_data::in, pred_proc_id::in,
    proc_layout_info::out) is det.

:- pred global_data_get_all_proc_vars(global_data::in,
    list(tabling_info_struct)::out) is det.

:- pred global_data_get_all_proc_layouts(global_data::in,
    list(proc_layout_info)::out) is det.

:- pred global_data_get_all_closure_layouts(global_data::in,
    list(layout_data)::out) is det.

:- pred global_data_get_static_cell_info(global_data::in,
    static_cell_info::out) is det.

:- pred global_data_set_static_cell_info(static_cell_info::in,
    global_data::in, global_data::out) is det.

:- type static_cell_info.

:- func init_static_cell_info(module_name, bool, bool) = static_cell_info.

:- pred add_scalar_static_cell(assoc_list(rval, llds_type)::in, data_addr::out,
    static_cell_info::in, static_cell_info::out) is det.

:- pred add_scalar_static_cell_natural_types(list(rval)::in, data_addr::out,
    static_cell_info::in, static_cell_info::out) is det.

:- pred find_general_llds_types(bool::in, list(mer_type)::in,
    list(list(rval))::in, list(llds_type)::out) is semidet.

:- pred add_vector_static_cell(list(llds_type)::in,
    list(list(rval))::in, data_addr::out,
    static_cell_info::in, static_cell_info::out) is det.

:- pred search_scalar_static_cell_offset(static_cell_info::in, data_addr::in,
    int::in, rval::out) is semidet.

:- pred get_static_cells(static_cell_info::in,
    list(scalar_common_data_array)::out, list(vector_common_data_array)::out)
    is det.

    % Given an rval, figure out the type it would have as an argument.
    % Normally that's the same as its usual type; the exception is that for
    % boxed floats, the type is data_ptr (i.e. the type of the boxed value)
    % rather than float (the type of the unboxed value).
    %
:- pred rval_type_as_arg(rval::in, exprn_opts::in, llds_type::out) is det.

%-----------------------------------------------------------------------------%

:- type static_cell_remap_info.

    % bump_type_num_counter(GlobalData, Increment)
    %
    % Return a copy of GlobalData with a type counter incremented by Increment.
    %
:- func bump_type_num_counter(global_data, int) = global_data.

    % merge_global_datas(GlobalDataA, GlobalDataB, GlobalData, Remap)
    %
    % Merge two global data structures, where static cell information from
    % GlobalDataA takes precedence over GlobalDataB.  The type numbers of the
    % two global_data structures must be distinct.  Remap contains the
    % information necessary for remap_static_cell_references/3.
    %
:- pred merge_global_datas(global_data::in, global_data::in, global_data::out,
    static_cell_remap_info::out) is det.

    % Update instructions in a C procedure that reference the static cells
    % from the GlobalDataB that was passed to merge_global_datas/4, to
    % reference the static cells of the merged global_data structure.
    %
:- pred remap_static_cell_references(static_cell_remap_info::in,
    c_procedure::in, c_procedure::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.rtti.
:- import_module libs.compiler_util.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds_out.

:- import_module bimap.
:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module svbimap.
:- import_module svmap.

%-----------------------------------------------------------------------------%

:- type proc_var_map    ==  map(pred_proc_id, tabling_info_struct).
:- type proc_layout_map ==  map(pred_proc_id, proc_layout_info).

:- type global_data
    --->    global_data(
                proc_var_map        :: proc_var_map,
                                    % Information about the global variables
                                    % defined by each procedure.

                proc_layout_map     :: proc_layout_map,
                                    % Information about the layout structures
                                    % defined by each procedure.

                closure_layouts     :: list(layout_data),
                                    % The list of all closure layouts generated
                                    % in this module. While all closure layouts
                                    % are different from all other layout_data,
                                    % it is possible, although unlikely, for
                                    % two closures to have the same layout.

                static_cell_info    :: static_cell_info
                                    % Information about all the statically
                                    % allocated cells created so far.
            ).

global_data_init(StaticCellInfo, GlobalData) :-
    map.init(EmptyDataMap),
    map.init(EmptyLayoutMap),
    GlobalData = global_data(EmptyDataMap, EmptyLayoutMap, [], StaticCellInfo).

global_data_add_new_proc_var(PredProcId, ProcVar, !GlobalData) :-
    ProcVarMap0 = !.GlobalData ^ proc_var_map,
    map.det_insert(ProcVarMap0, PredProcId, ProcVar, ProcVarMap),
    !:GlobalData = !.GlobalData ^ proc_var_map := ProcVarMap.

global_data_add_new_proc_layout(PredProcId, ProcLayout, !GlobalData) :-
    ProcLayoutMap0 = !.GlobalData ^ proc_layout_map,
    map.det_insert(ProcLayoutMap0, PredProcId, ProcLayout, ProcLayoutMap),
    !:GlobalData = !.GlobalData ^ proc_layout_map := ProcLayoutMap.

global_data_update_proc_layout(PredProcId, ProcLayout, !GlobalData) :-
    ProcLayoutMap0 = !.GlobalData ^ proc_layout_map,
    map.det_update(ProcLayoutMap0, PredProcId, ProcLayout, ProcLayoutMap),
    !:GlobalData = !.GlobalData ^ proc_layout_map := ProcLayoutMap.

global_data_add_new_closure_layouts(NewClosureLayouts, !GlobalData) :-
    ClosureLayouts0 = !.GlobalData ^ closure_layouts,
    list.append(NewClosureLayouts, ClosureLayouts0, ClosureLayouts),
    !:GlobalData = !.GlobalData ^ closure_layouts := ClosureLayouts.

global_data_maybe_get_proc_layout(GlobalData, PredProcId, ProcLayout) :-
    ProcLayoutMap = GlobalData ^ proc_layout_map,
    map.search(ProcLayoutMap, PredProcId, ProcLayout).

global_data_get_proc_layout(GlobalData, PredProcId, ProcLayout) :-
    ProcLayoutMap = GlobalData ^ proc_layout_map,
    map.lookup(ProcLayoutMap, PredProcId, ProcLayout).

global_data_get_all_proc_vars(GlobalData, ProcVars) :-
    ProcVarMap = GlobalData ^ proc_var_map,
    map.values(ProcVarMap, ProcVars).

global_data_get_all_proc_layouts(GlobalData, ProcLayouts) :-
    ProcLayoutMap = GlobalData ^ proc_layout_map,
    map.values(ProcLayoutMap, ProcLayouts).

global_data_get_all_closure_layouts(GlobalData, ClosureLayouts) :-
    ClosureLayouts = GlobalData ^ closure_layouts.

global_data_get_static_cell_info(GlobalData, StaticCellInfo) :-
    StaticCellInfo = GlobalData ^ static_cell_info.

global_data_set_static_cell_info(StaticCellInfo, !GlobalData) :-
    !:GlobalData = !.GlobalData ^ static_cell_info := StaticCellInfo.

%-----------------------------------------------------------------------------%

    % There is one scalar_cell_group for every group of scalar cells that
    % share the same sequence of argument types. We don't actually need the
    % cell type here, since we can't get to a scalar_cell_group from
    % the scalar_cell_group_map field of the static_cell_sub_info
    % without knowing it.
    %
:- type scalar_cell_group
    --->    scalar_cell_group(
                scalar_cell_counter         :: counter, % next cell number
                scalar_cell_group_members   :: bimap(list(rval), data_name),
                scalar_cell_rev_array       :: list(common_cell_value)
            ).

    % There is one vector_cell_group for every group of vector cells that
    % share the same sequence of argument types. We don't actually need the
    % cell type here, since we can't get to a vector_cell_group from
    % the vector_cell_group_map field of the static_cell_sub_info
    % without knowing it.
    %
    % Whereas in a scalar_cell_group, we try to find cells with the same
    % content and represent them just once, we do not do so for vectors,
    % because (a) the required lookup would be expensive due to the huge keys
    % required, and (b) the probability of finding two vectors with identical
    % contents is about zero.
    %
    % The vector_cell_map field maps the cell num of a vector cell to its
    % contents, the contents being a sequence of cells.
    %
:- type vector_cell_group
    --->    vector_cell_group(
                vector_cell_counter         :: counter, % next cell number
                vector_cell_map             :: map(int, vector_contents)
            ).

:- type vector_contents
    --->    vector_contents(list(common_cell_value)).

:- type static_cell_sub_info
    --->    static_cell_sub_info(
                module_name                 :: module_name, % base file name
                unbox_float                 :: bool,
                common_data                 :: bool
            ).

:- type static_cell_info
    --->    static_cell_info(
                sub_info                :: static_cell_sub_info,
                type_counter            :: counter, % next type number

                % Maps types to type numbers and vice versa.
                cell_type_num_map       :: bimap(common_cell_type, type_num),

                % Maps the cell type number to the information we have
                % for all scalar cells of that type.
                scalar_cell_group_map   :: map(type_num, scalar_cell_group),

                vector_cell_group_map   :: map(type_num, vector_cell_group)
            ).

init_static_cell_info(BaseName, UnboxFloat, CommonData) = Info0 :-
    SubInfo0 = static_cell_sub_info(BaseName, UnboxFloat, CommonData),
    Info0 = static_cell_info(SubInfo0, counter.init(0), bimap.init,
        map.init, map.init).

%-----------------------------------------------------------------------------%

add_scalar_static_cell_natural_types(Args, DataAddr, !Info) :-
    list.map(associate_natural_type(!.Info ^ sub_info ^ unbox_float),
        Args, ArgsTypes),
    add_scalar_static_cell(ArgsTypes, DataAddr, !Info).

add_scalar_static_cell(ArgsTypes0, DataAddr, !Info) :-
    % If we have an empty cell, place a dummy field in it,
    % so that the generated C structure isn't empty.
    (
        ArgsTypes0 = [],
        ArgsTypes = [const(llconst_int(-1)) - integer]
    ;
        ArgsTypes0 = [_ | _],
        ArgsTypes = ArgsTypes0
    ),
    compute_cell_type(ArgsTypes, CellType, CellTypeAndValue),
    do_add_scalar_static_cell(ArgsTypes, CellType, CellTypeAndValue, DataAddr,
        !Info).

:- pred do_add_scalar_static_cell(assoc_list(rval, llds_type)::in,
    common_cell_type::in, common_cell_value::in, data_addr::out,
    static_cell_info::in, static_cell_info::out) is det.

do_add_scalar_static_cell(ArgsTypes, CellType, CellValue, DataAddr, !Info) :-
    assoc_list.keys(ArgsTypes, Args),
    some [!CellGroup] (
        TypeNumMap0 = !.Info ^ cell_type_num_map,
        CellGroupMap0 = !.Info ^ scalar_cell_group_map,
        ( bimap.search(TypeNumMap0, CellType, TypeNumPrime) ->
            TypeNum = TypeNumPrime,
            ( map.search(CellGroupMap0, TypeNum, !:CellGroup) ->
                true
            ;
                !:CellGroup = init_scalar_cell_group
            )
        ;
            TypeNumCounter0 = !.Info ^ type_counter,
            counter.allocate(TypeNum0, TypeNumCounter0, TypeNumCounter),
            TypeNum = type_num(TypeNum0),
            !:Info = !.Info ^ type_counter := TypeNumCounter,

            bimap.det_insert(TypeNumMap0, CellType, TypeNum, TypeNumMap),
            !:Info = !.Info ^ cell_type_num_map := TypeNumMap,

            !:CellGroup = init_scalar_cell_group
        ),
        MembersMap0 = !.CellGroup ^ scalar_cell_group_members,
        ( bimap.search(MembersMap0, Args, DataNamePrime) ->
            DataName = DataNamePrime
        ;
            CellNumCounter0 = !.CellGroup ^ scalar_cell_counter,
            counter.allocate(CellNum, CellNumCounter0, CellNumCounter),
            !:CellGroup = !.CellGroup ^ scalar_cell_counter := CellNumCounter,
            DataName = scalar_common_ref(TypeNum, CellNum),
            RevArray0 = !.CellGroup ^ scalar_cell_rev_array,
            RevArray = [CellValue | RevArray0],
            !:CellGroup = !.CellGroup ^ scalar_cell_rev_array := RevArray,
            InsertCommonData = !.Info ^ sub_info ^ common_data,
            (
                InsertCommonData = yes,
                bimap.det_insert(MembersMap0, Args, DataName, MembersMap),
                !:CellGroup = !.CellGroup ^ scalar_cell_group_members
                    := MembersMap
            ;
                InsertCommonData = no
                % With --no-common-data, we never insert any cell into
                % CellGroupMap, ensuring that it stays empty. This can
                % be useful when comparing the LLDS and MLDS backends.
            ),
            map.set(CellGroupMap0, TypeNum, !.CellGroup, CellGroupMap),
            !:Info = !.Info ^ scalar_cell_group_map := CellGroupMap
        )
    ),
    ModuleName = !.Info ^ sub_info ^ module_name,
    DataAddr = data_addr(ModuleName, DataName).

:- func init_scalar_cell_group = scalar_cell_group.

init_scalar_cell_group = scalar_cell_group(counter.init(0), bimap.init, []).

search_scalar_static_cell_offset(Info, DataAddr, Offset, Rval) :-
    DataAddr = data_addr(Info ^ sub_info ^ module_name, DataName),
    DataName = scalar_common_ref(TypeNum, _CellNum),
    CellGroupMap = Info ^ scalar_cell_group_map,
    map.lookup(CellGroupMap, TypeNum, CellGroup),
    CellGroupMembers = CellGroup ^ scalar_cell_group_members,
    bimap.reverse_lookup(CellGroupMembers, Rvals, DataName),
    list.index0_det(Rvals, Offset, Rval).

%-----------------------------------------------------------------------------%

find_general_llds_types(UnboxFloat, Types, [Vector | Vectors], LLDSTypes) :-
    list.map(natural_type(UnboxFloat), Vector, LLDSTypes0),
    find_general_llds_types_2(UnboxFloat, Types, Vectors,
        LLDSTypes0, LLDSTypes).

:- pred find_general_llds_types_2(bool::in, list(mer_type)::in,
    list(list(rval))::in, list(llds_type)::in, list(llds_type)::out)
    is semidet.

find_general_llds_types_2(_UnboxFloat, _Types, [], !LLDSTypes).
find_general_llds_types_2(UnboxFloat, Types, [Vector | Vectors], !LLDSTypes) :-
    find_general_llds_types_in_cell(UnboxFloat, Types, Vector, !LLDSTypes),
    find_general_llds_types_2(UnboxFloat, Types, Vectors, !LLDSTypes).

:- pred find_general_llds_types_in_cell(bool::in, list(mer_type)::in,
    list(rval)::in, list(llds_type)::in, list(llds_type)::out) is semidet.

find_general_llds_types_in_cell(_UnboxFloat, [], [], [], []).
find_general_llds_types_in_cell(UnboxFloat, [_Type | Types], [Rval | Rvals],
        [LLDSType0 | LLDSTypes0], [LLDSType | LLDSTypes]) :-
    natural_type(UnboxFloat, Rval, NaturalType),
    % For user-defined types, some function symbols may be constants
    % (whose representations yield integer rvals) while others may be
    % non-constants (whose representations yield data_ptr rvals).
    % We need to be able to handle switches in which a variable of such a type
    % has a value of one kind in one switch arm and a value of the other kind
    % in another switch arm. We can mix the two because it is OK to initialize
    % a field declared to be a data_ptr with an integer rval.
    % 
    % If there are any other similar cases, they should be added here.
    % The value of Type may be useful in such code.
    (
        NaturalType = LLDSType0
    ->
        LLDSType = LLDSType0
    ;
        NaturalType = integer,
        LLDSType0 = data_ptr
    ->
        LLDSType = data_ptr
    ;
        NaturalType = data_ptr,
        LLDSType0 = integer
    ->
        LLDSType = data_ptr
    ;
        fail
    ),
    find_general_llds_types_in_cell(UnboxFloat, Types, Rvals,
        LLDSTypes0, LLDSTypes).

%-----------------------------------------------------------------------------%

add_vector_static_cell(LLDSTypes, VectorData, DataAddr, !Info) :-
    require(list.is_not_empty(LLDSTypes), "add_vector_static_cell: no types"),
    require(list.is_not_empty(VectorData), "add_vector_static_cell: no data"),

    % We don't to use grouped_args_type, since that would (a) make the code
    % below significantly more complex, and (b) the type declaration can be
    % expected to be only a small fraction of the size of the variable
    % definition, so the saving in C code size wouldn't be significant.

    CellType = plain_type(LLDSTypes),
    VectorCells = list.map(pair_vector_element(LLDSTypes), VectorData),
    some [!CellGroup] (
        TypeNumMap0 = !.Info ^ cell_type_num_map,
        CellGroupMap0 = !.Info ^ vector_cell_group_map,
        ( bimap.search(TypeNumMap0, CellType, TypeNumPrime) ->
            TypeNum = TypeNumPrime,
            ( map.search(CellGroupMap0, TypeNum, !:CellGroup) ->
                true
            ;
                !:CellGroup = init_vector_cell_group
            )
        ;
            TypeNumCounter0 = !.Info ^ type_counter,
            counter.allocate(TypeNum0, TypeNumCounter0, TypeNumCounter),
            TypeNum = type_num(TypeNum0),
            !:Info = !.Info ^ type_counter := TypeNumCounter,

            bimap.det_insert(TypeNumMap0, CellType, TypeNum, TypeNumMap),
            !:Info = !.Info ^ cell_type_num_map := TypeNumMap,

            !:CellGroup = init_vector_cell_group
        ),
        CellNumCounter0 = !.CellGroup ^ vector_cell_counter,
        counter.allocate(CellNum, CellNumCounter0, CellNumCounter),
        !:CellGroup = !.CellGroup ^ vector_cell_counter := CellNumCounter,
        DataName = vector_common_ref(TypeNum, CellNum),
        CellMap0 = !.CellGroup ^ vector_cell_map,
        VectorContents = vector_contents(VectorCells),
        map.det_insert(CellMap0, CellNum, VectorContents, CellMap),
        !:CellGroup = !.CellGroup ^ vector_cell_map := CellMap,
        map.set(CellGroupMap0, TypeNum, !.CellGroup, CellGroupMap),
        !:Info = !.Info ^ vector_cell_group_map := CellGroupMap
    ),
    ModuleName = !.Info ^ sub_info ^ module_name,
    DataAddr = data_addr(ModuleName, DataName).

:- func init_vector_cell_group = vector_cell_group.

init_vector_cell_group = vector_cell_group(counter.init(0), map.init).

:- func pair_vector_element(list(llds_type), list(rval)) = common_cell_value.

pair_vector_element(Types, Args) = plain_value(ArgsTypes) :-
    assoc_list.from_corresponding_lists(Args, Types, ArgsTypes).

%-----------------------------------------------------------------------------%

get_static_cells(Info, ScalarDatas, VectorDatas) :-
    ModuleName = Info ^ sub_info ^ module_name,
    TypeNumMap = Info ^ cell_type_num_map,
    map.foldl(add_scalar_static_cell_for_type(ModuleName, TypeNumMap),
        Info ^ scalar_cell_group_map, [], RevScalarDatas),
    list.reverse(RevScalarDatas, ScalarDatas),
    map.foldl(add_all_vector_static_cells_for_type(ModuleName, TypeNumMap),
        Info ^ vector_cell_group_map, [], RevVectorDatas),
    list.reverse(RevVectorDatas, VectorDatas).

:- pred add_scalar_static_cell_for_type(module_name::in,
    bimap(common_cell_type, type_num)::in, type_num::in, scalar_cell_group::in,
    list(scalar_common_data_array)::in, list(scalar_common_data_array)::out)
    is det.

add_scalar_static_cell_for_type(ModuleName, TypeNumMap, TypeNum, CellGroup,
        !Arrays) :-
    bimap.reverse_lookup(TypeNumMap, CellType, TypeNum),
    list.reverse(CellGroup ^ scalar_cell_rev_array, ArrayContents),
    Array = scalar_common_data_array(ModuleName, CellType, TypeNum,
        ArrayContents),
    !:Arrays = [Array | !.Arrays].

:- pred add_all_vector_static_cells_for_type(module_name::in,
    bimap(common_cell_type, type_num)::in, type_num::in, vector_cell_group::in,
    list(vector_common_data_array)::in, list(vector_common_data_array)::out)
    is det.

add_all_vector_static_cells_for_type(ModuleName, TypeNumMap, TypeNum,
        CellGroup, !Arrays) :-
    bimap.reverse_lookup(TypeNumMap, CellType, TypeNum),
    map.foldl(add_one_vector_static_cell(ModuleName, TypeNum, CellType),
        CellGroup ^ vector_cell_map, !Arrays).

:- pred add_one_vector_static_cell(module_name::in, type_num::in,
    common_cell_type::in, int::in, vector_contents::in,
    list(vector_common_data_array)::in, list(vector_common_data_array)::out)
    is det.

add_one_vector_static_cell(ModuleName, TypeNum, CellType, CellNum,
        vector_contents(VectorContents), !Arrays) :-
    Array = vector_common_data_array(ModuleName, CellType, TypeNum, CellNum,
        VectorContents),
    !:Arrays = [Array | !.Arrays].

%-----------------------------------------------------------------------------%

:- pred compute_cell_type(assoc_list(rval, llds_type)::in,
    common_cell_type::out, common_cell_value::out) is det.

compute_cell_type(ArgsTypes, CellType, CellValue) :-
    (
        ArgsTypes = [FirstArg - FirstArgType | LaterArgsTypes],
        threshold_group_types(FirstArgType, [FirstArg], LaterArgsTypes,
            TypeGroups, TypeAndArgGroups),
        OldLength = list.length(ArgsTypes),
        NewLength = list.length(TypeAndArgGroups),
        OldLength >= NewLength * 2
    ->
        CellType = grouped_args_type(TypeGroups),
        CellValue = grouped_args_value(TypeAndArgGroups)
    ;
        CellType = plain_type(assoc_list.values(ArgsTypes)),
        CellValue = plain_value(ArgsTypes)
    ).

:- pred threshold_group_types(llds_type::in, list(rval)::in,
    assoc_list(rval, llds_type)::in, assoc_list(llds_type, int)::out,
    list(common_cell_arg_group)::out) is semidet.

threshold_group_types(CurType, RevArgsSoFar, LaterArgsTypes, TypeGroups,
        TypeAndArgGroups) :-
    (
        LaterArgsTypes = [],
        make_arg_groups(CurType, RevArgsSoFar, TypeGroup, TypeAndArgGroup),
        TypeGroups = [TypeGroup],
        TypeAndArgGroups = [TypeAndArgGroup]
    ;
        LaterArgsTypes = [NextArg - NextType | MoreArgsTypes],
        ( CurType = NextType ->
            threshold_group_types(CurType, [NextArg | RevArgsSoFar],
                MoreArgsTypes, TypeGroups, TypeAndArgGroups)
        ;
            threshold_group_types(NextType, [NextArg], MoreArgsTypes,
                TypeGroupsTail, TypeAndArgGroupsTail),
            make_arg_groups(CurType, RevArgsSoFar, TypeGroup, TypeAndArgGroup),
            TypeGroups = [TypeGroup | TypeGroupsTail],
            TypeAndArgGroups = [TypeAndArgGroup | TypeAndArgGroupsTail]
        )
    ).

:- pred make_arg_groups(llds_type::in, list(rval)::in,
    pair(llds_type, int)::out, common_cell_arg_group::out) is det.

make_arg_groups(Type, RevArgs, TypeGroup, TypeAndArgGroup) :-
    ( RevArgs = [Arg] ->
        TypeGroup = Type - 1,
        TypeAndArgGroup = common_cell_ungrouped_arg(Type, Arg)
    ;
        list.length(RevArgs, NumArgs),
        list.reverse(RevArgs, Args),
        TypeGroup = Type - NumArgs,
        TypeAndArgGroup = common_cell_grouped_args(Type, NumArgs, Args)
    ).

%-----------------------------------------------------------------------------%

rval_type_as_arg(Rval, ExprnOpts, Type) :-
    natural_type(ExprnOpts ^ unboxed_float, Rval, Type).

:- pred natural_type(bool::in, rval::in, llds_type::out) is det.

natural_type(UnboxFloat, Rval, Type) :-
    llds.rval_type(Rval, Type0),
    (
        Type0 = float,
        UnboxFloat = no
    ->
        Type = data_ptr
    ;
        Type = Type0
    ).

:- pred associate_natural_type(bool::in, rval::in, pair(rval, llds_type)::out)
    is det.

associate_natural_type(UnboxFloat, Rval, Rval - Type) :-
    natural_type(UnboxFloat, Rval, Type).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type static_cell_remap_info
    --->    static_cell_remap_info(
                cell_type_num_remap,
                map(type_num, scalar_cell_group_remap)
                % A map from the _old_ type number, to the mapping of old
                % data_names to new_data names.
            ).

:- type cell_type_num_remap     == map(type_num, type_num).
                                    % Mapping of old to new type numbers.

:- type scalar_cell_group_remap == map(data_name, data_name).
                                    % Mapping of old to new data_names.

bump_type_num_counter(GlobalData0, Increment) = GlobalData :-
    Counter0 = GlobalData0 ^ static_cell_info ^ type_counter,
    counter.allocate(N, Counter0, _),
    Counter = counter.init(N + Increment),
    GlobalData = GlobalData0 ^ static_cell_info ^ type_counter := Counter.

merge_global_datas(GlobalDataA, GlobalDataB, GlobalData, Remap) :-
    GlobalDataA = global_data(ProcVarMapA, ProcLayoutMapA, ClosureLayoutsA,
        StaticCellInfoA),
    GlobalDataB = global_data(ProcVarMapB, ProcLayoutMapB, ClosureLayoutsB,
        StaticCellInfoB),
    GlobalData = global_data(ProcVarMap, ProcLayoutMap, ClosureLayouts,
        StaticCellInfo),
    ProcVarMap = map.merge(ProcVarMapA, ProcVarMapB),
    ProcLayoutMap = map.merge(ProcLayoutMapA, ProcLayoutMapB),
    ClosureLayouts = ClosureLayoutsA ++ ClosureLayoutsB,
    merge_static_cell_infos(StaticCellInfoA, StaticCellInfoB, StaticCellInfo,
        Remap).

:- pred merge_static_cell_infos(static_cell_info::in, static_cell_info::in,
    static_cell_info::out, static_cell_remap_info::out) is det.

merge_static_cell_infos(SCIa, SCIb, SCI, Remap) :-
    SCIa = static_cell_info(SubInfoA, TypeCounterA,
        CellTypeNumMapA, ScalarCellGroupMapA, VectorCellGroupMapA),
    SCIb = static_cell_info(SubInfoB, _TypeCounterB,
        CellTypeNumMapB, ScalarCellGroupMapB, VectorCellGroupMapB),
    expect(unify(SubInfoA, SubInfoB), this_file, "merge_static_cell_info"),

    % Merge cell type number maps.
    bimap.foldl3(merge_cell_type_num_maps, CellTypeNumMapB,
        TypeCounterA, TypeCounter, CellTypeNumMapA, CellTypeNumMap,
        map.init, CellTypeNumMapRemap),

    % Merge the scalar and vector cell group maps.
    merge_scalar_cell_group_maps(CellTypeNumMapRemap,
        ScalarCellGroupMapA, ScalarCellGroupMapB,
        ScalarCellGroupMap, ScalarCellGroupRemap),
    merge_vector_cell_group_maps(CellTypeNumMapRemap,
        VectorCellGroupMapA, VectorCellGroupMapB,
        VectorCellGroupMap),

    Remap = static_cell_remap_info(CellTypeNumMapRemap, ScalarCellGroupRemap),

    % Remap the information in the static_cell_info info itself.
    SCI0 = static_cell_info(SubInfoA, TypeCounter,
        CellTypeNumMap, ScalarCellGroupMap, VectorCellGroupMap),
    SCI = remap_static_cell_info(Remap, SCI0).

:- pred merge_cell_type_num_maps(common_cell_type::in, type_num::in,
    counter::in, counter::out, bimap(common_cell_type, type_num)::in,
    bimap(common_cell_type, type_num)::out,
    cell_type_num_remap::in, cell_type_num_remap::out) is det.

merge_cell_type_num_maps(CellType, BTypeNum,
        !TypeCounter, !CellTypeNumMap, !TypeNumRemap) :-
    (if bimap.search(!.CellTypeNumMap, CellType, ATypeNum) then
        % A type also in GlobalDataA.
        svmap.det_insert(BTypeNum, ATypeNum, !TypeNumRemap)
    else
        % A type not in GlobalDataA.
        counter.allocate(N, !TypeCounter),
        NewTypeNum = type_num(N),
        svmap.det_insert(BTypeNum, NewTypeNum, !TypeNumRemap),
        svbimap.det_insert(CellType, NewTypeNum, !CellTypeNumMap)
    ).

:- pred merge_scalar_cell_group_maps(cell_type_num_remap::in,
    map(type_num, scalar_cell_group)::in, map(type_num, scalar_cell_group)::in,
    map(type_num, scalar_cell_group)::out,
    map(type_num, scalar_cell_group_remap)::out) is det.

merge_scalar_cell_group_maps(TypeNumRemap,
        ScalarCellGroupMapA, ScalarCellGroupMapB,
        ScalarCellGroupMap, ScalarCellGroupRemap) :-
    map.foldl2(merge_scalar_cell_group_maps_2(TypeNumRemap),
        ScalarCellGroupMapB,
        ScalarCellGroupMapA, ScalarCellGroupMap,
        map.init, ScalarCellGroupRemap).

:- pred merge_scalar_cell_group_maps_2(cell_type_num_remap::in,
    type_num::in, scalar_cell_group::in,
    map(type_num, scalar_cell_group)::in,
    map(type_num, scalar_cell_group)::out,
    map(type_num, scalar_cell_group_remap)::in,
    map(type_num, scalar_cell_group_remap)::out) is det.

merge_scalar_cell_group_maps_2(TypeNumRemap, BTypeNum, BScalarCellGroup,
        !ScalarCellGroupMap, !Remap) :-
    map.lookup(TypeNumRemap, BTypeNum, TypeNum),
    (if
        ScalarCellGroupPrime = !.ScalarCellGroupMap ^ elem(TypeNum)
    then
        ScalarCellGroup0 = ScalarCellGroupPrime
    else
        % Could do this more efficiently.
        ScalarCellGroup0 = scalar_cell_group(counter.init(0), bimap.init, [])
    ),
    merge_scalar_cell_groups(TypeNum, ScalarCellGroup0, BScalarCellGroup,
        ScalarCellGroup, ScalarCellGroupRemap),
    svmap.set(TypeNum, ScalarCellGroup, !ScalarCellGroupMap),
    svmap.det_insert(BTypeNum, ScalarCellGroupRemap, !Remap).

:- pred merge_scalar_cell_groups(type_num::in,
    scalar_cell_group::in, scalar_cell_group::in, scalar_cell_group::out,
    scalar_cell_group_remap::out) is det.

merge_scalar_cell_groups(TypeNum, GroupA, GroupB, GroupAB, GroupRemap) :-
    GroupA  = scalar_cell_group(_CounterA, GroupMembersA,  RevArrayA),
    GroupB  = scalar_cell_group(_CounterB, GroupMembersB,  RevArrayB),
    GroupAB = scalar_cell_group(CounterAB, GroupMembersAB, RevArrayAB),

    CounterAB = counter.init(length(RevArrayAB)),

    ArrayA  = reverse(RevArrayA),
    ArrayB  = reverse(RevArrayB),
    ArrayAB = ArrayA ++ delete_elems(ArrayB, ArrayA),
    RevArrayAB = reverse(ArrayAB),

    bimap.foldl2(merge_scalar_cell_groups_2(TypeNum, ArrayB, ArrayAB),
        GroupMembersB,
        GroupMembersA, GroupMembersAB, map.init, GroupRemap).

:- pred merge_scalar_cell_groups_2(type_num::in,
    list(common_cell_value)::in, list(common_cell_value)::in, 
    list(rval)::in, data_name::in,
    bimap(list(rval), data_name)::in, bimap(list(rval), data_name)::out,
    scalar_cell_group_remap::in, scalar_cell_group_remap::out) is det.

merge_scalar_cell_groups_2(TypeNum, ArrayB, ArrayAB,
        Rvals, BDataName, !GroupMembers, !GroupRemap) :-
    (if
        bimap.search(!.GroupMembers, Rvals, DataName)
    then
        % Seen this list of rvals before in the group.
        svmap.det_insert(BDataName, DataName, !GroupRemap)
    else
        % Not seen this list of rvals before in the group.
        (
            BDataName = scalar_common_ref(_, BCellNum),
            % Look up what value this cell number referred to in the B array.
            % Find the cell number of the same value in the combined A+B array.
            CommonCellValue = list.det_index0(ArrayB, BCellNum),
            CellNum = nth_member_lookup0(ArrayAB, CommonCellValue),
            % Add the new data name.
            DataName = scalar_common_ref(TypeNum, CellNum),
            svbimap.det_insert(Rvals, DataName, !GroupMembers),
            svmap.det_insert(BDataName, DataName, !GroupRemap)
        ;
            ( BDataName = vector_common_ref(_, _)
            ; BDataName = proc_tabling_ref(_, _)
            ),
            unexpected(this_file, "merge_scalar_cell_groups_2")
        )
    ).

:- pred merge_vector_cell_group_maps(cell_type_num_remap::in,
    map(type_num, vector_cell_group)::in, map(type_num, vector_cell_group)::in,
    map(type_num, vector_cell_group)::out) is det.

merge_vector_cell_group_maps(TypeNumRemap, VectorCellGroupMapA,
        VectorCellGroupMapB, VectorCellGroupMap) :-
    map.foldl(merge_vector_cell_group_maps_2(TypeNumRemap),
        VectorCellGroupMapB,
        VectorCellGroupMapA, VectorCellGroupMap).

:- pred merge_vector_cell_group_maps_2(cell_type_num_remap::in,
    type_num::in, vector_cell_group::in, map(type_num, vector_cell_group)::in,
    map(type_num, vector_cell_group)::out) is det.

merge_vector_cell_group_maps_2(TypeNumRemap, OldTypeNum, VectorCellGroup,
        !VectorCellGroupMap) :-
    NewTypeNum = TypeNumRemap ^ det_elem(OldTypeNum),
    svmap.det_insert(NewTypeNum, VectorCellGroup, !VectorCellGroupMap).

:- func nth_member_lookup0(list(T), T) = int.

nth_member_lookup0(List, Elem) = Pos-1 :-
    list.nth_member_lookup(List, Elem, Pos).

%-----------------------------------------------------------------------------%

    % The scalar cell group and vector cell group contents themselves
    % need to be updated to use the merged cell information.
    %
:- func remap_static_cell_info(static_cell_remap_info,
    static_cell_info) = static_cell_info.

remap_static_cell_info(Remap, SCI0) = SCI :-
    ScalarMap = map.map_values(remap_scalar_cell_group(Remap),
        SCI0 ^ scalar_cell_group_map),
    VectorMap = map.map_values(remap_vector_cell_group(Remap),
        SCI0 ^ vector_cell_group_map),
    SCI = (SCI0 ^ scalar_cell_group_map := ScalarMap)
                ^ vector_cell_group_map := VectorMap.

:- func remap_scalar_cell_group(static_cell_remap_info,
    type_num, scalar_cell_group) = scalar_cell_group.

remap_scalar_cell_group(Remap, _, ScalarCellGroup0) = ScalarCellGroup :-
    Array0 = ScalarCellGroup0 ^ scalar_cell_rev_array,
    Array  = list.map(remap_common_cell_value(Remap), Array0),
    ScalarCellGroup = ScalarCellGroup0 ^ scalar_cell_rev_array := Array.

:- func remap_vector_cell_group(static_cell_remap_info,
    type_num, vector_cell_group) = vector_cell_group.

remap_vector_cell_group(Remap, _, VectorCellGroup0) = VectorCellGroup :-
    VectorCellGroup0 = vector_cell_group(Counter, Map0),
    VectorCellGroup  = vector_cell_group(Counter, Map),
    Map = map.map_values(remap_vector_contents(Remap), Map0).

:- func remap_vector_contents(static_cell_remap_info,
    int, vector_contents) = vector_contents.

remap_vector_contents(Remap, _, Contents0) = Contents :-
    Contents0 = vector_contents(Values0),
    Contents  = vector_contents(Values),
    Values = list.map(remap_common_cell_value(Remap), Values0).

:- func remap_common_cell_value(static_cell_remap_info,
    common_cell_value) = common_cell_value.

remap_common_cell_value(Remap, CommonCellValue0) = CommonCellValue :-
    (
        CommonCellValue0 = plain_value(RvalsTypes0),
        CommonCellValue  = plain_value(RvalsTypes),
        RvalsTypes = list.map(remap_plain_value(Remap), RvalsTypes0)
    ;
        CommonCellValue0 = grouped_args_value(ArgGroup0),
        CommonCellValue  = grouped_args_value(ArgGroup),
        ArgGroup = list.map(remap_arg_group_value(Remap), ArgGroup0)
    ).

:- func remap_plain_value(static_cell_remap_info,
    pair(rval, llds_type)) = pair(rval, llds_type).

remap_plain_value(Remap, Rval0 - Type) = Rval - Type :-
    Rval = remap_rval(Remap, Rval0).

:- func remap_arg_group_value(static_cell_remap_info,
    common_cell_arg_group) = common_cell_arg_group.

remap_arg_group_value(Remap, GroupedArgs0) = GroupedArgs :-
    (
        GroupedArgs0 = common_cell_grouped_args(Type, Fields, Rvals0),
        GroupedArgs  = common_cell_grouped_args(Type, Fields, Rvals),
        Rvals = list.map(remap_rval(Remap), Rvals0)
    ;
        GroupedArgs0 = common_cell_ungrouped_arg(Type, Rvals0),
        GroupedArgs  = common_cell_ungrouped_arg(Type, Rvals),
        Rvals = remap_rval(Remap, Rvals0)
    ).

%-----------------------------------------------------------------------------%

remap_static_cell_references(Remap, Procedure0, Procedure) :-
    Code0 = Procedure0 ^ cproc_code,
    Code  = list.map(remap_instruction(Remap), Code0),
    Procedure = Procedure0 ^ cproc_code := Code.

:- func remap_instruction(static_cell_remap_info, instruction) = instruction.

remap_instruction(Remap, Instr0 - Comment)
    = remap_instr(Remap, Instr0) - Comment.

:- func remap_instr(static_cell_remap_info, instr) = instr.

remap_instr(Remap, Instr0) = Instr :-
    (
        Instr0 = block(NumIntTemps, NumFloatTemps, Block0),
        Instr  = block(NumIntTemps, NumFloatTemps, Block),
        Block  = list.map(remap_instruction(Remap), Block0)
    ;
        Instr0 = assign(Lval, Rval0),
        Instr  = assign(Lval, Rval),
        Rval = remap_rval(Remap, Rval0)
    ;
        Instr0 = if_val(Rval0, CodeAddr),
        Instr  = if_val(Rval, CodeAddr),
        Rval = remap_rval(Remap, Rval0)
    ;
        Instr0 = pragma_c(A, Comps0, B, C, D, E, F, G, H),
        Instr  = pragma_c(A, Comps,  B, C, D, E, F, G, H),
        Comps = list.map(remap_pragma_c_component(Remap), Comps0)
    ;
        ( Instr0 = comment(_)
        ; Instr0 = livevals(_)
        ; Instr0 = llcall(_, _, _, _, _, _)
        ; Instr0 = mkframe(_, _)
        ; Instr0 = label(_)
        ; Instr0 = goto(_)
        ; Instr0 = computed_goto(_, _)
        ; Instr0 = arbitrary_c_code(_, _)
        ; Instr0 = save_maxfr(_)
        ; Instr0 = restore_maxfr(_)
        ; Instr0 = incr_hp(_, _, _, _, _, _)
        ; Instr0 = mark_hp(_)
        ; Instr0 = restore_hp(_)
        ; Instr0 = free_heap(_)
        ; Instr0 = store_ticket(_)
        ; Instr0 = reset_ticket(_, _)
        ; Instr0 = prune_ticket
        ; Instr0 = discard_ticket
        ; Instr0 = mark_ticket_stack(_)
        ; Instr0 = prune_tickets_to(_)
        ; Instr0 = incr_sp(_, _)
        ; Instr0 = decr_sp(_)
        ; Instr0 = decr_sp_and_return(_)
        ; Instr0 = init_sync_term(_, _)
        ; Instr0 = fork(_, _, _)
        ; Instr0 = join_and_terminate(_)
        ; Instr0 = join_and_continue(_, _)
        ),
        Instr = Instr0
    ).

:- func remap_pragma_c_component(static_cell_remap_info, pragma_c_component)
    = pragma_c_component.

remap_pragma_c_component(Remap, Comp0) = Comp :-
    (
        Comp0 = pragma_c_inputs(Inputs0),
        Comp  = pragma_c_inputs(Inputs),
        Inputs = list.map(remap_pragma_c_input(Remap), Inputs0) 
    ;
        Comp0 = pragma_c_outputs(Outputs0),
        Comp  = pragma_c_outputs(Outputs),
        Outputs = list.map(remap_pragma_c_output(Remap), Outputs0)
    ;
        ( Comp0 = pragma_c_raw_code(_, _, _)
        ; Comp0 = pragma_c_user_code(_, _)
        ; Comp0 = pragma_c_fail_to(_)
        ; Comp0 = pragma_c_noop
        ),
        Comp = Comp0
    ).

:- func remap_pragma_c_input(static_cell_remap_info, pragma_c_input)
    = pragma_c_input.

remap_pragma_c_input(Remap, pragma_c_input(A, B, C, D, Rval0, E, F))
    = pragma_c_input(A, B, C, D, Rval, E, F) :-
    Rval = remap_rval(Remap, Rval0).

:- func remap_pragma_c_output(static_cell_remap_info, pragma_c_output)
    = pragma_c_output.

remap_pragma_c_output(Remap, pragma_c_output(Lval0, A, B, C, D, E, F))
    = pragma_c_output(Lval, A, B, C, D, E, F) :-
    Lval = remap_lval(Remap, Lval0).

:- func remap_lval(static_cell_remap_info, lval) = lval.

remap_lval(Remap, Lval0) = Lval :-
    (
        Lval0 = field(MaybeTag, Rval0, FieldNum),
        Lval  = field(MaybeTag, Rval, FieldNum),
        Rval  = remap_rval(Remap, Rval0)
    ;
        Lval0 = mem_ref(Rval0),
        Lval  = mem_ref(Rval),
        Rval  = remap_rval(Remap, Rval0)
    ;
        ( Lval0 = reg(_, _)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = temp(_, _)
        ; Lval0 = stackvar(_)
        ; Lval0 = framevar(_)
        ; Lval0 = succip_slot(_)
        ; Lval0 = redoip_slot(_)
        ; Lval0 = redofr_slot(_)
        ; Lval0 = succfr_slot(_)
        ; Lval0 = prevfr_slot(_)
        ; Lval0 = global_var_ref(_)
        ; Lval0 = lvar(_)
        ),
        Lval = Lval0
    ).

:- func remap_rval(static_cell_remap_info, rval) = rval.

remap_rval(Remap, Rval0) = Rval :-
    (
        Rval0 = lval(Lval0),
        Rval  = lval(Lval),
        Lval  = remap_lval(Remap, Lval0)
    ;
        Rval0 = var(_),
        Rval  = Rval0
    ;
        Rval0 = mkword(Tag, Ptr0),
        Rval  = mkword(Tag, Ptr),
        Ptr   = remap_rval(Remap, Ptr0)
    ;
        Rval0 = const(Const0),
        Rval  = const(Const),
        Const = remap_rval_const(Remap, Const0)
    ;
        Rval0 = unop(Unop, A0),
        Rval  = unop(Unop, A),
        A = remap_rval(Remap, A0)
    ;
        Rval0 = binop(Binop, A0, B0),
        Rval  = binop(Binop, A, B),
        A = remap_rval(Remap, A0),
        B = remap_rval(Remap, B0)
    ;
        Rval0 = mem_addr(MemRef0),
        Rval  = mem_addr(MemRef),
        MemRef = remap_mem_ref(Remap, MemRef0)
    ).

:- func remap_rval_const(static_cell_remap_info, rval_const) = rval_const.

remap_rval_const(Remap, Const0) = Const :-
    (
        Const0 = llconst_data_addr(Addr0, MaybeOffset),
        Const  = llconst_data_addr(Addr,  MaybeOffset),
        (
            Addr0 = data_addr(ModuleName, DataName0),
            Addr  = data_addr(ModuleName, DataName),
            DataName = remap_data_name(Remap, DataName0)
        ;
            Addr0 = rtti_addr(_),
            Addr = Addr0
        ;
            Addr0 = layout_addr(_),
            Addr = Addr0
        )
    ;
        ( Const0 = llconst_true
        ; Const0 = llconst_false
        ; Const0 = llconst_int(_)
        ; Const0 = llconst_float(_)
        ; Const0 = llconst_string(_)
        ; Const0 = llconst_multi_string(_, _)
        ; Const0 = llconst_code_addr(_)
        ),
        Const = Const0
    ).

:- func remap_data_name(static_cell_remap_info, data_name) = data_name.

remap_data_name(Remap, DataName0) = DataName :-
    Remap = static_cell_remap_info(TypeNumRemap, ScalarCellGroupRemap),
    (
        DataName0 = scalar_common_ref(TypeNum0, _Offset),
        (if
            map.contains(TypeNumRemap, TypeNum0)
        then
            DataName = ScalarCellGroupRemap ^ det_elem(TypeNum0)
                                            ^ det_elem(DataName0)
        else
            DataName = DataName0
        )
    ;
        DataName0 = vector_common_ref(TypeNum0, Offset),
        (if
            map.search(TypeNumRemap, TypeNum0, TypeNum)
        then
            DataName = vector_common_ref(TypeNum, Offset)
        else
            DataName = DataName0
        )
    ;
        DataName0 = proc_tabling_ref(_, _),
        DataName = DataName0
    ).

:- func remap_mem_ref(static_cell_remap_info, mem_ref) = mem_ref.

remap_mem_ref(Remap, MemRef0) = MemRef :-
    (
        MemRef0 = stackvar_ref(_),
        MemRef = MemRef0
    ;
        MemRef0 = framevar_ref(_),
        MemRef = MemRef0
    ;
        MemRef0 = heap_ref(Ptr0, Tag, FieldNum),
        MemRef  = heap_ref(Ptr, Tag, FieldNum),
        Ptr = remap_rval(Remap, Ptr0)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "global_data.m".

%-----------------------------------------------------------------------------%
:- end_module global_data.
%-----------------------------------------------------------------------------%
