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

:- pred global_data_add_new_proc_var(pred_proc_id::in, comp_gen_c_var::in,
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
    list(comp_gen_c_var)::out) is det.

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

%-----------------------------------------------------------------------------%

:- type proc_var_map    ==  map(pred_proc_id, comp_gen_c_var).
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
                sub_info                    :: static_cell_sub_info,
                type_counter                :: counter, % next type number

                % Maps types to type numbers and vice versa.
                cell_type_num_map           :: bimap(common_cell_type, int),

                % Maps the cell type number to the information we have
                % for all scalar cells of that type.
                scalar_cell_group_map       :: map(int, scalar_cell_group),

                vector_cell_group_map       :: map(int, vector_cell_group)
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
        ArgsTypes = [const(int_const(-1)) - integer]
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
            counter.allocate(TypeNum, TypeNumCounter0, TypeNumCounter),
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
            counter.allocate(TypeNum, TypeNumCounter0, TypeNumCounter),
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
    bimap(common_cell_type, int)::in, int::in, scalar_cell_group::in,
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
    bimap(common_cell_type, int)::in, int::in, vector_cell_group::in,
    list(vector_common_data_array)::in, list(vector_common_data_array)::out)
    is det.

add_all_vector_static_cells_for_type(ModuleName, TypeNumMap, TypeNum,
        CellGroup, !Arrays) :-
    bimap.reverse_lookup(TypeNumMap, CellType, TypeNum),
    map.foldl(add_one_vector_static_cell(ModuleName, TypeNum, CellType),
        CellGroup ^ vector_cell_map, !Arrays).

:- pred add_one_vector_static_cell(module_name::in, int::in,
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

:- func this_file = string.

this_file = "global_data.m".

%-----------------------------------------------------------------------------%
:- end_module global_data.
%-----------------------------------------------------------------------------%
