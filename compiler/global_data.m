%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2012 The University of Melbourne.
% Copyright (C) 2013, 2015-2018 The Mercury team.
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
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.     % for module_name
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module set_tree234.

%-----------------------------------------------------------------------------%

:- type global_data.

:- pred global_data_init(static_cell_info::in, int::in, list(string)::in,
    global_data::out) is det.

:- pred global_data_add_new_proc_var(pred_proc_id::in, tabling_info_struct::in,
    global_data::in, global_data::out) is det.

:- pred global_data_add_new_proc_layout(pred_proc_id::in, proc_layout_info::in,
    global_data::in, global_data::out) is det.

:- pred global_data_update_proc_layout(pred_proc_id::in, proc_layout_info::in,
    global_data::in, global_data::out) is det.

:- pred global_data_add_new_closure_layouts(list(closure_proc_id_data)::in,
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
    list(closure_proc_id_data)::out) is det.

:- pred global_data_get_threadscope_string_table(global_data::in,
    list(string)::out) is det.

:- pred global_data_get_threadscope_rev_string_table(global_data::in,
    list(string)::out, int::out) is det.

:- pred global_data_set_threadscope_rev_string_table(list(string)::in, int::in,
    global_data::in, global_data::out) is det.

:- pred global_data_get_static_cell_info(global_data::in,
    static_cell_info::out) is det.

:- pred global_data_set_static_cell_info(static_cell_info::in,
    global_data::in, global_data::out) is det.

:- type static_cell_info.

:- func init_static_cell_info(module_name, have_unboxed_floats,
    have_unboxed_int64s, bool) = static_cell_info.

:- pred add_scalar_static_cell(list(typed_rval)::in, data_id::out,
    static_cell_info::in, static_cell_info::out) is det.

:- pred add_scalar_static_cell_natural_types(list(rval)::in, data_id::out,
    static_cell_info::in, static_cell_info::out) is det.

:- pred global_data_add_new_alloc_sites(set_tree234(alloc_site_info)::in,
    global_data::in, global_data::out) is det.

:- pred global_data_get_all_alloc_sites(global_data::in,
    list(alloc_site_info)::out, map(alloc_site_id, layout_slot_name)::out)
    is det.

:- pred find_general_llds_types(have_unboxed_floats::in,
    have_unboxed_int64s::in, list(mer_type)::in, list(list(rval))::in,
    list(llds_type)::out) is semidet.

:- pred add_vector_static_cell(list(llds_type)::in,
    list(list(rval))::in, data_id::out,
    static_cell_info::in, static_cell_info::out) is det.

:- pred search_scalar_static_cell_offset(static_cell_info::in, data_id::in,
    int::in, rval::out) is semidet.

:- pred get_static_cells(static_cell_info::in,
    list(scalar_common_data_array)::out, list(vector_common_data_array)::out)
    is det.

    % Given an rval, the value of the --unboxed-float option, the value of the
    % --unboxed-int64s and the width of the constructor argument, figure out
    % the type the rval would have as an argument. Normally that's the same as
    % its usual type; the exception is that for boxed floats, boxed int64s and
    % boxed uint64s the type is data_ptr (i.e. the type of the boxed value)
    % rather than float, int64, uint64 (the type of the unboxed value).
    %
:- func rval_type_as_arg(have_unboxed_floats,have_unboxed_int64s, arg_width,
    rval) = llds_type.

%-----------------------------------------------------------------------------%

:- type static_cell_remap_info.

    % bump_type_num_counter(Increment, !GlobalData)
    %
    % Increment the type counter in GlobalData by Increment.
    %
:- pred bump_type_num_counter(int::in, global_data::in, global_data::out)
    is det.

:- type global_data_remapping.

    % merge_global_datas(GlobalDataA, GlobalDataB, GlobalData, Remap)
    %
    % Merge two global data structures, where static cell information from
    % GlobalDataA takes precedence over GlobalDataB. The type numbers of the
    % two global_data structures must be distinct. Remap contains the
    % information necessary for remap_static_cell_references/3.
    %
:- pred merge_global_datas(global_data::in, global_data::in, global_data::out,
    global_data_remapping::out) is det.

    % Update instructions in a C procedure that reference things from
    % GlobalDataB that was passed to merge_global_datas/4, to reference things
    % from the merged global_data structure.
    %
:- pred remap_references_to_global_data(global_data_remapping::in,
    c_procedure::in, c_procedure::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bimap.
:- import_module counter.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type proc_var_map    ==  map(pred_proc_id, tabling_info_struct).
:- type proc_layout_map ==  map(pred_proc_id, proc_layout_info).

:- type global_data
    --->    global_data(
                % Information about the global variables defined by
                % each procedure.
                gd_proc_var_map             :: proc_var_map,

                % Information about the layout structures defined by
                % each procedure.
                gd_proc_layout_map          :: proc_layout_map,

                % The list of all closure layouts generated in this module.
                % While all closure layouts are different from all other
                % layout_data, it is possible, although unlikely, for
                % two closures to have the same layout.
                gd_closure_layouts          :: list(closure_proc_id_data),

                % A table for allocating and maintaining slots where string IDs
                % will be placed at runtime for threadscope profiling.
                % The actual string IDs are allocated at runtime and their IDs
                % are placed in an array slot which can be referred to
                % statically. The size of the table is maintained for
                % allocating offsets into it.
                gd_ts_string_table_size     :: int,
                gd_ts_rev_string_table      :: list(string),

                % Information about all the statically allocated cells
                % created so far.
                gd_static_cell_info         :: static_cell_info,

                % Information about all allocation sites in this module.
                gd_alloc_sites              :: set_tree234(alloc_site_info)
            ).

global_data_init(StaticCellInfo, TSStringTableSize, TSRevStringTable,
        GlobalData) :-
    map.init(EmptyDataMap),
    map.init(EmptyLayoutMap),
    GlobalData = global_data(EmptyDataMap, EmptyLayoutMap, [],
        TSStringTableSize, TSRevStringTable, StaticCellInfo,
        set_tree234.init).

global_data_add_new_proc_var(PredProcId, ProcVar, !GlobalData) :-
    ProcVarMap0 = !.GlobalData ^ gd_proc_var_map,
    map.det_insert(PredProcId, ProcVar, ProcVarMap0, ProcVarMap),
    !GlobalData ^ gd_proc_var_map := ProcVarMap.

global_data_add_new_proc_layout(PredProcId, ProcLayout, !GlobalData) :-
    ProcLayoutMap0 = !.GlobalData ^ gd_proc_layout_map,
    map.det_insert(PredProcId, ProcLayout, ProcLayoutMap0, ProcLayoutMap),
    !GlobalData ^ gd_proc_layout_map := ProcLayoutMap.

global_data_update_proc_layout(PredProcId, ProcLayout, !GlobalData) :-
    ProcLayoutMap0 = !.GlobalData ^ gd_proc_layout_map,
    map.det_update(PredProcId, ProcLayout, ProcLayoutMap0, ProcLayoutMap),
    !GlobalData ^ gd_proc_layout_map := ProcLayoutMap.

global_data_add_new_closure_layouts(NewClosureLayouts, !GlobalData) :-
    ClosureLayouts0 = !.GlobalData ^ gd_closure_layouts,
    list.append(NewClosureLayouts, ClosureLayouts0, ClosureLayouts),
    !GlobalData ^ gd_closure_layouts := ClosureLayouts.

global_data_maybe_get_proc_layout(GlobalData, PredProcId, ProcLayout) :-
    ProcLayoutMap = GlobalData ^ gd_proc_layout_map,
    map.search(ProcLayoutMap, PredProcId, ProcLayout).

global_data_get_proc_layout(GlobalData, PredProcId, ProcLayout) :-
    ProcLayoutMap = GlobalData ^ gd_proc_layout_map,
    map.lookup(ProcLayoutMap, PredProcId, ProcLayout).

global_data_get_all_proc_vars(GlobalData, ProcVars) :-
    ProcVarMap = GlobalData ^ gd_proc_var_map,
    map.values(ProcVarMap, ProcVars).

global_data_get_all_proc_layouts(GlobalData, ProcLayouts) :-
    ProcLayoutMap = GlobalData ^ gd_proc_layout_map,
    map.values(ProcLayoutMap, ProcLayouts).

global_data_get_all_closure_layouts(GlobalData, ClosureLayouts) :-
    ClosureLayouts = GlobalData ^ gd_closure_layouts.

global_data_get_threadscope_string_table(GlobalData, Table) :-
    global_data_get_threadscope_rev_string_table(GlobalData, RevTable, _),
    Table = list.reverse(RevTable).

global_data_get_threadscope_rev_string_table(GlobalData,
        RevTable, TableSize) :-
    RevTable = GlobalData ^ gd_ts_rev_string_table,
    TableSize = GlobalData ^ gd_ts_string_table_size.

global_data_set_threadscope_rev_string_table(RevTable, TableSize,
        !GlobalData) :-
    !GlobalData ^ gd_ts_rev_string_table := RevTable,
    !GlobalData ^ gd_ts_string_table_size := TableSize.

global_data_get_static_cell_info(GlobalData, StaticCellInfo) :-
    StaticCellInfo = GlobalData ^ gd_static_cell_info.

global_data_set_static_cell_info(StaticCellInfo, !GlobalData) :-
    !GlobalData ^ gd_static_cell_info := StaticCellInfo.

global_data_add_new_alloc_sites(NewAllocSites, !GlobalData) :-
    AllocSites0 = !.GlobalData ^ gd_alloc_sites,
    set_tree234.union(NewAllocSites, AllocSites0, AllocSites),
    !GlobalData ^ gd_alloc_sites := AllocSites.

global_data_get_all_alloc_sites(GlobalData, AllocSites, AllocIdMap) :-
    AllocSitesSet = GlobalData ^ gd_alloc_sites,
    AllocSites = set_tree234.to_sorted_list(AllocSitesSet),
    list.foldl2(make_alloc_id_map, AllocSites, 0, _Slot, map.init, AllocIdMap).

:- pred make_alloc_id_map(alloc_site_info::in, int::in, int::out,
    map(alloc_site_id, layout_slot_name)::in,
    map(alloc_site_id, layout_slot_name)::out) is det.

make_alloc_id_map(AllocSite, Slot, Slot + 1, !Map) :-
    AllocId = alloc_site_id(AllocSite),
    ArraySlot = layout_slot(alloc_site_array, Slot),
    map.det_insert(AllocId, ArraySlot, !Map).

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
                scalar_cell_group_members   :: bimap(list(rval), data_id),
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
                scsi_module_name            :: module_name, % base file name
                scsi_unbox_float            :: have_unboxed_floats,
                scsi_unbox_int64s           :: have_unboxed_int64s,
                scsi_common_data            :: bool
            ).

:- type cell_type_bimap == bimap(common_cell_type, type_num).
:- type scalar_type_cell_map == map(type_num, scalar_cell_group).
:- type vector_type_cell_map == map(type_num, vector_cell_group).

:- type static_cell_info
    --->    static_cell_info(
                sci_sub_info                :: static_cell_sub_info,
                sci_type_counter            :: counter, % next type number

                % Maps types to type numbers and vice versa.
                sci_cell_type_num_map       :: cell_type_bimap,

                % Maps the cell type number to the information we have
                % for all scalar cells of that type.
                sci_scalar_cell_group_map   :: scalar_type_cell_map,

                % Maps the cell type number to the information we have
                % for all vector cells of that type.
                sci_vector_cell_group_map   :: vector_type_cell_map
            ).

init_static_cell_info(BaseName, UnboxFloat, UnboxInt64s, CommonData) = Info0 :-
    SubInfo0 = static_cell_sub_info(BaseName, UnboxFloat, UnboxInt64s,
        CommonData),
    Info0 = static_cell_info(SubInfo0, counter.init(0), bimap.init,
        map.init, map.init).

%-----------------------------------------------------------------------------%

add_scalar_static_cell_natural_types(Args, DataId, !Info) :-
    UnboxFloat = !.Info ^ sci_sub_info ^ scsi_unbox_float,
    UnboxInt64s = !.Info ^ sci_sub_info ^ scsi_unbox_int64s,
    ArgWidth = full_word,
    list.map(associate_natural_type(UnboxFloat, UnboxInt64s, ArgWidth),
        Args, TypedArgs),
    add_scalar_static_cell(TypedArgs, DataId, !Info).

add_scalar_static_cell(TypedArgs0, DataId, !Info) :-
    % If we have an empty cell, place a dummy field in it,
    % so that the generated C structure isn't empty.
    (
        TypedArgs0 = [],
        TypedArgs = [typed_rval(const(llconst_int(-1)), lt_int(int_type_int))]
    ;
        TypedArgs0 = [_ | _],
        TypedArgs = TypedArgs0
    ),
    compute_cell_type(TypedArgs, CellType, CellTypeAndValue),
    do_add_scalar_static_cell(TypedArgs, CellType, CellTypeAndValue, DataId,
        !Info).

:- pred do_add_scalar_static_cell(list(typed_rval)::in,
    common_cell_type::in, common_cell_value::in, data_id::out,
    static_cell_info::in, static_cell_info::out) is det.

do_add_scalar_static_cell(TypedArgs, CellType, CellValue, DataId, !Info) :-
    Args = typed_rvals_project_rvals(TypedArgs),
    some [!CellGroup] (
        TypeNumMap0 = !.Info ^ sci_cell_type_num_map,
        CellGroupMap0 = !.Info ^ sci_scalar_cell_group_map,
        % We do not want to use bimap.search_insert here, since this search
        % usually succeeds.
        ( if bimap.search(TypeNumMap0, CellType, OldTypeNum) then
            TypeNum = OldTypeNum,
            ( if map.search(CellGroupMap0, TypeNum, !:CellGroup) then
                true
            else
                !:CellGroup = init_scalar_cell_group
            )
        else
            TypeNumCounter0 = !.Info ^ sci_type_counter,
            counter.allocate(TypeRawNum, TypeNumCounter0, TypeNumCounter),
            TypeNum = type_num(TypeRawNum),
            !Info ^ sci_type_counter := TypeNumCounter,

            bimap.det_insert(CellType, TypeNum, TypeNumMap0, TypeNumMap),
            !Info ^ sci_cell_type_num_map := TypeNumMap,

            !:CellGroup = init_scalar_cell_group
        ),

        InsertCommonData = !.Info ^ sci_sub_info ^ scsi_common_data,
        (
            InsertCommonData = yes,
            MembersMap0 = !.CellGroup ^ scalar_cell_group_members,
            CellNumCounter0 = !.CellGroup ^ scalar_cell_counter,
            counter.allocate(CellNum, CellNumCounter0, CellNumCounter),
            NewDataId = scalar_common_data_id(TypeNum, CellNum),

            bimap.search_insert(Args, NewDataId, MaybeOldDataId,
                MembersMap0, MembersMap),
            (
                MaybeOldDataId = yes(OldDataId),
                % We cannot get here if !.CellGroup wasn't found in
                % CellGroupMap0.
                DataId = OldDataId
            ;
                MaybeOldDataId = no,
                DataId = NewDataId,
                !CellGroup ^ scalar_cell_counter := CellNumCounter,
                !CellGroup ^ scalar_cell_group_members := MembersMap,
                RevArray0 = !.CellGroup ^ scalar_cell_rev_array,
                RevArray = [CellValue | RevArray0],
                !CellGroup ^ scalar_cell_rev_array := RevArray,
                map.set(TypeNum, !.CellGroup, CellGroupMap0, CellGroupMap),
                !Info ^ sci_scalar_cell_group_map := CellGroupMap
            )
        ;
            InsertCommonData = no,
            MembersMap0 = !.CellGroup ^ scalar_cell_group_members,
            ( if bimap.search(MembersMap0, Args, DataIdPrime) then
                DataId = DataIdPrime
            else
                CellNumCounter0 = !.CellGroup ^ scalar_cell_counter,
                counter.allocate(CellNum, CellNumCounter0, CellNumCounter),
                !CellGroup ^ scalar_cell_counter := CellNumCounter,
                DataId = scalar_common_data_id(TypeNum, CellNum),
                RevArray0 = !.CellGroup ^ scalar_cell_rev_array,
                RevArray = [CellValue | RevArray0],
                !CellGroup ^ scalar_cell_rev_array := RevArray,
                % With --no-common-data, we never insert any cell into
                % CellGroupMap, ensuring that it stays empty. This can
                % be useful when comparing the LLDS and MLDS backends.
                map.set(TypeNum, !.CellGroup, CellGroupMap0, CellGroupMap),
                !Info ^ sci_scalar_cell_group_map := CellGroupMap
            )
        )
    ).

:- func init_scalar_cell_group = scalar_cell_group.

init_scalar_cell_group = scalar_cell_group(counter.init(0), bimap.init, []).

search_scalar_static_cell_offset(Info, DataId, Offset, Rval) :-
    DataId = scalar_common_data_id(TypeNum, _CellNum),
    CellGroupMap = Info ^ sci_scalar_cell_group_map,
    map.lookup(CellGroupMap, TypeNum, CellGroup),
    CellGroupMembers = CellGroup ^ scalar_cell_group_members,
    bimap.reverse_lookup(CellGroupMembers, Rvals, DataId),
    list.det_index0(Rvals, Offset, Rval).

%-----------------------------------------------------------------------------%

find_general_llds_types(UnboxFloat, UnboxInt64s, Types, [Vector | Vectors],
        LLDSTypes) :-
    ArgWidth = full_word,
    list.map(natural_type(UnboxFloat, UnboxInt64s, ArgWidth), Vector,
        LLDSTypes0),
    find_general_llds_types_2(UnboxFloat, UnboxInt64s, Types, Vectors,
        LLDSTypes0, LLDSTypes).

:- pred find_general_llds_types_2(have_unboxed_floats::in,
    have_unboxed_int64s::in, list(mer_type)::in, list(list(rval))::in,
    list(llds_type)::in, list(llds_type)::out) is semidet.

find_general_llds_types_2(_UnboxFloat, _UnboxInt64s, _Types, [], !LLDSTypes).
find_general_llds_types_2(UnboxFloat, UnboxInt64s, Types, [Vector | Vectors], !LLDSTypes) :-
    find_general_llds_types_in_cell(UnboxFloat, UnboxInt64s, Types, Vector, !LLDSTypes),
    find_general_llds_types_2(UnboxFloat, UnboxInt64s, Types, Vectors, !LLDSTypes).

:- pred find_general_llds_types_in_cell(have_unboxed_floats::in,
    have_unboxed_int64s::in, list(mer_type)::in, list(rval)::in,
    list(llds_type)::in, list(llds_type)::out) is semidet.

find_general_llds_types_in_cell(_UnboxFloat, _UnboxInt64s, [], [], [], []).
find_general_llds_types_in_cell(UnboxFloat, UnboxInt64s, [_Type | Types],
        [Rval | Rvals], [LLDSType0 | LLDSTypes0], [LLDSType | LLDSTypes]) :-
    ArgWidth = full_word,
    natural_type(UnboxFloat, UnboxInt64s, ArgWidth, Rval, NaturalType),
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
    ( if
        NaturalType = LLDSType0
    then
        LLDSType = LLDSType0
    else if
        NaturalType = lt_int(int_type_int),
        LLDSType0 = lt_data_ptr
    then
        LLDSType = lt_data_ptr
    else if
        NaturalType = lt_data_ptr,
        LLDSType0 = lt_int(int_type_int)
    then
        LLDSType = lt_data_ptr
    else
        fail
    ),
    find_general_llds_types_in_cell(UnboxFloat, UnboxInt64s, Types, Rvals,
        LLDSTypes0, LLDSTypes).

%-----------------------------------------------------------------------------%

add_vector_static_cell(LLDSTypes, VectorData, DataId, !Info) :-
    expect(list.is_not_empty(LLDSTypes), $module, $pred, "no types"),
    expect(list.is_not_empty(VectorData), $module, $pred, "no data"),

    % We don't to use grouped_args_type, since that would (a) make the code
    % below significantly more complex, and (b) the type declaration can be
    % expected to be only a small fraction of the size of the variable
    % definition, so the saving in C code size wouldn't be significant.

    CellType = plain_type(LLDSTypes),
    VectorCells = list.map(pair_vector_element(LLDSTypes), VectorData),
    some [!CellGroup] (
        TypeNumMap0 = !.Info ^ sci_cell_type_num_map,
        CellGroupMap0 = !.Info ^ sci_vector_cell_group_map,
        ( if bimap.search(TypeNumMap0, CellType, TypeNumPrime) then
            TypeNum = TypeNumPrime,
            ( if map.search(CellGroupMap0, TypeNum, !:CellGroup) then
                true
            else
                !:CellGroup = init_vector_cell_group
            )
        else
            TypeNumCounter0 = !.Info ^ sci_type_counter,
            counter.allocate(TypeNum0, TypeNumCounter0, TypeNumCounter),
            TypeNum = type_num(TypeNum0),
            !Info ^ sci_type_counter := TypeNumCounter,

            bimap.det_insert(CellType, TypeNum, TypeNumMap0, TypeNumMap),
            !Info ^ sci_cell_type_num_map := TypeNumMap,

            !:CellGroup = init_vector_cell_group
        ),
        CellNumCounter0 = !.CellGroup ^ vector_cell_counter,
        counter.allocate(CellNum, CellNumCounter0, CellNumCounter),
        !CellGroup ^ vector_cell_counter := CellNumCounter,
        DataId = vector_common_data_id(TypeNum, CellNum),
        CellMap0 = !.CellGroup ^ vector_cell_map,
        VectorContents = vector_contents(VectorCells),
        map.det_insert(CellNum, VectorContents, CellMap0, CellMap),
        !CellGroup ^ vector_cell_map := CellMap,
        map.set(TypeNum, !.CellGroup, CellGroupMap0, CellGroupMap),
        !Info ^ sci_vector_cell_group_map := CellGroupMap
    ).

:- func init_vector_cell_group = vector_cell_group.

init_vector_cell_group = vector_cell_group(counter.init(0), map.init).

:- func pair_vector_element(list(llds_type), list(rval)) = common_cell_value.

pair_vector_element(Types, Args) = plain_value(TypedArgs) :-
    build_typed_rvals(Args, Types, TypedArgs).

%-----------------------------------------------------------------------------%

get_static_cells(Info, ScalarDatas, VectorDatas) :-
    TypeNumMap = Info ^ sci_cell_type_num_map,
    map.foldl(add_scalar_static_cell_for_type(TypeNumMap),
        Info ^ sci_scalar_cell_group_map, [], RevScalarDatas),
    list.reverse(RevScalarDatas, ScalarDatas),
    map.foldl(add_all_vector_static_cells_for_type(TypeNumMap),
        Info ^ sci_vector_cell_group_map, [], RevVectorDatas),
    list.reverse(RevVectorDatas, VectorDatas).

:- pred add_scalar_static_cell_for_type(cell_type_bimap::in,
    type_num::in, scalar_cell_group::in,
    list(scalar_common_data_array)::in, list(scalar_common_data_array)::out)
    is det.

add_scalar_static_cell_for_type(TypeNumMap, TypeNum, CellGroup,
        !Arrays) :-
    bimap.reverse_lookup(TypeNumMap, CellType, TypeNum),
    list.reverse(CellGroup ^ scalar_cell_rev_array, ArrayContents),
    Array = scalar_common_data_array(CellType, TypeNum, ArrayContents),
    !:Arrays = [Array | !.Arrays].

:- pred add_all_vector_static_cells_for_type(cell_type_bimap::in,
    type_num::in, vector_cell_group::in,
    list(vector_common_data_array)::in, list(vector_common_data_array)::out)
    is det.

add_all_vector_static_cells_for_type(TypeNumMap, TypeNum, CellGroup,
        !Arrays) :-
    bimap.reverse_lookup(TypeNumMap, CellType, TypeNum),
    map.foldl(add_one_vector_static_cell(TypeNum, CellType),
        CellGroup ^ vector_cell_map, !Arrays).

:- pred add_one_vector_static_cell(type_num::in, common_cell_type::in,
    int::in, vector_contents::in,
    list(vector_common_data_array)::in, list(vector_common_data_array)::out)
    is det.

add_one_vector_static_cell(TypeNum, CellType, CellNum,
        vector_contents(VectorContents), !Arrays) :-
    Array = vector_common_data_array(CellType, TypeNum, CellNum,
        VectorContents),
    !:Arrays = [Array | !.Arrays].

%-----------------------------------------------------------------------------%

:- pred compute_cell_type(list(typed_rval)::in,
    common_cell_type::out, common_cell_value::out) is det.

compute_cell_type(TypedArgs, CellType, CellValue) :-
    ( if
        TypedArgs = [typed_rval(FirstArg, FirstArgType) | LaterTypedArgs],
        threshold_group_types(FirstArgType, [FirstArg], LaterTypedArgs,
            TypeGroups, TypeAndArgGroups),
        OldLength = list.length(TypedArgs),
        NewLength = list.length(TypeAndArgGroups),
        OldLength >= NewLength * 2
    then
        CellType = grouped_args_type(TypeGroups),
        CellValue = grouped_args_value(TypeAndArgGroups)
    else
        CellType = plain_type(typed_rvals_project_types(TypedArgs)),
        CellValue = plain_value(TypedArgs)
    ).

:- pred threshold_group_types(llds_type::in, list(rval)::in,
    list(typed_rval)::in, assoc_list(llds_type, int)::out,
    list(common_cell_arg_group)::out) is semidet.

threshold_group_types(CurType, RevArgsSoFar, LaterArgsTypes, TypeGroups,
        TypeAndArgGroups) :-
    (
        LaterArgsTypes = [],
        make_arg_groups(CurType, RevArgsSoFar, TypeGroup, TypeAndArgGroup),
        TypeGroups = [TypeGroup],
        TypeAndArgGroups = [TypeAndArgGroup]
    ;
        LaterArgsTypes = [typed_rval(NextArg, NextType) | MoreArgsTypes],
        ( if CurType = NextType then
            threshold_group_types(CurType, [NextArg | RevArgsSoFar],
                MoreArgsTypes, TypeGroups, TypeAndArgGroups)
        else
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
    ( if RevArgs = [Arg] then
        TypeGroup = Type - 1,
        TypeAndArgGroup = common_cell_ungrouped_arg(Type, Arg)
    else
        list.length(RevArgs, NumArgs),
        list.reverse(RevArgs, Args),
        TypeGroup = Type - NumArgs,
        TypeAndArgGroup = common_cell_grouped_args(Type, NumArgs, Args)
    ).

%-----------------------------------------------------------------------------%

rval_type_as_arg(UnboxedFloat, UnboxedInt64s, ArgWidth, Rval) = Type :-
    natural_type(UnboxedFloat, UnboxedInt64s, ArgWidth, Rval, Type).

:- pred natural_type(have_unboxed_floats::in, have_unboxed_int64s::in,
    arg_width::in, rval::in, llds_type::out) is det.

natural_type(UnboxFloat, UnboxInt64s, ArgWidth, Rval, Type) :-
    llds.rval_type(Rval, Type0),
    ( if
        Type0 = lt_float,
        UnboxFloat = do_not_have_unboxed_floats,
        ArgWidth \= double_word
    then
        Type = lt_data_ptr
    else if
        ( Type0 = lt_int(int_type_int64)
        ; Type0 = lt_int(int_type_uint64)
        ),
        UnboxInt64s = do_not_have_unboxed_int64s,
        ArgWidth \= double_word
    then
        Type = lt_data_ptr
    else
        Type = Type0
    ).

:- pred associate_natural_type(have_unboxed_floats::in,
    have_unboxed_int64s::in, arg_width::in, rval::in, typed_rval::out) is det.

associate_natural_type(UnboxFloat, UnboxInt64s, ArgWidth, Rval,
        typed_rval(Rval, Type)) :-
    natural_type(UnboxFloat, UnboxInt64s, ArgWidth, Rval, Type).

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

:- type scalar_cell_group_remap == map(data_id, data_id).
                                    % Mapping of old to new data_names.

bump_type_num_counter(Increment, !GlobalData) :-
    Counter0 = !.GlobalData ^ gd_static_cell_info ^ sci_type_counter,
    counter.allocate(N, Counter0, _),
    Counter = counter.init(N + Increment),
    !GlobalData ^ gd_static_cell_info ^ sci_type_counter := Counter.

merge_global_datas(GlobalDataA, GlobalDataB, GlobalData, GlobalDataRemap) :-
    GlobalDataA = global_data(ProcVarMapA, ProcLayoutMapA, ClosureLayoutsA,
        TSStringSlotCounterA, TSRevStringTableA, StaticCellInfoA,
        AllocSitesA),
    GlobalDataB = global_data(ProcVarMapB, ProcLayoutMapB, ClosureLayoutsB,
        TSStringSlotCounterB, TSRevStringTableB, StaticCellInfoB,
        AllocSitesB),
    ProcVarMap = map.old_merge(ProcVarMapA, ProcVarMapB),
    ProcLayoutMap = map.old_merge(ProcLayoutMapA, ProcLayoutMapB),
    ClosureLayouts = ClosureLayoutsA ++ ClosureLayoutsB,
    merge_threadscope_string_tables(TSRevStringTableA, TSStringSlotCounterA,
        TSRevStringTableB, TSStringSlotCounterB,
        TSRevStringTable, TSStringSlotCounter, MaybeTSStringTableRemap),
    merge_static_cell_infos(StaticCellInfoA, StaticCellInfoB, StaticCellInfo,
        StaticCellRemap),
    set_tree234.union(AllocSitesA, AllocSitesB, AllocSites),
    GlobalData = global_data(ProcVarMap, ProcLayoutMap, ClosureLayouts,
        TSStringSlotCounter, TSRevStringTable, StaticCellInfo, AllocSites),
    GlobalDataRemap =
        global_data_remapping(MaybeTSStringTableRemap, StaticCellRemap).

    % merge_threadscope_string_tables(RevTableA, CounterA, RevTableB, CounterB,
    %   RevTable, Counter, MaybeRemapOffset).
    %
    % Merge the threadscope string tables.
    %
    % After doing this merge the references in RevTableB may be adjusted and
    % must be corrected by adding RemapOffset to them if MaybeRemapOffset =
    % yes(RemapOffset).
    %
:- pred merge_threadscope_string_tables(list(string)::in, int::in,
    list(string)::in, int::in,
    list(string)::out, int::out, maybe(int)::out) is det.

merge_threadscope_string_tables([], _, [], _, [], 0, no).
merge_threadscope_string_tables([], _, [X | Xs], N, [X | Xs], N, no).
merge_threadscope_string_tables([X | Xs], N, [], _, [X | Xs], N, no).
merge_threadscope_string_tables(RevTableA, CounterA, RevTableB, CounterB,
        RevTable, Counter, yes(RemapOffset)) :-
    RevTableA = [_ | _],
    RevTableB = [_ | _],
    RevTable = RevTableB ++ RevTableA,
    Counter = CounterA + CounterB,
    RemapOffset = CounterA.

:- pred merge_static_cell_infos(static_cell_info::in, static_cell_info::in,
    static_cell_info::out, static_cell_remap_info::out) is det.

merge_static_cell_infos(SCIa, SCIb, SCI, Remap) :-
    SCIa = static_cell_info(SubInfoA, TypeCounterA,
        CellTypeNumMapA, ScalarCellGroupMapA, VectorCellGroupMapA),
    SCIb = static_cell_info(SubInfoB, _TypeCounterB,
        CellTypeNumMapB, ScalarCellGroupMapB, VectorCellGroupMapB),
    expect(unify(SubInfoA, SubInfoB), $module, $pred, "mismatch"),

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
    remap_static_cell_info(Remap, SCI0, SCI).

:- pred merge_cell_type_num_maps(common_cell_type::in, type_num::in,
    counter::in, counter::out, cell_type_bimap::in, cell_type_bimap::out,
    cell_type_num_remap::in, cell_type_num_remap::out) is det.

merge_cell_type_num_maps(CellType, BTypeNum,
        !TypeCounter, !CellTypeNumMap, !TypeNumRemap) :-
    ( if bimap.search(!.CellTypeNumMap, CellType, ATypeNum) then
        % A type also in GlobalDataA.
        map.det_insert(BTypeNum, ATypeNum, !TypeNumRemap)
    else
        % A type not in GlobalDataA.
        counter.allocate(N, !TypeCounter),
        NewTypeNum = type_num(N),
        map.det_insert(BTypeNum, NewTypeNum, !TypeNumRemap),
        bimap.det_insert(CellType, NewTypeNum, !CellTypeNumMap)
    ).

:- pred merge_scalar_cell_group_maps(cell_type_num_remap::in,
    scalar_type_cell_map::in, scalar_type_cell_map::in,
    scalar_type_cell_map::out,
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
    scalar_type_cell_map::in,
    scalar_type_cell_map::out,
    map(type_num, scalar_cell_group_remap)::in,
    map(type_num, scalar_cell_group_remap)::out) is det.

merge_scalar_cell_group_maps_2(TypeNumRemap, BTypeNum, BScalarCellGroup,
        !ScalarCellGroupMap, !Remap) :-
    map.lookup(TypeNumRemap, BTypeNum, TypeNum),
    ( if map.search(!.ScalarCellGroupMap, TypeNum, ScalarCellGroupPrime) then
        ScalarCellGroup0 = ScalarCellGroupPrime
    else
        % Could do this more efficiently.
        ScalarCellGroup0 = scalar_cell_group(counter.init(0), bimap.init, [])
    ),
    merge_scalar_cell_groups(TypeNum, ScalarCellGroup0, BScalarCellGroup,
        ScalarCellGroup, ScalarCellGroupRemap),
    map.set(TypeNum, ScalarCellGroup, !ScalarCellGroupMap),
    map.det_insert(BTypeNum, ScalarCellGroupRemap, !Remap).

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
    list(rval)::in, data_id::in,
    bimap(list(rval), data_id)::in, bimap(list(rval), data_id)::out,
    scalar_cell_group_remap::in, scalar_cell_group_remap::out) is det.

merge_scalar_cell_groups_2(TypeNum, ArrayB, ArrayAB,
        Rvals, BDataId, !GroupMembers, !GroupRemap) :-
    ( if bimap.search(!.GroupMembers, Rvals, DataId) then
        % Seen this list of rvals before in the group.
        map.det_insert(BDataId, DataId, !GroupRemap)
    else
        % Not seen this list of rvals before in the group.
        (
            BDataId = scalar_common_data_id(_, BCellNum),
            % Look up what value this cell number referred to in the B array.
            % Find the cell number of the same value in the combined A+B array.
            CommonCellValue = list.det_index0(ArrayB, BCellNum),
            CellNum =
                list.det_index0_of_first_occurrence(ArrayAB, CommonCellValue),
            % Add the new data name.
            DataId = scalar_common_data_id(TypeNum, CellNum),
            bimap.det_insert(Rvals, DataId, !GroupMembers),
            map.det_insert(BDataId, DataId, !GroupRemap)
        ;
            ( BDataId = rtti_data_id(_)
            ; BDataId = proc_tabling_data_id(_, _)
            ; BDataId = vector_common_data_id(_, _)
            ; BDataId = layout_id(_)
            ; BDataId = layout_slot_id(_, _)
            ),
            unexpected($module, $pred, "unexpected BDataId")
        )
    ).

:- pred merge_vector_cell_group_maps(cell_type_num_remap::in,
    vector_type_cell_map::in, vector_type_cell_map::in,
    vector_type_cell_map::out) is det.

merge_vector_cell_group_maps(TypeNumRemap, VectorCellGroupMapA,
        VectorCellGroupMapB, VectorCellGroupMap) :-
    map.foldl(merge_vector_cell_group_maps_2(TypeNumRemap),
        VectorCellGroupMapB,
        VectorCellGroupMapA, VectorCellGroupMap).

:- pred merge_vector_cell_group_maps_2(cell_type_num_remap::in,
    type_num::in, vector_cell_group::in,
    vector_type_cell_map::in, vector_type_cell_map::out) is det.

merge_vector_cell_group_maps_2(TypeNumRemap, OldTypeNum, VectorCellGroup,
        !VectorCellGroupMap) :-
    map.lookup(TypeNumRemap, OldTypeNum, NewTypeNum),
    map.det_insert(NewTypeNum, VectorCellGroup, !VectorCellGroupMap).

%-----------------------------------------------------------------------------%

    % The scalar cell group and vector cell group contents themselves
    % need to be updated to use the merged cell information.
    %
:- pred remap_static_cell_info(static_cell_remap_info::in,
    static_cell_info::in, static_cell_info::out) is det.

remap_static_cell_info(Remap, !SCI) :-
    ScalarMap0 = !.SCI ^ sci_scalar_cell_group_map,
    VectorMap0 = !.SCI ^ sci_vector_cell_group_map,
    map.map_values_only(remap_scalar_cell_group(Remap), ScalarMap0, ScalarMap),
    map.map_values_only(remap_vector_cell_group(Remap), VectorMap0, VectorMap),
    !SCI ^ sci_scalar_cell_group_map := ScalarMap,
    !SCI ^ sci_vector_cell_group_map := VectorMap.

:- pred remap_scalar_cell_group(static_cell_remap_info::in,
    scalar_cell_group::in, scalar_cell_group::out) is det.

remap_scalar_cell_group(Remap, !ScalarCellGroup) :-
    Array0 = !.ScalarCellGroup ^ scalar_cell_rev_array,
    list.map(remap_common_cell_value(Remap), Array0, Array),
    !ScalarCellGroup ^ scalar_cell_rev_array := Array.

:- pred remap_vector_cell_group(static_cell_remap_info::in,
    vector_cell_group::in, vector_cell_group::out) is det.

remap_vector_cell_group(Remap, !VectorCellGroup) :-
    !.VectorCellGroup = vector_cell_group(Counter, Map0),
    map.map_values_only(remap_vector_contents(Remap), Map0, Map),
    !:VectorCellGroup = vector_cell_group(Counter, Map).

:- pred remap_vector_contents(static_cell_remap_info::in,
    vector_contents::in, vector_contents::out) is det.

remap_vector_contents(Remap, !Contents) :-
    !.Contents = vector_contents(Values0),
    list.map(remap_common_cell_value(Remap), Values0, Values),
    !:Contents = vector_contents(Values).

:- pred remap_common_cell_value(static_cell_remap_info::in,
    common_cell_value::in, common_cell_value::out) is det.

remap_common_cell_value(Remap, !CommonCellValue) :-
    (
        !.CommonCellValue = plain_value(RvalsTypes0),
        list.map(remap_plain_value(Remap), RvalsTypes0, RvalsTypes),
        !:CommonCellValue = plain_value(RvalsTypes)
    ;
        !.CommonCellValue = grouped_args_value(ArgGroup0),
        list.map(remap_arg_group_value(Remap), ArgGroup0, ArgGroup),
        !:CommonCellValue = grouped_args_value(ArgGroup)
    ).

:- pred remap_plain_value(static_cell_remap_info::in,
    typed_rval::in, typed_rval::out) is det.

remap_plain_value(Remap, typed_rval(Rval0, Type), typed_rval(Rval, Type)) :-
    remap_rval(Remap, Rval0, Rval).

:- pred remap_arg_group_value(static_cell_remap_info::in,
    common_cell_arg_group::in, common_cell_arg_group::out) is det.

remap_arg_group_value(Remap, !GroupedArgs) :-
    (
        !.GroupedArgs = common_cell_grouped_args(Type, Fields, Rvals0),
        list.map(remap_rval(Remap), Rvals0, Rvals),
        !:GroupedArgs = common_cell_grouped_args(Type, Fields, Rvals)
    ;
        !.GroupedArgs = common_cell_ungrouped_arg(Type, Rvals0),
        remap_rval(Remap, Rvals0, Rvals),
        !:GroupedArgs = common_cell_ungrouped_arg(Type, Rvals)
    ).

%-----------------------------------------------------------------------------%

:- type global_data_remapping
    --->    global_data_remapping(
                gdr_maybe_ts_table_offset   :: maybe(int),
                gdr_static_cell_remap_info  :: static_cell_remap_info
            ).

remap_references_to_global_data(Remap, !Procedure) :-
    Code0 = !.Procedure ^ cproc_code,
    list.map(remap_instruction(Remap), Code0, Code),
    !Procedure ^ cproc_code := Code.

:- pred remap_instruction(global_data_remapping::in,
    instruction::in, instruction::out) is det.

remap_instruction(Remap, !Instr) :-
    !.Instr = llds_instr(Uinstr0, Comment),
    remap_instr(Remap, Uinstr0, Uinstr),
    !:Instr = llds_instr(Uinstr, Comment).

:- pred remap_instr(global_data_remapping::in, instr::in, instr::out) is det.

remap_instr(GlobalDataRemap, Instr0, Instr) :-
    StaticCellRemap = GlobalDataRemap ^ gdr_static_cell_remap_info,
    (
        Instr0 = block(NumIntTemps, NumFloatTemps, Block0),
        list.map(remap_instruction(GlobalDataRemap), Block0, Block),
        Instr = block(NumIntTemps, NumFloatTemps, Block)
    ;
        Instr0 = assign(Lval, Rval0),
        remap_rval(StaticCellRemap, Rval0, Rval),
        Instr = assign(Lval, Rval)
    ;
        Instr0 = keep_assign(Lval, Rval0),
        remap_rval(StaticCellRemap, Rval0, Rval),
        Instr = keep_assign(Lval, Rval)
    ;
        Instr0 = if_val(Rval0, CodeAddr),
        remap_rval(StaticCellRemap, Rval0, Rval),
        Instr  = if_val(Rval, CodeAddr)
    ;
        Instr0 = foreign_proc_code(Decls, Comps0, MayCallMerc,
            FixNoLayout, FixLayout, FixOnlyLayout, NoFix,
            HashDefnLabel, StackSlotRef, MaybeDup),
        list.map(remap_foreign_proc_component(StaticCellRemap), Comps0, Comps),
        Instr  = foreign_proc_code(Decls, Comps,  MayCallMerc,
            FixNoLayout, FixLayout, FixOnlyLayout, NoFix,
            HashDefnLabel, StackSlotRef, MaybeDup)
    ;
        Instr0 = computed_goto(Rval0, MaybeLabels),
        remap_rval(StaticCellRemap, Rval0, Rval),
        Instr = computed_goto(Rval, MaybeLabels)
    ;
        Instr0 = save_maxfr(Lval0),
        remap_lval(StaticCellRemap, Lval0, Lval),
        Instr = save_maxfr(Lval)
    ;
        Instr0 = restore_maxfr(Lval0),
        remap_lval(StaticCellRemap, Lval0, Lval),
        Instr = restore_maxfr(Lval)
    ;
        Instr0 = incr_hp(Lval0, MaybeTag, MaybeOffset, SizeRval0, Prof,
            Atomic, MaybeRegion0, MaybeReuse0),
        remap_lval(StaticCellRemap, Lval0, Lval),
        remap_rval(StaticCellRemap, SizeRval0, SizeRval),
        (
            MaybeRegion0 = yes(Region0),
            remap_rval(StaticCellRemap, Region0, Region),
            MaybeRegion = yes(Region)
        ;
            MaybeRegion0 = no,
            MaybeRegion = no
        ),
        (
            MaybeReuse0 = llds_reuse(Reuse0, MaybeFlag0),
            remap_rval(StaticCellRemap, Reuse0, Reuse),
            (
                MaybeFlag0 = yes(Flag0),
                remap_lval(StaticCellRemap, Flag0, Flag),
                MaybeFlag = yes(Flag)
            ;
                MaybeFlag0 = no,
                MaybeFlag = no
            ),
            MaybeReuse = llds_reuse(Reuse, MaybeFlag)
        ;
            MaybeReuse0 = no_llds_reuse,
            MaybeReuse = no_llds_reuse
        ),
        Instr = incr_hp(Lval, MaybeTag, MaybeOffset, SizeRval, Prof,
            Atomic, MaybeRegion, MaybeReuse)
    ;
        Instr0 = mark_hp(Lval0),
        remap_lval(StaticCellRemap, Lval0, Lval),
        Instr = mark_hp(Lval)
    ;
        Instr0 = restore_hp(Rval0),
        remap_rval(StaticCellRemap, Rval0, Rval),
        Instr = restore_hp(Rval)
    ;
        Instr0 = free_heap(Rval0),
        remap_rval(StaticCellRemap, Rval0, Rval),
        Instr = free_heap(Rval)
    ;
        Instr0 = push_region_frame(StackId, EmbeddedStackFrame),
        Instr = push_region_frame(StackId, EmbeddedStackFrame)
    ;
        Instr0 = region_fill_frame(FillOp, EmbeddedStackFrame, IdRval0,
            NumLval0, AddrLval0),
        remap_rval(StaticCellRemap, IdRval0, IdRval),
        remap_lval(StaticCellRemap, NumLval0, NumLval),
        remap_lval(StaticCellRemap, AddrLval0, AddrLval),
        Instr = region_fill_frame(FillOp, EmbeddedStackFrame, IdRval,
            NumLval, AddrLval)
    ;
        Instr0 = region_set_fixed_slot(SetOp, EmbeddedStackFrame, ValueRval0),
        remap_rval(StaticCellRemap, ValueRval0, ValueRval),
        Instr = region_set_fixed_slot(SetOp, EmbeddedStackFrame, ValueRval)
    ;
        Instr0 = use_and_maybe_pop_region_frame(UseOp, EmbeddedStackFrame),
        Instr = use_and_maybe_pop_region_frame(UseOp, EmbeddedStackFrame)
    ;
        Instr0 = store_ticket(Lval0),
        remap_lval(StaticCellRemap, Lval0, Lval),
        Instr = store_ticket(Lval)
    ;
        Instr0 = reset_ticket(Rval0, Reason),
        remap_rval(StaticCellRemap, Rval0, Rval),
        Instr = reset_ticket(Rval, Reason)
    ;
        Instr0 = mark_ticket_stack(Lval0),
        remap_lval(StaticCellRemap, Lval0, Lval),
        Instr = mark_ticket_stack(Lval)
    ;
        Instr0 = prune_tickets_to(Rval0),
        remap_rval(StaticCellRemap, Rval0, Rval),
        Instr = prune_tickets_to(Rval)
    ;
        Instr0 = init_sync_term(Lval0, NumJoins, ConjId0),
        remap_lval(StaticCellRemap, Lval0, Lval),
        remap_ts_table_index(GlobalDataRemap ^ gdr_maybe_ts_table_offset,
            ConjId0, ConjId),
        Instr = init_sync_term(Lval, NumJoins, ConjId)
    ;
        Instr0 = join_and_continue(Lval0, Label),
        remap_lval(StaticCellRemap, Lval0, Lval),
        Instr = join_and_continue(Lval, Label)
    ;
        Instr0 = lc_create_loop_control(NumSlots, Lval0),
        remap_lval(StaticCellRemap, Lval0, Lval),
        Instr = lc_create_loop_control(NumSlots, Lval)
    ;
        Instr0 = lc_wait_free_slot(Rval0, Lval0, Label),
        remap_rval(StaticCellRemap, Rval0, Rval),
        remap_lval(StaticCellRemap, Lval0, Lval),
        Instr = lc_wait_free_slot(Rval, Lval, Label)
    ;
        Instr0 = lc_spawn_off(LCRval0, LCSRval0, Label),
        remap_rval(StaticCellRemap, LCRval0, LCRval),
        remap_rval(StaticCellRemap, LCSRval0, LCSRval),
        Instr = lc_spawn_off(LCRval, LCSRval, Label)
    ;
        Instr0 = lc_join_and_terminate(LCRval0, LCSRval0),
        remap_rval(StaticCellRemap, LCRval0, LCRval),
        remap_rval(StaticCellRemap, LCSRval0, LCSRval),
        Instr = lc_join_and_terminate(LCRval, LCSRval)
    ;
        ( Instr0 = comment(_)
        ; Instr0 = livevals(_)
        ; Instr0 = llcall(_, _, _, _, _, _)
        ; Instr0 = mkframe(_, _)
        ; Instr0 = label(_)
        ; Instr0 = goto(_)
        ; Instr0 = arbitrary_c_code(_, _, _)
        ; Instr0 = prune_ticket
        ; Instr0 = discard_ticket
        ; Instr0 = incr_sp(_, _, _)
        ; Instr0 = decr_sp(_)
        ; Instr0 = decr_sp_and_return(_)
        ; Instr0 = fork_new_child(_, _)
        ),
        Instr = Instr0
    ).

:- pred remap_ts_table_index(maybe(int)::in, int::in, int::out) is det.

remap_ts_table_index(no, !ConjId).
remap_ts_table_index(yes(Offset), ConjId, ConjId + Offset).

:- pred remap_foreign_proc_component(static_cell_remap_info::in,
    foreign_proc_component::in, foreign_proc_component::out) is det.

remap_foreign_proc_component(Remap, Comp0, Comp) :-
    (
        Comp0 = foreign_proc_inputs(Inputs0),
        list.map(remap_foreign_proc_input(Remap), Inputs0, Inputs),
        Comp = foreign_proc_inputs(Inputs)
    ;
        Comp0 = foreign_proc_outputs(Outputs0),
        list.map(remap_foreign_proc_output(Remap), Outputs0, Outputs),
        Comp = foreign_proc_outputs(Outputs)
    ;
        ( Comp0 = foreign_proc_raw_code(_, _, _, _)
        ; Comp0 = foreign_proc_user_code(_, _, _)
        ; Comp0 = foreign_proc_fail_to(_)
        ; Comp0 = foreign_proc_alloc_id(_)
        ; Comp0 = foreign_proc_noop
        ),
        Comp = Comp0
    ).

:- pred remap_foreign_proc_input(static_cell_remap_info::in,
    foreign_proc_input::in, foreign_proc_input::out) is det.

remap_foreign_proc_input(Remap, Input0, Input) :-
    Input0 = foreign_proc_input(A, B, C, D, Rval0, E, F),
    remap_rval(Remap, Rval0, Rval),
    Input = foreign_proc_input(A, B, C, D, Rval, E, F).

:- pred remap_foreign_proc_output(static_cell_remap_info::in,
    foreign_proc_output::in, foreign_proc_output::out) is det.

remap_foreign_proc_output(Remap, Output0, Output) :-
    Output0 = foreign_proc_output(Lval0, A, B, C, D, E, F),
    remap_lval(Remap, Lval0, Lval),
    Output = foreign_proc_output(Lval, A, B, C, D, E, F).

:- pred remap_lval(static_cell_remap_info::in, lval::in, lval::out) is det.

remap_lval(Remap, Lval0, Lval) :-
    (
        Lval0 = field(MaybeTag, Rval0, FieldNum),
        remap_rval(Remap, Rval0, Rval),
        Lval = field(MaybeTag, Rval, FieldNum)
    ;
        Lval0 = mem_ref(Rval0),
        remap_rval(Remap, Rval0, Rval),
        Lval = mem_ref(Rval)
    ;
        ( Lval0 = reg(_, _)
        ; Lval0 = succip
        ; Lval0 = maxfr
        ; Lval0 = curfr
        ; Lval0 = hp
        ; Lval0 = sp
        ; Lval0 = parent_sp
        ; Lval0 = temp(_, _)
        ; Lval0 = stackvar(_)
        ; Lval0 = parent_stackvar(_)
        ; Lval0 = framevar(_)
        ; Lval0 = double_stackvar(_, _)
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

:- pred remap_rval(static_cell_remap_info::in, rval::in, rval::out) is det.

remap_rval(Remap, Rval0, Rval) :-
    (
        Rval0 = lval(Lval0),
        remap_lval(Remap, Lval0, Lval),
        Rval = lval(Lval)
    ;
        Rval0 = var(_),
        Rval = Rval0
    ;
        Rval0 = mkword(Tag, Ptr0),
        remap_rval(Remap, Ptr0, Ptr),
        Rval = mkword(Tag, Ptr)
    ;
        Rval0 = mkword_hole(_Tag),
        Rval = Rval0
    ;
        Rval0 = const(Const0),
        remap_rval_const(Remap, Const0, Const),
        Rval = const(Const)
    ;
        Rval0 = unop(Unop, A0),
        remap_rval(Remap, A0, A),
        Rval = unop(Unop, A)
    ;
        Rval0 = binop(Binop, A0, B0),
        remap_rval(Remap, A0, A),
        remap_rval(Remap, B0, B),
        Rval = binop(Binop, A, B)
    ;
        Rval0 = mem_addr(MemRef0),
        remap_mem_ref(Remap, MemRef0, MemRef),
        Rval = mem_addr(MemRef)
    ).

:- pred remap_rval_const(static_cell_remap_info::in,
    rval_const::in, rval_const::out) is det.

remap_rval_const(Remap, Const0, Const) :-
    (
        Const0 = llconst_data_addr(DataId0, MaybeOffset),
        (
            DataId0 = scalar_common_data_id(TypeNum0, _CellNum),
            Remap = static_cell_remap_info(TypeNumRemap, ScalarCellGroupRemap),
            ( if map.contains(TypeNumRemap, TypeNum0) then
                map.lookup(ScalarCellGroupRemap, TypeNum0, ScalarCellGroup),
                map.lookup(ScalarCellGroup, DataId0, DataId)
            else
                DataId = DataId0
            )
        ;
            DataId0 = vector_common_data_id(TypeNum0, CellNum),
            Remap = static_cell_remap_info(TypeNumRemap, _),
            ( if map.search(TypeNumRemap, TypeNum0, TypeNum) then
                DataId = vector_common_data_id(TypeNum, CellNum)
            else
                DataId = DataId0
            )
        ;
            ( DataId0 = rtti_data_id(_)
            ; DataId0 = proc_tabling_data_id(_, _)
            ; DataId0 = layout_id(_)
            ; DataId0 = layout_slot_id(_, _)
            ),
            DataId = DataId0
        ),
        Const = llconst_data_addr(DataId,  MaybeOffset)
    ;
        ( Const0 = llconst_true
        ; Const0 = llconst_false
        ; Const0 = llconst_int(_)
        ; Const0 = llconst_uint(_)
        ; Const0 = llconst_int8(_)
        ; Const0 = llconst_uint8(_)
        ; Const0 = llconst_int16(_)
        ; Const0 = llconst_uint16(_)
        ; Const0 = llconst_int32(_)
        ; Const0 = llconst_uint32(_)
        ; Const0 = llconst_int64(_)
        ; Const0 = llconst_uint64(_)
        ; Const0 = llconst_foreign(_, _)
        ; Const0 = llconst_float(_)
        ; Const0 = llconst_string(_)
        ; Const0 = llconst_multi_string(_)
        ; Const0 = llconst_code_addr(_)
        ),
        Const = Const0
    ).

:- pred remap_mem_ref(static_cell_remap_info::in, mem_ref::in, mem_ref::out)
    is det.

remap_mem_ref(Remap, MemRef0, MemRef) :-
    (
        ( MemRef0 = stackvar_ref(_)
        ; MemRef0 = framevar_ref(_)
        ),
        MemRef = MemRef0
    ;
        MemRef0 = heap_ref(Ptr0, MaybeTag, FieldNum),
        remap_rval(Remap, Ptr0, Ptr),
        MemRef = heap_ref(Ptr, MaybeTag, FieldNum)
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.global_data.
%-----------------------------------------------------------------------------%
