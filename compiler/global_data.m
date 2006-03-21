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
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data. % for module_name

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

:- pred global_data_add_new_closure_layouts(list(comp_gen_c_data)::in,
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
    list(comp_gen_c_data)::out) is det.

:- pred global_data_get_static_cell_info(global_data::in,
    static_cell_info::out) is det.

:- pred global_data_set_static_cell_info(static_cell_info::in,
    global_data::in, global_data::out) is det.

:- type static_cell_info.

:- func init_static_cell_info(module_name, bool, bool) = static_cell_info.

:- pred add_static_cell(assoc_list(rval, llds_type)::in, data_addr::out,
    static_cell_info::in, static_cell_info::out) is det.

:- pred add_static_cell_natural_types(list(rval)::in, data_addr::out,
    static_cell_info::in, static_cell_info::out) is det.

:- pred search_static_cell_offset(static_cell_info::in, data_addr::in, int::in,
    rval::out) is semidet.

:- func get_static_cells(static_cell_info) = list(comp_gen_c_data).

    % Given an rval, figure out the type it would have as
    % an argument.  Normally that's the same as its usual type;
    % the exception is that for boxed floats, the type is data_ptr
    % (i.e. the type of the boxed value) rather than float
    % (the type of the unboxed value).
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
:- import_module set.
:- import_module std_util.

%-----------------------------------------------------------------------------%

:- type proc_var_map    ==  map(pred_proc_id, comp_gen_c_var).
:- type proc_layout_map ==  map(pred_proc_id, proc_layout_info).

:- type global_data
    --->    global_data(
                proc_var_map        :: proc_var_map,
                                    % Information about the global
                                    % variables defined by each
                                    % procedure.

                proc_layout_map     :: proc_layout_map,
                                    % Information about the
                                    % layout structures defined
                                    % by each procedure.

                closure_layouts     :: list(comp_gen_c_data),
                                    % The list of all closure
                                    % layouts generated in this
                                    % module. While all closure
                                    % layouts are different from
                                    % all other comp_gen_c_datas,
                                    % it is possible, although
                                    % unlikely, for two closures
                                    % to have the same layout.

                static_cell_info    :: static_cell_info
                                    % Information about all the
                                    % statically allocated cells
                                    % created so far.
            ).

:- func wrap_layout_data(layout_data) = comp_gen_c_data.

wrap_layout_data(LayoutData) = layout_data(LayoutData).

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

    % There is one cell_type_group for every group of cells that share
    % the same sequence of argument types. We don't actually need the
    % cell type here, since we can't get to a cell_type_group from
    % the cell_group_map without knowing it.
    %
:- type cell_type_group
    --->    cell_type_group(
                cell_counter        :: counter, % next cell number
                cell_group_members  :: bimap(list(rval), data_name),
                cell_rev_array      :: list(common_cell_value)
            ).

:- type static_cell_sub_info
    --->    static_cell_sub_info(
                module_name         :: module_name, % base file name
                unbox_float         :: bool,
                common_data         :: bool
            ).

:- type static_cell_info
    --->    static_cell_info(
                sub_info            :: static_cell_sub_info,
                type_counter        :: counter, % next type number

                cell_type_num_map   :: bimap(common_cell_type, int),
                                    % Maps types to type numbers and vice
                                    % versa.

                cell_group_map      :: map(int, cell_type_group)
                                    % Maps the cell type number to the
                                    % information we have for all cells of that
                                    % type.
            ).

init_static_cell_info(BaseName, UnboxFloat, CommonData) = Info0 :-
    SubInfo0 = static_cell_sub_info(BaseName, UnboxFloat, CommonData),
    Info0 = static_cell_info(SubInfo0, counter.init(0), bimap.init, map.init).

add_static_cell_natural_types(Args, DataAddr, !Info) :-
    list.map(associate_natural_type(!.Info ^ sub_info ^ unbox_float),
        Args, ArgsTypes),
    add_static_cell(ArgsTypes, DataAddr, !Info).

add_static_cell(ArgsTypes0, DataAddr, !Info) :-
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
    do_add_static_cell(ArgsTypes, CellType, CellTypeAndValue, DataAddr, !Info).

:- pred do_add_static_cell(assoc_list(rval, llds_type)::in,
    common_cell_type::in, common_cell_value::in, data_addr::out,
    static_cell_info::in, static_cell_info::out) is det.

do_add_static_cell(ArgsTypes, CellType, CellValue, DataAddr, !Info) :-
    assoc_list.keys(ArgsTypes, Args),
    some [!CellGroup] (
        TypeNumMap0 = !.Info ^ cell_type_num_map,
        CellGroupMap0 = !.Info ^ cell_group_map,
        ( bimap.search(TypeNumMap0, CellType, TypeNumPrime) ->
            TypeNum = TypeNumPrime,
            map.lookup(CellGroupMap0, TypeNum, !:CellGroup)
        ;
            TypeNumCounter0 = !.Info ^ type_counter,
            counter.allocate(TypeNum, TypeNumCounter0, TypeNumCounter),
            !:Info = !.Info ^ type_counter := TypeNumCounter,

            bimap.det_insert(TypeNumMap0, CellType, TypeNum, TypeNumMap),
            !:Info = !.Info ^ cell_type_num_map := TypeNumMap,

            !:CellGroup = cell_type_group(counter.init(0), bimap.init, [])
        ),
        MembersMap0 = !.CellGroup ^ cell_group_members,
        ( bimap.search(MembersMap0, Args, DataNamePrime) ->
            DataName = DataNamePrime
        ;
            CellNumCounter0 = !.CellGroup ^ cell_counter,
            counter.allocate(CellNum, CellNumCounter0, CellNumCounter),
            !:CellGroup = !.CellGroup ^ cell_counter := CellNumCounter,
            DataName = common_ref(TypeNum, CellNum),
            RevArray0 = !.CellGroup ^ cell_rev_array,
            RevArray = [CellValue | RevArray0],
            !:CellGroup = !.CellGroup ^ cell_rev_array := RevArray,
            InsertCommonData = !.Info ^ sub_info ^ common_data,
            (
                InsertCommonData = yes,
                bimap.det_insert(MembersMap0, Args, DataName, MembersMap),
                !:CellGroup = !.CellGroup ^ cell_group_members := MembersMap
            ;
                InsertCommonData = no
                % With --no-common-data, we never insert any cell into
                % CellGroupMap, ensuring that it stays empty. This can
                % be useful when comparing the LLDS and MLDS backends.
            ),
            map.set(CellGroupMap0, TypeNum, !.CellGroup, CellGroupMap),
            !:Info = !.Info ^ cell_group_map := CellGroupMap
        )
    ),
    ModuleName = !.Info ^ sub_info ^ module_name,
    DataAddr = data_addr(ModuleName, DataName).

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

search_static_cell_offset(Info, DataAddr, Offset, Rval) :-
    DataAddr = data_addr(Info ^ sub_info ^ module_name, DataName),
    DataName = common_ref(TypeNum, _CellNum),
    CellGroupMap = Info ^ cell_group_map,
    map.lookup(CellGroupMap, TypeNum, CellGroup),
    CellGroupMembers = CellGroup ^ cell_group_members,
    bimap.reverse_lookup(CellGroupMembers, Rvals, DataName),
    list.index0_det(Rvals, Offset, Rval).

%-----------------------------------------------------------------------------%

get_static_cells(Info) = Datas :-
    ModuleName = Info ^ sub_info ^ module_name,
    TypeNumMap = Info ^ cell_type_num_map,
    map.foldl(add_static_cell_for_type(ModuleName, TypeNumMap),
        Info ^ cell_group_map, [], RevDatas),
    list.reverse(RevDatas, Datas).

:- pred add_static_cell_for_type(module_name::in,
    bimap(common_cell_type, int)::in, int::in, cell_type_group::in,
    list(comp_gen_c_data)::in, list(comp_gen_c_data)::out) is det.

add_static_cell_for_type(ModuleName, TypeNumMap, TypeNum, CellGroup, !Datas) :-
    bimap.reverse_lookup(TypeNumMap, CellType, TypeNum),
    list.reverse(CellGroup ^ cell_rev_array, ArrayContents),
    Array = common_data_array(ModuleName, CellType, TypeNum, ArrayContents),
    Data = common_data(Array),
    !:Datas = [Data | !.Datas].

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
