%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Author: zs.
%
% This module manages global data structures for the LLDS backend.
%
%-----------------------------------------------------------------------------%

:- module ll_backend__global_data.

:- interface.

:- import_module hlds__hlds_pred.
:- import_module ll_backend__continuation_info.
:- import_module ll_backend__exprn_aux.
:- import_module ll_backend__llds.
:- import_module mdbcomp__prim_data. % for module_name

:- import_module assoc_list.
:- import_module bool.
:- import_module list.

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

:- implementation.

:- import_module backend_libs__rtti.
:- import_module ll_backend__layout.
:- import_module ll_backend__llds_out.
:- import_module parse_tree__error_util.

:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module std_util.

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
    map__init(EmptyDataMap),
    map__init(EmptyLayoutMap),
    GlobalData = global_data(EmptyDataMap, EmptyLayoutMap, [], StaticCellInfo).

global_data_add_new_proc_var(PredProcId, ProcVar, !GlobalData) :-
    ProcVarMap0 = !.GlobalData ^ proc_var_map,
    map__det_insert(ProcVarMap0, PredProcId, ProcVar, ProcVarMap),
    !:GlobalData = !.GlobalData ^ proc_var_map := ProcVarMap.

global_data_add_new_proc_layout(PredProcId, ProcLayout, !GlobalData) :-
    ProcLayoutMap0 = !.GlobalData ^ proc_layout_map,
    map__det_insert(ProcLayoutMap0, PredProcId, ProcLayout, ProcLayoutMap),
    !:GlobalData = !.GlobalData ^ proc_layout_map := ProcLayoutMap.

global_data_update_proc_layout(PredProcId, ProcLayout, !GlobalData) :-
    ProcLayoutMap0 = !.GlobalData ^ proc_layout_map,
    map__det_update(ProcLayoutMap0, PredProcId, ProcLayout, ProcLayoutMap),
    !:GlobalData = !.GlobalData ^ proc_layout_map := ProcLayoutMap.

global_data_add_new_closure_layouts(NewClosureLayouts, !GlobalData) :-
    ClosureLayouts0 = !.GlobalData ^ closure_layouts,
    list__append(NewClosureLayouts, ClosureLayouts0, ClosureLayouts),
    !:GlobalData = !.GlobalData ^ closure_layouts := ClosureLayouts.

global_data_maybe_get_proc_layout(GlobalData, PredProcId, ProcLayout) :-
    ProcLayoutMap = GlobalData ^ proc_layout_map,
    map__search(ProcLayoutMap, PredProcId, ProcLayout).

global_data_get_proc_layout(GlobalData, PredProcId, ProcLayout) :-
    ProcLayoutMap = GlobalData ^ proc_layout_map,
    map__lookup(ProcLayoutMap, PredProcId, ProcLayout).

global_data_get_all_proc_vars(GlobalData, ProcVars) :-
    ProcVarMap = GlobalData ^ proc_var_map,
    map__values(ProcVarMap, ProcVars).

global_data_get_all_proc_layouts(GlobalData, ProcLayouts) :-
    ProcLayoutMap = GlobalData ^ proc_layout_map,
    map__values(ProcLayoutMap, ProcLayouts).

global_data_get_all_closure_layouts(GlobalData, ClosureLayouts) :-
    ClosureLayouts = GlobalData ^ closure_layouts.

global_data_get_static_cell_info(GlobalData, StaticCellInfo) :-
    StaticCellInfo = GlobalData ^ static_cell_info.

global_data_set_static_cell_info(StaticCellInfo, !GlobalData) :-
    !:GlobalData = !.GlobalData ^ static_cell_info := StaticCellInfo.

%-----------------------------------------------------------------------------%

:- type cell_type
    --->    plain_type(list(llds_type))
    ;       grouped_type(assoc_list(llds_type, int)).

:- type cell_args
    --->    plain_args(assoc_list(rval, llds_type))
    ;       grouped_args(list(common_cell_arg_group)).

    % There is one cell_type_group for every group of cells that share
    % the same sequence of argument types. We don't actually need the
    % cell type here, since we can't get to a cell_type_group from
    % the cell_group_map without knowing it.
:- type cell_type_group
    --->    cell_type_group(
                cell_type_number    :: int,
                cell_group_members  :: map(list(rval), data_name)
            ).

:- type static_cell_info
    --->    static_cell_info(
                module_name         :: module_name, % base file name
                unbox_float         :: bool,
                common_data         :: bool,
                cell_counter        :: counter, % next cell number
                type_counter        :: counter, % next type number
                cells               :: map(int, common_data),
                cell_group_map      :: map(cell_type, cell_type_group)
                                    % map cell argument types and then cell
                                    % contents to the id of the common cell
            ).

init_static_cell_info(BaseName, UnboxFloat, CommonData) = Info0 :-
    map__init(Cells0),
    map__init(CellMap0),
    Info0 = static_cell_info(BaseName, UnboxFloat, CommonData,
        counter__init(0), counter__init(0), Cells0, CellMap0).

add_static_cell_natural_types(Args, DataAddr, !Info) :-
    list__map(associate_natural_type(!.Info ^ unbox_float), Args, ArgsTypes),
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

:- pred do_add_static_cell(assoc_list(rval, llds_type)::in, cell_type::in,
    cell_args::in, data_addr::out,
    static_cell_info::in, static_cell_info::out) is det.

do_add_static_cell(ArgsTypes, CellType, CellArgs, DataAddr, !Info) :-
    assoc_list__keys(ArgsTypes, Args),
    CellGroupMap0 = !.Info ^ cell_group_map,
    ( map__search(CellGroupMap0, CellType, CellGroup0) ->
        TypeNum = CellGroup0 ^ cell_type_number,
        CellGroup1 = CellGroup0
    ;
        TypeNumCounter0 = !.Info ^ type_counter,
        counter__allocate(TypeNum, TypeNumCounter0, TypeNumCounter),
        !:Info = !.Info ^ type_counter := TypeNumCounter,
        CellGroup1 = cell_type_group(TypeNum, map__init)
    ),
    MembersMap0 = CellGroup1 ^ cell_group_members,
    ModuleName = !.Info ^ module_name,
    ( map__search(MembersMap0, Args, DataNamePrime) ->
        DataName = DataNamePrime
    ;
        CellNumCounter0 = !.Info ^ cell_counter,
        counter__allocate(CellNum, CellNumCounter0, CellNumCounter),
        !:Info = !.Info ^ cell_counter := CellNumCounter,
        DataName = common(CellNum, TypeNum),
        (
            !.Info ^ common_data = yes,
            map__set(MembersMap0, Args, DataName, MembersMap),
            CellGroup = CellGroup1 ^ cell_group_members := MembersMap,
            map__set(CellGroupMap0, CellType, CellGroup, CellGroupMap),
            !:Info = !.Info ^ cell_group_map := CellGroupMap
        ;
            !.Info ^ common_data = no
            % With --no-common-data, we never insert any cell into
            % CellGroupMap, ensuring that it stays empty. This can
            % be useful when comparing the LLDS and MLDS backends.
        ),
        Cells0 = !.Info ^ cells,
        (
            CellArgs = plain_args(PlainArgs),
            CellTypeAndValue = plain_type_and_value(TypeNum, PlainArgs)
        ;
            CellArgs = grouped_args(GroupedArgs),
            CellTypeAndValue = grouped_type_and_value(TypeNum, GroupedArgs)
        ),
        Cell = common_data(ModuleName, CellNum, CellTypeAndValue),
        map__det_insert(Cells0, CellNum, Cell, Cells),
        !:Info = !.Info ^ cells := Cells
    ),
    DataAddr = data_addr(ModuleName, DataName).

:- pred compute_cell_type(assoc_list(rval, llds_type)::in, cell_type::out,
    cell_args::out) is det.

compute_cell_type(ArgsTypes, CellType, CellTypeAndValue) :-
    (
        ArgsTypes = [FirstArg - FirstArgType | LaterArgsTypes],
        threshold_group_types(FirstArgType, [FirstArg], LaterArgsTypes,
            TypeGroups, TypeAndArgGroups),
        OldLength = list__length(ArgsTypes),
        NewLength = list__length(TypeAndArgGroups),
        OldLength >= NewLength * 2
    ->
        CellType = grouped_type(TypeGroups),
        CellTypeAndValue = grouped_args(TypeAndArgGroups)
    ;
        CellType = plain_type(assoc_list__values(ArgsTypes)),
        CellTypeAndValue = plain_args(ArgsTypes)
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
        list__length(RevArgs, NumArgs),
        list__reverse(RevArgs, Args),
        TypeGroup = Type - NumArgs,
        TypeAndArgGroup = common_cell_grouped_args(Type, NumArgs, Args)
    ).

search_static_cell_offset(Info, DataAddr, Offset, Rval) :-
    DataAddr = data_addr(Info ^ module_name, DataName),
    DataName = common(CellNum, _),
    map__search(Info ^ cells, CellNum, CommonData),
    CommonData = common_data(_, _, TypeAndValue),
    (
        TypeAndValue = plain_type_and_value(_, ArgsTypes),
        list__index0_det(ArgsTypes, Offset, Rval - _)
    ;
        TypeAndValue = grouped_type_and_value(_, ArgGroups),
        offset_into_group(ArgGroups, Offset, Rval)
    ).

:- pred offset_into_group(list(common_cell_arg_group)::in, int::in, rval::out)
    is det.

offset_into_group([], _, _) :-
    unexpected(this_file, "offset_into_group: offset out of bounds").
offset_into_group([Group | Groups], Offset, Rval) :-
    (
        Group = common_cell_grouped_args(_, NumRvalsInGroup, Rvals),
        ( Offset < NumRvalsInGroup ->
            list__index0_det(Rvals, Offset, Rval)
        ;
            offset_into_group(Groups, Offset - NumRvalsInGroup, Rval)
        )
    ;
        Group = common_cell_ungrouped_arg(_, GroupRval),
        ( Offset = 0 ->
            Rval = GroupRval
        ;
            offset_into_group(Groups, Offset - 1, Rval)
        )
    ).

get_static_cells(Info) =
    list__map(wrap_common_data, map__values(Info ^ cells)).

:- func wrap_common_data(common_data) = comp_gen_c_data.

wrap_common_data(CommonData) = common_data(CommonData).

rval_type_as_arg(Rval, ExprnOpts, Type) :-
    natural_type(ExprnOpts ^ unboxed_float, Rval, Type).

:- pred natural_type(bool::in, rval::in, llds_type::out) is det.

natural_type(UnboxFloat, Rval, Type) :-
    llds__rval_type(Rval, Type0),
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
