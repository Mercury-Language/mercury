%-----------------------------------------------------------------------------%
% Copyright (C) 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Author: zs.

% This module manages global data structures for the LLDS backend.

%-----------------------------------------------------------------------------%

:- module ll_backend__global_data.

:- interface.

:- import_module hlds__hlds_pred.
:- import_module ll_backend__continuation_info.
:- import_module ll_backend__exprn_aux.
:- import_module ll_backend__llds.
:- import_module ll_backend__layout.
:- import_module parse_tree__prog_data. % for module_name

:- import_module bool, list.

:- type global_data.

:- pred global_data_init(list(layout_data)::in, static_cell_info::in,
	global_data::out) is det.

:- pred global_data_add_new_proc_var( pred_proc_id::in, comp_gen_c_var::in,
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

:- pred global_data_get_all_deep_prof_data(global_data::in,
	list(comp_gen_c_data)::out) is det.

:- pred global_data_get_static_cell_info(global_data::in,
	static_cell_info::out) is det.

:- pred global_data_set_static_cell_info(static_cell_info::in,
	global_data::in, global_data::out) is det.

:- import_module bool, list, assoc_list.

:- type static_cell_info.

:- func init_static_cell_info(module_name, bool, bool) = static_cell_info.

:- pred add_static_cell(assoc_list(rval, llds_type)::in, data_addr::out,
	static_cell_info::in, static_cell_info::out) is det.

:- pred add_static_cell_natural_types(list(rval)::in, data_addr::out,
	static_cell_info::in, static_cell_info::out) is det.

:- pred search_static_cell(static_cell_info::in, data_addr::in,
	assoc_list(rval, llds_type)::out) is semidet.

:- func get_static_cells(static_cell_info) = list(comp_gen_c_data).

	% Given an rval, figure out the type it would have as
	% an argument.  Normally that's the same as its usual type;
	% the exception is that for boxed floats, the type is data_ptr
	% (i.e. the type of the boxed value) rather than float
	% (the type of the unboxed value).

:- pred rval_type_as_arg(rval::in, exprn_opts::in, llds_type::out) is det.

:- implementation.

:- import_module backend_libs__rtti.
:- import_module ll_backend__layout.
:- import_module ll_backend__llds_out.

:- import_module int, counter, set, map, std_util.

:- type proc_var_map	==	map(pred_proc_id, comp_gen_c_var).
:- type proc_layout_map	==	map(pred_proc_id, proc_layout_info).

:- type global_data
	--->	global_data(
			proc_var_map		:: proc_var_map,
						% Information about the global
						% variables defined by each
						% procedure.
			proc_layout_map		:: proc_layout_map,
						% Information about the
						% layout structures defined
						% by each procedure.
			closure_layouts		:: list(comp_gen_c_data),
						% The list of all closure
						% layouts generated in this
						% module. While all closure
						% layouts are different from
						% all other comp_gen_c_datas,
						% it is possible, although
						% unlikely, for two closures
						% to have the same layout.
			deep_prof_data		:: list(comp_gen_c_data),
						% The list of global data
						% structures created by the
						% deep profiling
						% transformation.
			static_cell_info	:: static_cell_info
						% Information about all the
						% statically allocated cells
						% created so far.
		).

:- func wrap_layout_data(layout_data) = comp_gen_c_data.

wrap_layout_data(LayoutData) = layout_data(LayoutData).

global_data_init(LayoutData, StaticCellInfo, GlobalData) :-
	map__init(EmptyDataMap),
	map__init(EmptyLayoutMap),
	WrappedLayoutData = list__map(wrap_layout_data, LayoutData),
	GlobalData = global_data(EmptyDataMap, EmptyLayoutMap, [],
		WrappedLayoutData, StaticCellInfo).

global_data_add_new_proc_var(PredProcId, ProcVar, GlobalData0, GlobalData) :-
	ProcVarMap0 = GlobalData0 ^ proc_var_map,
	map__det_insert(ProcVarMap0, PredProcId, ProcVar, ProcVarMap),
	GlobalData = GlobalData0 ^ proc_var_map := ProcVarMap.

global_data_add_new_proc_layout(PredProcId, ProcLayout,
		GlobalData0, GlobalData) :-
	ProcLayoutMap0 = GlobalData0 ^ proc_layout_map,
	map__det_insert(ProcLayoutMap0, PredProcId, ProcLayout, ProcLayoutMap),
	GlobalData = GlobalData0 ^ proc_layout_map := ProcLayoutMap.

global_data_update_proc_layout(PredProcId, ProcLayout,
		GlobalData0, GlobalData) :-
	ProcLayoutMap0 = GlobalData0 ^ proc_layout_map,
	map__det_update(ProcLayoutMap0, PredProcId, ProcLayout, ProcLayoutMap),
	GlobalData = GlobalData0 ^ proc_layout_map := ProcLayoutMap.

global_data_add_new_closure_layouts(NewClosureLayouts,
		GlobalData0, GlobalData) :-
	ClosureLayouts0 = GlobalData0 ^ closure_layouts,
	list__append(NewClosureLayouts, ClosureLayouts0, ClosureLayouts),
	GlobalData = GlobalData0 ^ closure_layouts := ClosureLayouts.

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

global_data_get_all_deep_prof_data(GlobalData, LayoutData) :-
	LayoutData = GlobalData ^ deep_prof_data.

global_data_get_static_cell_info(GlobalData, StaticCellInfo) :-
	StaticCellInfo = GlobalData ^ static_cell_info.

global_data_set_static_cell_info(StaticCellInfo, GlobalData0, GlobalData) :-
	GlobalData = GlobalData0 ^ static_cell_info := StaticCellInfo.

%-----------------------------------------------------------------------------%

:- type cell_type_group
	--->	cell_type_group(
			% cell_arg_types		:: list(llds_type)
			cell_type_number 	:: int,
			cell_group_members	:: map(list(rval), data_name)
		).

:- type static_cell_info
	--->	static_cell_info(
			module_name	:: module_name,	% base file name
			unbox_float	:: bool,
			common_data	:: bool,
			cell_counter	:: counter,	% next cell number
			type_counter	:: counter,	% next type number
			cells		:: map(int, comp_gen_c_data),
			cell_group_map	:: map(list(llds_type),
						cell_type_group)
					% map cell argument types and then cell
					% contents to the id of the common cell
		).

init_static_cell_info(BaseName, UnboxFloat, CommonData) = Info0 :-
	map__init(Cells0),
	map__init(CellMap0),
	Info0 = static_cell_info(BaseName, UnboxFloat, CommonData,
		counter__init(0), counter__init(0), Cells0, CellMap0).

add_static_cell(ArgsTypes0, DataAddr, !Info) :-
		% If we have an empty cell, place a dummy field in it,
		% so that the generated C structure isn't empty.
	( ArgsTypes0 = [] ->
		ArgsTypes = [const(int_const(-1)) - integer]
	;
		ArgsTypes = ArgsTypes0
	),
	assoc_list__keys(ArgsTypes, Args),
	assoc_list__values(ArgsTypes, Types),
	CellGroupMap0 = !.Info ^ cell_group_map,
	( map__search(CellGroupMap0, Types, CellGroup0) ->
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
			CellGroup = CellGroup1 ^ cell_group_members
				:= MembersMap,
			map__set(CellGroupMap0, Types, CellGroup, CellGroupMap),
			!:Info = !.Info ^ cell_group_map := CellGroupMap
		;
			!.Info ^ common_data = no
			% With --no-common-data, we never insert any cell into
			% CellGroupMap, ensuring that it stays empty. This can
			% be useful when comparing the LLDS and MLDS backends.
		),
		Cells0 = !.Info ^ cells,
		Cell = common_data(ModuleName, CellNum, TypeNum, ArgsTypes),
		map__det_insert(Cells0, CellNum, Cell, Cells),
		!:Info = !.Info ^ cells := Cells
	),
	DataAddr = data_addr(ModuleName, DataName).

add_static_cell_natural_types(Args, DataAddr, !Info) :-
	list__map(associate_natural_type(!.Info ^ unbox_float), Args,
		ArgsTypes),
	add_static_cell(ArgsTypes, DataAddr, !Info).

search_static_cell(Info, DataAddr, ArgsTypes) :-
	DataAddr = data_addr(Info ^ module_name, DataName),
	DataName = common(CellNum, _),
	map__search(Info ^ cells, CellNum, CompGenCData),
	CompGenCData = common_data(_, _, _, ArgsTypes).

get_static_cells(Info) = map__values(Info ^ cells).

% %-----------------------------------------------------------------------------%
% 
% :- pred flatten_arg_types(list(rval)::in, create_arg_types::in,
% 	bool::in, assoc_list(rval, llds_type)::out) is det.
% 
% flatten_arg_types(Args, uniform(MaybeType), UnboxFloat, TypedRvals) :-
% 	flatten_uniform_arg_types(Args, MaybeType, UnboxFloat, TypedRvals).
% flatten_arg_types(Args, initial(InitialTypes, RestTypes), UnboxFloat,
% 		TypedRvals) :-
% 	flatten_initial_arg_types(Args, InitialTypes, RestTypes, UnboxFloat,
% 		TypedRvals).
% flatten_arg_types(Args, none, _, []) :-
% 	require(unify(Args, []), "too many args for specified arg types").
% 
% :- pred flatten_uniform_arg_types(list(rval)::in, maybe(llds_type)::in,
% 	bool::in, assoc_list(rval, llds_type)::out) is det.
% 
% flatten_uniform_arg_types([], _, _, []).
% flatten_uniform_arg_types([Rval | Rvals], MaybeType, UnboxFloat,
% 		[Rval - Type | TypedRvals]) :-
% 	llds_arg_type(Rval, MaybeType, UnboxFloat, Type),
% 	flatten_uniform_arg_types(Rvals, MaybeType, UnboxFloat, TypedRvals).
% 
% :- pred flatten_initial_arg_types(list(rval)::in, initial_arg_types::in,
% 	create_arg_types::in, bool::in, assoc_list(rval, llds_type)::out)
% 	is det.
% 
% flatten_initial_arg_types(Args, [], RestTypes, UnboxFloat, TypedRvals) :-
% 	flatten_arg_types(Args, RestTypes, UnboxFloat, TypedRvals).
% flatten_initial_arg_types(Args, [N - MaybeType | InitTypes], RestTypes,
% 		UnboxFloat, TypedRvals) :-
% 	flatten_initial_arg_types_2(Args, N, MaybeType, InitTypes, RestTypes,
% 		UnboxFloat, TypedRvals).
% 
% :- pred flatten_initial_arg_types_2(list(rval)::in, int::in,
% 	maybe(llds_type)::in, initial_arg_types::in, create_arg_types::in,
% 	bool::in, assoc_list(rval, llds_type)::out) is det.
% 
% flatten_initial_arg_types_2([], N, _, _, _, _, []) :-
% 	require(unify(N, 0), "not enough args for specified arg types").
% flatten_initial_arg_types_2([Rval | Rvals], N, MaybeType, InitTypes,
% 		RestTypes, UnboxFloat, TypedRvals) :-
% 	( N = 0 ->
% 		flatten_initial_arg_types([Rval | Rvals], InitTypes,
% 			RestTypes, UnboxFloat, TypedRvals)
% 	;
% 		llds_arg_type(Rval, MaybeType, UnboxFloat, Type),
% 		flatten_initial_arg_types_2(Rvals, N - 1, MaybeType,
% 			InitTypes, RestTypes, UnboxFloat,
% 			TypedRvalsTail),
% 		TypedRvals = [Rval - Type | TypedRvalsTail]
% 	).
% 
% 	% Given an rval, figure out the type it would have as an argument,
% 	% if it is not explicitly specified.
% 
% :- pred llds_arg_type(rval::in, maybe(llds_type)::in, bool::in,
% 	llds_type::out) is det.
% 
% llds_arg_type(Rval, MaybeType, UnboxFloat, Type) :-
% 	( MaybeType = yes(SpecType) ->
% 		Type = SpecType
% 	;
% 		rval_type_as_arg(Rval, UnboxFloat, Type)
% 	).

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
