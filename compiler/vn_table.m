%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% vn_table.m - predicates for managing vn tables.

% author: zs.

%-----------------------------------------------------------------------------%

:- module vn_table.

:- interface.

:- import_module vn_type.
:- import_module assoc_list, list, map.

:- type vn_tables.

% The definitions of the following types are exported only for debugging.

:- type lval_to_vn_table == map(vnlval, vn).
:- type rval_to_vn_table == map(vnrval, vn).
:- type vn_to_rval_table == map(vn, vnrval).
:- type vn_to_uses_table == map(vn, list(vn_src)).
:- type vn_to_locs_table == map(vn, list(vnlval)).
:- type loc_to_vn_table  == map(vnlval, vn).

:- pred vn_table__init_tables(vn_tables).
:- mode vn_table__init_tables(out) is det.

:- pred vn_table__get_next_vn(vn_tables, vn).
:- mode vn_table__get_next_vn(in, out) is det.

:- pred vn_table__get_lval_to_vn_table(vn_tables, lval_to_vn_table).
:- mode vn_table__get_lval_to_vn_table(in, out) is det.

:- pred vn_table__get_rval_to_vn_table(vn_tables, rval_to_vn_table).
:- mode vn_table__get_rval_to_vn_table(in, out) is det.

:- pred vn_table__get_vn_to_rval_table(vn_tables, vn_to_rval_table).
:- mode vn_table__get_vn_to_rval_table(in, out) is det.

:- pred vn_table__get_vn_to_uses_table(vn_tables, vn_to_uses_table).
:- mode vn_table__get_vn_to_uses_table(in, out) is det.

:- pred vn_table__get_vn_to_locs_table(vn_tables, vn_to_locs_table).
:- mode vn_table__get_vn_to_locs_table(in, out) is det.

:- pred vn_table__get_loc_to_vn_table(vn_tables, loc_to_vn_table).
:- mode vn_table__get_loc_to_vn_table(in, out) is det.

:- pred vn_table__lookup_desired_value(vnlval, vn, string, vn_tables).
:- mode vn_table__lookup_desired_value(in, out, in, in) is det.

:- pred vn_table__lookup_assigned_vn(vnrval, vn, string, vn_tables).
:- mode vn_table__lookup_assigned_vn(in, out, in, in) is det.

:- pred vn_table__lookup_defn(vn, vnrval, string, vn_tables).
:- mode vn_table__lookup_defn(in, out, in, in) is det.

:- pred vn_table__lookup_uses(vn, list(vn_src), string, vn_tables).
:- mode vn_table__lookup_uses(in, out, in, in) is det.

:- pred vn_table__lookup_current_locs(vn, list(vnlval), string, vn_tables).
:- mode vn_table__lookup_current_locs(in, out, in, in) is det.

:- pred vn_table__lookup_current_value(vnlval, vn, string, vn_tables).
:- mode vn_table__lookup_current_value(in, out, in, in) is det.

:- pred vn_table__search_desired_value(vnlval, vn, vn_tables).
:- mode vn_table__search_desired_value(in, out, in) is semidet.

:- pred vn_table__search_assigned_vn(vnrval, vn, vn_tables).
:- mode vn_table__search_assigned_vn(in, out, in) is semidet.

:- pred vn_table__search_defn(vn, vnrval, vn_tables).
:- mode vn_table__search_defn(in, out, in) is semidet.

:- pred vn_table__search_uses(vn, list(vn_src), vn_tables).
:- mode vn_table__search_uses(in, out, in) is semidet.

:- pred vn_table__search_current_locs(vn, list(vnlval), vn_tables).
:- mode vn_table__search_current_locs(in, out, in) is semidet.

:- pred vn_table__search_current_value(vnlval, vn, vn_tables).
:- mode vn_table__search_current_value(in, out, in) is semidet.

:- pred vn_table__get_vnlval_vn_list(vn_tables, assoc_list(vnlval, vn)).
:- mode vn_table__get_vnlval_vn_list(in, out) is det.

:- pred vn_table__add_new_use(vn, vn_src, vn_tables, vn_tables).
% :- mode vn_table__add_new_use(in, in, di, uo) is det.
:- mode vn_table__add_new_use(in, in, in, out) is det.

:- pred vn_table__del_old_use(vn, vn_src, vn_tables, vn_tables).
% :- mode vn_table__del_old_use(in, in, di, uo) is det.
:- mode vn_table__del_old_use(in, in, in, out) is det.

:- pred vn_table__del_old_uses(list(vn), vn_src, vn_tables, vn_tables).
% :- mode vn_table__del_old_uses(in, in, di, uo) is det.
:- mode vn_table__del_old_uses(in, in, in, out) is det.

:- pred vn_table__record_first_vnrval(vnrval, vn, vn_tables, vn_tables).
% :- mode vn_table__record_first_vnrval(in, out, di, uo) is det.
:- mode vn_table__record_first_vnrval(in, out, in, out) is det.

:- pred vn_table__record_new_vnrval(vnrval, vn, vn_tables, vn_tables).
% :- mode vn_table__record_first_vnrval(in, out, di, uo) is det.
:- mode vn_table__record_new_vnrval(in, out, in, out) is det.

:- pred vn_table__record_first_vnlval(vnlval, vn, vn_tables, vn_tables).
% :- mode vn_table__record_first_vnlval(in, out, di, uo) is det.
:- mode vn_table__record_first_vnlval(in, out, in, out) is det.

:- pred vn_table__set_desired_value(vnlval, vn, vn_tables, vn_tables).
% :- mode vn_table__set_desired_value(in, in, di, uo) is det.
:- mode vn_table__set_desired_value(in, in, in, out) is det.

:- pred vn_table__set_current_value(vnlval, vn, vn_tables, vn_tables).
% :- mode vn_table__set_current_value(in, in, di, uo) is det.
:- mode vn_table__set_current_value(in, in, in, out) is det.

:- pred vn_table__set_parallel_value(vnlval, vn, vn_tables, vn_tables).
% :- mode vn_table__set_parallel_value(in, in, di, uo) is det.
:- mode vn_table__set_parallel_value(in, in, in, out) is det.

:- pred vn_table__get_all_vnrvals(list(vnrval), vn_tables).
:- mode vn_table__get_all_vnrvals(out, in) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module llds, opt_debug.
:- import_module int, string, require.

:- type vn_tables --->	vn_tables(vn,
				lval_to_vn_table, rval_to_vn_table,
				vn_to_rval_table, vn_to_uses_table,
				vn_to_locs_table, loc_to_vn_table).

vn_table__get_next_vn(VnTables, NextVn) :-
	VnTables = vn_tables(NextVn,
		_Lval_to_vn_table, _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table).

vn_table__get_lval_to_vn_table(VnTables, Lval_to_vn_table) :-
	VnTables = vn_tables(_NextVn,
		 Lval_to_vn_table, _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table).

vn_table__get_rval_to_vn_table(VnTables, Rval_to_vn_table) :-
	VnTables = vn_tables(_NextVn,
		_Lval_to_vn_table,  Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table).

vn_table__get_vn_to_rval_table(VnTables, Vn_to_rval_table) :-
	VnTables = vn_tables(_NextVn,
		_Lval_to_vn_table, _Rval_to_vn_table,
		 Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table).

vn_table__get_vn_to_uses_table(VnTables, Vn_to_uses_table) :-
	VnTables = vn_tables(_NextVn,
		_Lval_to_vn_table, _Rval_to_vn_table,
		_Vn_to_rval_table,  Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table).

vn_table__get_vn_to_locs_table(VnTables, Vn_to_locs_table) :-
	VnTables = vn_tables(_NextVn,
		_Lval_to_vn_table, _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		Vn_to_locs_table,  _Loc_to_vn_table).

vn_table__get_loc_to_vn_table(VnTables, Loc_to_vn_table) :-
	VnTables = vn_tables(_NextVn,
		_Lval_to_vn_table, _Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, Loc_to_vn_table).

vn_table__init_tables(VnTables) :-
	map__init(Lval_to_vn_table0),
	map__init(Rval_to_vn_table0),
	map__init(Vn_to_rval_table0),
	map__init(Vn_to_uses_table0),
	map__init(Vn_to_locs_table0),
	map__init(Loc_to_vn_table0),
	VnTables = vn_tables(0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn_table__lookup_desired_value(Vnlval, Vn, From, VnTables) :-
	vn_table__get_lval_to_vn_table(VnTables, Lval_to_vn_table),
	( map__search(Lval_to_vn_table, Vnlval, VnPrime) ->
		Vn = VnPrime
	;
		opt_debug__dump_vnlval(Vnlval, Value),
		string__append_list([From, ": cannot find desired value for ", Value], Error),
		error(Error)
	).

vn_table__lookup_assigned_vn(Vnrval, Vn, From, VnTables) :-
	vn_table__get_rval_to_vn_table(VnTables, Rval_to_vn_table),
	( map__search(Rval_to_vn_table, Vnrval, VnPrime) ->
		Vn = VnPrime
	;
		opt_debug__dump_vnrval(Vnrval, Value),
		string__append_list([From, ": cannot find assigned vn for ", Value], Error),
		error(Error)
	).

vn_table__lookup_defn(Vn, Vnrval, From, VnTables) :-
	vn_table__get_vn_to_rval_table(VnTables, Vn_to_rval_table),
	( map__search(Vn_to_rval_table, Vn, VnrvalPrime) ->
		Vnrval = VnrvalPrime
	;
		opt_debug__dump_vn(Vn, Value),
		string__append_list([From, ": cannot find definition for ", Value], Error),
		error(Error)
	).

vn_table__lookup_uses(Vn, Uses, From, VnTables) :-
	vn_table__get_vn_to_uses_table(VnTables, Vn_to_uses_table),
	( map__search(Vn_to_uses_table, Vn, UsesPrime) ->
		Uses = UsesPrime
	;
		opt_debug__dump_vn(Vn, Value),
		string__append_list([From, ": cannot find uses for ", Value], Error),
		error(Error)
	).

vn_table__lookup_current_locs(Vn, Locs, From, VnTables) :-
	vn_table__get_vn_to_locs_table(VnTables, Vn_to_locs_table),
	( map__search(Vn_to_locs_table, Vn, LocsPrime) ->
		Locs = LocsPrime
	;
		opt_debug__dump_vn(Vn, Value),
		string__append_list([From, ": cannot find current locs for ", Value], Error),
		error(Error)
	).

vn_table__lookup_current_value(Vnlval, Vn, From, VnTables) :-
	vn_table__get_loc_to_vn_table(VnTables, Loc_to_vn_table),
	( map__search(Loc_to_vn_table, Vnlval, VnPrime) ->
		Vn = VnPrime
	;
		opt_debug__dump_vnlval(Vnlval, Value),
		string__append_list([From, ": cannot find current value for ", Value], Error),
		error(Error)
	).

%-----------------------------------------------------------------------------%

vn_table__search_desired_value(Vnlval, Vn, VnTables) :-
	vn_table__get_lval_to_vn_table(VnTables, Lval_to_vn_table),
	map__search(Lval_to_vn_table, Vnlval, Vn).

vn_table__search_assigned_vn(Vnrval, Vn, VnTables) :-
	vn_table__get_rval_to_vn_table(VnTables, Rval_to_vn_table),
	map__search(Rval_to_vn_table, Vnrval, Vn).

vn_table__search_defn(Vn, Vnrval, VnTables) :-
	vn_table__get_vn_to_rval_table(VnTables, Vn_to_rval_table),
	map__search(Vn_to_rval_table, Vn, Vnrval).

vn_table__search_uses(Vn, Uses, VnTables) :-
	vn_table__get_vn_to_uses_table(VnTables, Vn_to_uses_table),
	map__search(Vn_to_uses_table, Vn, Uses).

vn_table__search_current_locs(Vn, Locs, VnTables) :-
	vn_table__get_vn_to_locs_table(VnTables, Vn_to_locs_table),
	map__search(Vn_to_locs_table, Vn, Locs).

vn_table__search_current_value(Vnlval, Vn, VnTables) :-
	vn_table__get_loc_to_vn_table(VnTables, Loc_to_vn_table),
	map__search(Loc_to_vn_table, Vnlval, Vn).

vn_table__get_vnlval_vn_list(VnTables, Lval_vn_list) :-
	vn_table__get_lval_to_vn_table(VnTables, Lval_to_vn_table),
	map__to_assoc_list(Lval_to_vn_table, Lval_vn_list).

%-----------------------------------------------------------------------------%

vn_table__add_new_use(Vn, NewUse, VnTables0, VnTables) :-
	VnTables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	( map__search(Vn_to_uses_table0, Vn, Uses0Prime) ->
		Uses0 = Uses0Prime
	;
		error("cannot find old use set in add_new_use")
	),
	(
		list__member(NewUse, Uses0),
		\+ (
			NewUse = src_access(_)
		;
			NewUse = src_vn(UserVn),
			map__lookup(Vn_to_rval_table0, UserVn, VnRval),
			VnRval = vn_binop(Operator, _, _),
			( Operator = (+)
			; Operator = (*)
			; Operator = float_plus
			; Operator = float_times
			; Operator = (<<)
			; Operator = (>>)
			)
		)
	->
		opt_debug__dump_tables(VnTables0, T_str),
		opt_debug__dump_vn(Vn, V_str),
		opt_debug__dump_use(NewUse, U_str),
		string__append_list(["\n", T_str, "\n",
			"new use for vn ", V_str, " = ", U_str, "\n",
			"new use already known"], Msg),
		error(Msg)
	;
		Uses1 = [NewUse | Uses0]
	),
	map__det_update(Vn_to_uses_table0, Vn, Uses1, Vn_to_uses_table1),
	VnTables = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn_table__del_old_use(Vn, OldUse, VnTables0, VnTables) :-
	VnTables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	( map__search(Vn_to_uses_table0, Vn, Uses0Prime) ->
		Uses0 = Uses0Prime
	;
		error("cannot find old use set in add_new_use")
	),
	% a given src_access may appear several times, we delete just one
	( list__delete_first(Uses0, OldUse, Uses1Prime) ->
		Uses1 = Uses1Prime
	;
		% OldUse may not be in Uses0; it may have been deleted earlier
		% (if constant) or it may be an artificially introduced
		% src_liveval for a shared vn
		Uses1 = Uses0
	),
	map__det_update(Vn_to_uses_table0, Vn, Uses1, Vn_to_uses_table1),
	VnTables = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table1,
		Vn_to_locs_table0, Loc_to_vn_table0).

vn_table__del_old_uses([], _OldUse, VnTables, VnTables).
vn_table__del_old_uses([Vn | Vns], OldUse, VnTables0, VnTables) :-
	vn_table__del_old_use(Vn, OldUse, VnTables0, VnTables1),
	vn_table__del_old_uses(Vns, OldUse, VnTables1, VnTables).

vn_table__record_first_vnrval(Vnrval, Vn, VnTables0, VnTables) :-
	VnTables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	Vn = NextVn0,
	NextVn1 is NextVn0 + 1,
	map__det_insert(Rval_to_vn_table0, Vnrval, Vn, Rval_to_vn_table1),
	map__det_insert(Vn_to_rval_table0, Vn, Vnrval, Vn_to_rval_table1),
	map__det_insert(Vn_to_uses_table0, Vn, [], Vn_to_uses_table1),
	map__det_insert(Vn_to_locs_table0, Vn, [], Vn_to_locs_table1),
	VnTables = vn_tables(NextVn1,
		Lval_to_vn_table0, Rval_to_vn_table1,
		Vn_to_rval_table1, Vn_to_uses_table1,
		Vn_to_locs_table1, Loc_to_vn_table0).

vn_table__record_new_vnrval(Vnrval, Vn, VnTables0, VnTables) :-
	VnTables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	Vn = NextVn0,
	NextVn1 is NextVn0 + 1,
	( map__insert(Rval_to_vn_table0, Vnrval, Vn, NewRval_to_vn_table) ->
		Rval_to_vn_table1 = NewRval_to_vn_table
	;
		Rval_to_vn_table1 = Rval_to_vn_table0
	),
	map__det_insert(Vn_to_rval_table0, Vn, Vnrval, Vn_to_rval_table1),
	map__det_insert(Vn_to_uses_table0, Vn, [], Vn_to_uses_table1),
	map__det_insert(Vn_to_locs_table0, Vn, [], Vn_to_locs_table1),
	VnTables = vn_tables(NextVn1,
		Lval_to_vn_table0, Rval_to_vn_table1,
		Vn_to_rval_table1, Vn_to_uses_table1,
		Vn_to_locs_table1, Loc_to_vn_table0).

vn_table__record_first_vnlval(Vnlval, Vn, VnTables0, VnTables) :-
	VnTables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	Vn = NextVn0,
	NextVn is NextVn0 + 1,
	map__det_insert(Lval_to_vn_table0, Vnlval, Vn, Lval_to_vn_table1),
	map__det_insert(Rval_to_vn_table0, vn_origlval(Vnlval), Vn,
		Rval_to_vn_table1),
	map__det_insert(Vn_to_rval_table0, Vn, vn_origlval(Vnlval),
		Vn_to_rval_table1),
	map__det_insert(Vn_to_uses_table0, Vn, [], Vn_to_uses_table1),
	map__det_insert(Vn_to_locs_table0, Vn, [Vnlval], Vn_to_locs_table1),
	map__det_insert(Loc_to_vn_table0,  Vnlval, Vn, Loc_to_vn_table1),
	VnTables = vn_tables(NextVn,
		Lval_to_vn_table1, Rval_to_vn_table1,
		Vn_to_rval_table1, Vn_to_uses_table1,
		Vn_to_locs_table1, Loc_to_vn_table1).

vn_table__set_desired_value(Vnlval, Vn, VnTables0, VnTables) :-
	VnTables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	( map__search(Loc_to_vn_table0, Vnlval, _) ->
		map__det_update(Lval_to_vn_table0, Vnlval, Vn,
			Lval_to_vn_table1),
		VnTables = vn_tables(NextVn0,
			Lval_to_vn_table1, Rval_to_vn_table0,
			Vn_to_rval_table0, Vn_to_uses_table0,
			Vn_to_locs_table0, Loc_to_vn_table0)
	;
		vn_table__record_first_vnlval(Vnlval, _, VnTables0, VnTables1),
		VnTables1 = vn_tables(NextVn1,
			Lval_to_vn_table1, Rval_to_vn_table1,
			Vn_to_rval_table1, Vn_to_uses_table1,
			Vn_to_locs_table1, Loc_to_vn_table1),
		map__set(Lval_to_vn_table1, Vnlval, Vn, Lval_to_vn_table2),
		VnTables = vn_tables(NextVn1,
			Lval_to_vn_table2, Rval_to_vn_table1,
			Vn_to_rval_table1, Vn_to_uses_table1,
			Vn_to_locs_table1, Loc_to_vn_table1)
	).

	% We put the newly generated vnlval at the end of the list of vnlvals
	% containing this vn. This is because if there are several registers
	% holding a vn, we pick the one at the front, and on superscalar
	% machines it is better not to use a register immediately after
	% its definition.

vn_table__set_current_value(Vnlval, Vn, VnTables0, VnTables) :-
	VnTables0 = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table0, Loc_to_vn_table0),
	( map__search(Loc_to_vn_table0, Vnlval, OldVn) ->

		% change the forward mapping
		map__det_update(Loc_to_vn_table0, Vnlval, Vn, Loc_to_vn_table1),

		% change the reverse mapping, first for old vn, then the new
		map__lookup(Vn_to_locs_table0, OldVn, OldLocs0),
		list__delete_all(OldLocs0, Vnlval, OldLocs1),
		map__det_update(Vn_to_locs_table0, OldVn, OldLocs1,
			Vn_to_locs_table1),

		map__lookup(Vn_to_locs_table1, Vn, NewLocs0),
		list__append(NewLocs0, [Vnlval], NewLocs1),
		map__det_update(Vn_to_locs_table1, Vn, NewLocs1,
			Vn_to_locs_table2)
	;
		% The search in the condition can fail for newly introduced
		% templocs

		% change the forward mapping
		map__det_insert(Loc_to_vn_table0, Vnlval, Vn, Loc_to_vn_table1),

		% change the reverse mapping
		map__lookup(Vn_to_locs_table0, Vn, NewLocs0),
		list__append(NewLocs0, [Vnlval], NewLocs1),
		map__det_update(Vn_to_locs_table0, Vn, NewLocs1,
			Vn_to_locs_table2)
	),
	VnTables = vn_tables(NextVn0,
		Lval_to_vn_table0, Rval_to_vn_table0,
		Vn_to_rval_table0, Vn_to_uses_table0,
		Vn_to_locs_table2, Loc_to_vn_table1).

vn_table__set_parallel_value(Vnlval, Vn, VnTables0, VnTables) :-
	vn_table__set_desired_value(Vnlval, Vn, VnTables0, VnTables1),
	vn_table__set_current_value(Vnlval, Vn, VnTables1, VnTables).

vn_table__get_all_vnrvals(Vnrvals, VnTables) :-
	VnTables = vn_tables(_NextVn,
		_Lval_to_vn_table,  Rval_to_vn_table,
		_Vn_to_rval_table, _Vn_to_uses_table,
		_Vn_to_locs_table, _Loc_to_vn_table),
	map__keys(Rval_to_vn_table, Vnrvals).
