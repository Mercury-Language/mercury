%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: code_exprn.m
% main author: conway.
%
% This module defines a series of predicates that operate on the
% abstract 'exprn_info' structure which maintains information about
% the contents of registers, and manages the cached expressions for
% variables. These predicates are:
%
%	code_exprn__init_state(Arguments, Varset, ExprnInfo)
%		which produces an initial state of the ExprnInfo given
%		an association list of variables and lvalues. The initial
%		state places the given variables at their corresponding
%		locations. The Varset parameter contains a mapping from
%		variables to names which is used when code is generated
%		to provide meaningful comments.
%
%	code_exprn__clobber_regs(CriticalVars, ExprnInfo0, ExprnInfo)
%		which modifies the state ExprnInfo0 to produce ExprnInfo
%		in which all variables stored in registers are clobbered.
%		If any variables in CriticalVars are stored only in
%		registers, and are not stored on the stack, then this
%		predicate will abort.
%
%	code_exprn__set_var_location(Var, Lval, ExprnInfo0, ExprnInfo)
%		which modifies ExprnInfo0 to produce ExprnInfo in which
%		Var is *magically* stored in Lval.
%
%	code_exprn__var_becomes_dead(Var, ExprnInfo0, ExprnInfo)
%		which frees any code generator resources used by Var
%		in ExprnInfo0 to produce ExprnInfo (in the implementation,
%		any cached expressions which still need those resources
%		will inherit them appropriately).
%
%	code_exprn__cache_exprn(Var, Rval, ExprnInfo0, ExprnInfo)
%		which produces a modified ExprnInfo0, ExprnInfo
%		which indicates that when a value of Var is needed,
%		code to evaluate Rval should be produced.
%
%	code_exprn__place_var(Var, Lval, Code, ExprnInfo0, ExprnInfo)
%		which produces Code and a modified version of ExprnInfo0,
%		ExprnInfo which places the value of Var in Lval.
%
%	code_exprn__produce_var(Var, Rval, Code, ExprnInfo0, ExprnInfo)
%		which produces a code fragment Code to evaluate Var and
%		provide it as Rval (which may be a const, etc, or an lval).
%
%	code_exprn__acquire_reg(Reg, ExprnInfo0, ExprnInfo)
%		which finds an unused register and marks it as 'in use'.
%		
%	code_exprn__release_reg(Reg, ExprnInfo, ExprnInfo)
%		which marks a previously acquired reg and releases it so
%		that it can be reused.
%
%	code_exprn__lock_reg(Reg, ExprnInfo, ExprnInfo)
%		which prevents a register from being reused, even if
%		there are no variables refering to it.
%
%	code_exprn__unlock_reg(Reg, ExprnInfo0, ExprnInfo)
%		which undoes the previous operation.
%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module code_exprn.

:- interface.

:- import_module llds, list, varset, std_util, assoc_list, tree, options.

:- type exprn_info.

:- pred code_exprn__init_state(assoc_list(var, rval), varset, option_table, exprn_info).
:- mode code_exprn__init_state(in, in, in, out) is det.

:- pred code_exprn__clobber_regs(list(var), exprn_info, exprn_info).
:- mode code_exprn__clobber_regs(in, in, out) is det.

:- pred code_exprn__set_var_location(var, lval, exprn_info, exprn_info).
:- mode code_exprn__set_var_location(in, in, in, out) is det.

:- pred code_exprn__maybe_set_var_location(var, lval, exprn_info, exprn_info).
:- mode code_exprn__maybe_set_var_location(in, in, in, out) is det.

:- pred code_exprn__var_becomes_dead(var, exprn_info, exprn_info).
:- mode code_exprn__var_becomes_dead(in, in, out) is det.

:- pred code_exprn__cache_exprn(var, rval, exprn_info, exprn_info).
:- mode code_exprn__cache_exprn(in, in, in, out) is det.

:- pred code_exprn__place_var(var, lval, code_tree, exprn_info, exprn_info).
:- mode code_exprn__place_var(in, in, out, in, out) is det.

:- pred code_exprn__produce_var(var, rval, code_tree, exprn_info, exprn_info).
:- mode code_exprn__produce_var(in, out, out, in, out) is det.

:- pred code_exprn__produce_var_in_reg(var, rval, code_tree,
						exprn_info, exprn_info).
:- mode code_exprn__produce_var_in_reg(in, out, out, in, out) is det.

:- pred code_exprn__acquire_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__acquire_reg(out, in, out) is det.

:- pred code_exprn__release_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__release_reg(in, in, out) is det.

:- pred code_exprn__lock_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__lock_reg(in, in, out) is det.

:- pred code_exprn__unlock_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__unlock_reg(in, in, out) is det.

:- pred code_exprn__get_varlocs(map(var, set(rval)), exprn_info, exprn_info).
:- mode code_exprn__get_varlocs(out, in, out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module exprn_aux.
:- import_module bool, map, bag, set, require, int, term, string, getopt.

:- type var_stat	--->
		evaled(set(rval))
	;	cached(rval).

:- type var_map	==	map(var, var_stat).

:- type exprn_info	--->
		exprn_info(
			varset,		% all the variables and their names
			var_map,	% what each variable stands for
			bag(reg),	% the 'in use' markers for regs
			set(reg),	% extra markers for acquired regs
			option_table
		).

%------------------------------------------------------------------------------%

code_exprn__init_state(Initializations, Varset, Opts, ExprnInfo) :-
	map__init(Vars0),
	bag__init(Regs0),
	code_exprn__init_state_2(Initializations, Vars0, Vars, Regs0, Regs),
	set__init(Acqu),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opts).

:- pred code_exprn__init_state_2(assoc_list(var, rval), var_map, var_map,
							bag(reg), bag(reg)).
:- mode code_exprn__init_state_2(in, in, out, in, out) is det.

code_exprn__init_state_2([], Vars, Vars, Regs, Regs).
code_exprn__init_state_2([V - L|Rest], Vars0, Vars, Regs0, Regs) :-
	(
		map__search(Vars0, V, evaled(Vals0))
	->
		set__insert(Vals0, L, Vals)
	;
		set__singleton_set(Vals, L)
	),
	map__set(Vars0, V, evaled(Vals), Vars1),
	(
		L = lval(reg(R))
	->
		bag__insert(Regs0, R, Regs1)
	;
		Regs1 = Regs0
	),
	code_exprn__init_state_2(Rest, Vars1, Vars, Regs1, Regs).

%------------------------------------------------------------------------------%

code_exprn__get_varlocs(Locations) -->
	code_exprn__get_vars(Vars),
	{ map__to_assoc_list(Vars, VarList) },
	{ map__init(Locations0) },
	{ code_exprn__repackage_locations(VarList, Locations0, Locations) }.

:- pred code_exprn__repackage_locations(assoc_list(var, var_stat),
			map(var, set(rval)), map(var, set(rval))).
:- mode code_exprn__repackage_locations(in, in, out) is det.

code_exprn__repackage_locations([], Loc, Loc).
code_exprn__repackage_locations([V - Locs|Rest], Loc0, Loc) :-
	(
		Locs = cached(Rval),
		set__singleton_set(Rvals, Rval)
	;
		Locs = evaled(Rvals)
	),
	map__set(Loc0, V, Rvals, Loc1),
	code_exprn__repackage_locations(Rest, Loc1, Loc).

%------------------------------------------------------------------------------%

code_exprn__clobber_regs(CriticalVars) -->
	code_exprn__get_vars(Vars0),
	{ map__to_assoc_list(Vars0, VarsList) },
	{ map__init(Vars1) },
	{ code_exprn__clobber_regs_2(VarsList, CriticalVars, Vars0,
							Vars1, Vars) },
	code_exprn__set_vars(Vars),
	{ bag__init(Regs) },
	code_exprn__set_regs(Regs),
	{ set__init(Acqu) },
	code_exprn__set_acquired(Acqu).

:- pred code_exprn__clobber_regs_2(assoc_list(var, var_stat), list(var),
						var_map, var_map, var_map).
:- mode code_exprn__clobber_regs_2(in, in, in, in, out) is det.

code_exprn__clobber_regs_2([], _Critical, _OldVars, Vars, Vars).
code_exprn__clobber_regs_2([V - Stat|Rest], Critical, OldVars, Vars0, Vars) :-
	(
		Stat = cached(Exprn),
		(
			code_exprn__rval_depends_on_reg(Exprn, OldVars)
		->
			(
				list__member(V, Critical)
			->
				error("code_exprn__clobber_regs: attempt to clobber critical register")
			;
				true
			),
			Vars1 = Vars0
		;
			map__set(Vars0, V, Stat, Vars1)
		)
	;
		Stat = evaled(Rvals0),
		code_exprn__filter_out_reg_depending(Rvals0, OldVars, Rvals),
		(
			set__empty(Rvals)
		->
			(
				list__member(V, Critical)
			->
				error("code_exprn__clobber_regs: attempt to clobber critical register")
			;
				true
			),
			Vars1 = Vars0
		;
			map__set(Vars0, V, evaled(Rvals), Vars1)
		)
	),
	code_exprn__clobber_regs_2(Rest, Critical, OldVars, Vars1, Vars).

%------------------------------------------------------------------------------%

:- pred code_exprn__rval_depends_on_reg(rval, var_map).
:- mode code_exprn__rval_depends_on_reg(in, in) is semidet.

code_exprn__rval_depends_on_reg(lval(Lval), Vars) :-
	code_exprn__lval_depends_on_reg(Lval, Vars).
code_exprn__rval_depends_on_reg(var(Var), Vars) :-
	map__lookup(Vars, Var, Stat),
	(
		Stat = cached(Rval),
		code_exprn__rval_depends_on_reg(Rval, Vars)
	;
		Stat = evaled(Rvals0),
		code_exprn__filter_out_reg_depending(Rvals0, Vars, Rvals),
		set__empty(Rvals)
	).
code_exprn__rval_depends_on_reg(create(_Tag, Rvals, _LabNum), Vars) :-
	code_exprn__args_depend_on_reg(Rvals, Vars).
code_exprn__rval_depends_on_reg(mkword(_Tag, Rval), Vars) :-
	code_exprn__rval_depends_on_reg(Rval, Vars).
code_exprn__rval_depends_on_reg(const(_Const), _Vars) :-
	fail.
code_exprn__rval_depends_on_reg(unop(_Op, Rval), Vars) :-
	code_exprn__rval_depends_on_reg(Rval, Vars).
code_exprn__rval_depends_on_reg(binop(_Op, Rval0, Rval1), Vars) :-
	(
		code_exprn__rval_depends_on_reg(Rval0, Vars)
	;
		code_exprn__rval_depends_on_reg(Rval1, Vars)
	).

:- pred code_exprn__lval_depends_on_reg(lval, var_map).
:- mode code_exprn__lval_depends_on_reg(in, in) is semidet.

code_exprn__lval_depends_on_reg(reg(_), _Vars) :-
	true.
code_exprn__lval_depends_on_reg(lvar(Var), Vars) :-
	map__lookup(Vars, Var, Stat),
	(
		Stat = cached(Rval),
		code_exprn__rval_depends_on_reg(Rval, Vars)
	;
		Stat = evaled(Rvals0),
		code_exprn__filter_out_reg_depending(Rvals0, Vars, Rvals),
		set__empty(Rvals)
	).
code_exprn__lval_depends_on_reg(field(_Tag, Rval0, Rval1), Vars) :-
	(
		code_exprn__rval_depends_on_reg(Rval0, Vars)
	;
		code_exprn__rval_depends_on_reg(Rval1, Vars)
	).

:- pred code_exprn__args_depend_on_reg(list(maybe(rval)), var_map).
:- mode code_exprn__args_depend_on_reg(in, in) is semidet.

code_exprn__args_depend_on_reg([], _Vars) :-
	fail.
code_exprn__args_depend_on_reg([Arg|Args], Vars) :-
	(
		Arg = yes(Rval),
		code_exprn__rval_depends_on_reg(Rval, Vars)
	->
		true
	;
		code_exprn__args_depend_on_reg(Args, Vars)
	).

:- pred code_exprn__filter_out_reg_depending(set(rval), var_map, set(rval)).
:- mode code_exprn__filter_out_reg_depending(in, in, out) is det.

code_exprn__filter_out_reg_depending(Rvals0, Vars, Rvals) :-
	set__to_sorted_list(Rvals0, RvalList0),
	code_exprn__filter_out_reg_depending_2(RvalList0, Vars, RvalList),
	set__sorted_list_to_set(RvalList, Rvals).

:- pred code_exprn__filter_out_reg_depending_2(list(rval), var_map, list(rval)).
:- mode code_exprn__filter_out_reg_depending_2(in, in, out) is det.

code_exprn__filter_out_reg_depending_2([], _Vars, []).
code_exprn__filter_out_reg_depending_2([Rval0|Rvals0], Vars, Rvals) :-
	code_exprn__filter_out_reg_depending_2(Rvals0, Vars, Rvals1),
	(
		code_exprn__rval_depends_on_reg(Rval0, Vars)
	->
		Rvals = Rvals1
	;
		Rvals = [Rval0|Rvals1]
	).

%------------------------------------------------------------------------------%

code_exprn__set_var_location(Var, Lval) -->
	code_exprn__get_vars(Vars0),
	(
		{ code_exprn__lval_in_use(Lval, Vars0) }
	->
		{ error("code_exprn__set_var_location: location already in use") }
	;
		{ set__singleton_set(Locs, lval(Lval)) },
		{ map__set(Vars0, Var, evaled(Locs), Vars) },
		code_exprn__set_vars(Vars),
		code_exprn__add_lval_reg_dependencies(Lval)
	).

%------------------------------------------------------------------------------%

code_exprn__maybe_set_var_location(Var, Lval) -->
	code_exprn__get_vars(Vars0),
	{ set__singleton_set(Locs, lval(Lval)) },
	{ map__set(Vars0, Var, evaled(Locs), Vars) },
	code_exprn__set_vars(Vars),
	code_exprn__add_lval_reg_dependencies(Lval).

%------------------------------------------------------------------------------%

:- pred code_exprn__lval_in_use(lval, var_map).
:- mode code_exprn__lval_in_use(in, in) is semidet.

code_exprn__lval_in_use(Lval, Vars) :-
	map__values(Vars, StatList),
	list__member(Stat, StatList),
	(
		Stat = cached(Rval0),
		exprn_aux__rval_contains_lval(Rval0, Lval)
	;
		Stat = evaled(Rvals),
		set__member(Rval1, Rvals),
		exprn_aux__rval_contains_lval(Rval1, Lval)
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__add_lval_reg_dependencies(lval, exprn_info, exprn_info).
:- mode code_exprn__add_lval_reg_dependencies(in, in, out) is det.

code_exprn__add_lval_reg_dependencies(Lval) -->
	(
		{ Lval = reg(Reg) }
	->
		code_exprn__get_regs(Regs0),
		{ bag__insert(Regs0, Reg, Regs) },
		code_exprn__set_regs(Regs)
	;
		{ Lval = field(_Tag, Rval0, Rval1) }
	->
		code_exprn__add_rval_reg_dependencies(Rval0),
		code_exprn__add_rval_reg_dependencies(Rval1)
	;
		[]
	).

:- pred code_exprn__add_rval_list_reg_dependencies(list(rval),
						exprn_info, exprn_info).
:- mode code_exprn__add_rval_list_reg_dependencies(in, in, out) is det.

code_exprn__add_rval_list_reg_dependencies([]) --> [].
code_exprn__add_rval_list_reg_dependencies([R|Rs]) -->
	code_exprn__add_rval_reg_dependencies(R),
	code_exprn__add_rval_list_reg_dependencies(Rs).

:- pred code_exprn__add_rval_reg_dependencies(rval, exprn_info, exprn_info).
:- mode code_exprn__add_rval_reg_dependencies(in, in, out) is det.

code_exprn__add_rval_reg_dependencies(lval(Lval)) -->
	code_exprn__add_lval_reg_dependencies(Lval).
code_exprn__add_rval_reg_dependencies(var(_Var)) --> [].
code_exprn__add_rval_reg_dependencies(create(_, Rvals, _)) -->
	code_exprn__add_arg_reg_dependencies(Rvals).
code_exprn__add_rval_reg_dependencies(mkword(_Tag, Rval)) -->
	code_exprn__add_rval_reg_dependencies(Rval).
code_exprn__add_rval_reg_dependencies(const(_Const)) --> [].
code_exprn__add_rval_reg_dependencies(unop(_Op, Rval)) -->
	code_exprn__add_rval_reg_dependencies(Rval).
code_exprn__add_rval_reg_dependencies(binop(_Op, Rval0, Rval1)) -->
	code_exprn__add_rval_reg_dependencies(Rval0),
	code_exprn__add_rval_reg_dependencies(Rval1).

:- pred code_exprn__add_arg_reg_dependencies(list(maybe(rval)),
						exprn_info, exprn_info).
:- mode code_exprn__add_arg_reg_dependencies(in, in, out) is det.

code_exprn__add_arg_reg_dependencies([]) --> [].
code_exprn__add_arg_reg_dependencies([M|Ms]) -->
	(
		{ M = yes(Rval) }
	->
		code_exprn__add_rval_reg_dependencies(Rval)
	;
		[]
	),
	code_exprn__add_arg_reg_dependencies(Ms).

%------------------------------------------------------------------------------%

:- pred code_exprn__rem_lval_reg_dependencies(lval, exprn_info, exprn_info).
:- mode code_exprn__rem_lval_reg_dependencies(in, in, out) is det.

code_exprn__rem_lval_reg_dependencies(Lval) -->
	(
		{ Lval = reg(Reg) }
	->
		code_exprn__get_regs(Regs0),
		{ bag__remove(Regs0, Reg, Regs) },
		code_exprn__set_regs(Regs)
	;
		{ Lval = field(_Tag, Rval0, Rval1) }
	->
		code_exprn__rem_rval_reg_dependencies(Rval0),
		code_exprn__rem_rval_reg_dependencies(Rval1)
	;
		[]
	).

:- pred code_exprn__rem_rval_list_reg_dependencies(list(rval),
						exprn_info, exprn_info).
:- mode code_exprn__rem_rval_list_reg_dependencies(in, in, out) is det.

code_exprn__rem_rval_list_reg_dependencies([]) --> [].
code_exprn__rem_rval_list_reg_dependencies([R|Rs]) -->
	code_exprn__rem_rval_reg_dependencies(R),
	code_exprn__rem_rval_list_reg_dependencies(Rs).

:- pred code_exprn__rem_rval_reg_dependencies(rval, exprn_info, exprn_info).
:- mode code_exprn__rem_rval_reg_dependencies(in, in, out) is det.

code_exprn__rem_rval_reg_dependencies(lval(Lval)) -->
	code_exprn__rem_lval_reg_dependencies(Lval).
code_exprn__rem_rval_reg_dependencies(var(_Var)) --> [].
code_exprn__rem_rval_reg_dependencies(create(_, Rvals, _)) -->
	code_exprn__rem_arg_reg_dependencies(Rvals).
code_exprn__rem_rval_reg_dependencies(mkword(_Tag, Rval)) -->
	code_exprn__rem_rval_reg_dependencies(Rval).
code_exprn__rem_rval_reg_dependencies(const(_Const)) --> [].
code_exprn__rem_rval_reg_dependencies(unop(_Op, Rval)) -->
	code_exprn__rem_rval_reg_dependencies(Rval).
code_exprn__rem_rval_reg_dependencies(binop(_Op, Rval0, Rval1)) -->
	code_exprn__rem_rval_reg_dependencies(Rval0),
	code_exprn__rem_rval_reg_dependencies(Rval1).

:- pred code_exprn__rem_arg_reg_dependencies(list(maybe(rval)),
						exprn_info, exprn_info).
:- mode code_exprn__rem_arg_reg_dependencies(in, in, out) is det.

code_exprn__rem_arg_reg_dependencies([]) --> [].
code_exprn__rem_arg_reg_dependencies([M|Ms]) -->
	(
		{ M = yes(Rval) }
	->
		code_exprn__rem_rval_reg_dependencies(Rval)
	;
		[]
	),
	code_exprn__rem_arg_reg_dependencies(Ms).

%------------------------------------------------------------------------------%

code_exprn__var_becomes_dead(Var) -->
	code_exprn__get_vars(Vars0),
	code_exprn__get_options(Options),
	{ getopt__lookup_bool_option(Options, gcc_non_local_gotos, NLG) },
	{ getopt__lookup_bool_option(Options, asm_labels, ASM) },
	{ getopt__lookup_bool_option(Options, static_ground_terms, SGT) },
	(
		{ map__search(Vars0, Var, Stat) }
	->
		(
			{ Stat = cached(Rval0) },
			code_exprn__rem_rval_reg_dependencies(Rval0)
		;
			{ Stat = evaled(Rvals0) },
			{ set__to_sorted_list(Rvals0, RvalList0) },
			code_exprn__rem_rval_list_reg_dependencies(RvalList0),
			(
				{ code_exprn__member_expr_is_constant(RvalList0,
						Vars0,  NLG, ASM, SGT, Rval7) }
			->
				{ Rval0 = Rval7 }
			;
				{ code_exprn__select_rval(RvalList0, Rval0) }
			)
		),
		{ map__delete(Vars0, Var, Vars1) },
		code_exprn__set_vars(Vars1),
		code_exprn__update_dependent_vars(Var, Rval0)
	;
		% When we make the code generator tighter we can
		% reinstate this sanity check. In particular,
		% code_info needs to know which args (etc) have
		% been explicitly killed off during the generation
		% of the goal.
		% { error("code_exprn__var_becomes_dead: var not found!") }
		[]
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__update_dependent_vars(var, rval, exprn_info, exprn_info).
:- mode code_exprn__update_dependent_vars(in, in, in, out) is det.

code_exprn__update_dependent_vars(Var, Rval) -->
	code_exprn__get_vars(Vars0),
	{ map__to_assoc_list(Vars0, VarList0) },
	code_exprn__update_dependent_vars_2(VarList0, Var, Rval, VarList),
	{ map__from_assoc_list(VarList, Vars) },
	code_exprn__set_vars(Vars).

:- pred code_exprn__update_dependent_vars_2(assoc_list(var, var_stat),
		var, rval, assoc_list(var, var_stat), exprn_info, exprn_info).
:- mode code_exprn__update_dependent_vars_2(in, in, in, out, in, out) is det.

code_exprn__update_dependent_vars_2([], _Var, _Rval, []) --> [].
code_exprn__update_dependent_vars_2([V - Stat0|Rest0], Var, Rval,
							[V - Stat|Rest]) -->
	(
		{ Stat0 = cached(Exprn0) },
		{ exprn_aux__rval_contains_rval(Exprn0, var(Var)) }
	->
		code_exprn__add_rval_reg_dependencies(Rval),
		{ exprn_aux__substitute_rval_in_rval(var(Var), Rval,
							Exprn0, Exprn1) },
		{ exprn_aux__simplify_rval(Exprn1, Exprn) },
		(
			{ exprn_aux__vars_in_rval(Exprn, []) }
		->
			{ set__singleton_set(Rvals, Exprn) },
			{ Stat = evaled(Rvals) }
		;
			{ Stat = cached(Exprn) }
		)
	;
		{ Stat0 = evaled(Rvals0) }
	->
		{ set__to_sorted_list(Rvals0, RvalList0) },
		code_exprn__update_dependent_vars_3(RvalList0, Var, Rval,
					RvalList),
		{ set__sorted_list_to_set(RvalList, Rvals) },
		{ Stat = evaled(Rvals) }
	;
		% Stat0 = cached(Exprn), \+ contains
		{ Stat = Stat0 }
	),
	code_exprn__update_dependent_vars_2(Rest0, Var, Rval, Rest).

:- pred code_exprn__update_dependent_vars_3(list(rval), var, rval, list(rval),
					exprn_info, exprn_info).
:- mode code_exprn__update_dependent_vars_3(in, in, in, out, in, out) is det.

code_exprn__update_dependent_vars_3([], _Var, _Rval, []) --> [].
code_exprn__update_dependent_vars_3([R0|Rs0], Var, Rval, [R|Rs]) -->
	(
		{ exprn_aux__rval_contains_rval(R0, var(Var)) }
	->
		{ exprn_aux__substitute_rval_in_rval(var(Var), Rval, R0, R1) },
		{ exprn_aux__simplify_rval(R1, R) },
		code_exprn__rem_rval_reg_dependencies(R0),
		code_exprn__add_rval_reg_dependencies(R)
	;
		{ R = R0 }
	),
	code_exprn__update_dependent_vars_3(Rs0, Var, Rval, Rs).

%------------------------------------------------------------------------------%

:- pred code_exprn__select_rval(list(rval), rval).
:- mode code_exprn__select_rval(in, out) is det.

code_exprn__select_rval(Rs, Rval) :-
	(
		Rs = []
	->
		error("code_exprn__select_rval: no rvals")
	;
		code_exprn__select_reg(Rs, Rval0)
	->
		Rval = Rval0
	;
		code_exprn__select_simple_const(Rs, Rval1)
	->
		Rval = Rval1
	;
		code_exprn__select_stackvar(Rs, Rval2)
	->
		Rval = Rval2
	;
		Rs = [Rval3|_]
	->
		Rval = Rval3
	;
		error("code_exprn__select_rval: cosmic rays strike again")
	).

:- pred code_exprn__select_simple_const(list(rval), rval).
:- mode code_exprn__select_simple_const(in, out) is semidet.

code_exprn__select_simple_const([R|Rs], Rval) :-
	(
		R = const(_)
	->
		Rval = R
	;
		code_exprn__select_simple_const(Rs, Rval)
	).

:- pred code_exprn__select_stackvar(list(rval), rval).
:- mode code_exprn__select_stackvar(in, out) is semidet.

code_exprn__select_stackvar([R|Rs], Rval) :-
	(
		R = lval(C),
		(
			C = stackvar(_)
		;
			C = framevar(_)
		)
	->
		Rval = R
	;
		code_exprn__select_stackvar(Rs, Rval)
	).

:- pred code_exprn__select_reg(list(rval), rval).
:- mode code_exprn__select_reg(in, out) is semidet.

code_exprn__select_reg([R|Rs], Rval) :-
	(
		R = lval(reg(_))
	->
		Rval = R
	;
		code_exprn__select_reg(Rs, Rval)
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__expr_is_constant(rval, var_map, bool, bool, bool, rval).
:- mode code_exprn__expr_is_constant(in, in, in, in, in, out) is semidet.

code_exprn__expr_is_constant(const(Const), _Vars,
				NonLocalGotos, AsmLabels, _SGT, Rval) :-
	(
		Const = address_const(CodeAddress)
	->
		(
			CodeAddress = label(_)
		->
			true
		;
			CodeAddress = succip
		->
			fail
		;
			(
				NonLocalGotos = no
			;
				AsmLabels = yes
			)
		)
	;
		Const = float_const(_)
	->
		% Floating point constants are currently boxed by default;
		% the memory allocation means that they are not constant
		% expressions.
		fail
	;
		true
	),
	Rval = const(Const).

code_exprn__expr_is_constant(unop(Op, Expr0), Vars, NLG, ASM, SGT, unop(Op, Expr)) :-
	code_exprn__expr_is_constant(Expr0, Vars, NLG, ASM, SGT, Expr).

code_exprn__expr_is_constant(binop(Op, Expr1, Expr2), Vars,
					NLG, ASM, SGT, binop(Op, Expr3, Expr4)) :-
	code_exprn__expr_is_constant(Expr1, Vars, NLG, ASM, SGT, Expr3),
	code_exprn__expr_is_constant(Expr2, Vars, NLG, ASM, SGT, Expr4).

code_exprn__expr_is_constant(mkword(Tag, Expr0), Vars, NLG, ASM, SGT, mkword(Tag, Expr)) :-
	code_exprn__expr_is_constant(Expr0, Vars, NLG, ASM, SGT, Expr).

code_exprn__expr_is_constant(create(Tag, Args0, Label), Vars,
				NLG, ASM, SGT, create(Tag, Args, Label)) :-
	SGT = yes,
	code_exprn__args_are_constant(Args0, Vars, NLG, ASM, SGT, Args).

code_exprn__expr_is_constant(var(Var), Vars, NLG, ASM, SGT, Rval) :-
	map__search(Vars, Var, Stat),
	(
		Stat = cached(Rval0),
		code_exprn__expr_is_constant(Rval0, Vars, NLG, ASM, SGT, Rval)
	;
		Stat = evaled(Rvals),
		set__to_sorted_list(Rvals, RvalList),
		code_exprn__member_expr_is_constant(RvalList, Vars, NLG, ASM, SGT, Rval)
	).

:- pred code_exprn__args_are_constant(list(maybe(rval)), var_map,
					bool, bool, bool, list(maybe(rval))).
:- mode code_exprn__args_are_constant(in, in, in, in, in, out) is semidet.

code_exprn__args_are_constant([], _Vars, _NLG, _ASM, _SGT, []).
code_exprn__args_are_constant([Arg0 | Args0], Vars, NLG, ASM, SGT, [Arg | Args]) :-
	% if any of the fields are 'no' then we cannot treat the
	% term as a constant.
	Arg0 = yes(Rval0),
	code_exprn__expr_is_constant(Rval0, Vars, NLG, ASM, SGT, Rval),
	Arg = yes(Rval),
	code_exprn__args_are_constant(Args0, Vars, NLG, ASM, SGT, Args).

:- pred code_exprn__member_expr_is_constant(list(rval), var_map,
							bool, bool, bool, rval).
:- mode code_exprn__member_expr_is_constant(in, in, in, in, in, out) is semidet.

code_exprn__member_expr_is_constant([Rval0|Rvals0], Vars, NLG, ASM, SGT, Rval) :-
	(
		code_exprn__expr_is_constant(Rval0, Vars, NLG, ASM, SGT, Rval1)
	->
		Rval = Rval1
	;
		code_exprn__member_expr_is_constant(Rvals0, Vars, NLG, ASM, SGT, Rval)
	).

%------------------------------------------------------------------------------%

code_exprn__cache_exprn(Var, Rval) -->
	code_exprn__get_vars(Vars0),
	(
		{ map__search(Vars0, Var, _) }
	->
		{ error("code_exprn__cache_exprn: existing definition of var") }
	;
		[]
	),
	code_exprn__add_rval_reg_dependencies(Rval),
	(
		{ exprn_aux__vars_in_rval(Rval, []) }
	->
		{ set__singleton_set(Rvals, Rval) },
		{ map__set(Vars0, Var, evaled(Rvals), Vars) }
	;
		{ map__set(Vars0, Var, cached(Rval), Vars) }
	),
	code_exprn__set_vars(Vars).

%------------------------------------------------------------------------------%

code_exprn__place_var(Var, Lval, Code) -->
	code_exprn__get_vars(Vars0),
	(
		{ map__search(Vars0, Var, Stat0) }
	->
		{ Stat = Stat0 }
	;
		code_exprn__get_varset(VarSet),
		{ varset__lookup_name(VarSet, Var, Name) ->
			string__append("variable not found - ", Name, Msg),
			error(Msg)
		;
			error("variable not found")
		}
	),
	code_exprn__place_var_2(Stat, Var, Lval, Code).

:- pred code_exprn__place_var_2(var_stat, var, lval, code_tree,
						exprn_info, exprn_info).
:- mode code_exprn__place_var_2(in, in, in, out, in, out) is det.

code_exprn__place_var_2(cached(Exprn0), Var, Lval, Code) -->
	code_exprn__get_vars(Vars0),
	code_exprn__get_options(Options),
	{ getopt__lookup_bool_option(Options, gcc_non_local_gotos, NLG) },
	{ getopt__lookup_bool_option(Options, asm_labels, ASM) },
	{ getopt__lookup_bool_option(Options, static_ground_terms, SGT) },
	(
		{ exprn_aux__vars_in_rval(Exprn0, []) }
	->
		{ error("code_exprn__place_var: cached exprn with no vars!") }
	;
		{ code_exprn__expr_is_constant(Exprn0, Vars0,
						NLG, ASM, SGT, Exprn) }
	->
			% move stuff out of the way
			% We don't care about the renamed exprn
			% because it is a constant
		code_exprn__clear_lval(Lval, Exprn, _Exprn, ClearCode),
			% reserve the register
		code_exprn__add_lval_reg_dependencies(Lval),
		{ set__list_to_set([Exprn, lval(Lval)], Rvals) },
		{ Stat = evaled(Rvals) },
		code_exprn__get_var_name(Var, VarName),
		{ string__append("Assigning from ", VarName, Comment) },
		{ ExprnCode = node([
			assign(Lval, Exprn) - Comment
		]) }
	;
		% if the variable already has its value stored in the
		% right place, we don't need to generated any code
		{ Exprn0 = var(Var1) },
		code_exprn__get_vars(Vars0),
		{ map__search(Vars0, Var1, Stat0) },
		{ Stat0 = evaled(VarRvals) },
		{ set__member(lval(Lval), VarRvals) }
	->
		{ ClearCode = empty },
		{ ExprnCode = empty },
		code_exprn__add_lval_reg_dependencies(Lval),
		{ set__singleton_set(LvalSet, lval(Lval)) },
		{ Stat = evaled(LvalSet) }
	;
			% move stuff out of the way
		code_exprn__clear_lval(Lval, Exprn0, Exprn1, ClearCode),
			% reserve the register
		code_exprn__add_lval_reg_dependencies(Lval),
		{ exprn_aux__vars_in_rval(Exprn1, VarList) },
		code_exprn__produce_vars(VarList, VarLocList, Code0),
		code_exprn__rem_rval_reg_dependencies(Exprn1),
		{ exprn_aux__substitute_vars_in_rval(VarLocList,
					Exprn1, Exprn2) },
		(
			{ Exprn2 = create(_, _, _) }
		->
			% XXX why is this necessary?
			code_exprn__get_var_rvals(Var, Rvals7),
			{ set__to_sorted_list(Rvals7, RvalList7) },
			{ code_exprn__select_rval(RvalList7, Exprn3) },
			{ exprn_aux__substitute_vars_in_rval(VarLocList,
						Exprn3, Exprn) }
		;
			{ Exprn = Exprn2 }
		),
		code_exprn__add_rval_reg_dependencies(Exprn),
		{ set__list_to_set([Exprn, lval(Lval)], Rvals) },
		{ Stat = evaled(Rvals) },
		code_exprn__get_var_name(Var, VarName),
		{ code_exprn__construct_code(Lval, VarName, Exprn, Code1) },
		{ ExprnCode = tree(Code0, Code1) }
	),
	code_exprn__get_vars(Vars1),
	{ map__set(Vars1, Var, Stat, Vars) },
	code_exprn__set_vars(Vars),
	{ Code = tree(ClearCode, ExprnCode) }.

code_exprn__place_var_2(evaled(Rvals0), Var, Lval, Code) -->
	code_exprn__get_vars(Vars0),
	code_exprn__get_options(Options),
	{ getopt__lookup_bool_option(Options, gcc_non_local_gotos, NLG) },
	{ getopt__lookup_bool_option(Options, asm_labels, ASM) },
	{ getopt__lookup_bool_option(Options, static_ground_terms, SGT) },
	(
		{ set__member(lval(Lval), Rvals0) }
	->
		{ ClearCode = empty },
		{ ExprnCode = empty },
		{ Stat = evaled(Rvals0) }
	;
		{ set__to_sorted_list(Rvals0, RvalList) },
		{ code_exprn__select_rval(RvalList, Rval0) },
		{
			Rval0 = lval(reg(_))
		;
			Rval0 = lval(stackvar(_))
		;
			Rval0 = lval(framevar(_))
		}
	->
			% move stuff out of the way
		code_exprn__clear_lval(Lval, Rval0, Rval, ClearCode),
			% reserve the register
		code_exprn__add_lval_reg_dependencies(Lval),
		code_exprn__get_var_rvals(Var, Rvals1),
		{ set__insert(Rvals1, lval(Lval), Rvals) },
		{ Stat = evaled(Rvals) },
		code_exprn__get_var_name(Var, VarName),
		{ code_exprn__construct_code(Lval, VarName, Rval, ExprnCode) }
	;
		{set__to_sorted_list(Rvals0, RvalList0) },
		{ code_exprn__member_expr_is_constant(RvalList0,
				Vars0, NLG, ASM, SGT, Rval0) }
	->
			% move stuff out of the way
		code_exprn__clear_lval(Lval, Rval0, Rval1, ClearCode),
			% reserve the register
		code_exprn__add_lval_reg_dependencies(Lval),
		code_exprn__get_var_rvals(Var, Rvals1),
		{ set__insert(Rvals1, lval(Lval), Rvals) },
		{ Stat = evaled(Rvals) },
		code_exprn__get_var_name(Var, VarName),
		{ string__append("Assigning from ", VarName, Comment) },
		{ ExprnCode = node([
			assign(Lval, Rval1) - Comment
		]) }
	;
			% choose one of the rvals from
			% the old set, to avoid the
			% potential issue-slot conflict
			% unless the rval is a create,
			% which is non-atomic so we must
			% choose from the new set.
		{ set__to_sorted_list(Rvals0, RvalList) },
		{ code_exprn__select_rval(RvalList, Rval0) },
			% move stuff out of the way
		code_exprn__clear_lval(Lval, Rval0, Rval1, ClearCode),
			% reserve the register
		code_exprn__add_lval_reg_dependencies(Lval),
		(
			{ Rval1 = create(_, _, _) }
		->
			code_exprn__get_var_rvals(Var, Rvals7),
			{ set__to_sorted_list(Rvals7, RvalList7) },
			{ code_exprn__select_rval(RvalList7, Rval) }
		;
			{ Rval = Rval1 }
		),
			% Insert the destination into the
			% rvals for the variable
		code_exprn__get_var_rvals(Var, Rvals1),
		{ set__insert(Rvals1, lval(Lval), Rvals) },
		{ Stat = evaled(Rvals) },
		code_exprn__get_var_name(Var, VarName),
		{ code_exprn__construct_code(Lval, VarName, Rval, ExprnCode) }
	),
	code_exprn__get_vars(Vars1),
	{ map__set(Vars1, Var, Stat, Vars) },
	code_exprn__set_vars(Vars),
	{ Code = tree(ClearCode, ExprnCode) }.

%------------------------------------------------------------------------------%

:- pred code_exprn__clear_lval(lval, rval, rval,
				code_tree, exprn_info, exprn_info).
:- mode code_exprn__clear_lval(in, in, out, out, in, out) is det.

code_exprn__clear_lval(Lval, Rval0, Rval, Code) -->
	(
		code_exprn__get_vars(Vars0),
		{ code_exprn__lval_in_use(Lval, Vars0) }
	->
		code_exprn__get_spare_reg(Reg),
		{ map__to_assoc_list(Vars0, VarsList0) },
		code_exprn__relocate_lval(VarsList0, Lval, reg(Reg), VarsList),
		{ map__from_assoc_list(VarsList, Vars) },
		code_exprn__set_vars(Vars),
		{ exprn_aux__substitute_lval_in_rval(Lval, reg(Reg),
								Rval0, Rval) },
		{ Code = node([
			assign(reg(Reg), lval(Lval)) -
				"shuffle lval"
		]) }
	;
		{ Rval = Rval0 },
		{ Code = empty }
	).

:- pred code_exprn__relocate_lval(assoc_list(var, var_stat), lval, lval,
			assoc_list(var, var_stat), exprn_info, exprn_info).
:- mode code_exprn__relocate_lval(in, in, in, out, in, out) is det.

code_exprn__relocate_lval([], _OldVal, _NewVal, []) --> [].
code_exprn__relocate_lval([V - Stat0|Rest0], OldVal,
				NewVal, [V - Stat|Rest]) -->
	(
		{ Stat0 = cached(Exprn0) },
		{ exprn_aux__rval_contains_lval(Exprn0, OldVal) }
	->
		code_exprn__rem_rval_reg_dependencies(Exprn0),
		{ exprn_aux__substitute_lval_in_rval(OldVal, NewVal,
							Exprn0, Exprn) },
		code_exprn__add_rval_reg_dependencies(Exprn),
		{ Stat = cached(Exprn) }
	;
		{ Stat0 = evaled(Rvals0) }
	->
		{ set__to_sorted_list(Rvals0, RvalsList0) },
		code_exprn__relocate_lval_2(RvalsList0, OldVal,
							NewVal, RvalsList),
		{ set__sorted_list_to_set(RvalsList, Rvals) },
		{ Stat = evaled(Rvals) }
	;
		% Stat0 = cached(_), \+ contains
		{ Stat = Stat0 }
	),
	code_exprn__relocate_lval(Rest0, OldVal, NewVal, Rest).

:- pred code_exprn__relocate_lval_2(list(rval), lval, lval, list(rval),
							exprn_info, exprn_info).
:- mode code_exprn__relocate_lval_2(in, in, in, out, in, out) is det.

code_exprn__relocate_lval_2([], _OldVal, _NewVal, []) --> [].
code_exprn__relocate_lval_2([R0|Rs0], OldVal, NewVal, [R|Rs]) -->
	(
		{ exprn_aux__rval_contains_lval(R0, OldVal) }
	->
		code_exprn__rem_rval_reg_dependencies(R0),
		{ exprn_aux__substitute_lval_in_rval(OldVal, NewVal, R0, R) },
		code_exprn__add_rval_reg_dependencies(R)
	;
		{ R = R0 }
	),
	code_exprn__relocate_lval_2(Rs0, OldVal, NewVal, Rs).

%------------------------------------------------------------------------------%

:- pred code_exprn__construct_code(lval, string, rval, code_tree).
:- mode code_exprn__construct_code(in, in, in, out) is det.

code_exprn__construct_code(Lval, VarName, Rval0, Code) :-
	exprn_aux__simplify_rval(Rval0, Rval),
	(
		Rval = create(Tag, Rvals, _Label)
	->
		list__length(Rvals, Arity),
		(
			Arity = 0
		->
			Code = node([
				assign(Lval, mkword(Tag, const(int_const(0)))) -
					"Construct constant"
			])
		;
			string__append("Allocating heap for ", VarName, Comment),
			Code0 = node([
				incr_hp(Lval, yes(Tag), const(int_const(Arity)))
					- Comment
			]),
			code_exprn__construct_args(Rvals, Tag, Lval, 0, Code1),
			Code = tree(Code0, Code1)
		)
	;
		string__append("Assigning from ", VarName, Comment),
		Code = node([
			assign(Lval, Rval) - Comment
		])
	).

:- pred code_exprn__construct_args(list(maybe(rval)), int, lval, int,code_tree).
:- mode code_exprn__construct_args(in, in, in, in, out) is det.

code_exprn__construct_args([], _, _, _, empty).
code_exprn__construct_args([R|Rs], Tag, Lval, N0, Code) :-
	(
		R = yes(Rval)
	->
		code_exprn__construct_code(
			field(Tag, lval(Lval), const(int_const(N0))),
							"", Rval, Code0)
	;
		Code0 = empty
	),
	N1 is N0 + 1,
	Code = tree(Code0, Code1),
	code_exprn__construct_args(Rs, Tag, Lval, N1, Code1).

%------------------------------------------------------------------------------%

:- pred code_exprn__get_var_rvals(var, set(rval), exprn_info, exprn_info).
:- mode code_exprn__get_var_rvals(in, out, in, out) is det.

code_exprn__get_var_rvals(Var, Rvals) -->
	code_exprn__get_vars(Vars),
	{ map__lookup(Vars, Var, Stat) },
	(
		{ Stat = evaled(Rvals0) },
		{ Rvals = Rvals0 }
	;
		{ Stat = cached(Rval0) },
		{ set__singleton_set(Rvals, Rval0) }
	).

%------------------------------------------------------------------------------%

code_exprn__produce_var(Var, Rval, Code) -->
	code_exprn__get_vars(Vars0),
	{ map__lookup(Vars0, Var, Stat0) },
	(
		{ Stat0 = evaled(Rvals) },
		\+ (
			{ set__member(RvalX, Rvals) },
			{ RvalX = binop(_, _, _) ;
			  RvalX = unop(_, _) ;
			  RvalX = create(_, _, _) ;
			  RvalX = mkword(_, _)
			}
		)
	->
		{ set__to_sorted_list(Rvals, RvalList) },
		{ code_exprn__select_rval(RvalList, Rval) },
		{ Code = empty }
	;
		code_exprn__get_spare_reg(Reg),
		{ Lval = reg(Reg) },
		{ Rval = lval(Lval) },
		code_exprn__place_var(Var, Lval, Code)
	).

%------------------------------------------------------------------------------%

code_exprn__produce_var_in_reg(Var, Rval, Code) -->
	code_exprn__produce_var(Var, Rval0, Code0),
	(
		{ Rval0 = lval(reg(_)) }
	->
		{ Code = Code0 },
		{ Rval = Rval0 }
	;
		code_exprn__get_spare_reg(Reg),
		{ Lval = reg(Reg) },
		{ Rval = lval(Lval) },
		code_exprn__place_var(Var, Lval, Code1),
		{ Code = tree(Code0, Code1) }
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__produce_vars(list(var), assoc_list(var, rval), code_tree,
						exprn_info, exprn_info).
:- mode code_exprn__produce_vars(in, out, out, in, out) is det.

code_exprn__produce_vars([], [], empty) --> [].
code_exprn__produce_vars([V|Vs], [V - R|Rest], Code) -->
	code_exprn__produce_var(V, R, Code0),
	code_exprn__produce_vars(Vs, Rest, Code1),
	{ Code = tree(Code0, Code1) }.

%------------------------------------------------------------------------------%

code_exprn__acquire_reg(Reg) -->
	code_exprn__get_spare_reg(Reg),
	code_exprn__get_regs(Regs0),
	{ bag__insert(Regs0, Reg, Regs) },
	code_exprn__set_regs(Regs),
	code_exprn__get_acquired(Acqu0),
	{ set__insert(Acqu0, Reg, Acqu) },
	code_exprn__set_acquired(Acqu).

%------------------------------------------------------------------------------%

	% Warning: if you get a reg, you must mark it as in use yourself.

:- pred code_exprn__get_spare_reg(reg, exprn_info, exprn_info).
:- mode code_exprn__get_spare_reg(out, in, out) is det.

code_exprn__get_spare_reg(Reg) -->
	code_exprn__get_regs(Regs),
	{ code_exprn__get_spare_reg_2(1, Regs, Reg) }.

:- pred code_exprn__get_spare_reg_2(int, bag(reg), reg).
:- mode code_exprn__get_spare_reg_2(in, in, out) is det.

code_exprn__get_spare_reg_2(N0, Regs, Reg) :-
	Reg0 = r(N0),
	(
		bag__contains(Reg0, Regs)
	->
		N1 is N0 + 1,
		code_exprn__get_spare_reg_2(N1, Regs, Reg)
	;
		Reg = Reg0
	).

%------------------------------------------------------------------------------%

code_exprn__release_reg(Reg) -->
	code_exprn__get_acquired(Acqu0),
	(
		{ set__member(Reg, Acqu0) }
	->
		{ set__delete(Acqu0, Reg, Acqu) },
		code_exprn__set_acquired(Acqu),
		code_exprn__get_regs(Regs0),
		{ bag__remove(Regs0, Reg, Regs) },
		(
			{ bag__contains(Reg, Regs) }
		->
			{ error("code_exprn__release_reg: reg still has references") }
		;
			[]
		),
		code_exprn__set_regs(Regs)
	;
		{ error("code_exprn__release_reg: attempt to release an unacquired reg") }
	).

%------------------------------------------------------------------------------%

code_exprn__lock_reg(Reg) -->
	code_exprn__get_regs(Regs0),
	{ bag__insert(Regs0, Reg, Regs) },
	code_exprn__set_regs(Regs).

code_exprn__unlock_reg(Reg) -->
	code_exprn__get_regs(Regs0),
	{ bag__remove(Regs0, Reg, Regs) },
	code_exprn__set_regs(Regs).

%------------------------------------------------------------------------------%

:- pred code_exprn__get_var_name(var, string, exprn_info, exprn_info).
:- mode code_exprn__get_var_name(in, out, in, out) is det.

code_exprn__get_var_name(Var, Name) -->
	code_exprn__get_varset(Varset),
	(
		{ varset__lookup_name(Varset, Var, VarName) }
	->
		{ VarName = Name }
	;
		{ term__var_to_int(Var, Int) },
		{ string__int_to_string(Int, IntString) },
		{ string__append("variable number ", IntString, Name) }
	).

%------------------------------------------------------------------------------%

:- pred code_exprn__get_varset(varset, exprn_info, exprn_info).
:- mode code_exprn__get_varset(out, in, out) is det.

:- pred code_exprn__set_varset(varset, exprn_info, exprn_info).
:- mode code_exprn__set_varset(in, in, out) is det.

:- pred code_exprn__get_vars(var_map, exprn_info, exprn_info).
:- mode code_exprn__get_vars(out, in, out) is det.

:- pred code_exprn__set_vars(var_map, exprn_info, exprn_info).
:- mode code_exprn__set_vars(in, in, out) is det.

:- pred code_exprn__get_regs(bag(reg), exprn_info, exprn_info).
:- mode code_exprn__get_regs(out, in, out) is det.

:- pred code_exprn__set_regs(bag(reg), exprn_info, exprn_info).
:- mode code_exprn__set_regs(in, in, out) is det.

:- pred code_exprn__get_acquired(set(reg), exprn_info, exprn_info).
:- mode code_exprn__get_acquired(out, in, out) is det.

:- pred code_exprn__set_acquired(set(reg), exprn_info, exprn_info).
:- mode code_exprn__set_acquired(in, in, out) is det.

:- pred code_exprn__get_options(option_table, exprn_info, exprn_info).
:- mode code_exprn__get_options(out, in, out) is det.

:- pred code_exprn__set_options(option_table, exprn_info, exprn_info).
:- mode code_exprn__set_options(in, in, out) is det.

code_exprn__get_varset(Varset, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(Varset, _Vars, _Regs, _Acqu, _Opt).

code_exprn__set_varset(Varset, ExprnInfo0, ExprnInfo) :-
	ExprnInfo0 = exprn_info(_Varset, Vars, Regs, Acqu, Opt),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opt).

code_exprn__get_vars(Vars, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(_Varset, Vars, _Regs, _Acqu, _Opt).

code_exprn__set_vars(Vars, ExprnInfo0, ExprnInfo) :-
	ExprnInfo0 = exprn_info(Varset, _Vars, Regs, Acqu, Opt),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opt).

code_exprn__get_regs(Regs, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(_Varset, _Vars, Regs, _Acqu, _Opt).

code_exprn__set_regs(Regs, ExprnInfo0, ExprnInfo) :-
	ExprnInfo0 = exprn_info(Varset, Vars, _Regs, Acqu, Opt),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opt).

code_exprn__get_acquired(Acqu, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(_Varset, _Vars, _Regs, Acqu, _Opt).

code_exprn__set_acquired(Acqu, ExprnInfo0, ExprnInfo) :-
	ExprnInfo0 = exprn_info(Varset, Vars, Regs, _Acqu, Opt),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opt).

code_exprn__get_options(Opt, ExprnInfo, ExprnInfo) :-
	ExprnInfo = exprn_info(_Varset, _Vars, _Regs, _Acqu, Opt).

code_exprn__set_options(Opt, ExprnInfo0, ExprnInfo) :-
	ExprnInfo0 = exprn_info(Varset, Vars, Regs, Acqu, _Opt),
	ExprnInfo = exprn_info(Varset, Vars, Regs, Acqu, Opt).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
